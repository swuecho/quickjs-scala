package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.{JSContext, JSException}
import quickjs.runtime.builtins.BuiltinHelpers.{
  wrapPromise,
  getPromiseFrom,
  callFunctionValue
}

/** Promise built-in: Promise constructor, then, catch, finally, resolve,
  * reject, all, race, allSettled, any.
  */
object PromiseBuiltins {
  import quickjs.objmodel.JSObject

  private def isConstructor(value: JSValue): Boolean = value match {
    case f: JSValue.Function => f.isConstructor
    case JSValue.Native(_: NativeConstructor) => true
    case _ => false
  }

  private def constructValue(constructor: JSValue, args: Array[JSValue])(using
      ctx: JSContext
  ): JSValue = constructor match {
    case JSValue.Native(nc: NativeConstructor) => nc.construct(args)
    case f: JSValue.Function if f.isConstructor =>
      val proto = BuiltinHelpers.getPropertyWithGetter(f, "prototype") match {
        case JSValue.Object(obj) => obj
        case _                   => ctx.objectPrototype
      }
      val receiver = JSObject(prototype = proto, extensible = true)
      val result = quickjs.interpreter.Interpreter().call(
        BuiltinHelpers.functionToBytecode(f),
        JSValue.Object(receiver),
        args,
        f.closure,
        constructor
      )
      result match {
        case JSValue.Object(_) | _: JSValue.Function | JSValue.JSArrayVal(_) |
            JSValue.Native(_) => result
        case _ => JSValue.Object(receiver)
      }
    case _ => ctx.throwTypeError("Promise constructor is not a constructor")
  }

  private final case class PromiseCapability(
      promise: JSValue,
      resolve: JSValue,
      reject: JSValue
  )

  private def newPromiseCapability(constructor: JSValue)(using
      ctx: JSContext
  ): PromiseCapability = {
    if !isConstructor(constructor) then
      ctx.throwTypeError("Promise method called on a non-constructor")
    var resolve: JSValue = JSValue.Undefined
    var reject: JSValue = JSValue.Undefined
    val executor = NativeFunction(
      name = "",
      length = 2,
      impl = (args, ctx) => {
        if resolve != JSValue.Undefined || reject != JSValue.Undefined then
          ctx.throwTypeError("Promise capability executor called more than once")
        val supplied = if args.length >= 3 then args.drop(1) else args
        resolve = supplied.headOption.getOrElse(JSValue.Undefined)
        reject = supplied.lift(1).getOrElse(JSValue.Undefined)
        JSValue.Undefined
      }
    )
    val promise = constructValue(constructor, Array(JSValue.Native(executor)))
    if !BuiltinHelpers.isCallable(resolve) || !BuiltinHelpers.isCallable(reject)
    then ctx.throwTypeError("Promise capability functions are not callable")
    PromiseCapability(promise, resolve, reject)
  }

  private def makeAggregateError(errors: JSValue)(using
      ctx: JSContext
  ): JSValue =
    val message = JSValue.fromString("All promises were rejected")
    ctx.global.get("AggregateError") match {
      case JSValue.Native(constructor: NativeConstructor) =>
        constructor.construct(Array(errors, message))
      case _ =>
        val errorObj =
          JSObject(prototype = ctx.objectPrototype, extensible = true)
        errorObj.set("name", JSValue.fromString("AggregateError"))
        errorObj.set("message", message)
        errorObj.set("errors", errors)
        JSValue.Object(errorObj)
    }

  /** Helper to get Promise from an object */
  private def getPromise(obj: JSObject)(using
      ctx: JSContext
  ): Option[JSValue.Promise] =
    obj.getOwnProperty("__promise") match {
      case Some(p: JSValue.Promise) => Some(p)
      case _                        => None
    }

  /** Resolve a promise with a value */
  private def promiseResolve(promise: JSValue.Promise, value: JSValue)(using
      ctx: JSContext
  ): Unit = {
    if promise.state != JSValue.PromiseState.Pending then return

    promise.state = JSValue.PromiseState.Fulfilled
    promise.result = value

    val reactions = promise.fulfillReactions.toList
    promise.fulfillReactions.clear()
    promise.rejectReactions.clear()

    reactions.foreach { reaction =>
      ctx.queueMicrotask { () =>
        try {
          val result =
            if BuiltinHelpers.isCallable(reaction.onFulfilled) then
              BuiltinHelpers.callFunctionWithThis(
                reaction.onFulfilled,
                JSValue.Undefined,
                Array(value)
              )
            else value
          promiseResolve(reaction.promise, result)
        } catch {
          case error: JSException => promiseReject(reaction.promise, error.getValue)
        }
      }
    }
  }

  /** Create a resolved promise from a value - public helper for async/await */
  def promiseResolve(value: JSValue)(using ctx: JSContext): JSValue = {
    val alreadyPromise = value match {
      case JSValue.Object(obj) =>
        obj.getOwnProperty("__promise") match {
          case Some(_: JSValue.Promise) => true
          case _                        => false
        }
      case _ => false
    }
    if alreadyPromise then value
    else
      wrapPromise(
        JSValue.Promise(state = JSValue.PromiseState.Fulfilled, result = value)
      )
  }

  /** Reject a promise with a reason */
  private def promiseReject(promise: JSValue.Promise, reason: JSValue)(using
      ctx: JSContext
  ): Unit = {
    if promise.state != JSValue.PromiseState.Pending then return

    promise.state = JSValue.PromiseState.Rejected
    promise.result = reason

    val reactions = promise.rejectReactions.toList
    promise.fulfillReactions.clear()
    promise.rejectReactions.clear()

    reactions.foreach { reaction =>
      ctx.queueMicrotask { () =>
        if BuiltinHelpers.isCallable(reaction.onRejected) then
          try {
            val result = BuiltinHelpers.callFunctionWithThis(
              reaction.onRejected,
              JSValue.Undefined,
              Array(reason)
            )
            promiseResolve(reaction.promise, result)
          } catch {
            case error: JSException => promiseReject(reaction.promise, error.getValue)
          }
        else promiseReject(reaction.promise, reason)
      }
    }
  }

  def initialize(ctx: JSContext): Unit = {
    given JSContext = ctx

    def initializePromiseObject(receiver: JSObject, args: Array[JSValue]): JSValue = {
      val promise = JSValue.Promise()
      receiver.defineProperty(
        "__promise",
        promise,
        enumerable = false,
        writable = false,
        configurable = false
      )

      val resolveFunc = NativeFunction(
        name = "resolve",
        impl = (resolveArgs, _) => {
          val value = resolveArgs.lastOption.getOrElse(JSValue.Undefined)
          promiseResolve(promise, value)
          JSValue.Undefined
        }
      )
      val rejectFunc = NativeFunction(
        name = "reject",
        impl = (rejectArgs, _) => {
          val reason = rejectArgs.lastOption.getOrElse(JSValue.Undefined)
          promiseReject(promise, reason)
          JSValue.Undefined
        }
      )
      if args.isEmpty || !BuiltinHelpers.isCallable(args(0)) then
        ctx.throwTypeError("Promise resolver is not a function")
      BuiltinHelpers.callFunctionWithThis(
        args(0),
        JSValue.Undefined,
        Array(JSValue.Native(resolveFunc), JSValue.Native(rejectFunc))
      )
      JSValue.Object(receiver)
    }

    val promiseConstructor = quickjs.value.NativeConstructor(
      name = "Promise",
      callImpl = (args, callCtx) =>
        given JSContext = callCtx
        args.headOption match {
          case Some(JSValue.Object(receiver)) =>
            initializePromiseObject(receiver, args.drop(1))
          case _ => callCtx.currentThis match {
            case JSValue.Object(receiver) => initializePromiseObject(receiver, args)
            case _ => callCtx.throwTypeError("Constructor Promise requires 'new'")
          }
        }
      ,
      constructImpl = (args, constructCtx) => {
        given JSContext = constructCtx
        initializePromiseObject(
          JSObject(prototype = constructCtx.promisePrototype, extensible = true),
          args
        )
      }
      ,
      prototype = ctx.promisePrototype
    )
    BuiltinHelpers.initConstructor(promiseConstructor, length = 1)
    ctx.promisePrototype.defineProperty(
      "constructor",
      JSValue.Native(promiseConstructor),
      enumerable = false
    )

    // Promise.prototype.then(onFulfilled, onRejected)
    val promiseThen = NativeFunction(
      name = "then",
      impl = (args, ctx) =>
        given JSContext = ctx
        val promise =
          getPromiseFrom(args.headOption.getOrElse(JSValue.Undefined), "then")
        val onFulfilled = args.lift(1).getOrElse(JSValue.Undefined)
        val onRejected = args.lift(2).getOrElse(JSValue.Undefined)
        val chainedPromise = JSValue.Promise()
        val reaction =
          JSValue.PromiseReaction(onFulfilled, onRejected, chainedPromise)
        promise.state match {
          case JSValue.PromiseState.Pending =>
            promise.fulfillReactions += reaction;
            promise.rejectReactions += reaction
          case JSValue.PromiseState.Fulfilled =>
            ctx.queueMicrotask { () =>
              val result = callFunctionValue(
                onFulfilled,
                JSValue.Undefined,
                Array(promise.result)
              )
              promiseResolve(chainedPromise, result)
            }
          case JSValue.PromiseState.Rejected =>
            ctx.queueMicrotask { () =>
              val result = callFunctionValue(
                onRejected,
                JSValue.Undefined,
                Array(promise.result)
              )
              promiseResolve(chainedPromise, result)
            }
        }
        wrapPromise(chainedPromise)
    )

    // Promise.prototype.catch(onRejected)
    val promiseCatch = NativeFunction(
      name = "catch",
      impl = (args, ctx) =>
        given JSContext = ctx
        val promise =
          getPromiseFrom(args.headOption.getOrElse(JSValue.Undefined), "catch")
        val onRejected = args.lift(1).getOrElse(JSValue.Undefined)
        val chainedPromise = JSValue.Promise()
        val reaction =
          JSValue.PromiseReaction(JSValue.Undefined, onRejected, chainedPromise)
        promise.state match {
          case JSValue.PromiseState.Pending =>
            promise.fulfillReactions += reaction;
            promise.rejectReactions += reaction
          case JSValue.PromiseState.Fulfilled =>
            ctx.queueMicrotask { () =>
              promiseResolve(chainedPromise, promise.result)
            }
          case JSValue.PromiseState.Rejected =>
            ctx.queueMicrotask { () =>
              val result = callFunctionValue(
                onRejected,
                JSValue.Undefined,
                Array(promise.result)
              )
              promiseResolve(chainedPromise, result)
            }
        }
        wrapPromise(chainedPromise)
    )

    // Promise.prototype.finally(onFinally)
    val promiseFinally = NativeFunction(
      name = "finally",
      impl = (args, ctx) =>
        given JSContext = ctx
        val promise = getPromiseFrom(
          args.headOption.getOrElse(JSValue.Undefined),
          "finally"
        )
        val onFinally = args.lift(1).getOrElse(JSValue.Undefined)
        val chainedPromise = JSValue.Promise()
        val handler = onFinally match {
          case JSValue.Native(_: quickjs.value.NativeFunction) |
              _: JSValue.Function =>
            onFinally
          case _ => JSValue.Undefined
        }
        promise.state match {
          case JSValue.PromiseState.Pending =>
            val reaction =
              JSValue.PromiseReaction(handler, handler, chainedPromise)
            promise.fulfillReactions += reaction;
            promise.rejectReactions += reaction
          case _ =>
            ctx.queueMicrotask { () =>
              handler match {
                case JSValue.Native(native: quickjs.value.NativeFunction) =>
                  native.call(Array(JSValue.Undefined))
                case _ => ()
              }
              promiseResolve(chainedPromise, promise.result)
            }
        }
        wrapPromise(chainedPromise)
    )

    // Promise.resolve(value) - static method
    val promiseResolveStatic = NativeFunction(
      name = "resolve",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = args.lift(1).getOrElse(JSValue.Undefined)
        val alreadyPromise = value match {
          case JSValue.Object(obj) =>
            obj.getOwnProperty("__promise") match {
              case Some(_: JSValue.Promise) => true
              case _                        => false
            }
          case _ => false
        }
        if alreadyPromise then value
        else
          wrapPromise(
            JSValue.Promise(
              state = JSValue.PromiseState.Fulfilled,
              result = value
            )
          )
    )

    // Promise.reject(reason) - static method
    val promiseRejectStatic = NativeFunction(
      name = "reject",
      impl = (args, ctx) =>
        given JSContext = ctx
        val reason = args.lift(1).getOrElse(JSValue.Undefined)
        wrapPromise(
          JSValue.Promise(
            state = JSValue.PromiseState.Rejected,
            result = reason
          )
        )
    )

    // Promise.all(iterable) - static method
    val promiseAllStatic = NativeFunction(
      name = "all",
      impl = (args, ctx) =>
        given JSContext = ctx
        val constructor = args.headOption.getOrElse(JSValue.Undefined)
        val iterable = args.lift(1).getOrElse(JSValue.Undefined)
        val capability = newPromiseCapability(constructor)
        try {
        val constructorResolve =
          BuiltinHelpers.getPropertyWithGetter(constructor, "resolve")
        if !BuiltinHelpers.isCallable(constructorResolve) then
          ctx.throwTypeError("Promise.resolve is not callable")

        val promises = iterable match {
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj)     =>
            obj.get("length")(using ctx) match {
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
            }
          case _ => IndexedSeq.empty
        }

        if promises.isEmpty then
          BuiltinHelpers.callFunctionWithThis(
            capability.resolve,
            JSValue.Undefined,
            Array(JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty()))
          )
        else {
          val results = new Array[JSValue](promises.length)
          var remainingCount = promises.length

          promises.zipWithIndex.foreach { case (promiseValue, index) =>
            val nextPromise = BuiltinHelpers.callFunctionWithThis(
              constructorResolve,
              constructor,
              Array(promiseValue)
            )
            var alreadyCalled = false
            val resolveElement = NativeFunction(
              name = "",
              length = 1,
              impl = (resolveArgs, _) => {
                if !alreadyCalled then {
                  alreadyCalled = true
                  results(index) = resolveArgs.lastOption.getOrElse(JSValue.Undefined)
                  remainingCount -= 1
                  if remainingCount == 0 then {
                    val resultArray = quickjs.objmodel.JSArray.empty()
                    results.foreach(resultArray.push)
                    BuiltinHelpers.callFunctionWithThis(
                      capability.resolve,
                      JSValue.Undefined,
                      Array(JSValue.JSArrayVal(resultArray))
                    )
                  }
                }
                JSValue.Undefined
              }
            )
            resolveElement.funcObj.setPrototype(ctx.functionPrototype)
            val thenMethod =
              BuiltinHelpers.getPropertyWithGetter(nextPromise, "then")
            if !BuiltinHelpers.isCallable(thenMethod) then
              ctx.throwTypeError("Promise resolve result has no callable then")
            BuiltinHelpers.callFunctionWithThis(
              thenMethod,
              nextPromise,
              Array(JSValue.Native(resolveElement), capability.reject)
            )
          }
        }
        capability.promise
        } catch {
          case error: JSException =>
            BuiltinHelpers.callFunctionWithThis(
              capability.reject,
              JSValue.Undefined,
              Array(error.getValue)
            )
            capability.promise
        }
    )

    // Promise.race(iterable) - static method
    val promiseRaceStatic = NativeFunction(
      name = "race",
      impl = (args, ctx) =>
        given JSContext = ctx
        val iterable = args.lift(1).getOrElse(JSValue.Undefined)

        val resultPromise = JSValue.Promise()
        val resultObj =
          JSObject(prototype = ctx.promisePrototype, extensible = true)
        resultObj.defineProperty(
          "__promise",
          resultPromise,
          enumerable = false,
          writable = false,
          configurable = false
        )

        val promises = iterable match {
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj)     =>
            obj.get("length")(using ctx) match {
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
            }
          case _ => IndexedSeq.empty
        }

        var settled = false
        promises.foreach { promiseValue =>
          if !settled then
            promiseValue match {
              case JSValue.Object(obj) =>
                getPromise(obj) match {
                  case Some(p) =>
                    p.state match {
                      case JSValue.PromiseState.Fulfilled =>
                        settled = true
                        resultPromise.state = JSValue.PromiseState.Fulfilled
                        resultPromise.result = p.result
                      case JSValue.PromiseState.Rejected =>
                        settled = true
                        resultPromise.state = JSValue.PromiseState.Rejected
                        resultPromise.result = p.result
                      case JSValue.PromiseState.Pending =>
                        ()
                    }
                  case None =>
                    if !settled then {
                      settled = true
                      resultPromise.state = JSValue.PromiseState.Fulfilled
                      resultPromise.result = promiseValue
                    }
                }
              case _ =>
                if !settled then {
                  settled = true
                  resultPromise.state = JSValue.PromiseState.Fulfilled
                  resultPromise.result = promiseValue
                }
            }
        }

        JSValue.Object(resultObj)
    )

    // Promise.allSettled(iterable) - static method
    val promiseAllSettledStatic = NativeFunction(
      name = "allSettled",
      impl = (args, ctx) =>
        given JSContext = ctx
        val iterable = args.lift(1).getOrElse(JSValue.Undefined)

        val resultPromise = JSValue.Promise()
        val resultObj =
          JSObject(prototype = ctx.promisePrototype, extensible = true)
        resultObj.defineProperty(
          "__promise",
          resultPromise,
          enumerable = false,
          writable = false,
          configurable = false
        )

        val promises = iterable match {
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj)     =>
            obj.get("length")(using ctx) match {
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
            }
          case _ => IndexedSeq.empty
        }

        if promises.isEmpty then {
          resultPromise.state = JSValue.PromiseState.Fulfilled
          resultPromise.result =
            JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        }
        else {
          val results = quickjs.objmodel.JSArray.empty()
          promises.foreach { promiseValue =>
            val resultObj =
              JSObject(prototype = ctx.objectPrototype, extensible = true)

            promiseValue match {
              case JSValue.Object(obj) =>
                getPromise(obj) match {
                  case Some(p) =>
                    p.state match {
                      case JSValue.PromiseState.Fulfilled =>
                        resultObj.set("status", JSValue.fromString("fulfilled"))
                        resultObj.set("value", p.result)
                      case JSValue.PromiseState.Rejected =>
                        resultObj.set("status", JSValue.fromString("rejected"))
                        resultObj.set("reason", p.result)
                      case JSValue.PromiseState.Pending =>
                        resultObj.set("status", JSValue.fromString("fulfilled"))
                        resultObj.set("value", JSValue.Undefined)
                    }
                  case None =>
                    resultObj.set("status", JSValue.fromString("fulfilled"))
                    resultObj.set("value", promiseValue)
                }
              case _ =>
                resultObj.set("status", JSValue.fromString("fulfilled"))
                resultObj.set("value", promiseValue)
            }

            results.push(JSValue.Object(resultObj))
          }

          resultPromise.state = JSValue.PromiseState.Fulfilled
          resultPromise.result = JSValue.JSArrayVal(results)
        }

        JSValue.Object(resultObj)
    )

    // Promise.any(iterable) - static method
    val promiseAnyStatic = NativeFunction(
      name = "any",
      impl = (args, ctx) =>
        given JSContext = ctx
        val iterable = args.lift(1).getOrElse(JSValue.Undefined)

        val resultPromise = JSValue.Promise()
        val resultObj =
          JSObject(prototype = ctx.promisePrototype, extensible = true)
        resultObj.defineProperty(
          "__promise",
          resultPromise,
          enumerable = false,
          writable = false,
          configurable = false
        )

        val promises = iterable match {
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj)     =>
            obj.get("length")(using ctx) match {
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
            }
          case _ => IndexedSeq.empty
        }

        if promises.isEmpty then {
          resultPromise.state = JSValue.PromiseState.Rejected
          resultPromise.result = makeAggregateError(
            JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
          )
        }
        else {
          var fulfilled = false
          val errors = quickjs.objmodel.JSArray.empty()

          promises.foreach { promiseValue =>
            if !fulfilled then
              promiseValue match {
                case JSValue.Object(obj) =>
                  getPromise(obj) match {
                    case Some(p) =>
                      p.state match {
                        case JSValue.PromiseState.Fulfilled =>
                          fulfilled = true
                          resultPromise.state = JSValue.PromiseState.Fulfilled
                          resultPromise.result = p.result
                        case JSValue.PromiseState.Rejected =>
                          errors.push(p.result)
                        case JSValue.PromiseState.Pending =>
                          ()
                      }
                    case None =>
                      if !fulfilled then {
                        fulfilled = true
                        resultPromise.state = JSValue.PromiseState.Fulfilled
                        resultPromise.result = promiseValue
                      }
                  }
                case _ =>
                  if !fulfilled then {
                    fulfilled = true
                    resultPromise.state = JSValue.PromiseState.Fulfilled
                    resultPromise.result = promiseValue
                  }
              }
          }

          if !fulfilled then {
            resultPromise.state = JSValue.PromiseState.Rejected
            resultPromise.result = makeAggregateError(JSValue.JSArrayVal(errors))
          }
        }

        JSValue.Object(resultObj)
    )

    ctx.promisePrototype.defineProperty(
      "then",
      JSValue.Native(promiseThen),
      enumerable = false
    )
    ctx.promisePrototype.defineProperty(
      "catch",
      JSValue.Native(promiseCatch),
      enumerable = false
    )
    ctx.promisePrototype.defineProperty(
      "finally",
      JSValue.Native(promiseFinally),
      enumerable = false
    )

    promiseConstructor.funcObj.defineProperty(
      "resolve",
      JSValue.Native(promiseResolveStatic),
      enumerable = false
    )
    promiseConstructor.funcObj.defineProperty(
      "reject",
      JSValue.Native(promiseRejectStatic),
      enumerable = false
    )
    promiseConstructor.funcObj.defineProperty(
      "all",
      JSValue.Native(promiseAllStatic),
      enumerable = false
    )
    promiseConstructor.funcObj.defineProperty(
      "race",
      JSValue.Native(promiseRaceStatic),
      enumerable = false
    )
    promiseConstructor.funcObj.defineProperty(
      "allSettled",
      JSValue.Native(promiseAllSettledStatic),
      enumerable = false
    )
    promiseConstructor.funcObj.defineProperty(
      "any",
      JSValue.Native(promiseAnyStatic),
      enumerable = false
    )

    // Symbol.species getter returning this
    val symSpecies = ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.get("species")(using ctx)
      case _ => JSValue.Undefined
    }
    symSpecies match {
      case sym: JSValue.Symbol =>
        val speciesGetter = NativeFunction(
          name = "get [Symbol.species]",
          length = 0,
          impl = (args, ctx) => args(0)
        )
        promiseConstructor.funcObj.defineSymbolAccessorProperty(
          sym.value,
          getter = Some(JSValue.Native(speciesGetter)),
          setter = None,
          enumerable = false,
          configurable = true
        )
      case _ => ()
    }

    ctx.global.defineProperty(
      "Promise",
      JSValue.Native(promiseConstructor),
      enumerable = false
    )
  }
}
