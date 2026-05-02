package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers.{wrapPromise, getPromiseFrom, callFunctionValue}

/** Promise built-in: Promise constructor, then, catch, finally, resolve, reject, all, race, allSettled, any. */
object PromiseBuiltins:
  import quickjs.objmodel.JSObject

  /** Helper to get Promise from an object */
  private def getPromise(obj: JSObject)(using ctx: JSContext): Option[JSValue.Promise] =
    obj.getOwnProperty("__promise") match
      case Some(p: JSValue.Promise) => Some(p)
      case _ => None

  /** Resolve a promise with a value */
  private def promiseResolve(promise: JSValue.Promise, value: JSValue)(using ctx: JSContext): Unit =
    if promise.state != JSValue.PromiseState.Pending then return

    promise.state = JSValue.PromiseState.Fulfilled
    promise.result = value

    val reactions = promise.fulfillReactions.toList
    promise.fulfillReactions.clear()
    promise.rejectReactions.clear()

    reactions.foreach { reaction =>
      ctx.queueMicrotask { () =>
        val result = reaction.onFulfilled match
          case JSValue.Native(native: quickjs.value.NativeFunction) =>
            native.call(Array(JSValue.Undefined, value))
          case JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            value
          case _ =>
            value

        promiseResolve(reaction.promise, result)
      }
    }

  /** Create a resolved promise from a value - public helper for async/await */
  def promiseResolve(value: JSValue)(using ctx: JSContext): JSValue =
    val alreadyPromise = value match
      case JSValue.Object(obj) =>
        obj.getOwnProperty("__promise") match
          case Some(_: JSValue.Promise) => true
          case _ => false
      case _ => false
    if alreadyPromise then value
    else wrapPromise(JSValue.Promise(state = JSValue.PromiseState.Fulfilled, result = value))

  /** Reject a promise with a reason */
  private def promiseReject(promise: JSValue.Promise, reason: JSValue)(using ctx: JSContext): Unit =
    if promise.state != JSValue.PromiseState.Pending then return

    promise.state = JSValue.PromiseState.Rejected
    promise.result = reason

    val reactions = promise.rejectReactions.toList
    promise.fulfillReactions.clear()
    promise.rejectReactions.clear()

    reactions.foreach { reaction =>
      ctx.queueMicrotask { () =>
        val result = reaction.onRejected match
          case JSValue.Native(native: quickjs.value.NativeFunction) =>
            native.call(Array(JSValue.Undefined, reason))
          case JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            reason
          case _ =>
            reason

        promiseResolve(reaction.promise, result)
      }
    }

  def initialize(ctx: JSContext): Unit =
    given JSContext = ctx

    val promiseConstructor = quickjs.value.NativeConstructor(
      name = "Promise",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Constructor Promise requires 'new'"),
      constructImpl = (args, ctx) =>
        given JSContext = ctx

        val promise = JSValue.Promise()
        val obj = JSObject(prototype = ctx.promisePrototype, extensible = true)
        obj.defineProperty("__promise", promise, enumerable = false, writable = false, configurable = false)

        val resolveFunc = NativeFunction(
          name = "resolve",
          impl = (resolveArgs, _) =>
            val value = resolveArgs.lift(1).getOrElse(JSValue.Undefined)
            promiseResolve(promise, value)
            JSValue.Undefined
        )

        val rejectFunc = NativeFunction(
          name = "reject",
          impl = (rejectArgs, _) =>
            val reason = rejectArgs.lift(1).getOrElse(JSValue.Undefined)
            promiseReject(promise, reason)
            JSValue.Undefined
        )

        if args.nonEmpty then
          args(0) match
            case JSValue.Native(native: quickjs.value.NativeFunction) =>
              native.call(Array(JSValue.Undefined, JSValue.Native(resolveFunc), JSValue.Native(rejectFunc)))
            case JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
              ()
            case _ =>
              ctx.throwTypeError("Promise resolver is not a function")
          end match

        JSValue.Object(obj),
      prototype = ctx.promisePrototype
    )
    BuiltinHelpers.initConstructor(promiseConstructor, length = 1)
    ctx.promisePrototype.defineProperty("constructor", JSValue.Native(promiseConstructor), enumerable = false)

    // Promise.prototype.then(onFulfilled, onRejected)
    val promiseThen = NativeFunction(
      name = "then",
      impl = (args, ctx) =>
        given JSContext = ctx
        val promise = getPromiseFrom(args.headOption.getOrElse(JSValue.Undefined), "then")
        val onFulfilled = args.lift(1).getOrElse(JSValue.Undefined)
        val onRejected = args.lift(2).getOrElse(JSValue.Undefined)
        val chainedPromise = JSValue.Promise()
        val reaction = JSValue.PromiseReaction(onFulfilled, onRejected, chainedPromise)
        promise.state match
          case JSValue.PromiseState.Pending =>
            promise.fulfillReactions += reaction; promise.rejectReactions += reaction
          case JSValue.PromiseState.Fulfilled =>
            ctx.queueMicrotask { () =>
              val result = callFunctionValue(onFulfilled, JSValue.Undefined, Array(promise.result))
              promiseResolve(chainedPromise, result)
            }
          case JSValue.PromiseState.Rejected =>
            ctx.queueMicrotask { () =>
              val result = callFunctionValue(onRejected, JSValue.Undefined, Array(promise.result))
              promiseResolve(chainedPromise, result)
            }
        wrapPromise(chainedPromise)
    )

    // Promise.prototype.catch(onRejected)
    val promiseCatch = NativeFunction(
      name = "catch",
      impl = (args, ctx) =>
        given JSContext = ctx
        val promise = getPromiseFrom(args.headOption.getOrElse(JSValue.Undefined), "catch")
        val onRejected = args.lift(1).getOrElse(JSValue.Undefined)
        val chainedPromise = JSValue.Promise()
        val reaction = JSValue.PromiseReaction(JSValue.Undefined, onRejected, chainedPromise)
        promise.state match
          case JSValue.PromiseState.Pending =>
            promise.fulfillReactions += reaction; promise.rejectReactions += reaction
          case JSValue.PromiseState.Fulfilled =>
            ctx.queueMicrotask { () => promiseResolve(chainedPromise, promise.result) }
          case JSValue.PromiseState.Rejected =>
            ctx.queueMicrotask { () =>
              val result = callFunctionValue(onRejected, JSValue.Undefined, Array(promise.result))
              promiseResolve(chainedPromise, result)
            }
        wrapPromise(chainedPromise)
    )

    // Promise.prototype.finally(onFinally)
    val promiseFinally = NativeFunction(
      name = "finally",
      impl = (args, ctx) =>
        given JSContext = ctx
        val promise = getPromiseFrom(args.headOption.getOrElse(JSValue.Undefined), "finally")
        val onFinally = args.lift(1).getOrElse(JSValue.Undefined)
        val chainedPromise = JSValue.Promise()
        val handler = onFinally match
          case JSValue.Native(_: quickjs.value.NativeFunction) | _: JSValue.Function => onFinally
          case _ => JSValue.Undefined
        promise.state match
          case JSValue.PromiseState.Pending =>
            val reaction = JSValue.PromiseReaction(handler, handler, chainedPromise)
            promise.fulfillReactions += reaction; promise.rejectReactions += reaction
          case _ =>
            ctx.queueMicrotask { () =>
              handler match
                case JSValue.Native(native: quickjs.value.NativeFunction) => native.call(Array(JSValue.Undefined))
                case _ => ()
              promiseResolve(chainedPromise, promise.result)
            }
        wrapPromise(chainedPromise)
    )

    // Promise.resolve(value) - static method
    val promiseResolveStatic = NativeFunction(
      name = "resolve",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = args.lift(1).getOrElse(JSValue.Undefined)
        val alreadyPromise = value match
          case JSValue.Object(obj) =>
            obj.getOwnProperty("__promise") match
              case Some(_: JSValue.Promise) => true
              case _ => false
          case _ => false
        if alreadyPromise then value
        else wrapPromise(JSValue.Promise(state = JSValue.PromiseState.Fulfilled, result = value))
    )

    // Promise.reject(reason) - static method
    val promiseRejectStatic = NativeFunction(
      name = "reject",
      impl = (args, ctx) =>
        given JSContext = ctx
        val reason = args.lift(1).getOrElse(JSValue.Undefined)
        wrapPromise(JSValue.Promise(state = JSValue.PromiseState.Rejected, result = reason))
    )

    // Promise.all(iterable) - static method
    val promiseAllStatic = NativeFunction(
      name = "all",
      impl = (args, ctx) =>
        given JSContext = ctx
        val iterable = args.lift(1).getOrElse(JSValue.Undefined)

        val resultPromise = JSValue.Promise()
        val resultObj = JSObject(prototype = ctx.promisePrototype, extensible = true)
        resultObj.defineProperty("__promise", resultPromise, enumerable = false, writable = false, configurable = false)

        val promises = iterable match
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj) =>
            obj.get("length")(using ctx) match
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
          case _ => IndexedSeq.empty

        if promises.isEmpty then
          resultPromise.state = JSValue.PromiseState.Fulfilled
          resultPromise.result = JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else
          val results = new Array[JSValue](promises.length)
          var remainingCount = promises.length
          var rejected = false

          promises.zipWithIndex.foreach { case (promiseValue, index) =>
            val valuePromise = promiseValue match
              case JSValue.Object(obj) =>
                getPromise(obj) match
                  case Some(p) => p
                  case None =>
                    JSValue.Promise(state = JSValue.PromiseState.Fulfilled, result = promiseValue)
              case _ =>
                JSValue.Promise(state = JSValue.PromiseState.Fulfilled, result = promiseValue)

            valuePromise.state match
              case JSValue.PromiseState.Fulfilled =>
                results(index) = valuePromise.result
                remainingCount -= 1
              case JSValue.PromiseState.Rejected if !rejected =>
                rejected = true
                resultPromise.state = JSValue.PromiseState.Rejected
                resultPromise.result = valuePromise.result
              case JSValue.PromiseState.Pending =>
                results(index) = valuePromise.result
                remainingCount -= 1
              case _ => ()
          }

          if !rejected && remainingCount == 0 then
            val resultArray = quickjs.objmodel.JSArray.empty()
            results.foreach(resultArray.push)
            resultPromise.state = JSValue.PromiseState.Fulfilled
            resultPromise.result = JSValue.JSArrayVal(resultArray)

        JSValue.Object(resultObj)
    )

    // Promise.race(iterable) - static method
    val promiseRaceStatic = NativeFunction(
      name = "race",
      impl = (args, ctx) =>
        given JSContext = ctx
        val iterable = args.lift(1).getOrElse(JSValue.Undefined)

        val resultPromise = JSValue.Promise()
        val resultObj = JSObject(prototype = ctx.promisePrototype, extensible = true)
        resultObj.defineProperty("__promise", resultPromise, enumerable = false, writable = false, configurable = false)

        val promises = iterable match
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj) =>
            obj.get("length")(using ctx) match
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
          case _ => IndexedSeq.empty

        var settled = false
        promises.foreach { promiseValue =>
          if !settled then
            promiseValue match
              case JSValue.Object(obj) =>
                getPromise(obj) match
                  case Some(p) =>
                    p.state match
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
                  case None =>
                    if !settled then
                      settled = true
                      resultPromise.state = JSValue.PromiseState.Fulfilled
                      resultPromise.result = promiseValue
              case _ =>
                if !settled then
                  settled = true
                  resultPromise.state = JSValue.PromiseState.Fulfilled
                  resultPromise.result = promiseValue
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
        val resultObj = JSObject(prototype = ctx.promisePrototype, extensible = true)
        resultObj.defineProperty("__promise", resultPromise, enumerable = false, writable = false, configurable = false)

        val promises = iterable match
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj) =>
            obj.get("length")(using ctx) match
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
          case _ => IndexedSeq.empty

        if promises.isEmpty then
          resultPromise.state = JSValue.PromiseState.Fulfilled
          resultPromise.result = JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else
          val results = quickjs.objmodel.JSArray.empty()
          promises.foreach { promiseValue =>
            val resultObj = JSObject(prototype = ctx.objectPrototype, extensible = true)

            promiseValue match
              case JSValue.Object(obj) =>
                getPromise(obj) match
                  case Some(p) =>
                    p.state match
                      case JSValue.PromiseState.Fulfilled =>
                        resultObj.set("status", JSValue.fromString("fulfilled"))
                        resultObj.set("value", p.result)
                      case JSValue.PromiseState.Rejected =>
                        resultObj.set("status", JSValue.fromString("rejected"))
                        resultObj.set("reason", p.result)
                      case JSValue.PromiseState.Pending =>
                        resultObj.set("status", JSValue.fromString("fulfilled"))
                        resultObj.set("value", JSValue.Undefined)
                  case None =>
                    resultObj.set("status", JSValue.fromString("fulfilled"))
                    resultObj.set("value", promiseValue)
              case _ =>
                resultObj.set("status", JSValue.fromString("fulfilled"))
                resultObj.set("value", promiseValue)

            results.push(JSValue.Object(resultObj))
          }

          resultPromise.state = JSValue.PromiseState.Fulfilled
          resultPromise.result = JSValue.JSArrayVal(results)

        JSValue.Object(resultObj)
    )

    // Promise.any(iterable) - static method
    val promiseAnyStatic = NativeFunction(
      name = "any",
      impl = (args, ctx) =>
        given JSContext = ctx
        val iterable = args.lift(1).getOrElse(JSValue.Undefined)

        val resultPromise = JSValue.Promise()
        val resultObj = JSObject(prototype = ctx.promisePrototype, extensible = true)
        resultObj.defineProperty("__promise", resultPromise, enumerable = false, writable = false, configurable = false)

        val promises = iterable match
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj) =>
            obj.get("length")(using ctx) match
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
          case _ => IndexedSeq.empty

        if promises.isEmpty then
          val errorObj = JSObject(prototype = ctx.objectPrototype, extensible = true)
          errorObj.set("name", JSValue.fromString("AggregateError"))
          errorObj.set("message", JSValue.fromString("All promises were rejected"))
          errorObj.set("errors", JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty()))
          resultPromise.state = JSValue.PromiseState.Rejected
          resultPromise.result = JSValue.Object(errorObj)
        else
          var fulfilled = false
          val errors = quickjs.objmodel.JSArray.empty()

          promises.foreach { promiseValue =>
            if !fulfilled then
              promiseValue match
                case JSValue.Object(obj) =>
                  getPromise(obj) match
                    case Some(p) =>
                      p.state match
                        case JSValue.PromiseState.Fulfilled =>
                          fulfilled = true
                          resultPromise.state = JSValue.PromiseState.Fulfilled
                          resultPromise.result = p.result
                        case JSValue.PromiseState.Rejected =>
                          errors.push(p.result)
                        case JSValue.PromiseState.Pending =>
                          ()
                    case None =>
                      if !fulfilled then
                        fulfilled = true
                        resultPromise.state = JSValue.PromiseState.Fulfilled
                        resultPromise.result = promiseValue
                case _ =>
                  if !fulfilled then
                    fulfilled = true
                    resultPromise.state = JSValue.PromiseState.Fulfilled
                    resultPromise.result = promiseValue
          }

          if !fulfilled then
            val errorObj = JSObject(prototype = ctx.objectPrototype, extensible = true)
            errorObj.set("name", JSValue.fromString("AggregateError"))
            errorObj.set("message", JSValue.fromString("All promises were rejected"))
            errorObj.set("errors", JSValue.JSArrayVal(errors))
            resultPromise.state = JSValue.PromiseState.Rejected
            resultPromise.result = JSValue.Object(errorObj)

        JSValue.Object(resultObj)
    )

    ctx.promisePrototype.defineProperty("then", JSValue.Native(promiseThen), enumerable = false)
    ctx.promisePrototype.defineProperty("catch", JSValue.Native(promiseCatch), enumerable = false)
    ctx.promisePrototype.defineProperty("finally", JSValue.Native(promiseFinally), enumerable = false)

    promiseConstructor.funcObj.defineProperty("resolve", JSValue.Native(promiseResolveStatic), enumerable = false)
    promiseConstructor.funcObj.defineProperty("reject", JSValue.Native(promiseRejectStatic), enumerable = false)
    promiseConstructor.funcObj.defineProperty("all", JSValue.Native(promiseAllStatic), enumerable = false)
    promiseConstructor.funcObj.defineProperty("race", JSValue.Native(promiseRaceStatic), enumerable = false)
    promiseConstructor.funcObj.defineProperty("allSettled", JSValue.Native(promiseAllSettledStatic), enumerable = false)
    promiseConstructor.funcObj.defineProperty("any", JSValue.Native(promiseAnyStatic), enumerable = false)

    // Symbol.species getter returning this
    val symSpecies = ctx.global.get("Symbol") match
      case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc.funcObj.get("species")(using ctx)
      case _ => JSValue.Undefined
    symSpecies match
      case sym: JSValue.Symbol =>
        val speciesGetter = NativeFunction(
          name = "get [Symbol.species]",
          length = 0,
          impl = (args, ctx) => args(0))
        promiseConstructor.funcObj.defineSymbolAccessorProperty(sym.value, getter = Some(JSValue.Native(speciesGetter)), setter = None, enumerable = false, configurable = true)
      case _ => ()

    ctx.global.defineProperty("Promise", JSValue.Native(promiseConstructor), enumerable = false)
