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

  /** ES SpeciesConstructor(O, defaultConstructor). */
  private def speciesConstructor(obj: JSValue, defaultConstructor: JSValue)(using
      ctx: JSContext
  ): JSValue = {
    val ctor = BuiltinHelpers.getPropertyWithGetter(obj, "constructor")
    if ctor == JSValue.Undefined then defaultConstructor
    else {
      if !BuiltinHelpers.isObjectLikeValue(ctor) then
        ctx.throwTypeError("constructor is not an object")
      val species =
        BuiltinHelpers.getSymbolPropertyWithGetter(
          ctor,
          BuiltinHelpers.wellKnownSymbolId("species")
        )
      if species == JSValue.Undefined || species == JSValue.Null then
        defaultConstructor
      else if isConstructor(species) then species
      else ctx.throwTypeError("species is not a constructor")
    }
  }

  /** ES PerformPromiseThen with an explicit result capability. The reactions
    * settle the capability instead of an internal promise.
    */
  private def performThen(
      promise: JSValue.Promise,
      onFulfilled: JSValue,
      onRejected: JSValue,
      capability: PromiseCapability
  )(using ctx: JSContext): Unit = {
    val reaction = JSValue.PromiseReaction(
      onFulfilled,
      onRejected,
      promise,
      resolveFunc = capability.resolve,
      rejectFunc = capability.reject
    )
    promise.state match {
      case JSValue.PromiseState.Pending =>
        promise.fulfillReactions += reaction
        promise.rejectReactions += reaction
      case JSValue.PromiseState.Fulfilled =>
        ctx.queueMicrotask { () =>
          try {
            val result =
              if BuiltinHelpers.isCallable(onFulfilled) then
                BuiltinHelpers.callFunctionWithThis(
                  onFulfilled,
                  JSValue.Undefined,
                  Array(promise.result)
                )
              else promise.result
            settleReaction(reaction, result, isRejection = false)
          } catch {
            case error: JSException =>
              settleReaction(reaction, error.getValue, isRejection = true)
          }
        }
      case JSValue.PromiseState.Rejected =>
        ctx.queueMicrotask { () =>
          if BuiltinHelpers.isCallable(onRejected) then
            try {
              val result = BuiltinHelpers.callFunctionWithThis(
                onRejected,
                JSValue.Undefined,
                Array(promise.result)
              )
              settleReaction(reaction, result, isRejection = false)
            } catch {
              case error: JSException =>
                settleReaction(reaction, error.getValue, isRejection = true)
            }
          else settleReaction(reaction, promise.result, isRejection = true)
        }
    }
  }

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
    executor.funcObj.setPrototype(ctx.functionPrototype)
    val promise = constructValue(constructor, Array(JSValue.Native(executor)))
    if !BuiltinHelpers.isCallable(resolve) || !BuiltinHelpers.isCallable(reject)
    then ctx.throwTypeError("Promise capability functions are not callable")
    PromiseCapability(promise, resolve, reject)
  }

  /** ES PromiseResolve(C, x). */
  private def promiseResolveFor(constructor: JSValue, value: JSValue)(using
      ctx: JSContext
  ): JSValue = {
    val isPromise = value match {
      case JSValue.Object(obj) =>
        obj.getOwnProperty("__promise") match {
          case Some(_: JSValue.Promise) => true
          case _                        => false
        }
      case _ => false
    }
    if isPromise then {
      val valueConstructor =
        BuiltinHelpers.getPropertyWithGetter(value, "constructor")
      if valueConstructor == constructor then return value
    }
    val capability = newPromiseCapability(constructor)
    BuiltinHelpers.callFunctionWithThis(
      capability.resolve,
      JSValue.Undefined,
      Array(value)
    )
    capability.promise
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

  /** Settle a reaction's target: use the capability function when the reaction
    * was created by `then` with a species constructor, else the internal
    * reaction promise.
    */
  private def settleReaction(
      reaction: JSValue.PromiseReaction,
      value: JSValue,
      isRejection: Boolean
  )(using ctx: JSContext): Unit = {
    val func = if isRejection then reaction.rejectFunc else reaction.resolveFunc
    if BuiltinHelpers.isCallable(func) then
      BuiltinHelpers.callFunctionWithThis(func, JSValue.Undefined, Array(value))
    else if isRejection then promiseReject(reaction.promise, value)
    else promiseResolve(reaction.promise, value)
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
          settleReaction(reaction, result, isRejection = false)
        } catch {
          case error: JSException =>
            settleReaction(reaction, error.getValue, isRejection = true)
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
            settleReaction(reaction, result, isRejection = false)
          } catch {
            case error: JSException =>
              settleReaction(reaction, error.getValue, isRejection = true)
          }
        else settleReaction(reaction, reason, isRejection = true)
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
        name = "",
        length = 1,
        impl = (resolveArgs, _) => {
          val value = resolveArgs.lastOption.getOrElse(JSValue.Undefined)
          promiseResolve(promise, value)
          JSValue.Undefined
        }
      )
      resolveFunc.funcObj.setPrototype(ctx.functionPrototype)
      val rejectFunc = NativeFunction(
        name = "",
        length = 1,
        impl = (rejectArgs, _) => {
          val reason = rejectArgs.lastOption.getOrElse(JSValue.Undefined)
          promiseReject(promise, reason)
          JSValue.Undefined
        }
      )
      rejectFunc.funcObj.setPrototype(ctx.functionPrototype)
      if args.isEmpty || !BuiltinHelpers.isCallable(args(0)) then
        ctx.throwTypeError("Promise resolver is not a function")
      try
        BuiltinHelpers.callFunctionWithThis(
          args(0),
          JSValue.Undefined,
          Array(JSValue.Native(resolveFunc), JSValue.Native(rejectFunc))
        )
      catch {
        case error: JSException =>
          // An executor that throws (even after resolving) rejects the promise.
          promiseReject(promise, error.getValue)
      }
      JSValue.Object(receiver)
    }

    val promiseConstructor = quickjs.value.NativeConstructor(
      name = "Promise",
      callImpl = (args, callCtx) =>
        given JSContext = callCtx
        callCtx.throwTypeError("Constructor Promise requires 'new'")
      ,
      constructImpl = (args, constructCtx) => {
        given JSContext = constructCtx
        initializePromiseObject(
          JSObject(prototype = constructCtx.promisePrototype, extensible = true),
          args
        )
      }
      ,
      constructWithNewTarget = Some((args, newTarget, constructCtx) => {
        given JSContext = constructCtx
        if args.isEmpty || !BuiltinHelpers.isCallable(args(0)) then
          constructCtx.throwTypeError("Promise resolver is not a function")
        val proto =
          if newTarget == JSValue.Undefined then constructCtx.promisePrototype
          else
            BuiltinHelpers.getPropertyWithGetter(newTarget, "prototype") match {
              case JSValue.Object(obj) => obj
              case _                   => constructCtx.promisePrototype
            }
        initializePromiseObject(
          JSObject(prototype = proto, extensible = true),
          args
        )
      })
      ,
      superInitImpl = Some((thisValue, args, constructCtx) => {
        given JSContext = constructCtx
        if args.isEmpty || !BuiltinHelpers.isCallable(args(0)) then
          constructCtx.throwTypeError("Promise resolver is not a function")
        thisValue match {
          case JSValue.Object(receiver) =>
            initializePromiseObject(receiver, args)
          case _ =>
            constructCtx.throwTypeError("Constructor Promise requires 'new'")
        }
      })
      ,
      prototype = ctx.promisePrototype
    )
    BuiltinHelpers.initConstructor(promiseConstructor, length = 1)
    // Promise.prototype[Symbol.toStringTag] = "Promise"
    ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.getOwnProperty("toStringTag")(using ctx) match {
          case Some(JSValue.Symbol(id)) =>
            ctx.promisePrototype.initSymbolProperty(
              id,
              JSValue.fromString("Promise"),
              enumerable = false,
              writable = false,
              configurable = true
            )
          case _ => ()
        }
      case _ => ()
    }
    ctx.promisePrototype.defineProperty(
      "constructor",
      JSValue.Native(promiseConstructor),
      enumerable = false
    )

    // Promise.prototype.then(onFulfilled, onRejected)
    val promiseThen = NativeFunction(
      name = "then",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        val promise = getPromiseFrom(receiver, "then")
        val onFulfilled = args.lift(1).getOrElse(JSValue.Undefined)
        val onRejected = args.lift(2).getOrElse(JSValue.Undefined)
        val species =
          speciesConstructor(receiver, JSValue.Native(promiseConstructor))
        val capability = newPromiseCapability(species)
        performThen(promise, onFulfilled, onRejected, capability)
        capability.promise
    )

    // Promise.prototype.catch(onRejected) - Invoke(this, "then", ...)
    val promiseCatch = NativeFunction(
      name = "catch",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        val onRejected = args.lift(1).getOrElse(JSValue.Undefined)
        val boxed = BuiltinHelpers.toObject(receiver)
        val thenMethod = BuiltinHelpers.getPropertyWithGetter(boxed, "then")
        if !BuiltinHelpers.isCallable(thenMethod) then
          ctx.throwTypeError("then is not callable")
        BuiltinHelpers.callFunctionWithThis(
          thenMethod,
          boxed,
          Array(JSValue.Undefined, onRejected)
        )
    )

    // Promise.prototype.finally(onFinally)
    val promiseFinally = NativeFunction(
      name = "finally",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        val onFinally = args.lift(1).getOrElse(JSValue.Undefined)
        val species =
          speciesConstructor(receiver, JSValue.Native(promiseConstructor))

        def makeFinallyThunk(isReject: Boolean): JSValue = {
          val thunk = NativeFunction(
            name = "",
            length = 1,
            impl = (a, c) => {
              given JSContext = c
              val value = a.lastOption.getOrElse(JSValue.Undefined)
              val result = BuiltinHelpers.callFunctionWithThis(
                onFinally,
                JSValue.Undefined,
                Array.empty
              )
              val resolved = promiseResolveFor(species, result)
              val valueThunk = NativeFunction(
                name = "",
                length = 1,
                impl = (b, c2) => {
                  given JSContext = c2
                  if isReject then throw new JSException(value)
                  value
                }
              )
              val thenMethod =
                BuiltinHelpers.getPropertyWithGetter(resolved, "then")
              if isReject then
                BuiltinHelpers.callFunctionWithThis(
                  thenMethod,
                  resolved,
                  Array(JSValue.Undefined, JSValue.Native(valueThunk))
                )
              else
                BuiltinHelpers.callFunctionWithThis(
                  thenMethod,
                  resolved,
                  Array(JSValue.Native(valueThunk))
                )
            }
          )
          thunk.funcObj.setPrototype(ctx.functionPrototype)
          JSValue.Native(thunk)
        }

        val thenMethod = BuiltinHelpers.getPropertyWithGetter(receiver, "then")
        if !BuiltinHelpers.isCallable(thenMethod) then
          ctx.throwTypeError("then is not callable")
        if BuiltinHelpers.isCallable(onFinally) then
          BuiltinHelpers.callFunctionWithThis(
            thenMethod,
            receiver,
            Array(makeFinallyThunk(false), makeFinallyThunk(true))
          )
        else
          BuiltinHelpers.callFunctionWithThis(
            thenMethod,
            receiver,
            Array(onFinally, onFinally)
          )
    )

    // Promise.resolve(value) - static method
    val promiseResolveStatic = NativeFunction(
      name = "resolve",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val constructor = args.headOption.getOrElse(JSValue.Undefined)
        val value = args.lift(1).getOrElse(JSValue.Undefined)
        if !BuiltinHelpers.isObjectLikeValue(constructor) then
          ctx.throwTypeError("Promise.resolve called on non-object")
        promiseResolveFor(constructor, value)
    )

    // Promise.reject(reason) - static method
    val promiseRejectStatic = NativeFunction(
      name = "reject",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val constructor = args.headOption.getOrElse(JSValue.Undefined)
        val reason = args.lift(1).getOrElse(JSValue.Undefined)
        val capability = newPromiseCapability(constructor)
        BuiltinHelpers.callFunctionWithThis(
          capability.reject,
          JSValue.Undefined,
          Array(reason)
        )
        capability.promise
    )

    // Shared implementation of the static Promise combinators. Every
    // combinator creates its result through NewPromiseCapability(this), walks
    // the iterable with the iterator protocol, and rejects the capability when
    // iteration or `then` access fails (closing the iterator first).
    def makeElementFunction(
        impl: Array[JSValue] => JSValue
    )(using ctx: JSContext): JSValue = {
      val f = NativeFunction(
        name = "",
        length = 1,
        impl = (a, _) => impl(a)
      )
      f.funcObj.setPrototype(ctx.functionPrototype)
      JSValue.Native(f)
    }

    def lastElementArg(args: Array[JSValue]): JSValue =
      args.lastOption.getOrElse(JSValue.Undefined)

    def arrayOf(values: scala.collection.Seq[JSValue]): JSValue = {
      val arr = quickjs.objmodel.JSArray.empty()
      values.foreach(arr.push)
      JSValue.JSArrayVal(arr)
    }

    def settledRecord(
        status: String,
        key: String,
        value: JSValue
    ): JSValue = {
      val obj = JSObject(prototype = ctx.objectPrototype, extensible = true)
      obj.defineProperty(
        "status",
        JSValue.fromString(status),
        enumerable = true,
        writable = true,
        configurable = true
      )
      obj.defineProperty(
        key,
        value,
        enumerable = true,
        writable = true,
        configurable = true
      )
      JSValue.Object(obj)
    }

    def runCombinator(kind: Int, args: Array[JSValue])(using
        ctx: JSContext
    ): JSValue = {
      val constructor = args.headOption.getOrElse(JSValue.Undefined)
      val iterable = args.lift(1).getOrElse(JSValue.Undefined)
      if !isConstructor(constructor) then
        ctx.throwTypeError("Promise method called on a non-constructor")
      val capability = newPromiseCapability(constructor)
      var record: BuiltinHelpers.IteratorRecord = null
      def closeIterator(): Unit =
        BuiltinHelpers.iteratorCloseRecord(record)
      def resolveCapability(value: JSValue): Unit =
        BuiltinHelpers.callFunctionWithThis(
          capability.resolve,
          JSValue.Undefined,
          Array(value)
        )
      def rejectCapability(reason: JSValue): Unit =
        BuiltinHelpers.callFunctionWithThis(
          capability.reject,
          JSValue.Undefined,
          Array(reason)
        )

      try {
        val promiseResolve =
          BuiltinHelpers.getPropertyWithGetter(constructor, "resolve")
        if !BuiltinHelpers.isCallable(promiseResolve) then
          ctx.throwTypeError("Promise resolve is not callable")
        record = BuiltinHelpers.getIteratorRecord(iterable)

        val values = scala.collection.mutable.ArrayBuffer.empty[JSValue]
        val errors = scala.collection.mutable.ArrayBuffer.empty[JSValue]
        var remaining = 1
        var index = 0

        var step = BuiltinHelpers.iteratorStepValue(record)
        while step.isDefined do {
          val value = step.get
          val nextPromise = BuiltinHelpers.callFunctionWithThis(
            promiseResolve,
            constructor,
            Array(value)
          )
          val thenMethod =
            BuiltinHelpers.getPropertyWithGetter(nextPromise, "then")
          if !BuiltinHelpers.isCallable(thenMethod) then
            ctx.throwTypeError("Promise resolve result has no callable then")
          val myIndex = index
          index += 1
          kind match {
            case 0 => // Promise.all
              values += JSValue.Undefined
              remaining += 1
              var alreadyCalled = false
              val resolveElement = makeElementFunction { a =>
                if !alreadyCalled then {
                  alreadyCalled = true
                  values(myIndex) = lastElementArg(a)
                  remaining -= 1
                  if remaining == 0 then resolveCapability(arrayOf(values))
                }
                JSValue.Undefined
              }
              BuiltinHelpers.callFunctionWithThis(
                thenMethod,
                nextPromise,
                Array(resolveElement, capability.reject)
              )
            case 1 => // Promise.allSettled
              values += JSValue.Undefined
              remaining += 1
              var alreadyCalled = false
              def completeElement(record: JSValue): Unit = {
                values(myIndex) = record
                remaining -= 1
                if remaining == 0 then resolveCapability(arrayOf(values))
              }
              val resolveElement = makeElementFunction { a =>
                if !alreadyCalled then {
                  alreadyCalled = true
                  completeElement(
                    settledRecord("fulfilled", "value", lastElementArg(a))
                  )
                }
                JSValue.Undefined
              }
              val rejectElement = makeElementFunction { a =>
                if !alreadyCalled then {
                  alreadyCalled = true
                  completeElement(
                    settledRecord("rejected", "reason", lastElementArg(a))
                  )
                }
                JSValue.Undefined
              }
              BuiltinHelpers.callFunctionWithThis(
                thenMethod,
                nextPromise,
                Array(resolveElement, rejectElement)
              )
            case 2 => // Promise.any
              errors += JSValue.Undefined
              remaining += 1
              val resolveElement = makeElementFunction { a =>
                resolveCapability(lastElementArg(a))
                JSValue.Undefined
              }
              var rejectCalled = false
              val rejectElement = makeElementFunction { a =>
                if !rejectCalled then {
                  rejectCalled = true
                  errors(myIndex) = lastElementArg(a)
                  remaining -= 1
                  if remaining == 0 then
                    rejectCapability(makeAggregateError(arrayOf(errors)))
                }
                JSValue.Undefined
              }
              BuiltinHelpers.callFunctionWithThis(
                thenMethod,
                nextPromise,
                Array(resolveElement, rejectElement)
              )
            case _ => // Promise.race
              BuiltinHelpers.callFunctionWithThis(
                thenMethod,
                nextPromise,
                Array(capability.resolve, capability.reject)
              )
          }
          step = BuiltinHelpers.iteratorStepValue(record)
        }

        kind match {
          case 0 | 1 =>
            remaining -= 1
            if remaining == 0 then resolveCapability(arrayOf(values))
          case 2 =>
            if errors.isEmpty then
              rejectCapability(makeAggregateError(arrayOf(errors)))
            else {
              remaining -= 1
              if remaining == 0 then
                rejectCapability(makeAggregateError(arrayOf(errors)))
            }
          case _ => ()
        }

        capability.promise
      } catch {
        case error: JSException =>
          closeIterator()
          BuiltinHelpers.callFunctionWithThis(
            capability.reject,
            JSValue.Undefined,
            Array(error.getValue)
          )
          capability.promise
      }
    }

    val promiseAllStatic = NativeFunction(
      name = "all",
      impl = (args, ctx) =>
        given JSContext = ctx
        runCombinator(0, args)
    )

    val promiseAllSettledStatic = NativeFunction(
      name = "allSettled",
      impl = (args, ctx) =>
        given JSContext = ctx
        runCombinator(1, args)
    )

    val promiseAnyStatic = NativeFunction(
      name = "any",
      impl = (args, ctx) =>
        given JSContext = ctx
        runCombinator(2, args)
    )

    val promiseRaceStatic = NativeFunction(
      name = "race",
      impl = (args, ctx) =>
        given JSContext = ctx
        runCombinator(3, args)
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

    // Promise.withResolvers() - ES2024 static method
    val promiseWithResolversStatic = NativeFunction(
      name = "withResolvers",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val constructor = args.headOption.getOrElse(JSValue.Undefined)
        val capability = newPromiseCapability(constructor)
        val obj = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype)
        obj.defineProperty(
          "promise",
          capability.promise,
          enumerable = true,
          writable = true,
          configurable = true
        )
        obj.defineProperty(
          "resolve",
          capability.resolve,
          enumerable = true,
          writable = true,
          configurable = true
        )
        obj.defineProperty(
          "reject",
          capability.reject,
          enumerable = true,
          writable = true,
          configurable = true
        )
        JSValue.Object(obj)
    )

    promiseConstructor.funcObj.defineProperty(
      "resolve",
      JSValue.Native(promiseResolveStatic),
      enumerable = false
    )
    promiseConstructor.funcObj.defineProperty(
      "withResolvers",
      JSValue.Native(promiseWithResolversStatic),
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
