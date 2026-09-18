package quickjs.runtime.builtins

import quickjs.objmodel.{JSArray, JSObject}
import quickjs.runtime.{JSContext, JSException}
import quickjs.value.{JSValue, NativeFunction}

import scala.collection.mutable

/** ES2025 Iterator Helpers.
  *
  * Implements `%Iterator%`, the iterator helper methods on
  * `%IteratorPrototype%`, `%IteratorHelperPrototype%` (the prototype of the
  * lazy `map`/`filter`/`take`/`drop`/`flatMap` results) and
  * `%WrapForValidIteratorPrototype%` (returned by `Iterator.from` for
  * objects that are not already proper iterators).
  *
  * The lazy helpers are native state machines rather than JS generators so
  * that the exact spec behavior for `.return()` (close the underlying
  * iterator even before the first `next`), argument validation failures and
  * `IteratorClose` completion semantics is preserved.
  */
object IteratorHelpers {

  // =========================================================================
  // Shared primitives
  // =========================================================================

  private def isObjectLike(value: JSValue): Boolean = value match {
    case JSValue.Object(_) | JSValue.JSArrayVal(_) | _: JSValue.Function |
        JSValue.Native(_) =>
      true
    case _ => false
  }

  private def iterResult(value: JSValue, done: Boolean)(using
      ctx: JSContext
  ): JSValue = {
    val obj = JSObject(prototype = ctx.objectPrototype)
    obj.defineProperty(
      "value",
      value,
      enumerable = true,
      writable = true,
      configurable = true
    )
    obj.defineProperty(
      "done",
      JSValue.Bool(done),
      enumerable = true,
      writable = true,
      configurable = true
    )
    JSValue.Object(obj)
  }

  private def iteratorSymbolId(using ctx: JSContext): Int =
    wellKnownSymbol("iterator") match {
      case JSValue.Symbol(id) => id
      case _ => ctx.throwTypeError("Symbol.iterator is not available")
    }

  private def wellKnownSymbol(name: String)(using ctx: JSContext): JSValue =
    ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.get(name)
      case _ => JSValue.Undefined
    }

  private def getMethod(value: JSValue, key: String)(using
      ctx: JSContext
  ): JSValue = {
    val method = BuiltinHelpers.getPropertyWithGetter(value, key)
    if method == JSValue.Undefined || method == JSValue.Null then
      JSValue.Undefined
    else if BuiltinHelpers.isCallable(method) then method
    else ctx.throwTypeError(s"$key is not a function")
  }

  private def getSymbolMethod(value: JSValue, id: Int)(using
      ctx: JSContext
  ): JSValue = {
    val method = BuiltinHelpers.getSymbolPropertyWithGetter(value, id)
    if method == JSValue.Undefined || method == JSValue.Null then
      JSValue.Undefined
    else if BuiltinHelpers.isCallable(method) then method
    else ctx.throwTypeError("@@iterator is not a function")
  }

  /** Walk the prototype chain of an object-like value looking for `target`. */
  private def prototypeChainContains(value: JSValue, target: JSObject)(using
      ctx: JSContext
  ): Boolean = {
    var current: JSValue = value
    var guard = 0
    while guard < 10000 do {
      val protoObj = BuiltinHelpers.valuePrototype(current)
      if protoObj == null then return false
      if protoObj eq target then return true
      current = JSValue.Object(protoObj)
      guard += 1
    }
    false
  }

  // =========================================================================
  // Iterator records and stepping
  // =========================================================================

  private final class IteratorRecord(
      val iterator: JSValue,
      var nextMethod: JSValue,
      var done: Boolean
  )

  /** GetIteratorDirect(obj): read `next` once and remember it. */
  private def getIteratorDirect(value: JSValue)(using
      ctx: JSContext
  ): IteratorRecord = {
    val nextMethod = BuiltinHelpers.getPropertyWithGetter(value, "next")
    new IteratorRecord(value, nextMethod, done = false)
  }

  private def callIteratorNext(record: IteratorRecord)(using
      ctx: JSContext
  ): JSValue =
    BuiltinHelpers.callFunctionWithThis(
      record.nextMethod,
      record.iterator,
      Array.empty
    )

  /** IteratorStepValue: call `next`, validate the result object and extract
    * its value. Returns `None` when the iterator is exhausted.
    */
  private def iteratorStepValue(record: IteratorRecord)(using
      ctx: JSContext
  ): Option[JSValue] = {
    val result = callIteratorNext(record)
    if !isObjectLike(result) then
      ctx.throwTypeError("Iterator result is not an object")
    val done = BuiltinHelpers.getPropertyWithGetter(result, "done")
    if done.toBoolean then {
      record.done = true
      None
    } else Some(BuiltinHelpers.getPropertyWithGetter(result, "value"))
  }

  /** IteratorClose(record, throw completion): swallow close errors and
    * rethrow the original completion.
    */
  private def closeWithThrow(record: IteratorRecord, original: JSException)(
      using ctx: JSContext
  ): Nothing = {
    try {
      val ret = getMethod(record.iterator, "return")
      if ret != JSValue.Undefined then
        try
          BuiltinHelpers.callFunctionWithThis(ret, record.iterator, Array.empty)
        catch case _: JSException => ()
    } catch case _: JSException => ()
    throw original
  }

  /** IteratorClose(record, normal completion): close errors propagate and a
    * non-object return value is a TypeError.
    */
  private def closeWithReturn(record: IteratorRecord)(using
      ctx: JSContext
  ): Unit = {
    val ret = getMethod(record.iterator, "return")
    if ret != JSValue.Undefined then {
      val result =
        BuiltinHelpers.callFunctionWithThis(ret, record.iterator, Array.empty)
      if !isObjectLike(result) then
        ctx.throwTypeError("Iterator return result is not an object")
    }
  }

  /** Close `value` itself (used when argument validation fails before a
    * record exists), rethrowing the original error and swallowing close
    * errors.
    */
  private def closeValueWithThrow(value: JSValue, original: JSException)(using
      ctx: JSContext
  ): Nothing = {
    try {
      if isObjectLike(value) then {
        val ret = getMethod(value, "return")
        if ret != JSValue.Undefined then
          try
            BuiltinHelpers.callFunctionWithThis(
              ret,
              value,
              Array.empty
            )
          catch case _: JSException => ()
      }
    } catch case _: JSException => ()
    throw original
  }

  private def typeError(message: String)(using ctx: JSContext): JSException =
    new JSException(ctx.createError("TypeError", message))

  /** Validate `this` for a prototype method that takes a callback. The
    * callable check happens before `next` is read; on failure the iterator is
    * closed (IteratorClose with a throw completion).
    */
  private def requireCallbackReceiver(
      thisValue: JSValue,
      callback: JSValue,
      callbackName: String
  )(using ctx: JSContext): IteratorRecord = {
    if !isObjectLike(thisValue) then
      ctx.throwTypeError(s"Iterator.prototype method called on non-object")
    if !BuiltinHelpers.isCallable(callback) then
      closeValueWithThrow(
        thisValue,
        typeError(s"$callbackName is not a function")
      )
    getIteratorDirect(thisValue)
  }

  /** GetIteratorFlattenable(value, primitiveHandling). `Iterator.from`
    * accepts String primitives (using the primitive as the getter receiver),
    * while `flatMap` rejects every primitive.
    */
  private def getIteratorFlattenable(
      value: JSValue,
      allowStringPrimitive: Boolean = false
  )(using ctx: JSContext): IteratorRecord = {
    if !isObjectLike(value) then
      value match {
        case JSValue.JSStr(_) if allowStringPrimitive => ()
        case _ => ctx.throwTypeError("value is not an object")
      }
    val method = getSymbolMethod(value, iteratorSymbolId)
    val iterator = method match {
      case JSValue.Undefined => value
      case m =>
        val it = BuiltinHelpers.callFunctionWithThis(m, value, Array.empty)
        if !isObjectLike(it) then
          ctx.throwTypeError("iterator is not an object")
        it
    }
    getIteratorDirect(iterator)
  }

  // =========================================================================
  // Lazy helper state machines
  // =========================================================================

  private abstract class HelperState(val record: IteratorRecord) {
    var done: Boolean = false
    var executing: Boolean = false

    /** Produce the next value, or `None` when the helper is exhausted. */
    def step()(using ctx: JSContext): Option[JSValue]

    /** Close resources owned by this helper with a return completion. */
    def closeForReturn()(using ctx: JSContext): Unit =
      closeWithReturn(record)

    /** Close resources owned by this helper with a throw completion. */
    def closeForThrow(error: JSException)(using ctx: JSContext): Nothing =
      closeWithThrow(record, error)
  }

  private final class MapState(record: IteratorRecord, mapper: JSValue)
      extends HelperState(record) {
    private var counter = 0L

    def step()(using ctx: JSContext): Option[JSValue] =
      iteratorStepValue(record).map { value =>
        val mapped =
          try
            BuiltinHelpers.callFunctionWithThis(
              mapper,
              JSValue.Undefined,
              Array(value, JSValue.fromDouble(counter.toDouble))
            )
          catch {
            case e: JSException => closeForThrow(e)
          }
        counter += 1
        mapped
      }
  }

  private final class FilterState(
      record: IteratorRecord,
      predicate: JSValue
  ) extends HelperState(record) {
    private var counter = 0L

    def step()(using ctx: JSContext): Option[JSValue] = {
      while (true) {
        iteratorStepValue(record) match {
          case None => return None
          case Some(value) =>
            val selected =
              try
                BuiltinHelpers.callFunctionWithThis(
                  predicate,
                  JSValue.Undefined,
                  Array(value, JSValue.fromDouble(counter.toDouble))
                )
              catch {
                case e: JSException => closeForThrow(e)
              }
            counter += 1
            if selected.toBoolean then return Some(value)
        }
      }
      None
    }
  }

  private final class TakeState(record: IteratorRecord, private var remaining: Double)
      extends HelperState(record) {
    def step()(using ctx: JSContext): Option[JSValue] = {
      if remaining == 0 then {
        closeWithReturn(record)
        None
      } else {
        if remaining != Double.PositiveInfinity then remaining -= 1
        iteratorStepValue(record)
      }
    }
  }

  private final class DropState(record: IteratorRecord, private var remaining: Double)
      extends HelperState(record) {
    def step()(using ctx: JSContext): Option[JSValue] = {
      while remaining > 0 do {
        remaining -= 1
        iteratorStepValue(record) match {
          case None    => return None
          case Some(_) => ()
        }
      }
      iteratorStepValue(record)
    }
  }

  private final class FlatMapState(
      record: IteratorRecord,
      mapper: JSValue
  ) extends HelperState(record) {
    private var counter = 0L
    private var inner: IteratorRecord | Null = null

    def step()(using ctx: JSContext): Option[JSValue] = {
      while (true) {
        if inner != null then {
          iteratorStepValue(inner.nn) match {
            case Some(value) => return Some(value)
            case None        => inner = null
          }
        }
        iteratorStepValue(record) match {
          case None => return None
          case Some(outerValue) =>
            val mapped =
              try
                BuiltinHelpers.callFunctionWithThis(
                  mapper,
                  JSValue.Undefined,
                  Array(outerValue, JSValue.fromDouble(counter.toDouble))
                )
              catch {
                case e: JSException => closeForThrow(e)
              }
            counter += 1
            inner =
              try getIteratorFlattenable(mapped)
              catch {
                case e: JSException => closeForThrow(e)
              }
        }
      }
      None
    }

    override def closeForReturn()(using ctx: JSContext): Unit = {
      if inner != null then {
        closeWithReturn(inner.nn)
        inner = null
      }
      closeWithReturn(record)
    }
  }

  // =========================================================================
  // Initialization
  // =========================================================================

  def initialize(ctx: JSContext): Unit = {
    given JSContext = ctx
    val iteratorPrototype = ctx.iteratorPrototype

    // ---- helper prototypes ------------------------------------------------

    val wrapPrototype = JSObject(prototype = iteratorPrototype, extensible = true)
    val helperPrototype = JSObject(prototype = iteratorPrototype, extensible = true)

    def functionProto(): JSObject = ctx.functionPrototype

    def defineMethod(
        target: JSObject,
        name: String,
        length: Int
    )(impl: (JSValue, Array[JSValue], JSContext) => JSValue): Unit = {
      val native = NativeFunction(
        name = name,
        length = length,
        impl = (args, callCtx) =>
          impl(
            args.headOption.getOrElse(JSValue.Undefined),
            args.drop(1),
            callCtx
          )
      )
      native.funcObj.setPrototype(functionProto())
      target.defineProperty(
        name,
        JSValue.Native(native),
        enumerable = false,
        writable = true,
        configurable = true
      )
    }

    // ---- %WrapForValidIteratorPrototype% ---------------------------------

    def wrapperRecord(
        thisValue: JSValue
    )(using callCtx: JSContext): IteratorRecord =
      thisValue match {
        case JSValue.Object(obj) =>
          obj.getOwnProperty("__iterated") match {
            case Some(JSValue.Native(record: IteratorRecord)) => record
            case _ =>
              callCtx.throwTypeError(
                "method called on incompatible receiver"
              )
          }
        case _ =>
          callCtx.throwTypeError("method called on incompatible receiver")
      }

    defineMethod(wrapPrototype, "next", 0) { (thisValue, _, callCtx) =>
      given JSContext = callCtx
      val record = wrapperRecord(thisValue)
      callIteratorNext(record)
    }

    defineMethod(wrapPrototype, "return", 0) { (thisValue, _, callCtx) =>
      given JSContext = callCtx
      val record = wrapperRecord(thisValue)
      getMethod(record.iterator, "return") match {
        case JSValue.Undefined => iterResult(JSValue.Undefined, done = true)
        case method =>
          BuiltinHelpers.callFunctionWithThis(
            method,
            record.iterator,
            Array.empty
          )
      }
    }

    // ---- %IteratorHelperPrototype% ---------------------------------------

    def helperState(thisValue: JSValue)(using
        callCtx: JSContext
    ): HelperState =
      thisValue match {
        case JSValue.Object(obj) =>
          obj.getOwnProperty("__iteratorHelper") match {
            case Some(JSValue.Native(state: HelperState)) => state
            case _ =>
              callCtx.throwTypeError(
                "method called on incompatible receiver"
              )
          }
        case _ =>
          callCtx.throwTypeError("method called on incompatible receiver")
      }

    defineMethod(helperPrototype, "next", 0) { (thisValue, _, callCtx) =>
      given JSContext = callCtx
      val state = helperState(thisValue)
      if state.done then iterResult(JSValue.Undefined, done = true)
      else if state.executing then
        ctx.throwTypeError("Iterator Helper is already running")
      else {
        state.executing = true
        val value =
          try state.step()
          catch {
            case e: JSException =>
              state.done = true
              throw e
          } finally state.executing = false
        value match {
          case None =>
            state.done = true
            iterResult(JSValue.Undefined, done = true)
          case Some(v) => iterResult(v, done = false)
        }
      }
    }

    defineMethod(helperPrototype, "return", 0) { (thisValue, _, callCtx) =>
      given JSContext = callCtx
      val state = helperState(thisValue)
      if !state.done then {
        state.done = true
        state.closeForReturn()
      }
      iterResult(JSValue.Undefined, done = true)
    }

    wellKnownSymbol("toStringTag") match {
      case JSValue.Symbol(id) =>
        helperPrototype.defineSymbolProperty(
          id,
          JSValue.fromString("Iterator Helper"),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }

    def createHelper(state: HelperState): JSValue = {
      val obj = JSObject(prototype = helperPrototype, extensible = true)
      obj.defineProperty(
        "__iteratorHelper",
        JSValue.Native(state),
        enumerable = false,
        writable = false,
        configurable = false
      )
      JSValue.Object(obj)
    }

    // ---- helper methods ---------------------------------------------------

    defineMethod(iteratorPrototype, "map", 1) { (thisValue, args, callCtx) =>
      given JSContext = callCtx
      val mapper = args.headOption.getOrElse(JSValue.Undefined)
      val record = requireCallbackReceiver(thisValue, mapper, "mapper")
      createHelper(new MapState(record, mapper))
    }

    defineMethod(iteratorPrototype, "filter", 1) { (thisValue, args, callCtx) =>
      given JSContext = callCtx
      val predicate = args.headOption.getOrElse(JSValue.Undefined)
      val record = requireCallbackReceiver(thisValue, predicate, "predicate")
      createHelper(new FilterState(record, predicate))
    }

    defineMethod(iteratorPrototype, "take", 1) { (thisValue, args, callCtx) =>
      given JSContext = callCtx
      val limit = args.headOption.getOrElse(JSValue.Undefined)
      if !isObjectLike(thisValue) then
        ctx.throwTypeError("Iterator.prototype.take called on non-object")
      val numLimit =
        try BuiltinHelpers.toNumber(limit)
        catch {
          case e: JSException => closeValueWithThrow(thisValue, e)
        }
      if numLimit.isNaN then
        closeValueWithThrow(
          thisValue,
          new JSException(
            ctx.createError("RangeError", "limit must not be NaN")
          )
        )
      val integerLimit = toIntegerOrInfinity(numLimit)
      if integerLimit < 0 then
        closeValueWithThrow(
          thisValue,
          new JSException(
            ctx.createError("RangeError", "limit must not be negative")
          )
        )
      val record = getIteratorDirect(thisValue)
      createHelper(new TakeState(record, integerLimit))
    }

    defineMethod(iteratorPrototype, "drop", 1) { (thisValue, args, callCtx) =>
      given JSContext = callCtx
      val limit = args.headOption.getOrElse(JSValue.Undefined)
      if !isObjectLike(thisValue) then
        ctx.throwTypeError("Iterator.prototype.drop called on non-object")
      val numLimit =
        try BuiltinHelpers.toNumber(limit)
        catch {
          case e: JSException => closeValueWithThrow(thisValue, e)
        }
      if numLimit.isNaN then
        closeValueWithThrow(
          thisValue,
          new JSException(
            ctx.createError("RangeError", "limit must not be NaN")
          )
        )
      val integerLimit = toIntegerOrInfinity(numLimit)
      if integerLimit < 0 then
        closeValueWithThrow(
          thisValue,
          new JSException(
            ctx.createError("RangeError", "limit must not be negative")
          )
        )
      val record = getIteratorDirect(thisValue)
      createHelper(new DropState(record, integerLimit))
    }

    defineMethod(iteratorPrototype, "flatMap", 1) { (thisValue, args, callCtx) =>
      given JSContext = callCtx
      val mapper = args.headOption.getOrElse(JSValue.Undefined)
      val record = requireCallbackReceiver(thisValue, mapper, "mapper")
      createHelper(new FlatMapState(record, mapper))
    }

    // ---- eager methods ----------------------------------------------------

    def requireRecord(
        thisValue: JSValue,
        callback: Option[JSValue],
        callbackName: String
    )(using callCtx: JSContext): IteratorRecord =
      callback match {
        case Some(cb) => requireCallbackReceiver(thisValue, cb, callbackName)
        case None =>
          if !isObjectLike(thisValue) then
            callCtx.throwTypeError(
              "Iterator.prototype method called on non-object"
            )
          getIteratorDirect(thisValue)
      }

    defineMethod(iteratorPrototype, "toArray", 0) { (thisValue, _, callCtx) =>
      given JSContext = callCtx
      val record = requireRecord(thisValue, None, "")
      val array = JSArray.empty()
      var continue = true
      while continue do {
        iteratorStepValue(record) match {
          case None      => continue = false
          case Some(v)   => array.push(v)
        }
      }
      JSValue.JSArrayVal(array)
    }

    defineMethod(iteratorPrototype, "forEach", 1) { (thisValue, args, callCtx) =>
      given JSContext = callCtx
      val fn = args.headOption.getOrElse(JSValue.Undefined)
      val record = requireRecord(thisValue, Some(fn), "fn")
      var counter = 0L
      var continue = true
      while continue do {
        iteratorStepValue(record) match {
          case None => continue = false
          case Some(value) =>
            try
              BuiltinHelpers.callFunctionWithThis(
                fn,
                JSValue.Undefined,
                Array(value, JSValue.fromDouble(counter.toDouble))
              )
            catch {
              case e: JSException => closeWithThrow(record, e)
            }
            counter += 1
        }
      }
      JSValue.Undefined
    }

    def predicateMethod(name: String, onTrue: Boolean): Unit =
      defineMethod(iteratorPrototype, name, 1) { (thisValue, args, callCtx) =>
        given JSContext = callCtx
        val predicate = args.headOption.getOrElse(JSValue.Undefined)
        val record = requireRecord(thisValue, Some(predicate), "predicate")
        var counter = 0L
        var result: JSValue =
          if onTrue then JSValue.Bool(false) else JSValue.Bool(true)
        var continue = true
        while continue do {
          iteratorStepValue(record) match {
            case None => continue = false
            case Some(value) =>
              val selected =
                try
                  BuiltinHelpers.callFunctionWithThis(
                    predicate,
                    JSValue.Undefined,
                    Array(value, JSValue.fromDouble(counter.toDouble))
                  )
                catch {
                  case e: JSException => closeWithThrow(record, e)
                }
              counter += 1
              if selected.toBoolean == onTrue then {
                closeWithReturn(record)
                result =
                  if onTrue then JSValue.Bool(true) else JSValue.Bool(false)
                continue = false
              }
          }
        }
        result
      }

    predicateMethod("some", onTrue = true)
    predicateMethod("every", onTrue = false)

    defineMethod(iteratorPrototype, "find", 1) { (thisValue, args, callCtx) =>
      given JSContext = callCtx
      val predicate = args.headOption.getOrElse(JSValue.Undefined)
      val record = requireRecord(thisValue, Some(predicate), "predicate")
      var counter = 0L
      var result: JSValue = JSValue.Undefined
      var continue = true
      while continue do {
        iteratorStepValue(record) match {
          case None => continue = false
          case Some(value) =>
            val selected =
              try
                BuiltinHelpers.callFunctionWithThis(
                  predicate,
                  JSValue.Undefined,
                  Array(value, JSValue.fromDouble(counter.toDouble))
                )
              catch {
                case e: JSException => closeWithThrow(record, e)
              }
            counter += 1
            if selected.toBoolean then {
              closeWithReturn(record)
              result = value
              continue = false
            }
        }
      }
      result
    }

    defineMethod(iteratorPrototype, "reduce", 1) { (thisValue, args, callCtx) =>
      given JSContext = callCtx
      val reducer = args.headOption.getOrElse(JSValue.Undefined)
      val hasInitial = args.length > 1
      val record = requireRecord(thisValue, Some(reducer), "reducer")
      var counter = 0L
      var accumulator: JSValue = JSValue.Undefined
      if hasInitial then accumulator = args(1)
      else {
        iteratorStepValue(record) match {
          case None =>
            ctx.throwTypeError(
              "Reduce of empty iterator with no initial value"
            )
          case Some(first) =>
            accumulator = first
            counter = 1
        }
      }
      var continue = true
      while continue do {
        iteratorStepValue(record) match {
          case None => continue = false
          case Some(value) =>
            accumulator =
              try
                BuiltinHelpers.callFunctionWithThis(
                  reducer,
                  JSValue.Undefined,
                  Array(accumulator, value, JSValue.fromDouble(counter.toDouble))
                )
              catch {
                case e: JSException => closeWithThrow(record, e)
              }
            counter += 1
        }
      }
      accumulator
    }

    // ---- Iterator.from ----------------------------------------------------

    val iteratorFrom = NativeFunction(
      name = "from",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        // Native calls receive the receiver as the first argument; static
        // `Iterator.from` ignores it. Strings are accepted as primitives and
        // are not boxed before the `@@iterator` lookup (the getter observes
        // the primitive receiver).
        val value = args.lift(1).getOrElse(JSValue.Undefined)
        if !isObjectLike(value) then
          value match {
            case JSValue.JSStr(_) => ()
            case _ =>
              ctx.throwTypeError("Iterator.from called on non-object")
          }
        val record = getIteratorFlattenable(value, allowStringPrimitive = true)
        if prototypeChainContains(record.iterator, iteratorPrototype) then
          record.iterator
        else {
          val wrapper = JSObject(prototype = wrapPrototype, extensible = true)
          wrapper.defineProperty(
            "__iterated",
            JSValue.Native(record),
            enumerable = false,
            writable = false,
            configurable = false
          )
          JSValue.Object(wrapper)
        }
      }
    )
    iteratorFrom.funcObj.setPrototype(functionProto())

    // ---- %Iterator% constructor ------------------------------------------

    val iteratorConstructor = quickjs.value.NativeConstructor(
      name = "Iterator",
      callImpl = (_, callCtx) =>
        callCtx.throwTypeError("Iterator is not callable"),
      constructImpl = (_, callCtx) =>
        callCtx.throwTypeError("Iterator is not a constructor"),
      prototype = iteratorPrototype,
      length = 0,
      superInitImpl = Some((thisValue, _, _) => thisValue)
    )
    BuiltinHelpers.initConstructor(iteratorConstructor, length = 0)
    iteratorConstructor.funcObj.defineProperty(
      "from",
      JSValue.Native(iteratorFrom),
      enumerable = false,
      writable = true,
      configurable = true
    )

    // `%IteratorPrototype%` has accessor `constructor` / `@@toStringTag`
    // properties whose setters ignore the prototype property.
    installIgnoringAccessor(
      iteratorPrototype,
      "constructor",
      iteratorPrototype,
      symbolId = None
    )(JSValue.Native(iteratorConstructor))
    wellKnownSymbol("toStringTag") match {
      case JSValue.Symbol(id) =>
        installIgnoringAccessor(
          iteratorPrototype,
          "[Symbol.toStringTag]",
          iteratorPrototype,
          symbolId = Some(id)
        )(JSValue.fromString("Iterator"))
      case _ => ()
    }

    ctx.global.defineProperty(
      "Iterator",
      JSValue.Native(iteratorConstructor),
      enumerable = false
    )
  }

  /** `Set`-like helper for `SetterThatIgnoresPrototypeProperties`. */
  private def installIgnoringAccessor(
      target: JSObject,
      displayName: String,
      homeObject: JSObject,
      symbolId: Option[Int]
  )(value: => JSValue)(using ctx: JSContext): Unit = {
    val getter = NativeFunction(
      name = s"get $displayName",
      length = 0,
      impl = (_, _) => value
    )
    val setter = NativeFunction(
      name = s"set $displayName",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        val newValue = args.lift(1).getOrElse(JSValue.Undefined)
        setterThatIgnoresPrototypeProperties(
          receiver,
          homeObject,
          displayName,
          newValue,
          symbolId
        )
      }
    )
    getter.funcObj.setPrototype(ctx.functionPrototype)
    setter.funcObj.setPrototype(ctx.functionPrototype)
    symbolId match {
      case Some(id) =>
        target.defineSymbolAccessorPropertyDetailed(
          id,
          Some(JSValue.Native(getter)),
          Some(JSValue.Native(setter)),
          hasGetter = true,
          hasSetter = true,
          enumerable = Some(false),
          configurable = Some(true)
        )
      case None =>
        target.defineAccessorPropertyDetailed(
          displayName,
          Some(JSValue.Native(getter)),
          Some(JSValue.Native(setter)),
          hasGetter = true,
          hasSetter = true,
          enumerable = Some(false),
          configurable = Some(true)
        )
    }
  }

  private def setterThatIgnoresPrototypeProperties(
      receiver: JSValue,
      homeObject: JSObject,
      displayName: String,
      value: JSValue,
      symbolId: Option[Int]
  )(using ctx: JSContext): JSValue = {
    val obj = BuiltinHelpers.extractJSObject(receiver) match {
      case Some(o) => o
      case None =>
        ctx.throwTypeError(
          s"Cannot set property $displayName on non-object"
        )
    }
    if obj eq homeObject then
      ctx.throwTypeError(s"Cannot assign to read only property $displayName")
    val hasOwn = symbolId match {
      case Some(id) => obj.getOwnSymbolProperty(id).isDefined
      case None     => obj.getOwnProperty(displayName).isDefined
    }
    if !hasOwn && !obj.isExtensible then
      ctx.throwTypeError(
        s"Cannot create property $displayName on non-extensible object"
      )
    symbolId match {
      case Some(id) =>
        obj.defineSymbolProperty(
          id,
          value,
          enumerable = true,
          writable = true,
          configurable = true
        )
      case None =>
        obj.defineProperty(
          displayName,
          value,
          enumerable = true,
          writable = true,
          configurable = true
        )
    }
    JSValue.Undefined
  }

  /** ES ToIntegerOrInfinity over an already-converted Number. */
  private def toIntegerOrInfinity(value: Double): Double =
    if value.isNaN || value == 0.0 then 0.0
    else if value.isInfinite then value
    else if value > 0 then Math.floor(value)
    else Math.ceil(value)
}
