package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers.{
  initConstructor,
  callFunctionValue,
  callFunctionWithThis,
  nativeArgs,
  functionToBytecode,
  extractJSObject,
  getPropertyWithGetter,
  toPrimitive,
  toNumber,
  toIntegerOrInfinity,
  isCallable
}
import scala.util.Sorting

/** Array built-in constructor and prototype methods. */
object ArrayBuiltins {
  import quickjs.objmodel.{JSObject, JSArray}

  // --- Helpers ---

  /** Extract the underlying JSArray from args(0) (the `this` value). Throws
    * RuntimeException on mismatch (existing behavior, should be TypeError).
    */
  private def thisArray(args: Array[JSValue], method: String): JSArray = {
    if args.isEmpty then
      throw new RuntimeException(s"Array.prototype.$method called on non-array")
    args(0) match {
      case JSValue.JSArrayVal(arrVal) => arrVal
      case _                          =>
        throw new RuntimeException(
          s"Array.prototype.$method called on non-array"
        )
    }
  }

  /** ECMAScript Array.prototype methods are intentionally generic.  Keep the
    * receiver as a JSValue so callbacks observe the original value, while the
    * helpers below implement LengthOfArrayLike, HasProperty, and Get.
    */
  private def arrayLikeReceiver(
      args: Array[JSValue],
      method: String
  )(using ctx: JSContext): JSValue =
    if args.isEmpty || args(0) == JSValue.Undefined || args(0) == JSValue.Null then
      ctx.throwTypeError(s"Array.prototype.$method called on null or undefined")
    else BuiltinHelpers.toObject(args(0))

  private[builtins] def arrayLikeLengthLong(value: JSValue)(using ctx: JSContext): Long = {
    val lengthValue = value match {
      case JSValue.JSArrayVal(arr) => arr.getLengthValue
      case _ => getPropertyWithGetter(value, "length")
    }
    val length = toIntegerOrInfinity(lengthValue)
    if length <= 0 || length.isNaN then 0L
    else if length >= 9007199254740991.0 then 9007199254740991L
    else length.toLong
  }

  private def arrayLikeSet(
      value: JSValue,
      index: Long,
      element: JSValue
  )(using ctx: JSContext): Unit = value match {
    case JSValue.JSArrayVal(arr) =>
      if index > 4294967294L then {
        if !arr.defineNamedDataProperty(
            index.toString,
            Some(element),
            enumerable = Some(true),
            writable = Some(true),
            configurable = Some(true)
          )
        then ctx.throwTypeError(s"Cannot set property '$index'")
      } else {
        arr.getIndexAttributes(index) match {
          case Some(attrs) if attrs.setter.isDefined =>
            callFunctionWithThis(attrs.setter.get, value, Array(element))
          case Some(attrs) if attrs.isAccessor || attrs.getter.isDefined =>
            ctx.throwTypeError(s"Cannot set property '$index'")
          case Some(attrs) if !attrs.writable =>
            ctx.throwTypeError(s"Cannot set property '$index'")
          case Some(_) => arr.set(index, element)
          case None =>
            val inherited = arr.getPrototypeOverride match {
              case Some(JSValue.Object(proto)) =>
                proto.getPropertyDescriptorWithOwner(index.toString)
              case Some(JSValue.Null) => None
              case _ => ctx.arrayPrototype.getPropertyDescriptorWithOwner(index.toString)
            }
            inherited match {
              case Some((_, _, attrs)) if attrs.setter.isDefined =>
                callFunctionWithThis(attrs.setter.get, value, Array(element))
              case Some((_, _, attrs))
                  if attrs.isAccessor || attrs.getter.isDefined =>
                ctx.throwTypeError(s"Cannot set property '$index'")
              case Some((_, _, attrs)) if !attrs.writable =>
                ctx.throwTypeError(s"Cannot set property '$index'")
              case _ if !arr.isExtensible =>
                ctx.throwTypeError(s"Cannot add property '$index'")
              case _ => arr.set(index, element)
            }
        }
      }
    case _ =>
      extractJSObject(value) match {
        case Some(obj) =>
          obj.getPropertyDescriptorWithOwner(index.toString) match {
            case Some((_, _, attrs)) if attrs.setter.isDefined =>
              callFunctionWithThis(attrs.setter.get, value, Array(element))
            case Some((_, _, attrs))
                if attrs.isAccessor || attrs.getter.isDefined || !attrs.writable =>
              ctx.throwTypeError(s"Cannot set property '$index'")
            case _ =>
              Interpreter().setPropertyValue(
                obj,
                value,
                index.toString,
                element,
                Nil,
                quickjs.tracing.TraceRecorder.Noop,
                isStrict = true
              )
          }
        case None => ctx.throwTypeError("Cannot set property on non-object")
      }
  }

  private def arrayLikeSetLength(value: JSValue, length: Long)(using
      ctx: JSContext
  ): Unit = value match {
    case JSValue.JSArrayVal(arr) =>
      if length > 4294967295L then ctx.throwRangeError("Invalid array length")
      if !arr.isLengthWritable || !arr.setLength(length) then
        ctx.throwTypeError("Cannot set array length")
    case _ => extractJSObject(value) match {
      case Some(obj) =>
        obj.getPropertyDescriptorWithOwner("length") match {
          case Some((_, _, attrs)) if attrs.setter.isDefined =>
            callFunctionWithThis(
              attrs.setter.get,
              value,
              Array(JSValue.fromDouble(length.toDouble))
            )
          case Some((_, _, attrs))
              if attrs.isAccessor || attrs.getter.isDefined || !attrs.writable =>
            ctx.throwTypeError("Cannot set length")
          case _ =>
            Interpreter().setPropertyValue(
              obj,
              value,
              "length",
              JSValue.fromDouble(length.toDouble),
              Nil,
              quickjs.tracing.TraceRecorder.Noop,
              isStrict = true
            )
        }
      case None => ctx.throwTypeError("Cannot set length on non-object")
    }
  }

  private def arrayLikeHasLong(value: JSValue, index: Long)(using
      ctx: JSContext
  ): Boolean = value match {
    case JSValue.JSArrayVal(arr) =>
      (if index > 4294967294L then arr.getOwnProperty(index.toString).isDefined
       else arr.hasIndex(index)) || (arr.getPrototypeOverride match {
        case Some(JSValue.Null) => false
        case Some(proto) => arrayLikeHasLong(proto, index)
        case None => ctx.arrayPrototype.hasProperty(index.toString)
      })
    case _ => extractJSObject(value) match {
      case Some(obj) =>
        TypedArrayBuiltins.indexedElement(obj, index) match {
          case Some(_) => true
          case None => (obj.getOwnProperty("__proxy_target"), obj.getOwnProperty("__proxy_handler")) match {
          case (Some(target), Some(JSValue.Object(handler))) =>
            handler.get("has")(using ctx) match {
              case JSValue.Undefined => arrayLikeHasLong(target, index)
              case trap =>
                callFunctionWithThis(
                  trap,
                  JSValue.Object(handler),
                  Array(target, JSValue.fromString(index.toString))
                ).toBoolean
            }
          case _ => obj.hasProperty(index.toString)
        }
        }
      case None => false
    }
  }

  private[builtins] def arrayLikeGetLong(value: JSValue, index: Long)(using
      ctx: JSContext
  ): JSValue = value match {
    case JSValue.JSArrayVal(arr) if index > 4294967294L =>
      getPropertyWithGetter(value, index.toString)
    case JSValue.JSArrayVal(arr) if arr.hasIndex(index) =>
      arr.getIndexAttributes(index).flatMap(_.getter) match {
        case Some(getter) => callFunctionWithThis(getter, value, Array.empty)
        case None         => arr.get(index)
      }
    case JSValue.JSArrayVal(_) =>
      getPropertyWithGetter(value, index.toString)
    case JSValue.Object(obj) =>
      TypedArrayBuiltins.indexedElement(obj, index)
        .getOrElse(getPropertyWithGetter(value, index.toString))
    case _ => getPropertyWithGetter(value, index.toString)
  }

  private def arrayLikeDelete(value: JSValue, index: Long)(using
      ctx: JSContext
  ): Unit = value match {
    case JSValue.JSArrayVal(arr) =>
      val deleted =
        if index > 4294967294L then arr.deleteProperty(index.toString)
        else arr.deleteIndex(index)
      if !deleted then
        ctx.throwTypeError(s"Cannot delete property '$index'")
    case _ => extractJSObject(value) match {
      case Some(obj) =>
        (obj.getOwnProperty("__proxy_target"), obj.getOwnProperty("__proxy_handler")) match {
          case (Some(target), Some(JSValue.Object(handler))) =>
            val deleted = handler.get("deleteProperty")(using ctx) match {
              case JSValue.Undefined =>
                arrayLikeDelete(target, index)
                true
              case trap =>
                callFunctionWithThis(
                  trap,
                  JSValue.Object(handler),
                  Array(target, JSValue.fromString(index.toString))
                ).toBoolean
            }
            if !deleted then ctx.throwTypeError(s"Cannot delete property '$index'")
          case _ =>
            if !obj.deleteProperty(index.toString) then
              ctx.throwTypeError(s"Cannot delete property '$index'")
        }
      case None => ctx.throwTypeError("Cannot delete property on non-object")
    }
  }

  private def arrayLikeLength(value: JSValue)(using ctx: JSContext): Int =
    value match {
      case JSValue.JSArrayVal(arr) => arr.getLength
      case JSValue.JSStr(s)        => s.length
      case _                       =>
        val number = extractJSObject(value)
          .map(_ => toNumber(getPropertyWithGetter(value, "length")))
          .getOrElse(0.0)
        if number.isNaN || number <= 0 then 0
        else if number >= Int.MaxValue then Int.MaxValue
        else math.floor(number).toInt
    }

  private def arrayLikeHas(value: JSValue, index: Int)(using ctx: JSContext): Boolean =
    value match {
      case JSValue.JSArrayVal(arr) =>
        arr.hasIndex(index) || ctx.arrayPrototype.hasProperty(index.toString)
      case JSValue.JSStr(s) => index >= 0 && index < s.length
      case _ => extractJSObject(value).exists(_.hasProperty(index.toString))
    }

  private def arrayLikeGet(value: JSValue, index: Int)(using ctx: JSContext): JSValue =
    value match {
      case JSValue.JSArrayVal(arr) if arr.hasIndex(index) =>
        arr.getIndexAttributes(index).flatMap(_.getter) match {
          case Some(getter) => callFunctionWithThis(getter, value, Array.empty)
          case None         => arr.get(index)
        }
      case JSValue.JSArrayVal(_) =>
        ctx.arrayPrototype.getPropertyDescriptorWithOwner(index.toString) match {
          case Some((_, _, attrs)) if attrs.getter.isDefined =>
            callFunctionWithThis(attrs.getter.get, value, Array.empty)
          case Some((_, stored, _)) => stored
          case None                 => JSValue.Undefined
        }
      case JSValue.JSStr(s) if index >= 0 && index < s.length =>
        JSValue.JSStr(s.substring(index, index + 1))
      case _ =>
        extractJSObject(value) match {
          case Some(obj) =>
            obj.getPropertyDescriptorWithOwner(index.toString) match {
              case Some((_, _, attrs)) if attrs.getter.isDefined =>
                callFunctionWithThis(attrs.getter.get, value, Array.empty)
              case Some((_, stored, _)) => stored
              case None                 =>
                // An array-valued [[Prototype]] contributes its indexes.
                obj.getPrototypeValue match {
                  case JSValue.JSArrayVal(_) =>
                    arrayLikeGetLong(value, index.toLong)
                  case _ => JSValue.Undefined
                }
            }
          case None => JSValue.Undefined
        }
    }

  private def requireCallback(value: JSValue)(using ctx: JSContext): JSValue =
    if !isCallable(value) then ctx.throwTypeError("Callback is not a function")
    else value

  /** Native builtin loops must cooperate with the runner/embedding host's
    * thread interruption just like the bytecode dispatch loop does.
    */
  private def checkInterrupted(iteration: Int): Unit =
    if (iteration & 1023) == 0 && Thread.currentThread().isInterrupted then
      throw new InterruptedException("JavaScript execution interrupted")

  /** Iterate an array calling a callback(element, index, array) -> JSValue.
    * Returns a new array with callback results (like map).
    */
  private def iterateMap(
      arr: JSArray,
      callback: JSValue,
      thisArg: JSValue,
      ctx: JSContext
  ): JSArray = {
    val result = JSArray.empty()
    var i = 0
    given JSContext = ctx
    while i < arr.getLength do {
      result.push(
        callFunctionWithThis(
          callback,
          thisArg,
          Array(arr.get(i), JSValue.fromInt(i), JSValue.JSArrayVal(arr))
        )
      )
      i += 1
    }
    result
  }

  /** Iterate testing each element with callback(element, index, array). Returns
    * index of first true, or -1.
    */
  private def iterateFind(
      arr: JSArray,
      callback: JSValue,
      thisArg: JSValue,
      ctx: JSContext
  ): Int = {
    var i = 0
    given JSContext = ctx
    while i < arr.getLength do {
      if callFunctionWithThis(
          callback,
          thisArg,
          Array(arr.get(i), JSValue.fromInt(i), JSValue.JSArrayVal(arr))
        ).toBoolean
      then return i
      i += 1
    }
    -1
  }

  /** Execute callback(element, index, array) for each element (returning
    * nothing).
    */
  private def iterateForEach(
      arr: JSArray,
      callback: JSValue,
      thisArg: JSValue,
      ctx: JSContext
  ): Unit = {
    var i = 0
    given JSContext = ctx
    while i < arr.getLength do {
      callFunctionWithThis(
        callback,
        thisArg,
        Array(arr.get(i), JSValue.fromInt(i), JSValue.JSArrayVal(arr))
      )
      i += 1
    }
  }

  /** Clamp an index to [0, len], with negative values counting from end. */
  private def clampIndex(raw: Int, len: Int): Int =
    if raw < 0 then math.max(len + raw, 0) else math.min(raw, len)

  private def relativeIndex(value: JSValue, len: Int)(using ctx: JSContext): Int = {
    val raw = toIntegerOrInfinity(value)
    if raw == Double.NegativeInfinity then 0
    else if raw < 0 then math.max(len.toLong + raw.toLong, 0L).toInt
    else if raw == Double.PositiveInfinity then len
    else math.min(raw.toLong, len.toLong).toInt
  }

  private def relativeIndexLong(value: JSValue, len: Long)(using
      ctx: JSContext
  ): Long = {
    val raw = toIntegerOrInfinity(value)
    if raw == Double.NegativeInfinity then 0L
    else if raw < 0 then math.max(len + raw.toLong, 0L)
    else if raw == Double.PositiveInfinity then len
    else math.min(raw.toLong, len)
  }

  private def fromIndex(value: JSValue, len: Int)(using ctx: JSContext): Int = {
    val raw = toIntegerOrInfinity(value)
    if raw == Double.PositiveInfinity then len
    else if raw == Double.NegativeInfinity then 0
    else if raw >= 0 then math.min(raw.toLong, len.toLong).toInt
    else math.max(len.toLong + raw.toLong, 0L).toInt
  }

  private def getWellKnownSymbol(name: String)(using ctx: JSContext): JSValue =
    ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.get(name)(using ctx)
      case _ => JSValue.Undefined
    }

  /** Installed after Symbol initialization because Array.prototype itself is
    * initialized earlier to break the built-in dependency cycle. */
  def initializeArrayUnscopables(ctx: JSContext): Unit = {
    given JSContext = ctx
    getWellKnownSymbol("unscopables") match {
      case JSValue.Symbol(id) =>
        val value = JSObject(prototype = null, extensible = true)
        Seq(
          "at", "copyWithin", "entries", "fill", "find", "findIndex",
          "findLast", "findLastIndex", "flat", "flatMap", "includes",
          "keys", "toReversed", "toSorted", "toSpliced", "values"
        ).foreach(name => value.defineProperty(
          name, JSValue.Bool(true), enumerable = true, writable = true,
          configurable = true
        ))
        ctx.arrayPrototype.defineSymbolProperty(
          id,
          JSValue.Object(value),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }
  }

  def initializeArrayConstructor(ctx: JSContext): Unit = {
    def buildArray(values: Seq[JSValue]): JSValue = {
      val arr = quickjs.objmodel.JSArray.empty()
      values.foreach(arr.push)
      JSValue.JSArrayVal(arr)
    }

    def buildArrayFromArgs(args: Array[JSValue], offset: Int): JSValue =
      if args.length == offset then buildArray(Seq.empty)
      else if args.length == offset + 1 then
        args(offset) match {
          case JSValue.Int32(i) =>
            if i < 0 then ctx.throwRangeError("Invalid array length")
            JSValue.JSArrayVal(quickjs.objmodel.JSArray(i))
          case JSValue.Float64(d) =>
            if d.isNaN || d.isInfinite || d < 0 || d != math.floor(d) then
              ctx.throwRangeError("Invalid array length")
            else if d > Int.MaxValue then
              ctx.throwRangeError("Invalid array length")
            else JSValue.JSArrayVal(quickjs.objmodel.JSArray(d.toInt))
          case _ =>
            buildArray(Seq(args(offset)))
        }
      else buildArray(args.drop(offset).toSeq)

    /** Fill an existing (derived-class) array receiver like `new Array(...)`. */
    def initArrayFromArgs(
        arr: quickjs.objmodel.JSArray,
        args: Array[JSValue]
    ): Unit =
      if args.isEmpty then ()
      else if args.length == 1 then
        args(0) match {
          case JSValue.Int32(i) =>
            if i < 0 then ctx.throwRangeError("Invalid array length")
            arr.defineLength(Some(i.toLong), Some(true))
          case JSValue.Float64(d) =>
            if d.isNaN || d.isInfinite || d < 0 || d != math.floor(d) then
              ctx.throwRangeError("Invalid array length")
            else if d > Int.MaxValue then
              ctx.throwRangeError("Invalid array length")
            else arr.defineLength(Some(d.toLong), Some(true))
          case other => arr.push(other)
        }
      else {
        var i = 0
        while i < args.length do {
          arr.push(args(i))
          i += 1
        }
      }

    val arrayConstructor = quickjs.value.NativeConstructor(
      name = "Array",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        // `callImpl` receives the real arguments only (no receiver):
        // `Array(3)` and `new Array(3)` must behave identically.
        buildArrayFromArgs(args, 0)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildArrayFromArgs(args, 0)
      ,
      prototype = ctx.arrayPrototype,
      superInitImpl = Some((thisValue, args, initCtx) => {
        given JSContext = initCtx
        thisValue match {
          case JSValue.JSArrayVal(arr) =>
            initArrayFromArgs(arr, args)
            thisValue
          case JSValue.Object(obj) =>
            // The derived class did not receive an Array receiver (e.g. it was
            // constructed directly): fall back to filling an ordinary array.
            thisValue
          case _ =>
            initCtx.throwTypeError("Constructor Array requires 'new'")
        }
      })
    )
    given JSContext = ctx
    initConstructor(arrayConstructor, length = 1)
    ctx.global.set("Array", JSValue.Native(arrayConstructor))
    ctx.arrayPrototype.defineProperty(
      "constructor",
      JSValue.Native(arrayConstructor),
      enumerable = false
    )(using ctx)

    val arrayIsArray = NativeFunction(
      name = "isArray",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then JSValue.Bool(false)
        else {
          given JSContext = ctx
          def check(v: JSValue): Boolean = v match {
            case JSValue.JSArrayVal(_) => true
            case JSValue.Object(obj)   =>
              // Check if proxy and unwrap
              obj.getOwnProperty("__proxy_target") match {
                case Some(JSValue.Null) =>
                  ctx.throwTypeError(
                    "Cannot perform 'isArray' on a revoked proxy"
                  )
                case Some(target) => check(target)
                case None         => obj.isArray
              }
            case _ => false
          }
          JSValue.Bool(check(args(offset)))
        }
    )

    val arrayOf = NativeFunction(
      name = "of",
      impl = (args, _) =>
        val offset = if args.length >= 2 then 1 else 0
        val arr = quickjs.objmodel.JSArray.empty()
        var i = offset
        while i < args.length do {
          arr.push(args(i))
          i += 1
        }
        JSValue.JSArrayVal(arr)
    )

    val arrayFrom = NativeFunction(
      name = "from",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then
          ctx.throwTypeError("Array.from requires an array-like or iterable object")
        else {
          val thisValue =
            if offset == 1 then args(0) else JSValue.Undefined
          val source = args(offset)
          source match {
            case JSValue.Null | JSValue.Undefined =>
              ctx.throwTypeError("Array.from requires an array-like or iterable object")
            case _ => ()
          }
          val mapFn =
            if args.length > offset + 1 then Some(args(offset + 1)) else None
          val thisArg =
            if args.length > offset + 2 then args(offset + 2)
            else JSValue.Undefined
          given JSContext = ctx

          def isConstructor(value: JSValue): Boolean =
            value match {
              case func: JSValue.Function => func.isConstructor
              case JSValue.Native(_: quickjs.value.NativeConstructor) => true
              case _ => false
            }

          def constructFromThis(lengthArg: Option[Int]): JSValue =
            if isConstructor(thisValue) then
              val ctorArgs =
                lengthArg
                  .map(len => Array[JSValue](JSValue.fromInt(len)))
                  .getOrElse(Array.empty[JSValue])
              thisValue match {
                case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                  nc.construct(ctorArgs)
                case func: JSValue.Function =>
                  val prototype =
                    func.funcObj.get("prototype")(using ctx) match {
                      case JSValue.Object(proto) => proto
                      case _                     => ctx.objectPrototype
                    }
                  val newObj = JSObject(prototype = prototype, extensible = true)
                  val ret = Interpreter().call(
                    functionToBytecode(func),
                    JSValue.Object(newObj),
                    ctorArgs,
                    func.closure
                  )
                  ret match {
                    case _: JSValue.Object | _: JSValue.Function |
                        _: JSValue.JSArrayVal | _: JSValue.Generator |
                        _: JSValue.Promise | _: JSValue.Native |
                        _: JSValue.AsyncFunction =>
                      ret
                    case _ => JSValue.Object(newObj)
                  }
                case _ => JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
              }
            else
              lengthArg match {
                case Some(len) => JSValue.JSArrayVal(quickjs.objmodel.JSArray(len))
                case None      => JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
              }

          def defineResultIndex(target: JSValue, index: Int, value: JSValue): Unit =
            target match {
              case JSValue.JSArrayVal(arr) =>
                arr.defineIndexProperty(
                  index,
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )
              case JSValue.Object(obj) =>
                obj.defineProperty(
                  index.toString,
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )(using ctx)
              case func: JSValue.Function =>
                func.funcObj.defineProperty(
                  index.toString,
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )(using ctx)
              case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                nc.funcObj.defineProperty(
                  index.toString,
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )(using ctx)
              case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                nf.funcObj.defineProperty(
                  index.toString,
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )(using ctx)
              case _ =>
                ctx.throwTypeError("Array.from constructor result is not an object")
            }

          def setResultLength(target: JSValue, length: Int): Unit =
            target match {
              case JSValue.JSArrayVal(arr) => arr.setLength(length)
              case JSValue.Object(obj) =>
                obj.defineProperty(
                  "length",
                  JSValue.fromInt(length),
                  enumerable = false,
                  writable = true,
                  configurable = false
                )(using ctx)
              case func: JSValue.Function =>
                func.funcObj.defineProperty(
                  "length",
                  JSValue.fromInt(length),
                  enumerable = false,
                  writable = true,
                  configurable = false
                )(using ctx)
              case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                nc.funcObj.defineProperty(
                  "length",
                  JSValue.fromInt(length),
                  enumerable = false,
                  writable = true,
                  configurable = false
                )(using ctx)
              case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                nf.funcObj.defineProperty(
                  "length",
                  JSValue.fromInt(length),
                  enumerable = false,
                  writable = true,
                  configurable = false
                )(using ctx)
              case _ => ()
            }

          def getProperty(value: JSValue, key: JSValue): JSValue =
            // Accessor-aware [[Get]]: iterator results and methods can expose
            // getters (Array.from must observe a throwing `value` getter).
            key match {
              case JSValue.Symbol(sym) =>
                BuiltinHelpers.getSymbolPropertyWithGetter(value, sym)
              case JSValue.JSStr(name) =>
                BuiltinHelpers.getPropertyWithGetter(value, name)
              case other =>
                BuiltinHelpers.getPropertyWithGetter(value, other.toString)
            }

          def isCallable(value: JSValue): Boolean =
            value match {
              case _: JSValue.Function | JSValue.Native(_: NativeFunction) |
                  JSValue.Native(_: quickjs.value.NativeConstructor) =>
                true
              case _ => false
            }

          def iteratorClose(iterator: JSValue): Unit =
            try {
              val returnMethod = getProperty(iterator, JSValue.fromString("return"))
              if returnMethod != JSValue.Undefined then
                callFunctionWithThis(returnMethod, iterator, Array.empty)
            }
            catch {
              case _: Exception => ()
            }

          def pushValue(target: JSValue, value: JSValue, index: Int): Unit = {
            val mapped =
              mapFn match {
                case Some(func) if func != JSValue.Undefined =>
                  if !isCallable(func) then
                    ctx.throwTypeError("Array.from mapper must be callable")
                  callFunctionWithThis(
                    func,
                    thisArg,
                    Array(value, JSValue.fromInt(index))
                  )
                case None => value
                case _    => value
              }
            defineResultIndex(target, index, mapped)
          }

          val iteratorMethod = getWellKnownSymbol("iterator") match {
            case JSValue.Symbol(sym) =>
              // GetMethod(source, @@iterator) boxes primitive receivers so
              // `Array.from(5)` sees `Number.prototype[Symbol.iterator]`.
              BuiltinHelpers.getSymbolPropertyWithGetter(source, sym)
            case _ => JSValue.Undefined
          }

          if iteratorMethod != JSValue.Undefined && iteratorMethod != JSValue.Null
          then {
            if !isCallable(iteratorMethod) then
              ctx.throwTypeError("value is not iterable")
            val result = constructFromThis(None)
            val iterator =
              callFunctionWithThis(iteratorMethod, source, Array.empty)
            val nextMethod = getProperty(iterator, JSValue.fromString("next"))
            if !isCallable(nextMethod) then
              ctx.throwTypeError("iterator next is not callable")
            var index = 0
            try {
              var done = false
              while !done do {
                val nextResult =
                  callFunctionWithThis(nextMethod, iterator, Array.empty)
                nextResult match {
                  case JSValue.Object(_) =>
                    if getProperty(nextResult, JSValue.fromString("done")).toBoolean
                    then done = true
                    else {
                      val value =
                        getProperty(nextResult, JSValue.fromString("value"))
                      pushValue(result, value, index)
                      index += 1
                    }
                  case _ =>
                    iteratorClose(iterator)
                    ctx.throwTypeError("iterator result is not an object")
                }
              }
            }
            catch {
              case e: Exception =>
                iteratorClose(iterator)
                throw e
            }
            setResultLength(result, index)
            result
          }
          else {
            val length = source match {
              case JSValue.JSArrayVal(arr) => arr.getLength
              case JSValue.JSStr(str)      => str.length
              case JSValue.Object(obj)     => obj.get("length").toNumber.toInt
              case func: JSValue.Function  => func.funcObj.get("length").toNumber.toInt
              case _                       => 0
            }
            val result = constructFromThis(Some(length))
            source match {
              case JSValue.JSArrayVal(arr) =>
                var i = 0
                while i < arr.getLength do {
                  pushValue(result, arr.get(i), i)
                  i += 1
                }
              case JSValue.JSStr(str) =>
                var i = 0
                while i < str.length do {
                  pushValue(result, JSValue.fromString(str.charAt(i).toString), i)
                  i += 1
                }
              case JSValue.Object(obj) =>
                val len = obj.get("length").toNumber.toInt
                var i = 0
                while i < len do {
                  pushValue(result, obj.get(i.toString), i)
                  i += 1
                }
              case func: JSValue.Function =>
                val len = func.funcObj.get("length").toNumber.toInt
                var i = 0
                while i < len do {
                  pushValue(result, func.funcObj.get(i.toString), i)
                  i += 1
                }
              case _ => ()
            }
            setResultLength(result, length)
            result
          }
        }
    )

    arrayConstructor.funcObj.defineProperty(
      "isArray",
      JSValue.Native(arrayIsArray),
      enumerable = false,
      writable = true,
      configurable = true
    )(using ctx)
    arrayConstructor.funcObj.defineProperty(
      "of",
      JSValue.Native(arrayOf),
      enumerable = false,
      writable = true,
      configurable = true
    )(using ctx)
    arrayConstructor.funcObj.defineProperty(
      "from",
      JSValue.Native(arrayFrom),
      enumerable = false,
      writable = true,
      configurable = true
    )(using ctx)
  }

  /** Initialize Array.prototype methods */
  def initializeArrayPrototype(ctx: JSContext): Unit = {
    def strictEquals(a: JSValue, b: JSValue): Boolean = (a, b) match {
      case (JSValue.Int32(x), JSValue.Int32(y))     => x == y
      case (JSValue.Float64(x), JSValue.Float64(y)) =>
        !x.isNaN && !y.isNaN && x == y
      case (JSValue.Int32(x), JSValue.Float64(y)) =>
        !y.isNaN && x.toDouble == y
      case (JSValue.Float64(x), JSValue.Int32(y)) =>
        !x.isNaN && x == y.toDouble
      case _ => a == b
    }

    def sameValueZero(a: JSValue, b: JSValue): Boolean = (a, b) match {
      case (JSValue.Float64(x), JSValue.Float64(y)) if x.isNaN && y.isNaN =>
        true
      case _ => strictEquals(a, b)
    }

    def isArrayValue(value: JSValue)(using JSContext): Boolean = value match {
      case JSValue.JSArrayVal(_) => true
      case JSValue.Object(obj) =>
        obj.getOwnProperty("__proxy_revoked") match {
          case Some(JSValue.Bool(true)) => ctx.throwTypeError("Proxy has been revoked")
          case _ =>
            obj.getOwnProperty("__proxy_target").exists(isArrayValue)
        }
      case _ => false
    }

    def constructArraySpecies(constructor: JSValue, length: Long)(using
        JSContext
    ): JSValue = constructor match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.construct(Array(JSValue.fromDouble(length.toDouble)))
      case function: JSValue.Function if function.isConstructor =>
        val proto = getPropertyWithGetter(function, "prototype") match {
          case JSValue.Object(obj) => obj
          case _                   => ctx.objectPrototype
        }
        val receiver = JSObject(prototype = proto, extensible = true)
        val returned = Interpreter().call(
          functionToBytecode(function),
          JSValue.Object(receiver),
          Array(JSValue.fromDouble(length.toDouble)),
          function.closure,
          function
        )
        returned match {
          case JSValue.Object(_) | JSValue.JSArrayVal(_) | _: JSValue.Function |
              JSValue.Native(_) => returned
          case _ => JSValue.Object(receiver)
        }
      case _ => ctx.throwTypeError("Array species is not a constructor")
    }

    def arraySpeciesCreate(original: JSValue, length: Long)(using
        JSContext
    ): JSValue = {
      def intrinsicArray(): JSValue =
        if length > 4294967295L || length > Int.MaxValue.toLong then
          ctx.throwRangeError("Invalid array length")
        JSValue.JSArrayVal(JSArray(length.toInt))
      if !isArrayValue(original) then intrinsicArray()
      else {
        var constructor = getPropertyWithGetter(original, "constructor")
        constructor match {
          case JSValue.Object(_) | _: JSValue.Function | JSValue.Native(_) =>
            getWellKnownSymbol("species") match {
              case JSValue.Symbol(id) =>
                val species = extractJSObject(constructor)
                  .flatMap(_.getSymbolPropertyDescriptorWithOwner(id)) match {
                  case Some((_, _, attrs)) if attrs.getter.isDefined =>
                    callFunctionWithThis(
                      attrs.getter.get,
                      constructor,
                      Array.empty
                    )
                  case Some((_, value, _)) => value
                  case None                => JSValue.Undefined
                }
                constructor = species match {
                  case JSValue.Null | JSValue.Undefined => JSValue.Undefined
                  case value                            => value
                }
              case _ => ()
            }
          case JSValue.Undefined => ()
          case _ => ctx.throwTypeError("Array constructor is not an object")
        }
        if constructor == JSValue.Undefined then
          intrinsicArray()
        else constructArraySpecies(constructor, length)
      }
    }

    def createResultProperty(target: JSValue, index: Long, value: JSValue)(using
        JSContext
    ): Unit = target match {
      case JSValue.JSArrayVal(array) =>
        if !array.defineIndexProperty(
            index,
            value,
            enumerable = true,
            writable = true,
            configurable = true
          )
        then ctx.throwTypeError(s"Cannot create result property '$index'")
      case _ => extractJSObject(target) match {
        case Some(obj) =>
          if !obj.defineProperty(
              index.toString,
              value,
              enumerable = true,
              writable = true,
              configurable = true
            )
          then ctx.throwTypeError(s"Cannot create result property '$index'")
        case None => ctx.throwTypeError("Array species result is not an object")
      }
    }
    // Array.prototype.push(element1, ..., elementN)
    // Appends elements to the end of an array and returns the new length
    val arrayPrototypePush = NativeFunction(
      name = "push",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "push")
        val len = arrayLikeLengthLong(receiver)
        val count = args.length - 1
        if len + count.toLong > 9007199254740991L then
          ctx.throwTypeError("Array.prototype.push exceeds maximum safe length")
        var i = 0
        while i < count do {
          arrayLikeSet(receiver, len + i, args(i + 1))
          i += 1
        }
        val newLength = len + count
        arrayLikeSetLength(receiver, newLength)
        JSValue.fromDouble(newLength.toDouble)
    )

    // Array.prototype.map(callback)
    // Creates a new array with the results of calling a provided function on every element
    val arrayPrototypeMap = NativeFunction(
      name = "map",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "map")
        val len = arrayLikeLengthLong(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        val result = arraySpeciesCreate(receiver, len)
        var i = 0L
        while i < len do {
          checkInterrupted((i & Int.MaxValue.toLong).toInt)
          if arrayLikeHasLong(receiver, i) then
            createResultProperty(
              result,
              i,
              callFunctionWithThis(callback, thisArg,
                Array(arrayLikeGetLong(receiver, i), JSValue.fromDouble(i.toDouble), receiver))
            )
          i += 1
        }
        result
    )

    val arrayPrototypeFilter = NativeFunction(
      name = "filter",
      impl = (args, ctx) =>
        val receiver = arrayLikeReceiver(args, "filter")(using ctx)
        given JSContext = ctx
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        val result = arraySpeciesCreate(receiver, 0)
        var target = 0L
        var i = 0
        while i < len do {
          checkInterrupted(i)
          if arrayLikeHas(receiver, i) then {
            val elem = arrayLikeGet(receiver, i)
            if callFunctionWithThis(
              callback,
              thisArg,
              Array(elem, JSValue.fromInt(i), receiver)
            ).toBoolean
            then {
              createResultProperty(result, target, elem)
              target += 1
            }
          }
          i += 1
        }
        result
    )

    val arrayPrototypeForEach = NativeFunction(
      name = "forEach",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "forEach")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var i = 0
        while i < len do {
          checkInterrupted(i)
          if arrayLikeHas(receiver, i) then
            callFunctionWithThis(callback, thisArg,
              Array(arrayLikeGet(receiver, i), JSValue.fromInt(i), receiver))
          i += 1
        }
        JSValue.Undefined
    )

    val arrayPrototypeReduce = NativeFunction(
      name = "reduce",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "reduce")
        val len = arrayLikeLengthLong(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val hasInitial = args.length > 2
        var index = 0L
        var acc = if hasInitial then args(2) else JSValue.Undefined
        if !hasInitial then {
          while index < len && !arrayLikeHasLong(receiver, index) do {
            checkInterrupted((index & Int.MaxValue.toLong).toInt)
            index += 1
          }
          if index >= len then ctx.throwTypeError("Reduce of empty array with no initial value")
          acc = arrayLikeGetLong(receiver, index)
          index += 1
        }
        while index < len do {
          checkInterrupted((index & Int.MaxValue.toLong).toInt)
          if arrayLikeHasLong(receiver, index) then
            acc = callFunctionWithThis(callback, JSValue.Undefined,
              Array(acc, arrayLikeGetLong(receiver, index), JSValue.fromDouble(index.toDouble), receiver)
            )
          index += 1
        }
        acc
    )

    val arrayPrototypeIncludes = NativeFunction(
      name = "includes",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "includes")
        val search = if args.length > 1 then args(1) else JSValue.Undefined
        val len = arrayLikeLength(receiver)
        if len == 0 then JSValue.Bool(false)
        else {
          var k = if args.length > 2 then fromIndex(args(2), len) else 0
          var found = false
          while k < len && !found do {
            checkInterrupted(k)
            // includes uses Get rather than HasProperty, so holes compare as undefined.
            if sameValueZero(arrayLikeGet(receiver, k), search) then found = true
            k += 1
          }
          JSValue.fromBoolean(found)
        }
    )

    val arrayPrototypeIndexOf = NativeFunction(
      name = "indexOf",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "indexOf")
        val search = if args.length > 1 then args(1) else JSValue.Undefined
        val len = arrayLikeLength(receiver)
        var k = if args.length > 2 then fromIndex(args(2), len) else 0
        var idx = -1
        while k < len && idx < 0 do {
          checkInterrupted(k)
          if arrayLikeHas(receiver, k) && strictEquals(arrayLikeGet(receiver, k), search) then idx = k
          k += 1
        }
        JSValue.fromInt(idx)
    )

    val arrayPrototypeLastIndexOf = NativeFunction(
      name = "lastIndexOf",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "lastIndexOf")
        val search = if args.length > 1 then args(1) else JSValue.Undefined
        val len = arrayLikeLength(receiver)
        var k =
          if len == 0 then -1
          else if args.length <= 2 || args(2) == JSValue.Undefined then len - 1
          else {
            val n = args(2).toNumber
            if n.isNaN then 0
            else if n >= 0 then math.min(n.toInt, len - 1)
            else len + n.toInt
          }
        var idx = -1
        while k >= 0 && idx < 0 do {
          checkInterrupted(k)
          if arrayLikeHas(receiver, k) &&
              strictEquals(arrayLikeGet(receiver, k), search)
          then idx = k
          k -= 1
        }
        JSValue.fromInt(idx)
    )

    val arrayPrototypeEvery = NativeFunction(
      name = "every",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "every")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var i = 0
        var result = true
        while i < len && result do {
          checkInterrupted(i)
          if arrayLikeHas(receiver, i) then
            result = callFunctionWithThis(callback, thisArg,
              Array(arrayLikeGet(receiver, i), JSValue.fromInt(i), receiver)).toBoolean
          i += 1
        }
        JSValue.fromBoolean(result)
    )

    val arrayPrototypeSome = NativeFunction(
      name = "some",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "some")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var i = 0
        var result = false
        while i < len && !result do {
          checkInterrupted(i)
          if arrayLikeHas(receiver, i) then
            result = callFunctionWithThis(callback, thisArg,
              Array(arrayLikeGet(receiver, i), JSValue.fromInt(i), receiver)).toBoolean
          i += 1
        }
        JSValue.fromBoolean(result)
    )

    val arrayPrototypeFind = NativeFunction(
      name = "find",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "find")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var i = 0
        var result: JSValue = JSValue.Undefined
        var found = false
        while i < len && !found do {
          checkInterrupted(i)
          val value = arrayLikeGet(receiver, i)
          if callFunctionWithThis(callback, thisArg,
              Array(value, JSValue.fromInt(i), receiver)).toBoolean
          then { result = value; found = true }
          i += 1
        }
        result
    )

    val arrayPrototypeFindIndex = NativeFunction(
      name = "findIndex",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "findIndex")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var i = 0
        var result = -1
        while i < len && result < 0 do {
          checkInterrupted(i)
          if callFunctionWithThis(callback, thisArg,
              Array(arrayLikeGet(receiver, i), JSValue.fromInt(i), receiver)).toBoolean
          then result = i
          i += 1
        }
        JSValue.fromInt(result)
    )

    val arrayPrototypeFindLast = NativeFunction(
      name = "findLast",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "findLast")
        val len = arrayLikeLengthLong(receiver)
        val callback = requireCallback(
          if args.length > 1 then args(1) else JSValue.Undefined
        )
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var index = len - 1
        var result: JSValue = JSValue.Undefined
        var found = false
        while index >= 0 && !found do {
          checkInterrupted((index & Int.MaxValue.toLong).toInt)
          val value = arrayLikeGetLong(receiver, index)
          if callFunctionWithThis(
              callback,
              thisArg,
              Array(value, JSValue.fromDouble(index.toDouble), receiver)
            ).toBoolean
          then {
            result = value
            found = true
          }
          index -= 1
        }
        result
    )

    val arrayPrototypeFindLastIndex = NativeFunction(
      name = "findLastIndex",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "findLastIndex")
        val len = arrayLikeLengthLong(receiver)
        val callback = requireCallback(
          if args.length > 1 then args(1) else JSValue.Undefined
        )
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var index = len - 1
        var result = -1L
        while index >= 0 && result < 0 do {
          checkInterrupted((index & Int.MaxValue.toLong).toInt)
          val value = arrayLikeGetLong(receiver, index)
          if callFunctionWithThis(
              callback,
              thisArg,
              Array(value, JSValue.fromDouble(index.toDouble), receiver)
            ).toBoolean
          then result = index
          index -= 1
        }
        JSValue.fromDouble(result.toDouble)
    )

    val arrayPrototypeReverse = NativeFunction(
      name = "reverse",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "reverse")
        val len = arrayLikeLengthLong(receiver)
        var lower = 0L
        while lower < len / 2 do {
          checkInterrupted((lower & Int.MaxValue.toLong).toInt)
          val upper = len - lower - 1
          val lowerExists = arrayLikeHasLong(receiver, lower)
          val lowerValue =
            if lowerExists then arrayLikeGetLong(receiver, lower)
            else JSValue.Undefined
          val upperExists = arrayLikeHasLong(receiver, upper)
          val upperValue =
            if upperExists then arrayLikeGetLong(receiver, upper)
            else JSValue.Undefined
          if lowerExists && upperExists then {
            arrayLikeSet(receiver, lower, upperValue)
            arrayLikeSet(receiver, upper, lowerValue)
          } else if !lowerExists && upperExists then {
            arrayLikeSet(receiver, lower, upperValue)
            arrayLikeDelete(receiver, upper)
          } else if lowerExists then {
            arrayLikeDelete(receiver, lower)
            arrayLikeSet(receiver, upper, lowerValue)
          }
          lower += 1
        }
        receiver
    )

    val arrayPrototypeFill = NativeFunction(
      name = "fill",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "fill")
        val len = arrayLikeLengthLong(receiver)
        val value = if args.length > 1 then args(1) else JSValue.Undefined
        val start =
          if args.length > 2 && args(2) != JSValue.Undefined then
            relativeIndexLong(args(2), len)
          else 0L
        val end =
          if args.length > 3 && args(3) != JSValue.Undefined then
            relativeIndexLong(args(3), len)
          else len
        var i = start
        while i < end do {
          checkInterrupted((i & Int.MaxValue).toInt)
          arrayLikeSet(receiver, i, value)
          i += 1
        }
        receiver
    )

    val arrayPrototypeAt = NativeFunction(
      name = "at",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "at")
        val len = arrayLikeLength(receiver)
        val raw = if args.length > 1 then toIntegerOrInfinity(args(1)) else 0.0
        if raw.isInfinite then JSValue.Undefined
        else {
          val idx = raw.toInt
          val i = if idx < 0 then len + idx else idx
          if i < 0 || i >= len then JSValue.Undefined else arrayLikeGet(receiver, i)
        }
    )

    val arrayPrototypeCopyWithin = NativeFunction(
      name = "copyWithin",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "copyWithin")
        val len = arrayLikeLengthLong(receiver)
        val target =
          relativeIndexLong(
            if args.length > 1 then args(1) else JSValue.Undefined,
            len
          )
        val start =
          relativeIndexLong(
            if args.length > 2 then args(2) else JSValue.Undefined,
            len
          )
        val end =
          if args.length > 3 && args(3) != JSValue.Undefined then
            relativeIndexLong(args(3), len)
          else len
        val count = math.min(end - start, len - target)
        if count > 0 then {
          val dir = if start < target && target < start + count then -1 else 1
          var i = if dir > 0 then 0L else count - 1
          while i >= 0 && i < count do {
            checkInterrupted((i & Int.MaxValue).toInt)
            val from = start + i
            val to = target + i
            if arrayLikeHasLong(receiver, from) then
              arrayLikeSet(receiver, to, arrayLikeGetLong(receiver, from))
            else arrayLikeDelete(receiver, to)
            i += dir
          }
        }
        receiver
    )

    val arrayPrototypeSplice = NativeFunction(
      name = "splice",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "splice")
        val len = arrayLikeLengthLong(receiver)
        val actualStart = relativeIndexLong(
          if args.length > 1 then args(1) else JSValue.Undefined,
          len
        )
        val argumentCount = math.max(0, args.length - 1)
        val insertCount = math.max(0, args.length - 3).toLong
        val deleteCount =
          if argumentCount == 0 then 0L
          else if argumentCount == 1 then len - actualStart
          else {
            val requested = toIntegerOrInfinity(args(2))
            if requested <= 0 || requested.isNaN then 0L
            else if requested.isPosInfinity then len - actualStart
            else math.min(requested.toLong, len - actualStart)
          }
        val newLength = len - deleteCount + insertCount
        if newLength > 9007199254740991L then
          ctx.throwTypeError("Array length exceeds the maximum safe integer")

        val removed = arraySpeciesCreate(receiver, deleteCount)
        var k = 0L
        while k < deleteCount do {
          checkInterrupted((k & Int.MaxValue.toLong).toInt)
          val from = actualStart + k
          if arrayLikeHasLong(receiver, from) then
            createResultProperty(removed, k, arrayLikeGetLong(receiver, from))
          k += 1
        }
        arrayLikeSetLength(removed, deleteCount)

        if insertCount < deleteCount then {
          k = actualStart
          while k < len - deleteCount do {
            checkInterrupted((k & Int.MaxValue.toLong).toInt)
            val from = k + deleteCount
            val to = k + insertCount
            if arrayLikeHasLong(receiver, from) then
              arrayLikeSet(receiver, to, arrayLikeGetLong(receiver, from))
            else arrayLikeDelete(receiver, to)
            k += 1
          }
          k = len
          while k > newLength do {
            k -= 1
            arrayLikeDelete(receiver, k)
          }
        } else if insertCount > deleteCount then {
          k = len - deleteCount
          while k > actualStart do {
            k -= 1
            val from = k + deleteCount
            val to = k + insertCount
            if arrayLikeHasLong(receiver, from) then
              arrayLikeSet(receiver, to, arrayLikeGetLong(receiver, from))
            else arrayLikeDelete(receiver, to)
          }
        }

        var item = 0
        while item < insertCount.toInt do {
          arrayLikeSet(receiver, actualStart + item, args(item + 3))
          item += 1
        }
        arrayLikeSetLength(receiver, newLength)
        removed
    )

    val arrayPrototypeShift = NativeFunction(
      name = "shift",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "shift")
        val len = arrayLikeLengthLong(receiver)
        if len == 0 then {
          arrayLikeSetLength(receiver, 0)
          JSValue.Undefined
        }
        else {
          val first = arrayLikeGetLong(receiver, 0)
          var i = 1L
          while i < len do {
            checkInterrupted((i & Int.MaxValue.toLong).toInt)
            if arrayLikeHasLong(receiver, i) then
              arrayLikeSet(receiver, i - 1, arrayLikeGetLong(receiver, i))
            else arrayLikeDelete(receiver, i - 1)
            i += 1
          }
          arrayLikeDelete(receiver, len - 1)
          arrayLikeSetLength(receiver, len - 1)
          first
        }
    )

    val arrayPrototypeUnshift = NativeFunction(
      name = "unshift",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "unshift")
        val elements =
          if args.length > 1 then args.slice(1, args.length)
          else Array.empty[JSValue]
        val len = arrayLikeLengthLong(receiver)
        val n = elements.length
        if len + n.toLong > 9007199254740991L then
          ctx.throwTypeError("Array.prototype.unshift exceeds maximum safe length")
        if n > 0 then {
          var i = len
          while i > 0 do {
            val from = i - 1
            val to = from + n
            if arrayLikeHasLong(receiver, from) then
              arrayLikeSet(receiver, to, arrayLikeGetLong(receiver, from))
            else arrayLikeDelete(receiver, to)
            i -= 1
          }
        }
        var j = 0
        while j < n do { arrayLikeSet(receiver, j, elements(j)); j += 1 }
        val newLength = len + n
        arrayLikeSetLength(receiver, newLength)
        JSValue.fromDouble(newLength.toDouble)
    )

    // Array.prototype.pop()
    // Removes the last element from an array and returns that element
    val arrayPrototypePop = NativeFunction(
      name = "pop",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "pop")
        val len = arrayLikeLengthLong(receiver)
        if len == 0 then {
          arrayLikeSetLength(receiver, 0)
          JSValue.Undefined
        } else {
          val index = len - 1
          val value = arrayLikeGetLong(receiver, index)
          arrayLikeDelete(receiver, index)
          arrayLikeSetLength(receiver, index)
          value
        }
    )

    val arrayPrototypeReduceRight = NativeFunction(
      name = "reduceRight",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "reduceRight")
        val len = arrayLikeLengthLong(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val hasInitial = args.length > 2
        var index = len - 1
        var acc = if hasInitial then args(2) else JSValue.Undefined
        if !hasInitial then {
          while index >= 0 && !arrayLikeHasLong(receiver, index) do {
            checkInterrupted((index & Int.MaxValue.toLong).toInt)
            index -= 1
          }
          if index < 0 then ctx.throwTypeError("Reduce of empty array with no initial value")
          acc = arrayLikeGetLong(receiver, index)
          index -= 1
        }
        while index >= 0 do {
          checkInterrupted((index & Int.MaxValue.toLong).toInt)
          if arrayLikeHasLong(receiver, index) then
            acc = callFunctionWithThis(callback, JSValue.Undefined,
              Array(acc, arrayLikeGetLong(receiver, index), JSValue.fromDouble(index.toDouble), receiver)
            )
          index -= 1
        }
        acc
    )

    val arrayPrototypeSort = NativeFunction(
      name = "sort",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val compareFn =
          if args.length <= 1 || args(1) == JSValue.Undefined then None
          else if isCallable(args(1)) then Some(args(1))
          else ctx.throwTypeError("Array.prototype.sort comparator is not callable")
        val receiver = arrayLikeReceiver(args, "sort")
        val len = arrayLikeLengthLong(receiver)
        val collected = scala.collection.mutable.ArrayBuffer.empty[JSValue]
        var undefinedCount = 0L
        var index = 0L
        while index < len do {
          checkInterrupted((index & Int.MaxValue.toLong).toInt)
          if arrayLikeHasLong(receiver, index) then {
            val value = arrayLikeGetLong(receiver, index)
            if value == JSValue.Undefined then undefinedCount += 1
            else collected += value
          }
          index += 1
        }
        val values = collected.toArray
        def cmp(a: JSValue, b: JSValue): Int = compareFn match {
          case Some(func) =>
            val num = toNumber(callFunctionWithThis(
              func,
              JSValue.Undefined,
              Array(a, b)
            ))
            if num.isNaN then 0 else num.sign.toInt
          case None =>
            toPrimitive(a, "string").toString.compareTo(
              toPrimitive(b, "string").toString
            )
        }
        Sorting.stableSort(values, (a, b) => cmp(a, b) < 0)
        index = 0L
        var valueIndex = 0
        while valueIndex < values.length do {
          arrayLikeSet(receiver, index, values(valueIndex))
          index += 1
          valueIndex += 1
        }
        var remainingUndefined = undefinedCount
        while remainingUndefined > 0 do {
          arrayLikeSet(receiver, index, JSValue.Undefined)
          index += 1
          remainingUndefined -= 1
        }
        while index < len do {
          arrayLikeDelete(receiver, index)
          index += 1
        }
        receiver
    )

    // Array.prototype.toString()
    // Joins elements with commas
    val arrayPrototypeToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        args(0) match {
          case JSValue.JSArrayVal(arrVal) =>
            JSValue.fromString(
              (0 until arrVal.getLength)
                .map(i => arrVal.get(i).toString)
                .mkString(",")
            )
          case _ => JSValue.fromString("")
        }
    )

    val arrayPrototypeJoin = NativeFunction(
      name = "join",
      impl = (args, ctx) =>
        if args.headOption.exists(a =>
            a == JSValue.Undefined || a == JSValue.Null
          )
        then
          ctx.throwTypeError("Array.prototype.join called on null or undefined")
        val arr = thisArray(args, "join")
        val sep =
          if args.length > 1 && args(1) != JSValue.Undefined then
            args(1).toString
          else ","
        val sb = new StringBuilder()
        val len = arr.getLength
        var i = 0
        while i < len do {
          checkInterrupted(i)
          if i > 0 then sb.append(sep)
          arr.get(i) match {
            case JSValue.Undefined | JSValue.Null => ()
            case v                                => sb.append(v.toString)
          }
          i += 1
        }
        JSValue.fromString(sb.toString)
    )

    val arrayPrototypeToLocaleString = NativeFunction(
      name = "toLocaleString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val arr = thisArray(args, "toLocaleString")
        val sb = new StringBuilder()
        val len = arr.getLength
        var index = 0
        while index < len do {
          checkInterrupted(index)
          if index > 0 then sb.append(",")
          arr.get(index) match {
            case JSValue.Null | JSValue.Undefined => ()
            case value =>
              val method = BuiltinHelpers.getPropertyWithGetter(
                value,
                "toLocaleString"
              )
              if !BuiltinHelpers.isCallable(method) then
                ctx.throwTypeError("toLocaleString is not callable")
              sb.append(
                BuiltinHelpers
                  .callFunctionWithThis(method, value, Array.empty)
                  .toString
              )
          }
          index += 1
        }
        JSValue.fromString(sb.toString)
    )

    // Array.prototype.concat(value1, value2, ..., valueN)
    // Returns a new array comprised of this array joined with other array(s) and/or value(s)
    val arrayPrototypeConcat = NativeFunction(
      name = "concat",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "concat")
        val result = arraySpeciesCreate(receiver, 0)
        val spreadSymbol = getWellKnownSymbol("isConcatSpreadable") match {
          case JSValue.Symbol(id) => Some(id)
          case _                  => None
        }
        def symbolGet(value: JSValue, id: Int): JSValue = value match {
          case JSValue.JSArrayVal(arr) =>
            arr.getOwnSymbol(id).getOrElse(
              ctx.arrayPrototype.getSymbol(id)(using ctx)
            )
          case JSValue.Object(obj) =>
            obj.getOwnProperty("__proxy_revoked") match {
              case Some(JSValue.Bool(true)) =>
                ctx.throwTypeError("Proxy has been revoked")
              case _ =>
                (obj.getOwnProperty("__proxy_target"), obj.getOwnProperty("__proxy_handler")) match {
                  case (Some(target), Some(JSValue.Object(handler))) =>
                    handler.get("get")(using ctx) match {
                      case JSValue.Undefined => symbolGet(target, id)
                      case trap => callFunctionWithThis(
                        trap,
                        JSValue.Object(handler),
                        Array(target, JSValue.Symbol(id), value)
                      )
                    }
                  case _ =>
                    obj.getSymbolPropertyDescriptorWithOwner(id) match {
                      case Some((_, _, attrs)) if attrs.getter.isDefined =>
                        callFunctionWithThis(attrs.getter.get, value, Array.empty)
                      case Some((_, stored, _)) => stored
                      case None                 => JSValue.Undefined
                    }
                }
            }
          case _ => extractJSObject(value).flatMap(
            _.getSymbolPropertyDescriptorWithOwner(id)
          ) match {
            case Some((_, _, attrs)) if attrs.getter.isDefined =>
              callFunctionWithThis(attrs.getter.get, value, Array.empty)
            case Some((_, stored, _)) => stored
            case None                 => JSValue.Undefined
          }
        }
        def spreadOverride(value: JSValue): Option[Boolean] =
          spreadSymbol.flatMap { id =>
            val spreadable = symbolGet(value, id)
            if spreadable == JSValue.Undefined then None
            else Some(spreadable.toBoolean)
          }
        def isSpreadable(value: JSValue): Boolean =
          spreadOverride(value).getOrElse(isArrayValue(value))

        var target = 0L
        def append(value: JSValue): Unit =
          if isSpreadable(value) then {
            val len = arrayLikeLengthLong(value)
            if target + len > 9007199254740991L then
              ctx.throwTypeError("Array.prototype.concat exceeds maximum safe length")
            var sourceIndex = 0L
            while sourceIndex < len do {
              checkInterrupted((sourceIndex & Int.MaxValue.toLong).toInt)
              if arrayLikeHasLong(value, sourceIndex) then
                createResultProperty(result, target, arrayLikeGetLong(value, sourceIndex))
              sourceIndex += 1
              target += 1
            }
          } else {
            if target >= 9007199254740991L then
              ctx.throwTypeError("Array.prototype.concat exceeds maximum safe length")
            createResultProperty(result, target, value)
            target += 1
          }

        append(receiver)
        var j = 1
        while j < args.length do { append(args(j)); j += 1 }
        arrayLikeSetLength(result, target)
        result
    )

    // Array.prototype.slice(begin, end)
    // Returns a shallow copy of a portion of an array
    val arrayPrototypeSlice = NativeFunction(
      name = "slice",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "slice")
        val len = arrayLikeLengthLong(receiver)
        val start =
          if args.length > 1 then relativeIndexLong(args(1), len) else 0L
        val stop = if args.length > 2 && args(2) != JSValue.Undefined then
          relativeIndexLong(args(2), len)
        else len
        val count = math.max(stop - start, 0)
        val result = arraySpeciesCreate(receiver, count)
        var i = start
        var target = 0L
        while i < stop do {
          checkInterrupted((target & Int.MaxValue.toLong).toInt)
          if arrayLikeHasLong(receiver, i) then
            createResultProperty(result, target, arrayLikeGetLong(receiver, i))
          i += 1
          target += 1
        }
        result
    )

    def copyArrayLength(receiver: JSValue, method: String)(using
        JSContext
    ): Int = {
      val length = arrayLikeLengthLong(receiver)
      if length > 4294967295L then
        ctx.throwRangeError(s"Array.prototype.$method result is too large")
      if length > Int.MaxValue.toLong then
        ctx.throwRangeError(s"Array.prototype.$method result is too large")
      length.toInt
    }

    val arrayPrototypeToReversed = NativeFunction(
      name = "toReversed",
      length = 0,
      impl = (args, callCtx) =>
        given JSContext = callCtx
        val receiver = arrayLikeReceiver(args, "toReversed")
        val len = copyArrayLength(receiver, "toReversed")
        val result = JSArray(len)
        var index = 0
        while index < len do {
          result.set(index, arrayLikeGetLong(receiver, len.toLong - index - 1))
          index += 1
        }
        JSValue.JSArrayVal(result)
    )

    val arrayPrototypeWith = NativeFunction(
      name = "with",
      length = 2,
      impl = (args, callCtx) =>
        given JSContext = callCtx
        val receiver = arrayLikeReceiver(args, "with")
        val len = copyArrayLength(receiver, "with")
        val relative = toIntegerOrInfinity(
          if args.length > 1 then args(1) else JSValue.Undefined
        )
        val actual = if relative >= 0 then relative.toLong else len.toLong + relative.toLong
        if actual < 0 || actual >= len then
          callCtx.throwRangeError("Array.prototype.with index out of range")
        val replacement = if args.length > 2 then args(2) else JSValue.Undefined
        val result = JSArray(len)
        var index = 0
        while index < len do {
          result.set(
            index,
            if index.toLong == actual then replacement
            else arrayLikeGetLong(receiver, index)
          )
          index += 1
        }
        JSValue.JSArrayVal(result)
    )

    val arrayPrototypeToSorted = NativeFunction(
      name = "toSorted",
      length = 1,
      impl = (args, callCtx) =>
        given JSContext = callCtx
        val receiver = arrayLikeReceiver(args, "toSorted")
        val compareFn = if args.length > 1 then args(1) else JSValue.Undefined
        if compareFn != JSValue.Undefined && !isCallable(compareFn) then
          callCtx.throwTypeError("Comparator must be a function")
        val len = copyArrayLength(receiver, "toSorted")
        val values = Array.tabulate(len)(i => arrayLikeGetLong(receiver, i))
        def compare(left: JSValue, right: JSValue): Int = (left, right) match {
          case (JSValue.Undefined, JSValue.Undefined) => 0
          case (JSValue.Undefined, _) => 1
          case (_, JSValue.Undefined) => -1
          case _ if compareFn != JSValue.Undefined =>
            val number = toNumber(callFunctionWithThis(
              compareFn, JSValue.Undefined, Array(left, right)
            ))
            if number.isNaN then 0 else number.sign.toInt
          case _ => left.toString.compareTo(right.toString)
        }
        Sorting.stableSort(values, (a, b) => compare(a, b) < 0)
        val result = JSArray(len)
        values.indices.foreach(index => result.set(index, values(index)))
        JSValue.JSArrayVal(result)
    )

    val arrayPrototypeToSpliced = NativeFunction(
      name = "toSpliced",
      length = 2,
      impl = (args, callCtx) =>
        given JSContext = callCtx
        val receiver = arrayLikeReceiver(args, "toSpliced")
        val len = arrayLikeLengthLong(receiver)
        val start = relativeIndexLong(
          if args.length > 1 then args(1) else JSValue.Undefined,
          len
        )
        val deleteCount =
          if args.length <= 1 then 0L
          else if args.length == 2 then len - start
          else math.min(
            math.max(toIntegerOrInfinity(args(2)).toLong, 0L),
            len - start
          )
        val insertCount = math.max(args.length - 3, 0)
        val newLen = len - deleteCount + insertCount
        if newLen > 9007199254740991L then
          callCtx.throwTypeError("Array.prototype.toSpliced exceeds maximum safe length")
        if newLen > 4294967295L || newLen > Int.MaxValue.toLong then
          callCtx.throwRangeError("Array.prototype.toSpliced result is too large")
        val result = JSArray(newLen.toInt)
        var target = 0L
        while target < start do {
          result.set(target, arrayLikeGetLong(receiver, target))
          target += 1
        }
        var item = 0
        while item < insertCount do {
          result.set(target, args(item + 3))
          target += 1
          item += 1
        }
        var source = start + deleteCount
        while source < len do {
          result.set(target, arrayLikeGetLong(receiver, source))
          source += 1
          target += 1
        }
        JSValue.JSArrayVal(result)
    )

    // Add methods to Array.prototype
    given JSContext = ctx
    def defineArrayMethod(name: String, function: NativeFunction): Unit =
      ctx.arrayPrototype.defineProperty(name, JSValue.Native(function),
        enumerable = false, writable = true, configurable = true)

    defineArrayMethod("push", arrayPrototypePush)
    defineArrayMethod("pop", arrayPrototypePop)
    defineArrayMethod("map", arrayPrototypeMap)
    defineArrayMethod("filter", arrayPrototypeFilter)
    defineArrayMethod("forEach", arrayPrototypeForEach)
    defineArrayMethod("reduce", arrayPrototypeReduce)
    defineArrayMethod("includes", arrayPrototypeIncludes)
    defineArrayMethod("indexOf", arrayPrototypeIndexOf)
    defineArrayMethod("lastIndexOf", arrayPrototypeLastIndexOf)
    defineArrayMethod("every", arrayPrototypeEvery)
    defineArrayMethod("some", arrayPrototypeSome)
    defineArrayMethod("find", arrayPrototypeFind)
    defineArrayMethod("findIndex", arrayPrototypeFindIndex)
    defineArrayMethod("findLast", arrayPrototypeFindLast)
    defineArrayMethod("findLastIndex", arrayPrototypeFindLastIndex)
    defineArrayMethod("reverse", arrayPrototypeReverse)
    defineArrayMethod("fill", arrayPrototypeFill)
    defineArrayMethod("at", arrayPrototypeAt)
    defineArrayMethod("copyWithin", arrayPrototypeCopyWithin)
    defineArrayMethod("splice", arrayPrototypeSplice)
    defineArrayMethod("shift", arrayPrototypeShift)
    defineArrayMethod("unshift", arrayPrototypeUnshift)
    defineArrayMethod("toString", arrayPrototypeToString)
    defineArrayMethod("toLocaleString", arrayPrototypeToLocaleString)
    defineArrayMethod("reduceRight", arrayPrototypeReduceRight)
    defineArrayMethod("sort", arrayPrototypeSort)
    defineArrayMethod("join", arrayPrototypeJoin)
    defineArrayMethod("concat", arrayPrototypeConcat)
    defineArrayMethod("slice", arrayPrototypeSlice)
    defineArrayMethod("toReversed", arrayPrototypeToReversed)
    defineArrayMethod("toSorted", arrayPrototypeToSorted)
    defineArrayMethod("toSpliced", arrayPrototypeToSpliced)
    defineArrayMethod("with", arrayPrototypeWith)

    getWellKnownSymbol("unscopables") match {
      case JSValue.Symbol(id) =>
        val unscopables = JSObject(prototype = null, extensible = true)
        Seq(
          "at",
          "copyWithin",
          "entries",
          "fill",
          "find",
          "findIndex",
          "findLast",
          "findLastIndex",
          "flat",
          "flatMap",
          "includes",
          "keys",
          "toReversed",
          "toSorted",
          "toSpliced",
          "values"
        ).foreach { name =>
          unscopables.defineProperty(
            name,
            JSValue.Bool(true),
            enumerable = true,
            writable = true,
            configurable = true
          )
        }
        ctx.arrayPrototype.defineSymbolProperty(
          id,
          JSValue.Object(unscopables),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }


    val arrayPrototypeValues = NativeFunction(
      name = "values",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        IteratorBuiltins.createArrayIterator(arrayLikeReceiver(args, "values"), "value")
    )
    val arrayPrototypeKeys = NativeFunction(
      name = "keys",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        IteratorBuiltins.createArrayIterator(arrayLikeReceiver(args, "keys"), "key")
    )
    val arrayPrototypeEntries = NativeFunction(
      name = "entries",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        IteratorBuiltins.createArrayIterator(arrayLikeReceiver(args, "entries"), "entry")
    )
    ctx.arrayPrototype.defineProperty(
      "values",
      JSValue.Native(arrayPrototypeValues),
      enumerable = false,
      writable = true,
      configurable = true
    )
    ctx.arrayPrototype.defineProperty(
      "keys",
      JSValue.Native(arrayPrototypeKeys),
      enumerable = false,
      writable = true,
      configurable = true
    )
    ctx.arrayPrototype.defineProperty(
      "entries",
      JSValue.Native(arrayPrototypeEntries),
      enumerable = false,
      writable = true,
      configurable = true
    )
    getWellKnownSymbol("iterator") match {
      case JSValue.Symbol(sym) =>
        ctx.arrayPrototype.initSymbolProperty(
          sym,
          JSValue.Native(arrayPrototypeValues),
          enumerable = false,
          writable = true,
          configurable = true
        )
      case _ => ()
    }

    // Array.prototype.flat(depth)
    val arrayPrototypeFlat = NativeFunction(
      name = "flat",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "flat")
        val rawDepth =
          if args.length > 1 && args(1) != JSValue.Undefined then
            toIntegerOrInfinity(args(1))
          else 1.0
        val depth =
          if rawDepth <= 0 || rawDepth.isNaN then 0L
          else if rawDepth == Double.PositiveInfinity then Long.MaxValue
          else rawDepth.toLong
        val sourceLength = arrayLikeLengthLong(receiver)
        val result = arraySpeciesCreate(receiver, 0)
        var targetIndex = 0L
        def flatten(source: JSValue, sourceLength: Long, level: Long): Unit = {
          var index = 0L
          while index < sourceLength do {
            checkInterrupted((index & Int.MaxValue.toLong).toInt)
            if arrayLikeHasLong(source, index) then {
              val element = arrayLikeGetLong(source, index)
              if level > 0 && isArrayValue(element) then
                flatten(element, arrayLikeLengthLong(element), level - 1)
              else {
                createResultProperty(result, targetIndex, element)
                targetIndex += 1
              }
            }
            index += 1
          }
        }
        flatten(receiver, sourceLength, depth)
        result
    )
    defineArrayMethod("flat", arrayPrototypeFlat)

    // Array.prototype.flatMap(callback, thisArg)
    val arrayPrototypeFlatMap = NativeFunction(
      name = "flatMap",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "flatMap")
        val len = arrayLikeLengthLong(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        val result = arraySpeciesCreate(receiver, 0)
        var targetIndex = 0L
        var index = 0L
        while index < len do {
          checkInterrupted((index & Int.MaxValue.toLong).toInt)
          if arrayLikeHasLong(receiver, index) then {
            val mapped = callFunctionWithThis(
              callback,
              thisArg,
              Array(
                arrayLikeGetLong(receiver, index),
                JSValue.fromDouble(index.toDouble),
                receiver
              )
            )
            mapped match {
              case JSValue.JSArrayVal(inner) =>
                var innerIndex = 0L
                while innerIndex < inner.getLengthValue.toNumber.toLong do {
                  if inner.hasIndex(innerIndex) then {
                    createResultProperty(result, targetIndex, inner.get(innerIndex))
                    targetIndex += 1
                  }
                  innerIndex += 1
                }
              case value =>
                createResultProperty(result, targetIndex, value)
                targetIndex += 1
            }
          }
          index += 1
        }
        result
    )
    defineArrayMethod("flatMap", arrayPrototypeFlatMap)
  }
}
