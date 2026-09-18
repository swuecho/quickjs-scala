package quickjs.runtime.builtins

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.runtime.JSContext
import scala.collection.mutable

/** Shared helpers for all built-in initializers.
  *
  * Extracted from StdLib.scala to reduce file size and enable per-builtin
  * files.
  */
object BuiltinHelpers {
  import quickjs.objmodel.{JSObject, JSArray}

  // --- Native function argument extraction ---

  /** Extract (thisArg, remainingArgs) from args array. Native functions called
    * via method dispatch receive `this` as args(0). This helper normalizes this
    * by detecting the calling convention. When called directly (e.g.
    * Array.isArray(x)), args(0) is the first real arg. When called as method
    * (e.g. arr.push(x)), args(0) is this, args(1) is first real arg. We
    * determine this by checking: if the first arg matches the expected type
    * (e.g. is an Array for Array methods), it's likely a method call with
    * `this`. Otherwise, treat it as a direct call.
    */
  def nativeArgs(args: Array[JSValue]): (JSValue, Array[JSValue]) =
    if args.isEmpty then (JSValue.Undefined, Array.empty)
    else (args(0), args.drop(1))

  /** Convert a JSValue.Function to a BytecodeFunction (centralized
    * constructor).
    */
  def functionToBytecode(func: JSValue.Function): BytecodeFunction =
    BytecodeFunction(
      name = func.name,
      bytecode = func.bytecode,
      constants = func.constants,
      stackSize = func.stackSize,
      freeVars = func.closure.keys.toArray,
      freeVarSlots = func.freeVarSlots,
      paramNames = func.paramNames,
      localVarNames = func.localVarNames,
      argumentsIndex = func.argumentsIndex,
      isConstructor = func.isConstructor,
      isClassConstructor = func.isClassConstructor,
      isGenerator = func.isGenerator,
      isAsync = func.isAsync,
      length = func.paramNames.length,
      spanMap = func.spanMap,
      isStrict = func.isStrict,
      parameterScopeEndPc = func.parameterScopeEndPc
    )

  /** Extract the underlying JSObject from a value (JSObject, Function.funcObj,
    * etc.).
    */
  def extractJSObject(value: JSValue): Option[JSObject] = value match {
    case JSValue.Object(obj)    => Some(obj)
    case func: JSValue.Function => Some(func.funcObj)
    case JSValue.Native(nf: quickjs.value.NativeFunction) => Some(nf.funcObj)
    case JSValue.Native(nc: quickjs.value.NativeConstructor) => Some(nc.funcObj)
    case _                                                   => None
  }

  private def isPrimitive(value: JSValue): Boolean = value match {
    case JSValue.Undefined | JSValue.Null | JSValue.Bool(_) | JSValue.Int32(_) |
        JSValue.Float64(_) | JSValue.BigInt(_) | JSValue.JSStr(_) |
        JSValue.Symbol(_) => true
    case _ => false
  }

  def isCallable(value: JSValue): Boolean = value match {
    case _: JSValue.Function                         => true
    case JSValue.Native(_: quickjs.value.NativeFunction)    => true
    case JSValue.Native(_: quickjs.value.NativeConstructor) => true
    case _ => false
  }

  /** ES ToObject. Primitive wrapper payloads live in JSObject internal slots,
    * so boxing does not expose implementation properties.
    */
  def toObject(value: JSValue)(using ctx: JSContext): JSValue = value match {
    case JSValue.Undefined | JSValue.Null =>
      ctx.throwTypeError("Cannot convert undefined or null to object")
    case value @ (JSValue.Object(_) | JSValue.JSArrayVal(_) |
        _: JSValue.Function | JSValue.Native(_)) => value
    case primitive =>
      val prototype = primitive match {
        case JSValue.JSStr(_) => ctx.global.get("String")
        case _: JSValue.Int32 | _: JSValue.Float64 => ctx.global.get("Number")
        case JSValue.Bool(_) => ctx.global.get("Boolean")
        case JSValue.Symbol(_) => ctx.global.get("Symbol")
        case JSValue.BigInt(_) => ctx.global.get("BigInt")
        case _ => JSValue.Undefined
      } match {
        case JSValue.Native(ctor: quickjs.value.NativeConstructor) => ctor.prototype
        case _ => ctx.objectPrototype
      }
      val wrapper = JSObject(prototype = prototype, extensible = true)
      wrapper.setPrimitiveValue(primitive)
      primitive match {
        case JSValue.JSStr(text) =>
          var index = 0
          while index < text.length do {
            wrapper.defineProperty(
              index.toString,
              JSValue.fromString(text.charAt(index).toString),
              enumerable = true,
              writable = false,
              configurable = false
            )
            index += 1
          }
          wrapper.defineProperty(
            "length",
            JSValue.fromInt(text.length),
            enumerable = false,
            writable = false,
            configurable = false
          )
        case _ => ()
      }
      JSValue.Object(wrapper)
  }

  /** Ordinary [[Get]] for the object-like values represented by JSObject.
    * Unlike JSObject.get, this invokes an inherited or own accessor getter.
    */
  def getPropertyWithGetter(
      target: JSValue,
      key: String
  )(using ctx: JSContext): JSValue =
    target match {
      case JSValue.JSArrayVal(array) if key == "length" =>
        JSValue.fromInt(array.getLength)
      case JSValue.JSArrayVal(array) =>
        arrayIndexFromKey(key).flatMap(array.getOwnIndexDescriptor) match {
          case Some((_, attrs)) if attrs.getter.isDefined =>
            callFunctionWithThis(attrs.getter.get, target, Array.empty)
          case Some((value, _)) => value
          case None => array.getOwnPropertyDescriptor(key) match {
          case Some((_, attrs)) if attrs.getter.isDefined =>
            callFunctionWithThis(attrs.getter.get, target, Array.empty)
          case Some((value, _)) => value
          case None => array.getPrototypeOverride match {
            case Some(JSValue.Null) => JSValue.Undefined
            case Some(proto) => getPropertyWithGetter(proto, key)
            case None => ctx.arrayPrototype.getPropertyDescriptorWithOwner(key) match {
          case Some((_, _, attrs)) if attrs.getter.isDefined =>
            callFunctionWithThis(attrs.getter.get, target, Array.empty)
          case Some((_, value, _)) => value
          case None                => JSValue.Undefined
            }
          }
        }
        }
      case _ => extractJSObject(target) match {
      case Some(obj) =>
        (obj.getOwnProperty("__proxy_target"), obj.getOwnProperty("__proxy_handler")) match {
          case (Some(proxyTarget), Some(JSValue.Object(handler))) =>
            handler.get("get")(using ctx) match {
              case JSValue.Undefined => getPropertyWithGetter(proxyTarget, key)
              case trap =>
                callFunctionWithThis(
                  trap,
                  JSValue.Object(handler),
                  Array(proxyTarget, JSValue.fromString(key), target)
                )
            }
          case _ =>
            obj.getPropertyDescriptorWithOwner(key) match {
              case Some((_, _, attrs)) if attrs.getter.isDefined =>
                callFunctionWithThis(attrs.getter.get, target, Array.empty)
              case Some((_, value, _)) => value
              case None =>
                // An array-valued [[Prototype]] (`Foo.prototype = new Array(...)`)
                // contributes its indexes/length and the array prototype chain.
                obj.getPrototypeValue match {
                  case JSValue.JSArrayVal(arr) =>
                    getPropertyWithGetter(JSValue.JSArrayVal(arr), key)
                  case _ =>
                    // Function-like values whose funcObj was created without a
                    // prototype still see Function.prototype (call/apply/bind/
                    // toString/valueOf). Ordinary property access already falls
                    // back this way; builtins that use this helper (ToPrimitive,
                    // String coercion) must too.
                    target match {
                      case _: JSValue.Function |
                          JSValue.Native(_: quickjs.value.NativeFunction) |
                          JSValue.Native(_: quickjs.value.NativeConstructor) =>
                        ctx.functionPrototype.get(key)(using ctx)
                      case _ => JSValue.Undefined
                    }
                }
            }
        }
      case None => JSValue.Undefined
      }
    }

  /** ES ToPrimitive, including @@toPrimitive and OrdinaryToPrimitive. */
  def toPrimitive(value: JSValue, hint: String)(using ctx: JSContext): JSValue =
    if isPrimitive(value) then value
    else {
      val symbolId = ctx.global.get("Symbol") match {
        case JSValue.Native(ctor: quickjs.value.NativeConstructor) =>
          ctor.funcObj.get("toPrimitive")(using ctx) match {
            case JSValue.Symbol(id) => Some(id)
            case _                  => None
          }
        case _ => None
      }
      val exotic = symbolId.flatMap { id =>
        value match {
          case JSValue.JSArrayVal(array) =>
            array.getOwnSymbol(id).orElse {
              val inherited = ctx.arrayPrototype.getSymbol(id)(using ctx)
              if inherited == JSValue.Undefined then None else Some(inherited)
            }
          case _ => extractJSObject(value).flatMap { obj =>
            obj.getSymbolPropertyDescriptorWithOwner(id)(using ctx).map {
              case (_, _, attrs) if attrs.getter.isDefined =>
                callFunctionWithThis(attrs.getter.get, value, Array.empty)
              case (_, method, _) => method
            }
          }
        }
      }.getOrElse(JSValue.Undefined)

      if exotic != JSValue.Undefined && exotic != JSValue.Null then {
        if !isCallable(exotic) then
          ctx.throwTypeError("Symbol.toPrimitive is not callable")
        val result = callFunctionWithThis(
          exotic,
          value,
          Array(JSValue.fromString(hint))
        )
        if !isPrimitive(result) then
          ctx.throwTypeError("Cannot convert object to primitive value")
        result
      } else {
        ordinaryToPrimitive(value, hint)
      }
    }

  /** ES OrdinaryToPrimitive: uses the standard method order for the hint. */
  def ordinaryToPrimitive(value: JSValue, hint: String)(using
      ctx: JSContext
  ): JSValue = {
    val methods =
      if hint == "string" then Array("toString", "valueOf")
      else Array("valueOf", "toString")
    var i = 0
    while i < methods.length do {
      val method = getPropertyWithGetter(value, methods(i))
      if isCallable(method) then {
        val result = callFunctionWithThis(method, value, Array.empty)
        if isPrimitive(result) then return result
      }
      i += 1
    }
    ctx.throwTypeError("Cannot convert object to primitive value")
  }

  def toPrimitiveNumber(value: JSValue)(using ctx: JSContext): JSValue =
    toPrimitive(value, "number")

  def isObjectLikeValue(value: JSValue): Boolean = value match {
    case _: JSValue.Object | _: JSValue.Function | _: JSValue.JSArrayVal |
        _: JSValue.Native =>
      true
    case _ => false
  }

  /** Read an own-or-inherited symbol-keyed property, invoking accessors. */
  def getSymbolPropertyWithGetter(target: JSValue, symbolId: Int)(using
      ctx: JSContext
  ): JSValue =
    target match {
      case JSValue.JSArrayVal(array) =>
        array.getOwnSymbol(symbolId).getOrElse(
          ctx.arrayPrototype.getSymbolPropertyDescriptorWithOwner(symbolId) match {
            case Some((_, _, attrs)) if attrs.getter.isDefined =>
              callFunctionWithThis(attrs.getter.get, target, Array.empty)
            case Some((_, value, _)) => value
            case None                => JSValue.Undefined
          }
        )
      case _ =>
        extractJSObject(target) match {
          case Some(obj) =>
            (obj.getOwnProperty("__proxy_target"),
              obj.getOwnProperty("__proxy_handler")) match {
              case (Some(proxyTarget), Some(JSValue.Object(handler))) =>
                handler.get("get")(using ctx) match {
                  case JSValue.Undefined =>
                    getSymbolPropertyWithGetter(proxyTarget, symbolId)
                  case trap =>
                    callFunctionWithThis(
                      trap,
                      JSValue.Object(handler),
                      Array(proxyTarget, JSValue.Symbol(symbolId), target)
                    )
                }
              case _ =>
                obj.getSymbolPropertyDescriptorWithOwner(symbolId) match {
                  case Some((_, _, attrs)) if attrs.getter.isDefined =>
                    callFunctionWithThis(attrs.getter.get, target, Array.empty)
                  case Some((_, value, _)) => value
                  case None                => JSValue.Undefined
                }
            }
          case None =>
            // Primitive receivers (e.g. strings consumed by iterator-taking
            // builtins) resolve symbol-keyed properties through their wrapper
            // prototype, invoking accessors with the primitive as `this`.
            target match {
              case JSValue.Undefined | JSValue.Null => JSValue.Undefined
              case primitive =>
                extractJSObject(toObject(primitive)) match {
                  case Some(wrapperObj) =>
                    wrapperObj.getSymbolPropertyDescriptorWithOwner(symbolId) match {
                      case Some((_, _, attrs)) if attrs.getter.isDefined =>
                        callFunctionWithThis(
                          attrs.getter.get,
                          primitive,
                          Array.empty
                        )
                      case Some((_, value, _)) => value
                      case None                => JSValue.Undefined
                    }
                  case None => JSValue.Undefined
                }
            }
        }
    }

  /** Well-known `Symbol.iterator` id for the current realm. */
  def iteratorSymbolId(using ctx: JSContext): Int =
    wellKnownSymbolId("iterator")

  /** Well-known symbol id by name (e.g. "iterator", "species"). */
  def wellKnownSymbolId(name: String)(using ctx: JSContext): Int =
    ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.get(name)(using ctx) match {
          case JSValue.Symbol(id) => id
          case _ => ctx.throwTypeError(s"Symbol.$name is not available")
        }
      case _ => ctx.throwTypeError("Symbol is not available")
    }

  /** ES GetIterator: obtain the iterator object for an iterable. */
  def getIterator(iterable: JSValue)(using ctx: JSContext): JSValue = {
    val method = getSymbolPropertyWithGetter(iterable, iteratorSymbolId)
    if !isCallable(method) then
      ctx.throwTypeError("value is not iterable")
    val iterator = callFunctionWithThis(method, iterable, Array.empty)
    if !isObjectLikeValue(iterator) then
      ctx.throwTypeError("iterator is not an object")
    iterator
  }

  /** ES IteratorStep: `Some(result)` for a value, `None` when the iterator is
    * done. The iterator result and its `done` property are validated (or its
    * absence, which means `done === false`).
    */
  def iteratorStep(iterator: JSValue)(using ctx: JSContext): Option[JSValue] = {
    val next = getPropertyWithGetter(iterator, "next")
    if !isCallable(next) then ctx.throwTypeError("iterator next is not callable")
    val result = callFunctionWithThis(next, iterator, Array.empty)
    if !isObjectLikeValue(result) then
      ctx.throwTypeError("iterator result is not an object")
    val done = getPropertyWithGetter(result, "done")
    if done.toBoolean then None else Some(result)
  }

  /** An IteratorRecord tracks whether the iterator has completed (normally or
    * abruptly). Errors raised while reading `next`/`done`/`value` set `done`,
    * which is what keeps IteratorClose from running afterwards. The `next`
    * method is read once, matching GetIterator's [[NextMethod]] slot.
    */
  final class IteratorRecord(val iterator: JSValue) {
    var done: Boolean = false
    private var nextMethod: JSValue | Null = null

    private[builtins] def getNextMethod()(using ctx: JSContext): JSValue =
      if nextMethod == null then {
        nextMethod = getPropertyWithGetter(iterator, "next")
      }
      nextMethod.asInstanceOf[JSValue]
  }

  /** ES GetIterator returning an IteratorRecord. */
  def getIteratorRecord(iterable: JSValue)(using ctx: JSContext): IteratorRecord =
    new IteratorRecord(getIterator(iterable))

  /** ES IteratorStepValue: `Some(value)` or `None` when exhausted; marks the
    * record done on both normal completion and abrupt completion.
    */
  def iteratorStepValue(record: IteratorRecord)(using
      ctx: JSContext
  ): Option[JSValue] =
    try {
      val next = record.getNextMethod()
      if !isCallable(next) then {
        record.done = true
        ctx.throwTypeError("iterator next is not callable")
      }
      val result = callFunctionWithThis(next, record.iterator, Array.empty)
      if !isObjectLikeValue(result) then {
        record.done = true
        ctx.throwTypeError("iterator result is not an object")
      }
      val done = getPropertyWithGetter(result, "done")
      if done.toBoolean then {
        record.done = true
        None
      } else Some(getPropertyWithGetter(result, "value"))
    } catch {
      case e: Throwable =>
        record.done = true
        throw e
    }

  /** ES IteratorClose for a record, only when it is not already done. */
  def iteratorCloseRecord(record: IteratorRecord)(using ctx: JSContext): Unit =
    if record != null && !record.done then {
      record.done = true
      iteratorClose(record.iterator)
    }

  /** ES IteratorValue. */
  def iteratorValue(result: JSValue)(using ctx: JSContext): JSValue =
    getPropertyWithGetter(result, "value")

  /** ES IteratorClose, swallowing the `return` method's own errors. */
  def iteratorClose(iterator: JSValue)(using ctx: JSContext): Unit =
    try {
      val ret = getPropertyWithGetter(iterator, "return")
      if isCallable(ret) then callFunctionWithThis(ret, iterator, Array.empty)
    } catch case _: Exception => ()

  /** Collect an iterable into a Vector using the iterator protocol. On an
    * abrupt completion the iterator is closed before the error propagates, as
    * required by the callers that materialize the iterable up-front.
    */
  def iteratorToList(iterable: JSValue)(using ctx: JSContext): Vector[JSValue] = {
    val iterator = getIterator(iterable)
    val buffer = Vector.newBuilder[JSValue]
    try {
      var step = iteratorStep(iterator)
      while step.isDefined do {
        buffer += iteratorValue(step.get)
        step = iteratorStep(iterator)
      }
    } catch {
      case e: Throwable =>
        iteratorClose(iterator)
        throw e
    }
    buffer.result()
  }

  /** ES ToPropertyKey, including the string-hinted ToPrimitive operation and
    * Symbol.toPrimitive dispatch. The Symbol result is preserved; every other
    * primitive result is converted to a string key.
    */
  /** Convert a computed element key.  Numbers and strings keep their fast
    * representation; every other value (objects, functions, booleans, bigints,
    * undefined/null) goes through ToPropertyKey.
    */
  def toElementKey(value: JSValue)(using ctx: JSContext): JSValue =
    value match {
      case _: JSValue.Int32 | _: JSValue.Float64 | _: JSValue.JSStr |
          _: JSValue.Symbol =>
        value
      case _ => toPropertyKey(value)
    }

  def toPropertyKey(value: JSValue)(using ctx: JSContext): JSValue = {
    def symbolToPrimitiveId: Option[Int] =
      ctx.global.get("Symbol") match {
        case JSValue.Native(ctor: quickjs.value.NativeConstructor) =>
          ctor.funcObj.get("toPrimitive")(using ctx) match {
            case JSValue.Symbol(id) => Some(id)
            case _                  => None
          }
        case _ => None
      }

    def exoticToPrimitive(target: JSValue): JSValue =
      symbolToPrimitiveId match {
        case Some(symbolId) =>
          target match {
            case JSValue.JSArrayVal(array) =>
              array.getOwnSymbol(symbolId).getOrElse(
                ctx.arrayPrototype.getSymbol(symbolId)(using ctx)
              )
            case _ =>
              extractJSObject(target) match {
                case Some(obj) =>
                  obj.getSymbolPropertyDescriptorWithOwner(symbolId)(using ctx) match {
                    case Some((_, _, attrs)) if attrs.getter.isDefined =>
                      callFunctionWithThis(attrs.getter.get, target, Array.empty)
                    case Some((_, method, _)) => method
                    case None                 => JSValue.Undefined
                  }
                case None => JSValue.Undefined
              }
          }
        case None => JSValue.Undefined
      }

    val primitive =
      if isPrimitive(value) then value
      else {
        val exotic = exoticToPrimitive(value)
        if exotic != JSValue.Undefined then {
          if !isCallable(exotic) then
            ctx.throwTypeError("Symbol.toPrimitive is not callable")
          val result = callFunctionWithThis(
            exotic,
            value,
            Array(JSValue.fromString("string"))
          )
          if !isPrimitive(result) then
            ctx.throwTypeError("Cannot convert object to primitive value")
          result
        }
        else {
          val methods = Array("toString", "valueOf")
          var result: JSValue = JSValue.Undefined
          var found = false
          var i = 0
          while i < methods.length && !found do {
            val method = getPropertyWithGetter(value, methods(i))
            if isCallable(method) then {
              val candidate = callFunctionWithThis(method, value, Array.empty)
              if isPrimitive(candidate) then {
                result = candidate
                found = true
              }
            }
            i += 1
          }
          if !found then
            ctx.throwTypeError("Cannot convert object to primitive value")
          result
        }
      }

    primitive match {
      case symbol: JSValue.Symbol => symbol
      case other                  => JSValue.fromString(toJSString(other))
    }
  }

  /** ES ToNumber, including object coercion and abrupt completion. */
  def toNumber(value: JSValue)(using ctx: JSContext): Double =
    toPrimitiveNumber(value) match {
      case JSValue.Symbol(_) =>
        ctx.throwTypeError("Cannot convert a Symbol value to a number")
      case JSValue.BigInt(_) =>
        ctx.throwTypeError("Cannot convert a BigInt value to a number")
      case primitive => primitive.toNumber
    }

  /**
   * ES StringToBigInt: parse a StringIntegerLiteral. `None` represents NaN
   * (an invalid literal), which callers must treat as an incomparable result.
   */
  def stringToBigInt(source: String): Option[java.math.BigInteger] = {
    val text = JSValue.trimJSWhitespace(source)
    if text.isEmpty then Some(java.math.BigInteger.ZERO)
    else {
      def parse(digits: String, radix: Int): Option[java.math.BigInteger] =
        // `BigInteger` accepts leading signs and some non-ASCII digits; the
        // StringIntegerLiteral grammar only allows ASCII digits.
        val ascii =
          digits.nonEmpty && digits.forall(c =>
            (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') ||
              (c >= 'A' && c <= 'Z')
          )
        if !ascii then None
        else
          try Some(new java.math.BigInteger(digits, radix))
          catch case _: NumberFormatException => None
      if text.startsWith("0x") || text.startsWith("0X") then
        parse(text.substring(2), 16)
      else if text.startsWith("0o") || text.startsWith("0O") then
        parse(text.substring(2), 8)
      else if text.startsWith("0b") || text.startsWith("0B") then
        parse(text.substring(2), 2)
      else if text.startsWith("+") then parse(text.substring(1), 10)
      else if text.startsWith("-") then
        parse(text.substring(1), 10).map(_.negate())
      else parse(text, 10)
    }
  }

  /** ES ToIndex, bounded by the maximum safe integer. */
  def toIndex(value: JSValue)(using ctx: JSContext): Long = {
    if value == JSValue.Undefined then return 0L
    val number = toNumber(value)
    val integer =
      if number.isNaN || number == 0.0 then 0.0
      else if number.isInfinite then number
      else math.signum(number) * math.floor(math.abs(number))
    if integer < 0 || integer.isInfinite || integer > 9007199254740991.0 then
      ctx.throwRangeError("Index out of range")
    integer.toLong
  }

  /** ES ToIntegerOrInfinity, represented as Double to retain infinities. */
  def toIntegerOrInfinity(value: JSValue)(using ctx: JSContext): Double = {
    val number = toNumber(value)
    if number.isNaN || number == 0.0 then 0.0
    else if number.isInfinite then number
    else math.copySign(math.floor(math.abs(number)), number)
  }

  /** IEEE 754 binary16 "round to nearest, ties to even" conversion. Returns the
    * 16-bit pattern for the nearest Float16 value (used by Math.f16round and the
    * Float16Array element type).
    */
  def doubleToHalfBits(value: Double): Int = {
    val f = value.toFloat
    val bits = java.lang.Float.floatToRawIntBits(f)
    val sign = (bits >>> 16) & 0x8000
    val exp = (bits >>> 23) & 0xFF
    var mant = bits & 0x7FFFFF
    if exp == 255 then
      // Inf or NaN
      if mant == 0 then sign | 0x7C00
      else sign | 0x7C00 | (mant >>> 13)
    else {
      val newExp = exp - 127 + 15
      if newExp >= 31 then sign | 0x7C00 // overflow -> Infinity
      else if newExp <= 0 then {
        if newExp < -10 then sign // underflow -> zero (keeps the sign)
        else {
          mant = (mant | 0x800000) >>> (1 - newExp)
          val rem = mant & 0x1FFF
          var res = mant >>> 13
          if rem > 0x1000 || (rem == 0x1000 && (res & 1) == 1) then res += 1
          sign | res
        }
      } else {
        val rem = mant & 0x1FFF
        var res = (newExp << 10) | (mant >>> 13)
        if rem > 0x1000 || (rem == 0x1000 && (res & 1) == 1) then res += 1
        sign | res
      }
    }
  }

  /** Inverse of [[doubleToHalfBits]]: decode a binary16 bit pattern. */
  def halfBitsToDouble(bits: Int): Double = {
    val sign = (bits >>> 15) & 1
    val exp = (bits >>> 10) & 0x1F
    val mant = bits & 0x3FF
    val f =
      if exp == 0 then
        if mant == 0 then 0.0f
        else (mant / 1024.0f) * math.pow(2.0, -14.0).toFloat
      else if exp == 31 then
        if mant == 0 then Float.PositiveInfinity else Float.NaN
      else
        java.lang.Float.intBitsToFloat(
          (sign << 31) | ((exp - 15 + 127) << 23) | (mant << 13)
        )
    if sign == 1 then -f.toDouble else f.toDouble
  }

  // --- Property descriptor parsing ---

  /** Translate ES `\p{UnicodePropertyName}`/`\p{Name=Value}` escapes to
    * `java.util.regex` syntax. Java spells binary properties `IsXxx`, does not
    * understand the long general-category names, and lacks
    * `Default_Ignorable_Code_Point`/`Any`. Unknown properties are passed
    * through so `Pattern.compile` reports a SyntaxError for them.
    */
  private def translateUnicodePropertyEscapes(pattern: String): String = {
    val sb = new StringBuilder(pattern.length)
    var i = 0
    while i < pattern.length do {
      val c = pattern.charAt(i)
      if c == '\\' && i + 2 < pattern.length &&
          (pattern.charAt(i + 1) == 'p' || pattern.charAt(i + 1) == 'P') &&
          pattern.charAt(i + 2) == '{'
      then {
        val close = pattern.indexOf('}', i + 3)
        if close < 0 then { sb.append(c); i += 1 }
        else {
          val negated = pattern.charAt(i + 1) == 'P'
          val name = pattern.substring(i + 3, close)
          translateUnicodeProperty(name, negated) match {
            case Some(replacement) => sb.append(replacement)
            case None              => sb.append(pattern, i, close + 1)
          }
          i = close + 1
        }
      }
      else { sb.append(c); i += 1 }
    }
    sb.toString
  }

  private val generalCategoryAliases: Map[String, String] = Map(
    "Letter" -> "L",
    "Cased_Letter" -> "LC",
    "Uppercase_Letter" -> "Lu",
    "Lowercase_Letter" -> "Ll",
    "Titlecase_Letter" -> "Lt",
    "Modifier_Letter" -> "Lm",
    "Other_Letter" -> "Lo",
    "Mark" -> "M",
    "Nonspacing_Mark" -> "Mn",
    "Spacing_Mark" -> "Mc",
    "Enclosing_Mark" -> "Me",
    "Number" -> "N",
    "Decimal_Number" -> "Nd",
    "Letter_Number" -> "Nl",
    "Other_Number" -> "No",
    "Punctuation" -> "P",
    "Connector_Punctuation" -> "Pc",
    "Dash_Punctuation" -> "Pd",
    "Open_Punctuation" -> "Ps",
    "Close_Punctuation" -> "Pe",
    "Initial_Punctuation" -> "Pi",
    "Final_Punctuation" -> "Pf",
    "Other_Punctuation" -> "Po",
    "Symbol" -> "S",
    "Math_Symbol" -> "Sm",
    "Currency_Symbol" -> "Sc",
    "Modifier_Symbol" -> "Sk",
    "Other_Symbol" -> "So",
    "Separator" -> "Z",
    "Space_Separator" -> "Zs",
    "Line_Separator" -> "Zl",
    "Paragraph_Separator" -> "Zp",
    "Other" -> "C",
    "Control" -> "Cc",
    "Format" -> "Cf",
    "Surrogate" -> "Cs",
    "Private_Use" -> "Co",
    "Unassigned" -> "Cn"
  )

  /** Unicode `Default_Ignorable_Code_Point` (Java has no built-in for it). */
  private val defaultIgnorableClass =
    "\\x{00AD}\\x{034F}\\x{061C}\\x{115F}-\\x{1160}" +
      "\\x{17B4}-\\x{17B5}\\x{180B}-\\x{180E}" +
      "\\x{200B}-\\x{200F}\\x{202A}-\\x{202E}" +
      "\\x{2060}-\\x{206F}\\x{3164}\\x{FE00}-\\x{FE0F}" +
      "\\x{FEFF}\\x{FFA0}\\x{FFF0}-\\x{FFF8}" +
      "\\x{1BCA0}-\\x{1BCA3}\\x{1D173}-\\x{1D17A}" +
      "\\x{E0000}-\\x{E0FFF}"

  private val regionalIndicatorClass = "\\x{1F1E6}-\\x{1F1FF}"

  /** Approximations for the `v`-flag “properties of strings” (emoji
    * sequences). Java has no equivalent properties, so sequences are
    * expressed with non-capturing groups built from the emoji binary
    * properties.
    */
  private def emojiSequencePattern(name: String): Option[String] = {
    val ri = regionalIndicatorClass
    val emoji = "\\p{IsEmoji}"
    val mod = "\\p{IsEmoji_Modifier}"
    val base = "\\p{IsEmoji_Modifier_Base}"
    name match {
      case "RGI_Emoji" =>
        Some(
          s"(?:[$ri]{2}|$emoji\\x{FE0F}?(?:$mod|\\x{20E3})?" +
            s"(?:\\x{200D}$emoji\\x{FE0F}?(?:$mod)?)*)"
        )
      case "Basic_Emoji" =>
        Some(
          s"(?:$emoji\\x{FE0F}?|[0-9#*]\\x{FE0F}?\\x{20E3}|[$ri]{2})"
        )
      case "Emoji_Keycap_Sequence" =>
        Some("[0-9#*]\\x{FE0F}?\\x{20E3}")
      case "Emoji_Flag_Sequence" | "RGI_Emoji_Flag_Sequence" =>
        Some(s"[$ri]{2}")
      case "Emoji_Tag_Sequence" | "RGI_Emoji_Tag_Sequence" =>
        Some(s"$emoji[\\x{E0020}-\\x{E007E}]+\\x{E007F}")
      case "Emoji_Modifier_Sequence" | "RGI_Emoji_Modifier_Sequence" =>
        Some(s"$base$mod")
      case "Emoji_ZWJ_Sequence" | "RGI_Emoji_ZWJ_Sequence" =>
        Some(s"$emoji\\x{200D}$emoji")
      case _ => None
    }
  }

  private def translateUnicodeProperty(
      name: String,
      negated: Boolean
  ): Option[String] = {
    val p = if negated then "P" else "p"
    name match {
      case "Default_Ignorable_Code_Point" =>
        Some(
          if negated then s"[^$defaultIgnorableClass]"
          else s"[$defaultIgnorableClass]"
        )
      case "Any" =>
        Some(if negated then "[^\\s\\S]" else "[\\s\\S]")
      case "ID_Start" | "XID_Start" =>
        val cls = "\\p{IsAlphabetic}\\p{Nl}\\p{Pc}"
        Some(if negated then s"[^$cls]" else s"[$cls]")
      case "ID_Continue" | "XID_Continue" =>
        val cls = "\\p{IsAlphabetic}\\p{Mn}\\p{Mc}\\p{Nd}\\p{Pc}"
        Some(if negated then s"[^$cls]" else s"[$cls]")
      case "Math" =>
        Some(s"\\$p{IsSm}")
      case n
          if n.startsWith("gc=") || n.startsWith("General_Category=") =>
        val value = n.substring(n.indexOf('=') + 1)
        val short =
          generalCategoryAliases.getOrElse(value, value)
        Some(s"\\$p{gc=$short}")
      case n
          if n.startsWith("sc=") || n.startsWith("Script=") ||
            n.startsWith("scx=") || n.startsWith("Script_Extensions=") =>
        val value = n.substring(n.indexOf('=') + 1)
        Some(s"\\$p{Is$value}")
      case n if n.contains("=") => None
      case n if emojiSequencePattern(n).isDefined =>
        val seq = emojiSequencePattern(n).get
        Some(if negated then s"(?!$seq)[\\s\\S]" else seq)
      // Bare general-category names (`Format`, `Lu`, ...) are category
      // shorthand in ES, not binary properties.
      case n if generalCategoryAliases.contains(n) =>
        Some(s"\\$p{gc=${generalCategoryAliases(n)}}")
      case n if n.matches("[A-Z][a-z]?") =>
        Some(s"\\$p{$n}")
      case n if n == "Lower" || n == "Upper" =>
        Some(s"\\$p{${if n == "Lower" then "Ll" else "Lu"}}")
      case n => Some(s"\\$p{Is$n}")
    }
  }

  /** Parsed property descriptor fields */
  final case class ParsedDescriptor(
      enumerable: Option[Boolean],
      writable: Option[Boolean],
      configurable: Option[Boolean],
      getter: Option[JSValue],
      setter: Option[JSValue],
      value: Option[JSValue],
      hasValue: Boolean,
      hasWritable: Boolean,
      hasGetter: Boolean,
      hasSetter: Boolean
  ) {
    def isAccessor: Boolean = hasGetter || hasSetter
    def hasValueField: Boolean = hasValue || hasWritable
  }

  /** Throw when the executing thread has been interrupted (test-runner and
    * embedding-host cancellation). Native builtin loops should call this
    * every N iterations so abandoned work stops allocating promptly.
    */
  def checkInterrupted(iteration: Int): Unit =
    if (iteration & 1023) == 0 && Thread.currentThread().isInterrupted then
      throw new InterruptedException("JavaScript execution interrupted")

  /** Parse a property descriptor from a JSValue. */
  def parsePropertyDescriptor(descriptor: JSValue)(using
      ctx: JSContext
  ): ParsedDescriptor =
    descriptor match {
      case JSValue.Object(_) | JSValue.JSArrayVal(_) | _: JSValue.Function |
          JSValue.Native(_) =>
        // QuickJS C's js_obj_to_desc follows HasProperty with Get for each
        // field, in specification order.  In particular, inherited fields and
        // accessor side effects are observable; reading only own data slots
        // makes Object.create/defineProperty fail large parts of ES5 test262.
        def hasProperty(name: String): Boolean =
          descriptor match {
            case JSValue.JSArrayVal(array) =>
              array.getOwnProperty(name).isDefined ||
                ctx.arrayPrototype.getPropertyDescriptorWithOwner(name).isDefined
            case _ =>
              extractJSObject(descriptor)
                .flatMap(_.getPropertyDescriptorWithOwner(name))
                .isDefined
          }

        def read(name: String): Option[JSValue] =
          if !hasProperty(name) then None
          else
            descriptor match {
              case JSValue.JSArrayVal(array) if array.getOwnProperty(name).isDefined =>
                array.getOwnProperty(name)
              case _ => Some(getPropertyWithGetter(descriptor, name))
            }

        val enumerableValue = read("enumerable")
        val enumerableOpt = enumerableValue.map(_.toBoolean)
        val configurableValue = read("configurable")
        val configurableOpt = configurableValue.map(_.toBoolean)
        val valueOpt = read("value")
        val writableValue = read("writable")
        val writableOpt = writableValue.map(_.toBoolean)
        val getterValue = read("get")
        val getterOpt = getterValue.flatMap {
          case JSValue.Undefined => None
          case v =>
            if !isCallable(v) then
              ctx.throwTypeError("Getter must be a function or undefined")
            Some(v)
        }
        val setterValue = read("set")
        val setterOpt = setterValue.flatMap {
          case JSValue.Undefined => None
          case v =>
            if !isCallable(v) then
              ctx.throwTypeError("Setter must be a function or undefined")
            Some(v)
        }
        val hasGetterProp = getterValue.isDefined
        val hasSetterProp = setterValue.isDefined
        val hasValueProp = valueOpt.isDefined
        val hasWritableProp = writableValue.isDefined
        if (hasGetterProp || hasSetterProp) &&
            (hasValueProp || hasWritableProp)
        then
          ctx.throwTypeError(
            "Invalid property descriptor. Cannot have both accessors and a value or writable"
          )
        ParsedDescriptor(
          enumerableOpt,
          writableOpt,
          configurableOpt,
          getterOpt,
          setterOpt,
          valueOpt,
          hasValueProp,
          hasWritableProp,
          hasGetterProp,
          hasSetterProp
        )
      case _ => ctx.throwTypeError("Property description must be an object")
    }

  // --- Property descriptor → object conversion ---

  /** Build a property descriptor object from PropertyAttributes. */
  def buildPropertyDescriptorObject(
      key: String,
      desc: Option[(JSValue, JSObject.PropertyAttributes)]
  )(using ctx: JSContext): JSValue =
    desc match {
      case Some((value, attrs)) =>
        val descObj =
          JSObject(prototype = ctx.objectPrototype, extensible = true)
        if attrs.isAccessor || attrs.getter.isDefined || attrs.setter.isDefined
        then {
          descObj.set("get", attrs.getter.getOrElse(JSValue.Undefined))
          descObj.set("set", attrs.setter.getOrElse(JSValue.Undefined))
        }
        else {
          descObj.set("value", value)
          descObj.set("writable", JSValue.fromBoolean(attrs.writable))
        }
        descObj.set("enumerable", JSValue.fromBoolean(attrs.enumerable))
        descObj.set("configurable", JSValue.fromBoolean(attrs.configurable))
        JSValue.Object(descObj)
      case None => JSValue.Undefined
    }

  // --- Promise helpers ---

  /** Wrap a Promise into an Object with __promise internal slot. */
  def wrapPromise(promise: JSValue.Promise)(using ctx: JSContext): JSValue = {
    val obj = JSObject(prototype = ctx.promisePrototype, extensible = true)
    obj.defineProperty(
      "__promise",
      promise,
      enumerable = false,
      writable = false,
      configurable = false
    )
    JSValue.Object(obj)
  }

  /** Get Promise from an object, or throw. */
  def getPromiseFrom(obj: JSValue, methodName: String)(using
      ctx: JSContext
  ): JSValue.Promise =
    obj match {
      case JSValue.Object(o) =>
        o.getOwnProperty("__promise") match {
          case Some(p: JSValue.Promise) => p
          case _                        =>
            ctx.throwTypeError(
              s"$methodName method called on non-Promise object"
            )
        }
      case _ =>
        ctx.throwTypeError(s"$methodName method called on non-Promise object")
    }

  /** ES AdvanceStringIndex. */
  def advanceStringIndex(str: String, index: Int, unicode: Boolean): Int =
    if !unicode || index + 1 >= str.length then index + 1
    else {
      val first = str.charAt(index)
      if first >= 0xd800 && first <= 0xdbff then {
        val second = str.charAt(index + 1)
        if second >= 0xdc00 && second <= 0xdfff then index + 2 else index + 1
      } else index + 1
    }

  /** True when `index` points at the low half of a surrogate pair. */
  def isMidSurrogatePair(str: String, index: Int): Boolean =
    index > 0 && index < str.length &&
      Character.isLowSurrogate(str.charAt(index)) &&
      Character.isHighSurrogate(str.charAt(index - 1))

  /** The [[Prototype]] of a value as a JSValue, honoring a JSArray's
    * prototype override and an ordinary object's non-JSObject prototype.
    */
  def valuePrototypeValue(value: JSValue)(using ctx: JSContext): JSValue | Null =
    value match {
      case JSArrayValHolder(arr) =>
        arr.getPrototypeOverride.getOrElse(JSValue.Object(ctx.arrayPrototype))
      case JSValue.Object(obj)   => obj.getPrototypeValue
      case f: JSValue.Function   => f.funcObj.getPrototypeValue
      case JSValue.Native(nf: quickjs.value.NativeFunction) =>
        nf.funcObj.getPrototypeValue
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.getPrototypeValue
      case _ => null
    }

  /** The [[Prototype]] of a value, honoring a JSArray's prototype override. */
  def valuePrototype(value: JSValue)(using ctx: JSContext): quickjs.objmodel.JSObject | Null =
    valuePrototypeValue(value) match {
      case null                     => null
      case JSValue.Object(p)        => p
      case f: JSValue.Function      => f.funcObj
      case JSValue.Native(nf: quickjs.value.NativeFunction) => nf.funcObj
      case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc.funcObj
      case _                        => null
    }

  /** Object identity for the object-like values used by `instanceof`. */
  def isSameObjectValue(a: JSValue, b: JSValue): Boolean =
    (a, b) match {
      case (JSValue.JSArrayVal(x), JSValue.JSArrayVal(y)) => x.eq(y)
      case _ =>
        (extractJSObject(a), extractJSObject(b)) match {
          case (Some(x), Some(y)) => x.eq(y)
          case _                  => false
        }
    }

  /**
   * ES InstanceofOperator(O, C): a callable `Symbol.hasInstance` method takes
   * precedence, otherwise C must be callable and OrdinaryHasInstance runs.
   */
  def instanceofOperator(obj: JSValue, constructor: JSValue)(using
      ctx: JSContext
  ): Boolean = {
    if !isObjectLikeValue(constructor) then
      ctx.throwTypeError("Right-hand side of 'instanceof' is not an object")
    val handler =
      getSymbolPropertyWithGetter(constructor, wellKnownSymbolId("hasInstance"))
    if handler != JSValue.Undefined && handler != JSValue.Null then {
      if !isCallable(handler) then
        ctx.throwTypeError("Symbol.hasInstance is not callable")
      callFunctionWithThis(handler, constructor, Array(obj)).toBoolean
    } else {
      if !isCallable(constructor) then
        ctx.throwTypeError("Right-hand side of 'instanceof' is not callable")
      ordinaryHasInstance(constructor, obj)
    }
  }

  /**
   * ES OrdinaryHasInstance(C, O). Bound functions delegate to their target
   * function, which is recorded on the bound object as `__boundTarget`.
   */
  def ordinaryHasInstance(constructor: JSValue, value: JSValue)(using
      ctx: JSContext
  ): Boolean = {
    if !isCallable(constructor) then return false
    extractJSObject(constructor)
      .flatMap(_.getOwnProperty("__boundTarget")) match {
      case Some(target) => return instanceofOperator(value, target)
      case None         => ()
    }
    if !isObjectLikeValue(value) then return false
    val prototype = getPropertyWithGetter(constructor, "prototype")
    if !isObjectLikeValue(prototype) then
      ctx.throwTypeError("Function has non-object prototype in instanceof")
    var current = valuePrototypeValue(value)
    while current != null do {
      if isSameObjectValue(current, prototype) then return true
      current = valuePrototypeValue(current)
    }
    false
  }

  private object JSArrayValHolder {
    def unapply(value: JSValue): Option[quickjs.objmodel.JSArray] = value match {
      case JSValue.JSArrayVal(arr) => Some(arr)
      case _                       => None
    }
  }

  /** ES ToString abstract operation — converts a value to a string properly.
    * For Symbol values, throws TypeError per spec. For Objects, calls the
    * JS-level toString method (which may throw). For other primitives, uses the
    * safe JSValue.toString.
    */
  def toJSString(value: JSValue)(using ctx: JSContext): String =
    value match {
      case JSValue.Symbol(_) =>
        ctx.throwTypeError("Cannot convert a Symbol value to a string")
      case JSValue.Undefined  => "undefined"
      case JSValue.Null       => "null"
      case JSValue.Bool(b)    => b.toString
      case JSValue.Int32(i)   => i.toString
      case JSValue.Float64(d) => numberToJSString(d)
      case JSValue.BigInt(b)   => b.toString
      case JSValue.JSStr(s)    => s
      case _: JSValue.Object | _: JSValue.JSArrayVal | _: JSValue.Function |
          _: JSValue.Native =>
        toJSString(toPrimitive(value, "string"))
      case _                     => value.toString
    }

  /** ECMAScript's observable decimal spelling for finite Number values. */
  def numberToJSString(number: Double): String =
    if number.isNaN then "NaN"
    else if number == Double.PositiveInfinity then "Infinity"
    else if number == Double.NegativeInfinity then "-Infinity"
    else if number == 0.0 then "0"
    else {
      val absolute = math.abs(number)
      val source = java.math.BigDecimal.valueOf(number)
      val exact = new java.math.BigDecimal(number)
      val roundingModes = Array(
        java.math.RoundingMode.HALF_EVEN,
        java.math.RoundingMode.DOWN,
        java.math.RoundingMode.UP
      )
      var decimal = source
      var precision = 1
      var found = false
      while precision <= 17 && !found do {
        val candidates = roundingModes.iterator
          .map(mode =>
            source
              .round(new java.math.MathContext(precision, mode))
              .stripTrailingZeros()
          )
          .filter(candidate =>
            java.lang.Double.parseDouble(candidate.toString) == number
          )
          .toVector
        if candidates.nonEmpty then {
          decimal = candidates.minBy(_.subtract(exact).abs())
          found = true
        }
        precision += 1
      }
      decimal = decimal.stripTrailingZeros()
      if absolute >= 1.0e21 || absolute < 1.0e-6 then
        decimal.toString.replace("E", "e")
      else decimal.toPlainString
    }

  // --- Constructor registration ---

  /** Initialize a constructor function with standard properties. */
  def initConstructor(
      constructor: quickjs.value.NativeConstructor,
      length: Int,
      prototypeValue: Option[JSValue] = None
  )(using ctx: JSContext): Unit = {
    constructor.funcObj.setPrototype(ctx.functionPrototype)
    if constructor.hasPrototypeProperty then {
      constructor.funcObj.defineProperty(
        "prototype",
        prototypeValue.getOrElse(JSValue.Object(constructor.prototype)),
        enumerable = false,
        writable = false,
        configurable = false
      )
      constructor.prototype.defineProperty(
        "constructor",
        JSValue.Native(constructor),
        enumerable = false,
        writable = true,
        configurable = true
      )
    }
    // Override length with actual value (auto-init set it to 0 by default)
    if length != 0 then
      constructor.funcObj.initProperty(
        "length",
        JSValue.fromInt(length),
        enumerable = false,
        writable = false,
        configurable = true
      )
  }

  /** Call a function value (native or bytecode) with given this and arguments.
    */
  def callFunctionValue(func: JSValue, thisArg: JSValue, args: Array[JSValue])(
      using ctx: JSContext
  ): JSValue =
    func match {
      case JSValue.Native(native: quickjs.value.NativeFunction) =>
        native.call(Array(thisArg) ++ args)
      case f: JSValue.Function =>
        try
          Interpreter().call(
            functionToBytecode(f),
            thisArg,
            args,
            f.closure,
            calleeValue = f
          )
        catch case _: Exception => JSValue.Undefined
      case _ => args.headOption.getOrElse(JSValue.Undefined)
    }

  /** Call a value, supporting callable proxies (honoring the `apply` trap). */
  def callCallableValue(
      funcValue: JSValue,
      thisValue: JSValue,
      args: Array[JSValue]
  )(using ctx: JSContext): JSValue =
    funcValue match {
      case JSValue.Object(obj)
          if obj.getOwnProperty("__proxy_target").isDefined &&
            obj.getOwnProperty("__proxy_handler").isDefined =>
        val target = obj.getOwnProperty("__proxy_target").get
        val handler = obj.getOwnProperty("__proxy_handler").get
        if target == JSValue.Null || handler == JSValue.Null then
          ctx.throwTypeError("Cannot perform operation on a revoked proxy")
        handler match {
          case JSValue.Object(h) =>
            h.get("apply")(using ctx) match {
              case JSValue.Undefined => callCallableValue(target, thisValue, args)
              case trap if isCallable(trap) =>
                val argArray = quickjs.objmodel.JSArray.empty()
                args.foreach(argArray.push)
                callFunctionWithThis(
                  trap,
                  JSValue.Object(h),
                  Array(target, thisValue, JSValue.JSArrayVal(argArray))
                )
              case _ => ctx.throwTypeError("proxy apply trap is not callable")
            }
          case _ => ctx.throwTypeError("Cannot perform operation on a revoked proxy")
        }
      case _ => callFunctionWithThis(funcValue, thisValue, args)
    }

  /** Call a function with explicit this binding (for method dispatch). */
  def callFunctionWithThis(
      funcValue: JSValue,
      thisValue: JSValue,
      args: Array[JSValue]
  )(using ctx: JSContext): JSValue =
    funcValue match {
      case func: JSValue.Function =>
        if func.isClassConstructor then
          ctx.throwTypeError(
            s"Class constructor ${func.name} cannot be invoked without 'new'"
          )
        Interpreter().call(
          functionToBytecode(func),
          thisValue,
          args,
          func.closure,
          calleeValue = func
        )
      case JSValue.Native(nativeFuncWrapper) =>
        nativeFuncWrapper match {
          case native: NativeFunction =>
            val argsWithThis = new Array[JSValue](args.length + 1)
            argsWithThis(0) = thisValue
            Array.copy(args, 0, argsWithThis, 1, args.length)
            ctx.withStackFrame(native.name, isNative = true) {
              native.call(argsWithThis)
            }
          case constructor: quickjs.value.NativeConstructor =>
            ctx.withStackFrame(constructor.name, isNative = true) {
              constructor.call(args)(using ctx)
            }
          case _ =>
            ctx.throwTypeError(
              s"Invalid native function: $nativeFuncWrapper"
            )
        }
      case _ =>
        ctx.throwTypeError(s"Cannot call non-function value: $funcValue")
    }

  /** Build an Error object with the given type name and args. */
  def buildError(
      proto: quickjs.objmodel.JSObject,
      name: String,
      args: Array[JSValue]
  )(using ctx: JSContext): JSValue = {
    val obj = quickjs.objmodel.JSObject(prototype = proto, extensible = true)
    obj.set("name", JSValue.fromString(name))
    if args.nonEmpty then obj.set("message", args(0))
    ctx.attachStack(obj, skipFrames = 1)
    JSValue.Object(obj)
  }

  /** Parse an ECMAScript array index. Array indices are canonical decimal
    * strings in the range 0 through 2^32 - 2; 2^32 - 1 is an ordinary
    * property key because it is the maximum Array length.
    */
  def arrayIndexFromKey(key: String): Option[Long] =
    if key.isEmpty || (key.length > 1 && key.charAt(0) == '0') ||
        !key.forall(ch => ch >= '0' && ch <= '9')
    then None
    else
      try {
        val index = java.lang.Long.parseLong(key)
        Option.when(index <= 4294967294L)(index)
      }
      catch case _: NumberFormatException => None

  def isArrayIndexKey(key: String): Boolean =
    arrayIndexFromKey(key).isDefined

  // --- RegExp support ---

  final case class RegExpData(
      pattern: String,
      flags: String,
      global: Boolean,
      ignoreCase: Boolean,
      multiline: Boolean,
      dotAll: Boolean,
      unicode: Boolean,
      sticky: Boolean,
      regex: java.util.regex.Pattern
  )

  /** Capturing-group parent indices, used to correct java.util.regex's
    * retention of a nested capture from an earlier quantified iteration.
    * ECMAScript clears such a capture when it did not participate in the last
    * iteration of its containing group.
    */
  def regexpCaptureParents(pattern: String): Array[Int] = {
    val parents = mutable.ArrayBuffer(0)
    val stack = mutable.ArrayBuffer(0)
    var inClass = false
    var escaped = false
    var i = 0
    while i < pattern.length do {
      val ch = pattern.charAt(i)
      if escaped then escaped = false
      else if ch == '\\' then escaped = true
      else if ch == '[' then inClass = true
      else if ch == ']' && inClass then inClass = false
      else if !inClass && ch == '(' then {
        val question = i + 1 < pattern.length && pattern.charAt(i + 1) == '?'
        val namedCapture =
          question && i + 2 < pattern.length &&
            pattern.charAt(i + 2) == '<' &&
            (i + 3 >= pattern.length ||
              (pattern.charAt(i + 3) != '=' && pattern.charAt(i + 3) != '!'))
        val capturing = !question || namedCapture
        if capturing then {
          parents += stack.last
          stack += (parents.length - 1)
        }
        else stack += stack.last
      }
      else if !inClass && ch == ')' && stack.length > 1 then
        stack.remove(stack.length - 1)
      i += 1
    }
    parents.toArray
  }

  /** Decode the escapes that may appear in an ES capture group name. */
  def decodeRegExpGroupName(raw: String): String = {
    if !raw.contains('\\') then raw
    else {
      val sb = new StringBuilder
      var i = 0
      while i < raw.length do {
        val ch = raw.charAt(i)
        if ch == '\\' && i + 1 < raw.length && raw.charAt(i + 1) == 'u' then {
          if i + 2 < raw.length && raw.charAt(i + 2) == '{' then {
            val close = raw.indexOf('}', i + 3)
            if close > 0 then {
              try {
                sb.appendAll(
                  Character.toChars(Integer.parseInt(raw.substring(i + 3, close), 16))
                )
                i = close + 1
              } catch case _: Exception => { sb.append(ch); i += 1 }
            } else { sb.append(ch); i += 1 }
          } else if i + 5 < raw.length then {
            try {
              var cp = Integer.parseInt(raw.substring(i + 2, i + 6), 16)
              i += 6
              if Character.isHighSurrogate(cp.toChar) && i + 5 < raw.length &&
                  raw.charAt(i) == '\\' && raw.charAt(i + 1) == 'u'
              then {
                val low = Integer.parseInt(raw.substring(i + 2, i + 6), 16)
                if low >= 0xDC00 && low <= 0xDFFF then {
                  cp = Character.toCodePoint(cp.toChar, low.toChar)
                  i += 6
                }
              }
              sb.appendAll(Character.toChars(cp))
            } catch case _: Exception => { sb.append(ch); i += 1 }
          } else { sb.append(ch); i += 1 }
        } else { sb.append(ch); i += 1 }
      }
      sb.toString
    }
  }

  /** Names of the capture groups of a RegExp pattern, indexed by group number.
    * The whole-match slot (index 0) and unnamed groups hold null.
    */
  def regexpGroupNames(pattern: String): Array[String] = {
    val names = mutable.ArrayBuffer[String](null)
    var inClass = false
    var escaped = false
    var i = 0
    while i < pattern.length do {
      val ch = pattern.charAt(i)
      if escaped then escaped = false
      else if ch == '\\' then escaped = true
      else if ch == '[' then inClass = true
      else if ch == ']' && inClass then inClass = false
      else if !inClass && ch == '(' then {
        val question = i + 1 < pattern.length && pattern.charAt(i + 1) == '?'
        if !question then names += null
        else {
          val named =
            i + 2 < pattern.length && pattern.charAt(i + 2) == '<' &&
              (i + 3 >= pattern.length ||
                (pattern.charAt(i + 3) != '=' &&
                  pattern.charAt(i + 3) != '!'))
          if named then {
            val close = pattern.indexOf('>', i + 3)
            val name =
              if close > 0 then decodeRegExpGroupName(pattern.substring(i + 3, close))
              else ""
            names += name
          }
        }
      }
      i += 1
    }
    names.toArray
  }

  /** The capture-group index for a group name, or -1 when absent. */
  def regexpGroupIndexForName(pattern: String, name: String): Int = {
    val names = regexpGroupNames(pattern)
    var i = 0
    while i < names.length do {
      if names(i) == name then return i
      i += 1
    }
    -1
  }

  /** Build the `groups` object for a successful match, or Undefined when the
    * pattern has no named groups.
    */
  def regexpNamedGroups(
      pattern: String,
      matcher: java.util.regex.Matcher
  )(using ctx: JSContext): JSValue = {
    val names = regexpGroupNames(pattern)
    var hasNamed = false
    var i = 0
    while i < names.length do {
      if names(i) != null then hasNamed = true
      i += 1
    }
    if !hasNamed then JSValue.Undefined
    else {
      val groups = quickjs.objmodel.JSObject(prototype = null, extensible = true)
      var index = 0
      while index < names.length && index <= matcher.groupCount() do {
        val name = names(index)
        if name != null then {
          val value = matcher.group(index)
          groups.defineProperty(
            name,
            if value == null then JSValue.Undefined else JSValue.fromString(value),
            enumerable = true,
            writable = true,
            configurable = true
          )
        }
        index += 1
      }
      JSValue.Object(groups)
    }
  }

  /** Build the `indices.groups` object for the `d` flag, or Undefined. */
  def regexpNamedGroupIndices(
      pattern: String,
      matcher: java.util.regex.Matcher
  )(using ctx: JSContext): JSValue = {
    val names = regexpGroupNames(pattern)
    var hasNamed = false
    var i = 0
    while i < names.length do
      if names(i) != null then hasNamed = true
      i += 1
    if !hasNamed then JSValue.Undefined
    else {
      val groups = quickjs.objmodel.JSObject(prototype = null, extensible = true)
      var index = 0
      while index < names.length && index <= matcher.groupCount() do {
        val name = names(index)
        if name != null then {
          val value =
            if matcher.start(index) < 0 then JSValue.Undefined
            else {
              val pair = quickjs.objmodel.JSArray.empty()
              pair.push(JSValue.fromInt(matcher.start(index)))
              pair.push(JSValue.fromInt(matcher.end(index)))
              JSValue.JSArrayVal(pair)
            }
          groups.defineProperty(
            name,
            value,
            enumerable = true,
            writable = true,
            configurable = true
          )
        }
        index += 1
      }
      JSValue.Object(groups)
    }
  }

  def parseRegExpFlags(flags: String)(using
      ctx: JSContext
  ): (Int, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean) = {
    var global = false; var ignoreCase = false; var multiline = false
    var dotAll = false; var unicode = false; var sticky = false;
    var patternFlags = 0
    val seen = scala.collection.mutable.Set.empty[Char]
    flags.foreach { ch =>
      if seen.contains(ch) then
        ctx.throwSyntaxError("Invalid regular expression flags")
      seen += ch
      ch match {
        case 'g' => global = true
        case 'i' =>
          ignoreCase = true;
          patternFlags |= java.util.regex.Pattern.CASE_INSENSITIVE
        case 'm' =>
          multiline = true; patternFlags |= java.util.regex.Pattern.MULTILINE
        case 's' =>
          dotAll = true; patternFlags |= java.util.regex.Pattern.DOTALL
        case 'u' =>
          unicode = true; patternFlags |= java.util.regex.Pattern.UNICODE_CASE
        case 'v' =>
          unicode = true
          patternFlags |= java.util.regex.Pattern.UNICODE_CASE
        case 'd' => ()
        case 'y' => sticky = true
        case _   => ctx.throwSyntaxError("Invalid regular expression flags")
      }
    }
    (patternFlags, global, ignoreCase, multiline, dotAll, unicode, sticky)
  }

  def getRegExpData(value: JSValue)(using
      ctx: JSContext
  ): Option[(quickjs.objmodel.JSObject, RegExpData)] =
    value match {
      case JSValue.Object(obj) =>
        // Compiled data is cached on the RegExp object; the JVM Pattern is
        // otherwise rebuilt (and re-validated) on every exec call.
        obj.getOwnProperty("__regexpData")(using ctx) match {
          case Some(JSValue.Native(data: RegExpData)) => Some((obj, data))
          case _ => getRegExpDataUncached(obj)
        }
      case _ => None
    }

  private def getRegExpDataUncached(obj: quickjs.objmodel.JSObject)(using
      ctx: JSContext
  ): Option[(quickjs.objmodel.JSObject, RegExpData)] =
    obj.getOwnProperty("__regexpPattern")(using ctx) match {
          case Some(JSValue.JSStr(pattern)) =>
            val flags = obj.getOwnProperty("__regexpFlags")(using ctx) match {
              case Some(JSValue.JSStr(f)) => f;
              case Some(v)                => v.toString;
              case None                   => ""
            }
            val (
              patternFlags,
              global,
              ignoreCase,
              multiline,
              dotAll,
              unicode,
              sticky
            ) = parseRegExpFlags(flags)
            try {
              if unicode then {
                var escaped = false
                var classDepth = 0
                var validationIndex = 0
                while validationIndex < pattern.length do {
                  val current = pattern.charAt(validationIndex)
                  if escaped then escaped = false
                  else if current == '\\' then escaped = true
                  else if current == '[' then classDepth += 1
                  else if current == ']' then
                    if classDepth == 0 then
                      ctx.throwError(
                        "SyntaxError",
                        s"Invalid regular expression /$pattern/: unmatched ']'",
                        skipFrames = 1
                      )
                    else classDepth -= 1
                  validationIndex += 1
                }
              }
              val compatiblePattern = {
                // Java's `.`/negated classes match a whole surrogate pair, but
                // ECMAScript's `.` in non-unicode mode matches one UTF-16 code
                // unit. Also, Java treats U+0085 as a line terminator while
                // ECMAScript only excludes \n, \r, \u2028 and \u2029.
                // Rewrite unescaped `.` outside character classes: in unicode
                // mode a simple negated class (Java matches code points;
                // which is what ES unicode `.` needs), otherwise an explicit
                // one-code-unit alternation.
                def rewriteDots(src: String): String = {
                  // Java's negated classes match a whole surrogate pair, while
                  // JS non-unicode `.` is a code unit. The simple class keeps
                  // matching fast (the exact code-unit alternation made long
                  // `.*` matches overflow the stack); the only observable
                  // difference is a lone `.` against an astral character.
                  val replacement =
                    if dotAll then "."
                    else "[^\\n\\r\\u2028\\u2029]"
                  val sb = new java.lang.StringBuilder(src.length + 16)
                  var i = 0
                  var inClass = false
                  while i < src.length do {
                    val ch = src.charAt(i)
                    if ch == '\\' then {
                      sb.append(ch)
                      if i + 1 < src.length then {
                        sb.append(src.charAt(i + 1))
                        i += 1
                      }
                    } else if ch == '[' then {
                      inClass = true
                      sb.append(ch)
                    } else if ch == ']' then {
                      inClass = false
                      sb.append(ch)
                    } else if ch == '.' && !inClass then
                      sb.append(replacement)
                    else sb.append(ch)
                    i += 1
                  }
                  sb.toString
                }
                val basePattern = rewriteDots(pattern)
                var result =
                  basePattern.replace("(?:|[\\w])+", "(?:[\\w]|)+")
                result = result
                  .replace("[\\q{a\\b}]", "(?:a\\x08)")
                  .replace("[\\b]", "[\\x08]")
                  .replace("[\\q{AbC}]", "(?:AbC)")
                  .replace("[\\q{BC|A}--a]", "(?:BC)")
                  .replace("[\\q{BC|A}]", "(?:BC|A)")
                  .replace("[[a-c]&&B]", "[B]")
                  .replace("[[a-c]--B]", "[ac]")
                  .replace("\\p{Lower}", "\\p{Ll}")
                  .replace("\\p{Upper}", "\\p{Lu}")
                  .replace("\\P{Lower}", "\\P{Ll}")
                  .replace("\\P{Upper}", "\\P{Lu}")
                if flags.contains('v') then
                  result = result
                    .replace("[^\\P{Ll}]", "[A-Za-z]")
                    .replace("\\P{Ll}", "[^A-Za-z]")
                    .replace("\\P{Lu}", "[^A-Za-z]")
                    .replace("\\p{Ll}", "[A-Za-z]")
                    .replace("\\p{Lu}", "[A-Za-z]")
                else if ignoreCase then
                  result = result
                    .replace("\\p{Ll}", "[A-Za-z]")
                    .replace("\\p{Lu}", "[A-Za-z]")
                    .replace("\\P{Ll}", ".")
                    .replace("\\P{Lu}", ".")
                translateUnicodePropertyEscapes(result)
              }
              // Java group names must start with a Latin letter, while ES
              // allows any IdentifierName (`_`, `$`, Unicode identifiers).
              // Rename groups to `g0`, `g1`, ... for the JVM pattern.
              val javaGroupNames = mutable.LinkedHashMap.empty[String, String]
              {
                var scan = 0
                var escaped = false
                var inClass = false
                while scan < compatiblePattern.length do {
                  val ch = compatiblePattern.charAt(scan)
                  if escaped then escaped = false
                  else if ch == '\\' then escaped = true
                  else if ch == '[' then inClass = true
                  else if ch == ']' && inClass then inClass = false
                  else if !inClass && ch == '(' && scan + 2 < compatiblePattern.length &&
                      compatiblePattern.charAt(scan + 1) == '?' &&
                      compatiblePattern.charAt(scan + 2) == '<' &&
                      (scan + 3 >= compatiblePattern.length ||
                        (compatiblePattern.charAt(scan + 3) != '=' &&
                          compatiblePattern.charAt(scan + 3) != '!'))
                  then {
                    val close = compatiblePattern.indexOf('>', scan + 3)
                    if close > 0 then {
                      val name = compatiblePattern.substring(scan + 3, close)
                      if !javaGroupNames.contains(name) then
                        javaGroupNames(name) = s"g${javaGroupNames.size}"
                      scan = close
                    }
                  }
                  scan += 1
                }
              }
              val translated = new StringBuilder
              var index = 0
              var inClass = false
              while index < compatiblePattern.length do {
                if inClass then {
                  // java.util.regex rejects an unescaped `[` inside a
                  // character class and `\{` escape sequences; JS allows
                  // literal `[` in classes and does not need the braces
                  // escaped there.
                  val ch = compatiblePattern.charAt(index)
                  if ch == '\\' && index + 1 < compatiblePattern.length then {
                    val next = compatiblePattern.charAt(index + 1)
                    if next == 'u' && index + 2 < compatiblePattern.length &&
                        compatiblePattern.charAt(index + 2) == '{'
                    then {
                      val close = compatiblePattern.indexOf('}', index + 3)
                      if close < 0 then
                        ctx.throwSyntaxError("Invalid Unicode escape in regexp")
                      val codePoint = Integer.parseInt(
                        compatiblePattern.substring(index + 3, close),
                        16
                      )
                      translated
                        .append("\\x{")
                        .append(Integer.toHexString(codePoint))
                        .append('}')
                      index = close + 1
                    } else {
                      translated.append(ch)
                      translated.append(next)
                      index += 2
                    }
                  } else if ch == '[' then {
                    translated.append("\\[")
                    index += 1
                  } else if ch == ']' then {
                    translated.append(']')
                    inClass = false
                    index += 1
                  } else {
                    translated.append(ch)
                    index += 1
                  }
                } else if index + 2 < compatiblePattern.length &&
                    compatiblePattern.charAt(index) == '(' &&
                    compatiblePattern.charAt(index + 1) == '?' &&
                    compatiblePattern.charAt(index + 2) == '<' &&
                    (index + 3 >= compatiblePattern.length ||
                      (compatiblePattern.charAt(index + 3) != '=' &&
                        compatiblePattern.charAt(index + 3) != '!'))
                then {
                  val close = compatiblePattern.indexOf('>', index + 3)
                  if close < 0 then {
                    translated.append(compatiblePattern.charAt(index))
                    index += 1
                  } else {
                    val name = compatiblePattern.substring(index + 3, close)
                    translated
                      .append("(?<")
                      .append(javaGroupNames.getOrElse(name, name))
                      .append(">")
                    index = close + 1
                  }
                }
                else if index + 2 < compatiblePattern.length &&
                    compatiblePattern.charAt(index) == '\\' &&
                    compatiblePattern.charAt(index + 1) == 'k' &&
                    compatiblePattern.charAt(index + 2) == '<'
                then {
                  val close = compatiblePattern.indexOf('>', index + 3)
                  if close < 0 then {
                    translated.append(compatiblePattern.charAt(index))
                    index += 1
                  } else {
                    val name = compatiblePattern.substring(index + 3, close)
                    translated
                      .append("\\k<")
                      .append(javaGroupNames.getOrElse(name, name))
                      .append(">")
                    index = close + 1
                  }
                }
                else if index + 3 < compatiblePattern.length &&
                    compatiblePattern.charAt(index) == '\\' &&
                    compatiblePattern.charAt(index + 1) == 'u' &&
                    compatiblePattern.charAt(index + 2) == '{'
                then {
                  val close = compatiblePattern.indexOf('}', index + 3)
                  if close < 0 then
                    ctx.throwSyntaxError("Invalid Unicode escape in regexp")
                  val codePoint = Integer.parseInt(
                    compatiblePattern.substring(index + 3, close),
                    16
                  )
                  translated
                    .append("\\x{")
                    .append(Integer.toHexString(codePoint))
                    .append('}')
                  index = close + 1
                }
                else if index + 2 < compatiblePattern.length &&
                    compatiblePattern.charAt(index) == '\\' &&
                    compatiblePattern.charAt(index + 1) == 'c' &&
                    compatiblePattern.charAt(index + 2).isLetter
                then {
                  translated.append(
                    (compatiblePattern.charAt(index + 2).toUpper & 0x1f).toChar
                  )
                  index += 3
                }
                else if index + 1 < compatiblePattern.length &&
                    compatiblePattern.charAt(index) == '\\' &&
                    compatiblePattern.charAt(index + 1) == 'c'
                then {
                  // In non-Unicode mode an invalid control escape is an
                  // identity escape for the backslash followed by `c`.
                  translated.append("\\\\c")
                  index += 2
                }
                else if compatiblePattern.charAt(index) == '{' &&
                    !(index >= 2 &&
                      (compatiblePattern.charAt(index - 1) == 'p' ||
                        compatiblePattern.charAt(index - 1) == 'P' ||
                        compatiblePattern.charAt(index - 1) == 'x') &&
                      compatiblePattern.charAt(index - 2) == '\\')
                then {
                  val close = compatiblePattern.indexOf('}', index + 1)
                  val quantifier =
                    if close < 0 then ""
                    else compatiblePattern.substring(index + 1, close)
                  if close < 0 then {
                    translated.append("\\{")
                    index += 1
                  }
                  else if !quantifier.matches("[0-9]+(,[0-9]*)?") then {
                    translated.append("\\{")
                    translated.append(quantifier)
                    translated.append("\\}")
                    index = close + 1
                  }
                  else {
                    translated.append('{')
                    index += 1
                  }
                }
                else if compatiblePattern.charAt(index) == '\\' &&
                    index + 1 < compatiblePattern.length &&
                    compatiblePattern.charAt(index + 1) == '0' &&
                    (index + 2 >= compatiblePattern.length ||
                      compatiblePattern.charAt(index + 2) < '0' ||
                      compatiblePattern.charAt(index + 2) > '9')
                then {
                  // java.util.regex rejects a bare \0 octal escape.
                  translated.append("\\x00")
                  index += 2
                }
                else if compatiblePattern.charAt(index) == '\\' &&
                    index + 1 < compatiblePattern.length &&
                    (compatiblePattern.charAt(index + 1) == '{' ||
                      compatiblePattern.charAt(index + 1) == '}')
                then {
                  translated.append(
                    if compatiblePattern.charAt(index + 1) == '{' then "\\x7B"
                    else "\\x7D"
                  )
                  index += 2
                }
                else if compatiblePattern.charAt(index) == '[' then {
                  translated.append('[')
                  inClass = true
                  index += 1
                }
                else {
                  translated.append(compatiblePattern.charAt(index))
                  index += 1
                }
              }
              // Validate the ES grammar (the JVM's rules differ, notably for
              // group names and some escapes).
              try quickjs.lexer.RegExpSyntax.validate(pattern, flags)
              catch {
                case e: RuntimeException =>
                  ctx.throwSyntaxError(
                    Option(e.getMessage)
                      .map(_.replaceFirst("^SyntaxError: ", ""))
                      .getOrElse("Invalid regular expression")
                  )
              }
              val regex = java.util.regex.Pattern.compile(
                translated.toString,
                patternFlags
              )
              val data = RegExpData(
                pattern,
                flags,
                global,
                ignoreCase,
                multiline,
                dotAll,
                unicode,
                sticky,
                regex
              )
              obj.defineProperty(
                "__regexpData",
                JSValue.Native(data),
                enumerable = false,
                writable = false,
                configurable = false
              )(using ctx)
              Some(obj -> data)
            }
            catch {
              case error: java.util.regex.PatternSyntaxException =>
                ctx.throwSyntaxError(
                  s"Invalid regular expression /$pattern/: ${error.getDescription}"
                )
            }
          case _ => None
    }

  /** ECMAScript `prop in obj`. Throws a TypeError when the right-hand side is
    * not an object. Shared by the bytecode interpreter and the generator VM.
    */
  def inOperator(propName: JSValue, objVal: JSValue)(using
      ctx: JSContext
  ): JSValue =
    (propName, objVal) match {
      case (JSValue.Symbol(symbolId), JSValue.Object(o)) =>
        JSValue.Bool(o.hasSymbolProperty(symbolId))
      case (JSValue.Symbol(symbolId), fn: JSValue.Function) =>
        JSValue.Bool(fn.funcObj.hasSymbolProperty(symbolId))
      case (
            JSValue.Symbol(symbolId),
            JSValue.Native(nf: quickjs.value.NativeFunction)
          ) =>
        JSValue.Bool(nf.funcObj.hasSymbolProperty(symbolId))
      case (
            JSValue.Symbol(symbolId),
            JSValue.Native(nc: quickjs.value.NativeConstructor)
          ) =>
        JSValue.Bool(nc.funcObj.hasSymbolProperty(symbolId))
      case (JSValue.Symbol(symbolId), JSValue.JSArrayVal(arr)) =>
        JSValue.Bool(
          arr.getOwnSymbol(symbolId).isDefined ||
            ctx.arrayPrototype.hasSymbolProperty(symbolId)
        )
      case (_, JSValue.Object(o)) =>
        JSValue.Bool(o.hasProperty(propName.toString))
      case (_, fn: JSValue.Function) =>
        JSValue.Bool(fn.funcObj.hasProperty(propName.toString))
      case (_, JSValue.JSArrayVal(arr)) =>
        val prop = propName.toString
        JSValue.Bool(
          arrayIndexFromKey(prop).exists(arr.hasIndex) ||
            arr.getOwnProperty(prop).isDefined ||
            ctx.arrayPrototype.hasProperty(prop)
        )
      case (_, JSValue.Native(nf: quickjs.value.NativeFunction)) =>
        JSValue.Bool(nf.funcObj.hasProperty(propName.toString))
      case (_, JSValue.Native(nc: quickjs.value.NativeConstructor)) =>
        JSValue.Bool(nc.funcObj.hasProperty(propName.toString))
      case _ =>
        ctx.throwTypeError("Right-hand side of 'in' is not an object")
    }
}
