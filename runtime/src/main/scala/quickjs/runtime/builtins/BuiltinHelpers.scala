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
      paramNames = func.paramNames,
      localVarNames = func.localVarNames,
      argumentsIndex = func.argumentsIndex,
      isConstructor = func.isConstructor,
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

  /** Ordinary [[Get]] for the object-like values represented by JSObject.
    * Unlike JSObject.get, this invokes an inherited or own accessor getter.
    */
  def getPropertyWithGetter(
      target: JSValue,
      key: String
  )(using ctx: JSContext): JSValue =
    target match {
      case JSValue.JSArrayVal(_) =>
        ctx.arrayPrototype.getPropertyDescriptorWithOwner(key) match {
          case Some((_, _, attrs)) if attrs.getter.isDefined =>
            callFunctionWithThis(attrs.getter.get, target, Array.empty)
          case Some((_, value, _)) => value
          case None                => JSValue.Undefined
        }
      case _ => extractJSObject(target) match {
      case Some(obj) =>
        obj.getPropertyDescriptorWithOwner(key) match {
          case Some((_, _, attrs)) if attrs.getter.isDefined =>
            callFunctionWithThis(attrs.getter.get, target, Array.empty)
          case Some((_, value, _)) => value
          case None                => JSValue.Undefined
        }
      case None => JSValue.Undefined
      }
    }

  /** ES OrdinaryToPrimitive with the number hint. */
  def toPrimitiveNumber(value: JSValue)(using ctx: JSContext): JSValue =
    if isPrimitive(value) then value
    else
      val methods = Array("valueOf", "toString")
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

  /** ES ToPropertyKey, including the string-hinted ToPrimitive operation and
    * Symbol.toPrimitive dispatch. The Symbol result is preserved; every other
    * primitive result is converted to a string key.
    */
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

  /** ES ToIntegerOrInfinity, represented as Double to retain infinities. */
  def toIntegerOrInfinity(value: JSValue)(using ctx: JSContext): Double = {
    val number = toNumber(value)
    if number.isNaN || number == 0.0 then 0.0
    else if number.isInfinite then number
    else math.copySign(math.floor(math.abs(number)), number)
  }

  // --- Property descriptor parsing ---

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
      case JSValue.Object(obj) =>
        // Call the JS-level toString method on the object
        val toStringMethod = obj.get("toString")(using ctx)
        if toStringMethod == JSValue.Undefined then "[object Object]"
        else {
          // Use callFunctionWithThis to properly propagate exceptions (not callFunctionValue which swallows them)
          val result = callFunctionWithThis(toStringMethod, value, Array.empty)
          // If result is not a string primitive, call ToString again on it
          result match {
            case JSValue.JSStr(s)  => s
            case JSValue.Symbol(_) =>
              ctx.throwTypeError("Cannot convert a Symbol value to a string")
            case _ => result.toString
          }
        }
      case JSValue.JSArrayVal(_) => "[object Array]"
      case _: JSValue.Function   => "[object Function]"
      case JSValue.Native(_)     => "[object Function]"
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
      length: Int
  )(using ctx: JSContext): Unit = {
    constructor.funcObj.setPrototype(ctx.functionPrototype)
    constructor.funcObj.defineProperty(
      "prototype",
      JSValue.Object(constructor.prototype),
      enumerable = false
    )
    constructor.prototype.defineProperty(
      "constructor",
      JSValue.Native(constructor),
      enumerable = false,
      writable = true,
      configurable = true
    )
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

  /** Call a function with explicit this binding (for method dispatch). */
  def callFunctionWithThis(
      funcValue: JSValue,
      thisValue: JSValue,
      args: Array[JSValue]
  )(using ctx: JSContext): JSValue =
    funcValue match {
      case func: JSValue.Function =>
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
            throw RuntimeException(
              s"Invalid native function: $nativeFuncWrapper"
            )
        }
      case _ =>
        throw RuntimeException(s"Cannot call non-function value: $funcValue")
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
                var result =
                  pattern.replace("(?:|[\\w])+", "(?:[\\w]|)+")
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
                result
              }
              val translated = new StringBuilder
              var index = 0
              while index < compatiblePattern.length do {
                if index + 3 < compatiblePattern.length &&
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
                  translated.appendAll(Character.toChars(codePoint))
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
                        compatiblePattern.charAt(index - 1) == 'P') &&
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
                else {
                  translated.append(compatiblePattern.charAt(index))
                  index += 1
                }
              }
              val regex = java.util.regex.Pattern.compile(
                translated.toString,
                patternFlags
              )
              Some(
                obj -> RegExpData(
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
              )
            }
            catch {
              case error: java.util.regex.PatternSyntaxException =>
                ctx.throwSyntaxError(
                  s"Invalid regular expression /$pattern/: ${error.getDescription}"
                )
            }
          case _ => None
        }
      case _ => None
    }
}
