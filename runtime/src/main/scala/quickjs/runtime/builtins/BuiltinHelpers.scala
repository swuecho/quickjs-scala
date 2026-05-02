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
      isStrict = func.isStrict
    )

  /** Extract the underlying JSObject from a value (JSObject, Function.funcObj,
    * etc.).
    */
  def extractJSObject(value: JSValue): Option[JSObject] = value match {
    case JSValue.Object(obj)    => Some(obj)
    case func: JSValue.Function => Some(func.funcObj)
    case JSValue.Native(nc: quickjs.value.NativeConstructor) => Some(nc.funcObj)
    case _                                                   => None
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
      hasWritable: Boolean // true if "writable" key was explicitly present
  ) {
    def isAccessor: Boolean = getter.isDefined || setter.isDefined
    def hasValueField: Boolean = value.isDefined || hasWritable
  }

  /** Parse a property descriptor from a JSValue. */
  def parsePropertyDescriptor(descriptor: JSValue)(using
      ctx: JSContext
  ): ParsedDescriptor =
    descriptor match {
      case JSValue.Object(descObj) =>
        val enumerableOpt = descObj.getOwnProperty("enumerable") match {
          case Some(JSValue.Bool(b)) => Some(b)
          case Some(_)               => Some(false)
          case None                  => None
        }
        val writableOpt = descObj.getOwnProperty("writable") match {
          case Some(JSValue.Bool(b)) => Some(b)
          case Some(_)               => Some(false)
          case None                  => None
        }
        val configurableOpt = descObj.getOwnProperty("configurable") match {
          case Some(JSValue.Bool(b)) => Some(b)
          case Some(_)               => Some(false)
          case None                  => None
        }
        val getterOpt = descObj.getOwnProperty("get") match {
          case Some(JSValue.Undefined) | None => None
          case Some(v)                        => Some(v)
        }
        val setterOpt = descObj.getOwnProperty("set") match {
          case Some(JSValue.Undefined) | None => None
          case Some(v)                        => Some(v)
        }
        val valueOpt = descObj.getOwnProperty("value")
        val hasWritableProp = descObj.getOwnProperty("writable").isDefined
        ParsedDescriptor(
          enumerableOpt,
          writableOpt,
          configurableOpt,
          getterOpt,
          setterOpt,
          valueOpt,
          hasWritableProp
        )
      case _ => ParsedDescriptor(None, None, None, None, None, None, false)
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
        if attrs.getter.isDefined || attrs.setter.isDefined then {
          attrs.getter.foreach(v => descObj.set("get", v))
          attrs.setter.foreach(v => descObj.set("set", v))
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
      case JSValue.Float64(d) =>
        val raw = java.lang.Double.toString(d)
        if raw.indexOf('E') >= 0 || raw.indexOf('e') >= 0 then
          java.math.BigDecimal.valueOf(d).stripTrailingZeros().toPlainString()
        else raw
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
        try Interpreter().call(functionToBytecode(f), thisArg, args, f.closure)
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
          func.closure
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

  /** Check if a key is an array index (non-negative integer string). */
  def isArrayIndexKey(key: String): Boolean =
    key.nonEmpty && key
      .forall(_.isDigit) && (key.length == 1 || key.charAt(0) != '0')

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
              val regex = java.util.regex.Pattern.compile(pattern, patternFlags)
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
              case _: java.util.regex.PatternSyntaxException =>
                ctx.throwSyntaxError("Invalid regular expression")
            }
          case _ => None
        }
      case _ => None
    }
}
