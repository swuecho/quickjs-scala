package quickjs.runtime.builtins

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.runtime.JSContext
import scala.collection.mutable

/** Shared helpers for all built-in initializers.
  *
  * Extracted from StdLib.scala to reduce file size and enable per-builtin files.
  */
object BuiltinHelpers:

  /** Initialize a constructor function with standard properties. */
  def initConstructor(
    constructor: quickjs.value.NativeConstructor,
    length: Int
  )(using ctx: JSContext): Unit =
    constructor.funcObj.setPrototype(ctx.functionPrototype)
    constructor.funcObj.defineProperty("prototype", JSValue.Object(constructor.prototype), enumerable = false)
    constructor.funcObj.defineProperty("length", JSValue.fromInt(length), enumerable = false)
    constructor.funcObj.defineProperty("name", JSValue.fromString(constructor.name), enumerable = false)

  /** Call a function value (native or bytecode) with given this and arguments. */
  def callFunctionValue(func: JSValue, thisArg: JSValue, args: Array[JSValue])(using ctx: JSContext): JSValue =
    func match
      case JSValue.Native(native: quickjs.value.NativeFunction) =>
        native.call(Array(thisArg) ++ args)
      case JSValue.Function(name, bytecode, constants, stackSize, closure, paramNames, localVarNames, parentLocalVarNames, argumentsIndex, isConstructor, isGenerator, isAsync, funcObj, spanMap, isStrict) =>
        val bcFunc = BytecodeFunction(
          name = name, bytecode = bytecode, constants = constants, stackSize = stackSize,
          freeVars = closure.keys.toArray, paramNames = paramNames, localVarNames = localVarNames,
          argumentsIndex = argumentsIndex, isConstructor = isConstructor,
          isGenerator = isGenerator, isAsync = isAsync,
          length = paramNames.length, spanMap = spanMap, isStrict = isStrict
        )
        try Interpreter().call(bcFunc, thisArg, args, closure)
        catch case _: Exception => JSValue.Undefined
      case _ => args.headOption.getOrElse(JSValue.Undefined)

  /** Call a function with explicit this binding (for method dispatch). */
  def callFunctionWithThis(
    funcValue: JSValue, thisValue: JSValue, args: Array[JSValue]
  )(using ctx: JSContext): JSValue =
    funcValue match
      case func: JSValue.Function =>
        val bcFunc = BytecodeFunction(
          name = func.name, bytecode = func.bytecode, constants = func.constants,
          stackSize = func.stackSize, freeVars = Array.empty,
          paramNames = func.paramNames, localVarNames = func.localVarNames,
          argumentsIndex = func.argumentsIndex, isConstructor = func.isConstructor,
          spanMap = func.spanMap
        )
        Interpreter().call(bcFunc, thisValue, args, func.closure)
      case JSValue.Native(nativeFuncWrapper) =>
        nativeFuncWrapper match
          case native: NativeFunction =>
            val argsWithThis = new Array[JSValue](args.length + 1)
            argsWithThis(0) = thisValue
            Array.copy(args, 0, argsWithThis, 1, args.length)
            ctx.withStackFrame(native.name, isNative = true) { native.call(argsWithThis) }
          case constructor: quickjs.value.NativeConstructor =>
            ctx.withStackFrame(constructor.name, isNative = true) { constructor.call(args)(using ctx) }
          case _ => throw RuntimeException(s"Invalid native function: $nativeFuncWrapper")
      case _ => throw RuntimeException(s"Cannot call non-function value: $funcValue")

  /** Build an Error object with the given type name and args. */
  def buildError(proto: quickjs.objmodel.JSObject, name: String, args: Array[JSValue])(using ctx: JSContext): JSValue =
    val obj = quickjs.objmodel.JSObject(prototype = proto, extensible = true)
    obj.set("name", JSValue.fromString(name))
    if args.nonEmpty then obj.set("message", args(0))
    ctx.attachStack(obj, skipFrames = 1)
    JSValue.Object(obj)

  /** Check if a key is an array index (non-negative integer string). */
  def isArrayIndexKey(key: String): Boolean =
    key.nonEmpty && key.forall(_.isDigit) && (key.length == 1 || key.charAt(0) != '0')

  // --- RegExp support ---

  final case class RegExpData(
    pattern: String, flags: String, global: Boolean, ignoreCase: Boolean,
    multiline: Boolean, dotAll: Boolean, unicode: Boolean, sticky: Boolean,
    regex: java.util.regex.Pattern
  )

  def parseRegExpFlags(flags: String)(using ctx: JSContext): (Int, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean) =
    var global = false; var ignoreCase = false; var multiline = false
    var dotAll = false; var unicode = false; var sticky = false; var patternFlags = 0
    val seen = scala.collection.mutable.Set.empty[Char]
    flags.foreach { ch =>
      if seen.contains(ch) then ctx.throwSyntaxError("Invalid regular expression flags")
      seen += ch
      ch match
        case 'g' => global = true
        case 'i' => ignoreCase = true; patternFlags |= java.util.regex.Pattern.CASE_INSENSITIVE
        case 'm' => multiline = true; patternFlags |= java.util.regex.Pattern.MULTILINE
        case 's' => dotAll = true; patternFlags |= java.util.regex.Pattern.DOTALL
        case 'u' => unicode = true; patternFlags |= java.util.regex.Pattern.UNICODE_CASE
        case 'y' => sticky = true
        case _ => ctx.throwSyntaxError("Invalid regular expression flags")
    }
    (patternFlags, global, ignoreCase, multiline, dotAll, unicode, sticky)

  def getRegExpData(value: JSValue)(using ctx: JSContext): Option[(quickjs.objmodel.JSObject, RegExpData)] =
    value match
      case JSValue.Object(obj) =>
        obj.getOwnProperty("__regexpPattern")(using ctx) match
          case Some(JSValue.JSStr(pattern)) =>
            val flags = obj.getOwnProperty("__regexpFlags")(using ctx) match
              case Some(JSValue.JSStr(f)) => f; case Some(v) => v.toString; case None => ""
            val (patternFlags, global, ignoreCase, multiline, dotAll, unicode, sticky) = parseRegExpFlags(flags)
            try
              val regex = java.util.regex.Pattern.compile(pattern, patternFlags)
              Some(obj -> RegExpData(pattern, flags, global, ignoreCase, multiline, dotAll, unicode, sticky, regex))
            catch case _: java.util.regex.PatternSyntaxException => ctx.throwSyntaxError("Invalid regular expression")
          case _ => None
      case _ => None
