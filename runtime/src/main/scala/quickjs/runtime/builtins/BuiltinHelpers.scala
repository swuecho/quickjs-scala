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
