package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.runtime.JSContext

/** Function built-in prototype methods (call, apply, bind). */
object FunctionBuiltins:
  import quickjs.objmodel.JSObject

  def initialize(ctx: JSContext): Unit =
    given JSContext = ctx

    val functionPrototypeCall = NativeFunction(
      name = "call",
      impl = (args, ctx) =>
        if args.isEmpty then throw new RuntimeException("Function.prototype.call called on non-function")
        else
          val func = args(0)
          val thisArg = if args.length > 1 then args(1) else JSValue.Undefined
          val actualArgs = if args.length > 2 then args.slice(2, args.length) else Array.empty[JSValue]
          func match
            case f: JSValue.Function =>
              given JSContext = ctx
              val interpreter = Interpreter()
              val bcFunc = new BytecodeFunction(
                name = f.name, bytecode = f.bytecode, constants = f.constants,
                stackSize = f.stackSize, freeVars = Array.empty, paramNames = f.paramNames,
                localVarNames = f.localVarNames, argumentsIndex = f.argumentsIndex,
                isConstructor = f.isConstructor)
              interpreter.call(bcFunc, thisArg, actualArgs, f.closure)
            case JSValue.Native(nativeFuncWrapper) =>
              nativeFuncWrapper match
                case native: NativeFunction =>
                  val argsWithThis = new Array[JSValue](actualArgs.length + 1)
                  argsWithThis(0) = thisArg
                  Array.copy(actualArgs, 0, argsWithThis, 1, actualArgs.length)
                  given JSContext = ctx
                  native.call(argsWithThis)
                case constructor: quickjs.value.NativeConstructor =>
                  given JSContext = ctx; constructor.call(actualArgs)
                case _ => throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
            case _ => throw new RuntimeException(s"Function.prototype.call called on non-function: $func")
    )
    ctx.functionPrototype.set("call", JSValue.Native(functionPrototypeCall))

    val functionPrototypeApply = NativeFunction(
      name = "apply",
      impl = (args, ctx) =>
        if args.isEmpty then throw new RuntimeException("Function.prototype.apply called on non-function")
        else
          val func = args(0)
          val thisArg = if args.length > 1 then args(1) else JSValue.Undefined
          val actualArgs: Array[JSValue] = if args.length > 2 then
            args(2) match
              case JSValue.JSArrayVal(arr) =>
                val len = arr.length
                val result = new Array[JSValue](len)
                for i <- 0 until len do result(i) = arr.get(i)
                result
              case JSValue.Null | JSValue.Undefined => Array.empty[JSValue]
              case other =>
                other match
                  case JSValue.Object(obj) =>
                    given JSContext = ctx
                    obj.get("length") match
                      case JSValue.Int32(len) =>
                        val result = new Array[JSValue](len)
                        for i <- 0 until len do result(i) = obj.get(i.toString)
                        result
                      case _ => throw new RuntimeException("CreateListFromArrayLike called on non-object")
                  case _ => throw new RuntimeException("CreateListFromArrayLike called on non-object")
          else Array.empty[JSValue]
          func match
            case f: JSValue.Function =>
              given JSContext = ctx
              val interpreter = Interpreter()
              val bcFunc = new BytecodeFunction(
                name = f.name, bytecode = f.bytecode, constants = f.constants,
                stackSize = f.stackSize, freeVars = Array.empty, paramNames = f.paramNames,
                localVarNames = f.localVarNames, argumentsIndex = f.argumentsIndex,
                isConstructor = f.isConstructor)
              interpreter.call(bcFunc, thisArg, actualArgs, f.closure)
            case JSValue.Native(nativeFuncWrapper) =>
              nativeFuncWrapper match
                case native: NativeFunction =>
                  val argsWithThis = new Array[JSValue](actualArgs.length + 1)
                  argsWithThis(0) = thisArg
                  Array.copy(actualArgs, 0, argsWithThis, 1, actualArgs.length)
                  given JSContext = ctx; native.call(argsWithThis)
                case constructor: quickjs.value.NativeConstructor =>
                  given JSContext = ctx; constructor.call(actualArgs)
                case _ => throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
            case _ => throw new RuntimeException(s"Function.prototype.apply called on non-function: $func")
    )
    ctx.functionPrototype.set("apply", JSValue.Native(functionPrototypeApply))

    val functionPrototypeBind = NativeFunction(
      name = "bind",
      impl = (args, ctx) =>
        if args.isEmpty then throw new RuntimeException("Function.prototype.bind called on non-function")
        else
          val func = args(0)
          val boundThis = if args.length > 1 then args(1) else JSValue.Undefined
          val boundArgs = if args.length > 2 then args.slice(2, args.length) else Array.empty[JSValue]
          val boundFunction = NativeFunction(
            name = "bound",
            impl = (callArgs, callCtx) =>
              val actualCallArgs = if callArgs.length > 1 then callArgs.slice(1, callArgs.length) else Array.empty[JSValue]
              val combinedArgs = boundArgs ++ actualCallArgs
              func match
                case f: JSValue.Function =>
                  given JSContext = callCtx
                  val interpreter = Interpreter()
                  val bcFunc = new BytecodeFunction(
                    name = f.name, bytecode = f.bytecode, constants = f.constants,
                    stackSize = f.stackSize, freeVars = Array.empty, paramNames = f.paramNames,
                    localVarNames = f.localVarNames, argumentsIndex = f.argumentsIndex,
                    isConstructor = f.isConstructor)
                  interpreter.call(bcFunc, boundThis, combinedArgs, f.closure)
                case JSValue.Native(nativeFuncWrapper) =>
                  nativeFuncWrapper match
                    case native: NativeFunction =>
                      val argsWithThis = new Array[JSValue](combinedArgs.length + 1)
                      argsWithThis(0) = boundThis
                      Array.copy(combinedArgs, 0, argsWithThis, 1, combinedArgs.length)
                      given JSContext = callCtx; native.call(argsWithThis)
                    case constructor: quickjs.value.NativeConstructor =>
                      given JSContext = callCtx; constructor.call(combinedArgs)
                    case _ => throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
                case _ => throw new RuntimeException(s"Bound function called on non-function: $func")
          )
          JSValue.Native(boundFunction)
    )
    ctx.functionPrototype.set("bind", JSValue.Native(functionPrototypeBind))

    val functionPrototypeToString = NativeFunction(
      name = "toString",
      impl = (_, _) => JSValue.fromString("[object Function]")
    )
    ctx.functionPrototype.set("toString", JSValue.Native(functionPrototypeToString))
