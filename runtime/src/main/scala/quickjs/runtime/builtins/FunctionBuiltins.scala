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

    // Helper to build a Function from string args
    def buildFunction(args: Array[JSValue])(using JSContext): JSValue =
      val obj = quickjs.objmodel.JSObject(prototype = ctx.functionPrototype, extensible = true)
      if args.isEmpty then
        JSValue.Function(
          name = "anonymous", bytecode = Array(0: Byte), constants = Array.empty, stackSize = 0,
          closure = scala.collection.mutable.Map.empty, paramNames = Array.empty,
          localVarNames = Array.empty, parentLocalVarNames = Array.empty,
          argumentsIndex = -1, isConstructor = true, isGenerator = false, isAsync = false,
          funcObj = obj, spanMap = Array.empty[(Int, Int, Int)], isStrict = false)
      else
        val strings = args.map(_.toString)
        val paramNames = if strings.length > 1 then strings.init.toArray else Array.empty[String]
        val body = strings.last
        // Build and compile a function expression
        val source = "function(" + paramNames.mkString(",") + ") {\n" + body + "\n}"
        try
          val lexer = quickjs.lexer.Lexer(source)
          val tokens = lexer.tokenize()
          val parser = quickjs.parser.Parser(tokens)
          val ast = parser.parseScript()
          val compiler = quickjs.compiler.Compiler()
          val scriptFunc = compiler.compileScript(ast)
          // The compiled script contains the function as a constant (index 0)
          // Extract the inner BytecodeFunction
          if scriptFunc.constants.nonEmpty then
            scriptFunc.constants(0) match
              case innerFunc: quickjs.bytecode.BytecodeFunction =>
                JSValue.Function(
                  name = "anonymous", bytecode = innerFunc.bytecode, constants = innerFunc.constants,
                  stackSize = innerFunc.stackSize, closure = scala.collection.mutable.Map.empty,
                  paramNames = innerFunc.paramNames, localVarNames = innerFunc.localVarNames,
                  parentLocalVarNames = Array.empty, argumentsIndex = innerFunc.argumentsIndex,
                  isConstructor = true, isGenerator = false, isAsync = false,
                  funcObj = obj, spanMap = innerFunc.spanMap, isStrict = false)
              case _ =>
                ctx.throwSyntaxError("Failed to compile function")
          else
            ctx.throwSyntaxError("Failed to compile function")
        catch
          case e: quickjs.runtime.JSException => throw e
          case e: Exception =>
            ctx.throwSyntaxError(e.getMessage)

    // Function constructor: new Function(param1, ..., body)
    val functionConstructor = quickjs.value.NativeConstructor(
      name = "Function",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildFunction(args),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildFunction(args),
      prototype = ctx.functionPrototype
    )
    BuiltinHelpers.initConstructor(functionConstructor, length = 1)
    ctx.global.set("Function", JSValue.Native(functionConstructor))

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
              // Don't slice — use all callArgs as user arguments.
              // The calling convention inconsistency (Call vs CallMethod) means
              // callArgs may or may not include a prepended 'this'. We ignore
              // that and use captured boundThis instead.
              val combinedArgs = boundArgs ++ callArgs
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
