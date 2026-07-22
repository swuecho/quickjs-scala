package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers.functionToBytecode

/** Function built-in prototype methods (call, apply, bind). */
object FunctionBuiltins {
  import quickjs.objmodel.JSObject

  /** Call a JSValue.Function with Interpreter using centralized conversion. */
  private def callFunc(
      f: JSValue.Function,
      thisArg: JSValue,
      args: Array[JSValue],
      ctx: JSContext
  ): JSValue = {
    given JSContext = ctx
    Interpreter().call(
      functionToBytecode(f),
      thisArg,
      args,
      f.closure,
      calleeValue = f
    )
  }

  def initialize(ctx: JSContext): Unit = {
    given JSContext = ctx

    // Helper to build a Function from string args
    def buildFunction(args: Array[JSValue])(using JSContext): JSValue = {
      val obj = quickjs.objmodel.JSObject(
        prototype = ctx.functionPrototype,
        extensible = true
      )
      if args.isEmpty then
        JSValue.Function(
          name = "anonymous",
          bytecode = Array(0: Byte),
          constants = Array.empty,
          stackSize = 0,
          closure = scala.collection.mutable.Map.empty,
          paramNames = Array.empty,
          localVarNames = Array.empty,
          parentLocalVarNames = Array.empty,
          argumentsIndex = -1,
          isConstructor = true,
          isGenerator = false,
          isAsync = false,
          funcObj = obj,
          spanMap = Array.empty[(Int, Int, Int)],
          isStrict = false
        )
      else {
        val strings = args.map(_.toString)
        val paramNames =
          if strings.length > 1 then strings.init.toArray
          else Array.empty[String]
        val body = strings.last
        // Build and compile a function expression
        val source =
          "function(" + paramNames.mkString(",") + ") {\n" + body + "\n}"
        try {
          val lexer = quickjs.lexer.Lexer(source)
          val tokens = lexer.tokenize()
          val parser = quickjs.parser.Parser(tokens)
          val ast = parser.parseScript()
          val compiler = quickjs.compiler.Compiler()
          val scriptFunc = compiler.compileScript(ast)
          // The compiled script contains the function as a constant (index 0)
          // Extract the inner BytecodeFunction
          if scriptFunc.constants.nonEmpty then
            scriptFunc.constants(0) match {
              case innerFunc: quickjs.bytecode.BytecodeFunction =>
                JSValue.Function(
                  name = "anonymous",
                  bytecode = innerFunc.bytecode,
                  constants = innerFunc.constants,
                  stackSize = innerFunc.stackSize,
                  closure = scala.collection.mutable.Map.empty,
                  paramNames = innerFunc.paramNames,
                  localVarNames = innerFunc.localVarNames,
                  parentLocalVarNames = Array.empty,
                  argumentsIndex = innerFunc.argumentsIndex,
                  isConstructor = true,
                  isGenerator = false,
                  isAsync = false,
                  funcObj = obj,
                  spanMap = innerFunc.spanMap,
                  isStrict = false,
                  parameterScopeEndPc = innerFunc.parameterScopeEndPc
                )
              case _ =>
                ctx.throwSyntaxError("Failed to compile function")
            }
          else ctx.throwSyntaxError("Failed to compile function")
        }
        catch {
          case e: quickjs.runtime.JSException => throw e
          case e: Exception                   =>
            ctx.throwSyntaxError(e.getMessage)
        }
      }
    }

    // Function constructor: new Function(param1, ..., body)
    val functionConstructor = quickjs.value.NativeConstructor(
      name = "Function",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildFunction(args)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildFunction(args)
      ,
      prototype = ctx.functionPrototype
    )
    BuiltinHelpers.initConstructor(functionConstructor, length = 1)
    ctx.global.set("Function", JSValue.Native(functionConstructor))

    val functionPrototypeCall = NativeFunction(
      name = "call",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException(
            "Function.prototype.call called on non-function"
          )
        val func = args(0);
        val thisArg = if args.length > 1 then args(1) else JSValue.Undefined
        val actualArgs =
          if args.length > 2 then args.slice(2, args.length)
          else Array.empty[JSValue]
        func match {
          case f: JSValue.Function => callFunc(f, thisArg, actualArgs, ctx)
          case JSValue.Native(nf: NativeFunction) =>
            given JSContext = ctx
            val argsWithThis = new Array[JSValue](actualArgs.length + 1);
            argsWithThis(0) = thisArg
            Array.copy(actualArgs, 0, argsWithThis, 1, actualArgs.length);
            nf.call(argsWithThis)
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            given JSContext = ctx; nc.call(actualArgs)
          case _ =>
            throw new RuntimeException(
              s"Function.prototype.call called on non-function: $func"
            )
        }
    )
    ctx.functionPrototype.set("call", JSValue.Native(functionPrototypeCall))

    val functionPrototypeApply = NativeFunction(
      name = "apply",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException(
            "Function.prototype.apply called on non-function"
          )
        val func = args(0);
        val thisArg = if args.length > 1 then args(1) else JSValue.Undefined
        val actualArgs: Array[JSValue] =
          if args.length > 2 then
            args(2) match {
              case JSValue.JSArrayVal(arr) =>
                (0 until arr.length).map(arr.get).toArray
              case JSValue.Null | JSValue.Undefined => Array.empty[JSValue]
              case JSValue.Object(obj)              =>
                given JSContext = ctx
                obj.get("length") match {
                  case JSValue.Int32(len) =>
                    (0 until len).map(i => obj.get(i.toString)).toArray
                  case _ =>
                    throw new RuntimeException(
                      "CreateListFromArrayLike called on non-object"
                    )
                }
              case _ =>
                throw new RuntimeException(
                  "CreateListFromArrayLike called on non-object"
                )
            }
          else Array.empty[JSValue]
        func match {
          case f: JSValue.Function => callFunc(f, thisArg, actualArgs, ctx)
          case JSValue.Native(nf: NativeFunction) =>
            given JSContext = ctx
            val argsWithThis = new Array[JSValue](actualArgs.length + 1);
            argsWithThis(0) = thisArg
            Array.copy(actualArgs, 0, argsWithThis, 1, actualArgs.length);
            nf.call(argsWithThis)
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            given JSContext = ctx; nc.call(actualArgs)
          case _ =>
            throw new RuntimeException(
              s"Function.prototype.apply called on non-function: $func"
            )
        }
    )
    ctx.functionPrototype.set("apply", JSValue.Native(functionPrototypeApply))

    val functionPrototypeBind = NativeFunction(
      name = "bind",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException(
            "Function.prototype.bind called on non-function"
          )
        val func = args(0);
        val boundThis = if args.length > 1 then args(1) else JSValue.Undefined
        val boundArgs =
          if args.length > 2 then args.slice(2, args.length)
          else Array.empty[JSValue]
        // Determine the name: "bound " + original function name
        val originalName = func match {
          case f: JSValue.Function => f.name
          case JSValue.Native(nf: NativeFunction) => nf.name
          case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc.name
          case _ => ""
        }
        val boundName = "bound " + (if originalName.nonEmpty then originalName else "")
        
        // Check if the original function is a constructor
        val isConstructable = func match {
          case f: JSValue.Function => f.isConstructor
          case JSValue.Native(_: quickjs.value.NativeConstructor) => true
          case _ => false
        }
        
        val boundCallImpl = (callArgs: Array[JSValue], callCtx: JSContext) =>
          given JSContext = callCtx
          val combinedArgs = boundArgs ++ callArgs
          func match {
            case f: JSValue.Function =>
              callFunc(f, boundThis, combinedArgs, callCtx)
            case JSValue.Native(nf: NativeFunction) =>
              val argsWithThis = new Array[JSValue](combinedArgs.length + 1);
              argsWithThis(0) = boundThis
              Array.copy(combinedArgs, 0, argsWithThis, 1, combinedArgs.length)
              nf.call(argsWithThis)
            case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
              nc.call(combinedArgs)
            case _ =>
              throw new RuntimeException(
                s"Bound function called on non-function: $func"
              )
          }
        
        var boundConstructorRef: quickjs.value.NativeConstructor | Null = null

        val constructBound = (
            callArgs: Array[JSValue],
            incomingNewTarget: JSValue,
            callCtx: JSContext
        ) =>
          given JSContext = callCtx
          val combinedArgs = boundArgs ++ callArgs
          val effectiveNewTarget = incomingNewTarget match {
            case JSValue.Native(nc: quickjs.value.NativeConstructor)
                if boundConstructorRef != null &&
                  (nc.asInstanceOf[AnyRef] eq boundConstructorRef.asInstanceOf[AnyRef]) =>
              func
            case other => other
          }
          func match {
            case f: JSValue.Function =>
              val prototypeSource = effectiveNewTarget match {
                case targetFn: JSValue.Function => targetFn.funcObj
                case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                  nc.funcObj
                case JSValue.Object(obj) => obj
                case _                   => f.funcObj
              }
              val funcPrototype = prototypeSource.get("prototype")(using callCtx) match {
                case JSValue.Object(proto) => proto
                case _ => callCtx.objectPrototype
              }
              val newObj = quickjs.objmodel.JSObject(
                prototype = funcPrototype,
                extensible = true
              )
              val bcFunc = new quickjs.bytecode.BytecodeFunction(
                name = f.name,
                bytecode = f.bytecode,
                constants = f.constants,
                stackSize = f.stackSize,
                freeVars = Array.empty,
                paramNames = f.paramNames,
                localVarNames = f.localVarNames,
                argumentsIndex = f.argumentsIndex,
                isConstructor = f.isConstructor,
                isGenerator = f.isGenerator,
                spanMap = f.spanMap,
                isStrict = f.isStrict,
                parameterScopeEndPc = f.parameterScopeEndPc
              )
              val interpreter = quickjs.interpreter.Interpreter()
              val retValue = interpreter.call(
                bcFunc,
                JSValue.Object(newObj),
                combinedArgs,
                f.closure,
                effectiveNewTarget
              )
              retValue match {
                case _: JSValue.Object | _: JSValue.Function | _: JSValue.JSArrayVal => retValue
                case _ => JSValue.Object(newObj)
              }
            case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
              nc.construct(combinedArgs, effectiveNewTarget)
            case _ =>
              throw new RuntimeException(
                s"Bound function called on non-function: $func"
              )
          }
        
        if isConstructable then {
          val ncFuncObj = quickjs.objmodel.JSObject(
            prototype = ctx.functionPrototype,
            extensible = true
          )
          // Set length property (bound functions have adjusted length)
          val originalLength = func match {
            case f: JSValue.Function => f.paramNames.length
            case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc.length
            case JSValue.Native(nf: NativeFunction) => nf.length
            case _ => 0
          }
          val boundLength = math.max(0, originalLength - boundArgs.length)
          ncFuncObj.set("length", JSValue.fromInt(boundLength))(using ctx)
          ncFuncObj.set("name", JSValue.fromString(boundName.trim()))(using ctx)
          val boundConstructor = quickjs.value.NativeConstructor(
            name = boundName.trim(),
            callImpl = boundCallImpl,
            constructImpl = (callArgs, callCtx) =>
              constructBound(
                callArgs,
                JSValue.Native(
                  boundConstructorRef
                    .asInstanceOf[quickjs.value.NativeConstructor]
                ),
                callCtx
              ),
            prototype = quickjs.objmodel.JSObject(
              prototype = ctx.objectPrototype,
              extensible = true
            ),
            funcObj = ncFuncObj,
            constructWithNewTarget = Some(constructBound),
            hasPrototypeProperty = false
          )
          boundConstructorRef = boundConstructor
          JSValue.Native(boundConstructor)
        } else
          JSValue.Native(NativeFunction(
            name = boundName.trim(),
            impl = boundCallImpl
          ))
    )
    ctx.functionPrototype.defineProperty(
      "bind",
      JSValue.Native(functionPrototypeBind),
      enumerable = false,
      writable = true,
      configurable = true
    )

    val functionPrototypeToString = NativeFunction(
      name = "toString",
      impl = (_, _) => JSValue.fromString("[object Function]")
    )
    ctx.functionPrototype.set(
      "toString",
      JSValue.Native(functionPrototypeToString)
    )
  }
}
