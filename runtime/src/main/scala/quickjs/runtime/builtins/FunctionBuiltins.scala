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
    if f.isClassConstructor then
      ctx.throwTypeError(
        s"Class constructor ${f.name} cannot be invoked without 'new'"
      )
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
    def buildFunctionRaw(args: Array[JSValue])(using JSContext): JSValue = {
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
        // JS ToString on every argument (an array argument stringifies to a
        // comma-separated parameter list, as in `Function(['a','b'], body)`).
        val strings = args.map(a => BuiltinHelpers.toJSString(a))
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

    // ES `Function(...)` results are ordinary constructable functions, so
    // they need an own `prototype` object like compiled function literals.
    def buildFunction(args: Array[JSValue])(using JSContext): JSValue = {
      val funcValue = buildFunctionRaw(args)
      val obj = funcValue match {
        case f: JSValue.Function => f.funcObj
        case _ => ctx.throwSyntaxError("Failed to compile function")
      }
      val protoObj = quickjs.objmodel.JSObject(
        prototype = ctx.objectPrototype,
        extensible = true
      )
      protoObj.defineProperty("constructor", funcValue, enumerable = false)
      obj.defineProperty(
        "prototype",
        JSValue.Object(protoObj),
        enumerable = false,
        writable = true,
        configurable = false
      )
      funcValue
    }

    // %Function.prototype% is itself a callable function object (`typeof
    // Function.prototype === "function"`) sharing the functionPrototype
    // JSObject as its [[Prototype]] and property store.
    val functionPrototypeFunction = NativeFunction(
      name = "",
      impl = (_, _) => JSValue.Undefined,
      funcObj = ctx.functionPrototype,
      length = 0
    )

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
      prototype = ctx.functionPrototype,
      prototypeValue = Some(JSValue.Native(functionPrototypeFunction))
    )
    BuiltinHelpers.initConstructor(
      functionConstructor,
      length = 1,
      prototypeValue = Some(JSValue.Native(functionPrototypeFunction))
    )
    ctx.global.set("Function", JSValue.Native(functionConstructor))

    val functionPrototypeCall = NativeFunction(
      name = "call",
      impl = (args, ctx) =>
        if args.isEmpty then
          ctx.throwTypeError("Function.prototype.call called on non-function")
        val func = args(0);
        val thisArg = if args.length > 1 then args(1) else JSValue.Undefined
        val actualArgs =
          if args.length > 2 then args.slice(2, args.length)
          else Array.empty[JSValue]
        func match {
          case f: JSValue.Function => callFunc(f, thisArg, actualArgs, ctx)
          case JSValue.Native(nf: NativeFunction) =>
            given JSContext = ctx
            // `fn.apply(fn, args)` / `fn.call(fn, ...)` is the idiom for
            // calling an extracted standalone native function: the receiver
            // is the function itself, so pass the arguments through as-is.
            val selfApplied = thisArg match {
              case JSValue.Native(other: NativeFunction) => other eq nf
              case _                                     => false
            }
            if selfApplied then nf.call(actualArgs)
            else {
              val argsWithThis = new Array[JSValue](actualArgs.length + 1);
              argsWithThis(0) = thisArg
              Array.copy(actualArgs, 0, argsWithThis, 1, actualArgs.length);
              nf.call(argsWithThis)
            }
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            given JSContext = ctx; nc.call(actualArgs)
          case JSValue.Object(_) =>
            given JSContext = ctx
            BuiltinHelpers.callCallableValue(func, thisArg, actualArgs)
          case _ =>
            ctx.throwTypeError(
              s"Function.prototype.call called on non-function: $func"
            )
        }
    )
    ctx.functionPrototype.set("call", JSValue.Native(functionPrototypeCall))

    val functionPrototypeApply = NativeFunction(
      name = "apply",
      length = 2,
      impl = (args, ctx) =>
        if args.isEmpty then
          ctx.throwTypeError("Function.prototype.apply called on non-function")
        val func = args(0);
        val thisArg = if args.length > 1 then args(1) else JSValue.Undefined
        val actualArgs: Array[JSValue] =
          if args.length > 2 then
            args(2) match {
              case JSValue.JSArrayVal(arr) =>
                (0 until arr.length).map(arr.get).toArray
              case JSValue.Null | JSValue.Undefined => Array.empty[JSValue]
              case listValue if BuiltinHelpers.isObjectLikeValue(listValue) =>
                given JSContext = ctx
                // ES CreateListFromArrayLike: read `length` (ToLength) and
                // each index through [[Get]], accepting functions too.
                val lengthValue =
                  BuiltinHelpers.getPropertyWithGetter(listValue, "length")
                val lengthI =
                  BuiltinHelpers.toIntegerOrInfinity(lengthValue)
                val length =
                  if lengthI <= 0 then 0
                  else if lengthI > Int.MaxValue.toDouble then Int.MaxValue
                  else lengthI.toInt
                (0 until length)
                  .map(i =>
                    BuiltinHelpers.getPropertyWithGetter(listValue, i.toString)
                  )
                  .toArray
              case _ =>
                ctx.throwTypeError(
                  "CreateListFromArrayLike called on non-object"
                )
            }
          else Array.empty[JSValue]
        func match {
          case f: JSValue.Function => callFunc(f, thisArg, actualArgs, ctx)
          case JSValue.Native(nf: NativeFunction) =>
            given JSContext = ctx
            // `fn.apply(fn, args)` / `fn.call(fn, ...)` is the idiom for
            // calling an extracted standalone native function: the receiver
            // is the function itself, so pass the arguments through as-is.
            val selfApplied = thisArg match {
              case JSValue.Native(other: NativeFunction) => other eq nf
              case _                                     => false
            }
            if selfApplied then nf.call(actualArgs)
            else {
              val argsWithThis = new Array[JSValue](actualArgs.length + 1);
              argsWithThis(0) = thisArg
              Array.copy(actualArgs, 0, argsWithThis, 1, actualArgs.length);
              nf.call(argsWithThis)
            }
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            given JSContext = ctx; nc.call(actualArgs)
          case JSValue.Object(_) =>
            given JSContext = ctx
            BuiltinHelpers.callCallableValue(func, thisArg, actualArgs)
          case _ =>
            ctx.throwTypeError(
              s"Function.prototype.apply called on non-function: $func"
            )
        }
    )
    ctx.functionPrototype.set("apply", JSValue.Native(functionPrototypeApply))

    val functionPrototypeBind = NativeFunction(
      name = "bind",
      impl = (args, ctx) =>
        if args.isEmpty then
          ctx.throwTypeError("Function.prototype.bind called on non-function")
        val func = args(0)
        if !BuiltinHelpers.isCallable(func) then
          ctx.throwTypeError("Function.prototype.bind called on non-function")
        val boundThis = if args.length > 1 then args(1) else JSValue.Undefined
        val boundArgs =
          if args.length > 2 then args.slice(2, args.length)
          else Array.empty[JSValue]
        // SetFunctionName: "bound " + Get(target, "name") when that is a
        // String, otherwise just "bound " (the Get may throw).
        val originalName =
          BuiltinHelpers.getPropertyWithGetter(func, "name") match {
            case JSValue.JSStr(s) => s
            case _                => ""
          }
        val boundName = "bound " + originalName
        
        // Check if the original function is a constructor
        val isConstructable = func match {
          case f: JSValue.Function => f.isConstructor
          case JSValue.Native(_: quickjs.value.NativeConstructor) => true
          case _ => false
        }
        
        val boundCallImpl = (callArgs: Array[JSValue], callCtx: JSContext) =>
          given JSContext = callCtx
          // Bound functions are NativeConstructors, so `callImpl` always
          // receives plain arguments (the engine passes `this` separately).
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
                isClassConstructor = f.isClassConstructor,
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
        
        {
          val ncFuncObj = quickjs.objmodel.JSObject(
            prototype = ctx.functionPrototype,
            extensible = true
          )
          // OrdinaryHasInstance delegates bound functions to their target.
          ncFuncObj.defineProperty(
            "__boundTarget",
            func,
            enumerable = false,
            writable = false,
            configurable = false
          )
          // Set length property (bound functions have adjusted length).
          // Per spec: Get(target, "length") -> ToIntegerOrInfinity ->
          // max(targetLen - boundArgsCount, 0), retaining Infinity and values
          // beyond the Int32 range.
          // Only an own `length` counts (spec step: HasOwnProperty(Target,
          // "length") before Get).
          val targetHasLength =
            BuiltinHelpers
              .extractJSObject(func)
              .exists(_.getOwnProperty("length").isDefined)
          val rawLength =
            if targetHasLength then
              BuiltinHelpers.getPropertyWithGetter(func, "length") match {
                case JSValue.Int32(i)   => i.toDouble
                case JSValue.Float64(d) => d
                case _                  => 0.0
              }
            else 0.0
          val targetLength =
            if rawLength.isNaN then 0.0
            else if rawLength.isInfinite then rawLength
            else math.signum(rawLength) * math.floor(math.abs(rawLength))
          val boundLengthDouble = math.max(targetLength - boundArgs.length, 0.0)
          val boundLength =
            if boundLengthDouble > Int.MaxValue then Int.MaxValue
            else boundLengthDouble.toInt
          val boundConstructor = quickjs.value.NativeConstructor(
            name = boundName,
            callImpl = boundCallImpl,
            constructImpl = (callArgs, callCtx) =>
              if isConstructable then
                constructBound(
                  callArgs,
                  JSValue.Native(
                    boundConstructorRef
                      .asInstanceOf[quickjs.value.NativeConstructor]
                  ),
                  callCtx
                )
              else callCtx.throwTypeError(s"$boundName is not a constructor"),
            prototype = quickjs.objmodel.JSObject(
              prototype = ctx.objectPrototype,
              extensible = true
            ),
            funcObj = ncFuncObj,
            length = boundLength,
            constructWithNewTarget =
              if isConstructable then Some(constructBound) else None,
            hasPrototypeProperty = false
          )
          boundConstructorRef = boundConstructor
          // The NativeConstructor auto-init stores its Int-typed `length`; the
          // spec value can be Infinity or exceed Int32, so re-apply the exact
          // values after construction.
          ncFuncObj.defineProperty(
            "length",
            JSValue.fromDouble(boundLengthDouble),
            enumerable = false,
            writable = false,
            configurable = true
          )
          ncFuncObj.defineProperty(
            "name",
            JSValue.fromString(boundName),
            enumerable = false,
            writable = false,
            configurable = true
          )
          JSValue.Native(boundConstructor)
        }
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
      impl = (args, callCtx) => {
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        receiver match {
          case JSValue.Native(function: NativeFunction) =>
            JSValue.fromString(s"function ${function.name}() { [native code] }")
          case JSValue.Native(constructor: quickjs.value.NativeConstructor) =>
            JSValue.fromString(s"function ${constructor.name}() { [native code] }")
          case _: JSValue.Function =>
            // Bytecode functions do not yet retain their original source text.
            // Keep a syntactically valid function representation until that
            // source-span plumbing is added.
            JSValue.fromString("function () { [native code] }")
          case _ => callCtx.throwTypeError("Function.prototype.toString called on incompatible receiver")
        }
      },
      length = 0
    )
    ctx.functionPrototype.set(
      "toString",
      JSValue.Native(functionPrototypeToString)
    )

    // `%ThrowTypeError%`: Function.prototype has inherited `caller` and
    // `arguments` accessors that always throw (Annex B.3.2, as implemented by
    // QuickJS's shared throw_type_error C function). Bound functions inherit
    // them, so `bound.caller` throws and `hasOwnProperty('caller')` is false.
    val throwTypeError = NativeFunction(
      name = "",
      impl = (_, callCtx) =>
        callCtx.throwTypeError(
          "'caller', 'callee', and 'arguments' properties may not be accessed " +
            "on strict mode functions or the arguments objects for calls to them"
        ),
      length = 0
    )
    for key <- Seq("caller", "arguments") do
      ctx.functionPrototype.defineAccessorPropertyDetailed(
        key,
        Some(JSValue.Native(throwTypeError)),
        Some(JSValue.Native(throwTypeError)),
        hasGetter = true,
        hasSetter = true,
        enumerable = Some(false),
        configurable = Some(true)
      )
  }

  /**
   * Symbol-keyed Function.prototype methods. Kept separate because the Symbol
   * built-in must exist before the well-known symbol ids can be read.
   */
  def initializeSymbolMethods(ctx: JSContext): Unit = {
    given JSContext = ctx
    // %Function.prototype%[@@hasInstance] implements OrdinaryHasInstance;
    // the `instanceof` operator reaches it for every callable constructor.
    val functionPrototypeHasInstance = NativeFunction(
      name = "[Symbol.hasInstance]",
      impl = (args, callCtx) => {
        val target = args.headOption.getOrElse(JSValue.Undefined)
        val value = if args.length > 1 then args(1) else JSValue.Undefined
        JSValue.Bool(
          BuiltinHelpers.ordinaryHasInstance(target, value)(using callCtx)
        )
      },
      length = 1
    )
    val hasInstanceSymbol = BuiltinHelpers.wellKnownSymbolId("hasInstance")
    ctx.functionPrototype.defineSymbolDataProperty(
      hasInstanceSymbol,
      Some(JSValue.Native(functionPrototypeHasInstance)),
      enumerable = Some(false),
      writable = Some(false),
      configurable = Some(false)
    )
  }
}
