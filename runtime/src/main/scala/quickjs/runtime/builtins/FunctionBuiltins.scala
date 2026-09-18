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

    // Helper to build a Function/GeneratorFunction/AsyncFunction/
    // AsyncGeneratorFunction from string args
    def buildFunctionRaw(
        args: Array[JSValue],
        generator: Boolean = false,
        async: Boolean = false
    )(using JSContext): JSValue = {
      val obj = quickjs.objmodel.JSObject(
        prototype =
          if async && generator then ctx.asyncGeneratorFunctionPrototype
          else if async then ctx.asyncFunctionPrototype
          else if generator then ctx.generatorFunctionPrototype
          else ctx.functionPrototype,
        extensible = true
      )
      {
        // JS ToString on every argument (an array argument stringifies to a
        // comma-separated parameter list, as in `Function(['a','b'], body)`).
        // `new Function()` compiles an empty body like `new Function("")`.
        val strings =
          if args.isEmpty then Array("")
          else args.map(a => BuiltinHelpers.toJSString(a))
        val paramNames =
          if strings.length > 1 then strings.init.toArray
          else Array.empty[String]
        val body = strings.last
        // Build and compile a function expression. Parenthesized so that
        // `async function(...)` at statement start is not mistaken for an async
        // function declaration without a name.
        val source =
          "(" + (if async then "async " else "") + "function" +
            (if generator then "*" else "") + "(" +
            paramNames.mkString(",") + ") {\n" + body + "\n})"
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
                  isConstructor = innerFunc.isConstructor,
                  isGenerator = innerFunc.isGenerator,
                  isAsync = innerFunc.isAsync,
                  funcObj = obj,
                  spanMap = innerFunc.spanMap,
                  isStrict = innerFunc.isStrict,
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
    // they need an own `prototype` object like compiled function literals;
    // generator results get a %Generator%/%AsyncGenerator% prototype object,
    // while plain async functions have no own `prototype`.
    def buildFunction(
        args: Array[JSValue],
        generator: Boolean = false,
        async: Boolean = false
    )(using JSContext): JSValue = {
      val funcValue = buildFunctionRaw(args, generator, async)
      val obj = funcValue match {
        case f: JSValue.Function => f.funcObj
        case _ => ctx.throwSyntaxError("Failed to compile function")
      }
      // Ordinary functions and generator functions (including async
      // generators) own a `prototype` object; plain async functions do not.
      if generator || !async then {
        val protoObj = quickjs.objmodel.JSObject(
          prototype =
            if async then ctx.asyncGeneratorPrototype
            else if generator then ctx.generatorPrototype
            else ctx.objectPrototype,
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
      }
      // Instance name/length for constructor-created functions (compiled
      // literals get these in BytecodeLoop).
      val paramCount = funcValue match {
        case f: JSValue.Function => f.paramNames.length
        case _                   => 0
      }
      obj.defineProperty(
        "length",
        JSValue.fromInt(paramCount),
        enumerable = false,
        writable = false,
        configurable = true
      )
      obj.defineProperty(
        "name",
        JSValue.fromString("anonymous"),
        enumerable = false,
        writable = false,
        configurable = true
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

    // %GeneratorFunction%: creates generator functions from source. Not a
    // global property; it is reached through
    // %GeneratorFunction.prototype%.constructor (and is the [[Prototype]] of
    // generator functions). Mirrors QuickJS's JS_CLASS_GENERATOR_FUNCTION.
    val generatorFunctionConstructor = quickjs.value.NativeConstructor(
      name = "GeneratorFunction",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildFunction(args, generator = true)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildFunction(args, generator = true)
      ,
      prototype = ctx.generatorFunctionPrototype
    )
    BuiltinHelpers.initConstructor(generatorFunctionConstructor, length = 1)
    // [[Prototype]] of %GeneratorFunction% is %Function%.
    generatorFunctionConstructor.funcObj.setPrototype(
      functionConstructor.funcObj
    )
    // %GeneratorFunction%.prototype.constructor is non-writable, like
    // QuickJS's JS_NEW_CTOR_READONLY.
    ctx.generatorFunctionPrototype.defineProperty(
      "constructor",
      JSValue.Native(generatorFunctionConstructor),
      enumerable = false,
      writable = false,
      configurable = true
    )

    // %AsyncFunction%: creates async functions from source (no global).
    val asyncFunctionConstructor = quickjs.value.NativeConstructor(
      name = "AsyncFunction",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildFunction(args, generator = false, async = true)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildFunction(args, generator = false, async = true)
      ,
      prototype = ctx.asyncFunctionPrototype
    )
    BuiltinHelpers.initConstructor(asyncFunctionConstructor, length = 1)
    asyncFunctionConstructor.funcObj.setPrototype(functionConstructor.funcObj)
    ctx.asyncFunctionPrototype.defineProperty(
      "constructor",
      JSValue.Native(asyncFunctionConstructor),
      enumerable = false,
      writable = false,
      configurable = true
    )

    // %AsyncGeneratorFunction%: creates async generator functions from source
    // (no global). Its prototype already owns `prototype` pointing at
    // %AsyncGenerator%.
    val asyncGeneratorFunctionConstructor = quickjs.value.NativeConstructor(
      name = "AsyncGeneratorFunction",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildFunction(args, generator = true, async = true)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildFunction(args, generator = true, async = true)
      ,
      prototype = ctx.asyncGeneratorFunctionPrototype
    )
    BuiltinHelpers.initConstructor(asyncGeneratorFunctionConstructor, length = 1)
    asyncGeneratorFunctionConstructor.funcObj.setPrototype(
      functionConstructor.funcObj
    )
    ctx.asyncGeneratorFunctionPrototype.defineProperty(
      "constructor",
      JSValue.Native(asyncGeneratorFunctionConstructor),
      enumerable = false,
      writable = false,
      configurable = true
    )

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
    // `arguments` accessors. QuickJS's shared throw_type_error implements the
    // legacy exception: reading `caller`/`arguments` on a non-strict function
    // that has an own `prototype` returns undefined instead of throwing, so
    // ES5-era code keeps working; everything else (Function.prototype itself,
    // strict functions, methods/arrows, bound functions, writes) throws. A
    // single function is both getter and setter (the spec's
    // %ThrowTypeError% invariant); a setter call is recognised by its extra
    // argument.
    val throwTypeErrorMessage =
      "'caller', 'callee', and 'arguments' properties may not be accessed " +
        "on strict mode functions or the arguments objects for calls to them"
    val throwTypeError = NativeFunction(
      name = "",
      impl = (args, callCtx) =>
        given JSContext = callCtx
        val thisValue = args.headOption.getOrElse(JSValue.Undefined)
        val isSetter = args.length > 1
        val legacyCaller = !isSetter && (thisValue match {
          case f: JSValue.Function =>
            // QuickJS's js_throw_type_error only softens the error for plain
            // non-strict ordinary functions: generators, async functions and
            // async generators throw even though they own a `prototype`.
            !f.isStrict && !f.isGenerator && !f.isAsync &&
              f.funcObj.getOwnProperty("prototype").isDefined
          case _ => false
        })
        if legacyCaller then JSValue.Undefined
        else callCtx.throwTypeError(throwTypeErrorMessage)
      ,
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

    // %GeneratorFunction.prototype%[@@toStringTag] = "GeneratorFunction"
    // (registered here because the Symbol built-in must exist first).
    val toStringTagSymbol = BuiltinHelpers.wellKnownSymbolId("toStringTag")
    ctx.generatorFunctionPrototype.initSymbolProperty(
      toStringTagSymbol,
      JSValue.fromString("GeneratorFunction"),
      enumerable = false,
      writable = false,
      configurable = true
    )
    ctx.asyncFunctionPrototype.initSymbolProperty(
      toStringTagSymbol,
      JSValue.fromString("AsyncFunction"),
      enumerable = false,
      writable = false,
      configurable = true
    )
    ctx.asyncGeneratorFunctionPrototype.initSymbolProperty(
      toStringTagSymbol,
      JSValue.fromString("AsyncGeneratorFunction"),
      enumerable = false,
      writable = false,
      configurable = true
    )
    ctx.asyncGeneratorPrototype.initSymbolProperty(
      toStringTagSymbol,
      JSValue.fromString("AsyncGenerator"),
      enumerable = false,
      writable = false,
      configurable = true
    )

    // Shared %GeneratorPrototype% / %AsyncGeneratorPrototype% methods
    // (next/return/throw/@@iterator/constructor).
    Interpreter().installGeneratorPrototypes(ctx)
  }
}
