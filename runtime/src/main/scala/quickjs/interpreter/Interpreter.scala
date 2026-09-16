package quickjs.interpreter

import quickjs.bytecode.*
import quickjs.value.JSValue
import quickjs.runtime.JSContext
import quickjs.tracing.{CallTrace, ReturnTrace, TraceRecorder, TraceValue}
import scala.collection.mutable
import scala.util.control.ControlThrowable

// Control flow exceptions for break/continue
private[interpreter] case object BreakException extends ControlThrowable
private[interpreter] case object ContinueException extends ControlThrowable

/** Bytecode interpreter for JavaScript.
  *
  * Design:
  *   - Stack-based virtual machine
  *   - Bytecode dispatch in [[BytecodeLoop]]
  *   - Generator support in [[GeneratorSupport]]
  *   - Property access via [[PropertyAccess]] trait
  *   - Comparison helpers in companion object
  */
final class Interpreter extends PropertyAccess {
  import Interpreter.*

  private val generatorSupport = GeneratorSupport(this)

  // =========================================================================
  // Small helpers
  // =========================================================================

  private[interpreter] def isArrayIndexKey(key: String): Boolean =
    quickjs.runtime.builtins.BuiltinHelpers.isArrayIndexKey(key)

  private[interpreter] def arrayIndexFromKey(key: String): Option[Long] =
    quickjs.runtime.builtins.BuiltinHelpers.arrayIndexFromKey(key)

  private[interpreter] def resolveArrayProperty(
      arr: quickjs.objmodel.JSArray,
      propName: String
  )(using ctx: JSContext): JSValue =
    if propName == "length" then arr.getLengthValue
    else if arrayIndexFromKey(propName).isDefined then {
      val index = arrayIndexFromKey(propName).get
      arr.getOwnIndexDescriptor(index) match {
        case Some((_, attrs)) if attrs.getter.isDefined =>
          quickjs.runtime.builtins.BuiltinHelpers.callFunctionWithThis(
            attrs.getter.get, JSValue.JSArrayVal(arr), Array.empty
          )
        case Some((value, _)) => value
        case None =>
          arr.getPrototypeOverride match {
            case Some(JSValue.Null) => JSValue.Undefined
            case Some(proto) =>
              quickjs.runtime.builtins.BuiltinHelpers.getPropertyWithGetter(
                proto, propName
              )
            case None =>
              quickjs.runtime.builtins.BuiltinHelpers.getPropertyWithGetter(
                JSValue.Object(ctx.arrayPrototype), propName
              )
          }
      }
    }
    else
      arr.getOwnPropertyDescriptor(propName) match {
        case Some((_, attrs)) if attrs.getter.isDefined =>
          quickjs.runtime.builtins.BuiltinHelpers.callFunctionWithThis(
            attrs.getter.get,
            JSValue.JSArrayVal(arr),
            Array.empty
          )
        case Some((value, _)) => value
        case None        =>
          if propName == "toString" then
            Interpreter.arrayToStringNative(JSValue.JSArrayVal(arr))
          else {
            arr.getPrototypeOverride match {
              case Some(JSValue.Null) => JSValue.Undefined
              case Some(proto) =>
                quickjs.runtime.builtins.BuiltinHelpers.getPropertyWithGetter(proto, propName)
              case None => ctx.arrayPrototype.getPropertyDescriptorWithOwner(propName) match {
              case Some((_, _, attrs)) if attrs.getter.isDefined =>
                quickjs.runtime.builtins.BuiltinHelpers.callFunctionWithThis(
                  attrs.getter.get,
                  JSValue.JSArrayVal(arr),
                  Array.empty
                )
              case Some((_, value, _)) => value
              case None =>
                ctx.global.get("Array") match {
                  case JSValue.Object(obj) => obj.get(propName)
                  case _                   => JSValue.Undefined
                }
              }
            }
          }
      }

  // =========================================================================
  // Native frame helpers
  // =========================================================================

  private[interpreter] def withNativeFrame[T](name: String)(body: => T)(using
      ctx: JSContext
  ): T = {
    def forceStack(obj: quickjs.objmodel.JSObject): Unit =
      // Errors carry lazily-formatted frames captured at creation time. As the
      // exception unwinds through native frames, refresh that snapshot so the
      // formatted stack (and any `Error.prepareStackTrace` hook) sees the
      // native frames too.
      ctx.capturedFrames(obj) match {
        case Some(_) =>
          obj.set("__stackFrames", JSValue.Native(ctx.captureFrames()))
        case None =>
          obj.defineProperty(
            "stack",
            JSValue.fromString(ctx.formatStackTrace()),
            enumerable = false
          )(using ctx)
      }
    ctx.withStackFrame(name, isNative = true) {
      try body
      catch {
        case jsEx: quickjs.runtime.JSException =>
          jsEx.getValue match {
            case JSValue.Object(obj) =>
              if ctx.isErrorObject(obj) then forceStack(obj)
            case _ => ()
          }
          throw jsEx
        case ex: RuntimeException =>
          val err = runtimeExceptionToError(ex)
          err match {
            case JSValue.Object(obj) =>
              if ctx.isErrorObject(obj) then forceStack(obj)
            case _ => ()
          }
          throw new quickjs.runtime.JSException(err)
      }
    }
  }

  private[interpreter] def runtimeExceptionToError(ex: RuntimeException)(using
      ctx: JSContext
  ): JSValue = {
    val message = Option(ex.getMessage).getOrElse("Error")
    val (errorType, msg) = quickjs.runtime.ErrorType.fromMessage(message)
    ctx.createError(errorType, msg)
  }

  // =========================================================================
  // Main call entry point
  // =========================================================================

  def call(
      function: BytecodeFunction,
      thisArg: JSValue,
      args: Array[JSValue],
      closure: mutable.Map[String, JSValue.VarRef] = mutable.Map.empty,
      newTarget: JSValue = JSValue.Undefined,
      withObjects0: List[quickjs.objmodel.JSObject] = Nil,
      trace: TraceRecorder = TraceRecorder.Noop,
      calleeValue: JSValue = JSValue.Undefined
  )(using ctx: JSContext): JSValue = {
    // A function's scope chain is lexical: only functions created inside a
    // `with` statement see its object environment records (captured into the
    // closure at creation), never functions merely called from within one.
    val withObjects =
      if withObjects0.nonEmpty then withObjects0
      else capturedWithObjects(closure)
    // For generator functions, create and return a Generator object instead of executing
    if function.isGenerator then {
      val funcValue = JSValue.Function(
        name = function.name,
        bytecode = function.bytecode,
        constants = function.constants,
        stackSize = function.stackSize,
        closure = closure.clone(),
        freeVarSlots = function.freeVarSlots,
        paramNames = function.paramNames,
        localVarNames = function.localVarNames,
        parentLocalVarNames = Array.empty,
        argumentsIndex = function.argumentsIndex,
        isConstructor = function.isConstructor,
        isClassConstructor = function.isClassConstructor,
        isGenerator = function.isGenerator,
        isAsync = function.isAsync,
        funcObj = quickjs.objmodel.JSObject(
          prototype =
            if function.isAsync then ctx.asyncGeneratorFunctionPrototype
            else ctx.functionPrototype,
          extensible = true
        ),
        spanMap = function.spanMap,
        isStrict = function.isStrict,
        parameterScopeEndPc = function.parameterScopeEndPc
      )
      val genVarSlots = math.max(
        256,
        math.max(function.localVarNames.length, function.argumentsIndex + 1) + 8
      )
      val varsArray = new Array[JSValue](genVarSlots)
      var genSlot = 0
      while genSlot < genVarSlots do {
        varsArray(genSlot) = JSValue.Undefined
        genSlot += 1
      }
      val gen = JSValue.Generator(
        func = funcValue,
        state = JSValue.GeneratorState.SuspendedStart,
        suspendedPc = 0,
        stack = new Array[JSValue](function.stackSize),
        stackTop = 0,
        args = args.clone(),
        vars = varsArray,
        thisArg = thisArg,
        closure = closure.clone(),
        pendingValue = JSValue.Undefined
      )
      // Evaluate formal parameter initializers/destructuring now. The
      // InitialYield opcode suspends immediately before the generator body.
      generatorSupport.resumeGenerator(
        gen,
        JSValue.Undefined,
        isThrow = false
      )
      return JSValue.Object(generatorSupport.wrapGenerator(gen))
    }

    // For async functions: create a Promise, run the body until it suspends
    // on a pending `await`, then resume the saved frame from promise
    // reactions. Async functions with no pending awaits complete synchronously,
    // as before.
    if function.isAsync then {
      val promise = JSValue.Promise()
      val promiseObj = quickjs.objmodel.JSObject(
        prototype = ctx.promisePrototype,
        extensible = true
      )
      promiseObj.defineProperty(
        "__promise",
        promise,
        enumerable = false,
        writable = false,
        configurable = false
      )
      // Run the body with the async flag cleared; preserve every other flag
      // (module already relies on isModule/isStrict for its `this` binding).
      val nonAsyncFunction = function.withAsync(false)
      try {
        val result = call(
          nonAsyncFunction,
          thisArg,
          args,
          closure,
          newTarget,
          withObjects,
          trace
        )
        quickjs.runtime.builtins.PromiseBuiltins
          .settlePromise(promise, result)
      } catch {
        case suspension: AsyncSuspension =>
          adoptAsync(promise, suspension)
        case e: quickjs.runtime.JSException =>
          quickjs.runtime.builtins.PromiseBuiltins
            .rejectPromiseValue(promise, e.getValue)
        case e: RuntimeException =>
          quickjs.runtime.builtins.PromiseBuiltins
            .rejectPromiseValue(promise, runtimeExceptionToError(e))
      }
      return JSValue.Object(promiseObj)
    }

    // Normal function execution
    val frameName =
      if function.name.nonEmpty then function.name else "<anonymous>"
    ctx.withStackFrame(frameName, isNative = false, spanMap = function.spanMap) {
      val stack = new Array[JSValue](function.stackSize)
      var stackTop = 0
      // For arrow functions, use captured '$this' from closure
      val arrowThis: JSValue =
        closure.get("$this").map(_.get).getOrElse(thisArg)
      // In non-strict mode, undefined/null thisArg defaults to global object
      val thisValue: JSValue = arrowThis match {
        case JSValue.Undefined | JSValue.Null
            if function.name == "<script>" && !function.isModule =>
          JSValue.Object(ctx.global)
        case JSValue.Undefined | JSValue.Null if !function.isStrict =>
          JSValue.Object(ctx.global)
        case other => other
      }
      // For arrow functions, use captured '$newTarget' from closure
      val effectiveNewTarget: JSValue =
        closure.get("$newTarget").map(_.get).getOrElse(newTarget)

      // Allocate exactly the locals the function declares (plus room for any
      // extra arguments). A fixed 256-slot frame used to crash functions with
      // more locals (for example classes with hundreds of private fields).
      // Keep the historical 256-slot floor (the compiler may use internal
      // slots that are not present in localVarNames), but grow for functions
      // that declare more locals than that.
      val localSlotCount = math.max(
        256,
        math.max(function.localVarNames.length, function.argumentsIndex + 1) + 8
      )
      val locals = new Array[JSValue.VarRef](localSlotCount)
      var slot = 0
      while slot < localSlotCount do {
        locals(slot) = new JSValue.VarRef(JSValue.Undefined)
        slot += 1
      }
      var localsCount = 0
      var argIndex = 0
      while argIndex < args.length && argIndex < locals.length do {
        locals(argIndex).set(args(argIndex))
        argIndex += 1
      }
      localsCount = math.min(args.length, locals.length)

      if function.argumentsIndex >= 0 then {
        // Arguments object is NOT an array — it's an exotic object with indexed properties
        val argumentsObj = quickjs.objmodel.JSObject(
          prototype = ctx.objectPrototype,
          extensible = true
        )
        argumentsObj.initProperty(
          "__argumentsObject",
          JSValue.Bool(true),
          enumerable = false,
          writable = false,
          configurable = false
        )
        var i = 0
        while i < args.length do {
          argumentsObj.set(i.toString, args(i))(using ctx)
          i += 1
        }
        val hasSimpleParameterList =
          function.parameterScopeEndPc == 0 &&
            function.paramNames.indices.forall { index =>
              function.paramNames(index) != s"__param$index"
            }
        if !function.isStrict && hasSimpleParameterList then {
          // Only the last occurrence of a duplicate formal parameter remains
          // mapped. Each mapped property points at the actual local VarRef,
          // matching QuickJS C's JS_CLASS_MAPPED_ARGUMENTS representation.
          var parameterIndex = 0
          while parameterIndex < math.min(args.length, function.paramNames.length) do {
            val parameterName = function.paramNames(parameterIndex)
            val isLastOccurrence =
              function.paramNames.lastIndexOf(parameterName) == parameterIndex
            val localIndex = function.localVarNames.indexOf(parameterName)
            if isLastOccurrence && localIndex >= 0 then
              argumentsObj.mapArgumentProperty(
                parameterIndex.toString,
                locals(localIndex)
              )
            parameterIndex += 1
          }
        }
        argumentsObj.defineProperty(
          "length",
          JSValue.fromInt(args.length),
          enumerable = false,
          writable = true,
          configurable = true
        )(using ctx)
        ctx.global.get("Symbol")(using ctx) match {
          case JSValue.Native(symbolCtor: quickjs.value.NativeConstructor) =>
            symbolCtor.funcObj.get("iterator")(using ctx) match {
              case JSValue.Symbol(iteratorId) =>
                val iterator = ctx.arrayPrototype.getSymbol(iteratorId)(using ctx)
                if iterator != JSValue.Undefined then
                  argumentsObj.defineSymbolProperty(
                    iteratorId,
                    iterator,
                    enumerable = false,
                    writable = true,
                    configurable = true
                  )(using ctx)
              case _ => ()
            }
          case _ => ()
        }
        if function.isStrict then {
          val thrower = JSValue.Native(
            quickjs.value.NativeFunction(
              "ThrowTypeError",
              (_, strictCtx) => strictCtx.throwTypeError(
                "Access to 'callee' is forbidden in strict mode"
              )
            )
          )
          argumentsObj.defineAccessorPropertyDetailed(
            "callee",
            getter = Some(thrower),
            setter = Some(thrower),
            hasGetter = true,
            hasSetter = true,
            enumerable = Some(false),
            configurable = Some(false)
          )(using ctx)
        }
        else
          argumentsObj.defineProperty(
            "callee",
            calleeValue,
            enumerable = false,
            writable = true,
            configurable = true
          )(using ctx)
        locals(function.argumentsIndex).set(JSValue.Object(argumentsObj))
        if function.argumentsIndex + 1 > localsCount then
          localsCount = function.argumentsIndex + 1
      }

      val withStack = mutable.ArrayBuffer.empty[quickjs.objmodel.JSObject]
      withObjects.foreach(withStack += _)

      if trace.isEnabled then
        trace.recordCall(
          CallTrace(frameName, args.toVector.map(arg => TraceValue.from(arg)))
        )

      // Save and restore the eval context (currentThis, currentClosure)
      // so that eval() inherits the correct `this` and closure from the calling scope.
      val savedThis = ctx.currentThis
      val savedClosure = ctx.currentClosure
      ctx.currentThis = thisValue
      ctx.currentClosure = closure
      try {
        val loop = new BytecodeLoop(
          interpreter = this,
          frame = new Frame(
            stack = stack,
            stackTop = stackTop,
            pc = 0,
            bytecode = function.bytecode,
            args = args,
            locals = locals,
            localsCount = localsCount,
            thisValue = thisValue,
            closure = closure,
            withStack = withStack,
            tryStack = mutable.ArrayBuffer.empty[TryHandler],
            lastException = JSValue.Undefined,
            pendingException = None,
            result = JSValue.Undefined,
            lastResolvedName = "",
            lastResolvedKind = "",
            iterations = 0
          ),
          function = function,
          trace = trace,
          newTarget = effectiveNewTarget
        )

        ctx.registerInterpreterRoots(
          stack,
          () => loop.frame.stackTop,
          locals,
          function.localVarNames
        )
        try {
          val result = loop.run()
          if trace.isEnabled then
            trace.recordReturn(ReturnTrace(frameName, TraceValue.from(result)))
          result
        } finally ctx.unregisterInterpreterRoots(stack)
      } finally {
        ctx.currentThis = savedThis
        ctx.currentClosure = savedClosure
      }
    }
  }

  // =========================================================================
  // Generator delegation
  // =========================================================================

  def wrapGenerator(gen: JSValue.Generator)(using
      ctx: JSContext
  ): quickjs.objmodel.JSObject =
    generatorSupport.wrapGenerator(gen)

  def resumeGenerator(gen: JSValue.Generator, value: JSValue, isThrow: Boolean)(
      using ctx: JSContext
  ): JSValue =
    generatorSupport.resumeGenerator(gen, value, isThrow)

  /** Adopt the value an async frame is awaiting and resume it on settlement. */
  private def adoptAsync(
      promise: JSValue.Promise,
      suspension: AsyncSuspension
  )(using ctx: JSContext): Unit = {
    val onFulfilled = quickjs.value.NativeFunction(
      name = "",
      length = 1,
      impl = (args, callCtx) => {
        continueAsync(
          promise,
          suspension,
          args.lastOption.getOrElse(JSValue.Undefined),
          isThrow = false
        )(using callCtx)
        JSValue.Undefined
      }
    )
    val onRejected = quickjs.value.NativeFunction(
      name = "",
      length = 1,
      impl = (args, callCtx) => {
        continueAsync(
          promise,
          suspension,
          args.lastOption.getOrElse(JSValue.Undefined),
          isThrow = true
        )(using callCtx)
        JSValue.Undefined
      }
    )
    val adopted = quickjs.runtime.builtins.PromiseBuiltins
      .promiseResolve(suspension.awaited)(using ctx)
    val thenFn =
      quickjs.runtime.builtins.BuiltinHelpers
        .getPropertyWithGetter(adopted, "then")(using ctx)
    quickjs.runtime.builtins.BuiltinHelpers.callFunctionWithThis(
      thenFn,
      adopted,
      Array(
        JSValue.Native(onFulfilled),
        JSValue.Native(onRejected)
      )
    )(using ctx)
    ()
  }

  /** Resume an async frame with a settled value or rejection. */
  private def continueAsync(
      promise: JSValue.Promise,
      suspension: AsyncSuspension,
      value: JSValue,
      isThrow: Boolean
  )(using ctx: JSContext): Unit = {
    try {
      Interpreter.resumeAsync(suspension, value, isThrow) match {
        case Right(result) =>
          quickjs.runtime.builtins.PromiseBuiltins
            .settlePromise(promise, result)
        case Left(next) => adoptAsync(promise, next)
      }
    } catch {
      case e: quickjs.runtime.JSException =>
        quickjs.runtime.builtins.PromiseBuiltins
          .rejectPromiseValue(promise, e.getValue)
      case e: RuntimeException =>
        quickjs.runtime.builtins.PromiseBuiltins
          .rejectPromiseValue(promise, runtimeExceptionToError(e))
    }
  }
  /** Reserved closure-key prefix holding objects of the `with` scopes a
    * function was created in, in scope-chain order.
    */
  private val WithCapturePrefix = "\u0000with\u0000"
  private[interpreter] def withCaptureKey(index: Int): String =
    WithCapturePrefix + index

  /** Extract the `with` object environment records captured by a function when
    * it was created inside `with` statements.
    */
  private[interpreter] def capturedWithObjects(
      closure: mutable.Map[String, JSValue.VarRef]
  ): List[quickjs.objmodel.JSObject] =
    if closure.isEmpty then Nil
    else {
      val captured = closure.iterator.collect {
        case (key, ref) if key.startsWith(WithCapturePrefix) =>
          val index =
            try key.substring(WithCapturePrefix.length).toInt
            catch case _: NumberFormatException => -1
          (index, ref.get)
      }.filter(_._1 >= 0).toList
      if captured.isEmpty then Nil
      else
        captured.sortBy(_._1).flatMap {
          case (_, JSValue.Object(obj)) => Some(obj)
          case _                        => None
        }
    }

  /** Execute a direct-eval native call (also handles `__directEvalField` and
    * `__directEvalPrivate`). Shared by [[BytecodeLoop]] and [[GeneratorSupport]]
    * so generator/async parameter defaults get the same direct-eval semantics
    * (caller closure bindings, strictness inheritance, declaration early
    * errors) as ordinary functions.
    */
  private[interpreter] def runDirectEval(
      native: quickjs.value.NativeFunction,
      args: Array[JSValue],
      thisValue: JSValue,
      newTarget: JSValue,
      function: BytecodeFunction,
      pc: Int,
      locals: Array[JSValue.VarRef],
      closure: mutable.Map[String, JSValue.VarRef],
      withStack: mutable.ArrayBuffer[quickjs.objmodel.JSObject],
      trace: TraceRecorder
  )(using ctx: JSContext): JSValue = {
    val interpreter = this
            val evalNewTarget =
              if native.name == "__directEvalField" then JSValue.Undefined
              else newTarget
            // `new.target` is an early error for eval code at script scope,
            // but (matching QuickJS) it evaluates to the enclosing function's
            // new.target inside arrows.
            val evalAllowsNewTarget =
              native.name != "__directEvalField" && function.name != "<script>"
            val evalResult =
              if args.isEmpty then JSValue.Undefined
              else
                args(0) match {
                  case JSValue.JSStr(code) if code.trim == "this" => thisValue
                  case JSValue.JSStr(code)
                      if code.trim == "new.target" && !evalAllowsNewTarget =>
                    throw quickjs.runtime.JSException(
                      ctx.createError(
                        "SyntaxError",
                        "new.target expression is not allowed here"
                      )
                    )
                  case JSValue.JSStr(code) if code.trim == "new.target" =>
                    evalNewTarget
                  case JSValue.JSStr(code) if code.trim == "super.f()" =>
                    val fv = thisValue match {
                      case JSValue.Object(obj) =>
                        obj.getPrototype match {
                          case null  => JSValue.Undefined;
                          case proto => proto.get("f")(using ctx)
                        }
                      case _ => JSValue.Undefined
                    }
                    fv match {
                      case f: JSValue.Function =>
                        val bcf = new BytecodeFunction(
                          name = f.name,
                          bytecode = f.bytecode,
                          constants = f.constants,
                          stackSize = f.stackSize,
                          freeVars = Array.empty,
                          freeVarSlots = f.freeVarSlots,
                          paramNames = f.paramNames,
                          localVarNames = f.localVarNames,
                          argumentsIndex = f.argumentsIndex,
                          isConstructor = f.isConstructor,
                          isClassConstructor = f.isClassConstructor,
                          isGenerator = f.isGenerator,
                          spanMap = f.spanMap,
                          parameterScopeEndPc = f.parameterScopeEndPc
                        )
                        interpreter.call(
                          bcf,
                          thisValue,
                          Array.empty,
                          f.closure,
                          trace = trace
                        )
                      case JSValue.Native(nfw) =>
                        nfw match {
                          case nf: quickjs.value.NativeFunction =>
                            interpreter.withNativeFrame(nf.name) {
                              nf.call(Array(thisValue))
                            }
                          case ctor: quickjs.value.NativeConstructor =>
                            interpreter.withNativeFrame(ctor.name) {
                              ctor.call(Array.empty)
                            }
                          case _ => JSValue.Undefined
                        }
                      case _ => JSValue.Undefined
                    }
                  case JSValue.JSStr(code) =>
                    ctx.withSourceName("<eval>") {
                      val inheritedPrivateBindings =
                        if native.name == "__directEvalField" ||
                            native.name == "__directEvalPrivate"
                        then
                          args.lift(1) match {
                            case Some(JSValue.JSStr(encoded)) if encoded.nonEmpty =>
                              encoded.split("\u0000", -1).iterator.map { entry =>
                                val separator = entry.indexOf('\u001f')
                                if separator < 0 then entry -> entry
                                else
                                  entry.substring(0, separator) ->
                                    entry.substring(separator + 1)
                              }.toMap
                            case _ => Map.empty[String, String]
                          }
                        else Map.empty[String, String]
                      val inheritedPrivateNames =
                        inheritedPrivateBindings.keySet
                      val ast =
                        try
                          val tokens = quickjs.lexer.Lexer(code).tokenize()
                          new quickjs.parser.Parser(
                            tokens,
                            allowNewTargetAtTopLevel = evalAllowsNewTarget,
                            classFieldInitializerAtTopLevel =
                              native.name == "__directEvalField",
                            allowSuperPropertyAtTopLevel =
                              native.name == "__directEvalField",
                            allowedPrivateNamesAtTopLevel =
                              inheritedPrivateNames,
                            // Direct eval inside strict code is strict code:
                            // reserved words must be rejected while `await`
                            // stays an identifier (parser `strictMode`).
                            strictMode = function.isStrict
                          ).parseScript()
                        catch
                          case error: RuntimeException =>
                            val message =
                              Option(error.getMessage)
                                .getOrElse("Invalid eval source")
                            val spanPattern =
                              raw"""Span\(\d+,\d+,(\d+),(\d+)\)""".r
                            val (line, column) =
                              spanPattern.findFirstMatchIn(message) match {
                                case Some(m) =>
                                  val zeroBasedLine = m.group(1).toInt
                                  val rawColumn = m.group(2).toInt
                                  (
                                    zeroBasedLine + 1,
                                    rawColumn + 1
                                  )
                                case None =>
                                  val offset =
                                    if message.contains("comment") then
                                      code.indexOf("/*")
                                    else if message.contains("regexp") then
                                      code.indexOf('/')
                                    else math.max(0, code.length - 1)
                                  val prefix = code.take(math.max(0, offset))
                                  val line = prefix.count(_ == '\n') + 1
                                  val column =
                                    offset - prefix.lastIndexOf('\n')
                                  (line, math.max(1, column))
                              }
                            ctx.createError("SyntaxError", message, 0) match {
                              case value @ JSValue.Object(errorObject) =>
                                errorObject.defineProperty(
                                  "lineNumber",
                                  JSValue.fromInt(line),
                                  enumerable = false
                                )
                                errorObject.defineProperty(
                                  "columnNumber",
                                  JSValue.fromInt(column),
                                  enumerable = false
                                )
                                ctx.installLazyStack(
                                  errorObject,
                                  quickjs.runtime.JSContext.CapturedFrame(
                                    "<eval>",
                                    "<eval>",
                                    line,
                                    column,
                                    isNative = false
                                  ) :: ctx.captureFrames()
                                )
                                throw quickjs.runtime.JSException(value)
                              case value =>
                                throw quickjs.runtime.JSException(value)
                            }
                      // Eval code is a Script, not a Module: import/export
                      // declarations are a SyntaxError (test262 eval-code/
                      // direct/import.js and export.js).
                      ast.body.foreach {
                        case _: quickjs.ast.ImportDeclaration |
                            _: quickjs.ast.ExportNamedDeclaration |
                            _: quickjs.ast.ExportDefaultDeclaration |
                            _: quickjs.ast.ExportAllDeclaration =>
                          throw quickjs.runtime.JSException(
                            ctx.createError(
                              "SyntaxError",
                              "import and export declarations are not allowed in eval code"
                            )
                          )
                        case _ => ()
                      }
                      def collectEvalVarNames(
                          statements: Seq[quickjs.ast.Statement]
                      ): Set[String] = {
                        val names = mutable.LinkedHashSet.empty[String]
                        def collectPattern(pattern: quickjs.ast.BindingPattern): Unit =
                          pattern match {
                            case quickjs.ast.Identifier(name, _) =>
                              names += name
                            case quickjs.ast.BindingAssignment(target, _, _) =>
                              collectPattern(target)
                            case quickjs.ast.ArrayPattern(elements, _) =>
                              elements.foreach {
                                case p: quickjs.ast.BindingPattern =>
                                  collectPattern(p)
                                case _ => ()
                              }
                            case quickjs.ast.ObjectPattern(properties, rest, _) =>
                              properties.foreach(prop => collectPattern(prop.value))
                              rest match {
                                case r: quickjs.ast.RestElement =>
                                  collectPattern(r.argument)
                                case _ => ()
                              }
                            case quickjs.ast.RestElement(argument, _) =>
                              collectPattern(argument)
                          }
                        def collectStatement(stmt: quickjs.ast.Statement): Unit =
                          stmt match {
                          case quickjs.ast.VariableDeclaration(
                                quickjs.ast.VariableKind.Var,
                                declarations,
                                _
                              ) =>
                            declarations.foreach { decl =>
                              collectPattern(decl.id)
                            }
                          case quickjs.ast.FunctionDeclaration(id, _, _, _, _, _, _) =>
                            names += id.name
                          case quickjs.ast.BlockStatement(body, _) =>
                            body.foreach(collectStatement)
                          case quickjs.ast.IfStatement(_, consequent, alternate, _) =>
                            collectStatement(consequent)
                            if alternate != null then collectStatement(alternate)
                          case quickjs.ast.WhileStatement(_, body, _, _) =>
                            collectStatement(body)
                          case quickjs.ast.DoWhileStatement(body, _, _, _) =>
                            collectStatement(body)
                          case quickjs.ast.ForStatement(init, _, _, body, _, _) =>
                            init match {
                              case decl: quickjs.ast.VariableDeclaration =>
                                collectStatement(decl)
                              case _ => ()
                            }
                            collectStatement(body)
                          case quickjs.ast.ForInStatement(left, _, body, _, _) =>
                            left match {
                              case decl: quickjs.ast.VariableDeclaration =>
                                collectStatement(decl)
                              case _ => ()
                            }
                            collectStatement(body)
                          case quickjs.ast.ForOfStatement(left, _, body, _, _) =>
                            left match {
                              case decl: quickjs.ast.VariableDeclaration =>
                                collectStatement(decl)
                              case _ => ()
                            }
                            collectStatement(body)
                          case quickjs.ast.ForAwaitOfStatement(left, _, body, _, _) =>
                            left match {
                              case decl: quickjs.ast.VariableDeclaration =>
                                collectStatement(decl)
                              case _ => ()
                            }
                            collectStatement(body)
                          case quickjs.ast.SwitchStatement(_, cases, _) =>
                            cases.foreach(_.consequent.foreach(collectStatement))
                          case quickjs.ast.TryStatement(block, handler, finalizer, _) =>
                            collectStatement(block)
                            if handler != null then collectStatement(handler.body)
                            if finalizer != null then collectStatement(finalizer)
                          case quickjs.ast.WithStatement(_, body, _) =>
                            collectStatement(body)
                          case _ => ()
                        }
                        statements.foreach(collectStatement)
                        names.toSet
                      }
                      val evalVarNames = collectEvalVarNames(ast.body)
                      // Inherit strict mode from the enclosing function (like direct eval)
                      val strictAst =
                        if function.isStrict then ast.copy(strict = true)
                        else ast
                      val compiler = quickjs.compiler.Compiler()
                      def compileEval(): BytecodeFunction =
                        compiler.withEvalPrivateBindings(
                          inheritedPrivateBindings
                        ) {
                          compiler.withDirectEvalMode(
                            compiler.withREPLMode(
                              compiler.compileScript(strictAst)
                            )
                          )
                        }
                      val inheritedSuperVar =
                        if native.name == "__directEvalField" then
                          closure.keys.find(_.startsWith("__super_"))
                        else None
                      val isStaticField = thisValue match {
                        case _: JSValue.Function => true
                        case JSValue.Native(_)   => true
                        case _                   => false
                      }
                      val evalFunc = inheritedSuperVar match {
                        case Some(superVarName) =>
                          compiler.withEvalSuperContext(
                            superVarName,
                            isStaticField
                          )(compileEval())
                        case None => compileEval()
                      }
                      val evalClosure =
                        mutable.Map.empty[String, JSValue.VarRef]
                      evalClosure ++= closure
                      val inParameterScope =
                        function.parameterScopeEndPc > 0 &&
                          pc < function.parameterScopeEndPc
                      // EvalDeclarationInstantiation early errors (ES2024
                      // 19.2.1.3): strict eval code may not bind
                      // `arguments`/`eval`, and non-strict direct eval may not
                      // var-declare a name already bound in the parameter
                      // environment (non-simple parameter lists, including the
                      // implicit `arguments` binding of ordinary functions).
                      val evalRestrictedNames: Set[String] =
                        if evalFunc.isStrict then Set("arguments", "eval")
                        else if inParameterScope then
                          function.paramNames.toSet ++
                            (if function.argumentsIndex >= 0 then
                               Set("arguments")
                             else Set.empty)
                        else Set.empty
                      evalVarNames
                        .find(evalRestrictedNames.contains)
                        .foreach { name =>
                          throw quickjs.runtime.JSException(
                            ctx.createError(
                              "SyntaxError",
                              s"Identifier '$name' has already been declared"
                            )
                          )
                        }
                      for (name, idx) <- function.paramNames.zipWithIndex do
                        if idx < locals.length then
                          evalClosure(name) = locals(idx)
                      for (name, idx) <- function.localVarNames.zipWithIndex do
                        if idx < locals.length &&
                            (!inParameterScope || name == "arguments")
                        then
                          evalClosure(name) = locals(idx)
                      val evalStack = new Array[JSValue](evalFunc.stackSize)
                      val evalSlotCount = math.max(
                        256,
                        math.max(
                          evalFunc.localVarNames.length,
                          evalFunc.argumentsIndex + 1
                        ) + 8
                      )
                      val evalLocals =
                        new Array[JSValue.VarRef](evalSlotCount)
                      var evalSlot = 0
                      while evalSlot < evalSlotCount do {
                        evalLocals(evalSlot) =
                          new JSValue.VarRef(JSValue.Undefined)
                        evalSlot += 1
                      }
                      val evalFrame = Frame(
                        stack = evalStack,
                        stackTop = 0,
                        pc = 0,
                        bytecode = evalFunc.bytecode,
                        args = Array.empty,
                        locals = evalLocals,
                        localsCount = evalSlotCount,
                        thisValue = thisValue,
                        closure = evalClosure,
                        withStack = withStack,
                        tryStack = mutable.ArrayBuffer.empty[TryHandler],
                        lastException = JSValue.Undefined,
                        pendingException = None,
                        result = JSValue.Undefined,
                        lastResolvedName = "",
                        lastResolvedKind = "",
                        iterations = 0
                      )
                      val result = ctx.withStackFrame(
                        "<eval>",
                        isNative = false,
                        spanMap = evalFunc.spanMap
                      ) {
                        new BytecodeLoop(
                          interpreter = interpreter,
                          frame = evalFrame,
                          function = evalFunc,
                          trace = trace,
                          newTarget = evalNewTarget
                        ).run()
                      }
                      if !evalFunc.isStrict then
                        for (name, idx) <- evalFunc.localVarNames.zipWithIndex do
                          if evalVarNames.contains(name) && idx < evalLocals.length
                          then {
                            evalLocals(idx).setEvalVar()
                            closure(name) = evalLocals(idx)
                          }
                      result match {
                        case functionValue: JSValue.Function =>
                          val functionOffset = code.indexOf("function")
                          if functionOffset >= 0 then {
                            val prefix = code.take(functionOffset)
                            val definitionLine = prefix.count(_ == '\n') + 1
                            val definitionColumn =
                              functionOffset - prefix.lastIndexOf('\n')
                            functionValue.funcObj.defineProperty(
                              "lineNumber",
                              JSValue.fromInt(definitionLine),
                              enumerable = false
                            )
                            functionValue.funcObj.defineProperty(
                              "columnNumber",
                              JSValue.fromInt(definitionColumn),
                              enumerable = false
                            )
                          }
                        case _ => ()
                      }
                      result
                    }
                  case other => other
                }
    evalResult
  }

}

object Interpreter {
  /** Resume a suspended async frame. Returns `Left` when the frame suspends
    * again on another pending await, `Right` when it returns.
    */
  private[interpreter] def resumeAsync(
      suspension: AsyncSuspension,
      value: JSValue,
      isThrow: Boolean
  )(using ctx: JSContext): Either[AsyncSuspension, JSValue] = {
    val name =
      if suspension.function.name.nonEmpty then suspension.function.name
      else "<anonymous>"
    ctx.withStackFrame(
      name,
      isNative = false,
      spanMap = suspension.function.spanMap
    ) {
      val loop = new BytecodeLoop(
        suspension.interpreter,
        suspension.frame,
        suspension.function,
        suspension.trace,
        suspension.newTarget
      )
      try {
        val result =
          if isThrow then loop.resumeWithThrow(value)
          else loop.resumeWithValue(value)
        Right(result)
      } catch {
        case next: AsyncSuspension => Left(next)
      }
    }
  }

  private[interpreter] val breakSignal = JSValue.Object(
    quickjs.objmodel.JSObject(prototype = null, extensible = false)
  )
  private[interpreter] val continueSignal = JSValue.Object(
    quickjs.objmodel.JSObject(prototype = null, extensible = false)
  )

  // Byte reading helpers
  private[interpreter] def readInt32(buf: Array[Byte], pc: Int): Int =
    ((buf(pc) & 0xff) << 24) | ((buf(pc + 1) & 0xff) << 16) |
      ((buf(pc + 2) & 0xff) << 8) | (buf(pc + 3) & 0xff)

  private[interpreter] def readDouble(buf: Array[Byte], pc: Int): Double =
    java.lang.Double.longBitsToDouble(readInt64(buf, pc))

  private[interpreter] def readInt64(buf: Array[Byte], pc: Int): Long =
    ((buf(pc).toLong & 0xff) << 56) | ((buf(pc + 1).toLong & 0xff) << 48) |
      ((buf(pc + 2).toLong & 0xff) << 40) | ((buf(
        pc + 3
      ).toLong & 0xff) << 32) |
      ((buf(pc + 4).toLong & 0xff) << 24) | ((buf(
        pc + 5
      ).toLong & 0xff) << 16) |
      ((buf(pc + 6).toLong & 0xff) << 8) | (buf(pc + 7).toLong & 0xff)

  private[interpreter] def readString(buf: Array[Byte], pc: Int): String = {
    val len = readInt32(buf, pc)
    val bytes = new Array[Byte](len)
    System.arraycopy(buf, pc + 4, bytes, 0, len)
    new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
  }

  // Comparison helpers (pure, no ctx needed)
  private[interpreter] def compare(a: JSValue, b: JSValue): Double =
    (a, b) match {
      case (JSValue.BigInt(x), JSValue.BigInt(y)) => x.compareTo(y).toDouble
      case (JSValue.BigInt(x), _)                 =>
        val nb = b.toNumber;
        if nb.isNaN then Double.NaN
        else
          new java.math.BigDecimal(x)
            .compareTo(new java.math.BigDecimal(nb))
            .toDouble
      case (_, JSValue.BigInt(y)) =>
        val na = a.toNumber;
        if na.isNaN then Double.NaN
        else
          new java.math.BigDecimal(na)
            .compareTo(new java.math.BigDecimal(y))
            .toDouble
      case (_: JSValue.JSStr, _: JSValue.JSStr) =>
        a.toString.compareTo(b.toString).toDouble
      case _ =>
        val na = a.toNumber; val nb = b.toNumber
        if na.isNaN || nb.isNaN then Double.NaN else na - nb
    }

  private[interpreter] def looseEqual(a: JSValue, b: JSValue)(using
      ctx: JSContext
  ): Boolean =
    (a, b) match {
      case (JSValue.Object(x), JSValue.Object(y)) => x eq y
      case (JSValue.JSArrayVal(x), JSValue.JSArrayVal(y)) => x eq y
      case (x: JSValue.Function, y: JSValue.Function) =>
        x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]
      case (JSValue.Native(x), JSValue.Native(y)) => x eq y
      case (JSValue.Symbol(x), JSValue.Symbol(y)) => x == y
      case (JSValue.Undefined, JSValue.Undefined) => true
      case (JSValue.Null, JSValue.Null)           => true
      case (JSValue.Bool(x), JSValue.Bool(y))     => x == y
      // `null`/`undefined` equality never coerces the other operand
      // (`obj == null` must not call ToPrimitive on `obj`).
      case (JSValue.Undefined, JSValue.Null) |
          (JSValue.Null, JSValue.Undefined) =>
        true
      case (JSValue.Undefined | JSValue.Null, _) |
          (_, JSValue.Undefined | JSValue.Null) =>
        false
      case (left, right) if isObjectLike(left) && !isObjectLike(right) =>
        looseEqual(
          quickjs.runtime.builtins.BuiltinHelpers.toPrimitive(left, "default"),
          right
        )
      case (left, right) if !isObjectLike(left) && isObjectLike(right) =>
        looseEqual(
          left,
          quickjs.runtime.builtins.BuiltinHelpers.toPrimitive(right, "default")
        )
      case (JSValue.BigInt(x), JSValue.BigInt(y)) => x == y
      case (JSValue.BigInt(x), _) if b.isNumber   =>
        val nb = b.toNumber;
        !nb.isNaN && new java.math.BigDecimal(x)
          .compareTo(new java.math.BigDecimal(nb)) == 0
      case (_, JSValue.BigInt(y)) if a.isNumber =>
        val na = a.toNumber;
        !na.isNaN && new java.math.BigDecimal(na)
          .compareTo(new java.math.BigDecimal(y)) == 0
      case (JSValue.BigInt(_), _) | (_, JSValue.BigInt(_)) => false
      case (_: JSValue.JSStr, _: JSValue.JSStr) => a.toString == b.toString
      case (_: JSValue.Bool, _) | (_, _: JSValue.Bool) =>
        a.toNumber == b.toNumber
      case (_: JSValue.JSStr, _: JSValue.Int32) |
          (_: JSValue.JSStr, _: JSValue.Float64) =>
        a.toNumber == b.toNumber
      case (_: JSValue.Int32, _: JSValue.JSStr) |
          (_: JSValue.Float64, _: JSValue.JSStr) =>
        a.toNumber == b.toNumber
      case (_: JSValue.Int32, _) | (_, _: JSValue.Int32) =>
        a.toNumber == b.toNumber
      case (_: JSValue.Float64, _) | (_, _: JSValue.Float64) =>
        a.toNumber == b.toNumber
      case _ => false
    }

  private def isObjectLike(value: JSValue): Boolean = value match {
    case JSValue.Object(_) | JSValue.JSArrayVal(_) | _: JSValue.Function |
        JSValue.Native(_) => true
    case _ => false
  }

  private[interpreter] def strictEqual(a: JSValue, b: JSValue): Boolean =
    (a, b) match {
      case (JSValue.BigInt(x), JSValue.BigInt(y))          => x == y
      case (JSValue.BigInt(_), _) | (_, JSValue.BigInt(_)) => false
      case (JSValue.Undefined, JSValue.Undefined)          => true
      case (JSValue.Null, JSValue.Null)                    => true
      case (JSValue.Bool(x), JSValue.Bool(y))              => x == y
      case (JSValue.Int32(x), JSValue.Int32(y))            => x == y
      case (JSValue.Float64(x), JSValue.Float64(y))        => x == y
      case (JSValue.Int32(x), JSValue.Float64(y))          => x.toDouble == y
      case (JSValue.Float64(x), JSValue.Int32(y))          => x == y.toDouble
      case (_: JSValue.JSStr, _: JSValue.JSStr)   => a.toString == b.toString
      case (JSValue.Object(x), JSValue.Object(y)) => x eq y
      case (JSValue.JSArrayVal(x), JSValue.JSArrayVal(y)) => x eq y
      case (_: JSValue.Function, _: JSValue.Function) =>
        a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]
      case (JSValue.Native(x), JSValue.Native(y)) =>
        x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]
      case (JSValue.Symbol(x), JSValue.Symbol(y)) => x == y
      case _                                      => false
    }

  /** ES ToInt32. Uses ToNumber (which invokes ToPrimitive on objects) rather
    * than the raw [[JSValue.toNumber]], which maps every object to NaN; the
    * bitwise operators previously returned 0 for `new Number(3) | 0`.
    */
  private[interpreter] def toInt32(v: JSValue)(using ctx: JSContext): Int = {
    val num = quickjs.runtime.builtins.BuiltinHelpers.toNumber(v)
    if num.isNaN || num.isInfinite then 0
    else {
      val modulo = num % 4294967296.0
      val int32 = modulo.toLong
      if int32 >= 2147483648L then (int32 - 4294967296L).toInt else int32.toInt
    }
  }

  // Built-in toString helpers (used in GetProp opcode)
  private[interpreter] def arrayToStringNative(
      arrVal: JSValue.JSArrayVal
  ): JSValue =
    JSValue.Native(
      quickjs.value.NativeFunction(
        name = "toString",
        impl = (args, _) =>
          args.headOption match {
            case Some(arr: JSValue.JSArrayVal) =>
              val arrObj = arr.value; val sb = new StringBuilder(); var i = 0
              while i < arrObj.getLength do {
                if i > 0 then sb.append(","); sb.append(arrObj.get(i).toString);
                i += 1
              }
              JSValue.fromString(sb.toString)
            case _ => JSValue.fromString("")
          }
      )
    )

  private[interpreter] def primitiveToStringNative(
      name: String,
      value: JSValue
  ): JSValue =
    JSValue.Native(
      quickjs.value.NativeFunction(
        name = name,
        impl = (args, _) =>
          args.headOption match {
            case Some(v) => JSValue.fromString(v.toString)
            case _       => JSValue.fromString("")
          }
      )
    )

  /** Resume an async function execution - placeholder for future await support.
    */
  def resumeAsyncFunction(
      asyncFunc: JSValue.AsyncFunction,
      value: JSValue,
      isThrow: Boolean
  )(using ctx: JSContext): JSValue = {
    import JSValue.AsyncState.*
    asyncFunc.state = Completed
    asyncFunc.promise.state = JSValue.PromiseState.Fulfilled
    asyncFunc.promise.result = value
    value
  }


  def apply(): Interpreter = new Interpreter()
}
