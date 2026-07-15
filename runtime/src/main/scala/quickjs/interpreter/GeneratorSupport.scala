package quickjs.interpreter

import quickjs.bytecode.*
import quickjs.value.JSValue
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject
import quickjs.objmodel.JSArray
import quickjs.tracing.TraceRecorder
import quickjs.runtime.builtins.BuiltinHelpers.{
  callFunctionWithThis,
  isCallable,
  wrapPromise
}
import scala.collection.mutable
import scala.util.control.Breaks.*

/** Generator-related logic extracted from Interpreter. */
private[interpreter] final class GeneratorSupport(interpreter: Interpreter) {

  /** Wrap a Generator value in a JSObject with next/return/throw methods */
  def wrapGenerator(gen: JSValue.Generator)(using ctx: JSContext): JSObject = {
    val obj = JSObject()
    obj.defineProperty("__generator", gen, enumerable = false)

    def asyncResult(operation: => JSValue)(using ctx2: JSContext): JSValue =
      try
        wrapPromise(
          JSValue.Promise(
            state = JSValue.PromiseState.Fulfilled,
            result = operation
          )
        )
      catch
        case e: quickjs.runtime.JSException =>
          wrapPromise(
            JSValue.Promise(
              state = JSValue.PromiseState.Rejected,
              result = e.getValue
            )
          )
        case e: RuntimeException =>
          wrapPromise(
            JSValue.Promise(
              state = JSValue.PromiseState.Rejected,
              result = interpreter.runtimeExceptionToError(e)
            )
          )

    def result(operation: => JSValue)(using ctx2: JSContext): JSValue =
      if gen.func.isAsync then asyncResult(operation) else operation

    // next(value) method
    obj.defineProperty(
      "next",
      JSValue.Native(
        quickjs.value.NativeFunction(
          name = "next",
          impl = (args, ctx2) =>
            val value = args.lift(1).getOrElse(JSValue.Undefined)
            result(resumeGenerator(gen, value, isThrow = false)(using ctx2))(
              using ctx2
            )
        )
      ),
      enumerable = false
    )

    // return(value) method
    obj.defineProperty(
      "return",
      JSValue.Native(
        quickjs.value.NativeFunction(
          name = "return",
          impl = (args, ctx2) =>
            val value = args.lift(1).getOrElse(JSValue.Undefined)
            result({
              gen.state = JSValue.GeneratorState.Completed
              gen.makeResult(value, done = true)(using ctx2)
            })(using ctx2)
        )
      ),
      enumerable = false
    )

    // throw(error) method
    obj.defineProperty(
      "throw",
      JSValue.Native(
        quickjs.value.NativeFunction(
          name = "throw",
          impl = (args, ctx2) =>
            val error = args.lift(1).getOrElse(JSValue.Undefined)
            result(resumeGenerator(gen, error, isThrow = true)(using ctx2))(
              using ctx2
            )
        )
      ),
      enumerable = false
    )

    val iteratorName =
      if gen.func.isAsync then "Symbol.asyncIterator" else "Symbol.iterator"
    obj.defineProperty(
      iteratorName,
      JSValue.Native(
        quickjs.value.NativeFunction(
          name = s"[$iteratorName]",
          impl = (args, _) => args(0)
        )
      ),
      enumerable = false
    )

    // Generator objects return themselves from their matching iterator method.
    ctx.global.get("Symbol") match {
      case JSValue.Native(symbolCtor: quickjs.value.NativeConstructor) =>
        val symbolName = if gen.func.isAsync then "asyncIterator" else "iterator"
        symbolCtor.funcObj.get(symbolName)(using ctx) match {
          case JSValue.Symbol(sym) =>
            obj.initSymbolProperty(
              sym,
              JSValue.Native(
                quickjs.value.NativeFunction(
                  name = s"[Symbol.$symbolName]",
                  length = 0,
                  impl = (args, _) => args.headOption.getOrElse(JSValue.Undefined)
                )
              ),
              enumerable = false,
              writable = true,
              configurable = true
            )
          case _ => ()
        }
      case _ => ()
    }

    obj
  }

  /** Resume a suspended generator */
  def resumeGenerator(gen: JSValue.Generator, value: JSValue, isThrow: Boolean)(
      using ctx: JSContext
  ): JSValue = {
    import JSValue.GeneratorState.*
    import Interpreter.{readInt32, readDouble, readString}

    gen.state match {
      case Completed =>
        return gen.makeResult(JSValue.Undefined, done = true)
      case Executing =>
        return ctx.throwTypeError("Generator is already executing")
      case _ => ()
    }

    val func = gen.func
    val function = new BytecodeFunction(
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
      length = func.paramNames.length,
      spanMap = func.spanMap,
      isStrict = func.isStrict,
      parameterScopeEndPc = func.parameterScopeEndPc
    )

    val frameName =
      if function.name.nonEmpty then function.name else "<anonymous>"
    ctx.withStackFrame(frameName, isNative = false, spanMap = function.spanMap) {
      var stack = gen.stack
      var stackTop = gen.stackTop
      var pc = gen.suspendedPc
      val bytecode = function.bytecode

      val locals = new Array[JSValue.VarRef](256)
      for i <- 0 until 256 do locals(i) = new JSValue.VarRef(gen.vars(i))

      val starting = gen.state == SuspendedStart && gen.suspendedPc == 0
      if starting then
        for i <- gen.args.indices do locals(i).set(gen.args(i))

      if starting && function.argumentsIndex >= 0 && function.argumentsIndex < 256 then {
        val argumentsObj = quickjs.objmodel.JSObject(
          prototype = ctx.objectPrototype,
          extensible = true
        )
        var i = 0
        while i < gen.args.length do {
          argumentsObj.set(i.toString, gen.args(i))(using ctx)
          i += 1
        }
        argumentsObj.set("length", JSValue.fromInt(gen.args.length))(using ctx)
        locals(function.argumentsIndex).set(JSValue.Object(argumentsObj))
      }

      if isThrow then gen.pendingThrow = Some(value)
      else {
        gen.pendingValue = value
        gen.pendingThrow = None
      }

      val wasSuspendedYield = gen.state == SuspendedYield
      gen.state = Executing

      if wasSuspendedYield then {
        stack(stackTop) = gen.pendingValue
        stackTop += 1
      }

      var result: JSValue = JSValue.Undefined
      var generatorYielded = false
      var yieldedValue: JSValue = JSValue.Undefined
      var generatorReturned = false
      var returnValue: JSValue = JSValue.Undefined

      var iterations = 0
      val maxIterations = 100000

      val tryStack = mutable.ArrayBuffer.empty[TryHandler]
      var lastException: JSValue = JSValue.Undefined
      var pendingException: Option[JSValue] = None

      def iteratorSymbolId: Int =
        ctx.global.get("Symbol") match {
          case JSValue.Native(ctor: quickjs.value.NativeConstructor) =>
            ctor.funcObj.get("iterator")(using ctx) match {
              case JSValue.Symbol(id) => id
              case _ => ctx.throwTypeError("Symbol.iterator is not available")
            }
          case _ => ctx.throwTypeError("Symbol is not available")
        }

      def getIteratorMethod(iterable: JSValue): JSValue = {
        val symbolId = iteratorSymbolId
        iterable match {
          case JSValue.Object(obj) =>
            interpreter.getPropertyValueBySymbol(
              obj,
              iterable,
              symbolId,
              Nil,
              TraceRecorder.Noop
            )
          case JSValue.JSArrayVal(array) =>
            array.getOwnSymbol(symbolId).getOrElse(
              interpreter.getPropertyValueBySymbol(
                ctx.arrayPrototype,
                iterable,
                symbolId,
                Nil,
                TraceRecorder.Noop
              )
            )
          case fn: JSValue.Function =>
            interpreter.getPropertyValueBySymbol(
              fn.funcObj,
              iterable,
              symbolId,
              Nil,
              TraceRecorder.Noop
            )
          case JSValue.Native(fn: quickjs.value.NativeFunction) =>
            interpreter.getPropertyValueBySymbol(
              fn.funcObj,
              iterable,
              symbolId,
              Nil,
              TraceRecorder.Noop
            )
          case JSValue.Native(ctor: quickjs.value.NativeConstructor) =>
            interpreter.getPropertyValueBySymbol(
              ctor.funcObj,
              iterable,
              symbolId,
              Nil,
              TraceRecorder.Noop
            )
          case _ => JSValue.Undefined
        }
      }

      // QuickJS C lowers yield* through GetIterator before entering its
      // delegation loop. Keep the same separation here: the bytecode leaves
      // the iterable on the stack and the runtime stores the resulting
      // iterator across suspensions.
      def getIterator(iterable: JSValue): JSValue = {
        val method = getIteratorMethod(iterable)
        if !isCallable(method) then
          ctx.throwTypeError("yield* requires an iterable")
        callFunctionWithThis(method, iterable, Array.empty) match {
          case result @ (JSValue.Object(_) | JSValue.JSArrayVal(_) |
              _: JSValue.Function | JSValue.Native(_)) => result
          case _ => ctx.throwTypeError("Iterator method did not return an object")
        }
      }

      def handleException(value: JSValue): Boolean =
        if tryStack.nonEmpty then {
          val handler = tryStack.remove(tryStack.length - 1)
          stackTop = handler.stackTop
          lastException = value
          if handler.catchPc >= 0 then {
            pc = handler.catchPc
            stack(stackTop) = value
            stackTop += 1
            true
          }
          else if handler.finallyPc >= 0 then {
            pendingException = Some(value)
            pc = handler.finallyPc
            true
          }
          else false
        }
        else false

      breakable {
        while pc < bytecode.length do {
          iterations += 1
          if iterations > maxIterations then
            throw new RuntimeException(s"Infinite loop detected in generator")

          val opcode =
            Opcode.fromCode(bytecode(pc) & 0xff).getOrElse(Opcode.Invalid)
          pc += 1

          opcode match {
            case Opcode.InitialYield =>
              gen.suspendedPc = pc
              gen.stack = stack
              gen.stackTop = stackTop
              for i <- 0 until 256 do gen.vars(i) = locals(i).get
              gen.state = SuspendedStart
              generatorYielded = true
              break

            case Opcode.Yield =>
              yieldedValue = stack(stackTop - 1)
              stackTop -= 1
              gen.suspendedPc = pc
              gen.stack = stack
              gen.stackTop = stackTop
              for i <- 0 until 256 do gen.vars(i) = locals(i).get
              gen.state = SuspendedYield
              generatorYielded = true
              break

            case Opcode.YieldStar =>
              val iteratorValue = gen.delegatedIterator match {
                case Some(iter) => iter
                case None       =>
                  val iterable = stack(stackTop - 1)
                  stackTop -= 1
                  val iter = getIterator(iterable)
                  gen.delegatedIterator = Some(iter)
                  iter
              }

              iteratorValue match {
                case JSValue.Object(obj) =>
                  val nextMethod = obj.get("next")(using ctx)
                  val nextResult = nextMethod match {
                    case JSValue.Native(native: quickjs.value.NativeFunction) =>
                      native.call(Array(iteratorValue, JSValue.Undefined))
                    case f: JSValue.Function =>
                      val bcFunc = new BytecodeFunction(
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
                        isAsync = f.isAsync,
                        length = f.paramNames.length,
                        spanMap = f.spanMap,
                        isStrict = f.isStrict,
                        parameterScopeEndPc = f.parameterScopeEndPc
                      )
                      interpreter.call(
                        bcFunc,
                        iteratorValue,
                        Array(JSValue.Undefined),
                        f.closure
                      )
                    case _ =>
                      ctx.throwTypeError("Iterator next is not a function")
                  }

                  nextResult match {
                    case JSValue.Object(resultObj) =>
                      val doneVal = resultObj.get("done")(using ctx)
                      val valueVal = resultObj.get("value")(using ctx)
                      val isDone = doneVal == JSValue.Bool(true)

                      if isDone then {
                        gen.delegatedIterator = None
                        stack(stackTop) = valueVal
                        stackTop += 1
                      }
                      else {
                        gen.suspendedPc = pc - 1
                        gen.stack = stack
                        gen.stackTop = stackTop
                        for i <- 0 until 256 do gen.vars(i) = locals(i).get
                        gen.state = SuspendedYield
                        yieldedValue = valueVal
                        generatorYielded = true
                        break
                      }
                    case _ =>
                      ctx.throwTypeError(
                        "Iterator.next() did not return an object"
                      )
                  }
                case _ =>
                  ctx.throwTypeError("yield* requires an iterable")
              }

            case Opcode.Return =>
              result = stack(stackTop - 1)
              gen.state = Completed
              returnValue = gen.makeResult(result, done = true)
              generatorReturned = true
              break

            case Opcode.ReturnUndef =>
              gen.state = Completed
              returnValue = gen.makeResult(JSValue.Undefined, done = true)
              generatorReturned = true
              break

            case _ if gen.pendingThrow.isDefined && pc == gen.suspendedPc =>
              val ex = gen.pendingThrow.get
              gen.pendingThrow = None
              if !handleException(ex) then {
                gen.state = Completed
                throw quickjs.runtime.JSException(ex)
              }

            case Opcode.PushI32 =>
              val value = readInt32(bytecode, pc)
              pc += 4
              stack(stackTop) = JSValue.Int32(value)
              stackTop += 1

            case Opcode.PushFloat64 =>
              val value = readDouble(bytecode, pc)
              pc += 8
              stack(stackTop) = JSValue.Float64(value)
              stackTop += 1

            case Opcode.PushUndefined =>
              stack(stackTop) = JSValue.Undefined
              stackTop += 1

            case Opcode.PushNull =>
              stack(stackTop) = JSValue.Null
              stackTop += 1

            case Opcode.PushTrue =>
              stack(stackTop) = JSValue.Bool(true)
              stackTop += 1

            case Opcode.PushFalse =>
              stack(stackTop) = JSValue.Bool(false)
              stackTop += 1

            case Opcode.Drop =>
              stackTop -= 1

            case Opcode.Dup =>
              stack(stackTop) = stack(stackTop - 1)
              stackTop += 1

            case Opcode.Swap =>
              val top = stack(stackTop - 1)
              stack(stackTop - 1) = stack(stackTop - 2)
              stack(stackTop - 2) = top

            case Opcode.Rotate =>
              val a = stack(stackTop - 1)
              val b = stack(stackTop - 2)
              val c = stack(stackTop - 3)
              stack(stackTop - 1) = c
              stack(stackTop - 2) = a
              stack(stackTop - 3) = b

            case Opcode.Dup2 =>
              stack(stackTop) = stack(stackTop - 2)
              stack(stackTop + 1) = stack(stackTop - 1)
              stackTop += 2

            case Opcode.Nip =>
              stack(stackTop - 2) = stack(stackTop - 1)
              stackTop -= 1

            case Opcode.GetLoc =>
              val index = readInt32(bytecode, pc)
              pc += 4
              val value = locals(index).get
              if value == JSValue.Uninitialized then
                throw new RuntimeException(
                  "ReferenceError: Cannot access binding before initialization"
                )
              stack(stackTop) = value
              stackTop += 1

            case Opcode.GetArg =>
              val index = readInt32(bytecode, pc)
              pc += 4
              stack(stackTop) =
                if index >= 0 && index < gen.args.length then gen.args(index)
                else JSValue.Undefined
              stackTop += 1

            case Opcode.GetRestArgs =>
              val index = readInt32(bytecode, pc)
              pc += 4
              val rest = JSArray.empty()
              var i = math.max(index, 0)
              while i < gen.args.length do {
                rest.push(gen.args(i))
                i += 1
              }
              stack(stackTop) = JSValue.JSArrayVal(rest)
              stackTop += 1

            case Opcode.PutLoc =>
              val index = readInt32(bytecode, pc)
              pc += 4
              stackTop -= 1
              locals(index).set(stack(stackTop))

            case Opcode.SetLocUninitialized =>
              val index = readInt32(bytecode, pc)
              pc += 4
              locals(index).set(JSValue.Uninitialized)

            case Opcode.GetLocCheck =>
              val index = readInt32(bytecode, pc)
              pc += 4
              val local = locals(index).get
              if local == JSValue.Uninitialized then
                throw new RuntimeException("ReferenceError: Cannot access binding before initialization")
              stack(stackTop) = local
              stackTop += 1

            case Opcode.SetLocConst =>
              val index = readInt32(bytecode, pc)
              pc += 4
              locals(index).setConst()

            case Opcode.GetThis =>
              stack(stackTop) = gen.thisArg
              stackTop += 1

            case Opcode.StrictEq =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 1
              stack(stackTop - 1) = JSValue.Bool(Interpreter.strictEqual(a, b))

            case Opcode.StrictNeq =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 1
              stack(stackTop - 1) = JSValue.Bool(!Interpreter.strictEqual(a, b))

            case Opcode.IfFalse =>
              val offset = readInt32(bytecode, pc)
              stackTop -= 1
              if !stack(stackTop).toBoolean then pc += offset else pc += 4

            case Opcode.IfTrue =>
              val offset = readInt32(bytecode, pc)
              stackTop -= 1
              if stack(stackTop).toBoolean then pc += offset else pc += 4

            case Opcode.Goto =>
              val offset = readInt32(bytecode, pc)
              pc += offset

            case Opcode.Comma =>
              stack(stackTop - 2) = stack(stackTop - 1)
              stackTop -= 1

            case Opcode.GetConst =>
              val index = readInt32(bytecode, pc)
              pc += 4
              val constValue = function.constants(index)
              val value = constValue match {
                case bcFunc: BytecodeFunction =>
                  val newClosure = mutable.Map.empty[String, JSValue.VarRef]
                  for varName <- bcFunc.freeVars do
                    gen.closure.get(varName) match {
                      case Some(varRef) => newClosure(varName) = varRef
                      case None         => ()
                    }
                  JSValue.Function(
                    name = bcFunc.name,
                    bytecode = bcFunc.bytecode,
                    constants = bcFunc.constants,
                    stackSize = bcFunc.stackSize,
                    closure = newClosure,
                    paramNames = bcFunc.paramNames,
                    localVarNames = bcFunc.localVarNames,
                    parentLocalVarNames = function.localVarNames,
                    argumentsIndex = bcFunc.argumentsIndex,
                    isConstructor = bcFunc.isConstructor,
                    isGenerator = bcFunc.isGenerator,
                    isAsync = bcFunc.isAsync,
                    funcObj = JSObject(
                      prototype = ctx.functionPrototype,
                      extensible = true
                    ),
                    spanMap = bcFunc.spanMap,
                    isStrict = bcFunc.isStrict,
                    parameterScopeEndPc = bcFunc.parameterScopeEndPc
                  )
                case jsValue: JSValue => jsValue
                case _                => JSValue.Undefined
              }
              stack(stackTop) = value
              stackTop += 1

            case Opcode.GetGlobal =>
              val name = readString(bytecode, pc)
              pc += 4 + name
                .getBytes(java.nio.charset.StandardCharsets.UTF_8)
                .length
              val localIndex = function.localVarNames.indexOf(name)
              val paramIndex = function.paramNames.indexOf(name)
              def checkedLocal(index: Int): JSValue =
                val value = locals(index).get
                if value == JSValue.Uninitialized then
                  throw new RuntimeException(
                    s"ReferenceError: Cannot access '$name' before initialization"
                  )
                value
              val value =
                if name == "$newTarget" then JSValue.Undefined
                else if paramIndex >= 0 then checkedLocal(paramIndex)
                else if localIndex >= 0 then checkedLocal(localIndex)
                else gen.closure.get(name) match {
                case Some(varRef) =>
                  varRef.get match {
                    case JSValue.GlobalRef(refName) =>
                      ctx.globalScope.getVariable(refName).orElse {
                        if ctx.global.hasProperty(refName)(using ctx) then
                          Some(ctx.global.get(refName)(using ctx))
                        else None
                      }.getOrElse {
                        throw new RuntimeException(
                          s"ReferenceError: $refName is not defined"
                        )
                      }
                    case other => other
                  }
                case None =>
                  ctx.globalScope.getVariable(name).orElse {
                    if ctx.global.hasProperty(name)(using ctx) then
                      Some(ctx.global.get(name)(using ctx))
                    else None
                  }.getOrElse {
                    throw new RuntimeException(
                      s"ReferenceError: $name is not defined"
                    )
                  }
              }
              stack(stackTop) = value
              stackTop += 1

            case Opcode.GetGlobalOrUndefined =>
              val name = readString(bytecode, pc)
              pc += 4 + name
                .getBytes(java.nio.charset.StandardCharsets.UTF_8)
                .length
              val localIndex = function.localVarNames.indexOf(name)
              val paramIndex = function.paramNames.indexOf(name)
              def checkedLocal(index: Int): JSValue =
                val value = locals(index).get
                if value == JSValue.Uninitialized then
                  throw new RuntimeException(
                    s"ReferenceError: Cannot access '$name' before initialization"
                  )
                value
              val value =
                if paramIndex >= 0 then checkedLocal(paramIndex)
                else if localIndex >= 0 then checkedLocal(localIndex)
                else gen.closure.get(name) match {
                case Some(varRef) =>
                  varRef.get match {
                    case JSValue.GlobalRef(refName) =>
                      ctx.globalScope
                        .getVariable(refName)
                        .orElse {
                          val gv = ctx.global.get(refName)
                          if gv != JSValue.Undefined then Some(gv) else None
                        }
                        .getOrElse(JSValue.Undefined)
                    case other => other
                  }
                case None =>
                  ctx.globalScope
                    .getVariable(name)
                    .orElse {
                      val gv = ctx.global.get(name)
                      if gv != JSValue.Undefined then Some(gv) else None
                    }
                    .getOrElse(JSValue.Undefined)
              }
              stack(stackTop) = value
              stackTop += 1

            case Opcode.PutGlobal =>
              val name = readString(bytecode, pc)
              pc += 4 + name.getBytes(java.nio.charset.StandardCharsets.UTF_8).length
              stackTop -= 1
              val assigned = stack(stackTop)
              val localIndex = function.localVarNames.indexOf(name)
              val paramIndex = function.paramNames.indexOf(name)
              if paramIndex >= 0 then locals(paramIndex).set(assigned)
              else if localIndex >= 0 then locals(localIndex).set(assigned)
              else gen.closure.get(name) match {
                case Some(ref) =>
                  ref.get match {
                    case JSValue.GlobalRef(globalName) =>
                      if ctx.globalScope.has(globalName) then
                        ctx.globalScope.setVariable(globalName, assigned)
                      else ctx.global.set(globalName, assigned)(using ctx)
                    case _ => ref.set(assigned)
                  }
                case None =>
                  if ctx.globalScope.has(name) then
                    ctx.globalScope.setVariable(name, assigned)
                  else ctx.global.set(name, assigned)(using ctx)
              }

            case Opcode.DefVar | Opcode.DefFun =>
              val name = readString(bytecode, pc)
              pc += 4 + name.getBytes(java.nio.charset.StandardCharsets.UTF_8).length
              stackTop -= 1
              ctx.globalScope.setVariable(name, stack(stackTop))

            case Opcode.GetProp =>
              val name = readString(bytecode, pc)
              pc += 4 + name.getBytes(java.nio.charset.StandardCharsets.UTF_8).length
              stackTop -= 1
              val receiver = stack(stackTop)
              val property = receiver match {
                case JSValue.Object(obj) =>
                  interpreter.getPropertyValue(
                    obj,
                    receiver,
                    name,
                    Nil,
                    TraceRecorder.Noop
                  )
                case fn: JSValue.Function =>
                  interpreter.getPropertyValue(
                    fn.funcObj,
                    fn,
                    name,
                    Nil,
                    TraceRecorder.Noop
                  )
                case JSValue.Native(fn: quickjs.value.NativeFunction) =>
                  interpreter.getPropertyValue(
                    fn.funcObj,
                    receiver,
                    name,
                    Nil,
                    TraceRecorder.Noop
                  )
                case JSValue.Native(ctor: quickjs.value.NativeConstructor) =>
                  interpreter.getPropertyValue(
                    ctor.funcObj,
                    receiver,
                    name,
                    Nil,
                    TraceRecorder.Noop
                  )
                case arr: JSValue.JSArrayVal =>
                  if name == "length" then JSValue.fromInt(arr.value.length)
                  else arr.value.getProperty(name).getOrElse(ctx.arrayPrototype.get(name)(using ctx))
                case _ => JSValue.Undefined
              }
              stack(stackTop) = property
              stackTop += 1

            case Opcode.SetProp =>
              val name = readString(bytecode, pc)
              pc += 4 + name.getBytes(java.nio.charset.StandardCharsets.UTF_8).length
              val assigned = stack(stackTop - 1)
              val receiver = stack(stackTop - 2)
              stackTop -= 2
              receiver match {
                case JSValue.Object(obj) => obj.set(name, assigned)(using ctx)
                case fn: JSValue.Function => fn.funcObj.set(name, assigned)(using ctx)
                case arr: JSValue.JSArrayVal => arr.value.setProperty(name, assigned)
                case _ => ctx.throwTypeError("Cannot set property on non-object")
              }
              stack(stackTop) = assigned
              stackTop += 1

            case Opcode.NewObject =>
              stack(stackTop) = JSValue.Object(
                JSObject(prototype = ctx.objectPrototype, extensible = true)
              )
              stackTop += 1

            case Opcode.NewArray =>
              val size = readInt32(bytecode, pc)
              pc += 4
              stack(stackTop) = JSValue.JSArrayVal(JSArray(size))
              stackTop += 1

            case Opcode.InitElem =>
              val assigned = stack(stackTop - 1)
              val key = stack(stackTop - 2)
              val receiver = stack(stackTop - 3)
              stackTop -= 3
              (receiver, key) match {
                case (arr: JSValue.JSArrayVal, JSValue.Int32(index)) =>
                  arr.value.set(index, assigned)
                case (JSValue.Object(obj), JSValue.JSStr(name)) =>
                  obj.set(name, assigned)(using ctx)
                case _ => ()
              }
              stack(stackTop) = receiver
              stackTop += 1

            case Opcode.GetElem =>
              val key = stack(stackTop - 1)
              val receiver = stack(stackTop - 2)
              stackTop -= 2
              val keyString = key match {
                case JSValue.JSStr(value) => value
                case JSValue.Int32(value) => value.toString
                case JSValue.Float64(value) if value.isWhole => value.toLong.toString
                case _ => key.toString
              }
              val element = receiver match {
                case arr: JSValue.JSArrayVal =>
                  keyString.toIntOption match {
                    case Some(index) => arr.value.get(index)
                    case None => arr.value.getProperty(keyString).getOrElse(JSValue.Undefined)
                  }
                case JSValue.Object(obj) =>
                  interpreter.getPropertyValue(
                    obj, receiver, keyString, Nil, TraceRecorder.Noop
                  )
                case JSValue.JSStr(value) =>
                  keyString.toIntOption
                    .filter(index => index >= 0 && index < value.length)
                    .map(index => JSValue.fromString(value.charAt(index).toString))
                    .getOrElse(JSValue.Undefined)
                case _ => JSValue.Undefined
              }
              stack(stackTop) = element
              stackTop += 1

            case Opcode.CallMethod =>
              val argc = readInt32(bytecode, pc)
              pc += 4
              val thisValue = stack(stackTop - argc - 2)
              val method = stack(stackTop - argc - 1)
              val methodArgs = Array.tabulate(argc)(i => stack(stackTop - argc + i))
              stackTop -= argc + 2
              val callResult = method match {
                case f: JSValue.Function =>
                  val bcFunc = new BytecodeFunction(
                    f.name, f.bytecode, f.constants, f.stackSize,
                    Array.empty, f.paramNames, f.localVarNames,
                    f.argumentsIndex, f.isConstructor, f.isGenerator,
                    f.isAsync, f.paramNames.length, f.spanMap, f.isStrict,
                    parameterScopeEndPc = f.parameterScopeEndPc
                  )
                  interpreter.call(bcFunc, thisValue, methodArgs, f.closure)
                case JSValue.Native(native: quickjs.value.NativeFunction) =>
                  native.call(Array(thisValue) ++ methodArgs)
                case other =>
                  ctx.throwTypeError(
                    s"Value is not a function (generator method call: $other)"
                  )
              }
              stack(stackTop) = callResult
              stackTop += 1

            case Opcode.Call =>
              val argc = readInt32(bytecode, pc)
              pc += 4
              val funcVal = stack(stackTop - 1 - argc)
              val callArgs =
                (0 until argc).map(i => stack(stackTop - argc + i)).toArray
              stackTop -= argc + 1

              val callResult = funcVal match {
                case JSValue.Native(native: quickjs.value.NativeFunction) =>
                  native.call(callArgs)
                case f: JSValue.Function =>
                  val bcFunc = new BytecodeFunction(
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
                    isAsync = f.isAsync,
                    length = f.paramNames.length,
                    spanMap = f.spanMap,
                    isStrict = f.isStrict,
                    parameterScopeEndPc = f.parameterScopeEndPc
                  )
                  interpreter.call(
                    bcFunc,
                    JSValue.Undefined,
                    callArgs,
                    f.closure
                  )
                case other =>
                  ctx.throwTypeError(
                    s"Value is not a function (generator call: $other)"
                  )
              }

              stack(stackTop) = callResult
              stackTop += 1

            case Opcode.Add =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 1
              stack(stackTop - 1) = JSValue.add(a, b)

            case Opcode.Sub =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 1
              stack(stackTop - 1) = JSValue.subtract(a, b)

            case Opcode.Mul =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 1
              stack(stackTop - 1) = JSValue.multiply(a, b)

            case Opcode.Div =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 1
              stack(stackTop - 1) = JSValue.divide(a, b)

            case Opcode.Lt | Opcode.Lte | Opcode.Gt | Opcode.Gte =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 1
              val comparison = Interpreter.compare(a, b)
              val matches = opcode match {
                case Opcode.Lt  => comparison < 0
                case Opcode.Lte => comparison <= 0
                case Opcode.Gt  => comparison > 0
                case Opcode.Gte => comparison >= 0
                case _          => false
              }
              stack(stackTop - 1) = JSValue.Bool(matches)

            case Opcode.Eq | Opcode.Neq =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 1
              val equal = Interpreter.looseEqual(a, b)
              stack(stackTop - 1) =
                JSValue.Bool(if opcode == Opcode.Eq then equal else !equal)

            case Opcode.PreInc | Opcode.PreDec =>
              val original = stack(stackTop - 1)
              val delta = if opcode == Opcode.PreInc then 1 else -1
              stack(stackTop - 1) = original match {
                case JSValue.BigInt(value) =>
                  JSValue.BigInt(value.add(java.math.BigInteger.valueOf(delta)))
                case _ => JSValue.fromDouble(original.toNumber + delta)
              }

            case Opcode.PostInc | Opcode.PostDec =>
              val original = stack(stackTop - 1)
              val delta = if opcode == Opcode.PostInc then 1 else -1
              val oldValue = original match {
                case _: JSValue.BigInt => original
                case _                 => JSValue.fromDouble(original.toNumber)
              }
              val newValue = original match {
                case JSValue.BigInt(value) =>
                  JSValue.BigInt(value.add(java.math.BigInteger.valueOf(delta)))
                case _ => JSValue.fromDouble(original.toNumber + delta)
              }
              stack(stackTop - 1) = oldValue
              stack(stackTop) = newValue
              stackTop += 1

            case _ =>
              throw new RuntimeException(
                s"Unimplemented generator opcode: $opcode at pc ${pc - 1}"
              )
          }
        }
      }

      if generatorReturned then returnValue
      else if generatorYielded then gen.makeResult(yieldedValue, done = false)
      else gen.makeResult(JSValue.Undefined, done = true)
    }
  }
}
