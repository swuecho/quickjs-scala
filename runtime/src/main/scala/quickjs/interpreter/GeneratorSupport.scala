package quickjs.interpreter

import quickjs.bytecode.*
import quickjs.value.JSValue
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject
import scala.collection.mutable
import scala.util.control.Breaks.*

/** Generator-related logic extracted from Interpreter. */
private[interpreter] final class GeneratorSupport(interpreter: Interpreter) {

  /** Wrap a Generator value in a JSObject with next/return/throw methods */
  def wrapGenerator(gen: JSValue.Generator)(using ctx: JSContext): JSObject = {
    val obj = JSObject()
    obj.defineProperty("__generator", gen, enumerable = false)

    // next(value) method
    obj.defineProperty(
      "next",
      JSValue.Native(
        quickjs.value.NativeFunction(
          name = "next",
          impl = (args, ctx2) =>
            val value = args.lift(1).getOrElse(JSValue.Undefined)
            resumeGenerator(gen, value, isThrow = false)(using ctx2)
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
            gen.state = JSValue.GeneratorState.Completed
            gen.makeResult(value, done = true)(using ctx2)
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
            resumeGenerator(gen, error, isThrow = true)(using ctx2)
        )
      ),
      enumerable = false
    )

    // Symbol.iterator - returns the generator itself
    obj.defineProperty(
      "Symbol.iterator",
      JSValue.Native(
        quickjs.value.NativeFunction(
          name = "[Symbol.iterator]",
          impl = (args, _) => args(0)
        )
      ),
      enumerable = false
    )

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

      for i <- gen.args.indices do locals(i).set(gen.args(i))

      if function.argumentsIndex >= 0 && function.argumentsIndex < 256 then {
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
            // Skip - generator already created

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
                  val iter = stack(stackTop - 1)
                  stackTop -= 1
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

            case Opcode.GetLoc =>
              val index = readInt32(bytecode, pc)
              pc += 4
              stack(stackTop) = locals(index).get
              stackTop += 1

            case Opcode.PutLoc =>
              val index = readInt32(bytecode, pc)
              pc += 4
              locals(index).set(stack(stackTop - 1))

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
              val value = gen.closure.get(name) match {
                case Some(varRef) =>
                  varRef.get match {
                    case JSValue.GlobalRef(refName) =>
                      ctx.globalScope
                        .getVariable(refName)
                        .orElse(Some(ctx.global.get(name)))
                        .getOrElse(JSValue.Undefined)
                    case other => other
                  }
                case None =>
                  ctx.globalScope
                    .getVariable(name)
                    .orElse(Some(ctx.global.get(name)))
                    .getOrElse(JSValue.Undefined)
              }
              stack(stackTop) = value
              stackTop += 1

            case Opcode.GetGlobalOrUndefined =>
              val name = readString(bytecode, pc)
              pc += 4 + name
                .getBytes(java.nio.charset.StandardCharsets.UTF_8)
                .length
              val value = gen.closure.get(name) match {
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

            case Opcode.Call =>
              val argc = readInt32(bytecode, pc)
              pc += 4
              val funcVal = stack(stackTop - 1 - argc)
              val callArgs =
                (0 until argc).map(i => stack(stackTop - argc + i)).toArray
              stackTop -= argc + 1

              val callResult = funcVal match {
                case JSValue.Native(native: quickjs.value.NativeFunction) =>
                  val argsWithThis = new Array[JSValue](callArgs.length + 1)
                  argsWithThis(0) = JSValue.Undefined
                  Array.copy(callArgs, 0, argsWithThis, 1, callArgs.length)
                  native.call(argsWithThis)
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
                case _ =>
                  ctx.throwTypeError("Value is not a function")
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

            case _ =>
              ()
          }
        }
      }

      if generatorReturned then returnValue
      else if generatorYielded then gen.makeResult(yieldedValue, done = false)
      else gen.makeResult(JSValue.Undefined, done = true)
    }
  }
}
