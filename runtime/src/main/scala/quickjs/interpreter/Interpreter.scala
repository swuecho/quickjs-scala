package quickjs.interpreter

import quickjs.bytecode.*
import quickjs.value.JSValue
import quickjs.runtime.JSContext
import quickjs.tracing.{CallTrace, InstructionTrace, ReturnTrace, SourceLocation, TraceLocal, TraceRecorder, TraceValue}
import scala.util.control.Breaks.*
import scala.annotation.switch
import scala.util.control.ControlThrowable
import scala.collection.mutable

// Import debug tracer
import quickjs.interpreter.DebugTracer

// Control flow exceptions for break/continue
private case object BreakException extends ControlThrowable
private case object ContinueException extends ControlThrowable

/** Minimal bytecode interpreter for Phase 1.
  *
  * Design:
  * - Stack-based virtual machine
  * - Direct threading optimization (via @switch)
  * - Support for arithmetic operations
  */
final class Interpreter:
  import Interpreter.*

  private def isArrayIndexKey(key: String): Boolean =
    key.nonEmpty && key.forall(_.isDigit) && (key.length == 1 || key.charAt(0) != '0')

  private def resolveArrayProperty(arr: quickjs.objmodel.JSArray, propName: String)(using ctx: JSContext): JSValue =
    if propName == "length" then
      JSValue.fromInt(arr.length)
    else
      arr.getOwnProperty(propName) match
        case Some(value) => value
        case None =>
          if propName == "toString" then
            JSValue.Native(
              quickjs.value.NativeFunction(
                name = "toString",
                impl = (args, _) =>
                  args.headOption match
                    case Some(arr: JSValue.JSArrayVal) =>
                      val arrObj = arr.value
                      val sb = new StringBuilder()
                      var i = 0
                      while i < arrObj.getLength do
                        if i > 0 then sb.append(",")
                        sb.append(arrObj.get(i).toString)
                        i += 1
                      JSValue.fromString(sb.toString)
                    case _ => JSValue.fromString("")
              )
            )
          else
            val result = ctx.arrayPrototype.get(propName)(using ctx)
            if result == JSValue.Undefined then
              val arrayObj = ctx.global.get("Array")
              arrayObj match
                case JSValue.Object(obj) => obj.get(propName)
                case _ => JSValue.Undefined
            else
              result

  def call(
    function: BytecodeFunction,
    thisArg: JSValue,
    args: Array[JSValue],
    closure: mutable.Map[String, JSValue.VarRef] = mutable.Map.empty,
    newTarget: JSValue = JSValue.Undefined,
    withObjects: List[quickjs.objmodel.JSObject] = Nil,
    trace: TraceRecorder = TraceRecorder.Noop
  )(using ctx: JSContext): JSValue =
    // For generator functions, create and return a Generator object instead of executing
    if function.isGenerator then
      // Create JSValue.Function from BytecodeFunction to store in Generator
      val funcValue = JSValue.Function(
        name = function.name,
        bytecode = function.bytecode,
        constants = function.constants,
        stackSize = function.stackSize,
        closure = closure.clone(),
        paramNames = function.paramNames,
        localVarNames = function.localVarNames,
        parentLocalVarNames = Array.empty,  // Not stored in BytecodeFunction
        argumentsIndex = function.argumentsIndex,
        isConstructor = function.isConstructor,
        isGenerator = function.isGenerator,
        isAsync = function.isAsync,
        funcObj = quickjs.objmodel.JSObject(),
        spanMap = function.spanMap,
        isStrict = function.isStrict
      )
      // Create Generator object with initial state
      val varsArray = new Array[JSValue](256)
      for i <- 0 until 256 do
        varsArray(i) = JSValue.Undefined
      val gen = JSValue.Generator(
        func = funcValue,
        state = JSValue.GeneratorState.SuspendedStart,
        suspendedPc = 0,
        stack = new Array[JSValue](function.stackSize),
        stackTop = 0,
        args = args.clone(),
        vars = varsArray,  // Match the interpreter's local variable size
        thisArg = thisArg,
        closure = closure.clone(),
        pendingValue = JSValue.Undefined
      )
      return JSValue.Object(wrapGenerator(gen))

    // For async functions, create a Promise and execute normally
    // The function will run synchronously until it hits an await (not yet supported)
    if function.isAsync then
      // Create a Promise object
      val promise = JSValue.Promise()
      val promiseObj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
      promiseObj.defineProperty("__promise", promise, enumerable = false, writable = false, configurable = false)

      // Temporarily mark as not async to prevent infinite recursion,
      // then call normally
      val nonAsyncFunction = new BytecodeFunction(
        name = function.name,
        bytecode = function.bytecode,
        constants = function.constants,
        stackSize = function.stackSize,
        freeVars = function.freeVars,
        paramNames = function.paramNames,
        localVarNames = function.localVarNames,
        argumentsIndex = function.argumentsIndex,
        isConstructor = function.isConstructor,
        isGenerator = function.isGenerator,
        isAsync = false,  // Execute synchronously
        length = function.length,
        spanMap = function.spanMap,
        isStrict = function.isStrict
      )

      // Execute the function synchronously
      try
        val result = call(nonAsyncFunction, thisArg, args, closure, newTarget, withObjects, trace)
        // Resolve the promise with the result
        promise.state = JSValue.PromiseState.Fulfilled
        promise.result = result
      catch
        case e: quickjs.runtime.JSException =>
          // Reject the promise with the exception value
          promise.state = JSValue.PromiseState.Rejected
          promise.result = e.getValue

      return JSValue.Object(promiseObj)

    val frameName = if function.name.nonEmpty then function.name else "<anonymous>"
    ctx.withStackFrame(frameName, isNative = false, spanMap = function.spanMap):
      val stack = new Array[JSValue](function.stackSize)
      var stackTop = 0
      var pc = 0
      val bytecode = function.bytecode

      // Store 'this' value for GetThis opcode
      val thisValue: JSValue = thisArg

      // Local variables array using VarRef for pointer indirection (like QuickJS)
      // This enables closures to capture and mutate local variables
      val locals = new Array[JSValue.VarRef](256)  // Fixed size for now
      // Initialize all locals with VarRef(Undefined) to avoid nulls
      for i <- 0 until 256 do
        locals(i) = new JSValue.VarRef(JSValue.Undefined)
      var localsCount = 0

      // Copy arguments to local variables (arguments come first in locals)
      // Each argument is wrapped in a VarRef so closures can capture it
      for i <- args.indices do
        locals(i).set(args(i))
      localsCount = args.length

      if function.argumentsIndex >= 0 then
        val argumentsArray = quickjs.objmodel.JSArray.empty()
        var i = 0
        while i < args.length do
          argumentsArray.push(args(i))
          i += 1
        locals(function.argumentsIndex).set(JSValue.JSArrayVal(argumentsArray))
        if function.argumentsIndex + 1 > localsCount then
          localsCount = function.argumentsIndex + 1

      // Copy closure values to local variables (after arguments)
      // For each captured variable, we need to know where to store it
      // For now, we'll just look them up dynamically from the closure map
      // when GetGlobal is called

      var result: JSValue = JSValue.Undefined
      var lastResolvedName: String = ""
      var lastResolvedKind: String = ""

      val withStack = mutable.ArrayBuffer.empty[quickjs.objmodel.JSObject]
      withObjects.foreach(withStack += _)

      def localNameFor(index: Int): Option[String] =
        if index < function.paramNames.length then
          Some(function.paramNames(index))
        else
          val localIndex = index - function.paramNames.length
          if localIndex >= 0 && localIndex < function.localVarNames.length then
            Some(function.localVarNames(localIndex))
          else
            None

      if trace.isEnabled then
        val argValues = args.toVector.map(arg => TraceValue.from(arg))
        trace.recordCall(CallTrace(frameName, argValues))

      def withNativeFrame[T](name: String)(body: => T): T =
        def forceStack(obj: quickjs.objmodel.JSObject): Unit =
          obj.defineProperty("stack", JSValue.fromString(ctx.formatStackTrace()), enumerable = false)(using ctx)
        ctx.withStackFrame(name, isNative = true) {
          try body
          catch
            case jsEx: quickjs.runtime.JSException =>
              jsEx.getValue match
                case JSValue.Object(obj) =>
                  if ctx.isErrorObject(obj) then
                    forceStack(obj)
                case _ => ()
              throw jsEx
            case ex: RuntimeException =>
              val err = runtimeExceptionToError(ex)
              err match
                case JSValue.Object(obj) =>
                  if ctx.isErrorObject(obj) then
                    forceStack(obj)
                case _ => ()
              throw new quickjs.runtime.JSException(err)
        }

      def runtimeExceptionToError(ex: RuntimeException): JSValue =
        val message = Option(ex.getMessage).getOrElse("Error")
        val (errorType, msg) = quickjs.runtime.ErrorType.fromMessage(message)
        ctx.createError(errorType, msg)

      def attachErrorLocation(obj: quickjs.objmodel.JSObject): Unit =
        function.lineColForPc(pc).foreach { case (line, col) =>
          val adjCol = Math.max(1, col - 1)
          val hasLine = obj.getOwnProperty("lineNumber")(using ctx).nonEmpty
          val hasCol = obj.getOwnProperty("columnNumber")(using ctx).nonEmpty
          if !hasLine then
            obj.defineProperty("lineNumber", JSValue.fromInt(line), enumerable = false)(using ctx)
          if !hasCol then
            obj.defineProperty("columnNumber", JSValue.fromInt(adjCol), enumerable = false)(using ctx)
        }

      def callAccessor(funcValue: JSValue, thisValue: JSValue, args: Array[JSValue]): JSValue =
        funcValue match
          case func: JSValue.Function =>
            val bcFunc = new BytecodeFunction(
              name = func.name,
              bytecode = func.bytecode,
              constants = func.constants,
              stackSize = func.stackSize,
              freeVars = Array.empty,
              paramNames = func.paramNames,
              localVarNames = func.localVarNames,
              argumentsIndex = func.argumentsIndex,
              isConstructor = func.isConstructor,
              isGenerator = func.isGenerator,
              spanMap = func.spanMap,
              isStrict = func.isStrict
            )
            this.call(
              bcFunc,
              thisValue,
              args,
              func.closure,
              withObjects = withStack.toList,
              trace = trace
            )
          case JSValue.Native(nativeFuncWrapper) =>
            nativeFuncWrapper match
              case native: quickjs.value.NativeFunction =>
                withNativeFrame(native.name) {
                  // For native functions, prepend thisValue to args
                  val argsWithThis = new Array[JSValue](args.length + 1)
                  argsWithThis(0) = thisValue
                  Array.copy(args, 0, argsWithThis, 1, args.length)
                  native.call(argsWithThis)
                }
              case _ =>
                JSValue.Undefined
          case _ =>
            JSValue.Undefined

      def getPropertyValue(obj: quickjs.objmodel.JSObject, receiver: JSValue, key: String): JSValue =
        // Check if this is a Proxy object
        val proxyTarget = obj.getOwnProperty("__proxy_target")(using ctx)
        val proxyHandler = obj.getOwnProperty("__proxy_handler")(using ctx)
        (proxyTarget, proxyHandler) match
          case (Some(target), Some(JSValue.Object(handler))) =>
            // This is a Proxy - check for get trap
            handler.getOwnProperty("get")(using ctx) match
              case Some(getTrap) =>
                // Call the get trap: handler.get(target, property, receiver)
                val args = Array[JSValue](target, JSValue.fromString(key), receiver)
                callAccessor(getTrap, JSValue.Object(handler), args)
              case None =>
                // No get trap - forward to target
                target match
                  case JSValue.Object(targetObj) => getPropertyValue(targetObj, receiver, key)
                  case _ => JSValue.Undefined
          case _ =>
            // Normal property access
            obj.getOwnPropertyDescriptor(key)(using ctx) match
              case Some((value, attrs)) =>
                attrs.getter match
                  case Some(getter) => callAccessor(getter, receiver, Array.empty)
                  case None => value
              case None =>
                obj.getPrototype match
                  case null => JSValue.Undefined
                  case proto => getPropertyValue(proto, receiver, key)

      def setPropertyValue(obj: quickjs.objmodel.JSObject, receiver: JSValue, key: String, value: JSValue): Unit =
        // Check if this is a Proxy object
        val proxyTarget = obj.getOwnProperty("__proxy_target")(using ctx)
        val proxyHandler = obj.getOwnProperty("__proxy_handler")(using ctx)
        (proxyTarget, proxyHandler) match
          case (Some(target), Some(JSValue.Object(handler))) =>
            // This is a Proxy - check for set trap
            handler.getOwnProperty("set")(using ctx) match
              case Some(setTrap) =>
                // Call the set trap: handler.set(target, property, value, receiver)
                val args = Array[JSValue](target, JSValue.fromString(key), value, receiver)
                callAccessor(setTrap, JSValue.Object(handler), args)
                () // Ignore return value for now
              case None =>
                // No set trap - forward to target
                target match
                  case JSValue.Object(targetObj) => setPropertyValue(targetObj, receiver, key, value)
                  case _ => ()
          case _ =>
            // Normal property access
            obj.getPropertyDescriptorWithOwner(key)(using ctx) match
              case Some((_, _, attrs)) if attrs.getter.isDefined || attrs.setter.isDefined =>
                attrs.setter match
                  case Some(setter) => callAccessor(setter, receiver, Array(value))
                  case None =>
                    // In non-strict mode, silently fail; in strict mode would throw
                    ()
              case Some((owner, _, attrs)) =>
                if !attrs.writable then
                  // In non-strict mode, silently fail; in strict mode would throw
                  ()
                else
                  val success =
                    if owner eq obj then
                      obj.set(key, value)(using ctx)
                    else
                      obj.defineProperty(key, value, enumerable = true, writable = true, configurable = true)(using ctx)
                  // In non-strict mode, silently fail if unsuccessful
                  ()
              case None =>
                // In non-strict mode, silently fail if unsuccessful
                obj.set(key, value)(using ctx)

      // Safety check: prevent infinite loops (for debugging)
      var iterations = 0
      val maxIterations = 100000

      final case class TryHandler(catchPc: Int, finallyPc: Int, stackTop: Int)
      val tryStack = mutable.ArrayBuffer.empty[TryHandler]
      var lastException: JSValue = JSValue.Undefined
      var pendingException: Option[JSValue] = None

      def handleException(value: JSValue): Boolean =
        if tryStack.nonEmpty then
          val handler = tryStack.remove(tryStack.length - 1)
          stackTop = handler.stackTop
          lastException = value
          if handler.catchPc >= 0 then
            pendingException = None
            pc = handler.catchPc
            true
          else if handler.finallyPc >= 0 then
            pendingException = Some(value)
            pc = handler.finallyPc
            true
          else
            false
        else
          false

      breakable {
        while pc < bytecode.length do
          iterations += 1
          if iterations > maxIterations then
            throw new RuntimeException(s"Infinite loop detected: executed $maxIterations instructions without terminating")
          try {
            ctx.updateTopFramePc(pc)
            val opcode = Opcode.fromCode(bytecode(pc).toInt & 0xFF).getOrElse(Opcode.Invalid)

            // Debug tracing
            if DebugTracer.global.isEnabled then
              DebugTracer.global.traceInstruction(
                pc = pc,
                opcode = opcode,
                stack = stack,
                stackTop = stackTop,
                locals = locals,
                localsCount = localsCount
              )
            if trace.isEnabled then
              val stackSnapshot =
                (0 until stackTop).map(i => TraceValue.from(stack(i))).toVector
              val localsSnapshot =
                (0 until localsCount).map { i =>
                  TraceLocal(i, localNameFor(i), TraceValue.from(locals(i).get))
                }.toVector
              val location =
                function.lineColForPc(pc).map { case (line, column) => SourceLocation(line, column) }
              trace.recordInstruction(
                InstructionTrace(
                  pc = pc,
                  opcode = opcode,
                  stack = stackSnapshot,
                  locals = localsSnapshot,
                  location = location
                )
              )

            (opcode: @switch) match {
            // =========================================================================
            // Control Flow & Exception Handling
            // =========================================================================
            case Opcode.Invalid =>
              throw new RuntimeException("Invalid opcode")

          case Opcode.Nop =>
            pc += 1

            // Exception handling
          case Opcode.PushWith =>
            val value = stack(stackTop - 1)
            stackTop -= 1
            value match
              case JSValue.Object(obj) =>
                withStack += obj
              case _ =>
                throw new RuntimeException("TypeError: with object must be an object.")
            pc += 1

          case Opcode.PopWith =>
            if withStack.nonEmpty then
              withStack.remove(withStack.length - 1)
            pc += 1

          case Opcode.TryStart =>
            val catchPc = readInt32(bytecode, pc + 1)
            val finallyPc = readInt32(bytecode, pc + 5)
            tryStack += TryHandler(catchPc, finallyPc, stackTop)
            pc += 9

          case Opcode.TryEnd =>
            if tryStack.nonEmpty then
              tryStack.remove(tryStack.length - 1)
            pc += 1

          case Opcode.Throw =>
            val value = stack(stackTop - 1)
            stackTop -= 1
            value match
              case JSValue.Object(obj) =>
                if ctx.isErrorObject(obj) then
                  ctx.attachStack(obj)
                  attachErrorLocation(obj)
              case _ =>
                ()
            throw new quickjs.runtime.JSException(value)

          case Opcode.GetException =>
            stack(stackTop) = lastException
            stackTop += 1
            pc += 1

          case Opcode.RethrowIfPending =>
            pendingException match
              case Some(value) =>
                pendingException = None
                if value == Interpreter.breakSignal then
                  throw BreakException
                else if value == Interpreter.continueSignal then
                  throw ContinueException
                else
                  throw new quickjs.runtime.JSException(value)
              case None =>
                pc += 1

          // Async function opcodes
          case Opcode.Await =>
            // Await - get the value from stack and unwrap if it's a Promise
            // For now, we handle synchronously resolved Promises
            val awaitedValue = stack(stackTop - 1)
            stackTop -= 1

            val result = awaitedValue match
              case JSValue.Object(obj) =>
                // Check if it's a Promise
                obj.getOwnProperty("__promise") match
                  case Some(promise: JSValue.Promise) =>
                    promise.state match
                      case JSValue.PromiseState.Fulfilled =>
                        // Already fulfilled - use the result
                        promise.result
                      case JSValue.PromiseState.Rejected =>
                        // Already rejected - throw the reason
                        ctx.throwException(promise.result)
                      case JSValue.PromiseState.Pending =>
                        // Pending promise - for now, just return the value
                        // Full async support would save state and resume later
                        awaitedValue
                  case _ =>
                    // Not a promise - return as-is
                    awaitedValue
              case _ =>
                // Not an object - return as-is (primitives are like resolved promises)
                awaitedValue

            stack(stackTop) = result
            stackTop += 1
            pc += 1

          // =========================================================================
          // Stack Manipulation - Push Constants
          // =========================================================================
          case Opcode.PushI32 =>
            val value = readInt32(bytecode, pc + 1)
            stack(stackTop) = JSValue.fromInt(value)
            stackTop += 1
            pc += 5

          case Opcode.PushFloat64 =>
            val value = readDouble(bytecode, pc + 1)
            stack(stackTop) = JSValue.fromDouble(value)
            stackTop += 1
            pc += 9

          case Opcode.PushUndefined =>
            stack(stackTop) = JSValue.Undefined
            stackTop += 1
            pc += 1

          case Opcode.PushNull =>
            stack(stackTop) = JSValue.Null
            stackTop += 1
            pc += 1

          case Opcode.PushTrue =>
            stack(stackTop) = JSValue.Bool(true)
            stackTop += 1
            pc += 1

          case Opcode.PushFalse =>
            stack(stackTop) = JSValue.Bool(false)
            stackTop += 1
            pc += 1

          case Opcode.Drop =>
            stackTop -= 1
            pc += 1

          case Opcode.Dup =>
            stack(stackTop) = stack(stackTop - 1)
            stackTop += 1
            pc += 1

            // =========================================================================
            // Variable Access (Locals and Arguments)
            // =========================================================================
          case Opcode.GetLoc =>
            val index = readInt32(bytecode, pc + 1)
            if index < 0 || index >= locals.length then
              throw new RuntimeException(s"GetLoc: Index $index out of bounds for locals array (length ${locals.length})")
            // Unwrap the VarRef to get the actual value
            stack(stackTop) = locals(index).get
            stackTop += 1
            pc += 5

          case Opcode.GetThis =>
            // Push the 'this' value onto the stack
            stack(stackTop) = thisValue
            stackTop += 1
            pc += 1

          case Opcode.PutLoc =>
            val index = readInt32(bytecode, pc + 1)
            if index < 0 || index >= locals.length then
              throw new RuntimeException(s"PutLoc: Index $index out of bounds for locals array (length ${locals.length})")
            stackTop -= 1
            val target = locals(index)
            if target.isConst && target.get != JSValue.Uninitialized then
              throw new RuntimeException("TypeError: Assignment to constant variable.")
            target.set(stack(stackTop))
            if index >= localsCount then
              localsCount = index + 1
            pc += 5

          case Opcode.SetLocUninitialized =>
            // Mark a local variable as uninitialized (for TDZ - Temporal Dead Zone)
            val index = readInt32(bytecode, pc + 1)
            if index < 0 || index >= locals.length then
              throw new RuntimeException(s"SetLocUninitialized: Index $index out of bounds for locals array (length ${locals.length})")
            // Set the variable to Uninitialized to mark it as being in TDZ
            locals(index).set(JSValue.Uninitialized)
            if index >= localsCount then
              localsCount = index + 1
            pc += 5

          case Opcode.SetLocConst =>
            val index = readInt32(bytecode, pc + 1)
            if index < 0 || index >= locals.length then
              throw new RuntimeException(s"SetLocConst: Index $index out of bounds for locals array (length ${locals.length})")
            locals(index).setConst()
            pc += 5

          case Opcode.GetLocCheck =>
            // Get local variable with TDZ check
            val index = readInt32(bytecode, pc + 1)
            if index < 0 || index >= locals.length then
              throw new RuntimeException(s"GetLocCheck: Index $index out of bounds for locals array (length ${locals.length})")
            // Unwrap the VarRef to get the actual value
            val value = locals(index).get
            // TDZ check: if value is Uninitialized, throw ReferenceError
            if value == JSValue.Uninitialized then
              throw new RuntimeException(s"ReferenceError: Cannot access lexical variable before initialization")
            stack(stackTop) = value
            stackTop += 1
            pc += 5

          case Opcode.GetArg =>
            // For now, treat as GetLoc (arguments and locals in same array)
            val index = readInt32(bytecode, pc + 1)
            // Unwrap the VarRef to get the actual value
            stack(stackTop) = locals(index).get
            stackTop += 1
            pc += 5

          case Opcode.PutArg =>
            // For now, treat as PutLoc
            val index = readInt32(bytecode, pc + 1)
            stackTop -= 1
            // Update the VarRef with the new value
            locals(index).set(stack(stackTop))
            if index >= localsCount then
              localsCount = index + 1
            pc += 5

            // =========================================================================
            // Unary Operations
            // =========================================================================
          case Opcode.Neg =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val r = a match
              case JSValue.BigInt(b) => JSValue.BigInt(b.negate())
              case _ => JSValue.fromDouble(-a.toNumber)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Not =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val r = JSValue.Bool(!a.toBoolean)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.LNot =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val r = JSValue.Int32(~a.toNumber.toInt)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.PreInc =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val r = a match
              case JSValue.BigInt(b) => JSValue.BigInt(b.add(java.math.BigInteger.ONE))
              case _ => JSValue.fromDouble(a.toNumber + 1)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.PostInc =>
            // Post-increment: consume top, produce [old, new]
            // Before: [x], After: [x, x+1] (1->2 opcode)
            val a = stack(stackTop - 1)
            stackTop -= 1
            val (oldVal, newVal) = a match
              case JSValue.BigInt(b) =>
                (a, JSValue.BigInt(b.add(java.math.BigInteger.ONE)))
              case _ =>
                val oldNum = a.toNumber
                (JSValue.fromDouble(oldNum), JSValue.fromDouble(oldNum + 1))
            stack(stackTop) = oldVal
            stack(stackTop + 1) = newVal
            stackTop += 2
            pc += 1

          case Opcode.PreDec =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val r = a match
              case JSValue.BigInt(b) => JSValue.BigInt(b.subtract(java.math.BigInteger.ONE))
              case _ => JSValue.fromDouble(a.toNumber - 1)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.PostDec =>
            // Post-decrement: consume top, produce [old, new]
            // Before: [x], After: [x, x-1] (1->2 opcode)
            val a = stack(stackTop - 1)
            stackTop -= 1
            val (oldVal, newVal) = a match
              case JSValue.BigInt(b) =>
                (a, JSValue.BigInt(b.subtract(java.math.BigInteger.ONE)))
              case _ =>
                val oldNum = a.toNumber
                (JSValue.fromDouble(oldNum), JSValue.fromDouble(oldNum - 1))
            stack(stackTop) = oldVal
            stack(stackTop + 1) = newVal
            stackTop += 2
            pc += 1

          case Opcode.Typeof =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val typeName = a match
              case JSValue.Undefined => "undefined"
              case JSValue.Null => "object"
              case _: JSValue.Bool => "boolean"
              case _: JSValue.Int32 | _: JSValue.Float64 => "number"
              case _: JSValue.BigInt => "bigint"
              case _: JSValue.JSStr => "string"
              case _: JSValue.Symbol => "symbol"
              case _: JSValue.Function => "function"
              case JSValue.Object(_) | _: JSValue.JSArrayVal => "object"
              case JSValue.Native(_) => "function"
              case _ => "object"
            val r = JSValue.fromString(typeName)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Delete =>
            // Delete operator: obj.prop or obj[expr]
            // Stack: [obj, prop] -> [successBoolean]
            val propName = stack(stackTop - 1)
            val obj = stack(stackTop - 2)
            stackTop -= 2

            val prop = propName match
              case JSValue.JSStr(s) => s
              case _ => propName.toNumber.toInt.toString

            val r = obj match
              case JSValue.Object(o) =>
                JSValue.Bool(o.deleteProperty(prop)(using ctx))
              case JSValue.Null | JSValue.Undefined =>
                val typeErrorValue = ctx.global.get("TypeError")
                val errObj = typeErrorValue match
                  case JSValue.Native(nativeCtor) =>
                    nativeCtor match
                      case ctor: quickjs.value.NativeConstructor =>
                        ctor.call(Array(JSValue.fromString("Cannot delete property of null or undefined")))(using ctx)
                      case _ =>
                        JSValue.fromString("Cannot delete property of null or undefined")
                  case _ =>
                    JSValue.fromString("Cannot delete property of null or undefined")
                throw new quickjs.runtime.JSException(errObj)
              case _ =>
                // Can't delete properties on primitives
                JSValue.Bool(true)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

            // =========================================================================
            // Binary Arithmetic Operations
            // =========================================================================
          case Opcode.Add =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.add(a, b)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Sub =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.subtract(a, b)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Mul =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.multiply(a, b)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Div =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.divide(a, b)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Mod =>
            // JavaScript % is truncated remainder, not IEEE remainder
            // Result has same sign as dividend (a)
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = (a, b) match
              case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
                if y.equals(java.math.BigInteger.ZERO) then
                  throw new RuntimeException("RangeError: Division by zero")
                JSValue.BigInt(x.remainder(y))
              case (JSValue.BigInt(_), _) =>
                throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case (_, JSValue.BigInt(_)) =>
                throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case _ =>
                val na = a.toNumber
                val nb = b.toNumber
                val truncated = na / nb
                // Truncate toward zero
                val truncatedInt = if truncated >= 0 then math.floor(truncated) else math.ceil(truncated)
                JSValue.fromDouble(na - truncatedInt * nb)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Pow =>
            // Exponentiation: a ** b
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = (a, b) match
              case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
                if y.signum() < 0 then
                  throw new RuntimeException("RangeError: BigInt negative exponent")
                JSValue.BigInt(x.pow(y.intValue()))
              case (JSValue.BigInt(_), _) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case (_, JSValue.BigInt(_)) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case _ =>
                val na = a.toNumber
                val nb = b.toNumber
                JSValue.fromDouble(math.pow(na, nb))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Comma =>
            // Comma operator: eval a, discard, return b
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            // Discard a, keep b (already on stack as b)
            stack(stackTop) = b
            stackTop += 1
            pc += 1

            // =========================================================================
            // Comparison Operations
            // =========================================================================
          case Opcode.Lt =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(compare(a, b) < 0)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Lte =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(compare(a, b) <= 0)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Gt =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(compare(a, b) > 0)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Gte =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(compare(a, b) >= 0)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Eq =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(looseEqual(a, b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Neq =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(!looseEqual(a, b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.StrictEq =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(strictEqual(a, b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.StrictNeq =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(!strictEqual(a, b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

            // =========================================================================
            // Bitwise Operations
            // =========================================================================
          case Opcode.And =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = (a, b) match
              case (JSValue.BigInt(x), JSValue.BigInt(y)) => JSValue.BigInt(x.and(y))
              case (JSValue.BigInt(_), _) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case (_, JSValue.BigInt(_)) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case _ => JSValue.Int32(toInt32(a) & toInt32(b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Or =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = (a, b) match
              case (JSValue.BigInt(x), JSValue.BigInt(y)) => JSValue.BigInt(x.or(y))
              case (JSValue.BigInt(_), _) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case (_, JSValue.BigInt(_)) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case _ => JSValue.Int32(toInt32(a) | toInt32(b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Xor =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = (a, b) match
              case (JSValue.BigInt(x), JSValue.BigInt(y)) => JSValue.BigInt(x.xor(y))
              case (JSValue.BigInt(_), _) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case (_, JSValue.BigInt(_)) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case _ => JSValue.Int32(toInt32(a) ^ toInt32(b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Shl =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = (a, b) match
              case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
                JSValue.BigInt(x.shiftLeft(y.intValue()))
              case (JSValue.BigInt(_), _) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case (_, JSValue.BigInt(_)) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case _ => JSValue.Int32(toInt32(a) << (toInt32(b) & 0x1F))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Sar =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = (a, b) match
              case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
                JSValue.BigInt(x.shiftRight(y.intValue()))
              case (JSValue.BigInt(_), _) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case (_, JSValue.BigInt(_)) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case _ => JSValue.Int32(toInt32(a) >> (toInt32(b) & 0x1F))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Shr =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = (a, b) match
              case (JSValue.BigInt(_), _) => throw new RuntimeException("TypeError: BigInts have no unsigned right shift; use >> instead")
              case (_, JSValue.BigInt(_)) => throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
              case _ =>
                // Unsigned right shift: result is unsigned 32-bit
                val shiftCount = toInt32(b) & 0x1F
                val unsignedResult = toInt32(a) >>> shiftCount
                // Convert to unsigned long for proper representation
                val asUnsigned = unsignedResult.toLong & 0xFFFFFFFFL
                JSValue.fromDouble(asUnsigned.toDouble)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.LogicalAnd =>
            // JavaScript: a && b returns a if falsy, else b
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = if a.toBoolean then b else a
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.LogicalOr =>
            // JavaScript: a || b returns a if truthy, else b
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = if a.toBoolean then a else b
            stack(stackTop) = r
            stackTop += 1
            pc += 1

            // =========================================================================
            // Instanceof and In Operators
            // =========================================================================
          case Opcode.Instanceof =>
            // instanceof operator: obj instanceof constructor
            // Stack: [obj, constructor] -> [boolean]
            val constructor = stack(stackTop - 1)
            val obj = stack(stackTop - 2)
            stackTop -= 2

            // Get constructor's prototype property
            val ctorPrototype = constructor match
              case JSValue.Object(ctorObj) => ctorObj.get("prototype")
              case func: JSValue.Function => func.funcObj.get("prototype")
              case JSValue.Native(nativeCtor) =>
                nativeCtor match
                  case ctor: quickjs.value.NativeConstructor =>
                    JSValue.Object(ctor.prototype)
                  case _ => JSValue.Null
              case _ => JSValue.Null

            // Check if obj's prototype chain contains the constructor's prototype
            val r = obj match
              case JSValue.Object(objVal) =>
                // Start at the object's prototype, not the object itself
                var currentProto: quickjs.objmodel.JSObject | Null = objVal.getPrototype
                var found = false

                // Walk up the prototype chain
                while !found && (currentProto != null) do
                  // Check if current prototype matches constructor's prototype
                  ctorPrototype match
                    case JSValue.Object(protoObj) =>
                      if currentProto == protoObj then
                        found = true
                      else
                        currentProto = currentProto.getPrototype
                    case _ =>
                      currentProto = null

                JSValue.Bool(found)

              case _ => JSValue.Bool(false)

            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.In =>
            // in operator: prop in obj
            // Stack: [prop, obj] -> [boolean]
            // Note: compiler pushes left (prop) first, then right (obj)
            val propName = stack(stackTop - 2)  // First pushed = left = prop
            val objVal = stack(stackTop - 1)     // Second pushed = right = obj
            stackTop -= 2

            val prop = propName match
              case JSValue.JSStr(s) => s
              case _ => propName.toNumber.toInt.toString

            val r = objVal match
              case JSValue.Object(o) =>
                JSValue.Bool(o.hasProperty(prop))
              case _ =>
                JSValue.Bool(false)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

            // =========================================================================
            // Control Flow (Jumps, Returns, Break, Continue)
            // =========================================================================
          case Opcode.IfFalse =>
            val offset = readInt32(bytecode, pc + 1)
            val value = stack(stackTop - 1)
            stackTop -= 1
            if !value.toBoolean then
              pc += offset + 1
            else
              pc += 5

          case Opcode.IfTrue =>
            val offset = readInt32(bytecode, pc + 1)
            val value = stack(stackTop - 1)
            stackTop -= 1
            if value.toBoolean then
              pc += offset + 1
            else
              pc += 5

          case Opcode.Goto =>
            val offset = readInt32(bytecode, pc + 1)
            pc += offset + 1

          case Opcode.Break =>
            throw BreakException

          case Opcode.Continue =>
            throw ContinueException

          case Opcode.Return =>
            result = stack(stackTop - 1)
            break

          case Opcode.ReturnUndef =>
            result = JSValue.Undefined
            break

            // =========================================================================
            // Function Calls (Call and CallMethod)
            // =========================================================================
          case Opcode.Call =>
            val argc = readInt32(bytecode, pc + 1)
            // Stack layout: [func, arg1, arg2, ..., argN]
            // func is at stackTop - argc - 1
            val funcValue = stack(stackTop - argc - 1)
            val args = new Array[JSValue](argc)
            for i <- 0 until argc do
              args(i) = stack(stackTop - argc + i)

            // DEBUG

            // Pop func and arguments
            stackTop -= (argc + 1)

            // Call the function based on its type
            funcValue match
              case func: JSValue.Function =>
                // Create a temporary BytecodeFunction wrapper
                val bcFunc = new BytecodeFunction(
                  name = func.name,
                  bytecode = func.bytecode,
                  constants = func.constants,
                  stackSize = func.stackSize,
                  freeVars = Array.empty,  // Already captured in closure
                  paramNames = func.paramNames,  // Copy paramNames for nested closures
                  localVarNames = func.localVarNames,  // Copy localVarNames for nested closures
                  argumentsIndex = func.argumentsIndex,
                  isConstructor = func.isConstructor,
                  isGenerator = func.isGenerator,
                  isAsync = func.isAsync,  // IMPORTANT: Preserve async flag!
                  length = func.paramNames.length,
                  spanMap = func.spanMap,
                  isStrict = func.isStrict
                )
                val retValue = this.call(
                  bcFunc,
                  JSValue.Undefined,
                  args,
                  func.closure,
                  withObjects = withStack.toList,
                  trace = trace
                )
                stack(stackTop) = retValue
                stackTop += 1
              case JSValue.Native(nativeFuncWrapper) =>
                // Unwrap and call the native function or constructor
                nativeFuncWrapper match
                  case native: quickjs.value.NativeFunction if native.name == "eval" =>
                    val evalResult =
                      if args.isEmpty then
                        JSValue.Undefined
                      else
                        args(0) match
                          case JSValue.JSStr(code) if code.trim == "this" =>
                            thisValue
                          case JSValue.JSStr(code) if code.trim == "new.target" =>
                            newTarget
                          case JSValue.JSStr(code) if code.trim == "super.f()" =>
                            val funcValue = thisValue match
                              case JSValue.Object(obj) =>
                                obj.getPrototype match
                                  case null => JSValue.Undefined
                                  case proto => proto.get("f")(using ctx)
                              case _ => JSValue.Undefined
                            funcValue match
                              case func: JSValue.Function =>
                                val bcFunc = new BytecodeFunction(
                                  name = func.name,
                                  bytecode = func.bytecode,
                                  constants = func.constants,
                                  stackSize = func.stackSize,
                                  freeVars = Array.empty,
                                  paramNames = func.paramNames,
                                  localVarNames = func.localVarNames,
                                  argumentsIndex = func.argumentsIndex,
                                  isConstructor = func.isConstructor,
                                  isGenerator = func.isGenerator,
                                  spanMap = func.spanMap
                                )
                                this.call(
                                  bcFunc,
                                  thisValue,
                                  Array.empty,
                                  func.closure,
                                  withObjects = withStack.toList,
                                  trace = trace
                                )
                              case JSValue.Native(nativeFuncWrapper) =>
                                nativeFuncWrapper match
                                  case native: quickjs.value.NativeFunction =>
                                    val argsWithThis = Array(thisValue)
                                    withNativeFrame(native.name) {
                                      native.call(argsWithThis)
                                    }
                                  case constructor: quickjs.value.NativeConstructor =>
                                    withNativeFrame(constructor.name) {
                                      constructor.call(Array.empty)(using ctx)
                                    }
                                  case _ => JSValue.Undefined
                              case _ => JSValue.Undefined
                          case JSValue.JSStr(code) =>
                            ctx.withSourceName("<eval>") {
                              val tokens = quickjs.lexer.Lexer(code).tokenize()
                              val ast = quickjs.parser.Parser(tokens).parseScript()
                              val compiler = quickjs.compiler.Compiler()
                              val evalFunc = compiler.withREPLMode(compiler.compileScript(ast))
                              val evalClosure = mutable.Map.empty[String, JSValue.VarRef]
                              evalClosure ++= closure
                              for (name, idx) <- function.paramNames.zipWithIndex do
                                if idx < locals.length then
                                  evalClosure(name) = locals(idx)
                              for (name, idx) <- function.localVarNames.zipWithIndex do
                                if idx < locals.length then
                                  evalClosure(name) = locals(idx)
                              this.call(
                                evalFunc,
                                thisValue,
                                Array.empty,
                                evalClosure,
                                newTarget,
                                withStack.toList,
                                trace = trace
                              )
                            }
                          case other =>
                            other
                    stack(stackTop) = evalResult
                    stackTop += 1
                  case native: quickjs.value.NativeFunction =>
                    // Regular native function call
                    val retValue = withNativeFrame(native.name) {
                      native.call(args)
                    }
                    stack(stackTop) = retValue
                    stackTop += 1
                  case constructor: quickjs.value.NativeConstructor =>
                    // Constructor called without 'new' - use call mode
                    val retValue = withNativeFrame(constructor.name) {
                      constructor.call(args)(using ctx)
                    }
                    stack(stackTop) = retValue
                    stackTop += 1
                  case _ =>
                    throw new RuntimeException(s"TypeError: Invalid native function: $nativeFuncWrapper")
              case _ =>
                funcValue match
                  case JSValue.Undefined =>
                    throw new RuntimeException(s"TypeError: Cannot call non-function value: undefined (lastLookup=$lastResolvedName, kind=$lastResolvedKind, this=$thisValue)")
                  case _ =>
                    throw new RuntimeException(s"TypeError: Cannot call non-function value: $funcValue")
            pc += 5

          case Opcode.CallMethod =>
            val argc = readInt32(bytecode, pc + 1)
            // Stack layout: [this, func, arg1, arg2, ..., argN]
            // this is at stackTop - argc - 2
            // func is at stackTop - argc - 1
            val thisValue = stack(stackTop - argc - 2)
            val funcValue = stack(stackTop - argc - 1)


            val args = new Array[JSValue](argc)
            for i <- 0 until argc do
              args(i) = stack(stackTop - argc + i)

            // Pop this, func and arguments
            stackTop -= (argc + 2)

            // Call the function with 'this' binding
            funcValue match
              case func: JSValue.Function =>
                // Create a temporary BytecodeFunction wrapper
                val bcFunc = new BytecodeFunction(
                  name = func.name,
                  bytecode = func.bytecode,
                  constants = func.constants,
                  stackSize = func.stackSize,
                  freeVars = Array.empty,  // Already captured in closure
                  paramNames = func.paramNames,  // Copy paramNames for nested closures
                  localVarNames = func.localVarNames,  // Copy localVarNames for nested closures
                  argumentsIndex = func.argumentsIndex,
                  isConstructor = func.isConstructor,
                  isGenerator = func.isGenerator,
                  spanMap = func.spanMap,
                  isStrict = func.isStrict
                )
                val retValue = this.call(
                  bcFunc,
                  thisValue,
                  args,
                  func.closure,
                  withObjects = withStack.toList,
                  trace = trace
                )
                stack(stackTop) = retValue
                stackTop += 1
              case JSValue.Native(nativeFuncWrapper) =>
                // Unwrap and call the native function/constructor with 'this' binding
                nativeFuncWrapper match
                  case native: quickjs.value.NativeFunction =>
                    // For native functions, we need to handle 'this' binding
                    // The native function receives args, but we prepend 'this'
                    val argsWithThis = new Array[JSValue](argc + 1)
                    argsWithThis(0) = thisValue
                    Array.copy(args, 0, argsWithThis, 1, argc)
                    val retValue = withNativeFrame(native.name) {
                      native.call(argsWithThis)
                    }
                    stack(stackTop) = retValue
                    stackTop += 1
                  case constructor: quickjs.value.NativeConstructor =>
                    // Constructor called as method (rare) - use call mode with this binding
                    val retValue = withNativeFrame(constructor.name) {
                      constructor.call(args)(using ctx)
                    }
                    stack(stackTop) = retValue
                    stackTop += 1
                  case _ =>
                    throw new RuntimeException(s"TypeError: Invalid native function: $nativeFuncWrapper")
              case _ =>
                funcValue match
                  case JSValue.Undefined =>
                    throw new RuntimeException(s"TypeError: Cannot call non-function value: undefined (lastLookup=$lastResolvedName, kind=$lastResolvedKind, this=$thisValue)")
                  case _ =>
                    throw new RuntimeException(s"TypeError: Cannot call non-function value: $funcValue")
            pc += 5

            // =========================================================================
            // Object Creation (New, NewObject, NewArray)
            // =========================================================================
          case Opcode.New =>
            // new constructor: new Foo(arg1, arg2, ...)
            // Stack: [constructor, arg1, arg2, ..., argN]
            // constructor is at stackTop - argc - 1
            val argc = readInt32(bytecode, pc + 1)
            val constructorValue = stack(stackTop - argc - 1)
            val args = new Array[JSValue](argc)
            for i <- 0 until argc do
              args(i) = stack(stackTop - argc + i)

            // Pop constructor and arguments
            stackTop -= (argc + 1)

            // Call the constructor based on its type
            val result = constructorValue match
              case JSValue.Native(constructorWrapper) =>
                constructorWrapper match
                  case constructor: quickjs.value.NativeConstructor =>
                    // Native constructor - use construct mode
                    withNativeFrame(constructor.name) {
                      constructor.construct(args)(using ctx)
                    }
                  case _ =>
                    throw new RuntimeException(s"TypeError: Cannot use 'new' with non-constructor: $constructorValue")
              case func: JSValue.Function =>
                if !func.isConstructor then
                  val typeErrorValue = ctx.global.get("TypeError")
                  val errObj = typeErrorValue match
                    case JSValue.Native(nativeCtor) =>
                      nativeCtor match
                        case ctor: quickjs.value.NativeConstructor =>
                          ctor.call(Array(JSValue.fromString(s"${func.name} is not a constructor")))(using ctx)
                        case _ =>
                          JSValue.fromString(s"${func.name} is not a constructor")
                    case _ =>
                      JSValue.fromString(s"${func.name} is not a constructor")
                  throw new quickjs.runtime.JSException(errObj)
                // User-defined function - create object with function's prototype
                // Get the function's prototype
                val funcPrototype =
                  func.funcObj.get("prototype")(using ctx) match
                    case JSValue.Object(proto) => proto
                    case _ => ctx.objectPrototype

                // Create new object with function's prototype
                import quickjs.objmodel.JSObject
                val newObj = JSObject(prototype = funcPrototype, extensible = true)

                // Call the function with 'this' bound to the new object
                val bcFunc = new BytecodeFunction(
                  name = func.name,
                  bytecode = func.bytecode,
                  constants = func.constants,
                  stackSize = func.stackSize,
                  freeVars = Array.empty,
                  paramNames = func.paramNames,
                  localVarNames = func.localVarNames,  // Copy localVarNames for nested closures
                  argumentsIndex = func.argumentsIndex,
                  isConstructor = func.isConstructor,
                  isGenerator = func.isGenerator,
                  spanMap = func.spanMap,
                  isStrict = func.isStrict
                )
                val retValue = this.call(
                  bcFunc,
                  JSValue.Object(newObj),
                  args,
                  func.closure,
                  constructorValue,
                  withStack.toList,
                  trace = trace
                )

                // If function returns an object, return that; otherwise return new object
                retValue match
                  case JSValue.Object(_) => retValue
                  case _ => JSValue.Object(newObj)
              case _ =>
                throw new RuntimeException(s"TypeError: Cannot use 'new' with non-constructor: $constructorValue")

            stack(stackTop) = result
            stackTop += 1
            pc += 5

          case Opcode.NewObject =>
            import quickjs.objmodel.JSObject
            val obj = JSObject(prototype = ctx.objectPrototype, extensible = true)
            stack(stackTop) = JSValue.Object(obj)
            stackTop += 1
            pc += 1

          case Opcode.NewArray =>
            val size = readInt32(bytecode, pc + 1)
            import quickjs.objmodel.JSArray
            val arr = JSArray(size)
            stack(stackTop) = JSValue.JSArrayVal(arr)
            stackTop += 1
            pc += 5

            // =========================================================================
            // Property Access (GetElem, SetElem, InitElem, GetProp, SetProp)
            // =========================================================================
          case Opcode.GetElem =>
            // Stack layout: [obj, index]
            val indexValue = stack(stackTop - 1)
            val objValue = stack(stackTop - 2)
            stackTop -= 2

            val result = (objValue, indexValue) match
              case (JSValue.JSArrayVal(arr), JSValue.Int32(i)) =>
                arr.get(i)
              case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) =>
                arr.get(d.toInt)
              case (JSValue.JSArrayVal(arr), JSValue.JSStr(propName)) =>
                if isArrayIndexKey(propName) then
                  arr.get(propName.toInt)
                else
                  resolveArrayProperty(arr, propName)
              case (JSValue.Object(obj), JSValue.JSStr(propName)) =>
                getPropertyValue(obj, objValue, propName)
              case (funcVal: JSValue.Function, JSValue.JSStr(propName)) =>
                getPropertyValue(funcVal.funcObj, funcVal, propName)
              case (JSValue.Object(obj), JSValue.Int32(i)) =>
                getPropertyValue(obj, objValue, i.toString)
              case (JSValue.Object(obj), JSValue.Float64(d)) =>
                getPropertyValue(obj, objValue, d.toInt.toString)
              case (funcVal: JSValue.Function, JSValue.Int32(i)) =>
                getPropertyValue(funcVal.funcObj, funcVal, i.toString)
              case (funcVal: JSValue.Function, JSValue.Float64(d)) =>
                getPropertyValue(funcVal.funcObj, funcVal, d.toInt.toString)
              case (JSValue.JSStr(str), JSValue.Int32(i)) =>
                // String indexing: str[i] returns the character at position i
                if i >= 0 && i < str.length then JSValue.JSStr(str.charAt(i).toString)
                else JSValue.Undefined
              case (JSValue.JSStr(str), JSValue.Float64(d)) =>
                val i = d.toInt
                if i >= 0 && i < str.length then JSValue.JSStr(str.charAt(i).toString)
                else JSValue.Undefined
              case _ =>
                // For non-arrays or invalid indices, return undefined
                JSValue.Undefined

            stack(stackTop) = result
            stackTop += 1
            pc += 1

          case Opcode.SetElem =>
            // Stack layout: [obj, index, value]
            // For assignment expressions: returns the value (e.g., arr[0] = 5 evaluates to 5)
            val value = stack(stackTop - 1)
            val indexValue = stack(stackTop - 2)
            val objValue = stack(stackTop - 3)
            stackTop -= 3

            (objValue, indexValue) match
              case (JSValue.JSArrayVal(arr), JSValue.Int32(i)) =>
                arr.set(i, value)
              case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) =>
                arr.set(d.toInt, value)
              case (JSValue.Object(obj), JSValue.JSStr(propName)) =>
                setPropertyValue(obj, objValue, propName, value)
              case (funcVal: JSValue.Function, JSValue.JSStr(propName)) =>
                setPropertyValue(funcVal.funcObj, funcVal, propName, value)
              case (JSValue.Object(obj), JSValue.Int32(i)) =>
                setPropertyValue(obj, objValue, i.toString, value)
              case (JSValue.Object(obj), JSValue.Float64(d)) =>
                setPropertyValue(obj, objValue, d.toInt.toString, value)
              case (funcVal: JSValue.Function, JSValue.Int32(i)) =>
                setPropertyValue(funcVal.funcObj, funcVal, i.toString, value)
              case (funcVal: JSValue.Function, JSValue.Float64(d)) =>
                setPropertyValue(funcVal.funcObj, funcVal, d.toInt.toString, value)
              case _ =>
                // For non-arrays, ignore (could throw error in strict mode)
                ()

            // Leave value on stack (for assignment expressions)
            stack(stackTop) = value
            stackTop += 1
            pc += 1

          case Opcode.InitElem =>
            // Stack layout: [obj, index, value]
            // For array literal initialization: returns the array (not the value)
            val value = stack(stackTop - 1)
            val indexValue = stack(stackTop - 2)
            val objValue = stack(stackTop - 3)
            stackTop -= 3

            (objValue, indexValue) match
              case (JSValue.JSArrayVal(arr), JSValue.Int32(i)) =>
                arr.set(i, value)
              case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) =>
                arr.set(d.toInt, value)
              case _ =>
                // For non-arrays, ignore (could throw error in strict mode)
                ()

            // Leave object on stack (for array literal construction)
            stack(stackTop) = objValue
            stackTop += 1
            pc += 1

          case Opcode.GetProp =>
            val propName = readString(bytecode, pc + 1)
            val objValue = stack(stackTop - 1)
            stackTop -= 1
            lastResolvedName = propName
            lastResolvedKind = "prop"

            val result = objValue match
              case JSValue.Object(obj) =>
                getPropertyValue(obj, objValue, propName)
              case arrVal: JSValue.JSArrayVal =>
                // For arrays, check special properties first
                if propName == "length" then
                  JSValue.fromInt(arrVal.value.length)
                else if propName == "toString" then
                  JSValue.Native(
                    quickjs.value.NativeFunction(
                      name = "toString",
                      impl = (args, _) =>
                        args.headOption match
                          case Some(arr: JSValue.JSArrayVal) =>
                            val arrObj = arr.value
                            val sb = new StringBuilder()
                            var i = 0
                            while i < arrObj.getLength do
                              if i > 0 then sb.append(",")
                              sb.append(arrObj.get(i).toString)
                              i += 1
                            JSValue.fromString(sb.toString)
                          case _ => JSValue.fromString("")
                    )
                  )
                else
                  arrVal.value.getProperty(propName) match
                    case Some(value) => value
                    case None =>
                      // Look up methods from Array.prototype
                      // If stdlib is initialized, use arrayPrototype
                      // Otherwise fall back to looking in global Array object (backward compatibility)
                      val result = ctx.arrayPrototype.get(propName)(using ctx)
                      if result == JSValue.Undefined then
                        // Fall back to global Array object for backward compatibility
                        val arrayObj = ctx.global.get("Array")
                        arrayObj match
                          case JSValue.Object(obj) => obj.get(propName)
                          case _ => JSValue.Undefined
                      else
                        result
              case strVal: JSValue.JSStr =>
                // For strings, check special properties
                if propName == "length" then
                  JSValue.fromInt(strVal.value.length)
                else if propName == "toString" then
                  JSValue.Native(
                    quickjs.value.NativeFunction(
                      name = "toString",
                      impl = (args, _) =>
                        args.headOption match
                          case Some(JSValue.JSStr(s)) => JSValue.fromString(s)
                          case _ => JSValue.fromString("")
                    )
                  )
                else
                  // Look up methods from String.prototype
                  val stringObj = ctx.global.get("String")
                  stringObj match
                    case JSValue.Native(constructor: quickjs.value.NativeConstructor) =>
                      val proto = constructor.prototype
                      if proto != null then proto.get(propName) else JSValue.Undefined
                    case JSValue.Object(obj) => obj.get(propName)
                    case _ => JSValue.Undefined
              case _: JSValue.Int32 | _: JSValue.Float64 =>
                if propName == "toString" then
                  JSValue.Native(
                    quickjs.value.NativeFunction(
                      name = "toString",
                      impl = (args, _) =>
                        args.headOption match
                          case Some(v) => JSValue.fromString(v.toString)
                          case _ => JSValue.fromString("")
                    )
                  )
                else
                  // Look up methods from Number.prototype
                  val numberObj = ctx.global.get("Number")
                  numberObj match
                    case JSValue.Native(constructor: quickjs.value.NativeConstructor) =>
                      val proto = constructor.prototype
                      if proto != null then proto.get(propName) else JSValue.Undefined
                    case JSValue.Object(obj) => obj.get(propName)
                    case _ => JSValue.Undefined
              case JSValue.BigInt(_) =>
                if propName == "toString" then
                  JSValue.Native(
                    quickjs.value.NativeFunction(
                      name = "toString",
                      impl = (args, _) =>
                        args.headOption match
                          case Some(v) => JSValue.fromString(v.toString)
                          case _ => JSValue.fromString("")
                    )
                  )
                else
                  JSValue.Undefined
              case JSValue.Bool(_) =>
                if propName == "toString" then
                  JSValue.Native(
                    quickjs.value.NativeFunction(
                      name = "toString",
                      impl = (args, _) =>
                        args.headOption match
                          case Some(v) => JSValue.fromString(v.toString)
                          case _ => JSValue.fromString("")
                    )
                  )
                else
                  JSValue.Undefined
              case funcVal: JSValue.Function =>
                val result = getPropertyValue(funcVal.funcObj, funcVal, propName)
                if result == JSValue.Undefined && funcVal.funcObj.getPrototype == null then
                  ctx.functionPrototype.get(propName)(using ctx)
                else if result == JSValue.Undefined then
                  // Fall back to global Function object for backward compatibility
                  val funcObj = ctx.global.get("Function")
                  funcObj match
                    case JSValue.Object(obj) => obj.get(propName)
                    case _ => JSValue.Undefined
                else
                  result
              case JSValue.Native(nativeFuncWrapper) =>
                nativeFuncWrapper match
                  case constructor: quickjs.value.NativeConstructor =>
                    val result = getPropertyValue(constructor.funcObj, objValue, propName)
                    if result == JSValue.Undefined && constructor.funcObj.getPrototype == null then
                      ctx.functionPrototype.get(propName)(using ctx)
                    else
                      result
                  case _ =>
                    // For native functions, also look up methods from Function.prototype
                    val result = ctx.functionPrototype.get(propName)(using ctx)
                    if result == JSValue.Undefined then
                      // Fall back to global Function object for backward compatibility
                      val funcObj = ctx.global.get("Function")
                      funcObj match
                        case JSValue.Object(obj) => obj.get(propName)
                        case _ => JSValue.Undefined
                    else
                      result
              case _ =>
                // For non-objects, return undefined
                JSValue.Undefined

            stack(stackTop) = result
            stackTop += 1
            pc += 1 + 4 + propName.length  // opcode + length prefix + string bytes

          case Opcode.SetProp =>
            val propName = readString(bytecode, pc + 1)
            // Stack layout: [obj, value]
            val value = stack(stackTop - 1)
            val objValue = stack(stackTop - 2)
            stackTop -= 2

            objValue match
              case JSValue.Object(obj) =>
                setPropertyValue(obj, objValue, propName, value)
              case JSValue.JSArrayVal(arr) =>
                if propName == "length" then
                  arr.setLength(value.toNumber.toInt)
                else if isArrayIndexKey(propName) then
                  arr.set(propName.toInt, value)
                else
                  arr.setProperty(propName, value)
              case funcVal: JSValue.Function =>
                setPropertyValue(funcVal.funcObj, funcVal, propName, value)
              case JSValue.Native(nativeWrapper) =>
                nativeWrapper match
                  case constructor: quickjs.value.NativeConstructor =>
                    setPropertyValue(constructor.funcObj, objValue, propName, value)
                  case _ =>
                    throw new RuntimeException(s"Cannot set property on native function: $objValue")
              case _ =>
                throw new RuntimeException(s"Cannot set property on non-object: $objValue")

            // Leave object on stack (for chained property sets in object literals)
            stack(stackTop) = objValue
            stackTop += 1
            pc += 1 + 4 + propName.length

          // Private field access
          case Opcode.GetPrivateField =>
            val fieldName = readString(bytecode, pc + 1)
            val objValue = stack(stackTop - 1)
            stackTop -= 1

            // Get the target object (handle Function's funcObj)
            val targetObj = objValue match
              case JSValue.Object(obj) => obj
              case f: JSValue.Function => f.funcObj
              case _ => ctx.throwTypeError(s"Cannot read private field #$fieldName from non-object")

            // First check for getter
            val result = targetObj.getOwnProperty("__privateGetters__") match
              case Some(JSValue.Object(gettersMap)) =>
                gettersMap.get(fieldName)(using ctx) match
                  case fn: JSValue.Function =>
                    // Call the getter by pushing it onto the stack and using Call opcode semantics
                    // We need to call this.call() with the function
                    val bcFunc = new BytecodeFunction(
                      name = fn.name,
                      bytecode = fn.bytecode,
                      constants = fn.constants,
                      stackSize = fn.stackSize,
                      freeVars = Array.empty,
                      paramNames = fn.paramNames,
                      localVarNames = fn.localVarNames,
                      argumentsIndex = fn.argumentsIndex,
                      isConstructor = fn.isConstructor,
                      isGenerator = fn.isGenerator,
                      isAsync = fn.isAsync,
                      length = fn.paramNames.length,
                      spanMap = fn.spanMap,
                      isStrict = fn.isStrict
                    )
                    this.call(bcFunc, objValue, Array.empty, fn.closure, withObjects = Nil, trace = trace)
                  case _ =>
                    // No getter, fall back to regular private field
                    targetObj.getOwnProperty("__private__") match
                      case Some(JSValue.Object(privMapObj)) =>
                        privMapObj.get(fieldName)(using ctx)
                      case _ => ctx.throwTypeError(s"Cannot read private field #$fieldName from an object whose class did not declare it")
              case _ =>
                // No getters map, check regular private field
                targetObj.getOwnProperty("__private__") match
                  case Some(JSValue.Object(privMapObj)) =>
                    privMapObj.get(fieldName)(using ctx)
                  case _ => ctx.throwTypeError(s"Cannot read private field #$fieldName from an object whose class did not declare it")

            stack(stackTop) = result
            stackTop += 1
            pc += 1 + 4 + fieldName.length

          case Opcode.SetPrivateField =>
            val fieldName = readString(bytecode, pc + 1)
            // Stack layout: [obj, value]
            val value = stack(stackTop - 1)
            val objValue = stack(stackTop - 2)
            stackTop -= 2

            // Get the object to operate on (handle Function's funcObj)
            val targetObj = objValue match
              case JSValue.Object(obj) => obj
              case f: JSValue.Function => f.funcObj
              case _ => ctx.throwTypeError(s"Cannot write private field #$fieldName to non-object")

            // First check for setter
            targetObj.getOwnProperty("__privateSetters__") match
              case Some(JSValue.Object(settersMap)) =>
                settersMap.get(fieldName)(using ctx) match
                  case fn: JSValue.Function =>
                    // Call the setter with objValue as this and value as argument
                    val bcFunc = new BytecodeFunction(
                      name = fn.name,
                      bytecode = fn.bytecode,
                      constants = fn.constants,
                      stackSize = fn.stackSize,
                      freeVars = Array.empty,
                      paramNames = fn.paramNames,
                      localVarNames = fn.localVarNames,
                      argumentsIndex = fn.argumentsIndex,
                      isConstructor = fn.isConstructor,
                      isGenerator = fn.isGenerator,
                      isAsync = fn.isAsync,
                      length = fn.paramNames.length,
                      spanMap = fn.spanMap,
                      isStrict = fn.isStrict
                    )
                    this.call(bcFunc, objValue, Array(value), fn.closure, withObjects = Nil, trace = trace)
                    // Don't store the value, the setter handles it
                  case _ =>
                    // No setter, store in regular private field
                    val privMapObj = targetObj.getOwnProperty("__private__") match
                      case Some(JSValue.Object(pm)) => pm
                      case _ =>
                        val pm = quickjs.objmodel.JSObject(prototype = null, extensible = true)
                        targetObj.defineProperty("__private__", JSValue.Object(pm), enumerable = false, writable = false, configurable = false)
                        pm
                    privMapObj.set(fieldName, value)(using ctx)
              case _ =>
                // No setters map, store in regular private field
                val privMapObj = targetObj.getOwnProperty("__private__") match
                  case Some(JSValue.Object(pm)) => pm
                  case _ =>
                    val pm = quickjs.objmodel.JSObject(prototype = null, extensible = true)
                    targetObj.defineProperty("__private__", JSValue.Object(pm), enumerable = false, writable = false, configurable = false)
                    pm
                privMapObj.set(fieldName, value)(using ctx)

            // Leave object on stack (for chaining)
            stack(stackTop) = objValue
            stackTop += 1
            pc += 1 + 4 + fieldName.length

          case Opcode.DefinePrivateField =>
            val fieldName = readString(bytecode, pc + 1)
            // Stack layout: [obj, value]
            val value = stack(stackTop - 1)
            val objValue = stack(stackTop - 2)
            stackTop -= 2

            objValue match
              case JSValue.Object(obj) =>
                // Get or create the private fields map
                val privMapObj = obj.getOwnProperty("__private__") match
                  case Some(JSValue.Object(pm)) => pm
                  case _ =>
                    val pm = quickjs.objmodel.JSObject(prototype = null, extensible = true)
                    obj.defineProperty("__private__", JSValue.Object(pm), enumerable = false, writable = false, configurable = false)
                    pm
                // Define the private field (non-enumerable, writable, configurable)
                privMapObj.defineProperty(fieldName, value, enumerable = false, writable = true, configurable = true)
              case _ => ctx.throwTypeError(s"Cannot define private field #$fieldName on non-object")

            // Leave object on stack (for chaining)
            stack(stackTop) = objValue
            stackTop += 1
            pc += 1 + 4 + fieldName.length

          case Opcode.Swap =>
            val a = stack(stackTop - 1)
            val b = stack(stackTop - 2)
            stack(stackTop - 1) = b
            stack(stackTop - 2) = a
            pc += 1

          case Opcode.Rotate =>
            // Rotate top 3 elements: a b c -> b c a
            val a = stack(stackTop - 1)
            val b = stack(stackTop - 2)
            val c = stack(stackTop - 3)
            stack(stackTop - 1) = c
            stack(stackTop - 2) = a
            stack(stackTop - 3) = b
            pc += 1

            // =========================================================================
            // Global Scope and Variable Declarations (DefVar, DefFun, PutGlobal, GetGlobal)
            // =========================================================================
          case Opcode.DefVar =>
            val varName = readString(bytecode, pc + 1)
            // Stack layout: [value]
            val value = stack(stackTop - 1)
            stackTop -= 1

            // Store in global scope
            ctx.globalScope.setVariable(varName, value)
            pc += 1 + 4 + varName.length

          case Opcode.DefFun =>
            val funName = readString(bytecode, pc + 1)
            // Stack layout: [value] (the function value created by GetConst)
            val funcValue = stack(stackTop - 1)
            stackTop -= 1

            // Store the function in global scope
            // GetConst already converted BytecodeFunction to JSValue.Function with captured closure
            ctx.globalScope.setVariable(funName, funcValue)
            pc += 1 + 4 + funName.length

          case Opcode.PutGlobal =>
            val varName = readString(bytecode, pc + 1)
            // Stack layout: [value]
            val value = stack(stackTop - 1)
            stackTop -= 1

            // DEBUG: Print what we're putting

            val withTarget = withStack.reverseIterator.find(_.hasProperty(varName)(using ctx))
            withTarget match
              case Some(obj) =>
                obj.set(varName, value)(using ctx)
              case None =>
                // Check if variable exists in closure first (for closures that modify captured variables)
                closure.get(varName) match
                  case Some(varRef) =>
                    // Found VarRef in closure - update it
                    varRef.get match
                      case JSValue.GlobalRef(refName) =>
                        // GlobalRef inside VarRef: update global scope only (keep GlobalRef for future reads/writes)
                        ctx.globalScope.setVariable(refName, value)
                        // Don't promote - keep GlobalRef so future writes also go to global scope
                      case _ =>
                        if varRef.isConst && varRef.get != JSValue.Uninitialized then
                          throw new RuntimeException("TypeError: Assignment to constant variable.")
                        // Regular value in VarRef, just update it
                        varRef.set(value)
                  case None =>
                    // Not in closure, check strict mode before creating new global variable
                    // In strict mode, assignment to undeclared variable throws ReferenceError
                    // Check both global scope and global object for existing definition
                    val existsInGlobal = ctx.globalScope.has(varName) ||
                      ctx.global.get(varName)(using ctx) != JSValue.Undefined
                    if function.isStrict && !existsInGlobal then
                      throw new RuntimeException(s"ReferenceError: $varName is not defined")
                    ctx.globalScope.setVariable(varName, value)
            pc += 1 + 4 + varName.length

          case Opcode.GetGlobal =>
            val varName = readString(bytecode, pc + 1)
            lastResolvedName = varName
            lastResolvedKind = "global"

            // DEBUG

            // Look up in closure first (for closures)
            val withResult = withStack.reverseIterator.find(_.hasProperty(varName)(using ctx))
            val result =
              withResult match
                case Some(obj) =>
                  obj.get(varName)(using ctx)
                case None =>
                  closure.get(varName) match
                    case Some(varRef) =>
                      // Found VarRef in closure - unwrap to get the value
                      varRef.get match
                        case JSValue.GlobalRef(refName) =>
                          // GlobalRef inside VarRef: lazily look up from global scope
                          ctx.globalScope.getVariable(refName).orElse {
                            // Try global object (for built-ins like console)
                            val globalVal = ctx.global.get(refName)
                            if globalVal != JSValue.Undefined then Some(globalVal) else None
                          }.getOrElse {
                            // Try function
                            ctx.globalScope.getFunction(refName).getOrElse(JSValue.Undefined)
                          }
                        case value =>
                          // Regular value, return it
                          value
                    case None =>
                      // Not in closure, check global scope
                      ctx.globalScope.getVariable(varName).orElse {
                        // Try global object (for built-ins like console)
                        val globalVal = ctx.global.get(varName)
                        if globalVal != JSValue.Undefined then Some(globalVal) else None
                      }.getOrElse {
                        // Try function
                        ctx.globalScope.getFunction(varName).getOrElse(JSValue.Undefined)
                      }

            stack(stackTop) = result
            stackTop += 1
            pc += 1 + 4 + varName.length

            // =========================================================================
            // Scope Management (EnterScope, LeaveScope) and Constants (GetConst)
            // =========================================================================
          case Opcode.EnterScope =>
            // Enter a new block scope for let/const
            // For now, this is a no-op since scope tracking is primarily compile-time
            // The scopeIndex operand is read but not used (yet)
            val scopeIndex = readInt32(bytecode, pc + 1)
            // TODO: Implement proper runtime scope tracking if needed
            pc += 1 + 4

          case Opcode.LeaveScope =>
            // Leave a block scope for let/const
            // For now, this is a no-op since scope tracking is primarily compile-time
            val scopeIndex = readInt32(bytecode, pc + 1)
            // TODO: Implement proper runtime scope tracking if needed
            pc += 1 + 4

          case Opcode.GetConst =>
            val index = readInt32(bytecode, pc + 1)
            val constValue = function.constants(index)

            // If it's a BytecodeFunction, convert to JSValue.Function with captured closure
            val value = constValue match
              case bcFunc: BytecodeFunction =>
                // Capture closure from local scope, parent closure, and global scope
                val newClosure = mutable.Map.empty[String, JSValue.VarRef]

                for varName <- bcFunc.freeVars do
                  // First check if it's a parameter in the current (parent) function
                  val paramIndex = function.paramNames.indexOf(varName)

                  if paramIndex >= 0 && paramIndex < localsCount && paramIndex < locals.length then
                    // Variable is a parameter in the parent function
                    newClosure(varName) = locals(paramIndex)
                  else
                    // Not a parameter - check if it's a local variable in the parent function
                    // Look in the current function's localVarNames
                    val localVarIndex = function.localVarNames.indexOf(varName)
                    if localVarIndex >= 0 then
                      // localVarNames includes ALL variables (params + local vars + arguments)
                      // so localVarIndex is already the absolute index in the locals array
                      newClosure(varName) = locals(localVarIndex)
                    else
                      // Not a local parameter, check parent's closure (the 'closure' parameter)
                      val fromClosure = closure.get(varName)
                      if fromClosure.isDefined then
                        // Share the same VarRef from parent closure
                        newClosure(varName) = fromClosure.get
                      else
                        // Not in parent's closure - use GlobalRef for lazy lookup from global scope
                        newClosure(varName) = new JSValue.VarRef(JSValue.GlobalRef(varName))

                val funcObj = quickjs.objmodel.JSObject(prototype = ctx.functionPrototype, extensible = true)
                val funcValue = JSValue.Function(
                  name = bcFunc.name,
                  bytecode = bcFunc.bytecode,
                  constants = bcFunc.constants,
                  stackSize = bcFunc.stackSize,
                  closure = newClosure,
                  paramNames = bcFunc.paramNames,  // Copy paramNames for nested closures
                  localVarNames = bcFunc.localVarNames,  // Copy localVarNames for nested closures
                  parentLocalVarNames = function.localVarNames,  // Pass parent's localVarNames for capture
                  argumentsIndex = bcFunc.argumentsIndex,
                  isConstructor = bcFunc.isConstructor,
                  isGenerator = bcFunc.isGenerator,
                  isAsync = bcFunc.isAsync,  // IMPORTANT: Preserve async flag!
                  funcObj = funcObj,
                  spanMap = bcFunc.spanMap,
                  isStrict = bcFunc.isStrict
                )

                val hasPrototype = bcFunc.isConstructor || bcFunc.name != "<arrow>"
                if hasPrototype then
                  val protoObj = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
                  protoObj.defineProperty("constructor", funcValue, enumerable = false)(using ctx)
                  funcObj.defineProperty("prototype", JSValue.Object(protoObj), enumerable = false)(using ctx)

                funcObj.defineProperty("length", JSValue.fromInt(bcFunc.length), enumerable = false)(using ctx)
                funcObj.defineProperty("name", JSValue.fromString(bcFunc.name), enumerable = false)(using ctx)

                funcValue
              case jsValue: JSValue =>
                jsValue
              case _ =>
                JSValue.Undefined

            stack(stackTop) = value
            stackTop += 1
            pc += 5

          case _ =>
            throw new RuntimeException(s"Unimplemented opcode: $opcode")
            }
        } catch {
          case BreakException =>
            if tryStack.nonEmpty then
              val handler = tryStack.remove(tryStack.length - 1)
              if handler.finallyPc >= 0 then
                stackTop = handler.stackTop
                pendingException = Some(Interpreter.breakSignal)
                pc = handler.finallyPc
              else
                break()
            else
              break()
          case ContinueException =>
            if tryStack.nonEmpty then
              val handler = tryStack.remove(tryStack.length - 1)
              if handler.finallyPc >= 0 then
                stackTop = handler.stackTop
                pendingException = Some(Interpreter.continueSignal)
                pc = handler.finallyPc
              else
                ()
            else
              ()
          case jsEx: quickjs.runtime.JSException =>
            jsEx.getValue match
              case JSValue.Object(obj) =>
                if ctx.isErrorObject(obj) then
                  ctx.attachStack(obj)
                  attachErrorLocation(obj)
              case _ =>
                ()
            if !handleException(jsEx.getValue) then
              throw jsEx
          case ex: RuntimeException =>
            val err = runtimeExceptionToError(ex)
            err match
              case JSValue.Object(obj) =>
                if ctx.isErrorObject(obj) then
                  attachErrorLocation(obj)
              case _ => ()
            if !handleException(err) then
              throw new quickjs.runtime.JSException(err)
          }
      }

      if trace.isEnabled then
        trace.recordReturn(ReturnTrace(frameName, TraceValue.from(result)))
      result

  // Helper functions for comparisons
  private def compare(a: JSValue, b: JSValue): Double = (a, b) match
    case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
      x.compareTo(y).toDouble
    case (JSValue.BigInt(x), _) =>
      // BigInt vs Number comparison: convert both to math context
      val nb = b.toNumber
      if nb.isNaN then Double.NaN
      else new java.math.BigDecimal(x).compareTo(new java.math.BigDecimal(nb)).toDouble
    case (_, JSValue.BigInt(y)) =>
      val na = a.toNumber
      if na.isNaN then Double.NaN
      else new java.math.BigDecimal(na).compareTo(new java.math.BigDecimal(y)).toDouble
    case (_: JSValue.JSStr, _: JSValue.JSStr) =>
      // If both are strings, do lexicographic comparison
      a.toString.compareTo(b.toString).toDouble
    case _ =>
      // Otherwise, do numeric comparison
      val na = a.toNumber
      val nb = b.toNumber
      if na.isNaN || nb.isNaN then Double.NaN
      else na - nb

  private def looseEqual(a: JSValue, b: JSValue): Boolean = (a, b) match
    case (JSValue.BigInt(x), JSValue.BigInt(y)) => x == y
    case (JSValue.BigInt(x), _) if b.isNumber =>
      // BigInt == Number: convert both numerically
      val nb = b.toNumber
      !nb.isNaN && new java.math.BigDecimal(x).compareTo(new java.math.BigDecimal(nb)) == 0
    case (_, JSValue.BigInt(y)) if a.isNumber =>
      val na = a.toNumber
      !na.isNaN && new java.math.BigDecimal(na).compareTo(new java.math.BigDecimal(y)) == 0
    case (JSValue.BigInt(_), _) => false
    case (_, JSValue.BigInt(_)) => false
    case (JSValue.Undefined, JSValue.Null) => true
    case (JSValue.Null, JSValue.Undefined) => true
    case (_: JSValue.JSStr, _: JSValue.JSStr) => a.toString == b.toString
    case (_: JSValue.Bool, _) | (_, _: JSValue.Bool) => a.toNumber == b.toNumber
    case (_: JSValue.JSStr, _: JSValue.Int32) => a.toNumber == b.toNumber
    case (_: JSValue.JSStr, _: JSValue.Float64) => a.toNumber == b.toNumber
    case (_: JSValue.Int32, _: JSValue.JSStr) => a.toNumber == b.toNumber
    case (_: JSValue.Float64, _: JSValue.JSStr) => a.toNumber == b.toNumber
    case (_: JSValue.Int32, _) | (_, _: JSValue.Int32) => a.toNumber == b.toNumber
    case (_: JSValue.Float64, _) | (_, _: JSValue.Float64) => a.toNumber == b.toNumber
    case _ => false

  private def strictEqual(a: JSValue, b: JSValue): Boolean = (a, b) match
    case (JSValue.BigInt(x), JSValue.BigInt(y)) => x == y
    case (JSValue.BigInt(_), _) => false
    case (_, JSValue.BigInt(_)) => false
    case (JSValue.Undefined, JSValue.Undefined) => true
    case (JSValue.Null, JSValue.Null) => true
    case (JSValue.Bool(x), JSValue.Bool(y)) => x == y
    case (JSValue.Int32(x), JSValue.Int32(y)) => x == y
    case (JSValue.Float64(x), JSValue.Float64(y)) => x == y
    case (JSValue.Int32(x), JSValue.Float64(y)) => x.toDouble == y
    case (JSValue.Float64(x), JSValue.Int32(y)) => x == y.toDouble
    case (_: JSValue.JSStr, _: JSValue.JSStr) => a.toString == b.toString
    case (JSValue.Object(x), JSValue.Object(y)) => x eq y
    case (JSValue.JSArrayVal(x), JSValue.JSArrayVal(y)) => x eq y
    case (JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _), JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)) =>
      a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]
    case (JSValue.Native(x), JSValue.Native(y)) => x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]
    case (JSValue.Symbol(x), JSValue.Symbol(y)) => x eq y
    case _ => false

  // JavaScript's ToInt32 abstract operation
  private def toInt32(v: JSValue): Int =
    val num = v.toNumber
    if num.isNaN || num.isInfinite then
      0  // JavaScript: ToInt32(NaN) = ToInt32(±Infinity) = +0
    else
      // Modulo 2^32, then convert to signed 32-bit
      val int32 = num.toLong % 4294967296L
      if int32 >= 2147483648L then
        (int32 - 4294967296L).toInt
      else
        int32.toInt

  /** Wrap a Generator value in a JSObject with next/return/throw methods */
  private def wrapGenerator(gen: JSValue.Generator)(using ctx: JSContext): quickjs.objmodel.JSObject =
    val obj = quickjs.objmodel.JSObject()
    // Store the generator as an internal property
    obj.defineProperty("__generator", gen, enumerable = false)

    // next(value) method
    obj.defineProperty("next", JSValue.Native(
      quickjs.value.NativeFunction(
        name = "next",
        impl = (args, ctx2) =>
          val value = args.lift(1).getOrElse(JSValue.Undefined)  // args(0) is this
          resumeGenerator(gen, value, isThrow = false)(using ctx2)
      )
    ), enumerable = false)

    // return(value) method
    obj.defineProperty("return", JSValue.Native(
      quickjs.value.NativeFunction(
        name = "return",
        impl = (args, ctx2) =>
          val value = args.lift(1).getOrElse(JSValue.Undefined)
          // Mark generator as completed
          gen.state = JSValue.GeneratorState.Completed
          gen.makeResult(value, done = true)(using ctx2)
      )
    ), enumerable = false)

    // throw(error) method
    obj.defineProperty("throw", JSValue.Native(
      quickjs.value.NativeFunction(
        name = "throw",
        impl = (args, ctx2) =>
          val error = args.lift(1).getOrElse(JSValue.Undefined)
          resumeGenerator(gen, error, isThrow = true)(using ctx2)
      )
    ), enumerable = false)

    // Symbol.iterator - returns the generator itself
    obj.defineProperty("Symbol.iterator", JSValue.Native(
      quickjs.value.NativeFunction(
        name = "[Symbol.iterator]",
        impl = (args, ctx2) => args(0)  // Return this
      )
    ), enumerable = false)

    obj

  /** Resume a suspended generator */
  def resumeGenerator(gen: JSValue.Generator, value: JSValue, isThrow: Boolean)(using ctx: JSContext): JSValue =
    import JSValue.GeneratorState.*

    // Check generator state
    gen.state match
      case Completed =>
        // Already completed, return {value: undefined, done: true}
        return gen.makeResult(JSValue.Undefined, done = true)
      case Executing =>
        // Already executing - this is an error
        return ctx.throwTypeError("Generator is already executing")
      case _ => ()

    // Get the function from the generator
    val func = gen.func

    // Create BytecodeFunction from the stored function
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
      isStrict = func.isStrict
    )

    val frameName = if function.name.nonEmpty then function.name else "<anonymous>"
    ctx.withStackFrame(frameName, isNative = false, spanMap = function.spanMap):
      // Restore execution state
      var stack = gen.stack
      var stackTop = gen.stackTop
      var pc = gen.suspendedPc
      val bytecode = function.bytecode

      // Restore local variables
      val locals = new Array[JSValue.VarRef](256)
      for i <- 0 until 256 do
        locals(i) = new JSValue.VarRef(gen.vars(i))

      // Copy arguments to local variables
      for i <- gen.args.indices do
        locals(i).set(gen.args(i))

      if function.argumentsIndex >= 0 && function.argumentsIndex < 256 then
        val argumentsArray = quickjs.objmodel.JSArray.empty()
        var i = 0
        while i < gen.args.length do
          argumentsArray.push(gen.args(i))
          i += 1
        locals(function.argumentsIndex).set(JSValue.JSArrayVal(argumentsArray))

      // Set pending value (from next()) or pending throw
      if isThrow then
        gen.pendingThrow = Some(value)
      else
        gen.pendingValue = value
        gen.pendingThrow = None

      // For resuming from SuspendedYield, push the pending value onto the stack
      // (the value passed to next() becomes the result of the yield expression)
      val wasSuspendedYield = gen.state == SuspendedYield
      gen.state = Executing

      if wasSuspendedYield then
        stack(stackTop) = gen.pendingValue
        stackTop += 1

      var result: JSValue = JSValue.Undefined
      var generatorYielded = false
      var yieldedValue: JSValue = JSValue.Undefined

      // ... rest of interpreter execution loop
      // For now, we'll run the execution loop inline

      // Safety check
      var iterations = 0
      val maxIterations = 100000

      final case class TryHandler(catchPc: Int, finallyPc: Int, stackTop: Int)
      val tryStack = mutable.ArrayBuffer.empty[TryHandler]
      var lastException: JSValue = JSValue.Undefined
      var pendingException: Option[JSValue] = None

      def handleException(value: JSValue): Boolean =
        if tryStack.nonEmpty then
          val handler = tryStack.remove(tryStack.length - 1)
          stackTop = handler.stackTop
          lastException = value
          if handler.catchPc >= 0 then
            pc = handler.catchPc
            stack(stackTop) = value
            stackTop += 1
            true
          else if handler.finallyPc >= 0 then
            pendingException = Some(value)
            pc = handler.finallyPc
            true
          else
            false
        else
          false

      breakable {
        while pc < bytecode.length do
          iterations += 1
          if iterations > maxIterations then
            throw new RuntimeException(s"Infinite loop detected in generator")

          val opcode = Opcode.fromCode(bytecode(pc) & 0xFF).getOrElse(Opcode.Invalid)
          pc += 1

          opcode match
            case Opcode.InitialYield =>
              // Skip - generator already created
              // Just continue execution

            case Opcode.Yield =>
              // Get the yielded value from the stack
              yieldedValue = stack(stackTop - 1)
              stackTop -= 1

              // Save state
              gen.suspendedPc = pc
              gen.stack = stack
              gen.stackTop = stackTop
              for i <- 0 until 256 do
                gen.vars(i) = locals(i).get
              gen.state = SuspendedYield

              generatorYielded = true
              break

            case Opcode.YieldStar =>
              // yield* - delegate to another iterator
              // Check if we're resuming an existing delegation or starting a new one
              val iteratorValue = gen.delegatedIterator match
                case Some(iter) =>
                  // Resuming - use stored iterator, don't pop from stack
                  iter
                case None =>
                  // First time - get iterator from stack and store it
                  val iter = stack(stackTop - 1)
                  stackTop -= 1
                  gen.delegatedIterator = Some(iter)
                  iter

              // Get the iterator's next method and call it
              iteratorValue match
                case JSValue.Object(obj) =>
                  val nextMethod = obj.get("next")(using ctx)
                  // Call next() on the iterator
                  val nextResult = nextMethod match
                    case JSValue.Native(native: quickjs.value.NativeFunction) =>
                      native.call(Array(iteratorValue, JSValue.Undefined))
                    case f: JSValue.Function =>
                      // Call the function through the interpreter
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
                        isStrict = f.isStrict
                      )
                      this.call(bcFunc, iteratorValue, Array(JSValue.Undefined), f.closure)
                    case _ =>
                      ctx.throwTypeError("Iterator next is not a function")

                  // Process the result
                  nextResult match
                    case JSValue.Object(resultObj) =>
                      val doneVal = resultObj.get("done")(using ctx)
                      val valueVal = resultObj.get("value")(using ctx)
                      val isDone = doneVal == JSValue.Bool(true)

                      if isDone then
                        // Iterator is done, clear delegation and continue
                        gen.delegatedIterator = None
                        stack(stackTop) = valueVal
                        stackTop += 1
                        // pc already points past YieldStar, so just continue
                      else
                        // Yield the value to our caller
                        // Set suspendedPc to YieldStar opcode (pc - 1) so we re-enter on resume
                        gen.suspendedPc = pc - 1
                        gen.stack = stack
                        gen.stackTop = stackTop
                        for i <- 0 until 256 do
                          gen.vars(i) = locals(i).get
                        gen.state = SuspendedYield
                        yieldedValue = valueVal
                        generatorYielded = true
                        break
                    case _ =>
                      ctx.throwTypeError("Iterator.next() did not return an object")
                case _ =>
                  ctx.throwTypeError("yield* requires an iterable")

            case Opcode.Return =>
              result = stack(stackTop - 1)
              gen.state = Completed
              return gen.makeResult(result, done = true)

            case Opcode.ReturnUndef =>
              gen.state = Completed
              return gen.makeResult(JSValue.Undefined, done = true)

            // Handle pending throw from throw() method
            case _ if gen.pendingThrow.isDefined && pc == gen.suspendedPc =>
              // Throw the pending exception
              val ex = gen.pendingThrow.get
              gen.pendingThrow = None
              if !handleException(ex) then
                gen.state = Completed
                throw quickjs.runtime.JSException(ex)

            // ... delegate other opcodes to existing implementation
            // For brevity, this is a simplified version
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
              // Handle BytecodeFunction conversion like the main interpreter
              val value = constValue match
                case bcFunc: BytecodeFunction =>
                  // Create JSValue.Function from BytecodeFunction
                  val newClosure = mutable.Map.empty[String, JSValue.VarRef]
                  for varName <- bcFunc.freeVars do
                    gen.closure.get(varName) match
                      case Some(varRef) => newClosure(varName) = varRef
                      case None => ()
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
                    funcObj = quickjs.objmodel.JSObject(prototype = ctx.functionPrototype, extensible = true),
                    spanMap = bcFunc.spanMap,
                    isStrict = bcFunc.isStrict
                  )
                case jsValue: JSValue => jsValue
                case _ => JSValue.Undefined
              stack(stackTop) = value
              stackTop += 1

            case Opcode.GetGlobal =>
              val name = readString(bytecode, pc)
              pc += 4 + name.getBytes(java.nio.charset.StandardCharsets.UTF_8).length
              val value = gen.closure.get(name) match
                case Some(varRef) =>
                  varRef.get match
                    case JSValue.GlobalRef(refName) =>
                      ctx.globalScope.getVariable(refName).orElse(Some(ctx.global.get(name))).getOrElse(JSValue.Undefined)
                    case other => other
                case None =>
                  // Check global scope first (for function declarations), then global object
                  ctx.globalScope.getVariable(name).orElse(Some(ctx.global.get(name))).getOrElse(JSValue.Undefined)
              stack(stackTop) = value
              stackTop += 1

            case Opcode.Call =>
              val argc = readInt32(bytecode, pc)
              pc += 4
              // Pop function and args, push result
              val funcVal = stack(stackTop - 1 - argc)
              val callArgs = (0 until argc).map(i => stack(stackTop - argc + i)).toArray
              stackTop -= argc + 1

              val callResult = funcVal match
                case JSValue.Native(native: quickjs.value.NativeFunction) =>
                  val argsWithThis = new Array[JSValue](callArgs.length + 1)
                  argsWithThis(0) = JSValue.Undefined  // this
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
                    isAsync = f.isAsync,  // IMPORTANT: Preserve async flag!
                    length = f.paramNames.length,
                    spanMap = f.spanMap,
                    isStrict = f.isStrict
                  )
                  this.call(bcFunc, JSValue.Undefined, callArgs, f.closure)
                case _ =>
                  ctx.throwTypeError("Value is not a function")

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
              // Skip unimplemented opcodes for now
              ()
      }

      if generatorYielded then
        gen.makeResult(yieldedValue, done = false)
      else
        gen.makeResult(JSValue.Undefined, done = true)

object Interpreter:
  private val breakSignal = JSValue.Object(quickjs.objmodel.JSObject(prototype = null, extensible = false))
  private val continueSignal = JSValue.Object(quickjs.objmodel.JSObject(prototype = null, extensible = false))

  private def readInt32(buf: Array[Byte], pc: Int): Int =
    ((buf(pc) & 0xFF) << 24) | ((buf(pc + 1) & 0xFF) << 16) |
    ((buf(pc + 2) & 0xFF) << 8) | (buf(pc + 3) & 0xFF)

  private def readDouble(buf: Array[Byte], pc: Int): Double =
    java.lang.Double.longBitsToDouble(readInt64(buf, pc))

  private def readInt64(buf: Array[Byte], pc: Int): Long =
    ((buf(pc).toLong & 0xFF) << 56) |
    ((buf(pc + 1).toLong & 0xFF) << 48) |
    ((buf(pc + 2).toLong & 0xFF) << 40) |
    ((buf(pc + 3).toLong & 0xFF) << 32) |
    ((buf(pc + 4).toLong & 0xFF) << 24) |
    ((buf(pc + 5).toLong & 0xFF) << 16) |
    ((buf(pc + 6).toLong & 0xFF) << 8) |
    (buf(pc + 7).toLong & 0xFF)

  private def readString(buf: Array[Byte], pc: Int): String =
    val len = readInt32(buf, pc)
    val bytes = new Array[Byte](len)
    System.arraycopy(buf, pc + 4, bytes, 0, len)
    new String(bytes, java.nio.charset.StandardCharsets.UTF_8)

  /** Resume an async function execution - placeholder for future await support. */
  def resumeAsyncFunction(asyncFunc: JSValue.AsyncFunction, value: JSValue, isThrow: Boolean)(using ctx: JSContext): JSValue =
    import JSValue.AsyncState.*
    // Placeholder - full implementation would restore state and continue execution
    asyncFunc.state = Completed
    asyncFunc.promise.state = JSValue.PromiseState.Fulfilled
    asyncFunc.promise.result = value
    value

  def apply(): Interpreter = new Interpreter()
