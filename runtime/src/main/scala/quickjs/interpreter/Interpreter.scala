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

/** Bytecode interpreter for JavaScript.
  *
  * Design:
  * - Stack-based virtual machine
  * - Bytecode dispatch loop lives in [[BytecodeLoop]]
  * - Generator support in [[GeneratorSupport]]
  * - Property access helpers are methods on this class
  */
final class Interpreter:
  import Interpreter.*

  private val generatorSupport = GeneratorSupport(this)

  // =========================================================================
  // Small helpers
  // =========================================================================

  private[interpreter] def isArrayIndexKey(key: String): Boolean =
    key.nonEmpty && key.forall(_.isDigit) && (key.length == 1 || key.charAt(0) != '0')

  private[interpreter] def resolveArrayProperty(arr: quickjs.objmodel.JSArray, propName: String)(using ctx: JSContext): JSValue =
    if propName == "length" then
      JSValue.fromInt(arr.length)
    else
      arr.getOwnProperty(propName) match
        case Some(value) => value
        case None =>
          if propName == "toString" then
            arrayToStringNative(JSValue.JSArrayVal(arr))
          else
            val result = ctx.arrayPrototype.get(propName)(using ctx)
            if result == JSValue.Undefined then
              val arrayObj = ctx.global.get("Array")
              arrayObj match
                case JSValue.Object(obj) => obj.get(propName)
                case _ => JSValue.Undefined
            else
              result

  // =========================================================================
  // Property access helpers
  // =========================================================================

  private[interpreter] def callAccessor(
    funcValue: JSValue, thisValue: JSValue, args: Array[JSValue],
    withStack: List[quickjs.objmodel.JSObject], trace: TraceRecorder
  )(using ctx: JSContext): JSValue =
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
        this.call(bcFunc, thisValue, args, func.closure, withObjects = withStack, trace = trace)
      case JSValue.Native(nativeFuncWrapper) =>
        nativeFuncWrapper match
          case native: quickjs.value.NativeFunction =>
            withNativeFrame(native.name) {
              val argsWithThis = new Array[JSValue](args.length + 1)
              argsWithThis(0) = thisValue
              Array.copy(args, 0, argsWithThis, 1, args.length)
              native.call(argsWithThis)
            }
          case _ =>
            JSValue.Undefined
      case _ =>
        JSValue.Undefined

  private[interpreter] def getPropertyValue(
    obj: quickjs.objmodel.JSObject, receiver: JSValue, key: String,
    withStack: List[quickjs.objmodel.JSObject], trace: TraceRecorder
  )(using ctx: JSContext): JSValue =
    val proxyTarget = obj.getOwnProperty("__proxy_target")(using ctx)
    val proxyHandler = obj.getOwnProperty("__proxy_handler")(using ctx)
    (proxyTarget, proxyHandler) match
      case (Some(target), Some(JSValue.Object(handler))) =>
        handler.getOwnProperty("get")(using ctx) match
          case Some(getTrap) =>
            val args = Array[JSValue](target, JSValue.fromString(key), receiver)
            callAccessor(getTrap, JSValue.Object(handler), args, withStack, trace)
          case None =>
            target match
              case JSValue.Object(targetObj) => getPropertyValue(targetObj, receiver, key, withStack, trace)
              case _ => JSValue.Undefined
      case _ =>
        obj.getOwnPropertyDescriptor(key)(using ctx) match
          case Some((value, attrs)) =>
            attrs.getter match
              case Some(getter) => callAccessor(getter, receiver, Array.empty, withStack, trace)
              case None => value
          case None =>
            obj.getPrototype match
              case null => JSValue.Undefined
              case proto => getPropertyValue(proto, receiver, key, withStack, trace)

  private[interpreter] def setPropertyValue(
    obj: quickjs.objmodel.JSObject, receiver: JSValue, key: String, value: JSValue,
    withStack: List[quickjs.objmodel.JSObject], trace: TraceRecorder
  )(using ctx: JSContext): Unit =
    val proxyTarget = obj.getOwnProperty("__proxy_target")(using ctx)
    val proxyHandler = obj.getOwnProperty("__proxy_handler")(using ctx)
    (proxyTarget, proxyHandler) match
      case (Some(target), Some(JSValue.Object(handler))) =>
        handler.getOwnProperty("set")(using ctx) match
          case Some(setTrap) =>
            val args = Array[JSValue](target, JSValue.fromString(key), value, receiver)
            callAccessor(setTrap, JSValue.Object(handler), args, withStack, trace)
            ()
          case None =>
            target match
              case JSValue.Object(targetObj) => setPropertyValue(targetObj, receiver, key, value, withStack, trace)
              case _ => ()
      case _ =>
        obj.getPropertyDescriptorWithOwner(key)(using ctx) match
          case Some((_, _, attrs)) if attrs.getter.isDefined || attrs.setter.isDefined =>
            attrs.setter match
              case Some(setter) => callAccessor(setter, receiver, Array(value), withStack, trace)
              case None => ()
          case Some((owner, _, attrs)) =>
            if !attrs.writable then ()
            else
              if owner eq obj then
                obj.set(key, value)(using ctx)
              else
                obj.defineProperty(key, value, enumerable = true, writable = true, configurable = true)(using ctx)
              ()
          case None =>
            obj.set(key, value)(using ctx)

  // =========================================================================
  // Native frame helpers
  // =========================================================================

  private[interpreter] def withNativeFrame[T](name: String)(body: => T)(using ctx: JSContext): T =
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

  private[interpreter] def runtimeExceptionToError(ex: RuntimeException)(using ctx: JSContext): JSValue =
    val message = Option(ex.getMessage).getOrElse("Error")
    val (errorType, msg) = quickjs.runtime.ErrorType.fromMessage(message)
    ctx.createError(errorType, msg)

  // =========================================================================
  // Built-in toString helpers (used in GetProp opcode)
  // =========================================================================

  private[interpreter] def arrayToStringNative(arrVal: JSValue.JSArrayVal): JSValue =
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

  private[interpreter] def primitiveToStringNative(name: String, value: JSValue): JSValue =
    JSValue.Native(
      quickjs.value.NativeFunction(
        name = name,
        impl = (args, _) =>
          args.headOption match
            case Some(v) => JSValue.fromString(v.toString)
            case _ => JSValue.fromString("")
      )
    )

  // =========================================================================
  // Comparison helpers
  // =========================================================================

  private[interpreter] def compare(a: JSValue, b: JSValue): Double = (a, b) match
    case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
      x.compareTo(y).toDouble
    case (JSValue.BigInt(x), _) =>
      val nb = b.toNumber
      if nb.isNaN then Double.NaN
      else new java.math.BigDecimal(x).compareTo(new java.math.BigDecimal(nb)).toDouble
    case (_, JSValue.BigInt(y)) =>
      val na = a.toNumber
      if na.isNaN then Double.NaN
      else new java.math.BigDecimal(na).compareTo(new java.math.BigDecimal(y)).toDouble
    case (_: JSValue.JSStr, _: JSValue.JSStr) =>
      a.toString.compareTo(b.toString).toDouble
    case _ =>
      val na = a.toNumber
      val nb = b.toNumber
      if na.isNaN || nb.isNaN then Double.NaN
      else na - nb

  private[interpreter] def looseEqual(a: JSValue, b: JSValue): Boolean = (a, b) match
    case (JSValue.BigInt(x), JSValue.BigInt(y)) => x == y
    case (JSValue.BigInt(x), _) if b.isNumber =>
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

  private[interpreter] def strictEqual(a: JSValue, b: JSValue): Boolean = (a, b) match
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

  private[interpreter] def toInt32(v: JSValue): Int =
    val num = v.toNumber
    if num.isNaN || num.isInfinite then
      0
    else
      val int32 = num.toLong % 4294967296L
      if int32 >= 2147483648L then
        (int32 - 4294967296L).toInt
      else
        int32.toInt

  // =========================================================================
  // Main call entry point
  // =========================================================================

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
      val funcValue = JSValue.Function(
        name = function.name,
        bytecode = function.bytecode,
        constants = function.constants,
        stackSize = function.stackSize,
        closure = closure.clone(),
        paramNames = function.paramNames,
        localVarNames = function.localVarNames,
        parentLocalVarNames = Array.empty,
        argumentsIndex = function.argumentsIndex,
        isConstructor = function.isConstructor,
        isGenerator = function.isGenerator,
        isAsync = function.isAsync,
        funcObj = quickjs.objmodel.JSObject(),
        spanMap = function.spanMap,
        isStrict = function.isStrict
      )
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
        vars = varsArray,
        thisArg = thisArg,
        closure = closure.clone(),
        pendingValue = JSValue.Undefined
      )
      return JSValue.Object(generatorSupport.wrapGenerator(gen))

    // For async functions, create a Promise and execute normally
    if function.isAsync then
      val promise = JSValue.Promise()
      val promiseObj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
      promiseObj.defineProperty("__promise", promise, enumerable = false, writable = false, configurable = false)

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
        isAsync = false,
        length = function.length,
        spanMap = function.spanMap,
        isStrict = function.isStrict
      )

      try
        val result = call(nonAsyncFunction, thisArg, args, closure, newTarget, withObjects, trace)
        promise.state = JSValue.PromiseState.Fulfilled
        promise.result = result
      catch
        case e: quickjs.runtime.JSException =>
          promise.state = JSValue.PromiseState.Rejected
          promise.result = e.getValue

      return JSValue.Object(promiseObj)

    // Normal function execution
    val frameName = if function.name.nonEmpty then function.name else "<anonymous>"
    ctx.withStackFrame(frameName, isNative = false, spanMap = function.spanMap):
      // Set up stack
      val stack = new Array[JSValue](function.stackSize)
      var stackTop = 0
      val thisValue: JSValue = thisArg

      // Set up locals
      val locals = new Array[JSValue.VarRef](256)
      for i <- 0 until 256 do
        locals(i) = new JSValue.VarRef(JSValue.Undefined)
      var localsCount = 0

      // Copy arguments to local variables
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

      // Set up with stack
      val withStack = mutable.ArrayBuffer.empty[quickjs.objmodel.JSObject]
      withObjects.foreach(withStack += _)

      // Recording
      if trace.isEnabled then
        val argValues = args.toVector.map(arg => TraceValue.from(arg))
        trace.recordCall(CallTrace(frameName, argValues))

      // Build frame and run bytecode loop
      val loop = new BytecodeLoop(
        interpreter = this,
        frame = new Frame(
          stack = stack,
          stackTop = stackTop,
          pc = 0,
          bytecode = function.bytecode,
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
          iterations = 0,
        ),
        function = function,
        trace = trace,
        newTarget = newTarget,
      )

      val result = loop.run()

      if trace.isEnabled then
        trace.recordReturn(ReturnTrace(frameName, TraceValue.from(result)))
      result

  // =========================================================================
  // Generator delegation
  // =========================================================================

  def wrapGenerator(gen: JSValue.Generator)(using ctx: JSContext): quickjs.objmodel.JSObject =
    generatorSupport.wrapGenerator(gen)

  def resumeGenerator(gen: JSValue.Generator, value: JSValue, isThrow: Boolean)(using ctx: JSContext): JSValue =
    generatorSupport.resumeGenerator(gen, value, isThrow)

object Interpreter:
  private[interpreter] val breakSignal = JSValue.Object(quickjs.objmodel.JSObject(prototype = null, extensible = false))
  private[interpreter] val continueSignal = JSValue.Object(quickjs.objmodel.JSObject(prototype = null, extensible = false))

  private[interpreter] def readInt32(buf: Array[Byte], pc: Int): Int =
    ((buf(pc) & 0xFF) << 24) | ((buf(pc + 1) & 0xFF) << 16) |
    ((buf(pc + 2) & 0xFF) << 8) | (buf(pc + 3) & 0xFF)

  private[interpreter] def readDouble(buf: Array[Byte], pc: Int): Double =
    java.lang.Double.longBitsToDouble(readInt64(buf, pc))

  private[interpreter] def readInt64(buf: Array[Byte], pc: Int): Long =
    ((buf(pc).toLong & 0xFF) << 56) |
    ((buf(pc + 1).toLong & 0xFF) << 48) |
    ((buf(pc + 2).toLong & 0xFF) << 40) |
    ((buf(pc + 3).toLong & 0xFF) << 32) |
    ((buf(pc + 4).toLong & 0xFF) << 24) |
    ((buf(pc + 5).toLong & 0xFF) << 16) |
    ((buf(pc + 6).toLong & 0xFF) << 8) |
    (buf(pc + 7).toLong & 0xFF)

  private[interpreter] def readString(buf: Array[Byte], pc: Int): String =
    val len = readInt32(buf, pc)
    val bytes = new Array[Byte](len)
    System.arraycopy(buf, pc + 4, bytes, 0, len)
    new String(bytes, java.nio.charset.StandardCharsets.UTF_8)

  /** Resume an async function execution - placeholder for future await support. */
  def resumeAsyncFunction(asyncFunc: JSValue.AsyncFunction, value: JSValue, isThrow: Boolean)(using ctx: JSContext): JSValue =
    import JSValue.AsyncState.*
    asyncFunc.state = Completed
    asyncFunc.promise.state = JSValue.PromiseState.Fulfilled
    asyncFunc.promise.result = value
    value

  def apply(): Interpreter = new Interpreter()
