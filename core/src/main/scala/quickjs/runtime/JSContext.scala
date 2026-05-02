package quickjs.runtime

import quickjs.value.JSValue

import scala.collection.mutable
import scala.compiletime.uninitialized

/** JavaScript execution context.
  *
  * Per-context resources:
  * - Global object
  * - Current exception
  * - Intrinsics (Object, Array, Function constructors)
  * - Microtask queue for Promise resolution
  */
final class JSContext(private val runtime: JSRuntime):
  private var currentException: JSValue = JSValue.Undefined
  private val callStack = mutable.ArrayBuffer.empty[JSContext.StackFrame]
  private var currentSourceName: String = "<eval>"

  /** Current module path for resolving relative imports */
  var currentModulePath: String = ""

  /** Microtask queue for Promise resolution and async operations.
    * Microtasks are FIFO - first queued, first executed.
    */
  private val microtaskQueue: mutable.ArrayBuffer[() => Unit] = mutable.ArrayBuffer.empty

  /** Queue a microtask to be executed.
    * Microtasks run after the current script/function completes,
    * before returning control to the event loop (or in our case, before returning).
    */
  def queueMicrotask(task: () => Unit): Unit =
    microtaskQueue += task

  /** Run all pending microtasks until the queue is empty.
    * New microtasks queued during execution will also be run.
    */
  def runMicrotasks(): Unit =
    while microtaskQueue.nonEmpty do
      val task = microtaskQueue.remove(0)
      try
        task()
      catch
        case e: JSException =>
          // Store exception but continue processing other microtasks
          currentException = e.getValue
        case e: Exception =>
          // Log other exceptions but continue
          System.err.println(s"Microtask error: ${e.getMessage}")

  /** Check if there are pending microtasks */
  def hasPendingMicrotasks: Boolean = microtaskQueue.nonEmpty

  // Global scope for storing variables and functions
  val globalScope: GlobalScope = GlobalScope()

  // Create global object
  private val globalObject: quickjs.objmodel.JSObject = quickjs.objmodel.JSObject(prototype = null, extensible = true)

  // Intrinsics (lazily initialized)
  var objectPrototype: quickjs.objmodel.JSObject = uninitialized
  var functionPrototype: quickjs.objmodel.JSObject = uninitialized
  var arrayPrototype: quickjs.objmodel.JSObject = uninitialized
  var symbolPrototype: quickjs.objmodel.JSObject = uninitialized
  var mapPrototype: quickjs.objmodel.JSObject = uninitialized
  var setPrototype: quickjs.objmodel.JSObject = uninitialized
  var weakMapPrototype: quickjs.objmodel.JSObject = uninitialized
  var weakSetPrototype: quickjs.objmodel.JSObject = uninitialized
  var promisePrototype: quickjs.objmodel.JSObject = uninitialized

  // Initialize intrinsics
  initializeIntrinsics()

  def rt: JSRuntime = runtime

  // Exception handling
  def throwException(value: JSValue): Nothing =
    currentException = value
    throw new JSException(value)

  def catchException(): JSValue = currentException
  def hasException: Boolean = currentException != JSValue.Undefined
  def clearException(): Unit = currentException = JSValue.Undefined

  def setSourceName(name: String): Unit =
    currentSourceName = name

  def withSourceName[T](name: String)(body: => T): T =
    val prev = currentSourceName
    currentSourceName = name
    try body
    finally currentSourceName = prev

  def pushStackFrame(
    name: String,
    isNative: Boolean,
    spanMap: Array[(Int, Int, Int)] = Array.empty
  ): Unit =
    val sourceName = if isNative then "<native>" else currentSourceName
    callStack += JSContext.StackFrame(name, sourceName, isNative, spanMap, 0)

  def popStackFrame(): Unit =
    if callStack.nonEmpty then
      callStack.remove(callStack.length - 1)
      // When the call stack becomes empty, run all pending microtasks
      // This ensures Promises resolve after the current script completes
      if callStack.isEmpty && microtaskQueue.nonEmpty then
        runMicrotasks()

  def withStackFrame[T](
    name: String,
    isNative: Boolean,
    spanMap: Array[(Int, Int, Int)] = Array.empty
  )(body: => T): T =
    pushStackFrame(name, isNative, spanMap)
    try body
    finally popStackFrame()

  def updateTopFramePc(pc: Int): Unit =
    if callStack.nonEmpty then
      val idx = callStack.length - 1
      callStack(idx) = callStack(idx).copy(pc = pc)

  private def lineColForPc(spanMap: Array[(Int, Int, Int)], pc: Int): Option[(Int, Int)] =
    if spanMap.isEmpty then None
    else
      var idx = spanMap.length - 1
      while idx >= 0 && spanMap(idx)._1 > pc do
        idx -= 1
      if idx >= 0 then Some((spanMap(idx)._2, spanMap(idx)._3)) else None

  def formatStackTrace(skipFrames: Int = 0): String =
    val sb = new StringBuilder()
    var idx = callStack.length - 1 - skipFrames
    while idx >= 0 do
      val frame = callStack(idx)
      val frameName =
        if frame.name.nonEmpty then frame.name else "<anonymous>"
      sb.append("    at ").append(frameName)
      if frame.isNative then
        sb.append(" (native)")
      else
        lineColForPc(frame.spanMap, frame.pc) match
          case Some((line, col)) =>
            val adjCol = Math.max(1, col - 1)
            sb.append(" (").append(frame.source).append(":").append(line).append(":").append(adjCol).append(")")
          case None =>
            if frame.source.nonEmpty then
              sb.append(" (").append(frame.source).append(")")
      sb.append('\n')
      idx -= 1
    sb.toString()

  def attachStack(obj: quickjs.objmodel.JSObject, skipFrames: Int = 0): Unit =
    given JSContext = this
    val shouldAttach = obj.getOwnProperty("stack")(using this) match
      case Some(JSValue.JSStr(s)) => s.isEmpty
      case Some(_) => false
      case None => true
    if shouldAttach then
      val stack = formatStackTrace(skipFrames)
      obj.defineProperty("stack", JSValue.fromString(stack), enumerable = false)(using this)
    val lineColOpt =
      (obj.getOwnProperty("lineNumber")(using this), obj.getOwnProperty("columnNumber")(using this)) match
        case (Some(JSValue.Int32(line)), Some(JSValue.Int32(col))) => Some((line, col))
        case (Some(JSValue.Float64(line)), Some(JSValue.Float64(col))) => Some((line.toInt, col.toInt))
        case (Some(JSValue.Int32(line)), Some(JSValue.Float64(col))) => Some((line, col.toInt))
        case (Some(JSValue.Float64(line)), Some(JSValue.Int32(col))) => Some((line.toInt, col))
        case _ => None
    lineColOpt.foreach { case (line, col) =>
      obj.getOwnProperty("stack")(using this) match
        case Some(JSValue.JSStr(s)) if !s.contains(s":$line:$col") =>
          val prefix = s"    at <json>:$line:$col\n"
          obj.defineProperty("stack", JSValue.fromString(prefix + s), enumerable = false)(using this)
        case _ => ()
    }

  def createError(name: String, message: String, skipFrames: Int): JSValue =
    createError(ErrorType.fromString(name), message, skipFrames)

  def createError(name: String, message: String): JSValue =
    createError(ErrorType.fromString(name), message, 0)

  /** Create an error using the ErrorType enum (type-safe version). */
  def createError(errorType: ErrorType, message: String, skipFrames: Int = 0): JSValue =
    given JSContext = this
    val args =
      if message == null || message.isEmpty then Array.empty[JSValue]
      else Array(JSValue.fromString(message))
    val errorValue =
      global.get(errorType.name) match
        case JSValue.Native(constructor: quickjs.value.NativeConstructor) =>
          constructor.construct(args)
        case _ =>
          val obj = quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
          obj.set("name", JSValue.fromString(errorType.name))
          if args.nonEmpty then obj.set("message", args(0))
          JSValue.Object(obj)
    errorValue match
      case JSValue.Object(obj) =>
        attachStack(obj, skipFrames)
      case _ => ()
    errorValue

  def throwError(name: String, message: String, skipFrames: Int): Nothing =
    throwError(ErrorType.fromString(name), message, skipFrames)

  def throwError(name: String, message: String): Nothing =
    throwError(ErrorType.fromString(name), message, 0)

  /** Throw an error using the ErrorType enum (type-safe version). */
  def throwError(errorType: ErrorType, message: String, skipFrames: Int = 0): Nothing =
    val err = createError(errorType, message, skipFrames)
    throw new JSException(err)

  def throwTypeError(message: String): Nothing =
    throwError(ErrorType.TypeError, message)

  def throwReferenceError(message: String): Nothing =
    throwError(ErrorType.ReferenceError, message)

  def throwSyntaxError(message: String): Nothing =
    throwError(ErrorType.SyntaxError, message)

  def throwRangeError(message: String): Nothing =
    throwError(ErrorType.RangeError, message)

  def isErrorObject(obj: quickjs.objmodel.JSObject): Boolean =
    given JSContext = this
    val errorProto =
      global.get("Error") match
        case JSValue.Native(cons: quickjs.value.NativeConstructor) =>
          cons.prototype
        case _ => null
    if errorProto == null then false
    else (obj eq errorProto) || obj.hasPrototype(errorProto)

  // Global object
  def global: quickjs.objmodel.JSObject = globalObject

  private def initializeIntrinsics(): Unit =
    // Create prototypes
    objectPrototype = quickjs.objmodel.JSObject(prototype = null, extensible = true)
    functionPrototype = quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    arrayPrototype = quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    arrayPrototype.markAsArray()  // Array.prototype is itself an Array exotic object
    mapPrototype = quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    setPrototype = quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    weakMapPrototype = quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    weakSetPrototype = quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    promisePrototype = quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)

    // Set up global object properties
    given JSContext = this
    globalObject.set("undefined", JSValue.Undefined)
    globalObject.set("NaN", JSValue.Float64(Double.NaN))
    globalObject.set("Infinity", JSValue.Float64(Double.PositiveInfinity))
    globalObject.set("globalThis", JSValue.Object(globalObject))

    // Create Object constructor
    // Object() can be called as: Object(value) - converts value to object
    // Or used with new: new Object() - creates new object
    val objectConstructor = quickjs.value.NativeConstructor(
      name = "Object",
      callImpl = (args, ctx) =>
        // Call mode: Object(value) - convert to object
        if args.isEmpty then
          JSValue.Object(quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true))
        else
          args(0) match
            case JSValue.Null | JSValue.Undefined =>
              JSValue.Object(quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true))
            case JSValue.Object(_) | JSValue.JSArrayVal(_) | JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
              args(0)  // Already an object, return as-is
            case JSValue.JSStr(s) =>
              // String wrapper object
              val strProto = globalObject.get("String") match
                case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc.prototype
                case _ => objectPrototype
              val obj = quickjs.objmodel.JSObject(prototype = strProto, extensible = true)
              var i = 0
              while i < s.length do
                obj.defineProperty(i.toString, JSValue.fromString(s.charAt(i).toString), enumerable = true, writable = false)(using this)
                i += 1
              obj.defineProperty("length", JSValue.fromInt(s.length), enumerable = false, writable = false)(using this)
              obj.initProperty("__primitive", JSValue.JSStr(s), enumerable = false, writable = false, configurable = false)(using this)
              JSValue.Object(obj)
            case v @ (_: JSValue.Int32 | _: JSValue.Float64) =>
              val numProto = globalObject.get("Number") match
                case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc.prototype
                case _ => objectPrototype
              val wrapper = quickjs.objmodel.JSObject(prototype = numProto, extensible = true)
              wrapper.initProperty("__primitive", v, enumerable = false, writable = false, configurable = false)(using this)
              JSValue.Object(wrapper)
            case v @ JSValue.Bool(_) =>
              val boolProto = globalObject.get("Boolean") match
                case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc.prototype
                case _ => objectPrototype
              val wrapper = quickjs.objmodel.JSObject(prototype = boolProto, extensible = true)
              wrapper.initProperty("__primitive", v, enumerable = false, writable = false, configurable = false)(using this)
              JSValue.Object(wrapper)
            case v @ JSValue.Symbol(_) =>
              val wrapper = quickjs.objmodel.JSObject(prototype = symbolPrototype, extensible = true)
              wrapper.initProperty("__primitive", v, enumerable = false, writable = false, configurable = false)(using this)
              JSValue.Object(wrapper)
            case v @ JSValue.BigInt(_) =>
              val biProto = globalObject.get("BigInt") match
                case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc.prototype
                case _ => objectPrototype
              val wrapper = quickjs.objmodel.JSObject(prototype = biProto, extensible = true)
              wrapper.initProperty("__primitive", v, enumerable = false, writable = false, configurable = false)(using this)
              JSValue.Object(wrapper)
            case other =>
              // Unknown type — just return as-is
              other
      ,
      constructImpl = (args, ctx) =>
        // Construct mode: new Object() - create new object
        if args.isEmpty then
          JSValue.Object(quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true))
        else
          // new Object(value) - same as Object(value) for most cases
          args(0) match
            case JSValue.Null | JSValue.Undefined =>
              JSValue.Object(quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true))
            case JSValue.Object(obj) =>
              // Create a new object wrapping the provided object
              JSValue.Object(quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true))
            case other =>
              // For primitives, create a wrapper object (simplified)
              JSValue.Object(quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true))
      ,
      prototype = objectPrototype
    )
    objectConstructor.funcObj.setPrototype(functionPrototype)
    objectConstructor.funcObj.defineProperty("prototype", JSValue.Object(objectPrototype), enumerable = false)(using this)
    objectConstructor.funcObj.defineProperty("length", JSValue.fromInt(1), enumerable = false)(using this)
    objectConstructor.funcObj.defineProperty("name", JSValue.fromString("Object"), enumerable = false)(using this)
    objectPrototype.defineProperty("constructor", JSValue.Native(objectConstructor), enumerable = false)(using this)

    // Add the Object constructor to global scope
    globalObject.set("Object", JSValue.Native(objectConstructor))

object JSContext:
  def apply(runtime: JSRuntime): JSContext = new JSContext(runtime)
  final case class StackFrame(
    name: String,
    source: String,
    isNative: Boolean,
    spanMap: Array[(Int, Int, Int)],
    pc: Int = 0
  )

/** JavaScript exception with proper error message formatting. */
final class JSException(value: JSValue) extends Exception(JSException.formatMessage(value)):
  def getValue: JSValue = value

object JSException:
  /** Format the exception message by reading name/message from error objects. */
  private def formatMessage(value: JSValue): String = value match
    case JSValue.Object(obj) =>
      val name = obj.getOwnPropertyRaw("name").map(_.toString).getOrElse("Error")
      val msg = obj.getOwnPropertyRaw("message").map(_.toString).getOrElse("")
      if msg.nonEmpty then s"$name: $msg"
      else name
    case _ => s"JavaScript exception: $value"
