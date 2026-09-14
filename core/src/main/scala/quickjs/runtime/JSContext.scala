package quickjs.runtime

import quickjs.value.JSValue

import scala.collection.mutable
import scala.compiletime.uninitialized

/** JavaScript execution context.
  *
  * Per-context resources:
  *   - Global object
  *   - Current exception
  *   - Intrinsics (Object, Array, Function constructors)
  *   - Microtask queue for Promise resolution
  */
final class JSContext(private val runtime: JSRuntime) {
  private var currentException: JSValue = JSValue.Undefined
  private val callStack = mutable.ArrayBuffer.empty[JSContext.StackFrame]
  private val interpreterRoots =
    mutable.ArrayBuffer.empty[
      (Array[JSValue], () => Int, Array[JSValue.VarRef], Array[String])
    ]
  private var currentSourceName: String = "<eval>"

  def sourceName: String = currentSourceName

  /** Register an interpreter operand stack while its frame is active. */
  def registerInterpreterRoots(
      stack: Array[JSValue],
      top: () => Int,
      locals: Array[JSValue.VarRef],
      localNames: Array[String]
  ): Unit =
    interpreterRoots += ((stack, top, locals, localNames))

  def unregisterInterpreterRoots(stack: Array[JSValue]): Unit = {
    val index = interpreterRoots.lastIndexWhere(_._1 eq stack)
    if index >= 0 then interpreterRoots.remove(index)
  }

  /** Drop stale values above each active stack pointer before a JVM GC. */
  def clearInactiveOperandStackSlots(): Unit =
    interpreterRoots.foreach { case (stack, top, _, _) =>
      val from = math.max(0, math.min(top(), stack.length))
      java.util.Arrays.fill(
        stack.asInstanceOf[Array[Object]],
        from,
        stack.length,
        JSValue.Undefined
      )
    }

  /** Whether an active bytecode frame directly roots a value. */
  def hasActiveInterpreterRoot(value: JSValue): Boolean =
    interpreterRoots.exists { case (stack, top, locals, localNames) =>
      stack
        .take(math.max(0, math.min(top(), stack.length)))
        .exists(_ eq value) ||
      locals.indices.exists { index =>
        !localNames.lift(index).exists(_.startsWith("__objLit_")) &&
        (locals(index).get eq value)
      }
    }

  /** Current `this` value of the executing function (set by interpreter before call).
    * Used by eval() to inherit the calling context's `this` binding.
    */
  var currentThis: JSValue = JSValue.Undefined

  /** Current closure map of the executing function (set by interpreter before call).
    * Used by eval() to access closure variables from the calling scope.
    */
  var currentClosure: mutable.Map[String, JSValue.VarRef] = mutable.Map.empty

  /** Current module path for resolving relative imports */
  var currentModulePath: String = ""

  /** Microtask queue for Promise resolution and async operations. Microtasks
    * are FIFO - first queued, first executed.
    */
  private val microtaskQueue: mutable.ArrayBuffer[() => Unit] =
    mutable.ArrayBuffer.empty

  /** Queue a microtask to be executed. Microtasks run after the current
    * script/function completes, before returning control to the event loop (or
    * in our case, before returning).
    */
  def queueMicrotask(task: () => Unit): Unit =
    microtaskQueue += task

  /** Run all pending microtasks until the queue is empty. New microtasks queued
    * during execution will also be run.
    */
  def runMicrotasks(): Unit =
    while microtaskQueue.nonEmpty do {
      // Embedding hosts (e.g. the test262 runner) cancel a runaway script by
      // interrupting its thread; do not let microtask draining swallow that.
      if Thread.currentThread().isInterrupted then
        throw new InterruptedException("JavaScript execution interrupted")
      val task = microtaskQueue.remove(0)
      try task()
      catch {
        case e: InterruptedException => throw e
        case e: JSException =>
          // Store exception but continue processing other microtasks
          currentException = e.getValue
        case e: Exception =>
          // Log other exceptions but continue
          System.err.println(s"Microtask error: ${e.getMessage}")
      }
    }

  /** Check if there are pending microtasks */
  def hasPendingMicrotasks: Boolean = microtaskQueue.nonEmpty

  // Global scope for storing variables and functions
  val globalScope: GlobalScope = GlobalScope()

  /** Names of global object properties deleted through identifier/global delete.
    * This preserves QuickJS-compatible ReferenceError behavior for later normal
    * reads without changing legacy missing-global reads in minimal contexts.
    */
  val deletedGlobalProperties: mutable.Set[String] = mutable.Set.empty

  /** Cached tagged-template objects keyed by compiler call-site id. */
  val templateObjectCache: mutable.Map[String, JSValue] = mutable.Map.empty

  // Create global object
  private val globalObject: quickjs.objmodel.JSObject =
    quickjs.objmodel.JSObject(prototype = null, extensible = true)

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
  var arrayBufferPrototype: quickjs.objmodel.JSObject = uninitialized
  var typedArrayBaseObject: quickjs.objmodel.JSObject = uninitialized

  /** Registry of typed array prototypes keyed by class name (e.g. "Int8Array"). */
  private val typedArrayPrototypes: scala.collection.mutable.Map[String, quickjs.objmodel.JSObject] =
    scala.collection.mutable.HashMap.empty

  def registerTypedArrayPrototype(name: String, proto: quickjs.objmodel.JSObject): Unit =
    typedArrayPrototypes(name) = proto

  def getTypedArrayPrototype(name: String): quickjs.objmodel.JSObject =
    typedArrayPrototypes.getOrElse(name, objectPrototype)

  // Initialize intrinsics
  initializeIntrinsics()

  def rt: JSRuntime = runtime

  // Exception handling
  def throwException(value: JSValue): Nothing = {
    currentException = value
    throw new JSException(value)
  }

  def catchException(): JSValue = currentException
  def hasException: Boolean = currentException != JSValue.Undefined
  def clearException(): Unit = currentException = JSValue.Undefined

  def setSourceName(name: String): Unit =
    currentSourceName = name

  def withSourceName[T](name: String)(body: => T): T = {
    val prev = currentSourceName
    currentSourceName = name
    try body
    finally currentSourceName = prev
  }

  def pushStackFrame(
      name: String,
      isNative: Boolean,
      spanMap: Array[(Int, Int, Int)] = Array.empty
  ): Unit = {
    val sourceName = if isNative then "<native>" else currentSourceName
    callStack += JSContext.StackFrame(name, sourceName, isNative, spanMap, 0)
  }

  def popStackFrame(): Unit =
    if callStack.nonEmpty then {
      callStack.remove(callStack.length - 1)
      // When the call stack becomes empty, run all pending microtasks
      // This ensures Promises resolve after the current script completes
      if callStack.isEmpty && microtaskQueue.nonEmpty then runMicrotasks()
    }

  def withStackFrame[T](
      name: String,
      isNative: Boolean,
      spanMap: Array[(Int, Int, Int)] = Array.empty
  )(body: => T): T = {
    pushStackFrame(name, isNative, spanMap)
    try body
    finally popStackFrame()
  }

  def updateTopFramePc(pc: Int): Unit =
    if callStack.nonEmpty then callStack(callStack.length - 1).pc = pc

  private def lineColForPc(
      spanMap: Array[(Int, Int, Int)],
      pc: Int
  ): Option[(Int, Int)] =
    if spanMap.isEmpty then None
    else {
      var idx = spanMap.length - 1
      while idx >= 0 && spanMap(idx)._1 > pc do idx -= 1
      if idx >= 0 then Some((spanMap(idx)._2, spanMap(idx)._3)) else None
    }

  def formatStackTrace(skipFrames: Int = 0): String = {
    val sb = new StringBuilder()
    var idx = callStack.length - 1 - skipFrames
    while idx >= 0 do {
      val frame = callStack(idx)
      val frameName =
        if frame.name.nonEmpty then frame.name else "<anonymous>"
      sb.append("    at ").append(frameName)
      if frame.isNative then sb.append(" (native)")
      else
        lineColForPc(frame.spanMap, frame.pc) match {
          case Some((line, col)) =>
            val adjCol = Math.max(1, col)
            sb.append(" (")
              .append(frame.source)
              .append(":")
              .append(line)
              .append(":")
              .append(adjCol)
              .append(")")
          case None =>
            if frame.source.nonEmpty then
              sb.append(" (").append(frame.source).append(")")
        }
      sb.append('\n')
      idx -= 1
    }
    sb.toString()
  }

  def attachStack(obj: quickjs.objmodel.JSObject, skipFrames: Int = 0): Unit = {
    given JSContext = this
    val shouldAttach = obj.getOwnProperty("stack")(using this) match {
      case Some(JSValue.JSStr(s)) => s.isEmpty
      case Some(_)                => false
      case None                   => true
    }
    if shouldAttach then {
      val stack = formatStackTrace(skipFrames)
      obj.defineProperty(
        "stack",
        JSValue.fromString(stack),
        enumerable = false
      )(using this)
    }
    val lineColOpt =
      (
        obj.getOwnProperty("lineNumber")(using this),
        obj.getOwnProperty("columnNumber")(using this)
      ) match {
        case (Some(JSValue.Int32(line)), Some(JSValue.Int32(col))) =>
          Some((line, col))
        case (Some(JSValue.Float64(line)), Some(JSValue.Float64(col))) =>
          Some((line.toInt, col.toInt))
        case (Some(JSValue.Int32(line)), Some(JSValue.Float64(col))) =>
          Some((line, col.toInt))
        case (Some(JSValue.Float64(line)), Some(JSValue.Int32(col))) =>
          Some((line.toInt, col))
        case _ => None
      }
    lineColOpt.foreach { case (line, col) =>
      obj.getOwnProperty("stack")(using this) match {
        case Some(JSValue.JSStr(s)) if !s.contains(s":$line:$col") =>
          val prefix = s"    at <json>:$line:$col\n"
          obj.defineProperty(
            "stack",
            JSValue.fromString(prefix + s),
            enumerable = false
          )(using this)
        case _ => ()
      }
    }
  }

  def createError(name: String, message: String, skipFrames: Int): JSValue =
    createError(ErrorType.fromString(name), message, skipFrames)

  def createError(name: String, message: String): JSValue =
    createError(ErrorType.fromString(name), message, 0)

  /** Create an error using the ErrorType enum (type-safe version). */
  def createError(
      errorType: ErrorType,
      message: String,
      skipFrames: Int = 0
  ): JSValue = {
    given JSContext = this
    val args =
      if message == null || message.isEmpty then Array.empty[JSValue]
      else Array(JSValue.fromString(message))
    val errorValue =
      global.get(errorType.name) match {
        case JSValue.Native(constructor: quickjs.value.NativeConstructor) =>
          constructor.construct(args)
        case _ =>
          val obj = quickjs.objmodel.JSObject(
            prototype = objectPrototype,
            extensible = true
          )
          obj.set("name", JSValue.fromString(errorType.name))
          if args.nonEmpty then obj.set("message", args(0))
          JSValue.Object(obj)
      }
    errorValue match {
      case JSValue.Object(obj) =>
        // Native error constructors skip their own call frame. Internal
        // errors created by the runtime have no such frame, so replace the
        // constructor-produced stack with the actual current JS stack.
        obj.set(
          "stack",
          JSValue.fromString(formatStackTrace(skipFrames))
        )
      case _ => ()
    }
    errorValue
  }

  def throwError(name: String, message: String, skipFrames: Int): Nothing =
    throwError(ErrorType.fromString(name), message, skipFrames)

  def throwError(name: String, message: String): Nothing =
    throwError(ErrorType.fromString(name), message, 0)

  /** Throw an error using the ErrorType enum (type-safe version). */
  def throwError(
      errorType: ErrorType,
      message: String,
      skipFrames: Int = 0
  ): Nothing = {
    val err = createError(errorType, message, skipFrames)
    throw new JSException(err)
  }

  def throwTypeError(message: String): Nothing =
    throwError(ErrorType.TypeError, message)

  def throwReferenceError(message: String): Nothing =
    throwError(ErrorType.ReferenceError, message)

  def throwSyntaxError(message: String): Nothing =
    throwError(ErrorType.SyntaxError, message)

  def throwRangeError(message: String): Nothing =
    throwError(ErrorType.RangeError, message)

  def isErrorObject(obj: quickjs.objmodel.JSObject): Boolean = {
    given JSContext = this
    val errorProto =
      global.get("Error") match {
        case JSValue.Native(cons: quickjs.value.NativeConstructor) =>
          cons.prototype
        case _ => null
      }
    if errorProto == null then false
    else (obj eq errorProto) || obj.hasPrototype(errorProto)
  }

  // Global object
  def global: quickjs.objmodel.JSObject = globalObject

  private def initializeIntrinsics(): Unit = {
    // Create prototypes
    objectPrototype =
      quickjs.objmodel.JSObject(prototype = null, extensible = true)
    globalObject.setPrototype(objectPrototype)
    functionPrototype =
      quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    arrayPrototype =
      quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    arrayPrototype
      .markAsArray() // Array.prototype is itself an Array exotic object
    mapPrototype =
      quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    setPrototype =
      quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    weakMapPrototype =
      quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    weakSetPrototype =
      quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    promisePrototype =
      quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)

    // Set up global object properties
    given JSContext = this
    globalObject.defineProperty(
      "undefined",
      JSValue.Undefined,
      enumerable = false,
      writable = false,
      configurable = false
    )
    globalObject.defineProperty(
      "NaN",
      JSValue.Float64(Double.NaN),
      enumerable = false,
      writable = false,
      configurable = false
    )
    globalObject.defineProperty(
      "Infinity",
      JSValue.Float64(Double.PositiveInfinity),
      enumerable = false,
      writable = false,
      configurable = false
    )
    globalObject.defineProperty(
      "globalThis",
      JSValue.Object(globalObject),
      enumerable = false,
      writable = true,
      configurable = true
    )

    // Create Object constructor
    // Object() can be called as: Object(value) - converts value to object
    // Or used with new: new Object() - creates new object
    val objectConstructor = quickjs.value.NativeConstructor(
      name = "Object",
      callImpl = (args, ctx) =>
        // Call mode: Object(value) - convert to object
        if args.isEmpty then
          JSValue.Object(
            quickjs.objmodel
              .JSObject(prototype = objectPrototype, extensible = true)
          )
        else
          args(0) match {
            case JSValue.Null | JSValue.Undefined =>
              JSValue.Object(
                quickjs.objmodel
                  .JSObject(prototype = objectPrototype, extensible = true)
              )
            case JSValue.Object(_) | JSValue.JSArrayVal(_) |
                JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
              args(0) // Already an object, return as-is
            case JSValue.JSStr(s) =>
              // String wrapper object
              val strProto = globalObject.get("String") match {
                case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                  nc.prototype
                case _ => objectPrototype
              }
              val obj = quickjs.objmodel
                .JSObject(prototype = strProto, extensible = true)
              var i = 0
              while i < s.length do {
                obj.defineProperty(
                  i.toString,
                  JSValue.fromString(s.charAt(i).toString),
                  enumerable = true,
                  writable = false
                )(using this)
                i += 1
              }
              obj.defineProperty(
                "length",
                JSValue.fromInt(s.length),
                enumerable = false,
                writable = false
              )(using this)
              obj.setPrimitiveValue(JSValue.JSStr(s))
              JSValue.Object(obj)
            case v @ (_: JSValue.Int32 | _: JSValue.Float64) =>
              val numProto = globalObject.get("Number") match {
                case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                  nc.prototype
                case _ => objectPrototype
              }
              val wrapper = quickjs.objmodel
                .JSObject(prototype = numProto, extensible = true)
              wrapper.setPrimitiveValue(v)
              JSValue.Object(wrapper)
            case v @ JSValue.Bool(_) =>
              val boolProto = globalObject.get("Boolean") match {
                case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                  nc.prototype
                case _ => objectPrototype
              }
              val wrapper = quickjs.objmodel
                .JSObject(prototype = boolProto, extensible = true)
              wrapper.setPrimitiveValue(v)
              JSValue.Object(wrapper)
            case v @ JSValue.Symbol(_) =>
              val wrapper = quickjs.objmodel
                .JSObject(prototype = symbolPrototype, extensible = true)
              wrapper.setPrimitiveValue(v)
              JSValue.Object(wrapper)
            case v @ JSValue.BigInt(_) =>
              val biProto = globalObject.get("BigInt") match {
                case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                  nc.prototype
                case _ => objectPrototype
              }
              val wrapper = quickjs.objmodel
                .JSObject(prototype = biProto, extensible = true)
              wrapper.setPrimitiveValue(v)
              JSValue.Object(wrapper)
            case other =>
              // Unknown type — just return as-is
              other
          }
      ,
      constructImpl = (args, ctx) =>
        // Construct mode: new Object() - create new object
        if args.isEmpty then
          JSValue.Object(
            quickjs.objmodel
              .JSObject(prototype = objectPrototype, extensible = true)
          )
        else
          // new Object(value) - same as Object(value) for most cases
          args(0) match {
            case JSValue.Null | JSValue.Undefined =>
              JSValue.Object(
                quickjs.objmodel
                  .JSObject(prototype = objectPrototype, extensible = true)
              )
            case JSValue.Object(obj) =>
              // Create a new object wrapping the provided object
              JSValue.Object(
                quickjs.objmodel
                  .JSObject(prototype = objectPrototype, extensible = true)
              )
            case other =>
              // For primitives, create a wrapper object (simplified)
              JSValue.Object(
                quickjs.objmodel
                  .JSObject(prototype = objectPrototype, extensible = true)
              )
          }
      ,
      prototype = objectPrototype
    )
    objectConstructor.funcObj.setPrototype(functionPrototype)
    objectConstructor.funcObj.defineProperty(
      "prototype",
      JSValue.Object(objectPrototype),
      enumerable = false
    )(using this)
    objectConstructor.funcObj.defineProperty(
      "length",
      JSValue.fromInt(1),
      enumerable = false
    )(using this)
    objectConstructor.funcObj.defineProperty(
      "name",
      JSValue.fromString("Object"),
      enumerable = false
    )(using this)
    objectPrototype.defineProperty(
      "constructor",
      JSValue.Native(objectConstructor),
      enumerable = false
    )(using this)

    // Add the Object constructor to global scope
    globalObject.set("Object", JSValue.Native(objectConstructor))
  }
}

object JSContext {
  def apply(runtime: JSRuntime): JSContext = new JSContext(runtime)
  final case class StackFrame(
      name: String,
      source: String,
      isNative: Boolean,
      spanMap: Array[(Int, Int, Int)],
      // Mutable so the interpreter can update the program counter without
      // allocating a new StackFrame on every instruction.
      var pc: Int = 0
  )
}

/** JavaScript exception with proper error message formatting. */
final class JSException(value: JSValue)
    extends Exception(JSException.formatMessage(value)) {
  def getValue: JSValue = value
}

object JSException {

  /** Format the exception message by reading name/message from error objects.
    */
  private def formatMessage(value: JSValue): String = value match {
    case JSValue.Object(obj) =>
      val name =
        obj.getOwnPropertyRaw("name").map(_.toString).getOrElse("Error")
      val msg = obj.getOwnPropertyRaw("message").map(_.toString).getOrElse("")
      if msg.nonEmpty then s"$name: $msg"
      else name
    case _ => s"JavaScript exception: $value"
  }
}
