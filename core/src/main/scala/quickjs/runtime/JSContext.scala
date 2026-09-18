package quickjs.runtime

import quickjs.value.JSValue
import quickjs.objmodel.JSObject

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

  /** Maximum number of nested interpreter frames before a `RangeError` is
    * thrown. QuickJS C checks a stack limit before every call so that deep
    * recursion raises a JavaScript error instead of overflowing the host
    * stack; the default leaves headroom under the JVM's 1 MB thread stack.
    */
  private var maxJsCallDepth: Int = JSContext.DefaultMaxCallDepth
  private var jsCallDepth: Int = 0

  def maxCallDepth: Int = maxJsCallDepth
  def setMaxCallDepth(depth: Int): Unit = maxJsCallDepth = math.max(1, depth)
  def currentCallDepth: Int = jsCallDepth

  /** Enter an interpreter frame. Throws the JavaScript `RangeError` that
    * programs expect once the configured depth is reached.
    */
  private[quickjs] def enterJsFrame(): Unit = {
    if jsCallDepth >= maxJsCallDepth then
      throwRangeError("Maximum call stack size exceeded")
    jsCallDepth += 1
  }

  private[quickjs] def exitJsFrame(): Unit =
    if jsCallDepth > 0 then jsCallDepth -= 1

  /** Bytecode budget for a single interpreter frame (0 = unlimited). */
  def maxInstructionCount: Long = runtime.maxInstructionCount
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

  /** Guards against re-entrant microtask draining (see [[runMicrotasks]]). */
  private var drainingMicrotasks: Boolean = false

  /** Queue a microtask to be executed. Microtasks run after the current
    * script/function completes, before returning control to the event loop (or
    * in our case, before returning).
    */
  def queueMicrotask(task: () => Unit): Unit =
    microtaskQueue += task

  /** Run all pending microtasks until the queue is empty. New microtasks queued
    * during execution will also be run.
    *
    * Re-entrant calls are ignored: `popStackFrame` drains the queue when the
    * call stack empties, but a microtask that resumes an async frame pops
    * frames too. Without this guard each `await` in a long loop recursed one
    * JVM level deeper through `runMicrotasks` until the host stack overflowed.
    */
  def runMicrotasks(): Unit = {
    if drainingMicrotasks then return
    drainingMicrotasks = true
    try {
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
    } finally drainingMicrotasks = false
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

  /** Well-known `Symbol.unscopables` id, or -1 when symbols are not yet
    * initialized. Used by `with` binding resolution.
    */
  def unscopablesSymbolId: Int =
    global.get("Symbol")(using this) match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.get("unscopables")(using this) match {
          case JSValue.Symbol(id) => id
          case _                  => -1
        }
      case _ => -1
    }

  // Create global object
  private val globalObject: quickjs.objmodel.JSObject =
    quickjs.objmodel.JSObject(prototype = null, extensible = true)

  // Intrinsics (lazily initialized)
  var objectPrototype: quickjs.objmodel.JSObject = uninitialized
  var functionPrototype: quickjs.objmodel.JSObject = uninitialized
  var arrayPrototype: quickjs.objmodel.JSObject = uninitialized
  var symbolPrototype: quickjs.objmodel.JSObject = uninitialized
  // Shared iterator intrinsics: %IteratorPrototype%, %ArrayIteratorPrototype%
  // and %StringIteratorPrototype%.
  var iteratorPrototype: quickjs.objmodel.JSObject = uninitialized
  /** `%AsyncGeneratorFunction.prototype%` (the [[Prototype]] of async
    * generator functions) and `%AsyncGeneratorPrototype%` (its `prototype`).
    */
  var asyncFunctionPrototype: quickjs.objmodel.JSObject = uninitialized
  var asyncGeneratorFunctionPrototype: quickjs.objmodel.JSObject = uninitialized
  var asyncGeneratorPrototype: quickjs.objmodel.JSObject = uninitialized
  /** `%GeneratorFunction.prototype%` (the [[Prototype]] of generator
    * functions) and `%GeneratorPrototype%` (its `prototype`). Both are ordinary
    * objects: generator functions are not callable as functions.
    */
  var generatorFunctionPrototype: quickjs.objmodel.JSObject = uninitialized
  var generatorPrototype: quickjs.objmodel.JSObject = uninitialized
  var arrayIteratorPrototype: quickjs.objmodel.JSObject = uninitialized
  var stringIteratorPrototype: quickjs.objmodel.JSObject = uninitialized
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

  private def stackTraceLimit(): Int = {
    given JSContext = this
    global.get("Error") match {
      case JSValue.Native(err: quickjs.value.NativeConstructor) =>
        err.funcObj.get("stackTraceLimit") match {
          case JSValue.Int32(n) if n >= 0               => n
          case JSValue.Float64(d) if !d.isNaN && d >= 0 => d.toInt
          case _                                        => 10
        }
      case _ => 10
    }
  }

  /** Snapshot the current stack, newest frame first, capped by
    * `Error.stackTraceLimit`. Line/column numbers are resolved here because
    * the live frames' program counters move as execution continues.
    */
  def captureFrames(skipFrames: Int = 0): List[JSContext.CapturedFrame] = {
    val limit = stackTraceLimit()
    val out = mutable.ListBuffer.empty[JSContext.CapturedFrame]
    var idx = callStack.length - 1 - skipFrames
    while idx >= 0 && out.size < limit do {
      val frame = callStack(idx)
      val name = if frame.name.nonEmpty then frame.name else "<anonymous>"
      val (line, col) =
        if frame.isNative then (-1, -1)
        else lineColForPc(frame.spanMap, frame.pc).getOrElse((-1, -1))
      out += JSContext.CapturedFrame(name, frame.source, line, col, frame.isNative)
      idx -= 1
    }
    out.toList
  }

  def formatFrames(frames: List[JSContext.CapturedFrame]): String = {
    val sb = new StringBuilder()
    frames.foreach { frame =>
      sb.append("    at ").append(frame.name)
      if frame.isNative then sb.append(" (native)")
      else if frame.line >= 0 then
        sb.append(" (")
          .append(frame.source)
          .append(":")
          .append(frame.line)
          .append(":")
          .append(Math.max(1, frame.col))
          .append(")")
      else if frame.source.nonEmpty then
        sb.append(" (").append(frame.source).append(")")
      sb.append('\n')
    }
    sb.toString()
  }

  /** Installed by the runtime (`ErrorBuiltins`) so this core class can honor
    * the user-defined `Error.prepareStackTrace` hook without depending on the
    * interpreter.
    */
  private var stackFormatter: (JSObject, List[JSContext.CapturedFrame]) => JSValue =
    null

  def setStackFormatter(
      formatter: (JSObject, List[JSContext.CapturedFrame]) => JSValue
  ): Unit = stackFormatter = formatter

  /** Installed by the runtime to create the `stack` accessor getter (core
    * cannot construct native function values itself).
    */
  private var stackGetterFactory
      : (JSObject, List[JSContext.CapturedFrame]) => JSValue = null

  def setStackGetterFactory(
      factory: (JSObject, List[JSContext.CapturedFrame]) => JSValue
  ): Unit = stackGetterFactory = factory

  /** Value of `.stack` for an object whose frames were captured lazily. */
  def stackValueFor(
      obj: JSObject,
      frames: List[JSContext.CapturedFrame]
  ): JSValue =
    if stackFormatter != null then stackFormatter(obj, frames)
    else JSValue.fromString(formatFrames(frames))

  /** Attach a lazy own `stack` accessor backed by the supplied frames. */
  def installLazyStack(
      obj: JSObject,
      frames: List[JSContext.CapturedFrame]
  ): Unit = {
    given JSContext = this
    val frameValue = JSValue.Native(frames)
    if obj.getOwnPropertyRaw("__stackFrames").isDefined then
      obj.set("__stackFrames", frameValue)
    else
      obj.defineProperty(
        "__stackFrames",
        frameValue,
        enumerable = false,
        writable = true,
        configurable = true
      )(using this)
    if stackGetterFactory != null then
      obj.defineAccessorProperty(
        "stack",
        Some(stackGetterFactory(obj, frames)),
        None,
        enumerable = false,
        configurable = true
      )(using this)
    else
      // Runtime not installed yet (very early boot): fall back to an eager
      // formatted string.
      obj.defineProperty(
        "stack",
        JSValue.fromString(formatFrames(frames)),
        enumerable = false,
        writable = true,
        configurable = true
      )(using this)
  }

  /** Frames captured on an object by [[installLazyStack]] / [[attachStack]]. */
  def capturedFrames(
      obj: JSObject
  ): Option[List[JSContext.CapturedFrame]] =
    obj.getOwnPropertyRaw("__stackFrames") match {
      case Some(JSValue.Native(frames: List[?])) =>
        Some(frames.asInstanceOf[List[JSContext.CapturedFrame]])
      case _ => None
    }

  def attachStack(obj: quickjs.objmodel.JSObject, skipFrames: Int = 0): Unit = {
    given JSContext = this
    val alreadyAttached =
      obj.getOwnPropertyRaw("stack") match {
        case Some(JSValue.JSStr(s)) => s.isEmpty
        case Some(_)                => false
        case None                   => true
      }
    if alreadyAttached then {
      // JSON.parse failures carry a line/column pair; surface it as a
      // synthetic top frame, matching the previous eager formatting.
      val jsonFrame =
        (
          obj.getOwnPropertyRaw("lineNumber"),
          obj.getOwnPropertyRaw("columnNumber")
        ) match {
          case (Some(JSValue.Int32(line)), Some(JSValue.Int32(col))) =>
            Some(JSContext.CapturedFrame("<json>", "<json>", line, col, isNative = false))
          case (Some(JSValue.Float64(line)), Some(JSValue.Float64(col)))
              if !line.isNaN && !col.isNaN =>
            Some(JSContext.CapturedFrame("<json>", "<json>", line.toInt, col.toInt, isNative = false))
          case _ => None
        }
      val frames = jsonFrame match {
        case Some(frame) => frame :: captureFrames(skipFrames)
        case None        => captureFrames(skipFrames)
      }
      installLazyStack(obj, frames)
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
        installLazyStack(obj, captureFrames(skipFrames))
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

  /** Cached `JSValue.Object(globalObject)` wrapper: creating it on every
    * sloppy-mode call was a measurable allocation hotspot.
    */
  val globalObjectValue: JSValue = JSValue.Object(globalObject)

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
    iteratorPrototype =
      quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    asyncGeneratorPrototype =
      quickjs.objmodel.JSObject(prototype = iteratorPrototype, extensible = true)
    asyncFunctionPrototype =
      quickjs.objmodel.JSObject(prototype = functionPrototype, extensible = true)
    asyncGeneratorFunctionPrototype =
      quickjs.objmodel.JSObject(prototype = functionPrototype, extensible = true)
    generatorPrototype =
      quickjs.objmodel.JSObject(prototype = iteratorPrototype, extensible = true)
    generatorFunctionPrototype =
      quickjs.objmodel.JSObject(prototype = functionPrototype, extensible = true)
    {
      given JSContext = this
      asyncGeneratorFunctionPrototype.defineProperty(
        "prototype",
        JSValue.Object(asyncGeneratorPrototype),
        enumerable = false,
        writable = false,
        configurable = true
      )
      generatorFunctionPrototype.defineProperty(
        "prototype",
        JSValue.Object(generatorPrototype),
        enumerable = false,
        writable = false,
        configurable = true
      )
    }

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
                _: JSValue.Function =>
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

  /** Default interpreter call-depth ceiling. Chosen to fit comfortably below
    * the JVM's default 1 MB thread stack while still allowing recursive
    * algorithms that work in other engines; embedders can raise it with
    * [[JSContext.setMaxCallDepth]] when running on larger stacks.
    */
  val DefaultMaxCallDepth: Int = 1000

  final case class CapturedFrame(
      name: String,
      source: String,
      line: Int,
      col: Int,
      isNative: Boolean
  )
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
