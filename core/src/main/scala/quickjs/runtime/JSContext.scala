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
  */
final class JSContext(private val runtime: JSRuntime):
  private var currentException: JSValue = JSValue.Undefined

  // Global scope for storing variables and functions
  val globalScope: GlobalScope = GlobalScope()

  // Create global object
  private val globalObject: quickjs.objmodel.JSObject = quickjs.objmodel.JSObject(prototype = null, extensible = true)

  // Intrinsics (lazily initialized)
  var objectPrototype: quickjs.objmodel.JSObject = uninitialized
  var functionPrototype: quickjs.objmodel.JSObject = uninitialized
  var arrayPrototype: quickjs.objmodel.JSObject = uninitialized

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

  // Global object
  def global: quickjs.objmodel.JSObject = globalObject

  private def initializeIntrinsics(): Unit =
    // Create prototypes
    objectPrototype = quickjs.objmodel.JSObject(prototype = null, extensible = true)
    functionPrototype = quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)
    arrayPrototype = quickjs.objmodel.JSObject(prototype = objectPrototype, extensible = true)

    // Set up global object properties
    given JSContext = this
    globalObject.set("undefined", JSValue.Undefined)
    globalObject.set("NaN", JSValue.Float64(Double.NaN))
    globalObject.set("Infinity", JSValue.Float64(Double.PositiveInfinity))

    // Create Object constructor and add to global scope
    val objectConstructor = quickjs.objmodel.JSObject(prototype = functionPrototype, extensible = true)
    objectConstructor.set("prototype", JSValue.Object(objectPrototype))
    globalObject.set("Object", JSValue.Object(objectConstructor))

object JSContext:
  def apply(runtime: JSRuntime): JSContext = new JSContext(runtime)

/** JavaScript exception */
final class JSException(value: JSValue) extends Exception(s"JavaScript exception: $value"):
  def getValue: JSValue = value
