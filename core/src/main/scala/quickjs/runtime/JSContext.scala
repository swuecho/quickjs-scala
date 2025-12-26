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
            case JSValue.Object(_) | JSValue.JSArrayVal(_) | JSValue.Function(_, _, _, _, _, _, _, _) =>
              args(0)  // Already an object, return as-is
            case JSValue.JSStr(s) =>
              // String wrapper object (for now, just return the string)
              args(0)
            case other =>
              // Number/Boolean wrapper (for now, just return the value)
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

    // Add the Object constructor to global scope
    globalObject.set("Object", JSValue.Native(objectConstructor))

object JSContext:
  def apply(runtime: JSRuntime): JSContext = new JSContext(runtime)

/** JavaScript exception */
final class JSException(value: JSValue) extends Exception(s"JavaScript exception: $value"):
  def getValue: JSValue = value
