package quickjs.value

import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject

/** Native (built-in) constructor implemented in Scala.
  *
  * Used for standard library constructors like Object, Array, String, etc.
  *
  * JavaScript constructors have two modes:
  *   - Call mode: Object(42) - coerces value to object type
  *   - Construct mode: new Object() - creates new object with prototype
  *
  * @param name
  *   Constructor name (e.g., "Object", "Array")
  * @param callImpl
  *   Implementation for call mode (e.g., Object(42))
  * @param constructImpl
  *   Implementation for construct mode (e.g., new Object())
  * @param prototype
  *   The prototype property (e.g., Object.prototype)
  */
final case class NativeConstructor(
    name: String,
    callImpl: (Array[JSValue], JSContext) => JSValue,
    constructImpl: (Array[JSValue], JSContext) => JSValue,
    prototype: JSObject,
    funcObj: JSObject = JSObject(),
    length: Int = 0
) {
  // Auto-configure funcObj properties so that property descriptors are correctly settable.
  {
    // Store back-reference so we can find the NativeConstructor from funcObj
    funcObj.initProperty(
      "__nativeCtor",
      JSValue.Native(this),
      enumerable = false,
      writable = false,
      configurable = false
    )
    funcObj.initProperty(
      "name",
      JSValue.fromString(name),
      enumerable = false,
      writable = false,
      configurable = true
    )
    funcObj.initProperty(
      "length",
      JSValue.fromInt(length),
      enumerable = false,
      writable = false,
      configurable = true
    )
    funcObj.initProperty(
      "prototype",
      JSValue.Object(prototype),
      enumerable = false,
      writable = false,
      configurable = false
    )
  }

  /** Call mode: Object(42) */
  def call(args: Array[JSValue])(using ctx: JSContext): JSValue =
    callImpl(args, ctx)

  /** Construct mode: new Object() */
  def construct(args: Array[JSValue])(using ctx: JSContext): JSValue =
    constructImpl(args, ctx)
}
