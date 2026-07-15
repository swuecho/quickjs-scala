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
    length: Int = 0,
    constructWithNewTarget: Option[
      (Array[JSValue], JSValue, JSContext) => JSValue
    ] = None,
    hasPrototypeProperty: Boolean = true
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
    if hasPrototypeProperty then
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

  /** Construct with the ECMAScript NewTarget value. Native constructors may
    * provide specialized behavior (notably bound functions); ordinary native
    * constructors get the requested prototype applied to their object result.
    */
  def construct(args: Array[JSValue], newTarget: JSValue)(using
      ctx: JSContext
  ): JSValue =
    constructWithNewTarget match {
      case Some(impl) => impl(args, newTarget, ctx)
      case None =>
        val result = constructImpl(args, ctx)
        val isSameConstructor = newTarget match {
          case JSValue.Native(nc: NativeConstructor) =>
            nc.asInstanceOf[AnyRef] eq this.asInstanceOf[AnyRef]
          case _ => false
        }
        if !isSameConstructor then {
          val requestedPrototype = newTarget match {
            case fn: JSValue.Function => fn.funcObj.get("prototype")(using ctx)
            case JSValue.Native(nc: NativeConstructor) =>
              nc.funcObj.get("prototype")(using ctx)
            case JSValue.Object(obj) => obj.get("prototype")(using ctx)
            case _                   => JSValue.Undefined
          }
          requestedPrototype match {
            case JSValue.Object(proto) =>
              result match {
                case JSValue.Object(obj) => obj.setPrototype(proto)
                case fn: JSValue.Function => fn.funcObj.setPrototype(proto)
                case _ => ()
              }
            case _ => ()
          }
        }
        result
    }
}
