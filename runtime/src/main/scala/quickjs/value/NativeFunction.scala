package quickjs.value

import quickjs.runtime.JSContext

/** Native (built-in) function implemented in Scala.
  *
  * Used for standard library functions like console.log, Array.push, etc.
  *
  * Note: This is NOT a JSValue subtype to avoid circular dependencies. It's a
  * separate type that can be stored and pattern-matched.
  */
final case class NativeFunction(
    name: String,
    impl: (Array[JSValue], JSContext) => JSValue, // (args, context) => result
    funcObj: quickjs.objmodel.JSObject = quickjs.objmodel.JSObject(),
    length: Int = 1
) {
  def call(args: Array[JSValue])(using ctx: JSContext): JSValue =
    impl(args, ctx)

  // Store a back-reference so the underlying JSObject can be recognised as a
  // callable value (notably `Function.prototype`, whose JS-visible value is
  // this wrapper but whose property store is the shared function prototype).
  funcObj.initProperty(
    "__nativeFunc",
    JSValue.Native(this),
    enumerable = false,
    writable = false,
    configurable = false
  )

  // Auto-configure name and length properties on funcObj so that
  // hasOwnProperty, getOwnPropertyDescriptor, and deleteProperty work correctly.
  funcObj.initProperty(
    "length",
    JSValue.fromInt(length),
    enumerable = false,
    writable = false,
    configurable = true
  )
  funcObj.initProperty(
    "name",
    JSValue.JSStr(name),
    enumerable = false,
    writable = false,
    configurable = true
  )
}
