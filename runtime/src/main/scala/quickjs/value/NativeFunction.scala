package quickjs.value

import quickjs.runtime.JSContext

/** Native (built-in) function implemented in Scala.
  *
  * Used for standard library functions like console.log, Array.push, etc.
  *
  * Note: This is NOT a JSValue subtype to avoid circular dependencies.
  * It's a separate type that can be stored and pattern-matched.
  */
final case class NativeFunction(
  name: String,
  impl: (Array[JSValue], JSContext) => JSValue,  // (args, context) => result
  funcObj: quickjs.objmodel.JSObject = quickjs.objmodel.JSObject(),
  length: Int = 1
):
  def call(args: Array[JSValue])(using ctx: JSContext): JSValue = impl(args, ctx)
