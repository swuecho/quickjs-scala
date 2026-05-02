package quickjs.stdlib

import quickjs.value.JSValue
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject
import quickjs.util.PrettyPrinter

/** JavaScript console object implementation.
  *
  * Provides console.log() and related debugging functions.
  */
object Console {
  import NativeFunctionBuilder.*

  /** Initialize console in the given context */
  def initialize()(using ctx: JSContext): Unit = {
    val consoleObj = JSObject(prototype = null, extensible = true)
    consoleObj.set(
      "log",
      JSValue.Native(loggingFunc("log", PrettyPrinter.shortFormat))
    )
    ctx.global.set("console", JSValue.Object(consoleObj))
  }

  /** Create the console object with log method */
  def create()(using ctx: JSContext): JSValue.Object = {
    val consoleObj = JSObject(prototype = null, extensible = true)
    consoleObj.set(
      "log",
      JSValue.Native(loggingFunc("log", PrettyPrinter.shortFormat))
    )
    JSValue.Object(consoleObj)
  }
}
