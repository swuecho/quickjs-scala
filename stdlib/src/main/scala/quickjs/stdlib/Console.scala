package quickjs.stdlib

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject
import quickjs.util.PrettyPrinter

/** JavaScript console object implementation.
  *
  * Provides console.log() and related debugging functions. Calls made through
  * `console.method(...)` carry the console object as the receiver, which must
  * not be printed.
  */
object Console {

  private def createConsole()(using ctx: JSContext): JSObject = {
    val consoleObj = JSObject(prototype = null, extensible = true)

    /** Drop the receiver (`this` = the console object) from the arguments. */
    def actualArgs(args: Array[JSValue]): Array[JSValue] =
      if args.nonEmpty then
        args(0) match {
          case JSValue.Object(obj) if obj eq consoleObj => args.drop(1)
          case _                                       => args
        }
      else args

    def write(text: String, toStderr: Boolean): Unit =
      if toStderr then System.err.println(text) else System.out.println(text)

    def printMethod(name: String, toStderr: Boolean): Unit = {
      val fn = NativeFunction(
        name = name,
        length = 0,
        impl = (args, _) => {
          val values = actualArgs(args)
          write(values.map(PrettyPrinter.consoleFormat).mkString(" "), toStderr)
          JSValue.Undefined
        }
      )
      consoleObj.set(name, JSValue.Native(fn))
    }

    printMethod("log", toStderr = false)
    printMethod("info", toStderr = false)
    printMethod("debug", toStderr = false)
    printMethod("dir", toStderr = false)
    printMethod("error", toStderr = true)
    printMethod("warn", toStderr = true)

    val assertFn = NativeFunction(
      name = "assert",
      length = 0,
      impl = (args, _) => {
        val values = actualArgs(args)
        if values.isEmpty || !values(0).toBoolean then {
          val message =
            if values.length > 1 then
              values.drop(1).map(PrettyPrinter.consoleFormat).mkString(" ")
            else "console.assert"
          write("Assertion failed: " + message, toStderr = true)
        }
        JSValue.Undefined
      }
    )
    consoleObj.set("assert", JSValue.Native(assertFn))

    val traceFn = NativeFunction(
      name = "trace",
      length = 0,
      impl = (args, ctx) => {
        val values = actualArgs(args)
        if values.nonEmpty then
          write("Trace: " + values.map(PrettyPrinter.consoleFormat).mkString(" "), false)
        // Arguments aside, the stack is attached to a fresh Error object.
        val error = ctx.createError("Error", "")
        error match {
          case JSValue.Object(obj) =>
            obj.get("stack")(using ctx) match {
              case JSValue.JSStr(stack) => write(stack, toStderr = true)
              case _                    => ()
            }
          case _ => ()
        }
        JSValue.Undefined
      }
    )
    consoleObj.set("trace", JSValue.Native(traceFn))

    consoleObj
  }

  /** Initialize console in the given context */
  def initialize()(using ctx: JSContext): Unit = {
    ctx.global.set("console", JSValue.Object(createConsole()))
  }

  /** Create the console object without installing it on the global object. */
  def create()(using ctx: JSContext): JSValue.Object =
    JSValue.Object(createConsole())
}
