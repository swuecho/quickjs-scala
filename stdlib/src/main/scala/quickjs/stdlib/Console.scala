package quickjs.stdlib

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject
import quickjs.util.PrettyPrinter

/** JavaScript console object implementation.
  *
  * Provides console.log() and related debugging functions.
  */
object Console:
  /** Initialize console in the given context */
  def initialize()(using ctx: JSContext): Unit =
    val consoleObj = JSObject(prototype = null, extensible = true)

    // console.log - prints values to stdout separated by spaces
    val logFunc = NativeFunction("log", (args, context) =>
      val output = args.map(PrettyPrinter.shortFormat).mkString(" ")
      println(output)
      JSValue.Undefined
    )
    consoleObj.set("log", JSValue.Native(logFunc))

    ctx.global.set("console", JSValue.Object(consoleObj))

  /** Create the console object with log method */
  def create()(using ctx: JSContext): JSValue.Object =
    val consoleObj = JSObject(prototype = null, extensible = true)

    // console.log - prints values to stdout separated by spaces
    val logFunc = NativeFunction("log", (args, context) =>
      val output = args.map(PrettyPrinter.shortFormat).mkString(" ")
      println(output)
      JSValue.Undefined
    )
    consoleObj.set("log", JSValue.Native(logFunc))

    JSValue.Object(consoleObj)
