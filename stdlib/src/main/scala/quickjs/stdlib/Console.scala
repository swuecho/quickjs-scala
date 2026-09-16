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

    /** True when `container` carries `self` as one of its own properties, i.e.
      * the console method was copied onto it (`Base.consoleLog = console.log`)
      * and is being called as a method. The receiver must not be printed.
      */
    def holdsFunction(container: JSValue, self: JSValue): Boolean = {
      val objOpt: Option[JSObject] = container match {
        case JSValue.Object(obj)     => Some(obj)
        case f: JSValue.Function     => Some(f.funcObj)
        case JSValue.Native(nf: NativeFunction) => Some(nf.funcObj)
        case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
          Some(nc.funcObj)
        case _ => None
      }
      objOpt.exists { obj =>
        obj.getAllOwnPropertyKeys().exists {
          case key: String =>
            obj.getOwnPropertyDescriptor(key).exists { case (value, _) =>
              value == self
            }
          case _ => false
        }
      }
    }

    /** Drop the receiver (`this` = the console object or any object the
      * console method was copied onto) from the arguments.
      */
    def actualArgs(args: Array[JSValue], self: JSValue): Array[JSValue] =
      args.headOption match {
        case Some(JSValue.Object(obj)) if obj eq consoleObj => args.drop(1)
        case Some(value) if holdsFunction(value, self)      => args.drop(1)
        case _                                              => args
      }

    def write(text: String, toStderr: Boolean): Unit =
      if toStderr then System.err.println(text) else System.out.println(text)

    def printMethod(name: String, toStderr: Boolean): Unit = {
      var selfRef: NativeFunction = null
      val fn = NativeFunction(
        name = name,
        length = 0,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val values = actualArgs(args, JSValue.Native(selfRef))
          // Node routes console output through `util.format`, so `%s`/`%d`
          // substitutions and object inspection match the runtime.
          write(quickjs.node.NodeUtil.format(values), toStderr)
          JSValue.Undefined
        }
      )
      selfRef = fn
      consoleObj.set(name, JSValue.Native(fn))
    }

    printMethod("log", toStderr = false)
    printMethod("info", toStderr = false)
    printMethod("debug", toStderr = false)
    printMethod("dir", toStderr = false)
    printMethod("error", toStderr = true)
    printMethod("warn", toStderr = true)

    var assertRef: NativeFunction = null
    val assertFn = NativeFunction(
      name = "assert",
      length = 0,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val values = actualArgs(args, JSValue.Native(assertRef))
        if values.isEmpty || !values(0).toBoolean then {
          val message =
            if values.length > 1 then
              quickjs.node.NodeUtil.format(values.drop(1))
            else "console.assert"
          write("Assertion failed: " + message, toStderr = true)
        }
        JSValue.Undefined
      }
    )
    assertRef = assertFn
    consoleObj.set("assert", JSValue.Native(assertFn))

    var traceRef: NativeFunction = null
    val traceFn = NativeFunction(
      name = "trace",
      length = 0,
      impl = (args, ctx) => {
        given JSContext = ctx
        val values = actualArgs(args, JSValue.Native(traceRef))
        if values.nonEmpty then
          write("Trace: " + quickjs.node.NodeUtil.format(values), false)
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
    traceRef = traceFn
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
