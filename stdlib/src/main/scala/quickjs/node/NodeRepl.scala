package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.{JSContext, JSException}
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.JSObject

import scala.collection.mutable

/** Node's `repl` module.
  *
  * A line-based REPL built on `readline`: each line is evaluated in the
  * server context (global scope by default) and the result is printed with
  * Node-style inspection. `.help`, `.exit`, `.break` and `.clear` are
  * available, plus user commands registered with `defineCommand`.
  */
object NodeRepl {

  private def optionOf(options: JSValue, key: String)(using
      ctx: JSContext
  ): JSValue =
    options match {
      case JSValue.Object(_) => BuiltinHelpers.getPropertyWithGetter(options, key)
      case _                 => JSValue.Undefined
    }

  private def makeServer(options: JSValue)(using ctx: JSContext): JSObject = {
    val emitter = new JsEmitter
    val server = JSObject(prototype = ctx.objectPrototype)
    emitter.install(server)

    val context = optionOf(options, "context") match {
      case value @ JSValue.Object(_) => value
      case _                         => JSValue.Object(ctx.global)
    }
    val useGlobal = optionOf(options, "useGlobal") match {
      case JSValue.Bool(b) => b
      case _               => true
    }
    val prompt = optionOf(options, "prompt") match {
      case JSValue.JSStr(s) => s
      case _                => "> "
    }

    val commands = mutable.LinkedHashMap.empty[String, (String, JSValue)]

    def print(text: String): Unit =
      val output = optionOf(options, "output") match {
        case value if value != JSValue.Undefined => value
        case _ =>
          ctx.global.get("process") match {
            case JSValue.Object(process) =>
              BuiltinHelpers.getPropertyWithGetter(
                JSValue.Object(process),
                "stdout"
              )
            case _ => JSValue.Undefined
          }
      }
      val write = BuiltinHelpers.getPropertyWithGetter(output, "write")
      if BuiltinHelpers.isCallable(write) then
        BuiltinHelpers.callFunctionWithThis(
          write,
          output,
          Array(JSValue.fromString(text + "\n"))
        )

    def evaluate(source: String): Unit = {
      val trimmed = source.trim
      if trimmed.isEmpty then ()
      else if commands.contains(trimmed) then {
        val (_, action) = commands(trimmed)
        BuiltinHelpers.callFunctionWithThis(
          action,
          JSValue.Object(server),
          Array(JSValue.fromString(trimmed))
        )
      } else {
        val evalFn = ctx.global.get("eval")
        try {
          // Native functions treat the first argument as `this`; call `eval`
          // directly so the source is its first argument.
          val result = evalFn match {
            case JSValue.Native(nf: NativeFunction) =>
              nf.call(Array(JSValue.fromString(source)))
            case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
              nc.call(Array(JSValue.fromString(source)))
            case other =>
              BuiltinHelpers.callFunctionWithThis(
                other,
                JSValue.Undefined,
                Array(JSValue.fromString(source))
              )
          }
          if result != JSValue.Undefined then
            print(NodeUtil.inspect(result))
        } catch {
          case ex: JSException =>
            val message = ex.getValue match {
              case JSValue.Object(obj) =>
                obj.get("stack") match {
                  case JSValue.JSStr(stack) if stack.nonEmpty => stack
                  case _ =>
                    Option(ex.getMessage).getOrElse("Error")
                }
              case _ => Option(ex.getMessage).getOrElse("Error")
            }
            print(message.trim)
        }
      }
    }

    val readlineOptions = JSObject(prototype = ctx.objectPrototype)
    optionOf(options, "input") match {
      case value if value != JSValue.Undefined =>
        readlineOptions.set("input", value)
      case _ => ()
    }
    optionOf(options, "output") match {
      case value if value != JSValue.Undefined =>
        readlineOptions.set("output", value)
      case _ => ()
    }
    optionOf(options, "terminal") match {
      case value if value != JSValue.Undefined =>
        readlineOptions.set("terminal", value)
      case _ =>
        readlineOptions.set("terminal", JSValue.Bool(false))
    }
    readlineOptions.set("prompt", JSValue.fromString(prompt))

    val rl = NodeReadline.makeInterface(
      JSValue.Object(readlineOptions),
      promises = false
    )
    val onLine = NativeFunction(
      name = "onLine",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val line = args.lastOption.map(NodeHelpers.toStr(_)).getOrElse("")
        if line == ".exit" then {
          BuiltinHelpers.callFunctionWithThis(
            BuiltinHelpers.getPropertyWithGetter(JSValue.Object(rl), "close"),
            JSValue.Object(rl),
            Array.empty
          )
        } else if line == ".break" || line == ".clear" then print("")
        else evaluate(line)
        val promptFn = BuiltinHelpers.getPropertyWithGetter(
          JSValue.Object(rl),
          "prompt"
        )
        if BuiltinHelpers.isCallable(promptFn) then
          BuiltinHelpers.callFunctionWithThis(
            promptFn,
            JSValue.Object(rl),
            Array.empty
          )
        JSValue.Undefined
      }
    )
    val onClose = NativeFunction(
      name = "onClose",
      length = 0,
      impl = (_, _) => {
        server.set("closed", JSValue.Bool(true))
        emitter.emit("exit", Array.empty)
        JSValue.Undefined
      }
    )
    val on = BuiltinHelpers.getPropertyWithGetter(JSValue.Object(rl), "on")
    if BuiltinHelpers.isCallable(on) then {
      BuiltinHelpers.callFunctionWithThis(
        on,
        JSValue.Object(rl),
        Array(JSValue.fromString("line"), JSValue.Native(onLine))
      )
      BuiltinHelpers.callFunctionWithThis(
        on,
        JSValue.Object(rl),
        Array(JSValue.fromString("close"), JSValue.Native(onClose))
      )
    }

    // Test hook: expose the underlying readline interface (non-enumerable).
    server.defineProperty(
      "__rl",
      JSValue.Object(rl),
      enumerable = false,
      writable = false,
      configurable = true
    )(using ctx)
    server.set("context", context)
    server.set("useGlobal", JSValue.Bool(useGlobal))
    server.set("closed", JSValue.Bool(false))
    val defineCommand = NativeFunction(
      name = "defineCommand",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = args.headOption match {
          case Some(JSValue.Object(obj)) if obj eq server => args.drop(1)
          case _                                           => args
        }
        val name = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
        val spec = rest.lift(1).getOrElse(JSValue.Undefined)
        val help = BuiltinHelpers.getPropertyWithGetter(spec, "help") match {
          case JSValue.JSStr(s) => s
          case _                => ""
        }
        val action = BuiltinHelpers.getPropertyWithGetter(spec, "action")
        if name.nonEmpty then commands(name) = (help, action)
        JSValue.Undefined
      }
    )
    server.set("defineCommand", JSValue.Native(defineCommand))

    commands(".help") = (
      "Print this help message",
      JSValue.Native(
        NativeFunction(
          name = "help",
          length = 0,
          impl = (_, _) => {
            print(
              ".break  Sometimes you get stuck, this gets you out\n" +
                ".clear  Alias for .break\n" +
                ".exit   Exit the repl\n" +
                ".help   Print this help message"
            )
            JSValue.Undefined
          }
        )
      )
    )
    commands(".break") = (
      "",
      JSValue.Native(
        NativeFunction(name = "break", length = 0, impl = (_, _) => JSValue.Undefined)
      )
    )
    commands(".clear") = (
      "",
      JSValue.Native(
        NativeFunction(name = "clear", length = 0, impl = (_, _) => JSValue.Undefined)
      )
    )

    val closeFn = NativeFunction(
      name = "close",
      length = 0,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rlClose = BuiltinHelpers.getPropertyWithGetter(
          JSValue.Object(rl),
          "close"
        )
        if BuiltinHelpers.isCallable(rlClose) then
          BuiltinHelpers.callFunctionWithThis(
            rlClose,
            JSValue.Object(rl),
            Array.empty
          )
        JSValue.Undefined
      }
    )
    server.set("close", JSValue.Native(closeFn))

    // Kick off: print the prompt once the interface is live.
    val promptFn =
      BuiltinHelpers.getPropertyWithGetter(JSValue.Object(rl), "prompt")
    if BuiltinHelpers.isCallable(promptFn) then
      BuiltinHelpers.callFunctionWithThis(
        promptFn,
        JSValue.Object(rl),
        Array.empty
      )

    server
  }

  def create()(using ctx: JSContext): JSValue = {
    val repl = JSObject(prototype = ctx.objectPrototype)
    repl.set(
      "start",
      JSValue.Native(
        NativeFunction(
          name = "start",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = args.headOption match {
              case Some(JSValue.Object(obj)) if obj eq repl => args.drop(1)
              case _                                        => args
            }
            JSValue.Object(makeServer(rest.headOption.getOrElse(JSValue.Undefined)))
          }
        )
      )
    )
    val replServerCtor = NativeConstructor(
      name = "REPLServer",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        callCtx.throwTypeError(
          "Class constructor REPLServer cannot be invoked without 'new'"
        )
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        JSValue.Object(makeServer(args.headOption.getOrElse(JSValue.Undefined)))
      },
      prototype = JSObject(prototype = ctx.objectPrototype)
    )
    BuiltinHelpers.initConstructor(replServerCtor, length = 0)
    repl.set("REPLServer", JSValue.Native(replServerCtor))
    repl.set("REPL_MODE_SLOPPY", JSValue.fromString("sloppy"))
    repl.set("REPL_MODE_STRICT", JSValue.fromString("strict"))
    JSValue.Object(repl)
  }
}
