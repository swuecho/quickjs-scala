package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.{JSArray, JSObject}

/** Loop-backed timer globals plus the `timers` and `timers/promises` modules. */
object NodeTimers {

  def installGlobals(loop: HostEventLoop)(using ctx: JSContext): Unit = {
    def stripReceiver(args: Array[JSValue], receiver: JSObject): Array[JSValue] =
      NodeHelpers.stripReceiver(args, receiver)

    // The globals are frequently copied onto other objects (mocha does
    // `Runner.immediately = global.setImmediate`), so a method-style call
    // passes the new receiver as the first argument. Ignore a leading value
    // that cannot be a timer callback/id.
    def timerArgs(args: Array[JSValue]): Array[JSValue] =
      args.headOption match {
        case Some(value)
            if isTimerCallback(value) ||
              value.isInstanceOf[JSValue.JSStr] =>
          args
        case Some(_) if args.length > 1 => args.drop(1)
        case other                      => args
      }
    // Class constructors are functions but cannot be timer callbacks; when a
    // copied global is called as a method the receiver is a class
    // (`Runner.immediately = global.setImmediate`).
    def isTimerCallback(value: JSValue): Boolean = value match {
      case f: JSValue.Function => !f.isClassConstructor
      case JSValue.Native(_: quickjs.value.NativeFunction) => true
      case JSValue.Native(_: quickjs.value.NativeConstructor) => false
      case _ => false
    }
    def clearArgs(args: Array[JSValue]): Array[JSValue] =
      args.headOption match {
        case Some(_: JSValue.Int32) | Some(_: JSValue.Float64) => args
        case Some(_) if args.length > 1                        => args.drop(1)
        case other                                             => args
      }
    val setTimeoutFn = NativeFunction(
      name = "setTimeout",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val a = timerArgs(args)
        val callback = a.headOption.getOrElse(JSValue.Undefined)
        val delay = if a.length > 1 then NodeHelpers.toNumber(a(1)) else 0.0
        val rest = if a.length > 2 then a.drop(2) else Array.empty[JSValue]
        JSValue.fromInt(loop.schedule(asCallback(callback), delay, rest, interval = false))
      }
    )
    val setIntervalFn = NativeFunction(
      name = "setInterval",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val a = timerArgs(args)
        val callback = a.headOption.getOrElse(JSValue.Undefined)
        val delay = if a.length > 1 then NodeHelpers.toNumber(a(1)) else 0.0
        val rest = if a.length > 2 then a.drop(2) else Array.empty[JSValue]
        JSValue.fromInt(loop.schedule(asCallback(callback), delay, rest, interval = true))
      }
    )
    val clearFn = NativeFunction(
      name = "clearTimeout",
      length = 1,
      impl = (args, _) => {
        clearArgs(args).headOption.foreach {
          case JSValue.Int32(id)   => loop.clear(id)
          case JSValue.Float64(d)  => loop.clear(d.toInt)
          case _                   => ()
        }
        JSValue.Undefined
      }
    )
    val setImmediateFn = NativeFunction(
      name = "setImmediate",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val a = timerArgs(args)
        val callback = a.headOption.getOrElse(JSValue.Undefined)
        val rest = if a.length > 1 then a.drop(1) else Array.empty[JSValue]
        JSValue.fromInt(loop.scheduleImmediate(asCallback(callback), rest))
      }
    )
    val clearImmediateFn = NativeFunction(
      name = "clearImmediate",
      length = 1,
      impl = (args, _) => {
        clearArgs(args).headOption.foreach {
          case JSValue.Int32(id)  => loop.clear(id)
          case JSValue.Float64(d) => loop.clear(d.toInt)
          case _                  => ()
        }
        JSValue.Undefined
      }
    )

    ctx.global.set("setTimeout", JSValue.Native(setTimeoutFn))
    ctx.global.set("setInterval", JSValue.Native(setIntervalFn))
    ctx.global.set("clearTimeout", JSValue.Native(clearFn))
    ctx.global.set("clearInterval", JSValue.Native(clearFn))
    ctx.global.set("setImmediate", JSValue.Native(setImmediateFn))
    ctx.global.set("clearImmediate", JSValue.Native(clearImmediateFn))
  }

  /** String timer bodies are compiled and evaluated when the timer fires. */
  private def asCallback(value: JSValue)(using ctx: JSContext): JSValue =
    value match {
      case JSValue.JSStr(code) =>
        JSValue.Native(
          NativeFunction(
            name = "<setTimeout>",
            length = 0,
            impl = (_, fireCtx) => {
              given JSContext = fireCtx
              try {
                val tokens = quickjs.lexer.Lexer(code).tokenize()
                val ast = quickjs.parser.Parser(tokens).parseScript()
                val bytecode = quickjs.compiler.Compiler().compileScript(ast)
                quickjs.interpreter
                  .Interpreter()
                  .call(bytecode, JSValue.Undefined, Array.empty)
              } catch case _: Throwable => ()
              JSValue.Undefined
            }
          )
        )
      case other => other
    }

  /** `require('timers')`: the loop-backed global functions. */
  def createModule(loop: HostEventLoop)(using ctx: JSContext): JSValue = {
    val timers = JSObject(prototype = ctx.objectPrototype)
    Seq("setTimeout", "setInterval", "clearTimeout", "clearInterval", "setImmediate", "clearImmediate")
      .foreach(name => timers.set(name, ctx.global.get(name)))
    JSValue.Object(timers)
  }

  /** `require('timers/promises')`. */
  def createPromisesModule(loop: HostEventLoop)(using ctx: JSContext): JSValue = {
    val promises = JSObject(prototype = ctx.objectPrototype)

    def newPromise(
        executor: (JSValue, JSValue) => Unit
    ): JSValue = {
      ctx.global.get("Promise") match {
        case JSValue.Native(nc: NativeConstructor) =>
          val executorFn = NativeFunction(
            name = "executor",
            length = 2,
            impl = (args, _) => {
              val resolve = args.lift(1).getOrElse(JSValue.Undefined)
              val reject = args.lift(2).getOrElse(JSValue.Undefined)
              executor(resolve, reject)
              JSValue.Undefined
            }
          )
          nc.construct(Array(JSValue.Native(executorFn)))
        case _ => JSValue.Undefined
      }
    }

    promises.set(
      "setTimeout",
      JSValue.Native(
        NativeFunction(
          name = "setTimeout",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val delay = args.headOption.map(NodeHelpers.toNumber(_)).getOrElse(0.0)
            val value = args.lift(1).getOrElse(JSValue.Undefined)
            newPromise { (resolve, _) =>
              loop.schedule(
                JSValue.Native(
                  NativeFunction(
                    name = "resolveTimeout",
                    length = 0,
                    impl = (_, resolveCtx) =>
                      BuiltinHelpers.callFunctionWithThis(
                        resolve,
                        JSValue.Undefined,
                        Array(value)
                      )(using resolveCtx)
                  )
                ),
                delay,
                Array.empty,
                interval = false
              )
              ()
            }
          }
        )
      )
    )

    promises.set(
      "setImmediate",
      JSValue.Native(
        NativeFunction(
          name = "setImmediate",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val value = args.headOption.getOrElse(JSValue.Undefined)
            newPromise { (resolve, _) =>
              loop.scheduleImmediate(
                JSValue.Native(
                  NativeFunction(
                    name = "resolveImmediate",
                    length = 0,
                    impl = (_, resolveCtx) =>
                      BuiltinHelpers.callFunctionWithThis(
                        resolve,
                        JSValue.Undefined,
                        Array(value)
                      )(using resolveCtx)
                  )
                ),
                Array.empty
              )
              ()
            }
          }
        )
      )
    )

    JSValue.Object(promises)
  }
}
