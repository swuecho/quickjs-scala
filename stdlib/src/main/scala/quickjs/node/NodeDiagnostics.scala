package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.JSObject

/** Minimal `diagnostics_channel` module (channels that never have
  * subscribers). Enough for lru-cache and similar packages.
  */
object NodeDiagnostics {

  def create()(using ctx: JSContext): JSValue = {
    val module = JSObject(prototype = ctx.objectPrototype)
    val channels = scala.collection.mutable.HashMap.empty[String, JSObject]

    def channelObject(name: String): JSObject =
      channels.getOrElseUpdate(
        name, {
          val channel = JSObject(prototype = ctx.objectPrototype)
          channel.set("name", JSValue.fromString(name))
          channel.set("hasSubscribers", JSValue.Bool(false))
          def noop(n: String, len: Int): Unit =
            channel.set(
              n,
              JSValue.Native(
                NativeFunction(n, (args, _) => args.headOption.getOrElse(JSValue.Undefined), length = len)
              )
            )
          noop("subscribe", 1)
          noop("unsubscribe", 1)
          noop("publish", 1)
          noop("bindStore", 1)
          noop("unbindStore", 1)
          channel.set(
            "runStores",
            JSValue.Native(
              NativeFunction(
                "runStores",
                (args, callCtx) => {
                  given JSContext = callCtx
                  args.reverse.find(BuiltinHelpers.isCallable) match {
                    case Some(fn) =>
                      BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, Array.empty)
                    case None => JSValue.Undefined
                  }
                },
                length = 3
              )
            )
          )
          channel
        }
      )

    module.set(
      "channel",
      JSValue.Native(
        NativeFunction(
          name = "channel",
          length = 1,
          impl = (args, c) => {
            given JSContext = c
            JSValue.Object(
              channelObject(args.headOption.map(NodeHelpers.toStr(_)).getOrElse(""))
            )
          }
        )
      )
    )
    module.set(
      "tracingChannel",
      JSValue.Native(
        NativeFunction(
          name = "tracingChannel",
          length = 1,
          impl = (args, c) => {
            given JSContext = c
            val name = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            val tracing = JSObject(prototype = ctx.objectPrototype)
            tracing.set("hasSubscribers", JSValue.Bool(false))
            Seq("start", "end", "asyncStart", "asyncEnd", "error").foreach { key =>
              tracing.set(
                key,
                JSValue.Native(
                  NativeFunction(key, (_, _) => JSValue.Undefined, length = 1)
                )
              )
            }
            Seq("subscribe", "unsubscribe", "unsubscribe").foreach { key =>
              tracing.set(
                key,
                JSValue.Native(
                  NativeFunction(key, (a, _) => a.headOption.getOrElse(JSValue.Undefined), length = 1)
                )
              )
            }
            Seq(
              "traceSync",
              "tracePromise",
              "traceCallback"
            ).foreach { key =>
              tracing.set(
                key,
                JSValue.Native(
                  NativeFunction(
                    key,
                    (a, callCtx) => {
                      given JSContext = callCtx
                      val fn = a.reverse.find(BuiltinHelpers.isCallable)
                      fn match {
                        case Some(f) =>
                          BuiltinHelpers.callFunctionWithThis(f, JSValue.Undefined, a.drop(1))
                        case None => JSValue.Undefined
                      }
                    },
                    length = 2
                  )
                )
              )
            }
            tracing.set(
              "channel",
              JSValue.Object(channelObject(name))
            )
            JSValue.Object(tracing)
          }
        )
      )
    )
    JSValue.Object(module)
  }
}
