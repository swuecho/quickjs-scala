package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject

/** Node's `perf_hooks` module and the global `performance` object. */
object NodePerfHooks {

  def install()(using ctx: JSContext): JSValue = {
    val startNanos = System.nanoTime()
    val timeOrigin = System.currentTimeMillis().toDouble

    val performance = JSObject(prototype = ctx.objectPrototype)

    def method(name: String, length: Int)(impl: () => JSValue): Unit =
      performance.defineProperty(
        name,
        JSValue.Native(
          NativeFunction(
            name = name,
            length = length,
            impl = (_, _) => impl()
          )
        ),
        enumerable = false,
        writable = true,
        configurable = true
      )

    method("now", 0)(() =>
      JSValue.fromDouble((System.nanoTime() - startNanos).toDouble / 1e6)
    )
    performance.defineProperty(
      "timeOrigin",
      JSValue.fromDouble(timeOrigin),
      enumerable = false,
      writable = false,
      configurable = true
    )
    method("mark", 1)(() => JSValue.Undefined)
    method("measure", 1)(() => JSValue.Undefined)
    method("clearMarks", 1)(() => JSValue.Undefined)
    method("clearMeasures", 1)(() => JSValue.Undefined)
    method("toJSON", 0)(() =>
      JSValue.Object(
        NodeHelpers.objectOf(
          "timeOrigin" -> JSValue.fromDouble(timeOrigin)
        )
      )
    )

    ctx.global.set("performance", JSValue.Object(performance))

    val perfHooks = JSObject(prototype = ctx.objectPrototype)
    perfHooks.set("performance", JSValue.Object(performance))
    perfHooks.set(
      "constants",
      JSValue.Object(
        NodeHelpers.objectOf(
          "NODE_PERFORMANCE_GC_MAJOR" -> JSValue.fromInt(4),
          "NODE_PERFORMANCE_GC_MINOR" -> JSValue.fromInt(1),
          "NODE_PERFORMANCE_GC_INCREMENTAL" -> JSValue.fromInt(8),
          "NODE_PERFORMANCE_GC_WEAKCB" -> JSValue.fromInt(16)
        )
      )
    )
    JSValue.Object(perfHooks)
  }
}
