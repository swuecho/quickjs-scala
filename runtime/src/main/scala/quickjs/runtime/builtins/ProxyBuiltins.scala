package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext

/** Proxy built-in: Proxy constructor. */
object ProxyBuiltins:
  import quickjs.objmodel.JSObject

  def initialize(ctx: JSContext): Unit =
    val proxyConstructor = quickjs.value.NativeConstructor(
      name = "Proxy",
      callImpl = (_, _) =>
        throw new RuntimeException("Proxy constructor must be called with 'new'"),
      constructImpl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Proxy constructor requires target and handler")
        else
          val target = args(0)
          val handler = args(1)
          val proxyObj = JSObject(prototype = ctx.objectPrototype, extensible = true)
          given JSContext = ctx
          proxyObj.defineProperty("__proxy_target", target, enumerable = false)
          proxyObj.defineProperty("__proxy_handler", handler, enumerable = false)
          JSValue.Object(proxyObj),
      prototype = ctx.objectPrototype
    )

    val proxyRevocable = NativeFunction(
      name = "revocable",
      length = 2,
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Proxy.revocable requires target and handler")
        else
          given JSContext = ctx
          val target = args(0)
          val handler = args(1)
          var revoked = false
          val proxyObj = JSObject(prototype = ctx.objectPrototype, extensible = true)
          proxyObj.defineProperty("__proxy_target", target, enumerable = false)
          proxyObj.defineProperty("__proxy_handler", handler, enumerable = false)
          val revokeFunc = NativeFunction(
            name = "revoke",
            length = 0,
            impl = (_, _) =>
              if !revoked then
                revoked = true
                proxyObj.defineProperty("__proxy_target", JSValue.Null, enumerable = false)
                proxyObj.defineProperty("__proxy_handler", JSValue.Null, enumerable = false)
              JSValue.Undefined
          )
          val resultObj = JSObject(prototype = ctx.objectPrototype, extensible = true)
          resultObj.defineProperty("proxy", JSValue.Object(proxyObj), enumerable = true, writable = true, configurable = true)
          resultObj.defineProperty("revoke", JSValue.Native(revokeFunc), enumerable = true, writable = true, configurable = true)
          JSValue.Object(resultObj)
    )

    given JSContext = ctx
    BuiltinHelpers.initConstructor(proxyConstructor, length = 2)
    ctx.global.set("Proxy", JSValue.Native(proxyConstructor))
    proxyConstructor.funcObj.defineProperty("revocable", JSValue.Native(proxyRevocable), enumerable = false, writable = true, configurable = true)
