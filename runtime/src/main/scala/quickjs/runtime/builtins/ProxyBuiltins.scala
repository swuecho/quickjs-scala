package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext

/** Proxy built-in: Proxy constructor. */
object ProxyBuiltins {
  import quickjs.objmodel.JSObject

  private def isObjectLike(value: JSValue): Boolean =
    value match {
      case JSValue.Object(_) | _: JSValue.Function | JSValue.JSArrayVal(_) |
          JSValue.Native(_) =>
        true
      case _ => false
    }

  private def newProxyObject(target: JSValue, handler: JSValue)(using
      ctx: JSContext
  ): JSValue.Object = {
    if !isObjectLike(target) || !isObjectLike(handler) then
      ctx.throwTypeError("Proxy target and handler must be objects")
    val proxyObj =
      JSObject(prototype = ctx.objectPrototype, extensible = true)
    proxyObj.defineProperty("__proxy_target", target, enumerable = false)
    proxyObj.defineProperty(
      "__proxy_handler",
      handler,
      enumerable = false
    )
    proxyObj.defineProperty(
      "__proxy_revoked",
      JSValue.Bool(false),
      enumerable = false,
      writable = true,
      configurable = false
    )
    JSValue.Object(proxyObj)
  }

  def initialize(ctx: JSContext): Unit = {
    val proxyConstructor = quickjs.value.NativeConstructor(
      name = "Proxy",
      callImpl = (_, _) =>
        throw new RuntimeException(
          "Proxy constructor must be called with 'new'"
        ),
      constructImpl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException(
            "Proxy constructor requires target and handler"
          )
        else {
          given JSContext = ctx
          val target = args(0)
          val handler = args(1)
          newProxyObject(target, handler)
        }
      ,
      prototype = ctx.objectPrototype
    )

    val proxyRevocable = NativeFunction(
      name = "revocable",
      length = 2,
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException(
            "Proxy.revocable requires target and handler"
          )
        else {
          given JSContext = ctx
          val target = args(0)
          val handler = args(1)
          var revoked = false
          val proxy = newProxyObject(target, handler)
          val proxyObj = proxy.value
          val revokeFunc = NativeFunction(
            name = "revoke",
            length = 0,
            impl = (_, _) =>
              if !revoked then {
                revoked = true
                proxyObj.defineProperty(
                  "__proxy_target",
                  JSValue.Null,
                  enumerable = false
                )
                proxyObj.defineProperty(
                  "__proxy_handler",
                  JSValue.Null,
                  enumerable = false
                )
                proxyObj.defineProperty(
                  "__proxy_revoked",
                  JSValue.Bool(true),
                  enumerable = false,
                  writable = true,
                  configurable = false
                )
              }
              JSValue.Undefined
          )
          val resultObj =
            JSObject(prototype = ctx.objectPrototype, extensible = true)
          resultObj.defineProperty(
            "proxy",
            proxy,
            enumerable = true,
            writable = true,
            configurable = true
          )
          resultObj.defineProperty(
            "revoke",
            JSValue.Native(revokeFunc),
            enumerable = true,
            writable = true,
            configurable = true
          )
          JSValue.Object(resultObj)
        }
    )

    given JSContext = ctx
    BuiltinHelpers.initConstructor(proxyConstructor, length = 2)
    ctx.global.set("Proxy", JSValue.Native(proxyConstructor))
    proxyConstructor.funcObj.defineProperty(
      "revocable",
      JSValue.Native(proxyRevocable),
      enumerable = false,
      writable = true,
      configurable = true
    )
  }
}
