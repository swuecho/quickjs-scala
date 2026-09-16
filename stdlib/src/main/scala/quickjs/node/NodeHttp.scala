package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.{BuiltinHelpers, PromiseBuiltins}
import quickjs.objmodel.{JSArray, JSObject}

import java.net.URI
import java.net.http.{
  HttpClient,
  HttpRequest,
  HttpResponse,
  HttpTimeoutException
}
import java.nio.charset.StandardCharsets
import java.time.Duration
import scala.collection.mutable

/** WHATWG `fetch`, `Headers`, `Request`, `Response`, `AbortController`/
  * `AbortSignal` and Node's `http`/`https` client modules, built on
  * `java.net.http.HttpClient` and the host event loop.
  */
final class NodeHttp(loop: HostEventLoop)(using val ctx: JSContext) {

  private val client: HttpClient =
    HttpClient
      .newBuilder()
      .followRedirects(HttpClient.Redirect.NORMAL)
      .connectTimeout(Duration.ofSeconds(30))
      .build()

  // =========================================================================
  // Small utilities
  // =========================================================================

  private def errorWithName(
      name: String,
      message: String
  ): JSValue = {
    val error = ctx.createError("Error", message)
    error match {
      case JSValue.Object(obj) =>
        obj.defineProperty(
          "name",
          JSValue.fromString(name),
          enumerable = false,
          writable = true,
          configurable = true
        )
      case _ => ()
    }
    error
  }

  private def abortError(message: String = "The operation was aborted"): JSValue =
    errorWithName("AbortError", message)

  private def rejectedPromise(reason: JSValue): JSValue = {
    val promise = JSValue.Promise()
    PromiseBuiltins.rejectPromiseValue(promise, reason)
    BuiltinHelpers.wrapPromise(promise)
  }

  private def resolvedPromise(value: JSValue): JSValue = {
    val promise = JSValue.Promise()
    PromiseBuiltins.settlePromise(promise, value)
    BuiltinHelpers.wrapPromise(promise)
  }

  private def jvmErrorToTypeError(e: Throwable, url: String): JSValue = {
    val message =
      Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
    val error = errorWithName("TypeError", s"fetch failed: $message")
    error match {
      case JSValue.Object(obj) =>
        obj.defineProperty(
          "cause",
          JSValue.fromString(message),
          enumerable = false,
          writable = true,
          configurable = true
        )
        obj.defineProperty(
          "url",
          JSValue.fromString(url),
          enumerable = false,
          writable = true,
          configurable = true
        )
      case _ => ()
    }
    error
  }

  // =========================================================================
  // Headers
  // =========================================================================

  private final class HeadersData {
    val map = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[String]]

    def append(name: String, value: String): Unit = {
      val key = name.toLowerCase
      map.getOrElseUpdate(key, mutable.ArrayBuffer.empty) += value
    }

    def set(name: String, value: String): Unit = {
      val key = name.toLowerCase
      map.remove(key)
      map(key) = mutable.ArrayBuffer(value)
    }

    def get(name: String): Option[String] = {
      val key = name.toLowerCase
      map.get(key).map(_.mkString(", ")).filter(_ => map.contains(key)).orElse {
        // An empty value list is possible after delete; report null then.
        if map.contains(key) then Some("") else None
      }
    }

    def has(name: String): Boolean = map.contains(name.toLowerCase)

    def delete(name: String): Unit = map.remove(name.toLowerCase)

    def copy(): HeadersData = {
      val copy = new HeadersData
      map.foreach { case (key, values) =>
        copy.map(key) = mutable.ArrayBuffer.from(values)
      }
      copy
    }
  }

  private def headersDataOf(value: JSValue): Option[HeadersData] =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__headersData") match {
          case Some(JSValue.Native(data: HeadersData)) => Some(data)
          case _                                       => None
        }
      case _ => None
    }

  private def validHeaderName(name: String): Boolean =
    name.nonEmpty && !name.exists(c =>
      c <= ' ' || c >= 0x7f || "()<>@,;:\\\"/[]?={}".indexOf(c) >= 0
    )

  private def validHeaderValue(value: String): Boolean =
    !value.exists(c => c == '\r' || c == '\n' || c == '\u0000')

  private def fillHeaders(
      data: HeadersData,
      init: JSValue
  ): Unit =
    init match {
      case JSValue.Undefined | JSValue.Null => ()
      case value if headersDataOf(value).isDefined =>
        val source = headersDataOf(value).get
        source.map.foreach { case (key, values) =>
          values.foreach(v => data.append(key, v))
        }
      case JSValue.JSArrayVal(arr) =>
        var i = 0
        while i < arr.getLength do {
          val pair = arr.get(i)
          val name =
            BuiltinHelpers.getPropertyWithGetter(pair, "0") match {
              case JSValue.JSStr(s) => s
              case other            => NodeHelpers.toStr(other)
            }
          val value =
            BuiltinHelpers.getPropertyWithGetter(pair, "1") match {
              case JSValue.JSStr(s) => s
              case other            => NodeHelpers.toStr(other)
            }
          if !validHeaderName(name) then
            ctx.throwTypeError(s"Invalid header name: $name")
          if !validHeaderValue(value) then
            ctx.throwTypeError(s"Invalid header value: $value")
          data.append(name, value)
          i += 1
        }
      case JSValue.Object(obj) =>
        obj.getAllOwnPropertyKeys().foreach { key =>
          obj.getOwnProperty(key).foreach { value =>
            if !validHeaderName(key) then
              ctx.throwTypeError(s"Invalid header name: $key")
            val text = NodeHelpers.toStr(value)
            if !validHeaderValue(text) then
              ctx.throwTypeError(s"Invalid header value: $text")
            data.append(key, text)
          }
        }
      case _ => ()
    }

  private var headersProto: JSObject = null

  private def makeHeaders(init: JSValue): JSValue = {
    val data = new HeadersData
    fillHeaders(data, init)
    headersObject(data)
  }

  private def headersObject(data: HeadersData): JSValue = {
    val obj = JSObject(prototype = headersProto)
    obj.initProperty(
      "__headersData",
      JSValue.Native(data),
      enumerable = false,
      writable = false,
      configurable = false
    )
    JSValue.Object(obj)
  }

  private def requireHeaders(thisValue: JSValue): HeadersData =
    headersDataOf(thisValue).getOrElse {
      NodeHelpers.throwCoded(
        "TypeError",
        "Value is not a Headers object",
        "ERR_INVALID_THIS"
      )
    }

  /** Node's `IncomingMessage.headers` is a plain lowercase-keyed object. */
  private def headersToPlainObject(data: HeadersData): JSValue = {
    val obj = JSObject(prototype = null)
    data.map.foreach { case (key, values) =>
      obj.set(key, JSValue.fromString(values.mkString(", ")))
    }
    JSValue.Object(obj)
  }

  private def headersToRawArray(data: HeadersData): JSArray = {
    val arr = JSArray.empty()
    data.map.foreach { case (key, values) =>
      values.foreach { value =>
        arr.push(JSValue.fromString(key))
        arr.push(JSValue.fromString(value))
      }
    }
    arr
  }

  private def headersToInput(data: HeadersData): JSValue = {
    val arr = JSArray.empty()
    data.map.foreach { case (key, values) =>
      values.foreach { value =>
        val pair = JSArray.empty()
        pair.push(JSValue.fromString(key))
        pair.push(JSValue.fromString(value))
        arr.push(JSValue.JSArrayVal(pair))
      }
    }
    JSValue.JSArrayVal(arr)
  }

  // =========================================================================
  // AbortController / AbortSignal
  // =========================================================================

  private final class AbortState {
    var aborted: Boolean = false
    var reason: JSValue = JSValue.Undefined
    val jsListeners = mutable.ArrayBuffer.empty[JSValue]
    val handlers = mutable.ArrayBuffer.empty[() => Unit]
  }

  private def abortStateOf(value: JSValue): Option[AbortState] =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__abortState") match {
          case Some(JSValue.Native(state: AbortState)) => Some(state)
          case _                                       => None
        }
      case _ => None
    }

  private var signalProto: JSObject = null

  private def makeSignal(): (JSValue, AbortState) = {
    val state = new AbortState
    val obj = JSObject(prototype = signalProto)
    obj.initProperty(
      "__abortState",
      JSValue.Native(state),
      enumerable = false,
      writable = false,
      configurable = false
    )
    (JSValue.Object(obj), state)
  }

  private def abortEvent(signalValue: JSValue): JSValue = {
    val event = JSObject(prototype = ctx.objectPrototype)
    event.set("type", JSValue.fromString("abort"))
    event.set("target", signalValue)
    event.set("currentTarget", signalValue)
    JSValue.Object(event)
  }

  private def triggerAbort(
      signalValue: JSValue,
      state: AbortState,
      reason: JSValue
  ): Unit = {
    if state.aborted then return
    val finalReason =
      if reason == JSValue.Undefined then
        abortError("This operation was aborted")
      else reason
    state.aborted = true
    state.reason = finalReason
    val event = abortEvent(signalValue)
    state.jsListeners.foreach { listener =>
      ctx.queueMicrotask(() => {
        try {
          BuiltinHelpers.callFunctionWithThis(
            listener,
            signalValue,
            Array(event)
          )
          ()
        } catch case _: Throwable => ()
      })
    }
    val handlers = state.handlers.toList
    state.handlers.clear()
    handlers.foreach { handler =>
      try handler()
      catch case _: Throwable => ()
    }
  }

  private def installAbort(): Unit = {
    signalProto = JSObject(prototype = ctx.objectPrototype)
    val signalCtor = NativeConstructor(
      name = "AbortSignal",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        NodeHelpers.throwCoded(
          "TypeError",
          "Illegal constructor",
          "ERR_ILLEGAL_CONSTRUCTOR"
        )
      },
      constructImpl = (_, callCtx) => {
        given JSContext = callCtx
        NodeHelpers.throwCoded(
          "TypeError",
          "Illegal constructor",
          "ERR_ILLEGAL_CONSTRUCTOR"
        )
      },
      prototype = signalProto
    )
    BuiltinHelpers.initConstructor(signalCtor, length = 0)

    signalProto.defineProperty(
      "aborted",
      JSValue.Native(
        NativeFunction(
          name = "get aborted",
          length = 0,
          impl = (args, _) =>
            JSValue.Bool(
              abortStateOf(args.headOption.getOrElse(JSValue.Undefined))
                .exists(_.aborted)
            )
        )
      ),
      enumerable = false,
      writable = false,
      configurable = true
    )
    // Convert the accessor-style data property into a getter.
    signalProto.defineAccessorPropertyDetailed(
      "aborted",
      Some(JSValue.Native(NativeFunction("get aborted", (args, _) =>
        JSValue.Bool(
          abortStateOf(args.headOption.getOrElse(JSValue.Undefined))
            .exists(_.aborted)
        )
      ))),
      None,
      hasGetter = true,
      hasSetter = false,
      enumerable = Some(false),
      configurable = Some(true)
    )
    signalProto.defineAccessorPropertyDetailed(
      "reason",
      Some(JSValue.Native(NativeFunction("get reason", (args, _) => {
        abortStateOf(args.headOption.getOrElse(JSValue.Undefined)) match {
          case Some(state) if state.aborted => state.reason
          case _                            => JSValue.Undefined
        }
      }))),
      None,
      hasGetter = true,
      hasSetter = false,
      enumerable = Some(false),
      configurable = Some(true)
    )
    signalProto.defineProperty(
      "throwIfAborted",
      JSValue.Native(
        NativeFunction(
          name = "throwIfAborted",
          length = 0,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            abortStateOf(args.headOption.getOrElse(JSValue.Undefined)) match {
              case Some(state) if state.aborted =>
                throw new quickjs.runtime.JSException(state.reason)
              case _ => JSValue.Undefined
            }
          }
        )
      ),
      enumerable = false,
      writable = true,
      configurable = true
    )
    signalProto.defineProperty(
      "addEventListener",
      JSValue.Native(
        NativeFunction(
          name = "addEventListener",
          length = 2,
          impl = (args, callCtx) => {
            val rest = if args.nonEmpty then args.drop(1) else args
            val eventType = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            val listener = rest.lift(1).getOrElse(JSValue.Undefined)
            if eventType == "abort" && BuiltinHelpers.isCallable(listener) then
              abortStateOf(args.headOption.getOrElse(JSValue.Undefined)).foreach(
                _.jsListeners += listener
              )
            JSValue.Undefined
          }
        )
      ),
      enumerable = false,
      writable = true,
      configurable = true
    )
    signalProto.defineProperty(
      "removeEventListener",
      JSValue.Native(
        NativeFunction(
          name = "removeEventListener",
          length = 2,
          impl = (args, callCtx) => {
            val rest = if args.nonEmpty then args.drop(1) else args
            val eventType = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            val listener = rest.lift(1).getOrElse(JSValue.Undefined)
            if eventType == "abort" then
              abortStateOf(args.headOption.getOrElse(JSValue.Undefined)).foreach {
                state =>
                  val index =
                    state.jsListeners.indexWhere(l =>
                      NodeHelpers.sameValue(l, listener)
                    )
                  if index >= 0 then state.jsListeners.remove(index)
              }
            JSValue.Undefined
          }
        )
      ),
      enumerable = false,
      writable = true,
      configurable = true
    )

    def staticSignal(name: String)(
        impl: (Array[JSValue], JSContext) => JSValue
    ): Unit =
      signalCtor.funcObj.defineProperty(
        name,
        JSValue.Native(NativeFunction(name, impl, length = 1)),
        enumerable = false,
        writable = true,
        configurable = true
      )
    staticSignal("abort") { (args, callCtx) =>
      given JSContext = callCtx
      val (signal, state) = makeSignal()
      val reason = args.headOption.getOrElse(JSValue.Undefined)
      triggerAbort(signal, state, reason)
      signal
    }
    staticSignal("timeout") { (args, callCtx) =>
      given JSContext = callCtx
      val ms = args.headOption.map(NodeHelpers.toNumber(_)).getOrElse(0.0)
      val (signal, state) = makeSignal()
      loop.schedule(
        JSValue.Native(
          NativeFunction(
            name = "abortTimeout",
            length = 0,
            impl = (_, timeoutCtx) => {
              triggerAbort(
                signal,
                state,
                errorWithName("TimeoutError", "The operation was aborted due to timeout")
              )
              JSValue.Undefined
            }
          )
        ),
        ms,
        Array.empty,
        interval = false
      )
      signal
    }

    val controllerProto = JSObject(prototype = ctx.objectPrototype)
    val controllerCtor = NativeConstructor(
      name = "AbortController",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        NodeHelpers.throwCoded(
          "TypeError",
          "Class constructor AbortController cannot be invoked without 'new'",
          "ERR_CONSTRUCT_CALL_REQUIRED"
        )
      },
      constructImpl = (_, callCtx) => {
        given JSContext = callCtx
        val (signal, state) = makeSignal()
        val controller = JSObject(prototype = controllerProto)
        controller.set("signal", signal)
        controller.initProperty(
          "__abortState",
          JSValue.Native(state),
          enumerable = false,
          writable = false,
          configurable = false
        )
        JSValue.Object(controller)
      },
      prototype = controllerProto
    )
    BuiltinHelpers.initConstructor(controllerCtor, length = 0)
    controllerProto.defineProperty(
      "abort",
      JSValue.Native(
        NativeFunction(
          name = "abort",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val controller = args.headOption.getOrElse(JSValue.Undefined)
            val reason = args.lift(1).getOrElse(JSValue.Undefined)
            controller match {
              case JSValue.Object(obj) =>
                obj.getOwnPropertyRaw("__abortState") match {
                  case Some(JSValue.Native(state: AbortState)) =>
                    obj.getOwnProperty("signal").foreach { signal =>
                      triggerAbort(signal, state, reason)
                    }
                  case _ => ()
                }
              case _ => ()
            }
            JSValue.Undefined
          }
        )
      ),
      enumerable = false,
      writable = true,
      configurable = true
    )

    ctx.global.set("AbortController", JSValue.Native(controllerCtor))
    ctx.global.set("AbortSignal", JSValue.Native(signalCtor))
  }

  /** Node-style type errors for invalid header values and the like. */
  private def illegalInvocation(message: String): Nothing =
    NodeHelpers.throwCoded("TypeError", message, "ERR_INVALID_ARG_VALUE")

  // =========================================================================
  // Body helpers
  // =========================================================================

  private def bodyBytes(value: JSValue): Option[Array[Byte]] =
    value match {
      case JSValue.Undefined | JSValue.Null => None
      case JSValue.JSStr(s) =>
        Some(NodeEncodings.bytesFromString(s, "utf8"))
      case other =>
        NodeBuffer.bytesOfValue(other).orElse {
          // URLSearchParams and anything else with a usable string form.
          other match {
            case JSValue.Object(obj)
                if obj.getOwnPropertyRaw("__uspData").isDefined =>
              val toStringFn = BuiltinHelpers.getPropertyWithGetter(other, "toString")
              BuiltinHelpers.callFunctionWithThis(toStringFn, other, Array.empty) match {
                case JSValue.JSStr(s) =>
                  Some(NodeEncodings.bytesFromString(s, "utf8"))
                case _ => None
              }
            case _ => None
          }
        }
    }

  private def bytesToBuffer(bytes: Array[Byte]): JSValue =
    NodeBuffer.makeBuffer(bytes)

  private def bytesToArrayBuffer(bytes: Array[Byte]): JSValue =
    ctx.global.get("ArrayBuffer") match {
      case JSValue.Native(nc: NativeConstructor) =>
        val buffer = nc.construct(Array(JSValue.fromInt(bytes.length)))
        buffer match {
          case JSValue.Object(obj) =>
            obj.getOwnPropertyRaw("__abStorage") match {
              case Some(JSValue.Native(storage: quickjs.runtime.builtins.TypedArrayBuiltins.ArrayBufferStorage)) =>
                System.arraycopy(bytes, 0, storage.data, 0, bytes.length)
              case _ => ()
            }
          case _ => ()
        }
        buffer
      case _ => bytesToBuffer(bytes)
    }

  // =========================================================================
  // Headers constructor / prototype
  // =========================================================================

  private def installHeaders(): NativeConstructor = {
    headersProto = JSObject(prototype = ctx.objectPrototype)
    val headersCtor = NativeConstructor(
      name = "Headers",
      callImpl = (args, callCtx) => {
        given JSContext = callCtx
        makeHeaders(args.headOption.getOrElse(JSValue.Undefined))
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        makeHeaders(args.headOption.getOrElse(JSValue.Undefined))
      },
      prototype = headersProto
    )
    BuiltinHelpers.initConstructor(headersCtor, length = 0)

    def method(name: String, length: Int)(
        impl: (HeadersData, Array[JSValue]) => JSValue
    ): Unit =
      headersProto.defineProperty(
        name,
        JSValue.Native(
          NativeFunction(
            name = name,
            length = length,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              val thisValue = args.headOption.getOrElse(JSValue.Undefined)
              val data = requireHeaders(thisValue)
              impl(data, args.drop(1))
            }
          )
        ),
        enumerable = false,
        writable = true,
        configurable = true
      )

    method("append", 2) { (data, args) =>
      val name = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      val value = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
      if !validHeaderName(name) then illegalInvocation(s"Invalid header name: $name")
      if !validHeaderValue(value) then illegalInvocation(s"Invalid header value: $value")
      data.append(name, value)
      JSValue.Undefined
    }
    method("delete", 1) { (data, args) =>
      val name = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      if !validHeaderName(name) then illegalInvocation(s"Invalid header name: $name")
      data.delete(name)
      JSValue.Undefined
    }
    method("get", 1) { (data, args) =>
      val name = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      if !validHeaderName(name) then illegalInvocation(s"Invalid header name: $name")
      data.get(name) match {
        case Some(value) => JSValue.fromString(value)
        case None        => JSValue.Null
      }
    }
    method("getSetCookie", 0) { (data, _) =>
      val arr = JSArray.empty()
      data.map.get("set-cookie").foreach(_.foreach(v => arr.push(JSValue.fromString(v))))
      JSValue.JSArrayVal(arr)
    }
    method("has", 1) { (data, args) =>
      val name = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      if !validHeaderName(name) then illegalInvocation(s"Invalid header name: $name")
      JSValue.Bool(data.has(name))
    }
    method("set", 2) { (data, args) =>
      val name = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      val value = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
      if !validHeaderName(name) then illegalInvocation(s"Invalid header name: $name")
      if !validHeaderValue(value) then illegalInvocation(s"Invalid header value: $value")
      data.set(name, value)
      JSValue.Undefined
    }
    method("forEach", 2) { (data, args) =>
      val callback = args.headOption.getOrElse(JSValue.Undefined)
      val thisArg = args.lift(1).getOrElse(JSValue.Undefined)
      if !BuiltinHelpers.isCallable(callback) then
        illegalInvocation("callback must be a function")
      data.map.foreach { case (key, values) =>
        BuiltinHelpers.callFunctionWithThis(
          callback,
          thisArg,
          Array(JSValue.fromString(values.mkString(", ")), JSValue.fromString(key))
        )
      }
      JSValue.Undefined
    }
    method("keys", 0) { (data, _) =>
      val arr = JSArray.empty()
      data.map.keys.foreach(k => arr.push(JSValue.fromString(k)))
      JSValue.JSArrayVal(arr)
    }
    method("values", 0) { (data, _) =>
      val arr = JSArray.empty()
      data.map.foreach { case (_, values) =>
        arr.push(JSValue.fromString(values.mkString(", ")))
      }
      JSValue.JSArrayVal(arr)
    }
    method("entries", 0) { (data, _) =>
      val arr = JSArray.empty()
      data.map.foreach { case (key, values) =>
        val pair = JSArray.empty()
        pair.push(JSValue.fromString(key))
        pair.push(JSValue.fromString(values.mkString(", ")))
        arr.push(JSValue.JSArrayVal(pair))
      }
      JSValue.JSArrayVal(arr)
    }
    headersProto.setSymbol(
      BuiltinHelpers.wellKnownSymbolId("iterator"),
      headersProto.get("entries")(using ctx)
    )(using ctx)
    headersCtor
  }

  // =========================================================================
  // Response / Request
  // =========================================================================

  private var responseProto: JSObject = null
  private var requestProto: JSObject = null

  private final case class ResponseData(
      status: Int,
      statusText: String,
      url: String,
      headers: HeadersData,
      var bytes: Array[Byte],
      var bodyUsed: Boolean
  )

  private def responseDataOf(value: JSValue): Option[ResponseData] =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__responseData") match {
          case Some(JSValue.Native(data: ResponseData)) => Some(data)
          case _                                        => None
        }
      case _ => None
    }

  private def makeResponse(
      data: ResponseData,
      type_ : String = "basic",
      redirected: Boolean = false
  ): JSValue = {
    val obj = JSObject(prototype = responseProto)
    obj.initProperty(
      "__responseData",
      JSValue.Native(data),
      enumerable = false,
      writable = false,
      configurable = false
    )
    obj.set("status", JSValue.fromInt(data.status))
    obj.set("statusText", JSValue.fromString(data.statusText))
    obj.set("url", JSValue.fromString(data.url))
    obj.set("ok", JSValue.Bool(data.status >= 200 && data.status < 300))
    obj.set("redirected", JSValue.Bool(redirected))
    obj.set("type", JSValue.fromString(type_))
    obj.set("headers", headersObject(data.headers))
    obj.set("bodyUsed", JSValue.Bool(data.bodyUsed))
    JSValue.Object(obj)
  }

  private def syncBodyUsed(responseValue: JSValue, used: Boolean): Unit =
    responseValue match {
      case JSValue.Object(obj) => obj.set("bodyUsed", JSValue.Bool(used))
      case _                   => ()
    }

  private def installResponse(): NativeConstructor = {
    responseProto = JSObject(prototype = ctx.objectPrototype)
    val responseCtor = NativeConstructor(
      name = "Response",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        NodeHelpers.throwCoded(
          "TypeError",
          "Class constructor Response cannot be invoked without 'new'",
          "ERR_CONSTRUCT_CALL_REQUIRED"
        )
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val bodyValue = args.headOption.getOrElse(JSValue.Null)
        val init = args.lift(1).getOrElse(JSValue.Undefined)
        val bytes = bodyBytes(bodyValue).getOrElse(Array.emptyByteArray)
        val status =
          BuiltinHelpers.getPropertyWithGetter(init, "status") match {
            case JSValue.Int32(n)   => n
            case JSValue.Float64(d) => d.toInt
            case _                  => 200
          }
        val statusText =
          BuiltinHelpers.getPropertyWithGetter(init, "statusText") match {
            case JSValue.JSStr(s) => s
            case _                => ""
          }
        val headersInit =
          BuiltinHelpers.getPropertyWithGetter(init, "headers")
        val headers = new HeadersData
        fillHeaders(headers, headersInit)
        makeResponse(
          ResponseData(status, statusText, "", headers, bytes, bodyUsed = false)
        )
      },
      prototype = responseProto
    )
    BuiltinHelpers.initConstructor(responseCtor, length = 0)

    def method(name: String, length: Int)(
        impl: (ResponseData, JSValue, Array[JSValue]) => JSValue
    ): Unit =
      responseProto.defineProperty(
        name,
        JSValue.Native(
          NativeFunction(
            name = name,
            length = length,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              val thisValue = args.headOption.getOrElse(JSValue.Undefined)
              val data = responseDataOf(thisValue).getOrElse(
                illegalInvocation("Value is not a Response object")
              )
              impl(data, thisValue, args.drop(1))
            }
          )
        ),
        enumerable = false,
        writable = true,
        configurable = true
      )

    method("text", 0) { (data, thisValue, _) =>
      if data.bodyUsed then
        rejectedPromise(errorWithName("TypeError", "Body is unusable"))
      else {
        data.bodyUsed = true
        syncBodyUsed(thisValue, used = true)
        resolvedPromise(
          JSValue.fromString(
            NodeEncodings.stringFromBytes(data.bytes, "utf8")
          )
        )
      }
    }
    method("json", 0) { (data, thisValue, _) =>
      if data.bodyUsed then
        rejectedPromise(errorWithName("TypeError", "Body is unusable"))
      else {
        data.bodyUsed = true
        syncBodyUsed(thisValue, used = true)
        val text = NodeEncodings.stringFromBytes(data.bytes, "utf8")
        try {
          val json = ctx.global.get("JSON")
          val parseFn = BuiltinHelpers.getPropertyWithGetter(json, "parse")
          resolvedPromise(
            BuiltinHelpers.callFunctionWithThis(parseFn, json, Array(JSValue.fromString(text)))
          )
        } catch {
          case e: quickjs.runtime.JSException => rejectedPromise(e.getValue)
          case e: Throwable =>
            rejectedPromise(errorWithName("SyntaxError", String.valueOf(e.getMessage)))
        }
      }
    }
    method("arrayBuffer", 0) { (data, thisValue, _) =>
      if data.bodyUsed then
        rejectedPromise(errorWithName("TypeError", "Body is unusable"))
      else {
        data.bodyUsed = true
        syncBodyUsed(thisValue, used = true)
        resolvedPromise(bytesToArrayBuffer(data.bytes))
      }
    }
    method("bytes", 0) { (data, thisValue, _) =>
      if data.bodyUsed then
        rejectedPromise(errorWithName("TypeError", "Body is unusable"))
      else {
        data.bodyUsed = true
        syncBodyUsed(thisValue, used = true)
        resolvedPromise(bytesToBuffer(data.bytes))
      }
    }
    method("clone", 0) { (data, thisValue, _) =>
      if data.bodyUsed then
        illegalInvocation("Body is unusable")
      makeResponse(
        ResponseData(
          data.status,
          data.statusText,
          data.url,
          data.headers.copy(),
          data.bytes.clone(),
          bodyUsed = false
        )
      )
    }

    // Static helpers
    responseCtor.funcObj.defineProperty(
      "json",
      JSValue.Native(
        NativeFunction(
          name = "json",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val json = callCtx.global.get("JSON")
            val stringifyFn = BuiltinHelpers.getPropertyWithGetter(json, "stringify")
            val text =
              BuiltinHelpers.callFunctionWithThis(
                stringifyFn,
                json,
                Array(args.headOption.getOrElse(JSValue.Undefined))
              ) match {
                case JSValue.JSStr(s) => s
                case _                => "null"
              }
            val data = new ResponseData(
              200,
              "",
              "",
              new HeadersData,
              NodeEncodings.bytesFromString(text, "utf8"),
              bodyUsed = false
            )
            data.headers.set("content-type", "application/json")
            makeResponse(data)
          }
        )
      ),
      enumerable = false,
      writable = true,
      configurable = true
    )
    responseCtor.funcObj.defineProperty(
      "error",
      JSValue.Native(
        NativeFunction(
          name = "error",
          length = 0,
          impl = (_, callCtx) => {
            given JSContext = callCtx
            makeResponse(
              new ResponseData(0, "", "", new HeadersData, Array.emptyByteArray, bodyUsed = false),
              type_ = "error"
            )
          }
        )
      ),
      enumerable = false,
      writable = true,
      configurable = true
    )
    responseCtor
  }

  private def installRequest(): NativeConstructor = {
    requestProto = JSObject(prototype = ctx.objectPrototype)
    val requestCtor = NativeConstructor(
      name = "Request",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        NodeHelpers.throwCoded(
          "TypeError",
          "Class constructor Request cannot be invoked without 'new'",
          "ERR_CONSTRUCT_CALL_REQUIRED"
        )
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val input = args.headOption.getOrElse(JSValue.Undefined)
        val init = args.lift(1).getOrElse(JSValue.Undefined)
        val (url, method, headers, body, signal) =
          parseRequest(input, init)
        val obj = JSObject(prototype = requestProto)
        obj.set("url", JSValue.fromString(url))
        obj.set("method", JSValue.fromString(method))
        obj.set("headers", headersObject(headers))
        obj.set(
          "body",
          body match {
            case Some(bytes) => bytesToBuffer(bytes)
            case None        => JSValue.Null
          }
        )
        obj.set("signal", signal.getOrElse(JSValue.Null))
        obj.set("destination", JSValue.fromString(""))
        obj.set("cache", JSValue.fromString("default"))
        obj.set("credentials", JSValue.fromString("same-origin"))
        obj.set("mode", JSValue.fromString("cors"))
        obj.set("redirect", JSValue.fromString("follow"))
        obj.set("referrer", JSValue.fromString("about:client"))
        obj.set("referrerPolicy", JSValue.fromString(""))
        obj.set("integrity", JSValue.fromString(""))
        obj.initProperty(
          "__requestData",
          JSValue.Native((url, method, headers, body, signal)),
          enumerable = false,
          writable = false,
          configurable = false
        )
        JSValue.Object(obj)
      },
      prototype = requestProto
    )
    BuiltinHelpers.initConstructor(requestCtor, length = 0)
    requestCtor
  }

  /** Resolve `fetch(input, init)` into a concrete request description. */
  private def parseRequest(
      input: JSValue,
      init: JSValue
  ): (String, String, HeadersData, Option[Array[Byte]], Option[JSValue]) = {
    val baseUrl: String =
      input match {
        case JSValue.JSStr(s) => s
        case JSValue.Object(obj)
            if obj.getOwnPropertyRaw("__urlRecord").isDefined =>
          BuiltinHelpers.getPropertyWithGetter(input, "href") match {
            case JSValue.JSStr(s) => s
            case other            => NodeHelpers.toStr(other)
          }
        case JSValue.Object(obj)
            if obj.getOwnPropertyRaw("__requestData").isDefined =>
          BuiltinHelpers.getPropertyWithGetter(input, "url") match {
            case JSValue.JSStr(s) => s
            case other            => NodeHelpers.toStr(other)
          }
        case _ =>
          illegalInvocation("Failed to parse URL from fetch input")
      }

    def initField(key: String): JSValue =
      BuiltinHelpers.getPropertyWithGetter(init, key)

    val requestData = input match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__requestData") match {
          case Some(JSValue.Native(t)) =>
            Some(
              t.asInstanceOf[
                (String, String, HeadersData, Option[Array[Byte]], Option[JSValue])
              ]
            )
          case _ => None
        }
      case _ => None
    }

    val method = {
      val fromInit = initField("method")
      fromInit match {
        case JSValue.JSStr(s) if s.nonEmpty => s.toUpperCase
        case _ =>
          requestData.map(_._2).getOrElse("GET")
      }
    }
    val headers = new HeadersData
    requestData.foreach(rd => fillHeaders(headers, headersObject(rd._3)))
    val headersInit = initField("headers")
    if headersInit != JSValue.Undefined then fillHeaders(headers, headersInit)
    val body = {
      val fromInit = bodyBytes(initField("body"))
      fromInit.orElse(requestData.flatMap(_._4))
    }
    val signal = {
      val fromInit = initField("signal")
      abortStateOf(fromInit) match {
        case Some(_) => Some(fromInit)
        case None    => requestData.flatMap(_._5)
      }
    }
    (baseUrl, method, headers, body, signal)
  }

  // =========================================================================
  // fetch
  // =========================================================================

  private def installFetch(): Unit = {
    val fetchFn = NativeFunction(
      name = "fetch",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val input = args.headOption.getOrElse(JSValue.Undefined)
        val init = args.lift(1).getOrElse(JSValue.Undefined)
        val (url, method, headers, body, signal) = parseRequest(input, init)

        val promise = JSValue.Promise()
        val promiseValue = BuiltinHelpers.wrapPromise(promise)

        // Abort handling: reject immediately when already aborted, otherwise
        // cancel the in-flight request when the signal fires.
        val signalState = signal.flatMap(abortStateOf)
        signalState match {
          case Some(state) if state.aborted =>
            PromiseBuiltins.rejectPromiseValue(promise, state.reason)
            return promiseValue
          case _ => ()
        }

        val requestBuilder =
          try HttpRequest.newBuilder(URI.create(url))
          catch {
            case e: Throwable =>
              PromiseBuiltins.rejectPromiseValue(promise, jvmErrorToTypeError(e, url))
              return promiseValue
          }

        if (method == "GET" || method == "HEAD") && body.isDefined then
          NodeHelpers.throwCoded(
            "TypeError",
            "Request with GET/HEAD method cannot have body",
            "ERR_INVALID_ARG_VALUE"
          )

        val publisher = body match {
          case Some(bytes) => HttpRequest.BodyPublishers.ofByteArray(bytes)
          case None        => HttpRequest.BodyPublishers.noBody()
        }
        requestBuilder.method(method, publisher)
        applyRequestHeaders(requestBuilder, headers)
        // Default content types mirroring Node's fetch.
        if !headers.has("content-type") && body.isDefined then {
          val contentType = input match {
            case JSValue.Object(obj)
                if obj.getOwnPropertyRaw("__uspData").isDefined =>
              "application/x-www-form-urlencoded;charset=UTF-8"
            case _ =>
              bodyBytes(initProperty(init, "body")) match {
                case Some(_) =>
                  BuiltinHelpers.getPropertyWithGetter(init, "body") match {
                    case JSValue.Object(obj)
                        if obj.getOwnPropertyRaw("__uspData").isDefined =>
                      "application/x-www-form-urlencoded;charset=UTF-8"
                    case _ => "text/plain;charset=UTF-8"
                  }
                case None => ""
              }
          }
          if contentType.nonEmpty then requestBuilder.header("content-type", contentType)
        }

        val request =
          try requestBuilder.build()
          catch {
            case e: Throwable =>
              PromiseBuiltins.rejectPromiseValue(promise, jvmErrorToTypeError(e, url))
              return promiseValue
          }

        loop.retain()
        val future = client.sendAsync(request, HttpResponse.BodyHandlers.ofByteArray())
        signalState.foreach { state =>
          state.handlers += (() => future.cancel(true))
        }
        future.whenComplete { (response, error) =>
          loop.post { () =>
            loop.release()
            if response != null then {
              val data = new ResponseData(
                status = response.statusCode(),
                statusText = httpStatusText(response.statusCode()),
                url = response.uri().toString,
                headers = responseHeaders(response),
                bytes = response.body(),
                bodyUsed = false
              )
              PromiseBuiltins.settlePromise(promise, makeResponse(data))
            } else {
              val reason =
                if signalState.exists(_.aborted) then signalState.get.reason
                else
                  error match {
                    case _: java.util.concurrent.CancellationException =>
                      abortError()
                    case _: HttpTimeoutException =>
                      errorWithName("TimeoutError", "The operation was aborted due to timeout")
                    case other => jvmErrorToTypeError(other, url)
                  }
              PromiseBuiltins.rejectPromiseValue(promise, reason)
            }
          }
        }
        promiseValue
      }
    )
    ctx.global.set("fetch", JSValue.Native(fetchFn))
  }

  /** Headers java.net.http manages itself; passing them to the builder throws. */
  private val restrictedHeaders =
    Set("connection", "content-length", "expect", "host", "upgrade")

  private def applyRequestHeaders(
      builder: HttpRequest.Builder,
      data: HeadersData
  ): Unit =
    data.map.foreach { case (key, values) =>
      if !restrictedHeaders.contains(key) then
        values.foreach(value => builder.header(key, value))
    }

  private def initProperty(obj: JSValue, key: String): JSValue =
    BuiltinHelpers.getPropertyWithGetter(obj, key)

  private def responseHeaders(response: HttpResponse[?]): HeadersData = {
    val data = new HeadersData
    response.headers().map().forEach { (name, values) =>
      values.forEach(value => data.append(name, value))
    }
    data
  }

  private def httpStatusText(code: Int): String =
    code match {
      case 200 => "OK"
      case 201 => "Created"
      case 204 => "No Content"
      case 301 => "Moved Permanently"
      case 302 => "Found"
      case 304 => "Not Modified"
      case 400 => "Bad Request"
      case 401 => "Unauthorized"
      case 403 => "Forbidden"
      case 404 => "Not Found"
      case 405 => "Method Not Allowed"
      case 500 => "Internal Server Error"
      case 502 => "Bad Gateway"
      case 503 => "Service Unavailable"
      case _   => ""
    }

  // =========================================================================
  // http/https client modules
  // =========================================================================

  private final class EmitterData {
    val listeners =
      mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[JSValue]]
  }

  private def emitterDataOf(value: JSValue): Option[EmitterData] =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__emitter") match {
          case Some(JSValue.Native(data: EmitterData)) => Some(data)
          case _                                       => None
        }
      case _ => None
    }

  private def makeEmitter(proto: JSObject, extra: (JSObject => Unit)): JSObject = {
    val obj = JSObject(prototype = proto)
    obj.initProperty(
      "__emitter",
      JSValue.Native(new EmitterData),
      enumerable = false,
      writable = false,
      configurable = false
    )
    extra(obj)
    obj
  }

  private def emit(
      target: JSValue,
      event: String,
      eventArgs: Array[JSValue]
  ): Boolean =
    emitterDataOf(target) match {
      case Some(data) =>
        val listeners =
          data.listeners.get(event).map(_.toArray).getOrElse(Array.empty[JSValue])
        if listeners.isEmpty then {
          if event == "error" then {
            val reason =
              eventArgs.headOption.getOrElse(
                errorWithName("Error", "Unhandled 'error' event")
              )
            throw new quickjs.runtime.JSException(reason)
          }
          false
        } else {
          listeners.foreach { listener =>
            BuiltinHelpers.callFunctionWithThis(listener, target, eventArgs)
          }
          true
        }
      case None => false
    }

  private def installEmitterMethod(
      proto: JSObject,
      name: String
  )(impl: (EmitterData, JSValue, Array[JSValue]) => JSValue): Unit =
    proto.defineProperty(
      name,
      JSValue.Native(
        NativeFunction(
          name = name,
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val thisValue = args.headOption.getOrElse(JSValue.Undefined)
            val data = emitterDataOf(thisValue).getOrElse(
              illegalInvocation("Value is not an event emitter")
            )
            impl(data, thisValue, args.drop(1))
          }
        )
      ),
      enumerable = false,
      writable = true,
      configurable = true
    )

  private def installEmitterMethods(proto: JSObject): Unit = {
    installEmitterMethod(proto, "on") { (data, _, args) =>
      val event = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      if !BuiltinHelpers.isCallable(listener) then
        illegalInvocation("listener must be a function")
      data.listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty) += listener
      JSValue.Undefined
    }
    installEmitterMethod(proto, "addListener") { (data, _, args) =>
      val event = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      if !BuiltinHelpers.isCallable(listener) then
        illegalInvocation("listener must be a function")
      data.listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty) += listener
      JSValue.Undefined
    }
    installEmitterMethod(proto, "once") { (data, _, args) =>
      val event = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      if !BuiltinHelpers.isCallable(listener) then
        illegalInvocation("listener must be a function")
      var storedValue: JSValue = JSValue.Undefined
      val wrapper = NativeFunction(
        name = "onceWrapper",
        length = 0,
        impl = (callArgs, callCtx) => {
          val bucket = data.listeners.get(event)
          bucket.foreach { buffer =>
            val index =
              buffer.indexWhere(l => NodeHelpers.sameValue(l, storedValue))
            if index >= 0 then buffer.remove(index)
          }
          BuiltinHelpers.callFunctionWithThis(
            listener,
            callArgs.headOption.getOrElse(JSValue.Undefined),
            callArgs.drop(1)
          )(using callCtx)
        }
      )
      storedValue = JSValue.Native(wrapper)
      data.listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty) += storedValue
      JSValue.Undefined
    }
    installEmitterMethod(proto, "removeListener") { (data, _, args) =>
      val event = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      data.listeners.get(event).foreach { buffer =>
        val index = buffer.indexWhere(l => NodeHelpers.sameValue(l, listener))
        if index >= 0 then buffer.remove(index)
      }
      JSValue.Undefined
    }
    installEmitterMethod(proto, "off") { (data, _, args) =>
      val event = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      data.listeners.get(event).foreach { buffer =>
        val index = buffer.indexWhere(l => NodeHelpers.sameValue(l, listener))
        if index >= 0 then buffer.remove(index)
      }
      JSValue.Undefined
    }
    installEmitterMethod(proto, "removeAllListeners") { (data, _, args) =>
      args.headOption match {
        case Some(JSValue.Undefined) | None => data.listeners.clear()
        case Some(event) => data.listeners.remove(NodeHelpers.toStr(event))
      }
      JSValue.Undefined
    }
    installEmitterMethod(proto, "emit") { (data, thisValue, args) =>
      val event = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      emit(thisValue, event, args.drop(1))
      JSValue.Bool(true)
    }
    installEmitterMethod(proto, "listenerCount") { (data, _, args) =>
      val event = NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))
      JSValue.fromInt(data.listeners.get(event).map(_.length).getOrElse(0))
    }
  }

  private var incomingMessageProto: JSObject = null
  private var clientRequestProto: JSObject = null
  private var serverResponseProto: JSObject = null

  private def makeIncomingMessage(
      statusCode: Int,
      statusMessage: String,
      headers: HeadersData,
      url: String
  ): (JSValue, JSObject) = {
    val obj = makeEmitter(
      incomingMessageProto,
      o => {
        o.set("statusCode", JSValue.fromInt(statusCode))
        o.set("statusMessage", JSValue.fromString(statusMessage))
        o.set("httpVersion", JSValue.fromString("1.1"))
        o.set("url", JSValue.fromString(url))
        o.set("complete", JSValue.Bool(false))
        o.set("headers", headersToPlainObject(headers))
        o.set("rawHeaders", JSValue.JSArrayVal(headersToRawArray(headers)))
        o.initProperty(
          "__encoding",
          JSValue.Undefined,
          enumerable = false,
          writable = true,
          configurable = true
        )
      }
    )
    incomingMessageProto.defineProperty("setEncoding", JSValue.Native(NativeFunction(
      name = "setEncoding",
      length = 1,
      impl = (args, _) => {
        val encoding = args.lift(1).map(NodeHelpers.toStr(_)).getOrElse("utf8")
        args.headOption.foreach {
          case JSValue.Object(obj) => obj.set("__encoding", JSValue.fromString(encoding))
          case _                   => ()
        }
        args.headOption.getOrElse(JSValue.Undefined)
      }
    )), enumerable = false, writable = true, configurable = true)
    (JSValue.Object(obj), obj)
  }

  private def makeClientRequest(
      uri: URI,
      method: String,
      headers: HeadersData,
      responseCallback: Option[JSValue]
  ): (JSValue, JSObject) = {
    val chunks = mutable.ArrayBuffer.empty[Array[Byte]]
    var sent = false
    var aborted = false
    var timeoutMs: Option[Long] = None

    def send(requestObj: JSObject, thisValue: JSValue): Unit =
      if !sent then {
        sent = true
        val builder = HttpRequest.newBuilder(uri)
        val publisher =
          if chunks.isEmpty then HttpRequest.BodyPublishers.noBody()
          else
            HttpRequest.BodyPublishers.ofByteArray(
              chunks.foldLeft(Array.emptyByteArray)(_ ++ _)
            )
        builder.method(method, publisher)
        applyRequestHeaders(builder, headers)
        timeoutMs.foreach(ms => builder.timeout(Duration.ofMillis(ms)))
        val future =
          try client.sendAsync(builder.build(), HttpResponse.BodyHandlers.ofInputStream())
          catch {
            case e: Throwable =>
              loop.post { () =>
                emit(thisValue, "error", Array(jvmErrorToTypeError(e, uri.toString)))
              }
              return
          }
        loop.retain()
        future.whenComplete { (response, error) =>
          // All JS object construction must happen on the loop thread.
          loop.post { () =>
            loop.release()
            if response == null then {
              val reason = error match {
                case _: java.util.concurrent.CancellationException =>
                  abortError()
                case other => jvmErrorToTypeError(other, uri.toString)
              }
              emit(thisValue, "error", Array(reason))
            } else {
              val headersData = responseHeaders(response)
              val (incoming, incomingObj) = makeIncomingMessage(
                response.statusCode(),
                httpStatusText(response.statusCode()),
                headersData,
                uri.getPath + Option(uri.getQuery).map("?" + _).getOrElse("")
              )
              emit(thisValue, "response", Array(incoming))
              loop.execute {
                val stream = response.body()
                val buffer = new Array[Byte](16384)
                try {
                  var read = stream.read(buffer)
                  while read >= 0 do {
                    if read > 0 then {
                      val chunk = java.util.Arrays.copyOf(buffer, read)
                      loop.post { () =>
                        incomingObj.getOwnPropertyRaw("__encoding") match {
                          case Some(JSValue.JSStr(enc)) =>
                            emit(
                              incoming,
                              "data",
                              Array(
                                JSValue.fromString(
                                  NodeEncodings.stringFromBytes(chunk, enc)
                                )
                              )
                            )
                          case _ =>
                            emit(incoming, "data", Array(bytesToBuffer(chunk)))
                        }
                      }
                    }
                    read = stream.read(buffer)
                  }
                  stream.close()
                  loop.post { () =>
                    incomingObj.set("complete", JSValue.Bool(true))
                    emit(incoming, "end", Array.empty)
                  }
                } catch {
                  case e: Throwable =>
                    loop.post { () =>
                      emit(incoming, "error", Array(jvmErrorToTypeError(e, uri.toString)))
                    }
                }
              }
            }
          }
        }
        if aborted then future.cancel(true)
      }

    val obj = makeEmitter(
      clientRequestProto,
      o => {
        o.set("method", JSValue.fromString(method))
        o.set("path", JSValue.fromString(uri.getPath))
        o.set("host", JSValue.fromString(uri.getHost))
        o.set("protocol", JSValue.fromString(uri.getScheme + ":"))
        o.set("aborted", JSValue.Bool(false))
      }
    )
    val thisValue = JSValue.Object(obj)

    obj.set(
      "write",
      JSValue.Native(
        NativeFunction(
          name = "write",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val chunk = args.lift(1).getOrElse(JSValue.Undefined)
            bodyBytes(chunk).foreach(bytes => chunks += bytes)
            JSValue.Bool(true)
          }
        )
      )
    )
    obj.set(
      "end",
      JSValue.Native(
        NativeFunction(
          name = "end",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val chunk = args.lift(1).getOrElse(JSValue.Undefined)
            bodyBytes(chunk).foreach(bytes => chunks += bytes)
            send(obj, thisValue)
            JSValue.Undefined
          }
        )
      )
    )
    obj.set(
      "setHeader",
      JSValue.Native(
        NativeFunction(
          name = "setHeader",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val name = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
            val value = args.lift(2).getOrElse(JSValue.Undefined)
            headers.set(name, NodeHelpers.toStr(value))
            JSValue.Undefined
          }
        )
      )
    )
    obj.set(
      "getHeader",
      JSValue.Native(
        NativeFunction(
          name = "getHeader",
          length = 1,
          impl = (args, callCtx) => {
            val name = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
            headers.get(name) match {
              case Some(v) => JSValue.fromString(v)
              case None    => JSValue.Undefined
            }
          }
        )
      )
    )
    obj.set(
      "removeHeader",
      JSValue.Native(
        NativeFunction(
          name = "removeHeader",
          length = 1,
          impl = (args, callCtx) => {
            headers.delete(NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined)))
            JSValue.Undefined
          }
        )
      )
    )
    obj.set(
      "getHeaderNames",
      JSValue.Native(
        NativeFunction(
          name = "getHeaderNames",
          length = 0,
          impl = (args, callCtx) => {
            val arr = JSArray.empty()
            headers.map.keys.foreach(k => arr.push(JSValue.fromString(k)))
            JSValue.JSArrayVal(arr)
          }
        )
      )
    )
    obj.set(
      "abort",
      JSValue.Native(
        NativeFunction(
          name = "abort",
          length = 0,
          impl = (args, callCtx) => {
            aborted = true
            obj.set("aborted", JSValue.Bool(true))
            send(obj, thisValue)
            JSValue.Undefined
          }
        )
      )
    )
    obj.set(
      "setTimeout",
      JSValue.Native(
        NativeFunction(
          name = "setTimeout",
          length = 2,
          impl = (args, callCtx) => {
            val ms =
              args.lift(1).map(NodeHelpers.toNumber(_).toLong).getOrElse(0L)
            timeoutMs = Some(ms)
            JSValue.Object(obj)
          }
        )
      )
    )
    obj.set(
      "flushHeaders",
      JSValue.Native(
        NativeFunction(
          name = "flushHeaders",
          length = 0,
          impl = (_, _) => JSValue.Undefined
        )
      )
    )
    (thisValue, obj)
  }

  private def requestFromArgs(
      args: Array[JSValue],
      autoEnd: Boolean
  ): JSValue = {
    val first = args.headOption.getOrElse(JSValue.Undefined)
    val (url, options, callback) = first match {
      case JSValue.JSStr(_) =>
        // http.get(url[, options][, callback])
        val second = args.lift(1).getOrElse(JSValue.Undefined)
        if BuiltinHelpers.isCallable(second) then
          (first, JSValue.Undefined, Some(second))
        else (first, second, args.lift(2))
      case JSValue.Object(obj) if obj.getOwnPropertyRaw("__urlRecord").isDefined =>
        val second = args.lift(1).getOrElse(JSValue.Undefined)
        if BuiltinHelpers.isCallable(second) then
          (first, JSValue.Undefined, Some(second))
        else (first, second, args.lift(2))
      case _ =>
        (JSValue.Undefined, first, args.lift(1))
    }
    val cb = callback.filter(BuiltinHelpers.isCallable)

    def optionField(key: String): JSValue =
      if url == JSValue.Undefined then
        BuiltinHelpers.getPropertyWithGetter(options, key)
      else JSValue.Undefined

    val urlString = url match {
      case JSValue.JSStr(s) => s
      case JSValue.Object(_) =>
        BuiltinHelpers.getPropertyWithGetter(url, "href") match {
          case JSValue.JSStr(s) => s
          case other            => NodeHelpers.toStr(other)
        }
      case _ =>
        val protocol =
          optionField("protocol") match {
            case JSValue.JSStr(s) if s.nonEmpty => s.stripSuffix(":")
            case _                              => "http"
          }
        val host =
          optionField("hostname") match {
            case JSValue.JSStr(s) if s.nonEmpty => s
            case _ =>
              optionField("host") match {
                case JSValue.JSStr(s) if s.nonEmpty => s.takeWhile(_ != ':')
                case _                              => "localhost"
              }
          }
        val port =
          optionField("port") match {
            case JSValue.Int32(n)   => n
            case JSValue.JSStr(s) if s.nonEmpty => s.toInt
            case JSValue.Float64(d) => d.toInt
            case _                  => if protocol == "https" then 443 else 80
          }
        val path =
          optionField("path") match {
            case JSValue.JSStr(s) if s.nonEmpty => s
            case _                              => "/"
          }
        s"$protocol://$host:$port$path"
    }

    val method =
      optionField("method") match {
        case JSValue.JSStr(s) if s.nonEmpty => s.toUpperCase
        case _                              => "GET"
      }
    val headers = new HeadersData
    fillHeaders(headers, optionField("headers"))
    optionField("auth") match {
      case JSValue.JSStr(auth) if auth.nonEmpty =>
        headers.set(
          "authorization",
          "Basic " + java.util.Base64.getEncoder.encodeToString(
            NodeEncodings.bytesFromString(auth, "utf8")
          )
        )
      case _ => ()
    }

    val uri = URI.create(urlString)
    val (requestValue, requestObj) =
      makeClientRequest(uri, method, headers, cb)
    cb.foreach { callback =>
      // Register the callback for 'response'.
      val data = emitterDataOf(requestValue).get
      data.listeners.getOrElseUpdate("response", mutable.ArrayBuffer.empty) += callback
    }
    if autoEnd then {
      val endFn = BuiltinHelpers.getPropertyWithGetter(requestValue, "end")
      BuiltinHelpers.callFunctionWithThis(endFn, requestValue, Array.empty)
    }
    requestValue
  }

  private def installHttpEmitterProtos(): Unit = {
    incomingMessageProto = JSObject(prototype = ctx.objectPrototype)
    installEmitterMethods(incomingMessageProto)
    clientRequestProto = JSObject(prototype = ctx.objectPrototype)
    installEmitterMethods(clientRequestProto)
    serverResponseProto = JSObject(prototype = ctx.objectPrototype)
    installEmitterMethods(serverResponseProto)
  }

  // =========================================================================
  // HTTP server
  // =========================================================================

  private val defaultStatusText: Map[Int, String] = Map(
    100 -> "Continue",
    101 -> "Switching Protocols",
    200 -> "OK",
    201 -> "Created",
    202 -> "Accepted",
    203 -> "Non-Authoritative Information",
    204 -> "No Content",
    205 -> "Reset Content",
    206 -> "Partial Content",
    300 -> "Multiple Choices",
    301 -> "Moved Permanently",
    302 -> "Found",
    303 -> "See Other",
    304 -> "Not Modified",
    307 -> "Temporary Redirect",
    308 -> "Permanent Redirect",
    400 -> "Bad Request",
    401 -> "Unauthorized",
    402 -> "Payment Required",
    403 -> "Forbidden",
    404 -> "Not Found",
    405 -> "Method Not Allowed",
    406 -> "Not Acceptable",
    408 -> "Request Timeout",
    409 -> "Conflict",
    410 -> "Gone",
    411 -> "Length Required",
    412 -> "Precondition Failed",
    413 -> "Payload Too Large",
    414 -> "URI Too Long",
    415 -> "Unsupported Media Type",
    416 -> "Range Not Satisfiable",
    417 -> "Expectation Failed",
    418 -> "I'm a Teapot",
    422 -> "Unprocessable Entity",
    425 -> "Too Early",
    426 -> "Upgrade Required",
    428 -> "Precondition Required",
    429 -> "Too Many Requests",
    431 -> "Request Header Fields Too Large",
    451 -> "Unavailable For Legal Reasons",
    500 -> "Internal Server Error",
    501 -> "Not Implemented",
    502 -> "Bad Gateway",
    503 -> "Service Unavailable",
    504 -> "Gateway Timeout",
    505 -> "HTTP Version Not Supported"
  )

  private final class HttpConnState {
    val buffer = mutable.ArrayBuffer.empty[Byte]
    var parsed = false
    var requestEnded = false
    var responseFinished = false
    var chunkedRequest = false
    var chunkRemaining = -1L
    var chunkStage = 0 // 0 = size line, 1 = data, 2 = trailer/end
    var bodyRemaining = 0L
    var hasBody = false
    var req: JSObject = null
    var res: JSObject = null
    var reqEmitter: JsEmitter = null
    var resEmitter: JsEmitter = null
    var socket: JSValue = null
    var server: JSValue = null
    var headersSent = false
    var useChunked = false
    var wroteBody = false
    var encoding: Option[String] = None
    var statusCode = 200
    var statusMessage = ""
    val headers = mutable.LinkedHashMap.empty[String, (String, JSValue)]
    val requestHeaders = mutable.LinkedHashMap.empty[String, JSValue]
    var onFinish: () => Unit = () => ()
  }

  private def indexOfHeaderEnd(buffer: mutable.ArrayBuffer[Byte]): Int = {
    var i = 0
    while i + 3 < buffer.length do {
      if buffer(i) == 13 && buffer(i + 1) == 10 && buffer(i + 2) == 13 &&
          buffer(i + 3) == 10
      then return i
      i += 1
    }
    -1
  }

  private def bytesOfChunk(value: JSValue): Array[Byte] =
    value match {
      case JSValue.JSStr(s) => s.getBytes(StandardCharsets.UTF_8)
      case other =>
        NodeBuffer.bytesOfValue(other).getOrElse(Array.emptyByteArray)
    }

  private def headerValueString(value: JSValue): String =
    value match {
      case JSValue.JSStr(s) => s
      case other            => BuiltinHelpers.toJSString(other)
    }

  /** Parse a request head and set up the req/res pair. */
  private def startHttpRequest(state: HttpConnState): Unit = {
    val sep = indexOfHeaderEnd(state.buffer)
    if sep < 0 then return
    val headBytes = state.buffer.slice(0, sep).toArray
    state.buffer.remove(0, sep + 4)
    val head = new String(headBytes, StandardCharsets.ISO_8859_1)
    val lines = head.split("\r\n")
    if lines.isEmpty then return
    val requestLine = lines(0).split(" ", 3)
    val method = if requestLine.length > 0 then requestLine(0) else "GET"
    val url = if requestLine.length > 1 then requestLine(1) else "/"
    val version = if requestLine.length > 2 then requestLine(2) else "HTTP/1.1"
    val headers = mutable.LinkedHashMap.empty[String, JSValue]
    val rawHeaders = mutable.ArrayBuffer.empty[String]
    var i = 1
    while i < lines.length do {
      val line = lines(i)
      val colon = line.indexOf(':')
      if colon > 0 then {
        val name = line.substring(0, colon).trim
        val value = line.substring(colon + 1).trim
        val key = name.toLowerCase
        rawHeaders += name
        rawHeaders += value
        headers.get(key) match {
          case Some(existing) if key == "set-cookie" =>
            existing match {
              case JSValue.JSArrayVal(arr) =>
                arr.push(JSValue.fromString(value))
              case other =>
                val arr = JSArray.empty()
                arr.push(other)
                arr.push(JSValue.fromString(value))
                headers(key) = JSValue.JSArrayVal(arr)
            }
          case Some(existing) =>
            headers(key) = JSValue.fromString(
              s"${headerValueString(existing)}, $value"
            )
          case None => headers(key) = JSValue.fromString(value)
        }
      }
      i += 1
    }

    state.requestHeaders ++= headers
    val reqEmitter = new JsEmitter
    val req = JSObject(prototype = incomingMessageProto)
    reqEmitter.install(req)
    val headersObj = JSObject(prototype = ctx.objectPrototype)
    headers.foreach { case (key, value) => headersObj.set(key, value) }
    val rawHeadersArray = JSArray.empty()
    rawHeaders.foreach(value => rawHeadersArray.push(JSValue.fromString(value)))
    req.set("method", JSValue.fromString(method))
    req.set("url", JSValue.fromString(url))
    req.set("httpVersion", JSValue.fromString("1.1"))
    req.set("httpVersionMajor", JSValue.fromInt(1))
    req.set("httpVersionMinor", JSValue.fromInt(1))
    req.set("headers", JSValue.Object(headersObj))
    req.set("rawHeaders", JSValue.JSArrayVal(rawHeadersArray))
    req.set("trailers", JSValue.Object(JSObject(prototype = null)))
    req.set("rawTrailers", JSValue.JSArrayVal(JSArray.empty()))
    req.set("complete", JSValue.Bool(false))
    req.set("aborted", JSValue.Bool(false))
    req.set("socket", state.socket)
    req.set("connection", state.socket)
    req.set(
      "setEncoding",
      JSValue.Native(
        NativeFunction(
          name = "setEncoding",
          length = 1,
          impl = (args, _) => {
            state.encoding = args.lastOption.map(NodeHelpers.toStr(_))
            JSValue.Object(req)
          }
        )
      )
    )
    req.set(
      "pause",
      JSValue.Native(
        NativeFunction("pause", (_, _) => JSValue.Object(req), length = 0)
      )
    )
    req.set(
      "resume",
      JSValue.Native(
        NativeFunction("resume", (_, _) => JSValue.Object(req), length = 0)
      )
    )
    req.set(
      "destroy",
      JSValue.Native(
        NativeFunction(
          name = "destroy",
          length = 1,
          impl = (_, _) => {
            destroySocket(state)
            JSValue.Object(req)
          }
        )
      )
    )

    val resEmitter = new JsEmitter
    val res = JSObject(prototype = serverResponseProto)
    resEmitter.install(res)
    res.set("statusCode", JSValue.fromInt(200))
    res.set("statusMessage", JSValue.fromString(""))
    res.set("sendDate", JSValue.Bool(true))
    res.set("socket", state.socket)
    res.set("connection", state.socket)
    res.set("req", JSValue.Object(req))

    def statusCodeOf(): Int =
      res.get("statusCode") match {
        case JSValue.Int32(n)   => n
        case JSValue.Float64(d) => d.toInt
        case _                  => 200
      }
    def statusMessageOf(): String = {
      val explicit = res.get("statusMessage") match {
        case JSValue.JSStr(s) => s
        case _                => ""
      }
      if explicit.nonEmpty then explicit
      else defaultStatusText.getOrElse(statusCodeOf(), "")
    }

    def headerString(): String = {
      val sb = new StringBuilder
      state.headers.foreach { case (_, (name, value)) =>
        value match {
          case JSValue.JSArrayVal(arr) =>
            var index = 0
            while index < arr.getLength do {
              sb.append(name)
                .append(": ")
                .append(headerValueString(arr.get(index)))
                .append("\r\n")
              index += 1
            }
          case single =>
            sb.append(name)
              .append(": ")
              .append(headerValueString(single))
              .append("\r\n")
        }
      }
      sb.toString
    }

    def writeRaw(bytes: Array[Byte]): Unit = {
      val writeFn = BuiltinHelpers.getPropertyWithGetter(state.socket, "write")
      if BuiltinHelpers.isCallable(writeFn) then
        BuiltinHelpers.callFunctionWithThis(
          writeFn,
          state.socket,
          Array(NodeBuffer.makeBuffer(bytes))
        )
    }

    def sendHeaders(): Unit =
      if !state.headersSent then {
        state.headersSent = true
        // Callers decide framing: `write`/`writeHead`/`flushHeaders` enable
        // chunked, `end(data)` sets Content-Length.
        if state.useChunked && !state.headers.contains("transfer-encoding") then
          state.headers("transfer-encoding") = ("Transfer-Encoding", JSValue.fromString("chunked"))
        if !state.headers.contains("date") && res.get("sendDate") == JSValue.Bool(true) then
          state.headers("date") = (
            "Date",
            JSValue.fromString(
              java.time.ZonedDateTime
                .now(java.time.ZoneOffset.UTC)
                .format(java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME)
            )
          )
        if !state.headers.contains("connection") then
          state.headers("connection") = ("Connection", JSValue.fromString("close"))
        val head =
          new StringBuilder()
            .append("HTTP/1.1 ")
            .append(statusCodeOf())
            .append(' ')
            .append(statusMessageOf())
            .append("\r\n")
            .append(headerString())
            .append("\r\n")
            .toString
        writeRaw(head.getBytes(StandardCharsets.ISO_8859_1))
      }

    def finishResponse(): Unit =
      if !state.responseFinished then {
        state.responseFinished = true
        resEmitter.emit("finish", Array.empty)
        val endFn = BuiltinHelpers.getPropertyWithGetter(state.socket, "end")
        if BuiltinHelpers.isCallable(endFn) then
          BuiltinHelpers.callFunctionWithThis(endFn, state.socket, Array.empty)
      }

    def sendChunk(bytes: Array[Byte]): Unit =
      if bytes.nonEmpty then {
        val prefix =
          Integer.toHexString(bytes.length) + "\r\n"
        writeRaw(prefix.getBytes(StandardCharsets.ISO_8859_1))
        writeRaw(bytes)
        writeRaw("\r\n".getBytes(StandardCharsets.ISO_8859_1))
      }

    def writeBody(bytes: Array[Byte], finalChunk: Boolean): Unit =
      if state.headersSent && state.useChunked then sendChunk(bytes)
      else if finalChunk then writeRaw(bytes)
      else writeRaw(bytes)

    // ---- res methods ------------------------------------------------------

    res.set(
      "setHeader",
      JSValue.Native(
        NativeFunction(
          name = "setHeader",
          length = 2,
          impl = (args, _) => {
            if state.headersSent then
              NodeHelpers.throwCoded(
                "Error",
                "Cannot set headers after they are sent to the client",
                "ERR_HTTP_HEADERS_SENT"
              )
            val rest = args.headOption match {
              case Some(JSValue.Object(obj)) if obj eq res => args.drop(1)
              case _                                        => args
            }
            val name = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            val value = rest.lift(1).getOrElse(JSValue.Undefined)
            if name.nonEmpty then state.headers(name.toLowerCase) = (name, value)
            JSValue.Object(res)
          }
        )
      )
    )
    res.set(
      "getHeader",
      JSValue.Native(
        NativeFunction(
          name = "getHeader",
          length = 1,
          impl = (args, _) => {
            val rest = args.headOption match {
              case Some(JSValue.Object(obj)) if obj eq res => args.drop(1)
              case _                                        => args
            }
            val name = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            state.headers.get(name.toLowerCase).map(_._2).getOrElse(JSValue.Undefined)
          }
        )
      )
    )
    res.set(
      "hasHeader",
      JSValue.Native(
        NativeFunction(
          name = "hasHeader",
          length = 1,
          impl = (args, _) => {
            val rest = args.headOption match {
              case Some(JSValue.Object(obj)) if obj eq res => args.drop(1)
              case _                                        => args
            }
            val name = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            JSValue.Bool(state.headers.contains(name.toLowerCase))
          }
        )
      )
    )
    res.set(
      "removeHeader",
      JSValue.Native(
        NativeFunction(
          name = "removeHeader",
          length = 1,
          impl = (args, _) => {
            val rest = args.headOption match {
              case Some(JSValue.Object(obj)) if obj eq res => args.drop(1)
              case _                                        => args
            }
            val name = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            state.headers.remove(name.toLowerCase)
            JSValue.Undefined
          }
        )
      )
    )
    res.set(
      "getHeaders",
      JSValue.Native(
        NativeFunction(
          name = "getHeaders",
          length = 0,
          impl = (_, _) => {
            val obj = JSObject(prototype = null)
            state.headers.foreach { case (key, (_, value)) => obj.set(key, value) }
            JSValue.Object(obj)
          }
        )
      )
    )
    res.set(
      "getHeaderNames",
      JSValue.Native(
        NativeFunction(
          name = "getHeaderNames",
          length = 0,
          impl = (_, _) => {
            val array = JSArray.empty()
            state.headers.keysIterator.foreach(key =>
              array.push(JSValue.fromString(key))
            )
            JSValue.JSArrayVal(array)
          }
        )
      )
    )
    res.set(
      "writeHead",
      JSValue.Native(
        NativeFunction(
          name = "writeHead",
          length = 3,
          impl = (args, _) => {
            if state.headersSent then
              NodeHelpers.throwCoded(
                "Error",
                "Cannot write headers after they are sent to the client",
                "ERR_HTTP_HEADERS_SENT"
              )
            val rest = args.headOption match {
              case Some(JSValue.Object(obj)) if obj eq res => args.drop(1)
              case _                                        => args
            }
            rest.headOption.foreach(value => res.set("statusCode", value))
            rest.lift(1) match {
              case Some(JSValue.JSStr(message)) =>
                res.set("statusMessage", JSValue.fromString(message))
                rest.lift(2).foreach(mergeHeaders(_, state))
              case Some(JSValue.Object(headers)) =>
                mergeHeaders(JSValue.Object(headers), state)
              case _ => ()
            }
            // `writeHead` sends immediately (chunked unless content-length set).
            state.useChunked = !state.headers.contains("content-length") &&
              !state.headers.contains("transfer-encoding")
            sendHeaders()
            JSValue.Object(res)
          }
        )
      )
    )
    res.set(
      "flushHeaders",
      JSValue.Native(
        NativeFunction(
          name = "flushHeaders",
          length = 0,
          impl = (_, _) => {
            state.useChunked = !state.headers.contains("content-length") &&
              !state.headers.contains("transfer-encoding")
            sendHeaders()
            JSValue.Undefined
          }
        )
      )
    )
    res.set(
      "write",
      JSValue.Native(
        NativeFunction(
          name = "write",
          length = 3,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = args.headOption match {
              case Some(JSValue.Object(obj)) if obj eq res => args.drop(1)
              case _                                        => args
            }
            if !state.responseFinished then {
              rest.headOption.foreach { chunk =>
                if !BuiltinHelpers.isCallable(chunk) then {
                  val bytes = bytesOfChunk(chunk)
                  if !state.headersSent then {
                    state.useChunked = !state.headers.contains("content-length") &&
                      !state.headers.contains("transfer-encoding")
                    sendHeaders()
                  }
                  state.wroteBody = true
                  writeBody(bytes, finalChunk = false)
                }
              }
              rest.reverseIterator.find(BuiltinHelpers.isCallable).foreach(cb =>
                BuiltinHelpers.callFunctionWithThis(cb, JSValue.Undefined, Array.empty)
              )
            }
            JSValue.Bool(true)
          }
        )
      )
    )
    res.set(
      "end",
      JSValue.Native(
        NativeFunction(
          name = "end",
          length = 3,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = args.headOption match {
              case Some(JSValue.Object(obj)) if obj eq res => args.drop(1)
              case _                                        => args
            }
            if !state.responseFinished then {
              val chunk = rest.find(v =>
                v != JSValue.Undefined && v != JSValue.Null &&
                  !BuiltinHelpers.isCallable(v)
              )
              if !state.headersSent then {
                val bytes = chunk.map(bytesOfChunk).getOrElse(Array.emptyByteArray)
                state.useChunked = false
                if !state.headers.contains("content-length") &&
                    !state.headers.contains("transfer-encoding")
                then
                  state.headers("content-length") = (
                    "Content-Length",
                    JSValue.fromInt(bytes.length)
                  )
                sendHeaders()
                if bytes.nonEmpty then writeRaw(bytes)
              } else {
                if state.useChunked then {
                  chunk.foreach(value => sendChunk(bytesOfChunk(value)))
                  writeRaw("0\r\n\r\n".getBytes(StandardCharsets.ISO_8859_1))
                } else chunk.foreach(value => writeRaw(bytesOfChunk(value)))
              }
              rest.reverseIterator.find(BuiltinHelpers.isCallable).foreach(cb =>
                BuiltinHelpers.callFunctionWithThis(cb, JSValue.Undefined, Array.empty)
              )
              finishResponse()
            }
            JSValue.Object(res)
          }
        )
      )
    )
    res.set(
      "setTimeout",
      JSValue.Native(
        NativeFunction(
          name = "setTimeout",
          length = 2,
          impl = (_, _) => JSValue.Object(res)
        )
      )
    )
    res.set(
      "destroy",
      JSValue.Native(
        NativeFunction(
          name = "destroy",
          length = 1,
          impl = (_, _) => {
            state.responseFinished = true
            destroySocket(state)
            JSValue.Undefined
          }
        )
      )
    )
    res.defineAccessorProperty(
      "headersSent",
      getter = Some(
        JSValue.Native(
          NativeFunction(
            name = "get headersSent",
            length = 0,
            impl = (_, _) => JSValue.Bool(state.headersSent)
          )
        )
      ),
      setter = None,
      enumerable = false,
      configurable = true
    )
    res.defineAccessorProperty(
      "writableEnded",
      getter = Some(
        JSValue.Native(
          NativeFunction(
            name = "get writableEnded",
            length = 0,
            impl = (_, _) => JSValue.Bool(state.responseFinished)
          )
        )
      ),
      setter = None,
      enumerable = false,
      configurable = true
    )
    res.defineAccessorProperty(
      "finished",
      getter = Some(
        JSValue.Native(
          NativeFunction(
            name = "get finished",
            length = 0,
            impl = (_, _) => JSValue.Bool(state.responseFinished)
          )
        )
      ),
      setter = None,
      enumerable = false,
      configurable = true
    )

    state.req = req
    state.res = res
    state.reqEmitter = reqEmitter
    state.resEmitter = resEmitter

    // Emit the request to the server listeners.
    val emitFn = BuiltinHelpers.getPropertyWithGetter(state.server, "emit")
    if BuiltinHelpers.isCallable(emitFn) then
      BuiltinHelpers.callFunctionWithThis(
        emitFn,
        state.server,
        Array(JSValue.fromString("request"), JSValue.Object(req), JSValue.Object(res))
      )

    // Request body framing.
    state.requestHeaders.get("transfer-encoding") match {
      case Some(value)
          if headerValueString(value).toLowerCase.contains("chunked") =>
        state.chunkedRequest = true
        state.chunkStage = 0
      case _ =>
        state.requestHeaders.get("content-length") match {
          case Some(value) =>
            val length =
              try headerValueString(value).trim.toLong
              catch case _: Throwable => 0L
            state.bodyRemaining = length
            state.hasBody = length > 0
          case None => state.hasBody = false
        }
    }
    if !state.chunkedRequest && !state.hasBody then endRequestBody(state)
  }

  private def destroySocket(state: HttpConnState): Unit = {
    val destroyFn = BuiltinHelpers.getPropertyWithGetter(state.socket, "destroy")
    if BuiltinHelpers.isCallable(destroyFn) then
      BuiltinHelpers.callFunctionWithThis(destroyFn, state.socket, Array.empty)
  }

  private def endRequestBody(state: HttpConnState): Unit =
    if !state.requestEnded then {
      state.requestEnded = true
      if state.req != null then {
        state.req.set("complete", JSValue.Bool(true))
        state.reqEmitter.emit("end", Array.empty)
      }
    }

  private def feedRequestBody(state: HttpConnState, bytes: Array[Byte]): Unit = {
    if state.requestEnded || state.req == null then return
    if state.chunkedRequest then {
      state.buffer ++= bytes
      var continue = true
      while continue && !state.requestEnded do {
        state.chunkStage match {
          case 0 =>
            val lineEnd = {
              var i = 0
              var found = -1
              while found < 0 && i + 1 < state.buffer.length do {
                if state.buffer(i) == 13 && state.buffer(i + 1) == 10 then found = i
                i += 1
              }
              found
            }
            if lineEnd >= 0 then {
              val line =
                new String(state.buffer.slice(0, lineEnd).toArray, StandardCharsets.ISO_8859_1)
              state.buffer.remove(0, lineEnd + 2)
              val size = line.split(";").headOption.getOrElse("").trim
              val parsed =
                try java.lang.Long.parseLong(size, 16)
                catch case _: Throwable => 0L
              if parsed == 0 then {
                state.chunkStage = 2
              } else {
                state.chunkRemaining = parsed
                state.chunkStage = 1
              }
            } else continue = false
          case 1 =>
            if state.buffer.length >= state.chunkRemaining then {
              val chunk = state.buffer.take(state.chunkRemaining.toInt).toArray
              state.buffer.remove(0, state.chunkRemaining.toInt)
              // Trailing CRLF
              if state.buffer.length >= 2 then state.buffer.remove(0, 2)
              emitReqData(state, chunk)
              state.chunkStage = 0
            } else continue = false
          case 2 =>
            val lineEnd = {
              var i = 0
              var found = -1
              while found < 0 && i + 1 < state.buffer.length do {
                if state.buffer(i) == 13 && state.buffer(i + 1) == 10 then found = i
                i += 1
              }
              found
            }
            if lineEnd >= 0 then {
              state.buffer.remove(0, lineEnd + 2)
              state.chunkStage = 0
              endRequestBody(state)
            } else continue = false
          case _ => continue = false
        }
      }
    } else if state.hasBody then {
      val take = math.min(state.bodyRemaining, bytes.length.toLong).toInt
      if take > 0 then {
        emitReqData(state, bytes.take(take))
        state.bodyRemaining -= take
      }
      if state.bodyRemaining <= 0 then endRequestBody(state)
    }
  }

  private def emitReqData(state: HttpConnState, bytes: Array[Byte]): Unit = {
    if state.reqEmitter != null then {
      state.encoding match {
        case Some(enc) =>
          state.reqEmitter.emit(
            "data",
            Array(JSValue.fromString(NodeEncodings.stringFromBytes(bytes, enc)))
          )
        case None =>
          state.reqEmitter.emit("data", Array(NodeBuffer.makeBuffer(bytes)))
      }
    }
  }

  private def mergeHeaders(
      headers: JSValue,
      state: HttpConnState
  )(using ctx: JSContext): Unit =
    headers match {
      case JSValue.Object(obj) =>
        obj.getAllOwnPropertyKeys().foreach {
          case name: String =>
            val value = BuiltinHelpers.getPropertyWithGetter(headers, name)
            if name.nonEmpty then state.headers(name.toLowerCase) = (name, value)
          case _ => ()
        }
      case _ => ()
    }

  private def handleConnection(server: JSValue, socket: JSValue): Unit = {
    val state = new HttpConnState
    state.socket = socket
    state.server = server

    def socketOn(event: String, handler: JSValue): Unit = {
      val on = BuiltinHelpers.getPropertyWithGetter(socket, "on")
      if BuiltinHelpers.isCallable(on) then
        BuiltinHelpers.callFunctionWithThis(
          on,
          socket,
          Array(JSValue.fromString(event), handler)
        )
    }

    val dataHandler = NativeFunction(
      name = "ondata",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        args.lastOption.filter(_ != JSValue.Undefined).foreach { chunk =>
          val bytes = bytesOfChunk(chunk)
          if !state.parsed then {
            state.buffer ++= bytes
            val sep = indexOfHeaderEnd(state.buffer)
            if sep >= 0 then {
              state.parsed = true
              startHttpRequest(state)
              // Feed any body bytes already buffered.
              if state.buffer.nonEmpty && !state.requestEnded then {
                val body = state.buffer.toArray
                state.buffer.clear()
                feedRequestBody(state, body)
              }
            }
          } else feedRequestBody(state, bytes)
        }
        JSValue.Undefined
      }
    )
    val endHandler = NativeFunction(
      name = "onend",
      length = 0,
      impl = (_, callCtx) => {
        given JSContext = callCtx
        endRequestBody(state)
        if !state.responseFinished then {
          if !state.headersSent then {
            state.statusCode = 400
            state.useChunked = false
            state.headers("content-length") = ("Content-Length", JSValue.fromInt(0))
          }
          state.resEmitter.emit("close", Array.empty)
        }
        JSValue.Undefined
      }
    )
    val errorHandler = NativeFunction(
      name = "onerror",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        if state.req != null then {
          state.req.set("aborted", JSValue.Bool(true))
          state.reqEmitter.emit("aborted", Array.empty)
          state.reqEmitter.emit("error", Array(args.lastOption.getOrElse(JSValue.Undefined)))
        }
        JSValue.Undefined
      }
    )
    socketOn("data", JSValue.Native(dataHandler))
    socketOn("end", JSValue.Native(endHandler))
    socketOn("error", JSValue.Native(errorHandler))
  }

  private def makeHttpServer(
      netModule: JSValue,
      requestListener: Option[JSValue]
  ): JSValue = {
    val createServerFn =
      BuiltinHelpers.getPropertyWithGetter(netModule, "createServer")
    val server = BuiltinHelpers.callFunctionWithThis(
      createServerFn,
      netModule,
      Array.empty[JSValue]
    )
    def callMethod(
        target: JSValue,
        name: String,
        args: Array[JSValue]
    ): JSValue = {
      val fn = BuiltinHelpers.getPropertyWithGetter(target, name)
      if BuiltinHelpers.isCallable(fn) then
        BuiltinHelpers.callFunctionWithThis(fn, target, args)
      else JSValue.Undefined
    }
    requestListener.foreach(listener =>
      callMethod(server, "on", Array(JSValue.fromString("request"), listener))
    )
    val connectionHandler = NativeFunction(
      name = "onconnection",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        args.lastOption.foreach(socket => handleConnection(server, socket))
        JSValue.Undefined
      }
    )
    callMethod(
      server,
      "on",
      Array(JSValue.fromString("connection"), JSValue.Native(connectionHandler))
    )
    server match {
      case JSValue.Object(obj) =>
        obj.set("timeout", JSValue.fromInt(0))
        obj.set("keepAliveTimeout", JSValue.fromInt(5000))
        obj.set("headersTimeout", JSValue.fromInt(60000))
        obj.set("requestTimeout", JSValue.fromInt(300000))
        obj.set("maxHeadersCount", JSValue.fromInt(2000))
        obj.set(
          "closeAllConnections",
          JSValue.Native(
            NativeFunction("closeAllConnections", (_, _) => JSValue.Undefined, length = 0)
          )
        )
        obj.set(
          "closeIdleConnections",
          JSValue.Native(
            NativeFunction("closeIdleConnections", (_, _) => JSValue.Undefined, length = 0)
          )
        )
        obj.set(
          "setTimeout",
          JSValue.Native(
            NativeFunction(
              name = "setTimeout",
              length = 2,
              impl = (_, _) => JSValue.Object(obj)
            )
          )
        )
      case _ => ()
    }
    server
  }

  // =========================================================================
  // Installation
  // =========================================================================

  def install(): Unit = {
    val headersCtor = installHeaders()
    val responseCtor = installResponse()
    val requestCtor = installRequest()
    installAbort()
    installHttpEmitterProtos()
    installFetch()
    ctx.global.set("Headers", JSValue.Native(headersCtor))
    ctx.global.set("Response", JSValue.Native(responseCtor))
    ctx.global.set("Request", JSValue.Native(requestCtor))
  }

  def createHttpModule(secure: Boolean, netModule: JSValue): JSValue = {
    val http = JSObject(prototype = ctx.objectPrototype)
    val defaultProtocol = if secure then "https" else "http"

    val incomingMessageCtor = NativeConstructor(
      name = "IncomingMessage",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        callCtx.throwTypeError(
          "Class constructor IncomingMessage cannot be invoked without 'new'"
        )
      },
      constructImpl = (_, callCtx) => {
        given JSContext = callCtx
        JSValue.Object(JSObject(prototype = incomingMessageProto))
      },
      prototype = incomingMessageProto
    )
    BuiltinHelpers.initConstructor(incomingMessageCtor, length = 1)
    http.set("IncomingMessage", JSValue.Native(incomingMessageCtor))
    val serverResponseCtor = NativeConstructor(
      name = "ServerResponse",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        callCtx.throwTypeError(
          "Class constructor ServerResponse cannot be invoked without 'new'"
        )
      },
      constructImpl = (_, callCtx) => {
        given JSContext = callCtx
        JSValue.Object(JSObject(prototype = serverResponseProto))
      },
      prototype = serverResponseProto
    )
    BuiltinHelpers.initConstructor(serverResponseCtor, length = 1)
    http.set("ServerResponse", JSValue.Native(serverResponseCtor))
    val outgoingMessageCtor = NativeConstructor(
      name = "OutgoingMessage",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        callCtx.throwTypeError(
          "Class constructor OutgoingMessage cannot be invoked without 'new'"
        )
      },
      constructImpl = (_, callCtx) => {
        given JSContext = callCtx
        JSValue.Object(JSObject(prototype = serverResponseProto))
      },
      prototype = serverResponseProto
    )
    BuiltinHelpers.initConstructor(outgoingMessageCtor, length = 1)
    http.set("OutgoingMessage", JSValue.Native(outgoingMessageCtor))
    val serverCtor = NativeConstructor(
      name = "Server",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        callCtx.throwTypeError(
          "Class constructor Server cannot be invoked without 'new'"
        )
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = args.headOption match {
          case Some(JSValue.Object(obj)) if obj eq http => args.drop(1)
          case _                                         => args
        }
        val listener = rest.find(BuiltinHelpers.isCallable)
        makeHttpServer(netModule, listener)
      },
      prototype = JSObject(prototype = ctx.objectPrototype)
    )
    BuiltinHelpers.initConstructor(serverCtor, length = 1)
    http.set("Server", JSValue.Native(serverCtor))
    http.set(
      "createServer",
      JSValue.Native(
        NativeFunction(
          name = "createServer",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = NodeHelpers.stripReceiver(args, http)
            val listener = rest.find(BuiltinHelpers.isCallable)
            makeHttpServer(netModule, listener)
          }
        )
      )
    )
    http.set(
      "request",
      JSValue.Native(
        NativeFunction(
          name = "request",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            requestFromArgs(NodeHelpers.stripReceiver(args, http), autoEnd = false)
          }
        )
      )
    )
    http.set(
      "get",
      JSValue.Native(
        NativeFunction(
          name = "get",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            requestFromArgs(NodeHelpers.stripReceiver(args, http), autoEnd = true)
          }
        )
      )
    )
    val agent = JSObject(prototype = ctx.objectPrototype)
    agent.set("maxSockets", JSValue.fromInt(Int.MaxValue))
    agent.set("keepAlive", JSValue.Bool(false))
    agent.set("protocol", JSValue.fromString(defaultProtocol + ":"))
    agent.set("destroy", JSValue.Native(NativeFunction("destroy", (_, _) => JSValue.Undefined, length = 0)))
    http.set("globalAgent", JSValue.Object(agent))
    http.set(
      "Agent",
      JSValue.Native(
        NativeConstructor(
          name = "Agent",
          callImpl = (_, _) => JSValue.Object(agent),
          constructImpl = (_, _) => JSValue.Object(agent),
          prototype = ctx.objectPrototype
        )
      )
    )
    val statusCodes = JSObject(prototype = null)
    Seq(
      200 -> "OK",
      201 -> "Created",
      204 -> "No Content",
      301 -> "Moved Permanently",
      302 -> "Found",
      304 -> "Not Modified",
      400 -> "Bad Request",
      401 -> "Unauthorized",
      403 -> "Forbidden",
      404 -> "Not Found",
      405 -> "Method Not Allowed",
      500 -> "Internal Server Error",
      502 -> "Bad Gateway",
      503 -> "Service Unavailable"
    ).foreach { case (code, text) =>
      statusCodes.set(code.toString, JSValue.fromString(text))
    }
    http.set("STATUS_CODES", JSValue.Object(statusCodes))
    val methods = JSArray.empty()
    Seq("GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS").foreach(m =>
      methods.push(JSValue.fromString(m))
    )
    http.set("METHODS", JSValue.JSArrayVal(methods))
    JSValue.Object(http)
  }
}
