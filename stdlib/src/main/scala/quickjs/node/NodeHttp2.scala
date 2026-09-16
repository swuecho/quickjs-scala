package quickjs.node

import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.value.{JSValue, NativeFunction}
import quickjs.objmodel.JSObject

/** Minimal `node:http2` module. Normal HTTP/1 requests never touch it; the
  * API surface exists so packages that `require('http2')` at load (axios)
  * work, while actually connecting reports that HTTP/2 is unsupported. */
object NodeHttp2 {

  def create()(using ctx: JSContext): JSValue = {
    val http2 = JSObject(prototype = ctx.objectPrototype)

    def unsupported(name: String): NativeFunction =
      NativeFunction(
        name = name,
        length = 0,
        impl = (_, callCtx) =>
          given JSContext = callCtx
          NodeHelpers.throwCoded(
            "Error",
            "HTTP/2 is not supported by this runtime",
            "ERR_HTTP2_UNSUPPORTED"
          )
      )

    http2.set("connect", JSValue.Native(unsupported("connect")))
    http2.set("createServer", JSValue.Native(unsupported("createServer")))

    val constants = JSObject(prototype = ctx.objectPrototype)
    Seq(
      "NGHTTP2_SESSION_SERVER" -> 0,
      "NGHTTP2_SESSION_CLIENT" -> 1,
      "NGHTTP2_STREAM_STATE_IDLE" -> 1,
      "NGHTTP2_STREAM_STATE_OPEN" -> 2,
      "NGHTTP2_STREAM_STATE_RESERVED_LOCAL" -> 3,
      "NGHTTP2_STREAM_STATE_RESERVED_REMOTE" -> 4,
      "NGHTTP2_STREAM_STATE_HALF_CLOSED_LOCAL" -> 5,
      "NGHTTP2_STREAM_STATE_HALF_CLOSED_REMOTE" -> 6,
      "NGHTTP2_STREAM_STATE_CLOSED" -> 7,
      "NGHTTP2_NO_ERROR" -> 0,
      "NGHTTP2_PROTOCOL_ERROR" -> 1,
      "NGHTTP2_INTERNAL_ERROR" -> 2,
      "NGHTTP2_FLOW_CONTROL_ERROR" -> 3,
      "NGHTTP2_SETTINGS_TIMEOUT" -> 4,
      "NGHTTP2_STREAM_CLOSED" -> 5,
      "NGHTTP2_FRAME_SIZE_ERROR" -> 6,
      "NGHTTP2_REFUSED_STREAM" -> 7,
      "NGHTTP2_CANCEL" -> 8,
      "NGHTTP2_COMPRESSION_ERROR" -> 9,
      "NGHTTP2_CONNECT_ERROR" -> 10,
      "NGHTTP2_ENHANCE_YOUR_CALM" -> 11,
      "NGHTTP2_INADEQUATE_SECURITY" -> 12,
      "NGHTTP2_HTTP_1_1_REQUIRED" -> 13,
      "NGHTTP2_DEFAULT_WEIGHT" -> 16
    ).foreach((name, value) => constants.set(name, JSValue.fromInt(value)))
    // The header-name/method constants are strings.
    Seq(
      "HTTP2_HEADER_STATUS" -> ":status",
      "HTTP2_HEADER_METHOD" -> ":method",
      "HTTP2_HEADER_PATH" -> ":path",
      "HTTP2_HEADER_SCHEME" -> ":scheme",
      "HTTP2_HEADER_AUTHORITY" -> ":authority",
      "HTTP2_HEADER_CONTENT_TYPE" -> "content-type",
      "HTTP2_HEADER_CONTENT_LENGTH" -> "content-length",
      "HTTP2_HEADER_ACCEPT" -> "accept",
      "HTTP2_HEADER_USER_AGENT" -> "user-agent",
      "HTTP2_METHOD_GET" -> "GET",
      "HTTP2_METHOD_POST" -> "POST"
    ).foreach((name, value) => constants.set(name, JSValue.fromString(value)))
    http2.set("constants", JSValue.Object(constants))

    http2.set(
      "getDefaultSettings",
      JSValue.Native(NativeFunction(
        name = "getDefaultSettings",
        length = 0,
        impl = (_, callCtx) =>
          given JSContext = callCtx
          JSValue.Object(JSObject(prototype = callCtx.objectPrototype))
      ))
    )
    http2.set(
      "getPackedSettings",
      JSValue.Native(NativeFunction(
        name = "getPackedSettings",
        length = 1,
        impl = (_, callCtx) =>
          given JSContext = callCtx
          NodeBuffer.makeBuffer(Array.emptyByteArray)
      ))
    )
    http2.set(
      "getUnpackedSettings",
      JSValue.Native(NativeFunction(
        name = "getUnpackedSettings",
        length = 1,
        impl = (_, callCtx) =>
          given JSContext = callCtx
          JSValue.Object(JSObject(prototype = callCtx.objectPrototype))
      ))
    )
    http2.set("sensitiveHeaders", JSValue.fromString(""))
    Seq(
      "Http2Session",
      "Http2Stream",
      "ServerHttp2Session",
      "ClientHttp2Session",
      "Http2ServerRequest",
      "Http2ServerResponse"
    ).foreach(name => http2.set(name, JSValue.Native(unsupported(name))))

    JSValue.Object(http2)
  }
}
