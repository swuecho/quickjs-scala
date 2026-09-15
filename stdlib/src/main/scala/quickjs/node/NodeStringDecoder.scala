package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.JSObject

/** Node's `string_decoder` module (`StringDecoder`). */
object NodeStringDecoder {

  private final class DecoderState(var encoding: String) {
    var pending: Array[Byte] = Array.emptyByteArray
  }

  def create()(using ctx: JSContext): JSValue = {
    val proto = JSObject(prototype = ctx.objectPrototype)

    val ctor = NativeConstructor(
      name = "StringDecoder",
      callImpl = (args, callCtx) => {
        given JSContext = callCtx
        NodeHelpers.throwCoded(
          "TypeError",
          "Class constructor StringDecoder cannot be invoked without 'new'",
          "ERR_CONSTRUCT_CALL_REQUIRED"
        )
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val encoding =
          args.headOption.map(v => NodeHelpers.toStr(v)).getOrElse("utf8")
        val obj = JSObject(prototype = proto)
        obj.initProperty(
          "__decoderState",
          JSValue.Native(new DecoderState(encoding)),
          enumerable = false,
          writable = false,
          configurable = false
        )
        obj.set("encoding", JSValue.fromString(encoding))
        JSValue.Object(obj)
      },
      prototype = proto
    )
    BuiltinHelpers.initConstructor(ctor, length = 1)

    def stateOf(value: JSValue): Option[DecoderState] =
      value match {
        case JSValue.Object(obj) =>
          obj.getOwnPropertyRaw("__decoderState") match {
            case Some(JSValue.Native(s: DecoderState)) => Some(s)
            case _                                     => None
          }
        case _ => None
      }

    // Keep only whole code points; the tail is returned on the next write.
    def decode(state: DecoderState, bytes: Array[Byte]): String = {
      val all =
        if state.pending.isEmpty then bytes
        else {
          val joined = new Array[Byte](state.pending.length + bytes.length)
          System.arraycopy(state.pending, 0, joined, 0, state.pending.length)
          System.arraycopy(bytes, 0, joined, state.pending.length, bytes.length)
          joined
        }
      if state.encoding != "utf8" && state.encoding != "utf-8" then {
        state.pending = Array.emptyByteArray
        return NodeEncodings.stringFromBytes(all, state.encoding)
      }
      // Find the start of a possibly-incomplete trailing sequence.
      var end = all.length
      var start = all.length
      while start > 0 && (all(start - 1) & 0xc0) == 0x80 do start -= 1
      if start > 0 then {
        val lead = all(start - 1) & 0xff
        val expected =
          if (lead & 0x80) == 0 then 0
          else if (lead & 0xe0) == 0xc0 then 2
          else if (lead & 0xf0) == 0xe0 then 3
          else if (lead & 0xf8) == 0xf0 then 4
          else 1
        val available = all.length - (start - 1)
        if expected > available then end = start - 1
      }
      state.pending = java.util.Arrays.copyOfRange(all, end, all.length)
      NodeEncodings.stringFromBytes(
        java.util.Arrays.copyOfRange(all, 0, end),
        "utf8"
      )
    }

    proto.defineProperty(
      "write",
      JSValue.Native(
        NativeFunction(
          name = "write",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val thisValue = args.headOption.getOrElse(JSValue.Undefined)
            val chunk = args.lift(1).getOrElse(JSValue.Undefined)
            stateOf(thisValue) match {
              case Some(state) =>
                val bytes = NodeBuffer
                  .bytesOfValue(chunk)
                  .getOrElse(
                    NodeEncodings.bytesFromString(NodeHelpers.toStr(chunk), "utf8")
                  )
                JSValue.fromString(decode(state, bytes))
              case None => JSValue.fromString("")
            }
          }
        )
      ),
      enumerable = false,
      writable = true,
      configurable = true
    )
    proto.defineProperty(
      "end",
      JSValue.Native(
        NativeFunction(
          name = "end",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val thisValue = args.headOption.getOrElse(JSValue.Undefined)
            val chunk = args.lift(1).getOrElse(JSValue.Undefined)
            stateOf(thisValue) match {
              case Some(state) =>
                val text =
                  if chunk == JSValue.Undefined then ""
                  else {
                    val bytes = NodeBuffer
                      .bytesOfValue(chunk)
                      .getOrElse(
                        NodeEncodings.bytesFromString(NodeHelpers.toStr(chunk), "utf8")
                      )
                    // Flush any incomplete tail as replacement characters.
                    NodeEncodings.stringFromBytes(bytes, "utf8")
                  }
                state.pending = Array.emptyByteArray
                JSValue.fromString(text)
              case None => JSValue.fromString("")
            }
          }
        )
      ),
      enumerable = false,
      writable = true,
      configurable = true
    )

    val module = JSObject(prototype = ctx.objectPrototype)
    module.set("StringDecoder", JSValue.Native(ctor))
    JSValue.Object(module)
  }
}
