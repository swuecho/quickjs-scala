package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.runtime.builtins.TypedArrayBuiltins.TypedArrayView
import quickjs.objmodel.JSObject

import java.security.{MessageDigest, SecureRandom}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/** Node's `crypto` module (hashing, HMAC, random values) and the global
  * `crypto` WebCrypto subset.
  */
object NodeCrypto {

  private val random = new SecureRandom()

  private final class HashState(val algorithm: String, var digest: MessageDigest)
  private final class HmacState(val algorithm: String, var mac: Mac)

  private def digestName(algorithm: String): String =
    algorithm.toLowerCase match {
      case "md5"    => "MD5"
      case "sha1"   => "SHA-1"
      case "sha224" => "SHA-224"
      case "sha256" => "SHA-256"
      case "sha384" => "SHA-384"
      case "sha512" => "SHA-512"
      case "sha3-256" => "SHA3-256"
      case "sha3-512" => "SHA3-512"
      case other    => other
    }

  private def macName(algorithm: String): String =
    algorithm.toLowerCase match {
      case "md5"    => "HmacMD5"
      case "sha1"   => "HmacSHA1"
      case "sha224" => "HmacSHA224"
      case "sha256" => "HmacSHA256"
      case "sha384" => "HmacSHA384"
      case "sha512" => "HmacSHA512"
      case other    => other
    }

  private def bytesOf(value: JSValue, encoding: String)(using
      ctx: JSContext
  ): Array[Byte] =
    value match {
      case JSValue.JSStr(s) => NodeEncodings.bytesFromString(s, encoding)
      case other =>
        NodeBuffer.bytesOfValue(other).getOrElse(
          NodeEncodings.bytesFromString(BuiltinHelpers.toJSString(other), encoding)
        )
    }

  def create()(using ctx: JSContext): JSValue = {
    val crypto = JSObject(prototype = ctx.objectPrototype)

    def strip(args: Array[JSValue]): Array[JSValue] =
      NodeHelpers.stripReceiver(args, crypto)

    def method(name: String, arity: Int)(
        impl: (Array[JSValue], JSContext) => JSValue
    ): NativeFunction = {
      val fn = NativeFunction(
        name = name,
        length = arity,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          impl(strip(args), callCtx)
        }
      )
      crypto.set(name, JSValue.Native(fn))
      fn
    }

    // ---- random ------------------------------------------------------------

    method("randomBytes", 2)((args, callCtx) => {
      given JSContext = callCtx
      val size = args.headOption.map(v => NodeHelpers.toNumber(v).toInt).getOrElse(0)
      if size < 0 then
        NodeHelpers.throwCoded(
          "RangeError",
          "The value of \"size\" is out of range",
          "ERR_OUT_OF_RANGE"
        )
      val bytes = new Array[Byte](size)
      random.nextBytes(bytes)
      val buffer = NodeBuffer.makeBuffer(bytes)
      args.lift(1) match {
        case Some(callback) if BuiltinHelpers.isCallable(callback) =>
          callCtx.queueMicrotask { () =>
            BuiltinHelpers.callFunctionWithThis(
              callback,
              JSValue.Undefined,
              Array(JSValue.Null, buffer)
            )
          }
        case _ => ()
      }
      buffer
    })

    method("randomUUID", 0)((_, _) => {
      val bytes = new Array[Byte](16)
      random.nextBytes(bytes)
      bytes(6) = ((bytes(6) & 0x0f) | 0x40).toByte
      bytes(8) = ((bytes(8) & 0x3f) | 0x80).toByte
      val sb = new StringBuilder(36)
      for i <- 0 until 16 do {
        if i == 4 || i == 6 || i == 8 || i == 10 then sb.append('-')
        sb.append(f"${bytes(i) & 0xff}%02x")
      }
      JSValue.fromString(sb.toString)
    })

    method("randomInt", 3)((args, callCtx) => {
      given JSContext = callCtx
      def numberAt(i: Int): Option[Long] =
        args.lift(i) match {
          case Some(JSValue.Int32(n)) => Some(n.toLong)
          case Some(JSValue.Float64(d)) if !d.isNaN && !d.isInfinite => Some(d.toLong)
          case _ => None
        }
      val (min, max) = (numberAt(0), numberAt(1)) match {
        case (Some(a), Some(b)) => (a, b)
        case (Some(a), None)    => (0L, a)
        case _                  => (0L, 1L)
      }
      if max <= min then
        NodeHelpers.throwCoded(
          "RangeError",
          "The value of \"max\" is out of range",
          "ERR_OUT_OF_RANGE"
        )
      val span = max - min
      val value = min + Math.floorMod(random.nextLong(), span)
      val result = JSValue.fromDouble(value.toDouble)
      args.lastOption match {
        case Some(callback) if BuiltinHelpers.isCallable(callback) =>
          callCtx.queueMicrotask { () =>
            BuiltinHelpers.callFunctionWithThis(
              callback,
              JSValue.Undefined,
              Array(JSValue.Null, result)
            )
          }
        case _ => ()
      }
      result
    })

    // ---- hashing -----------------------------------------------------------

    def createHashImpl(algorithm: String)(using ctx: JSContext): JSValue = {
      val md =
        try MessageDigest.getInstance(digestName(algorithm))
        catch
          case _: Exception =>
            NodeHelpers.throwCoded(
              "Error",
              s"Digest method not supported: $algorithm",
              "ERR_CRYPTO_INVALID_DIGEST"
            )
      val state = new HashState(algorithm, md)
      val hash = JSObject(prototype = ctx.objectPrototype)
      hash.initProperty("__hashState", JSValue.Native(state), enumerable = false, writable = false, configurable = false)
      def install(name: String, length: Int)(
          impl: (Array[JSValue], JSContext) => JSValue
      ): Unit =
        // Hash methods are always called on the hash object, so args(0) is
        // the receiver.
        hash.set(
          name,
          JSValue.Native(
            NativeFunction(
              name = name,
              length = length,
              impl = (args, callCtx) => impl(args.drop(1), callCtx)
            )
          )
        )
      install("update", 2) { (args, callCtx) =>
        given JSContext = callCtx
        val value = args.headOption.getOrElse(JSValue.Undefined)
        val encoding =
          args.lift(1).filter(_ != JSValue.Undefined).map(v => NodeHelpers.toStr(v)).getOrElse("utf8")
        state.digest.update(bytesOf(value, encoding))
        JSValue.Object(hash)
      }
      install("digest", 1) { (args, callCtx) =>
        given JSContext = callCtx
        val bytes = state.digest.digest()
        args.headOption.filter(_ != JSValue.Undefined) match {
          case Some(encoding) =>
            JSValue.fromString(
              NodeEncodings.stringFromBytes(bytes, NodeHelpers.toStr(encoding))
            )
          case None => NodeBuffer.makeBuffer(bytes)
        }
      }
      install("copy", 0) { (args, callCtx) =>
        given JSContext = callCtx
        val copy = state.digest.clone().asInstanceOf[MessageDigest]
        val hash = JSObject(prototype = callCtx.objectPrototype)
        val newState = new HashState(algorithm, copy)
        hash.initProperty("__hashState", JSValue.Native(newState), enumerable = false, writable = false, configurable = false)
        JSValue.Object(hash)
      }
      JSValue.Object(hash)
    }

    method("createHash", 2)((args, callCtx) => {
      given JSContext = callCtx
      createHashImpl(args.headOption.map(NodeHelpers.toStr(_)).getOrElse("sha256"))
    })

    def createHmacImpl(algorithm: String, key: Array[Byte])(using
        ctx: JSContext
    ): JSValue = {
      val mac =
        try {
          val m = Mac.getInstance(macName(algorithm))
          m.init(new SecretKeySpec(key, macName(algorithm)))
          m
        } catch
          case _: Exception =>
            NodeHelpers.throwCoded(
              "Error",
              s"Digest method not supported: $algorithm",
              "ERR_CRYPTO_INVALID_DIGEST"
            )
      val state = new HmacState(algorithm, mac)
      val hmac = JSObject(prototype = ctx.objectPrototype)
      hmac.initProperty("__hmacState", JSValue.Native(state), enumerable = false, writable = false, configurable = false)
      hmac.set(
        "update",
        JSValue.Native(
          NativeFunction(
            name = "update",
            length = 2,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              val rest = if args.nonEmpty then args.drop(1) else args
              val value = rest.headOption.getOrElse(JSValue.Undefined)
              val encoding =
                rest.lift(1).filter(_ != JSValue.Undefined).map(v => NodeHelpers.toStr(v)).getOrElse("utf8")
              state.mac.update(bytesOf(value, encoding))
              JSValue.Object(hmac)
            }
          )
        )
      )
      hmac.set(
        "digest",
        JSValue.Native(
          NativeFunction(
            name = "digest",
            length = 1,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              val rest = if args.nonEmpty then args.drop(1) else args
              val bytes = state.mac.doFinal()
              rest.headOption.filter(_ != JSValue.Undefined) match {
                case Some(encoding) =>
                  JSValue.fromString(
                    NodeEncodings.stringFromBytes(bytes, NodeHelpers.toStr(encoding))
                  )
                case None => NodeBuffer.makeBuffer(bytes)
              }
            }
          )
        )
      )
      JSValue.Object(hmac)
    }

    method("createHmac", 3)((args, callCtx) => {
      given JSContext = callCtx
      val algorithm = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("sha256")
      val keyValue = args.lift(1).getOrElse(JSValue.Undefined)
      val key =
        keyValue match {
          case JSValue.JSStr(s) => NodeEncodings.bytesFromString(s, "utf8")
          case other            => NodeBuffer.bytesOfValue(other).getOrElse(Array.emptyByteArray)
        }
      createHmacImpl(algorithm, key)
    })

    method("timingSafeEqual", 2)((args, callCtx) => {
      given JSContext = callCtx
      val a = NodeBuffer.bytesOfValue(args.headOption.getOrElse(JSValue.Undefined)).getOrElse(Array.emptyByteArray)
      val b = NodeBuffer.bytesOfValue(args.lift(1).getOrElse(JSValue.Undefined)).getOrElse(Array.emptyByteArray)
      if a.length != b.length then
        NodeHelpers.throwCoded(
          "RangeError",
          "Input buffers must have the same byte length",
          "ERR_CRYPTO_TIMING_SAFE_EQUAL_LENGTH"
        )
      var diff = 0
      var i = 0
      while i < a.length do {
        diff |= (a(i) ^ b(i)) & 0xff
        i += 1
      }
      JSValue.Bool(diff == 0)
    })

    method("getRandomValues", 1)((args, callCtx) => {
      given JSContext = callCtx
      val target =
        args.find(isTypedArrayValue).getOrElse(args.headOption.getOrElse(JSValue.Undefined))
      getRandomValues(target)
    })
    crypto.set(
      "constants",
      JSValue.Object(JSObject(prototype = null))
    )
    crypto.set("webcrypto", JSValue.Undefined)

    JSValue.Object(crypto)
  }

  private def isTypedArrayValue(value: JSValue): Boolean =
    value match {
      case JSValue.Object(obj) => obj.getOwnPropertyRaw("__taView").isDefined
      case _                   => false
    }

  /** Fill a typed array with random bytes (`crypto.getRandomValues`). */
  def getRandomValues(array: JSValue)(using ctx: JSContext): JSValue = {
    array match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__taView") match {
          case Some(JSValue.Native(view: TypedArrayView)) =>
            if view.buffer.detached then
              ctx.throwTypeError("ArrayBuffer is detached")
            val bytes = new Array[Byte](view.byteLength)
            random.nextBytes(bytes)
            System.arraycopy(bytes, 0, view.buffer.data, view.byteOffset, bytes.length)
            array
          case _ =>
            ctx.throwTypeError(
              "The \"array\" argument must be an instance of an integer typed array"
            )
        }
      case _ =>
        ctx.throwTypeError(
          "The \"array\" argument must be an instance of an integer typed array"
        )
    }
  }

  /** Install the global `crypto` object (Node 19+). */
  def installGlobal(cryptoModule: JSValue)(using ctx: JSContext): Unit = {
    val webcrypto = JSObject(prototype = ctx.objectPrototype)
    webcrypto.set(
      "getRandomValues",
      JSValue.Native(
        NativeFunction(
          name = "getRandomValues",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val target =
              args.find(isTypedArrayValue).getOrElse(args.headOption.getOrElse(JSValue.Undefined))
            getRandomValues(target)
          }
        )
      )
    )
    webcrypto.set(
      "randomUUID",
      JSValue.Native(
        NativeFunction(
          name = "randomUUID",
          length = 0,
          impl = (_, _) => {
            val bytes = new Array[Byte](16)
            random.nextBytes(bytes)
            bytes(6) = ((bytes(6) & 0x0f) | 0x40).toByte
            bytes(8) = ((bytes(8) & 0x3f) | 0x80).toByte
            val sb = new StringBuilder(36)
            for i <- 0 until 16 do {
              if i == 4 || i == 6 || i == 8 || i == 10 then sb.append('-')
              sb.append(f"${bytes(i) & 0xff}%02x")
            }
            JSValue.fromString(sb.toString)
          }
        )
      )
    )
    webcrypto.set("subtle", JSValue.Undefined)
    cryptoModule match {
      case JSValue.Object(obj) => obj.set("webcrypto", JSValue.Object(webcrypto))
      case _                   => ()
    }
    ctx.global.set("crypto", JSValue.Object(webcrypto))
  }
}
