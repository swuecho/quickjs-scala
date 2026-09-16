package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.runtime.builtins.TypedArrayBuiltins.TypedArrayView
import quickjs.objmodel.JSObject

import java.security.{
  KeyFactory,
  KeyPairGenerator,
  MessageDigest,
  SecureRandom,
  Signature
}
import java.security.interfaces.RSAPrivateCrtKey
import java.security.spec.{ECGenParameterSpec, RSAKeyGenParameterSpec, RSAPublicKeySpec}
import javax.crypto.{Cipher, Mac}
import javax.crypto.spec.{GCMParameterSpec, IvParameterSpec, SecretKeySpec}
import java.math.BigInteger
import java.nio.charset.StandardCharsets
import scala.collection.mutable

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

  def create(loop: HostEventLoop)(using ctx: JSContext): JSValue = {
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
          case JSValue.Object(obj) =>
            obj.getOwnProperty("__secretBytes") match {
              case Some(JSValue.Native(bytes: Array[Byte])) => bytes
              case _ =>
                NodeBuffer
                  .bytesOfValue(keyValue)
                  .getOrElse(Array.emptyByteArray)
            }
          case other =>
            NodeBuffer.bytesOfValue(other).getOrElse(Array.emptyByteArray)
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
    // =======================================================================
    // Ciphers
    // =======================================================================

    /** (javaName, keyAlgorithm, keyBytes, ivBytes, gcm, ecb) */
    def cipherSpec(
        algorithm: String
    ): (String, String, Int, Int, Boolean, Boolean) =
      algorithm.toLowerCase match {
        case "aes-128-cbc"  => ("AES/CBC/PKCS5Padding", "AES", 16, 16, false, false)
        case "aes-192-cbc"  => ("AES/CBC/PKCS5Padding", "AES", 24, 16, false, false)
        case "aes-256-cbc"  => ("AES/CBC/PKCS5Padding", "AES", 32, 16, false, false)
        case "aes-128-ecb"  => ("AES/ECB/PKCS5Padding", "AES", 16, 0, false, true)
        case "aes-192-ecb"  => ("AES/ECB/PKCS5Padding", "AES", 24, 0, false, true)
        case "aes-256-ecb"  => ("AES/ECB/PKCS5Padding", "AES", 32, 0, false, true)
        case "aes-128-ctr"  => ("AES/CTR/NoPadding", "AES", 16, 16, false, false)
        case "aes-192-ctr"  => ("AES/CTR/NoPadding", "AES", 24, 16, false, false)
        case "aes-256-ctr"  => ("AES/CTR/NoPadding", "AES", 32, 16, false, false)
        case "aes-128-cfb"  => ("AES/CFB/NoPadding", "AES", 16, 16, false, false)
        case "aes-256-cfb"  => ("AES/CFB/NoPadding", "AES", 32, 16, false, false)
        case "aes-128-cfb8" => ("AES/CFB8/NoPadding", "AES", 16, 16, false, false)
        case "aes-128-ofb"  => ("AES/OFB/NoPadding", "AES", 16, 16, false, false)
        case "aes-256-ofb"  => ("AES/OFB/NoPadding", "AES", 32, 16, false, false)
        case "aes-128-gcm"  => ("AES/GCM/NoPadding", "AES", 16, 12, true, false)
        case "aes-192-gcm"  => ("AES/GCM/NoPadding", "AES", 24, 12, true, false)
        case "aes-256-gcm"  => ("AES/GCM/NoPadding", "AES", 32, 12, true, false)
        case "des-cbc"      => ("DES/CBC/PKCS5Padding", "DES", 8, 8, false, false)
        case "des-ede3-cbc" => ("DESede/CBC/PKCS5Padding", "DESede", 24, 8, false, false)
        case "des-ede3"     => ("DESede/ECB/PKCS5Padding", "DESede", 24, 0, false, true)
        case "chacha20-poly1305" =>
          ("ChaCha20-Poly1305", "ChaCha20", 32, 12, true, false)
        case other =>
          NodeHelpers.throwCoded(
            "Error",
            s"Unknown cipher: $other",
            "ERR_CRYPTO_UNKNOWN_CIPHER"
          )
      }

    final class CipherState(
        val algorithm: String,
        val javaName: String,
        val keyAlgorithm: String,
        val keyBytes: Array[Byte],
        val iv: Array[Byte],
        val gcm: Boolean,
        val tagLength: Int,
        val ecb: Boolean,
        val encrypting: Boolean
    ) {
      var autoPadding = true
      var cipher: Cipher = null
      var authTag: Array[Byte] = null
      var started = false
      var finalized = false

      def initialize(): Unit = {
        val name =
          if javaName.endsWith("PKCS5Padding") && !autoPadding then
            javaName.replace("PKCS5Padding", "NoPadding")
          else javaName
        cipher = Cipher.getInstance(name)
        val keySpec = new SecretKeySpec(keyBytes, keyAlgorithm)
        val mode = if encrypting then Cipher.ENCRYPT_MODE else Cipher.DECRYPT_MODE
        if ecb then cipher.init(mode, keySpec)
        else if gcm && keyAlgorithm != "ChaCha20" then
          cipher.init(mode, keySpec, new GCMParameterSpec(tagLength * 8, iv))
        else cipher.init(mode, keySpec, new IvParameterSpec(iv))
      }
    }

    def makeCipher(
        algorithm: String,
        key: Array[Byte],
        iv: Array[Byte],
        options: JSValue,
        encrypting: Boolean
    )(using ctx: JSContext): JSValue = {
      val (javaName, keyAlgorithm, keyBytes, ivBytes, gcm, ecb) =
        cipherSpec(algorithm)
      if key.length != keyBytes then
        NodeHelpers.throwCoded(
          "Error",
          s"Invalid key length: ${key.length}",
          "ERR_CRYPTO_INVALID_KEYLEN"
        )
      if !ecb && iv.length != ivBytes then
        NodeHelpers.throwCoded(
          "Error",
          s"Invalid initialization vector length: ${iv.length}",
          "ERR_CRYPTO_INVALID_IV"
        )
      val tagLength =
        options match {
          case JSValue.Object(_) =>
            BuiltinHelpers.getPropertyWithGetter(options, "authTagLength") match {
              case JSValue.Int32(n)   => n
              case JSValue.Float64(d) => d.toInt
              case _                  => 16
            }
          case _ => 16
        }
      val state = new CipherState(
        algorithm,
        javaName,
        keyAlgorithm,
        key,
        iv,
        gcm,
        tagLength,
        ecb,
        encrypting
      )
      state.initialize()
      val obj = JSObject(prototype = ctx.objectPrototype)
      obj.initProperty(
        "__cipherState",
        JSValue.Native(state),
        enumerable = false,
        writable = false,
        configurable = false
      )
      def stripSelf(args: Array[JSValue]): Array[JSValue] =
        args.headOption match {
          case Some(JSValue.Object(o)) if o eq obj => args.drop(1)
          case _                                   => args
        }
      def setMethod(name: String, arity: Int)(
          impl: (Array[JSValue], JSContext) => JSValue
      ): Unit =
        obj.set(
          name,
          JSValue.Native(
            NativeFunction(
              name = name,
              length = arity,
              impl = (args, callCtx) => {
                given JSContext = callCtx
                impl(stripSelf(args), callCtx)
              }
            )
          )
        )
      setMethod("update", 3) { (args, callCtx) =>
        given JSContext = callCtx
        val data = args.headOption.getOrElse(JSValue.Undefined)
        val inputEncoding =
          args
            .lift(1)
            .filter(_ != JSValue.Undefined)
            .map(NodeHelpers.toStr(_))
            .getOrElse("utf8")
        val outputEncoding = args.lift(2).filter(_ != JSValue.Undefined)
        state.started = true
        val output = state.cipher.update(bytesOf(data, inputEncoding))
        outputEncoding match {
          case Some(enc) =>
            JSValue.fromString(
              NodeEncodings.stringFromBytes(output, NodeHelpers.toStr(enc))
            )
          case None => NodeBuffer.makeBuffer(output)
        }
      }
      setMethod("final", 1) { (args, callCtx) =>
        given JSContext = callCtx
        if state.finalized then
          NodeHelpers.throwCoded(
            "Error",
            "Cipher already finalized",
            "ERR_CRYPTO_INVALID_STATE"
          )
        state.finalized = true
        try {
          val output =
            if state.gcm && state.encrypting then {
              val all = state.cipher.doFinal()
              state.authTag = all.takeRight(state.tagLength)
              all.dropRight(state.tagLength)
            } else if state.gcm && !state.encrypting && state.authTag != null then
              state.cipher.doFinal(state.authTag)
            else state.cipher.doFinal()
          args.headOption.filter(_ != JSValue.Undefined) match {
            case Some(enc) =>
              JSValue.fromString(
                NodeEncodings.stringFromBytes(output, NodeHelpers.toStr(enc))
              )
            case None => NodeBuffer.makeBuffer(output)
          }
        } catch {
          case _: javax.crypto.AEADBadTagException =>
            NodeHelpers.throwCoded(
              "Error",
              "Unsupported state or unable to authenticate data",
              "ERR_OSSL_EVP_BAD_DECRYPT"
            )
          case _: javax.crypto.BadPaddingException =>
            NodeHelpers.throwCoded(
              "Error",
              "error:1C800064:Provider routines::bad decrypt",
              "ERR_OSSL_EVP_BAD_DECRYPT"
            )
          case _: javax.crypto.IllegalBlockSizeException =>
            NodeHelpers.throwCoded(
              "Error",
              "wrong final block length",
              "ERR_OSSL_EVP_BAD_DECRYPT"
            )
        }
      }
      setMethod("setAutoPadding", 1) { (args, _) =>
        if state.started then
          NodeHelpers.throwCoded(
            "Error",
            "Cannot change padding after data has been written",
            "ERR_CRYPTO_INVALID_STATE"
          )
        state.autoPadding = args.headOption.exists(_.toBoolean)
        state.initialize()
        JSValue.Object(obj)
      }
      setMethod("setAAD", 2) { (args, callCtx) =>
        given JSContext = callCtx
        val data = args.headOption.getOrElse(JSValue.Undefined)
        val encoding =
          args
            .lift(1)
            .filter(_ != JSValue.Undefined)
            .map(NodeHelpers.toStr(_))
            .getOrElse("utf8")
        state.cipher.updateAAD(bytesOf(data, encoding))
        JSValue.Object(obj)
      }
      setMethod("getAuthTag", 0) { (_, _) =>
        if state.authTag == null then
          NodeHelpers.throwCoded(
            "Error",
            "Auth tag must be set before finalizing the decipher",
            "ERR_CRYPTO_INVALID_STATE"
          )
        NodeBuffer.makeBuffer(state.authTag)
      }
      setMethod("setAuthTag", 1) { (args, _) =>
        state.authTag = NodeBuffer
          .bytesOfValue(args.headOption.getOrElse(JSValue.Undefined))
          .getOrElse(Array.emptyByteArray)
        JSValue.Object(obj)
      }
      obj.set("algorithm", JSValue.fromString(algorithm))
      JSValue.Object(obj)
    }

    method("createCipheriv", 3)((args, callCtx) => {
      given JSContext = callCtx
      val algorithm = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val key = bytesOf(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val iv = bytesOf(args.lift(2).getOrElse(JSValue.Undefined), "utf8")
      makeCipher(algorithm, key, iv, args.lift(3).getOrElse(JSValue.Undefined), true)
    })
    method("createDecipheriv", 3)((args, callCtx) => {
      given JSContext = callCtx
      val algorithm = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val key = bytesOf(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val iv = bytesOf(args.lift(2).getOrElse(JSValue.Undefined), "utf8")
      makeCipher(algorithm, key, iv, args.lift(3).getOrElse(JSValue.Undefined), false)
    })

    /** OpenSSL `EVP_BytesToKey` with MD5 (legacy `createCipher`). */
    def evpBytesToKey(
        password: Array[Byte],
        salt: Array[Byte],
        keyLen: Int,
        ivLen: Int
    ): (Array[Byte], Array[Byte]) = {
      val total = new java.io.ByteArrayOutputStream()
      var previous = Array.emptyByteArray
      while total.size() < keyLen + ivLen do {
        val md = MessageDigest.getInstance("MD5")
        md.update(previous)
        md.update(password)
        if salt.nonEmpty then md.update(salt)
        previous = md.digest()
        total.write(previous)
      }
      val combined = total.toByteArray
      (combined.take(keyLen), combined.slice(keyLen, keyLen + ivLen))
    }

    method("createCipher", 2)((args, callCtx) => {
      given JSContext = callCtx
      val algorithm = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val password = bytesOf(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val (_, _, keyBytes, ivBytes, _, _) = cipherSpec(algorithm)
      val (key, iv) = evpBytesToKey(password, Array.emptyByteArray, keyBytes, ivBytes)
      makeCipher(algorithm, key, iv, JSValue.Undefined, true)
    })
    method("createDecipher", 2)((args, callCtx) => {
      given JSContext = callCtx
      val algorithm = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val password = bytesOf(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val (_, _, keyBytes, ivBytes, _, _) = cipherSpec(algorithm)
      val (key, iv) = evpBytesToKey(password, Array.emptyByteArray, keyBytes, ivBytes)
      makeCipher(algorithm, key, iv, JSValue.Undefined, false)
    })
    method("getCiphers", 0)((_, _) => {
      val array = quickjs.objmodel.JSArray.empty()
      Seq(
        "aes-128-cbc", "aes-192-cbc", "aes-256-cbc",
        "aes-128-ecb", "aes-192-ecb", "aes-256-ecb",
        "aes-128-ctr", "aes-192-ctr", "aes-256-ctr",
        "aes-128-gcm", "aes-192-gcm", "aes-256-gcm",
        "chacha20-poly1305", "des-cbc", "des-ede3-cbc", "des-ede3"
      ).foreach(name => array.push(JSValue.fromString(name)))
      JSValue.JSArrayVal(array)
    })
    method("getHashes", 0)((_, _) => {
      val array = quickjs.objmodel.JSArray.empty()
      Seq("md5", "sha1", "sha224", "sha256", "sha384", "sha512", "sha3-256", "sha3-512")
        .foreach(name => array.push(JSValue.fromString(name)))
      JSValue.JSArrayVal(array)
    })

    // =======================================================================
    // KDFs
    // =======================================================================

    def kdfBytes(value: JSValue, encoding: String)(using
        ctx: JSContext
    ): Array[Byte] =
      value match {
        case JSValue.JSStr(s) => NodeEncodings.bytesFromString(s, encoding)
        case other =>
          NodeBuffer.bytesOfValue(other).getOrElse(Array.emptyByteArray)
      }

    def intOf(value: JSValue): Int =
      value match {
        case JSValue.Int32(n)   => n
        case JSValue.Float64(d) => d.toInt
        case _                  => 0
      }

    method("pbkdf2Sync", 5)((args, callCtx) => {
      given JSContext = callCtx
      val password = kdfBytes(args.headOption.getOrElse(JSValue.Undefined), "utf8")
      val salt = kdfBytes(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val iterations = intOf(args.lift(2).getOrElse(JSValue.Int32(1)))
      val keyLength = intOf(args.lift(3).getOrElse(JSValue.Int32(0)))
      val digest = args.lift(4).map(NodeHelpers.toStr(_)).getOrElse("sha256")
      NodeBuffer.makeBuffer(
        NodeCryptoExtras.pbkdf2(password, salt, iterations, keyLength, macName(digest))
      )
    })
    method("pbkdf2", 6)((args, callCtx) => {
      given JSContext = callCtx
      val password = kdfBytes(args.headOption.getOrElse(JSValue.Undefined), "utf8")
      val salt = kdfBytes(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val iterations = intOf(args.lift(2).getOrElse(JSValue.Int32(1)))
      val keyLength = intOf(args.lift(3).getOrElse(JSValue.Int32(0)))
      val digest = args.lift(4).map(NodeHelpers.toStr(_)).getOrElse("sha256")
      val callback = args.reverseIterator.find(BuiltinHelpers.isCallable)
      loop.execute {
        val result =
          try
            Right(
              NodeCryptoExtras.pbkdf2(
                password,
                salt,
                iterations,
                keyLength,
                macName(digest)
              )
            )
          catch case e: Throwable => Left(e)
        loop.post { () =>
          callback.foreach { cb =>
            result match {
              case Right(key) =>
                BuiltinHelpers.callFunctionWithThis(
                  cb,
                  JSValue.Undefined,
                  Array(JSValue.Null, NodeBuffer.makeBuffer(key))
                )
              case Left(error) =>
                BuiltinHelpers.callFunctionWithThis(
                  cb,
                  JSValue.Undefined,
                  Array(
                    ctx.createError(
                      "Error",
                      Option(error.getMessage).getOrElse("pbkdf2 failed")
                    )
                  )
                )
            }
          }
        }
      }
      JSValue.Undefined
    })

    def scryptOptions(options: JSValue): (Int, Int, Int, Int) = {
      def optInt(name: String, default: Int): Int =
        options match {
          case JSValue.Object(_) =>
            BuiltinHelpers.getPropertyWithGetter(options, name) match {
              case JSValue.Int32(n) if n > 0   => n
              case JSValue.Float64(d) if d > 0 => d.toInt
              case _                           => default
            }
          case _ => default
        }
      (optInt("N", 16384), optInt("r", 8), optInt("p", 1), optInt("maxmem", 32 * 1024 * 1024))
    }

    def checkScryptParams(n: Int, r: Int, maxmem: Int)(using JSContext): Unit = {
      if n <= 1 || (n & (n - 1)) != 0 then
        NodeHelpers.throwCoded(
          "RangeError",
          "Invalid scrypt params: N must be a power of 2 greater than 1",
          "ERR_CRYPTO_INVALID_SCRYPT_PARAMS"
        )
      if 128L * n * r > maxmem then
        NodeHelpers.throwCoded(
          "RangeError",
          "Invalid scrypt params: memory limit exceeded",
          "ERR_CRYPTO_INVALID_SCRYPT_PARAMS"
        )
    }

    method("scryptSync", 3)((args, callCtx) => {
      given JSContext = callCtx
      val password = kdfBytes(args.headOption.getOrElse(JSValue.Undefined), "utf8")
      val salt = kdfBytes(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val keyLength = intOf(args.lift(2).getOrElse(JSValue.Int32(0)))
      val (n, r, p, maxmem) =
        scryptOptions(args.lift(3).getOrElse(JSValue.Undefined))
      checkScryptParams(n, r, maxmem)
      NodeBuffer.makeBuffer(
        NodeCryptoExtras.scrypt(password, salt, keyLength, n, r, p)
      )
    })
    method("scrypt", 4)((args, callCtx) => {
      given JSContext = callCtx
      val password = kdfBytes(args.headOption.getOrElse(JSValue.Undefined), "utf8")
      val salt = kdfBytes(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val keyLength = intOf(args.lift(2).getOrElse(JSValue.Int32(0)))
      val options = args.lift(3).getOrElse(JSValue.Undefined)
      val (n, r, p, maxmem) = scryptOptions(options)
      val callback = args.reverseIterator.find(BuiltinHelpers.isCallable)
      loop.execute {
        val result =
          try {
            checkScryptParams(n, r, maxmem)
            Right(NodeCryptoExtras.scrypt(password, salt, keyLength, n, r, p))
          } catch case e: Throwable => Left(e)
        loop.post { () =>
          callback.foreach { cb =>
            result match {
              case Right(key) =>
                BuiltinHelpers.callFunctionWithThis(
                  cb,
                  JSValue.Undefined,
                  Array(JSValue.Null, NodeBuffer.makeBuffer(key))
                )
              case Left(error) =>
                val err =
                  if error.isInstanceOf[quickjs.runtime.JSException] then
                    error.asInstanceOf[quickjs.runtime.JSException].getValue
                  else
                    ctx.createError(
                      "Error",
                      Option(error.getMessage).getOrElse("scrypt failed")
                    )
                BuiltinHelpers.callFunctionWithThis(cb, JSValue.Undefined, Array(err))
            }
          }
        }
      }
      JSValue.Undefined
    })

    method("hkdfSync", 5)((args, callCtx) => {
      given JSContext = callCtx
      val digest = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("sha256")
      val ikm = kdfBytes(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val salt = kdfBytes(args.lift(2).getOrElse(JSValue.Undefined), "utf8")
      val info = kdfBytes(args.lift(3).getOrElse(JSValue.Undefined), "utf8")
      val keyLength = intOf(args.lift(4).getOrElse(JSValue.Int32(0)))
      NodeBuffer.makeBuffer(
        NodeCryptoExtras.hkdf(macName(digest), ikm, salt, info, keyLength)
      )
    })
    method("hkdf", 6)((args, callCtx) => {
      given JSContext = callCtx
      val digest = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("sha256")
      val ikm = kdfBytes(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val salt = kdfBytes(args.lift(2).getOrElse(JSValue.Undefined), "utf8")
      val info = kdfBytes(args.lift(3).getOrElse(JSValue.Undefined), "utf8")
      val keyLength = intOf(args.lift(4).getOrElse(JSValue.Int32(0)))
      val callback = args.reverseIterator.find(BuiltinHelpers.isCallable)
      loop.execute {
        val result =
          try
            Right(NodeCryptoExtras.hkdf(macName(digest), ikm, salt, info, keyLength))
          catch case e: Throwable => Left(e)
        loop.post { () =>
          callback.foreach { cb =>
            result match {
              case Right(key) =>
                BuiltinHelpers.callFunctionWithThis(
                  cb,
                  JSValue.Undefined,
                  Array(JSValue.Null, NodeBuffer.makeBuffer(key))
                )
              case Left(error) =>
                BuiltinHelpers.callFunctionWithThis(
                  cb,
                  JSValue.Undefined,
                  Array(
                    ctx.createError(
                      "Error",
                      Option(error.getMessage).getOrElse("hkdf failed")
                    )
                  )
                )
            }
          }
        }
      }
      JSValue.Undefined
    })

    // =======================================================================
    // Keys and signatures
    // =======================================================================

    val keyObjectProto = JSObject(prototype = ctx.objectPrototype)
    val keyObjectCtor = quickjs.value.NativeConstructor(
      name = "KeyObject",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        callCtx.throwTypeError(
          "Class constructor KeyObject cannot be invoked without 'new'"
        )
      },
      constructImpl = (_, callCtx) => {
        given JSContext = callCtx
        JSValue.Object(JSObject(prototype = keyObjectProto))
      },
      prototype = keyObjectProto
    )
    BuiltinHelpers.initConstructor(keyObjectCtor, length = 0)
    crypto.set("KeyObject", JSValue.Native(keyObjectCtor))

    def keyObject(key: java.security.Key, kind: String, isPrivate: Boolean)(using
        ctx: JSContext
    ): JSObject = {
      val obj = JSObject(prototype = keyObjectProto)
      obj.initProperty(
        "__key",
        JSValue.Native(key),
        enumerable = false,
        writable = false,
        configurable = false
      )
      obj.set(
        "type",
        JSValue.fromString(if isPrivate then "private" else "public")
      )
      obj.set("asymmetricKeyType", JSValue.fromString(kind))
      obj.set(
        "export",
        JSValue.Native(
          NativeFunction(
            name = "export",
            length = 1,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              val rest = args.headOption match {
                case Some(JSValue.Object(o)) if o eq obj => args.drop(1)
                case _                                   => args
              }
              val options = rest.headOption.getOrElse(JSValue.Undefined)
              val format =
                options match {
                  case JSValue.Object(_) =>
                    BuiltinHelpers.getPropertyWithGetter(options, "format") match {
                      case JSValue.JSStr(s) => s
                      case _                => "pem"
                    }
                  case JSValue.JSStr(s) => s
                  case _                => "pem"
                }
              val der = key.getEncoded
              if format == "der" then NodeBuffer.makeBuffer(der)
              else if format == "jwk" then
                NodeHelpers.throwCoded(
                  "Error",
                  "JWK export is not supported",
                  "ERR_CRYPTO_JWK_UNSUPPORTED_KEY_TYPE"
                )
              else {
                val label = if isPrivate then "PRIVATE KEY" else "PUBLIC KEY"
                JSValue.fromString(NodeCryptoExtras.pemEncode(label, der))
              }
            }
          )
        )
      )
      obj
    }

    def keyBytesAndKind(value: JSValue)(using
        ctx: JSContext
    ): (java.security.Key, String, Boolean) =
      value match {
        case JSValue.Object(obj) =>
          obj.getOwnProperty("__key") match {
            case Some(JSValue.Native(key: java.security.Key)) =>
              val kind =
                obj.get("asymmetricKeyType") match {
                  case JSValue.JSStr(s) => s
                  case _                => "rsa"
                }
              val isPrivate =
                obj.get("type") match {
                  case JSValue.JSStr(s) => s == "private"
                  case _                => key.isInstanceOf[java.security.PrivateKey]
                }
              (key, kind, isPrivate)
            case _ =>
              NodeHelpers.throwCoded(
                "TypeError",
                "Invalid key object",
                "ERR_INVALID_ARG_TYPE"
              )
          }
        case JSValue.JSStr(text) =>
          NodeCryptoExtras.pemDecode(text) match {
            case Some((label, der)) =>
              val isPrivate = label.contains("PRIVATE")
              NodeCryptoExtras.decodeKey(der, isPrivate) match {
                case Some((key, kind)) => (key, kind, isPrivate)
                case None =>
                  NodeHelpers.throwCoded(
                    "Error",
                    "Failed to parse key",
                    "ERR_OSSL_ASN1_D2I_READ_BIO"
                  )
              }
            case None =>
              NodeHelpers.throwCoded(
                "Error",
                "Failed to parse key",
                "ERR_OSSL_ASN1_D2I_READ_BIO"
              )
          }
        case other =>
          NodeBuffer.bytesOfValue(other) match {
            case Some(der) =>
              NodeCryptoExtras.decodeKey(der, privateKey = true)
                .orElse(NodeCryptoExtras.decodeKey(der, privateKey = false)) match {
                case Some((key, kind)) =>
                  (key, kind, key.isInstanceOf[java.security.PrivateKey])
                case None =>
                  NodeHelpers.throwCoded(
                    "Error",
                    "Failed to parse key",
                    "ERR_OSSL_ASN1_D2I_READ_BIO"
                  )
              }
            case None =>
              NodeHelpers.throwCoded(
                "TypeError",
                "Invalid key",
                "ERR_INVALID_ARG_TYPE"
              )
          }
      }

    def signatureName(algorithm: String, kind: String): String =
      if kind == "ed25519" || kind == "ed448" then
        if kind == "ed25519" then "Ed25519" else "Ed448"
      else
        algorithm.toLowerCase match {
          case "rsa-sha256" | "sha256" =>
            if kind == "ec" then "SHA256withECDSA" else "SHA256withRSA"
          case "rsa-sha384" | "sha384" =>
            if kind == "ec" then "SHA384withECDSA" else "SHA384withRSA"
          case "rsa-sha512" | "sha512" =>
            if kind == "ec" then "SHA512withECDSA" else "SHA512withRSA"
          case "rsa-sha1" | "sha1" =>
            if kind == "ec" then "SHA1withECDSA" else "SHA1withRSA"
          case "md5" => "MD5withRSA"
          case "ecdsa-with-sha256" => "SHA256withECDSA"
          case "" =>
            NodeHelpers.throwCoded(
              "Error",
              "Algorithm is required for signing",
              "ERR_CRYPTO_INVALID_DIGEST"
            )
          case other =>
            NodeHelpers.throwCoded(
              "Error",
              s"Unknown message digest: $other",
              "ERR_CRYPTO_INVALID_DIGEST"
            )
        }

    def doSign(
        algorithm: JSValue,
        data: Array[Byte],
        keyValue: JSValue
    )(using ctx: JSContext): Array[Byte] = {
      val (key, kind, _) = keyBytesAndKind(keyValue)
      val name = signatureName(
        algorithm match {
          case JSValue.JSStr(s) => s
          case _                => ""
        },
        kind
      )
      val signature = Signature.getInstance(name)
      signature.initSign(key.asInstanceOf[java.security.PrivateKey])
      signature.update(data)
      signature.sign()
    }

    def doVerify(
        algorithm: JSValue,
        data: Array[Byte],
        keyValue: JSValue,
        signatureBytes: Array[Byte]
    )(using ctx: JSContext): Boolean = {
      val (key, kind, _) = keyBytesAndKind(keyValue)
      val name = signatureName(
        algorithm match {
          case JSValue.JSStr(s) => s
          case _                => ""
        },
        kind
      )
      try {
        val signature = Signature.getInstance(name)
        signature.initVerify(key.asInstanceOf[java.security.PublicKey])
        signature.update(data)
        signature.verify(signatureBytes)
      } catch case _: Throwable => false
    }

    method("sign", 4)((args, callCtx) => {
      given JSContext = callCtx
      val algorithm = args.headOption.getOrElse(JSValue.Undefined)
      val data = kdfBytes(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val keyValue = args.lift(2).getOrElse(JSValue.Undefined)
      NodeBuffer.makeBuffer(doSign(algorithm, data, keyValue))
    })
    method("verify", 5)((args, callCtx) => {
      given JSContext = callCtx
      val algorithm = args.headOption.getOrElse(JSValue.Undefined)
      val data = kdfBytes(args.lift(1).getOrElse(JSValue.Undefined), "utf8")
      val keyValue = args.lift(2).getOrElse(JSValue.Undefined)
      val signatureBytes =
        kdfBytes(args.lift(3).getOrElse(JSValue.Undefined), "utf8")
      JSValue.Bool(doVerify(algorithm, data, keyValue, signatureBytes))
    })

    def makeSigner(algorithm: JSValue, verifying: Boolean)(using
        ctx: JSContext
    ): JSObject = {
      val obj = JSObject(prototype = ctx.objectPrototype)
      val chunks = mutable.ArrayBuffer.empty[Array[Byte]]
      def stripSelf(args: Array[JSValue]): Array[JSValue] =
        args.headOption match {
          case Some(JSValue.Object(o)) if o eq obj => args.drop(1)
          case _                                   => args
        }
      obj.set(
        "update",
        JSValue.Native(
          NativeFunction(
            name = "update",
            length = 2,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              val rest = stripSelf(args)
              val data = rest.headOption.getOrElse(JSValue.Undefined)
              val encoding =
                rest
                  .lift(1)
                  .filter(_ != JSValue.Undefined)
                  .map(NodeHelpers.toStr(_))
                  .getOrElse("utf8")
              chunks += kdfBytes(data, encoding)
              JSValue.Object(obj)
            }
          )
        )
      )
      if verifying then
        obj.set(
          "verify",
          JSValue.Native(
            NativeFunction(
              name = "verify",
              length = 2,
              impl = (args, callCtx) => {
                given JSContext = callCtx
                val rest = stripSelf(args)
                val keyValue = rest.headOption.getOrElse(JSValue.Undefined)
                val signatureBytes =
                  kdfBytes(rest.lift(1).getOrElse(JSValue.Undefined), "base64")
                JSValue.Bool(
                  doVerify(
                    algorithm,
                    chunks.foldLeft(Array.emptyByteArray)(_ ++ _),
                    keyValue,
                    signatureBytes
                  )
                )
              }
            )
          )
        )
      else
        obj.set(
          "sign",
          JSValue.Native(
            NativeFunction(
              name = "sign",
              length = 2,
              impl = (args, callCtx) => {
                given JSContext = callCtx
                val rest = stripSelf(args)
                val keyValue = rest.headOption.getOrElse(JSValue.Undefined)
                val outputEncoding = rest.lift(1).filter(_ != JSValue.Undefined)
                val signatureBytes = doSign(
                  algorithm,
                  chunks.foldLeft(Array.emptyByteArray)(_ ++ _),
                  keyValue
                )
                outputEncoding match {
                  case Some(enc) =>
                    JSValue.fromString(
                      NodeEncodings.stringFromBytes(
                        signatureBytes,
                        NodeHelpers.toStr(enc)
                      )
                    )
                  case None => NodeBuffer.makeBuffer(signatureBytes)
                }
              }
            )
          )
        )
      obj
    }

    method("createSign", 1)((args, callCtx) => {
      given JSContext = callCtx
      JSValue.Object(
        makeSigner(args.headOption.getOrElse(JSValue.Undefined), verifying = false)
      )
    })
    method("createVerify", 1)((args, callCtx) => {
      given JSContext = callCtx
      JSValue.Object(
        makeSigner(args.headOption.getOrElse(JSValue.Undefined), verifying = true)
      )
    })

    def mapCurve(name: String): String =
      name match {
        case "prime256v1" | "p-256" | "P-256" => "secp256r1"
        case "secp384r1" | "p-384"           => "secp384r1"
        case "secp521r1" | "p-521"           => "secp521r1"
        case other                           => other
      }

    def generateKeyPair(
        kind: String,
        options: JSValue
    ): (java.security.PublicKey, java.security.PrivateKey, String) = {
      def option(name: String): JSValue =
        options match {
          case JSValue.Object(_) =>
            BuiltinHelpers.getPropertyWithGetter(options, name)
          case _ => JSValue.Undefined
        }
      kind.toLowerCase match {
        case "rsa" =>
          val bits = option("modulusLength") match {
            case JSValue.Int32(n) if n > 0   => n
            case JSValue.Float64(d) if d > 0 => d.toInt
            case _                           => 2048
          }
          val exponent = option("publicExponent") match {
            case JSValue.Int32(n)   => BigInteger.valueOf(n.toLong)
            case JSValue.Float64(d) => BigInteger.valueOf(d.toLong)
            case JSValue.BigInt(b)  => b
            case _                  => BigInteger.valueOf(65537)
          }
          val generator = KeyPairGenerator.getInstance("RSA")
          generator.initialize(new RSAKeyGenParameterSpec(bits, exponent))
          val pair = generator.generateKeyPair()
          (pair.getPublic, pair.getPrivate, "rsa")
        case "ec" =>
          val curve = option("namedCurve") match {
            case JSValue.JSStr(s) => s
            case _                => "prime256v1"
          }
          val generator = KeyPairGenerator.getInstance("EC")
          generator.initialize(new ECGenParameterSpec(mapCurve(curve)))
          val pair = generator.generateKeyPair()
          (pair.getPublic, pair.getPrivate, "ec")
        case "ed25519" =>
          val generator = KeyPairGenerator.getInstance("Ed25519")
          val pair = generator.generateKeyPair()
          (pair.getPublic, pair.getPrivate, "ed25519")
        case other =>
          NodeHelpers.throwCoded(
            "Error",
            s"Unknown key type: $other",
            "ERR_CRYPTO_INVALID_KEYTYPE"
          )
      }
    }

    method("generateKeyPairSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val kind = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("rsa")
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val (publicKey, privateKey, tag) = generateKeyPair(kind, options)
      val result = JSObject(prototype = ctx.objectPrototype)
      result.set(
        "publicKey",
        JSValue.Object(keyObject(publicKey, tag, isPrivate = false))
      )
      result.set(
        "privateKey",
        JSValue.Object(keyObject(privateKey, tag, isPrivate = true))
      )
      JSValue.Object(result)
    })
    method("generateKeyPair", 3)((args, callCtx) => {
      given JSContext = callCtx
      val kind = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("rsa")
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val callback = args.reverseIterator.find(BuiltinHelpers.isCallable)
      loop.execute {
        val result =
          try Right(generateKeyPair(kind, options))
          catch case e: Throwable => Left(e)
        loop.post { () =>
          callback.foreach { cb =>
            result match {
              case Right((publicKey, privateKey, tag)) =>
                BuiltinHelpers.callFunctionWithThis(
                  cb,
                  JSValue.Undefined,
                  Array(
                    JSValue.Null,
                    JSValue.Object(keyObject(publicKey, tag, isPrivate = false)),
                    JSValue.Object(keyObject(privateKey, tag, isPrivate = true))
                  )
                )
              case Left(error) =>
                BuiltinHelpers.callFunctionWithThis(
                  cb,
                  JSValue.Undefined,
                  Array(
                    ctx.createError(
                      "Error",
                      Option(error.getMessage).getOrElse("generateKeyPair failed")
                    )
                  )
                )
            }
          }
        }
      }
      JSValue.Undefined
    })

    method("createSecretKey", 2)((args, callCtx) => {
      given JSContext = callCtx
      val keyValue = args.headOption.getOrElse(JSValue.Undefined)
      val bytes =
        keyValue match {
          case JSValue.JSStr(text) => text.getBytes(StandardCharsets.UTF_8)
          case other =>
            NodeBuffer.bytesOfValue(other).getOrElse(Array.emptyByteArray)
        }
      val obj = JSObject(prototype = keyObjectProto)
      obj.initProperty(
        "__secretBytes",
        JSValue.Native(bytes),
        enumerable = false,
        writable = false,
        configurable = false
      )
      obj.set("type", JSValue.fromString("secret"))
      obj.set("symmetricKeySize", JSValue.fromInt(bytes.length))
      obj.set(
        "export",
        JSValue.Native(
          NativeFunction(
            name = "export",
            length = 1,
            impl = (exportArgs, exportCtx) => {
              given JSContext = exportCtx
              val rest = exportArgs.headOption match {
                case Some(JSValue.Object(o)) if o eq obj => exportArgs.drop(1)
                case _                                   => exportArgs
              }
              val format =
                rest.headOption match {
                  case Some(JSValue.Object(options)) =>
                    BuiltinHelpers.getPropertyWithGetter(
                      JSValue.Object(options),
                      "format"
                    ) match {
                      case JSValue.JSStr(s) => s
                      case _                => "buffer"
                    }
                  case _ => "buffer"
                }
              if format == "jwk" then {
                val jwk = JSObject(prototype = null)
                jwk.set("kty", JSValue.fromString("oct"))
                jwk.set(
                  "k",
                  JSValue.fromString(
                    java.util.Base64.getUrlEncoder
                      .withoutPadding()
                      .encodeToString(bytes)
                  )
                )
                JSValue.Object(jwk)
              } else NodeBuffer.makeBuffer(bytes)
            }
          )
        )
      )
      JSValue.Object(obj)
    })

    method("createPrivateKey", 1)((args, callCtx) => {
      given JSContext = callCtx
      val (key, kind, _) =
        keyBytesAndKind(args.headOption.getOrElse(JSValue.Undefined))
      JSValue.Object(keyObject(key, kind, isPrivate = true))
    })
    method("createPublicKey", 1)((args, callCtx) => {
      given JSContext = callCtx
      val (key, kind, _) =
        keyBytesAndKind(args.headOption.getOrElse(JSValue.Undefined))
      key match {
        case privateKey: java.security.PrivateKey
            if privateKey.isInstanceOf[RSAPrivateCrtKey] =>
          val crt = privateKey.asInstanceOf[RSAPrivateCrtKey]
          val factory = KeyFactory.getInstance("RSA")
          val publicKey = factory.generatePublic(
            new RSAPublicKeySpec(crt.getModulus, crt.getPublicExponent)
          )
          JSValue.Object(keyObject(publicKey, kind, isPrivate = false))
        case _: java.security.PrivateKey =>
          NodeHelpers.throwCoded(
            "Error",
            "Deriving a public key from this private key is not supported",
            "ERR_CRYPTO_INVALID_KEY_OBJECT_TYPE"
          )
        case _ => JSValue.Object(keyObject(key, kind, isPrivate = false))
      }
    })

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
