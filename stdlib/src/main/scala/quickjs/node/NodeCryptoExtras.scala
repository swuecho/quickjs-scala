package quickjs.node

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64
import java.security.KeyFactory
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}

/** Pure-JVM implementations of the crypto primitives Node exposes that the
  * JDK does not provide directly: PBKDF2, scrypt, HKDF and PEM helpers.
  */
object NodeCryptoExtras {

  // =========================================================================
  // HMAC / PBKDF2
  // =========================================================================

  /** HMAC implemented over `MessageDigest` so empty keys work (the JCE
    * rejects an empty `SecretKeySpec`).
    */
  def hmac(macName: String, key0: Array[Byte], data: Array[Byte]): Array[Byte] = {
    val digestName = if macName.startsWith("Hmac") then macName.drop(4) else macName
    val md = MessageDigest.getInstance(digestName)
    val blockSize =
      if digestName == "SHA-384" || digestName == "SHA-512" then 128 else 64
    val key = if key0.length > blockSize then md.digest(key0) else key0
    val padded = java.util.Arrays.copyOf(key, blockSize)
    val inner = new Array[Byte](blockSize)
    val outer = new Array[Byte](blockSize)
    var i = 0
    while i < blockSize do {
      inner(i) = (padded(i) ^ 0x36).toByte
      outer(i) = (padded(i) ^ 0x5c).toByte
      i += 1
    }
    md.reset()
    md.update(inner)
    val innerHash = md.digest(data)
    md.reset()
    md.update(outer)
    md.digest(innerHash)
  }

  private def digestLength(digestName: String): Int =
    digestName match {
      case "MD5"     => 16
      case "SHA-1"   => 20
      case "SHA-224" => 28
      case "SHA-256" => 32
      case "SHA-384" => 48
      case "SHA-512" => 64
      case other     => MessageDigest.getInstance(other).getDigestLength
    }

  private def intToBytes(value: Int): Array[Byte] = Array(
    ((value >>> 24) & 0xff).toByte,
    ((value >>> 16) & 0xff).toByte,
    ((value >>> 8) & 0xff).toByte,
    (value & 0xff).toByte
  )

  /** RFC 2898 PBKDF2 with the PRF described by `macName` (`HmacSHA256`, ...). */
  def pbkdf2(
      password: Array[Byte],
      salt: Array[Byte],
      iterations: Int,
      keyLength: Int,
      macName: String
  ): Array[Byte] = {
    val digestName =
      if macName.startsWith("Hmac") then macName.drop(4) else macName
    val hLen = digestLength(digestName)
    val blocks = math.max(1, math.ceil(keyLength.toDouble / hLen).toInt)
    val out = new Array[Byte](keyLength)
    var produced = 0
    var blockIndex = 1
    while blockIndex <= blocks && produced < keyLength do {
      val block = new Array[Byte](hLen)
      // U1 = PRF(P, S || INT_BE(i))
      var current = hmac(macName, password, salt ++ intToBytes(blockIndex))
      System.arraycopy(current, 0, block, 0, hLen)
      var iteration = 1
      while iteration < iterations do {
        current = hmac(macName, password, current)
        var i = 0
        while i < hLen do {
          block(i) = (block(i) ^ current(i)).toByte
          i += 1
        }
        iteration += 1
      }
      val take = math.min(hLen, keyLength - produced)
      System.arraycopy(block, 0, out, produced, take)
      produced += take
      blockIndex += 1
    }
    out
  }

  // =========================================================================
  // scrypt (RFC 7914)
  // =========================================================================

  private def salsa20_8(input: Array[Int]): Array[Int] = {
    val x = new Array[Int](16)
    System.arraycopy(input, 0, x, 0, 16)
    // Salsa20/8 = 4 double rounds (column round + row round).
    var i = 0
    while i < 4 do {
      x(4) ^= rotl(x(0) + x(12), 7)
      x(8) ^= rotl(x(4) + x(0), 9)
      x(12) ^= rotl(x(8) + x(4), 13)
      x(0) ^= rotl(x(12) + x(8), 18)
      x(9) ^= rotl(x(5) + x(1), 7)
      x(13) ^= rotl(x(9) + x(5), 9)
      x(1) ^= rotl(x(13) + x(9), 13)
      x(5) ^= rotl(x(1) + x(13), 18)
      x(14) ^= rotl(x(10) + x(6), 7)
      x(2) ^= rotl(x(14) + x(10), 9)
      x(6) ^= rotl(x(2) + x(14), 13)
      x(10) ^= rotl(x(6) + x(2), 18)
      x(3) ^= rotl(x(15) + x(11), 7)
      x(7) ^= rotl(x(3) + x(15), 9)
      x(11) ^= rotl(x(7) + x(3), 13)
      x(15) ^= rotl(x(11) + x(7), 18)
      x(1) ^= rotl(x(0) + x(3), 7)
      x(2) ^= rotl(x(1) + x(0), 9)
      x(3) ^= rotl(x(2) + x(1), 13)
      x(0) ^= rotl(x(3) + x(2), 18)
      x(6) ^= rotl(x(5) + x(4), 7)
      x(7) ^= rotl(x(6) + x(5), 9)
      x(4) ^= rotl(x(7) + x(6), 13)
      x(5) ^= rotl(x(4) + x(7), 18)
      x(11) ^= rotl(x(10) + x(9), 7)
      x(8) ^= rotl(x(11) + x(10), 9)
      x(9) ^= rotl(x(8) + x(11), 13)
      x(10) ^= rotl(x(9) + x(8), 18)
      x(12) ^= rotl(x(15) + x(14), 7)
      x(13) ^= rotl(x(12) + x(15), 9)
      x(14) ^= rotl(x(13) + x(12), 13)
      x(15) ^= rotl(x(14) + x(13), 18)
      i += 1
    }
    val out = new Array[Int](16)
    i = 0
    while i < 16 do {
      out(i) = x(i) + input(i)
      i += 1
    }
    out
  }

  private def rotl(value: Int, count: Int): Int =
    (value << count) | (value >>> (32 - count))

  /** scrypt `ROMix` over little-endian 32-bit words. */
  private def roMix(block: Array[Int], n: Int, r: Int): Array[Int] = {
    val wordsPerBlock = 32 * r
    val v = Array.ofDim[Int](n * wordsPerBlock)
    val x = new Array[Int](wordsPerBlock)
    System.arraycopy(block, 0, x, 0, wordsPerBlock)
    var i = 0
    while i < n do {
      System.arraycopy(x, 0, v, i * wordsPerBlock, wordsPerBlock)
      blockMixInPlace(x, r)
      i += 1
    }
    i = 0
    while i < n do {
      val j = x(wordsPerBlock - 16) & (n - 1)
      var k = 0
      while k < wordsPerBlock do {
        x(k) = x(k) ^ v(j * wordsPerBlock + k)
        k += 1
      }
      blockMixInPlace(x, r)
      i += 1
    }
    x
  }

  private def blockMixInPlace(x: Array[Int], r: Int): Unit = {
    val y = new Array[Int](x.length)
    val t = new Array[Int](16)
    System.arraycopy(x, (2 * r - 1) * 16, t, 0, 16)
    var i = 0
    while i < 2 * r do {
      var k = 0
      while k < 16 do {
        t(k) = t(k) ^ x(i * 16 + k)
        k += 1
      }
      val mixed = salsa20_8(t)
      System.arraycopy(mixed, 0, t, 0, 16)
      // Even blocks first, then odd blocks.
      val destIndex = if i % 2 == 0 then (i / 2) * 16 else (r + (i - 1) / 2) * 16
      System.arraycopy(mixed, 0, y, destIndex, 16)
      i += 1
    }
    System.arraycopy(y, 0, x, 0, x.length)
  }

  private def bytesToWordsLittleEndian(bytes: Array[Byte]): Array[Int] = {
    val out = new Array[Int](bytes.length / 4)
    var i = 0
    while i < out.length do {
      out(i) = (bytes(i * 4) & 0xff) |
        ((bytes(i * 4 + 1) & 0xff) << 8) |
        ((bytes(i * 4 + 2) & 0xff) << 16) |
        ((bytes(i * 4 + 3) & 0xff) << 24)
      i += 1
    }
    out
  }

  private def wordsToBytesLittleEndian(words: Array[Int]): Array[Byte] = {
    val out = new Array[Byte](words.length * 4)
    var i = 0
    while i < words.length do {
      out(i * 4) = (words(i) & 0xff).toByte
      out(i * 4 + 1) = ((words(i) >>> 8) & 0xff).toByte
      out(i * 4 + 2) = ((words(i) >>> 16) & 0xff).toByte
      out(i * 4 + 3) = ((words(i) >>> 24) & 0xff).toByte
      i += 1
    }
    out
  }

  def scrypt(
      password: Array[Byte],
      salt: Array[Byte],
      keyLength: Int,
      n: Int,
      r: Int,
      p: Int
  ): Array[Byte] = {
    val blockWords = 32 * r
    val bytes = pbkdf2(password, salt, 1, p * blockWords * 4, "HmacSHA256")
    var i = 0
    while i < p do {
      val offset = i * blockWords * 4
      val slice = bytes.slice(offset, offset + blockWords * 4)
      val mixed = roMix(bytesToWordsLittleEndian(slice), n, r)
      val mixedBytes = wordsToBytesLittleEndian(mixed)
      System.arraycopy(mixedBytes, 0, bytes, offset, mixedBytes.length)
      i += 1
    }
    pbkdf2(password, bytes, 1, keyLength, "HmacSHA256")
  }

  // =========================================================================
  // HKDF (RFC 5869)
  // =========================================================================

  def hkdf(
      macName: String,
      ikm: Array[Byte],
      salt: Array[Byte],
      info: Array[Byte],
      keyLength: Int
  ): Array[Byte] = {
    val digestName =
      if macName.startsWith("Hmac") then macName.drop(4) else macName
    val hashLen = digestLength(digestName)
    val effectiveSalt =
      if salt == null || salt.isEmpty then new Array[Byte](hashLen) else salt
    val prk = hmac(macName, effectiveSalt, ikm)
    val out = new Array[Byte](keyLength)
    var previous = Array.emptyByteArray
    var produced = 0
    var counter = 1
    while produced < keyLength do {
      previous = hmac(macName, prk, previous ++ info ++ Array(counter.toByte))
      val take = math.min(previous.length, keyLength - produced)
      System.arraycopy(previous, 0, out, produced, take)
      produced += take
      counter += 1
    }
    out
  }

  // =========================================================================
  // PEM helpers
  // =========================================================================

  def pemEncode(label: String, der: Array[Byte]): String = {
    val base64 = Base64.getEncoder.encodeToString(der)
    val body = base64.grouped(64).mkString("\n")
    s"-----BEGIN $label-----\n$body\n-----END $label-----\n"
  }

  /** Returns `(label, der)` for a PEM document. */
  def pemDecode(text: String): Option[(String, Array[Byte])] = {
    val begin = text.indexOf("-----BEGIN ")
    if begin < 0 then None
    else {
      val labelEnd = text.indexOf("-----", begin + 11)
      if labelEnd < 0 then None
      else {
        val label = text.substring(begin + 11, labelEnd)
        val bodyStart = text.indexOf('\n', labelEnd)
        val end = text.indexOf(s"-----END $label-----", labelEnd)
        if bodyStart < 0 || end < 0 then None
        else {
          val body = text
            .substring(bodyStart, end)
            .filterNot(_.isWhitespace)
          try Some(label -> Base64.getDecoder.decode(body))
          catch case _: Throwable => None
        }
      }
    }
  }

  /** Parse a PKCS#8/X.509 DER blob by trying the supported key algorithms. */
  def decodeKey(
      der: Array[Byte],
      privateKey: Boolean
  ): Option[(java.security.Key, String)] = {
    val algorithms = Seq(
      ("RSA", "rsa"),
      ("EC", "ec"),
      ("Ed25519", "ed25519"),
      ("Ed448", "ed448"),
      ("DSA", "dsa")
    )
    algorithms.iterator
      .flatMap { case (algorithm, tag) =>
        try {
          val factory = KeyFactory.getInstance(algorithm)
          val spec =
            if privateKey then new PKCS8EncodedKeySpec(der)
            else new X509EncodedKeySpec(der)
          val key =
            if privateKey then factory.generatePrivate(spec)
            else factory.generatePublic(spec)
          Some(key -> tag)
        } catch case _: Throwable => None
      }
      .take(1)
      .toList
      .headOption
  }

  def utf8(text: String): Array[Byte] = text.getBytes(StandardCharsets.UTF_8)
}
