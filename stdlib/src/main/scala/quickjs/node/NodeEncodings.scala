package quickjs.node

import java.nio.charset.StandardCharsets
import java.util.Base64

/** Shared binary encoding helpers used by Buffer and fs. */
private[node] object NodeEncodings {

  def normalize(label: String): String =
    label.toLowerCase.replace("_", "").replace("-", "") match {
      case "utf8"                                  => "utf8"
      case "utf16le" | "ucs2" | "ucs2le" | "utf16" => "utf16le"
      case "latin1" | "binary"                     => "latin1"
      case "ascii"                                 => "ascii"
      case "base64"                                => "base64"
      case "base64url"                             => "base64url"
      case "hex"                                   => "hex"
      case other                                   => other
    }

  def bytesFromString(text: String, encoding: String): Array[Byte] =
    normalize(encoding) match {
      case "utf8" => text.getBytes(StandardCharsets.UTF_8)
      case "latin1" =>
        val out = new Array[Byte](text.length)
        var i = 0
        while i < text.length do {
          out(i) = (text.charAt(i) & 0xff).toByte
          i += 1
        }
        out
      case "ascii" =>
        val out = new Array[Byte](text.length)
        var i = 0
        while i < text.length do {
          out(i) = (text.charAt(i) & 0x7f).toByte
          i += 1
        }
        out
      case "utf16le" =>
        val out = new Array[Byte](text.length * 2)
        var i = 0
        while i < text.length do {
          val c = text.charAt(i)
          out(i * 2) = (c & 0xff).toByte
          out(i * 2 + 1) = ((c >> 8) & 0xff).toByte
          i += 1
        }
        out
      case "hex" =>
        val cleaned = text.trim
        if cleaned.length % 2 != 0 then
          throw new IllegalArgumentException("Invalid hexadecimal string")
        val out = new Array[Byte](cleaned.length / 2)
        var i = 0
        while i < out.length do {
          val hi = Character.digit(cleaned.charAt(i * 2), 16)
          val lo = Character.digit(cleaned.charAt(i * 2 + 1), 16)
          if hi < 0 || lo < 0 then
            throw new IllegalArgumentException("Invalid hexadecimal string")
          out(i) = ((hi << 4) | lo).toByte
          i += 1
        }
        out
      case "base64" | "base64url" =>
        val normalized = text
          .replace('-', '+')
          .replace('_', '/')
          .filterNot(c => c == '\n' || c == '\r' || c == ' ' || c == '\t')
        val padded = normalized + ("=" * ((4 - normalized.length % 4) % 4))
        Base64.getDecoder.decode(padded)
      case other =>
        throw new IllegalArgumentException(s"Unknown encoding: $other")
    }

  def stringFromBytes(bytes: Array[Byte], encoding: String): String =
    normalize(encoding) match {
      case "utf8" => new String(bytes, StandardCharsets.UTF_8)
      case "latin1" =>
        val sb = new StringBuilder(bytes.length)
        bytes.foreach(b => sb.append((b & 0xff).toChar))
        sb.toString
      case "ascii" =>
        val sb = new StringBuilder(bytes.length)
        bytes.foreach(b => sb.append((b & 0x7f).toChar))
        sb.toString
      case "utf16le" =>
        val sb = new StringBuilder(bytes.length / 2)
        var i = 0
        while i + 1 < bytes.length do {
          sb.append(((bytes(i) & 0xff) | ((bytes(i + 1) & 0xff) << 8)).toChar)
          i += 2
        }
        sb.toString
      case "hex" =>
        val sb = new StringBuilder(bytes.length * 2)
        bytes.foreach(b => sb.append(f"${b & 0xff}%02x"))
        sb.toString
      case "base64"    => Base64.getEncoder.encodeToString(bytes)
      case "base64url" => Base64.getUrlEncoder.withoutPadding.encodeToString(bytes)
      case other =>
        throw new IllegalArgumentException(s"Unknown encoding: $other")
    }
}
