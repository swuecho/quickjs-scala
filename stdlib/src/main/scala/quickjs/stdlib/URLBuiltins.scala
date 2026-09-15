package quickjs.stdlib

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.JSObject
import scala.collection.mutable
import java.util.Locale

/** WHATWG `URL` and `URLSearchParams` host globals.
  *
  * The URL parser follows the WHATWG URL Standard: special schemes with
  * default-port elision, percent-encoding sets, host parsing (IDNA via a
  * punycode implementation, IPv4 and IPv6), relative resolution against a
  * base URL, file URLs with Windows drive letters, and opaque paths for
  * cannot-be-a-base URLs. `URLSearchParams` implements the
  * application/x-www-form-urlencoded parser and serializer and is live with
  * respect to its owning `URL` (the same object is returned from
  * `url.searchParams`, and `url.search`/`url.href` assignments re-parse it).
  */
object URLBuiltins {

  // =====================================================================
  // URL record
  // =====================================================================

  private final class URLRecord(
      var scheme: String,
      var username: String,
      var password: String,
      var host: Option[String],
      var port: Option[Int],
      var path: mutable.ArrayBuffer[String],
      var query: Option[String],
      var fragment: Option[String],
      var opaquePath: Boolean
  ) {
    def copyOf(): URLRecord =
      new URLRecord(
        scheme,
        username,
        password,
        host,
        port,
        path.clone(),
        query,
        fragment,
        opaquePath
      )
  }

  private object URLRecord {
    def empty: URLRecord =
      new URLRecord(
        "",
        "",
        "",
        None,
        None,
        mutable.ArrayBuffer.empty,
        None,
        None,
        false
      )
  }

  private object ParseFailure
      extends RuntimeException(null, null, false, false)
  private object ParseStop extends RuntimeException(null, null, false, false)

  // =====================================================================
  // Character helpers
  // =====================================================================

  private def isAsciiDigit(c: Int): Boolean = c >= '0' && c <= '9'
  private def isAsciiAlpha(c: Int): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  private def isAsciiAlphanumeric(c: Int): Boolean =
    isAsciiDigit(c) || isAsciiAlpha(c)
  private def isAsciiHex(c: Int): Boolean =
    isAsciiDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  private def hexValue(c: Int): Int =
    if c >= '0' && c <= '9' then c - '0'
    else if c >= 'a' && c <= 'f' then c - 'a' + 10
    else c - 'A' + 10
  private def lowerAscii(c: Int): Char =
    if c >= 'A' && c <= 'Z' then (c + 32).toChar else c.toChar

  private def codePointCountOf(s: CharSequence): Int = {
    var count = 0
    var i = 0
    while i < s.length do {
      val c = s.charAt(i)
      i += (if Character.isHighSurrogate(c) && i + 1 < s.length
            && Character.isLowSurrogate(s.charAt(i + 1)) then 2
            else 1)
      count += 1
    }
    count
  }

  // =====================================================================
  // Percent encoding / decoding
  // =====================================================================

  private def isC0ControlPercentEncode(c: Int): Boolean =
    c <= 0x1F || c > 0x7E
  private def isFragmentPercentEncode(c: Int): Boolean =
    isC0ControlPercentEncode(c) || c == ' ' || c == '"' || c == '<' || c == '>' || c == '`'
  private def isQueryPercentEncode(c: Int): Boolean =
    isC0ControlPercentEncode(c) || c == ' ' || c == '"' || c == '#' || c == '<' || c == '>'
  private def isSpecialQueryPercentEncode(c: Int): Boolean =
    isQueryPercentEncode(c) || c == '\''
  private def isPathPercentEncode(c: Int): Boolean =
    isQueryPercentEncode(c) || c == '?' || c == '`' || c == '{' || c == '}'
  private def isUserinfoPercentEncode(c: Int): Boolean =
    isPathPercentEncode(c) || c == '/' || c == ':' || c == ';' || c == '=' || c == '@' ||
      (c >= '[' && c <= '^') || c == '|'
  private def isFormPercentEncode(c: Int): Boolean =
    !(isAsciiAlphanumeric(c) || c == '*' || c == '-' || c == '.' || c == '_')

  /** WHATWG UTF-8 encode of a single code point; lone surrogates become the
    * replacement character (encoded as EF BF BD).
    */
  private def utf8Bytes(cp: Int): Array[Byte] = {
    if cp < 0x80 then Array(cp.toByte)
    else if cp < 0x800 then
      Array((0xc0 | (cp >> 6)).toByte, (0x80 | (cp & 0x3f)).toByte)
    else if cp >= 0xd800 && cp <= 0xdfff then
      Array(0xef.toByte, 0xbf.toByte, 0xbd.toByte)
    else if cp < 0x10000 then
      Array(
        (0xe0 | (cp >> 12)).toByte,
        (0x80 | ((cp >> 6) & 0x3f)).toByte,
        (0x80 | (cp & 0x3f)).toByte
      )
    else
      Array(
        (0xf0 | (cp >> 18)).toByte,
        (0x80 | ((cp >> 12) & 0x3f)).toByte,
        (0x80 | ((cp >> 6) & 0x3f)).toByte,
        (0x80 | (cp & 0x3f)).toByte
      )
  }

  private def percentEncodeCodePoint(cp: Int): String = {
    val bytes = utf8Bytes(cp)
    val sb = new StringBuilder
    var i = 0
    while i < bytes.length do {
      val b = bytes(i) & 0xff
      sb.append('%')
      sb.append("0123456789ABCDEF".charAt(b >> 4))
      sb.append("0123456789ABCDEF".charAt(b & 0xf))
      i += 1
    }
    sb.toString
  }

  private def percentEncode(cp: Int, inSet: Int => Boolean): String =
    if inSet(cp) then percentEncodeCodePoint(cp)
    else new String(Character.toChars(cp))

  private def encodeCodePoints(s: CharSequence, inSet: Int => Boolean): String = {
    val sb = new StringBuilder
    val cps = s.toString.codePoints().toArray
    var i = 0
    while i < cps.length do {
      sb.append(percentEncode(cps(i), inSet))
      i += 1
    }
    sb.toString
  }

  /** UTF-8 percent-decode: `%XX` escapes become bytes; everything else is
    * encoded as UTF-8 bytes; the byte sequence is decoded as UTF-8 with
    * replacement.
    */
  private def percentDecode(input: String): String = {
    val out = new java.io.ByteArrayOutputStream()
    val cps = input.codePoints().toArray
    var i = 0
    while i < cps.length do {
      val c = cps(i)
      if c == '%' && i + 2 < cps.length && isAsciiHex(cps(i + 1)) && isAsciiHex(cps(i + 2)) then {
        out.write((hexValue(cps(i + 1)) << 4) | hexValue(cps(i + 2)))
        i += 3
      } else {
        val bytes = utf8Bytes(c)
        out.write(bytes, 0, bytes.length)
        i += 1
      }
    }
    new String(out.toByteArray, java.nio.charset.StandardCharsets.UTF_8)
  }

  // =====================================================================
  // Host parsing
  // =====================================================================

  private def containsForbiddenHostCodePoint(s: String): Boolean =
    s.exists(c =>
      c == '\u0000' || c == '\u0009' || c == '\u000A' || c == '\u000D' ||
        c == '\u0020' || c == '#' || c == '/' || c == ':' || c == '<' ||
        c == '>' || c == '?' || c == '@' || c == '[' || c == '\\' ||
        c == ']' || c == '^' || c == '|'
    )

  private def containsForbiddenDomainCodePoint(s: String): Boolean =
    s.exists(c =>
      c <= '\u001f' || c == '%' || containsForbiddenHostCodePoint(c.toString)
    )

  private def containsForbiddenHostCodePointExcludingPercent(s: String): Boolean =
    s.exists(c =>
      c == '\u0000' || c == '\u0009' || c == '\u000A' || c == '\u000D' ||
        c == '\u0020' || c == '#' || c == '/' || c == ':' || c == '<' ||
        c == '>' || c == '?' || c == '@' || c == '[' || c == '\\' ||
        c == ']' || c == '^' || c == '|'
    )

  // --- UTS #46 (domain to ASCII) via ICU ---

  private val idna: com.ibm.icu.text.IDNA = {
    val options = com.ibm.icu.text.IDNA.CHECK_BIDI |
      com.ibm.icu.text.IDNA.CHECK_CONTEXTJ |
      com.ibm.icu.text.IDNA.NONTRANSITIONAL_TO_ASCII
    com.ibm.icu.text.IDNA.getUTS46Instance(options)
  }

  /** Error types that make `domain to ASCII` fail. tr46 (the WHATWG
    * reference implementation) does not reject empty labels, hyphens
    * (checkHyphens is false) or CONTEXTO checks, so neither do we.
    */
  private val fatalIDNAErrors: Set[com.ibm.icu.text.IDNA.Error] = {
    import com.ibm.icu.text.IDNA.Error.*
    Set(
      DISALLOWED,
      PUNYCODE,
      INVALID_ACE_LABEL,
      LABEL_HAS_DOT,
      LEADING_COMBINING_MARK,
      BIDI,
      CONTEXTJ
    )
  }

  /** `domain to ASCII`: UTS #46 non-transitional processing with CHECK_BIDI
    * and CHECK_CONTEXTJ, matching the URL Standard's parameters.
    */
  private def domainToASCII(domain: String): Option[String] = {
    if domain.isEmpty then return None
    val info = new com.ibm.icu.text.IDNA.Info
    val out = new java.lang.StringBuilder
    try idna.nameToASCII(domain, out, info)
    catch case _: RuntimeException => return None
    if fatalIDNAErrors.exists(info.getErrors.contains) then None
    else {
      val ascii = out.toString
      if ascii.isEmpty then None else Some(ascii)
    }
  }


  private def parseIPv4Number(input0: String): Option[Long] = {
    if input0.isEmpty then return None
    var input = input0
    var radix = 10
    if input.length >= 2 && input.charAt(0) == '0' &&
      (input.charAt(1) == 'x' || input.charAt(1) == 'X')
    then {
      input = input.substring(2)
      radix = 16
    } else if input.length >= 2 && input.charAt(0) == '0' then {
      input = input.substring(1)
      radix = 8
    }
    if input.isEmpty then Some(0L)
    else if radix == 10 && !input.forall(isAsciiDigit) then None
    else if radix == 16 && !input.forall(isAsciiHex) then None
    else if radix == 8 && !input.forall(c => c >= '0' && c <= '7') then None
    else
      try Some(java.lang.Long.parseLong(input, radix))
      catch case _: NumberFormatException => None
  }

  private def parseIPv4(input: String): Option[String] = {
    var parts = input.split("\\.", -1).toList
    if parts.nonEmpty && parts.last.isEmpty && parts.length > 1 then
      parts = parts.dropRight(1)
    if parts.length > 4 then return None
    val numbers = mutable.ArrayBuffer.empty[Long]
    var idx = 0
    while idx < parts.length do {
      val p = parts(idx)
      if p.isEmpty then return None
      parseIPv4Number(p) match {
        case Some(n) => numbers += n
        case None    => return None
      }
      idx += 1
    }
    var i = 0
    while i < numbers.length - 1 do {
      if numbers(i) > 255 then return None
      i += 1
    }
    val limit = math.pow(256.0, (5 - numbers.length).toDouble).toLong
    if numbers.last >= limit then return None
    var ipv4 = numbers.last
    var counter = 3
    i = 0
    while i < numbers.length - 1 do {
      ipv4 += numbers(i) * math.pow(256.0, counter.toDouble).toLong
      counter -= 1
      i += 1
    }
    Some(
      s"${((ipv4 >> 24) & 255).toInt}.${((ipv4 >> 16) & 255).toInt}." +
        s"${((ipv4 >> 8) & 255).toInt}.${(ipv4 & 255).toInt}"
    )
  }

  private def endsInANumber(input: String): Boolean = {
    val parts = input.split("\\.", -1)
    if parts.isEmpty then return false
    var last = parts(parts.length - 1)
    if last.isEmpty then {
      if parts.length == 1 then return false
      last = parts(parts.length - 2)
    }
    if last.nonEmpty && last.forall(isAsciiDigit) then true
    else parseIPv4Number(last).isDefined
  }

  private def parseIPv6(input: String): Option[Array[Int]] = {
    val address = new Array[Int](8)
    var pieceIndex = 0
    var compress = -1
    var pointer = 0
    val cps = input.codePoints().toArray
    def at(i: Int): Int = if i >= 0 && i < cps.length then cps(i) else -1

    if at(pointer) == ':' then {
      if at(pointer + 1) != ':' then return None
      pointer += 2
      pieceIndex += 1
      compress = pieceIndex
    }

    while pointer < cps.length do {
      if pieceIndex == 8 then return None
      if at(pointer) == ':' then {
        if compress != -1 then return None
        pointer += 1
        pieceIndex += 1
        compress = pieceIndex
      } else {
        var value = 0
        var length = 0
        while length < 4 && isAsciiHex(at(pointer)) do {
          value = value * 0x10 + hexValue(at(pointer))
          pointer += 1
          length += 1
        }

        if at(pointer) == '.' then {
          if length == 0 then return None
          pointer -= length
          if pieceIndex > 6 then return None
          var numbersSeen = 0
          while at(pointer) != -1 do {
            var ipv4Piece: Int = -1
            if numbersSeen > 0 then {
              if at(pointer) == '.' && numbersSeen < 4 then pointer += 1
              else return None
            }
            if !isAsciiDigit(at(pointer)) then return None
            while isAsciiDigit(at(pointer)) do {
              val number = at(pointer) - '0'
              if ipv4Piece == -1 then ipv4Piece = number
              else if ipv4Piece == 0 then return None
              else ipv4Piece = ipv4Piece * 10 + number
              if ipv4Piece > 255 then return None
              pointer += 1
            }
            address(pieceIndex) = address(pieceIndex) * 0x100 + ipv4Piece
            numbersSeen += 1
            if numbersSeen == 2 || numbersSeen == 4 then pieceIndex += 1
          }
          if numbersSeen != 4 then return None
          // Exit the outer loop; the address is complete.
          pointer = cps.length
        } else {
          if at(pointer) == ':' then {
            pointer += 1
            if at(pointer) == -1 then return None
          } else if at(pointer) != -1 then return None
          address(pieceIndex) = value
          pieceIndex += 1
        }
      }
    }

    if compress != -1 then {
      var swaps = pieceIndex - compress
      pieceIndex = 7
      while pieceIndex != 0 && swaps > 0 do {
        val temp = address(compress + swaps - 1)
        address(compress + swaps - 1) = address(pieceIndex)
        address(pieceIndex) = temp
        pieceIndex -= 1
        swaps -= 1
      }
    } else if pieceIndex != 8 then return None

    Some(address)
  }

  private def findLongestZeroSequence(address: Array[Int]): (Int, Int) = {
    var maxIdx = -1
    var maxLen = 1
    var currStart = -1
    var currLen = 0
    var i = 0
    while i < address.length do {
      if address(i) != 0 then {
        if currLen > maxLen then {
          maxIdx = currStart
          maxLen = currLen
        }
        currStart = -1
        currLen = 0
      } else {
        if currStart == -1 then currStart = i
        currLen += 1
      }
      i += 1
    }
    if currLen > maxLen then {
      maxIdx = currStart
      maxLen = currLen
    }
    (maxIdx, maxLen)
  }

  private def serializeIPv6(address: Array[Int]): String = {
    val out = new StringBuilder
    val (compress, _) = findLongestZeroSequence(address)
    var ignore0 = false
    var pieceIndex = 0
    while pieceIndex <= 7 do {
      if ignore0 && address(pieceIndex) == 0 then ()
      else {
        if ignore0 then ignore0 = false
        if compress == pieceIndex then {
          out.append(if pieceIndex == 0 then "::" else ":")
          ignore0 = true
        } else {
          out.append(Integer.toHexString(address(pieceIndex)))
          if pieceIndex != 7 then out.append(':')
        }
      }
      pieceIndex += 1
    }
    out.toString
  }

  private def parseOpaqueHost(input: String): Option[String] = {
    if containsForbiddenHostCodePointExcludingPercent(input) then None
    else Some(encodeCodePoints(input, isC0ControlPercentEncode))
  }

  private def parseHost(input: String, special: Boolean): Option[String] = {
    if input.startsWith("[") then {
      if !input.endsWith("]") then None
      else
        parseIPv6(input.substring(1, input.length - 1))
          .map(a => "[" + serializeIPv6(a) + "]")
    } else if !special then parseOpaqueHost(input)
    else {
      var domain = percentDecode(input)
      // The URL Standard percent-decodes hosts with BOM removal.
      if domain.startsWith("\uFEFF") then domain = domain.substring(1)
      if containsForbiddenDomainCodePoint(domain) then None
      else
        domainToASCII(domain) match {
          case None => None
          case Some(ascii) =>
            if containsForbiddenDomainCodePoint(ascii) then None
            else if endsInANumber(ascii) then parseIPv4(ascii)
            else Some(ascii)
        }
    }
  }

  // =====================================================================
  // Special schemes and path helpers
  // =====================================================================

  private val specialSchemes: Map[String, Option[Int]] = Map(
    "ftp" -> Some(21),
    "file" -> None,
    "http" -> Some(80),
    "https" -> Some(443),
    "ws" -> Some(80),
    "wss" -> Some(443)
  )

  private def isSpecialScheme(scheme: String): Boolean =
    specialSchemes.contains(scheme)

  private def defaultPort(scheme: String): Option[Int] =
    specialSchemes.getOrElse(scheme, None)

  private def isSingleDot(buffer: String): Boolean =
    buffer == "." || buffer.equalsIgnoreCase("%2e")

  private def isDoubleDot(buffer: String): Boolean = {
    val b = buffer.toLowerCase(Locale.ROOT)
    b == ".." || b == "%2e." || b == ".%2e" || b == "%2e%2e"
  }

  private def isWindowsDriveLetterString(s: String): Boolean =
    s.length == 2 && isAsciiAlpha(s.charAt(0)) &&
      (s.charAt(1) == ':' || s.charAt(1) == '|')

  private def isNormalizedWindowsDriveLetter(s: String): Boolean =
    s.length == 2 && isAsciiAlpha(s.charAt(0)) && s.charAt(1) == ':'

  /** "Starts with a Windows drive letter": the first two code points are
    * alpha + `:`/`|` and the third is EOF, `/`, `\`, `?` or `#`.
    */
  private def startsWithWindowsDriveLetter(cps: Array[Int], pointer: Int): Boolean =
    pointer + 1 < cps.length && isAsciiAlpha(cps(pointer)) &&
      (cps(pointer + 1) == ':' || cps(pointer + 1) == '|') &&
      (pointer + 2 >= cps.length || cps(pointer + 2) == '/' ||
        cps(pointer + 2) == '\\' || cps(pointer + 2) == '?' ||
        cps(pointer + 2) == '#')

  private def shortenPath(url: URLRecord): Unit = {
    if url.path.isEmpty then ()
    else if url.scheme == "file" && url.path.length == 1 &&
      isNormalizedWindowsDriveLetter(url.path.head)
    then ()
    else url.path.remove(url.path.length - 1)
  }

  private def cannotHaveAUsernamePasswordPort(url: URLRecord): Boolean =
    url.host.isEmpty || url.host.contains("") || url.opaquePath || url.scheme == "file"

  // =====================================================================
  // The basic URL parser
  // =====================================================================

  private def trimC0ControlOrSpace(input: String): String = {
    var start = 0
    var end = input.length
    while start < end && (input.charAt(start) <= ' ' ) do start += 1
    while end > start && (input.charAt(end - 1) <= ' ') do end -= 1
    input.substring(start, end)
  }

  private def removeTabAndNewline(input: String): String =
    if !input.exists(c => c == '\t' || c == '\n' || c == '\r') then input
    else input.filterNot(c => c == '\t' || c == '\n' || c == '\r')

  /** Runs the URL state machine over `input`, mutating `url`. Returns false
    * when the input fails to parse.
    */
  private def basicURLParse(
      input0: String,
      base: Option[URLRecord],
      url: URLRecord,
      stateOverride: Option[String],
      trimControls: Boolean
  ): Boolean = {
    try {
      var input = input0
      if trimControls then input = trimC0ControlOrSpace(input)
      input = removeTabAndNewline(input)
      val cps = input.codePoints().toArray
      var pointer = 0
      var state = stateOverride.getOrElse("scheme start")
      val buffer = new StringBuilder
      // Opaque paths accumulate here and are flushed into the record when the
      // path ends (avoiding quadratic string concatenation for data: URLs).
      val opaqueBuffer = new StringBuilder
      var parsedOpaquePath = false
      def flushOpaquePath(): Unit =
        if parsedOpaquePath && url.path.nonEmpty then
          url.path(0) = opaqueBuffer.toString
      var atFlag = false
      var arrFlag = false
      var passwordTokenSeenFlag = false

      def at(i: Int): Int = if i >= 0 && i < cps.length then cps(i) else -1
      def nextIsSlashSlash: Boolean =
        at(pointer) == '/' && at(pointer + 1) == '/'

      while pointer <= cps.length do {
        val c = at(pointer)
        val cStr = if c == -1 then "" else new String(Character.toChars(c))

        state match {
          case "scheme start" =>
            if isAsciiAlpha(c) then {
              buffer.append(lowerAscii(c))
              state = "scheme"
            } else if stateOverride.isEmpty then {
              state = "no scheme"
              pointer -= 1
            } else throw ParseFailure

          case "scheme" =>
            if isAsciiAlphanumeric(c) || c == '+' || c == '-' || c == '.' then
              buffer.append(lowerAscii(c))
            else if c == ':' then {
              val schemeText = buffer.toString
              if stateOverride.isDefined then {
                if isSpecialScheme(url.scheme) && !isSpecialScheme(schemeText) then
                  throw ParseStop
                if !isSpecialScheme(url.scheme) && isSpecialScheme(schemeText) then
                  throw ParseStop
                if (url.username.nonEmpty || url.password.nonEmpty ||
                  url.port.isDefined) && schemeText == "file"
                then throw ParseStop
                if url.scheme == "file" && url.host.contains("") then
                  throw ParseStop
              }
              url.scheme = schemeText
              if stateOverride.isDefined then {
                // Changing the scheme to one whose default port matches the
                // current port drops the explicit port.
                if url.port == defaultPort(url.scheme) then url.port = None
                throw ParseStop
              }
              buffer.clear()
              if url.scheme == "file" then state = "file"
              else if isSpecialScheme(url.scheme) &&
                base.exists(_.scheme == url.scheme)
              then state = "special relative or authority"
              else if isSpecialScheme(url.scheme) then
                state = "special authority slashes"
              else if at(pointer + 1) == '/' then {
                state = "path or authority"
                pointer += 1
              } else {
                url.opaquePath = true
                url.path.append("")
                parsedOpaquePath = true
                state = "opaque path"
              }
            } else if stateOverride.isEmpty then {
              buffer.clear()
              state = "no scheme"
              pointer = -1
            } else throw ParseFailure

          case "no scheme" =>
            if base.isEmpty || (base.get.opaquePath && c != '#') then
              throw ParseFailure
            else if base.get.opaquePath && c == '#' then {
              url.scheme = base.get.scheme
              url.path = base.get.path.clone()
              url.query = base.get.query
              url.fragment = Some("")
              url.opaquePath = true
              state = "fragment"
            } else if base.get.scheme == "file" then {
              state = "file"
              pointer -= 1
            } else {
              state = "relative"
              pointer -= 1
            }

          case "special relative or authority" =>
            if nextIsSlashSlash then {
              state = "special authority ignore slashes"
              pointer += 1
            } else {
              state = "relative"
              pointer -= 1
            }

          case "path or authority" =>
            if c == '/' then state = "authority"
            else {
              state = "path"
              pointer -= 1
            }

          case "relative" =>
            url.scheme = base.get.scheme
            if c == -1 then {
              url.username = base.get.username
              url.password = base.get.password
              url.host = base.get.host
              url.port = base.get.port
              url.path = base.get.path.clone()
              url.query = base.get.query
            } else if c == '/' then state = "relative slash"
            else if c == '?' then {
              url.username = base.get.username
              url.password = base.get.password
              url.host = base.get.host
              url.port = base.get.port
              url.path = base.get.path.clone()
              url.query = Some("")
              state = "query"
            } else if c == '#' then {
              url.username = base.get.username
              url.password = base.get.password
              url.host = base.get.host
              url.port = base.get.port
              url.path = base.get.path.clone()
              url.query = base.get.query
              url.fragment = Some("")
              state = "fragment"
            } else if isSpecialScheme(url.scheme) && c == '\\' then
              state = "relative slash"
            else {
              url.username = base.get.username
              url.password = base.get.password
              url.host = base.get.host
              url.port = base.get.port
              url.path = base.get.path.clone()
              if url.path.nonEmpty then url.path.remove(url.path.length - 1)
              state = "path"
              pointer -= 1
            }

          case "relative slash" =>
            if isSpecialScheme(url.scheme) && (c == '/' || c == '\\') then
              state = "special authority ignore slashes"
            else if c == '/' then state = "authority"
            else {
              url.username = base.get.username
              url.password = base.get.password
              url.host = base.get.host
              url.port = base.get.port
              state = "path"
              pointer -= 1
            }

          case "special authority slashes" =>
            if nextIsSlashSlash then {
              state = "special authority ignore slashes"
              pointer += 1
            } else {
              state = "special authority ignore slashes"
              pointer -= 1
            }

          case "special authority ignore slashes" =>
            if c != '/' && c != '\\' then {
              state = "authority"
              pointer -= 1
            }

          case "authority" =>
            if c == '@' then {
              if atFlag then buffer.insert(0, "%40")
              atFlag = true
              val text = buffer.toString
              val len = text.codePointCount(0, text.length)
              var ci = 0
              var idx = 0
              while ci < len do {
                val cp = text.codePointAt(idx)
                if cp == ':' && !passwordTokenSeenFlag then
                  passwordTokenSeenFlag = true
                else {
                  val encoded = percentEncode(cp, isUserinfoPercentEncode)
                  if passwordTokenSeenFlag then url.password += encoded
                  else url.username += encoded
                }
                idx += Character.charCount(cp)
                ci += 1
              }
              buffer.clear()
            } else if c == -1 || c == '/' || c == '?' || c == '#' ||
              (isSpecialScheme(url.scheme) && c == '\\')
            then {
              if atFlag && buffer.isEmpty then throw ParseFailure
              pointer -= codePointCountOf(buffer) + 1
              buffer.clear()
              state = "host"
            } else buffer.append(cStr)

          case "host" | "hostname" =>
            if stateOverride.isDefined && url.scheme == "file" then {
              pointer -= 1
              state = "file host"
            } else if c == ':' && !arrFlag then {
              if buffer.isEmpty then throw ParseFailure
              // Setting `hostname` stops at the first colon without touching
              // the existing host.
              if stateOverride.contains("hostname") then throw ParseStop
              parseHost(buffer.toString, isSpecialScheme(url.scheme)) match {
                case Some(h) => url.host = Some(h)
                case None    => throw ParseFailure
              }
              buffer.clear()
              state = "port"
            } else if c == -1 || c == '/' || c == '?' || c == '#' ||
              (isSpecialScheme(url.scheme) && c == '\\')
            then {
              pointer -= 1
              if isSpecialScheme(url.scheme) && buffer.isEmpty then
                throw ParseFailure
              else if stateOverride.isDefined && buffer.isEmpty &&
                (url.username.nonEmpty || url.password.nonEmpty ||
                  url.port.isDefined)
              then throw ParseStop
              parseHost(buffer.toString, isSpecialScheme(url.scheme)) match {
                case Some(h) => url.host = Some(h)
                case None    => throw ParseFailure
              }
              buffer.clear()
              state = "path start"
              if stateOverride.isDefined then throw ParseStop
            } else {
              if c == '[' then arrFlag = true
              else if c == ']' then arrFlag = false
              buffer.append(cStr)
            }

          case "port" =>
            if isAsciiDigit(c) then buffer.append(cStr)
            else if c == -1 || c == '/' || c == '?' || c == '#' ||
              (isSpecialScheme(url.scheme) && c == '\\') ||
              stateOverride.isDefined
            then {
              if buffer.nonEmpty then {
                val text = buffer.toString
                if !text.forall(isAsciiDigit) then throw ParseFailure
                val port =
                  try text.toLong
                  catch case _: NumberFormatException => throw ParseFailure
                if port > 65535L then throw ParseFailure
                url.port =
                  if defaultPort(url.scheme).contains(port.toInt) then None
                  else Some(port.toInt)
                buffer.clear()
              }
              if stateOverride.isDefined then throw ParseStop
              state = "path start"
              pointer -= 1
            } else throw ParseFailure

          case "file" =>
            url.scheme = "file"
            url.host = Some("")
            if c == '/' || c == '\\' then state = "file slash"
            else if base.isDefined && base.get.scheme == "file" then {
              url.host = base.get.host
              url.path = base.get.path.clone()
              url.query = base.get.query
              if c == '?' then {
                url.query = Some("")
                state = "query"
              } else if c == '#' then {
                url.fragment = Some("")
                state = "fragment"
              } else if c != -1 then {
                url.query = None
                if !startsWithWindowsDriveLetter(cps, pointer) then shortenPath(url)
                else url.path.clear()
                state = "path"
                pointer -= 1
              }
            } else {
              state = "path"
              pointer -= 1
            }

          case "file slash" =>
            if c == '/' || c == '\\' then state = "file host"
            else {
              if base.isDefined && base.get.scheme == "file" then {
                if !startsWithWindowsDriveLetter(cps, pointer) &&
                  base.get.path.nonEmpty &&
                  isNormalizedWindowsDriveLetter(base.get.path.head)
                then url.path.append(base.get.path.head)
                url.host = base.get.host
              }
              state = "path"
              pointer -= 1
            }

          case "file host" =>
            if c == -1 || c == '/' || c == '\\' || c == '?' || c == '#' then {
              pointer -= 1
              val text = buffer.toString
              if stateOverride.isEmpty && isWindowsDriveLetterString(text) then
                state = "path"
              else if text.isEmpty then {
                url.host = Some("")
                if stateOverride.isDefined then throw ParseStop
                state = "path start"
              } else {
                val parsed = parseHost(text, special = true) match {
                  case Some(h) => h
                  case None    => throw ParseFailure
                }
                url.host = Some(if parsed == "localhost" then "" else parsed)
                if stateOverride.isDefined then throw ParseStop
                buffer.clear()
                state = "path start"
              }
            } else buffer.append(cStr)

          case "path start" =>
            if isSpecialScheme(url.scheme) then {
              state = "path"
              if c != '/' && c != '\\' then pointer -= 1
            } else if stateOverride.isEmpty && c == '?' then {
              url.query = Some("")
              state = "query"
            } else if stateOverride.isEmpty && c == '#' then {
              url.fragment = Some("")
              state = "fragment"
            } else if c != -1 then {
              state = "path"
              if c != '/' then pointer -= 1
            } else if stateOverride.isDefined && url.host.isEmpty then
              url.path.append("")

          case "path" =>
            if c == -1 || c == '/' || (isSpecialScheme(url.scheme) && c == '\\') ||
              (stateOverride.isEmpty && (c == '?' || c == '#'))
            then {
              val buf = buffer.toString
              if isDoubleDot(buf) then {
                shortenPath(url)
                if c != '/' && !(isSpecialScheme(url.scheme) && c == '\\') then
                  url.path.append("")
              } else if isSingleDot(buf) && c != '/' &&
                !(isSpecialScheme(url.scheme) && c == '\\')
              then url.path.append("")
              else if !isSingleDot(buf) then {
                if url.scheme == "file" && url.path.isEmpty &&
                  isWindowsDriveLetterString(buf)
                then buffer.setCharAt(1, ':')
                url.path.append(buffer.toString)
              }
              buffer.clear()
              if c == '?' then {
                url.query = Some("")
                state = "query"
              }
              if c == '#' then {
                url.fragment = Some("")
                state = "fragment"
              }
            } else buffer.append(percentEncode(c, isPathPercentEncode))

          case "opaque path" =>
            if c == '?' then {
              flushOpaquePath()
              url.query = Some("")
              state = "query"
            } else if c == '#' then {
              flushOpaquePath()
              url.fragment = Some("")
              state = "fragment"
            } else if c != -1 then {
              opaqueBuffer.append(percentEncode(c, isC0ControlPercentEncode))
            }

          case "query" =>
            if c == -1 || (stateOverride.isEmpty && c == '#') then {
              if buffer.nonEmpty then {
                val inSet: Int => Boolean =
                  if isSpecialScheme(url.scheme) then isSpecialQueryPercentEncode
                  else isQueryPercentEncode
                url.query = Some(
                  url.query.getOrElse("") +
                    encodeCodePoints(buffer.toString, inSet)
                )
                buffer.clear()
              }
              if c == '#' then {
                url.fragment = Some("")
                state = "fragment"
              }
            } else buffer.append(cStr)

          case "fragment" =>
            if c != -1 then
              url.fragment = Some(
                url.fragment.getOrElse("") +
                  percentEncode(c, isFragmentPercentEncode)
              )

          case other =>
            throw ParseFailure
        }
        pointer += 1
      }
      flushOpaquePath()
      true
    } catch {
      case ParseFailure => false
      case ParseStop    => true
    }
  }

  private def parseURL(input: String, base: Option[URLRecord]): Option[URLRecord] = {
    val record = URLRecord.empty
    if basicURLParse(input, base, record, None, trimControls = true) then Some(record)
    else None
  }

  /** Parse `input` with a state override applied to a copy of `record`;
    * commits on success.
    */
  private def applyOverride(
      input: String,
      record: URLRecord,
      state: String
  ): Boolean = {
    val clone = record.copyOf()
    if basicURLParse(input, None, clone, Some(state), trimControls = false) then {
      record.scheme = clone.scheme
      record.username = clone.username
      record.password = clone.password
      record.host = clone.host
      record.port = clone.port
      record.path = clone.path.clone()
      record.query = clone.query
      record.fragment = clone.fragment
      record.opaquePath = clone.opaquePath
      true
    } else false
  }

  private def serializeURL(url: URLRecord, excludeFragment: Boolean): String = {
    val out = new StringBuilder(url.scheme)
    out.append(':')
    url.host match {
      case Some(h) =>
        out.append("//")
        if url.username.nonEmpty || url.password.nonEmpty then {
          out.append(url.username)
          if url.password.nonEmpty then {
            out.append(':')
            out.append(url.password)
          }
          out.append('@')
        }
        out.append(h)
        url.port.foreach { p =>
          out.append(':')
          out.append(p)
        }
      case None =>
        if url.scheme == "file" then out.append("//")
    }
    if url.opaquePath then out.append(if url.path.isEmpty then "" else url.path.head)
    else {
      if url.host.isEmpty && url.path.length > 1 && url.path.head.isEmpty then
        out.append("/.")
      var i = 0
      while i < url.path.length do {
        out.append('/')
        out.append(url.path(i))
        i += 1
      }
    }
    url.query.foreach { q =>
      out.append('?')
      out.append(q)
    }
    if !excludeFragment then
      url.fragment.foreach { f =>
        out.append('#')
        out.append(f)
      }
    out.toString
  }

  private def originOf(url: URLRecord): String = {
    url.scheme match {
      case "blob" =>
        val inner =
          if url.path.nonEmpty then parseURL(url.path.head, None) else None
        inner match {
          case Some(r) if r.scheme == "http" || r.scheme == "https" =>
            originOf(r)
          case _ => "null"
        }
      case "ftp" | "http" | "https" | "ws" | "wss" =>
        val host = url.host.getOrElse("")
        val port = url.port.map(":" + _).getOrElse("")
        s"${url.scheme}://$host$port"
      case _ => "null"
    }
  }

  // =====================================================================
  // URLSearchParams data
  // =====================================================================

  private final class SearchParamsData(
      var pairs: mutable.ArrayBuffer[(String, String)],
      var url: Option[JSValue]
  )

  private def parseFormURLEncoded(input: String): mutable.ArrayBuffer[(String, String)] = {
    val out = mutable.ArrayBuffer.empty[(String, String)]
    if input.isEmpty then return out
    val sequences = input.split("&", -1)
    var i = 0
    while i < sequences.length do {
      val seq = sequences(i)
      if seq.nonEmpty then {
        val eq = seq.indexOf('=')
        if eq < 0 then out += ((decodeFormComponent(seq), ""))
        else
          out += ((
            decodeFormComponent(seq.substring(0, eq)),
            decodeFormComponent(seq.substring(eq + 1))
          ))
      }
      i += 1
    }
    out
  }

  private def decodeFormComponent(s: String): String =
    percentDecode(if s.indexOf('+') >= 0 then s.replace('+', ' ') else s)

  private def encodeFormComponent(s: String): String = {
    val out = new StringBuilder
    val cps = s.codePoints().toArray
    var i = 0
    while i < cps.length do {
      val cp = cps(i)
      if cp == ' ' then out.append('+')
      else if isFormPercentEncode(cp) then out.append(percentEncodeCodePoint(cp))
      else out.append(new String(Character.toChars(cp)))
      i += 1
    }
    out.toString
  }

  private def serializeFormURLEncoded(
      pairs: mutable.ArrayBuffer[(String, String)]
  ): String = {
    val out = new StringBuilder
    var i = 0
    while i < pairs.length do {
      if i > 0 then out.append('&')
      out.append(encodeFormComponent(pairs(i)._1))
      out.append('=')
      out.append(encodeFormComponent(pairs(i)._2))
      i += 1
    }
    out.toString
  }

  private def searchParamsDataOf(value: JSValue)(using ctx: JSContext): JSObject = {
    value match {
      case JSValue.Object(obj)
          if obj.getOwnProperty("__uspData").exists(_.isInstanceOf[JSValue.Native]) =>
        obj
      case _ =>
        ctx.throwTypeError("Value is not a URLSearchParams object")
    }
  }

  private def dataOf(obj: JSObject)(using ctx: JSContext): SearchParamsData =
    obj.getOwnProperty("__uspData") match {
      case Some(JSValue.Native(d: SearchParamsData)) => d
      case _ => throw new IllegalStateException("missing URLSearchParams data")
    }

  private def updateOwningURL(data: SearchParamsData)(using ctx: JSContext): Unit = {
    data.url.foreach {
      case JSValue.Object(urlObj) =>
        urlObj.getOwnProperty("__urlRecord") match {
          case Some(JSValue.Native(record: URLRecord)) =>
            record.query = Some(serializeFormURLEncoded(data.pairs))
          case _ => ()
        }
      case _ => ()
    }
  }

  // =====================================================================
  // Initialization
  // =====================================================================

  private final class SearchParamsHolder {
    var prototype: JSObject = null
  }

  def initialize()(using ctx: JSContext): Unit = {
    val holder = new SearchParamsHolder
    initializeSearchParams(holder)
    initializeURL(holder)
  }

  // --- URLSearchParams ---------------------------------------------------

  private def initializeSearchParams(
      holder: SearchParamsHolder
  )(using ctx: JSContext): Unit = {
    given JSContext = ctx
    // Object.prototype.toString optimization: URLSearchParams instances.
    val proto = JSObject(prototype = ctx.objectPrototype)
    holder.prototype = proto

    def initData(obj: JSObject, init: JSValue): Unit = {
      val pairs = collectInitPairs(init)
      obj.initProperty(
        "__uspData",
        JSValue.Native(new SearchParamsData(pairs, None)),
        enumerable = false,
        writable = false,
        configurable = false
      )
    }

    val ctor = NativeConstructor(
      name = "URLSearchParams",
      callImpl = (_, c) => {
        given JSContext = c
        c.throwTypeError("Constructor URLSearchParams requires 'new'")
      },
      constructImpl = (args, c) => {
        given JSContext = c
        val obj = JSObject(prototype = proto)
        initData(obj, args.headOption.getOrElse(JSValue.Undefined))
        JSValue.Object(obj)
      },
      prototype = proto,
      superInitImpl = Some((thisValue, args, c) => {
        given JSContext = c
        thisValue match {
          case JSValue.Object(obj) =>
            initData(obj, args.headOption.getOrElse(JSValue.Undefined))
            thisValue
          case _ =>
            c.throwTypeError("Constructor URLSearchParams requires 'new'")
        }
      })
    )
    BuiltinHelpers.initConstructor(ctor, length = 0)
    ctx.global.defineProperty(
      "URLSearchParams",
      JSValue.Native(ctor),
      enumerable = false,
      writable = true,
      configurable = true
    )

    def requiredArg(args: Array[JSValue], index: Int): JSValue =
      if args.length <= index then
        ctx.throwTypeError("URLSearchParams method called with too few arguments")
      else args(index)

    def appendPair(obj: JSObject, name: JSValue, value: JSValue): Unit = {
      val data = dataOf(obj)
      data.pairs += ((BuiltinHelpers.toJSString(name), BuiltinHelpers.toJSString(value)))
      updateOwningURL(data)
    }

    val appendFn = NativeFunction(
      name = "append",
      length = 2,
      impl = (args, c) => {
        given JSContext = c
        val obj = searchParamsDataOf(args.headOption.getOrElse(JSValue.Undefined))
        appendPair(obj, requiredArg(args, 1), requiredArg(args, 2))
        JSValue.Undefined
      }
    )

    val deleteFn = NativeFunction(
      name = "delete",
      length = 1,
      impl = (args, c) => {
        given JSContext = c
        val obj = searchParamsDataOf(args.headOption.getOrElse(JSValue.Undefined))
        val data = dataOf(obj)
        val name = BuiltinHelpers.toJSString(requiredArg(args, 1))
        val hasValue = args.length > 2 && args(2) != JSValue.Undefined
        val value = if hasValue then BuiltinHelpers.toJSString(args(2)) else ""
        data.pairs.filterInPlace { case (n, v) =>
          !(n == name && (!hasValue || v == value))
        }
        updateOwningURL(data)
        JSValue.Undefined
      }
    )

    val getFn = NativeFunction(
      name = "get",
      length = 1,
      impl = (args, c) => {
        given JSContext = c
        val obj = searchParamsDataOf(args.headOption.getOrElse(JSValue.Undefined))
        val data = dataOf(obj)
        val name = BuiltinHelpers.toJSString(requiredArg(args, 1))
        data.pairs.find(_._1 == name) match {
          case Some((_, v)) => JSValue.fromString(v)
          case None         => JSValue.Null
        }
      }
    )

    val getAllFn = NativeFunction(
      name = "getAll",
      length = 1,
      impl = (args, c) => {
        given JSContext = c
        val obj = searchParamsDataOf(args.headOption.getOrElse(JSValue.Undefined))
        val data = dataOf(obj)
        val name = BuiltinHelpers.toJSString(requiredArg(args, 1))
        val array = quickjs.objmodel.JSArray.empty()
        data.pairs.foreach { case (n, v) => if n == name then array.push(JSValue.fromString(v)) }
        JSValue.JSArrayVal(array)
      }
    )

    val hasFn = NativeFunction(
      name = "has",
      length = 1,
      impl = (args, c) => {
        given JSContext = c
        val obj = searchParamsDataOf(args.headOption.getOrElse(JSValue.Undefined))
        val data = dataOf(obj)
        val name = BuiltinHelpers.toJSString(requiredArg(args, 1))
        val hasValue = args.length > 2 && args(2) != JSValue.Undefined
        val value = if hasValue then BuiltinHelpers.toJSString(args(2)) else ""
        JSValue.Bool(
          data.pairs.exists { case (n, v) =>
            n == name && (!hasValue || v == value)
          }
        )
      }
    )

    val setFn = NativeFunction(
      name = "set",
      length = 2,
      impl = (args, c) => {
        given JSContext = c
        val obj = searchParamsDataOf(args.headOption.getOrElse(JSValue.Undefined))
        val data = dataOf(obj)
        val name = BuiltinHelpers.toJSString(requiredArg(args, 1))
        val value = BuiltinHelpers.toJSString(requiredArg(args, 2))
        val firstIdx = data.pairs.indexWhere(_._1 == name)
        if firstIdx < 0 then data.pairs += ((name, value))
        else {
          data.pairs(firstIdx) = (name, value)
          var i = data.pairs.length - 1
          while i > firstIdx do {
            if data.pairs(i)._1 == name then data.pairs.remove(i)
            i -= 1
          }
        }
        updateOwningURL(data)
        JSValue.Undefined
      }
    )

    val sortFn = NativeFunction(
      name = "sort",
      length = 0,
      impl = (args, c) => {
        given JSContext = c
        val obj = searchParamsDataOf(args.headOption.getOrElse(JSValue.Undefined))
        val data = dataOf(obj)
        // Stable sort by name only: equal names keep their relative order.
        val sorted = data.pairs.toVector.sortBy(_._1)
        data.pairs.clear()
        data.pairs ++= sorted
        updateOwningURL(data)
        JSValue.Undefined
      }
    )

    val sizeGetter = NativeFunction(
      name = "get size",
      length = 0,
      impl = (args, c) => {
        given JSContext = c
        val obj = searchParamsDataOf(args.headOption.getOrElse(JSValue.Undefined))
        JSValue.fromInt(dataOf(obj).pairs.length)
      }
    )

    val forEachFn = NativeFunction(
      name = "forEach",
      length = 1,
      impl = (args, c) => {
        given JSContext = c
        val obj = searchParamsDataOf(args.headOption.getOrElse(JSValue.Undefined))
        val callback = requiredArg(args, 1)
        if !BuiltinHelpers.isCallable(callback) then
          c.throwTypeError("callback is not a function")
        val thisArg = args.lift(2).getOrElse(JSValue.Undefined)
        val data = dataOf(obj)
        var i = 0
        while i < data.pairs.length do {
          val (name, value) = data.pairs(i)
          BuiltinHelpers.callFunctionWithThis(
            callback,
            thisArg,
            Array(JSValue.fromString(value), JSValue.fromString(name), JSValue.Object(obj))
          )
          i += 1
        }
        JSValue.Undefined
      }
    )

    // --- iterators ---

    val iterProto = JSObject(prototype = ctx.iteratorPrototype)
    val iteratorNext = NativeFunction(
      name = "next",
      length = 0,
      impl = (args, c) => {
        given JSContext = c
        args.headOption match {
          case Some(JSValue.Object(it))
              if it.getOwnProperty("__uspIteratorTarget").isDefined =>
            val target = it.getOwnProperty("__uspIteratorTarget").get
            val kind = it
              .getOwnProperty("__uspIteratorKind")
              .map(_.toString)
              .getOrElse("entry")
            val index = it
              .getOwnProperty("__uspIteratorIndex")
              .map(_.toNumber.toInt)
              .getOrElse(0)
            val data = dataOf(searchParamsDataOf(target))
            if index >= data.pairs.length then iteratorResult(JSValue.Undefined, done = true)
            else {
              it.set("__uspIteratorIndex", JSValue.fromInt(index + 1))
              val (name, value) = data.pairs(index)
              val out = kind match {
                case "key" => JSValue.fromString(name)
                case "value" => JSValue.fromString(value)
                case _ =>
                  val pair = quickjs.objmodel.JSArray.empty()
                  pair.push(JSValue.fromString(name))
                  pair.push(JSValue.fromString(value))
                  JSValue.JSArrayVal(pair)
              }
              iteratorResult(out, done = false)
            }
          case _ =>
            c.throwTypeError(
              "URLSearchParams Iterator.prototype.next called on incompatible receiver"
            )
        }
      }
    )
    iterProto.defineProperty(
      "next",
      JSValue.Native(iteratorNext),
      enumerable = false,
      writable = true,
      configurable = true
    )
    val toStringTag = BuiltinHelpers.wellKnownSymbolId("toStringTag")
    iterProto.initSymbolProperty(
      toStringTag,
      JSValue.fromString("URLSearchParams Iterator"),
      enumerable = false,
      writable = false,
      configurable = true
    )

    def createIterator(obj: JSValue, kind: String): JSValue = {
      val it = JSObject(prototype = iterProto)
      it.initProperty(
        "__uspIteratorTarget",
        obj,
        enumerable = false,
        writable = false,
        configurable = false
      )
      it.initProperty(
        "__uspIteratorKind",
        JSValue.fromString(kind),
        enumerable = false,
        writable = false,
        configurable = false
      )
      it.initProperty(
        "__uspIteratorIndex",
        JSValue.Int32(0),
        enumerable = false,
        writable = true,
        configurable = false
      )
      JSValue.Object(it)
    }

    def iteratorMethod(name: String, kind: String): NativeFunction =
      NativeFunction(
        name = name,
        length = 0,
        impl = (args, c) => {
          given JSContext = c
          val obj = searchParamsDataOf(args.headOption.getOrElse(JSValue.Undefined))
          createIterator(JSValue.Object(obj), kind)
        }
      )

    val entriesFn = iteratorMethod("entries", "entry")
    val keysFn = iteratorMethod("keys", "key")
    val valuesFn = iteratorMethod("values", "value")

    val paramsToStringFn = NativeFunction(
      name = "toString",
      length = 0,
      impl = (args, c) => {
        given JSContext = c
        val obj = searchParamsDataOf(args.headOption.getOrElse(JSValue.Undefined))
        JSValue.fromString(serializeFormURLEncoded(dataOf(obj).pairs))
      }
    )

    def defineMethod(name: String, fn: NativeFunction): Unit =
      proto.defineProperty(
        name,
        JSValue.Native(fn),
        enumerable = true,
        writable = true,
        configurable = true
      )

    defineMethod("append", appendFn)
    defineMethod("delete", deleteFn)
    defineMethod("get", getFn)
    defineMethod("getAll", getAllFn)
    defineMethod("has", hasFn)
    defineMethod("set", setFn)
    defineMethod("sort", sortFn)
    defineMethod("entries", entriesFn)
    defineMethod("keys", keysFn)
    defineMethod("values", valuesFn)
    defineMethod("forEach", forEachFn)
    defineMethod("toString", paramsToStringFn)

    proto.defineAccessorProperty(
      "size",
      getter = Some(JSValue.Native(sizeGetter)),
      setter = None,
      enumerable = true,
      configurable = true
    )

    val iteratorSym = BuiltinHelpers.iteratorSymbolId
    proto.initSymbolProperty(
      iteratorSym,
      JSValue.Native(entriesFn),
      enumerable = false,
      writable = true,
      configurable = true
    )
    proto.initSymbolProperty(
      toStringTag,
      JSValue.fromString("URLSearchParams"),
      enumerable = false,
      writable = false,
      configurable = true
    )
  }

  /** Convert the `URLSearchParams(init)` argument following the WebIDL union
    * rules: iterable objects use the sequence path, other objects the record
    * path, everything else is a query string.
    */
  private def collectInitPairs(init: JSValue)(using
      ctx: JSContext
  ): mutable.ArrayBuffer[(String, String)] = {
    given JSContext = ctx
    val pairs = mutable.ArrayBuffer.empty[(String, String)]
    init match {
      case JSValue.Object(_) | JSValue.JSArrayVal(_) =>
        val iteratorMethod =
          BuiltinHelpers.getSymbolPropertyWithGetter(init, BuiltinHelpers.iteratorSymbolId)
        if BuiltinHelpers.isCallable(iteratorMethod) then {
          val record = BuiltinHelpers.getIteratorRecord(init)
          var finished = false
          try {
            while !finished do {
              BuiltinHelpers.iteratorStepValue(record) match {
                case None => finished = true
                case Some(item) =>
                  if !BuiltinHelpers.isObjectLikeValue(item) then
                    ctx.throwTypeError(
                      "URLSearchParams constructor: iterator value is not an entry object"
                    )
                  else {
                    // JSArray exposes `length` through its exotic internal
                    // slot, which the generic property helper does not read.
                    val lengthVal = item match {
                      case JSValue.JSArrayVal(arr) => JSValue.fromInt(arr.getLength)
                      case _ => BuiltinHelpers.getPropertyWithGetter(item, "length")
                    }
                    val rawLength = BuiltinHelpers.toNumber(lengthVal)
                    val length =
                      if rawLength.isNaN || rawLength <= 0.0 then 0.0
                      else if rawLength.isInfinite then Double.MaxValue
                      else math.floor(rawLength)
                    if length != 2.0 then
                      ctx.throwTypeError(
                        "URLSearchParams constructor: iterator value is not an entry object"
                      )
                    val name = BuiltinHelpers.getPropertyWithGetter(item, "0")
                    val value = BuiltinHelpers.getPropertyWithGetter(item, "1")
                    pairs += ((BuiltinHelpers.toJSString(name), BuiltinHelpers.toJSString(value)))
                  }
              }
            }
          } catch {
            case e: Throwable =>
              BuiltinHelpers.iteratorCloseRecord(record)
              throw e
          }
        } else init match {
          case JSValue.Object(obj) =>
            val keys = obj.getEnumerableOwnStringPropertyKeys()
            var i = 0
            while i < keys.length do {
              val key = keys(i)
              val value = BuiltinHelpers.getPropertyWithGetter(init, key)
              pairs += ((key, BuiltinHelpers.toJSString(value)))
              i += 1
            }
          case _ => ()
        }
      case JSValue.Undefined => ()
      case other =>
        val input = BuiltinHelpers.toJSString(other)
        val stripped = if input.startsWith("?") then input.substring(1) else input
        pairs ++= parseFormURLEncoded(stripped)
    }
    pairs
  }

  private def iteratorResult(value: JSValue, done: Boolean)(using
      ctx: JSContext
  ): JSValue = {
    val obj = JSObject(prototype = ctx.objectPrototype)
    obj.defineProperty("value", value, enumerable = true, writable = true, configurable = true)
    obj.defineProperty("done", JSValue.Bool(done), enumerable = true, writable = true, configurable = true)
    JSValue.Object(obj)
  }

  // --- URL ---------------------------------------------------------------

  private def initializeURL(
      holder: SearchParamsHolder
  )(using ctx: JSContext): Unit = {
    given JSContext = ctx
    val proto = JSObject(prototype = ctx.objectPrototype)

    def newURLObject(args: Array[JSValue]): JSValue = {
      if args.isEmpty then
        ctx.throwTypeError("Failed to construct 'URL': 1 argument required")
      val base: Option[URLRecord] =
        if args.length > 1 && args(1) != JSValue.Undefined then {
          val baseText = BuiltinHelpers.toJSString(args(1))
          parseURL(baseText, None) match {
            case Some(r) => Some(r)
            case None =>
              ctx.throwTypeError("Invalid base URL")
          }
        } else None
      val input = BuiltinHelpers.toJSString(args(0))
      parseURL(input, base) match {
        case Some(record) =>
          val obj = JSObject(prototype = proto)
          obj.initProperty(
            "__urlRecord",
            JSValue.Native(record),
            enumerable = false,
            writable = false,
            configurable = false
          )
          JSValue.Object(obj)
        case None =>
          ctx.throwTypeError("Invalid URL")
      }
    }

    val ctor = NativeConstructor(
      name = "URL",
      callImpl = (_, c) => {
        given JSContext = c
        c.throwTypeError("Constructor URL requires 'new'")
      },
      constructImpl = (args, c) => {
        given JSContext = c
        newURLObject(args)
      },
      prototype = proto,
      superInitImpl = Some((thisValue, args, c) => {
        given JSContext = c
        thisValue match {
          case JSValue.Object(obj) =>
            val parsed = newURLObject(args)
            parsed match {
              case JSValue.Object(src) =>
                src.getOwnProperty("__urlRecord") match {
                  case Some(rec) =>
                    obj.initProperty(
                      "__urlRecord",
                      rec,
                      enumerable = false,
                      writable = false,
                      configurable = false
                    )
                  case None => ()
                }
              case _ => ()
            }
            thisValue
          case _ =>
            c.throwTypeError("Constructor URL requires 'new'")
        }
      })
    )
    BuiltinHelpers.initConstructor(ctor, length = 1)
    ctx.global.defineProperty(
      "URL",
      JSValue.Native(ctor),
      enumerable = false,
      writable = true,
      configurable = true
    )

    def recordOf(value: JSValue): URLRecord =
      value match {
        case JSValue.Object(obj) =>
          obj.getOwnProperty("__urlRecord") match {
            case Some(JSValue.Native(rec: URLRecord)) => rec
            case _ => ctx.throwTypeError("Value is not a URL object")
          }
        case _ => ctx.throwTypeError("Value is not a URL object")
      }

    def urlObjectOf(value: JSValue): JSObject =
      value match {
        case JSValue.Object(obj)
            if obj.getOwnProperty("__urlRecord").exists(_.isInstanceOf[JSValue.Native]) =>
          obj
        case _ => ctx.throwTypeError("Value is not a URL object")
      }

    def afterQueryChanged(urlObj: JSObject, record: URLRecord): Unit = {
      urlObj.getOwnProperty("__urlSearchParams") match {
        case Some(JSValue.Object(spObj)) =>
          dataOf(spObj).pairs = parseFormURLEncoded(record.query.getOrElse(""))
        case _ => ()
      }
    }

    def accessor(
        name: String,
        getter: URLRecord => JSValue,
        setter: Option[(JSObject, URLRecord, JSValue) => Unit]
    ): Unit = {
      val getFn = NativeFunction(
        name = "get " + name,
        length = 0,
        impl = (args, c) => {
          given JSContext = c
          getter(recordOf(args.headOption.getOrElse(JSValue.Undefined)))
        }
      )
      val setFn = setter.map { f =>
        NativeFunction(
          name = "set " + name,
          length = 1,
          impl = (args, c) => {
            given JSContext = c
            val thisVal = args.headOption.getOrElse(JSValue.Undefined)
            val obj = urlObjectOf(thisVal)
            f(obj, recordOf(thisVal), args.lift(1).getOrElse(JSValue.Undefined))
            JSValue.Undefined
          }
        )
      }
      proto.defineAccessorProperty(
        name,
        getter = Some(JSValue.Native(getFn)),
        setter = setFn.map(JSValue.Native(_)),
        enumerable = true,
        configurable = true
      )
    }

    def pathnameOf(record: URLRecord): String =
      if record.opaquePath then
        if record.path.isEmpty then "" else record.path.head
      else
        val sb = new StringBuilder
        var i = 0
        while i < record.path.length do {
          sb.append('/')
          sb.append(record.path(i))
          i += 1
        }
        sb.toString

    def getOrCreateSearchParams(urlObj: JSObject, record: URLRecord): JSValue = {
      urlObj.getOwnProperty("__urlSearchParams") match {
        case Some(v @ JSValue.Object(_)) => v
        case _ =>
          val spObj = JSObject(prototype = holder.prototype)
          spObj.initProperty(
            "__uspData",
            JSValue.Native(
              new SearchParamsData(
                parseFormURLEncoded(record.query.getOrElse("")),
                Some(JSValue.Object(urlObj))
              )
            ),
            enumerable = false,
            writable = false,
            configurable = false
          )
          urlObj.initProperty(
            "__urlSearchParams",
            JSValue.Object(spObj),
            enumerable = false,
            writable = true,
            configurable = false
          )
          JSValue.Object(spObj)
      }
    }

    def searchOf(record: URLRecord): String =
      record.query match {
        case None | Some("") => ""
        case Some(q)         => "?" + q
      }

    def hashOf(record: URLRecord): String =
      record.fragment match {
        case None | Some("") => ""
        case Some(f)         => "#" + f
      }

    accessor("href", r => JSValue.fromString(serializeURL(r, excludeFragment = false)), Some { (obj, record, value) =>
      val text = BuiltinHelpers.toJSString(value)
      parseURL(text, None) match {
        case Some(fresh) =>
          record.scheme = fresh.scheme
          record.username = fresh.username
          record.password = fresh.password
          record.host = fresh.host
          record.port = fresh.port
          record.path = fresh.path.clone()
          record.query = fresh.query
          record.fragment = fresh.fragment
          record.opaquePath = fresh.opaquePath
          afterQueryChanged(obj, record)
        case None => ctx.throwTypeError("Invalid URL")
      }
    })
    accessor("origin", r => JSValue.fromString(originOf(r)), None)
    accessor("protocol", r => JSValue.fromString(r.scheme + ":"), Some { (_, record, value) =>
      val text = BuiltinHelpers.toJSString(value) + ":"
      applyOverride(text, record, "scheme start")
      ()
    })
    accessor(
      "username",
      r => JSValue.fromString(r.username),
      Some { (_, record, value) =>
        if !cannotHaveAUsernamePasswordPort(record) then
          record.username = encodeCodePoints(BuiltinHelpers.toJSString(value), isUserinfoPercentEncode)
      }
    )
    accessor(
      "password",
      r => JSValue.fromString(r.password),
      Some { (_, record, value) =>
        if !cannotHaveAUsernamePasswordPort(record) then
          record.password = encodeCodePoints(BuiltinHelpers.toJSString(value), isUserinfoPercentEncode)
      }
    )
    accessor(
      "host",
      r =>
        r.host match {
          case None => JSValue.fromString("")
          case Some(h) =>
            JSValue.fromString(r.port.map(p => s"$h:$p").getOrElse(h))
        },
      Some { (_, record, value) =>
        if !record.opaquePath then
          applyOverride(BuiltinHelpers.toJSString(value), record, "host")
      }
    )
    accessor(
      "hostname",
      r => JSValue.fromString(r.host.getOrElse("")),
      Some { (_, record, value) =>
        if !record.opaquePath then
          applyOverride(BuiltinHelpers.toJSString(value), record, "hostname")
      }
    )
    accessor(
      "port",
      r => JSValue.fromString(r.port.map(_.toString).getOrElse("")),
      Some { (_, record, value) =>
        if !cannotHaveAUsernamePasswordPort(record) then {
          val text = BuiltinHelpers.toJSString(value)
          if text.isEmpty then record.port = None
          else applyOverride(text, record, "port")
        }
      }
    )
    accessor(
      "pathname",
      r => JSValue.fromString(pathnameOf(r)),
      Some { (_, record, value) =>
        if !record.opaquePath then {
          val text = BuiltinHelpers.toJSString(value)
          val clone = record.copyOf()
          clone.path.clear()
          if basicURLParse(text, None, clone, Some("path start"), trimControls = false) then {
            record.path = clone.path.clone()
          }
        }
      }
    )
    accessor(
      "search",
      r => JSValue.fromString(searchOf(r)),
      Some { (obj, record, value) =>
        val text = BuiltinHelpers.toJSString(value)
        if text.isEmpty then {
          record.query = None
          afterQueryChanged(obj, record)
        } else {
          val input = if text.startsWith("?") then text.substring(1) else text
          val clone = record.copyOf()
          clone.query = Some("")
          if basicURLParse(input, None, clone, Some("query"), trimControls = false) then {
            record.query = clone.query
            afterQueryChanged(obj, record)
          }
        }
      }
    )
    accessor(
      "hash",
      r => JSValue.fromString(hashOf(r)),
      Some { (_, record, value) =>
        val text = BuiltinHelpers.toJSString(value)
        if text.isEmpty then record.fragment = None
        else {
          val input = if text.startsWith("#") then text.substring(1) else text
          val clone = record.copyOf()
          clone.fragment = Some("")
          if basicURLParse(input, None, clone, Some("fragment"), trimControls = false) then
            record.fragment = clone.fragment
        }
      }
    )

    // `searchParams` must see its receiver, so its getter is defined by hand.
    val searchParamsGetter = NativeFunction(
      name = "get searchParams",
      length = 0,
      impl = (args, c) => {
        given JSContext = c
        val thisVal = args.headOption.getOrElse(JSValue.Undefined)
        val obj = urlObjectOf(thisVal)
        getOrCreateSearchParams(obj, recordOf(thisVal))
      }
    )
    proto.defineAccessorProperty(
      "searchParams",
      getter = Some(JSValue.Native(searchParamsGetter)),
      setter = None,
      enumerable = true,
      configurable = true
    )

    val toStringFn = NativeFunction(
      name = "toString",
      length = 0,
      impl = (args, c) => {
        given JSContext = c
        JSValue.fromString(serializeURL(recordOf(args.headOption.getOrElse(JSValue.Undefined)), excludeFragment = false))
      }
    )
    val toJSONFn = NativeFunction(
      name = "toJSON",
      length = 0,
      impl = (args, c) => {
        given JSContext = c
        JSValue.fromString(serializeURL(recordOf(args.headOption.getOrElse(JSValue.Undefined)), excludeFragment = false))
      }
    )
    proto.defineProperty("toString", JSValue.Native(toStringFn), enumerable = true, writable = true, configurable = true)
    proto.defineProperty("toJSON", JSValue.Native(toJSONFn), enumerable = true, writable = true, configurable = true)

    // Static methods receive the constructor as `this` at index 0.
    def parseOrNull(args: Array[JSValue], useBase: Boolean): Option[JSValue] = {
      if args.length <= 1 then
        ctx.throwTypeError("1 argument required, but only 0 present")
      else {
        val base: Option[URLRecord] =
          if useBase && args.length > 2 && args(2) != JSValue.Undefined then
            parseURL(BuiltinHelpers.toJSString(args(2)), None)
          else None
        parseURL(BuiltinHelpers.toJSString(args(1)), base).map { record =>
          val obj = JSObject(prototype = proto)
          obj.initProperty(
            "__urlRecord",
            JSValue.Native(record),
            enumerable = false,
            writable = false,
            configurable = false
          )
          JSValue.Object(obj)
        }
      }
    }

    val canParseFn = NativeFunction(
      name = "canParse",
      length = 1,
      impl = (args, c) => {
        given JSContext = c
        JSValue.Bool(parseOrNull(args, useBase = true).isDefined)
      }
    )
    val parseStaticFn = NativeFunction(
      name = "parse",
      length = 1,
      impl = (args, c) => {
        given JSContext = c
        parseOrNull(args, useBase = true).getOrElse(JSValue.Null)
      }
    )
    ctor.funcObj.defineProperty(
      "canParse",
      JSValue.Native(canParseFn),
      enumerable = false,
      writable = true,
      configurable = true
    )
    ctor.funcObj.defineProperty(
      "parse",
      JSValue.Native(parseStaticFn),
      enumerable = false,
      writable = true,
      configurable = true
    )

    proto.initSymbolProperty(
      BuiltinHelpers.wellKnownSymbolId("toStringTag"),
      JSValue.fromString("URL"),
      enumerable = false,
      writable = false,
      configurable = true
    )
  }

}
