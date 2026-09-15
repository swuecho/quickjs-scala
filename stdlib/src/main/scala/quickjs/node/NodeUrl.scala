package quickjs.node

import quickjs.value.{JSValue, NativeFunction, NativeConstructor}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.JSObject

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** Node's `url` module plus `fileURLToPath`/`pathToFileURL` helpers. */
object NodeUrl {

  // =========================================================================
  // Helpers shared with fs/path
  // =========================================================================

  /** Extract the `href` of a URL object (throws when not a URL). */
  private def hrefOf(value: JSValue)(using ctx: JSContext): String =
    value match {
      case JSValue.Object(obj) if obj.getOwnPropertyRaw("__urlRecord").isDefined =>
        BuiltinHelpers.getPropertyWithGetter(value, "href") match {
          case JSValue.JSStr(s) => s
          case other            => NodeHelpers.toStr(other)
        }
      case _ =>
        NodeHelpers.throwCoded(
          "TypeError",
          "The URL must be of scheme file",
          "ERR_INVALID_URL_SCHEME"
        )
    }

  /** Convert a `file:` URL string (or URL object) to a filesystem path. */
  def fileURLToPath(url: JSValue)(using ctx: JSContext): String = {
    val href =
      url match {
        case JSValue.JSStr(s) => s
        case _                => hrefOf(url)
      }
    fileUrlStringToPath(href)
  }

  /** Parse a `file://` href into a native path. */
  def fileUrlStringToPath(href: String): String = parseFileHref(href)

  private def parseFileHref(href: String): String = {
    if !href.startsWith("file:") then
      throw new IllegalArgumentException(s"Not a file URL: $href")
    var rest = href.substring("file:".length)
    var host = ""
    if rest.startsWith("//") then {
      rest = rest.substring(2)
      val slash = rest.indexOf('/')
      if slash < 0 then {
        host = rest
        rest = "//"
      } else {
        host = rest.substring(0, slash)
        rest = rest.substring(slash)
      }
    }
    val isWindows = NodeOs.isWindows
    val decoded = decodePercent(rest)
    if decoded.contains('\u0000') then
      throw new IllegalArgumentException("File URL path contains a null byte")
    val path =
      if isWindows then windowsFilePath(host, decoded)
      else posixFilePath(host, decoded)
    path
  }

  private def posixFilePath(host: String, path: String): String =
    if host.nonEmpty && host != "localhost" then {
      // UNC-style host on POSIX is preserved with a leading double slash.
      val body = if path.startsWith("/") then path else "/" + path
      s"//$host$body"
    } else path

  private def windowsFilePath(host: String, path: String): String = {
    val normalized = path.replace('/', '\\')
    val withDrive =
      if normalized.length >= 3 && normalized.charAt(0) == '\\' &&
          normalized.charAt(2) == ':' && normalized.charAt(1).isLetter
      then normalized.substring(1)
      else normalized
    if host.nonEmpty && host != "localhost" then s"\\\\$host$withDrive"
    else withDrive
  }

  /** `pathToFileURL`: build a URL object for a filesystem path. */
  def pathToFileURL(pathValue: String)(using ctx: JSContext): JSValue = {
    val absolute =
      if NodeOs.isWindows then {
        val p = Paths.get(pathValue).toAbsolutePath.normalize.toString
        p.replace('\\', '/')
      } else Paths.get(pathValue).toAbsolutePath.normalize.toString
    val slashPath =
      if NodeOs.isWindows && absolute.length >= 2 && absolute.charAt(1) == ':' then
        "/" + absolute
      else absolute
    val encoded = slashPath
      .split("/", -1)
      .map(encodePathSegment)
      .mkString("/")
    val href = s"file://$encoded"
    val ctor = ctx.global.get("URL")
    ctor match {
      case JSValue.Native(nc: NativeConstructor) =>
        nc.construct(Array(JSValue.fromString(href)))
      case _ => JSValue.fromString(href)
    }
  }

  /** Encode a path segment per the WHATWG path percent-encode set. */
  def encodePathSegment(segment: String): String = {
    val sb = new StringBuilder
    segment.getBytes(StandardCharsets.UTF_8).foreach { byte =>
      val c = (byte & 0xff).toChar
      val keep =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
          (c >= '0' && c <= '9') || c == '-' || c == '.' || c == '_' || c == '~' ||
          c == '!' || c == '$' || c == '&' || c == '\'' || c == '(' || c == ')' ||
          c == '*' || c == '+' || c == ',' || c == ';' || c == '=' || c == '@' ||
          c == ':' || c == '%'
      if keep then sb.append(c)
      else sb.append(f"%%${byte & 0xff}%02X")
    }
    sb.toString
  }

  /** Decode percent-escapes; leaves invalid sequences as-is (matches Node). */
  def decodePercent(text: String): String = {
    if !text.contains('%') then text
    else {
      val bytes = new java.io.ByteArrayOutputStream()
      var i = 0
      while i < text.length do {
        val c = text.charAt(i)
        if c == '%' && i + 2 < text.length then {
          val hex = text.substring(i + 1, i + 3)
          try {
            bytes.write(Integer.parseInt(hex, 16))
            i += 3
          } catch {
            case _: NumberFormatException =>
              bytes.write(c.toInt)
              i += 1
          }
        } else {
          bytes.write(c.toString.getBytes(StandardCharsets.UTF_8))
          i += 1
        }
      }
      new String(bytes.toByteArray, StandardCharsets.UTF_8)
    }
  }

  private def moduleArgs(
      module: JSObject,
      args: Array[JSValue]
  ): Array[JSValue] = NodeHelpers.stripReceiver(args, module)

  // =========================================================================
  // Module factory
  // =========================================================================

  def create()(using ctx: JSContext): JSValue = {
    val url = JSObject(prototype = ctx.objectPrototype)

    val fileURLToPathFn = NativeFunction(
      name = "fileURLToPath",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val real = moduleArgs(url, args)
        real.headOption match {
          case Some(JSValue.JSStr(s)) =>
            try JSValue.fromString(parseFileHref(s))
            catch
              case _: IllegalArgumentException =>
                NodeHelpers.throwCoded(
                  "TypeError",
                  "The URL must be of scheme file",
                  "ERR_INVALID_URL_SCHEME"
                )
          case Some(value) => JSValue.fromString(fileURLToPath(value))
          case None =>
            NodeHelpers.throwCoded(
              "TypeError",
              "The \"url\" argument must be of type string or an instance of URL",
              "ERR_INVALID_ARG_TYPE"
            )
        }
      }
    )

    val pathToFileURLFn = NativeFunction(
      name = "pathToFileURL",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val real = moduleArgs(url, args)
        real.headOption match {
          case Some(value) => pathToFileURL(NodeHelpers.toPath(value))
          case None =>
            NodeHelpers.throwCoded(
              "TypeError",
              "The \"path\" argument must be of type string",
              "ERR_INVALID_ARG_TYPE"
            )
        }
      }
    )

    val formatFn = NativeFunction(
      name = "format",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val real = moduleArgs(url, args)
        real.headOption match {
          case Some(value @ JSValue.Object(obj))
              if obj.getOwnPropertyRaw("__urlRecord").isDefined =>
            BuiltinHelpers.getPropertyWithGetter(value, "href")
          case Some(JSValue.JSStr(s)) => JSValue.fromString(s)
          case Some(value)            => legacyUrlFormat(value)
          case None                   => JSValue.fromString("")
        }
      }
    )

    val urlToHttpOptionsFn = NativeFunction(
      name = "urlToHttpOptions",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val real = moduleArgs(url, args)
        val urlValue = real.headOption.getOrElse(JSValue.Undefined)
        val options = JSObject(prototype = ctx.objectPrototype)
        def get(key: String): JSValue =
          BuiltinHelpers.getPropertyWithGetter(urlValue, key)
        options.set("protocol", get("protocol"))
        options.set("hostname", get("hostname"))
        options.set("hash", get("hash"))
        options.set("search", get("search"))
        options.set("pathname", get("pathname"))
        options.set("path", JSValue.fromString(
          NodeHelpers.toStr(get("pathname")) + NodeHelpers.toStr(get("search"))
        ))
        options.set("href", get("href"))
        options.set("port", get("port"))
        options.set("auth", JSValue.Undefined)
        JSValue.Object(options)
      }
    )

    val domainToASCII = NativeFunction(
      name = "domainToASCII",
      length = 1,
      impl = (args, callCtx) => {
        val real = moduleArgs(url, args)
        real.headOption match {
          case Some(JSValue.JSStr(s)) =>
            try JSValue.fromString(java.net.IDN.toASCII(s))
            catch case _: Throwable => JSValue.fromString("")
          case _ => JSValue.fromString("")
        }
      }
    )

    val domainToUnicode = NativeFunction(
      name = "domainToUnicode",
      length = 1,
      impl = (args, callCtx) => {
        val real = moduleArgs(url, args)
        real.headOption match {
          case Some(JSValue.JSStr(s)) =>
            try JSValue.fromString(java.net.IDN.toUnicode(s))
            catch case _: Throwable => JSValue.fromString("")
          case _ => JSValue.fromString("")
        }
      }
    )

    def define(name: String, fn: NativeFunction): Unit =
      url.set(name, JSValue.Native(fn))

    define("fileURLToPath", fileURLToPathFn)
    define("pathToFileURL", pathToFileURLFn)
    define("format", formatFn)
    define("urlToHttpOptions", urlToHttpOptionsFn)
    define("domainToASCII", domainToASCII)
    define("domainToUnicode", domainToUnicode)

    // Re-export the WHATWG constructors installed by Globals/URLBuiltins.
    url.set("URL", ctx.global.get("URL"))
    url.set("URLSearchParams", ctx.global.get("URLSearchParams"))

    JSValue.Object(url)
  }

  private def legacyUrlFormat(value: JSValue)(using ctx: JSContext): JSValue = {
    def get(key: String): String = value match {
      case JSValue.Object(_) | JSValue.JSArrayVal(_) =>
        BuiltinHelpers.getPropertyWithGetter(value, key) match {
          case JSValue.Undefined => ""
          case other             => NodeHelpers.toStr(other)
        }
      case _ => ""
    }
    val protocol = get("protocol")
    val host = get("host")
    val hostname = get("hostname")
    val port = get("port")
    val pathname = get("pathname")
    val search = get("search")
    val hash = get("hash")
    val auth = get("auth")
    val slashes = get("slashes").nonEmpty && get("slashes") != "false"
    val hostPart =
      if host.nonEmpty then host
      else if port.nonEmpty then s"$hostname:$port"
      else hostname
    val prefix =
      if protocol.nonEmpty then
        if slashes then s"$protocol//" else protocol
      else ""
    val userinfo = if auth.nonEmpty then s"$auth@" else ""
    JSValue.fromString(s"$prefix$userinfo$hostPart$pathname$search$hash")
  }
}
