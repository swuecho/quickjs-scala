package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject

/** POSIX `path` module (Node-compatible string manipulation).
  *
  * The algorithms mirror Node's `lib/path.js` for the POSIX implementation.
  * `path.win32` currently aliases the POSIX behavior.
  */
object NodePath {

  private val Sep = '/'
  private val Delimiter = ':'

  private def normalizeString(path: String, allowAboveRoot: Boolean): String = {
    val out = scala.collection.mutable.ArrayBuffer.empty[String]
    val segments = path.split("/", -1)
    for seg <- segments if seg.nonEmpty do {
      seg match {
        case "." => ()
        case ".." =>
          if out.nonEmpty && out.last != ".." then out.remove(out.length - 1)
          else if allowAboveRoot then out += ".."
          else ()
        case other => out += other
      }
    }
    out.mkString("/")
  }

  def normalize(path: String): String = {
    if path.isEmpty then "."
    else {
      val isAbsolute = path.charAt(0) == '/'
      var trailingSeparator = false
      var end = path.length
      while end > 1 && path.charAt(end - 1) == '/' do {
        trailingSeparator = true
        end -= 1
      }
      // A path that is exactly "/" is the root.
      if isAbsolute && end == 1 then "/"
      else {
        val body = normalizeString(path.substring(0, end), !isAbsolute)
        if body.isEmpty then {
          if isAbsolute then "/"
          else if trailingSeparator then "./"
          else "."
        } else if trailingSeparator then s"$body/"
        else if isAbsolute then s"/$body"
        else body
      }
    }
  }

  def join(paths: Seq[String]): String = {
    if paths.isEmpty then "."
    else {
      val joined = paths.filter(_.nonEmpty).mkString("/")
      if joined.isEmpty then "." else normalize(joined)
    }
  }

  def resolve(cwd: String, paths: Seq[String]): String = {
    var resolved = ""
    var isAbsolute = false
    var i = paths.length - 1
    while i >= 0 && !isAbsolute do {
      val p = paths(i)
      if p.nonEmpty then {
        resolved = if resolved.isEmpty then p else s"$p/$resolved"
        isAbsolute = p.charAt(0) == '/'
      }
      i -= 1
    }
    if !isAbsolute then
      resolved = if resolved.isEmpty then cwd else s"$cwd/$resolved"
    normalize(resolved)
  }

  def dirname(path: String): String = {
    if path.isEmpty then "."
    else {
      val hasRoot = path.charAt(0) == '/'
      var end = -1
      var matchedSlash = true
      var i = path.length - 1
      while i >= 1 do {
        if path.charAt(i) == '/' then {
          if !matchedSlash then {
            end = i
            i = 0
          }
        } else matchedSlash = false
        i -= 1
      }
      if end == -1 then (if hasRoot then "/" else ".")
      else if hasRoot && end == 1 then "//"
      else path.substring(0, end)
    }
  }

  def basename(path: String, ext: Option[String]): String = {
    var start = 0
    var end = -1
    var matchedSlash = true
    if path.nonEmpty then {
      var i = path.length - 1
      while i >= 0 do {
        if path.charAt(i) == '/' then {
          if !matchedSlash then {
            start = i + 1
            i = -1
          }
        } else if end == -1 then {
          matchedSlash = false
          end = i + 1
        }
        i -= 1
      }
      val base =
        if end == -1 then ""
        else path.substring(start, end)
      ext match {
        case Some(e)
            if e.nonEmpty && base.endsWith(e) && base.length > e.length =>
          base.substring(0, base.length - e.length)
        case Some(e) if e.nonEmpty && base == e => ""
        case _                                  => base
      }
    } else ""
  }

  def extname(path: String): String = {
    var startDot = -1
    var startPart = 0
    var end = -1
    var matchedSlash = true
    var preDotState = 0
    var i = path.length - 1
    while i >= 0 do {
      val c = path.charAt(i)
      if c == '/' then {
        if !matchedSlash then {
          startPart = i + 1
          i = -1
        }
      } else {
        if end == -1 then {
          matchedSlash = false
          end = i + 1
        }
        if c == '.' then {
          if startDot == -1 then startDot = i
          else if preDotState != 1 then preDotState = 1
        } else if startDot != -1 then preDotState = -1
      }
      i -= 1
    }
    if startDot == -1 || end == -1 || preDotState == 0 ||
        (preDotState == 1 && startDot == end - 1 && startDot == startPart + 1)
    then ""
    else path.substring(startDot, end)
  }

  def relative(from: String, to: String, cwd: String): String = {
    val fromResolved = resolve(cwd, Seq(from))
    val toResolved = resolve(cwd, Seq(to))
    if fromResolved == toResolved then ""
    else {
      val fromParts = fromResolved.split("/", -1).filter(_.nonEmpty)
      val toParts = toResolved.split("/", -1).filter(_.nonEmpty)
      var common = 0
      val max = math.min(fromParts.length, toParts.length)
      while common < max && fromParts(common) == toParts(common) do common += 1
      val up = fromParts.length - common
      val segments =
        Array.fill(up)("../").mkString + toParts.drop(common).mkString("/")
      if segments.endsWith("/") && segments.length > 1 then
        segments.substring(0, segments.length - 1)
      else segments
    }
  }

  def parse(path: String): (String, String, String, String, String) = {
    val root = if path.startsWith("/") then "/" else ""
    val dir = {
      val d = dirname(path)
      if d == "." && !path.contains("/") then "" else d
    }
    val base = basename(path, None)
    val ext = extname(base)
    val name = base.substring(0, base.length - ext.length)
    (root, dir, base, ext, name)
  }

  def format(
      dir: String,
      root: String,
      base: String,
      name: String,
      ext: String
  ): String = {
    val effectiveBase = if base.nonEmpty then base else name + ext
    val effectiveDir = if dir.nonEmpty then dir else root
    if effectiveDir.isEmpty then effectiveBase
    else if effectiveDir == root then effectiveDir + effectiveBase
    else if effectiveBase.isEmpty then effectiveDir
    else effectiveDir + "/" + effectiveBase
  }

  // =========================================================================
  // Module factory
  // =========================================================================

  def create(state: NodeState)(using ctx: JSContext): JSValue = {
    val path = JSObject(prototype = ctx.objectPrototype)

    def strip(args: Array[JSValue]): Array[JSValue] =
      NodeHelpers.stripReceiver(args, path)

    def stringsOf(args: Array[JSValue])(using ctx: JSContext): Seq[String] =
      args.toSeq.map(v => NodeHelpers.toPath(v))

    def define(name: String, arity: Int)(
        impl: Array[JSValue] => JSValue
    ): NativeFunction = {
      val fn = NativeFunction(
        name = name,
        length = arity,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          impl(strip(args))
        }
      )
      path.set(name, JSValue.Native(fn))
      fn
    }

    define("normalize", 1)(args =>
      JSValue.fromString(normalize(stringsOf(args).headOption.getOrElse("")))
    )
    define("join", 0)(args =>
      JSValue.fromString(join(stringsOf(args)))
    )
    define("resolve", 0)(args =>
      JSValue.fromString(resolve(state.cwd.toString, stringsOf(args)))
    )
    define("dirname", 1)(args =>
      JSValue.fromString(dirname(stringsOf(args).headOption.getOrElse("")))
    )
    define("basename", 2)(args => {
      val values = stringsOf(args)
      val p = values.headOption.getOrElse("")
      val ext = values.lift(1)
      JSValue.fromString(basename(p, ext))
    })
    define("extname", 1)(args =>
      JSValue.fromString(extname(stringsOf(args).headOption.getOrElse("")))
    )
    define("relative", 2)(args => {
      val values = stringsOf(args)
      JSValue.fromString(
        relative(
          values.headOption.getOrElse(""),
          values.lift(1).getOrElse(""),
          state.cwd.toString
        )
      )
    })
    define("isAbsolute", 1)(args =>
      JSValue.Bool(stringsOf(args).headOption.exists(_.startsWith("/")))
    )
    define("parse", 1)(args => {
      val (root, dir, base, ext, name) =
        parse(stringsOf(args).headOption.getOrElse(""))
      JSValue.Object(
        NodeHelpers.objectOf(
          "root" -> JSValue.fromString(root),
          "dir" -> JSValue.fromString(dir),
          "base" -> JSValue.fromString(base),
          "ext" -> JSValue.fromString(ext),
          "name" -> JSValue.fromString(name)
        )
      )
    })
    define("format", 1)(args => {
      def field(value: JSValue, key: String): String =
        value match {
          case JSValue.Object(_) =>
            quickjs.runtime.builtins.BuiltinHelpers
              .getPropertyWithGetter(value, key) match {
              case JSValue.Undefined => ""
              case other             => NodeHelpers.toStr(other)
            }
          case _ => ""
        }
      val value = args.headOption.getOrElse(JSValue.Undefined)
      JSValue.fromString(
        format(
          field(value, "dir"),
          field(value, "root"),
          field(value, "base"),
          field(value, "name"),
          field(value, "ext")
        )
      )
    })
    define("toNamespacedPath", 1)(args =>
      args.headOption.getOrElse(JSValue.fromString(""))
    )

    path.set("sep", JSValue.fromString(Sep.toString))
    path.set("delimiter", JSValue.fromString(Delimiter.toString))
    path.set("posix", JSValue.Object(path))
    // `win32` shares the POSIX implementation for now; only its separators
    // are exposed distinctly.
    path.set("win32", JSValue.Object(path))

    JSValue.Object(path)
  }
}
