package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.{JSArray, JSObject}

import java.nio.channels.FileChannel
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{
  AccessDeniedException,
  DirectoryNotEmptyException,
  FileAlreadyExistsException,
  FileSystems,
  Files,
  LinkOption,
  NoSuchFileException,
  NotDirectoryException,
  Path,
  Paths,
  StandardCopyOption,
  StandardOpenOption,
  StandardWatchEventKinds
}
import scala.collection.mutable

/** Node's `fs` module: synchronous APIs plus a `fs.promises` subset. */
object NodeFs {

  private val openChannels = mutable.HashMap.empty[Int, FileChannel]
  private var nextFd = 3

  /** Raw stat data gathered off the JS thread. */
  private final case class RawStat(
      size: Long,
      mtimeMs: Long,
      atimeMs: Long,
      ctimeMs: Long,
      birthtimeMs: Long,
      dev: Long,
      ino: Long,
      mode: Int,
      nlink: Long,
      uid: Int,
      gid: Int,
      rdev: Long,
      isFile: Boolean,
      isDirectory: Boolean,
      isSymbolicLink: Boolean
  )

  private def rawStat(path: Path, followLinks: Boolean): RawStat = {
    val options =
      if followLinks then Array.empty[LinkOption]
      else Array(LinkOption.NOFOLLOW_LINKS)
    val attrs = Files.readAttributes(path, classOf[BasicFileAttributes], options*)
    val isSymlink = Files.isSymbolicLink(path)
    def unixAttr(name: String, fallback: Long): Long =
      try Files.getAttribute(path, name).asInstanceOf[Number].longValue()
      catch case _: Throwable => fallback
    RawStat(
      size = attrs.size(),
      mtimeMs = attrs.lastModifiedTime().toMillis,
      atimeMs = attrs.lastAccessTime().toMillis,
      ctimeMs = attrs.lastModifiedTime().toMillis,
      birthtimeMs = attrs.creationTime().toMillis,
      dev = unixAttr("unix:dev", 0L),
      ino = unixAttr("unix:ino", 0L),
      mode = unixAttr("unix:mode", 0x81a4L).toInt,
      nlink = unixAttr("unix:nlink", 1L),
      uid = unixAttr("unix:uid", -1L).toInt,
      gid = unixAttr("unix:gid", -1L).toInt,
      rdev = unixAttr("unix:rdev", 0L),
      isFile = attrs.isRegularFile && !isSymlink,
      isDirectory = attrs.isDirectory,
      isSymbolicLink = isSymlink
    )
  }

  /** List directory entries with their types (sorted for determinism). */
  private def rawReaddir(path: Path): Vector[(String, Boolean, Boolean, Boolean)] = {
    val stream = Files.list(path)
    try {
      val builder = Vector.newBuilder[(String, Boolean, Boolean, Boolean)]
      stream.forEach { child =>
        val name = child.getFileName.toString
        val isSymlink = Files.isSymbolicLink(child)
        val attrs =
          try
            Files.readAttributes(
              child,
              classOf[BasicFileAttributes],
              LinkOption.NOFOLLOW_LINKS
            )
          catch case _: Throwable => null
        builder += ((
          name,
          attrs != null && attrs.isRegularFile && !isSymlink,
          attrs != null && attrs.isDirectory,
          isSymlink
        ))
      }
      builder.result().sortBy(_._1)
    } finally stream.close()
  }

  def create(state: NodeState, loop: HostEventLoop)(using
      ctx: JSContext
  ): JSValue = {
    val fs = JSObject(prototype = ctx.objectPrototype)

    def strip(args: Array[JSValue]): Array[JSValue] =
      NodeHelpers.stripPathReceiver(args)

    /** Node's `mkdtemp` prefix is a path whose parent must exist; Java only
      * accepts a parent directory plus a name prefix.
      */
    def mkdtempParentAndPrefix(
        rawPrefix: String
    ): (Path, String) = {
      val raw = Paths.get(rawPrefix)
      val full = if raw.isAbsolute then raw else state.cwd.resolve(raw)
      val parent = Option(full.getParent).getOrElse(state.cwd)
      val namePrefix =
        Option(full.getFileName).map(_.toString).getOrElse("tmp")
      // java.io.File.createTempFile requires at least three characters.
      (parent, if namePrefix.length >= 3 then namePrefix else namePrefix.padTo(3, '_'))
    }

    def resolvePath(value: JSValue)(using ctx: JSContext): Path = {
      val text = NodeHelpers.toPath(value)
      val raw = Paths.get(text)
      if raw.isAbsolute then raw else state.cwd.resolve(raw)
    }

    def pathString(value: JSValue)(using ctx: JSContext): String =
      resolvePath(value).toString

    /** Build a Node-style Error with a `code` property for a filesystem
      * failure. Safe to call on the loop thread when delivering async results.
      */
    def fsErrorValue(
        e: Throwable,
        syscall: String,
        target: String
    )(using ctx: JSContext): JSValue = {
      val (code, description) = e match {
        case _: NoSuchFileException =>
          ("ENOENT", "no such file or directory")
        case _: AccessDeniedException =>
          ("EACCES", "permission denied")
        case _: FileAlreadyExistsException =>
          ("EEXIST", "file already exists")
        case _: DirectoryNotEmptyException =>
          ("ENOTEMPTY", "directory not empty")
        case _: NotDirectoryException =>
          ("ENOTDIR", "not a directory")
        case e: java.nio.file.FileSystemException =>
          val reason = Option(e.getReason).getOrElse(e.getMessage)
          ("EIO", Option(reason).getOrElse("i/o error"))
        case _ =>
          ("EIO", Option(e.getMessage).getOrElse("i/o error"))
      }
      val message = s"$code: $description, $syscall '$target'"
      val error = ctx.createError("Error", message)
      error match {
        case JSValue.Object(obj) =>
          obj.defineProperty(
            "code",
            JSValue.fromString(code),
            enumerable = false,
            writable = true,
            configurable = true
          )
          obj.defineProperty(
            "syscall",
            JSValue.fromString(syscall),
            enumerable = false,
            writable = true,
            configurable = true
          )
          obj.defineProperty(
            "path",
            JSValue.fromString(target),
            enumerable = false,
            writable = true,
            configurable = true
          )
        case _ => ()
      }
      error
    }

    def throwFsError(
        e: Throwable,
        syscall: String,
        target: String
    )(using ctx: JSContext): Nothing = {
      throw new quickjs.runtime.JSException(
        fsErrorValue(e, syscall, target)
      )
    }

    def guard[A](syscall: String, target: String)(body: => A)(
        using ctx: JSContext
    ): A =
      try body
      catch {
        case e: java.nio.file.FileSystemException =>
          throwFsError(e, syscall, target)
        case e: java.io.FileNotFoundException =>
          throwFsError(e, syscall, target)
      }

    def optionField(
        options: JSValue,
        key: String
    )(using ctx: JSContext): JSValue =
      options match {
        case JSValue.Object(_) =>
          BuiltinHelpers.getPropertyWithGetter(options, key)
        case _ => JSValue.Undefined
      }

    def encodingOf(options: JSValue)(using ctx: JSContext): Option[String] =
      options match {
        case JSValue.JSStr(s) => Some(s)
        case JSValue.Object(_) =>
          optionField(options, "encoding") match {
            case JSValue.JSStr(s) => Some(s)
            case _                => None
          }
        case _ => None
      }

    /** Convert a JS value to the bytes `fs.write` should store. */
    def bytesFromData(value: JSValue, encoding: String)(using
        ctx: JSContext
    ): Array[Byte] =
      value match {
        case JSValue.JSStr(s) => NodeEncodings.bytesFromString(s, encoding)
        case other =>
          NodeBuffer.bytesOfValue(other).getOrElse {
            BuiltinHelpers.toJSString(other) match {
              case text => NodeEncodings.bytesFromString(text, encoding)
            }
          }
      }

    def bufferOrString(bytes: Array[Byte], options: JSValue)(using
        ctx: JSContext
    ): JSValue =
      encodingOf(options) match {
        case Some(enc) => JSValue.fromString(NodeEncodings.stringFromBytes(bytes, enc))
        case None      => NodeBuffer.makeBuffer(bytes)
      }

    def dateValue(millis: Long)(using ctx: JSContext): JSValue =
      ctx.global.get("Date") match {
        case JSValue.Native(nc: NativeConstructor) =>
          nc.construct(Array(JSValue.fromDouble(millis.toDouble)))
        case _ => JSValue.Undefined
      }

    def statObject(path: Path, followLinks: Boolean)(using
        ctx: JSContext
    ): JSObject = {
      val data = guard("stat", path.toString)(rawStat(path, followLinks))
      statValue(data)
    }

    def statValue(data: RawStat)(using ctx: JSContext): JSObject = {
      val obj = JSObject(prototype = ctx.objectPrototype)
      def flag(name: String, value: Boolean): Unit =
        obj.set(
          name,
          JSValue.Native(
            NativeFunction(
              name = name,
              length = 0,
              impl = (_, _) => JSValue.Bool(value)
            )
          )
        )
      flag("isFile", data.isFile)
      flag("isDirectory", data.isDirectory)
      flag("isSymbolicLink", data.isSymbolicLink)
      flag("isBlockDevice", false)
      flag("isCharacterDevice", false)
      flag("isFIFO", false)
      flag("isSocket", false)
      obj.set("size", JSValue.fromDouble(data.size.toDouble))
      obj.set("mtimeMs", JSValue.fromDouble(data.mtimeMs.toDouble))
      obj.set("atimeMs", JSValue.fromDouble(data.atimeMs.toDouble))
      obj.set("ctimeMs", JSValue.fromDouble(data.ctimeMs.toDouble))
      obj.set("birthtimeMs", JSValue.fromDouble(data.birthtimeMs.toDouble))
      obj.set("mtime", dateValue(data.mtimeMs))
      obj.set("atime", dateValue(data.atimeMs))
      obj.set("ctime", dateValue(data.ctimeMs))
      obj.set("birthtime", dateValue(data.birthtimeMs))
      obj.set("dev", JSValue.fromDouble(data.dev.toDouble))
      obj.set("ino", JSValue.fromDouble(data.ino.toDouble))
      obj.set("mode", JSValue.fromInt(data.mode))
      obj.set("nlink", JSValue.fromDouble(data.nlink.toDouble))
      obj.set("uid", JSValue.fromInt(data.uid))
      obj.set("gid", JSValue.fromInt(data.gid))
      obj.set("rdev", JSValue.fromDouble(data.rdev.toDouble))
      obj.set("blksize", JSValue.fromInt(4096))
      obj.set("blocks", JSValue.fromDouble(0))
      obj
    }

    def direntValue(
        path: Path,
        name: String,
        isFile: Boolean,
        isDirectory: Boolean,
        isSymbolicLink: Boolean
    )(using ctx: JSContext): JSObject = {
      val obj = JSObject(prototype = ctx.objectPrototype)
      obj.set("name", JSValue.fromString(name))
      obj.set("parentPath", JSValue.fromString(path.toString))
      obj.set("path", JSValue.fromString(path.toString))
      def flag(label: String, value: Boolean): Unit =
        obj.set(
          label,
          JSValue.Native(
            NativeFunction(label, (_, _) => JSValue.Bool(value), length = 0)
          )
        )
      flag("isFile", isFile)
      flag("isDirectory", isDirectory)
      flag("isSymbolicLink", isSymbolicLink)
      flag("isBlockDevice", false)
      flag("isCharacterDevice", false)
      flag("isFIFO", false)
      flag("isSocket", false)
      obj
    }

    def dirent(path: Path, name: String)(using ctx: JSContext): JSObject = {
      val flags = rawReaddir(path).find(_._1 == name)
      flags match {
        case Some((_, isFile, isDirectory, isSymbolicLink)) =>
          direntValue(path, name, isFile, isDirectory, isSymbolicLink)
        case None =>
          direntValue(path, name, false, false, false)
      }
    }

    def method(
        name: String,
        arity: Int
    )(impl: (Array[JSValue], JSContext) => JSValue): NativeFunction = {
      val fn = NativeFunction(
        name = name,
        length = arity,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          impl(strip(args), callCtx)
        }
      )
      fs.set(name, JSValue.Native(fn))
      fn
    }

    // ---- reading / writing -------------------------------------------------

    method("readFileSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val target = args.headOption.getOrElse(JSValue.Undefined)
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val path = resolvePath(target)
      val bytes = guard("open", path.toString) {
        Files.readAllBytes(path)
      }
      bufferOrString(bytes, options)
    })

    def writeFileImpl(
        args: Array[JSValue],
        callCtx: JSContext,
        append: Boolean
    ): JSValue = {
      given JSContext = callCtx
      val target = args.headOption.getOrElse(JSValue.Undefined)
      val data = args.lift(1).getOrElse(JSValue.Undefined)
      val options = args.lift(2).getOrElse(JSValue.Undefined)
      val encoding = encodingOf(options).getOrElse("utf8")
      val path = resolvePath(target)
      val bytes = bytesFromData(data, encoding)
      guard(if append then "open" else "open", path.toString) {
        val opts =
          if append then
            Array(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
          else
            Array(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
        Files.write(path, bytes, opts*)
      }
      JSValue.Undefined
    }

    method("writeFileSync", 3)((args, callCtx) =>
      writeFileImpl(args, callCtx, append = false)
    )
    method("appendFileSync", 3)((args, callCtx) =>
      writeFileImpl(args, callCtx, append = true)
    )

    method("existsSync", 1)((args, _) => {
      val target = args.headOption.getOrElse(JSValue.Undefined)
      try {
        val path = resolvePath(target)
        JSValue.Bool(Files.exists(path))
      } catch case _: Throwable => JSValue.Bool(false)
    })

    method("accessSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val target = args.headOption.getOrElse(JSValue.Undefined)
      val path = resolvePath(target)
      if !Files.exists(path) then
        NodeHelpers.throwCoded(
          "Error",
          s"ENOENT: no such file or directory, access '${path}'",
          "ENOENT"
        )
      JSValue.Undefined
    })

    method("statSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      JSValue.Object(statObject(path, followLinks = true))
    })
    method("lstatSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      JSValue.Object(statObject(path, followLinks = false))
    })

    method("readdirSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val target = args.headOption.getOrElse(JSValue.Undefined)
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val path = resolvePath(target)
      val withFileTypes = optionField(options, "withFileTypes").toBoolean
      val entries = guard("scandir", path.toString)(rawReaddir(path))
      val result = JSArray.empty()
      entries.foreach { case (name, isFile, isDirectory, isSymbolicLink) =>
        result.push(
          if withFileTypes then
            JSValue.Object(
              direntValue(path, name, isFile, isDirectory, isSymbolicLink)
            )
          else JSValue.fromString(name)
        )
      }
      JSValue.JSArrayVal(result)
    })

    method("mkdirSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val target = args.headOption.getOrElse(JSValue.Undefined)
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val path = resolvePath(target)
      val recursive =
        if options.isInstanceOf[JSValue.Object] then
          optionField(options, "recursive").toBoolean
        else options.toBoolean
      guard("mkdir", path.toString) {
        if recursive then Files.createDirectories(path)
        else Files.createDirectory(path)
      }
      JSValue.Undefined
    })

    method("rmSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val target = args.headOption.getOrElse(JSValue.Undefined)
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val path = resolvePath(target)
      val recursive = optionField(options, "recursive").toBoolean
      val force = optionField(options, "force").toBoolean
      if !Files.exists(path) then {
        if !force then
          NodeHelpers.throwCoded(
            "Error",
            s"ENOENT: no such file or directory, rm '${path}'",
            "ENOENT"
          )
      } else if Files.isDirectory(path) && !Files.isSymbolicLink(path) then {
        if recursive then {
          guard("rm", path.toString) {
            val stream = Files.walk(path)
            try {
              stream
                .sorted(java.util.Comparator.reverseOrder())
                .forEach(p => Files.deleteIfExists(p))
            } finally stream.close()
          }
        } else {
          guard("rm", path.toString)(Files.delete(path))
        }
      } else {
        guard("unlink", path.toString)(Files.delete(path))
      }
      JSValue.Undefined
    })

    method("unlinkSync", 1)((args, callCtx) => {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      guard("unlink", path.toString)(Files.delete(path))
      JSValue.Undefined
    })

    method("rmdirSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val target = args.headOption.getOrElse(JSValue.Undefined)
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val recursive = optionField(options, "recursive").toBoolean
      val path = resolvePath(target)
      if recursive then {
        guard("rmdir", path.toString) {
          val stream = Files.walk(path)
          try {
            stream
              .sorted(java.util.Comparator.reverseOrder())
              .forEach(p => Files.deleteIfExists(p))
          } finally stream.close()
        }
      } else guard("rmdir", path.toString)(Files.delete(path))
      JSValue.Undefined
    })

    method("renameSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val from = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      val to = resolvePath(args.lift(1).getOrElse(JSValue.Undefined))
      guard("rename", from.toString) { Files.move(from, to) }
      JSValue.Undefined
    })

    method("copyFileSync", 3)((args, callCtx) => {
      given JSContext = callCtx
      val from = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      val to = resolvePath(args.lift(1).getOrElse(JSValue.Undefined))
      val mode = args.lift(2).getOrElse(JSValue.Undefined)
      val flags = if mode.isInstanceOf[JSValue.Int32] then mode.toNumber.toInt else 0
      val options =
        if (flags & 1) != 0 then
          Array(StandardCopyOption.REPLACE_EXISTING)
        else Array.empty[StandardCopyOption]
      guard("copyfile", from.toString) {
        Files.copy(from, to, options*)
      }
      JSValue.Undefined
    })

    method("realpathSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val target = args.headOption.getOrElse(JSValue.Undefined)
      val path = resolvePath(target)
      val real = guard("realpath", path.toString)(path.toRealPath())
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      options match {
        case JSValue.Object(_) =>
          JSValue.Object(
            NodeHelpers.objectOf(
              "path" -> JSValue.fromString(real.toString)
            )
          )
        case _ => JSValue.fromString(real.toString)
      }
    })

    method("readlinkSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      val target = guard("readlink", path.toString)(Files.readSymbolicLink(path))
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      options match {
        case JSValue.Object(_) =>
          NodeHelpers.throwCoded(
            "Error",
            "readlink with encoding 'buffer' is not supported",
            "ERR_INVALID_ARG_VALUE"
          )
        case _ => JSValue.fromString(target.toString)
      }
    })

    method("symlinkSync", 3)((args, callCtx) => {
      given JSContext = callCtx
      val linkTarget = NodeHelpers.toPath(args.headOption.getOrElse(JSValue.Undefined))
      val linkPath = resolvePath(args.lift(1).getOrElse(JSValue.Undefined))
      guard("symlink", linkPath.toString) {
        Files.createSymbolicLink(linkPath, Paths.get(linkTarget))
      }
      JSValue.Undefined
    })

    method("truncateSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      val length = args.lift(1).filter(_ != JSValue.Undefined).map(v => NodeHelpers.toNumber(v).toLong).getOrElse(0L)
      guard("open", path.toString) {
        val channel = FileChannel.open(path, StandardOpenOption.WRITE)
        try channel.truncate(length)
        finally channel.close()
      }
      JSValue.Undefined
    })

    method("chmodSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      val mode = args.lift(1).filter(_ != JSValue.Undefined).map(v => NodeHelpers.toNumber(v).toInt).getOrElse(0)
      guard("chmod", path.toString) {
        import java.nio.file.attribute.PosixFilePermission
        val permissions = new java.util.HashSet[PosixFilePermission]()
        if (mode & 0x100) != 0 then permissions.add(PosixFilePermission.OWNER_READ)
        if (mode & 0x80) != 0 then permissions.add(PosixFilePermission.OWNER_WRITE)
        if (mode & 0x40) != 0 then permissions.add(PosixFilePermission.OWNER_EXECUTE)
        if (mode & 0x20) != 0 then permissions.add(PosixFilePermission.GROUP_READ)
        if (mode & 0x10) != 0 then permissions.add(PosixFilePermission.GROUP_WRITE)
        if (mode & 0x08) != 0 then permissions.add(PosixFilePermission.GROUP_EXECUTE)
        if (mode & 0x04) != 0 then permissions.add(PosixFilePermission.OTHERS_READ)
        if (mode & 0x02) != 0 then permissions.add(PosixFilePermission.OTHERS_WRITE)
        if (mode & 0x01) != 0 then permissions.add(PosixFilePermission.OTHERS_EXECUTE)
        Files.setPosixFilePermissions(path, permissions)
      }
      JSValue.Undefined
    })

    method("chownSync", 3)((args, callCtx) => JSValue.Undefined)

    method("utimesSync", 3)((args, callCtx) => {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      val atime = NodeHelpers.toNumber(args.lift(1).getOrElse(JSValue.Undefined))
      val mtime = NodeHelpers.toNumber(args.lift(2).getOrElse(JSValue.Undefined))
      guard("utime", path.toString) {
        val view = java.nio.file.attribute.FileTime.fromMillis((mtime * 1000).toLong)
        val access = java.nio.file.attribute.FileTime.fromMillis((atime * 1000).toLong)
        Files.setAttribute(path, "basic:lastModifiedTime", view)
        Files.setAttribute(path, "basic:lastAccessTime", access)
      }
      JSValue.Undefined
    })

    method("mkdtempSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val prefix = NodeHelpers.toPath(args.headOption.getOrElse(JSValue.Undefined))
      val (parent, namePrefix) = mkdtempParentAndPrefix(prefix)
      val dir = guard("mkdtemp", prefix) {
        Files.createTempDirectory(parent, namePrefix)
      }
      JSValue.fromString(dir.toString)
    })

    // ---- open/read/write/close --------------------------------------------

    def openOptions(flags: String): Array[java.nio.file.OpenOption] =
      flags match {
        case "r"   => Array(StandardOpenOption.READ)
        case "r+"  => Array(StandardOpenOption.READ, StandardOpenOption.WRITE)
        case "w"   => Array(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
        case "w+"  => Array(StandardOpenOption.CREATE, StandardOpenOption.READ, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
        case "a"   => Array(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
        case "a+"  => Array(StandardOpenOption.CREATE, StandardOpenOption.READ, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
        case "wx"  => Array(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
        case "wx+" => Array(StandardOpenOption.CREATE_NEW, StandardOpenOption.READ, StandardOpenOption.WRITE)
        case other =>
          if other.contains("+") then
            Array(StandardOpenOption.READ, StandardOpenOption.WRITE)
          else Array(StandardOpenOption.READ)
      }

    method("openSync", 3)((args, callCtx) => {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      val flagsValue = args.lift(1).getOrElse(JSValue.Undefined)
      val flags =
        flagsValue match {
          case JSValue.JSStr(s) => s
          case JSValue.Int32(n) => n match {
            case 0 => "r"
            case 1 => "w"
            case 2 => "r+"
            case _ => "r"
          }
          case _ => "r"
        }
      val channel = guard("open", path.toString) {
        FileChannel.open(path, openOptions(flags)*)
      }
      val fd = nextFd
      nextFd += 1
      openChannels(fd) = channel
      JSValue.fromInt(fd)
    })

    method("closeSync", 1)((args, _) => {
      args.headOption match {
        case Some(JSValue.Int32(fd)) =>
          openChannels.remove(fd).foreach(_.close())
        case _ => ()
      }
      JSValue.Undefined
    })

    method("readSync", 5)((args, callCtx) => {
      given JSContext = callCtx
      val fd = args.headOption.collect { case JSValue.Int32(n) => n }.getOrElse(-1)
      val buffer = args.lift(1).getOrElse(JSValue.Undefined)
      val offset = args.lift(2).filter(_ != JSValue.Undefined).map(v => NodeHelpers.toNumber(v).toInt).getOrElse(0)
      val length = args.lift(3).filter(_ != JSValue.Undefined).map(v => NodeHelpers.toNumber(v).toInt).getOrElse(0)
      val position = args.lift(4).filter(_ != JSValue.Undefined).map(v => NodeHelpers.toNumber(v).toLong)
      openChannels.get(fd) match {
        case Some(channel) =>
          val bufferView = NodeBuffer.bytesOfValue(buffer).getOrElse(Array.emptyByteArray)
          val temp = java.nio.ByteBuffer.allocate(length)
          val read =
            position match {
              case Some(pos) => channel.read(temp, pos)
              case None      => channel.read(temp)
            }
          if read > 0 then {
            // Copy back through the original typed array view.
            buffer match {
              case JSValue.Object(obj) =>
                obj.getOwnPropertyRaw("__taView") match {
                  case Some(JSValue.Native(view: quickjs.runtime.builtins.TypedArrayBuiltins.TypedArrayView)) =>
                    var i = 0
                    while i < read do {
                      view.buffer.data(view.byteOffset + offset + i) =
                        temp.array()(i)
                      i += 1
                    }
                  case _ => ()
                }
              case _ => ()
            }
          }
          JSValue.fromInt(math.max(0, read))
        case None => JSValue.fromInt(-1)
      }
    })

    method("writeSync", 5)((args, callCtx) => {
      given JSContext = callCtx
      val fd = args.headOption.collect { case JSValue.Int32(n) => n }.getOrElse(-1)
      args.lift(1).getOrElse(JSValue.Undefined) match {
        case JSValue.JSStr(text) =>
          openChannels.get(fd) match {
            case Some(channel) =>
              val bytes = NodeEncodings.bytesFromString(text, "utf8")
              val written = channel.write(java.nio.ByteBuffer.wrap(bytes))
              JSValue.fromInt(written)
            case None => JSValue.fromInt(-1)
          }
        case buffer =>
          openChannels.get(fd) match {
            case Some(channel) =>
              val bytes = NodeBuffer.bytesOfValue(buffer).getOrElse(Array.emptyByteArray)
              val offset = args.lift(2).filter(_ != JSValue.Undefined).map(v => NodeHelpers.toNumber(v).toInt).getOrElse(0)
              val length = args.lift(3).filter(_ != JSValue.Undefined).map(v => NodeHelpers.toNumber(v).toInt).getOrElse(bytes.length - offset)
              val slice = bytes.slice(offset, offset + length)
              val written = channel.write(java.nio.ByteBuffer.wrap(slice))
              JSValue.fromInt(written)
            case None => JSValue.fromInt(-1)
          }
      }
    })

    method("fstatSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val fd = args.headOption.collect { case JSValue.Int32(n) => n }.getOrElse(-1)
      openChannels.get(fd) match {
        case Some(channel) =>
          // Reuse stat by path is not possible; return a minimal stats object.
          val obj = JSObject(prototype = ctx.objectPrototype)
          obj.set("size", JSValue.fromDouble(channel.size().toDouble))
          JSValue.Object(obj)
        case None =>
          NodeHelpers.throwCoded("Error", "EBADF: bad file descriptor", "EBADF")
      }
    })

    // ---- async callbacks ---------------------------------------------------

    /** One asynchronous operation: `work` runs on the host pool, `finish`
      * runs back on the loop thread and produces the callback arguments.
      */
    final case class AsyncSpec(
        syscall: String,
        target: String,
        work: () => Any,
        finish: Either[Throwable, Any] => Array[JSValue]
    )

    def splitCallback(args: Array[JSValue]): (Array[JSValue], Option[JSValue]) =
      if args.nonEmpty && BuiltinHelpers.isCallable(args.last) then
        (args.dropRight(1), Some(args.last))
      else (args, None)

    def asyncMethod(name: String, arity: Int)(
        setup: (Array[JSValue], JSContext) => AsyncSpec
    ): NativeFunction = {
      val fn = NativeFunction(
        name = name,
        length = arity,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val real = strip(args)
          splitCallback(real) match {
            case (opArgs, Some(callback)) =>
              val spec = setup(opArgs, callCtx)
              loop.execute {
                val result =
                  try Right(spec.work())
                  catch { case e: Throwable => Left(e) }
                loop.post { () =>
                  val callbackArgs = spec.finish(result)
                  BuiltinHelpers.callFunctionWithThis(
                    callback,
                    JSValue.Undefined,
                    callbackArgs
                  )
                }
              }
            case (_, None) =>
              NodeHelpers.throwCoded(
                "TypeError",
                "Callback must be a function",
                "ERR_INVALID_ARG_TYPE"
              )
          }
          JSValue.Undefined
        }
      )
      fs.set(name, JSValue.Native(fn))
      fn
    }

    def unitFinish(
        syscall: String,
        target: String
    )(result: Either[Throwable, Any])(using ctx: JSContext): Array[JSValue] =
      result match {
        case Left(e)  => Array(fsErrorValue(e, syscall, target))
        case Right(_) => Array(JSValue.Null)
      }

    asyncMethod("readFile", 3)((args, callCtx) => {
      given JSContext = callCtx
      val targetValue = args.headOption.getOrElse(JSValue.Undefined)
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val path = resolvePath(targetValue)
      AsyncSpec(
        "open",
        path.toString,
        () => Files.readAllBytes(path),
        result => {
          given JSContext = callCtx
          result match {
            case Left(e) => Array(fsErrorValue(e, "open", path.toString))
            case Right(bytes) =>
              Array(
                JSValue.Null,
                bufferOrString(bytes.asInstanceOf[Array[Byte]], options)
              )
          }
        }
      )
    })

    def writeAsyncSetup(append: Boolean)(
        args: Array[JSValue],
        callCtx: JSContext
    ): AsyncSpec = {
      given JSContext = callCtx
      val targetValue = args.headOption.getOrElse(JSValue.Undefined)
      val dataValue = args.lift(1).getOrElse(JSValue.Undefined)
      val options = args.lift(2).getOrElse(JSValue.Undefined)
      val path = resolvePath(targetValue)
      val encoding = encodingOf(options).getOrElse("utf8")
      val bytes = bytesFromData(dataValue, encoding)
      AsyncSpec(
        "open",
        path.toString,
        () => {
          val opts =
            if append then
              Array(
                StandardOpenOption.CREATE,
                StandardOpenOption.WRITE,
                StandardOpenOption.APPEND
              )
            else
              Array(
                StandardOpenOption.CREATE,
                StandardOpenOption.WRITE,
                StandardOpenOption.TRUNCATE_EXISTING
              )
          Files.write(path, bytes, opts*)
          ()
        },
        result => unitFinish("open", path.toString)(result)(using callCtx)
      )
    }
    asyncMethod("writeFile", 3)(writeAsyncSetup(append = false))
    asyncMethod("appendFile", 3)(writeAsyncSetup(append = true))

    asyncMethod("readdir", 3)((args, callCtx) => {
      given JSContext = callCtx
      val targetValue = args.headOption.getOrElse(JSValue.Undefined)
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val path = resolvePath(targetValue)
      AsyncSpec(
        "scandir",
        path.toString,
        () => rawReaddir(path),
        result => {
          given JSContext = callCtx
          result match {
            case Left(e) => Array(fsErrorValue(e, "scandir", path.toString))
            case Right(raw) =>
              val withFileTypes = optionField(options, "withFileTypes").toBoolean
              val result = JSArray.empty()
              raw
                .asInstanceOf[Vector[(String, Boolean, Boolean, Boolean)]]
                .foreach { case (name, isFile, isDirectory, isSymbolicLink) =>
                  result.push(
                    if withFileTypes then
                      JSValue.Object(
                        direntValue(
                          path,
                          name,
                          isFile,
                          isDirectory,
                          isSymbolicLink
                        )
                      )
                    else JSValue.fromString(name)
                  )
                }
              Array(JSValue.Null, JSValue.JSArrayVal(result))
          }
        }
      )
    })

    def statAsyncSetup(followLinks: Boolean)(
        args: Array[JSValue],
        callCtx: JSContext
    ): AsyncSpec = {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      AsyncSpec(
        "stat",
        path.toString,
        () => rawStat(path, followLinks),
        result => {
          given JSContext = callCtx
          result match {
            case Left(e) => Array(fsErrorValue(e, "stat", path.toString))
            case Right(data) =>
              Array(JSValue.Null, JSValue.Object(statValue(data.asInstanceOf[RawStat])))
          }
        }
      )
    }
    asyncMethod("stat", 2)(statAsyncSetup(followLinks = true))
    asyncMethod("lstat", 2)(statAsyncSetup(followLinks = false))

    asyncMethod("mkdir", 2)((args, callCtx) => {
      given JSContext = callCtx
      val targetValue = args.headOption.getOrElse(JSValue.Undefined)
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val path = resolvePath(targetValue)
      val recursive =
        if options.isInstanceOf[JSValue.Object] then
          optionField(options, "recursive").toBoolean
        else options.toBoolean
      AsyncSpec(
        "mkdir",
        path.toString,
        () => {
          if recursive then Files.createDirectories(path)
          else Files.createDirectory(path)
          ()
        },
        result => unitFinish("mkdir", path.toString)(result)(using callCtx)
      )
    })

    asyncMethod("rm", 2)((args, callCtx) => {
      given JSContext = callCtx
      val targetValue = args.headOption.getOrElse(JSValue.Undefined)
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val path = resolvePath(targetValue)
      val recursive = optionField(options, "recursive").toBoolean
      val force = optionField(options, "force").toBoolean
      AsyncSpec(
        "rm",
        path.toString,
        () => {
          if !Files.exists(path) then {
            if !force then throw new NoSuchFileException(path.toString)
          } else if Files.isDirectory(path) && !Files.isSymbolicLink(path) then {
            if recursive then {
              val stream = Files.walk(path)
              try
                stream
                  .sorted(java.util.Comparator.reverseOrder())
                  .forEach(p => Files.deleteIfExists(p))
              finally stream.close()
            } else Files.delete(path)
          } else Files.delete(path)
          ()
        },
        result => unitFinish("rm", path.toString)(result)(using callCtx)
      )
    })

    def deleteAsyncSetup(syncName: String, syscall: String)(
        args: Array[JSValue],
        callCtx: JSContext
    ): AsyncSpec = {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      AsyncSpec(
        syscall,
        path.toString,
        () => { Files.delete(path); () },
        result => unitFinish(syscall, path.toString)(result)(using callCtx)
      )
    }
    asyncMethod("unlink", 1)(deleteAsyncSetup("unlink", "unlink"))
    asyncMethod("rmdir", 2)(deleteAsyncSetup("rmdir", "rmdir"))

    asyncMethod("rename", 2)((args, callCtx) => {
      given JSContext = callCtx
      val from = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      val to = resolvePath(args.lift(1).getOrElse(JSValue.Undefined))
      AsyncSpec(
        "rename",
        from.toString,
        () => { Files.move(from, to); () },
        result => unitFinish("rename", from.toString)(result)(using callCtx)
      )
    })

    asyncMethod("copyFile", 3)((args, callCtx) => {
      given JSContext = callCtx
      val from = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      val to = resolvePath(args.lift(1).getOrElse(JSValue.Undefined))
      val mode = args.lift(2).getOrElse(JSValue.Undefined)
      val flags = if mode.isInstanceOf[JSValue.Int32] then mode.toNumber.toInt else 0
      AsyncSpec(
        "copyfile",
        from.toString,
        () => {
          val options =
            if (flags & 1) != 0 then Array(StandardCopyOption.REPLACE_EXISTING)
            else Array.empty[StandardCopyOption]
          Files.copy(from, to, options*)
          ()
        },
        result => unitFinish("copyfile", from.toString)(result)(using callCtx)
      )
    })

    asyncMethod("realpath", 2)((args, callCtx) => {
      given JSContext = callCtx
      val targetValue = args.headOption.getOrElse(JSValue.Undefined)
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val path = resolvePath(targetValue)
      AsyncSpec(
        "realpath",
        path.toString,
        () => path.toRealPath().toString,
        result => {
          given JSContext = callCtx
          result match {
            case Left(e) => Array(fsErrorValue(e, "realpath", path.toString))
            case Right(real) =>
              val text = real.asInstanceOf[String]
              options match {
                case JSValue.Object(_) =>
                  Array(
                    JSValue.Null,
                    JSValue.Object(
                      NodeHelpers.objectOf("path" -> JSValue.fromString(text))
                    )
                  )
                case _ => Array(JSValue.Null, JSValue.fromString(text))
              }
          }
        }
      )
    })

    def accessAsyncSetup(
        args: Array[JSValue],
        callCtx: JSContext
    ): AsyncSpec = {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      AsyncSpec(
        "access",
        path.toString,
        () => {
          if !Files.exists(path) then throw new NoSuchFileException(path.toString)
          ()
        },
        result => unitFinish("access", path.toString)(result)(using callCtx)
      )
    }
    asyncMethod("access", 2)(accessAsyncSetup)

    asyncMethod("readlink", 2)((args, callCtx) => {
      given JSContext = callCtx
      val path = resolvePath(args.headOption.getOrElse(JSValue.Undefined))
      AsyncSpec(
        "readlink",
        path.toString,
        () => Files.readSymbolicLink(path).toString,
        result => {
          given JSContext = callCtx
          result match {
            case Left(e) => Array(fsErrorValue(e, "readlink", path.toString))
            case Right(target) => Array(JSValue.Null, JSValue.fromString(target.asInstanceOf[String]))
          }
        }
      )
    })

    asyncMethod("symlink", 3)((args, callCtx) => {
      given JSContext = callCtx
      val linkTarget = NodeHelpers.toPath(args.headOption.getOrElse(JSValue.Undefined))
      val path = resolvePath(args.lift(1).getOrElse(JSValue.Undefined))
      AsyncSpec(
        "symlink",
        path.toString,
        () => { Files.createSymbolicLink(path, Paths.get(linkTarget)); () },
        result => unitFinish("symlink", path.toString)(result)(using callCtx)
      )
    })

    asyncMethod("truncate", 2)((args, callCtx) => {
      given JSContext = callCtx
      val targetValue = args.headOption.getOrElse(JSValue.Undefined)
      val path = resolvePath(targetValue)
      val length =
        args
          .lift(1)
          .filter(_ != JSValue.Undefined)
          .map(v => NodeHelpers.toNumber(v).toLong)
          .getOrElse(0L)
      AsyncSpec(
        "open",
        path.toString,
        () => {
          val channel = FileChannel.open(path, StandardOpenOption.WRITE)
          try channel.truncate(length)
          finally channel.close()
          ()
        },
        result => unitFinish("open", path.toString)(result)(using callCtx)
      )
    })

    asyncMethod("chmod", 2)((args, callCtx) => {
      given JSContext = callCtx
      val targetValue = args.headOption.getOrElse(JSValue.Undefined)
      val path = resolvePath(targetValue)
      val mode =
        args
          .lift(1)
          .filter(_ != JSValue.Undefined)
          .map(v => NodeHelpers.toNumber(v).toInt)
          .getOrElse(0)
      AsyncSpec(
        "chmod",
        path.toString,
        () => {
          import java.nio.file.attribute.PosixFilePermission
          val permissions =
            new java.util.HashSet[PosixFilePermission]()
          if (mode & 0x100) != 0 then permissions.add(PosixFilePermission.OWNER_READ)
          if (mode & 0x80) != 0 then permissions.add(PosixFilePermission.OWNER_WRITE)
          if (mode & 0x40) != 0 then permissions.add(PosixFilePermission.OWNER_EXECUTE)
          if (mode & 0x20) != 0 then permissions.add(PosixFilePermission.GROUP_READ)
          if (mode & 0x10) != 0 then permissions.add(PosixFilePermission.GROUP_WRITE)
          if (mode & 0x08) != 0 then permissions.add(PosixFilePermission.GROUP_EXECUTE)
          if (mode & 0x04) != 0 then permissions.add(PosixFilePermission.OTHERS_READ)
          if (mode & 0x02) != 0 then permissions.add(PosixFilePermission.OTHERS_WRITE)
          if (mode & 0x01) != 0 then permissions.add(PosixFilePermission.OTHERS_EXECUTE)
          Files.setPosixFilePermissions(path, permissions)
          ()
        },
        result => unitFinish("chmod", path.toString)(result)(using callCtx)
      )
    })

    asyncMethod("mkdtemp", 2)((args, callCtx) => {
      given JSContext = callCtx
      val prefix = NodeHelpers.toPath(args.headOption.getOrElse(JSValue.Undefined))
      val (parent, namePrefix) = mkdtempParentAndPrefix(prefix)
      AsyncSpec(
        "mkdtemp",
        prefix,
        () => Files.createTempDirectory(parent, namePrefix).toString,
        result => {
          given JSContext = callCtx
          result match {
            case Left(e)  => Array(fsErrorValue(e, "mkdtemp", prefix))
            case Right(dir) => Array(JSValue.Null, JSValue.fromString(dir.asInstanceOf[String]))
          }
        }
      )
    })

    // ---- promises ----------------------------------------------------------

    val promises = JSObject(prototype = ctx.objectPrototype)

    def promiseMethod(promiseName: String, asyncName: String, arity: Int): Unit = {
      fs.get(asyncName)(using ctx) match {
        case JSValue.Native(asyncFn: NativeFunction) =>
          val wrapper = NativeFunction(
            name = promiseName,
            length = arity,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              // Drop any leading receiver (`fs/promises` itself, or an
              // arbitrary object via `stat.call(obj, path)`); the async
              // implementation strips its own `fs` receiver afterwards.
              val real = NodeHelpers.stripPathReceiver(args)
              val promiseCtor = callCtx.global.get("Promise") match {
                case JSValue.Native(nc: NativeConstructor) => nc
                case _ =>
                  NodeHelpers.throwCoded(
                    "TypeError",
                    "Promise is not available",
                    "ERR_INTERNAL"
                  )
              }
              val executor = NativeFunction(
                name = "executor",
                length = 2,
                impl = (execArgs, execCtx) => {
                  given JSContext = execCtx
                  val resolve = execArgs.lift(1).getOrElse(JSValue.Undefined)
                  val reject = execArgs.lift(2).getOrElse(JSValue.Undefined)
                  val callback = NativeFunction(
                    name = "callback",
                    length = 2,
                    impl = (cbArgs, cbCtx) => {
                      given JSContext = cbCtx
                      val err = cbArgs.lift(1).getOrElse(JSValue.Undefined)
                      val value = cbArgs.lift(2).getOrElse(JSValue.Undefined)
                      if err != JSValue.Undefined && err != JSValue.Null then
                        BuiltinHelpers.callFunctionWithThis(
                          reject,
                          JSValue.Undefined,
                          Array(err)
                        )
                      else
                        BuiltinHelpers.callFunctionWithThis(
                          resolve,
                          JSValue.Undefined,
                          Array(value)
                        )
                      JSValue.Undefined
                    }
                  )
                  asyncFn.call(
                    Array(JSValue.Object(fs)) ++ real ++ Array(JSValue.Native(callback))
                  )
                  JSValue.Undefined
                }
              )
              promiseCtor.construct(Array(JSValue.Native(executor)))
            }
          )
          promises.set(promiseName, JSValue.Native(wrapper))
        case _ => ()
      }
    }

    Seq(
      ("readFile", 2),
      ("writeFile", 3),
      ("appendFile", 3),
      ("readdir", 2),
      ("stat", 2),
      ("lstat", 2),
      ("mkdir", 2),
      ("rm", 2),
      ("unlink", 1),
      ("rmdir", 2),
      ("rename", 2),
      ("copyFile", 3),
      ("realpath", 2),
      ("access", 2),
      ("readlink", 2),
      ("symlink", 3),
      ("truncate", 2),
      ("chmod", 2),
      ("mkdtemp", 2)
    ).foreach((name, arity) => promiseMethod(name, name, arity))
    fs.set("promises", JSValue.Object(promises))

    // ---- constants ---------------------------------------------------------

    val constants = JSObject(prototype = null)
    val constantValues = Map(
      "F_OK" -> 0,
      "R_OK" -> 4,
      "W_OK" -> 2,
      "X_OK" -> 1,
      "COPYFILE_EXCL" -> 1,
      "COPYFILE_FICLONE" -> 2,
      "COPYFILE_FICLONE_FORCE" -> 4,
      "O_RDONLY" -> 0,
      "O_WRONLY" -> 1,
      "O_RDWR" -> 2,
      "O_CREAT" -> 64,
      "O_EXCL" -> 128,
      "O_NOCTTY" -> 256,
      "O_TRUNC" -> 512,
      "O_APPEND" -> 1024,
      "O_NONBLOCK" -> 2048,
      "O_DSYNC" -> 4096,
      "O_DIRECT" -> 16384,
      "O_DIRECTORY" -> 65536,
      "O_NOFOLLOW" -> 131072,
      "O_NOATIME" -> 262144,
      "O_SYNC" -> 1052672,
      "UV_FS_O_FILEMAP" -> 0,
      "S_IFMT" -> 0xf000,
      "S_IFREG" -> 0x8000,
      "S_IFDIR" -> 0x4000,
      "S_IFLNK" -> 0xa000,
      "S_IFIFO" -> 0x1000,
      "S_IFCHR" -> 0x2000,
      "S_IFBLK" -> 0x6000,
      "S_IFSOCK" -> 0xc000,
      "S_IRWXU" -> 448,
      "S_IRUSR" -> 256,
      "S_IWUSR" -> 128,
      "S_IXUSR" -> 64,
      "S_IRWXG" -> 56,
      "S_IRGRP" -> 32,
      "S_IWGRP" -> 16,
      "S_IXGRP" -> 8,
      "S_IRWXO" -> 7,
      "S_IROTH" -> 4,
      "S_IWOTH" -> 2,
      "S_IXOTH" -> 1
    )
    constantValues.foreach((name, value) =>
      constants.set(name, JSValue.fromInt(value))
    )
    fs.set("constants", JSValue.Object(constants))

    // =======================================================================
    // fs.watch / fs.watchFile
    // =======================================================================



    /** Shared `WatchService` (one inotify instance for all `fs.watch`
      * handles, like libuv) with per-key dispatch.
      */

    /** Shared `WatchService` (one inotify instance for all `fs.watch`
      * handles, like libuv) with per-key dispatch.
      */
    trait WatchTarget {
      def dispatch(
          dir: Path,
          events: java.util.List[java.nio.file.WatchEvent[?]]
      ): Unit
    }


    final class WatchRegistry {
      private var service: java.nio.file.WatchService = null
      private val keyOwners =
        mutable.HashMap.empty[java.nio.file.WatchKey, (WatchTarget, Path)]
      private val active = mutable.LinkedHashSet.empty[WatchTarget]

      private def ensureStarted(): Unit =
        if service == null then {
          service = FileSystems.getDefault.newWatchService()
          loop.retain()
          val thread = new Thread(() => run())
          thread.setDaemon(true)
          thread.start()
        }

      def addWatcher(handle: WatchTarget): Unit =
        ensureStarted()
        active += handle

      def register(handle: WatchTarget, dir: Path): Unit =
        if service != null then
          try {
            val key = dir.register(
              service,
              StandardWatchEventKinds.ENTRY_CREATE,
              StandardWatchEventKinds.ENTRY_DELETE,
              StandardWatchEventKinds.ENTRY_MODIFY
            )
            keyOwners(key) = (handle, dir)
          } catch case _: Throwable => ()

      def close(handle: WatchTarget): Unit = {
        active -= handle
        val keys = keyOwners
          .collect { case (key, (owner, _)) if owner eq handle => key }
          .toList
        keys.foreach { key =>
          keyOwners.remove(key)
          try key.cancel() catch case _: Throwable => ()
        }
        if active.isEmpty && service != null then {
          val current = service
          service = null
          try current.close() catch case _: Throwable => ()
        }
      }

      private def run(): Unit =
        try {
          var running = true
          while running do {
            val key =
              try service.take()
              catch {
                case _: java.nio.file.ClosedWatchServiceException => null
                case _: InterruptedException                     => null
              }
            if key == null then running = false
            else {
              keyOwners.get(key) match {
                case Some((handle, dir)) => handle.dispatch(dir, key.pollEvents())
                case None                => key.pollEvents()
              }
              if !key.reset() then keyOwners.remove(key)
            }
          }
        } finally loop.release()
    }

    val watchRegistry = new WatchRegistry

    final class WatchHandle(
        val target: Path,
        isDirectory: Boolean,
        recursive: Boolean,
        listener: JSValue
    ) extends WatchTarget {
      @volatile private var closed = false
      private val emitter = new JsEmitter
      val obj = JSObject(prototype = ctx.objectPrototype)
      emitter.install(obj)
      obj.set(
        "close",
        JSValue.Native(
          NativeFunction(
            name = "close",
            length = 0,
            impl = (_, _) => {
              close()
              JSValue.Undefined
            }
          )
        )
      )
      obj.set(
        "ref",
        JSValue.Native(
          NativeFunction(
            name = "ref",
            length = 0,
            impl = (_, _) => JSValue.Object(obj)
          )
        )
      )
      obj.set(
        "unref",
        JSValue.Native(
          NativeFunction(
            name = "unref",
            length = 0,
            impl = (_, _) => JSValue.Object(obj)
          )
        )
      )
      obj.set("path", JSValue.fromString(target.toString))

      private def registerDir(dir: Path): Unit =
        watchRegistry.register(this, dir)

      private def registerSubdirs(dir: Path): Unit =
        try {
          val stream = Files.newDirectoryStream(dir)
          try
            stream.forEach { child =>
              if Files.isDirectory(child) && !Files.isSymbolicLink(child) then {
                registerDir(child)
                registerSubdirs(child)
              }
            }
          finally stream.close()
        } catch case _: Throwable => ()

      def start(): Unit = {
        if !Files.exists(target, LinkOption.NOFOLLOW_LINKS) then
          loop.post { () =>
            val error = ctx.createError(
              "Error",
              s"ENOENT: no such file or directory, watch '${target}'"
            )
            error match {
              case JSValue.Object(errObj) =>
                errObj.set("code", JSValue.fromString("ENOENT"))
                errObj.set("syscall", JSValue.fromString("watch"))
                errObj.set("path", JSValue.fromString(target.toString))
              case _ => ()
            }
            emitter.emit("error", Array(error))
          }
        watchRegistry.addWatcher(this)
        if isDirectory then {
          registerDir(target)
          if recursive then registerSubdirs(target)
        } else Option(target.getParent).foreach(registerDir)
      }

      /** Called on the shared watch thread. */
      def dispatch(
          dir: Path,
          events: java.util.List[java.nio.file.WatchEvent[?]]
      ): Unit =
        if !closed then {
          var index = 0
          while index < events.size() do {
            val event = events.get(index)
            val kind = event.kind()
            if kind != StandardWatchEventKinds.OVERFLOW then {
              val relative = event.context().asInstanceOf[Path]
              val full = if dir != null then dir.resolve(relative) else relative
              if kind == StandardWatchEventKinds.ENTRY_CREATE && recursive &&
                  Files.isDirectory(full)
              then registerSubdirs(full)
              val relevant =
                isDirectory || (dir != null && full == target)
              if relevant then {
                val eventType =
                  if kind == StandardWatchEventKinds.ENTRY_MODIFY then "change"
                  else "rename"
                val name =
                  if isDirectory then relative.toString
                  else target.getFileName.toString
                loop.post { () =>
                  if !closed then
                    BuiltinHelpers.callFunctionWithThis(
                      listener,
                      JSValue.Undefined,
                      Array(
                        JSValue.fromString(eventType),
                        JSValue.fromString(name)
                      )
                    )
                }
              }
            }
            index += 1
          }
        }

      def close(): Unit =
        if !closed then {
          closed = true
          watchRegistry.close(this)
          emitter.emit("close", Array.empty)
        }
    }


    fs.set(
      "watch",
      JSValue.Native(
        NativeFunction(
          name = "watch",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            val path = resolvePath(rest.headOption.getOrElse(JSValue.Undefined))
            val options =
              if rest.lift(1).exists(BuiltinHelpers.isCallable) then
                JSValue.Undefined
              else rest.lift(1).getOrElse(JSValue.Undefined)
            val listener = rest
              .find(BuiltinHelpers.isCallable)
              .getOrElse(JSValue.Undefined)
            if !BuiltinHelpers.isCallable(listener) then
              NodeHelpers.throwCoded(
                "TypeError",
                "The listener argument must be of type function",
                "ERR_INVALID_ARG_TYPE"
              )
            val recursive = options match {
              case JSValue.Object(_) =>
                BuiltinHelpers.getPropertyWithGetter(options, "recursive") match {
                  case JSValue.Bool(true) => true
                  case _                  => false
                }
              case _ => false
            }
            val handle =
              new WatchHandle(path, Files.isDirectory(path), recursive, listener)
            handle.start()
            JSValue.Object(handle.obj)
          }
        )
      )
    )

    final class StatWatcherHandle(
        val target: Path,
        val intervalMs: Long,
        val listener: JSValue
    ) {
      @volatile private var closed = false
      private val emitter = new JsEmitter
      val obj = JSObject(prototype = ctx.objectPrototype)
      emitter.install(obj)
      private var last: RawStat = null
      private val thread = new Thread(() => run())
      thread.setDaemon(true)

      private def currentStat(): Option[RawStat] =
        try Some(rawStat(target, true))
        catch case _: Throwable => None

      def start(): Unit = {
        last = currentStat().orNull
        thread.start()
      }

      private def run(): Unit =
        while !closed do {
          try Thread.sleep(intervalMs)
          catch case _: InterruptedException => ()
          if !closed then
            currentStat() match {
              case Some(current) =>
                val previous = last
                val changed =
                  previous == null || previous.mtimeMs != current.mtimeMs ||
                    previous.size != current.size ||
                    previous.isFile != current.isFile
                if changed then {
                  last = current
                  loop.post { () =>
                    if !closed then
                      BuiltinHelpers.callFunctionWithThis(
                        listener,
                        JSValue.Undefined,
                        Array(
                          JSValue.Object(statValue(current)),
                          JSValue.Object(
                            if previous != null then statValue(previous)
                            else statValue(current)
                          )
                        )
                      )
                  }
                }
              case None => ()
            }
        }

      def close(): Unit =
        if !closed then {
          closed = true
          thread.interrupt()
          emitter.emit("close", Array.empty)
        }
      obj.set(
        "close",
        JSValue.Native(
          NativeFunction(
            name = "close",
            length = 0,
            impl = (_, _) => {
              close()
              JSValue.Undefined
            }
          )
        )
      )
      obj.set(
        "stop",
        JSValue.Native(
          NativeFunction(
            name = "stop",
            length = 0,
            impl = (_, _) => {
              close()
              JSValue.Undefined
            }
          )
        )
      )
      obj.set(
        "ref",
        JSValue.Native(
          NativeFunction(
            name = "ref",
            length = 0,
            impl = (_, _) => JSValue.Object(obj)
          )
        )
      )
      obj.set(
        "unref",
        JSValue.Native(
          NativeFunction(
            name = "unref",
            length = 0,
            impl = (_, _) => JSValue.Object(obj)
          )
        )
      )
    }

    val statWatchers =
      mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[StatWatcherHandle]]

    fs.set(
      "watchFile",
      JSValue.Native(
        NativeFunction(
          name = "watchFile",
          length = 3,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            val path = resolvePath(rest.headOption.getOrElse(JSValue.Undefined))
            val (options, listener) =
              if rest.lift(1).exists(BuiltinHelpers.isCallable) then
                (JSValue.Undefined, rest.lift(1).getOrElse(JSValue.Undefined))
              else
                (
                  rest.lift(1).getOrElse(JSValue.Undefined),
                  rest.lift(2).getOrElse(JSValue.Undefined)
                )
            if !BuiltinHelpers.isCallable(listener) then
              NodeHelpers.throwCoded(
                "TypeError",
                "The listener argument must be of type function",
                "ERR_INVALID_ARG_TYPE"
              )
            val interval = options match {
              case JSValue.Object(_) =>
                BuiltinHelpers.getPropertyWithGetter(options, "interval") match {
                  case JSValue.Int32(n) if n > 0     => n.toLong
                  case JSValue.Float64(d) if d > 0   => d.toLong
                  case _                             => 5007L
                }
              case _ => 5007L
            }
            val handle = new StatWatcherHandle(path, interval, listener)
            handle.start()
            statWatchers
              .getOrElseUpdate(path.toString, mutable.ArrayBuffer.empty) += handle
            JSValue.Undefined
          }
        )
      )
    )
    fs.set(
      "unwatchFile",
      JSValue.Native(
        NativeFunction(
          name = "unwatchFile",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            val key =
              resolvePath(rest.headOption.getOrElse(JSValue.Undefined)).toString
            val listener = rest.lift(1)
            statWatchers.get(key).foreach { handles =>
              listener match {
                case Some(fn) =>
                  val (matching, remaining) =
                    handles.partition(h => NodeHelpers.sameValue(h.listener, fn))
                  matching.foreach(_.close())
                  handles.clear()
                  handles ++= remaining
                case None =>
                  handles.foreach(_.close())
                  handles.clear()
              }
              if handles.isEmpty then statWatchers.remove(key)
            }
            JSValue.Undefined
          }
        )
      )
    )

    JSValue.Object(fs)
  }
}
