package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.objmodel.{JSArray, JSObject}

import java.net.InetAddress

/** Node's `os` module. */
object NodeOs {

  val isWindows: Boolean =
    System.getProperty("os.name", "").toLowerCase.contains("win")

  def platformName: String = {
    val os = System.getProperty("os.name", "").toLowerCase
    if os.contains("win") then "win32"
    else if os.contains("mac") || os.contains("darwin") then "darwin"
    else if os.contains("linux") then "linux"
    else if os.contains("freebsd") then "freebsd"
    else if os.contains("sunos") || os.contains("solaris") then "sunos"
    else os.replaceAll("\\s+", "")
  }

  def archName: String = {
    val arch = System.getProperty("os.arch", "").toLowerCase
    arch match {
      case "amd64" | "x86_64"  => "x64"
      case "x86" | "i386" | "i486" | "i586" | "i686" => "ia32"
      case "aarch64" | "arm64" => "arm64"
      case "arm"               => "arm"
      case other               => other
    }
  }

  def create()(using ctx: JSContext): JSValue = {
    val os = JSObject(prototype = ctx.objectPrototype)

    def define(name: String, value: JSValue): Unit = os.set(name, value)
    def method(name: String, arity: Int)(impl: () => JSValue): Unit =
      define(
        name,
        JSValue.Native(
          NativeFunction(
            name = name,
            length = arity,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              val real = NodeHelpers.stripReceiver(args, os)
              impl()
            }
          )
        )
      )

    method("platform", 0)(() => JSValue.fromString(platformName))
    method("arch", 0)(() => JSValue.fromString(archName))
    method("type", 0)(() =>
      JSValue.fromString(platformName match {
        case "linux"  => "Linux"
        case "darwin" => "Darwin"
        case "win32"  => "Windows_NT"
        case other    => other
      })
    )
    method("endianness", 0)(() =>
      JSValue.fromString(
        if java.nio.ByteOrder.nativeOrder() == java.nio.ByteOrder.LITTLE_ENDIAN
        then "LE"
        else "BE"
      )
    )
    define("EOL", JSValue.fromString(if isWindows then "\r\n" else "\n"))
    define("devNull", JSValue.fromString(if isWindows then "\\\\.\\nul" else "/dev/null"))

    method("release", 0)(() =>
      JSValue.fromString(
        Option(System.getProperty("os.version")).getOrElse("unknown")
      )
    )
    method("homedir", 0)(() =>
      JSValue.fromString(
        Option(System.getProperty("user.home")).getOrElse("/")
      )
    )
    method("tmpdir", 0)(() =>
      JSValue.fromString(
        Option(System.getProperty("java.io.tmpdir"))
          .map(_.stripSuffix("/").stripSuffix("\\"))
          .getOrElse("/tmp")
      )
    )
    method("hostname", 0)(() =>
      try JSValue.fromString(InetAddress.getLocalHost.getHostName)
      catch case _: Throwable => JSValue.fromString("localhost")
    )

    // os.constants (Linux values, matching Node on Linux).
    val signals = JSObject(prototype = ctx.objectPrototype)
    Seq(
      "SIGHUP" -> 1, "SIGINT" -> 2, "SIGQUIT" -> 3, "SIGILL" -> 4,
      "SIGTRAP" -> 5, "SIGABRT" -> 6, "SIGIOT" -> 6, "SIGBUS" -> 7,
      "SIGFPE" -> 8, "SIGKILL" -> 9, "SIGUSR1" -> 10, "SIGSEGV" -> 11,
      "SIGUSR2" -> 12, "SIGPIPE" -> 13, "SIGALRM" -> 14, "SIGTERM" -> 15,
      "SIGSTKFLT" -> 16, "SIGCHLD" -> 17, "SIGCONT" -> 18, "SIGSTOP" -> 19,
      "SIGTSTP" -> 20, "SIGTTIN" -> 21, "SIGTTOU" -> 22, "SIGURG" -> 23,
      "SIGXCPU" -> 24, "SIGXFSZ" -> 25, "SIGVTALRM" -> 26, "SIGPROF" -> 27,
      "SIGWINCH" -> 28, "SIGIO" -> 29, "SIGPOLL" -> 29, "SIGPWR" -> 30,
      "SIGSYS" -> 31
    ).foreach((name, value) => signals.set(name, JSValue.fromInt(value)))

    val errno = JSObject(prototype = ctx.objectPrototype)
    Seq(
      "E2BIG" -> 7, "EACCES" -> 13, "EADDRINUSE" -> 98, "EADDRNOTAVAIL" -> 99,
      "EAFNOSUPPORT" -> 97, "EAGAIN" -> 11, "EALREADY" -> 114, "EBADF" -> 9,
      "EBADMSG" -> 74, "EBUSY" -> 16, "ECANCELED" -> 125, "ECHILD" -> 10,
      "ECONNABORTED" -> 103, "ECONNREFUSED" -> 111, "ECONNRESET" -> 104,
      "EDEADLK" -> 35, "EDESTADDRREQ" -> 89, "EDOM" -> 33, "EDQUOT" -> 122,
      "EEXIST" -> 17, "EFAULT" -> 14, "EFBIG" -> 27, "EHOSTUNREACH" -> 113,
      "EIDRM" -> 43, "EILSEQ" -> 84, "EINPROGRESS" -> 115, "EINTR" -> 4,
      "EINVAL" -> 22, "EIO" -> 5, "EISCONN" -> 106, "EISDIR" -> 21,
      "ELOOP" -> 40, "EMFILE" -> 24, "EMLINK" -> 31, "EMSGSIZE" -> 90,
      "EMULTIHOP" -> 72, "ENAMETOOLONG" -> 36, "ENETDOWN" -> 100,
      "ENETRESET" -> 102, "ENETUNREACH" -> 101, "ENFILE" -> 23,
      "ENOBUFS" -> 105, "ENODATA" -> 61, "ENODEV" -> 19, "ENOENT" -> 2,
      "ENOEXEC" -> 8, "ENOLCK" -> 37, "ENOLINK" -> 67, "ENOMEM" -> 12,
      "ENOMSG" -> 42, "ENOPROTOOPT" -> 92, "ENOSPC" -> 28, "ENOSR" -> 63,
      "ENOSTR" -> 60, "ENOSYS" -> 38, "ENOTCONN" -> 107, "ENOTDIR" -> 20,
      "ENOTEMPTY" -> 39, "ENOTSOCK" -> 88, "ENOTSUP" -> 95, "ENOTTY" -> 25,
      "ENXIO" -> 6, "EOPNOTSUPP" -> 95, "EOVERFLOW" -> 75, "EPERM" -> 1,
      "EPIPE" -> 32, "EPROTO" -> 71, "EPROTONOSUPPORT" -> 93,
      "EPROTOTYPE" -> 91, "ERANGE" -> 34, "EROFS" -> 30, "ESPIPE" -> 29,
      "ESRCH" -> 3, "ESTALE" -> 116, "ETIME" -> 62, "ETIMEDOUT" -> 110,
      "ETXTBSY" -> 26, "EWOULDBLOCK" -> 11, "EXDEV" -> 18
    ).foreach((name, value) => errno.set(name, JSValue.fromInt(value)))

    val priority = JSObject(prototype = ctx.objectPrototype)
    Seq(
      "PRIORITY_LOW" -> 19, "PRIORITY_BELOW_NORMAL" -> 10,
      "PRIORITY_NORMAL" -> 0, "PRIORITY_ABOVE_NORMAL" -> -7,
      "PRIORITY_HIGH" -> -14, "PRIORITY_HIGHEST" -> -20
    ).foreach((name, value) => priority.set(name, JSValue.fromInt(value)))

    val dlopen = JSObject(prototype = ctx.objectPrototype)
    Seq(
      "RTLD_LAZY" -> 1, "RTLD_NOW" -> 2, "RTLD_GLOBAL" -> 256,
      "RTLD_LOCAL" -> 0, "RTLD_DEEPBIND" -> 8
    ).foreach((name, value) => dlopen.set(name, JSValue.fromInt(value)))

    val osConstants = JSObject(prototype = ctx.objectPrototype)
    osConstants.set("signals", JSValue.Object(signals))
    osConstants.set("errno", JSValue.Object(errno))
    osConstants.set("priority", JSValue.Object(priority))
    osConstants.set("dlopen", JSValue.Object(dlopen))
    osConstants.set("UV_UDP_REUSEADDR", JSValue.fromInt(4))
    os.set("constants", JSValue.Object(osConstants))
    method("userInfo", 0)(() => {
      val info = JSObject(prototype = ctx.objectPrototype)
      info.set("username", JSValue.fromString(Option(System.getProperty("user.name")).getOrElse("")))
      info.set("uid", JSValue.fromInt(-1))
      info.set("gid", JSValue.fromInt(-1))
      info.set("shell", JSValue.Null)
      info.set("homedir", JSValue.fromString(Option(System.getProperty("user.home")).getOrElse("/")))
      JSValue.Object(info)
    })
    method("uptime", 0)(() =>
      JSValue.fromDouble(
        java.lang.management.ManagementFactory.getRuntimeMXBean.getUptime.toDouble / 1000.0
      )
    )
    method("totalmem", 0)(() =>
      JSValue.fromDouble(
        java.lang.management.ManagementFactory.getOperatingSystemMXBean match {
          case bean: com.sun.management.OperatingSystemMXBean =>
            bean.getTotalMemorySize.toDouble
          case _ => Runtime.getRuntime.maxMemory.toDouble
        }
      )
    )
    method("freemem", 0)(() =>
      JSValue.fromDouble(
        java.lang.management.ManagementFactory.getOperatingSystemMXBean match {
          case bean: com.sun.management.OperatingSystemMXBean =>
            bean.getFreeMemorySize.toDouble
          case _ => Runtime.getRuntime.freeMemory.toDouble
        }
      )
    )
    method("cpus", 0)(() => {
      val arr = JSArray.empty()
      val count = Runtime.getRuntime.availableProcessors()
      var i = 0
      while i < count do {
        val cpu = JSObject(prototype = ctx.objectPrototype)
        cpu.set("model", JSValue.fromString(""))
        cpu.set("speed", JSValue.fromInt(0))
        cpu.set(
          "times",
          JSValue.Object(NodeHelpers.objectOf(
            "user" -> JSValue.fromInt(0),
            "nice" -> JSValue.fromInt(0),
            "sys" -> JSValue.fromInt(0),
            "idle" -> JSValue.fromInt(0),
            "irq" -> JSValue.fromInt(0)
          ))
        )
        arr.push(JSValue.Object(cpu))
        i += 1
      }
      JSValue.JSArrayVal(arr)
    })
    define("availableParallelism", JSValue.Native(NativeFunction(
      name = "availableParallelism",
      length = 0,
      impl = (_, _) =>
        JSValue.fromInt(Runtime.getRuntime.availableProcessors())
    )))
    define("constants", JSValue.Object(osConstants))
    define("networkInterfaces", JSValue.Native(NativeFunction(
      name = "networkInterfaces",
      length = 0,
      impl = (_, callCtx) => {
        given JSContext = callCtx
        JSValue.Object(JSObject(prototype = null))
      }
    )))
    define("version", JSValue.Native(NativeFunction(
      name = "version",
      length = 0,
      impl = (_, _) =>
        JSValue.fromString(
          "Node " + Option(System.getProperty("java.version")).getOrElse("unknown")
        )
    )))

    JSValue.Object(os)
  }
}
