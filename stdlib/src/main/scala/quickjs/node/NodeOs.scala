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
    define("constants", JSValue.Object(JSObject(prototype = null)))
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
