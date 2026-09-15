package quickjs.node

import quickjs.runtime.JSContext
import quickjs.value.JSValue
import quickjs.objmodel.JSObject

import java.nio.file.Path

/** Installs the Node compatibility surface: globals and built-in modules.
  *
  * {{{
  * given ctx: JSContext = ...
  * val node = NodeRuntime.install(NodeOptions(argv = Vector("node", "script.js")))
  * node.loader.runMainFile(Paths.get("script.js"))
  * }}}
  */
final class NodeRuntime(val options: NodeOptions)(using val ctx: JSContext) {
  val state = new NodeState(options)
  val loop = new HostEventLoop
  val loader = new NodeModuleLoader(options.cwd, options)

  private var fsModule: JSValue = JSValue.Undefined
  private var utilModule: JSValue = JSValue.Undefined

  /** Create all modules and install globals. Returns the module loader so the
    * caller can set it on the runtime.
    */
  def install(): NodeModuleLoader = {
    // Buffer must exist before fs, which creates Buffer instances.
    val buffer = NodeBuffer.create()
    ctx.global.set("Buffer", buffer)
    val bufferModule = JSObject(prototype = ctx.objectPrototype)
    bufferModule.set("Buffer", buffer)
    bufferModule.set("SlowBuffer", buffer)
    bufferModule.set("kMaxLength", JSValue.fromDouble(Int.MaxValue.toDouble))
    bufferModule.set("INSPECT_MAX_BYTES", JSValue.fromInt(50))
    loader.registerBuiltin("buffer", JSValue.Object(bufferModule))

    val process = NodeProcess.create(state)
    ctx.global.set("process", JSValue.Object(process))
    loader.registerBuiltin("process", JSValue.Object(process))

    NodeTimers.installGlobals(loop)
    loader.registerBuiltin("timers", NodeTimers.createModule(loop))
    loader.registerBuiltin("timers/promises", NodeTimers.createPromisesModule(loop))

    // `global` is an alias for `globalThis` in Node.
    ctx.global.set("global", JSValue.Object(ctx.global))

    val path = NodePath.create(state)
    loader.registerBuiltin("path", path)
    path match {
      case JSValue.Object(obj) =>
        obj.get("win32")(using ctx) match {
          case win32 @ JSValue.Object(_) => loader.registerBuiltin("path/win32", win32)
          case _                         => ()
        }
        obj.get("posix")(using ctx) match {
          case posix @ JSValue.Object(_) => loader.registerBuiltin("path/posix", posix)
          case _                         => ()
        }
      case _ => ()
    }

    val os = NodeOs.create()
    loader.registerBuiltin("os", os)

    fsModule = NodeFs.create(state, loop)
    loader.registerBuiltin("fs", fsModule)
    fsModule match {
      case JSValue.Object(obj) =>
        obj.get("promises")(using ctx) match {
          case promises @ JSValue.Object(_) =>
            loader.registerBuiltin("fs/promises", promises)
          case _ => ()
        }
      case _ => ()
    }

    utilModule = NodeUtil.create()
    loader.registerBuiltin("util", utilModule)
    utilModule match {
      case JSValue.Object(obj) =>
        obj.get("types")(using ctx) match {
          case types @ JSValue.Object(_) => loader.registerBuiltin("util/types", types)
          case _                         => ()
        }
      case _ => ()
    }

    val assertModule = NodeAssert.create()
    loader.registerBuiltin("assert", assertModule)
    quickjs.runtime.builtins.BuiltinHelpers.extractJSObject(assertModule).foreach { obj =>
      obj.get("strict")(using ctx) match {
        case JSValue.Undefined => ()
        case strict            => loader.registerBuiltin("assert/strict", strict)
      }
    }

    val events = NodeEvents.create()
    loader.registerBuiltin("events", events)

    val url = NodeUrl.create()
    loader.registerBuiltin("url", url)

    val cryptoModule = NodeCrypto.create()
    loader.registerBuiltin("crypto", cryptoModule)
    NodeCrypto.installGlobal(cryptoModule)

    val childProcess = NodeChildProcess.create(state)
    loader.registerBuiltin("child_process", childProcess)

    val querystring = NodeQuerystring.create()
    loader.registerBuiltin("querystring", querystring)

    loader.registerBuiltin("perf_hooks", NodePerfHooks.install())
    loader.registerBuiltin("zlib", NodeZlib.create(loop))

    val nodeStream = new NodeStream(loop)
    loader.registerBuiltin("stream", nodeStream.create())
    loader.registerBuiltin("tty", NodeTty.create())
    loader.registerBuiltin("string_decoder", NodeStringDecoder.create())
    loader.registerBuiltin("diagnostics_channel", NodeDiagnostics.create())
    fsModule match {
      case JSValue.Object(obj) =>
        obj.set("createReadStream", JSValue.Native(nodeStream.createReadStream(state)))
        obj.set("createWriteStream", JSValue.Native(nodeStream.createWriteStream(state)))
      case _ => ()
    }

    val http = new NodeHttp(loop)
    http.install()
    loader.registerBuiltin("http", http.createHttpModule(secure = false))
    loader.registerBuiltin("https", http.createHttpModule(secure = true))

    loader
  }

  /** Execute a CommonJS source string as the entry point (used by `--eval`). */
  def runSource(source: String, filename: String): JSValue =
    loader.executeCJS(source, filename)

  /** Execute a file as the entry point (CJS or ESM based on extension and the
    * nearest package.json).
    */
  def runFile(path: Path): JSValue =
    loader.runMainFile(path)

  /** Run `process.exit` listeners and return the code the runner should use. */
  def exitCode: Int =
    state.exitCode
}

object NodeRuntime {
  def install(options: NodeOptions = NodeOptions())(using
      ctx: JSContext
  ): NodeRuntime = {
    val runtime = new NodeRuntime(options)
    runtime.install()
    runtime
  }
}
