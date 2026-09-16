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
    // Top-level await may wait on timers or async I/O; drive the event loop
    // until the awaited module promise settles.
    ctx.rt.setHostAwaitDriver(until =>
      loop.runUntil(ctx, propagateErrors = true, until)
    )

    // Buffer must exist before fs, which creates Buffer instances.
    val buffer = NodeBuffer.create()
    ctx.global.set("Buffer", buffer)
    val bufferModule = JSObject(prototype = ctx.objectPrototype)
    bufferModule.set("Buffer", buffer)
    bufferModule.set("SlowBuffer", buffer)
    bufferModule.set("kMaxLength", JSValue.fromDouble(Int.MaxValue.toDouble))
    bufferModule.set("INSPECT_MAX_BYTES", JSValue.fromInt(50))
    loader.registerBuiltin("buffer", JSValue.Object(bufferModule))

    // Stream prototypes must exist before `process.stdin`/`stdout` (and any
    // other host stream) are created from them.
    // `require('console')` returns the global console object (ts-node).
    if ctx.global.get("console") == JSValue.Undefined then
      quickjs.stdlib.Console.initialize()
    ctx.global.get("console") match {
      case value @ JSValue.Object(_) =>
        loader.registerBuiltin("console", value)
      case _ => ()
    }

    val nodeStream = new NodeStream(loop)
    val streamModule = nodeStream.create()
    val process = NodeProcess.create(state, loop, nodeStream)
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
        // Legacy `constants` module: the same values as `fs.constants`, but an
        // ordinary object (with Object.prototype) so `hasOwnProperty` works,
        // matching Node (`graceful-fs` requires it).
        obj.get("constants")(using ctx) match {
          case JSValue.Object(constants) =>
            val legacy = JSObject(prototype = ctx.objectPrototype)
            constants.getAllOwnPropertyKeys().foreach { key =>
              constants.getOwnPropertyDescriptor(key).foreach {
                case (value, attrs) =>
                  legacy.defineProperty(
                    key,
                    value,
                    enumerable = attrs.enumerable,
                    writable = attrs.writable,
                    configurable = attrs.configurable
                  )
              }
            }
            loader.registerBuiltin("constants", JSValue.Object(legacy))
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

    val childProcess = NodeChildProcess.create(state, loop, nodeStream)
    loader.registerBuiltin("child_process", childProcess)

    val querystring = NodeQuerystring.create()
    loader.registerBuiltin("querystring", querystring)

    loader.registerBuiltin("perf_hooks", NodePerfHooks.install())
    loader.registerBuiltin("v8", NodeV8.create())
    val nodeNet = new NodeNet(loop)
    val netModule = nodeNet.create()
    val tlsModule = nodeNet.createTlsModule()
    loader.registerBuiltin("net", netModule)
    loader.registerBuiltin("tls", tlsModule)
    loader.registerBuiltin("http2", NodeHttp2.create())
    loader.registerBuiltin("zlib", NodeZlib.create(loop))

    loader.registerBuiltin("stream", streamModule)
    streamModule match {
      case JSValue.Object(obj) =>
        obj.get("promises")(using ctx) match {
          case promises @ JSValue.Object(_) =>
            loader.registerBuiltin("stream/promises", promises)
          case _ => ()
        }
      case _ => ()
    }
    val readline = NodeReadline.create()
    loader.registerBuiltin("readline", readline)
    loader.registerBuiltin("readline/promises", NodeReadline.createPromises())
    loader.registerBuiltin("repl", NodeRepl.create())
    loader.registerBuiltin("vm", NodeVm.create())
    loader.registerBuiltin("tty", NodeTty.create())
    loader.registerBuiltin("string_decoder", NodeStringDecoder.create())
    loader.registerBuiltin("diagnostics_channel", NodeDiagnostics.create())
    loader.registerBuiltin("module", JSValue.Object(loader.createModuleBuiltin()))
    fsModule match {
      case JSValue.Object(obj) =>
        obj.set("createReadStream", JSValue.Native(nodeStream.createReadStream(state)))
        obj.set("createWriteStream", JSValue.Native(nodeStream.createWriteStream(state)))
      case _ => ()
    }

    val http = new NodeHttp(loop)
    http.install()
    loader.registerBuiltin("http", http.createHttpModule(secure = false, netModule))
    loader.registerBuiltin("https", http.createHttpModule(secure = true, tlsModule))

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
