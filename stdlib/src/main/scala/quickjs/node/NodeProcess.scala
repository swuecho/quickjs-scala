package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.{JSArray, JSObject}

import java.nio.file.{Files, Paths}

import scala.collection.mutable

/** Node's `process` global. */
object NodeProcess {

  /** Best-effort path to a `node` binary on PATH. Scripts (shelljs) spawn
    * `process.execPath` to run JS helpers; our JVM home is not a JS runtime,
    * so prefer a real Node when one is installed. */
  private[node] def findNodeExecutable(): Option[String] =
    val names = Seq("node", "node.exe")
    val pathDirs = sys.env
      .get("PATH")
      .toSeq
      .flatMap(_.split(java.io.File.pathSeparator))
    pathDirs.iterator
      .flatMap(dir => names.iterator.map(n => java.nio.file.Paths.get(dir, n)))
      .find(p => java.nio.file.Files.isExecutable(p))
      .map(_.toString)

  def create(state: NodeState, loop: HostEventLoop, streams: NodeStream)(using
      ctx: JSContext
  ): JSObject = {
    val process = JSObject(prototype = ctx.objectPrototype)

    def strip(args: Array[JSValue]): Array[JSValue] =
      NodeHelpers.stripReceiver(args, process)

    def method(name: String, arity: Int)(
        impl: (Array[JSValue], JSContext) => JSValue
    ): NativeFunction = {
      val fn = NativeFunction(
        name = name,
        length = arity,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          impl(strip(args), callCtx)
        }
      )
      process.set(name, JSValue.Native(fn))
      fn
    }

    // ---- identity / environment -------------------------------------------

    val argvArray = JSArray.empty()
    state.argv.foreach(a => argvArray.push(JSValue.fromString(a)))
    process.set("argv", JSValue.JSArrayVal(argvArray))
    process.set(
      "argv0",
      JSValue.fromString(state.argv.headOption.getOrElse("node"))
    )
    process.set(
      "execPath",
      JSValue.fromString(
        NodeProcess.findNodeExecutable().getOrElse(
          Option(System.getProperty("java.home")).getOrElse("node")
        )
      )
    )
    process.set("execArgv", JSValue.JSArrayVal(JSArray.empty()))
    process.set("pid", JSValue.fromInt(ProcessHandle.current().pid().toInt))
    process.set(
      "ppid",
      JSValue.fromInt(
        ProcessHandle.current().parent().map(_.pid().toInt).orElse(0)
      )
    )
    process.set("platform", JSValue.fromString(NodeOs.platformName))
    process.set("arch", JSValue.fromString(NodeOs.archName))
    process.set("version", JSValue.fromString(state.options.version))
    process.set(
      "versions",
      JSValue.Object(
        NodeHelpers.objectOf(
          "node" -> JSValue.fromString(state.options.version),
          "v8" -> JSValue.fromString("11.0.0"),
          "uv" -> JSValue.fromString("1.44.0"),
          "openssl" -> JSValue.fromString("3.0.0")
        )
      )
    )
    process.set(
      "release",
      JSValue.Object(
        NodeHelpers.objectOf(
          "name" -> JSValue.fromString("node"),
          "sourceUrl" -> JSValue.fromString(""),
          "headersUrl" -> JSValue.fromString("")
        )
      )
    )
    process.set("title", JSValue.fromString("node"))
    process.set("connected", JSValue.Bool(false))
    process.set("channel", JSValue.Undefined)
    process.set("allowedNodeEnvironmentFlags", JSValue.JSArrayVal(JSArray.empty()))
    process.set("config", JSValue.Object(JSObject(prototype = null)))
    process.set("features", JSValue.Object(JSObject(prototype = null)))

    val env = JSObject(prototype = null)
    state.options.env.toSeq.sortBy(_._1).foreach { case (key, value) =>
      env.set(key, JSValue.fromString(value))
    }
    process.set("env", JSValue.Object(env))

    // ---- cwd / chdir -------------------------------------------------------

    method("cwd", 0)((_, _) => JSValue.fromString(state.cwd.toString))
    method("chdir", 1)((args, callCtx) => {
      given JSContext = callCtx
      val target = args.headOption.map(NodeHelpers.toPath(_)).getOrElse("")
      val path =
        if Paths.get(target).isAbsolute then Paths.get(target)
        else state.cwd.resolve(target)
      val normalized = path.toAbsolutePath.normalize
      if !Files.isDirectory(normalized) then
        NodeHelpers.throwCoded(
          "Error",
          s"ENOENT: no such file or directory, chdir '$target'",
          "ENOENT"
        )
      state.chdir(normalized)
      JSValue.Undefined
    })

    // ---- exit --------------------------------------------------------------

    // exitCode is a plain property updated on exit().
    process.set("exitCode", JSValue.Undefined)

    def runExitListeners(code: Int): Unit = {
      state.exitListeners.foreach { listener =>
        try
          BuiltinHelpers.callFunctionWithThis(
            listener,
            JSValue.Object(process),
            Array(JSValue.fromInt(code))
          )
        catch case _: Throwable => ()
      }
      state.exitListeners.clear()
    }

    method("exit", 1)((args, callCtx) => {
      given JSContext = callCtx
      val code =
        args.headOption match {
          case Some(JSValue.Int32(n))   => n
          case Some(JSValue.Float64(d)) => d.toInt
          case Some(JSValue.Undefined) | None => state.exitCode
          case Some(other)              => NodeHelpers.toNumber(other).toInt
        }
      state.exitCode = code
      process.set("exitCode", JSValue.fromInt(code))
      runExitListeners(code)
      throw new NodeExit(code)
    })

    method("reallyExit", 1)((args, callCtx) => {
      given JSContext = callCtx
      val code =
        args.headOption match {
          case Some(JSValue.Int32(n)) => n
          case _                      => state.exitCode
        }
      throw new NodeExit(code)
    })

    // ---- scheduled work ----------------------------------------------------

    method("nextTick", 1)((args, callCtx) => {
      given JSContext = callCtx
      val callback = args.headOption.getOrElse(JSValue.Undefined)
      val rest = if args.length > 1 then args.drop(1) else Array.empty[JSValue]
      callCtx.queueMicrotask { () =>
        try
          BuiltinHelpers.callFunctionWithThis(
            callback,
            JSValue.Undefined,
            rest
          )
        catch case _: Throwable => ()
      }
      JSValue.Undefined
    })

    val startNanos = System.nanoTime()

    def hrtimeImpl(args: Array[JSValue], callCtx: JSContext): JSValue = {
      given JSContext = callCtx
      def current(): (Long, Long) = {
        val elapsed = System.nanoTime() - startNanos
        (elapsed / 1000000000L, elapsed % 1000000000L)
      }
      val (seconds, nanos) =
        if args.nonEmpty && args(0) != JSValue.Undefined then {
          args(0) match {
            case JSValue.JSArrayVal(arr) if arr.getLength >= 2 =>
              val prevSeconds = arr.get(0).toNumber.toLong
              val prevNanos = arr.get(1).toNumber.toLong
              val now = current()
              var s = now._1 - prevSeconds
              var n = now._2 - prevNanos
              if n < 0 then {
                s -= 1
                n += 1000000000L
              }
              (s, n)
            case _ => current()
          }
        } else current()
      val arr = JSArray.empty()
      arr.push(JSValue.fromDouble(seconds.toDouble))
      arr.push(JSValue.fromDouble(nanos.toDouble))
      JSValue.JSArrayVal(arr)
    }

    val hrtimeFn = NativeFunction(
      name = "hrtime",
      length = 1,
      impl = (args, callCtx) => hrtimeImpl(strip(args), callCtx)
    )
    val hrtimeBigintFn = NativeFunction(
      name = "bigint",
      length = 0,
      impl = (_, _) => {
        val elapsed = System.nanoTime() - startNanos
        JSValue.BigInt(java.math.BigInteger.valueOf(elapsed))
      }
    )
    hrtimeFn.funcObj.set("bigint", JSValue.Native(hrtimeBigintFn))
    process.set("hrtime", JSValue.Native(hrtimeFn))

    // ---- misc --------------------------------------------------------------

    method("uptime", 0)((_, _) =>
      JSValue.fromDouble(
        (System.nanoTime() - startNanos).toDouble / 1e9
      )
    )
    method("memoryUsage", 0)((_, _) => {
      val runtime = Runtime.getRuntime
      val used = runtime.totalMemory() - runtime.freeMemory()
      JSValue.Object(
        NodeHelpers.objectOf(
          "rss" -> JSValue.fromDouble(
            java.lang.management.ManagementFactory.getOperatingSystemMXBean match {
              case bean: com.sun.management.OperatingSystemMXBean =>
                bean.getCommittedVirtualMemorySize.toDouble
              case _ => runtime.totalMemory().toDouble
            }
          ),
          "heapTotal" -> JSValue.fromDouble(runtime.totalMemory().toDouble),
          "heapUsed" -> JSValue.fromDouble(used.toDouble),
          "external" -> JSValue.fromDouble(0),
          "arrayBuffers" -> JSValue.fromDouble(0)
        )
      )
    })
    method("getuid", 0)((_, _) => JSValue.fromInt(-1))
    method("getgid", 0)((_, _) => JSValue.fromInt(-1))
    method("setuid", 1)((_, _) => JSValue.Undefined)
    method("setgid", 1)((_, _) => JSValue.Undefined)
    method("umask", 1)((_, _) => JSValue.fromInt(0))
    method("emitWarning", 1)((_, _) => JSValue.Undefined)
    method("abort", 0)((_, _) => throw new NodeExit(134))
    method("loadEnvFile", 1)((_, _) => JSValue.Undefined)
    method("kill", 1)((_, _) => JSValue.Bool(false))

    // ---- event emitter -----------------------------------------------------

    val processListeners =
      mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[JSValue]]

    def addProcessListener(event: String, listener: JSValue): Unit = {
      processListeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty) += listener
      if event == "exit" then state.exitListeners += listener
    }

    def emitProcessEvent(event: String, eventArgs: Array[JSValue]): Boolean = {
      val handlers =
        processListeners.getOrElse(event, mutable.ArrayBuffer.empty).toSeq
      handlers.foreach(fn =>
        BuiltinHelpers.callFunctionWithThis(
          fn,
          JSValue.Object(process),
          eventArgs
        )
      )
      handlers.nonEmpty
    }

    method("on", 2)((args, callCtx) => {
      given JSContext = callCtx
      val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      addProcessListener(event, listener)
      JSValue.Object(process)
    })
    method("addListener", 2)((args, callCtx) => {
      given JSContext = callCtx
      val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      addProcessListener(event, listener)
      JSValue.Object(process)
    })
    // `once` registers a wrapper that removes itself after the first call.
    method("once", 2)((args, callCtx) => {
      given JSContext = callCtx
      val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      lazy val wrapper: NativeFunction = NativeFunction(
        name = event,
        impl = (callArgs, innerCtx) => {
          given JSContext = innerCtx
          removeProcessListener(event, JSValue.Native(wrapper))
          BuiltinHelpers.callFunctionWithThis(
            listener,
            JSValue.Object(process),
            callArgs
          )
        }
      )
      addProcessListener(event, JSValue.Native(wrapper))
      JSValue.Object(process)
    })
    def removeProcessListener(event: String, listener: JSValue): Unit = {
      processListeners.get(event).foreach { list =>
        val index = list.indexWhere(l => NodeHelpers.sameValue(l, listener))
        if index >= 0 then list.remove(index)
      }
      if event == "exit" then {
        val index = state.exitListeners.indexWhere(l =>
          NodeHelpers.sameValue(l, listener)
        )
        if index >= 0 then state.exitListeners.remove(index)
      }
    }
    method("removeListener", 2)((args, callCtx) => {
      val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      removeProcessListener(event, listener)
      JSValue.Object(process)
    })
    method("off", 2)((args, callCtx) => {
      val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      removeProcessListener(event, listener)
      JSValue.Object(process)
    })
    method("removeAllListeners", 1)((args, callCtx) => {
      val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      if event.isEmpty then processListeners.clear()
      else processListeners.remove(event)
      if event.isEmpty || event == "exit" then state.exitListeners.clear()
      JSValue.Object(process)
    })
    method("emit", 2)((args, callCtx) => {
      given JSContext = callCtx
      val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      JSValue.Bool(emitProcessEvent(event, args.drop(1)))
    })
    method("listenerCount", 2)((args, callCtx) => {
      val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val count = processListeners.get(event).map(_.length).getOrElse(0)
      JSValue.fromInt(count)
    })

    // ---- streams -----------------------------------------------------------

    def makeWriteStream(
        name: String,
        writeImpl: String => Unit,
        toStdErr: Boolean
    ): JSObject = {
      val stream = JSObject(prototype = null)
      stream.set("isTTY", JSValue.Bool(false))
      stream.set("fd", JSValue.fromInt(if toStdErr then 2 else 1))
      stream.set(
        "write",
        JSValue.Native(
          NativeFunction(
            name = "write",
            length = 1,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              val text = args.lift(1).map(NodeHelpers.toStr(_)).getOrElse("")
              writeImpl(text)
              JSValue.Bool(true)
            }
          )
        )
      )
      stream.set("end", JSValue.Native(NativeFunction(
        name = "end",
        length = 0,
        impl = (_, _) => JSValue.Undefined
      )))
      // Node-style emitter surface: rxjs's `fromEvent` classifies a target as
      // an event emitter only when it has `addListener` and `removeListener`,
      // and many libraries call `emit`/`once`/`off` on process streams.
      val streamListeners =
        mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[JSValue]]
      def streamStrip(args: Array[JSValue]): Array[JSValue] =
        args.headOption match {
          case Some(JSValue.Object(o)) if o eq stream => args.drop(1)
          case _                                      => args
        }
      def addStreamListener(event: String, listener: JSValue): Unit =
        streamListeners.getOrElseUpdate(
          event,
          mutable.ArrayBuffer.empty
        ) += listener
      def emitStreamEvent(
          event: String,
          eventArgs: Array[JSValue]
      )(using JSContext): Boolean = {
        val handlers =
          streamListeners.getOrElse(event, mutable.ArrayBuffer.empty).toSeq
        handlers.foreach(fn =>
          BuiltinHelpers.callFunctionWithThis(
            fn,
            JSValue.Object(stream),
            eventArgs
          )
        )
        handlers.nonEmpty
      }
      def streamMethod(name: String, arity: Int)(
          impl: (Array[JSValue], JSContext) => JSValue
      ): Unit =
        stream.set(
          name,
          JSValue.Native(NativeFunction(
            name = name,
            length = arity,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              impl(streamStrip(args), callCtx)
            }
          ))
        )
      streamMethod("on", 2)((args, callCtx) => {
        given JSContext = callCtx
        addStreamListener(
          args.headOption.map(NodeHelpers.toStr(_)).getOrElse(""),
          args.lift(1).getOrElse(JSValue.Undefined)
        )
        JSValue.Object(stream)
      })
      streamMethod("addListener", 2)((args, callCtx) => {
        given JSContext = callCtx
        addStreamListener(
          args.headOption.map(NodeHelpers.toStr(_)).getOrElse(""),
          args.lift(1).getOrElse(JSValue.Undefined)
        )
        JSValue.Object(stream)
      })
      streamMethod("once", 2)((args, callCtx) => {
        given JSContext = callCtx
        val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
        val listener = args.lift(1).getOrElse(JSValue.Undefined)
        lazy val wrapper: NativeFunction = NativeFunction(
          name = event,
          impl = (callArgs, innerCtx) => {
            given JSContext = innerCtx
            streamListeners.get(event).foreach { list =>
              val index = list.indexWhere(l =>
                NodeHelpers.sameValue(l, JSValue.Native(wrapper))
              )
              if index >= 0 then list.remove(index)
            }
            BuiltinHelpers.callFunctionWithThis(
              listener,
              JSValue.Object(stream),
              callArgs
            )
          }
        )
        addStreamListener(event, JSValue.Native(wrapper))
        JSValue.Object(stream)
      })
      streamMethod("removeListener", 2)((args, _) => {
        val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
        val listener = args.lift(1).getOrElse(JSValue.Undefined)
        streamListeners.get(event).foreach { list =>
          val index = list.indexWhere(l => NodeHelpers.sameValue(l, listener))
          if index >= 0 then list.remove(index)
        }
        JSValue.Object(stream)
      })
      streamMethod("off", 2)((args, _) => {
        val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
        val listener = args.lift(1).getOrElse(JSValue.Undefined)
        streamListeners.get(event).foreach { list =>
          val index = list.indexWhere(l => NodeHelpers.sameValue(l, listener))
          if index >= 0 then list.remove(index)
        }
        JSValue.Object(stream)
      })
      streamMethod("removeAllListeners", 1)((args, _) => {
        val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
        if event.isEmpty then streamListeners.clear()
        else streamListeners.remove(event)
        JSValue.Object(stream)
      })
      streamMethod("emit", 2)((args, callCtx) => {
        given JSContext = callCtx
        val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
        JSValue.Bool(emitStreamEvent(event, args.drop(1)))
      })
      stream.set(
        "listenerCount",
        JSValue.Native(NativeFunction(
          name = "listenerCount",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val event = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            JSValue.fromInt(streamListeners.get(event).map(_.length).getOrElse(0))
          }
        ))
      )
      stream.set("pipe", JSValue.Native(NativeFunction(
        name = "pipe",
        length = 1,
        impl = (args, _) => args.headOption.getOrElse(JSValue.Undefined)
      )))
      stream
    }

    process.set(
      "stdout",
      JSValue.Object(makeWriteStream("stdout", s => System.out.print(s), false))
    )
    process.set(
      "stderr",
      JSValue.Object(makeWriteStream("stderr", s => System.err.print(s), true))
    )
    // ---- stdin ------------------------------------------------------------
    // A real Readable backed by `System.in`. Reading starts lazily the first
    // time something asks for data (readline, `on('data')`, `resume()`), and
    // the loop is retained until EOF so a script waiting on stdin stays
    // alive.
    val stdinValue = streams.newReadable()
    process.set("stdin", stdinValue)

    var stdinStarted = false
    def startStdinReading(): Unit =
      if !stdinStarted then {
        stdinStarted = true
        loop.retain()
        val thread = new Thread(() => {
          val buffer = new Array[Byte](4096)
          try {
            var read = System.in.read(buffer)
            while read >= 0 do {
              if read > 0 then {
                val chunk = java.util.Arrays.copyOf(buffer, read)
                loop.post { () =>
                  streams.pushToStream(stdinValue, NodeBuffer.makeBuffer(chunk))
                }
              }
              read = System.in.read(buffer)
            }
          } catch case _: Throwable => ()
          finally {
            loop.post { () => streams.endStream(stdinValue) }
            loop.release()
          }
        })
        thread.setDaemon(true)
        thread.start()
      }

    stdinValue match {
      case JSValue.Object(stdin) =>
        stdin.set("isTTY", JSValue.Bool(System.console() != null))
        stdin.set("fd", JSValue.fromInt(0))
        val protoOn = BuiltinHelpers.getPropertyWithGetter(stdinValue, "on")
        val protoResume =
          BuiltinHelpers.getPropertyWithGetter(stdinValue, "resume")
        val protoRead = BuiltinHelpers.getPropertyWithGetter(stdinValue, "read")
        def stripStdin(args: Array[JSValue]): Array[JSValue] =
          args.headOption match {
            case Some(JSValue.Object(obj)) if obj eq stdin => args.drop(1)
            case _                                          => args
          }
        def wrap(name: String, arity: Int, delegate: JSValue): Unit =
          stdin.set(
            name,
            JSValue.Native(
              NativeFunction(
                name = name,
                length = arity,
                impl = (args, callCtx) => {
                  given JSContext = callCtx
                  startStdinReading()
                  if BuiltinHelpers.isCallable(delegate) then
                    BuiltinHelpers.callFunctionWithThis(
                      delegate,
                      stdinValue,
                      stripStdin(args)
                    )
                  else JSValue.Undefined
                }
              )
            )
          )
        wrap("on", 2, protoOn)
        wrap("addListener", 2, protoOn)
        wrap("resume", 0, protoResume)
        if protoRead != JSValue.Undefined then wrap("read", 1, protoRead)
        stdin.set(
          "setRawMode",
          JSValue.Native(
            NativeFunction(
              name = "setRawMode",
              length = 1,
              impl = (_, _) => stdinValue
            )
          )
        )
        stdin.set(
          "ref",
          JSValue.Native(
            NativeFunction(
              name = "ref",
              length = 0,
              impl = (_, _) => stdinValue
            )
          )
        )
        stdin.set(
          "unref",
          JSValue.Native(
            NativeFunction(
              name = "unref",
              length = 0,
              impl = (_, _) => stdinValue
            )
          )
        )
      case _ => ()
    }

    process
  }

  /** Return the current `process.exitCode` (used by the runner). */
  def exitCodeOf(process: JSObject)(using ctx: JSContext): Int =
    process.get("exitCode") match {
      case JSValue.Int32(n)   => n
      case JSValue.Float64(d) => d.toInt
      case _                  => 0
    }
}
