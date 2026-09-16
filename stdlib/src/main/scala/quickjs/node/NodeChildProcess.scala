package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.JSObject

import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Node's `child_process` module (synchronous variants). */
object NodeChildProcess {

  private final case class RunResult(
      status: Int,
      stdout: Array[Byte],
      stderr: Array[Byte],
      timedOut: Boolean,
      startError: Option[Throwable]
  )

  private def readAll(stream: java.io.InputStream): Array[Byte] = {
    val buffer = new java.io.ByteArrayOutputStream()
    val chunk = new Array[Byte](8192)
    var read = stream.read(chunk)
    while read >= 0 do {
      buffer.write(chunk, 0, read)
      read = stream.read(chunk)
    }
    buffer.toByteArray
  }

  private def runCommand(
      command: String,
      args: Seq[String],
      options: JSValue,
      state: NodeState
  )(using ctx: JSContext): RunResult = {
    def optionField(key: String): JSValue =
      options match {
        case JSValue.Object(_) => BuiltinHelpers.getPropertyWithGetter(options, key)
        case _                 => JSValue.Undefined
      }

    val isWindows = NodeOs.isWindows
    val fullCommand =
      if isWindows then Seq("cmd.exe", "/d", "/s", "/c", command)
      else Seq("/bin/sh", "-c", command)

    val builder = new ProcessBuilder(
      (if args.isEmpty then fullCommand else args).asJava
    )
    optionField("cwd") match {
      case JSValue.JSStr(cwd) if cwd.nonEmpty =>
        val path = Paths.get(cwd)
        builder.directory(
          (if path.isAbsolute then path else state.cwd.resolve(path)).toFile
        )
      case _ => builder.directory(state.cwd.toFile)
    }
    optionField("env") match {
      case JSValue.Object(env) =>
        val environment = builder.environment()
        environment.clear()
        env.getAllOwnPropertyKeys().foreach { key =>
          env.getOwnProperty(key).foreach { value =>
            environment.put(key, NodeHelpers.toStr(value))
          }
        }
      case _ => ()
    }

    try {
      val process = builder.start()
      // Feed stdin (the `input` option) then close it.
      optionField("input") match {
        case JSValue.Undefined | JSValue.Null => process.getOutputStream.close()
        case input =>
          val bytes =
            input match {
              case JSValue.JSStr(s) => s.getBytes(StandardCharsets.UTF_8)
              case other            => NodeBuffer.bytesOfValue(other).getOrElse(Array.emptyByteArray)
            }
          process.getOutputStream.write(bytes)
          process.getOutputStream.close()
      }
      var stdoutBytes: Array[Byte] = Array.emptyByteArray
      var stderrBytes: Array[Byte] = Array.emptyByteArray
      val stdoutThread = new Thread(() => stdoutBytes = readAll(process.getInputStream))
      val stderrThread = new Thread(() => stderrBytes = readAll(process.getErrorStream))
      stdoutThread.start()
      stderrThread.start()
      val timeout =
        optionField("timeout") match {
          case JSValue.Int32(n)   => n.toLong
          case JSValue.Float64(d) => d.toLong
          case _                  => 0L
        }
      val finished =
        if timeout > 0 then process.waitFor(timeout, java.util.concurrent.TimeUnit.MILLISECONDS)
        else {
          process.waitFor()
          true
        }
      if !finished then {
        process.destroyForcibly()
        process.waitFor()
      }
      stdoutThread.join()
      stderrThread.join()
      RunResult(
        status = if finished then process.exitValue() else 124,
        stdout = stdoutBytes,
        stderr = stderrBytes,
        timedOut = !finished,
        startError = None
      )
    } catch {
      case e: Throwable =>
        RunResult(1, Array.emptyByteArray, Array.emptyByteArray, timedOut = false, startError = Some(e))
    }
  }

  private def outputValue(
      bytes: Array[Byte],
      encoding: Option[String]
  )(using ctx: JSContext): JSValue =
    encoding match {
      // `encoding: 'buffer'` returns raw Buffers (Node's spawnSync default).
      case Some("buffer") => NodeBuffer.makeBuffer(bytes)
      case Some(enc)      => JSValue.fromString(NodeEncodings.stringFromBytes(bytes, enc))
      case None           => NodeBuffer.makeBuffer(bytes)
    }

  private def encodingOf(options: JSValue)(using ctx: JSContext): Option[String] =
    options match {
      case JSValue.JSStr(s) => Some(s)
      case JSValue.Object(_) =>
        BuiltinHelpers.getPropertyWithGetter(options, "encoding") match {
          case JSValue.JSStr(s) => Some(s)
          case _                => None
        }
      case _ => None
    }

  def create(state: NodeState, loop: HostEventLoop, streams: NodeStream)(using
      ctx: JSContext
  ): JSValue = {
    val childProcess = JSObject(prototype = ctx.objectPrototype)

    def strip(args: Array[JSValue]): Array[JSValue] =
      NodeHelpers.stripReceiver(args, childProcess)

    def method(name: String, arity: Int)(
        impl: (Array[JSValue], JSContext) => JSValue
    ): Unit =
      childProcess.set(
        name,
        JSValue.Native(
          NativeFunction(
            name = name,
            length = arity,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              impl(strip(args), callCtx)
            }
          )
        )
      )

    method("execSync", 2)((args, callCtx) => {
      given JSContext = callCtx
      val command = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val options = args.lift(1).getOrElse(JSValue.Undefined)
      val result = runCommand(command, Seq.empty, options, state)
      result.startError match {
        case Some(error) =>
          NodeHelpers.throwCoded(
            "Error",
            Option(error.getMessage).getOrElse("spawn failed"),
            "ENOENT"
          )
        case None => ()
      }
      if result.status != 0 then {
        val stderrText = new String(result.stderr, StandardCharsets.UTF_8)
        val error = ctx.createError(
          "Error",
          s"Command failed: $command\n$stderrText"
        )
        error match {
          case JSValue.Object(obj) =>
            obj.defineProperty("status", JSValue.fromInt(result.status), enumerable = false, writable = true, configurable = true)
            obj.defineProperty("stdout", outputValue(result.stdout, encodingOf(options)), enumerable = false, writable = true, configurable = true)
            obj.defineProperty("stderr", outputValue(result.stderr, encodingOf(options)), enumerable = false, writable = true, configurable = true)
            obj.defineProperty("signal", JSValue.Null, enumerable = false, writable = true, configurable = true)
          case _ => ()
        }
        throw new quickjs.runtime.JSException(error)
      }
      outputValue(result.stdout, encodingOf(options))
    })

    method("execFileSync", 3)((args, callCtx) => {
      given JSContext = callCtx
      val file = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val fileArgs = args.lift(1) match {
        case Some(JSValue.JSArrayVal(arr)) =>
          (0 until arr.getLength).map(i => NodeHelpers.toStr(arr.get(i)))
        case _ => Seq.empty[String]
      }
      val options = args.lift(2).getOrElse(JSValue.Undefined)
      val result = runCommandWithArgs(file, fileArgs, options)
      if result.status != 0 then {
        val error = ctx.createError(
          "Error",
          s"Command failed: $file\n${new String(result.stderr, StandardCharsets.UTF_8)}"
        )
        error match {
          case JSValue.Object(obj) =>
            obj.defineProperty("status", JSValue.fromInt(result.status), enumerable = false, writable = true, configurable = true)
          case _ => ()
        }
        throw new quickjs.runtime.JSException(error)
      }
      outputValue(result.stdout, encodingOf(options))
    })

    method("spawnSync", 3)((args, callCtx) => {
      given JSContext = callCtx
      val file = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val fileArgs = args.lift(1) match {
        case Some(JSValue.JSArrayVal(arr)) =>
          (0 until arr.getLength).map(i => NodeHelpers.toStr(arr.get(i)))
        case _ => Seq.empty[String]
      }
      val options = args.lift(2).getOrElse(JSValue.Undefined)
      val result = runCommandWithArgs(file, fileArgs, options)
      val encoding = encodingOf(options)
      val obj = JSObject(prototype = ctx.objectPrototype)
      obj.set("pid", JSValue.fromInt(-1))
      obj.set("status", JSValue.fromInt(if result.startError.isDefined then 1 else result.status))
      obj.set("signal", JSValue.Null)
      obj.set("stdout", outputValue(result.stdout, encoding))
      obj.set("stderr", outputValue(result.stderr, encoding))
      // Node's spawnSync result includes `output: [null, stdout, stderr]`.
      val outputArr = quickjs.objmodel.JSArray.empty()
      outputArr.push(JSValue.Null)
      outputArr.push(outputValue(result.stdout, encoding))
      outputArr.push(outputValue(result.stderr, encoding))
      obj.set("output", JSValue.JSArrayVal(outputArr))
      result.startError match {
        case Some(error) =>
          obj.set(
            "error",
            ctx.createError("Error", Option(error.getMessage).getOrElse("spawn failed"))
          )
        case None => obj.set("error", JSValue.Undefined)
      }
      JSValue.Object(obj)
    })

    def runCommandWithArgs(
        file: String,
        args: Seq[String],
        options: JSValue
    ): RunResult = {
      // spawn-style calls provide the executable directly, so bypass the shell.
      val builder = new ProcessBuilder((Seq(file) ++ args).asJava)
      options match {
        case JSValue.Object(_) =>
          BuiltinHelpers.getPropertyWithGetter(options, "cwd") match {
            case JSValue.JSStr(cwd) if cwd.nonEmpty =>
              val path = Paths.get(cwd)
              builder.directory(
                (if path.isAbsolute then path else state.cwd.resolve(path)).toFile
              )
            case _ => ()
          }
        case _ => ()
      }
      try {
        val process = builder.start()
        process.getOutputStream.close()
        var stdoutBytes: Array[Byte] = Array.emptyByteArray
        var stderrBytes: Array[Byte] = Array.emptyByteArray
        val outThread = new Thread(() => stdoutBytes = readAll(process.getInputStream))
        val errThread = new Thread(() => stderrBytes = readAll(process.getErrorStream))
        outThread.start()
        errThread.start()
        process.waitFor()
        outThread.join()
        errThread.join()
        RunResult(process.exitValue(), stdoutBytes, stderrBytes, timedOut = false, startError = None)
      } catch {
        case e: Throwable =>
          RunResult(1, Array.emptyByteArray, Array.emptyByteArray, timedOut = false, startError = Some(e))
      }
    }

    // =======================================================================
    // Event emitter plumbing (ChildProcess and its stdio streams)
    // =======================================================================

    final class Emitter {
      private val listeners =
        mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[JSValue]]

      def add(event: String, fn: JSValue): Unit =
        if BuiltinHelpers.isCallable(fn) then
          listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty) += fn

      def remove(event: String, fn: JSValue): Unit =
        listeners.get(event).foreach { list =>
          val index = list.indexWhere(l => NodeHelpers.sameValue(l, fn))
          if index >= 0 then list.remove(index)
        }

      def removeAll(event: String): Unit =
        if event.isEmpty then listeners.clear() else listeners.remove(event)

      def emit(event: String, eventArgs: Array[JSValue])(using
          ctx: JSContext
      ): Boolean = {
        val handlers =
          listeners.getOrElse(event, mutable.ArrayBuffer.empty).toSeq
        handlers.foreach(fn =>
          BuiltinHelpers.callFunctionWithThis(
            fn,
            JSValue.Undefined,
            eventArgs
          )
        )
        handlers.nonEmpty
      }

      def install(target: JSObject): Unit = {
        def strip(args: Array[JSValue]): Array[JSValue] =
          args.headOption match {
            case Some(JSValue.Object(obj)) if obj eq target => args.drop(1)
            case _                                          => args
          }
        def set(name: String, arity: Int)(
            body: (Array[JSValue], JSContext) => JSValue
        ): Unit =
          target.set(
            name,
            JSValue.Native(
              NativeFunction(
                name = name,
                length = arity,
                impl = (args, callCtx) => {
                  given JSContext = callCtx
                  body(strip(args), callCtx)
                }
              )
            )
          )
        def eventName(args: Array[JSValue]): String =
          args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
        set("on", 2) { (args, _) =>
          add(eventName(args), args.lift(1).getOrElse(JSValue.Undefined))
          JSValue.Object(target)
        }
        set("addListener", 2) { (args, _) =>
          add(eventName(args), args.lift(1).getOrElse(JSValue.Undefined))
          JSValue.Object(target)
        }
        set("once", 2) { (args, callCtx) =>
          given JSContext = callCtx
          val fn = args.lift(1).getOrElse(JSValue.Undefined)
          lazy val wrapper: NativeFunction = NativeFunction(
            name = eventName(args),
            length = 0,
            impl = (callArgs, innerCtx) => {
              given JSContext = innerCtx
              remove(eventName(args), JSValue.Native(wrapper))
              BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, callArgs)
            }
          )
          add(eventName(args), JSValue.Native(wrapper))
          JSValue.Object(target)
        }
        set("removeListener", 2) { (args, _) =>
          remove(eventName(args), args.lift(1).getOrElse(JSValue.Undefined))
          JSValue.Object(target)
        }
        set("off", 2) { (args, _) =>
          remove(eventName(args), args.lift(1).getOrElse(JSValue.Undefined))
          JSValue.Object(target)
        }
        set("removeAllListeners", 1) { (args, _) =>
          removeAll(eventName(args))
          JSValue.Object(target)
        }
        set("emit", 2) { (args, callCtx) =>
          given JSContext = callCtx
          JSValue.Bool(emit(eventName(args), args.drop(1)))
        }
        set("listenerCount", 2) { (args, _) =>
          JSValue.fromInt(
            listeners.get(eventName(args)).map(_.length).getOrElse(0)
          )
        }
        set("prependListener", 2) { (args, _) =>
          val event = eventName(args)
          val fn = args.lift(1).getOrElse(JSValue.Undefined)
          if BuiltinHelpers.isCallable(fn) then
            listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty)
              .prepend(fn)
          JSValue.Object(target)
        }
      }
    }

    def optionOf(options: JSValue, key: String): JSValue =
      options match {
        case JSValue.Object(_) =>
          BuiltinHelpers.getPropertyWithGetter(options, key)
        case _ => JSValue.Undefined
      }

    def configureBuilder(
        builder: java.lang.ProcessBuilder,
        options: JSValue
    ): Unit = {
      optionOf(options, "cwd") match {
        case JSValue.JSStr(cwd) if cwd.nonEmpty =>
          val path = Paths.get(cwd)
          builder.directory(
            (if path.isAbsolute then path else state.cwd.resolve(path)).toFile
          )
        case _ => builder.directory(state.cwd.toFile)
      }
      optionOf(options, "env") match {
        case JSValue.Object(env) =>
          val environment = builder.environment()
          environment.clear()
          env.getAllOwnPropertyKeys().foreach { key =>
            env.getOwnProperty(key).foreach { value =>
              environment.put(key, NodeHelpers.toStr(value))
            }
          }
        case _ => ()
      }
    }

    def bytesOfChunk(value: JSValue): Array[Byte] = value match {
      case JSValue.JSStr(s) => s.getBytes(StandardCharsets.UTF_8)
      case other =>
        NodeBuffer.bytesOfValue(other).getOrElse(Array.emptyByteArray)
    }

    /** Start a process and wire its stdio into stream objects. */
    def spawnAsync(
        file: String,
        fileArgs: Seq[String],
        options: JSValue,
        shell: Boolean,
        stdoutSink: Option[Array[Byte] => Unit],
        stderrSink: Option[Array[Byte] => Unit],
        onClose: Option[(Int, JSValue, Boolean) => Unit]
    ): JSObject = {
      val emitter = new Emitter
      val child = JSObject(prototype = ctx.objectPrototype)
      emitter.install(child)
      val stdout = streams.newReadable()
      val stderr = streams.newReadable()
      val stdin = streams.newWritable()
      child.set("stdout", stdout)
      child.set("stderr", stderr)
      child.set("stdin", stdin)
      child.set("pid", JSValue.fromInt(-1))
      child.set("killed", JSValue.Bool(false))
      child.set("exitCode", JSValue.Null)
      child.set("signalCode", JSValue.Null)
      child.set("spawnfile", JSValue.fromString(file))
      child.set("connected", JSValue.Bool(false))
      val spawnargs = quickjs.objmodel.JSArray.empty()
      spawnargs.push(JSValue.fromString(file))
      fileArgs.foreach(arg => spawnargs.push(JSValue.fromString(arg)))
      child.set("spawnargs", JSValue.JSArrayVal(spawnargs))

      var procRef: java.lang.Process = null
      var killed = false

      child.set(
        "kill",
        JSValue.Native(
          NativeFunction(
            name = "kill",
            length = 1,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              val signal = args.headOption
                .map(NodeHelpers.toStr(_))
                .filter(_.nonEmpty)
                .getOrElse("SIGTERM")
              val process = procRef
              if process == null || !process.isAlive then JSValue.Bool(false)
              else {
                killed = true
                child.set("killed", JSValue.Bool(true))
                if signal == "SIGKILL" then process.destroyForcibly()
                else process.destroy()
                JSValue.Bool(true)
              }
            }
          )
        )
      )
      child.set(
        "ref",
        JSValue.Native(
          NativeFunction(name = "ref", length = 0, impl = (_, _) => JSValue.Object(child))
        )
      )
      child.set(
        "unref",
        JSValue.Native(
          NativeFunction(name = "unref", length = 0, impl = (_, _) => JSValue.Object(child))
        )
      )
      child.set(
        "disconnect",
        JSValue.Native(
          NativeFunction(name = "disconnect", length = 0, impl = (_, _) => JSValue.Object(child))
        )
      )
      child.set(
        "send",
        JSValue.Native(
          NativeFunction(name = "send", length = 1, impl = (_, _) => JSValue.Bool(false))
        )
      )

      val command =
        if shell then Seq("/bin/sh", "-c", file) ++ fileArgs
        else Seq(file) ++ fileArgs
      val builder = new java.lang.ProcessBuilder(command.asJava)
      configureBuilder(builder, options)

      try {
        val process = builder.start()
        procRef = process
        child.set("pid", JSValue.fromInt(process.pid().toInt))

        // stdin writes go to the process pipe.
        stdin match {
          case JSValue.Object(stdinObj) =>
            def writeChunk(value: JSValue): Unit = {
              val bytes = bytesOfChunk(value)
              if bytes.nonEmpty then
                try process.getOutputStream.write(bytes)
                catch case _: Throwable => ()
            }
            def stripStdin(args: Array[JSValue]): Array[JSValue] =
              args.headOption match {
                case Some(JSValue.Object(obj)) if obj eq stdinObj => args.drop(1)
                case _                                            => args
              }
            stdinObj.set(
              "write",
              JSValue.Native(
                NativeFunction(
                  name = "write",
                  length = 3,
                  impl = (args, callCtx) => {
                    given JSContext = callCtx
                    stripStdin(args).headOption.foreach(writeChunk)
                    JSValue.Bool(true)
                  }
                )
              )
            )
            stdinObj.set(
              "end",
              JSValue.Native(
                NativeFunction(
                  name = "end",
                  length = 3,
                  impl = (args, callCtx) => {
                    given JSContext = callCtx
                    stripStdin(args).headOption.foreach {
                      case value if BuiltinHelpers.isCallable(value) => ()
                      case value                                     => writeChunk(value)
                    }
                    try process.getOutputStream.close()
                    catch case _: Throwable => ()
                    JSValue.Object(stdinObj)
                  }
                )
              )
            )
          case _ => ()
        }

        def reader(
            in: java.io.InputStream,
            target: JSValue,
            sink: Option[Array[Byte] => Unit]
        ): Thread = {
          val thread = new Thread(() => {
            val buffer = new Array[Byte](8192)
            try {
              var read = in.read(buffer)
              while read >= 0 do {
                if read > 0 then {
                  val chunk = java.util.Arrays.copyOf(buffer, read)
                  sink.foreach(_(chunk))
                  loop.post { () =>
                    streams.pushToStream(target, NodeBuffer.makeBuffer(chunk))
                  }
                }
                read = in.read(buffer)
              }
            } catch case _: Throwable => ()
            finally loop.post { () => streams.endStream(target) }
          })
          thread.setDaemon(true)
          thread
        }

        val outThread = reader(process.getInputStream, stdout, stdoutSink)
        val errThread = reader(process.getErrorStream, stderr, stderrSink)
        outThread.start()
        errThread.start()

        loop.post { () => emitter.emit("spawn", Array.empty); () }

        loop.execute {
          val status =
            try process.waitFor()
            catch case _: Throwable => -1
          outThread.join()
          errThread.join()
          loop.post { () =>
            child.set(
              "exitCode",
              if status >= 0 then JSValue.fromInt(status) else JSValue.Null
            )
            child.set("signalCode", JSValue.Null)
            val exitArgs: Array[JSValue] = Array(
              if status >= 0 then JSValue.fromInt(status) else JSValue.Null,
              JSValue.Null
            )
            emitter.emit("exit", exitArgs)
            emitter.emit("close", exitArgs)
            onClose.foreach(cb => cb(status, JSValue.Null, killed))
          }
        }
      } catch {
        case e: Throwable =>
          loop.post { () =>
            val error = ctx.createError("Error", s"spawn $file ENOENT")
            error match {
              case JSValue.Object(obj) =>
                obj.set("code", JSValue.fromString("ENOENT"))
                obj.set("errno", JSValue.fromInt(-2))
                obj.set("syscall", JSValue.fromString("spawn " + file))
                obj.set("path", JSValue.fromString(file))
              case _ => ()
            }
            child.set("pid", JSValue.fromInt(-1))
            emitter.emit("error", Array(error))
            val exitArgs: Array[JSValue] = Array(JSValue.Null, JSValue.Null)
            emitter.emit("close", exitArgs)
            onClose.foreach(cb => cb(-2, error, killed))
          }
      }
      child
    }

    def argStrings(value: JSValue): Seq[String] =
      value match {
        case JSValue.JSArrayVal(arr) =>
          (0 until arr.getLength).map(i => NodeHelpers.toStr(arr.get(i)))
        case _ => Seq.empty[String]
      }

    // -- spawn ---------------------------------------------------------------

    method("spawn", 3)((args, callCtx) => {
      given JSContext = callCtx
      val file = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val fileArgs = argStrings(args.lift(1).getOrElse(JSValue.Undefined))
      val options = args.lift(2).getOrElse(JSValue.Undefined)
      val shell = optionOf(options, "shell") match {
        case JSValue.JSStr(_) => true
        case JSValue.Bool(true) => true
        case _                  => false
      }
      JSValue.Object(
        spawnAsync(file, fileArgs, options, shell, None, None, None)
      )
    })

    /** Error-first callback runner shared by `exec`/`execFile`. */
    def callbackArgs(args: Array[JSValue]): (JSValue, Option[JSValue]) =
      args.lift(1) match {
        case Some(value) if BuiltinHelpers.isCallable(value) =>
          (JSValue.Undefined, Some(value))
        case _ =>
          (args.lift(1).getOrElse(JSValue.Undefined),
            args.lift(2).filter(BuiltinHelpers.isCallable))
      }

    def runWithCallback(
        file: String,
        fileArgs: Seq[String],
        options: JSValue,
        shell: Boolean,
        callback: Option[JSValue],
        describe: String
    ): JSObject = {
      val stdoutBuffer = new java.io.ByteArrayOutputStream()
      val stderrBuffer = new java.io.ByteArrayOutputStream()
      val encoding = encodingOf(options).orElse(Some("utf8"))
      var timeoutId = -1
      val child = spawnAsync(
        file,
        fileArgs,
        options,
        shell,
        stdoutSink = Some(bytes => stdoutBuffer.write(bytes)),
        stderrSink = Some(bytes => stderrBuffer.write(bytes)),
        onClose = Some { (status, signalValue, wasKilled) =>
          if timeoutId >= 0 then loop.clear(timeoutId)
          callback.foreach { fn =>
            val stdoutValue = outputValue(stdoutBuffer.toByteArray, encoding)
            val stderrValue = outputValue(stderrBuffer.toByteArray, encoding)
            val error: JSValue =
              if status == 0 && !wasKilled then JSValue.Null
              else {
                val message = if status == 0 then describe else s"Command failed: $describe"
                val err = ctx.createError("Error", message)
                err match {
                  case JSValue.Object(obj) =>
                    obj.set("code", JSValue.fromInt(if status < 0 then -1 else status))
                    obj.set("status", JSValue.fromInt(if status < 0 then -1 else status))
                    obj.set("signal", signalValue)
                    obj.set("killed", JSValue.Bool(wasKilled))
                    obj.set("cmd", JSValue.fromString(describe))
                    obj.set("stdout", stdoutValue)
                    obj.set("stderr", stderrValue)
                  case _ => ()
                }
                err
              }
            BuiltinHelpers.callFunctionWithThis(
              fn,
              JSValue.Undefined,
              Array(error, stdoutValue, stderrValue)
            )
          }
          ()
        }
      )
      optionOf(options, "timeout") match {
        case JSValue.Int32(ms) if ms > 0 =>
          val killFn =
            BuiltinHelpers.getPropertyWithGetter(JSValue.Object(child), "kill")
          val timeoutCb = NativeFunction(
            name = "timeout",
            length = 0,
            impl = (_, callCtx) => {
              given JSContext = callCtx
              BuiltinHelpers.callFunctionWithThis(
                killFn,
                JSValue.Object(child),
                Array(JSValue.fromString("SIGTERM"))
              )
            }
          )
          timeoutId =
            loop.schedule(JSValue.Native(timeoutCb), ms.toDouble, Array.empty, interval = false)
        case _ => ()
      }
      child
    }

    // -- exec / execFile -----------------------------------------------------

    method("exec", 3)((args, callCtx) => {
      given JSContext = callCtx
      val command = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val (options, callback) = callbackArgs(args)
      JSValue.Object(
        runWithCallback(
          command,
          Seq.empty,
          options,
          shell = true,
          callback,
          describe = command
        )
      )
    })

    method("execFile", 4)((args, callCtx) => {
      given JSContext = callCtx
      val file = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val fileArgs = argStrings(args.lift(1).getOrElse(JSValue.Undefined))
      val (options, callback) = {
        val second = args.lift(1)
        if second.exists(BuiltinHelpers.isCallable) then
          (JSValue.Undefined, second.filter(BuiltinHelpers.isCallable))
        else {
          val tail = args.drop(2)
          tail.headOption match {
            case Some(value) if BuiltinHelpers.isCallable(value) => (args.lift(1).getOrElse(JSValue.Undefined), Some(value))
            case _ => callbackArgs(args.drop(1))
          }
        }
      }
      JSValue.Object(
        runWithCallback(
          file,
          fileArgs,
          options,
          shell = false,
          callback,
          describe = (Seq(file) ++ fileArgs).mkString(" ")
        )
      )
    })

    JSValue.Object(childProcess)
  }
}
