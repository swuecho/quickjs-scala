package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.JSObject

import java.nio.charset.StandardCharsets
import java.nio.file.Paths
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
      case Some(enc) => JSValue.fromString(NodeEncodings.stringFromBytes(bytes, enc))
      case None      => NodeBuffer.makeBuffer(bytes)
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

  def create(state: NodeState)(using ctx: JSContext): JSValue = {
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

    JSValue.Object(childProcess)
  }
}
