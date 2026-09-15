package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.JSObject

import java.io.ByteArrayOutputStream
import java.util.zip.{
  Deflater,
  DeflaterOutputStream,
  GZIPInputStream,
  GZIPOutputStream,
  Inflater,
  InflaterInputStream
}

/** Node's `zlib` module (synchronous deflate/gzip plus loop-backed async and
  * `zlib.promises` variants).
  */
object NodeZlib {

  private def deflateBytes(
      input: Array[Byte],
      raw: Boolean,
      gzip: Boolean
  ): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    if gzip then {
      val stream = new GZIPOutputStream(out)
      stream.write(input)
      stream.close()
    } else {
      val deflater = new Deflater(Deflater.DEFAULT_COMPRESSION, raw)
      val stream = new DeflaterOutputStream(out, deflater)
      stream.write(input)
      stream.finish()
      stream.close()
      deflater.end()
    }
    out.toByteArray
  }

  private def inflateBytes(
      input: Array[Byte],
      raw: Boolean,
      gzip: Boolean
  ): Array[Byte] = {
    val stream =
      if gzip then new GZIPInputStream(new java.io.ByteArrayInputStream(input))
      else
        new InflaterInputStream(
          new java.io.ByteArrayInputStream(input),
          new Inflater(raw)
        )
    try {
      val out = new ByteArrayOutputStream()
      val buffer = new Array[Byte](16384)
      var read = stream.read(buffer)
      while read >= 0 do {
        if read > 0 then out.write(buffer, 0, read)
        read = stream.read(buffer)
      }
      out.toByteArray
    } finally stream.close()
  }

  private def gunzipAny(input: Array[Byte]): Array[Byte] =
    try inflateBytes(input, raw = false, gzip = true)
    catch {
      case _: Throwable =>
        try inflateBytes(input, raw = false, gzip = false)
        catch case _: Throwable => inflateBytes(input, raw = true, gzip = false)
    }

  private def inputBytes(value: JSValue)(using ctx: JSContext): Array[Byte] =
    value match {
      case JSValue.JSStr(s) => NodeEncodings.bytesFromString(s, "utf8")
      case other =>
        NodeBuffer.bytesOfValue(other).getOrElse(
          NodeEncodings.bytesFromString(NodeHelpers.toStr(other), "utf8")
        )
    }

  def create(loop: HostEventLoop)(using ctx: JSContext): JSValue = {
    val zlib = JSObject(prototype = ctx.objectPrototype)
    val promises = JSObject(prototype = ctx.objectPrototype)

    def strip(args: Array[JSValue]): Array[JSValue] =
      NodeHelpers.stripReceiver(args, zlib)

    /** Synchronous transform callable from Scala (runs on the host pool). */
    def syncTransform(
        transform: Array[Byte] => Array[Byte]
    )(args: Array[JSValue])(using ctx: JSContext): JSValue = {
      val bytes = inputBytes(args.headOption.getOrElse(JSValue.Undefined))
      NodeBuffer.makeBuffer(transform(bytes))
    }

    def installTransform(
        name: String,
        transform: Array[Byte] => Array[Byte]
    ): Unit = {
      val syncName = s"${name}Sync"
      zlib.set(
        syncName,
        JSValue.Native(
          NativeFunction(
            name = syncName,
            length = 2,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              syncTransform(transform)(strip(args))
            }
          )
        )
      )

      val asyncFn = NativeFunction(
        name = name,
        length = 3,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val real = strip(args)
          val hasCallback = real.nonEmpty && BuiltinHelpers.isCallable(real.last)
          val callback = if hasCallback then real.last else JSValue.Undefined
          val opArgs = if hasCallback then real.dropRight(1) else real
          loop.execute {
            val result =
              try Right(syncTransform(transform)(opArgs)(using callCtx))
              catch
                case e: quickjs.runtime.JSException => Left(e.getValue)
                case e: Throwable =>
                  Left(callCtx.createError("Error", String.valueOf(e.getMessage)))
            loop.post { () =>
              result match {
                case Right(value) =>
                  BuiltinHelpers.callFunctionWithThis(
                    callback,
                    JSValue.Undefined,
                    Array(JSValue.Null, value)
                  )
                case Left(error) =>
                  BuiltinHelpers.callFunctionWithThis(
                    callback,
                    JSValue.Undefined,
                    Array(error)
                  )
              }
              ()
            }
          }
          JSValue.Undefined
        }
      )
      zlib.set(name, JSValue.Native(asyncFn))

      val promiseFn = NativeFunction(
        name = name,
        length = 2,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val stripped = strip(args)
          val real =
            if stripped.nonEmpty then
              stripped(0) match {
                case JSValue.Object(obj) if obj eq promises => stripped.drop(1)
                case _                                      => stripped
              }
            else stripped
          val promise = JSValue.Promise()
          loop.execute {
            val result =
              try Right(syncTransform(transform)(real)(using callCtx))
              catch
                case e: quickjs.runtime.JSException => Left(e.getValue)
                case e: Throwable =>
                  Left(callCtx.createError("Error", String.valueOf(e.getMessage)))
            loop.post { () =>
              result match {
                case Right(value) =>
                  quickjs.runtime.builtins.PromiseBuiltins
                    .settlePromise(promise, value)
                case Left(error) =>
                  quickjs.runtime.builtins.PromiseBuiltins
                    .rejectPromiseValue(promise, error)
              }
              ()
            }
          }
          BuiltinHelpers.wrapPromise(promise)
        }
      )
      promises.set(name, JSValue.Native(promiseFn))
    }

    installTransform("gzip", bytes => deflateBytes(bytes, raw = false, gzip = true))
    installTransform("gunzip", bytes => inflateBytes(bytes, raw = false, gzip = true))
    installTransform("deflate", bytes => deflateBytes(bytes, raw = false, gzip = false))
    installTransform("inflate", bytes => inflateBytes(bytes, raw = false, gzip = false))
    installTransform("deflateRaw", bytes => deflateBytes(bytes, raw = true, gzip = false))
    installTransform("inflateRaw", bytes => inflateBytes(bytes, raw = true, gzip = false))
    installTransform("unzip", gunzipAny)

    zlib.set("promises", JSValue.Object(promises))
    zlib.set(
      "constants",
      JSValue.Object(
        NodeHelpers.objectOf(
          "Z_NO_FLUSH" -> JSValue.fromInt(0),
          "Z_PARTIAL_FLUSH" -> JSValue.fromInt(1),
          "Z_SYNC_FLUSH" -> JSValue.fromInt(2),
          "Z_FULL_FLUSH" -> JSValue.fromInt(3),
          "Z_FINISH" -> JSValue.fromInt(4),
          "Z_OK" -> JSValue.fromInt(0),
          "Z_STREAM_END" -> JSValue.fromInt(1),
          "Z_DEFAULT_COMPRESSION" -> JSValue.fromInt(-1),
          "Z_DEFAULT_STRATEGY" -> JSValue.fromInt(0),
          "Z_BEST_SPEED" -> JSValue.fromInt(1),
          "Z_BEST_COMPRESSION" -> JSValue.fromInt(9)
        )
      )
    )
    JSValue.Object(zlib)
  }
}
