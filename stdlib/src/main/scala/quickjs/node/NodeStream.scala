package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.{BuiltinHelpers, PromiseBuiltins}
import quickjs.objmodel.{JSArray, JSObject}

import java.io.{FileOutputStream, RandomAccessFile}
import java.nio.file.{Files, Paths}
import scala.collection.mutable

/** A pragmatic subset of Node's `stream` module: EventEmitter-based
  * `Readable`/`Writable`/`Duplex`/`Transform`/`PassThrough` with `pipe`,
  * `pipeline`, `finished`, plus `fs.createReadStream`/`createWriteStream` and
  * zlib transform help.
  */
final class NodeStream(loop: HostEventLoop)(using ctx: JSContext) {

  // =========================================================================
  // Emitter plumbing
  // =========================================================================

  private final class EmitterData {
    val listeners = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[JSValue]]
  }

  private def emitterOf(value: JSValue): Option[EmitterData] =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__emitter") match {
          case Some(JSValue.Native(data: EmitterData)) => Some(data)
          case _                                       => None
        }
      case _ => None
    }

  private def emit(target: JSValue, event: String, eventArgs: Array[JSValue]): Boolean =
    emitterOf(target) match {
      case Some(data) =>
        val listeners =
          data.listeners.get(event).map(_.toArray).getOrElse(Array.empty[JSValue])
        if listeners.isEmpty then {
          if event == "error" then {
            val reason = eventArgs.headOption.getOrElse(
              errorWithName("Error", "Unhandled 'error' event")
            )
            throw new quickjs.runtime.JSException(reason)
          }
          false
        } else {
          listeners.foreach(listener =>
            BuiltinHelpers.callFunctionWithThis(listener, target, eventArgs)
          )
          true
        }
      case None => false
    }

  private def errorWithName(name: String, message: String): JSValue = {
    val error = ctx.createError("Error", message)
    error match {
      case JSValue.Object(obj) =>
        obj.defineProperty("name", JSValue.fromString(name), enumerable = false, writable = true, configurable = true)
      case _ => ()
    }
    error
  }

  private def attachEmitter(obj: JSObject): JSObject = {
    if obj.getOwnPropertyRaw("__emitter").isEmpty then
      obj.initProperty("__emitter", JSValue.Native(new EmitterData), enumerable = false, writable = false, configurable = false)
    obj
  }

  // =========================================================================
  // Stream state
  // =========================================================================

  private final class StreamState {
    val buffer = mutable.ArrayBuffer.empty[Array[Byte]]
    var flowing = false
    var readableEnded = false
    var endedPushed = false
    var writableFinished = false
    var encoding: Option[String] = None
    var output: Option[FileOutputStream] = None
    var outputPath: Option[String] = None
  }

  private def stateOf(value: JSValue): Option[StreamState] =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__streamState") match {
          case Some(JSValue.Native(state: StreamState)) => Some(state)
          case _                                        => None
        }
      case _ => None
    }

  private def bytesOf(value: JSValue): Array[Byte] =
    value match {
      case JSValue.JSStr(s) => NodeEncodings.bytesFromString(s, stateEncodingHint(s))
      case other =>
        NodeBuffer.bytesOfValue(other).getOrElse(
          NodeEncodings.bytesFromString(NodeHelpers.toStr(other), "utf8")
        )
    }

  private def stateEncodingHint(_unused: String): String = "utf8"

  private def chunkValue(state: StreamState, bytes: Array[Byte]): JSValue =
    state.encoding match {
      case Some(enc) =>
        JSValue.fromString(NodeEncodings.stringFromBytes(bytes, enc))
      case None => NodeBuffer.makeBuffer(bytes)
    }

  private def flushReadable(target: JSValue, state: StreamState): Unit = {
    val pending = state.buffer.toList
    state.buffer.clear()
    pending.foreach(bytes => emit(target, "data", Array(chunkValue(state, bytes))))
    if state.readableEnded && !state.endedPushed then {
      state.endedPushed = true
      emit(target, "end", Array.empty)
    }
  }

  private def push(target: JSValue, state: StreamState, chunk: JSValue): Boolean =
    chunk match {
      case JSValue.Undefined | JSValue.Null =>
        state.readableEnded = true
        if state.flowing then {
          if !state.endedPushed then {
            state.endedPushed = true
            emit(target, "end", Array.empty)
          }
        }
        false
      case _ =>
        val bytes = bytesOf(chunk)
        if state.flowing then emit(target, "data", Array(chunkValue(state, bytes)))
        else state.buffer += bytes
        true
    }

  // =========================================================================
  // Class installation
  // =========================================================================

  private def method(
      proto: JSObject,
      name: String,
      length: Int
  )(impl: (Array[JSValue], JSContext) => JSValue): Unit =
    proto.defineProperty(
      name,
      JSValue.Native(NativeFunction(name, impl, length = length)),
      enumerable = false,
      writable = true,
      configurable = true
    )

  private def thisAndRest(args: Array[JSValue]): (JSValue, Array[JSValue]) =
    if args.isEmpty then (JSValue.Undefined, Array.empty)
    else (args(0), args.drop(1))

  private def makeEmitterProto(): JSObject = {
    val proto = JSObject(prototype = ctx.objectPrototype)
    method(proto, "on", 2) { (args, c) =>
      given JSContext = c
      val (thisValue, rest) = thisAndRest(args)
      val event = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val listener = rest.lift(1).getOrElse(JSValue.Undefined)
      if !BuiltinHelpers.isCallable(listener) then
        NodeHelpers.throwCoded("TypeError", "listener must be a function", "ERR_INVALID_ARG_TYPE")
      emitterOf(thisValue).foreach { data =>
        data.listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty) += listener
      }
      if event == "data" then {
        stateOf(thisValue).foreach { state =>
          state.flowing = true
          flushReadable(thisValue, state)
        }
      }
      thisValue
    }
    method(proto, "addListener", 2) { (args, c) =>
      given JSContext = c
      val (thisValue, rest) = thisAndRest(args)
      val event = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val listener = rest.lift(1).getOrElse(JSValue.Undefined)
      emitterOf(thisValue).foreach { data =>
        data.listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty) += listener
      }
      if event == "data" then
        stateOf(thisValue).foreach { state =>
          state.flowing = true
          flushReadable(thisValue, state)
        }
      thisValue
    }
    method(proto, "once", 2) { (args, c) =>
      given JSContext = c
      val (thisValue, rest) = thisAndRest(args)
      val event = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val listener = rest.lift(1).getOrElse(JSValue.Undefined)
      var stored: JSValue = JSValue.Undefined
      val wrapper = NativeFunction(
        name = "onceWrapper",
        length = 0,
        impl = (callArgs, callCtx) => {
          emitterOf(thisValue).foreach { data =>
            data.listeners.get(event).foreach { buffer =>
              val index = buffer.indexWhere(l => NodeHelpers.sameValue(l, stored))
              if index >= 0 then buffer.remove(index)
            }
          }
          BuiltinHelpers.callFunctionWithThis(
            listener,
            callArgs.headOption.getOrElse(JSValue.Undefined),
            callArgs.drop(1)
          )(using callCtx)
        }
      )
      stored = JSValue.Native(wrapper)
      emitterOf(thisValue).foreach { data =>
        data.listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty) += stored
      }
      if event == "data" then
        stateOf(thisValue).foreach { state =>
          state.flowing = true
          flushReadable(thisValue, state)
        }
      thisValue
    }
    method(proto, "off", 2) { (args, c) =>
      val (thisValue, rest) = thisAndRest(args)
      val event = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val listener = rest.lift(1).getOrElse(JSValue.Undefined)
      emitterOf(thisValue).foreach { data =>
        data.listeners.get(event).foreach { buffer =>
          val index = buffer.indexWhere(l => NodeHelpers.sameValue(l, listener))
          if index >= 0 then buffer.remove(index)
        }
      }
      thisValue
    }
    method(proto, "removeListener", 2) { (args, c) =>
      val (thisValue, rest) = thisAndRest(args)
      val event = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val listener = rest.lift(1).getOrElse(JSValue.Undefined)
      emitterOf(thisValue).foreach { data =>
        data.listeners.get(event).foreach { buffer =>
          val index = buffer.indexWhere(l => NodeHelpers.sameValue(l, listener))
          if index >= 0 then buffer.remove(index)
        }
      }
      thisValue
    }
    method(proto, "removeAllListeners", 1) { (args, c) =>
      val (thisValue, rest) = thisAndRest(args)
      rest.headOption match {
        case Some(JSValue.Undefined) | None => emitterOf(thisValue).foreach(_.listeners.clear())
        case Some(event) => emitterOf(thisValue).foreach(_.listeners.remove(NodeHelpers.toStr(event)))
      }
      thisValue
    }
    method(proto, "emit", 1) { (args, c) =>
      given JSContext = c
      val (thisValue, rest) = thisAndRest(args)
      val event = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      emit(thisValue, event, rest.drop(1))
      JSValue.Bool(true)
    }
    method(proto, "listenerCount", 1) { (args, _) =>
      val (thisValue, rest) = thisAndRest(args)
      val event = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      JSValue.fromInt(
        emitterOf(thisValue).flatMap(_.listeners.get(event)).map(_.length).getOrElse(0)
      )
    }
    proto
  }

  private var readableProto: JSObject = null
  private var writableProto: JSObject = null
  private var duplexProto: JSObject = null
  private var transformProto: JSObject = null
  private var passThroughProto: JSObject = null

  private def installReadable(proto: JSObject): Unit = {
    method(proto, "push", 1) { (args, c) =>
      val (thisValue, rest) = thisAndRest(args)
      stateOf(thisValue) match {
        case Some(state) =>
          JSValue.Bool(push(thisValue, state, rest.headOption.getOrElse(JSValue.Undefined)))
        case None => JSValue.Bool(false)
      }
    }
    method(proto, "unshift", 1) { (args, c) =>
      val (thisValue, rest) = thisAndRest(args)
      stateOf(thisValue).foreach { state =>
        rest.headOption.foreach { chunk =>
          if chunk != JSValue.Undefined && chunk != JSValue.Null then
            state.buffer.prepend(bytesOf(chunk))
        }
      }
      JSValue.Undefined
    }
    method(proto, "read", 1) { (args, c) =>
      val (thisValue, rest) = thisAndRest(args)
      stateOf(thisValue) match {
        case Some(state) if state.buffer.nonEmpty =>
          val count = rest.headOption.map(v => NodeHelpers.toNumber(v).toInt).getOrElse(-1)
          val take = if count < 0 then state.buffer.length else math.min(count, state.buffer.length)
          val parts = state.buffer.take(take)
          state.buffer.remove(0, take)
          val joined = parts.foldLeft(Array.emptyByteArray)(_ ++ _)
          chunkValue(state, joined)
        case Some(_) if stateOf(thisValue).exists(_.readableEnded) => JSValue.Null
        case _                                                     => JSValue.Null
      }
    }
    method(proto, "resume", 0) { (args, _) =>
      val (thisValue, _) = thisAndRest(args)
      stateOf(thisValue).foreach { state =>
        state.flowing = true
        flushReadable(thisValue, state)
      }
      thisValue
    }
    method(proto, "pause", 0) { (args, _) =>
      val (thisValue, _) = thisAndRest(args)
      stateOf(thisValue).foreach(_.flowing = false)
      thisValue
    }
    method(proto, "isPaused", 0) { (args, _) =>
      val (thisValue, _) = thisAndRest(args)
      JSValue.Bool(!stateOf(thisValue).exists(_.flowing))
    }
    method(proto, "setEncoding", 1) { (args, c) =>
      val (thisValue, rest) = thisAndRest(args)
      stateOf(thisValue).foreach { state =>
        state.encoding = rest.headOption.map(NodeHelpers.toStr(_))
      }
      thisValue
    }
    method(proto, "pipe", 2) { (args, c) =>
      given JSContext = c
      val (thisValue, rest) = thisAndRest(args)
      val dest = rest.headOption.getOrElse(JSValue.Undefined)
      val destWrite = BuiltinHelpers.getPropertyWithGetter(dest, "write")
      val destEnd = BuiltinHelpers.getPropertyWithGetter(dest, "end")
      val onData = NativeFunction(
        name = "onData",
        length = 1,
        impl = (callArgs, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.callFunctionWithThis(
            destWrite,
            dest,
            Array(callArgs.lastOption.getOrElse(JSValue.Undefined))
          )
          JSValue.Undefined
        }
      )
      val onEnd = NativeFunction(
        name = "onEnd",
        length = 0,
        impl = (_, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.callFunctionWithThis(destEnd, dest, Array.empty)
          JSValue.Undefined
        }
      )
      emitterOf(thisValue).foreach { data =>
        data.listeners.getOrElseUpdate("data", mutable.ArrayBuffer.empty) += JSValue.Native(onData)
        data.listeners.getOrElseUpdate("end", mutable.ArrayBuffer.empty) += JSValue.Native(onEnd)
      }
      stateOf(thisValue).foreach { state =>
        state.flowing = true
        flushReadable(thisValue, state)
      }
      dest
    }
    method(proto, "destroy", 1) { (args, c) =>
      given JSContext = c
      val (thisValue, rest) = thisAndRest(args)
      rest.headOption.filter(_ != JSValue.Undefined).foreach { err =>
        emit(thisValue, "error", Array(err))
      }
      emit(thisValue, "close", Array.empty)
      thisValue
    }
  }

  private def installWritable(proto: JSObject): Unit = {
    method(proto, "write", 3) { (args, c) =>
      given JSContext = c
      val (thisValue, rest) = thisAndRest(args)
      val chunk = rest.headOption.getOrElse(JSValue.Undefined)
      val callback = rest.reverse.find(BuiltinHelpers.isCallable)
      val writeFn = BuiltinHelpers.getPropertyWithGetter(thisValue, "_write")
      if BuiltinHelpers.isCallable(writeFn) then {
        val cb = NativeFunction(
          name = "onwrite",
          length = 1,
          impl = (callArgs, callCtx) => {
            callback.foreach(fn =>
              BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, Array.empty)(using callCtx)
            )
            JSValue.Undefined
          }
        )
        BuiltinHelpers.callFunctionWithThis(
          writeFn,
          thisValue,
          Array(chunk, JSValue.fromString("buffer"), JSValue.Native(cb))
        )
      } else {
        stateOf(thisValue).foreach { state =>
          state.output match {
            case Some(out) =>
              try out.write(bytesOf(chunk))
              catch case e: Throwable => emit(thisValue, "error", Array(errorWithName("Error", String.valueOf(e.getMessage))))
            case None => ()
          }
        }
        callback.foreach(fn =>
          ctx.queueMicrotask(() =>
            BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, Array.empty)
          )
        )
        ctx.queueMicrotask(() => emit(thisValue, "drain", Array.empty))
      }
      JSValue.Bool(true)
    }
    method(proto, "end", 2) { (args, c) =>
      given JSContext = c
      val (thisValue, rest) = thisAndRest(args)
      val chunk = rest.headOption.getOrElse(JSValue.Undefined)
      val callback = rest.reverse.find(BuiltinHelpers.isCallable)
      if chunk != JSValue.Undefined && chunk != JSValue.Null then {
        val writeFn = BuiltinHelpers.getPropertyWithGetter(thisValue, "write")
        BuiltinHelpers.callFunctionWithThis(writeFn, thisValue, Array(chunk))
      }
      stateOf(thisValue).foreach { state =>
        if !state.writableFinished then {
          state.writableFinished = true
          state.output.foreach { out =>
            try out.close()
            catch case _: Throwable => ()
          }
        }
      }
      val flushFn = BuiltinHelpers.getPropertyWithGetter(thisValue, "_flush")
      if BuiltinHelpers.isCallable(flushFn) then {
        val cb = NativeFunction(
          name = "onflush",
          length = 0,
          impl = (_, callCtx) => {
            given JSContext = callCtx
            emit(thisValue, "finish", Array.empty)
            callback.foreach(fn =>
              BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, Array.empty)
            )
            JSValue.Undefined
          }
        )
        BuiltinHelpers.callFunctionWithThis(flushFn, thisValue, Array(JSValue.Native(cb)))
      } else {
        ctx.queueMicrotask(() => emit(thisValue, "finish", Array.empty))
        callback.foreach(fn =>
          ctx.queueMicrotask(() =>
            BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, Array.empty)
          )
        )
      }
      thisValue
    }
    method(proto, "cork", 0) { (args, _) => args.headOption.getOrElse(JSValue.Undefined) }
    method(proto, "uncork", 0) { (args, _) => args.headOption.getOrElse(JSValue.Undefined) }
    method(proto, "setDefaultEncoding", 1) { (args, _) =>
      args.headOption.getOrElse(JSValue.Undefined)
    }
    method(proto, "endWritable", 0) { (args, _) =>
      args.headOption.getOrElse(JSValue.Undefined)
    }
    method(proto, "destroy", 1) { (args, c) =>
      val (thisValue, _) = thisAndRest(args)
      stateOf(thisValue).foreach(_.output.foreach(out => try out.close() catch case _: Throwable => ()))
      thisValue
    }
  }

  private def makeStreamObject(
      proto: JSObject,
      withReadable: Boolean,
      withWritable: Boolean
  ): JSValue = {
    val obj = JSObject(prototype = proto)
    attachEmitter(obj)
    val state = new StreamState
    obj.initProperty("__streamState", JSValue.Native(state), enumerable = false, writable = false, configurable = false)
    if withReadable then {
      obj.set("readable", JSValue.Bool(true))
      obj.set("readableEnded", JSValue.Bool(false))
    }
    if withWritable then {
      obj.set("writable", JSValue.Bool(true))
      obj.set("writableEnded", JSValue.Bool(false))
    }
    obj.set("destroyed", JSValue.Bool(false))
    JSValue.Object(obj)
  }

  private def installClass(
      ctorName: String,
      proto: JSObject,
      withReadable: Boolean,
      withWritable: Boolean
  )(afterInit: (JSObject, JSValue) => Unit): NativeConstructor = {
    def construct(args: Array[JSValue], callCtx: JSContext): JSValue = {
      given JSContext = callCtx
      val value = makeStreamObject(proto, withReadable, withWritable)
      value match {
        case JSValue.Object(obj) =>
          afterInit(obj, value)
          // Invoke the user's constructor body when subclassing: the
          // interpreter's derived-constructor path initializes the receiver
          // via superInitImpl and then runs the subclass body; here we just
          // return the initialized object.
          value
        case _ => value
      }
    }
    val ctor = NativeConstructor(
      name = ctorName,
      callImpl = (args, callCtx) => {
        given JSContext = callCtx
        NodeHelpers.throwCoded(
          "TypeError",
          s"Class constructor $ctorName cannot be invoked without 'new'",
          "ERR_CONSTRUCT_CALL_REQUIRED"
        )
      },
      constructImpl = (args, callCtx) => construct(args, callCtx),
      prototype = proto,
      superInitImpl = Some((thisValue, _, initCtx) => {
        given JSContext = initCtx
        thisValue match {
          case JSValue.Object(obj) =>
            attachEmitter(obj)
            if obj.getOwnPropertyRaw("__streamState").isEmpty then
              obj.initProperty("__streamState", JSValue.Native(new StreamState), enumerable = false, writable = false, configurable = false)
            thisValue
          case _ => thisValue
        }
      })
    )
    BuiltinHelpers.initConstructor(ctor, length = 1)
    ctor
  }

  private def makeProtos(): Unit = {
    val emitterProto = makeEmitterProto()
    readableProto = JSObject(prototype = emitterProto)
    installReadable(readableProto)
    writableProto = JSObject(prototype = emitterProto)
    installWritable(writableProto)
    duplexProto = JSObject(prototype = readableProto)
    installWritable(duplexProto)
    transformProto = JSObject(prototype = duplexProto)
    installReadable(transformProto)
    installWritable(transformProto)
    // Transform's write feeds _transform.
    transformProto.defineProperty(
      "write",
      JSValue.Native(
        NativeFunction(
          name = "write",
          length = 3,
          impl = (args, c) => {
            given JSContext = c
            val (thisValue, rest) = thisAndRest(args)
            val chunk = rest.headOption.getOrElse(JSValue.Undefined)
            val callback = rest.reverse.find(BuiltinHelpers.isCallable)
            val transformFn = BuiltinHelpers.getPropertyWithGetter(thisValue, "_transform")
            val pushFn = BuiltinHelpers.getPropertyWithGetter(thisValue, "push")
            val cb = NativeFunction(
              name = "ontransform",
              length = 0,
              impl = (_, _) => {
                callback.foreach(fn =>
                  BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, Array.empty)
                )
                JSValue.Undefined
              }
            )
            if BuiltinHelpers.isCallable(transformFn) then
              BuiltinHelpers.callFunctionWithThis(
                transformFn,
                thisValue,
                Array(chunk, JSValue.fromString("buffer"), JSValue.Native(cb))
              )
            else {
              BuiltinHelpers.callFunctionWithThis(pushFn, thisValue, Array(chunk))
              BuiltinHelpers.callFunctionWithThis(JSValue.Native(cb), JSValue.Undefined, Array.empty[JSValue])
            }
            JSValue.Bool(true)
          }
        )
      ),
      enumerable = false,
      writable = true,
      configurable = true
    )
    passThroughProto = JSObject(prototype = transformProto)
  }

  // =========================================================================
  // Module factory
  // =========================================================================

  def create(): JSValue = {
    makeProtos()

    val streamCtor = NativeConstructor(
      name = "Stream",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        NodeHelpers.throwCoded("TypeError", "Class constructor Stream cannot be invoked without 'new'", "ERR_CONSTRUCT_CALL_REQUIRED")
      },
      constructImpl = (_, callCtx) => {
        given JSContext = callCtx
        makeStreamObject(readableProto, withReadable = false, withWritable = false)
      },
      prototype = readableProto
    )
    BuiltinHelpers.initConstructor(streamCtor, length = 0)

    val readableCtor = installClass("Readable", readableProto, true, false)((_, _) => ())
    val writableCtor = installClass("Writable", writableProto, false, true)((_, _) => ())
    val duplexCtor = installClass("Duplex", duplexProto, true, true)((_, _) => ())
    val transformCtor = installClass("Transform", transformProto, true, true)((_, _) => ())
    val passThroughCtor = installClass("PassThrough", passThroughProto, true, true)((_, _) => ())

    val stream = JSObject(prototype = ctx.objectPrototype)
    stream.set("Stream", JSValue.Native(streamCtor))
    stream.set("Readable", JSValue.Native(readableCtor))
    stream.set("Writable", JSValue.Native(writableCtor))
    stream.set("Duplex", JSValue.Native(duplexCtor))
    stream.set("Transform", JSValue.Native(transformCtor))
    stream.set("PassThrough", JSValue.Native(passThroughCtor))

    def pipelineImpl(args: Array[JSValue], c: JSContext): JSValue = {
      given JSContext = c
      val streams = args.dropRight(1).filter(v => v != JSValue.Undefined)
      val callback = args.lastOption.filter(BuiltinHelpers.isCallable)
      var index = 0
      while index < streams.length - 1 do {
        val src = streams(index)
        val dest = streams(index + 1)
        val pipeFn = BuiltinHelpers.getPropertyWithGetter(src, "pipe")
        BuiltinHelpers.callFunctionWithThis(pipeFn, src, Array(dest))
        if callback.isDefined then {
          val onError = NativeFunction(
            name = "onError",
            length = 1,
            impl = (errArgs, errCtx) => {
              BuiltinHelpers.callFunctionWithThis(
                callback.get,
                JSValue.Undefined,
                Array(errArgs.lastOption.getOrElse(JSValue.Undefined))
              )(using errCtx)
              JSValue.Undefined
            }
          )
          emitterOf(src).foreach { data =>
            data.listeners.getOrElseUpdate("error", mutable.ArrayBuffer.empty) += JSValue.Native(onError)
          }
          emitterOf(dest).foreach { data =>
            data.listeners.getOrElseUpdate("error", mutable.ArrayBuffer.empty) += JSValue.Native(onError)
          }
        }
        index += 1
      }
      if callback.isDefined && streams.nonEmpty then {
        val last = streams.last
        val onFinish = NativeFunction(
          name = "onFinish",
          length = 0,
          impl = (_, _) => {
            BuiltinHelpers.callFunctionWithThis(callback.get, JSValue.Undefined, Array(JSValue.Null))
            JSValue.Undefined
          }
        )
        emitterOf(last).foreach { data =>
          data.listeners.getOrElseUpdate("finish", mutable.ArrayBuffer.empty) += JSValue.Native(onFinish)
          data.listeners.getOrElseUpdate("end", mutable.ArrayBuffer.empty) += JSValue.Native(onFinish)
        }
      }
      streams.lastOption.getOrElse(JSValue.Undefined)
    }

    stream.set(
      "pipeline",
      JSValue.Native(NativeFunction(name = "pipeline", length = 1, impl = (args, c) => pipelineImpl(args, c)))
    )
    stream.set(
      "finished",
      JSValue.Native(
        NativeFunction(
          name = "finished",
          length = 1,
          impl = (args, c) => {
            given JSContext = c
            val target = args.headOption.getOrElse(JSValue.Undefined)
            val callback = args.lastOption.filter(BuiltinHelpers.isCallable)
            callback.foreach { cb =>
              Seq("finish", "end", "error", "close").foreach { event =>
                val wrapper = NativeFunction(
                  name = "onfinished",
                  length = 1,
                  impl = (callArgs, callCtx) => {
                    given JSContext = callCtx
                    val error = callArgs.lastOption.getOrElse(JSValue.Undefined)
                    val result =
                      if event == "error" then error else JSValue.Null
                    BuiltinHelpers.callFunctionWithThis(cb, JSValue.Undefined, Array(result))
                    JSValue.Undefined
                  }
                )
                emitterOf(target).foreach { data =>
                  data.listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty) += JSValue.Native(wrapper)
                }
              }
            }
            JSValue.Undefined
          }
        )
      )
    )
    stream.set(
      "promises",
      JSValue.Object(
        NodeHelpers.objectOf(
          "pipeline" -> JSValue.Native(
            NativeFunction(
              name = "pipeline",
              length = 1,
              impl = (args, c) => {
                given JSContext = c
                val promise = JSValue.Promise()
                val callback = NativeFunction(
                  name = "onpipeline",
                  length = 1,
                  impl = (cbArgs, cbCtx) => {
                    given JSContext = cbCtx
                    cbArgs.lastOption match {
                      case Some(JSValue.Undefined) | Some(JSValue.Null) | None =>
                        PromiseBuiltins.settlePromise(promise, JSValue.Undefined)
                      case Some(err) =>
                        PromiseBuiltins.rejectPromiseValue(promise, err)
                    }
                    JSValue.Undefined
                  }
                )
                pipelineImpl(args :+ JSValue.Native(callback), c)
                BuiltinHelpers.wrapPromise(promise)
              }
            )
          ),
          "finished" -> JSValue.Native(
            NativeFunction(
              name = "finished",
              length = 1,
              impl = (args, c) => {
                given JSContext = c
                val promise = JSValue.Promise()
                PromiseBuiltins.settlePromise(promise, JSValue.Undefined)
                BuiltinHelpers.wrapPromise(promise)
              }
            )
          )
        )
      )
    )

    JSValue.Object(stream)
  }

  // =========================================================================
  // fs stream helpers
  // =========================================================================

  /** `fs.createReadStream(path[, options])`: reads the whole file on the host
    * pool and pushes it through a Readable.
    */
  def createReadStream(state: NodeState): NativeFunction =
    NativeFunction(
      name = "createReadStream",
      length = 2,
      impl = (args, c) => {
        given JSContext = c
        val real = NodeHelpers.stripPathReceiver(args)
        val target = real.headOption.getOrElse(JSValue.Undefined)
        val options = real.lift(1).getOrElse(JSValue.Undefined)
        val path =
          try {
            val raw = Paths.get(NodeHelpers.toPath(target))
            if raw.isAbsolute then raw else state.cwd.resolve(raw)
          } catch case _: Throwable => Paths.get(NodeHelpers.toPath(target))
        val encoding = BuiltinHelpers.getPropertyWithGetter(options, "encoding") match {
          case JSValue.JSStr(s) => Some(s)
          case _                => None
        }
        val streamValue = makeStreamObject(readableProto, true, false)
        streamValue match {
          case JSValue.Object(obj) =>
            encoding.foreach(enc => stateOf(streamValue).foreach(_.encoding = Some(enc)))
            loop.execute {
              val bytes =
                try Files.readAllBytes(path)
                catch case _: Throwable => Array.emptyByteArray
              loop.post { () =>
                val pushFn = BuiltinHelpers.getPropertyWithGetter(streamValue, "push")
                BuiltinHelpers.callFunctionWithThis(pushFn, streamValue, Array(NodeBuffer.makeBuffer(bytes)))
                BuiltinHelpers.callFunctionWithThis(pushFn, streamValue, Array(JSValue.Null))
              }
            }
          case _ => ()
        }
        streamValue
      }
    )

  /** `fs.createWriteStream(path[, options])`. */
  def createWriteStream(state: NodeState): NativeFunction =
    NativeFunction(
      name = "createWriteStream",
      length = 2,
      impl = (args, c) => {
        given JSContext = c
        val real = NodeHelpers.stripPathReceiver(args)
        val target = real.headOption.getOrElse(JSValue.Undefined)
        val path =
          try {
            val raw = Paths.get(NodeHelpers.toPath(target))
            if raw.isAbsolute then raw else state.cwd.resolve(raw)
          } catch case _: Throwable => Paths.get(NodeHelpers.toPath(target))
        val streamValue = makeStreamObject(writableProto, false, true)
        stateOf(streamValue).foreach { st =>
          try st.output = Some(new FileOutputStream(path.toFile, true))
          catch case _: Throwable => ()
        }
        streamValue
      }
    )
}
