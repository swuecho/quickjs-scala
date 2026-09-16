package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.{JSArray, JSObject}

import java.nio.charset.StandardCharsets
import scala.collection.mutable

/** Node's `readline` and `readline/promises` modules.
  *
  * The interface is an event emitter over complete lines read from an input
  * stream. It supports `line`/`close`/`pause`/`resume`/`history` events,
  * `question`, `prompt`, `setPrompt`, `pause`/`resume`/`close`/`write`,
  * `history`, and async iteration (`for await (const line of rl)`); the
  * promises variant returns promises from `question` and its async iterator.
  */
object NodeReadline {

  private def optionOf(options: JSValue, key: String)(using
      ctx: JSContext
  ): JSValue =
    options match {
      case JSValue.Object(_) => BuiltinHelpers.getPropertyWithGetter(options, key)
      case _                 => JSValue.Undefined
    }

  private def optionString(options: JSValue, key: String)(using
      ctx: JSContext
  ): Option[String] =
    optionOf(options, key) match {
      case JSValue.JSStr(s) => Some(s)
      case _                => None
    }

  private def optionNumber(options: JSValue, key: String)(using
      ctx: JSContext
  ): Option[Double] =
    optionOf(options, key) match {
      case JSValue.Int32(n)   => Some(n.toDouble)
      case JSValue.Float64(d) => Some(d)
      case _                  => None
    }

  private def defaultProcessStream(name: String)(using
      ctx: JSContext
  ): JSValue =
    ctx.global.get("process") match {
      case JSValue.Object(process) =>
        BuiltinHelpers.getPropertyWithGetter(JSValue.Object(process), name)
      case _ => JSValue.Undefined
    }

  private def decodeChunk(chunk: JSValue): String =
    chunk match {
      case JSValue.JSStr(s) => s
      case other =>
        NodeBuffer
          .bytesOfValue(other)
          .map(bytes => new String(bytes, StandardCharsets.UTF_8))
          .getOrElse("")
    }

  private def writeTo(target: JSValue, text: String)(using
      ctx: JSContext
  ): Unit =
    if text.nonEmpty then {
      val write = BuiltinHelpers.getPropertyWithGetter(target, "write")
      if BuiltinHelpers.isCallable(write) then
        BuiltinHelpers.callFunctionWithThis(
          write,
          target,
          Array(JSValue.fromString(text))
        )
    }

  private def deferred()(using ctx: JSContext): (JSValue, JSValue, JSValue) = {
    val promiseCtor = ctx.global.get("Promise")
    val withResolvers =
      BuiltinHelpers.getPropertyWithGetter(promiseCtor, "withResolvers")
    val holder = BuiltinHelpers.callFunctionWithThis(
      withResolvers,
      promiseCtor,
      Array.empty
    )
    holder match {
      case JSValue.Object(_) =>
        (
          BuiltinHelpers.getPropertyWithGetter(holder, "promise"),
          BuiltinHelpers.getPropertyWithGetter(holder, "resolve"),
          BuiltinHelpers.getPropertyWithGetter(holder, "reject")
        )
      case _ => (JSValue.Undefined, JSValue.Undefined, JSValue.Undefined)
    }
  }

  private[node] def makeInterface(
      options: JSValue,
      promises: Boolean
  )(using ctx: JSContext): JSObject = {
    val emitter = new JsEmitter
    val iface = JSObject(prototype = ctx.objectPrototype)
    emitter.install(iface)

    val input = optionOf(options, "input") match {
      case value if value != JSValue.Undefined => value
      case _                                   => defaultProcessStream("stdin")
    }
    val output = optionOf(options, "output") match {
      case value if value != JSValue.Undefined => value
      case _                                   => defaultProcessStream("stdout")
    }
    val terminal = optionOf(options, "terminal") match {
      case JSValue.Bool(b) => b
      case JSValue.Undefined =>
        BuiltinHelpers
          .getPropertyWithGetter(input, "isTTY")
          .toBoolean
      case _ => false
    }
    val historySize =
      optionNumber(options, "historySize").map(_.toInt).getOrElse(30)
    val completer = optionOf(options, "completer")

    var closed = false
    var paused = false
    var promptText = optionString(options, "prompt").getOrElse("> ")
    val history = mutable.ArrayBuffer.empty[JSValue]
    val lineBuffer = new StringBuilder
    var questionCallback: Option[JSValue] = None
    val pendingNext =
      mutable.Queue.empty[(JSValue, JSValue)]
    // Lines that arrived while no `line` listener and no pending `next()`
    // existed (the async iterator often requests the next line a microtask
    // after the previous one resolves).
    val pendingLines = mutable.Queue.empty[String]

    def historyArray(): JSArray = {
      val array = JSArray.empty()
      history.foreach(array.push)
      array
    }

    def storeHistory(line: String): Unit =
      if historySize > 0 then {
        history += JSValue.fromString(line)
        while history.length > historySize do history.remove(0)
        emitter.emit("history", Array(JSValue.JSArrayVal(historyArray())))
      }

    def resolveNext(value: JSValue, done: Boolean): Unit =
      if pendingNext.nonEmpty then {
        val (resolve, _) = pendingNext.dequeue()
        val result = JSObject(prototype = ctx.objectPrototype)
        result.set("value", value)
        result.set("done", JSValue.Bool(done))
        BuiltinHelpers.callFunctionWithThis(
          resolve,
          JSValue.Undefined,
          Array(JSValue.Object(result))
        )
      }

    def dispatchLine(line: String): Unit = {
      storeHistory(line)
      questionCallback match {
        case Some(callback) =>
          questionCallback = None
          BuiltinHelpers.callFunctionWithThis(
            callback,
            JSValue.Undefined,
            Array(JSValue.fromString(line))
          )
        case None =>
          if pendingNext.nonEmpty then
            resolveNext(JSValue.fromString(line), done = false)
          else if emitter.listenerCount("line") > 0 then
            emitter.emit("line", Array(JSValue.fromString(line)))
          else pendingLines.enqueue(line)
      }
    }

    def onData(chunk: JSValue): Unit =
      if !closed then {
        val text = decodeChunk(chunk)
        if terminal && text.nonEmpty then writeTo(output, text)
        lineBuffer.append(text)
        var index = lineBuffer.indexOf("\n")
        while index >= 0 do {
          var line = lineBuffer.substring(0, index)
          lineBuffer.delete(0, index + 1)
          if line.endsWith("\r") then line = line.dropRight(1)
          dispatchLine(line)
          index = lineBuffer.indexOf("\n")
        }
      }

    def close(): Unit =
      if !closed then {
        closed = true
        if lineBuffer.nonEmpty then {
          val remaining = lineBuffer.toString
          lineBuffer.clear()
          dispatchLine(remaining)
        }
        while pendingNext.nonEmpty do
          resolveNext(JSValue.Undefined, done = true)
        emitter.emit("close", Array.empty)
      }

    val dataHandler = NativeFunction(
      name = "ondata",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        // Native callbacks receive the stream as the first argument.
        args.lastOption.foreach(onData)
        JSValue.Undefined
      }
    )
    val endHandler = NativeFunction(
      name = "onend",
      length = 0,
      impl = (_, callCtx) => {
        given JSContext = callCtx
        close()
        JSValue.Undefined
      }
    )
    val errorHandler = NativeFunction(
      name = "onerror",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val error = args.lastOption.getOrElse(JSValue.Undefined)
        emitter.emit("error", Array(error))
        JSValue.Undefined
      }
    )

    def attachInput(): Unit = {
      val on = BuiltinHelpers.getPropertyWithGetter(input, "on")
      if BuiltinHelpers.isCallable(on) then {
        BuiltinHelpers.callFunctionWithThis(
          on,
          input,
          Array(JSValue.fromString("data"), JSValue.Native(dataHandler))
        )
        BuiltinHelpers.callFunctionWithThis(
          on,
          input,
          Array(JSValue.fromString("end"), JSValue.Native(endHandler))
        )
        BuiltinHelpers.callFunctionWithThis(
          on,
          input,
          Array(JSValue.fromString("error"), JSValue.Native(errorHandler))
        )
      }
    }
    attachInput()
    if !paused then {
      val resume = BuiltinHelpers.getPropertyWithGetter(input, "resume")
      if BuiltinHelpers.isCallable(resume) then
        BuiltinHelpers.callFunctionWithThis(resume, input, Array.empty)
    }

    def detachInput(): Unit = {
      val remove = BuiltinHelpers.getPropertyWithGetter(input, "removeListener")
      if BuiltinHelpers.isCallable(remove) then {
        BuiltinHelpers.callFunctionWithThis(
          remove,
          input,
          Array(JSValue.fromString("data"), JSValue.Native(dataHandler))
        )
        BuiltinHelpers.callFunctionWithThis(
          remove,
          input,
          Array(JSValue.fromString("end"), JSValue.Native(endHandler))
        )
      }
    }

    def setMethod(name: String, arity: Int)(
        body: (Array[JSValue], JSContext) => JSValue
    ): Unit = {
      iface.set(
        name,
        JSValue.Native(
          NativeFunction(
            name = name,
            length = arity,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              val rest = args.headOption match {
                case Some(JSValue.Object(obj)) if obj eq iface => args.drop(1)
                case _                                          => args
              }
              body(rest, callCtx)
            }
          )
        )
      )
    }

    setMethod("setPrompt", 1) { (args, callCtx) =>
      given JSContext = callCtx
      promptText = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      JSValue.Undefined
    }
    setMethod("getPrompt", 0)((_, _) => JSValue.fromString(promptText))
    setMethod("prompt", 1) { (args, callCtx) =>
      given JSContext = callCtx
      if !closed && !paused then writeTo(output, promptText)
      JSValue.Undefined
    }
    setMethod("question", 3) { (args, callCtx) =>
      given JSContext = callCtx
      val query = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val callback = args.reverseIterator.find(BuiltinHelpers.isCallable)
      val (promise, resolve, _) = if promises then deferred() else (JSValue.Undefined, JSValue.Undefined, JSValue.Undefined)
      writeTo(output, query)
      if promises then
        questionCallback = Some(resolve)
      else questionCallback = callback
      promise
    }
    setMethod("pause", 0) { (_, callCtx) =>
      given JSContext = callCtx
      paused = true
      val pause = BuiltinHelpers.getPropertyWithGetter(input, "pause")
      if BuiltinHelpers.isCallable(pause) then
        BuiltinHelpers.callFunctionWithThis(pause, input, Array.empty)
      emitter.emit("pause", Array.empty)
      JSValue.Object(iface)
    }
    setMethod("resume", 0) { (_, callCtx) =>
      given JSContext = callCtx
      paused = false
      val resume = BuiltinHelpers.getPropertyWithGetter(input, "resume")
      if BuiltinHelpers.isCallable(resume) then
        BuiltinHelpers.callFunctionWithThis(resume, input, Array.empty)
      emitter.emit("resume", Array.empty)
      JSValue.Object(iface)
    }
    setMethod("close", 0) { (_, callCtx) =>
      given JSContext = callCtx
      detachInput()
      close()
      JSValue.Undefined
    }
    setMethod("write", 1) { (args, callCtx) =>
      given JSContext = callCtx
      args.headOption.foreach(chunk => onData(chunk))
      JSValue.Undefined
    }
    setMethod("getCursorPos", 0) { (_, _) =>
      val pos = JSObject(prototype = ctx.objectPrototype)
      pos.set("rows", JSValue.fromInt(0))
      pos.set("cols", JSValue.fromInt(0))
      JSValue.Object(pos)
    }
    // `completer` support: resolve the first callback argument.
    if completer != JSValue.Undefined then
      iface.set("completer", completer)

    // Dynamic `history` getter.
    val historyGetter = NativeFunction(
      name = "get history",
      length = 0,
      impl = (_, _) => JSValue.JSArrayVal(historyArray())
    )
    iface.defineAccessorProperty(
      "history",
      Some(JSValue.Native(historyGetter)),
      None,
      enumerable = false,
      configurable = true
    )
    iface.set("terminal", JSValue.Bool(terminal))
    iface.set("closed", JSValue.Bool(false))

    // Async iteration: `next()` waits for the next line; `return()` closes.
    val asyncIterator = JSObject(prototype = ctx.objectPrototype)
    emitter.install(asyncIterator)
    asyncIterator.set(
      "next",
      JSValue.Native(
        NativeFunction(
          name = "next",
          length = 0,
          impl = (_, callCtx) => {
            given JSContext = callCtx
            if closed then {
              val result = JSObject(prototype = ctx.objectPrototype)
              result.set("value", JSValue.Undefined)
              result.set("done", JSValue.Bool(true))
              val (promise, resolve, _) = deferred()
              BuiltinHelpers.callFunctionWithThis(
                resolve,
                JSValue.Undefined,
                Array(JSValue.Object(result))
              )
              promise
            } else {
              val (promise, resolve, _) = deferred()
              if pendingLines.nonEmpty then {
                val line = pendingLines.dequeue()
                val result = JSObject(prototype = ctx.objectPrototype)
                result.set("value", JSValue.fromString(line))
                result.set("done", JSValue.Bool(false))
                BuiltinHelpers.callFunctionWithThis(
                  resolve,
                  JSValue.Undefined,
                  Array(JSValue.Object(result))
                )
              } else pendingNext.enqueue((resolve, JSValue.Undefined))
              promise
            }
          }
        )
      )
    )
    asyncIterator.set(
      "return",
      JSValue.Native(
        NativeFunction(
          name = "return",
          length = 0,
          impl = (_, callCtx) => {
            given JSContext = callCtx
            close()
            val result = JSObject(prototype = ctx.objectPrototype)
            result.set("value", JSValue.Undefined)
            result.set("done", JSValue.Bool(true))
            val (promise, resolve, _) = deferred()
            BuiltinHelpers.callFunctionWithThis(
              resolve,
              JSValue.Undefined,
              Array(JSValue.Object(result))
            )
            promise
          }
        )
      )
    )
    val asyncIteratorSymbol =
      BuiltinHelpers.wellKnownSymbolId("asyncIterator")
    asyncIterator.defineSymbolProperty(
      asyncIteratorSymbol,
      JSValue.Native(
        NativeFunction(
          name = "[Symbol.asyncIterator]",
          length = 0,
          impl = (_, _) => JSValue.Object(asyncIterator)
        )
      ),
      enumerable = false,
      configurable = true
    )
    iface.defineSymbolProperty(
      asyncIteratorSymbol,
      JSValue.Native(
        NativeFunction(
          name = "[Symbol.asyncIterator]",
          length = 0,
          impl = (_, _) => JSValue.Object(asyncIterator)
        )
      ),
      enumerable = false,
      configurable = true
    )

    iface
  }

  private def cursorWrite(
      stream: JSValue,
      text: String
  )(using ctx: JSContext): Unit =
    writeTo(stream, text)

  def create()(using ctx: JSContext): JSValue = {
    val readline = JSObject(prototype = ctx.objectPrototype)
    readline.set(
      "createInterface",
      JSValue.Native(
        NativeFunction(
          name = "createInterface",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = args.headOption match {
              case Some(JSValue.Object(obj)) if obj eq readline => args.drop(1)
              case _                                            => args
            }
            val options = rest.headOption.getOrElse(JSValue.Undefined)
            JSValue.Object(makeInterface(options, promises = false))
          }
        )
      )
    )
    readline.set(
      "cursorTo",
      JSValue.Native(
        NativeFunction(
          name = "cursorTo",
          length = 3,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val stream = args.headOption.getOrElse(JSValue.Undefined)
            val x = args.lift(1).map(v => NodeHelpers.toNumber(v).toInt).getOrElse(0)
            cursorWrite(stream, s"\u001b[${x + 1}G")
            JSValue.Undefined
          }
        )
      )
    )
    readline.set(
      "moveCursor",
      JSValue.Native(
        NativeFunction(
          name = "moveCursor",
          length = 3,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val stream = args.headOption.getOrElse(JSValue.Undefined)
            val dx = args.lift(1).map(v => NodeHelpers.toNumber(v).toInt).getOrElse(0)
            val dy = args.lift(2).map(v => NodeHelpers.toNumber(v).toInt).getOrElse(0)
            val code =
              (if dy > 0 then s"\u001b[${dy}B" else if dy < 0 then s"\u001b[${-dy}A" else "") +
                (if dx > 0 then s"\u001b[${dx}C" else if dx < 0 then s"\u001b[${-dx}D" else "")
            cursorWrite(stream, code)
            JSValue.Undefined
          }
        )
      )
    )
    readline.set(
      "clearLine",
      JSValue.Native(
        NativeFunction(
          name = "clearLine",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val stream = args.headOption.getOrElse(JSValue.Undefined)
            val dir = args.lift(1).map(v => NodeHelpers.toNumber(v).toInt).getOrElse(0)
            cursorWrite(stream, s"\u001b[${if dir < 0 then 1 else if dir > 0 then 0 else 2}K")
            JSValue.Undefined
          }
        )
      )
    )
    readline.set(
      "clearScreenDown",
      JSValue.Native(
        NativeFunction(
          name = "clearScreenDown",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val stream = args.headOption.getOrElse(JSValue.Undefined)
            cursorWrite(stream, "\u001b[0J")
            JSValue.Undefined
          }
        )
      )
    )
    readline.set(
      "emitKeypressEvents",
      JSValue.Native(
        NativeFunction(
          name = "emitKeypressEvents",
          length = 2,
          impl = (_, _) => JSValue.Undefined
        )
      )
    )
    JSValue.Object(readline)
  }

  /** `readline/promises.createInterface`. */
  def createPromises()(using ctx: JSContext): JSValue = {
    val module = JSObject(prototype = ctx.objectPrototype)
    module.set(
      "createInterface",
      JSValue.Native(
        NativeFunction(
          name = "createInterface",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = args.headOption match {
              case Some(JSValue.Object(obj)) if obj eq module => args.drop(1)
              case _                                          => args
            }
            val options = rest.headOption.getOrElse(JSValue.Undefined)
            JSValue.Object(makeInterface(options, promises = true))
          }
        )
      )
    )
    module.set(
      "ReadlinePromises",
      JSValue.Native(
        NativeFunction(
          name = "ReadlinePromises",
          length = 0,
          impl = (_, callCtx) => {
            given JSContext = callCtx
            JSValue.Object(makeInterface(JSValue.Undefined, promises = true))
          }
        )
      )
    )
    JSValue.Object(module)
  }
}
