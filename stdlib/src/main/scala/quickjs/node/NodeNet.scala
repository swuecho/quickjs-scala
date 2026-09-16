package quickjs.node

import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.objmodel.{JSArray, JSObject}

import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket}
import javax.net.ssl.SSLSocketFactory
import scala.collection.mutable

/** A pragmatic subset of Node's `net` module: EventEmitter-based
  * `Socket`/`connect`/`createConnection` backed by `java.net.Socket`, and
  * `createServer`/`Server` backed by `java.net.ServerSocket`. The `tls`
  * module reuses the same socket plumbing with an `SSLSocketFactory`.
  */
final class NodeNet(loop: HostEventLoop)(using ctx: JSContext) {

  private final class Listener(val original: JSValue, val once: Boolean)

  private final class SocketState {
    val listeners = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[Listener]]
    var encoding: Option[String] = None
    var paused = false
    val pausedData = mutable.ArrayBuffer.empty[Array[Byte]]
    var socket: Socket = null
    var serverSocket: ServerSocket = null
    var destroyed = false
    var connecting = false
    var secure = false
    var timeoutMs = 0
  }

  private final class ServerState {
    val listeners = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[Listener]]
    var serverSocket: ServerSocket = null
    var listening = false
    var port = 0
    var host = "0.0.0.0"
  }

  private val socketPrototype =
    JSObject(prototype = ctx.objectPrototype, extensible = true)
  private val serverPrototype =
    JSObject(prototype = ctx.objectPrototype, extensible = true)

  private def stateOf(value: JSValue): Option[SocketState] =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__netSocket") match {
          case Some(JSValue.Native(state: SocketState)) => Some(state)
          case _                                        => None
        }
      case _ => None
    }

  private def serverStateOf(value: JSValue): Option[ServerState] =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__netServer") match {
          case Some(JSValue.Native(state: ServerState)) => Some(state)
          case _                                        => None
        }
      case _ => None
    }

  private def attachSocket(obj: JSObject, state: SocketState): Unit =
    obj.initProperty(
      "__netSocket",
      JSValue.Native(state),
      enumerable = false,
      writable = false,
      configurable = false
    )

  private def attachServer(obj: JSObject, state: ServerState): Unit =
    obj.initProperty(
      "__netServer",
      JSValue.Native(state),
      enumerable = false,
      writable = false,
      configurable = false
    )

  private def codedError(code: String, message: String): JSValue = {
    val error = ctx.createError("Error", Option(message).getOrElse("socket error"))
    error match {
      case JSValue.Object(obj) =>
        obj.defineProperty(
          "code",
          JSValue.fromString(code),
          enumerable = false,
          writable = true,
          configurable = true
        )
      case _ => ()
    }
    error
  }

  /** Emit an event; unhandled `error` events throw (Node semantics). */
  private def emit(
      target: JSValue,
      listeners: mutable.LinkedHashMap[String, mutable.ArrayBuffer[Listener]],
      event: String,
      eventArgs: Array[JSValue]
  ): Boolean =
    listeners.get(event) match {
      case Some(entries) if entries.nonEmpty =>
        val snapshot = entries.toArray
        snapshot.foreach { listener =>
          if listener.once then entries.remove(entries.indexWhere(_ eq listener))
          BuiltinHelpers.callFunctionWithThis(
            listener.original,
            target,
            eventArgs
          )
        }
        if entries.isEmpty then listeners.remove(event)
        true
      case _ =>
        if event == "error" then
          throw new quickjs.runtime.JSException(
            eventArgs.headOption.getOrElse(
              codedError("Error", "Unhandled 'error' event")
            )
          )
        false
    }

  private def emitSocket(
      socket: JSValue,
      state: SocketState,
      event: String,
      eventArgs: Array[JSValue]
  ): Boolean =
    emit(socket, state.listeners, event, eventArgs)

  private def addListener(
      listeners: mutable.LinkedHashMap[String, mutable.ArrayBuffer[Listener]],
      event: String,
      listener: JSValue,
      once: Boolean
  ): Unit =
    listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty) +=
      new Listener(listener, once)

  private def boolOption(options: JSValue, name: String): Option[Boolean] =
    BuiltinHelpers.getPropertyWithGetter(options, name) match {
      case JSValue.Bool(b) => Some(b)
      case _               => None
    }

  // =========================================================================
  // Socket creation / connection
  // =========================================================================

  private def newSocket(secure: Boolean): (JSValue, SocketState) = {
    val obj = JSObject(prototype = socketPrototype, extensible = true)
    val state = new SocketState
    state.secure = secure
    attachSocket(obj, state)
    (JSValue.Object(obj), state)
  }

  private def closeSocket(socket: JSValue, state: SocketState): Unit =
    if !state.destroyed then {
      state.destroyed = true
      state.socket match {
        case null => ()
        case s    => try s.close() catch case _: Throwable => ()
      }
      emitSocket(socket, state, "close", Array.empty)
    }

  /** Shared connect implementation for `net.connect` and `tls.connect`. */
  private def connect(
      host: String,
      port: Int,
      secure: Boolean,
      servername: Option[String],
      timeoutMs: Int
  ): JSValue = {
    val (socketVal, state) = newSocket(secure)
    state.connecting = true
    loop.execute {
      try {
        val raw: Socket =
          if secure then SSLSocketFactory.getDefault.createSocket()
          else new Socket()
        raw.connect(new InetSocketAddress(host, port), timeoutMs)
        if secure then {
          raw match {
            case ssl: javax.net.ssl.SSLSocket =>
              if servername.isDefined then {
                val params = ssl.getSSLParameters
                params.setEndpointIdentificationAlgorithm("HTTPS")
                ssl.setSSLParameters(params)
              }
              ssl.startHandshake()
            case _ => ()
          }
        }
        state.socket = raw
        state.connecting = false
        loop.post { () =>
          emitSocket(socketVal, state, "connect", Array.empty)
          startReading(socketVal, state)
        }
      } catch {
        case e: Throwable =>
          state.connecting = false
          loop.post { () =>
            val message = Option(e.getMessage).getOrElse("connect failed")
            emitSocket(
              socketVal,
              state,
              "error",
              Array(codedError("ECONNREFUSED", message))
            )
            closeSocket(socketVal, state)
          }
      }
    }
    socketVal
  }

  private def startReading(socket: JSValue, state: SocketState): Unit =
    loop.execute {
      try {
        val in = state.socket.getInputStream
        val buf = new Array[Byte](16384)
        var n = in.read(buf)
        while n >= 0 do {
          if n > 0 then {
            val data = java.util.Arrays.copyOf(buf, n)
            loop.post { () => deliverData(socket, state, data) }
          }
          n = in.read(buf)
        }
        loop.post { () =>
          emitSocket(socket, state, "end", Array.empty)
          closeSocket(socket, state)
        }
      } catch {
        case _: Throwable =>
          loop.post { () => closeSocket(socket, state) }
      }
    }

  private def deliverData(
      socket: JSValue,
      state: SocketState,
      data: Array[Byte]
  ): Unit =
    if state.paused then state.pausedData += data
    else
      state.encoding match {
        case Some(enc) =>
          emitSocket(
            socket,
            state,
            "data",
            Array(JSValue.fromString(NodeEncodings.stringFromBytes(data, enc)))
          )
        case None =>
          emitSocket(socket, state, "data", Array(NodeBuffer.makeBuffer(data)))
      }

  // =========================================================================
  // Public module
  // =========================================================================

  private def socketMethod(name: String, length: Int)(
      impl: (SocketState, JSValue, Array[JSValue], JSContext) => JSValue
  ): NativeFunction =
    NativeFunction(
      name = name,
      length = length,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val thisValue = args.headOption.getOrElse(JSValue.Undefined)
        stateOf(thisValue) match {
          case Some(state) =>
            impl(state, thisValue, args.drop(1), callCtx)
          case None =>
            callCtx.throwTypeError(
              s"${name} called on a non-Socket receiver"
            )
        }
      }
    )

  def create(): JSValue = {
    val net = JSObject(prototype = ctx.objectPrototype)

    // ---- Socket prototype -------------------------------------------------
    val onFn = NativeFunction(
      name = "on",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        args.headOption.foreach { thisValue =>
          stateOf(thisValue).foreach { state =>
            val event = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
            val listener = args.lift(2).getOrElse(JSValue.Undefined)
            if BuiltinHelpers.isCallable(listener) then
              addListener(state.listeners, event, listener, once = false)
          }
        }
        args.headOption.getOrElse(JSValue.Undefined)
      }
    )
    val onceFn = NativeFunction(
      name = "once",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        args.headOption.foreach { thisValue =>
          stateOf(thisValue).foreach { state =>
            val event = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
            val listener = args.lift(2).getOrElse(JSValue.Undefined)
            if BuiltinHelpers.isCallable(listener) then
              addListener(state.listeners, event, listener, once = true)
          }
        }
        args.headOption.getOrElse(JSValue.Undefined)
      }
    )
    val removeListenerFn = NativeFunction(
      name = "removeListener",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        args.headOption.foreach { thisValue =>
          stateOf(thisValue).foreach { state =>
            val event = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
            val listener = args.lift(2).getOrElse(JSValue.Undefined)
            state.listeners.get(event).foreach { entries =>
              entries.remove(entries.indexWhere(_.original == listener))
              if entries.isEmpty then state.listeners.remove(event)
            }
          }
        }
        args.headOption.getOrElse(JSValue.Undefined)
      }
    )
    val emitFn = NativeFunction(
      name = "emit",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        args.headOption match {
          case Some(thisValue) =>
            stateOf(thisValue) match {
              case Some(state) =>
                val event =
                  NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
                val eventArgs = if args.length > 2 then args.drop(2) else Array.empty[JSValue]
                JSValue.Bool(emitSocket(thisValue, state, event, eventArgs))
              case None => JSValue.Bool(false)
            }
          case None => JSValue.Bool(false)
        }
      }
    )
    val writeFn = socketMethod("write", 1) { (state, socketVal, rest, callCtx) =>
      given JSContext = callCtx
      if state.destroyed || state.socket == null then
        throw new quickjs.runtime.JSException(
          codedError("ERR_SOCKET_CLOSED", "Socket is not open")
        )
      val chunk = rest.headOption.getOrElse(JSValue.Undefined)
      val bytes = NodeBuffer.bytesOfValue(chunk).getOrElse(
        NodeEncodings.bytesFromString(
          BuiltinHelpers.toJSString(chunk),
          rest.lift(1).filter(_ != JSValue.Undefined).map(NodeHelpers.toStr).getOrElse("utf8")
        )
      )
      state.socket.getOutputStream.write(bytes)
      state.socket.getOutputStream.flush()
      rest.drop(2).find(BuiltinHelpers.isCallable).foreach(cb =>
        BuiltinHelpers.callFunctionWithThis(cb, JSValue.Undefined, Array.empty)
      )
      JSValue.Bool(true)
    }
    val endFn = socketMethod("end", 0) { (state, socketVal, rest, callCtx) =>
      given JSContext = callCtx
      if !state.destroyed && state.socket != null then {
        rest.headOption.filter(v => v != JSValue.Undefined && !BuiltinHelpers.isCallable(v)).foreach { chunk =>
          val bytes = NodeBuffer.bytesOfValue(chunk).getOrElse(
            NodeEncodings.bytesFromString(BuiltinHelpers.toJSString(chunk), "utf8")
          )
          state.socket.getOutputStream.write(bytes)
        }
        try state.socket.shutdownOutput()
        catch case _: Throwable => ()
        rest.find(BuiltinHelpers.isCallable).foreach(cb =>
          BuiltinHelpers.callFunctionWithThis(cb, JSValue.Undefined, Array.empty)
        )
        emitSocket(socketVal, state, "finish", Array.empty)
      }
      socketVal
    }
    val destroyFn = socketMethod("destroy", 0) { (state, socketVal, rest, callCtx) =>
      given JSContext = callCtx
      closeSocket(socketVal, state)
      socketVal
    }
    val setEncodingFn = socketMethod("setEncoding", 1) { (state, socketVal, rest, callCtx) =>
      state.encoding = rest.headOption.filter(_ != JSValue.Undefined).map(NodeHelpers.toStr)
      socketVal
    }
    val pauseFn = socketMethod("pause", 0) { (state, socketVal, _, _) =>
      state.paused = true
      socketVal
    }
    val resumeFn = socketMethod("resume", 0) { (state, socketVal, _, _) =>
      state.paused = false
      val pending = state.pausedData.toArray
      state.pausedData.clear()
      pending.foreach(data => deliverData(socketVal, state, data))
      socketVal
    }
    val setTimeoutFn = socketMethod("setTimeout", 1) { (state, socketVal, rest, _) =>
      state.timeoutMs =
        rest.headOption.map(BuiltinHelpers.toNumber(_).toInt).getOrElse(0)
      socketVal
    }
    val noopSocketFn = socketMethod("noop", 0) { (_, socketVal, _, _) =>
      socketVal
    }
    val addressFn = socketMethod("address", 0) { (state, _, _, callCtx) =>
      given JSContext = callCtx
      if state.socket == null then JSValue.Null
      else
        val obj = JSObject(prototype = callCtx.objectPrototype)
        obj.set(
          "address",
          JSValue.fromString(
            Option(state.socket.getLocalAddress)
              .map(_.getHostAddress)
              .getOrElse("0.0.0.0")
          )
        )
        obj.set("family", JSValue.fromString(if state.secure then "IPv6" else "IPv4"))
        obj.set("port", JSValue.fromInt(state.socket.getLocalPort))
        JSValue.Object(obj)
    }

    val socketDefs: Seq[(String, NativeFunction)] = Seq(
      "on" -> onFn,
      "addListener" -> onFn,
      "once" -> onceFn,
      "removeListener" -> removeListenerFn,
      "off" -> removeListenerFn,
      "emit" -> emitFn,
      "write" -> writeFn,
      "end" -> endFn,
      "destroy" -> destroyFn,
      "setEncoding" -> setEncodingFn,
      "setDefaultEncoding" -> setEncodingFn,
      "pause" -> pauseFn,
      "resume" -> resumeFn,
      "setTimeout" -> setTimeoutFn,
      "setNoDelay" -> noopSocketFn,
      "setKeepAlive" -> noopSocketFn,
      "ref" -> noopSocketFn,
      "unref" -> noopSocketFn,
      "address" -> addressFn
    )
    socketDefs.foreach { case (name, fn) =>
      socketPrototype.defineProperty(name, JSValue.Native(fn), enumerable = false)
    }
    // Read-only accessors.
    def accessor(name: String)(get: SocketState => JSValue): Unit =
      socketPrototype.defineAccessorProperty(
        name,
        getter = Some(
          JSValue.Native(
            NativeFunction(
              name = s"get $name",
              length = 0,
              impl = (args, _) =>
                args.headOption.flatMap(stateOf).map(get).getOrElse(JSValue.Undefined)
            )
          )
        ),
        setter = None,
        enumerable = false,
        configurable = true
      )
    accessor("remoteAddress")(s =>
      if s.socket == null then JSValue.Undefined
      else JSValue.fromString(s.socket.getInetAddress.getHostAddress)
    )
    accessor("remoteFamily")(s =>
      if s.socket == null then JSValue.Undefined
      else JSValue.fromString("IPv4")
    )
    accessor("remotePort")(s =>
      if s.socket == null then JSValue.Undefined else JSValue.fromInt(s.socket.getPort)
    )
    accessor("localAddress")(s =>
      if s.socket == null then JSValue.Undefined
      else JSValue.fromString(s.socket.getLocalAddress.getHostAddress)
    )
    accessor("localPort")(s =>
      if s.socket == null then JSValue.Undefined
      else JSValue.fromInt(s.socket.getLocalPort)
    )
    accessor("connecting")(s => JSValue.Bool(s.connecting))
    accessor("destroyed")(s => JSValue.Bool(s.destroyed))
    accessor("pending")(s => JSValue.Bool(false))
    accessor("bytesRead")(_ => JSValue.fromInt(0))
    accessor("bytesWritten")(_ => JSValue.fromInt(0))

    val socketConstructor = NativeConstructor(
      name = "Socket",
      callImpl = (_, callCtx) =>
        callCtx.throwTypeError("Constructor Socket requires 'new'"),
      constructImpl = (_, callCtx) => {
        given JSContext = callCtx
        newSocket(secure = false)._1
      },
      prototype = socketPrototype
    )
    BuiltinHelpers.initConstructor(socketConstructor, length = 0)
    socketPrototype.defineProperty(
      "constructor",
      JSValue.Native(socketConstructor),
      enumerable = false
    )

    // ---- connect ----------------------------------------------------------
    def connectArgs(args: Array[JSValue]): (String, Int, Option[String], Int) =
      args.headOption match {
        case Some(JSValue.Int32(port)) =>
          val host = args.lift(1).filter(_ != JSValue.Undefined).map(NodeHelpers.toStr).getOrElse("localhost")
          (host, port, None, 0)
        case Some(JSValue.Float64(d)) =>
          val host = args.lift(1).filter(_ != JSValue.Undefined).map(NodeHelpers.toStr).getOrElse("localhost")
          (host, d.toInt, None, 0)
        case Some(options @ JSValue.Object(_)) =>
          val port = BuiltinHelpers.toNumber(
            BuiltinHelpers.getPropertyWithGetter(options, "port")
          ).toInt
          val host =
            BuiltinHelpers.getPropertyWithGetter(options, "host") match {
              case JSValue.JSStr(s) => s
              case _                => "localhost"
            }
          val servername =
            BuiltinHelpers.getPropertyWithGetter(options, "servername") match {
              case JSValue.JSStr(s) => Some(s)
              case _                => Some(host)
            }
          val timeout = BuiltinHelpers.toNumber(
            BuiltinHelpers.getPropertyWithGetter(options, "timeout")
          ).toInt
          (host, port, servername, math.max(timeout, 0))
        case Some(JSValue.JSStr(path)) =>
          // Unix domain sockets are not supported; surface an error.
          throw new quickjs.runtime.JSException(
            codedError("ERR_UNSUPPORTED", "Unix domain sockets are not supported")
          )
        case _ => ("localhost", 0, None, 0)
      }

    /** Method calls on `net`/`tls` receive the module object as `this`. */
    def stripModule(args: Array[JSValue], module: JSObject): Array[JSValue] =
      if args.nonEmpty && (args(0) match {
            case JSValue.Object(o) => o eq module
            case _                 => false
          })
      then args.drop(1)
      else args

    def makeConnect(secure: Boolean, module: JSObject): NativeFunction =
      NativeFunction(
        name = "connect",
        length = 1,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val rest = stripModule(args, module)
          val (host, port, servername, timeout) = connectArgs(rest)
          val socketVal = connect(host, port, secure, servername, timeout)
          rest.find(BuiltinHelpers.isCallable).foreach(cb =>
            stateOf(socketVal).foreach(state =>
              addListener(state.listeners, "connect", cb, once = true)
            )
          )
          socketVal
        }
      )

    net.set("Socket", JSValue.Native(socketConstructor))
    net.set("connect", JSValue.Native(makeConnect(secure = false, net)))
    net.set("createConnection", JSValue.Native(makeConnect(secure = false, net)))
    net.set("isIP", JSValue.Native(NativeFunction(
      name = "isIP",
      length = 1,
      impl = (args, _) => JSValue.fromInt(isIPv4(stripModule(args, net).headOption.map(NodeHelpers.toStr).getOrElse("")) match {
        case true  => 4
        case false =>
          isIPv6(args.headOption.map(NodeHelpers.toStr).getOrElse("")) match {
            case true => 6
            case false => 0
          }
      })
    )))
    net.set("isIPv4", JSValue.Native(NativeFunction(
      name = "isIPv4",
      length = 1,
      impl = (args, _) => JSValue.Bool(isIPv4(stripModule(args, net).headOption.map(NodeHelpers.toStr).getOrElse("")))
    )))
    net.set("isIPv6", JSValue.Native(NativeFunction(
      name = "isIPv6",
      length = 1,
      impl = (args, _) => JSValue.Bool(isIPv6(stripModule(args, net).headOption.map(NodeHelpers.toStr).getOrElse("")))
    )))

    // ---- Server -----------------------------------------------------------
    net.set("Server", JSValue.Native(
      NativeConstructor(
        name = "Server",
        callImpl = (_, callCtx) => callCtx.throwTypeError("Constructor Server requires 'new'"),
        constructImpl = (_, callCtx) => {
          given JSContext = callCtx
          createServer(JSValue.Undefined)
        },
        prototype = serverPrototype
      )
    ))
    net.set("createServer", JSValue.Native(NativeFunction(
      name = "createServer",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = stripModule(args, net)
        createServer(rest.headOption.getOrElse(JSValue.Undefined))
      }
    )))

    JSValue.Object(net)
  }

  private def createServer(connectionListener: JSValue): JSValue = {
    val obj = JSObject(prototype = serverPrototype, extensible = true)
    val state = new ServerState
    attachServer(obj, state)
    val serverVal = JSValue.Object(obj)
    if BuiltinHelpers.isCallable(connectionListener) then
      addListener(state.listeners, "connection", connectionListener, once = false)

    val listenFn = NativeFunction(
      name = "listen",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = args.drop(1)
        val port = rest.headOption.map(BuiltinHelpers.toNumber(_).toInt).getOrElse(0)
        val host = rest.lift(1).filter(_ != JSValue.Undefined).map(NodeHelpers.toStr).getOrElse("0.0.0.0")
        loop.execute {
          try {
            val ss = new ServerSocket()
            ss.bind(new InetSocketAddress(host, port))
            state.serverSocket = ss
            state.listening = true
            state.port = ss.getLocalPort
            state.host = host
            loop.post { () =>
              rest.find(BuiltinHelpers.isCallable).foreach(cb =>
                BuiltinHelpers.callFunctionWithThis(cb, JSValue.Undefined, Array.empty)
              )
              emit(serverVal, state.listeners, "listening", Array.empty)
            }
            while !ss.isClosed do {
              val client = ss.accept()
              val (socketVal, socketState) = newSocket(secure = false)
              socketState.socket = client
              loop.post { () =>
                emit(serverVal, state.listeners, "connection", Array(socketVal))
                startReading(socketVal, socketState)
              }
            }
          } catch {
            case _: Throwable =>
              if state.listening then
                loop.post { () =>
                  emit(
                    serverVal,
                    state.listeners,
                    "error",
                    Array(codedError("EADDRINUSE", "listen failed"))
                  )
                }
          }
        }
        serverVal
      }
    )
    val closeFn = NativeFunction(
      name = "close",
      length = 0,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        // Clear the listening flag before closing so the accept loop's
        // exception handler does not report a spurious "listen failed" error.
        state.listening = false
        state.serverSocket match {
          case null => ()
          case ss   => try ss.close() catch case _: Throwable => ()
        }
        args.find(BuiltinHelpers.isCallable).foreach(cb =>
          BuiltinHelpers.callFunctionWithThis(cb, JSValue.Undefined, Array.empty)
        )
        serverVal
      }
    )
    val addressFn = NativeFunction(
      name = "address",
      length = 0,
      impl = (_, callCtx) => {
        given JSContext = callCtx
        val obj = JSObject(prototype = callCtx.objectPrototype)
        obj.set("address", JSValue.fromString(state.host))
        obj.set("family", JSValue.fromString("IPv4"))
        obj.set("port", JSValue.fromInt(state.port))
        JSValue.Object(obj)
      }
    )
    val onFn = NativeFunction(
      name = "on",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val event = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
        val listener = args.lift(2).getOrElse(JSValue.Undefined)
        if BuiltinHelpers.isCallable(listener) then
          addListener(state.listeners, event, listener, once = false)
        serverVal
      }
    )
    val onceFn = NativeFunction(
      name = "once",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val event = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
        val listener = args.lift(2).getOrElse(JSValue.Undefined)
        if BuiltinHelpers.isCallable(listener) then
          addListener(state.listeners, event, listener, once = true)
        serverVal
      }
    )
    def serverAccessor(name: String)(get: ServerState => JSValue): Unit =
      serverPrototype.defineAccessorProperty(
        name,
        getter = Some(
          JSValue.Native(
            NativeFunction(
              name = s"get $name",
              length = 0,
              impl = (_, _) => get(state)
            )
          )
        ),
        setter = None,
        enumerable = false,
        configurable = true
      )
    val emitFn = NativeFunction(
      name = "emit",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val event = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
        JSValue.Bool(emit(serverVal, state.listeners, event, args.drop(2)))
      }
    )
    val removeListenerFn = NativeFunction(
      name = "removeListener",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val event = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
        val listener = args.lift(2).getOrElse(JSValue.Undefined)
        state.listeners.get(event).foreach { entries =>
          val index =
            entries.indexWhere(e => NodeHelpers.sameValue(e.original, listener))
          if index >= 0 then entries.remove(index)
        }
        serverVal
      }
    )
    val removeAllListenersFn = NativeFunction(
      name = "removeAllListeners",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val event = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
        if event.isEmpty then state.listeners.clear()
        else state.listeners.remove(event)
        serverVal
      }
    )
    val listenerCountFn = NativeFunction(
      name = "listenerCount",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val event = NodeHelpers.toStr(args.lift(1).getOrElse(JSValue.Undefined))
        JSValue.fromInt(state.listeners.get(event).map(_.length).getOrElse(0))
      }
    )
    Seq(
      "listen" -> listenFn,
      "close" -> closeFn,
      "address" -> addressFn,
      "on" -> onFn,
      "addListener" -> onFn,
      "once" -> onceFn,
      "emit" -> emitFn,
      "removeListener" -> removeListenerFn,
      "off" -> removeListenerFn,
      "removeAllListeners" -> removeAllListenersFn,
      "listenerCount" -> listenerCountFn
    ).foreach { case (name, fn) =>
      serverPrototype.defineProperty(name, JSValue.Native(fn), enumerable = false)
    }
    serverAccessor("listening")(s => JSValue.Bool(s.listening))
    serverVal
  }

  /** The `tls` module: `connect` reuses the socket implementation with an
    * `SSLSocketFactory`; `createServer`/`TLSSocket` are pragmatic aliases.
    */
  def createTlsModule(): JSValue = {
    val tls = JSObject(prototype = ctx.objectPrototype)
    val connectFn = NativeFunction(
      name = "connect",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = if args.nonEmpty && (args(0) match {
              case JSValue.Object(o) => o eq tls
              case _                 => false
            }) then args.drop(1) else args
        val (host, port, servername, timeout) = rest.headOption match {
          case Some(options @ JSValue.Object(_)) =>
            val p = BuiltinHelpers
              .toNumber(BuiltinHelpers.getPropertyWithGetter(options, "port"))
              .toInt
            val h = BuiltinHelpers.getPropertyWithGetter(options, "host") match {
              case JSValue.JSStr(s) => s
              case _                => "localhost"
            }
            val sn = BuiltinHelpers.getPropertyWithGetter(options, "servername") match {
              case JSValue.JSStr(s) => Some(s)
              case _                => Some(h)
            }
            val t = BuiltinHelpers
              .toNumber(BuiltinHelpers.getPropertyWithGetter(options, "timeout"))
              .toInt
            (h, p, sn, math.max(t, 0))
          case Some(JSValue.Int32(port)) =>
            (
              rest.lift(1).filter(_ != JSValue.Undefined).map(NodeHelpers.toStr).getOrElse("localhost"),
              port,
              None,
              0
            )
          case _ => ("localhost", 0, None, 0)
        }
        val socketVal = connect(host, port, secure = true, servername, timeout)
        rest.find(BuiltinHelpers.isCallable).foreach(cb =>
          stateOf(socketVal).foreach(state =>
            addListener(state.listeners, "secureConnect", cb, once = true)
          )
        )
        socketVal
      }
    )
    tls.set("connect", JSValue.Native(connectFn))
    tls.set("createServer", JSValue.Native(NativeFunction(
      name = "createServer",
      length = 0,
      impl = (_, callCtx) =>
        given JSContext = callCtx
        createServer(JSValue.Undefined)
    )))
    tls.set("checkServerIdentity", JSValue.Native(NativeFunction(
      name = "checkServerIdentity",
      length = 2,
      impl = (_, _) => JSValue.Undefined
    )))
    tls.set("DEFAULT_MIN_VERSION", JSValue.fromString("TLSv1.2"))
    tls.set("DEFAULT_MAX_VERSION", JSValue.fromString("TLSv1.3"))
    JSValue.Object(tls)
  }

  private def isIPv4(value: String): Boolean =
    value.nonEmpty && value.matches("^(\\d{1,3}\\.){3}\\d{1,3}$") && {
      val parts = value.split("\\.")
      parts.length == 4 && parts.forall(p =>
        p.nonEmpty && p.toIntOption.exists(n => n >= 0 && n <= 255)
      )
    }

  private def isIPv6(value: String): Boolean =
    value.nonEmpty && value.contains(":") && {
      try {
        InetAddress.getByName(value).isInstanceOf[java.net.Inet6Address]
      } catch case _: Throwable => false
    }
}
