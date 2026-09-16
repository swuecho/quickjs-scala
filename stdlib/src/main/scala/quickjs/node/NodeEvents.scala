package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.{JSArray, JSObject}

import scala.collection.mutable

/** Node's `events` module (EventEmitter). */
object NodeEvents {

  private final class EventEntry(val original: JSValue, val wrapper: JSValue)

  private final class EventStore {
    val listeners: mutable.LinkedHashMap[String, mutable.ArrayBuffer[EventEntry]] =
      mutable.LinkedHashMap.empty
    var maxListeners: Int = 10
  }

  private def objectOf(value: JSValue): Option[JSObject] =
    value match {
      case JSValue.Object(obj)     => Some(obj)
      case f: JSValue.Function     => Some(f.funcObj)
      case JSValue.Native(nf: quickjs.value.NativeFunction) =>
        Some(nf.funcObj)
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        Some(nc.funcObj)
      case _ => None
    }

  private def storeOf(value: JSValue): Option[EventStore] =
    objectOf(value).flatMap { obj =>
      obj.getOwnPropertyRaw("__events") match {
        case Some(JSValue.Native(store: EventStore)) => Some(store)
        case _                                       => None
      }
    }

  private def keyOf(event: JSValue)(using ctx: JSContext): String =
    event match {
      case JSValue.Symbol(id) => s"\u0000symbol:$id"
      case other              => NodeHelpers.toStr(other)
    }

  def create()(using ctx: JSContext): JSValue = {
    val emitterPrototype = JSObject(prototype = ctx.objectPrototype)
    var emitterCtorRef: NativeConstructor = null

    def strip(args: Array[JSValue], receiver: JSObject): Array[JSValue] =
      NodeHelpers.stripReceiver(args, receiver)

    def method(name: String, arity: Int)(
        impl: (EventStore, JSValue, Array[JSValue], JSContext) => JSValue
    ): NativeFunction = {
      val fn = NativeFunction(
        name = name,
        length = arity,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val thisValue = args.headOption.getOrElse(JSValue.Undefined)
          storeOf(thisValue) match {
            case Some(store) =>
              impl(store, thisValue, args.drop(1), callCtx)
            case None =>
              // EventEmitter methods lazily initialize the listener store, so
              // mixing the prototype into a plain object (express's
              // `mixin(app, EventEmitter.prototype)`) works like Node.
              thisValue match {
                case JSValue.Object(_) | _: JSValue.Function |
                    JSValue.Native(_) =>
                  initializeEmitter(thisValue)
                  storeOf(thisValue) match {
                    case Some(store) =>
                      impl(store, thisValue, args.drop(1), callCtx)
                    case None =>
                      NodeHelpers.throwCoded(
                        "TypeError",
                        "The \"this\" argument must be an instance of EventEmitter",
                        "ERR_INVALID_THIS"
                      )
                  }
                case _ =>
                  NodeHelpers.throwCoded(
                    "TypeError",
                    "The \"this\" argument must be an instance of EventEmitter",
                    "ERR_INVALID_THIS"
                  )
              }
          }
        }
      )
      emitterPrototype.defineProperty(
        name,
        JSValue.Native(fn),
        enumerable = false,
        writable = true,
        configurable = true
      )
      fn
    }

    def addListener(
        store: EventStore,
        thisValue: JSValue,
        args: Array[JSValue],
        prepend: Boolean,
        once: Boolean
    )(using ctx: JSContext): JSValue = {
      val event = args.headOption.getOrElse(JSValue.Undefined)
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      if !BuiltinHelpers.isCallable(listener) then
        NodeHelpers.throwCoded(
          "TypeError",
          "The \"listener\" argument must be of type function",
          "ERR_INVALID_ARG_TYPE"
        )
      val key = keyOf(event)(using ctx)
      val entry =
        if once then {
          // The wrapper removes itself before invoking the original.
          val wrapper = NativeFunction(
            name = "onceWrapper",
            length = 0,
            impl = (callArgs, callCtx) => {
              given JSContext = callCtx
              val bucket = store.listeners.get(key)
              bucket.foreach { buffer =>
                val index = buffer.indexWhere(_.original == listener)
                if index >= 0 then buffer.remove(index)
              }
              // callArgs(0) is the emitter receiver added by dispatch.
              BuiltinHelpers.callFunctionWithThis(
                listener,
                thisValue,
                callArgs.drop(1)
              )
            }
          )
          new EventEntry(listener, JSValue.Native(wrapper))
        } else new EventEntry(listener, listener)
      val bucket = store.listeners.getOrElseUpdate(
        key,
        mutable.ArrayBuffer.empty
      )
      if prepend then bucket.prepend(entry) else bucket += entry
      thisValue
    }

    method("on", 2)((store, thisValue, args, c) =>
      addListener(store, thisValue, args, prepend = false, once = false)(using c)
    )
    method("addListener", 2)((store, thisValue, args, c) =>
      addListener(store, thisValue, args, prepend = false, once = false)(using c)
    )
    method("once", 2)((store, thisValue, args, c) =>
      addListener(store, thisValue, args, prepend = false, once = true)(using c)
    )
    method("prependListener", 2)((store, thisValue, args, c) =>
      addListener(store, thisValue, args, prepend = true, once = false)(using c)
    )
    method("prependOnceListener", 2)((store, thisValue, args, c) =>
      addListener(store, thisValue, args, prepend = true, once = true)(using c)
    )

    def removeListenerImpl(
        store: EventStore,
        thisValue: JSValue,
        args: Array[JSValue]
    )(using ctx: JSContext): JSValue = {
      val event = args.headOption.getOrElse(JSValue.Undefined)
      val listener = args.lift(1).getOrElse(JSValue.Undefined)
      val key = keyOf(event)(using ctx)
      store.listeners.get(key).foreach { bucket =>
        val index = bucket.indexWhere(entry =>
          NodeHelpers.sameValue(entry.original, listener)
        )
        if index >= 0 then bucket.remove(index)
      }
      thisValue
    }

    method("removeListener", 2)((store, thisValue, args, c) =>
      removeListenerImpl(store, thisValue, args)(using c)
    )
    method("off", 2)((store, thisValue, args, c) =>
      removeListenerImpl(store, thisValue, args)(using c)
    )

    method("removeAllListeners", 1)((store, thisValue, args, c) => {
      given JSContext = c
      args.headOption match {
        case Some(JSValue.Undefined) | None => store.listeners.clear()
        case Some(event)                    => store.listeners.remove(keyOf(event))
      }
      thisValue
    })

    method("emit", 1)((store, thisValue, args, c) => {
      given JSContext = c
      val event = args.headOption.getOrElse(JSValue.Undefined)
      val key = keyOf(event)(using c)
      val rest = args.drop(1)
      val entries = store.listeners.get(key).map(_.toArray).getOrElse(Array.empty[EventEntry])
      if entries.isEmpty then {
        if key == "error" then {
          val error =
            args.lift(1).getOrElse(
              c.createError("Error", "Unhandled error event")
            )
          throw new quickjs.runtime.JSException(error)
        }
        JSValue.Bool(false)
      } else {
        entries.foreach { entry =>
          BuiltinHelpers.callFunctionWithThis(entry.wrapper, thisValue, rest)
        }
        JSValue.Bool(true)
      }
    })

    method("listenerCount", 2)((store, thisValue, args, c) => {
      given JSContext = c
      val event = args.headOption.getOrElse(JSValue.Undefined)
      JSValue.fromInt(
        store.listeners.get(keyOf(event)).map(_.length).getOrElse(0)
      )
    })

    method("listeners", 2)((store, thisValue, args, c) => {
      given JSContext = c
      val event = args.headOption.getOrElse(JSValue.Undefined)
      val arr = JSArray.empty()
      store.listeners.get(keyOf(event)).foreach(_.foreach(e => arr.push(e.original)))
      JSValue.JSArrayVal(arr)
    })

    method("rawListeners", 2)((store, thisValue, args, c) => {
      given JSContext = c
      val event = args.headOption.getOrElse(JSValue.Undefined)
      val arr = JSArray.empty()
      store.listeners.get(keyOf(event)).foreach(_.foreach(e => arr.push(e.wrapper)))
      JSValue.JSArrayVal(arr)
    })

    method("eventNames", 0)((store, thisValue, args, c) => {
      val arr = JSArray.empty()
      store.listeners.keys.foreach { key =>
        if key.startsWith("\u0000symbol:") then
          arr.push(JSValue.Symbol(key.stripPrefix("\u0000symbol:").toInt))
        else arr.push(JSValue.fromString(key))
      }
      JSValue.JSArrayVal(arr)
    })

    method("setMaxListeners", 1)((store, thisValue, args, c) => {
      given JSContext = c
      store.maxListeners =
        args.headOption.map(v => NodeHelpers.toNumber(v).toInt).getOrElse(10)
      thisValue
    })
    method("getMaxListeners", 0)((store, thisValue, args, c) =>
      JSValue.fromInt(store.maxListeners)
    )

    def initializeEmitter(value: JSValue)(using ctx: JSContext): JSValue = {
      // Function objects can also receive EventEmitter methods (express's
      // application is a function with the prototype mixed in).
      objectOf(value).foreach { obj =>
        if obj.getOwnPropertyRaw("__events").isEmpty then
          obj.initProperty(
            "__events",
            JSValue.Native(new EventStore),
            enumerable = false,
            writable = false,
            configurable = false
          )
      }
      value
    }

    def makeEmitter()(using ctx: JSContext): JSValue = {
      val obj = JSObject(prototype = emitterPrototype)
      initializeEmitter(JSValue.Object(obj))
    }

    val emitterCtor = NativeConstructor(
      name = "EventEmitter",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        NodeHelpers.throwCoded(
          "TypeError",
          "Class constructor EventEmitter cannot be invoked without 'new'",
          "ERR_CONSTRUCT_CALL_REQUIRED"
        )
      },
      constructImpl = (_, callCtx) => {
        given JSContext = callCtx
        makeEmitter()
      },
      prototype = emitterPrototype,
      superInitImpl = Some((thisValue, _, initCtx) => {
        given JSContext = initCtx
        initializeEmitter(thisValue)
      })
    )
    emitterCtorRef = emitterCtor
    BuiltinHelpers.initConstructor(emitterCtor, length = 0)

    def stripCtor(args: Array[JSValue]): Array[JSValue] =
      args match {
        case Array(JSValue.Native(nc: NativeConstructor), rest @ _*) if
            nc.asInstanceOf[AnyRef].eq(emitterCtor.asInstanceOf[AnyRef]) =>
          rest.toArray
        case Array(JSValue.Object(obj), rest @ _*) if obj eq emitterCtor.funcObj =>
          rest.toArray
        case _ => args
      }

    emitterCtor.funcObj.defineProperty(
      "EventEmitter",
      JSValue.Native(emitterCtor),
      enumerable = true,
      writable = true,
      configurable = true
    )
    emitterCtor.funcObj.defineProperty(
      "defaultMaxListeners",
      JSValue.fromInt(10),
      enumerable = true,
      writable = true,
      configurable = true
    )
    emitterCtor.funcObj.defineProperty(
      "once",
      JSValue.Native(
        NativeFunction(
          name = "once",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = stripCtor(args)
            val emitter = rest.headOption.getOrElse(JSValue.Undefined)
            val event = rest.lift(1).getOrElse(JSValue.Undefined)
            val promiseCtor = callCtx.global.get("Promise") match {
              case JSValue.Native(nc: NativeConstructor) => nc
              case _ => JSValue.Undefined
            }
            val executor = NativeFunction(
              name = "executor",
              length = 2,
              impl = (execArgs, execCtx) => {
                given JSContext = execCtx
                val resolve = execArgs.lift(1).getOrElse(JSValue.Undefined)
                val reject = execArgs.lift(2).getOrElse(JSValue.Undefined)
                val onEvent = NativeFunction(
                  name = "onEvent",
                  length = 0,
                  impl = (eventArgs, eCtx) =>
                    BuiltinHelpers.callFunctionWithThis(
                      resolve,
                      JSValue.Undefined,
                      Array(JSValue.JSArrayVal(arrayOf(eventArgs.drop(1))))
                    )
                )
                val onError = NativeFunction(
                  name = "onError",
                  length = 1,
                  impl = (errorArgs, eCtx) =>
                    BuiltinHelpers.callFunctionWithThis(
                      reject,
                      JSValue.Undefined,
                      Array(errorArgs.lastOption.getOrElse(JSValue.Undefined))
                    )
                )
                val onFn = BuiltinHelpers.getPropertyWithGetter(emitter, "on")
                BuiltinHelpers.callFunctionWithThis(onFn, emitter, Array(event, JSValue.Native(onEvent)))
                BuiltinHelpers.callFunctionWithThis(onFn, emitter, Array(JSValue.fromString("error"), JSValue.Native(onError)))
                JSValue.Undefined
              }
            )
            promiseCtor match {
              case nc: NativeConstructor =>
                nc.construct(Array(JSValue.Native(executor)))
              case _ => JSValue.Undefined
            }
          }
        )
      ),
      enumerable = true,
      writable = true,
      configurable = true
    )
    emitterCtor.funcObj.defineProperty(
      "listenerCount",
      JSValue.Native(
        NativeFunction(
          name = "listenerCount",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = stripCtor(args)
            val emitter = rest.headOption.getOrElse(JSValue.Undefined)
            val event = rest.lift(1).getOrElse(JSValue.Undefined)
            storeOf(emitter) match {
              case Some(store) => JSValue.fromInt(store.listeners.get(keyOf(event)).map(_.length).getOrElse(0))
              case None        => JSValue.fromInt(0)
            }
          }
        )
      ),
      enumerable = true,
      writable = true,
      configurable = true
    )

    JSValue.Native(emitterCtor)
  }

  private def arrayOf(values: Array[JSValue]): JSArray = {
    val arr = JSArray.empty()
    values.foreach(arr.push)
    arr
  }
}
