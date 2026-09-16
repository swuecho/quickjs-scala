package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.{JSArray, JSObject}

import scala.collection.mutable

/** A small Node-style event emitter installed on plain objects.
  *
  * Used by host modules that need `on`/`once`/`emit`/`removeListener` without
  * pulling in the full `events` module implementation (`readline`, `repl`,
  * child processes, stdio).
  */
final class JsEmitter {

  private val listeners =
    mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[JSValue]]

  def add(event: String, listener: JSValue): Unit =
    if BuiltinHelpers.isCallable(listener) then
      listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty) += listener

  def prepend(event: String, listener: JSValue): Unit =
    if BuiltinHelpers.isCallable(listener) then
      listeners.getOrElseUpdate(event, mutable.ArrayBuffer.empty)
        .prepend(listener)

  def remove(event: String, listener: JSValue): Unit =
    listeners.get(event).foreach { list =>
      val index = list.indexWhere(l => NodeHelpers.sameValue(l, listener))
      if index >= 0 then list.remove(index)
    }

  def removeAll(event: String): Unit =
    if event.isEmpty then listeners.clear() else listeners.remove(event)

  def listenerCount(event: String): Int =
    listeners.get(event).map(_.length).getOrElse(0)

  def eventNames(): Seq[String] = listeners.keys.toSeq

  def emit(event: String, args: Array[JSValue])(using
      ctx: JSContext
  ): Boolean = {
    val handlers =
      listeners.getOrElse(event, mutable.ArrayBuffer.empty).toSeq
    handlers.foreach(fn =>
      BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, args)
    )
    handlers.nonEmpty
  }

  /** Install emitter methods on `target`; methods return `target` where Node
    * does (so chaining works).
    */
  def install(target: JSObject)(using JSContext): Unit = {
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
    set("prependListener", 2) { (args, _) =>
      prepend(eventName(args), args.lift(1).getOrElse(JSValue.Undefined))
      JSValue.Object(target)
    }
    set("once", 2) { (args, callCtx) =>
      given JSContext = callCtx
      val event = eventName(args)
      val fn = args.lift(1).getOrElse(JSValue.Undefined)
      lazy val wrapper: NativeFunction = NativeFunction(
        name = event,
        length = 0,
        impl = (callArgs, innerCtx) => {
          given JSContext = innerCtx
          remove(event, JSValue.Native(wrapper))
          BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, callArgs)
        }
      )
      add(event, JSValue.Native(wrapper))
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
      JSValue.fromInt(listenerCount(eventName(args)))
    }
    set("eventNames", 0) { (_, _) =>
      val array = JSArray.empty()
      eventNames().foreach(name => array.push(JSValue.fromString(name)))
      JSValue.JSArrayVal(array)
    }
  }
}
