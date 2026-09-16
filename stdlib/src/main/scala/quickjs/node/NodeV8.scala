package quickjs.node

import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.value.{JSValue, NativeFunction}
import quickjs.objmodel.JSObject

/** Minimal `node:v8` module. `serialize`/`deserialize` use a JSON envelope so
  * values survive an in-process round-trip (execa's advanced IPC
  * serialization); heap statistics are stubbed. */
object NodeV8 {

  def create()(using ctx: JSContext): JSValue = {
    val v8 = JSObject(prototype = ctx.objectPrototype)

    def strip(args: Array[JSValue]): Array[JSValue] =
      if args.nonEmpty && (args(0) match {
            case JSValue.Object(o) => o eq v8
            case _                 => false
          })
      then args.drop(1)
      else args

    def jsonFn(name: String, callCtx: JSContext) =
      BuiltinHelpers.getPropertyWithGetter(
        callCtx.global.get("JSON"),
        name
      )

    def serializeImpl(args: Array[JSValue], callCtx: JSContext): JSValue = {
      given JSContext = callCtx
      val value = strip(args).headOption.getOrElse(JSValue.Undefined)
      val envelope = JSObject()
      envelope.set("v", value)
      val json = BuiltinHelpers.callFunctionWithThis(
        jsonFn("stringify", callCtx),
        callCtx.global.get("JSON"),
        Array(JSValue.Object(envelope))
      )
      val text =
        if json == JSValue.Undefined then ""
        else BuiltinHelpers.toJSString(json)
      val bufferCtor = callCtx.global.get("Buffer")
      BuiltinHelpers.callFunctionWithThis(
        BuiltinHelpers.getPropertyWithGetter(bufferCtor, "from"),
        bufferCtor,
        Array(JSValue.fromString(text))
      )
    }

    def deserializeImpl(args: Array[JSValue], callCtx: JSContext): JSValue = {
      given JSContext = callCtx
      val text = strip(args).headOption
        .flatMap(NodeBuffer.bytesOfValue)
        .map(bytes => new String(bytes, "UTF-8"))
        .getOrElse(callCtx.throwTypeError("deserialize expects a Buffer"))
      val json = BuiltinHelpers.callFunctionWithThis(
        jsonFn("parse", callCtx),
        callCtx.global.get("JSON"),
        Array(JSValue.fromString(text))
      )
      json match {
        case JSValue.Object(obj) => obj.get("v")
        case _                   => JSValue.Undefined
      }
    }

    def fn(name: String, arity: Int)(
        impl: (Array[JSValue], JSContext) => JSValue
    ): Unit =
      v8.set(
        name,
        JSValue.Native(NativeFunction(name = name, length = arity, impl = impl))
      )

    fn("serialize", 1)(serializeImpl)
    fn("deserialize", 1)(deserializeImpl)
    fn("isUtf8", 1)((args, callCtx) =>
      given JSContext = callCtx
      val bytes = strip(args).headOption.flatMap(NodeBuffer.bytesOfValue)
      val result = bytes.forall(b =>
        java.util.Arrays.equals(
          new String(b, "UTF-8").getBytes("UTF-8"),
          b
        )
      )
      JSValue.Bool(result)
    )
    fn("getHeapStatistics", 0)((_, callCtx) =>
      given JSContext = callCtx
      val stats = JSObject(prototype = callCtx.objectPrototype)
      Seq(
        "total_heap_size" -> 0.0,
        "total_heap_size_executable" -> 0.0,
        "total_physical_size" -> 0.0,
        "total_available_size" -> 0.0,
        "used_heap_size" -> 0.0,
        "heap_size_limit" -> 0.0,
        "malloced_memory" -> 0.0,
        "peak_malloced_memory" -> 0.0,
        "does_zap_garbage" -> 0.0,
        "number_of_native_contexts" -> 1.0,
        "number_of_detached_contexts" -> 0.0
      ).foreach((name, value) => stats.set(name, JSValue.fromDouble(value)))
      JSValue.Object(stats)
    )
    fn("getHeapSpaceStatistics", 0)((_, _) =>
      JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
    )
    fn("cachedDataVersionTag", 0)((_, _) => JSValue.fromInt(0))
    fn("setFlagsFromString", 1)((_, _) => JSValue.Undefined)
    fn("takeCoverage", 0)((_, _) => JSValue.Undefined)
    fn("stopCoverage", 0)((_, _) => JSValue.Undefined)
    fn("writeHeapSnapshot", 1)((_, _) => JSValue.Undefined)

    JSValue.Object(v8)
  }
}
