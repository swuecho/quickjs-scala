package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext

/** Error built-in: Error and native error constructors.
  */
object ErrorBuiltins {
  import quickjs.objmodel.JSObject

  def initialize(ctx: JSContext): Unit = {
    def defineCause(obj: JSObject, args: Array[JSValue], index: Int)(using
        JSContext
    ): Unit =
      args.lift(index) match {
        case Some(JSValue.Object(options)) if options.hasProperty("cause") =>
          obj.set("cause", options.get("cause"))
        case _ => ()
      }

    def buildError(
        proto: JSObject,
        name: String,
        args: Array[JSValue],
        messageIndex: Int = 0
    )(using
        JSContext
    ): JSValue = {
      val obj = JSObject(prototype = proto, extensible = true)
      obj.set("name", JSValue.fromString(name))
      args.lift(messageIndex) match {
        case Some(value) if value != JSValue.Undefined =>
          obj.set("message", JSValue.fromString(BuiltinHelpers.toJSString(value)))
        case _ => ()
      }
      defineCause(obj, args, messageIndex + 1)
      ctx.attachStack(obj, skipFrames = 1)
      JSValue.Object(obj)
    }

    def arrayFrom(value: JSValue)(using JSContext): JSValue =
      ctx.global.get("Array") match {
        case JSValue.Native(arrayCtor: NativeConstructor) =>
          arrayCtor.funcObj.get("from") match {
            case from if from != JSValue.Undefined =>
              BuiltinHelpers.callFunctionWithThis(
                from,
                JSValue.Native(arrayCtor),
                Array(value)
              )
            case _ =>
              ctx.throwTypeError("Array.from is not available")
          }
        case _ =>
          ctx.throwTypeError("Array constructor is not available")
      }

    def buildAggregateError(proto: JSObject, args: Array[JSValue])(using
        JSContext
    ): JSValue = {
      val errorList = arrayFrom(args.headOption.getOrElse(JSValue.Undefined))
      buildError(proto, "AggregateError", args, messageIndex = 1) match {
        case JSValue.Object(obj) =>
          obj.set("errors", errorList)
          JSValue.Object(obj)
        case other => other
      }
    }

    def defineNativeError(
        name: String,
        parentPrototype: JSObject,
        length: Int = 1,
        builder: Option[(JSObject, Array[JSValue], JSContext) => JSValue] =
          None
    )(using JSContext): JSObject = {
      val prototype =
        JSObject(prototype = parentPrototype, extensible = true)
      prototype.set("name", JSValue.fromString(name))
      val errorBuilder =
        builder.getOrElse { (proto, args, callCtx) =>
          given JSContext = callCtx
          buildError(proto, name, args)
        }
      val constructor = NativeConstructor(
        name = name,
        callImpl = (args, callCtx) => errorBuilder(prototype, args, callCtx),
        constructImpl = (args, callCtx) =>
          errorBuilder(prototype, args, callCtx),
        prototype = prototype
      )
      BuiltinHelpers.initConstructor(constructor, length = length)
      prototype.defineProperty(
        "constructor",
        JSValue.Native(constructor),
        enumerable = false
      )(using ctx)
      ctx.global.set(name, JSValue.Native(constructor))
      prototype
    }

    given JSContext = ctx

    val errorPrototype =
      JSObject(prototype = ctx.objectPrototype, extensible = true)
    errorPrototype.set("name", JSValue.fromString("Error"))
    val errorConstructor = quickjs.value.NativeConstructor(
      name = "Error",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(errorPrototype, "Error", args)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(errorPrototype, "Error", args)
      ,
      prototype = errorPrototype
    )
    BuiltinHelpers.initConstructor(errorConstructor, length = 1)
    errorPrototype.defineProperty(
      "constructor",
      JSValue.Native(errorConstructor),
      enumerable = false
    )(using ctx)
    ctx.global.set("Error", JSValue.Native(errorConstructor))

    defineNativeError("EvalError", errorPrototype)
    defineNativeError("RangeError", errorPrototype)
    defineNativeError("ReferenceError", errorPrototype)
    defineNativeError("SyntaxError", errorPrototype)
    defineNativeError("TypeError", errorPrototype)
    defineNativeError("URIError", errorPrototype)
    defineNativeError(
      "AggregateError",
      errorPrototype,
      length = 2,
      builder = Some((proto, args, callCtx) =>
        given JSContext = callCtx
        buildAggregateError(proto, args)
      )
    )

    val errorPrototypeToString = NativeFunction(
      name = "toString",
      length = 0,
      impl = (args, ctx) =>
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        thisValue match {
          case JSValue.Object(obj) =>
            val nameValue = obj.get("name")(using ctx)
            val nameStr =
              if nameValue == JSValue.Undefined then "Error"
              else nameValue.toString
            val msgValue = obj.get("message")(using ctx)
            val msgStr =
              if msgValue == JSValue.Undefined then "" else msgValue.toString
            if nameStr.nonEmpty && msgStr.nonEmpty then
              JSValue.fromString(s"$nameStr: $msgStr")
            else if nameStr.nonEmpty then JSValue.fromString(nameStr)
            else JSValue.fromString(msgStr)
          case _ =>
            ctx.throwTypeError("Error.prototype.toString called on non-object")
        }
    )
    errorPrototype.defineProperty(
      "toString",
      JSValue.Native(errorPrototypeToString),
      enumerable = false
    )(using ctx)
  }
}
