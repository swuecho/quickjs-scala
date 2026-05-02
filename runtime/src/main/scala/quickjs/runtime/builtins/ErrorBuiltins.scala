package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext

/** Error built-in: Error, TypeError, ReferenceError, SyntaxError, RangeError
  * constructors.
  */
object ErrorBuiltins {
  import quickjs.objmodel.JSObject

  def initialize(ctx: JSContext): Unit = {
    def buildError(proto: JSObject, name: String, args: Array[JSValue])(using
        JSContext
    ): JSValue = {
      val obj = JSObject(prototype = proto, extensible = true)
      obj.set("name", JSValue.fromString(name))
      if args.nonEmpty then obj.set("message", args(0))
      ctx.attachStack(obj, skipFrames = 1)
      JSValue.Object(obj)
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

    val typeErrorPrototype =
      JSObject(prototype = errorPrototype, extensible = true)
    typeErrorPrototype.set("name", JSValue.fromString("TypeError"))
    val typeErrorConstructor = quickjs.value.NativeConstructor(
      name = "TypeError",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(typeErrorPrototype, "TypeError", args)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(typeErrorPrototype, "TypeError", args)
      ,
      prototype = typeErrorPrototype
    )
    BuiltinHelpers.initConstructor(typeErrorConstructor, length = 1)
    typeErrorPrototype.defineProperty(
      "constructor",
      JSValue.Native(typeErrorConstructor),
      enumerable = false
    )(using ctx)
    ctx.global.set("TypeError", JSValue.Native(typeErrorConstructor))

    val referenceErrorPrototype =
      JSObject(prototype = errorPrototype, extensible = true)
    referenceErrorPrototype.set("name", JSValue.fromString("ReferenceError"))
    val referenceErrorConstructor = quickjs.value.NativeConstructor(
      name = "ReferenceError",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(referenceErrorPrototype, "ReferenceError", args)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(referenceErrorPrototype, "ReferenceError", args)
      ,
      prototype = referenceErrorPrototype
    )
    BuiltinHelpers.initConstructor(referenceErrorConstructor, length = 1)
    referenceErrorPrototype.defineProperty(
      "constructor",
      JSValue.Native(referenceErrorConstructor),
      enumerable = false
    )(using ctx)
    ctx.global.set("ReferenceError", JSValue.Native(referenceErrorConstructor))

    val syntaxErrorPrototype =
      JSObject(prototype = errorPrototype, extensible = true)
    syntaxErrorPrototype.set("name", JSValue.fromString("SyntaxError"))
    val syntaxErrorConstructor = quickjs.value.NativeConstructor(
      name = "SyntaxError",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(syntaxErrorPrototype, "SyntaxError", args)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(syntaxErrorPrototype, "SyntaxError", args)
      ,
      prototype = syntaxErrorPrototype
    )
    BuiltinHelpers.initConstructor(syntaxErrorConstructor, length = 1)
    syntaxErrorPrototype.defineProperty(
      "constructor",
      JSValue.Native(syntaxErrorConstructor),
      enumerable = false
    )(using ctx)
    ctx.global.set("SyntaxError", JSValue.Native(syntaxErrorConstructor))

    val rangeErrorPrototype =
      JSObject(prototype = errorPrototype, extensible = true)
    rangeErrorPrototype.set("name", JSValue.fromString("RangeError"))
    val rangeErrorConstructor = quickjs.value.NativeConstructor(
      name = "RangeError",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(rangeErrorPrototype, "RangeError", args)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(rangeErrorPrototype, "RangeError", args)
      ,
      prototype = rangeErrorPrototype
    )
    BuiltinHelpers.initConstructor(rangeErrorConstructor, length = 1)
    rangeErrorPrototype.defineProperty(
      "constructor",
      JSValue.Native(rangeErrorConstructor),
      enumerable = false
    )(using ctx)
    ctx.global.set("RangeError", JSValue.Native(rangeErrorConstructor))

    val errorPrototypeToString = NativeFunction(
      name = "toString",
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
