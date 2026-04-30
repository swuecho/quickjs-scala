package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext

/** RegExp built-in: RegExp constructor, RegExp.prototype.exec, test, toString. */
object RegExpBuiltins:
  import quickjs.objmodel.JSObject

  private final case class RegExpData(
    pattern: String, flags: String, global: Boolean, ignoreCase: Boolean,
    multiline: Boolean, dotAll: Boolean, unicode: Boolean, sticky: Boolean,
    regex: java.util.regex.Pattern
  )

  private def parseRegExpFlags(flags: String)(using ctx: JSContext) = BuiltinHelpers.parseRegExpFlags(flags)
  private def getRegExpData(value: JSValue)(using ctx: JSContext) = BuiltinHelpers.getRegExpData(value)

  def initialize(ctx: JSContext): Unit =
    val regexpPrototype = JSObject(prototype = ctx.objectPrototype, extensible = true)
    given JSContext = ctx

    def buildRegExp(patternValue: JSValue, flagsValue: JSValue): JSValue =
      val (pattern, flags) =
        getRegExpData(patternValue) match
          case Some((_, data)) =>
            if flagsValue == JSValue.Undefined then
              return patternValue
            else
              (data.pattern, flagsValue.toString)
          case None =>
            (patternValue.toString, if flagsValue == JSValue.Undefined then "" else flagsValue.toString)
      val (_, global, ignoreCase, multiline, dotAll, unicode, sticky) = parseRegExpFlags(flags)
      val obj = JSObject(prototype = regexpPrototype, extensible = true)
      obj.defineProperty("__regexpPattern", JSValue.fromString(pattern), enumerable = false)(using ctx)
      obj.defineProperty("__regexpFlags", JSValue.fromString(flags), enumerable = false)(using ctx)
      obj.defineProperty("source", JSValue.fromString(pattern), enumerable = false)(using ctx)
      obj.defineProperty("flags", JSValue.fromString(flags), enumerable = false)(using ctx)
      obj.defineProperty("global", JSValue.fromBoolean(global), enumerable = false)(using ctx)
      obj.defineProperty("ignoreCase", JSValue.fromBoolean(ignoreCase), enumerable = false)(using ctx)
      obj.defineProperty("multiline", JSValue.fromBoolean(multiline), enumerable = false)(using ctx)
      obj.defineProperty("dotAll", JSValue.fromBoolean(dotAll), enumerable = false)(using ctx)
      obj.defineProperty("unicode", JSValue.fromBoolean(unicode), enumerable = false)(using ctx)
      obj.defineProperty("sticky", JSValue.fromBoolean(sticky), enumerable = false)(using ctx)
      obj.defineProperty("lastIndex", JSValue.fromInt(0), enumerable = false, writable = true, configurable = false)(using ctx)
      JSValue.Object(obj)

    val regexpConstructor = quickjs.value.NativeConstructor(
      name = "RegExp",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        val pattern = if args.nonEmpty then args(0) else JSValue.fromString("")
        val flags = if args.length > 1 then args(1) else JSValue.Undefined
        buildRegExp(pattern, flags),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val pattern = if args.nonEmpty then args(0) else JSValue.fromString("")
        val flags = if args.length > 1 then args(1) else JSValue.Undefined
        buildRegExp(pattern, flags),
      prototype = regexpPrototype
    )
    BuiltinHelpers.initConstructor(regexpConstructor, length = 2)

    val regexpExec = NativeFunction(
      name = "exec",
      impl = (args, ctx) =>
        given JSContext = ctx
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        val input = if args.length > 1 then args(1).toString else ""
        getRegExpData(thisValue) match
          case Some((obj, data)) =>
            val start =
              if data.global then math.max(0, obj.get("lastIndex")(using ctx).toNumber.toInt)
              else 0
            val matcher = data.regex.matcher(input)
            if matcher.find(start) then
              if data.global then
                obj.set("lastIndex", JSValue.fromInt(matcher.end()))(using ctx)
              val arr = quickjs.objmodel.JSArray.empty()
              var i = 0
              while i <= matcher.groupCount() do
                arr.push(JSValue.fromString(matcher.group(i)))
                i += 1
              arr.setProperty("index", JSValue.fromInt(matcher.start()))
              arr.setProperty("input", JSValue.fromString(input))
              arr.setProperty("groups", JSValue.Undefined)
              JSValue.JSArrayVal(arr)
            else
              if data.global then obj.set("lastIndex", JSValue.fromInt(0))(using ctx)
              JSValue.Null
          case None =>
            ctx.throwTypeError("RegExp.prototype.exec called on non-RegExp")
    )

    val regexpTest = NativeFunction(
      name = "test",
      impl = (args, ctx) =>
        given JSContext = ctx
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        val input = if args.length > 1 then args(1).toString else ""
        getRegExpData(thisValue) match
          case Some((obj, data)) =>
            val start =
              if data.global then math.max(0, obj.get("lastIndex")(using ctx).toNumber.toInt)
              else 0
            val matcher = data.regex.matcher(input)
            val matched = matcher.find(start)
            if matched && data.global then
              obj.set("lastIndex", JSValue.fromInt(matcher.end()))(using ctx)
            else if !matched && data.global then
              obj.set("lastIndex", JSValue.fromInt(0))(using ctx)
            JSValue.fromBoolean(matched)
          case None =>
            ctx.throwTypeError("RegExp.prototype.test called on non-RegExp")
    )

    val regexpToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        getRegExpData(thisValue) match
          case Some((_, data)) =>
            JSValue.fromString(s"/${data.pattern}/${data.flags}")
          case None =>
            ctx.throwTypeError("RegExp.prototype.toString called on non-RegExp")
    )

    regexpPrototype.defineProperty("exec", JSValue.Native(regexpExec), enumerable = false)
    regexpPrototype.defineProperty("test", JSValue.Native(regexpTest), enumerable = false)
    regexpPrototype.defineProperty("toString", JSValue.Native(regexpToString), enumerable = false)
    regexpPrototype.set("constructor", JSValue.Native(regexpConstructor))
    ctx.global.set("RegExp", JSValue.Native(regexpConstructor))
