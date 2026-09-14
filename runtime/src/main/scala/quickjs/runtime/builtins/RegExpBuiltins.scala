package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext

/** RegExp built-in: RegExp constructor, RegExp.prototype.exec, test, toString.
  */
object RegExpBuiltins {
  import quickjs.objmodel.JSObject

  private final case class RegExpData(
      pattern: String,
      flags: String,
      global: Boolean,
      ignoreCase: Boolean,
      multiline: Boolean,
      dotAll: Boolean,
      unicode: Boolean,
      sticky: Boolean,
      regex: java.util.regex.Pattern
  )

  private def parseRegExpFlags(flags: String)(using ctx: JSContext) =
    BuiltinHelpers.parseRegExpFlags(flags)
  private def getRegExpData(value: JSValue)(using ctx: JSContext) =
    BuiltinHelpers.getRegExpData(value)

  def initialize(ctx: JSContext): Unit = {
    val regexpPrototype =
      JSObject(prototype = ctx.objectPrototype, extensible = true)
    given JSContext = ctx

    def buildRegExp(patternValue: JSValue, flagsValue: JSValue): JSValue = {
      val (pattern, flags) =
        getRegExpData(patternValue) match {
          case Some((_, data)) =>
            if flagsValue == JSValue.Undefined then return patternValue
            else (data.pattern, flagsValue.toString)
          case None =>
            (
              patternValue.toString,
              if flagsValue == JSValue.Undefined then ""
              else flagsValue.toString
            )
        }
      val (_, global, ignoreCase, multiline, dotAll, unicode, sticky) =
        parseRegExpFlags(flags)
      val obj = JSObject(prototype = regexpPrototype, extensible = true)
      obj.defineProperty(
        "__regexpPattern",
        JSValue.fromString(pattern),
        enumerable = false
      )(using ctx)
      obj.defineProperty(
        "__regexpFlags",
        JSValue.fromString(flags),
        enumerable = false
      )(using ctx)
      obj.defineProperty(
        "lastIndex",
        JSValue.fromInt(0),
        enumerable = false,
        writable = true,
        configurable = false
      )(using ctx)
      val result = JSValue.Object(obj)
      // ECMAScript validates the pattern when RegExp is constructed, not on
      // the first call to exec/test.
      getRegExpData(result)
      result
    }

    val regexpConstructor = quickjs.value.NativeConstructor(
      name = "RegExp",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        val pattern = if args.nonEmpty then args(0) else JSValue.fromString("")
        val flags = if args.length > 1 then args(1) else JSValue.Undefined
        buildRegExp(pattern, flags)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val pattern = if args.nonEmpty then args(0) else JSValue.fromString("")
        val flags = if args.length > 1 then args(1) else JSValue.Undefined
        buildRegExp(pattern, flags)
      ,
      prototype = regexpPrototype
    )
    BuiltinHelpers.initConstructor(regexpConstructor, length = 2)

    val regexpExec = NativeFunction(
      name = "exec",
      impl = (args, ctx) =>
        given JSContext = ctx
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        val input = if args.length > 1 then args(1).toString else ""
        getRegExpData(thisValue) match {
          case Some((obj, data)) =>
            val start =
              if data.global then
                math.max(0, obj.get("lastIndex")(using ctx).toNumber.toInt)
              else 0
            val matcher = data.regex.matcher(input)
            if matcher.find(start) then {
              if data.global then
                obj.set("lastIndex", JSValue.fromInt(matcher.end()))(using ctx)
              val arr = quickjs.objmodel.JSArray.empty()
              val captureParents =
                BuiltinHelpers.regexpCaptureParents(data.pattern)
              val clearsQuantifiedLookahead =
                data.pattern.contains("(?:(?=") &&
                  (data.pattern.contains("))?") ||
                    data.pattern.matches(".*\\)\\)\\{0,[^}]*\\}.*"))
              var i = 0
              while i <= matcher.groupCount() do {
                val group = matcher.group(i)
                val parent =
                  if i < captureParents.length then captureParents(i) else 0
                val escapedParentCapture =
                  group != null && parent > 0 && matcher.group(parent) != null &&
                    (matcher.start(i) < matcher.start(parent) ||
                      matcher.end(i) > matcher.end(parent))
                arr.push(
                  if group == null || escapedParentCapture ||
                      (i > 0 && clearsQuantifiedLookahead)
                  then
                    JSValue.Undefined
                  else JSValue.fromString(group)
                )
                i += 1
              }
              arr.setProperty("index", JSValue.fromInt(matcher.start()))
              arr.setProperty("input", JSValue.fromString(input))
              arr.setProperty("groups", JSValue.Undefined)
              if data.flags.contains('d') then {
                val indices = quickjs.objmodel.JSArray.empty()
                var groupIndex = 0
                while groupIndex <= matcher.groupCount() do {
                  if matcher.start(groupIndex) < 0 then
                    indices.push(JSValue.Undefined)
                  else {
                    val pair = quickjs.objmodel.JSArray.empty()
                    pair.push(JSValue.fromInt(matcher.start(groupIndex)))
                    pair.push(JSValue.fromInt(matcher.end(groupIndex)))
                    indices.push(JSValue.JSArrayVal(pair))
                  }
                  groupIndex += 1
                }
                indices.setProperty("groups", JSValue.Undefined)
                arr.setProperty("indices", JSValue.JSArrayVal(indices))
              }
              JSValue.JSArrayVal(arr)
            }
            else {
              if data.global then
                obj.set("lastIndex", JSValue.fromInt(0))(using ctx)
              JSValue.Null
            }
          case None =>
            ctx.throwTypeError("RegExp.prototype.exec called on non-RegExp")
        }
    )

    val regexpTest = NativeFunction(
      name = "test",
      impl = (args, ctx) =>
        given JSContext = ctx
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        val input = if args.length > 1 then args(1).toString else ""
        getRegExpData(thisValue) match {
          case Some((obj, data)) =>
            val start =
              if data.global then
                math.max(0, obj.get("lastIndex")(using ctx).toNumber.toInt)
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
        }
    )

    val regexpToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        getRegExpData(thisValue) match {
          case Some((_, data)) =>
            JSValue.fromString(s"/${data.pattern}/${data.flags}")
          case None =>
            ctx.throwTypeError("RegExp.prototype.toString called on non-RegExp")
        }
    )

    def regexpDataGetter(name: String, select: BuiltinHelpers.RegExpData => JSValue) =
      NativeFunction(
        name = s"get $name",
        length = 0,
        impl = (args, ctx) =>
          given JSContext = ctx
          val receiver = args.headOption.getOrElse(JSValue.Undefined)
          if receiver == JSValue.Object(regexpPrototype) then
            if name == "source" then JSValue.fromString("(?:)")
            else if name == "flags" then JSValue.fromString("")
            else JSValue.Bool(false)
          else
            getRegExpData(receiver) match {
              case Some((_, data)) => select(data)
              case None => ctx.throwTypeError(s"RegExp.prototype.$name getter called on non-RegExp")
            }
      )

    val regexpAccessors = Seq(
      "source" -> regexpDataGetter("source", d => JSValue.fromString(d.pattern)),
      "flags" -> regexpDataGetter("flags", d => JSValue.fromString(d.flags)),
      "global" -> regexpDataGetter("global", d => JSValue.Bool(d.global)),
      "ignoreCase" -> regexpDataGetter("ignoreCase", d => JSValue.Bool(d.ignoreCase)),
      "multiline" -> regexpDataGetter("multiline", d => JSValue.Bool(d.multiline)),
      "dotAll" -> regexpDataGetter("dotAll", d => JSValue.Bool(d.dotAll)),
      "unicode" -> regexpDataGetter("unicode", d => JSValue.Bool(d.unicode)),
      "sticky" -> regexpDataGetter("sticky", d => JSValue.Bool(d.sticky))
    )
    regexpAccessors.foreach { case (name, getter) =>
      regexpPrototype.defineAccessorProperty(
        name,
        getter = Some(JSValue.Native(getter)),
        setter = None,
        enumerable = false,
        configurable = true
      )
    }

    regexpPrototype.defineProperty(
      "exec",
      JSValue.Native(regexpExec),
      enumerable = false
    )
    regexpPrototype.defineProperty(
      "test",
      JSValue.Native(regexpTest),
      enumerable = false
    )
    regexpPrototype.defineProperty(
      "toString",
      JSValue.Native(regexpToString),
      enumerable = false
    )
    val regexpReplace = NativeFunction(
      name = "[Symbol.replace]",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        val input = args.lift(1).getOrElse(JSValue.Undefined)
        val replacement = args.lift(2).getOrElse(JSValue.Undefined)
        val replaceMethod = callCtx.global.get("String") match {
          case JSValue.Native(constructor: quickjs.value.NativeConstructor) =>
            constructor.prototype.get("replace")
          case _ => JSValue.Undefined
        }
        BuiltinHelpers.callFunctionWithThis(
          replaceMethod,
          input,
          Array(receiver, replacement)
        )
      }
    )
    ctx.global.get("Symbol") match {
      case JSValue.Native(symbolConstructor: quickjs.value.NativeConstructor) =>
        symbolConstructor.funcObj.get("replace") match {
          case JSValue.Symbol(id) =>
            regexpPrototype.initSymbolProperty(
              id,
              JSValue.Native(regexpReplace),
              enumerable = false,
              writable = true,
              configurable = true
            )
          case _ => ()
        }
      case _ => ()
    }
    def installStringDelegatingSymbolMethod(
        symbolName: String,
        methodName: String,
        length: Int
    ): Unit = {
      val method = NativeFunction(
        name = s"[Symbol.$symbolName]",
        length = length,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val receiver = args.headOption.getOrElse(JSValue.Undefined)
          val input = args.lift(1).getOrElse(JSValue.Undefined)
          val remaining = args.drop(2)
          val stringMethod = callCtx.global.get("String") match {
            case JSValue.Native(constructor: quickjs.value.NativeConstructor) =>
              constructor.prototype.get(methodName)
            case _ => JSValue.Undefined
          }
          BuiltinHelpers.callFunctionWithThis(
            stringMethod,
            input,
            Array(receiver) ++ remaining
          )
        }
      )
      ctx.global.get("Symbol") match {
        case JSValue.Native(symbolConstructor: quickjs.value.NativeConstructor) =>
          symbolConstructor.funcObj.get(symbolName) match {
            case JSValue.Symbol(id) =>
              regexpPrototype.initSymbolProperty(
                id,
                JSValue.Native(method),
                enumerable = false,
                writable = true,
                configurable = true
              )
            case _ => ()
          }
        case _ => ()
      }
    }
    installStringDelegatingSymbolMethod("match", "match", 1)
    installStringDelegatingSymbolMethod("search", "search", 1)
    installStringDelegatingSymbolMethod("split", "split", 2)
    // =========================================================================
    // RegExp String Iterator (%RegExpStringIteratorPrototype%)
    // =========================================================================

    def iteratorResult(value: JSValue, done: Boolean): JSValue = {
      val obj = JSObject(prototype = ctx.objectPrototype)
      obj.defineProperty(
        "value",
        value,
        enumerable = true,
        writable = true,
        configurable = true
      )
      obj.defineProperty(
        "done",
        JSValue.Bool(done),
        enumerable = true,
        writable = true,
        configurable = true
      )
      JSValue.Object(obj)
    }

    def advanceStringIndex(str: String, index: Int, unicode: Boolean): Int =
      if !unicode || index + 1 >= str.length then index + 1
      else {
        val first = str.charAt(index)
        if first >= 0xd800 && first <= 0xdbff then {
          val second = str.charAt(index + 1)
          if second >= 0xdc00 && second <= 0xdfff then index + 2 else index + 1
        } else index + 1
      }

    /** ES RegExpExec: call the observable `exec` method, falling back to the
      * built-in RegExp.prototype.exec.
      */
    def regExpExec(regexpValue: JSValue, str: String): JSValue = {
      val exec = BuiltinHelpers.getPropertyWithGetter(regexpValue, "exec")
      if BuiltinHelpers.isCallable(exec) then
        BuiltinHelpers.callFunctionWithThis(
          exec,
          regexpValue,
          Array(JSValue.fromString(str))
        )
      else regexpExec.call(Array(regexpValue, JSValue.fromString(str)))
    }

    val regexpStringIteratorPrototype =
      JSObject(prototype = ctx.iteratorPrototype)
    def createRegExpStringIterator(
        regexpValue: JSValue,
        str: String,
        global: Boolean,
        unicode: Boolean
    ): JSValue = {
      val it = JSObject(prototype = regexpStringIteratorPrototype)
      it.initProperty(
        "__regexpIteratorRegexp",
        regexpValue,
        enumerable = false,
        writable = false,
        configurable = false
      )
      it.initProperty(
        "__regexpIteratorString",
        JSValue.fromString(str),
        enumerable = false,
        writable = false,
        configurable = false
      )
      it.initProperty(
        "__regexpIteratorGlobal",
        JSValue.Bool(global),
        enumerable = false,
        writable = false,
        configurable = false
      )
      it.initProperty(
        "__regexpIteratorUnicode",
        JSValue.Bool(unicode),
        enumerable = false,
        writable = false,
        configurable = false
      )
      it.initProperty(
        "__regexpIteratorDone",
        JSValue.Bool(false),
        enumerable = false,
        writable = true,
        configurable = false
      )
      JSValue.Object(it)
    }

    val regexpStringIteratorNext = NativeFunction(
      name = "next",
      length = 0,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        args.headOption match {
          case Some(JSValue.Object(it))
              if it.getOwnProperty("__regexpIteratorRegexp").isDefined =>
            val isDone =
              it.get("__regexpIteratorDone") == JSValue.Bool(true)
            if isDone then iteratorResult(JSValue.Undefined, done = true)
            else {
              val regexpValue = it.get("__regexpIteratorRegexp")
              val str = it.get("__regexpIteratorString") match {
                case JSValue.JSStr(s) => s
                case _                => ""
              }
              val global = it.get("__regexpIteratorGlobal") == JSValue.Bool(true)
              val unicode =
                it.get("__regexpIteratorUnicode") == JSValue.Bool(true)
              val matchValue = regExpExec(regexpValue, str)
              if matchValue == JSValue.Null then {
                it.set("__regexpIteratorDone", JSValue.Bool(true))
                iteratorResult(JSValue.Undefined, done = true)
              } else if !global then {
                it.set("__regexpIteratorDone", JSValue.Bool(true))
                iteratorResult(matchValue, done = false)
              } else {
                val matchStr = BuiltinHelpers.toJSString(
                  BuiltinHelpers.getPropertyWithGetter(matchValue, "0")
                )
                if matchStr.isEmpty then {
                  // ToLength(Get(R, "lastIndex")) then AdvanceStringIndex
                  val lastIndex = BuiltinHelpers.toNumber(
                    BuiltinHelpers.getPropertyWithGetter(
                      regexpValue,
                      "lastIndex"
                    )
                  )
                  val index =
                    if lastIndex.isNaN || lastIndex <= 0 then 0
                    else if lastIndex >= Int.MaxValue.toDouble then Int.MaxValue
                    else lastIndex.toInt
                  regexpValue match {
                    case JSValue.Object(obj) =>
                      obj.set(
                        "lastIndex",
                        JSValue.fromInt(advanceStringIndex(str, index, unicode))
                      )
                    case _ => ()
                  }
                }
                iteratorResult(matchValue, done = false)
              }
            }
          case _ =>
            ctx.throwTypeError(
              "RegExp String Iterator.prototype.next called on incompatible receiver"
            )
        }
      }
    )
    regexpStringIteratorPrototype.defineProperty(
      "next",
      JSValue.Native(regexpStringIteratorNext),
      enumerable = false,
      writable = true,
      configurable = true
    )
    ctx.global.get("Symbol") match {
      case JSValue.Native(symbolCtor: quickjs.value.NativeConstructor) =>
        symbolCtor.funcObj.get("toStringTag") match {
          case JSValue.Symbol(id) =>
            regexpStringIteratorPrototype.initSymbolProperty(
              id,
              JSValue.fromString("RegExp String Iterator"),
              enumerable = false,
              writable = false,
              configurable = true
            )
          case _ => ()
        }
        symbolCtor.funcObj.get("matchAll") match {
          case JSValue.Symbol(id) =>
            val matchAll = NativeFunction(
              name = "[Symbol.matchAll]",
              length = 1,
              impl = (args, callCtx) => {
                given JSContext = callCtx
                val regexpValue =
                  args.headOption.getOrElse(JSValue.Undefined)
                val isObject = regexpValue match {
                  case JSValue.Object(_) | JSValue.JSArrayVal(_) |
                      _: JSValue.Function | JSValue.Native(_) =>
                    true
                  case _ => false
                }
                if !isObject then
                  ctx.throwTypeError(
                    "RegExp.prototype[Symbol.matchAll] called on non-object"
                  )
                val str =
                  args.lift(1).map(a => BuiltinHelpers.toJSString(a)).getOrElse("")
                val flags = BuiltinHelpers.toJSString(
                  BuiltinHelpers.getPropertyWithGetter(regexpValue, "flags")
                )
                // The iterator runs on a fresh matcher; its lastIndex starts
                // from the receiver's.
                val matcher = buildRegExp(
                  regexpValue,
                  JSValue.fromString(flags)
                )
                matcher match {
                  case JSValue.Object(matcherObj) =>
                    val lastIndex = BuiltinHelpers.toNumber(
                      BuiltinHelpers.getPropertyWithGetter(
                        regexpValue,
                        "lastIndex"
                      )
                    )
                    val toLength =
                      if lastIndex.isNaN || lastIndex <= 0 then 0
                      else if lastIndex >= 9007199254740991.0 then
                        9007199254740991L
                      else lastIndex.toLong
                    matcherObj.set("lastIndex", JSValue.fromDouble(toLength.toDouble))
                  case _ => ()
                }
                createRegExpStringIterator(
                  matcher,
                  str,
                  flags.contains('g'),
                  flags.contains('u') || flags.contains('v')
                )
              }
            )
            regexpPrototype.initSymbolProperty(
              id,
              JSValue.Native(matchAll),
              enumerable = false,
              writable = true,
              configurable = true
            )
          case _ => ()
        }
      case _ => ()
    }

    regexpPrototype.defineProperty(
      "constructor",
      JSValue.Native(regexpConstructor),
      enumerable = false,
      writable = true,
      configurable = true
    )
    ctx.global.set("RegExp", JSValue.Native(regexpConstructor))
  }
}
