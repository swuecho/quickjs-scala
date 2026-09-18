package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.interpreter.Interpreter
import scala.collection.mutable

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

    /** ES IsRegExp: the `Symbol.match` property is read (observable). */
    def isRegExpValue(value: JSValue): Boolean =
      if !BuiltinHelpers.isObjectLikeValue(value) then false
      else {
        val matcher = BuiltinHelpers.getSymbolPropertyWithGetter(
          value,
          BuiltinHelpers.wellKnownSymbolId("match")
        )
        matcher != JSValue.Undefined && matcher.toBoolean
      }

    def fillRegExpObject(
        obj: JSObject,
        patternValue: JSValue,
        flagsValue: JSValue
    ): JSValue = {
      val (pattern, flags) =
        getRegExpData(patternValue) match {
          case Some((_, data)) =>
            if flagsValue == JSValue.Undefined then (data.pattern, data.flags)
            else (data.pattern, BuiltinHelpers.toJSString(flagsValue))
          case None =>
            // A non-[[RegExpMatcher]] object with a truthy `Symbol.match`
            // contributes its `source`/`flags` properties (ES2024 22.2.4.1
            // step 3); everything else is ToString-ed as the pattern.
            if isRegExpValue(patternValue) then {
              val source = BuiltinHelpers.getPropertyWithGetter(
                patternValue,
                "source"
              )
              val flags =
                if flagsValue == JSValue.Undefined then
                  BuiltinHelpers.getPropertyWithGetter(
                    patternValue,
                    "flags"
                  )
                else flagsValue
              (
                BuiltinHelpers.toJSString(source),
                BuiltinHelpers.toJSString(flags)
              )
            } else
              (
                BuiltinHelpers.toJSString(patternValue),
                if flagsValue == JSValue.Undefined then ""
                else BuiltinHelpers.toJSString(flagsValue)
              )
        }
      val (_, global, ignoreCase, multiline, dotAll, unicode, sticky) =
        parseRegExpFlags(flags)
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

    def buildRegExp(patternValue: JSValue, flagsValue: JSValue): JSValue =
      if getRegExpData(patternValue).isDefined && flagsValue == JSValue.Undefined
      then patternValue
      else
        fillRegExpObject(
          JSObject(prototype = regexpPrototype, extensible = true),
          patternValue,
          flagsValue
        )

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
      prototype = regexpPrototype,
      superInitImpl = Some((thisValue, args, initCtx) => {
        given JSContext = initCtx
        thisValue match {
          case JSValue.Object(obj) =>
            val pattern = if args.nonEmpty then args(0) else JSValue.fromString("")
            val flags = if args.length > 1 then args(1) else JSValue.Undefined
            fillRegExpObject(obj, pattern, flags)
          case _ => initCtx.throwTypeError("Constructor RegExp requires 'new'")
        }
      })
    )
    BuiltinHelpers.initConstructor(regexpConstructor, length = 2)

    // RegExp.escape(string) - ES2025 static method. Mirrors the QuickJS C
    // EncodeForRegExpEscape over UTF-16 code units.
    def isRegExpEscapeSpace(c: Int): Boolean =
      Character.isWhitespace(c) || Character.isSpaceChar(c) || c == 0xFEFF

    val regexpEscape = NativeFunction(
      name = "escape",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = args.lift(1).getOrElse(JSValue.Undefined)
        value match {
          case JSValue.JSStr(str) =>
            val sb = new java.lang.StringBuilder(str.length + 8)
            var i = 0
            while i < str.length do {
              val c = str.charAt(i).toInt
              if c < 33 then {
                if c >= 9 && c <= 13 then {
                  sb.append('\\')
                  sb.append("tnvfr".charAt(c - 9))
                } else sb.append(f"\\x$c%02x")
              } else if c < 128 then {
                val isAlnum =
                  (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') ||
                    (c >= 'a' && c <= 'z')
                if isAlnum then {
                  if i == 0 then sb.append(f"\\x$c%02x")
                  else sb.append(c.toChar)
                } else if ",-=<>#&!%:;@~'`\"".indexOf(c) >= 0 then
                  sb.append(f"\\x$c%02x")
                else {
                  if c != '_' then sb.append('\\')
                  sb.append(c.toChar)
                }
              } else if c < 256 then sb.append(f"\\x$c%02x")
              else if Character.isSurrogate(c.toChar) || isRegExpEscapeSpace(c)
              then sb.append(f"\\u$c%04x")
              else sb.append(c.toChar)
              i += 1
            }
            JSValue.fromString(sb.toString)
          case _ =>
            ctx.throwTypeError("RegExp.escape argument must be a string")
        }
    )
    regexpConstructor.funcObj.defineProperty(
      "escape",
      JSValue.Native(regexpEscape),
      enumerable = false,
      writable = true,
      configurable = true
    )

    /** Spec `Set(receiver, "lastIndex", value, true)`: invoke accessor setters,
      * throw TypeError for missing setters / non-writable data properties and
      * otherwise write through the ordinary property machinery.
      */
    def assignLastIndex(receiver: JSValue, value: JSValue): Unit =
      receiver match {
        case JSValue.Object(obj) =>
          obj.getPropertyDescriptorWithOwner("lastIndex") match {
            case Some((_, _, attrs)) if attrs.setter.isDefined =>
              BuiltinHelpers.callFunctionWithThis(
                attrs.setter.get,
                receiver,
                Array(value)
              )
            case Some((_, _, attrs))
                if attrs.isAccessor || attrs.getter.isDefined ||
                  !attrs.writable =>
              ctx.throwTypeError(
                "Cannot assign to read only property 'lastIndex'"
              )
            case _ =>
              Interpreter().setPropertyValue(
                obj,
                receiver,
                "lastIndex",
                value,
                Nil,
                quickjs.tracing.TraceRecorder.Noop,
                isStrict = true
              )
          }
        case JSValue.JSArrayVal(arr) => arr.setProperty("lastIndex", value)
        case _                       => ()
      }

    def setLastIndexChecked(obj: JSObject, value: JSValue): Unit =
      assignLastIndex(JSValue.Object(obj), value)

    def advanceStringIndex(str: String, index: Int, unicode: Boolean): Int =
      if !unicode || index + 1 >= str.length then index + 1
      else {
        val first = str.charAt(index)
        if first >= 0xd800 && first <= 0xdbff then {
          val second = str.charAt(index + 1)
          if second >= 0xdc00 && second <= 0xdfff then index + 2 else index + 1
        } else index + 1
      }

    /** ES RegExpBuiltinExec start position handling: a Unicode regex cannot
      * begin matching in the middle of a surrogate pair.
      */
    def splitsSurrogatePair(input: String, start: Int, unicode: Boolean): Boolean =
      unicode && start > 0 && start < input.length &&
        Character.isLowSurrogate(input.charAt(start)) &&
        Character.isHighSurrogate(input.charAt(start - 1))

    val regexpExec = NativeFunction(
      name = "exec",
      impl = (args, ctx) =>
        given JSContext = ctx
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        val input = if args.length > 1 then BuiltinHelpers.toJSString(args(1)) else ""
        getRegExpData(thisValue) match {
          case Some((obj, data)) =>
            val global = data.global || data.sticky
            val start =
              if global then
                val idx = BuiltinHelpers.toNumber(
                  obj.get("lastIndex")(using ctx)
                )
                if idx.isNaN || idx < 0 then 0 else idx.toInt
              else 0
            val unicode = data.flags.contains('u') || data.flags.contains('v')
            val matcher = data.regex.matcher(input)
            // A Unicode regex treats `lastIndex` inside a surrogate pair as
            // pointing at the start of the code point (like QuickJS).
            var from =
              if splitsSurrogatePair(input, start, unicode) then start - 1
              else start
            var matched = false
            if from > input.length then from = input.length + 1
            while !matched && from <= input.length do {
              if matcher.find(from) then {
                // Sticky matching must start exactly at `lastIndex`; a later
                // occurrence is not a match. (`region`/`lookingAt` cannot be
                // used: a region boundary would anchor `^` where lastIndex is.)
                if data.sticky && matcher.start() != from then
                  from = input.length + 1
                else if splitsSurrogatePair(input, matcher.start(), unicode)
                then from = advanceStringIndex(input, matcher.start(), unicode)
                else matched = true
              } else from = input.length + 1
            }
            if matched then {
              if global then
                setLastIndexChecked(obj, JSValue.fromInt(matcher.end()))
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
              arr.setProperty(
                "groups",
                BuiltinHelpers.regexpNamedGroups(data.pattern, matcher)
              )
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
                indices.setProperty(
                  "groups",
                  BuiltinHelpers.regexpNamedGroupIndices(data.pattern, matcher)
                )
                arr.setProperty("indices", JSValue.JSArrayVal(indices))
              }
              JSValue.JSArrayVal(arr)
            }
            else {
              if global then setLastIndexChecked(obj, JSValue.fromInt(0))
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
        val input = if args.length > 1 then BuiltinHelpers.toJSString(args(1)) else ""
        getRegExpData(thisValue) match {
          case Some((obj, data)) =>
            val global = data.global || data.sticky
            val start =
              if global then
                val idx = BuiltinHelpers.toNumber(
                  obj.get("lastIndex")(using ctx)
                )
                if idx.isNaN || idx < 0 then 0 else idx.toInt
              else 0
            val unicode = data.flags.contains('u') || data.flags.contains('v')
            val matcher = data.regex.matcher(input)
            // A Unicode regex treats `lastIndex` inside a surrogate pair as
            // pointing at the start of the code point (like QuickJS).
            var from =
              if splitsSurrogatePair(input, start, unicode) then start - 1
              else start
            var matched = false
            if from > input.length then from = input.length + 1
            while !matched && from <= input.length do {
              if matcher.find(from) then {
                // See regexpExec: sticky requires the match to start exactly
                // at `from`, without region-anchoring `^`.
                if data.sticky && matcher.start() != from then
                  from = input.length + 1
                else if splitsSurrogatePair(input, matcher.start(), unicode)
                then from = advanceStringIndex(input, matcher.start(), unicode)
                else matched = true
              } else from = input.length + 1
            }
            if matched && global then
              setLastIndexChecked(obj, JSValue.fromInt(matcher.end()))
            else if !matched && global then
              setLastIndexChecked(obj, JSValue.fromInt(0))
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

    // `flags` is composed from the individual flag properties with [[Get]],
    // so own properties/getters that override `global`, `unicode`, ... are
    // observed (ES2024 get RegExp.prototype.flags).
    val regexpFlagsGetter = NativeFunction(
      name = "get flags",
      length = 0,
      impl = (args, callCtx) =>
        given JSContext = callCtx
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        if !BuiltinHelpers.isObjectLikeValue(receiver) then
          ctx.throwTypeError(
            "RegExp.prototype.flags getter called on non-object"
          )
        val sb = new StringBuilder()
        def appendFlag(property: String, ch: Char): Unit =
          if BuiltinHelpers
              .getPropertyWithGetter(receiver, property)
              .toBoolean
          then sb.append(ch)
        appendFlag("hasIndices", 'd')
        appendFlag("global", 'g')
        appendFlag("ignoreCase", 'i')
        appendFlag("multiline", 'm')
        appendFlag("dotAll", 's')
        appendFlag("unicode", 'u')
        appendFlag("unicodeSets", 'v')
        appendFlag("sticky", 'y')
        JSValue.fromString(sb.toString)
    )

    val regexpAccessors = Seq(
      "source" -> regexpDataGetter("source", d => JSValue.fromString(d.pattern)),
      "flags" -> regexpFlagsGetter,
      "global" -> regexpDataGetter("global", d => JSValue.Bool(d.global)),
      "ignoreCase" -> regexpDataGetter("ignoreCase", d => JSValue.Bool(d.ignoreCase)),
      "multiline" -> regexpDataGetter("multiline", d => JSValue.Bool(d.multiline)),
      "dotAll" -> regexpDataGetter("dotAll", d => JSValue.Bool(d.dotAll)),
      "unicode" -> regexpDataGetter("unicode", d => JSValue.Bool(d.unicode)),
      "unicodeSets" -> regexpDataGetter("unicodeSets", d => JSValue.Bool(d.flags.contains('v'))),
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

    /** ES RegExpExec: call the observable `exec` method, falling back to the
      * built-in RegExp.prototype.exec.
      */
    def regExpExec(regexpValue: JSValue, str: String): JSValue = {
      val exec = BuiltinHelpers.getPropertyWithGetter(regexpValue, "exec")
      val result =
        if BuiltinHelpers.isCallable(exec) then
          BuiltinHelpers.callFunctionWithThis(
            exec,
            regexpValue,
            Array(JSValue.fromString(str))
          )
        else regexpExec.call(Array(regexpValue, JSValue.fromString(str)))
      result match {
        case JSValue.Null => result
        case value if BuiltinHelpers.isObjectLikeValue(value) => result
        case _ =>
          ctx.throwTypeError(
            "RegExp exec method returned something other than an Object or null"
          )
      }
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
    def isConstructorValue(value: JSValue): Boolean = value match {
      case func: JSValue.Function => func.isConstructor
      case JSValue.Native(_: quickjs.value.NativeConstructor) => true
      case _ => false
    }
    def constructValue(
        constructorValue: JSValue,
        ctorArgs: Array[JSValue]
    ): JSValue = constructorValue match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.construct(ctorArgs)
      case func: JSValue.Function if func.isConstructor =>
        val prototype =
          BuiltinHelpers.getPropertyWithGetter(func, "prototype") match {
            case JSValue.Object(proto) => proto
            case _                     => ctx.objectPrototype
          }
        val newObj = JSObject(prototype = prototype, extensible = true)
        val ret = Interpreter().call(
          BuiltinHelpers.functionToBytecode(func),
          JSValue.Object(newObj),
          ctorArgs,
          func.closure,
          constructorValue
        )
        ret match {
          case _: JSValue.Object | _: JSValue.Function |
              _: JSValue.JSArrayVal | _: JSValue.Generator |
              _: JSValue.Promise | _: JSValue.Native |
              _: JSValue.AsyncFunction =>
            ret
          case _ => JSValue.Object(newObj)
        }
      case _ =>
        ctx.throwTypeError("constructor is not a constructor")
    }
    def speciesConstructorValue(
        obj: JSValue,
        defaultCtor: JSValue
    ): JSValue = {
      val ctor =
        BuiltinHelpers.getPropertyWithGetter(obj, "constructor")
      if ctor == JSValue.Undefined then defaultCtor
      else {
        if !BuiltinHelpers.isObjectLikeValue(ctor) then
          ctx.throwTypeError("constructor is not an object")
        val species = BuiltinHelpers.getSymbolPropertyWithGetter(
          ctor,
          BuiltinHelpers.wellKnownSymbolId("species")
        )
        if species == JSValue.Undefined || species == JSValue.Null then
          defaultCtor
        else if isConstructorValue(species) then species
        else ctx.throwTypeError("species is not a constructor")
      }
    }

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
                if !BuiltinHelpers.isObjectLikeValue(regexpValue) then
                  ctx.throwTypeError(
                    "RegExp.prototype[Symbol.matchAll] called on non-object"
                  )
                val str =
                  args.lift(1).map(a => BuiltinHelpers.toJSString(a)).getOrElse("")
                val flags = BuiltinHelpers.toJSString(
                  BuiltinHelpers.getPropertyWithGetter(regexpValue, "flags")
                )
                // SpeciesConstructor + Construct produce a fresh matcher;
                // its lastIndex starts from the receiver's.
                val ctor = speciesConstructorValue(
                  regexpValue,
                  ctx.global.get("RegExp")
                )
                val matcher = constructValue(
                  ctor,
                  Array(regexpValue, JSValue.fromString(flags))
                )
                val lastIndex = BuiltinHelpers.toNumber(
                  BuiltinHelpers.getPropertyWithGetter(
                    regexpValue,
                    "lastIndex"
                  )
                )
                val toLength =
                  if lastIndex.isNaN || lastIndex <= 0 then 0L
                  else if lastIndex >= 9007199254740991.0 then
                    9007199254740991L
                  else lastIndex.toLong
                matcher match {
                  case JSValue.Object(matcherObj) =>
                    matcherObj.set(
                      "lastIndex",
                      JSValue.fromDouble(toLength.toDouble)
                    )
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

    // -------------------------------------------------------------------
    // Generic RegExp.prototype[Symbol.match/search/replace] (ES2024 22.2.6)
    // -------------------------------------------------------------------

    def isObjectReceiver(value: JSValue, method: String): Unit =
      if !BuiltinHelpers.isObjectLikeValue(value) then
        ctx.throwTypeError(
          s"RegExp.prototype[Symbol.$method] called on non-object"
        )

    def receiverFlags(receiver: JSValue): String =
      BuiltinHelpers.toJSString(
        BuiltinHelpers.getPropertyWithGetter(receiver, "flags")
      )

    def receiverLastIndex(receiver: JSValue): JSValue =
      BuiltinHelpers.getPropertyWithGetter(receiver, "lastIndex")

    def setReceiverLastIndex(receiver: JSValue, value: JSValue): Unit =
      assignLastIndex(receiver, value)

    def toLengthValue(value: JSValue): Long = {
      val n = BuiltinHelpers.toIntegerOrInfinity(value)
      if n.isNaN || n <= 0.0 then 0L
      else if n >= 9007199254740991.0 then 9007199254740991L
      else n.toLong
    }

    /** AdvanceStringIndex over Long indexes: `index + 1` when the index is
      * already at (or beyond) the string end, so lastIndex values above Int
      * range do not overflow.
      */
    def advanceIndexLong(str: String, index: Long, unicode: Boolean): Long =
      if !unicode || index + 1 >= str.length.toLong then index + 1
      else advanceStringIndex(str, index.toInt, unicode).toLong

    def sameValueJS(a: JSValue, b: JSValue): Boolean = (a, b) match {
      case (JSValue.Int32(x), JSValue.Int32(y))     => x == y
      case (JSValue.Float64(x), JSValue.Float64(y)) =>
        java.lang.Double.doubleToLongBits(x) ==
          java.lang.Double.doubleToLongBits(y)
      case (JSValue.Int32(x), JSValue.Float64(y)) =>
        x.toDouble == y && !(y == 0.0 && (1.0 / y) < 0.0)
      case (JSValue.Float64(x), JSValue.Int32(y)) =>
        x == y.toDouble && !(x == 0.0 && (1.0 / x) < 0.0)
      case (x, y) => x == y
    }

    /** ES GetSubstitution over materialized captures. */
    def getSubstitution(
        matched: String,
        str: String,
        position: Int,
        captures: Array[JSValue],
        namedCaptures: JSValue,
        replacement: String
    ): String = {
      val sb = new StringBuilder()
      val len = replacement.length
      var i = 0
      while i < len do {
        val dollar = replacement.indexOf('$', i)
        if dollar < 0 || dollar + 1 >= len then {
          sb.append(replacement.substring(i))
          i = len
        } else {
          sb.append(replacement.substring(i, dollar))
          var j = dollar + 1
          val c = replacement.charAt(j)
          j += 1
          c match {
            case '$' => sb.append('$')
            case '&' => sb.append(matched)
            case '`' =>
              sb.append(str.substring(0, math.min(position, str.length)))
            case '\'' =>
              val from = math.min(position + matched.length, str.length)
              sb.append(str.substring(from))
            case d if d >= '0' && d <= '9' =>
              var k = d - '0'
              if j < len then {
                val c1 = replacement.charAt(j)
                if c1 >= '0' && c1 <= '9' then {
                  val k1 = k * 10 + (c1 - '0')
                  if k1 >= 1 && k1 < captures.length then {
                    k = k1
                    j += 1
                  }
                }
              }
              if k >= 1 && k < captures.length then
                captures(k) match {
                  case JSValue.Undefined => ()
                  case capture =>
                    sb.append(BuiltinHelpers.toJSString(capture))
                }
              else sb.append(replacement.substring(dollar, j))
            case '<' if namedCaptures != JSValue.Undefined =>
              val close = replacement.indexOf('>', j)
              if close < 0 then sb.append(replacement.substring(dollar, j))
              else {
                val name = replacement.substring(j, close)
                BuiltinHelpers.getPropertyWithGetter(namedCaptures, name) match {
                  case JSValue.Undefined => ()
                  case capture =>
                    sb.append(BuiltinHelpers.toJSString(capture))
                }
                j = close + 1
              }
            case _ => sb.append(replacement.substring(dollar, j))
          }
          i = j
        }
      }
      sb.toString
    }

    val regexpSymbolMatch = NativeFunction(
      name = "[Symbol.match]",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        isObjectReceiver(receiver, "match")
        val str =
          BuiltinHelpers.toJSString(args.lift(1).getOrElse(JSValue.Undefined))
        // `flags` is read (and ToString-ed) from the receiver; the flags
        // string decides global/unicode handling (ES2024 22.2.6.8).
        val flags = receiverFlags(receiver)
        if !flags.contains('g') then regExpExec(receiver, str)
        else {
          val fullUnicode = flags.contains('u') || flags.contains('v')
          setReceiverLastIndex(receiver, JSValue.fromInt(0))
          val result = quickjs.objmodel.JSArray.empty()
          var n = 0
          var done = false
          while !done do {
            val execResult = regExpExec(receiver, str)
            if execResult == JSValue.Null then done = true
            else {
              val matchStr = BuiltinHelpers.toJSString(
                BuiltinHelpers.getPropertyWithGetter(execResult, "0")
              )
              result.push(JSValue.fromString(matchStr))
              n += 1
              if matchStr.isEmpty then {
                val thisIndex = toLengthValue(receiverLastIndex(receiver))
                setReceiverLastIndex(
                  receiver,
                  JSValue.fromLong(advanceIndexLong(str, thisIndex, fullUnicode))
                )
              }
            }
          }
          if n == 0 then JSValue.Null else JSValue.JSArrayVal(result)
        }
      }
    )

    val regexpSymbolSearch = NativeFunction(
      name = "[Symbol.search]",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        isObjectReceiver(receiver, "search")
        val str =
          BuiltinHelpers.toJSString(args.lift(1).getOrElse(JSValue.Undefined))
        val previousLastIndex = receiverLastIndex(receiver)
        if !sameValueJS(previousLastIndex, JSValue.fromInt(0)) then
          setReceiverLastIndex(receiver, JSValue.fromInt(0))
        val result = regExpExec(receiver, str)
        val currentLastIndex = receiverLastIndex(receiver)
        if !sameValueJS(currentLastIndex, previousLastIndex) then
          setReceiverLastIndex(receiver, previousLastIndex)
        if result == JSValue.Null then JSValue.fromInt(-1)
        else {
          val index = BuiltinHelpers.toIntegerOrInfinity(
            BuiltinHelpers.getPropertyWithGetter(result, "index")
          )
          JSValue.fromDouble(if index.isNaN then 0.0 else index)
        }
      }
    )

    val regexpSymbolReplace = NativeFunction(
      name = "[Symbol.replace]",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        isObjectReceiver(receiver, "replace")
        val str =
          BuiltinHelpers.toJSString(args.lift(1).getOrElse(JSValue.Undefined))
        val replaceValue = args.lift(2).getOrElse(JSValue.Undefined)
        val functionalReplace = BuiltinHelpers.isCallable(replaceValue)
        val replacementString =
          if functionalReplace then ""
          else BuiltinHelpers.toJSString(replaceValue)
        val flags = receiverFlags(receiver)
        val isGlobal = flags.contains('g')
        val fullUnicode = flags.contains('u') || flags.contains('v')
        if isGlobal then setReceiverLastIndex(receiver, JSValue.fromInt(0))
        val results = mutable.ArrayBuffer.empty[JSValue]
        var done = false
        while !done do {
          val result = regExpExec(receiver, str)
          if result == JSValue.Null then done = true
          else {
            results += result
            if !isGlobal then done = true
            else {
              val matched = BuiltinHelpers.toJSString(
                BuiltinHelpers.getPropertyWithGetter(result, "0")
              )
              if matched.isEmpty then {
                val thisIndex = toLengthValue(receiverLastIndex(receiver))
                setReceiverLastIndex(
                  receiver,
                  JSValue.fromLong(advanceIndexLong(str, thisIndex, fullUnicode))
                )
              }
            }
          }
        }
        val out = new StringBuilder()
        var nextSourcePosition = 0
        for result <- results do {
          val matched = BuiltinHelpers.toJSString(
            BuiltinHelpers.getPropertyWithGetter(result, "0")
          )
          val positionD = BuiltinHelpers.toIntegerOrInfinity(
            BuiltinHelpers.getPropertyWithGetter(result, "index")
          )
          val position =
            if positionD.isNaN || positionD <= 0.0 then 0
            else if positionD > str.length.toDouble then str.length
            else positionD.toInt
          val captureCount = math.max(
            toLengthValue(
              BuiltinHelpers.getPropertyWithGetter(result, "length")
            ).toInt,
            1
          )
          val captures = new Array[JSValue](captureCount)
          captures(0) = JSValue.fromString(matched)
          var n = 1
          while n < captureCount do {
            val capture =
              BuiltinHelpers.getPropertyWithGetter(result, n.toString)
            captures(n) =
              if capture == JSValue.Undefined then capture
              else JSValue.fromString(BuiltinHelpers.toJSString(capture))
            n += 1
          }
          val namedCapturesRaw =
            BuiltinHelpers.getPropertyWithGetter(result, "groups")
          val replacementText =
            if functionalReplace then {
              val callArgs = mutable.ArrayBuffer[JSValue](
                JSValue.fromString(matched)
              )
              var i = 1
              while i < captureCount do {
                callArgs += captures(i)
                i += 1
              }
              callArgs += JSValue.fromInt(position)
              callArgs += JSValue.fromString(str)
              if namedCapturesRaw != JSValue.Undefined then
                callArgs += namedCapturesRaw
              BuiltinHelpers.toJSString(
                BuiltinHelpers.callFunctionWithThis(
                  replaceValue,
                  JSValue.Undefined,
                  callArgs.toArray
                )
              )
            } else {
              val namedCaptures =
                if namedCapturesRaw == JSValue.Undefined then
                  JSValue.Undefined
                else BuiltinHelpers.toObject(namedCapturesRaw)
              getSubstitution(
                matched,
                str,
                position,
                captures,
                namedCaptures,
                replacementString
              )
            }
          if position >= nextSourcePosition then {
            out.append(str.substring(nextSourcePosition, position))
            out.append(replacementText)
            nextSourcePosition = position + matched.length
          }
        }
        out.append(str.substring(math.min(nextSourcePosition, str.length)))
        JSValue.fromString(out.toString)
      }
    )

    def toUint32Value(value: JSValue): Long = {
      val n = BuiltinHelpers.toNumber(value)
      if n.isNaN || n.isInfinite then 0L
      else (math.floor(math.abs(n)) * math.signum(n)).toLong & 0xffffffffL
    }

    val regexpSymbolSplit = NativeFunction(
      name = "[Symbol.split]",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        isObjectReceiver(receiver, "split")
        val str =
          BuiltinHelpers.toJSString(args.lift(1).getOrElse(JSValue.Undefined))
        val limitValue = args.lift(2).getOrElse(JSValue.Undefined)
        val ctor = speciesConstructorValue(receiver, ctx.global.get("RegExp"))
        val flags = receiverFlags(receiver)
        val unicodeMatching = flags.contains('u') || flags.contains('v')
        val newFlags = if flags.contains('y') then flags else flags + "y"
        val splitter = constructValue(
          ctor,
          Array(receiver, JSValue.fromString(newFlags))
        )
        val result = quickjs.objmodel.JSArray.empty()
        var lengthA = 0L
        val lim =
          if limitValue == JSValue.Undefined then 4294967295L
          else toUint32Value(limitValue)
        if lim == 0L then JSValue.JSArrayVal(result)
        else if str.isEmpty then {
          if regExpExec(splitter, str) != JSValue.Null then
            JSValue.JSArrayVal(result)
          else {
            result.push(JSValue.fromString(str))
            JSValue.JSArrayVal(result)
          }
        } else {
          var p = 0
          var q = 0
          var done = false
          while !done do {
            if q >= str.length then done = true
            else {
              assignLastIndex(splitter, JSValue.fromInt(q))
              val z = regExpExec(splitter, str)
              if z == JSValue.Null then
                q = advanceIndexLong(str, q.toLong, unicodeMatching).toInt
              else {
                val e = math.min(
                  toLengthValue(receiverLastIndex(splitter)),
                  str.length.toLong
                )
                if e == p.toLong then
                  q = advanceIndexLong(str, q.toLong, unicodeMatching).toInt
                else {
                  result.push(JSValue.fromString(str.substring(p, q)))
                  lengthA += 1
                  if lengthA == lim then done = true
                  else {
                    p = e.toInt
                    val numberOfCaptures = math.max(
                      toLengthValue(
                        BuiltinHelpers.getPropertyWithGetter(z, "length")
                      ) - 1,
                      0L
                    )
                    var i = 1L
                    var capsDone = false
                    while !capsDone && i <= numberOfCaptures do {
                      result.push(
                        BuiltinHelpers.getPropertyWithGetter(z, i.toString)
                      )
                      lengthA += 1
                      if lengthA == lim then {
                        capsDone = true
                        done = true
                      } else i += 1
                    }
                    if !done then q = p
                  }
                }
              }
            }
          }
          if lengthA < lim then
            result.push(JSValue.fromString(str.substring(p)))
          JSValue.JSArrayVal(result)
        }
      }
    )

    def installSymbolMethod(symbolName: String, method: JSValue): Unit =
      ctx.global.get("Symbol") match {
        case JSValue.Native(symbolConstructor: quickjs.value.NativeConstructor) =>
          symbolConstructor.funcObj.get(symbolName) match {
            case JSValue.Symbol(id) =>
              regexpPrototype.initSymbolProperty(
                id,
                method,
                enumerable = false,
                writable = true,
                configurable = true
              )
            case _ => ()
          }
        case _ => ()
      }
    installSymbolMethod("match", JSValue.Native(regexpSymbolMatch))
    installSymbolMethod("search", JSValue.Native(regexpSymbolSearch))
    installSymbolMethod("replace", JSValue.Native(regexpSymbolReplace))
    installSymbolMethod("split", JSValue.Native(regexpSymbolSplit))

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
