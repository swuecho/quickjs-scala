package quickjs.stdlib

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject

/** JavaScript String object with methods.
  *
  * Provides str.length, str.trim(), str.split(), etc. Uses proper 'this'
  * binding for method calls.
  */
object StringStatics:
  /** Initialize String methods */
  def initialize()(using ctx: JSContext): Unit =
    val stringObj = JSObject(prototype = null, extensible = true)

    // str.length - get string length
    // Note: length is a property, not a method, handled in GetProp

    // str.trim() - remove whitespace from both ends
    val trimFunc = NativeFunction(
      "trim",
      (args, context) =>
        if args.isEmpty then JSValue.fromString("")
        else
          args(0) match
            case JSValue.JSStr(s) =>
              JSValue.fromString(s.trim)
            case _ =>
              JSValue.Undefined
    )
    stringObj.set("trim", JSValue.Native(trimFunc))

    // str.toLowerCase() - convert to lowercase
    val toLowerCaseFunc = NativeFunction(
      "toLowerCase",
      (args, context) =>
        if args.isEmpty then JSValue.fromString("")
        else
          args(0) match
            case JSValue.JSStr(s) =>
              JSValue.fromString(s.toLowerCase)
            case _ =>
              JSValue.Undefined
    )
    stringObj.set("toLowerCase", JSValue.Native(toLowerCaseFunc))

    // str.toUpperCase() - convert to uppercase
    val toUpperCaseFunc = NativeFunction(
      "toUpperCase",
      (args, context) =>
        if args.isEmpty then JSValue.fromString("")
        else
          args(0) match
            case JSValue.JSStr(s) =>
              JSValue.fromString(s.toUpperCase)
            case _ =>
              JSValue.Undefined
    )
    stringObj.set("toUpperCase", JSValue.Native(toUpperCaseFunc))

    // str.indexOf(search) - find substring
    val indexOfFunc = NativeFunction(
      "indexOf",
      (args, context) =>
        if args.length < 2 then JSValue.fromInt(-1)
        else
          args(0) match
            case JSValue.JSStr(s) =>
              val search = args(1) match
                case JSValue.JSStr(sub) => sub
                case _                  => ""
              val start = if args.length >= 3 then args(2).toNumber.toInt else 0
              JSValue.fromInt(s.indexOf(search, Math.max(0, start)))
            case _ =>
              JSValue.fromInt(-1)
    )
    stringObj.set("indexOf", JSValue.Native(indexOfFunc))

    // str.includes(search) - check if string contains substring
    val includesFunc = NativeFunction(
      "includes",
      (args, context) =>
        if args.length < 2 then JSValue.Bool(false)
        else
          args(0) match
            case JSValue.JSStr(s) =>
              val search = args(1) match
                case JSValue.JSStr(sub) => sub
                case _                  => ""
              JSValue.Bool(s.contains(search))
            case _ =>
              JSValue.Bool(false)
    )
    stringObj.set("includes", JSValue.Native(includesFunc))

    // str.startsWith(search) - check if string starts with prefix
    val startsWithFunc = NativeFunction(
      "startsWith",
      (args, context) =>
        if args.length < 2 then JSValue.Bool(false)
        else
          args(0) match
            case JSValue.JSStr(s) =>
              val prefix = args(1) match
                case JSValue.JSStr(pre) => pre
                case _                  => ""
              JSValue.Bool(s.startsWith(prefix))
            case _ =>
              JSValue.Bool(false)
    )
    stringObj.set("startsWith", JSValue.Native(startsWithFunc))

    // str.endsWith(search) - check if string ends with suffix
    val endsWithFunc = NativeFunction(
      "endsWith",
      (args, context) =>
        if args.length < 2 then JSValue.Bool(false)
        else
          args(0) match
            case JSValue.JSStr(s) =>
              val suffix: String = args(1) match
                case JSValue.JSStr(suf) => suf
                case _                  => ""
              JSValue.Bool(s.endsWith(suffix))
            case _ =>
              JSValue.Bool(false)
    )
    stringObj.set("endsWith", JSValue.Native(endsWithFunc))

    // str.split(separator) - split string into array
    val splitFunc = NativeFunction(
      "split",
      (args, context) =>
        if args.isEmpty then
          JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else
          args(0) match
            case JSValue.JSStr(s) =>
              val sep =
                if args.length >= 2 then
                  args(1) match
                    case JSValue.JSStr(separator) => separator
                    case _                        => ""
                else ""

              if sep.isEmpty then
                // Split into individual characters
                val arr = quickjs.objmodel.JSArray.empty()
                for c <- s do arr.push(JSValue.fromString(c.toString))
                JSValue.JSArrayVal(arr)
              else
                // Split by separator
                val parts = s.split(java.util.regex.Pattern.quote(sep), -1)
                val arr = quickjs.objmodel.JSArray.empty()
                for part <- parts do arr.push(JSValue.fromString(part))
                JSValue.JSArrayVal(arr)
            case _ =>
              JSValue.Undefined
    )
    stringObj.set("split", JSValue.Native(splitFunc))

    // str.substring(start, end) - extract substring
    val substringFunc = NativeFunction(
      "substring",
      (args, context) =>
        if args.isEmpty then JSValue.fromString("")
        else
          args(0) match
            case JSValue.JSStr(s) =>
              val len = s.length
              val start =
                if args.length >= 2 then
                  Math.max(0, Math.min(len, args(1).toNumber.toInt))
                else 0
              val end =
                if args.length >= 3 then
                  Math.max(0, Math.min(len, args(2).toNumber.toInt))
                else len

              JSValue.fromString(s.substring(start, end))
            case _ =>
              JSValue.Undefined
    )
    stringObj.set("substring", JSValue.Native(substringFunc))

    // str.charAt(index) - get character at index
    val charAtFunc = NativeFunction(
      "charAt",
      (args, context) =>
        if args.isEmpty then JSValue.fromString("")
        else
          args(0) match
            case JSValue.JSStr(s) =>
              val index = if args.length >= 2 then args(1).toNumber.toInt else 0
              if index >= 0 && index < s.length then
                JSValue.fromString(s.charAt(index).toString)
              else JSValue.fromString("")
            case _ =>
              JSValue.fromString("")
    )
    stringObj.set("charAt", JSValue.Native(charAtFunc))

    // str.charCodeAt(index) - get character code at index
    val charCodeAtFunc = NativeFunction(
      "charCodeAt",
      (args, context) =>
        if args.isEmpty then JSValue.fromInt(Double.NaN.toInt) // NaN
        else
          args(0) match
            case JSValue.JSStr(s) =>
              val index = if args.length >= 2 then args(1).toNumber.toInt else 0
              if index >= 0 && index < s.length then
                JSValue.fromInt(s.charAt(index).toInt)
              else JSValue.fromInt(Double.NaN.toInt)
            case _ =>
              JSValue.fromInt(Double.NaN.toInt)
    )
    stringObj.set("charCodeAt", JSValue.Native(charCodeAtFunc))

    // str.replace(search, replacement) - replace substring
    val replaceFunc = NativeFunction(
      "replace",
      (args, context) =>
        if args.length < 3 then args(0)
        else
          args(0) match
            case JSValue.JSStr(s) =>
              val search = args(1) match
                case JSValue.JSStr(str) => str
                case _                  => ""
              val replacement = args(2) match
                case JSValue.JSStr(rep) => rep
                case _                  => ""
              JSValue.fromString(s.replace(search, replacement))
            case _ =>
              JSValue.Undefined
    )
    stringObj.set("replace", JSValue.Native(replaceFunc))

    ctx.global.set("String", JSValue.Object(stringObj))

    // Minimal String() conversion function for top-level calls.
    val stringCall = NativeFunction(
      "String",
      (args, context) =>
        if args.isEmpty then JSValue.fromString("")
        else JSValue.fromString(args(0).toString)
    )
    ctx.globalScope.setVariable("String", JSValue.Native(stringCall))
