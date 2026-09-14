package quickjs.runtime.builtins

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers.{
  initConstructor,
  getRegExpData,
  parseRegExpFlags,
  RegExpData,
  numberToJSString,
  toIntegerOrInfinity,
  toJSString,
  toNumber
}
import java.math.{BigDecimal, BigInteger, MathContext, RoundingMode}
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale
import scala.collection.mutable

/** Number, String, and Boolean prototype methods. */
object NumberStringBuiltins {
  import quickjs.objmodel.{JSObject, JSArray}

  def initialize(ctx: JSContext): Unit = {
    val numberPrototype = quickjs.objmodel.JSObject(
      prototype = ctx.objectPrototype,
      extensible = true
    )
    val stringPrototype = quickjs.objmodel.JSObject(
      prototype = ctx.objectPrototype,
      extensible = true
    )
    val booleanPrototype = quickjs.objmodel.JSObject(
      prototype = ctx.objectPrototype,
      extensible = true
    )
    numberPrototype.setPrimitiveValue(JSValue.fromInt(0))
    stringPrototype.setPrimitiveValue(JSValue.fromString(""))
    booleanPrototype.setPrimitiveValue(JSValue.fromBoolean(false))

    def boxPrimitive(
        value: JSValue,
        prototype: JSObject,
        stringValue: Option[String] = None
    )(using context: JSContext): JSValue = {
      val wrapper = JSObject(prototype = prototype, extensible = true)
      stringValue.foreach { text =>
        var index = 0
        while index < text.length do {
          wrapper.defineProperty(
            index.toString,
            JSValue.fromString(text.charAt(index).toString),
            enumerable = true,
            writable = false,
            configurable = false
          )
          index += 1
        }
        wrapper.defineProperty(
          "length",
          JSValue.fromInt(text.length),
          enumerable = false,
          writable = false,
          configurable = false
        )
      }
      wrapper.setPrimitiveValue(value)
      JSValue.Object(wrapper)
    }

    val numberConstructor = quickjs.value.NativeConstructor(
      name = "Number",
      callImpl = (args, _) =>
        if args.isEmpty then JSValue.fromInt(0)
        else JSValue.fromDouble(args(0).toNumber),
      constructImpl = (args, context) =>
        given JSContext = context
        val value =
          if args.isEmpty then JSValue.fromInt(0)
          else JSValue.fromDouble(args(0).toNumber)
        boxPrimitive(value, numberPrototype),
      prototype = numberPrototype
    )

    val stringConstructor = quickjs.value.NativeConstructor(
      name = "String",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        if args.isEmpty then JSValue.fromString("")
        else
          args(0) match {
            case sym: JSValue.Symbol =>
              // Use Symbol.prototype.toString for proper description display
              ctx.symbolPrototype.get("toString")(using ctx) match {
                case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                  nf.call(Array(sym)) match {
                    case JSValue.JSStr(s) => JSValue.fromString(s)
                    case _                => JSValue.fromString(sym.toString)
                  }
                case _ => JSValue.fromString(sym.toString)
              }
            case other => JSValue.fromString(other.toString)
          },
      constructImpl = (args, context) =>
        given JSContext = context
        val text = if args.isEmpty then "" else args(0).toString
        boxPrimitive(JSValue.fromString(text), stringPrototype, Some(text)),
      prototype = stringPrototype
    )

    val booleanConstructor = quickjs.value.NativeConstructor(
      name = "Boolean",
      callImpl = (args, _) =>
        if args.isEmpty then JSValue.fromBoolean(false)
        else JSValue.fromBoolean(args(0).toBoolean),
      constructImpl = (args, context) =>
        given JSContext = context
        val value =
          if args.isEmpty then JSValue.fromBoolean(false)
          else JSValue.fromBoolean(args(0).toBoolean)
        boxPrimitive(value, booleanPrototype),
      prototype = booleanPrototype
    )

    given JSContext = ctx
    initConstructor(numberConstructor, length = 1)
    initConstructor(stringConstructor, length = 1)
    initConstructor(booleanConstructor, length = 1)
    ctx.global.set("Number", JSValue.Native(numberConstructor))
    ctx.global.set("String", JSValue.Native(stringConstructor))
    ctx.global.set("Boolean", JSValue.Native(booleanConstructor))

    def requireThisNumber(args: Array[JSValue], method: String)(using
        JSContext
    ): Double =
      if args.isEmpty then
        ctx.throwTypeError(
          s"Number.prototype.$method called on null or undefined"
        )
      else
        args(0) match {
          case JSValue.Null | JSValue.Undefined =>
            ctx.throwTypeError(
              s"Number.prototype.$method called on null or undefined"
            )
          case JSValue.Object(obj) =>
            obj.getPrimitiveValue match {
              case Some(JSValue.Int32(value))   => value.toDouble
              case Some(JSValue.Float64(value)) => value
              case _ => ctx.throwTypeError(
                  s"Number.prototype.$method called on incompatible receiver"
                )
            }
          case JSValue.Int32(value)   => value.toDouble
          case JSValue.Float64(value) => value
          case _ => ctx.throwTypeError(
              s"Number.prototype.$method called on incompatible receiver"
            )
        }

    def requireThisBoolean(args: Array[JSValue], method: String)(using
        JSContext
    ): Boolean =
      if args.isEmpty then
        ctx.throwTypeError(
          s"Boolean.prototype.$method called on null or undefined"
        )
      else
        args(0) match {
          case JSValue.Bool(b)     => b
          case JSValue.Object(obj) =>
            obj.getPrimitiveValue match {
              case Some(JSValue.Bool(b)) => b
              case _                     => ctx.throwTypeError("not a boolean")
            }
          case JSValue.Null | JSValue.Undefined =>
            ctx.throwTypeError(
              s"Boolean.prototype.$method called on null or undefined"
            )
          case _ =>
            ctx.throwTypeError("not a boolean")
        }

    def numberToString(value: Double, radix: Int): String =
      if radix == 10 then numberToJSString(value)
      else if value.isNaN || value.isInfinite then numberToJSString(value)
      else {
        val negative = value < 0
        val absolute = math.abs(value)
        val integer = math.floor(absolute)
        val integerText =
          BigDecimal.valueOf(integer).toBigInteger.toString(radix)
        var fraction = new BigDecimal(absolute)
          .subtract(BigDecimal.valueOf(integer))
        if fraction.signum() == 0 then
          (if negative then "-" else "") + integerText
        else {
          // A binary64 has 53 significant bits. Generate that many radix
          // digits plus one guard digit, then round the retained result.
          val significantDigits =
            math.ceil(53.0 / (math.log(radix) / math.log(2.0))).toInt
          val retained = significantDigits
          val digits = mutable.ArrayBuffer.empty[Int]
          val radixDecimal = BigDecimal.valueOf(radix.toLong)
          var i = 0
          while i <= retained do {
            fraction = fraction.multiply(radixDecimal)
            val digit = fraction.intValue()
            digits += digit
            fraction = fraction.subtract(BigDecimal.valueOf(digit.toLong))
            i += 1
          }
          val guard = digits.remove(digits.length - 1)
          if guard * 2 >= radix then {
            var pos = digits.length - 1
            var carry = true
            while pos >= 0 && carry do {
              val next = digits(pos) + 1
              if next == radix then digits(pos) = 0
              else {
                digits(pos) = next
                carry = false
              }
              pos -= 1
            }
            if carry then
              return (if negative then "-" else "") +
                BigDecimal.valueOf(integer + 1).toBigInteger.toString(radix)
          }
          while digits.nonEmpty && digits.last == 0 do
            digits.remove(digits.length - 1)
          val fractionText = digits.iterator
            .map(d => Character.forDigit(d, radix))
            .mkString
          (if negative then "-" else "") + integerText + "." + fractionText
        }
      }

    def parseIntString(input: String, radixRaw: Int): Double = {
      var s = input.dropWhile(_.isWhitespace)
      if s.isEmpty then Double.NaN
      else {
        var sign = 1
        if s.head == '+' || s.head == '-' then {
          if s.head == '-' then sign = -1
          s = s.tail
        }
        var radix = radixRaw
        if radix == 0 then
          if s.startsWith("0x") || s.startsWith("0X") then {
            radix = 16
            s = s.drop(2)
          }
          else radix = 10
        else if radix == 16 && (s.startsWith("0x") || s.startsWith("0X")) then
          s = s.drop(2)
        if radix < 2 || radix > 36 then Double.NaN
        else {
          var value = BigInteger.ZERO
          var digits = 0
          var i = 0
          var done = false
          while i < s.length && !done do {
            val d = Character.digit(s.charAt(i), radix)
            if d < 0 then done = true
            else {
              value = value
                .multiply(BigInteger.valueOf(radix.toLong))
                .add(BigInteger.valueOf(d.toLong))
              digits += 1
              i += 1
            }
          }
          if digits == 0 then Double.NaN
          else value.multiply(BigInteger.valueOf(sign.toLong)).doubleValue()
        }
      }
    }

    def parseFloatString(input: String): Double = {
      val trimmed = input.dropWhile(_.isWhitespace)
      if trimmed.startsWith("Infinity") then Double.PositiveInfinity
      else if trimmed.startsWith("+Infinity") then Double.PositiveInfinity
      else if trimmed.startsWith("-Infinity") then Double.NegativeInfinity
      else {
        val pattern = """^[+-]?((\d+(\.\d*)?)|(\.\d+))([eE][+-]?\d+)?""".r
        pattern.findPrefixOf(trimmed) match {
          case Some(prefix) =>
            try prefix.toDouble
            catch case _: NumberFormatException => Double.NaN
          case None => Double.NaN
        }
      }
    }

    val numberIsNaN = NativeFunction(
      name = "isNaN",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        args.lift(offset) match {
          case Some(JSValue.Float64(d)) => JSValue.fromBoolean(d.isNaN)
          case Some(JSValue.Int32(_))   => JSValue.fromBoolean(false)
          case _                        => JSValue.fromBoolean(false)
        }
    )

    val numberIsFinite = NativeFunction(
      name = "isFinite",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        args.lift(offset) match {
          case Some(JSValue.Float64(d)) =>
            JSValue.fromBoolean(java.lang.Double.isFinite(d))
          case Some(JSValue.Int32(_)) => JSValue.fromBoolean(true)
          case _                      => JSValue.fromBoolean(false)
        }
    )

    val numberIsInteger = NativeFunction(
      name = "isInteger",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        args.lift(offset) match {
          case Some(JSValue.Int32(_))   => JSValue.fromBoolean(true)
          case Some(JSValue.Float64(d)) =>
            JSValue.fromBoolean(
              java.lang.Double.isFinite(d) && math.floor(d) == d
            )
          case _ => JSValue.fromBoolean(false)
        }
    )

    val numberIsSafeInteger = NativeFunction(
      name = "isSafeInteger",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        val limit = 9007199254740991.0
        args.lift(offset) match {
          case Some(JSValue.Int32(i)) =>
            JSValue.fromBoolean(math.abs(i.toLong) <= limit)
          case Some(JSValue.Float64(d)) =>
            JSValue.fromBoolean(
              java.lang.Double.isFinite(d) && math.floor(d) == d && math.abs(
                d
              ) <= limit
            )
          case _ => JSValue.fromBoolean(false)
        }
    )

    val numberPrototypeToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "toString")
        val radix =
          if args.length > 1 && args(1) != JSValue.Undefined then
            toNumber(args(1)).toInt
          else 10
        if radix < 2 || radix > 36 then
          ctx.throwRangeError("radix must be between 2 and 36")
        JSValue.fromString(numberToString(value, radix))
    )

    val numberPrototypeToFixed = NativeFunction(
      name = "toFixed",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "toFixed")
        val digits = if args.length > 1 then args(1).toNumber.toInt else 0
        if digits < 0 || digits > 100 then
          ctx.throwRangeError("invalid number of digits")
        if value.isNaN || value.isInfinite then
          JSValue.fromString(value.toString)
        else {
          val bd =
            new BigDecimal(value).setScale(digits, RoundingMode.HALF_UP)
          val text = bd.toPlainString
          JSValue.fromString(
            if value < 0 && bd.signum() == 0 && !text.startsWith("-") then
              "-" + text
            else text
          )
        }
    )

    val numberPrototypeToExponential = NativeFunction(
      name = "toExponential",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "toExponential")
        if value.isNaN || value.isInfinite then
          JSValue.fromString(value.toString)
        else {
          val hasDigits = args.length > 1 && args(1) != JSValue.Undefined
          val digits = if hasDigits then args(1).toNumber.toInt else 0
          if hasDigits && (digits < 0 || digits > 100) then
            ctx.throwRangeError("invalid number of digits")
          if !hasDigits then
            JSValue.fromString(value.toString.replace("E", "e"))
          else {
            val pattern =
              if digits == 0 then "0E0" else "0." + ("0" * digits) + "E0"
            val fmt =
              new DecimalFormat(pattern, new DecimalFormatSymbols(Locale.US))
            fmt.setRoundingMode(RoundingMode.HALF_UP)
            val formatted = fmt.format(value).replace("E", "e")
            val exponent = formatted.indexOf('e')
            JSValue.fromString(
              if exponent >= 0 && formatted.charAt(exponent + 1) != '-' then
                formatted.substring(0, exponent + 1) + "+" +
                  formatted.substring(exponent + 1)
              else formatted
            )
          }
        }
    )

    val numberPrototypeToPrecision = NativeFunction(
      name = "toPrecision",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "toPrecision")
        if args.length < 2 || args(1) == JSValue.Undefined then
          JSValue.fromString(value.toString.replace("E", "e"))
        else if value.isNaN || value.isInfinite then
          JSValue.fromString(value.toString)
        else {
          val precision = args(1).toNumber.toInt
          if precision < 1 || precision > 100 then
            ctx.throwRangeError("invalid number of digits")
          val mc = MathContext(precision, RoundingMode.HALF_UP)
          val bd = BigDecimal.valueOf(value).round(mc)
          JSValue.fromString(bd.toString.replace("E", "e"))
        }
    )

    val numberPrototypeValueOf = NativeFunction(
      name = "valueOf",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "valueOf")
        JSValue.fromDouble(value)
    )

    val numberPrototypeToLocaleString = NativeFunction(
      name = "toLocaleString",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "toLocaleString")
        JSValue.fromString(value.toString.replace("E", "e"))
    )

    val booleanPrototypeToString = NativeFunction(
      name = "toString",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisBoolean(args, "toString")
        JSValue.fromString(if value then "true" else "false")
    )

    val booleanPrototypeValueOf = NativeFunction(
      name = "valueOf",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisBoolean(args, "valueOf")
        JSValue.fromBoolean(value)
    )

    val parseIntFunc = NativeFunction(
      name = "parseInt",
      length = 2,
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        val input = if args.length > offset then args(offset).toString else ""
        val radix =
          if args.length > offset + 1 then args(offset + 1).toNumber.toInt
          else 0
        JSValue.fromDouble(parseIntString(input, radix))
    )

    val parseFloatFunc = NativeFunction(
      name = "parseFloat",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        val input = if args.length > offset then args(offset).toString else ""
        JSValue.fromDouble(parseFloatString(input))
    )

    numberConstructor.funcObj.defineProperty(
      "MAX_VALUE",
      JSValue.fromDouble(1.7976931348623157e+308),
      enumerable = false
    )
    numberConstructor.funcObj.defineProperty(
      "MIN_VALUE",
      JSValue.fromDouble(5e-324),
      enumerable = false
    )
    numberConstructor.funcObj.defineProperty(
      "NaN",
      JSValue.Float64(Double.NaN),
      enumerable = false
    )
    numberConstructor.funcObj.defineProperty(
      "NEGATIVE_INFINITY",
      JSValue.Float64(Double.NegativeInfinity),
      enumerable = false
    )
    numberConstructor.funcObj.defineProperty(
      "POSITIVE_INFINITY",
      JSValue.Float64(Double.PositiveInfinity),
      enumerable = false
    )
    numberConstructor.funcObj.defineProperty(
      "EPSILON",
      JSValue.fromDouble(2.220446049250313e-16),
      enumerable = false
    )
    numberConstructor.funcObj.defineProperty(
      "MAX_SAFE_INTEGER",
      JSValue.fromDouble(9007199254740991.0),
      enumerable = false
    )
    numberConstructor.funcObj.defineProperty(
      "MIN_SAFE_INTEGER",
      JSValue.fromDouble(-9007199254740991.0),
      enumerable = false
    )
    numberConstructor.funcObj.set("parseInt", JSValue.Native(parseIntFunc))
    numberConstructor.funcObj.set("parseFloat", JSValue.Native(parseFloatFunc))
    numberConstructor.funcObj.set("isNaN", JSValue.Native(numberIsNaN))
    numberConstructor.funcObj.set("isFinite", JSValue.Native(numberIsFinite))
    numberConstructor.funcObj.set("isInteger", JSValue.Native(numberIsInteger))
    numberConstructor.funcObj.set(
      "isSafeInteger",
      JSValue.Native(numberIsSafeInteger)
    )

    numberPrototype.defineProperty(
      "toString",
      JSValue.Native(numberPrototypeToString),
      enumerable = false
    )
    numberPrototype.defineProperty(
      "toFixed",
      JSValue.Native(numberPrototypeToFixed),
      enumerable = false
    )
    numberPrototype.defineProperty(
      "toExponential",
      JSValue.Native(numberPrototypeToExponential),
      enumerable = false
    )
    numberPrototype.defineProperty(
      "toPrecision",
      JSValue.Native(numberPrototypeToPrecision),
      enumerable = false
    )
    numberPrototype.defineProperty(
      "valueOf",
      JSValue.Native(numberPrototypeValueOf),
      enumerable = false
    )
    numberPrototype.defineProperty(
      "toLocaleString",
      JSValue.Native(numberPrototypeToLocaleString),
      enumerable = false
    )

    booleanPrototype.defineProperty(
      "toString",
      JSValue.Native(booleanPrototypeToString),
      enumerable = false
    )
    booleanPrototype.defineProperty(
      "valueOf",
      JSValue.Native(booleanPrototypeValueOf),
      enumerable = false
    )

    ctx.global.set("parseInt", JSValue.Native(parseIntFunc))
    ctx.global.set("parseFloat", JSValue.Native(parseFloatFunc))

    // Global isNaN (coerces to number first, unlike Number.isNaN)
    val globalIsNaN = NativeFunction(
      name = "isNaN",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.lift(1) match
          case Some(v) => JSValue.fromBoolean(v.toNumber.isNaN)
          case None    => JSValue.fromBoolean(true)  // isNaN(undefined) = true
    )
    ctx.global.set("isNaN", JSValue.Native(globalIsNaN))

    // Global isFinite (coerces to number first, unlike Number.isFinite)
    val globalIsFinite = NativeFunction(
      name = "isFinite",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.lift(1) match
          case Some(v) =>
            val n = v.toNumber
            JSValue.fromBoolean(!n.isNaN && !n.isInfinite)
          case None => JSValue.fromBoolean(false)  // isFinite(undefined) = false
    )
    ctx.global.set("isFinite", JSValue.Native(globalIsFinite))

    val componentUnescaped =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.!~*'()"
    val uriReserved = ";/?:@&=+$,#"

    def encodeUriText(input: String, preserveReserved: Boolean)(using
        JSContext
    ): String = {
      val safe =
        if preserveReserved then componentUnescaped + uriReserved
        else componentUnescaped
      val out = new StringBuilder
      var index = 0
      while index < input.length do {
        val ch = input.charAt(index)
        val codePoint =
          if Character.isHighSurrogate(ch) then {
            if index + 1 >= input.length ||
                !Character.isLowSurrogate(input.charAt(index + 1))
            then ctx.throwError(quickjs.runtime.ErrorType.URIError, "Malformed URI")
            val cp = Character.toCodePoint(ch, input.charAt(index + 1))
            index += 2
            cp
          }
          else if Character.isLowSurrogate(ch) then
            ctx.throwError(quickjs.runtime.ErrorType.URIError, "Malformed URI")
          else {
            index += 1
            ch.toInt
          }
        if codePoint < 128 && safe.indexOf(codePoint.toChar) >= 0 then
          out.append(codePoint.toChar)
        else {
          val bytes = new String(Character.toChars(codePoint))
            .getBytes(java.nio.charset.StandardCharsets.UTF_8)
          bytes.foreach { raw =>
            out.append('%')
            out.append(f"${raw & 0xff}%02X")
          }
        }
      }
      out.toString
    }

    def decodeUriText(input: String, preserveReserved: Boolean)(using
        JSContext
    ): String = {
      def hexDigit(ch: Char): Int = Character.digit(ch, 16)
      def byteAt(position: Int): Int = {
        if position + 2 >= input.length || input.charAt(position) != '%' then
          ctx.throwError(quickjs.runtime.ErrorType.URIError, "Malformed URI")
        val hi = hexDigit(input.charAt(position + 1))
        val lo = hexDigit(input.charAt(position + 2))
        if hi < 0 || lo < 0 then
          ctx.throwError(quickjs.runtime.ErrorType.URIError, "Malformed URI")
        (hi << 4) | lo
      }
      val out = new StringBuilder
      var index = 0
      while index < input.length do {
        if input.charAt(index) != '%' then {
          out.append(input.charAt(index))
          index += 1
        }
        else {
          val first = byteAt(index)
          if first < 0x80 then {
            val decoded = first.toChar
            if preserveReserved && uriReserved.indexOf(decoded) >= 0 then
              out.append(input.substring(index, index + 3))
            else out.append(decoded)
            index += 3
          }
          else {
            val count =
              if (first & 0xe0) == 0xc0 then 2
              else if (first & 0xf0) == 0xe0 then 3
              else if (first & 0xf8) == 0xf0 then 4
              else ctx.throwError(quickjs.runtime.ErrorType.URIError, "Malformed URI")
            val bytes = new Array[Byte](count)
            bytes(0) = first.toByte
            var j = 1
            while j < count do {
              val next = byteAt(index + j * 3)
              if (next & 0xc0) != 0x80 then
                ctx.throwError(quickjs.runtime.ErrorType.URIError, "Malformed URI")
              bytes(j) = next.toByte
              j += 1
            }
            val decoder = java.nio.charset.StandardCharsets.UTF_8
              .newDecoder()
              .onMalformedInput(java.nio.charset.CodingErrorAction.REPORT)
              .onUnmappableCharacter(java.nio.charset.CodingErrorAction.REPORT)
            try out.append(decoder.decode(java.nio.ByteBuffer.wrap(bytes)))
            catch
              case _: java.nio.charset.CharacterCodingException =>
                ctx.throwError(quickjs.runtime.ErrorType.URIError, "Malformed URI")
            index += count * 3
          }
        }
      }
      out.toString
    }

    def uriFunction(name: String, encode: Boolean, preserveReserved: Boolean) =
      NativeFunction(
        name = name,
        length = 1,
        impl = (args, ctx) =>
          given JSContext = ctx
          val offset = if args.length >= 2 then 1 else 0
          val input = args.lift(offset).getOrElse(JSValue.Undefined).toString
          JSValue.fromString(
            if encode then encodeUriText(input, preserveReserved)
            else decodeUriText(input, preserveReserved)
          )
      )

    ctx.global.set("encodeURI", JSValue.Native(uriFunction("encodeURI", true, true)))
    ctx.global.set(
      "encodeURIComponent",
      JSValue.Native(uriFunction("encodeURIComponent", true, false))
    )
    ctx.global.set("decodeURI", JSValue.Native(uriFunction("decodeURI", false, true)))
    ctx.global.set(
      "decodeURIComponent",
      JSValue.Native(uriFunction("decodeURIComponent", false, false))
    )

    def requireThisString(args: Array[JSValue], method: String)(using
        JSContext
    ): String =
      if args.isEmpty then
        ctx.throwTypeError(
          s"String.prototype.$method called on null or undefined"
        )
      else
        args(0) match {
          case JSValue.Null | JSValue.Undefined =>
            ctx.throwTypeError(
              s"String.prototype.$method called on null or undefined"
            )
          case other => BuiltinHelpers.toJSString(other)
        }

    def requireStringValue(args: Array[JSValue], method: String)(using
        JSContext
    ): String =
      if args.isEmpty then ctx.throwTypeError(s"String.prototype.$method called on incompatible receiver")
      else args(0) match {
        case JSValue.JSStr(value) => value
        case JSValue.Object(obj) => obj.getPrimitiveValue match {
          case Some(JSValue.JSStr(value)) => value
          case _ => ctx.throwTypeError(s"String.prototype.$method called on incompatible receiver")
        }
        case _ => ctx.throwTypeError(s"String.prototype.$method called on incompatible receiver")
      }

    def isEcmaWhitespace(ch: Char): Boolean =
      Character.isWhitespace(ch) || Character.isSpaceChar(ch) || ch == '\ufeff'

    def trimString(text: String, start: Boolean, end: Boolean): String = {
      var from = 0
      var until = text.length
      if start then
        while from < until && isEcmaWhitespace(text.charAt(from)) do from += 1
      if end then
        while until > from && isEcmaWhitespace(text.charAt(until - 1)) do until -= 1
      text.substring(from, until)
    }

    def expandReplacement(
        replacement: String,
        input: String,
        matcher: java.util.regex.Matcher
    ): String = {
      val sb = new StringBuilder()
      var i = 0
      while i < replacement.length do {
        val ch = replacement.charAt(i)
        if ch == '$' && i + 1 < replacement.length then {
          val next = replacement.charAt(i + 1)
          next match {
            case '$' =>
              sb.append('$')
              i += 2
            case '&' =>
              sb.append(matcher.group())
              i += 2
            case '`' =>
              sb.append(input.substring(0, matcher.start()))
              i += 2
            case '\'' =>
              sb.append(input.substring(matcher.end()))
              i += 2
            case d if d >= '0' && d <= '9' =>
              var j = i + 1
              var groupNum = 0
              var count = 0
              while j < replacement.length && count < 2 && replacement
                  .charAt(j)
                  .isDigit
              do {
                groupNum = groupNum * 10 + (replacement.charAt(j) - '0')
                j += 1
                count += 1
              }
              if groupNum > 0 && groupNum <= matcher.groupCount() then {
                val groupVal = matcher.group(groupNum)
                if groupVal != null then sb.append(groupVal)
              }
              i = j
            case _ =>
              sb.append('$').append(next)
              i += 2
          }
        }
        else {
          sb.append(ch)
          i += 1
        }
      }
      sb.toString()
    }

    val stringPrototypeSplit = NativeFunction(
      name = "split",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "split")
        val separator = if args.length > 1 then args(1) else JSValue.Undefined
        val limitLong =
          if args.length <= 2 || args(2) == JSValue.Undefined then 0xffffffffL
          else {
            val number = toNumber(args(2))
            if number.isNaN || number == 0 || number.isInfinite then 0L
            else {
              val integer = math.signum(number) * math.floor(math.abs(number))
              val modulo = integer % 4294967296.0
              (if modulo < 0 then modulo + 4294967296.0 else modulo).toLong
            }
          }
        val limit = math.min(limitLong, Int.MaxValue.toLong).toInt
        val result = quickjs.objmodel.JSArray.empty()
        if limit == 0 then JSValue.JSArrayVal(result)
        else if separator == JSValue.Undefined then {
          result.push(JSValue.fromString(str))
          JSValue.JSArrayVal(result)
        }
        else {
          getRegExpData(separator) match {
            case Some((_, data)) =>
              val matcher = data.regex.matcher(str)
              var lastEnd = 0
              while matcher.find() && result.getLength < limit do {
                if result.getLength < limit then
                  result.push(
                    JSValue.fromString(str.substring(lastEnd, matcher.start()))
                  )
                var groupIndex = 1
                while groupIndex <= matcher
                    .groupCount() && result.getLength < limit
                do {
                  val groupVal = matcher.group(groupIndex)
                  result.push(
                    if groupVal == null then JSValue.Undefined
                    else JSValue.fromString(groupVal)
                  )
                  groupIndex += 1
                }
                lastEnd = matcher.end()
              }
              if result.getLength < limit then
                result.push(JSValue.fromString(str.substring(lastEnd)))
            case None =>
              val sepStr = toJSString(separator)
              if sepStr.isEmpty then {
                var i = 0
                while i < str.length && i < limit do {
                  result.push(JSValue.fromString(str.charAt(i).toString))
                  i += 1
                }
              }
              else {
                var position = 0
                var done = false
                while !done && result.getLength < limit do {
                  val next = str.indexOf(sepStr, position)
                  if next < 0 then {
                    result.push(JSValue.fromString(str.substring(position)))
                    done = true
                  }
                  else {
                    result.push(
                      JSValue.fromString(str.substring(position, next))
                    )
                    position = next + sepStr.length
                  }
                }
              }
          }
          JSValue.JSArrayVal(result)
        }
    )

    val stringPrototypeTrim = NativeFunction(
      name = "trim",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "trim")
        JSValue.fromString(trimString(str, start = true, end = true))
    )

    val stringPrototypeToLowerCase = NativeFunction(
      name = "toLowerCase",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "toLowerCase")
        JSValue.fromString(str.toLowerCase(Locale.ROOT))
    )

    val stringPrototypeToUpperCase = NativeFunction(
      name = "toUpperCase",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "toUpperCase")
        JSValue.fromString(str.toUpperCase(Locale.ROOT))
    )

    val stringPrototypeToLocaleLowerCase = NativeFunction(
      name = "toLocaleLowerCase",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "toLocaleLowerCase")
        JSValue.fromString(str.toLowerCase(Locale.ROOT))
    )

    val stringPrototypeToLocaleUpperCase = NativeFunction(
      name = "toLocaleUpperCase",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "toLocaleUpperCase")
        JSValue.fromString(str.toUpperCase(Locale.ROOT))
    )

    val stringPrototypeToString = NativeFunction(
      name = "toString",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        JSValue.fromString(requireStringValue(args, "toString"))
    )

    val stringPrototypeValueOf = NativeFunction(
      name = "valueOf",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        JSValue.fromString(requireStringValue(args, "valueOf"))
    )

    val stringPrototypeReplace = NativeFunction(
      name = "replace",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "replace")
        if args.length < 2 then JSValue.fromString(str)
        else {
          val replaceValue = if args.length > 2 then args(2) else JSValue.Undefined
          val functional = BuiltinHelpers.isCallable(replaceValue)
          lazy val replacement = toJSString(replaceValue)
          def replacementFor(matcher: java.util.regex.Matcher): String =
            if !functional then expandReplacement(replacement, str, matcher)
            else {
              val callArgs = mutable.ArrayBuffer[JSValue](JSValue.fromString(matcher.group()))
              var group = 1
              while group <= matcher.groupCount() do {
                val capture = matcher.group(group)
                callArgs += (if capture == null then JSValue.Undefined else JSValue.fromString(capture))
                group += 1
              }
              callArgs += JSValue.fromInt(matcher.start())
              callArgs += JSValue.fromString(str)
              toJSString(BuiltinHelpers.callFunctionWithThis(
                replaceValue, JSValue.Undefined, callArgs.toArray
              ))
            }
          getRegExpData(args(1)) match {
            case Some((_, data)) =>
              val matcher = data.regex.matcher(str)
              val sb = new StringBuilder()
              var lastEnd = 0
              var replaced = false
              while matcher.find() && (data.global || !replaced) do {
                sb.append(str.substring(lastEnd, matcher.start()))
                sb.append(replacementFor(matcher))
                lastEnd = matcher.end()
                replaced = true
              }
              if replaced then {
                sb.append(str.substring(lastEnd))
                JSValue.fromString(sb.toString())
              }
              else JSValue.fromString(str)
            case None =>
              val search = toJSString(args(1))
              val idx = str.indexOf(search)
              if idx < 0 then JSValue.fromString(str)
              else {
                val matcher = java.util.regex.Pattern.quote(search)
                val pattern = java.util.regex.Pattern.compile(matcher)
                val m = pattern.matcher(str)
                m.find()
                val replaced = replacementFor(m)
                val updated = str.substring(0, idx) + replaced + str.substring(
                  idx + search.length
                )
                JSValue.fromString(updated)
              }
          }
        }
    )

    val stringPrototypeReplaceAll = NativeFunction(
      name = "replaceAll",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "replaceAll")
        if args.length < 2 then JSValue.fromString(str)
        else {
          val replaceValue = if args.length > 2 then args(2) else JSValue.Undefined
          val functional = BuiltinHelpers.isCallable(replaceValue)
          lazy val replacement = toJSString(replaceValue)
          def replacementFor(matcher: java.util.regex.Matcher): String =
            if !functional then expandReplacement(replacement, str, matcher)
            else {
              val callArgs = mutable.ArrayBuffer[JSValue](JSValue.fromString(matcher.group()))
              var group = 1
              while group <= matcher.groupCount() do {
                val capture = matcher.group(group)
                callArgs += (if capture == null then JSValue.Undefined else JSValue.fromString(capture))
                group += 1
              }
              callArgs += JSValue.fromInt(matcher.start())
              callArgs += JSValue.fromString(str)
              toJSString(BuiltinHelpers.callFunctionWithThis(
                replaceValue, JSValue.Undefined, callArgs.toArray
              ))
            }
          getRegExpData(args(1)) match {
            case Some((_, data)) =>
              if !data.global then
                ctx.throwTypeError("replaceAll with non-global RegExp")
              val matcher = data.regex.matcher(str)
              val sb = new StringBuilder()
              var lastEnd = 0
              while matcher.find() do {
                sb.append(str.substring(lastEnd, matcher.start()))
                sb.append(replacementFor(matcher))
                lastEnd = matcher.end()
              }
              sb.append(str.substring(lastEnd))
              JSValue.fromString(sb.toString)
            case None =>
              val search = toJSString(args(1))
              if search.isEmpty then {
                val sb = new StringBuilder()
                var i = 0
                while i < str.length do {
                  val emptyMatcher = java.util.regex.Pattern.compile("").matcher(str)
                  emptyMatcher.find(i)
                  sb.append(replacementFor(emptyMatcher))
                  sb.append(str.charAt(i))
                  i += 1
                }
                val finalMatcher = java.util.regex.Pattern.compile("").matcher(str)
                finalMatcher.find(str.length)
                sb.append(replacementFor(finalMatcher))
                JSValue.fromString(sb.toString)
              }
              else {
                val pattern = java.util.regex.Pattern
                  .compile(java.util.regex.Pattern.quote(search))
                val matcher = pattern.matcher(str)
                val sb = new StringBuilder()
                var lastEnd = 0
                while matcher.find() do {
                  sb.append(str.substring(lastEnd, matcher.start()))
                  sb.append(replacementFor(matcher))
                  lastEnd = matcher.end()
                }
                sb.append(str.substring(lastEnd))
                JSValue.fromString(sb.toString)
              }
          }
        }
    )

    val stringPrototypeIncludes = NativeFunction(
      name = "includes",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "includes")
        val searchValue = if args.length > 1 then args(1) else JSValue.Undefined
        if getRegExpData(searchValue).nonEmpty then
          ctx.throwTypeError("regexp not supported")
        val search = toJSString(searchValue)
        val rawPos =
          if args.length > 2 then toNumber(args(2)).toInt else 0
        val pos = math.min(math.max(rawPos, 0), str.length)
        JSValue.fromBoolean(str.indexOf(search, pos) >= 0)
    )

    val stringPrototypeMatch = NativeFunction(
      name = "match",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "match")
        val pattern = if args.length > 1 then args(1) else JSValue.Undefined
        if pattern == JSValue.Undefined then {
          val arr = quickjs.objmodel.JSArray.empty()
          arr.push(JSValue.fromString(str))
          JSValue.JSArrayVal(arr)
        }
        else
          getRegExpData(pattern) match {
            case Some((_, data)) =>
              val matcher = data.regex.matcher(str)
              if data.global then {
                val arr = quickjs.objmodel.JSArray.empty()
                var start = 0
                while matcher.find(start) do {
                  arr.push(JSValue.fromString(matcher.group()))
                  val end = matcher.end()
                  start = if end == start then start + 1 else end
                }
                if arr.getLength == 0 then JSValue.Null
                else JSValue.JSArrayVal(arr)
              }
              else if matcher.find() then {
                val arr = quickjs.objmodel.JSArray.empty()
                var i = 0
                while i <= matcher.groupCount() do {
                  val group = matcher.group(i)
                  arr.push(
                    if group == null then JSValue.Undefined
                    else JSValue.fromString(group)
                  )
                  i += 1
                }
                arr.setProperty("index", JSValue.fromInt(matcher.start()))
                arr.setProperty("input", JSValue.fromString(str))
                arr.setProperty("groups", JSValue.Undefined)
                JSValue.JSArrayVal(arr)
              }
              else JSValue.Null
            case None =>
              val needle = pattern.toString
              val idx = str.indexOf(needle)
              if idx < 0 then JSValue.Null
              else {
                val arr = quickjs.objmodel.JSArray.empty()
                arr.push(JSValue.fromString(needle))
                JSValue.JSArrayVal(arr)
              }
          }
    )

    val stringPrototypeSearch = NativeFunction(
      name = "search",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "search")
        val pattern = if args.length > 1 then args(1) else JSValue.Undefined
        getRegExpData(pattern) match {
          case Some((_, data)) =>
            val matcher = data.regex.matcher(str)
            if matcher.find(0) then JSValue.fromInt(matcher.start())
            else JSValue.fromInt(-1)
          case None =>
            val needle = pattern.toString
            JSValue.fromInt(str.indexOf(needle))
        }
    )

    val stringPrototypeMatchAll = NativeFunction(
      name = "matchAll",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "matchAll")
        val patternValue =
          if args.length > 1 then args(1) else JSValue.Undefined
        val dataOpt =
          getRegExpData(patternValue) match {
            case Some((_, data)) => Some(data)
            case None            =>
              val pattern = patternValue.toString
              val (patternFlags, _, _, _, _, _, _) = parseRegExpFlags("g")
              val regex = java.util.regex.Pattern.compile(pattern, patternFlags)
              Some(
                RegExpData(
                  pattern,
                  "g",
                  global = true,
                  ignoreCase = false,
                  multiline = false,
                  dotAll = false,
                  unicode = false,
                  sticky = false,
                  regex
                )
              )
          }
        val resultArr = quickjs.objmodel.JSArray.empty()
        dataOpt match {
          case Some(data) =>
            val matcher = data.regex.matcher(str)
            var start = 0
            while matcher.find(start) do {
              val arr = quickjs.objmodel.JSArray.empty()
              var i = 0
              while i <= matcher.groupCount() do {
                val group = matcher.group(i)
                arr.push(
                  if group == null then JSValue.Undefined
                  else JSValue.fromString(group)
                )
                i += 1
              }
              arr.setProperty("index", JSValue.fromInt(matcher.start()))
              arr.setProperty("input", JSValue.fromString(str))
              arr.setProperty("groups", JSValue.Undefined)
              resultArr.push(JSValue.JSArrayVal(arr))
              val end = matcher.end()
              start = if end == start then start + 1 else end
            }
          case None => ()
        }
        JSValue.JSArrayVal(resultArr)
    )

    val stringPrototypeIndexOf = NativeFunction(
      name = "indexOf",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "indexOf")
        val search = toJSString(if args.length > 1 then args(1) else JSValue.Undefined)
        val rawPos = toIntegerOrInfinity(if args.length > 2 then args(2) else JSValue.Undefined)
        val pos = if rawPos <= 0 || rawPos.isNaN then 0
          else if rawPos >= str.length then str.length else rawPos.toInt
        JSValue.fromInt(str.indexOf(search, pos))
    )

    val stringPrototypeLastIndexOf = NativeFunction(
      name = "lastIndexOf",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "lastIndexOf")
        val search = toJSString(if args.length > 1 then args(1) else JSValue.Undefined)
        val numericPosition = if args.length > 2 then toNumber(args(2)) else Double.PositiveInfinity
        val rawPos = if numericPosition.isNaN then Double.PositiveInfinity
          else if numericPosition == 0 then numericPosition
          else math.signum(numericPosition) * math.floor(math.abs(numericPosition))
        val pos =
          if rawPos.isNaN then str.length
          else math.min(math.max(rawPos.toInt, 0), str.length)
        JSValue.fromInt(str.lastIndexOf(search, pos))
    )

    val stringPrototypeSlice = NativeFunction(
      name = "slice",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "slice")
        val len = str.length
        val startRaw = toIntegerOrInfinity(if args.length > 1 then args(1) else JSValue.Undefined)
        val endRaw = if args.length > 2 && args(2) != JSValue.Undefined then toIntegerOrInfinity(args(2)) else len.toDouble
        def clampIndex(idx: Double): Int =
          if idx.isNegInfinity then 0
          else if idx < 0 then math.max(len.toDouble + idx, 0).toInt
          else if idx.isPosInfinity then len
          else math.min(idx, len).toInt
        val start = clampIndex(startRaw)
        val end = clampIndex(endRaw)
        if end <= start then JSValue.fromString("")
        else JSValue.fromString(str.substring(start, end))
    )

    val stringPrototypeSubstring = NativeFunction(
      name = "substring",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "substring")
        val len = str.length
        val startRaw = toIntegerOrInfinity(if args.length > 1 then args(1) else JSValue.Undefined)
        val endRaw = if args.length > 2 && args(2) != JSValue.Undefined then toIntegerOrInfinity(args(2)) else len.toDouble
        def clamp(value: Double): Int =
          if value <= 0 || value.isNaN then 0
          else if value >= len then len else value.toInt
        val start = clamp(startRaw)
        val end = clamp(endRaw)
        val (from, to) = if start <= end then (start, end) else (end, start)
        JSValue.fromString(str.substring(from, to))
    )

    // String.prototype.substr(start, length) - deprecated but ES5
    val stringPrototypeSubstr = NativeFunction(
      name = "substr",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "substr")
        val len = str.length
        val startRaw = if args.length > 1 then args(1).toNumber.toInt else 0
        // Handle negative start (counts from end)
        val start =
          if startRaw < 0 then math.max(0, len + startRaw)
          else math.min(startRaw, len)
        // Length defaults to rest of string
        val length = if args.length > 2 then {
          val l = args(2).toNumber.toInt
          math.max(0, l)
        }
        else len - start
        val end = math.min(start + length, len)
        if start >= len || length <= 0 then JSValue.fromString("")
        else JSValue.fromString(str.substring(start, end))
    )

    val stringPrototypeCharAt = NativeFunction(
      name = "charAt",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "charAt")
        val index = toIntegerOrInfinity(if args.length > 1 then args(1) else JSValue.Undefined)
        if index < 0 || index >= str.length || index.isInfinite then JSValue.fromString("")
        else JSValue.fromString(str.charAt(index.toInt).toString)
    )

    val stringPrototypeCharCodeAt = NativeFunction(
      name = "charCodeAt",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "charCodeAt")
        val index = toIntegerOrInfinity(if args.length > 1 then args(1) else JSValue.Undefined)
        if index < 0 || index >= str.length || index.isInfinite then JSValue.Float64(Double.NaN)
        else JSValue.fromInt(str.charAt(index.toInt).toInt)
    )

    val stringPrototypeCodePointAt = NativeFunction(
      name = "codePointAt",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "codePointAt")
        val number = if args.length > 1 then toNumber(args(1)) else 0.0
        val index =
          if number.isNaN then 0L
          else if number.isInfinite then
            if number > 0 then Long.MaxValue else Long.MinValue
          else number.toLong
        if index < 0 || index >= str.length then JSValue.Undefined
        else JSValue.fromInt(str.codePointAt(index.toInt))
    )

    val stringPrototypeAt = NativeFunction(
      name = "at",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "at")
        val relative = toIntegerOrInfinity(
          if args.length > 1 then args(1) else JSValue.Undefined
        )
        if relative.isInfinite then JSValue.Undefined
        else {
          val index = if relative >= 0 then relative else str.length + relative
          if index < 0 || index >= str.length then JSValue.Undefined
          else JSValue.fromString(str.charAt(index.toInt).toString)
        }
    )

    val stringPrototypeConcat = NativeFunction(
      name = "concat",
      impl = (args, ctx) =>
        given JSContext = ctx
        val base = requireThisString(args, "concat")
        if args.length <= 1 then JSValue.fromString(base)
        else {
          val sb = new StringBuilder(base)
          var i = 1
          while i < args.length do {
            sb.append(BuiltinHelpers.toJSString(args(i)))
            i += 1
          }
          JSValue.fromString(sb.toString)
        }
    )

    val stringPrototypeRepeat = NativeFunction(
      name = "repeat",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "repeat")
        val countRaw = if args.length > 1 then args(1).toNumber else 0.0
        if countRaw.isNaN then JSValue.fromString("")
        else if countRaw < 0 || countRaw.isInfinite then
          ctx.throwRangeError("Invalid count value")
        else {
          val count = math.floor(countRaw).toInt
          if count == 0 then JSValue.fromString("")
          else JSValue.fromString(str.repeat(count))
        }
    )

    val stringPrototypeLocaleCompare = NativeFunction(
      name = "localeCompare",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "localeCompare")
        val compare = if args.length > 1 then args(1).toString else ""
        val result = str.compareTo(compare)
        if result < 0 then JSValue.fromInt(-1)
        else if result > 0 then JSValue.fromInt(1)
        else JSValue.fromInt(0)
    )

    val stringPrototypeTrimStart = NativeFunction(
      name = "trimStart",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "trimStart")
        JSValue.fromString(trimString(str, start = true, end = false))
    )

    val stringPrototypeTrimEnd = NativeFunction(
      name = "trimEnd",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "trimEnd")
        JSValue.fromString(trimString(str, start = false, end = true))
    )

    val stringPrototypeStartsWith = NativeFunction(
      name = "startsWith",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "startsWith")
        val search =
          toJSString(if args.length > 1 then args(1) else JSValue.Undefined)
        val position =
          if args.length > 2 then
            math.max(0, toNumber(args(2)).toInt)
          else 0
        JSValue.fromBoolean(str.startsWith(search, position))
    )

    val stringPrototypeEndsWith = NativeFunction(
      name = "endsWith",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "endsWith")
        val search =
          toJSString(if args.length > 1 then args(1) else JSValue.Undefined)
        val endPos =
          if args.length > 2 then toNumber(args(2)).toInt
          else str.length
        val clamped = math.min(math.max(endPos, 0), str.length)
        JSValue.fromBoolean(str.substring(0, clamped).endsWith(search))
    )

    val stringPrototypePadStart = NativeFunction(
      name = "padStart",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "padStart")
        val targetLength = if args.length > 1 then args(1).toNumber.toInt else 0
        val padString =
          if args.length > 2 && args(2) != JSValue.Undefined then
            args(2).toString
          else " "
        if targetLength <= str.length || padString.isEmpty then
          JSValue.fromString(str)
        else {
          val padNeeded = targetLength - str.length
          val repeatCount =
            (padNeeded + padString.length - 1) / padString.length
          val pad = padString.repeat(repeatCount).substring(0, padNeeded)
          JSValue.fromString(pad + str)
        }
    )

    val stringPrototypePadEnd = NativeFunction(
      name = "padEnd",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "padEnd")
        val targetLength = if args.length > 1 then args(1).toNumber.toInt else 0
        val padString =
          if args.length > 2 && args(2) != JSValue.Undefined then
            args(2).toString
          else " "
        if targetLength <= str.length || padString.isEmpty then
          JSValue.fromString(str)
        else {
          val padNeeded = targetLength - str.length
          val repeatCount =
            (padNeeded + padString.length - 1) / padString.length
          val pad = padString.repeat(repeatCount).substring(0, padNeeded)
          JSValue.fromString(str + pad)
        }
    )

    val stringPrototypeIsWellFormed = NativeFunction(
      name = "isWellFormed",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "isWellFormed")
        var result = true
        var i = 0
        while i < str.length && result do {
          val c = str(i).toInt
          if c >= 0xd800 && c <= 0xdbff then
            // Lead surrogate - must be followed by trail surrogate
            if i + 1 >= str.length then result = false
            else {
              val next = str(i + 1).toInt
              if next < 0xdc00 || next > 0xdfff then result = false
              else i += 1
            }
          else if c >= 0xdc00 && c <= 0xdfff then
            // Trail surrogate without lead - not well-formed
            result = false
          i += 1
        }
        JSValue.Bool(result)
    )

    val stringPrototypeToWellFormed = NativeFunction(
      name = "toWellFormed",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "toWellFormed")
        val sb = new StringBuilder
        var i = 0
        while i < str.length do {
          val c = str(i).toInt
          if c >= 0xd800 && c <= 0xdbff then
            // Lead surrogate - must be followed by trail surrogate
            if i + 1 < str.length then {
              val next = str(i + 1).toInt
              if next >= 0xdc00 && next <= 0xdfff then {
                sb.append(str(i))
                sb.append(str(i + 1))
                i += 2
              }
              else {
                sb.append('\uFFFD')
                i += 1
              }
            }
            else {
              sb.append('\uFFFD')
              i += 1
            }
          else if c >= 0xdc00 && c <= 0xdfff then {
            // Lone trail surrogate
            sb.append('\uFFFD')
            i += 1
          }
          else {
            sb.append(str(i))
            i += 1
          }
        }
        JSValue.fromString(sb.toString)
    )

    stringPrototype.defineProperty(
      "split",
      JSValue.Native(stringPrototypeSplit),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "trim",
      JSValue.Native(stringPrototypeTrim),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "toLowerCase",
      JSValue.Native(stringPrototypeToLowerCase),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "toUpperCase",
      JSValue.Native(stringPrototypeToUpperCase),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "toLocaleLowerCase",
      JSValue.Native(stringPrototypeToLocaleLowerCase),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "toLocaleUpperCase",
      JSValue.Native(stringPrototypeToLocaleUpperCase),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "toString",
      JSValue.Native(stringPrototypeToString),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "valueOf",
      JSValue.Native(stringPrototypeValueOf),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "replace",
      JSValue.Native(stringPrototypeReplace),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "replaceAll",
      JSValue.Native(stringPrototypeReplaceAll),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "includes",
      JSValue.Native(stringPrototypeIncludes),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "match",
      JSValue.Native(stringPrototypeMatch),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "search",
      JSValue.Native(stringPrototypeSearch),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "matchAll",
      JSValue.Native(stringPrototypeMatchAll),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "indexOf",
      JSValue.Native(stringPrototypeIndexOf),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "lastIndexOf",
      JSValue.Native(stringPrototypeLastIndexOf),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "slice",
      JSValue.Native(stringPrototypeSlice),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "substring",
      JSValue.Native(stringPrototypeSubstring),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "substr",
      JSValue.Native(stringPrototypeSubstr),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "charAt",
      JSValue.Native(stringPrototypeCharAt),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "charCodeAt",
      JSValue.Native(stringPrototypeCharCodeAt),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "codePointAt",
      JSValue.Native(stringPrototypeCodePointAt),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "at",
      JSValue.Native(stringPrototypeAt),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "concat",
      JSValue.Native(stringPrototypeConcat),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "repeat",
      JSValue.Native(stringPrototypeRepeat),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "localeCompare",
      JSValue.Native(stringPrototypeLocaleCompare),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "trimStart",
      JSValue.Native(stringPrototypeTrimStart),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "trimLeft",
      JSValue.Native(stringPrototypeTrimStart),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "trimEnd",
      JSValue.Native(stringPrototypeTrimEnd),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "trimRight",
      JSValue.Native(stringPrototypeTrimEnd),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "startsWith",
      JSValue.Native(stringPrototypeStartsWith),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "endsWith",
      JSValue.Native(stringPrototypeEndsWith),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "padStart",
      JSValue.Native(stringPrototypePadStart),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "padEnd",
      JSValue.Native(stringPrototypePadEnd),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "isWellFormed",
      JSValue.Native(stringPrototypeIsWellFormed),
      enumerable = false
    )
    stringPrototype.defineProperty(
      "toWellFormed",
      JSValue.Native(stringPrototypeToWellFormed),
      enumerable = false
    )

    val stringRaw = NativeFunction(
      name = "raw",
      impl = (args, ctx) =>
        given JSContext = ctx
        val offset = if args.nonEmpty then 1 else 0
        if args.length <= offset then JSValue.fromString("")
        else {
          def getProp(value: JSValue, key: String): JSValue =
            value match {
              case JSValue.JSArrayVal(arr) =>
                if key == "length" then JSValue.fromInt(arr.getLength)
                else if key.forall(_.isDigit) && key.nonEmpty then
                  arr.get(key.toInt)
                else arr.getProperty(key).getOrElse(JSValue.Undefined)
              case JSValue.Object(obj) => obj.get(key)
              case _                   => JSValue.Undefined
            }

          def toLength(value: JSValue): Int =
            value match {
              case JSValue.Int32(n)   => math.max(0, n)
              case JSValue.Float64(d) =>
                if d.isNaN || d <= 0 then 0
                else math.min(d, Int.MaxValue.toDouble).toInt
              case _ => math.max(0, value.toNumber.toInt)
            }

          val substitutions = args.drop(offset + 1)
          val raw = getProp(args(offset), "raw")
          val len = toLength(getProp(raw, "length"))
          if len == 0 then JSValue.fromString("")
          else {
            val sb = new StringBuilder()
            var i = 0
            while i < len do {
              sb.append(getProp(raw, i.toString).toString)
              if i < len - 1 && i < substitutions.length then
                sb.append(substitutions(i).toString)
              i += 1
            }
            JSValue.fromString(sb.toString)
          }
        }
    )
    val stringFromCharCode = NativeFunction(
      name = "fromCharCode",
      impl = (args, _) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then JSValue.fromString("")
        else {
          val sb = new StringBuilder()
          var i = offset
          while i < args.length do {
            val code = args(i).toNumber.toInt & 0xffff
            sb.append(code.toChar)
            i += 1
          }
          JSValue.fromString(sb.toString)
        }
    )

    val stringFromCodePoint = NativeFunction(
      name = "fromCodePoint",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then JSValue.fromString("")
        else {
          val sb = new StringBuilder()
          var i = offset
          while i < args.length do {
            val codePoint = args(i).toNumber.toInt
            if codePoint < 0 || codePoint > 0x10ffff then
              ctx.throwRangeError("Invalid code point")
            sb.appendAll(Character.toChars(codePoint))
            i += 1
          }
          JSValue.fromString(sb.toString)
        }
    )

    stringConstructor.funcObj.set("raw", JSValue.Native(stringRaw))
    stringConstructor.funcObj.set(
      "fromCharCode",
      JSValue.Native(stringFromCharCode)
    )
    stringConstructor.funcObj.set(
      "fromCodePoint",
      JSValue.Native(stringFromCodePoint)
    )
  }
}

  // ============================================================
