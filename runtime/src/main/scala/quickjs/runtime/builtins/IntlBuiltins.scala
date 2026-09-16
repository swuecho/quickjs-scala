package quickjs.runtime.builtins

import quickjs.runtime.JSContext
import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.objmodel.{JSArray, JSObject}

import java.time.{Instant, ZoneId}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, FormatStyle}
import java.time.temporal.ChronoField
import java.util.Locale
import scala.jdk.CollectionConverters.*

/** A pragmatic `Intl` implementation backed by the JDK/ICU: `Segmenter`
  * (grapheme/word/sentence), `NumberFormat`, `DateTimeFormat`, `Collator`,
  * `PluralRules`, `ListFormat` and `DisplayNames`, plus the static locale
  * helpers. Locale data comes from the JDK CLDR and ICU4J (already a
  * dependency).
  */
object IntlBuiltins {

  def initialize(ctx: JSContext): Unit = {
    given JSContext = ctx
    val intl = JSObject(prototype = ctx.objectPrototype)

    // ---- shared helpers ----------------------------------------------------
    def localesOf(value: JSValue): List[String] =
      value match {
        case JSValue.JSStr(s) => List(s)
        case JSValue.JSArrayVal(arr) =>
          (0 until arr.getLength).toList.flatMap { i =>
            arr.get(i) match {
              case JSValue.JSStr(s) => Some(s)
              case _                => None
            }
          }
        case _ => Nil
      }

    def javaLocale(names: List[String]): Locale =
      names.headOption
        .flatMap(n =>
          try {
            val tag = n.replace('_', '-').trim
            if tag.isEmpty then None
            else Some(Locale.forLanguageTag(tag))
          } catch case _: Throwable => None
        )
        .filterNot(_.getLanguage.isEmpty)
        .getOrElse(Locale.getDefault)

    def option(value: JSValue, name: String): JSValue =
      if value == JSValue.Undefined || value == JSValue.Null then
        JSValue.Undefined
      else BuiltinHelpers.getPropertyWithGetter(value, name)

    def optionString(value: JSValue, name: String): Option[String] =
      option(value, name) match {
        case JSValue.JSStr(s) => Some(s)
        case _                => None
      }

    def optionBool(value: JSValue, name: String): Option[Boolean] =
      option(value, name) match {
        case JSValue.Bool(b) => Some(b)
        case _               => None
      }

    def optionNumber(value: JSValue, name: String): Option[Double] =
      option(value, name) match {
        case v if v == JSValue.Undefined => None
        case v                           => Some(BuiltinHelpers.toNumber(v))
      }

    def makeObject(proto: JSObject): JSObject =
      JSObject(prototype = proto, extensible = true)

    def resolvedOptions(
        proto: JSObject,
        locale: Locale,
        extra: (String, JSValue)*
    ): JSValue = {
      val obj = makeObject(proto)
      obj.set("locale", JSValue.fromString(locale.toLanguageTag))
      extra.foreach { case (k, v) => obj.set(k, v) }
      JSValue.Object(obj)
    }

    // =======================================================================
    // Intl.Segmenter
    // =======================================================================
    val segmenterPrototype = makeObject(ctx.objectPrototype)
    val segmenterCtor = NativeConstructor(
      name = "Segmenter",
      callImpl = (_, callCtx) =>
        callCtx.throwTypeError("Constructor Intl.Segmenter requires 'new'"),
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val locale = javaLocale(localesOf(args.headOption.getOrElse(JSValue.Undefined)))
        val granularity =
          optionString(args.lift(1).getOrElse(JSValue.Undefined), "granularity")
            .getOrElse("grapheme")
        val obj = makeObject(segmenterPrototype)
        obj.set("__locale", JSValue.fromString(locale.toLanguageTag))
        obj.set("__granularity", JSValue.fromString(granularity))
        JSValue.Object(obj)
      },
      prototype = segmenterPrototype
    )
    BuiltinHelpers.initConstructor(segmenterCtor, length = 0)
    segmenterPrototype.defineProperty(
      "constructor",
      JSValue.Native(segmenterCtor),
      enumerable = false
    )
    segmenterPrototype.defineProperty(
      "resolvedOptions",
      JSValue.Native(NativeFunction(
        name = "resolvedOptions",
        length = 0,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.extractJSObject(args.headOption.getOrElse(JSValue.Undefined)) match {
            case Some(obj) =>
              val tag = obj.get("__locale").toString
              val gran = obj.get("__granularity").toString
              resolvedOptions(
                callCtx.objectPrototype,
                javaLocale(List(tag)),
                "granularity" -> JSValue.fromString(gran)
              )
            case None => JSValue.Undefined
          }
        }
      )),
      enumerable = false
    )
    segmenterPrototype.defineProperty(
      "segment",
      JSValue.Native(NativeFunction(
        name = "segment",
        length = 1,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val thisObj = BuiltinHelpers
            .extractJSObject(args.headOption.getOrElse(JSValue.Undefined))
          val input =
            BuiltinHelpers.toJSString(args.lift(1).getOrElse(JSValue.Undefined))
          val tag = thisObj.map(_.get("__locale").toString).getOrElse("en")
          val granularity =
            thisObj.map(_.get("__granularity").toString).getOrElse("grapheme")
          val locale = javaLocale(List(tag))
          val it = granularity match {
            case "word"     => java.text.BreakIterator.getWordInstance(locale)
            case "sentence" => java.text.BreakIterator.getSentenceInstance(locale)
            case _          => java.text.BreakIterator.getCharacterInstance(locale)
          }
          it.setText(input)
          val result = JSArray.empty()
          var start = it.first()
          var end = it.next()
          while end != java.text.BreakIterator.DONE do {
            val segObj = makeObject(callCtx.objectPrototype)
            segObj.set("segment", JSValue.fromString(input.substring(start, end)))
            segObj.set("index", JSValue.fromInt(start))
            segObj.set("input", JSValue.fromString(input))
            result.push(JSValue.Object(segObj))
            start = end
            end = it.next()
          }
          JSValue.JSArrayVal(result)
        }
      )),
      enumerable = false
    )
    intl.set("Segmenter", JSValue.Native(segmenterCtor))

    // =======================================================================
    // Intl.NumberFormat
    // =======================================================================
    val numberFormatPrototype = makeObject(ctx.objectPrototype)
    def constructNumberFormat(args: Array[JSValue]): JSValue = {
      val first = args.headOption.getOrElse(JSValue.Undefined)
      val (localesValue, optionsValue) =
        if first == JSValue.Undefined || first == JSValue.Null then
          (first, args.lift(1).getOrElse(JSValue.Undefined))
        else if first.isInstanceOf[JSValue.JSStr] || first.isInstanceOf[JSValue.JSArrayVal]
        then (first, args.lift(1).getOrElse(JSValue.Undefined))
        else (JSValue.Undefined, first)
      val locale = javaLocale(localesOf(localesValue))
      val obj = makeObject(numberFormatPrototype)
      obj.set("__locale", JSValue.fromString(locale.toLanguageTag))
      obj.set("__options", optionsValue)
      JSValue.Object(obj)
    }

    val numberFormatCtor = NativeConstructor(
      name = "NumberFormat",
      callImpl = (args, callCtx) => {
        given JSContext = callCtx
        constructNumberFormat(args)
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        constructNumberFormat(args)
      },
      prototype = numberFormatPrototype
    )
    BuiltinHelpers.initConstructor(numberFormatCtor, length = 0)
    numberFormatPrototype.defineProperty(
      "constructor",
      JSValue.Native(numberFormatCtor),
      enumerable = false
    )
    def numberFormatterOf(obj: JSObject): java.text.NumberFormat = {
      val locale = javaLocale(List(obj.get("__locale").toString))
      val options = obj.get("__options")
      val style = optionString(options, "style").getOrElse("decimal")
      val nf: java.text.NumberFormat = style match {
        case "currency" =>
          val currency = optionString(options, "currency").getOrElse("USD")
          val f = java.text.NumberFormat.getCurrencyInstance(locale)
          try f.setCurrency(java.util.Currency.getInstance(currency))
          catch case _: Throwable => ()
          f
        case "percent" => java.text.NumberFormat.getPercentInstance(locale)
        case "unit"    => java.text.NumberFormat.getNumberInstance(locale)
        case _         => java.text.NumberFormat.getNumberInstance(locale)
      }
      optionBool(options, "useGrouping").foreach(nf.setGroupingUsed)
      optionNumber(options, "minimumFractionDigits").foreach(d =>
        nf.setMinimumFractionDigits(d.toInt)
      )
      optionNumber(options, "maximumFractionDigits").foreach(d =>
        nf.setMaximumFractionDigits(d.toInt)
      )
      optionNumber(options, "minimumIntegerDigits").foreach(d =>
        nf.setMinimumIntegerDigits(d.toInt)
      )
      nf
    }
    def formatNumberImpl(obj: JSObject, value: JSValue): JSValue = {
      val nf = numberFormatterOf(obj)
      val num = BuiltinHelpers.toNumber(value)
      JSValue.fromString(nf.format(num))
    }
    numberFormatPrototype.defineProperty(
      "format",
      JSValue.Native(NativeFunction(
        name = "format",
        length = 1,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.extractJSObject(args.headOption.getOrElse(JSValue.Undefined)) match {
            case Some(obj) =>
              formatNumberImpl(obj, args.lift(1).getOrElse(JSValue.Undefined))
            case None => JSValue.Undefined
          }
        }
      )),
      enumerable = false
    )
    numberFormatPrototype.defineProperty(
      "formatToParts",
      JSValue.Native(NativeFunction(
        name = "formatToParts",
        length = 1,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.extractJSObject(args.headOption.getOrElse(JSValue.Undefined)) match {
            case Some(obj) =>
              val formatted =
                formatNumberImpl(obj, args.lift(1).getOrElse(JSValue.Undefined))
              val part = makeObject(callCtx.objectPrototype)
              part.set("type", JSValue.fromString("literal"))
              part.set("value", formatted)
              val arr = JSArray.empty()
              arr.push(JSValue.Object(part))
              JSValue.JSArrayVal(arr)
            case None => JSValue.JSArrayVal(JSArray.empty())
          }
        }
      )),
      enumerable = false
    )
    numberFormatPrototype.defineProperty(
      "resolvedOptions",
      JSValue.Native(NativeFunction(
        name = "resolvedOptions",
        length = 0,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.extractJSObject(args.headOption.getOrElse(JSValue.Undefined)) match {
            case Some(obj) =>
              val style =
                optionString(obj.get("__options"), "style").getOrElse("decimal")
              resolvedOptions(
                callCtx.objectPrototype,
                javaLocale(List(obj.get("__locale").toString)),
                "style" -> JSValue.fromString(style),
                "numberingSystem" -> JSValue.fromString("latn")
              )
            case None => JSValue.Undefined
          }
        }
      )),
      enumerable = false
    )
    numberFormatPrototype.defineProperty(
      "supportedLocalesOf",
      JSValue.Native(NativeFunction(
        name = "supportedLocalesOf",
        length = 1,
        impl = (args, _) => JSValue.JSArrayVal(JSArray.empty())
      )),
      enumerable = false
    )
    intl.set("NumberFormat", JSValue.Native(numberFormatCtor))

    // =======================================================================
    // Intl.DateTimeFormat
    // =======================================================================
    val dateTimeFormatPrototype = makeObject(ctx.objectPrototype)
    def constructDateTimeFormat(args: Array[JSValue]): JSValue = {
      val first = args.headOption.getOrElse(JSValue.Undefined)
      val (localesValue, optionsValue) =
        if first == JSValue.Undefined || first == JSValue.Null then
          (first, args.lift(1).getOrElse(JSValue.Undefined))
        else if first.isInstanceOf[JSValue.JSStr] || first.isInstanceOf[JSValue.JSArrayVal]
        then (first, args.lift(1).getOrElse(JSValue.Undefined))
        else (JSValue.Undefined, first)
      val locale = javaLocale(localesOf(localesValue))
      val obj = makeObject(dateTimeFormatPrototype)
      obj.set("__locale", JSValue.fromString(locale.toLanguageTag))
      obj.set("__options", optionsValue)
      JSValue.Object(obj)
    }

    val dateTimeFormatCtor = NativeConstructor(
      name = "DateTimeFormat",
      callImpl = (args, callCtx) => {
        given JSContext = callCtx
        constructDateTimeFormat(args)
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        constructDateTimeFormat(args)
      },
      prototype = dateTimeFormatPrototype
    )
    BuiltinHelpers.initConstructor(dateTimeFormatCtor, length = 0)
    dateTimeFormatPrototype.defineProperty(
      "constructor",
      JSValue.Native(dateTimeFormatCtor),
      enumerable = false
    )
    def dateStyle(name: String): Option[FormatStyle] =
      name match {
        case "full"   => Some(FormatStyle.FULL)
        case "long"   => Some(FormatStyle.LONG)
        case "medium" => Some(FormatStyle.MEDIUM)
        case "short"  => Some(FormatStyle.SHORT)
        case _        => None
      }
    def dateTimeFormatterOf(obj: JSObject): (DateTimeFormatter, ZoneId) = {
      val locale = javaLocale(List(obj.get("__locale").toString))
      val options = obj.get("__options")
      val zoneId =
        optionString(options, "timeZone") match {
          case Some(tz) =>
            try ZoneId.of(tz)
            catch case _: Throwable => ZoneId.systemDefault()
          case None => ZoneId.systemDefault()
        }
      val dateStyleOpt = optionString(options, "dateStyle").flatMap(dateStyle)
      val timeStyleOpt = optionString(options, "timeStyle").flatMap(dateStyle)
      val hasExplicitFields = Seq(
        "year",
        "month",
        "day",
        "hour",
        "minute",
        "second",
        "weekday"
      ).exists(name => option(options, name) != JSValue.Undefined)
      val formatter =
        if dateStyleOpt.isDefined || timeStyleOpt.isDefined then {
          val dateFmt = dateStyleOpt
            .map(s => DateTimeFormatter.ofLocalizedDate(s))
            .getOrElse(DateTimeFormatter.ofLocalizedDate(FormatStyle.SHORT))
          val timeFmt = timeStyleOpt.map(s => DateTimeFormatter.ofLocalizedTime(s))
          val combined = timeFmt match {
            case Some(t) =>
              DateTimeFormatterBuilder()
                .append(dateFmt)
                .appendLiteral(' ')
                .append(t)
                .toFormatter(locale)
            case None => dateFmt.withLocale(locale)
          }
          combined.withLocale(locale)
        } else if hasExplicitFields then {
          val pattern = new StringBuilder()
          def appendPattern(p: String): Unit = {
            if pattern.nonEmpty then pattern.append(' ')
            pattern.append(p)
          }
          optionString(options, "weekday").foreach {
            case "long"    => appendPattern("EEEE")
            case "short"   => appendPattern("EEE")
            case "narrow"  => appendPattern("EEEEE")
            case _         => ()
          }
          optionString(options, "year").foreach {
            case "2-digit" => appendPattern("yy")
            case _         => appendPattern("yyyy")
          }
          optionString(options, "month").foreach {
            case "2-digit" => appendPattern("MM")
            case "long"    => appendPattern("MMMM")
            case "short"   => appendPattern("MMM")
            case "narrow"  => appendPattern("MMMMM")
            case _         => appendPattern("M")
          }
          optionString(options, "day").foreach {
            case "2-digit" => appendPattern("dd")
            case _         => appendPattern("d")
          }
          // Time fields (hour/minute/second) use HH:mm:ss; hour12 adds an AM/PM part.
          val timePattern = new StringBuilder()
          optionString(options, "hour").foreach { _ =>
            val hour12 = optionBool(options, "hour12").getOrElse(false)
            timePattern.append(if hour12 then "hh" else "HH")
          }
          optionString(options, "minute").foreach { _ =>
            if timePattern.isEmpty then timePattern.append("HH")
            timePattern.append(":mm")
          }
          optionString(options, "second").foreach { _ =>
            if timePattern.isEmpty then timePattern.append("HH")
            timePattern.append(":ss")
          }
          if timePattern.nonEmpty then {
            if pattern.nonEmpty then pattern.append(' ')
            pattern.append(timePattern)
            if optionBool(options, "hour12").getOrElse(false) then
              pattern.append(" a")
          }
          if pattern.isEmpty then pattern.append("yMd")
          val skeleton = pattern.toString
            .replace("EEEE", "E")
            .replace("yyyy", "y")
            .replace("yy", "y")
            .replace("MMMMM", "MMMM")
            .replace("MMM", "MMM")
            .replace("MM", "MM")
            .replace("dd", "d")
            .replace("hh", "h")
            .replace("HH", "H")
            .replace("mm", "m")
            .replace("ss", "s")
          val best =
            try
              com.ibm.icu.text.DateTimePatternGenerator
                .getInstance(com.ibm.icu.util.ULocale.forLanguageTag(locale.toLanguageTag))
                .getBestPattern(skeleton)
            catch case _: Throwable => pattern.toString
          DateTimeFormatter.ofPattern(best, locale)
        } else DateTimeFormatter.ofLocalizedDate(FormatStyle.SHORT).withLocale(locale)
      (formatter, zoneId)
    }
    def formatDateImpl(obj: JSObject, value: JSValue): JSValue = {
      val (formatter, zoneId) = dateTimeFormatterOf(obj)
      val ms = BuiltinHelpers.toNumber(value)
      if ms.isNaN || ms.isInfinite then JSValue.fromString("Invalid Date")
      else {
        val zoned = Instant.ofEpochMilli(ms.toLong).atZone(zoneId)
        JSValue.fromString(formatter.format(zoned))
      }
    }
    def dateValueOf(args: Array[JSValue]): Option[(JSObject, JSValue)] =
      BuiltinHelpers.extractJSObject(args.headOption.getOrElse(JSValue.Undefined))
        .map(obj => (obj, args.lift(1).getOrElse(JSValue.Undefined)))
    dateTimeFormatPrototype.defineProperty(
      "format",
      JSValue.Native(NativeFunction(
        name = "format",
        length = 1,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          dateValueOf(args) match {
            case Some((obj, v)) => formatDateImpl(obj, v)
            case None           => JSValue.Undefined
          }
        }
      )),
      enumerable = false
    )
    dateTimeFormatPrototype.defineProperty(
      "resolvedOptions",
      JSValue.Native(NativeFunction(
        name = "resolvedOptions",
        length = 0,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.extractJSObject(args.headOption.getOrElse(JSValue.Undefined)) match {
            case Some(obj) =>
              resolvedOptions(
                callCtx.objectPrototype,
                javaLocale(List(obj.get("__locale").toString)),
                "calendar" -> JSValue.fromString("gregory"),
                "timeZone" -> JSValue.fromString(ZoneId.systemDefault().getId)
              )
            case None => JSValue.Undefined
          }
        }
      )),
      enumerable = false
    )
    intl.set("DateTimeFormat", JSValue.Native(dateTimeFormatCtor))

    // =======================================================================
    // Intl.Collator
    // =======================================================================
    val collatorPrototype = makeObject(ctx.objectPrototype)
    def constructCollator(args: Array[JSValue]): JSValue = {
      val first = args.headOption.getOrElse(JSValue.Undefined)
      val (localesValue, optionsValue) =
        if first == JSValue.Undefined || first == JSValue.Null then
          (first, args.lift(1).getOrElse(JSValue.Undefined))
        else if first.isInstanceOf[JSValue.JSStr] || first.isInstanceOf[JSValue.JSArrayVal]
        then (first, args.lift(1).getOrElse(JSValue.Undefined))
        else (JSValue.Undefined, first)
      val locale = javaLocale(localesOf(localesValue))
      val obj = makeObject(collatorPrototype)
      obj.set("__locale", JSValue.fromString(locale.toLanguageTag))
      obj.set("__options", optionsValue)
      JSValue.Object(obj)
    }

    val collatorCtor = NativeConstructor(
      name = "Collator",
      callImpl = (args, callCtx) => {
        given JSContext = callCtx
        constructCollator(args)
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        constructCollator(args)
      },
      prototype = collatorPrototype
    )
    BuiltinHelpers.initConstructor(collatorCtor, length = 0)
    collatorPrototype.defineProperty(
      "constructor",
      JSValue.Native(collatorCtor),
      enumerable = false
    )
    def javaCollatorOf(obj: JSObject): java.text.Collator = {
      val locale = javaLocale(List(obj.get("__locale").toString))
      val collator = java.text.Collator.getInstance(locale)
      optionString(obj.get("__options"), "sensitivity").foreach {
        case "base"  => collator.setStrength(java.text.Collator.PRIMARY)
        case "accent" => collator.setStrength(java.text.Collator.SECONDARY)
        case "case"  => collator.setStrength(java.text.Collator.PRIMARY)
        case _       => collator.setStrength(java.text.Collator.TERTIARY)
      }
      collator
    }
    collatorPrototype.defineProperty(
      "compare",
      JSValue.Native(NativeFunction(
        name = "compare",
        length = 2,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.extractJSObject(args.headOption.getOrElse(JSValue.Undefined)) match {
            case Some(obj) =>
              val a = BuiltinHelpers.toJSString(args.lift(1).getOrElse(JSValue.Undefined))
              val b = BuiltinHelpers.toJSString(args.lift(2).getOrElse(JSValue.Undefined))
              val cmp = javaCollatorOf(obj).compare(a, b)
              JSValue.fromInt(if cmp < 0 then -1 else if cmp > 0 then 1 else 0)
            case None => JSValue.fromInt(0)
          }
        }
      )),
      enumerable = false
    )
    collatorPrototype.defineProperty(
      "resolvedOptions",
      JSValue.Native(NativeFunction(
        name = "resolvedOptions",
        length = 0,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.extractJSObject(args.headOption.getOrElse(JSValue.Undefined)) match {
            case Some(obj) =>
              resolvedOptions(
                callCtx.objectPrototype,
                javaLocale(List(obj.get("__locale").toString)),
                "usage" -> JSValue.fromString("sort"),
                "sensitivity" -> JSValue.fromString("variant")
              )
            case None => JSValue.Undefined
          }
        }
      )),
      enumerable = false
    )
    intl.set("Collator", JSValue.Native(collatorCtor))

    // =======================================================================
    // Intl.PluralRules (ICU)
    // =======================================================================
    val pluralRulesPrototype = makeObject(ctx.objectPrototype)
    val pluralRulesCtor = NativeConstructor(
      name = "PluralRules",
      callImpl = (_, callCtx) =>
        callCtx.throwTypeError("Constructor Intl.PluralRules requires 'new'"),
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val first = args.headOption.getOrElse(JSValue.Undefined)
        val locale = javaLocale(localesOf(first))
        val obj = makeObject(pluralRulesPrototype)
        obj.set("__locale", JSValue.fromString(locale.toLanguageTag))
        obj.set(
          "__type",
          JSValue.fromString(
            optionString(args.lift(1).getOrElse(JSValue.Undefined), "type")
              .getOrElse("cardinal")
          )
        )
        JSValue.Object(obj)
      },
      prototype = pluralRulesPrototype
    )
    BuiltinHelpers.initConstructor(pluralRulesCtor, length = 0)
    pluralRulesPrototype.defineProperty(
      "constructor",
      JSValue.Native(pluralRulesCtor),
      enumerable = false
    )
    pluralRulesPrototype.defineProperty(
      "select",
      JSValue.Native(NativeFunction(
        name = "select",
        length = 1,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.extractJSObject(args.headOption.getOrElse(JSValue.Undefined)) match {
            case Some(obj) =>
              val number =
                BuiltinHelpers.toNumber(args.lift(1).getOrElse(JSValue.Undefined))
              val localeTag = obj.get("__locale").toString
              val kind =
                if obj.get("__type").toString == "ordinal" then
                  com.ibm.icu.text.PluralRules.PluralType.ORDINAL
                else com.ibm.icu.text.PluralRules.PluralType.CARDINAL
              val rules = com.ibm.icu.text.PluralRules.forLocale(
                com.ibm.icu.util.ULocale.forLanguageTag(localeTag),
                kind
              )
              JSValue.fromString(rules.select(number))
            case None => JSValue.fromString("other")
          }
        }
      )),
      enumerable = false
    )
    intl.set("PluralRules", JSValue.Native(pluralRulesCtor))

    // =======================================================================
    // Intl.ListFormat / RelativeTimeFormat / DisplayNames (basic)
    // =======================================================================
    val listFormatPrototype = makeObject(ctx.objectPrototype)
    val listFormatCtor = NativeConstructor(
      name = "ListFormat",
      callImpl = (_, callCtx) =>
        callCtx.throwTypeError("Constructor Intl.ListFormat requires 'new'"),
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val first = args.headOption.getOrElse(JSValue.Undefined)
        val locale = javaLocale(localesOf(first))
        val obj = makeObject(listFormatPrototype)
        obj.set("__locale", JSValue.fromString(locale.toLanguageTag))
        obj.set(
          "__type",
          JSValue.fromString(
            optionString(args.lift(1).getOrElse(JSValue.Undefined), "type")
              .getOrElse("conjunction")
          )
        )
        JSValue.Object(obj)
      },
      prototype = listFormatPrototype
    )
    BuiltinHelpers.initConstructor(listFormatCtor, length = 0)
    listFormatPrototype.defineProperty(
      "format",
      JSValue.Native(NativeFunction(
        name = "format",
        length = 1,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.extractJSObject(args.headOption.getOrElse(JSValue.Undefined)) match {
            case Some(obj) =>
              val items = args.lift(1) match {
                case Some(JSValue.JSArrayVal(arr)) =>
                  (0 until arr.getLength).map(i =>
                    BuiltinHelpers.toJSString(arr.get(i))
                  )
                case _ => Seq.empty[String]
              }
              val localeTag = obj.get("__locale").toString
              val icuLocale = com.ibm.icu.util.ULocale.forLanguageTag(localeTag)
              val style = obj.get("__type").toString match {
                case "disjunction" => com.ibm.icu.text.ListFormatter.Type.OR
                case "unit"        => com.ibm.icu.text.ListFormatter.Type.UNITS
                case _             => com.ibm.icu.text.ListFormatter.Type.AND
              }
              val formatter = com.ibm.icu.text.ListFormatter.getInstance(
                icuLocale,
                style,
                com.ibm.icu.text.ListFormatter.Width.WIDE
              )
              JSValue.fromString(formatter.format(items.toList.asJava))
            case None => JSValue.fromString("")
          }
        }
      )),
      enumerable = false
    )
    intl.set("ListFormat", JSValue.Native(listFormatCtor))

    val relativeTimePrototype = makeObject(ctx.objectPrototype)
    val relativeTimeCtor = NativeConstructor(
      name = "RelativeTimeFormat",
      callImpl = (_, callCtx) =>
        callCtx.throwTypeError(
          "Constructor Intl.RelativeTimeFormat requires 'new'"
        ),
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val locale = javaLocale(localesOf(args.headOption.getOrElse(JSValue.Undefined)))
        val obj = makeObject(relativeTimePrototype)
        obj.set("__locale", JSValue.fromString(locale.toLanguageTag))
        JSValue.Object(obj)
      },
      prototype = relativeTimePrototype
    )
    BuiltinHelpers.initConstructor(relativeTimeCtor, length = 0)
    relativeTimePrototype.defineProperty(
      "format",
      JSValue.Native(NativeFunction(
        name = "format",
        length = 2,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          BuiltinHelpers.extractJSObject(args.headOption.getOrElse(JSValue.Undefined)) match {
            case Some(obj) =>
              val value =
                BuiltinHelpers.toNumber(args.lift(1).getOrElse(JSValue.Undefined))
              val unit = NodeHelpersOrString(args.lift(2).getOrElse(JSValue.Undefined))
              val localeTag = obj.get("__locale").toString
              val icuLocale = com.ibm.icu.util.ULocale.forLanguageTag(localeTag)
              val formatter = com.ibm.icu.text.RelativeDateTimeFormatter
                .getInstance(icuLocale)
              val direction =
                if value < 0 then com.ibm.icu.text.RelativeDateTimeFormatter.Direction.LAST
                else com.ibm.icu.text.RelativeDateTimeFormatter.Direction.NEXT
              val relUnit = unit match {
                case "year"        => com.ibm.icu.text.RelativeDateTimeFormatter.RelativeUnit.YEARS
                case "quarter"     => com.ibm.icu.text.RelativeDateTimeFormatter.RelativeUnit.QUARTERS
                case "month"       => com.ibm.icu.text.RelativeDateTimeFormatter.RelativeUnit.MONTHS
                case "week"        => com.ibm.icu.text.RelativeDateTimeFormatter.RelativeUnit.WEEKS
                case "day"         => com.ibm.icu.text.RelativeDateTimeFormatter.RelativeUnit.DAYS
                case "hour"        => com.ibm.icu.text.RelativeDateTimeFormatter.RelativeUnit.HOURS
                case "minute"      => com.ibm.icu.text.RelativeDateTimeFormatter.RelativeUnit.MINUTES
                case "second"      => com.ibm.icu.text.RelativeDateTimeFormatter.RelativeUnit.SECONDS
                case _             => com.ibm.icu.text.RelativeDateTimeFormatter.RelativeUnit.DAYS
              }
              JSValue.fromString(
                formatter.format(
                  java.lang.Math.abs(value),
                  direction,
                  relUnit
                )
              )
            case None => JSValue.fromString("")
          }
        }
      )),
      enumerable = false
    )
    intl.set("RelativeTimeFormat", JSValue.Native(relativeTimeCtor))

    val displayNamesPrototype = makeObject(ctx.objectPrototype)
    val displayNamesCtor = NativeConstructor(
      name = "DisplayNames",
      callImpl = (_, callCtx) =>
        callCtx.throwTypeError("Constructor Intl.DisplayNames requires 'new'"),
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val locale = javaLocale(localesOf(args.headOption.getOrElse(JSValue.Undefined)))
        val obj = makeObject(displayNamesPrototype)
        obj.set("__locale", JSValue.fromString(locale.toLanguageTag))
        obj.set(
          "__type",
          JSValue.fromString(
            optionString(args.lift(1).getOrElse(JSValue.Undefined), "type")
              .getOrElse("language")
          )
        )
        JSValue.Object(obj)
      },
      prototype = displayNamesPrototype
    )
    BuiltinHelpers.initConstructor(displayNamesCtor, length = 0)
    displayNamesPrototype.defineProperty(
      "of",
      JSValue.Native(NativeFunction(
        name = "of",
        length = 1,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val code =
            BuiltinHelpers.toJSString(args.lift(1).getOrElse(JSValue.Undefined))
          val localeTag =
            BuiltinHelpers
              .extractJSObject(args.headOption.getOrElse(JSValue.Undefined))
              .map(_.get("__locale").toString)
              .getOrElse("en")
          try {
            val names = com.ibm.icu.text.LocaleDisplayNames.getInstance(
              com.ibm.icu.util.ULocale.forLanguageTag(localeTag),
              com.ibm.icu.text.LocaleDisplayNames.DialectHandling.STANDARD_NAMES
            )
            val displayType =
              BuiltinHelpers
                .extractJSObject(args.headOption.getOrElse(JSValue.Undefined))
                .map(_.get("__type").toString)
                .getOrElse("language")
            val name = displayType match {
              case "region" => names.regionDisplayName(code)
              case "script" => names.scriptDisplayName(code)
              case _        => names.languageDisplayName(code)
            }
            JSValue.fromString(Option(name).getOrElse(code))
          } catch case _: Throwable => JSValue.fromString(code)
        }
      )),
      enumerable = false
    )
    intl.set("DisplayNames", JSValue.Native(displayNamesCtor))

    // =======================================================================
    // Statics
    // =======================================================================
    def staticArgs(args: Array[JSValue]): Array[JSValue] =
      if args.nonEmpty && (args(0) match {
            case JSValue.Object(o) => o eq intl
            case _                 => false
          })
      then args.drop(1)
      else args

    intl.set(
      "getCanonicalLocales",
      JSValue.Native(NativeFunction(
        name = "getCanonicalLocales",
        length = 1,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val values = localesOf(staticArgs(args).headOption.getOrElse(JSValue.Undefined))
          val arr = JSArray.empty()
          values.foreach { tag =>
            val canonical =
              try Locale.forLanguageTag(tag.replace('_', '-')).toLanguageTag
              catch case _: Throwable => tag
            arr.push(JSValue.fromString(canonical))
          }
          JSValue.JSArrayVal(arr)
        }
      ))
    )
    intl.set(
      "supportedLocalesOf",
      JSValue.Native(NativeFunction(
        name = "supportedLocalesOf",
        length = 1,
        impl = (_, _) => JSValue.JSArrayVal(JSArray.empty())
      ))
    )
    intl.set(
      "supportedValuesOf",
      JSValue.Native(NativeFunction(
        name = "supportedValuesOf",
        length = 1,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val key = BuiltinHelpers.toJSString(staticArgs(args).headOption.getOrElse(JSValue.Undefined))
          val values: Seq[String] = key match {
            case "calendar" => Seq("gregory")
            case "collation" => Seq("default")
            case "currency" =>
              java.util.Currency.getAvailableCurrencies.toArray
                .map(_.asInstanceOf[java.util.Currency].getCurrencyCode)
                .toSeq
                .sorted
            case "numberingSystem" => Seq("latn")
            case "timeZone" => ZoneId.getAvailableZoneIds.toArray.map(_.toString).toSeq.sorted
            case "unit" => Seq("meter", "second", "kilogram")
            case _ => Seq.empty
          }
          val arr = JSArray.empty()
          values.foreach(v => arr.push(JSValue.fromString(v)))
          JSValue.JSArrayVal(arr)
        }
      ))
    )

    ctx.global.defineProperty(
      "Intl",
      JSValue.Object(intl),
      enumerable = false,
      writable = true,
      configurable = true
    )
  }

  private def NodeHelpersOrString(value: JSValue): String =
    value match {
      case JSValue.JSStr(s) => s
      case other            => other.toString
    }
}
