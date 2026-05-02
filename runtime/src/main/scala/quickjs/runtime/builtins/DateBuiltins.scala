package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import java.time.{Instant, LocalDateTime, ZoneId, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.Locale

/** Date built-in: Date constructor, Date.now, Date.parse, Date.UTC, prototype
  * methods.
  */
object DateBuiltins {
  import quickjs.objmodel.JSObject

  def initialize(ctx: JSContext): Unit = {
    given JSContext = ctx

    val datePrototype =
      JSObject(prototype = ctx.objectPrototype, extensible = true)

    def toMillisOrNaN(value: JSValue): Double =
      value match {
        case JSValue.Int32(i)   => i.toDouble
        case JSValue.Float64(d) => d
        case _                  => value.toNumber
      }

    def setDateValue(obj: JSObject, millis: Double)(using JSContext): Unit =
      obj.defineProperty(
        "__dateValue",
        JSValue.fromDouble(millis),
        enumerable = false,
        writable = true,
        configurable = false
      )

    def getDateValue(obj: JSObject)(using JSContext): Double =
      obj.getOwnProperty("__dateValue") match {
        case Some(value) => value.toNumber
        case None        => Double.NaN
      }

    def newDateObject(millis: Double)(using JSContext): JSValue = {
      val obj = JSObject(prototype = datePrototype, extensible = true)
      setDateValue(obj, millis)
      JSValue.Object(obj)
    }

    def parseFractionalMillis(raw: String): Int =
      if raw.isEmpty then 0
      else {
        val digits =
          if raw.length >= 3 then raw.substring(0, 3) else raw.padTo(3, '0')
        digits.toInt
      }

    def parseIso(input: String): Option[Double] = {
      val isoRegex =
        """^([+-]?\d{4,6})(?:-(\d{2})(?:-(\d{2}))?)?(?:T(\d{2}):(\d{2})(?::(\d{2})(?:\.(\d+))?)?)?(?:Z|([+-])(\d{2}):?(\d{2}))?$""".r
      input match {
        case isoRegex(
              yearStr,
              monthStr,
              dayStr,
              hourStr,
              minuteStr,
              secondStr,
              fracStr,
              tzSign,
              tzHourStr,
              tzMinStr
            ) =>
          val year = yearStr.toInt
          val month = if monthStr == null then 1 else monthStr.toInt
          val day = if dayStr == null then 1 else dayStr.toInt
          val hasTime = hourStr != null
          val hour = if hourStr == null then 0 else hourStr.toInt
          val minute = if minuteStr == null then 0 else minuteStr.toInt
          val second = if secondStr == null then 0 else secondStr.toInt
          val millis =
            if fracStr == null then 0 else parseFractionalMillis(fracStr)
          val hasTz = tzSign != null || input.endsWith("Z")
          val isLocal = hasTime && !hasTz
          val zone =
            if hasTz then
              if input.endsWith("Z") then ZoneOffset.UTC
              else {
                val sign = if tzSign == "-" then -1 else 1
                val tzHour = tzHourStr.toInt
                val tzMin = tzMinStr.toInt
                ZoneOffset.ofHoursMinutes(sign * tzHour, sign * tzMin)
              }
            else if !hasTime then ZoneOffset.UTC
            else ZoneId.systemDefault
          val ldt = LocalDateTime.of(
            year,
            month,
            day,
            hour,
            minute,
            second,
            millis * 1000000
          )
          val instant =
            if isLocal then ldt.atZone(ZoneId.systemDefault).toInstant
            else ldt.atZone(zone).toInstant
          Some(instant.toEpochMilli.toDouble)
        case _ =>
          None
      }
    }

    def parseMonth(token: String): Option[Int] = {
      val months = Array(
        "jan",
        "feb",
        "mar",
        "apr",
        "may",
        "jun",
        "jul",
        "aug",
        "sep",
        "oct",
        "nov",
        "dec"
      )
      val idx = months.indexOf(token.toLowerCase(Locale.ROOT))
      if idx >= 0 then Some(idx + 1) else None
    }

    def parseTextDate(input: String): Option[Double] = {
      val cleaned = input.trim.replaceAll("\\s+", " ")
      if cleaned.isEmpty then return None
      val tokens = cleaned.split(" ").toList
      val weekdays = Set("mon", "tue", "wed", "thu", "fri", "sat", "sun")
      val withoutWeekday =
        tokens match {
          case head :: tail
              if weekdays.contains(head.take(3).toLowerCase(Locale.ROOT)) =>
            tail
          case _ => tokens
        }
      if withoutWeekday.length < 3 then return None
      val monthOpt = parseMonth(withoutWeekday.head)
      if monthOpt.isEmpty then return None
      val month = monthOpt.get
      val day = withoutWeekday(1).toInt
      val year = withoutWeekday(2).toInt
      var hour = 0
      var minute = 0
      var second = 0
      var millis = 0
      var zone: ZoneId | ZoneOffset = ZoneId.systemDefault
      if withoutWeekday.length >= 4 then {
        val timeToken = withoutWeekday(3)
        if timeToken.contains(":") then {
          val parts = timeToken.split(":")
          if parts.length >= 2 then {
            hour = parts(0).toInt
            minute = parts(1).toInt
          }
          if parts.length >= 3 then {
            val secPart = parts(2)
            val secSplit = secPart.split("\\.")
            second = secSplit(0).toInt
            if secSplit.length > 1 then
              millis = parseFractionalMillis(secSplit(1))
          }
        }
      }
      if withoutWeekday.length >= 5 then {
        val tzToken = withoutWeekday(4)
        if tzToken.startsWith("GMT") && tzToken.length >= 8 then {
          val sign = if tzToken.charAt(3) == '-' then -1 else 1
          val hh = tzToken.substring(4, 6).toInt
          val mm = tzToken.substring(6, 8).toInt
          zone = ZoneOffset.ofHoursMinutes(sign * hh, sign * mm)
        }
      }
      val ldt = LocalDateTime.of(
        year,
        month,
        day,
        hour,
        minute,
        second,
        millis * 1000000
      )
      val instant = ldt.atZone(zone).toInstant
      Some(instant.toEpochMilli.toDouble)
    }

    def parseDateString(input: String): Double = {
      val trimmed = input.trim
      if trimmed.isEmpty then Double.NaN
      else
        parseIso(trimmed)
          .orElse {
            if trimmed.matches("""^[+-]?\d{4,6}T.*""") then
              parseIso(trimmed.replaceFirst("T", "-01-01T"))
            else None
          }
          .orElse(parseTextDate(trimmed))
          .getOrElse(Double.NaN)
    }

    def formatToISOString(millis: Double): String = {
      val instant = Instant.ofEpochMilli(millis.toLong)
      val formatter = DateTimeFormatter
        .ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
        .withZone(ZoneOffset.UTC)
      formatter.format(instant)
    }

    def formatToString(millis: Double): String = {
      val formatter = DateTimeFormatter.ofPattern(
        "EEE MMM dd yyyy HH:mm:ss 'GMT'XXX",
        Locale.ENGLISH
      )
      val zdt = ZonedDateTime.ofInstant(
        Instant.ofEpochMilli(millis.toLong),
        ZoneId.systemDefault
      )
      formatter.format(zdt)
    }

    def requireDateObject(args: Array[JSValue], method: String)(using
        JSContext
    ): (JSObject, Double) = {
      if args.isEmpty then
        ctx.throwTypeError(s"Date.prototype.$method called on undefined")
      args(0) match {
        case JSValue.Object(obj) =>
          val value = getDateValue(obj)
          (obj, value)
        case _ =>
          ctx.throwTypeError(s"Date.prototype.$method called on non-object")
      }
    }

    val dateConstructor = quickjs.value.NativeConstructor(
      name = "Date",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        val now = System.currentTimeMillis().toDouble
        JSValue.fromString(formatToString(now))
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val millis =
          if args.isEmpty then System.currentTimeMillis().toDouble
          else
            args(0) match {
              case JSValue.Object(obj) if !getDateValue(obj).isNaN =>
                getDateValue(obj)
              case JSValue.JSStr(s) =>
                parseDateString(s)
              case _ =>
                toMillisOrNaN(args(0))
            }
        newDateObject(millis)
      ,
      prototype = datePrototype
    )
    BuiltinHelpers.initConstructor(dateConstructor, length = 7)

    val dateNow = NativeFunction(
      name = "now",
      impl = (_, _) => JSValue.fromDouble(System.currentTimeMillis().toDouble)
    )
    val dateParse = NativeFunction(
      name = "parse",
      impl = (args, _) =>
        val actualArgs = if args.length >= 2 then args.drop(1) else args
        if actualArgs.isEmpty then JSValue.fromDouble(Double.NaN)
        else JSValue.fromDouble(parseDateString(actualArgs(0).toString))
    )
    val dateUTC = NativeFunction(
      name = "UTC",
      impl = (args, _) =>
        val actualArgs = if args.length >= 2 then args.drop(1) else args
        if actualArgs.isEmpty then JSValue.fromDouble(Double.NaN)
        else {
          val nums = actualArgs.take(7).map(toMillisOrNaN)
          if nums.exists(_.isNaN) then JSValue.fromDouble(Double.NaN)
          else {
            val yearRaw = nums(0).toInt
            val year =
              if yearRaw >= 0 && yearRaw <= 99 then yearRaw + 1900 else yearRaw
            val month = if nums.length > 1 then nums(1).toInt else 0
            val day = if nums.length > 2 then nums(2).toInt else 1
            val hour = if nums.length > 3 then nums(3).toInt else 0
            val minute = if nums.length > 4 then nums(4).toInt else 0
            val second = if nums.length > 5 then nums(5).toInt else 0
            val ms = if nums.length > 6 then nums(6).toInt else 0
            val ldt = LocalDateTime.of(
              year,
              month + 1,
              day,
              hour,
              minute,
              second,
              ms * 1000000
            )
            JSValue.fromDouble(
              ldt.toInstant(ZoneOffset.UTC).toEpochMilli.toDouble
            )
          }
        }
    )

    val dateToISOString = NativeFunction(
      name = "toISOString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "toISOString")
        if value.isNaN then ctx.throwRangeError("Invalid time value")
        JSValue.fromString(formatToISOString(value))
    )

    val dateToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "toString")
        if value.isNaN then JSValue.fromString("Invalid Date")
        else JSValue.fromString(formatToString(value))
    )

    val dateGetTime = NativeFunction(
      name = "getTime",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getTime")
        JSValue.fromDouble(value)
    )

    val dateValueOf = NativeFunction(
      name = "valueOf",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "valueOf")
        JSValue.fromDouble(value)
    )

    val dateSetUTCHours = NativeFunction(
      name = "setUTCHours",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (obj, value) = requireDateObject(args, "setUTCHours")
        if value.isNaN then {
          setDateValue(obj, Double.NaN)
          JSValue.fromDouble(Double.NaN)
        }
        else {
          val instant = Instant.ofEpochMilli(value.toLong)
          val base = ZonedDateTime.ofInstant(instant, ZoneOffset.UTC)
          val hour =
            if args.length > 1 then toMillisOrNaN(args(1)).toInt
            else base.getHour
          val minute =
            if args.length > 2 then toMillisOrNaN(args(2)).toInt
            else base.getMinute
          val second =
            if args.length > 3 then toMillisOrNaN(args(3)).toInt
            else base.getSecond
          val ms =
            if args.length > 4 then toMillisOrNaN(args(4)).toInt
            else base.getNano / 1000000
          val updated = base
            .withHour(hour)
            .withMinute(minute)
            .withSecond(second)
            .withNano(ms * 1000000)
          val newMillis = updated.toInstant.toEpochMilli.toDouble
          setDateValue(obj, newMillis)
          JSValue.fromDouble(newMillis)
        }
    )

    val dateGetFullYear = NativeFunction(
      name = "getFullYear",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getFullYear")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else {
          val zdt = ZonedDateTime.ofInstant(
            Instant.ofEpochMilli(value.toLong),
            ZoneId.systemDefault()
          )
          JSValue.fromInt(zdt.getYear)
        }
    )

    val dateGetMonth = NativeFunction(
      name = "getMonth",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getMonth")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else {
          val zdt = ZonedDateTime.ofInstant(
            Instant.ofEpochMilli(value.toLong),
            ZoneId.systemDefault()
          )
          JSValue.fromInt(
            zdt.getMonthValue - 1
          ) // JavaScript months are 0-indexed
        }
    )

    val dateGetDate = NativeFunction(
      name = "getDate",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getDate")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else {
          val zdt = ZonedDateTime.ofInstant(
            Instant.ofEpochMilli(value.toLong),
            ZoneId.systemDefault()
          )
          JSValue.fromInt(zdt.getDayOfMonth)
        }
    )

    val dateGetHours = NativeFunction(
      name = "getHours",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getHours")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else {
          val zdt = ZonedDateTime.ofInstant(
            Instant.ofEpochMilli(value.toLong),
            ZoneId.systemDefault()
          )
          JSValue.fromInt(zdt.getHour)
        }
    )

    val dateGetMinutes = NativeFunction(
      name = "getMinutes",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getMinutes")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else {
          val zdt = ZonedDateTime.ofInstant(
            Instant.ofEpochMilli(value.toLong),
            ZoneId.systemDefault()
          )
          JSValue.fromInt(zdt.getMinute)
        }
    )

    val dateGetSeconds = NativeFunction(
      name = "getSeconds",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getSeconds")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else {
          val zdt = ZonedDateTime.ofInstant(
            Instant.ofEpochMilli(value.toLong),
            ZoneId.systemDefault()
          )
          JSValue.fromInt(zdt.getSecond)
        }
    )

    val dateGetDay = NativeFunction(
      name = "getDay",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getDay")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else {
          val zdt = ZonedDateTime.ofInstant(
            Instant.ofEpochMilli(value.toLong),
            ZoneId.systemDefault()
          )
          // JavaScript: Sunday = 0, Monday = 1, ..., Saturday = 6
          // Java: Monday = 1, ..., Sunday = 7
          val javaDay = zdt.getDayOfWeek.getValue
          JSValue.fromInt(if javaDay == 7 then 0 else javaDay)
        }
    )

    val dateGetMilliseconds = NativeFunction(
      name = "getMilliseconds",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getMilliseconds")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else {
          val zdt = ZonedDateTime.ofInstant(
            Instant.ofEpochMilli(value.toLong),
            ZoneId.systemDefault()
          )
          JSValue.fromInt(zdt.getNano / 1000000)
        }
    )

    datePrototype.defineProperty(
      "toISOString",
      JSValue.Native(dateToISOString),
      enumerable = false
    )
    datePrototype.defineProperty(
      "toString",
      JSValue.Native(dateToString),
      enumerable = false
    )
    datePrototype.defineProperty(
      "getTime",
      JSValue.Native(dateGetTime),
      enumerable = false
    )
    datePrototype.defineProperty(
      "valueOf",
      JSValue.Native(dateValueOf),
      enumerable = false
    )
    datePrototype.defineProperty(
      "setUTCHours",
      JSValue.Native(dateSetUTCHours),
      enumerable = false
    )
    datePrototype.defineProperty(
      "getFullYear",
      JSValue.Native(dateGetFullYear),
      enumerable = false
    )
    datePrototype.defineProperty(
      "getMonth",
      JSValue.Native(dateGetMonth),
      enumerable = false
    )
    datePrototype.defineProperty(
      "getDate",
      JSValue.Native(dateGetDate),
      enumerable = false
    )
    datePrototype.defineProperty(
      "getHours",
      JSValue.Native(dateGetHours),
      enumerable = false
    )
    datePrototype.defineProperty(
      "getMinutes",
      JSValue.Native(dateGetMinutes),
      enumerable = false
    )
    datePrototype.defineProperty(
      "getSeconds",
      JSValue.Native(dateGetSeconds),
      enumerable = false
    )
    datePrototype.defineProperty(
      "getDay",
      JSValue.Native(dateGetDay),
      enumerable = false
    )
    datePrototype.defineProperty(
      "getMilliseconds",
      JSValue.Native(dateGetMilliseconds),
      enumerable = false
    )

    dateConstructor.funcObj.defineProperty(
      "now",
      JSValue.Native(dateNow),
      enumerable = false
    )
    dateConstructor.funcObj.defineProperty(
      "parse",
      JSValue.Native(dateParse),
      enumerable = false
    )
    dateConstructor.funcObj.defineProperty(
      "UTC",
      JSValue.Native(dateUTC),
      enumerable = false
    )

    datePrototype.defineProperty(
      "constructor",
      JSValue.Native(dateConstructor),
      enumerable = false
    )(using ctx)
    ctx.global.set("Date", JSValue.Native(dateConstructor))
  }
}
