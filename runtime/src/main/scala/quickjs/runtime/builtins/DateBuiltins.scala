package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import java.time.{
  Instant,
  LocalDate,
  LocalDateTime,
  ZoneId,
  ZoneOffset,
  ZonedDateTime
}
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
          // Brand check: only objects created by the Date constructor carry the
          // non-configurable [[DateValue]] slot.
          if obj.getOwnProperty("__dateValue").isEmpty then
            ctx.throwTypeError(
              s"Date.prototype.$method called on non-Date object"
            )
          (obj, getDateValue(obj))
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
      length = 0,
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
      length = 7,
      impl = (args, _) =>
        val actualArgs = if args.length >= 2 then args.drop(1) else args
        if actualArgs.isEmpty then JSValue.fromDouble(Double.NaN)
        else {
          val nums = actualArgs.take(7).map(toMillisOrNaN)
          if nums.exists(_.isNaN) then JSValue.fromDouble(Double.NaN)
          else {
            val yearRaw = nums(0).toLong
            val year =
              if yearRaw >= 0 && yearRaw <= 99 then yearRaw + 1900 else yearRaw
            val month = if nums.length > 1 then nums(1).toLong else 0L
            val yearWithMonth = year + Math.floorDiv(month, 12L)
            val normalizedMonth = Math.floorMod(month, 12L).toInt
            if yearWithMonth < -999999999L || yearWithMonth > 999999999L then
              JSValue.fromDouble(Double.NaN)
            else {
              val firstDay = LocalDate
                .of(yearWithMonth.toInt, normalizedMonth + 1, 1)
                .toEpochDay
                .toDouble
              val day = if nums.length > 2 then nums(2) else 1.0
              val hour = if nums.length > 3 then nums(3) else 0.0
              val minute = if nums.length > 4 then nums(4) else 0.0
              val second = if nums.length > 5 then nums(5) else 0.0
              val ms = if nums.length > 6 then nums(6) else 0.0
              val dayNumber = firstDay + day - 1.0
              val time =
                hour * 3600000.0 + minute * 60000.0 + second * 1000.0 + ms
              val result = dayNumber * 86400000.0 + time
              JSValue.fromDouble(
                if result.isInfinite || math.abs(result) > 8.64e15 then
                  Double.NaN
                else result.toLong.toDouble
              )
            }
          }
        }
    )

    val dateToISOString = NativeFunction(
      name = "toISOString",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "toISOString")
        if value.isNaN then ctx.throwRangeError("Invalid time value")
        JSValue.fromString(formatToISOString(value))
    )

    val dateToJSON = NativeFunction(
      name = "toJSON",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.isEmpty || args(0) == JSValue.Null || args(0) == JSValue.Undefined then
          ctx.throwTypeError("Date.prototype.toJSON called on null or undefined")
        val receiver = args(0)
        val primitive = BuiltinHelpers.toPrimitiveNumber(receiver)
        val nonFiniteNumber = primitive match {
          case JSValue.Float64(d) => !d.isFinite
          case _                  => false
        }
        if nonFiniteNumber then JSValue.Null
        else {
          val method = BuiltinHelpers.getPropertyWithGetter(receiver, "toISOString")
          if !BuiltinHelpers.isCallable(method) then
            ctx.throwTypeError("toISOString is not callable")
          BuiltinHelpers.callFunctionWithThis(method, receiver, Array.empty)
        }
    )

    val dateToString = NativeFunction(
      name = "toString",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "toString")
        if value.isNaN then JSValue.fromString("Invalid Date")
        else JSValue.fromString(formatToString(value))
    )

    val dateGetTime = NativeFunction(
      name = "getTime",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getTime")
        JSValue.fromDouble(value)
    )

    val dateValueOf = NativeFunction(
      name = "valueOf",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "valueOf")
        JSValue.fromDouble(value)
    )

    val dateSetUTCHours = NativeFunction(
      name = "setUTCHours",
      length = 4,
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
      length = 0,
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
      length = 0,
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
      length = 0,
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
      length = 0,
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
      length = 0,
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
      length = 0,
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
      length = 0,
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
      length = 0,
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

    final case class DateFields(
        year: Int,
        month: Int,
        day: Int,
        hour: Int,
        minute: Int,
        second: Int,
        millisecond: Int
    )

    def dateFields(millis: Double, utc: Boolean): Option[DateFields] =
      if millis.isNaN || millis.isInfinite then None
      else {
        val zone = if utc then ZoneOffset.UTC else ZoneId.systemDefault()
        val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis.toLong), zone)
        Some(
          DateFields(
            zdt.getYear,
            zdt.getMonthValue - 1,
            zdt.getDayOfMonth,
            zdt.getHour,
            zdt.getMinute,
            zdt.getSecond,
            zdt.getNano / 1000000
          )
        )
      }

    def normalizedMillis(fields: DateFields, utc: Boolean): Double =
      try {
        val normalized = LocalDateTime
          .of(fields.year, 1, 1, 0, 0)
          .plusMonths(fields.month.toLong)
          .plusDays(fields.day.toLong - 1)
          .plusHours(fields.hour.toLong)
          .plusMinutes(fields.minute.toLong)
          .plusSeconds(fields.second.toLong)
          .plusNanos(fields.millisecond.toLong * 1000000L)
        val instant =
          if utc then normalized.toInstant(ZoneOffset.UTC)
          else normalized.atZone(ZoneId.systemDefault()).toInstant
        val result = instant.toEpochMilli.toDouble
        if math.abs(result) > 8.64e15 then Double.NaN else result
      }
      catch case _: java.time.DateTimeException => Double.NaN

    def dateGetter(
        name: String,
        utc: Boolean,
        select: DateFields => Int
    ): NativeFunction =
      NativeFunction(
        name = name,
        length = 0,
        impl = (args, ctx) =>
          given JSContext = ctx
          val (_, millis) = requireDateObject(args, name)
          dateFields(millis, utc)
            .map(fields => JSValue.fromInt(select(fields)))
            .getOrElse(JSValue.fromDouble(Double.NaN))
      )

    def dateSetter(
        name: String,
        arity: Int,
        utc: Boolean,
        reviveInvalid: Boolean,
        update: (DateFields, Array[Double]) => DateFields
    ): NativeFunction =
      NativeFunction(
        name = name,
        length = arity,
        impl = (args, ctx) =>
          given JSContext = ctx
          val (obj, millis) = requireDateObject(args, name)
          val values = args.drop(1).map(toMillisOrNaN)
          val baseMillis =
            if millis.isNaN && reviveInvalid then 0.0 else millis
          val result =
            if values.isEmpty || values.exists(v => v.isNaN || v.isInfinite) then
              Double.NaN
            else
              dateFields(baseMillis, utc) match {
                case Some(base) => normalizedMillis(update(base, values), utc)
                case None       => Double.NaN
              }
          setDateValue(obj, result)
          JSValue.fromDouble(result)
      )

    val dateGetTimezoneOffset = NativeFunction(
      name = "getTimezoneOffset",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, millis) = requireDateObject(args, "getTimezoneOffset")
        if millis.isNaN then JSValue.fromDouble(Double.NaN)
        else {
          val instant = Instant.ofEpochMilli(millis.toLong)
          val seconds = ZoneId.systemDefault().getRules.getOffset(instant).getTotalSeconds
          JSValue.fromInt(-(seconds / 60))
        }
    )

    val generatedDateMethods: Seq[(String, NativeFunction)] = Seq(
      "getUTCFullYear" -> dateGetter("getUTCFullYear", true, _.year),
      "getUTCMonth" -> dateGetter("getUTCMonth", true, _.month),
      "getUTCDate" -> dateGetter("getUTCDate", true, _.day),
      "getUTCDay" -> NativeFunction(
        name = "getUTCDay",
        length = 0,
        impl = (args, ctx) =>
          given JSContext = ctx
          val (_, millis) = requireDateObject(args, "getUTCDay")
          if millis.isNaN then JSValue.fromDouble(Double.NaN)
          else {
            val day = ZonedDateTime
              .ofInstant(Instant.ofEpochMilli(millis.toLong), ZoneOffset.UTC)
              .getDayOfWeek.getValue
            JSValue.fromInt(if day == 7 then 0 else day)
          }
      ),
      "getUTCHours" -> dateGetter("getUTCHours", true, _.hour),
      "getUTCMinutes" -> dateGetter("getUTCMinutes", true, _.minute),
      "getUTCSeconds" -> dateGetter("getUTCSeconds", true, _.second),
      "getUTCMilliseconds" -> dateGetter("getUTCMilliseconds", true, _.millisecond),
      "setMilliseconds" -> dateSetter("setMilliseconds", 1, false, false, (b, v) => b.copy(millisecond = v(0).toInt)),
      "setUTCMilliseconds" -> dateSetter("setUTCMilliseconds", 1, true, false, (b, v) => b.copy(millisecond = v(0).toInt)),
      "setSeconds" -> dateSetter("setSeconds", 2, false, false, (b, v) => b.copy(second = v(0).toInt, millisecond = if v.length > 1 then v(1).toInt else b.millisecond)),
      "setUTCSeconds" -> dateSetter("setUTCSeconds", 2, true, false, (b, v) => b.copy(second = v(0).toInt, millisecond = if v.length > 1 then v(1).toInt else b.millisecond)),
      "setMinutes" -> dateSetter("setMinutes", 3, false, false, (b, v) => b.copy(minute = v(0).toInt, second = if v.length > 1 then v(1).toInt else b.second, millisecond = if v.length > 2 then v(2).toInt else b.millisecond)),
      "setUTCMinutes" -> dateSetter("setUTCMinutes", 3, true, false, (b, v) => b.copy(minute = v(0).toInt, second = if v.length > 1 then v(1).toInt else b.second, millisecond = if v.length > 2 then v(2).toInt else b.millisecond)),
      "setHours" -> dateSetter("setHours", 4, false, false, (b, v) => b.copy(hour = v(0).toInt, minute = if v.length > 1 then v(1).toInt else b.minute, second = if v.length > 2 then v(2).toInt else b.second, millisecond = if v.length > 3 then v(3).toInt else b.millisecond)),
      "setUTCHours" -> dateSetter("setUTCHours", 4, true, false, (b, v) => b.copy(hour = v(0).toInt, minute = if v.length > 1 then v(1).toInt else b.minute, second = if v.length > 2 then v(2).toInt else b.second, millisecond = if v.length > 3 then v(3).toInt else b.millisecond)),
      "setDate" -> dateSetter("setDate", 1, false, false, (b, v) => b.copy(day = v(0).toInt)),
      "setUTCDate" -> dateSetter("setUTCDate", 1, true, false, (b, v) => b.copy(day = v(0).toInt)),
      "setMonth" -> dateSetter("setMonth", 2, false, false, (b, v) => b.copy(month = v(0).toInt, day = if v.length > 1 then v(1).toInt else b.day)),
      "setUTCMonth" -> dateSetter("setUTCMonth", 2, true, false, (b, v) => b.copy(month = v(0).toInt, day = if v.length > 1 then v(1).toInt else b.day)),
      "setFullYear" -> dateSetter("setFullYear", 3, false, true, (b, v) => b.copy(year = v(0).toInt, month = if v.length > 1 then v(1).toInt else b.month, day = if v.length > 2 then v(2).toInt else b.day)),
      "setUTCFullYear" -> dateSetter("setUTCFullYear", 3, true, true, (b, v) => b.copy(year = v(0).toInt, month = if v.length > 1 then v(1).toInt else b.month, day = if v.length > 2 then v(2).toInt else b.day))
    )

    val dateSetTime = NativeFunction(
      name = "setTime",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val (obj, _) = requireDateObject(args, "setTime")
        val raw = args.lift(1).map(toMillisOrNaN).getOrElse(Double.NaN)
        val value = if raw.isNaN || raw.isInfinite || math.abs(raw) > 8.64e15 then Double.NaN else raw.toLong.toDouble
        setDateValue(obj, value)
        JSValue.fromDouble(value)
    )

    def dateStringMethod(name: String, format: (Double => String)): NativeFunction =
      NativeFunction(
        name = name,
        length = 0,
        impl = (args, ctx) =>
          given JSContext = ctx
          val (_, millis) = requireDateObject(args, name)
          if millis.isNaN then JSValue.fromString("Invalid Date")
          else JSValue.fromString(format(millis))
      )

    val generatedStringMethods = Seq(
      "toUTCString" -> dateStringMethod("toUTCString", millis =>
        DateTimeFormatter.RFC_1123_DATE_TIME.format(
          ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis.toLong), ZoneOffset.UTC)
        )
      ),
      "toDateString" -> dateStringMethod("toDateString", millis =>
        DateTimeFormatter.ofPattern("EEE MMM dd yyyy", Locale.ENGLISH).format(
          ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis.toLong), ZoneId.systemDefault())
        )
      ),
      "toTimeString" -> dateStringMethod("toTimeString", millis =>
        DateTimeFormatter.ofPattern("HH:mm:ss 'GMT'XXX", Locale.ENGLISH).format(
          ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis.toLong), ZoneId.systemDefault())
        )
      ),
      "toLocaleString" -> dateStringMethod("toLocaleString", formatToString),
      "toLocaleDateString" -> dateStringMethod("toLocaleDateString", millis =>
        DateTimeFormatter.ofPattern("yyyy/M/d", Locale.getDefault).format(
          ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis.toLong), ZoneId.systemDefault())
        )
      ),
      "toLocaleTimeString" -> dateStringMethod("toLocaleTimeString", millis =>
        DateTimeFormatter.ofPattern("HH:mm:ss", Locale.getDefault).format(
          ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis.toLong), ZoneId.systemDefault())
        )
      )
    )

    datePrototype.defineProperty(
      "toISOString",
      JSValue.Native(dateToISOString),
      enumerable = false
    )
    datePrototype.defineProperty(
      "toJSON",
      JSValue.Native(dateToJSON),
      enumerable = false,
      writable = true,
      configurable = true
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
    datePrototype.defineProperty(
      "getTimezoneOffset",
      JSValue.Native(dateGetTimezoneOffset),
      enumerable = false
    )
    datePrototype.defineProperty(
      "setTime",
      JSValue.Native(dateSetTime),
      enumerable = false
    )
    generatedDateMethods.foreach { case (name, function) =>
      datePrototype.defineProperty(
        name,
        JSValue.Native(function),
        enumerable = false,
        writable = true,
        configurable = true
      )
    }
    generatedStringMethods.foreach { case (name, function) =>
      datePrototype.defineProperty(
        name,
        JSValue.Native(function),
        enumerable = false,
        writable = true,
        configurable = true
      )
    }

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
