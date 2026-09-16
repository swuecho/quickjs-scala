package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import scala.util.Random
import java.math.{BigDecimal, MathContext}

/** Math built-in methods: Math.abs, Math.floor, Math.ceil, etc. */
object MathBuiltins {
  import quickjs.objmodel.JSObject

  def initialize(ctx: JSContext): Unit = {
    given JSContext = ctx

    val mathObj = JSObject(prototype = ctx.objectPrototype, extensible = true)

    // Helper: register a math function with correct property descriptor
    def registerFunc(
        name: String,
        length: Int,
        impl: (Array[JSValue], JSContext) => JSValue
    ): Unit = {
      // Math methods ignore `this`. Method calls (`Math.ceil(x)`) pass the
      // Math object as the receiver; extracted calls (`const f = Math.ceil`)
      // do not. Normalize to the receiver-first shape the impls expect.
      val normalizedImpl = (args: Array[JSValue], callCtx: JSContext) => {
        val hasMathReceiver = args.nonEmpty && (args(0) match {
          case JSValue.Object(o) => o eq mathObj
          case _                 => false
        })
        val normalized =
          if hasMathReceiver then args
          else {
            val withReceiver = new Array[JSValue](args.length + 1)
            withReceiver(0) = JSValue.Object(mathObj)
            Array.copy(args, 0, withReceiver, 1, args.length)
            withReceiver
          }
        impl(normalized, callCtx)
      }
      val func = NativeFunction(
        name,
        normalizedImpl,
        quickjs.objmodel.JSObject(
          prototype = ctx.functionPrototype,
          extensible = true
        ),
        length
      )
      mathObj.defineProperty(
        name,
        JSValue.Native(func),
        enumerable = false,
        writable = true,
        configurable = true
      )
    }

    // Helper: register a math constant (non-writable, non-enumerable, non-configurable)
    def registerConst(name: String, value: Double): Unit =
      mathObj.defineProperty(
        name,
        JSValue.fromDouble(value),
        enumerable = false,
        writable = false,
        configurable = false
      )

    /** ES ToNumber for a Math argument (calls valueOf/toString, propagates
      * abrupt completions and Symbol/BigInt TypeErrors).
      */
    def num(value: JSValue)(using ctx: JSContext): Double =
      BuiltinHelpers.toNumber(value)

    /** ES ToInt32. */
    def toInt32(value: JSValue)(using ctx: JSContext): Int = {
      val d = BuiltinHelpers.toNumber(value)
      val finite = !d.isNaN && !d.isInfinite && d != 0.0
      if !finite then 0
      else {
        val rem = d % 4294967296.0
        rem.toLong.toInt
      }
    }

    /** ES Number::round, preserving -0 and infinities. */
    def jsRound(x: Double): Double =
      if x.isNaN || x.isInfinite || x == 0.0 then x
      else if x >= 0.5 && x < 1.0 then 0.0
      else if x >= -0.5 && x < 0.0 then -0.0
      else math.floor(x + 0.5)

    /** ES Math.pow special cases not covered by java.lang.Math.pow. */
    def jsPow(base: Double, exponent: Double): Double =
      if exponent.isNaN then Double.NaN
      else if exponent == 0.0 then 1.0
      else if base.isNaN then Double.NaN
      else if (base == 1.0 || base == -1.0) && exponent.isInfinite then Double.NaN
      else if base == 0.0 && exponent < 0 then
        // +0 ** negative -> +Inf, -0 ** negative odd integer -> -Inf
        if base == 0.0 && 1.0 / base < 0 && isOddInteger(exponent) then
          Double.NegativeInfinity
        else Double.PositiveInfinity
      else math.pow(base, exponent)

    def isOddInteger(x: Double): Boolean =
      x.isFinite && x == math.floor(x) && math.abs(x) < 1e18 && math
        .abs(x)
        .toLong % 2L == 1L

    def asinh(x: Double): Double =
      if x.isNaN || x.isInfinite || x == 0.0 then x
      else {
        val ax = math.abs(x)
        val r =
          if ax > 1e154 then math.log(ax) + math.log(2.0)
          else math.log1p(ax + ax * ax / (1.0 + math.sqrt(1.0 + ax * ax)))
        if x < 0 then -r else r
      }

    def atanh(x: Double): Double =
      if x.isNaN || x == 0.0 then x
      else if x == 1.0 then Double.PositiveInfinity
      else if x == -1.0 then Double.NegativeInfinity
      else if x < -1.0 || x > 1.0 then Double.NaN
      else 0.5 * math.log1p(2.0 * x / (1.0 - x))

    /** One Math argument; NaN when absent. */
    def arg(args: Array[JSValue], index: Int = 1)(using
        ctx: JSContext
    ): Double =
      if args.length <= index then Double.NaN else num(args(index))

    registerFunc(
      "abs",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.abs(arg(args)))
    )

    registerFunc(
      "floor",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.floor(arg(args)))
    )

    registerFunc(
      "ceil",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.ceil(arg(args)))
    )

    registerFunc(
      "round",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(jsRound(arg(args)))
    )

    registerFunc(
      "max",
      2,
      (args, ctx) =>
        given JSContext = ctx
        if args.length <= 1 then JSValue.fromDouble(Double.NegativeInfinity)
        else {
          // Coerce every argument in order, propagating abrupt completions.
          var result = Double.NegativeInfinity
          var foundNaN = false
          var i = 1
          while i < args.length do {
            val v = num(args(i))
            if v.isNaN then foundNaN = true
            else if v > result || (v == 0.0 && result == 0.0 && 1.0 / v > 0)
            then result = v
            i += 1
          }
          if foundNaN then JSValue.fromDouble(Double.NaN)
          else JSValue.fromDouble(result)
        }
    )

    registerFunc(
      "min",
      2,
      (args, ctx) =>
        given JSContext = ctx
        if args.length <= 1 then JSValue.fromDouble(Double.PositiveInfinity)
        else {
          var result = Double.PositiveInfinity
          var foundNaN = false
          var i = 1
          while i < args.length do {
            val v = num(args(i))
            if v.isNaN then foundNaN = true
            else if v < result || (v == 0.0 && result == 0.0 && 1.0 / v < 0)
            then result = v
            i += 1
          }
          if foundNaN then JSValue.fromDouble(Double.NaN)
          else JSValue.fromDouble(result)
        }
    )

    registerFunc(
      "pow",
      2,
      (args, ctx) =>
        given JSContext = ctx
        if args.length < 3 then JSValue.fromDouble(Double.NaN)
        else JSValue.fromDouble(jsPow(num(args(1)), num(args(2))))
    )

    registerFunc(
      "sqrt",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.sqrt(arg(args)))
    )

    registerFunc("random", 0, (_, _) => JSValue.fromDouble(Random.nextDouble()))

    registerFunc(
      "sin",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.sin(arg(args)))
    )

    registerFunc(
      "cos",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.cos(arg(args)))
    )

    registerFunc(
      "tan",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.tan(arg(args)))
    )

    registerFunc(
      "asin",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.asin(arg(args)))
    )

    registerFunc(
      "acos",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.acos(arg(args)))
    )

    registerFunc(
      "atan",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.atan(arg(args)))
    )

    registerFunc(
      "atan2",
      2,
      (args, ctx) =>
        given JSContext = ctx
        if args.length < 3 then JSValue.fromDouble(Double.NaN)
        else JSValue.fromDouble(math.atan2(num(args(1)), num(args(2))))
    )

    registerFunc(
      "acosh",
      1,
      (args, ctx) =>
        given JSContext = ctx
        val x = arg(args)
        val result =
          if x.isNaN || x < 1.0 then Double.NaN
          else if x == 1.0 then 0.0
          else if x.isInfinite then Double.PositiveInfinity
          else math.log(x + math.sqrt(x * x - 1))
        JSValue.fromDouble(result)
    )

    registerFunc(
      "asinh",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(asinh(arg(args)))
    )

    registerFunc(
      "atanh",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(atanh(arg(args)))
    )

    registerFunc(
      "cosh",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.cosh(arg(args)))
    )

    registerFunc(
      "sinh",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.sinh(arg(args)))
    )

    registerFunc(
      "tanh",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.tanh(arg(args)))
    )

    registerFunc(
      "cbrt",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.cbrt(arg(args)))
    )

    registerFunc(
      "clz32",
      1,
      (args, ctx) =>
        given JSContext = ctx
        if args.length <= 1 then JSValue.fromInt(32)
        else JSValue.fromInt(Integer.numberOfLeadingZeros(toInt32(args(1))))
    )

    registerFunc(
      "expm1",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.expm1(arg(args)))
    )

    registerFunc(
      "log1p",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.log1p(arg(args)))
    )

    registerFunc(
      "imul",
      2,
      (args, ctx) =>
        given JSContext = ctx
        if args.length < 3 then JSValue.fromInt(0)
        else {
          val a = toInt32(args(1))
          val b = toInt32(args(2))
          JSValue.fromInt(a * b)
        }
    )

    registerFunc(
      "fround",
      1,
      (args, ctx) =>
        given JSContext = ctx
        if args.length <= 1 then JSValue.fromDouble(Double.NaN)
        else JSValue.fromDouble(num(args(1)).toFloat.toDouble)
    )

    registerFunc(
      "f16round",
      1,
      (args, ctx) =>
        given JSContext = ctx
        if args.length <= 1 then JSValue.fromDouble(Double.NaN)
        else
          JSValue.fromDouble(
            BuiltinHelpers.halfBitsToDouble(
              BuiltinHelpers.doubleToHalfBits(num(args(1)))
            )
          )
    )

    registerFunc(
      "hypot",
      2,
      (args, ctx) =>
        given JSContext = ctx
        if args.length <= 1 then JSValue.fromDouble(0.0)
        else {
          var max = 0.0
          var anyNaN = false
          var anyInfinite = false
          // First pass: ToNumber in order (abrupt completions propagate) and
          // find the largest magnitude to scale by.
          val values = new Array[Double](args.length - 1)
          var i = 0
          while i < values.length do {
            val v = math.abs(num(args(i + 1)))
            values(i) = v
            if v.isNaN then anyNaN = true
            if v.isInfinite then anyInfinite = true
            if v > max then max = v
            i += 1
          }
          if anyInfinite then JSValue.fromDouble(Double.PositiveInfinity)
          else if anyNaN then JSValue.fromDouble(Double.NaN)
          else if max == 0.0 then JSValue.fromDouble(0.0)
          else {
            var sum = 0.0
            i = 0
            while i < values.length do {
              val scaled = values(i) / max
              sum += scaled * scaled
              i += 1
            }
            JSValue.fromDouble(max * math.sqrt(sum))
          }
        }
    )

    registerFunc(
      "exp",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.exp(arg(args)))
    )

    registerFunc(
      "log",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.log(arg(args)))
    )

    registerFunc(
      "log10",
      1,
      (args, ctx) =>
        given JSContext = ctx
        JSValue.fromDouble(math.log10(arg(args)))
    )

    registerFunc(
      "log2",
      1,
      (args, ctx) =>
        given JSContext = ctx
        val x = arg(args)
        val result =
          if x.isNaN || x < 0.0 then Double.NaN
          else if x == 0.0 then Double.NegativeInfinity
          else if x.isInfinite then Double.PositiveInfinity
          else {
            // Use the exact power-of-two path when possible for precision.
            val exp = math.getExponent(x)
            val mant = math.scalb(x, -exp)
            if mant == 1.0 then exp.toDouble
            else math.log(x) / math.log(2.0)
          }
        JSValue.fromDouble(result)
    )

    registerFunc(
      "trunc",
      1,
      (args, ctx) =>
        given JSContext = ctx
        val value = arg(args)
        val truncated =
          if value.isNaN || value.isInfinite || value == 0.0 then value
          else if value < 0 then math.ceil(value)
          else math.floor(value)
        JSValue.fromDouble(truncated)
    )

    registerFunc(
      "sign",
      1,
      (args, ctx) =>
        given JSContext = ctx
        val value = arg(args)
        if value.isNaN then JSValue.Float64(Double.NaN)
        else if value == 0.0 then JSValue.Float64(value)
        else if value > 0 then JSValue.fromInt(1)
        else JSValue.fromInt(-1)
    )

    registerFunc(
      "sumPrecise",
      1,
      (args, ctx) =>
        given JSContext = ctx
        {
          val iterable = if args.length > 1 then args(1) else JSValue.Undefined
          val record = BuiltinHelpers.getIteratorRecord(iterable)
          var sum = new java.math.BigDecimal(0)
          var hasNaN = false
          var hasPosInf = false
          var hasNegInf = false
          var hasPositive = false
          var hasAny = false
          var step = BuiltinHelpers.iteratorStepValue(record)
          while step.isDefined do {
            step.get match {
              case JSValue.Int32(i) =>
                hasAny = true
                // Integer zero is +0, which breaks an all-negative-zero sum.
                if i >= 0 then hasPositive = true
                sum = sum.add(java.math.BigDecimal.valueOf(i.toLong))
              case JSValue.Float64(d) =>
                hasAny = true
                if d.isNaN then hasNaN = true
                else if d.isInfinite then {
                  if d > 0 then hasPosInf = true else hasNegInf = true
                } else {
                  if d > 0 || (d == 0.0 && 1.0 / d > 0) then hasPositive = true
                  sum = sum.add(new java.math.BigDecimal(d))
                }
              case _: JSValue.BigInt =>
                BuiltinHelpers.iteratorCloseRecord(record)
                ctx.throwTypeError("Cannot convert a BigInt value to a number")
              case _ =>
                BuiltinHelpers.iteratorCloseRecord(record)
                ctx.throwTypeError("Math.sumPrecise expects numeric values")
            }
            step = BuiltinHelpers.iteratorStepValue(record)
          }
          val result =
            if hasNaN || (hasPosInf && hasNegInf) then Double.NaN
            else if hasPosInf then Double.PositiveInfinity
            else if hasNegInf then Double.NegativeInfinity
            else {
              val d = sum.doubleValue()
              // A zero sum is -0 only when there were no positive addends.
              if d == 0.0 && !hasPositive then -0.0 else d
            }
          JSValue.fromDouble(result)
        }
    )

    // Constants (non-writable, non-enumerable, non-configurable)
    registerConst("PI", math.Pi)
    registerConst("E", math.E)
    registerConst("SQRT2", math.sqrt(2))
    registerConst("SQRT1_2", 1.0 / math.sqrt(2))
    registerConst("LN2", math.log(2))
    registerConst("LN10", math.log(10))
    registerConst("LOG2E", 1.0 / math.log(2))
    registerConst("LOG10E", 1.0 / math.log(10))

    // Note: Symbol.toStringTag would be set here, but Symbol-keyed properties
    // are not yet supported in JSObject. Will be added later.

    ctx.global.defineProperty(
      "Math",
      JSValue.Object(mathObj),
      enumerable = true,
      writable = true,
      configurable = true
    )

    // Set Symbol.toStringTag on Math
    val toStringTagSym = ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.getOwnProperty("toStringTag")(using ctx) match {
          case Some(JSValue.Symbol(sym)) => Some(sym)
          case _                         => None
        }
      case _ => None
    }
    toStringTagSym.foreach { tagId =>
      mathObj.initSymbolProperty(
        tagId,
        JSValue.fromString("Math"),
        enumerable = false,
        writable = false,
        configurable = true
      )
    }
  }
}
