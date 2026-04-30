package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import scala.util.Random
import java.math.{BigDecimal, MathContext}

/** Math built-in methods: Math.abs, Math.floor, Math.ceil, etc. */
object MathBuiltins:
  import quickjs.objmodel.JSObject

  def initialize(ctx: JSContext): Unit =
    given JSContext = ctx

    val mathObj = JSObject(prototype = null, extensible = true)

    val absFunc = NativeFunction("abs", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.abs(args(1).toNumber))
    )
    mathObj.set("abs", JSValue.Native(absFunc))

    val floorFunc = NativeFunction("floor", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.floor(args(1).toNumber))
    )
    mathObj.set("floor", JSValue.Native(floorFunc))

    val ceilFunc = NativeFunction("ceil", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.ceil(args(1).toNumber))
    )
    mathObj.set("ceil", JSValue.Native(ceilFunc))

    val roundFunc = NativeFunction("round", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.round(args(1).toNumber))
    )
    mathObj.set("round", JSValue.Native(roundFunc))

    val maxFunc = NativeFunction("max", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        val values = args.drop(1).map(_.toNumber)
        JSValue.fromDouble(values.max)
    )
    mathObj.set("max", JSValue.Native(maxFunc))

    val minFunc = NativeFunction("min", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        val values = args.drop(1).map(_.toNumber)
        JSValue.fromDouble(values.min)
    )
    mathObj.set("min", JSValue.Native(minFunc))

    val powFunc = NativeFunction("pow", (args, _) =>
      if args.length < 3 then JSValue.fromInt(1)
      else JSValue.fromDouble(math.pow(args(1).toNumber, args(2).toNumber))
    )
    mathObj.set("pow", JSValue.Native(powFunc))

    val sqrtFunc = NativeFunction("sqrt", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.sqrt(args(1).toNumber))
    )
    mathObj.set("sqrt", JSValue.Native(sqrtFunc))

    val randomFunc = NativeFunction("random", (_, _) =>
      JSValue.fromDouble(Random.nextDouble())
    )
    mathObj.set("random", JSValue.Native(randomFunc))

    val sinFunc = NativeFunction("sin", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.sin(args(1).toNumber))
    )
    mathObj.set("sin", JSValue.Native(sinFunc))

    val cosFunc = NativeFunction("cos", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.cos(args(1).toNumber))
    )
    mathObj.set("cos", JSValue.Native(cosFunc))

    val tanFunc = NativeFunction("tan", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.tan(args(1).toNumber))
    )
    mathObj.set("tan", JSValue.Native(tanFunc))

    val asinFunc = NativeFunction("asin", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.asin(args(1).toNumber))
    )
    mathObj.set("asin", JSValue.Native(asinFunc))

    val acosFunc = NativeFunction("acos", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.acos(args(1).toNumber))
    )
    mathObj.set("acos", JSValue.Native(acosFunc))

    val atanFunc = NativeFunction("atan", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.atan(args(1).toNumber))
    )
    mathObj.set("atan", JSValue.Native(atanFunc))

    val atan2Func = NativeFunction("atan2", (args, _) =>
      if args.length < 3 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.atan2(args(1).toNumber, args(2).toNumber))
    )
    mathObj.set("atan2", JSValue.Native(atan2Func))

    val imulFunc = NativeFunction("imul", (args, _) =>
      if args.length < 3 then JSValue.fromInt(0)
      else
        val a = args(1).toNumber.toInt
        val b = args(2).toNumber.toInt
        JSValue.fromInt(a * b)
    )
    mathObj.set("imul", JSValue.Native(imulFunc))

    val froundFunc = NativeFunction("fround", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(args(1).toNumber.toFloat.toDouble)
    )
    mathObj.set("fround", JSValue.Native(froundFunc))

    val hypotFunc = NativeFunction("hypot", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        var result = 0.0
        var i = 1
        while i < args.length do
          result = math.hypot(result, args(i).toNumber)
          i += 1
        JSValue.fromDouble(result)
    )
    mathObj.set("hypot", JSValue.Native(hypotFunc))

    val expFunc = NativeFunction("exp", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.exp(args(1).toNumber))
    )
    mathObj.set("exp", JSValue.Native(expFunc))

    val logFunc = NativeFunction("log", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.log(args(1).toNumber))
    )
    mathObj.set("log", JSValue.Native(logFunc))

    val log10Func = NativeFunction("log10", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.log10(args(1).toNumber))
    )
    mathObj.set("log10", JSValue.Native(log10Func))

    val log2Func = NativeFunction("log2", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.log(args(1).toNumber) / math.log(2.0))
    )
    mathObj.set("log2", JSValue.Native(log2Func))

    val truncFunc = NativeFunction("trunc", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        val value = args(1).toNumber
        val truncated = if value < 0 then math.ceil(value) else math.floor(value)
        JSValue.fromDouble(truncated)
    )
    mathObj.set("trunc", JSValue.Native(truncFunc))

    val signFunc = NativeFunction("sign", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        val value = args(1).toNumber
        if value.isNaN then
          JSValue.Float64(Double.NaN)
        else if value == 0.0 then
          JSValue.Float64(value)
        else if value > 0 then
          JSValue.fromInt(1)
        else
          JSValue.fromInt(-1)
    )
    mathObj.set("sign", JSValue.Native(signFunc))

    val sumPreciseFunc = NativeFunction("sumPrecise", (args, ctx) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        args(1) match
          case JSValue.JSArrayVal(arr) =>
            var sum = BigDecimal.ZERO
            var i = 0
            while i < arr.getLength do
              val value = arr.get(i).toNumber
              sum = sum.add(BigDecimal(value, MathContext.DECIMAL128))
              i += 1
            JSValue.fromDouble(sum.doubleValue())
          case _ =>
            ctx.throwTypeError("Math.sumPrecise expects an array")
    )
    mathObj.set("sumPrecise", JSValue.Native(sumPreciseFunc))

    mathObj.set("PI", JSValue.fromDouble(math.Pi))
    mathObj.set("E", JSValue.fromDouble(math.E))
    mathObj.set("SQRT2", JSValue.fromDouble(math.sqrt(2)))
    mathObj.set("SQRT1_2", JSValue.fromDouble(1.0 / math.sqrt(2)))
    mathObj.set("LN2", JSValue.fromDouble(math.log(2)))
    mathObj.set("LN10", JSValue.fromDouble(math.log(10)))
    mathObj.set("LOG2E", JSValue.fromDouble(1.0 / math.log(2)))
    mathObj.set("LOG10E", JSValue.fromDouble(1.0 / math.log(10)))

    ctx.global.set("Math", JSValue.Object(mathObj))
