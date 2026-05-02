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

    // Helper: register a math function with correct property descriptor
    def registerFunc(name: String, length: Int, impl: (Array[JSValue], JSContext) => JSValue): Unit =
      val func = NativeFunction(name, impl, quickjs.objmodel.JSObject(prototype = null, extensible = true), length)
      mathObj.defineProperty(name, JSValue.Native(func), enumerable = false, writable = true, configurable = true)

    // Helper: register a math constant (non-writable, non-enumerable, non-configurable)
    def registerConst(name: String, value: Double): Unit =
      mathObj.defineProperty(name, JSValue.fromDouble(value), enumerable = false, writable = false, configurable = false)

    registerFunc("abs", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.abs(args(1).toNumber))
    )

    registerFunc("floor", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.floor(args(1).toNumber))
    )

    registerFunc("ceil", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.ceil(args(1).toNumber))
    )

    registerFunc("round", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.round(args(1).toNumber))
    )

    registerFunc("max", 2, (args, _) =>
      if args.length <= 1 then JSValue.fromDouble(Double.NegativeInfinity)
      else
        val values = args.drop(1).map(_.toNumber)
        JSValue.fromDouble(values.max)
    )

    registerFunc("min", 2, (args, _) =>
      if args.length <= 1 then JSValue.fromDouble(Double.PositiveInfinity)
      else
        val values = args.drop(1).map(_.toNumber)
        JSValue.fromDouble(values.min)
    )

    registerFunc("pow", 2, (args, _) =>
      if args.length < 3 then JSValue.fromInt(1)
      else JSValue.fromDouble(math.pow(args(1).toNumber, args(2).toNumber))
    )

    registerFunc("sqrt", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.sqrt(args(1).toNumber))
    )

    registerFunc("random", 0, (_, _) =>
      JSValue.fromDouble(Random.nextDouble())
    )

    registerFunc("sin", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.sin(args(1).toNumber))
    )

    registerFunc("cos", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.cos(args(1).toNumber))
    )

    registerFunc("tan", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.tan(args(1).toNumber))
    )

    registerFunc("asin", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.asin(args(1).toNumber))
    )

    registerFunc("acos", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.acos(args(1).toNumber))
    )

    registerFunc("atan", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.atan(args(1).toNumber))
    )

    registerFunc("atan2", 2, (args, _) =>
      if args.length < 3 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.atan2(args(1).toNumber, args(2).toNumber))
    )

    registerFunc("acosh", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromDouble(Double.NaN)
      else
        val x = args(1).toNumber
        JSValue.fromDouble(math.log(x + math.sqrt(x * x - 1)))
    )

    registerFunc("asinh", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromDouble(Double.NaN)
      else
        val x = args(1).toNumber
        if x.isInfinite then JSValue.fromDouble(x)
        else if x == 0.0 then JSValue.fromDouble(x)  // preserves -0
        else JSValue.fromDouble(math.log(x + math.sqrt(x * x + 1)))
    )

    registerFunc("atanh", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromDouble(Double.NaN)
      else
        val x = args(1).toNumber
        JSValue.fromDouble(0.5 * math.log((1 + x) / (1 - x)))
    )

    registerFunc("cosh", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(1)
      else JSValue.fromDouble(math.cosh(args(1).toNumber))
    )

    registerFunc("sinh", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.sinh(args(1).toNumber))
    )

    registerFunc("tanh", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.tanh(args(1).toNumber))
    )

    registerFunc("cbrt", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.cbrt(args(1).toNumber))
    )

    registerFunc("clz32", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(32)
      else JSValue.fromInt(Integer.numberOfLeadingZeros(args(1).toNumber.toInt))
    )

    registerFunc("expm1", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.expm1(args(1).toNumber))
    )

    registerFunc("log1p", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.log1p(args(1).toNumber))
    )

    registerFunc("imul", 2, (args, _) =>
      if args.length < 3 then JSValue.fromInt(0)
      else
        val a = args(1).toNumber.toInt
        val b = args(2).toNumber.toInt
        JSValue.fromInt(a * b)
    )

    registerFunc("fround", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(args(1).toNumber.toFloat.toDouble)
    )

    registerFunc("hypot", 2, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        var result = 0.0
        var i = 1
        while i < args.length do
          result = math.hypot(result, args(i).toNumber)
          i += 1
        JSValue.fromDouble(result)
    )

    registerFunc("exp", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.exp(args(1).toNumber))
    )

    registerFunc("log", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.log(args(1).toNumber))
    )

    registerFunc("log10", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.log10(args(1).toNumber))
    )

    registerFunc("log2", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.log(args(1).toNumber) / math.log(2.0))
    )

    registerFunc("trunc", 1, (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        val value = args(1).toNumber
        val truncated = if value < 0 then math.ceil(value) else math.floor(value)
        JSValue.fromDouble(truncated)
    )

    registerFunc("sign", 1, (args, _) =>
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

    registerFunc("sumPrecise", 1, (args, ctx) =>
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

    ctx.global.defineProperty("Math", JSValue.Object(mathObj), enumerable = true, writable = true, configurable = true)

    // Set Symbol.toStringTag on Math
    val toStringTagSym = ctx.global.get("Symbol") match
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.getOwnProperty("toStringTag")(using ctx) match
          case Some(JSValue.Symbol(sym)) => Some(sym)
          case _ => None
      case _ => None
    toStringTagSym.foreach { tagId =>
      mathObj.initSymbolProperty(tagId, JSValue.fromString("Math"), enumerable = false, writable = false, configurable = true)
    }
