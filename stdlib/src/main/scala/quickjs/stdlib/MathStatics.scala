package quickjs.stdlib

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject
import scala.util.Random

/** JavaScript Math object with functions.
  *
  * Provides Math.abs(), Math.floor(), Math.random(), etc.
  */
object MathStatics:
  /** Initialize Math object */
  def initialize()(using ctx: JSContext): Unit =
    val mathObj = JSObject(prototype = null, extensible = true)

    // Math.abs(x) - absolute value
    val absFunc = NativeFunction("abs", (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        // args(0) is 'this' (Math object), args(1) is the actual argument
        val x = args(1).toNumber
        JSValue.fromDouble(math.abs(x))
    )
    mathObj.set("abs", JSValue.Native(absFunc))

    // Math.floor(x) - round down to nearest integer
    val floorFunc = NativeFunction("floor", (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        val x = args(1).toNumber
        JSValue.fromDouble(math.floor(x))
    )
    mathObj.set("floor", JSValue.Native(floorFunc))

    // Math.ceil(x) - round up to nearest integer
    val ceilFunc = NativeFunction("ceil", (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        val x = args(1).toNumber
        JSValue.fromDouble(math.ceil(x))
    )
    mathObj.set("ceil", JSValue.Native(ceilFunc))

    // Math.round(x) - round to nearest integer
    val roundFunc = NativeFunction("round", (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        val x = args(1).toNumber
        JSValue.fromDouble(math.round(x))
    )
    mathObj.set("round", JSValue.Native(roundFunc))

    // Math.max(x, y, ...) - maximum value
    val maxFunc = NativeFunction("max", (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        // args(0) is 'this', rest are actual arguments
        val values = args.drop(1).map(_.toNumber)
        if values.isEmpty then
          JSValue.fromInt(0)
        else
          JSValue.fromDouble(values.max)
    )
    mathObj.set("max", JSValue.Native(maxFunc))

    // Math.min(x, y, ...) - minimum value
    val minFunc = NativeFunction("min", (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        // args(0) is 'this', rest are actual arguments
        val values = args.drop(1).map(_.toNumber)
        if values.isEmpty then
          JSValue.fromInt(0)
        else
          JSValue.fromDouble(values.min)
    )
    mathObj.set("min", JSValue.Native(minFunc))

    // Math.pow(x, y) - x raised to power y
    val powFunc = NativeFunction("pow", (args, context) =>
      if args.length < 3 then
        JSValue.fromInt(1)
      else
        // args(0) is 'this', args(1) is x, args(2) is y
        val x = args(1).toNumber
        val y = args(2).toNumber
        JSValue.fromDouble(math.pow(x, y))
    )
    mathObj.set("pow", JSValue.Native(powFunc))

    // Math.sqrt(x) - square root
    val sqrtFunc = NativeFunction("sqrt", (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        val x = args(1).toNumber
        JSValue.fromDouble(math.sqrt(x))
    )
    mathObj.set("sqrt", JSValue.Native(sqrtFunc))

    // Math.random() - random number between 0 and 1
    val randomFunc = NativeFunction("random", (args, context) =>
      JSValue.fromDouble(Random.nextDouble())
    )
    mathObj.set("random", JSValue.Native(randomFunc))

    // Math.sin(x) - sine
    val sinFunc = NativeFunction("sin", (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        val x = args(1).toNumber
        JSValue.fromDouble(math.sin(x))
    )
    mathObj.set("sin", JSValue.Native(sinFunc))

    // Math.cos(x) - cosine
    val cosFunc = NativeFunction("cos", (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        val x = args(1).toNumber
        JSValue.fromDouble(math.cos(x))
    )
    mathObj.set("cos", JSValue.Native(cosFunc))

    // Math.tan(x) - tangent
    val tanFunc = NativeFunction("tan", (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        val x = args(1).toNumber
        JSValue.fromDouble(math.tan(x))
    )
    mathObj.set("tan", JSValue.Native(tanFunc))

    // Math.PI - constant
    mathObj.set("PI", JSValue.fromDouble(math.Pi))

    // Math.E - constant
    mathObj.set("E", JSValue.fromDouble(math.E))

    // Math.SQRT2 - constant
    mathObj.set("SQRT2", JSValue.fromDouble(math.sqrt(2)))

    // Math.SQRT1_2 - constant
    mathObj.set("SQRT1_2", JSValue.fromDouble(1.0 / math.sqrt(2)))

    // Math.LN2 - constant
    mathObj.set("LN2", JSValue.fromDouble(math.log(2)))

    // Math.LN10 - constant
    mathObj.set("LN10", JSValue.fromDouble(math.log(10)))

    // Math.LOG2E - constant
    mathObj.set("LOG2E", JSValue.fromDouble(1.0 / math.log(2)))

    // Math.LOG10E - constant
    mathObj.set("LOG10E", JSValue.fromDouble(1.0 / math.log(10)))

    ctx.global.set("Math", JSValue.Object(mathObj))
