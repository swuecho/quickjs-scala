package quickjs.stdlib

import quickjs.value.JSValue
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject
import scala.util.Random

/** JavaScript Math object with functions.
  *
  * Provides Math.abs(), Math.floor(), Math.random(), etc.
  */
object MathStatics:
  import NativeFunctionBuilder._

  /** Initialize Math object */
  def initialize()(using ctx: JSContext): Unit =
    val mathObj = JSObject(prototype = null, extensible = true)

    // Unary math operations: abs, floor, ceil, round, sqrt, sin, cos, tan
    mathObj.set("abs", JSValue.Native(unaryMathOp("abs", math.abs)))
    mathObj.set("floor", JSValue.Native(unaryMathOp("floor", math.floor)))
    mathObj.set("ceil", JSValue.Native(unaryMathOp("ceil", math.ceil)))
    mathObj.set("round", JSValue.Native(unaryMathOp("round", math.round)))
    mathObj.set("sqrt", JSValue.Native(unaryMathOp("sqrt", math.sqrt)))
    mathObj.set("sin", JSValue.Native(unaryMathOp("sin", math.sin)))
    mathObj.set("cos", JSValue.Native(unaryMathOp("cos", math.cos)))
    mathObj.set("tan", JSValue.Native(unaryMathOp("tan", math.tan)))

    // Binary math operations: pow
    mathObj.set("pow", JSValue.Native(binaryMathOp("pow", math.pow)))

    // Variadic math operations: max, min
    mathObj.set("max", JSValue.Native(variadicMathOp("max", _.max)))
    mathObj.set("min", JSValue.Native(variadicMathOp("min", _.min)))

    // Nullary operations: random
    mathObj.set("random", JSValue.Native(nullaryOp("random", JSValue.fromDouble(Random.nextDouble()))))

    // Math constants
    mathObj.set("PI", JSValue.fromDouble(math.Pi))
    mathObj.set("E", JSValue.fromDouble(math.E))
    mathObj.set("SQRT2", JSValue.fromDouble(math.sqrt(2)))
    mathObj.set("SQRT1_2", JSValue.fromDouble(1.0 / math.sqrt(2)))
    mathObj.set("LN2", JSValue.fromDouble(math.log(2)))
    mathObj.set("LN10", JSValue.fromDouble(math.log(10)))
    mathObj.set("LOG2E", JSValue.fromDouble(1.0 / math.log(2)))
    mathObj.set("LOG10E", JSValue.fromDouble(1.0 / math.log(10)))

    ctx.global.set("Math", JSValue.Object(mathObj))
