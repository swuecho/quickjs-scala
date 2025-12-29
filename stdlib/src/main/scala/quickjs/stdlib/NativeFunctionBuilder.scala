package quickjs.stdlib

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject

/** Builder for creating native JavaScript functions with common patterns.
  *
  * Reduces code duplication by providing builders for frequently used
  * native function patterns like unary operations, binary operations, etc.
  */
object NativeFunctionBuilder:

  /** Create a unary math operation function.
    *
    * The function takes one argument (after 'this') and applies the operation.
    * Returns 0 if no argument is provided.
    *
    * @param name Function name
    * @param op Operation to apply to the argument
    * @return NativeFunction that implements the operation
    *
    * Example:
    * {{{
    * val absFunc = unaryMathOp("abs", math.abs)
    * // Math.abs(x) -> calls math.abs(x.toNumber)
    * }}}
    */
  def unaryMathOp(name: String, op: Double => Double): NativeFunction =
    NativeFunction(name, (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        // args(0) is 'this' (Math object), args(1) is the actual argument
        val x = args(1).toNumber
        JSValue.fromDouble(op(x))
    )

  /** Create a binary math operation function.
    *
    * The function takes two arguments (after 'this') and applies the operation.
    * Returns 1 if insufficient arguments are provided.
    *
    * @param name Function name
    * @param op Operation to apply to the two arguments
    * @return NativeFunction that implements the operation
    *
    * Example:
    * {{{
    * val powFunc = binaryMathOp("pow", math.pow)
    * // Math.pow(x, y) -> calls math.pow(x.toNumber, y.toNumber)
    * }}}
    */
  def binaryMathOp(name: String, op: (Double, Double) => Double): NativeFunction =
    NativeFunction(name, (args, context) =>
      if args.length < 3 then
        JSValue.fromInt(1)
      else
        // args(0) is 'this', args(1) is first arg, args(2) is second arg
        val x = args(1).toNumber
        val y = args(2).toNumber
        JSValue.fromDouble(op(x, y))
    )

  /** Create a variadic math operation function (like min/max).
    *
    * The function takes any number of arguments and applies the reduction operation.
    * Returns 0 if no arguments are provided.
    *
    * @param name Function name
    * @param op Reduction operation (e.g., _.min, _.max)
    * @return NativeFunction that implements the operation
    *
    * Example:
    * {{{
    * val maxFunc = variadicMathOp("max", _.max)
    * val minFunc = variadicMathOp("min", _.min)
    * // Math.max(x, y, z) -> calls Seq(x, y, z).max
    * }}}
    */
  def variadicMathOp(name: String, op: Seq[Double] => Double): NativeFunction =
    NativeFunction(name, (args, context) =>
      if args.length <= 1 then
        JSValue.fromInt(0)
      else
        // args(0) is 'this', rest are actual arguments
        val values = args.drop(1).map(_.toNumber)
        if values.isEmpty then
          JSValue.fromInt(0)
        else
          JSValue.fromDouble(op(values))
    )

  /** Create a nullary function (no arguments required).
    *
    * Useful for functions like Math.random() that don't take arguments.
    *
    * @param name Function name
    * @param impl Function implementation
    * @return NativeFunction with the given implementation
    *
    * Example:
    * {{{
    * val randomFunc = nullaryOp("random", () =>
    *   JSValue.fromDouble(Random.nextDouble())
    * )
    * }}}
    */
  def nullaryOp(name: String, impl: => JSValue): NativeFunction =
    NativeFunction(name, (_, _) => impl)

  /** Create an array method that operates on 'this'.
    *
    * The function expects 'this' to be an array and applies the operation.
    *
    * @param name Method name
    * @param impl Operation that receives the array value and arguments
    * @return NativeFunction that implements the array method
    *
    * Example:
    * {{{
    * val popFunc = arrayMethod("pop") { (arr, args) =>
    *   arr.pop()
    * }
    * }}}
    */
  def arrayMethod(name: String)(impl: JSValue.JSArrayVal => JSValue): NativeFunction =
    NativeFunction(name, (args, context) =>
      if args.isEmpty then
        JSValue.Undefined
      else
        // args(0) is 'this' (the array)
        args(0) match
          case arrVal: JSValue.JSArrayVal => impl(arrVal)
          case _ => JSValue.Undefined
    )

  /** Create an array method with additional arguments.
    *
    * The function expects 'this' to be an array and passes additional arguments.
    *
    * @param name Method name
    * @param impl Operation that receives the array value and all args
    * @return NativeFunction that implements the array method
    *
    * Example:
    * {{{
    * val sliceFunc = arrayMethodWithArgs("slice") { (arr, args) =>
    *   val start = if args.length >= 1 then args(0).toNumber.toInt else 0
    *   // ... use start to slice array
    * }
    * }}}
    */
  def arrayMethodWithArgs(name: String)(
    impl: (JSValue.JSArrayVal, Array[JSValue]) => JSValue
  ): NativeFunction =
    NativeFunction(name, (args, context) =>
      if args.isEmpty then
        JSValue.Undefined
      else
        // args(0) is 'this' (the array), rest are method arguments
        args(0) match
          case arrVal: JSValue.JSArrayVal => impl(arrVal, args)
          case _ => JSValue.Undefined
    )

  /** Create a simple logging function.
    *
    * Formats all arguments using a formatter and prints them.
    *
    * @param name Function name
    * @param formatter Function to format each argument
    * @param output Function to output the formatted string (default: println)
    * @return NativeFunction that logs values
    *
    * Example:
    * {{{
    * val logFunc = loggingFunc("log", PrettyPrinter.shortFormat)
    * }}}
    */
  def loggingFunc(
    name: String,
    formatter: JSValue => String,
    output: String => Unit = println
  ): NativeFunction =
    NativeFunction(name, (args, context) =>
      val text = args.map(formatter).mkString(" ")
      output(text)
      JSValue.Undefined
    )
