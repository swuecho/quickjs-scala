package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext

/** BigInt built-in: BigInt constructor, BigInt.asIntN, BigInt.asUintN,
  * prototype methods.
  */
object BigIntBuiltins {

  /** ToBigInt abstract operation: convert value to BigInt */
  private def toBigInt(value: JSValue)(using
      ctx: JSContext
  ): java.math.BigInteger = value match {
    case JSValue.BigInt(b) => b
    case JSValue.Bool(b)   =>
      if b then java.math.BigInteger.ONE else java.math.BigInteger.ZERO
    case JSValue.JSStr(s) =>
      val trimmed = s.trim()
      if trimmed.isEmpty then java.math.BigInteger.ZERO
      else
        try {
          val (str, radix) =
            if trimmed.startsWith("0x") || trimmed.startsWith("0X") then
              (trimmed.substring(2), 16)
            else if trimmed.startsWith("0o") || trimmed.startsWith("0O") then
              (trimmed.substring(2), 8)
            else if trimmed.startsWith("0b") || trimmed.startsWith("0B") then
              (trimmed.substring(2), 2)
            else (trimmed, 10)
          new java.math.BigInteger(str, radix)
        }
        catch {
          case _: NumberFormatException =>
            ctx.throwSyntaxError(s"Cannot convert $s to a BigInt")
        }
    case JSValue.Int32(i) =>
      java.math.BigInteger.valueOf(i.toLong)
    case JSValue.Float64(d) =>
      if d.isNaN || d.isInfinite then
        ctx.throwRangeError(
          "The number cannot be converted to a BigInt because it is not an integer"
        )
      if d != d.floor then
        ctx.throwRangeError(
          "The number cannot be converted to a BigInt because it is not an integer"
        )
      java.math.BigInteger.valueOf(d.toLong)
    case JSValue.Object(obj) =>
      val primitive = obj.getPrimitiveValue.getOrElse(
        BuiltinHelpers.toPrimitiveNumber(value)
      )
      toBigInt(primitive)
    case _: JSValue.JSArrayVal | _: JSValue.Function | JSValue.Native(_) =>
      toBigInt(BuiltinHelpers.toPrimitiveNumber(value))
    case _ => ctx.throwTypeError(s"Cannot convert ${value} to a BigInt")
  }

  /** ToBigInt for BigInt.asIntN/asUintN — throws TypeError for Numbers */
  private def toBigIntStrict(value: JSValue)(using
      ctx: JSContext
  ): java.math.BigInteger = value match {
    case JSValue.BigInt(b) => b
    case JSValue.Bool(b)   =>
      if b then java.math.BigInteger.ONE else java.math.BigInteger.ZERO
    case JSValue.JSStr(s) =>
      val trimmed = s.trim()
      if trimmed.isEmpty then java.math.BigInteger.ZERO
      else
        try {
          val (str, radix) =
            if trimmed.startsWith("0x") || trimmed.startsWith("0X") then
              (trimmed.substring(2), 16)
            else if trimmed.startsWith("0o") || trimmed.startsWith("0O") then
              (trimmed.substring(2), 8)
            else if trimmed.startsWith("0b") || trimmed.startsWith("0B") then
              (trimmed.substring(2), 2)
            else (trimmed, 10)
          new java.math.BigInteger(str, radix)
        }
        catch {
          case _: NumberFormatException =>
            ctx.throwSyntaxError(s"Cannot convert $s to a BigInt")
        }
    case JSValue.Int32(_) | JSValue.Float64(_) =>
      ctx.throwTypeError("BigInt.asIntN expects a BigInt")
    case JSValue.Object(obj) =>
      val primitive = obj.getPrimitiveValue.getOrElse(
        BuiltinHelpers.toPrimitiveNumber(value)
      )
      toBigIntStrict(primitive)
    case _: JSValue.JSArrayVal | _: JSValue.Function | JSValue.Native(_) =>
      toBigIntStrict(BuiltinHelpers.toPrimitiveNumber(value))
    case _ => ctx.throwTypeError("BigInt.asIntN expects a BigInt")
  }

  /** ToIndex abstract operation: if index < 0 or is Infinity/NaN, throw
    * RangeError. If index > 2^53-1, throw RangeError.
    */
  private def toIndex(value: JSValue)(using ctx: JSContext): Int = {
    val n = BuiltinHelpers.toIntegerOrInfinity(value)
    if n.isInfinite || n < 0 then
      ctx.throwRangeError("ToIndex: argument must be a non-negative integer")
    if n > 9007199254740991.0 then  // 2^53 - 1
      ctx.throwRangeError("ToIndex: argument must be <= 2^53-1")
    val intVal = n.toLong
    if intVal > Int.MaxValue then Int.MaxValue else intVal.toInt
  }

  def initialize(ctx: JSContext): Unit = {
    given JSContext = ctx

    // Create BigInt.prototype object
    val bigIntPrototype = quickjs.objmodel.JSObject(
      prototype = ctx.objectPrototype,
      extensible = true
    )

    // BigInt.prototype.toString(radix)
    val bigIntToString = NativeFunction(
      name = "toString",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        def thisBigInt(value: JSValue): Option[java.math.BigInteger] = value match {
          case JSValue.BigInt(b) => Some(b)
          case JSValue.Object(obj) => obj.getPrimitiveValue match {
            case Some(JSValue.BigInt(b)) => Some(b)
            case _                       => None
          }
          case _ => None
        }
        args.headOption.flatMap(thisBigInt) match {
          case Some(b) =>
            val radix =
              if args.length <= 1 || args(1) == JSValue.Undefined then 10
              else BuiltinHelpers.toIntegerOrInfinity(args(1)).toInt
            if radix < 2 || radix > 36 then
              ctx.throwRangeError(
                "toString() radix argument must be between 2 and 36"
              )
            JSValue.fromString(b.toString(radix))
          case _ =>
            ctx.throwTypeError("BigInt.prototype.toString called on non-BigInt")
        }
    )

    // BigInt.prototype.valueOf()
    val bigIntValueOf = NativeFunction(
      name = "valueOf",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(b: JSValue.BigInt) => b
          case Some(JSValue.Object(obj)) => obj.getPrimitiveValue match {
            case Some(b: JSValue.BigInt) => b
            case _ =>
              ctx.throwTypeError("BigInt.prototype.valueOf called on non-BigInt")
          }
          case _                       =>
            ctx.throwTypeError("BigInt.prototype.valueOf called on non-BigInt")
        }
    )

    bigIntPrototype.defineProperty(
      "toString",
      JSValue.Native(bigIntToString),
      enumerable = false
    )
    bigIntPrototype.defineProperty(
      "valueOf",
      JSValue.Native(bigIntValueOf),
      enumerable = false
    )

    val bigIntConstructor = quickjs.value.NativeConstructor(
      name = "BigInt",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        if args.isEmpty then JSValue.BigInt(java.math.BigInteger.ZERO)
        else
          try JSValue.BigInt(toBigInt(args(0)))
          catch {
            case e: quickjs.runtime.JSException => throw e
            case _: Exception                   =>
              ctx.throwTypeError(s"Cannot convert ${args(0)} to a BigInt")
          },
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError(
          "BigInt is not a constructor. Use BigInt() without 'new'."
        )
      ,
      prototype = bigIntPrototype
    )

    // BigInt.asIntN(bits, bigint)
    val bigIntAsIntN = NativeFunction(
      name = "asIntN",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 3 then
          ctx.throwTypeError("BigInt.asIntN requires 2 arguments")
        val bits = toIndex(args(1))
        val bigint = toBigIntStrict(args(2))
        if bits == 0 then JSValue.BigInt(java.math.BigInteger.ZERO)
        else {
          val mask = java.math.BigInteger.ONE
            .shiftLeft(bits)
            .subtract(java.math.BigInteger.ONE)
          val masked = bigint.and(mask)
          val signBit = java.math.BigInteger.ONE.shiftLeft(bits - 1)
          val result =
            if masked.testBit(bits - 1) then
              masked.subtract(mask).subtract(java.math.BigInteger.ONE)
            else masked
          JSValue.BigInt(result)
        }
    )

    // BigInt.asUintN(bits, bigint)
    val bigIntAsUintN = NativeFunction(
      name = "asUintN",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 3 then
          ctx.throwTypeError("BigInt.asUintN requires 2 arguments")
        val bits = toIndex(args(1))
        val bigint = toBigIntStrict(args(2))
        if bits == 0 then JSValue.BigInt(java.math.BigInteger.ZERO)
        else {
          val mask = java.math.BigInteger.ONE
            .shiftLeft(bits)
            .subtract(java.math.BigInteger.ONE)
          JSValue.BigInt(bigint.and(mask))
        }
    )

    BuiltinHelpers.initConstructor(bigIntConstructor, length = 1)
    bigIntPrototype.defineProperty(
      "constructor",
      JSValue.Native(bigIntConstructor),
      enumerable = false
    )
    bigIntConstructor.funcObj.defineProperty(
      "asIntN",
      JSValue.Native(bigIntAsIntN),
      enumerable = false
    )
    bigIntConstructor.funcObj.defineProperty(
      "asUintN",
      JSValue.Native(bigIntAsUintN),
      enumerable = false
    )
    ctx.global.defineProperty(
      "BigInt",
      JSValue.Native(bigIntConstructor),
      enumerable = false
    )

    // Symbol.toStringTag
    val symToStringTag = ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.get("toStringTag")(using ctx)
      case _ => JSValue.Undefined
    }
    symToStringTag match {
      case sym: JSValue.Symbol =>
        bigIntPrototype.initSymbolProperty(
          sym.value,
          JSValue.fromString("BigInt"),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }
  }
}
