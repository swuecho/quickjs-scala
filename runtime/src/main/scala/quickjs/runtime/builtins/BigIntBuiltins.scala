package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext

/** BigInt built-in: BigInt constructor, BigInt.asIntN, BigInt.asUintN, prototype methods. */
object BigIntBuiltins:

  def initialize(ctx: JSContext): Unit =
    given JSContext = ctx

    val bigIntConstructor = quickjs.value.NativeConstructor(
      name = "BigInt",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        if args.isEmpty then JSValue.BigInt(java.math.BigInteger.ZERO)
        else args(0) match
          case JSValue.BigInt(b) => JSValue.BigInt(b)
          case JSValue.JSStr(s) =>
            val trimmed = s.trim()
            if trimmed.isEmpty then
              JSValue.BigInt(java.math.BigInteger.ZERO)
            else
              try
                val (str, radix) =
                  if trimmed.startsWith("0x") || trimmed.startsWith("0X") then (trimmed.substring(2), 16)
                  else if trimmed.startsWith("0o") || trimmed.startsWith("0O") then (trimmed.substring(2), 8)
                  else if trimmed.startsWith("0b") || trimmed.startsWith("0B") then (trimmed.substring(2), 2)
                  else (trimmed, 10)
                JSValue.BigInt(new java.math.BigInteger(str, radix))
              catch
                case e: quickjs.runtime.JSException => throw e
                case _: NumberFormatException =>
                  ctx.throwSyntaxError(s"Cannot convert $s to a BigInt")
          case JSValue.Int32(i) => JSValue.BigInt(java.math.BigInteger.valueOf(i.toLong))
          case JSValue.Float64(d) =>
            if d.isNaN || d.isInfinite then
              ctx.throwRangeError("The number cannot be converted to a BigInt because it is not an integer")
            JSValue.BigInt(java.math.BigInteger.valueOf(d.toLong))
          case JSValue.Bool(b) => JSValue.BigInt(if b then java.math.BigInteger.ONE else java.math.BigInteger.ZERO)
          case _ => ctx.throwTypeError(s"Cannot convert ${args(0)} to a BigInt"),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("BigInt is not a constructor. Use BigInt() without 'new'."),
      prototype = ctx.objectPrototype
    )

    // BigInt.asIntN(bits, bigint)
    val bigIntAsIntN = NativeFunction(
      name = "asIntN",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 3 then
          ctx.throwTypeError("BigInt.asIntN requires 2 arguments")
        val bits = args(1).toNumber.toInt
        val bigint = args(2) match
          case JSValue.BigInt(b) => b
          case _ => ctx.throwTypeError("BigInt.asIntN expects a BigInt")
        val mask = java.math.BigInteger.ONE.shiftLeft(bits).subtract(java.math.BigInteger.ONE)
        val masked = bigint.and(mask)
        val signBit = java.math.BigInteger.ONE.shiftLeft(bits - 1)
        val result = if masked.testBit(bits - 1) then masked.subtract(mask).subtract(java.math.BigInteger.ONE) else masked
        JSValue.BigInt(result)
    )

    // BigInt.asUintN(bits, bigint)
    val bigIntAsUintN = NativeFunction(
      name = "asUintN",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 3 then
          ctx.throwTypeError("BigInt.asUintN requires 2 arguments")
        val bits = args(1).toNumber.toInt
        val bigint = args(2) match
          case JSValue.BigInt(b) => b
          case _ => ctx.throwTypeError("BigInt.asUintN expects a BigInt")
        val mask = java.math.BigInteger.ONE.shiftLeft(bits).subtract(java.math.BigInteger.ONE)
        JSValue.BigInt(bigint.and(mask))
    )

    BuiltinHelpers.initConstructor(bigIntConstructor, length = 1)
    bigIntConstructor.funcObj.set("asIntN", JSValue.Native(bigIntAsIntN))
    bigIntConstructor.funcObj.set("asUintN", JSValue.Native(bigIntAsUintN))
    ctx.global.set("BigInt", JSValue.Native(bigIntConstructor))

    // BigInt.prototype.toString
    val bigIntToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.BigInt(b)) =>
            val radix = if args.length > 1 then args(1).toNumber.toInt else 10
            if radix < 2 || radix > 36 then
              ctx.throwRangeError("toString() radix argument must be between 2 and 36")
            JSValue.fromString(b.toString(radix))
          case _ =>
            ctx.throwTypeError("BigInt.prototype.toString called on non-BigInt")
    )

    // BigInt.prototype.valueOf
    val bigIntValueOf = NativeFunction(
      name = "valueOf",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(b: JSValue.BigInt) => b
          case _ => ctx.throwTypeError("BigInt.prototype.valueOf called on non-BigInt")
    )

    ctx.global.set("__BigInt_toString", JSValue.Native(bigIntToString))
    ctx.global.set("__BigInt_valueOf", JSValue.Native(bigIntValueOf))
