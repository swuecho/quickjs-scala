package quickjs.value

import scala.annotation.targetName

/** JavaScript value representation using tagged union.
  *
  * Design decisions:
  * - Uses sealed trait with case classes for type safety
  * - Inline storage for small values (Int32, Bool, Null, Undefined)
  * - Reference storage for objects (String, Object, BigInt, Symbol)
  * - Smart constructors for type coercion and optimization
  */
sealed trait JSValue:
  import JSValue.Tag

  /** Get the type tag for this value */
  def tag: Tag

  /** Type check operations */
  def isUndefined: Boolean = this == JSValue.Undefined
  def isNull: Boolean = this == JSValue.Null
  def isBoolean: Boolean = tag == Tag.Bool
  def isNumber: Boolean = tag == Tag.Int32 || tag == Tag.Float64
  def isString: Boolean = tag == Tag.String
  def isSymbol: Boolean = tag == Tag.Symbol
  def isBigInt: Boolean = tag == Tag.BigInt
  def isObject: Boolean = tag == Tag.Object
  def isPrimitive: Boolean = tag != Tag.Object

  /** Conversion operations */
  def toBoolean: Boolean = this match
    case JSValue.Undefined | JSValue.Null => false
    case JSValue.Bool(b) => b
    case JSValue.Int32(i) => i != 0
    case JSValue.Float64(d) => d != 0.0 && !d.isNaN
    case JSValue.JSStr(s) => s.nonEmpty
    case _ => true

  def toNumber: Double = this match
    case JSValue.Undefined => Double.NaN
    case JSValue.Null => 0.0
    case JSValue.Bool(b) => if b then 1.0 else 0.0
    case JSValue.Int32(i) => i.toDouble
    case JSValue.Float64(d) => d
    case JSValue.JSStr(s) => try s.toDouble catch case _: NumberFormatException => Double.NaN
    case _ => Double.NaN

  override def toString: String = this match
    case JSValue.Undefined => "undefined"
    case JSValue.Null => "null"
    case JSValue.Bool(b) => b.toString
    case JSValue.Int32(i) => i.toString
    case JSValue.Float64(d) => d.toString
    case JSValue.JSStr(s) => s
    case _ => throw new UnsupportedOperationException("Cannot convert object to string")

object JSValue:
  /** Value type tags for fast dispatch */
  enum Tag:
    case Undefined, Null, Bool, Int32, Float64, String, Symbol, BigInt, Object, Function

  // Primitive singleton values
  case object Undefined extends JSValue:
    def tag: Tag = Tag.Undefined

  case object Null extends JSValue:
    def tag: Tag = Tag.Null

  // Boolean values
  final case class Bool(value: scala.Boolean) extends JSValue:
    def tag: Tag = Tag.Bool

  // Number representations - use most compact form
  sealed trait Number extends JSValue:
    def toDouble: Double
    def toInt: Int = toDouble.toInt
    def toLong: Long = toDouble.toLong

  // Inline integer for values that fit in Int32
  final case class Int32(value: scala.Int) extends Number:
    def tag: Tag = Tag.Int32
    def toDouble: Double = value.toDouble

  // Double precision floating point
  final case class Float64(value: scala.Double) extends Number:
    def tag: Tag = Tag.Float64
    def toDouble: Double = value

  // Reference types
  final case class JSStr(value: java.lang.String) extends JSValue:
    def tag: Tag = Tag.String

  // Placeholder for Symbol - will be implemented with atom system
  final case class Symbol(value: Int) extends JSValue:
    def tag: Tag = Tag.Symbol

  // Placeholder for BigInt - will be implemented with BigInteger
  final case class BigInt(value: java.math.BigInteger) extends JSValue:
    def tag: Tag = Tag.BigInt

  // Object reference
  final case class Object(value: quickjs.objmodel.JSObject) extends JSValue:
    def tag: Tag = Tag.Object

  // Array reference (named JSArrayVal to avoid conflict with Scala's Array)
  final case class JSArrayVal(value: quickjs.objmodel.JSArray) extends JSValue:
    def tag: Tag = Tag.Object  // Arrays are objects in JavaScript

  // Function reference (stores bytecode directly to avoid circular dependency)
  final case class Function(
    name: String,
    bytecode: Array[Byte],
    constants: Array[AnyRef],
    stackSize: Int
  ) extends JSValue:
    def tag: Tag = Tag.Function

  // Smart constructors for type coercion and optimization
  def fromInt(v: Int): JSValue = Int32(v)

  def fromLong(v: Long): JSValue =
    if v >= Int.MinValue.toLong && v <= Int.MaxValue.toLong then Int32(v.toInt)
    else Float64(v.toDouble)

  def fromDouble(v: Double): JSValue =
    if v.isNaN || v.isInfinite then Float64(v)
    else
      val rounded = v.round
      if v == rounded && v >= Int.MinValue.toDouble && v <= Int.MaxValue.toDouble
      then Int32(rounded.toInt)
      else Float64(v)

  def fromBoolean(v: Boolean): JSValue = if v then Bool(true) else Bool(false)

  def fromString(v: java.lang.String): JSValue = JSStr(v)

  // Type-safe operations
  @targetName("add")
  def add(a: JSValue, b: JSValue): JSValue = (a, b) match
    case (Int32(x), Int32(y)) =>
      val result = x.toLong + y.toLong
      fromLong(result)
    case (Float64(x), Int32(y)) => Float64(x + y.toDouble)
    case (Int32(x), Float64(y)) => Float64(x.toDouble + y)
    case (Float64(x), Float64(y)) => Float64(x + y)
    case (JSStr(x), _) => JSStr(x + b.toString)
    case (_, JSStr(y)) => JSStr(a.toString + y)
    case _ => Float64(a.toNumber + b.toNumber)

  @targetName("subtract")
  def subtract(a: JSValue, b: JSValue): JSValue = (a, b) match
    case (Int32(x), Int32(y)) =>
      val result = x.toLong - y.toLong
      fromLong(result)
    case (Float64(x), Int32(y)) => Float64(x - y.toDouble)
    case (Int32(x), Float64(y)) => Float64(x.toDouble - y)
    case (Float64(x), Float64(y)) => Float64(x - y)
    case _ => Float64(a.toNumber - b.toNumber)

  @targetName("multiply")
  def multiply(a: JSValue, b: JSValue): JSValue = (a, b) match
    case (Int32(x), Int32(y)) =>
      val result = x.toLong * y.toLong
      fromLong(result)
    case (Float64(x), Int32(y)) => Float64(x * y.toDouble)
    case (Int32(x), Float64(y)) => Float64(x.toDouble * y)
    case (Float64(x), Float64(y)) => Float64(x * y)
    case _ => Float64(a.toNumber * b.toNumber)

  @targetName("divide")
  def divide(a: JSValue, b: JSValue): JSValue =
    if b.toNumber == 0.0 then
      if a.toNumber == 0.0 then Float64(Double.NaN)
      else if a.toNumber < 0 then Float64(Double.NegativeInfinity)
      else Float64(Double.PositiveInfinity)
    else Float64(a.toNumber / b.toNumber)
