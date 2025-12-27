package quickjs.value

import scala.annotation.targetName
import scala.collection.mutable

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
  def isUninitialized: Boolean = this == JSValue.Uninitialized
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
    case JSValue.JSStr(s) =>
      // JavaScript: empty string or whitespace-only string converts to 0
      if s.isEmpty || s.trim.isEmpty then 0.0
      // Handle hex strings (0x prefix)
      else if s.startsWith("0x") || s.startsWith("0X") then
        try java.lang.Integer.decode(s).toDouble
        catch case _: NumberFormatException => Double.NaN
      else
        try s.toDouble
        catch case _: NumberFormatException => Double.NaN
    case _ => Double.NaN

  override def toString: String = this match
    case JSValue.Undefined => "undefined"
    case JSValue.Null => "null"
    case JSValue.Bool(b) => b.toString
    case JSValue.Int32(i) => i.toString
    case JSValue.Float64(d) => d.toString
    case JSValue.JSStr(s) => s
    case JSValue.BigInt(b) => b.toString
    case JSValue.Object(_) => "[object Object]"
    case JSValue.JSArrayVal(_) => "[object Array]"
    case JSValue.Function(_, _, _, _, _, _, _, _) => "[object Function]"
    case JSValue.Native(_) => "[object Function]"
    case _ => throw new UnsupportedOperationException(s"Cannot convert $this to string")

object JSValue:
  /** Value type tags for fast dispatch */
  enum Tag:
    case Undefined, Null, Bool, Int32, Float64, String, Symbol, BigInt, Object, Function

  // Primitive singleton values
  case object Undefined extends JSValue:
    def tag: Tag = Tag.Undefined

  case object Null extends JSValue:
    def tag: Tag = Tag.Null

  // Uninitialized value for TDZ (Temporal Dead Zone) tracking
  case object Uninitialized extends JSValue:
    def tag: Tag = Tag.Undefined  // Use Undefined tag for now

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

    override def toString: String = s"[${value.getClass.getSimpleName}]"

  // Function reference (stores bytecode directly to avoid circular dependency)
  final case class Function(
    name: String,
    bytecode: Array[Byte],
    constants: Array[AnyRef],
    stackSize: Int,
    closure: mutable.Map[String, VarRef] = mutable.Map.empty,  // Captured outer variables as VarRef (for shared mutable storage)
    paramNames: Array[String] = Array.empty,  // Parameter names (for nested closure capture)
    localVarNames: Array[String] = Array.empty,  // Local variable names (var x = ...) for nested closure capture
    parentLocalVarNames: Array[String] = Array.empty  // Parent function's local variable names (for capturing local vars)
  ) extends JSValue:
    def tag: Tag = Tag.Function

  // Wrapper for native functions (to avoid circular dependency with runtime module)
  final case class Native(func: AnyRef) extends JSValue:
    def tag: Tag = Tag.Function

  // VarRef - a mutable reference to a variable value (for closure capture)
  // Similar to QuickJS's JSVarRef.pvalue indirection
  final class VarRef(var value: JSValue, private var constFlag: Boolean = false):
    def get: JSValue = value
    def set(v: JSValue): Unit = value = v
    def isConst: Boolean = constFlag
    def setConst(): Unit = constFlag = true
    override def toString: String = s"VarRef($value)"
    override def hashCode(): Int = System.identityHashCode(this)
    override def equals(obj: Any): Boolean = obj match
      case other: VarRef => this eq other  // Reference equality
      case _ => false

  // Marker for closure variables that reference global scope (for lazy lookup)
  final case class GlobalRef(varName: String) extends JSValue:
    def tag: Tag = Tag.Object  // Use Object tag for our internal marker

  // Smart constructors for type coercion and optimization
  def fromInt(v: Int): JSValue = Int32(v)

  def fromLong(v: Long): JSValue =
    if v >= Int.MinValue.toLong && v <= Int.MaxValue.toLong then Int32(v.toInt)
    else Float64(v.toDouble)

  def fromDouble(v: Double): JSValue =
    if v.isNaN || v.isInfinite then Float64(v)
    else if v == 0.0 then
      // Preserve signed zero
      if java.lang.Double.doubleToRawLongBits(v) < 0 then Float64(-0.0)
      else Int32(0)
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
    case _ => fromDouble(a.toNumber + b.toNumber)

  @targetName("subtract")
  def subtract(a: JSValue, b: JSValue): JSValue = (a, b) match
    case (Int32(x), Int32(y)) =>
      val result = x.toLong - y.toLong
      fromLong(result)
    case (Float64(x), Int32(y)) => Float64(x - y.toDouble)
    case (Int32(x), Float64(y)) => Float64(x.toDouble - y)
    case (Float64(x), Float64(y)) => Float64(x - y)
    case _ => fromDouble(a.toNumber - b.toNumber)

  @targetName("multiply")
  def multiply(a: JSValue, b: JSValue): JSValue = (a, b) match
    case (Int32(x), Int32(y)) =>
      val result = x.toLong * y.toLong
      fromLong(result)
    case (Float64(x), Int32(y)) => Float64(x * y.toDouble)
    case (Int32(x), Float64(y)) => Float64(x.toDouble * y)
    case (Float64(x), Float64(y)) => Float64(x * y)
    case _ => fromDouble(a.toNumber * b.toNumber)

  @targetName("divide")
  def divide(a: JSValue, b: JSValue): JSValue =
    val bNum = b.toNumber
    if bNum == 0.0 then
      // Check if b is negative zero (using sign bit)
      val bIsNegativeZero = bNum == 0.0 && java.lang.Double.doubleToRawLongBits(bNum) < 0

      if a.toNumber == 0.0 then Float64(Double.NaN)
      else if (a.toNumber < 0) ^ bIsNegativeZero then Float64(Double.NegativeInfinity)
      else Float64(Double.PositiveInfinity)
    else fromDouble(a.toNumber / bNum)
