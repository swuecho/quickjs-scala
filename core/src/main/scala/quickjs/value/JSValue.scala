package quickjs.value

import scala.annotation.targetName
import scala.collection.mutable

/** JavaScript value representation using tagged union.
  *
  * Design decisions:
  *   - Uses sealed trait with case classes for type safety
  *   - Inline storage for small values (Int32, Bool, Null, Undefined)
  *   - Reference storage for objects (String, Object, BigInt, Symbol)
  *   - Smart constructors for type coercion and optimization
  */
sealed trait JSValue {
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
  def toBoolean: Boolean = this match {
    case JSValue.Undefined | JSValue.Null => false
    case JSValue.Bool(b)                  => b
    case JSValue.Int32(i)                 => i != 0
    case JSValue.Float64(d)               => d != 0.0 && !d.isNaN
    case JSValue.BigInt(b)                => b.signum() != 0
    case JSValue.JSStr(s)                 => s.nonEmpty
    case _                                => true
  }

  def toNumber: Double = this match {
    case JSValue.Undefined  => Double.NaN
    case JSValue.Null       => 0.0
    case JSValue.Bool(b)    => if b then 1.0 else 0.0
    case JSValue.Int32(i)   => i.toDouble
    case JSValue.Float64(d) => d
    case JSValue.BigInt(b)  => b.doubleValue()
    case JSValue.JSStr(s)   =>
      // JavaScript: empty string or whitespace-only string converts to 0, and
      // the non-decimal integer literals (0x/0o/0b) are unsigned.
      val t = jsTrimWhitespace(s)
      if t.isEmpty then 0.0
      else if t.startsWith("0x") || t.startsWith("0X") then
        parseUnsignedInteger(t.substring(2), 16)
      else if t.startsWith("0o") || t.startsWith("0O") then
        parseUnsignedInteger(t.substring(2), 8)
      else if t.startsWith("0b") || t.startsWith("0B") then
        parseUnsignedInteger(t.substring(2), 2)
      else
        try t.toDouble
        catch case _: NumberFormatException => Double.NaN
    case _ => Double.NaN
  }

  /** ECMAScript whitespace (used to trim StringNumericLiterals). */
  private def isJSWhitespace(c: Char): Boolean =
    c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\u000B' ||
      c == '\f' || c == '\u00A0' || c == '\uFEFF' ||
      Character.getType(c) == Character.SPACE_SEPARATOR ||
      Character.getType(c) == Character.LINE_SEPARATOR ||
      Character.getType(c) == Character.PARAGRAPH_SEPARATOR

  private def jsTrimWhitespace(s: String): String =
    s.dropWhile(isJSWhitespace).reverse.dropWhile(isJSWhitespace).reverse

  private def parseUnsignedInteger(digits: String, radix: Int): Double =
    if digits.isEmpty then Double.NaN
    else {
      var i = 0
      while i < digits.length do {
        if Character.digit(digits.charAt(i), radix) < 0 then return Double.NaN
        i += 1
      }
      try new java.math.BigInteger(digits, radix).doubleValue()
      catch case _: NumberFormatException => Double.NaN
    }

  override def toString: String = this match {
    case JSValue.Undefined     => "undefined"
    case JSValue.Null          => "null"
    case JSValue.Uninitialized => "<uninitialized>"
    case JSValue.Bool(b)       => b.toString
    case JSValue.Int32(i)      => i.toString
    case JSValue.Float64(d)    =>
      val raw = java.lang.Double.toString(d)
      if raw.indexOf('E') >= 0 || raw.indexOf('e') >= 0 then
        java.math.BigDecimal.valueOf(d).stripTrailingZeros().toPlainString()
      else raw
    case JSValue.JSStr(s)      => s
    case JSValue.BigInt(b)     => b.toString
    case JSValue.Symbol(id)    => s"Symbol($id)"
    case JSValue.Object(_)     => "[object Object]"
    case JSValue.JSArrayVal(_) => "[object Array]"
    case _: JSValue.Function =>
      "[object Function]"
    case JSValue.Native(_) => "[object Function]"
    case _: JSValue.Generator =>
      "[object Generator]"
    case JSValue.Promise(_, _, _, _, _) => "[object Promise]"
    case JSValue.GlobalRef(name)        => s"<global:$name>"
    case other                          =>
      val typeName = other.getClass.getSimpleName
      throw new UnsupportedOperationException(
        s"Cannot convert $typeName to string"
      )
  }
}

object JSValue {

  /** Value type tags for fast dispatch */
  enum Tag {
    case Undefined, Null, Bool, Int32, Float64, String, Symbol, BigInt, Object,
      Function, Generator, Promise
  }

  // Primitive singleton values
  case object Undefined extends JSValue {
    def tag: Tag = Tag.Undefined
  }

  case object Null extends JSValue {
    def tag: Tag = Tag.Null
  }

  // Uninitialized value for TDZ (Temporal Dead Zone) tracking
  case object Uninitialized extends JSValue {
    def tag: Tag = Tag.Undefined // Use Undefined tag for now
  }

  // Boolean values
  final case class Bool(value: scala.Boolean) extends JSValue {
    def tag: Tag = Tag.Bool
  }

  // Number representations - use most compact form
  sealed trait Number extends JSValue {
    def toDouble: Double
    def toInt: Int = toDouble.toInt
    def toLong: Long = toDouble.toLong
  }

  // Inline integer for values that fit in Int32
  final case class Int32(value: scala.Int) extends Number {
    def tag: Tag = Tag.Int32
    def toDouble: Double = value.toDouble
  }

  // Double precision floating point
  final case class Float64(value: scala.Double) extends Number {
    def tag: Tag = Tag.Float64
    def toDouble: Double = value
  }

  // Reference types
  final case class JSStr(value: java.lang.String) extends JSValue {
    def tag: Tag = Tag.String
  }

  // Placeholder for Symbol - will be implemented with atom system
  final case class Symbol(value: Int) extends JSValue {
    def tag: Tag = Tag.Symbol
  }

  // Placeholder for BigInt - will be implemented with BigInteger
  final case class BigInt(value: java.math.BigInteger) extends JSValue {
    def tag: Tag = Tag.BigInt
  }

  // Object reference
  final case class Object(value: quickjs.objmodel.JSObject) extends JSValue {
    def tag: Tag = Tag.Object
  }

  // Array reference (named JSArrayVal to avoid conflict with Scala's Array)
  final case class JSArrayVal(value: quickjs.objmodel.JSArray) extends JSValue {
    def tag: Tag = Tag.Object // Arrays are objects in JavaScript

    override def toString: String = s"[${value.getClass.getSimpleName}]"
  }

  // Function reference (stores bytecode directly to avoid circular dependency)
  final case class Function(
      name: String,
      bytecode: Array[Byte],
      constants: Array[AnyRef],
      stackSize: Int,
      closure: mutable.Map[String, VarRef] =
        mutable.Map.empty, // Captured outer variables as VarRef (for shared mutable storage)
      paramNames: Array[String] =
        Array.empty, // Parameter names (for nested closure capture)
      localVarNames: Array[String] =
        Array.empty, // Local variable names (var x = ...) for nested closure capture
      parentLocalVarNames: Array[String] =
        Array.empty, // Parent function's local variable names (for capturing local vars)
      freeVarSlots: Map[String, Int] =
        Map.empty, // Free vars captured by parent slot index
      argumentsIndex: Int = -1,
      isConstructor: Boolean = true,
      isGenerator: Boolean = false, // True for function* declarations
      isAsync: Boolean = false, // True for async function declarations
      funcObj: quickjs.objmodel.JSObject = quickjs.objmodel.JSObject(),
      spanMap: Array[(Int, Int, Int)] = Array.empty,
      isStrict: Boolean = false,
      parameterScopeEndPc: Int = 0,
      isClassConstructor: Boolean = false // Class constructors require 'new'
  ) extends JSValue {
    def tag: Tag = Tag.Function
  }

  // Wrapper for native functions (to avoid circular dependency with runtime module)
  final case class Native(func: AnyRef) extends JSValue {
    def tag: Tag = Tag.Function
  }

  /** Generator state enum - tracks the execution state of a generator */
  enum GeneratorState {
    case SuspendedStart // Initial state, never resumed
    case SuspendedYield // Paused at a yield expression
    case Executing // Currently executing (prevents re-entry)
    case Completed // Finished execution (returned or threw)
  }

  /** Generator object - holds suspended execution state for resumable
    * functions.
    *
    * When a generator function is called, it returns a Generator object instead
    * of executing immediately. The generator can be resumed via
    * next()/return()/throw().
    *
    * @param func
    *   The generator function bytecode
    * @param state
    *   Current execution state
    * @param suspendedPc
    *   Program counter to resume at (after yield)
    * @param stack
    *   Saved operand stack
    * @param stackTop
    *   Saved stack pointer
    * @param args
    *   Saved arguments
    * @param vars
    *   Saved local variables
    * @param thisArg
    *   Saved 'this' binding
    * @param closure
    *   Saved closure variables
    * @param pendingValue
    *   Value passed to next() to be returned from yield
    * @param pendingThrow
    *   Exception to throw on resume (for throw() method)
    */
  final case class Generator(
      func: Function,
      var state: GeneratorState,
      var suspendedPc: Int,
      var stack: Array[JSValue],
      var stackTop: Int,
      var args: Array[JSValue],
      var vars: Array[JSValue],
      var thisArg: JSValue,
      var closure: mutable.Map[String, VarRef],
      var pendingValue: JSValue,
      var pendingThrow: Option[JSValue] = None,
      var delegatedIterator: Option[JSValue] = None, // For yield* delegation
      var tryHandlers: List[(Int, Int, Int)] = Nil,
      var lastException: JSValue = Undefined,
      var pendingException: Option[JSValue] = None,
      // Active `with` scopes, preserved across yield/resume so `with` blocks
      // may contain suspension points.
      var withStack: mutable.ArrayBuffer[quickjs.objmodel.JSObject] =
        mutable.ArrayBuffer.empty
  ) extends JSValue {
    def tag: Tag = Tag.Generator

    /** Create result object {value, done} */
    def makeResult(value: JSValue, done: Boolean)(using
        ctx: quickjs.runtime.JSContext
    ): JSValue = {
      val obj = quickjs.objmodel.JSObject()
      obj.defineProperty("value", value, enumerable = true)
      obj.defineProperty("done", JSValue.Bool(done), enumerable = true)
      JSValue.Object(obj)
    }
  }

  /** Promise state enum - tracks the settlement state of a promise */
  enum PromiseState {
    case Pending // Initial state, not yet settled
    case Fulfilled // Successfully resolved with a value
    case Rejected // Rejected with a reason (error)
  }

  /** Promise object - represents an eventual completion (or failure) of an
    * async operation.
    *
    * A Promise is in one of three states: Pending, Fulfilled, or Rejected. When
    * pending, it can transition to either fulfilled or rejected. Once settled
    * (fulfilled or rejected), it cannot change state.
    *
    * @param state
    *   Current promise state
    * @param result
    *   The fulfillment value or rejection reason
    * @param fulfillReactions
    *   Callbacks to run when fulfilled (from .then())
    * @param rejectReactions
    *   Callbacks to run when rejected (from .catch()/.then())
    * @param isHandled
    *   Whether rejection has been handled (for unhandled rejection tracking)
    */
  final case class Promise(
      var state: PromiseState = PromiseState.Pending,
      var result: JSValue = JSValue.Undefined,
      val fulfillReactions: mutable.ArrayBuffer[PromiseReaction] =
        mutable.ArrayBuffer.empty,
      val rejectReactions: mutable.ArrayBuffer[PromiseReaction] =
        mutable.ArrayBuffer.empty,
      var isHandled: Boolean = false
  ) extends JSValue {
    def tag: Tag = Tag.Promise

    /** Check if promise is settled (no longer pending) */
    def isSettled: Boolean = state != PromiseState.Pending
  }

  /** A reaction to be executed when a promise settles.
    * @param onFulfilled
    *   Callback for fulfillment (or null)
    * @param onRejected
    *   Callback for rejection (or null)
    * @param promise
    *   The promise that will be resolved with the callback's result
    */
  final case class PromiseReaction(
      onFulfilled: JSValue, // Function or Undefined
      onRejected: JSValue, // Function or Undefined
      promise: Promise, // The promise to resolve with the result
      resolveFunc: JSValue = JSValue.Undefined, // Optional capability resolve
      rejectFunc: JSValue = JSValue.Undefined // Optional capability reject
  )

  /** Async function state enum - tracks the execution state of an async
    * function
    */
  enum AsyncState {
    case SuspendedStart // Initial state, never resumed
    case SuspendedAwait // Paused at an await expression
    case Executing // Currently executing (prevents re-entry)
    case Completed // Finished execution (returned or threw)
  }

  /** Async function object - holds suspended execution state for async
    * functions.
    *
    * When an async function is called, it returns a Promise immediately. The
    * function executes until it hits an await, then suspends. When the awaited
    * Promise resolves, execution resumes. When the function returns, the
    * Promise is resolved with the return value. If the function throws, the
    * Promise is rejected with the error.
    *
    * @param func
    *   The async function bytecode
    * @param promise
    *   The promise returned to the caller
    * @param state
    *   Current execution state
    * @param suspendedPc
    *   Program counter to resume at (after await)
    * @param stack
    *   Saved operand stack
    * @param stackTop
    *   Saved stack pointer
    * @param args
    *   Saved arguments
    * @param vars
    *   Saved local variables
    * @param thisArg
    *   Saved 'this' binding
    * @param closure
    *   Saved closure variables
    */
  final case class AsyncFunction(
      func: Function,
      promise: Promise,
      var state: AsyncState,
      var suspendedPc: Int,
      var stack: Array[JSValue],
      var stackTop: Int,
      var args: Array[JSValue],
      var vars: Array[JSValue],
      var thisArg: JSValue,
      var closure: mutable.Map[String, VarRef]
  ) extends JSValue {
    def tag: Tag =
      Tag.Promise // Use Promise tag since async functions return Promises
  }

  // VarRef - a mutable reference to a variable value (for closure capture)
  // Similar to QuickJS's JSVarRef.pvalue indirection
  final class VarRef(
      var value: JSValue,
      private var constFlag: Boolean = false,
      private var functionNameFlag: Boolean = false,
      private var evalVarFlag: Boolean = false
  ) {
    def get: JSValue = value
    def set(v: JSValue): Unit = value = v
    def isConst: Boolean = constFlag
    def setConst(): Unit = constFlag = true
    def isFunctionName: Boolean = functionNameFlag
    def setFunctionName(): Unit = functionNameFlag = true
    def isEvalVar: Boolean = evalVarFlag
    def setEvalVar(): Unit = evalVarFlag = true
    override def toString: String = s"VarRef($value)"
    override def hashCode(): Int = System.identityHashCode(this)
    override def equals(obj: Any): Boolean = obj match {
      case other: VarRef => this eq other // Reference equality
      case _             => false
    }
  }

  // Marker for closure variables that reference global scope (for lazy lookup)
  final case class GlobalRef(varName: String) extends JSValue {
    def tag: Tag = Tag.Object // Use Object tag for our internal marker
  }

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
    else {
      val rounded = v.round
      if v == rounded && v >= Int.MinValue.toDouble && v <= Int.MaxValue.toDouble
      then Int32(rounded.toInt)
      else Float64(v)
    }

  def fromBoolean(v: Boolean): JSValue = if v then Bool(true) else Bool(false)

  def fromString(v: java.lang.String): JSValue = JSStr(v)

  // Type-safe operations
  @targetName("add")
  def add(a: JSValue, b: JSValue): JSValue = (a, b) match {
    case (BigInt(x), BigInt(y)) => BigInt(x.add(y))
    case (BigInt(_), _)         =>
      throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
    case (_, BigInt(_)) =>
      throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
    case (Int32(x), Int32(y)) =>
      val result = x.toLong + y.toLong
      fromLong(result)
    case (Float64(x), Int32(y))   => Float64(x + y.toDouble)
    case (Int32(x), Float64(y))   => Float64(x.toDouble + y)
    case (Float64(x), Float64(y)) => Float64(x + y)
    case (JSStr(x), _)            => JSStr(x + b.toString)
    case (_, JSStr(y))            => JSStr(a.toString + y)
    case _                        => fromDouble(a.toNumber + b.toNumber)
  }

  @targetName("subtract")
  def subtract(a: JSValue, b: JSValue): JSValue = (a, b) match {
    case (BigInt(x), BigInt(y)) => BigInt(x.subtract(y))
    case (BigInt(_), _)         =>
      throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
    case (_, BigInt(_)) =>
      throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
    case (Int32(x), Int32(y)) =>
      val result = x.toLong - y.toLong
      fromLong(result)
    case (Float64(x), Int32(y))   => Float64(x - y.toDouble)
    case (Int32(x), Float64(y))   => Float64(x.toDouble - y)
    case (Float64(x), Float64(y)) => Float64(x - y)
    case _                        => fromDouble(a.toNumber - b.toNumber)
  }

  @targetName("multiply")
  def multiply(a: JSValue, b: JSValue): JSValue = (a, b) match {
    case (BigInt(x), BigInt(y)) => BigInt(x.multiply(y))
    case (BigInt(_), _)         =>
      throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
    case (_, BigInt(_)) =>
      throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
    case (Int32(x), Int32(y)) =>
      val result = x.toLong * y.toLong
      fromLong(result)
    case (Float64(x), Int32(y))   => Float64(x * y.toDouble)
    case (Int32(x), Float64(y))   => Float64(x.toDouble * y)
    case (Float64(x), Float64(y)) => Float64(x * y)
    case _                        => fromDouble(a.toNumber * b.toNumber)
  }

  @targetName("divide")
  def divide(a: JSValue, b: JSValue): JSValue = (a, b) match {
    case (BigInt(x), BigInt(y)) =>
      if y.equals(java.math.BigInteger.ZERO) then
        throw new RuntimeException("RangeError: Division by zero")
      BigInt(x.divide(y))
    case (BigInt(_), _) =>
      throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
    case (_, BigInt(_)) =>
      throw new RuntimeException("TypeError: Cannot mix BigInt and other types")
    case _ =>
      val bNum = b.toNumber
      if bNum == 0.0 then {
        // Check if b is negative zero (using sign bit)
        val bIsNegativeZero =
          bNum == 0.0 && java.lang.Double.doubleToRawLongBits(bNum) < 0

        if a.toNumber == 0.0 then Float64(Double.NaN)
        else if (a.toNumber < 0) ^ bIsNegativeZero then
          Float64(Double.NegativeInfinity)
        else Float64(Double.PositiveInfinity)
      } else fromDouble(a.toNumber / bNum)
  }
}
