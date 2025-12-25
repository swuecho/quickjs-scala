package quickjs.bytecode

import scala.collection.mutable.{ArrayBuffer, StringBuilder}

/** Bytecode instruction encoding.
  *
  * Encoding format:
  * - 1 byte: opcode
  * - 0-8 bytes: operands (depending on opcode)
  * - Operands can be: u8, i8, u16, i16, u32, i32, i64, f64
  */
final class Instruction(
  val opcode: Opcode,
  private val operands: Array[AnyRef]
):
  def size: Int = 1 + operands.foldLeft(0)(_ + operandSize(_))

  private def operandSize(operand: AnyRef): Int = operand match
    case _: Int => 4
    case _: Long => 8
    case _: Double => 8
    case _ => 0

  def encode(): Array[Byte] =
    val buffer = ArrayBuffer[Byte]()
    buffer += opcode.code.toByte
    operands.foreach(encodeOperand(_, buffer))
    buffer.toArray

  private def encodeOperand(operand: AnyRef, buffer: ArrayBuffer[Byte]): Unit =
    operand match
      case i: Int =>
        buffer += ((i >> 24) & 0xFF).toByte
        buffer += ((i >> 16) & 0xFF).toByte
        buffer += ((i >> 8) & 0xFF).toByte
        buffer += (i & 0xFF).toByte
      case l: Long =>
        var x = l
        (0 until 8).foreach { _ =>
          buffer += (x & 0xFF).toByte
          x = x >> 8
        }
      case d: Double =>
        val bits = java.lang.Double.doubleToLongBits(d)
        encodeOperand(bits, buffer)
      case _ =>

  override def toString: String =
    s"$opcode${operands.mkString("(", ", ", ")")}"

object Instruction:
  def pushI32(value: Int): Instruction =
    new Instruction(Opcode.PushI32, Array[AnyRef](value))

  def pushFloat64(value: Double): Instruction =
    new Instruction(Opcode.PushFloat64, Array[AnyRef](value))

  def pushUndefined(): Instruction =
    new Instruction(Opcode.PushUndefined, Array.empty)

  def pushNull(): Instruction =
    new Instruction(Opcode.PushNull, Array.empty)

  def pushTrue(): Instruction =
    new Instruction(Opcode.PushTrue, Array.empty)

  def pushFalse(): Instruction =
    new Instruction(Opcode.PushFalse, Array.empty)

  def drop(): Instruction =
    new Instruction(Opcode.Drop, Array.empty)

  def dup(): Instruction =
    new Instruction(Opcode.Dup, Array.empty)

  def unary(op: UnaryOpcode): Instruction =
    new Instruction(op.toOpcode, Array.empty)

  def binary(op: BinaryOpcode): Instruction =
    new Instruction(op.toOpcode, Array.empty)

  def returnUndef(): Instruction =
    new Instruction(Opcode.ReturnUndef, Array.empty)

enum UnaryOpcode:
  case Neg, Not, LNot

  def toOpcode: Opcode = this match
    case Neg => Opcode.Neg
    case Not => Opcode.Not
    case LNot => Opcode.LNot

enum BinaryOpcode:
  case Add, Sub, Mul, Div, Mod
  case Lt, Lte, Gt, Gte, Eq, Neq, StrictEq, StrictNeq
  case And, Or, Xor, Shl, Sar, Shr
  case LogicalAnd, LogicalOr

  def toOpcode: Opcode = this match
    case Add => Opcode.Add
    case Sub => Opcode.Sub
    case Mul => Opcode.Mul
    case Div => Opcode.Div
    case Mod => Opcode.Mod
    case Lt => Opcode.Lt
    case Lte => Opcode.Lte
    case Gt => Opcode.Gt
    case Gte => Opcode.Gte
    case Eq => Opcode.Eq
    case Neq => Opcode.Neq
    case StrictEq => Opcode.StrictEq
    case StrictNeq => Opcode.StrictNeq
    case And => Opcode.And
    case Or => Opcode.Or
    case Xor => Opcode.Xor
    case Shl => Opcode.Shl
    case Sar => Opcode.Sar
    case Shr => Opcode.Shr
    case LogicalAnd => Opcode.LogicalAnd
    case LogicalOr => Opcode.LogicalOr

/** Bytecode function.
  */
final class BytecodeFunction(
  val name: String,
  val bytecode: Array[Byte],
  val constants: Array[AnyRef],
  val stackSize: Int
):
  override def toString: String =
    s"BytecodeFunction($name, ${bytecode.length} bytes, ${constants.length} constants)"
