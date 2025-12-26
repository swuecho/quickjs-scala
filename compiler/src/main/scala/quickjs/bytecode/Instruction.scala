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
    case _: String => 4 + operand.asInstanceOf[String].length  // length prefix + UTF-8 bytes
    case _ => 0

  def encode(): Array[Byte] =
    val buffer = ArrayBuffer[Byte]()
    buffer += opcode.code.toByte
    operands.foreach(encodeOperand(_, buffer))
    buffer.toArray

  private def encodeOperand(operand: AnyRef, buffer: ArrayBuffer[Byte]): Unit =
    operand match
      case i: java.lang.Integer =>
        val value = i.intValue()
        buffer += ((value >> 24) & 0xFF).toByte
        buffer += ((value >> 16) & 0xFF).toByte
        buffer += ((value >> 8) & 0xFF).toByte
        buffer += (value & 0xFF).toByte
      case l: java.lang.Long =>
        var x = l.longValue()
        (0 until 8).foreach { _ =>
          buffer += (x & 0xFF).toByte
          x = x >> 8
        }
      case d: java.lang.Double =>
        val bits = java.lang.Double.doubleToLongBits(d.doubleValue())
        encodeOperand(java.lang.Long.valueOf(bits), buffer)
      case s: String =>
        val bytes = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        val len = java.lang.Integer.valueOf(bytes.length)
        encodeOperand(len, buffer)
        buffer ++= bytes
      case _ =>

  override def toString: String =
    s"$opcode${operands.mkString("(", ", ", ")")}"

object Instruction:
  def pushI32(value: Int): Instruction =
    new Instruction(Opcode.PushI32, Array[AnyRef](java.lang.Integer.valueOf(value)))

  def pushFloat64(value: Double): Instruction =
    new Instruction(Opcode.PushFloat64, Array[AnyRef](java.lang.Double.valueOf(value)))

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

  def swap(): Instruction =
    new Instruction(Opcode.Swap, Array.empty)

  def rotate(): Instruction =
    new Instruction(Opcode.Rotate, Array.empty)

  def unary(op: UnaryOpcode): Instruction =
    new Instruction(op.toOpcode, Array.empty)

  def binary(op: BinaryOpcode): Instruction =
    new Instruction(op.toOpcode, Array.empty)

  def returnUndef(): Instruction =
    new Instruction(Opcode.ReturnUndef, Array.empty)

  def returnInst(): Instruction =
    new Instruction(Opcode.Return, Array.empty)

  def getLoc(index: Int): Instruction =
    new Instruction(Opcode.GetLoc, Array[AnyRef](java.lang.Integer.valueOf(index)))

  def putLoc(index: Int): Instruction =
    new Instruction(Opcode.PutLoc, Array[AnyRef](java.lang.Integer.valueOf(index)))

  def ifFalse(offset: Int): Instruction =
    new Instruction(Opcode.IfFalse, Array[AnyRef](java.lang.Integer.valueOf(offset)))

  def ifTrue(offset: Int): Instruction =
    new Instruction(Opcode.IfTrue, Array[AnyRef](java.lang.Integer.valueOf(offset)))

  def goto(offset: Int): Instruction =
    new Instruction(Opcode.Goto, Array[AnyRef](java.lang.Integer.valueOf(offset)))

  def breakInst(): Instruction =
    new Instruction(Opcode.Break, Array.empty)

  def continueInst(): Instruction =
    new Instruction(Opcode.Continue, Array.empty)

  def call(argc: Int): Instruction =
    new Instruction(Opcode.Call, Array[AnyRef](java.lang.Integer.valueOf(argc)))

  def newObject(): Instruction =
    new Instruction(Opcode.NewObject, Array.empty)

  def getProp(name: String): Instruction =
    new Instruction(Opcode.GetProp, Array[AnyRef](name))

  def setProp(name: String): Instruction =
    new Instruction(Opcode.SetProp, Array[AnyRef](name))

  def defVar(name: String): Instruction =
    new Instruction(Opcode.DefVar, Array[AnyRef](name))

  def defFun(name: String): Instruction =
    new Instruction(Opcode.DefFun, Array[AnyRef](name))

  def getGlobal(name: String): Instruction =
    new Instruction(Opcode.GetGlobal, Array[AnyRef](name))

  def getConst(index: Int): Instruction =
    new Instruction(Opcode.GetConst, Array[AnyRef](java.lang.Integer.valueOf(index)))

  def newArray(size: Int): Instruction =
    new Instruction(Opcode.NewArray, Array[AnyRef](java.lang.Integer.valueOf(size)))

  def getElem(): Instruction =
    new Instruction(Opcode.GetElem, Array.empty)

  def setElem(): Instruction =
    new Instruction(Opcode.SetElem, Array.empty)

  def initElem(): Instruction =
    new Instruction(Opcode.InitElem, Array.empty)

enum UnaryOpcode:
  case Neg, Not, LNot
  case PreInc, PostInc, PreDec, PostDec

  def toOpcode: Opcode = this match
    case Neg => Opcode.Neg
    case Not => Opcode.Not
    case LNot => Opcode.LNot
    case PreInc => Opcode.PreInc
    case PostInc => Opcode.PostInc
    case PreDec => Opcode.PreDec
    case PostDec => Opcode.PostDec

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
  val stackSize: Int,
  val freeVars: Array[String] = Array.empty,  // Variables to capture from outer scope
  val paramNames: Array[String] = Array.empty  // Parameter names in order (for closure capture)
):
  override def toString: String =
    s"BytecodeFunction($name, ${bytecode.length} bytes, ${constants.length} constants, ${freeVars.length} free vars)"
