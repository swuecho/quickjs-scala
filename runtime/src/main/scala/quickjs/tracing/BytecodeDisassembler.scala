package quickjs.tracing

import quickjs.bytecode.Opcode
import java.nio.charset.StandardCharsets

final case class InstructionInfo(
    pc: Int,
    opcode: Opcode,
    size: Int,
    operand: Option[String]
)

object BytecodeDisassembler {
  def disassemble(bytecode: Array[Byte]): Vector[InstructionInfo] = {
    val buffer = Vector.newBuilder[InstructionInfo]
    var pc = 0
    while pc < bytecode.length do {
      val opcode =
        Opcode.fromCode(bytecode(pc).toInt & 0xff).getOrElse(Opcode.Invalid)
      val (size, operand) = operandSizeAndPreview(opcode, bytecode, pc)
      buffer += InstructionInfo(pc, opcode, size, operand)
      pc += size
    }
    buffer.result()
  }

  private def operandSizeAndPreview(
      opcode: Opcode,
      bytecode: Array[Byte],
      pc: Int
  ): (Int, Option[String]) =
    opcode match {
      case Opcode.PushI32 | Opcode.GetLoc | Opcode.PutLoc | Opcode.GetArg |
          Opcode.PutArg | Opcode.IfFalse | Opcode.IfTrue | Opcode.Goto |
          Opcode.Call | Opcode.CallMethod | Opcode.New | Opcode.GetConst |
          Opcode.NewArray | Opcode.EnterScope | Opcode.LeaveScope |
          Opcode.SetLocUninitialized | Opcode.GetLocCheck |
          Opcode.SetLocConst =>
        val value = readInt32(bytecode, pc + 1)
        (5, Some(value.toString))
      case Opcode.TryStart =>
        val catchPc = readInt32(bytecode, pc + 1)
        val finallyPc = readInt32(bytecode, pc + 5)
        (9, Some(s"catch=$catchPc, finally=$finallyPc"))
      case Opcode.PushFloat64 =>
        val value = readDouble(bytecode, pc + 1)
        (9, Some(value.toString))
      case Opcode.GetProp | Opcode.SetProp | Opcode.GetGlobal |
          Opcode.PutGlobal | Opcode.DefVar | Opcode.DefFun =>
        val (name, size) = readString(bytecode, pc + 1)
        (1 + size, Some(name))
      case _ =>
        (1, None)
    }

  private def readInt32(buf: Array[Byte], pc: Int): Int =
    ((buf(pc) & 0xff) << 24) | ((buf(pc + 1) & 0xff) << 16) |
      ((buf(pc + 2) & 0xff) << 8) | (buf(pc + 3) & 0xff)

  private def readInt64(buf: Array[Byte], pc: Int): Long =
    ((buf(pc).toLong & 0xff) << 56) |
      ((buf(pc + 1).toLong & 0xff) << 48) |
      ((buf(pc + 2).toLong & 0xff) << 40) |
      ((buf(pc + 3).toLong & 0xff) << 32) |
      ((buf(pc + 4).toLong & 0xff) << 24) |
      ((buf(pc + 5).toLong & 0xff) << 16) |
      ((buf(pc + 6).toLong & 0xff) << 8) |
      (buf(pc + 7).toLong & 0xff)

  private def readDouble(buf: Array[Byte], pc: Int): Double =
    java.lang.Double.longBitsToDouble(readInt64(buf, pc))

  private def readString(buf: Array[Byte], pc: Int): (String, Int) = {
    val len = readInt32(buf, pc)
    val bytes = new Array[Byte](len)
    System.arraycopy(buf, pc + 4, bytes, 0, len)
    (new String(bytes, StandardCharsets.UTF_8), 4 + len)
  }
}
