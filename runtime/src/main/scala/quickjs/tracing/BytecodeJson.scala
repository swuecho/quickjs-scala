package quickjs.tracing

object BytecodeJson:
  def instructionsToJson(instructions: Vector[InstructionInfo]): String =
    val sb = StringBuilder()
    sb.append('[')
    instructions.zipWithIndex.foreach { case (inst, idx) =>
      if idx > 0 then sb.append(',')
      sb.append('{')
      JsonUtil.appendString(sb, "pc")
      sb.append(':')
      sb.append(inst.pc)
      sb.append(',')
      JsonUtil.appendString(sb, "opcode")
      sb.append(':')
      JsonUtil.appendString(sb, inst.opcode.toString)
      sb.append(',')
      JsonUtil.appendString(sb, "size")
      sb.append(':')
      sb.append(inst.size)
      sb.append(',')
      JsonUtil.appendString(sb, "operand")
      sb.append(':')
      inst.operand match
        case Some(value) => JsonUtil.appendString(sb, value)
        case None => sb.append("null")
      sb.append('}')
    }
    sb.append(']')
    sb.toString
