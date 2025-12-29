package quickjs.tracing

object TraceJson:
  def eventsToJson(events: Vector[TraceEvent]): String =
    val sb = StringBuilder()
    sb.append('[')
    events.zipWithIndex.foreach { case (event, idx) =>
      if idx > 0 then sb.append(',')
      appendEvent(sb, event)
    }
    sb.append(']')
    sb.toString

  private def appendEvent(sb: StringBuilder, event: TraceEvent): Unit =
    event match
      case InstructionTrace(pc, opcode, stack, locals, location) =>
        sb.append('{')
        appendField(sb, "type", "instruction"); sb.append(',')
        appendField(sb, "pc", pc); sb.append(',')
        appendField(sb, "opcode", opcode.toString); sb.append(',')
        sb.append("\"stack\":"); appendValues(sb, stack); sb.append(',')
        sb.append("\"locals\":"); appendLocals(sb, locals); sb.append(',')
        sb.append("\"location\":")
        location match
          case Some(loc) => appendLocation(sb, loc)
          case None => sb.append("null")
        sb.append('}')
      case CallTrace(functionName, args) =>
        sb.append('{')
        appendField(sb, "type", "call"); sb.append(',')
        appendField(sb, "functionName", functionName); sb.append(',')
        sb.append("\"args\":"); appendValues(sb, args)
        sb.append('}')
      case ReturnTrace(functionName, value) =>
        sb.append('{')
        appendField(sb, "type", "return"); sb.append(',')
        appendField(sb, "functionName", functionName); sb.append(',')
        sb.append("\"value\":"); appendValue(sb, value)
        sb.append('}')

  private def appendLocation(sb: StringBuilder, loc: SourceLocation): Unit =
    sb.append('{')
    appendField(sb, "line", loc.line); sb.append(',')
    appendField(sb, "column", loc.column)
    sb.append('}')

  private def appendLocals(sb: StringBuilder, locals: Vector[TraceLocal]): Unit =
    sb.append('[')
    locals.zipWithIndex.foreach { case (local, idx) =>
      if idx > 0 then sb.append(',')
      sb.append('{')
      appendField(sb, "index", local.index); sb.append(',')
      sb.append("\"name\":")
      local.name match
        case Some(name) => JsonUtil.appendString(sb, name)
        case None => sb.append("null")
      sb.append(',')
      sb.append("\"value\":"); appendValue(sb, local.value)
      sb.append('}')
    }
    sb.append(']')

  private def appendValues(sb: StringBuilder, values: Vector[TraceValue]): Unit =
    sb.append('[')
    values.zipWithIndex.foreach { case (value, idx) =>
      if idx > 0 then sb.append(',')
      appendValue(sb, value)
    }
    sb.append(']')

  private def appendValue(sb: StringBuilder, value: TraceValue): Unit =
    sb.append('{')
    appendField(sb, "kind", value.kind); sb.append(',')
    appendField(sb, "display", value.display); sb.append(',')
    sb.append("\"preview\":")
    value.preview match
      case Some(preview) => JsonUtil.appendString(sb, preview)
      case None => sb.append("null")
    sb.append('}')

  private def appendField(sb: StringBuilder, key: String, value: String): Unit =
    JsonUtil.appendString(sb, key)
    sb.append(':')
    JsonUtil.appendString(sb, value)

  private def appendField(sb: StringBuilder, key: String, value: Int): Unit =
    JsonUtil.appendString(sb, key)
    sb.append(':')
    sb.append(value)
