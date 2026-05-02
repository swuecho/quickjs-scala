package quickjs.tracing

object JsonUtil:
  def appendString(sb: StringBuilder, value: String): Unit =
    sb.append('"')
    value.foreach {
      case '"'              => sb.append("\\\"")
      case '\\'             => sb.append("\\\\")
      case '\b'             => sb.append("\\b")
      case '\f'             => sb.append("\\f")
      case '\n'             => sb.append("\\n")
      case '\r'             => sb.append("\\r")
      case '\t'             => sb.append("\\t")
      case ch if ch <= 0x1f => sb.append(f"\\u${ch.toInt}%04x")
      case ch               => sb.append(ch)
    }
    sb.append('"')
