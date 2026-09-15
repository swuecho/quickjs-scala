package quickjs.tracing

import quickjs.bytecode.Opcode
import quickjs.value.JSValue
import scala.collection.mutable

final case class SourceLocation(line: Int, column: Int)

sealed trait TraceEvent
final case class InstructionTrace(
    pc: Int,
    opcode: Opcode,
    stack: Vector[TraceValue],
    locals: Vector[TraceLocal],
    location: Option[SourceLocation]
) extends TraceEvent
final case class CallTrace(
    functionName: String,
    args: Vector[TraceValue]
) extends TraceEvent
final case class ReturnTrace(
    functionName: String,
    value: TraceValue
) extends TraceEvent

final case class TraceLocal(index: Int, name: Option[String], value: TraceValue)
final case class TraceValue(
    kind: String,
    display: String,
    preview: Option[String] = None
)

object TraceValue {
  def from(value: JSValue, maxPreview: Int = 40): TraceValue =
    value match {
      case JSValue.Undefined  => TraceValue("undefined", "undefined")
      case JSValue.Null       => TraceValue("null", "null")
      case JSValue.Bool(b)    => TraceValue("boolean", b.toString)
      case JSValue.Int32(i)   => TraceValue("int32", i.toString)
      case JSValue.Float64(d) => TraceValue("float64", d.toString)
      case JSValue.JSStr(s)   =>
        val preview =
          if s.length > maxPreview then Some(s.take(maxPreview))
          else None
        TraceValue("string", s, preview)
      case JSValue.Symbol(id)      => TraceValue("symbol", s"Symbol($id)")
      case JSValue.BigInt(b)       => TraceValue("bigint", b.toString)
      case JSValue.JSArrayVal(arr) =>
        TraceValue("array", s"Array(length=${arr.getLength})")
      case JSValue.Object(_) =>
        TraceValue("object", "Object")
      case JSValue.Function(name, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        val display = if name.nonEmpty then name else "(anonymous)"
        TraceValue("function", display)
      case JSValue.Native(func) =>
        val display = func.getClass.getSimpleName match {
          case ""   => "NativeFunction"
          case name => name
        }
        TraceValue("native", display)
      case JSValue.GlobalRef(name) =>
        TraceValue("global-ref", name)
      case other =>
        TraceValue("unknown", other.toString)
    }
}

trait TraceRecorder {
  def isEnabled: Boolean
  def recordInstruction(event: InstructionTrace): Unit
  def recordCall(event: CallTrace): Unit
  def recordReturn(event: ReturnTrace): Unit
}

object TraceRecorder {
  val Noop: TraceRecorder = new TraceRecorder {
    def isEnabled: Boolean = false
    def recordInstruction(event: InstructionTrace): Unit = ()
    def recordCall(event: CallTrace): Unit = ()
    def recordReturn(event: ReturnTrace): Unit = ()
  }
}

final class TraceCollector(maxEvents: Int = 0) extends TraceRecorder {
  private val events = mutable.ArrayBuffer.empty[TraceEvent]

  def isEnabled: Boolean = true

  def recordInstruction(event: InstructionTrace): Unit =
    append(event)

  def recordCall(event: CallTrace): Unit =
    append(event)

  def recordReturn(event: ReturnTrace): Unit =
    append(event)

  def getEvents: Vector[TraceEvent] = events.toVector

  def clear(): Unit = events.clear()

  private def append(event: TraceEvent): Unit = {
    events += event
    if maxEvents > 0 && events.length > maxEvents then
      events.remove(0, events.length - maxEvents)
  }
}
