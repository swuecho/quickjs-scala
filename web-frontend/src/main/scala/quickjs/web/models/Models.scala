package quickjs.web.models

import scala.scalajs.js

// =================== API Models ===================
// Models for API communication with trace server

final case class TraceMeta(
  bytecodeLength: Int,
  constantsCount: Int,
  functionName: String
)

final case class TraceResponse(
  error: Option[String],
  stack: Option[String],
  bytecodeLength: Int,
  constantsCount: Int,
  functionName: String,
  trace: js.Array[js.Dynamic],
  bytecodeHex: String,
  instructions: js.Array[js.Dynamic]
)

object TraceResponse:
  def fromDynamic(payload: js.Dynamic): TraceResponse =
    TraceResponse(
      error = if js.typeOf(payload.error) != "undefined" then Some(payload.error.toString) else None,
      stack = if js.typeOf(payload.stack) != "undefined" then Some(payload.stack.toString) else None,
      bytecodeLength = payload.bytecodeLength.asInstanceOf[Int],
      constantsCount = payload.constantsCount.asInstanceOf[Int],
      functionName = payload.functionName.asInstanceOf[String],
      trace = payload.trace.asInstanceOf[js.Array[js.Dynamic]],
      bytecodeHex = payload.bytecodeHex.asInstanceOf[String],
      instructions = payload.instructions.asInstanceOf[js.Array[js.Dynamic]]
    )

// =================== Application Models ===================
// Models for application state and component data

final case class TraceData(
  meta: Option[TraceMeta],
  events: js.Array[js.Dynamic],
  bytecode: Vector[String],
  instructions: js.Array[js.Dynamic]
)

final case class EditorState(
  source: String,
  replMode: Boolean,
  isRunning: Boolean,
  error: Option[String]
)

final case class SelectionState(
  selectedIndex: Option[Int],
  stackDepth: Int,
  stackDelta: String
)

// =================== Factory Methods ===================
// Convenient empty/default state constructors

object TraceData:
  def empty: TraceData = TraceData(
    meta = None,
    events = js.Array(),
    bytecode = Vector.empty,
    instructions = js.Array()
  )

object EditorState:
  val defaultSource =
    """// Try it:
      |const add = (a, b) => a + b;
      |let total = 0;
      |for (let i = 0; i < 3; i++) {
      |  total = add(total, i);
      |}
      |total;""".stripMargin
  
  def empty: EditorState = EditorState(
    source = defaultSource,
    replMode = false,
    isRunning = false,
    error = None
  )

object SelectionState:
  def empty: SelectionState = SelectionState(
    selectedIndex = None,
    stackDepth = 0,
    stackDelta = "none"
  )

  def compute(
    newIndex: Option[Int],
    events: js.Array[js.Dynamic],
    previous: SelectionState
  ): SelectionState =
    val depth = currentStackDepth(newIndex, events)
    val delta =
      if depth < 0 then "none"
      else if depth > previous.stackDepth then "push"
      else if depth < previous.stackDepth then "pop"
      else "same"
    SelectionState(
      selectedIndex = newIndex,
      stackDepth = if depth < 0 then 0 else depth,
      stackDelta = delta
    )

  private def currentStackDepth(
    idxOpt: Option[Int],
    events: js.Array[js.Dynamic]
  ): Int =
    idxOpt match
      case Some(idx) if idx >= 0 && idx < events.length =>
        val event = events(idx)
        if event.`type`.toString == "instruction" && js.typeOf(event.stack) != "undefined" then
          event.stack.asInstanceOf[js.Array[js.Dynamic]].length
        else
          -1
      case _ => -1
