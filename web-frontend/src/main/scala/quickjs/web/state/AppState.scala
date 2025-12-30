package quickjs.web.state

import quickjs.web.models.{EditorState, TraceData, SelectionState}

final case class AppState(
  editor: EditorState,
  traceData: TraceData,
  selection: SelectionState
)

object AppState:
  def empty: AppState = AppState(
    editor = EditorState.empty,
    traceData = TraceData.empty,
    selection = SelectionState.empty
  )
