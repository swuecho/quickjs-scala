package quickjs.web.state

import quickjs.web.features.editor.EditorFeature
import quickjs.web.features.trace.TraceFeature
import quickjs.web.features.stack.StackFeature

final case class AppState(
    editor: EditorFeature.State,
    trace: TraceFeature.State,
    stack: StackFeature.State
)

object AppState:
  def empty: AppState =
    val traceState = TraceFeature.empty
    AppState(
      editor = EditorFeature.empty,
      trace = traceState,
      stack = StackFeature.fromTrace(traceState)
    )
