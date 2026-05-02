package quickjs.web.state

import quickjs.web.features.editor.EditorFeature
import quickjs.web.features.trace.TraceFeature
import quickjs.web.features.stack.StackFeature

object Reducer:
  def reduce(state: AppState, action: AppAction): (AppState, List[Effect]) =
    action match
      case AppAction.Editor(editorAction) =>
        val nextEditor = EditorFeature.reduce(state.editor, editorAction)
        (state.copy(editor = nextEditor), Nil)
      case AppAction.Trace(traceAction) =>
        val nextTrace = TraceFeature.reduce(state.trace, traceAction)
        val nextStack = StackFeature.fromTrace(nextTrace)
        val nextEditor =
          traceAction match
            case TraceFeature.Action.SetData(_) =>
              state.editor.copy(isRunning = false, error = None)
            case _ =>
              state.editor
        (
          state.copy(editor = nextEditor, trace = nextTrace, stack = nextStack),
          Nil
        )
      case AppAction.RunTrace =>
        val nextEditor = state.editor.copy(isRunning = true, error = None)
        (state.copy(editor = nextEditor), List(Effect.FetchTrace(nextEditor)))
      case AppAction.TraceFailed(error) =>
        val nextEditor =
          state.editor.copy(isRunning = false, error = Some(error))
        (state.copy(editor = nextEditor), Nil)
