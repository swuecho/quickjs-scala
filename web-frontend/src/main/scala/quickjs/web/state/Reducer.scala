package quickjs.web.state

import quickjs.web.domain.TraceDomain
import quickjs.web.models.{SelectionState, TraceData}

object Reducer:
  def reduce(state: AppState, action: Action): (AppState, List[Effect]) =
    action match
      case Action.UpdateSource(value) =>
        (state.copy(editor = state.editor.copy(source = value)), Nil)
      case Action.SetReplMode(enabled) =>
        (state.copy(editor = state.editor.copy(replMode = enabled)), Nil)
      case Action.RunTrace =>
        val nextState = state.copy(editor = state.editor.copy(isRunning = true, error = None))
        (nextState, List(Effect.FetchTrace(state.editor)))
      case Action.TraceLoaded(data) =>
        (
          state.copy(
            editor = state.editor.copy(isRunning = false, error = None),
            traceData = data,
            selection = SelectionState.empty
          ),
          Nil
        )
      case Action.TraceFailed(error) =>
        (state.copy(editor = state.editor.copy(isRunning = false, error = Some(error))), Nil)
      case Action.SelectIndex(index) =>
        val selection = TraceDomain.computeSelection(index, state.traceData.events, state.selection)
        (state.copy(selection = selection), Nil)
      case Action.StepPrev =>
        (stepSelection(state, -1), Nil)
      case Action.StepNext =>
        (stepSelection(state, 1), Nil)

  private def stepSelection(state: AppState, delta: Int): AppState =
    val events = state.traceData.events
    if events.isEmpty then
      state
    else
      state.selection.selectedIndex match
        case Some(current) =>
          val maxIndex = Math.max(0, events.length - 1)
          val nextIndex = Math.min(maxIndex, Math.max(0, current + delta))
          val selection = TraceDomain.computeSelection(Some(nextIndex), events, state.selection)
          state.copy(selection = selection)
        case None =>
          state
