package quickjs.web.features.editor

import com.raquo.laminar.api.L.*
import quickjs.web.components.EditorComponent
import quickjs.web.models.EditorState

object EditorFeature {
  type State = EditorState

  enum Action {
    case UpdateSource(value: String)
    case SetReplMode(enabled: Boolean)
  }

  def empty: State = EditorState.empty

  def reduce(state: State, action: Action): State =
    action match {
      case Action.UpdateSource(value) =>
        state.copy(source = value)
      case Action.SetReplMode(enabled) =>
        state.copy(replMode = enabled)
    }

  def view(
      state: Signal[State],
      dispatch: Observer[Action],
      onRun: Observer[Unit]
  ): HtmlElement =
    EditorComponent(
      state = state,
      onSourceChange = dispatch.contramap(Action.UpdateSource(_)),
      onReplToggle = dispatch.contramap(Action.SetReplMode(_)),
      onRun = onRun
    )
}
