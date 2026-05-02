package quickjs.web.features.stack

import com.raquo.laminar.api.L.*
import scala.scalajs.js
import quickjs.web.components.StackComponent
import quickjs.web.features.trace.TraceFeature
import quickjs.web.models.SelectionState

object StackFeature {
  final case class State(
      selection: SelectionState,
      events: js.Array[js.Dynamic]
  )

  enum Action {
    case SyncFromTrace(trace: TraceFeature.State)
  }

  def fromTrace(trace: TraceFeature.State): State =
    State(
      selection = trace.selection,
      events = trace.traceData.events
    )

  def reduce(state: State, action: Action): State =
    action match {
      case Action.SyncFromTrace(trace) =>
        fromTrace(trace)
    }

  def view(state: Signal[State]): HtmlElement =
    StackComponent(
      selection = state.map(_.selection),
      events = state.map(_.events)
    )
}
