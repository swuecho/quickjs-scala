package quickjs.web.features.trace

import com.raquo.laminar.api.L.*
import quickjs.web.components.{
  BytecodeComponent,
  BytecodeProps,
  TraceListComponent
}
import quickjs.web.domain.TraceDomain
import quickjs.web.models.{SelectionState, TraceData}

object TraceFeature:
  final case class State(
      traceData: TraceData,
      selection: SelectionState
  )

  enum Action:
    case SetData(data: TraceData)
    case SelectIndex(index: Option[Int])
    case StepPrev
    case StepNext

  def empty: State = State(
    traceData = TraceData.empty,
    selection = SelectionState.empty
  )

  def reduce(state: State, action: Action): State =
    action match
      case Action.SetData(data) =>
        state.copy(traceData = data, selection = SelectionState.empty)
      case Action.SelectIndex(index) =>
        val selection = TraceDomain.computeSelection(
          index,
          state.traceData.events,
          state.selection
        )
        state.copy(selection = selection)
      case Action.StepPrev =>
        stepSelection(state, -1)
      case Action.StepNext =>
        stepSelection(state, 1)

  def view(state: Signal[State], dispatch: Observer[Action]): HtmlElement =
    val traceDataSignal = state.map(_.traceData)
    val selectionSignal = state.map(_.selection)
    val eventsSignal = traceDataSignal.map(_.events)
    val metaTextSignal =
      traceDataSignal.map(data => TraceSelectors.metaText(data.meta))
    val canStepPrevSignal = selectionSignal.map(TraceSelectors.canStepPrev)
    val canStepNextSignal =
      selectionSignal.combineWith(eventsSignal).map {
        case (selection, events) =>
          TraceSelectors.canStepNext(selection, events)
      }

    div(
      child <-- bytecodeView(traceDataSignal, selectionSignal),
      TraceListComponent(
        events = eventsSignal,
        selection = selectionSignal,
        onSelectIndex = dispatch.contramap(Action.SelectIndex(_)),
        onStepPrev = dispatch.contramap(_ => Action.StepPrev),
        onStepNext = dispatch.contramap(_ => Action.StepNext),
        canStepPrev = canStepPrevSignal,
        canStepNext = canStepNextSignal,
        metaText = metaTextSignal
      )
    )

  private def bytecodeView(
      traceDataSignal: Signal[TraceData],
      selectionSignal: Signal[SelectionState]
  ): Signal[HtmlElement] =
    traceDataSignal.combineWith(selectionSignal).map {
      case (traceData, selection) =>
        val selectedPc = TraceSelectors.selectedPc(traceData, selection)

        BytecodeComponent(
          BytecodeProps(
            instructions = traceData.instructions,
            bytecode = traceData.bytecode,
            selectedPc = selectedPc
          )
        )
    }

  private def stepSelection(state: State, delta: Int): State =
    val events = state.traceData.events
    if events.isEmpty then state
    else
      state.selection.selectedIndex match
        case Some(current) =>
          val maxIndex = Math.max(0, events.length - 1)
          val nextIndex = Math.min(maxIndex, Math.max(0, current + delta))
          val selection = TraceDomain.computeSelection(
            Some(nextIndex),
            events,
            state.selection
          )
          state.copy(selection = selection)
        case None =>
          state
