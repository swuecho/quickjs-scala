package quickjs.web.domain

import scala.scalajs.js
import quickjs.web.models.{SelectionState, TraceData}

object TraceDomain {
  def computeSelection(
      newIndex: Option[Int],
      events: js.Array[js.Dynamic],
      previous: SelectionState
  ): SelectionState = {
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
  }

  def selectedPc(traceData: TraceData, selection: SelectionState): Option[Int] =
    selection.selectedIndex.flatMap { idx =>
      if idx >= 0 && idx < traceData.events.length then {
        val event = traceData.events(idx)
        if event.`type`.toString == "instruction" then
          Some(event.pc.asInstanceOf[Int])
        else None
      }
      else None
    }

  private def currentStackDepth(
      idxOpt: Option[Int],
      events: js.Array[js.Dynamic]
  ): Int =
    idxOpt match {
      case Some(idx) if idx >= 0 && idx < events.length =>
        val event = events(idx)
        if event.`type`.toString == "instruction" && js.typeOf(
            event.stack
          ) != "undefined"
        then event.stack.asInstanceOf[js.Array[js.Dynamic]].length
        else -1
      case _ => -1
    }
}
