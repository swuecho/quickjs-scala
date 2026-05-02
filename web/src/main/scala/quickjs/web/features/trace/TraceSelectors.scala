package quickjs.web.features.trace

import scala.scalajs.js
import quickjs.web.domain.TraceDomain
import quickjs.web.models.{SelectionState, TraceMeta, TraceData}

object TraceSelectors:
  def selectedPc(traceData: TraceData, selection: SelectionState): Option[Int] =
    TraceDomain.selectedPc(traceData, selection)

  def canStepPrev(selection: SelectionState): Boolean =
    selection.selectedIndex.forall(_ <= 0)

  def canStepNext(
      selection: SelectionState,
      events: js.Array[js.Dynamic]
  ): Boolean =
    selection.selectedIndex.forall(_ >= events.length - 1)

  def metaText(meta: Option[TraceMeta]): String =
    meta match
      case Some(data) =>
        val name =
          if data.functionName.nonEmpty then data.functionName else "<script>"
        s"bytecode ${data.bytecodeLength} bytes, constants ${data.constantsCount}, function $name"
      case None => "No trace yet"
