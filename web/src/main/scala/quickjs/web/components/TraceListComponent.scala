package quickjs.web.components

import com.raquo.laminar.api.L.*
import scala.scalajs.js
import quickjs.web.models.SelectionState

object TraceListComponent {
  def apply(
      events: Signal[js.Array[js.Dynamic]],
      selection: Signal[SelectionState],
      onSelectIndex: Observer[Option[Int]],
      onStepPrev: Observer[Unit],
      onStepNext: Observer[Unit],
      canStepPrev: Signal[Boolean],
      canStepNext: Signal[Boolean],
      metaText: Signal[String]
  ): HtmlElement =
    div(
      cls := "panel",
      div(
        cls := "panel-header",
        div(cls := "panel-title", "Trace Events"),
        div(
          cls := "stepper",
          button(
            "Prev",
            disabled <-- canStepPrev,
            onClick.mapTo(()) --> onStepPrev
          ),
          button(
            "Next",
            disabled <-- canStepNext,
            onClick.mapTo(()) --> onStepNext
          )
        ),
        div(
          cls := "meta",
          child.text <-- metaText
        )
      ),
      div(
        cls := "events",
        children <-- events.map { dataEvents =>
          dataEvents.zipWithIndex.map { case (event, idx) =>
            eventCard(event, idx, selection, onSelectIndex)
          }.toSeq
        }
      )
    )

  private def eventCard(
      event: js.Dynamic,
      index: Int,
      selection: Signal[SelectionState],
      onSelectIndex: Observer[Option[Int]]
  ): HtmlElement =
    div(
      cls := "event",
      cls.toggle("selected") <-- selection.map(_.selectedIndex.contains(index)),
      onClick.mapTo(Some(index)) --> onSelectIndex,
      div(cls := "kind", event.`type`.toString),
      div(cls := "title", eventSummary(event))
    )

  private def eventSummary(event: js.Dynamic): String =
    event.`type`.toString match {
      case "instruction" =>
        val location =
          if js.typeOf(event.location) != "undefined" && event.location != null
          then {
            val line = event.location.line.asInstanceOf[Int]
            val column = event.location.column.asInstanceOf[Int]
            s"@$line:$column"
          } else ""
        s"pc ${event.pc} · ${event.opcode} $location".trim
      case "call"   => s"call ${event.functionName}"
      case "return" => s"return ${event.functionName}"
      case _        => "event"
    }
}
