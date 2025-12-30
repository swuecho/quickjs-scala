package quickjs.web.components

import com.raquo.laminar.api.L.*
import scala.scalajs.js
import quickjs.web.models.{TraceData, TraceMeta, SelectionState}

object TraceListComponent:
  def apply(
    traceData: Signal[TraceData],
    selection: Signal[SelectionState],
    onSelectIndex: Observer[Option[Int]],
    onStepPrev: Observer[Unit],
    onStepNext: Observer[Unit]
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
            disabled <-- selection.map(s => s.selectedIndex.forall(_ <= 0)),
            onClick.mapTo(()) --> onStepPrev
          ),
          button(
            "Next",
            disabled <-- selection.combineWith(traceData).map { case (sel, data) =>
              sel.selectedIndex.forall(_ >= data.events.length - 1)
            },
            onClick.mapTo(()) --> onStepNext
          )
        ),
        div(
          cls := "meta",
          child.text <-- traceData.map(data => formatMeta(data.meta))
        )
      ),
      div(
        cls := "events",
        children <-- traceData.map { data =>
          data.events.zipWithIndex.map { case (event, idx) =>
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
    event.`type`.toString match
      case "instruction" =>
        val location =
          if js.typeOf(event.location) != "undefined" && event.location != null then
            val line = event.location.line.asInstanceOf[Int]
            val column = event.location.column.asInstanceOf[Int]
            s"@$line:$column"
          else ""
        s"pc ${event.pc} · ${event.opcode} $location".trim
      case "call" => s"call ${event.functionName}"
      case "return" => s"return ${event.functionName}"
      case _ => "event"
  
  private def formatMeta(meta: Option[TraceMeta]): String =
    meta match
      case Some(data) =>
        val name = if data.functionName.nonEmpty then data.functionName else "<script>"
        s"bytecode ${data.bytecodeLength} bytes, constants ${data.constantsCount}, function $name"
      case None => "No trace yet"
  
