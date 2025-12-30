package quickjs.web.components

import com.raquo.laminar.api.L.*
import scala.scalajs.js
import quickjs.web.models.{TraceData, TraceMeta, SelectionState}

case class TraceListProps(
  traceData: TraceData,
  onSelectionChange: SelectionState => Unit
)

object TraceListComponent:
  def apply(props: TraceListProps): HtmlElement =
    val selectionVar = Var(SelectionState.empty)
    
    def updateSelection(newIndex: Option[Int]): Unit =
      val current = selectionVar.now()
      val newStackDepth = calculateStackDepth(newIndex, props.traceData.events)
      val newStackDelta = calculateStackDelta(current.stackDepth, newStackDepth)
      
      val newSelection = current.copy(
        selectedIndex = newIndex,
        stackDepth = newStackDepth,
        stackDelta = newStackDelta
      )
      selectionVar.set(newSelection)
      props.onSelectionChange(newSelection)
    
    div(
      cls := "panel",
      div(
        cls := "panel-header",
        div(cls := "panel-title", "Trace Events"),
        div(
          cls := "stepper",
          button(
            "Prev",
            disabled <-- selectionVar.signal.map(s => s.selectedIndex.forall(_ <= 0)),
            onClick --> { _ => 
              selectionVar.now().selectedIndex.foreach { current =>
                updateSelection(Some(Math.max(0, current - 1)))
              }
            }
          ),
          button(
            "Next",
            disabled <-- selectionVar.signal.map { s =>
              s.selectedIndex.forall(_ >= props.traceData.events.length - 1)
            },
            onClick --> { _ => 
              selectionVar.now().selectedIndex.foreach { current =>
                val maxIndex = props.traceData.events.length - 1
                updateSelection(Some(Math.min(maxIndex, current + 1)))
              }
            }
          )
        ),
        div(
          cls := "meta",
          formatMeta(props.traceData.meta)
        )
      ),
      div(
        cls := "events",
        props.traceData.events.zipWithIndex.map { case (event, idx) => 
          eventCard(event, idx, selectionVar, updateSelection)
        }
      )
    )
  
  private def eventCard(
    event: js.Dynamic, 
    index: Int, 
    selectionVar: Var[SelectionState],
    updateSelection: Option[Int] => Unit
  ): HtmlElement =
    div(
      cls := "event",
      cls.toggle("selected") <-- selectionVar.signal.map(_.selectedIndex.contains(index)),
      onClick --> { _ => updateSelection(Some(index)) },
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
  
  private def calculateStackDepth(idxOpt: Option[Int], events: js.Array[js.Dynamic]): Int =
    idxOpt match
      case Some(idx) if idx >= 0 && idx < events.length =>
        val event = events(idx)
        if event.`type`.toString == "instruction" && js.typeOf(event.stack) != "undefined" then
          event.stack.asInstanceOf[js.Array[js.Dynamic]].length
        else -1
      case _ => -1
  
  private def calculateStackDelta(oldDepth: Int, newDepth: Int): String =
    if newDepth > oldDepth then "push"
    else if newDepth < oldDepth then "pop"
    else "same"