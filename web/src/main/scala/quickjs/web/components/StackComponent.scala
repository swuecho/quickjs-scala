package quickjs.web.components

import com.raquo.laminar.api.L.*
import scala.scalajs.js
import scala.scalajs.js.JSON
import quickjs.web.models.SelectionState

object StackComponent:
  def apply(
    selection: Signal[SelectionState],
    events: Signal[js.Array[js.Dynamic]]
  ): HtmlElement =
    val contentSignal = selection.combineWith(events).map { case (currentSelection, currentEvents) =>
      calculateContent(currentSelection, currentEvents)
    }

    div(
      cls := "panel details",
      div(
        cls := "panel-header",
        div(cls := "panel-title", "Runtime Stack")
      ),
      div(
        cls := "stack",
        cls.toggle("stack-push") <-- selection.map(_.stackDelta == "push"),
        cls.toggle("stack-pop") <-- selection.map(_.stackDelta == "pop"),
        children <-- contentSignal.map(_._1)
      ),
      div(
        cls := "panel-header",
        div(cls := "panel-title", "Event Details")
      ),
      pre(
        idAttr := "details",
        child.text <-- contentSignal.map(_._2)
      )
    )
  
  private def calculateContent(
    selection: SelectionState,
    events: js.Array[js.Dynamic]
  ): (Seq[HtmlElement], String) =
    selection.selectedIndex match
      case Some(idx) if idx >= 0 && idx < events.length =>
        val event = events(idx)
        if event.`type`.toString == "instruction" && js.typeOf(event.stack) != "undefined" then
          val stack = event.stack.asInstanceOf[js.Array[js.Dynamic]]
          val eventDetails = JSON.stringify(events(idx), space = 2)
          
          if stack.isEmpty then
            (Seq(div(cls := "stack-empty", "Stack is empty")), eventDetails)
          else
            val topIndex = stack.length - 1
            val stackContent = (topIndex to 0 by -1).map { i =>
              val value = stack(i)
              val display = value.display.toString
              val kind = value.kind.toString
              div(
                cls := "stack-item",
                cls.toggle("top") := (i == topIndex),
                span(cls := "stack-kind", kind),
                span(cls := "stack-value", display),
                if i == topIndex then span(cls := "stack-top", "TOP") else emptyNode
              )
            }.toSeq
            (stackContent, eventDetails)
        else
          (Seq(div(cls := "stack-empty", "Select an instruction event to see stack state.")), "Select a trace event to inspect.")
      case _ =>
        (Seq(div(cls := "stack-empty", "Select an instruction event to see stack state.")), "Select a trace event to inspect.")
