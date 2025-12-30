package quickjs.web.components

import com.raquo.laminar.api.L.*
import scala.scalajs.js
import scala.scalajs.js.JSON
import quickjs.web.models.SelectionState

case class StackProps(
  selection: SelectionState,
  events: js.Array[js.Dynamic]
)

object StackComponent:
  def apply(props: StackProps): HtmlElement =
    val (stackContent, eventDetails) = calculateContent(props)
    
    div(
      cls := "panel details",
      div(
        cls := "panel-header",
        div(cls := "panel-title", "Runtime Stack")
      ),
      div(
        cls := "stack",
        cls.toggle("stack-push") := props.selection.stackDelta == "push",
        cls.toggle("stack-pop") := props.selection.stackDelta == "pop",
        stackContent
      ),
      div(
        cls := "panel-header",
        div(cls := "panel-title", "Event Details")
      ),
      pre(
        idAttr := "details",
        eventDetails
      )
    )
  
  private def calculateContent(props: StackProps): (Seq[HtmlElement], String) =
    props.selection.selectedIndex match
      case Some(idx) if idx >= 0 && idx < props.events.length =>
        val event = props.events(idx)
        if event.`type`.toString == "instruction" && js.typeOf(event.stack) != "undefined" then
          val stack = event.stack.asInstanceOf[js.Array[js.Dynamic]]
          val eventDetails = JSON.stringify(props.events(idx), space = 2)
          
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