package quickjs.web.components

import com.raquo.laminar.api.L.*
import quickjs.web.models.EditorState

object EditorComponent:
  def apply(
    state: Signal[EditorState],
    onSourceChange: Observer[String],
    onReplToggle: Observer[Boolean],
    onRun: Observer[Unit]
  ): HtmlElement =
    div(
      cls := "panel",
      div(
        cls := "panel-header",
        div(cls := "panel-title", "Source"),
        div(
          cls := "controls",
          label(
            cls := "toggle",
            input(
              typ := "checkbox",
              controlled(
                checked <-- state.map(_.replMode),
                onClick.mapToChecked --> onReplToggle
              )
            ),
            span("REPL mode")
          ),
          button(
            child.text <-- state.map(state => if state.isRunning then "Running..." else "Run Trace"),
            disabled <-- state.map(_.isRunning),
            onClick.mapTo(()) --> onRun
          )
        )
      ),
      textArea(
        spellCheck := false,
        controlled(
          value <-- state.map(_.source),
          onInput.mapToValue --> onSourceChange
        )
      ),
      div(
        cls := "error",
        cls.toggle("hidden") <-- state.map(_.error.isEmpty),
        child.text <-- state.map(_.error.getOrElse(""))
      )
    )
