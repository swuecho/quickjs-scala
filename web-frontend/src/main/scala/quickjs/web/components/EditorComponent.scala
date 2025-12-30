package quickjs.web.components

import com.raquo.laminar.api.L.*
import quickjs.web.models.EditorState

type EditorCallback = EditorState => Unit

object EditorComponent:
  def apply(
    initialState: EditorState,
    onChange: EditorCallback,
    onRun: EditorCallback
  ): HtmlElement =
    val stateVar = Var(initialState)
    
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
                checked <-- stateVar.signal.map(_.replMode),
                onClick.mapToChecked --> { checked =>
                  val newState = stateVar.now().copy(replMode = checked)
                  stateVar.set(newState)
                  onChange(newState)
                }
              )
            ),
            span("REPL mode")
          ),
          button(
            child.text <-- stateVar.signal.map(state => if state.isRunning then "Running..." else "Run Trace"),
            disabled <-- stateVar.signal.map(_.isRunning),
            onClick --> { _ => 
              onRun(stateVar.now())
            }
          )
        )
      ),
      textArea(
        spellCheck := false,
        controlled(
          value <-- stateVar.signal.map(_.source),
          onInput.mapToValue --> { source =>
            val newState = stateVar.now().copy(source = source)
            stateVar.set(newState)
            onChange(newState)
          }
        )
      ),
      div(
        cls := "error",
        cls.toggle("hidden") <-- stateVar.signal.map(_.error.isEmpty),
        child.text <-- stateVar.signal.map(_.error.getOrElse(""))
      )
    )
  
  def updateError(stateVar: Var[EditorState], error: Option[String]): Unit =
    stateVar.update(_.copy(error = error, isRunning = false))