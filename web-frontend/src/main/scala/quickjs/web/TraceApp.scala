package quickjs.web

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import quickjs.web.models.{EditorState, TraceData, SelectionState}
import quickjs.web.components.*
import quickjs.web.client.TraceApiClient
import quickjs.web.state.AppState

object TraceApp:
  private val traceEndpoint = "/trace"

  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(
      dom.document.getElementById("app"),
      appView()
    )

  private def appView(): HtmlElement =
    val appStateVar = Var(AppState.empty)
    val editorSignal = appStateVar.signal.map(_.editor)
    val traceDataSignal = appStateVar.signal.map(_.traceData)
    val selectionSignal = appStateVar.signal.map(_.selection)
    
    div(
      cls := "app",
      StatusIndicatorComponent(
        StatusIndicatorProps(
          endpoint = traceEndpoint,
          onStatusChange = Observer[String](_ => ())
        )
      ),
      
      HeaderComponent(),
      
      EditorComponent(
        state = editorSignal,
        onChange = Observer[EditorState](newState =>
          appStateVar.update(state => state.copy(editor = newState))
        ),
        onRun = Observer[EditorState](editorState =>
          runTrace(editorState, appStateVar)
        )
      ),
      
      div(
        cls := "grid",
        div(
          cls := "column",
          child <-- createBytecodeComponent(traceDataSignal, selectionSignal),
          TraceListComponent(
            traceData = traceDataSignal,
            selection = selectionSignal,
            onSelectIndex = Observer[Option[Int]](newIndex =>
              appStateVar.update { state =>
                val newSelection = SelectionState.compute(
                  newIndex,
                  state.traceData.events,
                  state.selection
                )
                state.copy(selection = newSelection)
              }
            )
          )
        ),
        StackComponent(
          selection = selectionSignal,
          events = traceDataSignal.map(_.events)
        )
      )
    )
  
  private def createBytecodeComponent(
    traceDataSignal: Signal[TraceData],
    selectionSignal: Signal[SelectionState]
  ): Signal[HtmlElement] =
    traceDataSignal.combineWith(selectionSignal).map { case (traceData, selection) =>
      val selectedPc = selection.selectedIndex.flatMap { idx =>
        if idx >= 0 && idx < traceData.events.length then
          val event = traceData.events(idx)
          if event.`type`.toString == "instruction" then
            Some(event.pc.asInstanceOf[Int])
          else None
        else None
      }
      
      BytecodeComponent(
        BytecodeProps(
          instructions = traceData.instructions,
          bytecode = traceData.bytecode,
          selectedPc = selectedPc
        )
      )
    }
  
  private def runTrace(
    editorState: EditorState,
    appStateVar: Var[AppState]
  ): Unit =
    appStateVar.update(state =>
      state.copy(editor = state.editor.copy(isRunning = true, error = None))
    )
    
    TraceApiClient.fetchTrace(traceEndpoint, editorState, {
      case Right(newTraceData) =>
        appStateVar.update { state =>
          state.copy(
            editor = state.editor.copy(isRunning = false, error = None),
            traceData = newTraceData,
            selection = SelectionState.empty
          )
        }
      case Left(error) =>
        appStateVar.update(state =>
          state.copy(editor = state.editor.copy(isRunning = false, error = Some(error)))
        )
    })
