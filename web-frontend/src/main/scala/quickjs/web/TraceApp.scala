package quickjs.web

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import quickjs.web.models.{EditorState, TraceData, SelectionState}
import quickjs.web.components.*
import quickjs.web.client.TraceApiClient

object TraceApp:
  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(
      dom.document.getElementById("app"),
      appView()
    )

  private def appView(): HtmlElement =
    // Minimal root state - only what truly needs coordination
    val editorStateVar = Var(EditorState.empty)
    val traceDataVar = Var(TraceData.empty)
    val selectionVar = Var(SelectionState.empty)
    
    div(
      cls := "app",
      // Status indicator - completely self-contained component
      StatusIndicatorComponent(
        StatusIndicatorProps(
          endpoint = "/trace",
          onStatusChange = (status: String) => () // No coordination needed
        )
      ),
      
      // Header - pure component, no state
      HeaderComponent(),
      
      // Editor section - manages its own state, communicates via callbacks
      EditorComponent(
        initialState = editorStateVar.now(),
        onChange = (newState: EditorState) => editorStateVar.set(newState),
        onRun = (editorState: EditorState) => 
          runTrace(editorState, traceDataVar, editorStateVar, selectionVar)
      ),
      
      // Main display area - components receive data as immutable props
      div(
        cls := "grid",
        div(
          cls := "column",
          // Bytecode display - pure component with calculated props
          child <-- createBytecodeComponent(traceDataVar, selectionVar),
          // Trace list - manages internal selection, communicates via callback
          child <-- createTraceListComponent(traceDataVar, selectionVar)
        ),
        // Stack display - receives selection and events as props
        child <-- createStackComponent(traceDataVar, selectionVar)
      )
    )
  
  private def createBytecodeComponent(
    traceDataVar: Var[TraceData],
    selectionVar: Var[SelectionState]
  ): Signal[HtmlElement] =
    traceDataVar.signal.combineWith(selectionVar.signal).map { case (traceData, selection) =>
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
  
  private def createTraceListComponent(
    traceDataVar: Var[TraceData],
    selectionVar: Var[SelectionState]
  ): Signal[HtmlElement] =
    traceDataVar.signal.map { traceData =>
      TraceListComponent(
        TraceListProps(
          traceData = traceData,
          onSelectionChange = (newSelection: SelectionState) => {
            selectionVar.set(newSelection)
          }
        )
      )
    }
  
  private def createStackComponent(
    traceDataVar: Var[TraceData],
    selectionVar: Var[SelectionState]
  ): Signal[HtmlElement] =
    selectionVar.signal.combineWith(traceDataVar.signal).map { case (selection, traceData) =>
      StackComponent(
        StackProps(
          selection = selection,
          events = traceData.events
        )
      )
    }
  
  private def runTrace(
    editorState: EditorState,
    traceDataVar: Var[TraceData],
    editorStateVar: Var[EditorState],
    selectionVar: Var[SelectionState]
  ): Unit =
    val runningState = editorState.copy(isRunning = true, error = None)
    editorStateVar.set(runningState)
    
    TraceApiClient.fetchTrace("/trace", editorState, {
      case Right(newTraceData) =>
        traceDataVar.set(newTraceData)
        editorStateVar.set(editorState.copy(isRunning = false))
        // Reset selection when new data arrives
        selectionVar.set(SelectionState.empty)
      case Left(error) =>
        val errorState = editorState.copy(isRunning = false, error = Some(error))
        editorStateVar.set(errorState)
    })
