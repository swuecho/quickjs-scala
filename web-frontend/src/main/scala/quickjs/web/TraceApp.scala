package quickjs.web

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import quickjs.web.components.*
import quickjs.web.domain.TraceDomain
import quickjs.web.models.{TraceData, SelectionState}
import quickjs.web.state.{Action, Store}

object TraceApp:
  private val traceEndpoint = "/trace"

  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(
      dom.document.getElementById("app"),
      appView()
    )

  private def appView(): HtmlElement =
    val store = Store(traceEndpoint)
    val editorSignal = store.state.map(_.editor)
    val traceDataSignal = store.state.map(_.traceData)
    val selectionSignal = store.state.map(_.selection)
    
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
        onSourceChange = store.actions.contramap(Action.UpdateSource(_)),
        onReplToggle = store.actions.contramap(Action.SetReplMode(_)),
        onRun = Observer[Unit](_ => store.actions.onNext(Action.RunTrace))
      ),
      
      div(
        cls := "grid",
        div(
          cls := "column",
          child <-- createBytecodeComponent(traceDataSignal, selectionSignal),
          TraceListComponent(
            traceData = traceDataSignal,
            selection = selectionSignal,
            onSelectIndex = store.actions.contramap(Action.SelectIndex(_)),
            onStepPrev = Observer[Unit](_ => store.actions.onNext(Action.StepPrev)),
            onStepNext = Observer[Unit](_ => store.actions.onNext(Action.StepNext))
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
      val selectedPc = TraceDomain.selectedPc(traceData, selection)
      
      BytecodeComponent(
        BytecodeProps(
          instructions = traceData.instructions,
          bytecode = traceData.bytecode,
          selectedPc = selectedPc
        )
      )
    }
  
