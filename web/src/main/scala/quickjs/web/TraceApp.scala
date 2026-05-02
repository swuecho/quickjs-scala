package quickjs.web

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import quickjs.web.components.*
import quickjs.web.features.editor.EditorFeature
import quickjs.web.features.stack.StackFeature
import quickjs.web.features.trace.TraceFeature
import quickjs.web.state.{AppAction, Store}

object TraceApp {
  private val traceEndpoint = "/trace"

  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(
      dom.document.getElementById("app"),
      appView()
    )

  private def appView(): HtmlElement = {
    val store = Store(traceEndpoint)
    val editorSignal = store.state.map(_.editor)
    val traceSignal = store.state.map(_.trace)
    val stackSignal = store.state.map(_.stack)

    div(
      cls := "app",
      StatusIndicatorComponent(
        StatusIndicatorProps(
          endpoint = traceEndpoint,
          onStatusChange = Observer[String](_ => ())
        )
      ),
      HeaderComponent(),
      EditorFeature.view(
        state = editorSignal,
        dispatch = store.actions.contramap(AppAction.Editor(_)),
        onRun = Observer[Unit](_ => store.actions.onNext(AppAction.RunTrace))
      ),
      div(
        cls := "grid",
        div(
          cls := "column",
          TraceFeature.view(
            state = traceSignal,
            dispatch = store.actions.contramap(AppAction.Trace(_))
          )
        ),
        StackFeature.view(stackSignal)
      )
    )
  }
}
