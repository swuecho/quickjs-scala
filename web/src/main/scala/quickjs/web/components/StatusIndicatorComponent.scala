package quickjs.web.components

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.Thenable.Implicits.*

case class StatusIndicatorProps(
    endpoint: String,
    onStatusChange: Observer[String]
)

object StatusIndicatorComponent {
  def apply(props: StatusIndicatorProps): HtmlElement = {
    val statusVar = Var("unknown")

    div(
      cls := "status-indicator",
      cls.toggle("ok") <-- statusVar.signal.map(_ == "ok"),
      cls.toggle("warn") <-- statusVar.signal.map(_ == "fail"),
      cls.toggle("pending") <-- statusVar.signal.map(_ == "checking"),
      title <-- statusVar.signal.map(getStatusTitle),
      onMountCallback { _ =>
        checkStatus(props.endpoint, statusVar, props.onStatusChange)
        // Check status periodically when component is mounted
        val interval = dom.window.setInterval(
          () => checkStatus(props.endpoint, statusVar, props.onStatusChange),
          5000
        )
        // Clean up interval when component unmounts
        onUnmountCallback(_ => dom.window.clearInterval(interval))
      }
    )
  }

  private def checkStatus(
      endpoint: String,
      statusVar: Var[String],
      onStatusChange: Observer[String]
  ): Unit =
    if endpoint.trim.isEmpty then {
      statusVar.set("fail")
      onStatusChange.onNext("fail")
    }
    else {
      statusVar.set("checking")
      onStatusChange.onNext("checking")

      dom
        .fetch(
          endpoint,
          new dom.RequestInit { method = dom.HttpMethod.OPTIONS }
        )
        .toFuture
        .map { response =>
          val newStatus = if response.ok then "ok" else "fail"
          statusVar.set(newStatus)
          onStatusChange.onNext(newStatus)
        }
        .recover { case _ =>
          statusVar.set("fail")
          onStatusChange.onNext("fail")
        }
    }

  private def getStatusTitle(status: String): String = status match {
    case "ok"       => "Trace server reachable"
    case "checking" => "Checking trace server"
    case "fail"     => "Trace server unreachable"
    case _          => "Trace server status unknown"
  }
}
