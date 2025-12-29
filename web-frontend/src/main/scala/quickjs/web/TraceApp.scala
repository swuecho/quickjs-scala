package quickjs.web

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Thenable.Implicits.*

object TraceApp:
  private val endpointVar = Var("/trace")
  private val replModeVar = Var(false)
  private val sourceVar = Var(
    """// Try it:
      |const add = (a, b) => a + b;
      |let total = 0;
      |for (let i = 0; i < 3; i++) {
      |  total = add(total, i);
      |}
      |total;""".stripMargin
  )
  private val errorVar = Var(Option.empty[String])
  private val pingStatusVar = Var("unknown")
  private val metaVar = Var(Option.empty[TraceMeta])
  private val eventsVar = Var(js.Array[js.Dynamic]())
  private val bytecodeVar = Var(Vector.empty[String])
  private val instructionsVar = Var(js.Array[js.Dynamic]())
  private val selectedIndexVar = Var(Option.empty[Int])
  private val lastStackDepthVar = Var(0)
  private val stackDeltaVar = Var("none")
  private val runningVar = Var(false)

  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(
      dom.document.getElementById("app"),
      appView()
    )

  private def appView(): HtmlElement =
    div(
      cls := "app",
      onMountCallback { ctx =>
        pingServer()
        selectedIndexVar.signal
          .combineWith(eventsVar.signal)
          .foreach { case (idxOpt, events) =>
            val depth = currentStackDepth(idxOpt, events)
            if depth >= 0 then
              val previous = lastStackDepthVar.now()
              if depth > previous then stackDeltaVar.set("push")
              else if depth < previous then stackDeltaVar.set("pop")
              else stackDeltaVar.set("same")
              lastStackDepthVar.set(depth)
            else
              stackDeltaVar.set("none")
              lastStackDepthVar.set(0)
          }(ctx.owner)
      },
      div(
        cls := "status-indicator",
        cls.toggle("ok") <-- pingStatusVar.signal.map(_ == "ok"),
        cls.toggle("warn") <-- pingStatusVar.signal.map(_ == "fail"),
        cls.toggle("pending") <-- pingStatusVar.signal.map(_ == "checking"),
        title <-- pingStatusVar.signal.map {
          case "ok" => "Trace server reachable"
          case "checking" => "Checking trace server"
          case "fail" => "Trace server unreachable"
          case _ => "Trace server status unknown"
        }
      ),
      headerView(),
      sourcePanelView(),
      gridView()
    )

  private def headerView(): HtmlElement =
    div(
      cls := "hero",
      div(
        span(cls := "eyebrow", "QuickJS-Scala Educational Platform"),
        h1("Execution Trace Studio"),
        p(
          "Paste JavaScript, run it against the QuickJS-Scala trace server, and inspect every instruction, stack shape, and call/return event."
        )
      )
    )

  private def sourcePanelView(): HtmlElement =
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
                checked <-- replModeVar.signal,
                onClick.mapToChecked --> replModeVar.writer
              )
            ),
            span("REPL mode")
          ),
          button(
            child.text <-- runningVar.signal.map(running => if running then "Running..." else "Run Trace"),
            disabled <-- runningVar.signal,
            onClick --> { _ => fetchTrace() }
          )
        )
      ),
      textArea(
        spellCheck := false,
        controlled(
          value <-- sourceVar.signal,
          onInput.mapToValue --> sourceVar.writer
        )
      ),
      div(
        cls := "error",
        cls.toggle("hidden") <-- errorVar.signal.map(_.isEmpty),
        child.text <-- errorVar.signal.map(_.getOrElse(""))
      )
    )

  private def gridView(): HtmlElement =
    val selectedPcSignal = selectedIndexVar.signal.combineWith(eventsVar.signal).map {
      case (Some(idx), events) if idx >= 0 && idx < events.length =>
        val event = events(idx)
        if event.`type`.toString == "instruction" then
          Some(event.pc.asInstanceOf[Int])
        else
          None
      case _ => None
    }

    div(
      cls := "grid",
      div(
        cls := "column",
        div(
          cls := "panel bytecode-panel",
          div(
            cls := "panel-header",
            div(cls := "panel-title", "Bytecode"),
            div(
              cls := "meta",
              child.text <-- bytecodeVar.signal.map { bytes =>
                if bytes.isEmpty then "No bytecode yet" else s"${bytes.length} bytes"
              }
            )
          ),
          div(
            cls := "instructions",
            children <-- instructionsVar.signal.map { instructions =>
              instructions.zipWithIndex.map { case (inst, _) =>
                div(
                  cls := "instruction",
                  cls.toggle("highlight") <-- selectedPcSignal.map(_.contains(inst.pc.asInstanceOf[Int])),
                  span(cls := "instr-pc", f"${inst.pc.asInstanceOf[Int]}%03d"),
                  span(cls := "instr-op", inst.opcode.toString),
                  span(cls := "instr-opnd", inst.operand.asInstanceOf[js.UndefOr[String]].getOrElse(""))
                )
              }.toSeq
            }
          ),
          div(
            cls := "bytecode",
            children <-- bytecodeVar.signal.map { bytes =>
              bytes.zipWithIndex.map { case (value, idx) =>
                div(
                  cls := "byte",
                  cls.toggle("highlight") <-- selectedPcSignal.map(_.contains(idx)),
                  span(cls := "byte-idx", f"$idx%03d"),
                  span(cls := "byte-val", value)
                )
              }.toSeq
            }
          )
        ),
        div(
          cls := "panel",
          div(
            cls := "panel-header",
            div(cls := "panel-title", "Trace Events"),
            div(
              cls := "stepper",
              button(
                "Prev",
                disabled <-- selectedIndexVar.signal.map(_.forall(_ <= 0)),
                onClick --> { _ => stepSelection(-1) }
              ),
              button(
                "Next",
                disabled <-- selectedIndexVar.signal.combineWith(eventsVar.signal).map {
                  case (Some(idx), events) => idx >= events.length - 1
                  case _ => true
                },
                onClick --> { _ => stepSelection(1) }
              )
            ),
            div(
              cls := "meta",
              child.text <-- metaVar.signal.map(formatMeta)
            )
          ),
          div(
            cls := "events",
            children <-- eventsVar.signal.map { events =>
              events.zipWithIndex.map { case (event, idx) => eventCard(event, idx) }.toSeq
            }
          )
        )
      ),
      div(
        cls := "panel details",
        div(
          cls := "panel-header",
          div(cls := "panel-title", "Runtime Stack")
        ),
        div(
          cls := "stack",
          cls.toggle("stack-push") <-- stackDeltaVar.signal.map(_ == "push"),
          cls.toggle("stack-pop") <-- stackDeltaVar.signal.map(_ == "pop"),
          children <-- selectedIndexVar.signal.combineWith(eventsVar.signal).map {
            case (Some(idx), events) if idx >= 0 && idx < events.length =>
              val event = events(idx)
              if event.`type`.toString == "instruction" && js.typeOf(event.stack) != "undefined" then
                val stack = event.stack.asInstanceOf[js.Array[js.Dynamic]]
                if stack.isEmpty then
                  Seq(div(cls := "stack-empty", "Stack is empty"))
                else
                  val topIndex = stack.length - 1
                  (topIndex to 0 by -1).map { i =>
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
              else
                Seq(div(cls := "stack-empty", "Select an instruction event to see stack state."))
            case _ =>
              Seq(div(cls := "stack-empty", "Select an instruction event to see stack state."))
          }
        ),
        div(
          cls := "panel-header",
          div(cls := "panel-title", "Event Details")
        ),
        pre(
          idAttr := "details",
          child.text <-- selectedIndexVar.signal.combineWith(eventsVar.signal).map {
            case (Some(idx), events) if idx >= 0 && idx < events.length =>
              JSON.stringify(events(idx), space = 2)
            case _ => "Select a trace event to inspect."
          }
        )
      )
    )

  private def eventCard(event: js.Dynamic, index: Int): HtmlElement =
    div(
      cls := "event",
      cls.toggle("selected") <-- selectedIndexVar.signal.map(_.contains(index)),
      onClick --> { _ => selectedIndexVar.set(Some(index)) },
      div(
        cls := "kind",
        event.`type`.toString
      ),
      div(
        cls := "title",
        eventSummary(event)
      )
    )

  private def eventSummary(event: js.Dynamic): String =
    val eventType = event.`type`.toString
    eventType match
      case "instruction" =>
        val location =
          if js.typeOf(event.location) != "undefined" && event.location != null then
            val line = event.location.line.asInstanceOf[Int]
            val column = event.location.column.asInstanceOf[Int]
            s"@$line:$column"
          else ""
        s"pc ${event.pc} · ${event.opcode} $location".trim
      case "call" =>
        s"call ${event.functionName}"
      case "return" =>
        s"return ${event.functionName}"
      case _ =>
        "event"

  private def pingServer(): Unit =
    val endpoint = endpointVar.now().trim
    if endpoint.isEmpty then
      pingStatusVar.set("fail")
    else
      pingStatusVar.set("checking")
      dom.fetch(endpoint, new dom.RequestInit { method = dom.HttpMethod.OPTIONS }).toFuture.map { response =>
        pingStatusVar.set(if response.ok then "ok" else "fail")
      }.recover { case _ =>
        pingStatusVar.set("fail")
      }

  private def fetchTrace(): Unit =
    val endpoint = endpointVar.now().trim
    val sourceBody = sourceVar.now()
    val replFlag = if replModeVar.now() then "?repl=1" else ""
    if endpoint.isEmpty then
      errorVar.set(Some("Trace endpoint is required."))
      return

    errorVar.set(None)
    runningVar.set(true)

    val init = new dom.RequestInit {
      method = dom.HttpMethod.POST
      headers = js.Dictionary("Content-Type" -> "text/plain")
      body = sourceBody
    }

    dom.fetch(endpoint + replFlag, init).toFuture
      .flatMap { response =>
        response.text().toFuture.map(text => (response, text))
      }
      .map { case (response, text) =>
        val trimmed = text.trim
        if trimmed.isEmpty then
          errorVar.set(Some(s"Empty response from trace server (status ${response.status})."))
          metaVar.set(None)
          eventsVar.set(js.Array())
          bytecodeVar.set(Vector.empty)
          instructionsVar.set(js.Array())
          selectedIndexVar.set(None)
        else
          try
            val payload = JSON.parse(trimmed).asInstanceOf[js.Dynamic]
            if !response.ok || js.typeOf(payload.error) != "undefined" then
              val message =
                if js.typeOf(payload.error) != "undefined" then payload.error.toString
                else s"Trace server error (status ${response.status})."
              val stack =
                if js.typeOf(payload.stack) != "undefined" then payload.stack.toString else ""
              val combined =
                if stack.nonEmpty then s"$message\n$stack" else message
              errorVar.set(Some(combined))
              metaVar.set(None)
              eventsVar.set(js.Array())
              bytecodeVar.set(Vector.empty)
              instructionsVar.set(js.Array())
              selectedIndexVar.set(None)
            else
              errorVar.set(None)
              val meta = TraceMeta(
                bytecodeLength = payload.bytecodeLength.asInstanceOf[Int],
                constantsCount = payload.constantsCount.asInstanceOf[Int],
                functionName = payload.functionName.asInstanceOf[String]
              )
              metaVar.set(Some(meta))
              val trace = payload.trace.asInstanceOf[js.Array[js.Dynamic]]
              eventsVar.set(trace)
              bytecodeVar.set(parseHex(payload.bytecodeHex.asInstanceOf[String]))
              instructionsVar.set(payload.instructions.asInstanceOf[js.Array[js.Dynamic]])
              if trace.nonEmpty then
                selectedIndexVar.set(Some(0))
              else
                selectedIndexVar.set(None)
          catch
            case e: Throwable =>
              errorVar.set(Some(s"Invalid JSON from trace server: ${e.getMessage}"))
              metaVar.set(None)
              eventsVar.set(js.Array())
              bytecodeVar.set(Vector.empty)
              instructionsVar.set(js.Array())
              selectedIndexVar.set(None)
      }
      .recover { case e =>
        errorVar.set(Some(s"Failed to reach trace server: ${e.getMessage}"))
        metaVar.set(None)
        eventsVar.set(js.Array())
        bytecodeVar.set(Vector.empty)
        instructionsVar.set(js.Array())
        selectedIndexVar.set(None)
      }
      .foreach { _ =>
        runningVar.set(false)
      }

  private def formatMeta(meta: Option[TraceMeta]): String =
    meta match
      case Some(data) =>
        val name = if data.functionName.nonEmpty then data.functionName else "<script>"
        s"bytecode ${data.bytecodeLength} bytes, constants ${data.constantsCount}, function $name"
      case None =>
        "No trace yet"

  private def parseHex(hex: String): Vector[String] =
    hex.split("\\s+").toVector.filter(_.nonEmpty)

  private def stepSelection(delta: Int): Unit =
    val events = eventsVar.now()
    if events.isEmpty then
      selectedIndexVar.set(None)
    else
      val current = selectedIndexVar.now().getOrElse(0)
      val next = Math.max(0, Math.min(events.length - 1, current + delta))
      selectedIndexVar.set(Some(next))

  private def currentStackDepth(idxOpt: Option[Int], events: js.Array[js.Dynamic]): Int =
    idxOpt match
      case Some(idx) if idx >= 0 && idx < events.length =>
        val event = events(idx)
        if event.`type`.toString == "instruction" && js.typeOf(event.stack) != "undefined" then
          event.stack.asInstanceOf[js.Array[js.Dynamic]].length
        else
          -1
      case _ => -1

  private final case class TraceMeta(
    bytecodeLength: Int,
    constantsCount: Int,
    functionName: String
  )
