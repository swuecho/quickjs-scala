package quickjs.web

import org.scalajs.dom
import org.scalajs.dom.{document, html}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Thenable.Implicits.*

object TraceApp:
  def main(args: Array[String]): Unit =
    val root = document.getElementById("app")
    if root == null then return

    val app = div("app")
    root.appendChild(app)

    val header = buildHeader()
    val sourcePanel = buildSourcePanel()
    val grid = buildGrid()

    app.appendChild(header.container)
    app.appendChild(sourcePanel.container)
    app.appendChild(grid.container)

    wireActions(header, sourcePanel, grid)

  private def buildHeader(): Header =
    val container = document.createElement("header").asInstanceOf[html.Element]
    container.classList.add("hero")

    val left = div("")
    left.appendChild(span("eyebrow", "QuickJS-Scala Educational Platform"))
    val title = document.createElement("h1").asInstanceOf[html.Element]
    title.textContent = "Execution Trace Studio"
    left.appendChild(title)
    val blurb = document.createElement("p").asInstanceOf[html.Element]
    blurb.textContent =
      "Paste JavaScript, run it against the QuickJS-Scala trace server, and inspect every instruction, stack shape, and call/return event."
    left.appendChild(blurb)

    val status = div("status-card")
    val label = div("label")
    label.textContent = "Trace Server"
    val row = div("row")
    val endpoint = document.createElement("input").asInstanceOf[html.Input]
    endpoint.value = "http://localhost:8080/trace"
    endpoint.`type` = "text"
    val ping = button("Ping")
    row.appendChild(endpoint)
    row.appendChild(ping)
    val pingResult = div("subtle")
    pingResult.textContent = "POST /trace"

    status.appendChild(label)
    status.appendChild(row)
    status.appendChild(pingResult)

    container.appendChild(left)
    container.appendChild(status)

    Header(container, endpoint, ping, pingResult)

  private def buildSourcePanel(): SourcePanel =
    val container = div("panel")
    val header = div("panel-header")
    val title = div("panel-title")
    title.textContent = "Source"
    val controls = div("controls")
    val toggleLabel = document.createElement("label").asInstanceOf[html.Label]
    toggleLabel.classList.add("toggle")
    val repl = document.createElement("input").asInstanceOf[html.Input]
    repl.`type` = "checkbox"
    val replText = document.createElement("span").asInstanceOf[html.Element]
    replText.textContent = "REPL mode"
    toggleLabel.appendChild(repl)
    toggleLabel.appendChild(replText)
    val run = button("Run Trace")
    controls.appendChild(toggleLabel)
    controls.appendChild(run)

    header.appendChild(title)
    header.appendChild(controls)

    val textarea = document.createElement("textarea").asInstanceOf[html.TextArea]
    textarea.spellcheck = false
    textarea.value =
      """// Try it:
        |const add = (a, b) => a + b;
        |let total = 0;
        |for (let i = 0; i < 3; i++) {
        |  total = add(total, i);
        |}
        |total;""".stripMargin

    val error = div("error hidden")

    container.appendChild(header)
    container.appendChild(textarea)
    container.appendChild(error)

    SourcePanel(container, textarea, repl, run, error)

  private def buildGrid(): Grid =
    val container = div("grid")

    val eventsPanel = div("panel")
    val eventsHeader = div("panel-header")
    val eventsTitle = div("panel-title")
    eventsTitle.textContent = "Trace Events"
    val meta = div("meta")
    meta.textContent = "No trace yet"
    eventsHeader.appendChild(eventsTitle)
    eventsHeader.appendChild(meta)
    val events = div("events")
    eventsPanel.appendChild(eventsHeader)
    eventsPanel.appendChild(events)

    val detailsPanel = div("panel details")
    val detailsHeader = div("panel-header")
    val detailsTitle = div("panel-title")
    detailsTitle.textContent = "Event Details"
    detailsHeader.appendChild(detailsTitle)
    val details = document.createElement("pre").asInstanceOf[html.Element]
    details.id = "details"
    details.textContent = "Select a trace event to inspect."
    detailsPanel.appendChild(detailsHeader)
    detailsPanel.appendChild(details)

    container.appendChild(eventsPanel)
    container.appendChild(detailsPanel)

    Grid(container, events, details, meta)

  private def wireActions(header: Header, source: SourcePanel, grid: Grid): Unit =
    header.pingButton.addEventListener("click", (_: dom.Event) => pingServer(header))
    source.runButton.addEventListener("click", (_: dom.Event) => fetchTrace(header, source, grid))

  private def pingServer(header: Header): Unit =
    val endpoint = header.endpoint.value.trim
    if endpoint.isEmpty then
      header.pingResult.textContent = "Missing endpoint"
      return
    header.pingResult.textContent = "Checking..."
    dom.fetch(endpoint, new dom.RequestInit { method = dom.HttpMethod.OPTIONS }).toFuture.map { response =>
      header.pingResult.textContent = if response.ok then "Server ready" else "Server not responding"
    }.recover { case _ =>
      header.pingResult.textContent = "Server unreachable"
    }

  private def fetchTrace(header: Header, source: SourcePanel, grid: Grid): Unit =
    val endpoint = header.endpoint.value.trim
    val sourceBody = source.textarea.value
    val replFlag = if source.replMode.checked then "?repl=1" else ""
    if endpoint.isEmpty then
      setError(source, "Trace endpoint is required.")
      return

    setError(source, "")
    source.runButton.disabled = true
    source.runButton.textContent = "Running..."

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
        val payload = JSON.parse(text).asInstanceOf[js.Dynamic]
        if !response.ok || js.typeOf(payload.error) != "undefined" then
          val message =
            if js.typeOf(payload.error) != "undefined" then payload.error.toString
            else "Trace server error."
          setError(source, message)
          setMeta(grid, None)
          renderEvents(grid, js.Array())
        else
          setError(source, "")
          val meta = TraceMeta(
            bytecodeLength = payload.bytecodeLength.asInstanceOf[Int],
            constantsCount = payload.constantsCount.asInstanceOf[Int],
            functionName = payload.functionName.asInstanceOf[String]
          )
          setMeta(grid, Some(meta))
          val trace = payload.trace.asInstanceOf[js.Array[js.Dynamic]]
          renderEvents(grid, trace)
      }
      .recover { case e =>
        setError(source, s"Failed to reach trace server: ${e.getMessage}")
        setMeta(grid, None)
        renderEvents(grid, js.Array())
      }
      .foreach { _ =>
        source.runButton.disabled = false
        source.runButton.textContent = "Run Trace"
      }

  private def renderEvents(grid: Grid, trace: js.Array[js.Dynamic]): Unit =
    grid.events.innerHTML = ""
    grid.details.textContent =
      if trace.isEmpty then "No trace events to display." else "Select a trace event to inspect."

    trace.zipWithIndex.foreach { case (event, idx) =>
      val card = div("event")
      card.setAttribute("data-index", idx.toString)
      val kind = div("kind")
      kind.textContent = event.`type`.toString
      val title = div("title")
      val eventType = event.`type`.toString
      val summary =
        eventType match
          case "instruction" =>
            val location =
              if js.typeOf(event.location) != "undefined" && event.location != null then
                val line = event.location.line.asInstanceOf[Int]
                val column = event.location.column.asInstanceOf[Int]
                s"@$line:$column"
              else ""
            s"pc ${event.pc} · ${event.opcode} $location"
          case "call" =>
            s"call ${event.functionName}"
          case "return" =>
            s"return ${event.functionName}"
          case _ =>
            "event"
      title.textContent = summary.trim

      card.appendChild(kind)
      card.appendChild(title)
      card.addEventListener("click", (_: dom.Event) => selectEvent(grid, card, event))
      grid.events.appendChild(card)
    }

    if trace.nonEmpty then
      val first = grid.events.querySelector(".event").asInstanceOf[html.Element | Null]
      if first != null then selectEvent(grid, first, trace(0))

  private def selectEvent(grid: Grid, card: html.Element, event: js.Dynamic): Unit =
    val selected = grid.events.querySelector(".event.selected")
    if selected != null then selected.classList.remove("selected")
    card.classList.add("selected")
    grid.details.textContent = JSON.stringify(event, space = 2)

  private def setError(panel: SourcePanel, message: String): Unit =
    if message.isEmpty then
      panel.error.classList.add("hidden")
      panel.error.textContent = ""
    else
      panel.error.textContent = message
      panel.error.classList.remove("hidden")

  private def setMeta(grid: Grid, meta: Option[TraceMeta]): Unit =
    meta match
      case Some(data) =>
        val name = if data.functionName.nonEmpty then data.functionName else "<script>"
        grid.meta.textContent = s"bytecode ${data.bytecodeLength} bytes, constants ${data.constantsCount}, function $name"
      case None =>
        grid.meta.textContent = "No trace yet"

  private def div(className: String): html.Div =
    val element = document.createElement("div").asInstanceOf[html.Div]
    if className.nonEmpty then
      className.split("\\s+").foreach(element.classList.add)
    element

  private def span(className: String, text: String): html.Span =
    val element = document.createElement("span").asInstanceOf[html.Span]
    if className.nonEmpty then
      element.classList.add(className)
    element.textContent = text
    element

  private def button(label: String): html.Button =
    val element = document.createElement("button").asInstanceOf[html.Button]
    element.textContent = label
    element

  private final case class Header(
    container: html.Element,
    endpoint: html.Input,
    pingButton: html.Button,
    pingResult: html.Element
  )

  private final case class SourcePanel(
    container: html.Element,
    textarea: html.TextArea,
    replMode: html.Input,
    runButton: html.Button,
    error: html.Element
  )

  private final case class Grid(
    container: html.Element,
    events: html.Element,
    details: html.Element,
    meta: html.Element
  )

  private final case class TraceMeta(
    bytecodeLength: Int,
    constantsCount: Int,
    functionName: String
  )
