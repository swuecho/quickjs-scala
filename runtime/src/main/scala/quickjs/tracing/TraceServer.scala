package quickjs.tracing

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets
import java.net.InetSocketAddress
import quickjs.runtime.{JSContext, JSRuntime}

object TraceServer:
  def main(args: Array[String]): Unit =
    val port = args.headOption.flatMap(toInt).getOrElse(8125)
    val server = HttpServer.create(InetSocketAddress(port), 0)
    val staticRoot = Paths.get("web").toAbsolutePath.normalize
    server.createContext("/", StaticHandler(staticRoot))
    server.createContext("/trace", TraceHandler())
    server.setExecutor(null)
    println(s"Trace server listening on http://localhost:$port/trace")
    server.start()

  private def toInt(value: String): Option[Int] =
    try Some(value.toInt)
    catch case _: NumberFormatException => None

  def buildResponse(result: TraceResult): String =
    val sb = StringBuilder()
    sb.append('{')
    JsonUtil.appendString(sb, "trace")
    sb.append(':')
    sb.append(result.json)
    sb.append(',')
    JsonUtil.appendString(sb, "instructions")
    sb.append(':')
    sb.append(BytecodeJson.instructionsToJson(result.instructions))
    sb.append(',')
    appendField(sb, "bytecodeHex", result.bytecodeHex)
    sb.append(',')
    appendField(sb, "bytecodeLength", result.bytecodeLength)
    sb.append(',')
    appendField(sb, "constantsCount", result.constantsCount)
    sb.append(',')
    appendField(sb, "functionName", result.functionName)
    sb.append('}')
    sb.toString

  private def appendField(sb: StringBuilder, key: String, value: String): Unit =
    JsonUtil.appendString(sb, key)
    sb.append(':')
    JsonUtil.appendString(sb, value)

  private def appendField(sb: StringBuilder, key: String, value: Int): Unit =
    JsonUtil.appendString(sb, key)
    sb.append(':')
    sb.append(value)

  def buildError(message: String): String =
    val sb = StringBuilder()
    sb.append('{')
    JsonUtil.appendString(sb, "error")
    sb.append(':')
    JsonUtil.appendString(sb, message)
    sb.append('}')
    sb.toString

  def buildErrorWithStack(message: String, stack: String): String =
    val sb = StringBuilder()
    sb.append('{')
    JsonUtil.appendString(sb, "error")
    sb.append(':')
    JsonUtil.appendString(sb, message)
    sb.append(',')
    JsonUtil.appendString(sb, "stack")
    sb.append(':')
    JsonUtil.appendString(sb, stack)
    sb.append('}')
    sb.toString

final class TraceHandler extends HttpHandler:
  def handle(exchange: HttpExchange): Unit =
    try
      if exchange.getRequestMethod == "OPTIONS" then
        send(exchange, 204, "text/plain", "")
      else if exchange.getRequestMethod != "POST" then
        send(exchange, 405, "text/plain", "Method Not Allowed")
      else
        val replMode = hasReplFlag(exchange.getRequestURI.getQuery)
        val body = new String(
          exchange.getRequestBody.readAllBytes(),
          StandardCharsets.UTF_8
        )
        if body.trim.isEmpty then
          val response = TraceServer.buildError("Request body is empty.")
          send(exchange, 400, "application/json", response)
        else
          given JSRuntime = JSRuntime()
          given JSContext = JSContext(summon[JSRuntime])
          try
            val result = TraceSession.run(body, replMode)
            val response = TraceServer.buildResponse(result)
            send(exchange, 200, "application/json", response)
          catch
            case e: quickjs.runtime.JSException =>
              val (message, stack) = extractError(e.getValue)
              val response = TraceServer.buildErrorWithStack(message, stack)
              send(exchange, 400, "application/json", response)
    catch
      case e: Throwable =>
        e.printStackTrace()
        val message = Option(e.getMessage).getOrElse(e.toString)
        val response = TraceServer.buildError(message)
        send(exchange, 500, "application/json", response)
    finally exchange.close()

  private def hasReplFlag(query: String | Null): Boolean =
    if query == null || query.isEmpty then false
    else
      query.split("&").exists { part =>
        part == "repl=1" || part == "repl=true"
      }

  private def extractError(value: quickjs.value.JSValue)(using
      ctx: JSContext
  ): (String, String) =
    value match
      case quickjs.value.JSValue.Object(obj) =>
        val message =
          obj.get("message") match
            case quickjs.value.JSValue.JSStr(s) => s
            case _                              => value.toString
        val stack =
          obj.get("stack") match
            case quickjs.value.JSValue.JSStr(s) => s
            case _                              => ctx.formatStackTrace()
        (message, stack)
      case _ =>
        (value.toString, ctx.formatStackTrace())

  private def send(
      exchange: HttpExchange,
      status: Int,
      contentType: String,
      body: String
  ): Unit =
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.set("Content-Type", contentType)
    exchange.getResponseHeaders.set("Access-Control-Allow-Origin", "*")
    exchange.getResponseHeaders.set(
      "Access-Control-Allow-Headers",
      "Content-Type"
    )
    exchange.getResponseHeaders.set(
      "Access-Control-Allow-Methods",
      "POST, OPTIONS"
    )
    exchange.sendResponseHeaders(status, bytes.length.toLong)
    val os = exchange.getResponseBody
    try os.write(bytes)
    finally os.close()

final class StaticHandler(root: Path) extends HttpHandler:
  def handle(exchange: HttpExchange): Unit =
    try
      exchange.getRequestMethod match
        case "GET" | "HEAD" =>
          val requested = sanitizePath(exchange.getRequestURI.getPath)
          val resolved = root.resolve(requested).normalize
          if !resolved.startsWith(root) then
            sendStatus(exchange, 403, "Forbidden")
          else if Files.isDirectory(resolved) then
            sendFile(
              exchange,
              resolved.resolve("index.html"),
              isHead = exchange.getRequestMethod == "HEAD"
            )
          else
            sendFile(
              exchange,
              resolved,
              isHead = exchange.getRequestMethod == "HEAD"
            )
        case _ =>
          sendStatus(exchange, 405, "Method Not Allowed")
    finally
      exchange.close()

  private def sanitizePath(path: String): Path =
    val clean =
      if path == null || path.isEmpty || path == "/" then "index.html"
      else if path.startsWith("/") then path.drop(1)
      else path
    Paths.get(clean)

  private def sendFile(
      exchange: HttpExchange,
      path: Path,
      isHead: Boolean
  ): Unit =
    if !Files.exists(path) || Files.isDirectory(path) then
      sendStatus(exchange, 404, "Not Found")
    else
      val bytes = Files.readAllBytes(path)
      val contentType = contentTypeFor(path)
      exchange.getResponseHeaders.set("Content-Type", contentType)
      exchange.sendResponseHeaders(200, bytes.length.toLong)
      if !isHead then
        val os = exchange.getResponseBody
        try os.write(bytes)
        finally os.close()

  private def sendStatus(
      exchange: HttpExchange,
      status: Int,
      message: String
  ): Unit =
    val bytes = message.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.set("Content-Type", "text/plain")
    exchange.sendResponseHeaders(status, bytes.length.toLong)
    val os = exchange.getResponseBody
    try os.write(bytes)
    finally os.close()

  private def contentTypeFor(path: Path): String =
    val name = path.getFileName.toString
    if name.endsWith(".html") then "text/html; charset=utf-8"
    else if name.endsWith(".css") then "text/css; charset=utf-8"
    else if name.endsWith(".js") then "application/javascript"
    else if name.endsWith(".map") then "application/json"
    else "application/octet-stream"
