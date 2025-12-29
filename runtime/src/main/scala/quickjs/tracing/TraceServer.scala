package quickjs.tracing

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import quickjs.runtime.{JSContext, JSRuntime}

object TraceServer:
  def main(args: Array[String]): Unit =
    val port = args.headOption.flatMap(toInt).getOrElse(8080)
    val server = HttpServer.create(InetSocketAddress(port), 0)
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

final class TraceHandler extends HttpHandler:
  def handle(exchange: HttpExchange): Unit =
    try
      if exchange.getRequestMethod == "OPTIONS" then
        send(exchange, 204, "text/plain", "")
      else if exchange.getRequestMethod != "POST" then
        send(exchange, 405, "text/plain", "Method Not Allowed")
      else
        val replMode = hasReplFlag(exchange.getRequestURI.getQuery)
        val body = new String(exchange.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
        if body.trim.isEmpty then
          val response = TraceServer.buildError("Request body is empty.")
          send(exchange, 400, "application/json", response)
        else
          given JSRuntime = JSRuntime()
          given JSContext = JSContext(summon[JSRuntime])
          val result = TraceSession.run(body, replMode)
          val response = TraceServer.buildResponse(result)
          send(exchange, 200, "application/json", response)
    catch
      case e: Throwable =>
        val response = TraceServer.buildError(e.getMessage)
        send(exchange, 500, "application/json", response)
    finally
      exchange.close()

  private def hasReplFlag(query: String | Null): Boolean =
    if query == null || query.isEmpty then false
    else
      query.split("&").exists { part =>
        part == "repl=1" || part == "repl=true"
      }

  private def send(exchange: HttpExchange, status: Int, contentType: String, body: String): Unit =
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.set("Content-Type", contentType)
    exchange.getResponseHeaders.set("Access-Control-Allow-Origin", "*")
    exchange.getResponseHeaders.set("Access-Control-Allow-Headers", "Content-Type")
    exchange.getResponseHeaders.set("Access-Control-Allow-Methods", "POST, OPTIONS")
    exchange.sendResponseHeaders(status, bytes.length.toLong)
    val os = exchange.getResponseBody
    try os.write(bytes)
    finally os.close()
