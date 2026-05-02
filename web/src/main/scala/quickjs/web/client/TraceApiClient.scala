package quickjs.web.client

import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Thenable.Implicits.*
import quickjs.web.models.{TraceResponse, TraceData, EditorState}

type TraceCallback = Either[String, TraceData] => Unit

object TraceApiClient {
  def fetchTrace(
      endpoint: String,
      editorState: EditorState,
      callback: TraceCallback
  ): Unit = {
    val replFlag = if editorState.replMode then "?repl=1" else ""

    if endpoint.trim.isEmpty then {
      callback(Left("Trace endpoint is required."))
      return
    }

    val init = new dom.RequestInit {
      method = dom.HttpMethod.POST
      headers = js.Dictionary("Content-Type" -> "text/plain")
      body = editorState.source
    }

    dom
      .fetch(endpoint + replFlag, init)
      .toFuture
      .flatMap { response =>
        response.text().toFuture.map(text => (response, text))
      }
      .map { case (response, text) =>
        processResponse(response, text.trim, callback)
      }
      .recover { case e =>
        callback(Left(s"Failed to reach trace server: ${e.getMessage}"))
      }
  }

  private def processResponse(
      response: dom.Response,
      text: String,
      callback: TraceCallback
  ): Unit =
    if text.isEmpty then
      callback(
        Left(s"Empty response from trace server (status ${response.status}).")
      )
    else
      try {
        val payload = JSON.parse(text).asInstanceOf[js.Dynamic]
        val traceResponse = TraceResponse.fromDynamic(payload)

        if !response.ok || traceResponse.error.isDefined then {
          val errorMessage = traceResponse.error.getOrElse(
            s"Trace server error (status ${response.status})."
          )
          val fullError = traceResponse.stack match {
            case Some(stack) if stack.nonEmpty => s"$errorMessage\n$stack"
            case _                             => errorMessage
          }
          callback(Left(fullError))
        } else {
          val traceData = TraceData(
            meta = Some(
              quickjs.web.models.TraceMeta(
                bytecodeLength = traceResponse.bytecodeLength,
                constantsCount = traceResponse.constantsCount,
                functionName = traceResponse.functionName
              )
            ),
            events = traceResponse.trace,
            bytecode = parseHex(traceResponse.bytecodeHex),
            instructions = traceResponse.instructions
          )
          callback(Right(traceData))
        }
      } catch {
        case e: Throwable =>
          callback(Left(s"Invalid JSON from trace server: ${e.getMessage}"))
      }

  private def parseHex(hex: String): Vector[String] =
    hex.split("\\s+").toVector.filter(_.nonEmpty)
}
