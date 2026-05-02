package quickjs.diagnostic

import quickjs.ast.Span
import quickjs.value.JSValue
import scala.collection.mutable.ArrayBuffer

/** Error with source location information.
  *
  * @param message
  *   Error message
  * @param span
  *   Source location
  * @param errorType
  *   Type of error (syntax, runtime, type, etc.)
  */
case class Error(
    message: String,
    span: Span,
    errorType: ErrorType
)

/** Types of errors */
enum ErrorType {
  case SyntaxError
  case RuntimeError
  case TypeError
  case ReferenceError
  case RangeError
}

/** Stack frame representing a function call.
  *
  * @param functionName
  *   Name of the function
  * @param source
  *   Source file or "<eval>"
  * @param line
  *   Line number
  * @param column
  *   Column number
  */
case class StackFrame(
    functionName: String,
    source: String,
    line: Int,
    column: Int
)

/** Error handler with source location and stack trace support.
  *
  * Provides rich error messages with:
  *   - Source code context
  *   - Line and column numbers
  *   - Caret pointing to error location
  *   - Stack traces with call chain
  */
object ErrorHandler {
  private def extractJsErrorInfo(
      value: JSValue
  ): (String, Option[String], Option[Int], Option[Int]) =
    value match {
      case JSValue.Object(obj) =>
        val props = obj.getAllProperties
        val name = props.get("name").map(_.toString).filter(_.nonEmpty)
        val message = props.get("message").map(_.toString).filter(_.nonEmpty)
        val stack = props.get("stack") match {
          case Some(JSValue.JSStr(s)) if s.nonEmpty => Some(s)
          case _                                    => None
        }
        val lineNumber = props.get("lineNumber") match {
          case Some(JSValue.Int32(i))   => Some(i)
          case Some(JSValue.Float64(d)) => Some(d.toInt)
          case _                        => None
        }
        val columnNumber = props.get("columnNumber") match {
          case Some(JSValue.Int32(i))   => Some(i)
          case Some(JSValue.Float64(d)) => Some(d.toInt)
          case _                        => None
        }
        val header =
          (name, message) match {
            case (Some(n), Some(m)) => s"$n: $m"
            case (Some(n), None)    => n
            case (None, Some(m))    => m
            case _                  => value.toString
          }
        (header, stack, lineNumber, columnNumber)
      case _ =>
        (value.toString, None, None, None)
    }

  private def formatSourceContext(
      sourceName: String,
      sourceText: String,
      lineNumber: Option[Int],
      columnNumber: Option[Int]
  ): String =
    if sourceText.isEmpty then ""
    else {
      val lines = sourceText.split("\n", -1)
      val sb = StringBuilder()
      sb.append(s"  \u001B[90m// where: $sourceName\u001B[0m\n")
      lineNumber match {
        case Some(line1Based)
            if line1Based >= 1 && line1Based <= lines.length =>
          val lineIndex = line1Based - 1
          val startLine = Math.max(0, lineIndex - 2)
          val endLine = Math.min(lines.length - 1, lineIndex + 2)
          for i <- startLine to endLine do {
            val lineNum = i + 1
            val lineNumStr = lineNum.toString
            val prefix = if i == lineIndex then ">>" else "  "
            sb.append(s"$prefix \u001B[90m$lineNum|\u001B[0m ${lines(i)}\n")
            if i == lineIndex then {
              val colIndex = columnNumber.map(_ - 1).getOrElse(0).max(0)
              val prefixLen = prefix.length + 1 + lineNumStr.length + 1 + 1
              val caretSpaces = " " * (prefixLen + colIndex)
              sb.append(s"$caretSpaces\u001B[31m^\u001B[0m\n")
            }
          }
        case _ =>
          for (line, i) <- lines.take(3).zipWithIndex do
            sb.append(s"  ${i + 1}| $line\n")
      }
      sb.toString()
    }

  /** Format an error with source location.
    *
    * @param source
    *   Source code
    * @param error
    *   Error to format
    * @return
    *   Formatted error message
    */
  def formatError(source: String, error: Error): String = {
    val lines = source.split("\n", -1)
    val errorLine = error.span.line
    val errorCol = error.span.column

    // Get context lines (show 2 lines before and after)
    val startLine = Math.max(0, errorLine - 2)
    val endLine = Math.min(lines.length - 1, errorLine + 2)

    val sb = StringBuilder()

    // Error header with color
    sb.append(
      s"\u001B[31m\u001B[1m${error.errorType}: ${error.message}\u001B[0m\n"
    )
    sb.append(
      s"  \u001B[90m// at line ${errorLine + 1}, column ${errorCol + 1}\u001B[0m\n"
    )

    // Show source context
    for i <- startLine to endLine do {
      val lineNum = i + 1
      val lineNumStr = lineNum.toString
      val prefix = if i == errorLine then ">>" else "  "
      sb.append(s"$prefix \u001B[90m$lineNum|\u001B[0m ${lines(i)}\n")

      // Show caret on error line
      if i == errorLine then {
        val prefixLen = prefix.length + 1 + lineNumStr.length + 1 + 1
        val caretSpaces = " " * (prefixLen + errorCol)
        sb.append(s"$caretSpaces\u001B[31m^\u001B[0m\n")
      }
    }

    sb.toString()
  }

  /** Format an exception without span information.
    *
    * @param sourceName
    *   Source file or "<eval>"
    * @param sourceText
    *   Source code
    * @param ex
    *   Exception to format
    * @return
    *   Formatted error message
    */
  def formatException(
      sourceName: String,
      sourceText: String,
      ex: Throwable
  ): String = {
    val sb = StringBuilder()
    ex match {
      case jsEx: quickjs.runtime.JSException =>
        val (header, stackOpt, lineNumber, columnNumber) = extractJsErrorInfo(
          jsEx.getValue
        )
        val message =
          if header.nonEmpty then header
          else s"JavaScript exception: ${jsEx.getValue}"
        sb.append(s"\u001B[31m\u001B[1m$message\u001B[0m\n")
        sb.append(
          formatSourceContext(sourceName, sourceText, lineNumber, columnNumber)
        )
        stackOpt.foreach { stack =>
          sb.append("\u001B[90m  Stack trace:\u001B[0m\n")
          stack.linesIterator.foreach { line =>
            if line.nonEmpty then sb.append(s"\u001B[90m$line\u001B[0m\n")
          }
        }
      case _ =>
        val errorType = ex match {
          case _: RuntimeException => "RuntimeError"
          case _                   => ex.getClass.getSimpleName
        }
        sb.append(s"\u001B[31m\u001B[1m$errorType: ${ex.getMessage}\u001B[0m\n")
        sb.append(formatSourceContext(sourceName, sourceText, None, None))
    }
    sb.toString()
  }

  /** Backward compatible formatter. */
  def formatException(source: String, ex: Throwable): String =
    formatException("<source>", source, ex)

  /** Format a stack trace.
    *
    * @param frames
    *   Stack frames
    * @return
    *   Formatted stack trace
    */
  def formatStackTrace(frames: Seq[StackFrame]): String =
    if frames.isEmpty then ""
    else {
      val sb = StringBuilder()
      sb.append("\u001B[90m  Stack trace:\u001B[0m\n")

      for (frame, i) <- frames.zipWithIndex do {
        val prefix = if i == 0 then "    at" else "    from"
        sb.append(s"\u001B[90m$prefix\u001B[0m ${frame.functionName} ")
        sb.append(
          s"(\u001B[36m${frame.source}\u001B[0m:${frame.line}:${frame.column})\n"
        )
      }

      sb.toString()
    }

  /** Create a stack frame from a bytecode function.
    *
    * @param functionName
    *   Function name
    * @param bytecodeOffset
    *   Current bytecode position
    * @return
    *   Stack frame
    */
  def createStackFrame(
      functionName: String,
      source: String = "<eval>",
      line: Int = 0,
      column: Int = 0
  ): StackFrame =
    StackFrame(functionName, source, line, column)
}

/** Error reporter that collects errors during compilation/execution.
  *
  * Used by the compiler and interpreter to report errors with rich context.
  */
class ErrorReporter {
  import ErrorHandler.*

  private val errors = ArrayBuffer[Error]()
  private val stackTrace = ArrayBuffer[StackFrame]()

  /** Report an error.
    *
    * @param message
    *   Error message
    * @param span
    *   Source location
    * @param errorType
    *   Type of error
    */
  def report(message: String, span: Span, errorType: ErrorType): Unit =
    errors += Error(message, span, errorType)

  /** Report an error without span.
    *
    * @param message
    *   Error message
    * @param errorType
    *   Type of error
    */
  def reportSimple(message: String, errorType: ErrorType): Unit =
    // Use a default span
    errors += Error(message, Span(0, 0, 0, 0), errorType)

  /** Add a stack frame.
    *
    * @param frame
    *   Stack frame to add
    */
  def pushStackFrame(frame: StackFrame): Unit =
    stackTrace += frame

  /** Remove the most recent stack frame.
    */
  def popStackFrame(): Unit =
    if stackTrace.nonEmpty then stackTrace.remove(stackTrace.length - 1)

  /** Get all errors.
    *
    * @return
    *   Seq of errors
    */
  def getErrors: Seq[Error] = errors.toSeq

  /** Get current stack trace.
    *
    * @return
    *   Seq of stack frames
    */
  def getStackTrace: Seq[StackFrame] = stackTrace.toSeq

  /** Clear all errors.
    */
  def clearErrors(): Unit =
    errors.clear()

  /** Clear stack trace.
    */
  def clearStackTrace(): Unit =
    stackTrace.clear()

  /** Check if there are any errors.
    *
    * @return
    *   true if there are errors
    */
  def hasErrors: Boolean = errors.nonEmpty

  /** Format all errors.
    *
    * @param source
    *   Source code
    * @return
    *   Formatted error messages
    */
  def formatErrors(source: String): String =
    if errors.isEmpty then ""
    else {
      val sb = StringBuilder()
      for error <- errors do {
        sb.append(formatError(source, error))
        sb.append("\n")
      }

      // Add stack trace if available
      if stackTrace.nonEmpty then sb.append(formatStackTrace(stackTrace.toSeq))

      sb.toString()
    }

  /** Format the first error.
    *
    * @param source
    *   Source code
    * @return
    *   Formatted error message
    */
  def formatFirstError(source: String): String =
    if errors.isEmpty then "Unknown error"
    else {
      val sb = StringBuilder()
      sb.append(formatError(source, errors.head))

      // Add stack trace if available
      if stackTrace.nonEmpty then sb.append(formatStackTrace(stackTrace.toSeq))

      sb.toString()
    }
}
