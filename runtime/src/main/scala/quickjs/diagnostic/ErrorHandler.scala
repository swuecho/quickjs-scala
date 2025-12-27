package quickjs.diagnostic

import quickjs.ast.Span
import quickjs.value.JSValue
import scala.collection.mutable.ArrayBuffer

/** Error with source location information.
  *
  * @param message Error message
  * @param span Source location
  * @param errorType Type of error (syntax, runtime, type, etc.)
  */
case class Error(
  message: String,
  span: Span,
  errorType: ErrorType
)

/** Types of errors */
enum ErrorType:
  case SyntaxError
  case RuntimeError
  case TypeError
  case ReferenceError
  case RangeError

/** Stack frame representing a function call.
  *
  * @param functionName Name of the function
  * @param source Source file or "<eval>"
  * @param line Line number
  * @param column Column number
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
  * - Source code context
  * - Line and column numbers
  * - Caret pointing to error location
  * - Stack traces with call chain
  */
object ErrorHandler:

  /** Format an error with source location.
    *
    * @param source Source code
    * @param error Error to format
    * @return Formatted error message
    */
  def formatError(source: String, error: Error): String =
    val lines = source.split("\n", -1)
    val errorLine = error.span.line
    val errorCol = error.span.column

    // Get context lines (show 2 lines before and after)
    val startLine = Math.max(0, errorLine - 2)
    val endLine = Math.min(lines.length - 1, errorLine + 2)

    val sb = StringBuilder()

    // Error header with color
    sb.append(s"\u001B[31m\u001B[1m${error.errorType}: ${error.message}\u001B[0m\n")
    sb.append(s"  \u001B[90m// at line ${errorLine + 1}, column ${errorCol + 1}\u001B[0m\n")

    // Show source context
    for i <- startLine to endLine do
      val lineNum = i + 1
      val prefix = if i == errorLine then ">>" else "  "
      sb.append(s"$prefix \u001B[90m$lineNum|\u001B[0m ${lines(i)}\n")

      // Show caret on error line
      if i == errorLine then
        val caretSpaces = " " * (errorCol + 4)  // 4 = ">> |".length
        sb.append(s"$caretSpaces\u001B[31m^\u001B[0m\n")

    sb.toString()

  /** Format an exception without span information.
    *
    * @param source Source code
    * @param ex Exception to format
    * @return Formatted error message
    */
  def formatException(source: String, ex: Throwable): String =
    val errorType = ex match
      case _: RuntimeException => "RuntimeError"
      case _ => ex.getClass.getSimpleName

    val lines = source.split("\n", -1)

    val sb = StringBuilder()
    sb.append(s"\u001B[31m\u001B[1m$errorType: ${ex.getMessage}\u001B[0m\n")

    // Show first few lines of source
    if lines.nonEmpty then
      sb.append(s"  \u001B[90m// where:\u001B[0m\n")
      for (line, i) <- lines.take(3).zipWithIndex do
        sb.append(s"  ${i + 1}| $line\n")

    sb.toString()

  /** Format a stack trace.
    *
    * @param frames Stack frames
    * @return Formatted stack trace
    */
  def formatStackTrace(frames: Seq[StackFrame]): String =
    if frames.isEmpty then
      ""
    else
      val sb = StringBuilder()
      sb.append("\u001B[90m  Stack trace:\u001B[0m\n")

      for (frame, i) <- frames.zipWithIndex do
        val prefix = if i == 0 then "    at" else "    from"
        sb.append(s"\u001B[90m$prefix\u001B[0m ${frame.functionName} ")
        sb.append(s"(\u001B[36m${frame.source}\u001B[0m:${frame.line}:${frame.column})\n")

      sb.toString()

  /** Create a stack frame from a bytecode function.
    *
    * @param functionName Function name
    * @param bytecodeOffset Current bytecode position
    * @return Stack frame
    */
  def createStackFrame(
    functionName: String,
    source: String = "<eval>",
    line: Int = 0,
    column: Int = 0
  ): StackFrame =
    StackFrame(functionName, source, line, column)

/** Error reporter that collects errors during compilation/execution.
  *
  * Used by the compiler and interpreter to report errors with rich context.
  */
class ErrorReporter:
  import ErrorHandler.*

  private val errors = ArrayBuffer[Error]()
  private val stackTrace = ArrayBuffer[StackFrame]()

  /** Report an error.
    *
    * @param message Error message
    * @param span Source location
    * @param errorType Type of error
    */
  def report(message: String, span: Span, errorType: ErrorType): Unit =
    errors += Error(message, span, errorType)

  /** Report an error without span.
    *
    * @param message Error message
    * @param errorType Type of error
    */
  def reportSimple(message: String, errorType: ErrorType): Unit =
    // Use a default span
    errors += Error(message, Span(0, 0, 0, 0), errorType)

  /** Add a stack frame.
    *
    * @param frame Stack frame to add
    */
  def pushStackFrame(frame: StackFrame): Unit =
    stackTrace += frame

  /** Remove the most recent stack frame.
    */
  def popStackFrame(): Unit =
    if stackTrace.nonEmpty then
      stackTrace.remove(stackTrace.length - 1)

  /** Get all errors.
    *
    * @return Seq of errors
    */
  def getErrors: Seq[Error] = errors.toSeq

  /** Get current stack trace.
    *
    * @return Seq of stack frames
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
    * @return true if there are errors
    */
  def hasErrors: Boolean = errors.nonEmpty

  /** Format all errors.
    *
    * @param source Source code
    * @return Formatted error messages
    */
  def formatErrors(source: String): String =
    if errors.isEmpty then
      ""
    else
      val sb = StringBuilder()
      for error <- errors do
        sb.append(formatError(source, error))
        sb.append("\n")

      // Add stack trace if available
      if stackTrace.nonEmpty then
        sb.append(formatStackTrace(stackTrace.toSeq))

      sb.toString()

  /** Format the first error.
    *
    * @param source Source code
    * @return Formatted error message
    */
  def formatFirstError(source: String): String =
    if errors.isEmpty then
      "Unknown error"
    else
      val sb = StringBuilder()
      sb.append(formatError(source, errors.head))

      // Add stack trace if available
      if stackTrace.nonEmpty then
        sb.append(formatStackTrace(stackTrace.toSeq))

      sb.toString()
