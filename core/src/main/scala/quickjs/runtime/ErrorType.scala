package quickjs.runtime

/** JavaScript error types.
  *
  * Represents the standard JavaScript error types that can be thrown at
  * runtime. This enum provides type safety and prevents typos when creating
  * errors.
  */
enum ErrorType {
  case TypeError
  case ReferenceError
  case SyntaxError
  case RangeError
  case Error

  /** Get the error name as a string. */
  def name: String = this.toString
}

object ErrorType {

  /** Parse an error type from a string name. */
  def fromString(name: String): ErrorType =
    name match {
      case "TypeError"      => ErrorType.TypeError
      case "ReferenceError" => ErrorType.ReferenceError
      case "SyntaxError"    => ErrorType.SyntaxError
      case "RangeError"     => ErrorType.RangeError
      case "Error"          => ErrorType.Error
      case _                => ErrorType.Error
    }

  /** Parse an error type from a message string.
    *
    * Looks for error type prefixes in the format "TypeError: message",
    * "ReferenceError: message", etc.
    *
    * @param message
    *   The error message to parse
    * @return
    *   The parsed ErrorType and the extracted message
    */
  def fromMessage(message: String): (ErrorType, String) =
    if message.startsWith("TypeError:") then
      (ErrorType.TypeError, message.stripPrefix("TypeError:").trim)
    else if message.startsWith("ReferenceError:") then
      (ErrorType.ReferenceError, message.stripPrefix("ReferenceError:").trim)
    else if message.startsWith("SyntaxError:") then
      (ErrorType.SyntaxError, message.stripPrefix("SyntaxError:").trim)
    else if message.startsWith("RangeError:") then
      (ErrorType.RangeError, message.stripPrefix("RangeError:").trim)
    else (ErrorType.Error, message)

  /** Format an error message with the error type prefix. */
  def formatMessage(errorType: ErrorType, message: String): String =
    s"${errorType.name}: $message"
}
