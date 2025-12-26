package quickjs.repl

import org.jline.reader.Completer
import org.jline.reader.Candidate
import org.jline.reader.LineReader
import org.jline.reader.ParsedLine
import scala.collection.JavaConverters.*
import java.util.List

/** Tab completer for REPL commands.
  *
  * Completes:
  * - REPL commands (.help, .quit, etc.)
  * - JavaScript keywords
  * - Common global objects
  */
class REPLCompleter extends Completer:

  private val commands = Seq(
    ".help", ".h", ".quit", ".exit",
    ".debug", ".trace", ".nodebug", ".notrace",
    ".trace show", ".vars", ".v", ".vars global", ".vg",
    ".bt", ".backtrace", ".stack"
  )

  private val keywords = Seq(
    "var", "let", "const", "function", "return",
    "if", "else", "while", "for", "break", "continue",
    "true", "false", "null", "undefined",
    "typeof", "instanceof", "in", "delete", "new"
  )

  private val globals = Seq(
    "console", "Array", "Object", "String", "Math", "Function"
  )

  override def complete(
    reader: LineReader,
    line: ParsedLine,
    candidates: List[Candidate]
  ): Unit =
    val buffer = line.wordCursor
    val word = line.word()

    if word.startsWith(".") then
      // Complete REPL commands
      val matching = commands.filter(_.startsWith(word))
      for cmd <- matching do
        candidates.add(new Candidate(cmd, cmd, null, null, null, null, true))
    else
      // Complete JavaScript keywords and globals
      val matching = (keywords ++ globals).filter(_.startsWith(word))
      for kw <- matching do
        candidates.add(new Candidate(kw, kw, null, null, null, null, true))
