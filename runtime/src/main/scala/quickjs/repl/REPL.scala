package quickjs.repl

import quickjs.lexer.Lexer
import quickjs.parser.Parser as QuickJSParser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import org.jline.reader.*
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.Terminal
import org.jline.terminal.TerminalBuilder
import org.jline.reader.EndOfFileException
import org.jline.reader.UserInterruptException
import scala.util.{Try, Success, Failure}
import java.io.PrintWriter

/** Read-Eval-Print Loop for QuickJS-Scala.
  *
  * Provides an interactive JavaScript shell.
  */
class REPL(runtime: JSRuntime, ctx: JSContext):
  import REPL.*

  private var running = true
  private var multiline = false
  private var multilineBuffer = StringBuilder()

  /** Start the REPL loop */
  def run(): Unit =
    val terminal = TerminalBuilder.builder()
      .system(true)
      .build()

    try
      val reader = LineReaderBuilder.builder()
        .terminal(terminal)
        .build()

      // Setup history
      val history = reader.getHistory
      try
        history.load()
      catch
        case _: java.io.IOException => // No existing history

      println(welcomeMessage)
      println()

      while running do
        try
          val prompt = if multiline then " ... " else "js> "
          val line = reader.readLine(prompt)

          if line == null then
            // EOF (Ctrl-D)
            println()
            running = false
          else if line == ".quit" || line == ".exit" then
            running = false
          else if line == ".help" then
            showHelp()
          else if line.startsWith(".") then
            println(s"Unknown command: $line")
            println("Type .help for available commands")
          else
            processLine(line)
        catch
          case _: EndOfFileException =>
            println()
            running = false
          case _: UserInterruptException =>
            println("\nUse .quit to exit")
          case ex: RuntimeException =>
            terminal.writer().println(s"Error: ${ex.getMessage}")
            terminal.writer().flush()
            multiline = false
            multilineBuffer.clear()

      // Save history
      try
        reader.getHistory.save()
      catch
        case _: java.io.IOException => // Failed to save

    finally
      terminal.close()

  /** Process a single line of input */
  private def processLine(line: String): Unit =
    if multiline then
      multilineBuffer.append("\n").append(line)
      // Check if input is complete (balanced braces/parens)
      if isComplete(multilineBuffer.toString) then
        evaluate(multilineBuffer.toString())
        multiline = false
        multilineBuffer.clear()
      // else continue waiting for more input
    else
      // Check if this might be multi-line
      if needsMoreLines(line) then
        multiline = true
        multilineBuffer.append(line)
      else
        evaluate(line)

  /** Evaluate JavaScript code and print result */
  private def evaluate(source: String): Unit =
    val start = System.nanoTime()

    try
      // Tokenize
      val lexer = Lexer(source)
      val tokens = lexer.tokenize()

      // Parse
      val parser = QuickJSParser(tokens)
      val ast = parser.parseScript()

      // Compile
      val compiler = Compiler()
      val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }

      // Execute
      given JSContext = ctx
      val interpreter = Interpreter()
      val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

      val elapsed = (System.nanoTime() - start) / 1_000_000.0

      // Print result
      result match
        case JSValue.Undefined =>
          // Don't print anything for undefined
          ()
        case _ =>
          println(formatValue(result))

      if showTiming then
        println(s"// Time: ${elapsed}ms")

    catch
      case ex: RuntimeException =>
        println(s"RuntimeError: ${ex.getMessage}")
      case ex: Exception =>
        println(s"Error: ${ex.getClass.getSimpleName}: ${ex.getMessage}")
        if showStackTrace then
          ex.printStackTrace()

  /** Check if input needs more lines (unbalanced braces/parens) */
  private def needsMoreLines(line: String): Boolean =
    val openBraces = line.count(_ == '{')
    val closeBraces = line.count(_ == '}')
    val openParens = line.count(_ == '(')
    val closeParens = line.count(_ == ')')
    val openBrackets = line.count(_ == '[')
    val closeBrackets = line.count(_ == ']')

    openBraces > closeBraces || openParens > closeParens || openBrackets > closeBrackets ||
    line.endsWith("{") || line.endsWith("(") || line.endsWith("[")

  /** Check if complete input is balanced */
  private def isComplete(input: String): Boolean =
    val openBraces = input.count(_ == '{')
    val closeBraces = input.count(_ == '}')
    val openParens = input.count(_ == '(')
    val closeParens = input.count(_ == ')')
    val openBrackets = input.count(_ == '[')
    val closeBrackets = input.count(_ == ']')

    openBraces == closeBraces && openParens == closeParens && openBrackets == closeBrackets

  /** Format a JSValue for display */
  private def formatValue(value: JSValue): String = value match
    case JSValue.Undefined => "undefined"
    case JSValue.Null => "null"
    case JSValue.Bool(b) => b.toString
    case JSValue.Int32(i) => i.toString
    case JSValue.Float64(d) =>
      if d == Math.floor(d) then s"${d.toLong}.0"
      else d.toString
    case JSValue.JSStr(s) => s""""$s""""
    case JSValue.JSArrayVal(arr) => arr.toString
    case JSValue.Function(name, _, _, _) => s"[Function: $name]"
    case JSValue.Object(obj) => s"[Object $obj]"
    case _ => value.toString

  /** Show help message */
  private def showHelp(): Unit =
    println("""Available commands:
      |  .quit, .exit    Exit the REPL
      |  .help           Show this help message
      |
      |JavaScript features supported:
      |  - Arithmetic: +, -, *, /, %
      |  - Comparison: <, >, <=, >=, ==, !=, ===, !==
      |  - Logical: &&, ||, !
      |  - Variables: var, let, const
      |  - Control flow: if/else, while, for
      |  - Functions: function declarations
      |  - Increment/decrement: ++, --
      |  - Comments: // single line
      |
      |Examples:
      |  1 + 2
      |  var x = 42
      |  if (x > 0) { x + 1; } else { 0; }
      |  while (x < 50) { x = x + 1; }
      |  function add(a, b) { return a + b; }
      |""".stripMargin)

object REPL:
  /** Show timing information */
  private var showTiming: Boolean = false

  /** Show stack traces */
  private var showStackTrace: Boolean = false

  /** Welcome message */
  private val welcomeMessage: String =
    """QuickJS-Scala REPL v0.1.0
      |Type .help for help, .quit to exit
      |""".stripMargin

  /** Main entry point */
  def main(args: Array[String]): Unit =
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val repl = new REPL(summon[JSRuntime], summon[JSContext])
    repl.run()

  /** Create a REPL instance */
  def apply()(using runtime: JSRuntime, ctx: JSContext): REPL =
    new REPL(runtime, ctx)
