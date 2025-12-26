package quickjs.repl

import quickjs.lexer.Lexer
import quickjs.parser.Parser as QuickJSParser
import quickjs.compiler.Compiler
import quickjs.interpreter.{Interpreter, DebugTracer, VariableInspector, DebugCommand}
import quickjs.diagnostic.{ErrorHandler, ErrorType}
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import quickjs.util.PrettyPrinter
import org.jline.reader.*
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.Terminal
import org.jline.terminal.TerminalBuilder
import org.jline.reader.EndOfFileException
import org.jline.reader.UserInterruptException
import scala.util.{Try, Success, Failure}
import java.io.PrintWriter
import org.jline.utils.AttributedString
import org.jline.utils.AttributedStringBuilder
import org.jline.utils.AttributedStyle

/** Enhanced Read-Eval-Print Loop for QuickJS-Scala.
  *
  * Provides an interactive JavaScript shell with:
  * - Multi-line input support
  * - Debug/trace mode
  * - Variable inspection
  * - Better error messages
  * - Stack traces
  */
class REPL(runtime: JSRuntime, ctx: JSContext):
  import REPL.*

  private var running = true
  private var multiline = false
  private var multilineBuffer = StringBuilder()
  private var terminal: Terminal = _
  private var reader: LineReader = _

  /** Helper to print with color support */
  private def printColor(msg: String): Unit =
    // Strip ANSI escape codes for clean output
    // This ensures output is readable in all terminals
    val plain = msg.replaceAll("\u001B\\[[0-9;]+m", "")
    println(plain)

  /** Start the REPL loop */
  def run(): Unit =
    terminal = TerminalBuilder.builder()
      .system(true)
      .build()

    try
      reader = LineReaderBuilder.builder()
        .terminal(terminal)
        .completer(new REPLCompleter)
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
          val prompt = if multiline then continuationPrompt else if DebugTracer.global.isEnabled then debugPrompt else normalPrompt
          val line = reader.readLine(prompt)

          if line == null then
            // EOF (Ctrl-D)
            println()
            running = false
          else if line.isEmpty && multiline then
            // Empty line in multiline mode - try to execute
            if isComplete(multilineBuffer.toString) then
              evaluate(multilineBuffer.toString())
              multiline = false
              multilineBuffer.clear()
            else
              // Still incomplete, continue waiting
              ()
          else if line.startsWith(".") then
            handleCommand(line)
          else
            processLine(line)
        catch
          case _: EndOfFileException =>
            println()
            running = false
          case _: UserInterruptException =>
            println("\nUse .quit to exit")
            multiline = false
            multilineBuffer.clear()
          case ex: Exception =>
            terminal.writer().println(ErrorHandler.formatException("", ex))
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

  /** Handle REPL commands (starting with .) */
  private def handleCommand(line: String): Unit =
    val cmd = DebugCommand.parse(line)

    cmd match
      case DebugCommand.Help =>
        showHelp()

      case DebugCommand.TraceEnable =>
        DebugTracer.global.enable()
        printColor("\u001B[32mDebug mode enabled.\u001B[0m")
        printColor("  Instructions will be traced as they execute.")
        printColor("  Use .nodebug to disable.")

      case DebugCommand.TraceDisable =>
        DebugTracer.global.disable()
        printColor("\u001B[33mDebug mode disabled.\u001B[0m")

      case DebugCommand.TraceShow =>
        val trace = DebugTracer.global.getOutput
        if trace.isEmpty then
          printColor("No trace output available.")
        else
          printColor("\n\u001B[36mExecution trace:\u001B[0m")
          printColor(trace)

      case DebugCommand.Vars =>
        // Can't show locals without execution context
        printColor("\u001B[36mGlobal variables:\u001B[0m")
        given JSContext = ctx
        printColor(VariableInspector.global.inspectGlobals)

      case DebugCommand.VarsGlobal =>
        given JSContext = ctx
        printColor(VariableInspector.global.inspectGlobals)

      case DebugCommand.StackTrace =>
        // Stack trace not yet implemented
        printColor("Stack trace tracking coming soon.")

      case DebugCommand.Unknown =>
        printColor(s"\u001B[31mUnknown command: $line\u001B[0m")
        printColor("Type .help for available commands")

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

    // Clear trace if not in persistent trace mode
    if !DebugTracer.global.isEnabled then
      DebugTracer.global.clear()

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
        println(f"\u001B[90m// Time: $elapsed%.2fms\u001B[0m")

      // Show trace if enabled
      if DebugTracer.global.isEnabled then
        val trace = DebugTracer.global.getOutput
        if trace.nonEmpty then
          println()
          printColor("\u001B[36mExecution trace:\u001B[0m")
          printColor(trace)

    catch
      case ex: RuntimeException =>
        println(ErrorHandler.formatException(source, ex))
      case ex: Exception =>
        println(ErrorHandler.formatException(source, ex))
        if showStackTrace then
          println()
          println("\u001B[90mStack trace (Scala):\u001B[0m")
          ex.printStackTrace()

  /** Format a JSValue for display */
  private def formatValue(value: JSValue): String =
    PrettyPrinter.shortFormat(value)

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

  /** Show help message */
  private def showHelp(): Unit =
    printColor("""
      |\u001B[1mAvailable Commands:\u001B[0m
      |  \u001B[36m.quit, .exit\u001B[0m          Exit the REPL
      |  \u001B[36m.help, .h\u001B[0m             Show this help message
      |  \u001B[36m.debug, .trace\u001B[0m        Enable debug/trace mode
      |  \u001B[36m.nodebug, .notrace\u001B[0m    Disable debug/trace mode
      |  \u001B[36m.trace show\u001B[0m           Show execution trace
      |  \u001B[36m.vars, .v\u001B[0m             Show global variables
      |  \u001B[36m.bt, .backtrace\u001B[0m       Show stack trace
      |
      |\u001B[1mJavaScript Features:\u001B[0m
      |  \u001B[33mArithmetic:\u001B[0m       +, -, *, /, %
      |  \u001B[33mComparison:\u001B[0m       <, >, <=, >=, ==, !=, ===, !==
      |  \u001B[33mLogical:\u001B[0m          &&, ||, !
      |  \u001B[33mOperators:\u001B[0m        typeof, instanceof, in, delete
      |  \u001B[33mVariables:\u001B[0m        var, let, const
      |  \u001B[33mControl flow:\u001B[0m     if/else, while, for, for...of
      |  \u001B[33mFunctions:\u001B[0m       function declarations, expressions
      |  \u001B[33mArrays:\u001B[0m          [1, 2, 3], arr[0], arr.push(1)
      |  \u001B[33mObjects:\u001B[0m         {x: 1, y: 2}, obj.prop
      |  \u001B[33mMethods:\u001B[0m         arr.map(), arr.filter(), str.trim()
      |  \u001B[33mMath:\u001B[0m            Math.abs(), Math.random(), etc.
      |  \u001B[33mMulti-line:\u001B[0m      Automatic detection with balanced braces
      |
      |\u001B[1mExamples:\u001B[0m
      |  js> 1 + 2
      |  3
      |  js> var x = 42
      |  js> typeof x
      |  "number"
      |  js> var arr = [1, 2, 3]
      |  js> arr.map(x => x * 2)
      |  [2, 4, 6]
      |  js> .debug
      |  Debug mode enabled.
      |  js> 1 + 2
      |
      |  Execution trace:
      |  [0] PushI32 | stack: [1]
      |  [1] PushI32 | stack: [2, 1]
      |  [2] Add | stack: [3]
      |
      |  3
      |  js> .quit
      |""".stripMargin)

object REPL:
  /** Show timing information */
  private var showTiming: Boolean = false

  /** Show stack traces */
  private var showStackTrace: Boolean = false

  /** Normal prompt */
  private val normalPrompt: String = "js> "

  /** Debug prompt */
  private val debugPrompt: String = "\u001B[31mdebug\u001B[0m js> "

  /** Continuation prompt for multiline */
  private val continuationPrompt: String = " ... "

  /** Welcome message */
  private val welcomeMessage: String =
    """\u001B[1mQuickJS-Scala REPL v0.2.0\u001B[0m
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
