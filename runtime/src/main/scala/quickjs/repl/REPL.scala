package quickjs.repl

import quickjs.lexer.Lexer
import quickjs.parser.Parser as QuickJSParser
import quickjs.compiler.Compiler
import quickjs.interpreter.{
  Interpreter,
  DebugTracer,
  VariableInspector,
  DebugCommand
}
import quickjs.diagnostic.{ErrorHandler, ErrorType}
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import quickjs.util.PrettyPrinter
import scala.compiletime.uninitialized
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
  *   - Multi-line input support
  *   - Debug/trace mode
  *   - Variable inspection
  *   - Better error messages
  *   - Stack traces
  *
  * @param reinitialize
  *   Optional callback invoked after `.reset` deletes the configurable global
  *   properties. Hosts pass an initializer that re-installs the standard
  *   library and host globals, so a reset does not leave the session without
  *   `console`/`JSON`/`Math`.
  */
class REPL(
    runtime: JSRuntime,
    ctx: JSContext,
    reinitialize: () => Unit = () => ()
) {
  import REPL.*

  private var running = true
  private var multiline = false
  private var multilineBuffer = StringBuilder()
  private var terminal: Terminal = uninitialized
  private var reader: LineReader = uninitialized
  private var lastResult: JSValue = JSValue.Undefined // For _ special variable

  /** Helper to print with color support using AttributedStringBuilder */
  private def printStyled(build: AttributedStringBuilder => Unit): Unit = {
    val sb = AttributedStringBuilder()
    build(sb)
    val styled = sb.toAttributedString
    if terminal != null then styled.print(terminal)
    terminal.writer().println()
    terminal.writer().flush()
  }

  /** Helper to print plain text with optional color */
  private def printColor(msg: String): Unit =
    if terminal != null then {
      terminal.writer().println(msg)
      terminal.writer().flush()
    }
    else println(msg)

  /** Start the REPL loop */
  def run(): Unit = {
    terminal = TerminalBuilder
      .builder()
      .system(true)
      .build()

    try {
      reader = LineReaderBuilder
        .builder()
        .terminal(terminal)
        .completer(new REPLCompleter)
        // JLine's history `!` event expansion also eats backslashes, which
        // corrupts JavaScript string escapes and regexp literals. JavaScript
        // uses `!` for negation, so disable the expansion entirely.
        .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
        .build()

      // Persist history across sessions (JLine only saves when the
      // `history-file` variable is set).
      reader.setVariable(
        LineReader.HISTORY_FILE,
        java.nio.file.Paths
          .get(System.getProperty("user.home"), ".quickjs-scala-history")
      )

      // Setup history
      val history = reader.getHistory
      try history.load()
      catch {
        case _: java.io.IOException => // No existing history
      }

      printWelcomeMessage()
      println()

      while running do
        try {
          val prompt =
            if multiline then continuationPrompt
            else if DebugTracer.global.isEnabled then debugPrompt
            else normalPrompt
          val line = reader.readLine(prompt)

          if line == null then {
            // EOF (Ctrl-D)
            println()
            running = false
          }
          else if line.isEmpty && multiline then
            // Empty line in multiline mode - try to execute
            if isComplete(multilineBuffer.toString) then {
              evaluate(multilineBuffer.toString())
              multiline = false
              multilineBuffer.clear()
            }
            else
              // Still incomplete, continue waiting
              (
            )
          else if line.startsWith(".") then handleCommand(line)
          else processLine(line)
        }
        catch {
          case _: EndOfFileException =>
            println()
            running = false
          case _: UserInterruptException =>
            println("\nUse .quit to exit")
            multiline = false
            multilineBuffer.clear()
          case ex: Exception =>
            terminal
              .writer()
              .println(ErrorHandler.formatException("<repl>", "", ex))
            terminal.writer().flush()
            multiline = false
            multilineBuffer.clear()
        }

      // Save history
      try reader.getHistory.save()
      catch {
        case _: java.io.IOException => // Failed to save
      }
    }

    finally terminal.close()
  }

  /** Handle REPL commands (starting with .) */
  private def handleCommand(line: String): Unit = {
    val cmd = DebugCommand.parse(line)

    cmd match {
      case DebugCommand.Help =>
        showHelp()

      case DebugCommand.Quit =>
        running = false

      case DebugCommand.Load(filename) =>
        loadScript(filename)

      case DebugCommand.Reset =>
        resetContext()

      case DebugCommand.TraceEnable =>
        DebugTracer.global.enable()
        printStyled { sb =>
          sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN))
            .append("Debug mode enabled.")
            .style(AttributedStyle.DEFAULT)
        }
        printColor("  Instructions will be traced as they execute.")
        printColor("  Use .nodebug to disable.")

      case DebugCommand.TraceDisable =>
        DebugTracer.global.disable()
        printStyled { sb =>
          sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
            .append("Debug mode disabled.")
            .style(AttributedStyle.DEFAULT)
        }

      case DebugCommand.TraceShow =>
        val trace = DebugTracer.global.getOutput
        if trace.isEmpty then printColor("No trace output available.")
        else {
          printStyled { sb =>
            sb.append("\n")
              .style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
              .append("Execution trace:")
              .style(AttributedStyle.DEFAULT)
          }
          printColor(trace)
        }

      case DebugCommand.Vars =>
        // Can't show locals without execution context
        given JSContext = ctx
        printColor(VariableInspector.global.inspectGlobals)

      case DebugCommand.VarsGlobal =>
        given JSContext = ctx
        printColor(VariableInspector.global.inspectGlobals)

      case DebugCommand.StackTrace =>
        // Stack trace not yet implemented
        printColor("Stack trace tracking coming soon.")

      case DebugCommand.Unknown =>
        printStyled { sb =>
          sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.RED))
            .append(s"Unknown command: $line")
            .style(AttributedStyle.DEFAULT)
        }
        printColor("Type .help for available commands")
    }
  }

  /** Process a single line of input */
  private def processLine(line: String): Unit =
    if multiline then {
      multilineBuffer.append("\n").append(line)
      // Check if input is complete (balanced braces/parens)
      if isComplete(multilineBuffer.toString) then {
        evaluate(multilineBuffer.toString())
        multiline = false
        multilineBuffer.clear()
      }
      // else continue waiting for more input
    }
    else
      // Check if this might be multi-line
      if needsMoreLines(line) then {
        multiline = true
        multilineBuffer.append(line)
      }
      else evaluate(line)

  /** Evaluate JavaScript code and print result */
  private def evaluate(source: String): Unit = {
    val start = System.nanoTime()
    ctx.setSourceName("<repl>")

    // Clear trace if not in persistent trace mode
    if !DebugTracer.global.isEnabled then DebugTracer.global.clear()

    try {
      // Tokenize
      val lexer = Lexer(source)
      val tokens = lexer.tokenize()

      // Parse
      val parser =
        new QuickJSParser(tokens, allowTopLevelReturn = true, source = source)
      val ast = parser.parseScript()

      // Compile
      val compiler = Compiler()
      val bytecode = compiler.withREPLMode(compiler.compileScript(ast))

      // Execute
      given JSContext = ctx
      val interpreter = Interpreter()
      val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

      val elapsed = (System.nanoTime() - start) / 1_000_000.0

      // Save last result for _ special variable
      lastResult = result

      // Update _ variable in global scope
      if result != JSValue.Undefined then ctx.global.set("_", result)

      // Print result
      result match {
        case JSValue.Undefined =>
          // Don't print anything for undefined
          ()
        case _ =>
          println(formatValue(result))
      }

      if showTiming then
        printStyled { sb =>
          sb.style(
            AttributedStyle.DEFAULT.foreground(AttributedStyle.BLACK).bold()
          ).append(f"// Time: $elapsed%.2fms")
            .style(AttributedStyle.DEFAULT)
        }

      // Show trace if enabled
      if DebugTracer.global.isEnabled then {
        val trace = DebugTracer.global.getOutput
        if trace.nonEmpty then {
          println()
          printStyled { sb =>
            sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
              .append("Execution trace:")
              .style(AttributedStyle.DEFAULT)
          }
          printColor(trace)
        }
      }
    }

    catch {
      case ex: RuntimeException =>
        println(ErrorHandler.formatException("<repl>", source, ex))
      case ex: Exception =>
        println(ErrorHandler.formatException("<repl>", source, ex))
        if showStackTrace then {
          println()
          printStyled { sb =>
            sb.style(
              AttributedStyle.DEFAULT.foreground(AttributedStyle.BLACK).bold()
            ).append("Stack trace (Scala):")
              .style(AttributedStyle.DEFAULT)
          }
          ex.printStackTrace()
        }
    }
  }

  /** Format a JSValue for display */
  private def formatValue(value: JSValue): String =
    PrettyPrinter.shortFormat(value)

  /** Load and execute a JavaScript file.
    *
    * @param filename
    *   Path to the file
    */
  private def loadScript(filename: String): Unit =
    try {
      val source = scala.io.Source.fromFile(filename)
      val content =
        try source.mkString
        finally source.close()

      printStyled { sb =>
        sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
          .append(s"Loading: $filename")
          .style(AttributedStyle.DEFAULT)
      }

      evaluate(content)
    }
    catch {
      case e: java.io.FileNotFoundException =>
        printStyled { sb =>
          sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.RED))
            .append(s"File not found: $filename")
            .style(AttributedStyle.DEFAULT)
        }
      case e: java.io.IOException =>
        printStyled { sb =>
          sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.RED))
            .append(s"Error reading file: $filename")
            .style(AttributedStyle.DEFAULT)
        }
        printColor(e.getMessage)
    }

  /** Reset the REPL context. Clears all user-defined variables and resets the
    * global object.
    */
  private def resetContext(): Unit = {
    printStyled { sb =>
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("Resetting context...")
        .style(AttributedStyle.DEFAULT)
    }

    // Clear all properties from the global object
    given JSContext = ctx
    val keys = ctx.global.getOwnPropertyKeys()
    for key <- keys do ctx.global.deleteProperty(key)

    // Reset last result
    lastResult = JSValue.Undefined

    // Re-install the host environment (built-ins, console, JSON, host globals).
    // Non-configurable bindings (var/function declarations, some built-ins)
    // survive the deletion above, but configurable host globals do not.
    reinitialize()

    // Re-initialize basic global properties
    ctx.global.set("undefined", JSValue.Undefined)
    ctx.global.set("NaN", JSValue.Float64(Double.NaN))
    ctx.global.set("Infinity", JSValue.Float64(Double.PositiveInfinity))

    printStyled { sb =>
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN))
        .append("Context reset.")
        .style(AttributedStyle.DEFAULT)
    }
  }

  /** Check if input needs more lines (unbalanced braces/parens) */
  private def needsMoreLines(line: String): Boolean = {
    val openBraces = line.count(_ == '{')
    val closeBraces = line.count(_ == '}')
    val openParens = line.count(_ == '(')
    val closeParens = line.count(_ == ')')
    val openBrackets = line.count(_ == '[')
    val closeBrackets = line.count(_ == ']')

    openBraces > closeBraces || openParens > closeParens || openBrackets > closeBrackets ||
    line.endsWith("{") || line.endsWith("(") || line.endsWith("[")
  }

  /** Check if complete input is balanced */
  private def isComplete(input: String): Boolean = {
    val openBraces = input.count(_ == '{')
    val closeBraces = input.count(_ == '}')
    val openParens = input.count(_ == '(')
    val closeParens = input.count(_ == ')')
    val openBrackets = input.count(_ == '[')
    val closeBrackets = input.count(_ == ']')

    openBraces == closeBraces && openParens == closeParens && openBrackets == closeBrackets
  }

  /** Show help message */
  private def showHelp(): Unit =
    printStyled { sb =>
      sb.style(AttributedStyle.BOLD)
        .append("Available Commands:")
        .style(AttributedStyle.DEFAULT)
      sb.append("\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
        .append("  .quit, .exit, .q")
        .style(AttributedStyle.DEFAULT)
        .append("       Exit the REPL\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
        .append("  .help, .h")
        .style(AttributedStyle.DEFAULT)
        .append("             Show this help message\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
        .append("  .load <file>")
        .style(AttributedStyle.DEFAULT)
        .append("         Load and execute JavaScript file\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
        .append("  .reset, .clear")
        .style(AttributedStyle.DEFAULT)
        .append("        Clear all variables (reset context)\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
        .append("  .debug, .trace")
        .style(AttributedStyle.DEFAULT)
        .append("        Enable debug/trace mode\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
        .append("  .nodebug, .notrace")
        .style(AttributedStyle.DEFAULT)
        .append("    Disable debug/trace mode\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
        .append("  .trace show")
        .style(AttributedStyle.DEFAULT)
        .append("           Show execution trace\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
        .append("  .vars, .v")
        .style(AttributedStyle.DEFAULT)
        .append("             Show global variables\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN))
        .append("  .bt, .backtrace")
        .style(AttributedStyle.DEFAULT)
        .append("       Show stack trace\n")
      sb.append("\n")
      sb.style(AttributedStyle.BOLD)
        .append("Special Variables:")
        .style(AttributedStyle.DEFAULT)
      sb.append("\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  _")
        .style(AttributedStyle.DEFAULT)
        .append("                  Last expression result\n")
      sb.append("\n")
      sb.style(AttributedStyle.BOLD)
        .append("JavaScript Features:")
        .style(AttributedStyle.DEFAULT)
      sb.append("\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Arithmetic:")
        .style(AttributedStyle.DEFAULT)
        .append("       +, -, *, /, %\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Comparison:")
        .style(AttributedStyle.DEFAULT)
        .append("       <, >, <=, >=, ==, !=, ===, !==\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Logical:")
        .style(AttributedStyle.DEFAULT)
        .append("          &&, ||, !\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Operators:")
        .style(AttributedStyle.DEFAULT)
        .append("        typeof, instanceof, in, delete\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Variables:")
        .style(AttributedStyle.DEFAULT)
        .append("        var, let, const\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Control flow:")
        .style(AttributedStyle.DEFAULT)
        .append("     if/else, while, for, for...of\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Functions:")
        .style(AttributedStyle.DEFAULT)
        .append("       function declarations, expressions\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Arrays:")
        .style(AttributedStyle.DEFAULT)
        .append("          [1, 2, 3], arr[0], arr.push(1)\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Objects:")
        .style(AttributedStyle.DEFAULT)
        .append("         {x: 1, y: 2}, obj.prop\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Methods:")
        .style(AttributedStyle.DEFAULT)
        .append("         arr.map(), arr.filter(), str.trim()\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Math:")
        .style(AttributedStyle.DEFAULT)
        .append("            Math.abs(), Math.random(), etc.\n")
      sb.style(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW))
        .append("  Multi-line:")
        .style(AttributedStyle.DEFAULT)
        .append("      Automatic detection with balanced braces\n")
      sb.append("\n")
      sb.style(AttributedStyle.BOLD)
        .append("Examples:")
        .style(AttributedStyle.DEFAULT)
      sb.append("""
        |  js> 1 + 2
        |  3
        |  js> _ * 3
        |  9
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
    }
}

object REPL {
  /** Show timing information */
  private var showTiming: Boolean = false

  /** Show stack traces */
  private var showStackTrace: Boolean = false

  /** Normal prompt */
  private val normalPrompt: String = "js> "

  /** Debug prompt - using AttributedString for proper coloring */
  private val debugPrompt: String =
    AttributedStringBuilder()
      .style(AttributedStyle.DEFAULT.foreground(AttributedStyle.RED).bold())
      .append("debug")
      .style(AttributedStyle.DEFAULT)
      .append(" js> ")
      .toAttributedString
      .toAnsi()

  /** Continuation prompt for multiline */
  private val continuationPrompt: String = " ... "

  /** Welcome message - print without ANSI codes since terminal not ready yet */
  private def printWelcomeMessage(): Unit = {
    println("QuickJS-Scala REPL v0.2.0")
    println("Type .help for help, .quit to exit")
  }

  /** Main entry point */
  def main(args: Array[String]): Unit = {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Runtime built-ins only; the `stdlib` Main adds JSON, console and the
    // remaining host globals.
    StdLib.initialize(summon[JSContext])

    val repl = new REPL(
      summon[JSRuntime],
      summon[JSContext],
      () => StdLib.initialize(summon[JSContext])
    )
    repl.run()
  }

  /** Create a REPL instance */
  def apply(
      reinitialize: () => Unit
  )(using runtime: JSRuntime, ctx: JSContext): REPL =
    new REPL(runtime, ctx, reinitialize)

  /** Create a REPL instance with the default (no-op) reset initializer */
  def apply()(using runtime: JSRuntime, ctx: JSContext): REPL =
    new REPL(runtime, ctx)
}
