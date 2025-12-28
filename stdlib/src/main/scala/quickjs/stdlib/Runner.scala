package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.diagnostic.ErrorHandler
import quickjs.runtime.{JSRuntime, JSContext}
import quickjs.value.JSValue
import scala.util.{Try, Success, Failure}

/** Standalone JavaScript file runner for QuickJS-Scala.
  *
  * Runs JavaScript files similar to Node.js:
  * {{{
  *   scala quickjs.stdlib.Runner script.js
  * }}}
  *
  * Usage:
  *   - `quickjs-runner script.js` - Execute a JavaScript file
  *   - `quickjs-runner --eval "console.log('hello')"` - Evaluate inline JavaScript
  *   - `quickjs-runner --version` - Show version
  *   - `quickjs-runner --help` - Show help
  */
object Runner:
  val version = "0.2.0"

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      showHelp()
      sys.exit(1)

    args.head match
      case "--help" | "-h" =>
        showHelp()
        sys.exit(0)

      case "--version" | "-v" =>
        showVersion()
        sys.exit(0)

      case "--eval" | "-e" =>
        if args.length < 2 then
          System.err.println("Error: --eval requires an argument")
          sys.exit(1)
        evalCode(args(1))
        sys.exit(0)

      case filename =>
        runFile(filename)
        sys.exit(0)

  /** Run a JavaScript file */
  private def runFile(filename: String): Unit =
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Initialize standard library
    ArrayStatics.initialize()
    JSON.initialize()
    Console.initialize()

    try
      // Read file
      val source = scala.io.Source.fromFile(filename)
      val content = try source.mkString finally source.close()

      // Parse, compile, and execute
      execute(content, filename)

    catch
      case e: java.io.FileNotFoundException =>
        System.err.println(s"Error: File not found: $filename")
        sys.exit(1)

      case e: java.io.IOException =>
        System.err.println(s"Error reading file: $filename")
        System.err.println(e.getMessage)
        sys.exit(1)

      case ex: Exception =>
        System.err.println(ErrorHandler.formatException(s"$filename:", ex))
        sys.exit(1)

  /** Evaluate inline JavaScript code */
  private def evalCode(code: String): Unit =
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Initialize standard library
    ArrayStatics.initialize()
    JSON.initialize()
    Console.initialize()

    try
      execute(code, "<eval>")
    catch
      case ex: Exception =>
        System.err.println(ErrorHandler.formatException("<eval>:", ex))
        sys.exit(1)

  /** Execute JavaScript code */
  private def execute(source: String, sourceName: String)(using runtime: JSRuntime, ctx: JSContext): Unit =
    // Tokenize
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()

    // Parse
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    // Compile
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  /** Show help message */
  private def showHelp(): Unit =
    println(s"QuickJS-Scala Runner v$version")
    println()
    println("Usage: quickjs-runner [options] <script.js>")
    println()
    println("Options:")
    println("  -e, --eval <code>    Evaluate inline JavaScript code")
    println("  -v, --version        Show version number")
    println("  -h, --help           Show this help message")
    println()
    println("Examples:")
    println("  quickjs-runner script.js")
    println("  quickjs-runner --eval \"console.log('hello')\"")

  /** Show version */
  private def showVersion(): Unit =
    println(s"QuickJS-Scala v$version")
