package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.diagnostic.ErrorHandler
import quickjs.module.FileModuleLoader
import quickjs.node.{NodeExit, NodeOptions, NodeRuntime}
import quickjs.runtime.{JSRuntime, JSContext, StdLib}
import quickjs.value.JSValue
import java.nio.file.{Files, Paths}

/** Standalone JavaScript file runner for QuickJS-Scala.
  *
  * Runs JavaScript files similar to the QuickJS `qjs` command line:
  * {{{
  *   java -jar quickjs-runner.jar script.js
  *   java -jar quickjs-runner.jar --module module.mjs
  *   java -jar quickjs-runner.jar --eval "console.log('hello')"
  * }}}
  *
  * Files ending in `.mjs` (or passed with `-m/--module`) are executed as ES
  * modules: `import`, dynamic `import()`, `import.meta` and top-level await
  * are available and relative specifiers resolve against the file's directory.
  */
object Runner {
  val version = "0.2.0"

  def main(args: Array[String]): Unit = {
    if args.isEmpty then {
      showHelp()
      sys.exit(1)
    }

    var moduleMode = false
    var nodeMode = false
    var evalCodeArg: Option[String] = None
    var fileArg: Option[String] = None
    var scriptArgs = Vector.empty[String]

    var i = 0
    while i < args.length && fileArg.isEmpty && evalCodeArg.isEmpty do {
      args(i) match {
        case "--help" | "-h" =>
          showHelp()
          sys.exit(0)
        case "--version" | "-v" =>
          showVersion()
          sys.exit(0)
        case "--module" | "-m" =>
          moduleMode = true
        case "--node" =>
          nodeMode = true
        case "--eval" | "-e" =>
          if i + 1 >= args.length then {
            System.err.println("Error: --eval requires an argument")
            sys.exit(1)
          }
          evalCodeArg = Some(args(i + 1))
          i += 1
        case "--" =>
          if i + 1 < args.length then fileArg = Some(args(i + 1))
          if i + 2 < args.length then scriptArgs = args.drop(i + 2).toVector
          i = args.length
        case arg if arg.startsWith("-") && arg != "-" =>
          System.err.println(s"Error: unknown option: $arg")
          sys.exit(1)
        case arg =>
          fileArg = Some(arg)
          if i + 1 < args.length then scriptArgs = args.drop(i + 1).toVector
          i = args.length
      }
      i += 1
    }

    evalCodeArg match {
      case Some(code) =>
        evalCode(code, scriptArgs, nodeMode)
      case None =>
        fileArg match {
          case Some(file) =>
            runFile(file, scriptArgs, forceModule = moduleMode, nodeMode = nodeMode)
          case None =>
            showHelp()
            sys.exit(1)
        }
    }
  }

  /** Run a JavaScript file. `moduleMode` forces ES module execution (also
    * implied by a `.mjs` extension).
    */
  private def runFile(
      filename: String,
      scriptArgs: Vector[String],
      forceModule: Boolean,
      nodeMode: Boolean
  ): Unit = {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Initialize standard library
    StdLib.initialize(summon[JSContext])
    JSON.initialize()
    Console.initialize()
    Globals.initialize()
    val timers = Timers.newState()
    Timers.initialize(timers)

    var content = ""
    val path = Paths.get(filename).toAbsolutePath.normalize
    val baseDir =
      Option(path.getParent).getOrElse(Paths.get(".").toAbsolutePath.normalize)

    var nodeRuntime: NodeRuntime = null
    if nodeMode then {
      nodeRuntime = NodeRuntime.install(
        NodeOptions(
          argv = Vector("node", path.toString) ++ scriptArgs,
          cwd = Paths.get("").toAbsolutePath.normalize
        )
      )
      summon[JSRuntime].setModuleLoader(nodeRuntime.loader)
    } else {
      // Resolve `import`/dynamic import relative to the script's directory.
      summon[JSRuntime].setModuleLoader(FileModuleLoader(baseDir))
    }

    def installScriptArgs(): Unit = {
      val arr = quickjs.objmodel.JSArray.empty()
      arr.push(JSValue.fromString(path.toString))
      scriptArgs.foreach(a => arr.push(JSValue.fromString(a)))
      summon[JSContext].global.set("scriptArgs", JSValue.JSArrayVal(arr))
    }

    try {
      // Read file
      val source = scala.io.Source.fromFile(filename)
      content =
        try source.mkString
        finally source.close()

      installScriptArgs()
      val isModule = forceModule || filename.endsWith(".mjs")
      if nodeMode && !isModule then {
        nodeRuntime.loader.runMainFile(path)
        nodeRuntime.loop.run(summon[JSContext], propagateErrors = true)
      } else {
        execute(content, path.toString, isModule, timers)
        if nodeMode then
          nodeRuntime.loop.run(summon[JSContext], propagateErrors = true)
      }
      if nodeMode && nodeRuntime.state.exitCode != 0 then
        sys.exit(nodeRuntime.state.exitCode)
    } catch {
      case exit: NodeExit =>
        sys.exit(exit.code)

      case e: java.io.FileNotFoundException =>
        System.err.println(s"Error: File not found: $filename")
        sys.exit(1)

      case e: java.io.IOException =>
        System.err.println(s"Error reading file: $filename")
        System.err.println(e.getMessage)
        sys.exit(1)

      case ex: Exception =>
        System.err.println(ErrorHandler.formatException(filename, content, ex))
        sys.exit(1)
    }
  }

  /** Evaluate inline JavaScript code as a script (dynamic `import()` resolves
    * relative to the current working directory).
    */
  private def evalCode(
      code: String,
      scriptArgs: Vector[String],
      nodeMode: Boolean
  ): Unit = {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Initialize standard library
    StdLib.initialize(summon[JSContext])
    JSON.initialize()
    Console.initialize()
    Globals.initialize()
    val timers = Timers.newState()
    Timers.initialize(timers)
    var nodeRuntime: NodeRuntime = null
    if nodeMode then {
      nodeRuntime = NodeRuntime.install(
        NodeOptions(
          argv = Vector("node") ++ scriptArgs,
          cwd = Paths.get("").toAbsolutePath.normalize
        )
      )
      summon[JSRuntime].setModuleLoader(nodeRuntime.loader)
    } else {
      summon[JSRuntime].setModuleLoader(
        FileModuleLoader(Paths.get(".").toAbsolutePath.normalize)
      )
    }

    def installScriptArgs(): Unit = {
      val arr = quickjs.objmodel.JSArray.empty()
      arr.push(JSValue.fromString("-e"))
      scriptArgs.foreach(a => arr.push(JSValue.fromString(a)))
      summon[JSContext].global.set("scriptArgs", JSValue.JSArrayVal(arr))
    }

    try {
      installScriptArgs()
      if nodeMode then {
        nodeRuntime.loader.executeCJS(code, "<eval>")
        nodeRuntime.loop.run(summon[JSContext], propagateErrors = true)
        if nodeRuntime.state.exitCode != 0 then
          sys.exit(nodeRuntime.state.exitCode)
      } else {
        execute(code, "<eval>", isModule = false, timers)
      }
    } catch {
      case exit: NodeExit =>
        sys.exit(exit.code)
      case ex: Exception =>
        System.err.println(ErrorHandler.formatException("<eval>", code, ex))
        sys.exit(1)
    }
  }

  /** Execute JavaScript code as a script or an ES module. */
  private def execute(
      source: String,
      sourceName: String,
      isModule: Boolean,
      timers: Timers.State
  )(using runtime: JSRuntime, ctx: JSContext): Unit = {
    ctx.setSourceName(sourceName)
    // Tokenize
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()

    // Parse
    val parser = new Parser(tokens, moduleMode = isModule)
    val ast = parser.parseScript()

    // Compile
    val compiler = Compiler()
    val bytecode =
      if isModule then compiler.compileModule(ast, sourceName)
      else compiler.compileScript(ast)

    // Execute
    ctx.currentModulePath = sourceName
    val interpreter = Interpreter()
    try {
      interpreter.call(bytecode, JSValue.Undefined, Array.empty)
      // Drain promise reactions / dynamic imports, then pending timers.
      ctx.runMicrotasks()
      Timers.runPending(timers)
    } finally ctx.currentModulePath = ""
  }

  /** Show help message */
  private def showHelp(): Unit = {
    println(s"QuickJS-Scala Runner v$version")
    println()
    println("Usage: quickjs-runner [options] <script.js> [args...]")
    println()
    println("Options:")
    println("  -m, --module         Execute the file as an ES module")
    println("      --node           Run with Node.js compatibility (require, process, fs, ...)")
    println("  -e, --eval <code>    Evaluate inline JavaScript code")
    println("  -v, --version        Show version number")
    println("  -h, --help           Show this help message")
    println()
    println("Examples:")
    println("  quickjs-runner script.js")
    println("  quickjs-runner module.mjs")
    println("  quickjs-runner -m script.js")
    println("  quickjs-runner --node script.js")
    println("  quickjs-runner --eval \"console.log('hello')\"")
  }

  /** Show version */
  private def showVersion(): Unit =
    println(s"QuickJS-Scala v$version")
}
