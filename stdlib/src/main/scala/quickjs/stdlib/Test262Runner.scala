package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSRuntime, JSContext, StdLib}
import quickjs.value.JSValue

import java.io.File
import java.nio.file.{Files, Paths, Path}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{Callable, ExecutorCompletionService, Executors, ThreadFactory, TimeUnit, TimeoutException}
import scala.collection.mutable
import scala.collection.concurrent.TrieMap
import scala.util.{Try, Success, Failure}
import scala.io.Source

/** Test262 conformance test runner for QuickJS-Scala.
  *
  * Parses test262.conf, enumerates test files from the test262 test suite, and
  * runs each test against the engine. Reports pass/fail/error/skip counts.
  *
  * Usage: Test262Runner.run(configPath = "test262.conf", testDirOverride =
  * None, maxTests = None)
  *
  * The test262 test suite must be available. By default it's expected at the
  * path specified in test262.conf (usually ../test262 relative to the project
  * root, or symlinked).
  */
object Test262Runner {

  // Per-variant timeout. The default is deliberately short: the interpreter
  // is slow enough that genuinely stuck tests otherwise occupy workers for the
  // whole sweep and cause GC/memory buildup from abandoned threads.
  private def perVariantTimeoutSeconds: Long =
    math.max(1L, java.lang.Long.getLong("quickjs.test262.timeoutSeconds", 5L))
  private val timeoutThreadFactory = new ThreadFactory {
    private val nextId = new java.util.concurrent.atomic.AtomicLong(0L)
    override def newThread(runnable: Runnable): Thread = {
      val thread = new Thread(runnable, s"test262-worker-${nextId.incrementAndGet()}")
      thread.setDaemon(true)
      thread
    }
  }
  private val testExecutor = Executors.newCachedThreadPool(timeoutThreadFactory)

  private def runWithTimeout(testPath: String)(body: => TestResult): TestResult = {
    val started = System.currentTimeMillis()
    val future = testExecutor.submit(new Callable[TestResult] {
      override def call(): TestResult = body
    })
    try future.get(perVariantTimeoutSeconds, TimeUnit.SECONDS)
    catch {
      case _: TimeoutException =>
        future.cancel(true)
        TestResult.Timeout(testPath, System.currentTimeMillis() - started)
      case ex: java.util.concurrent.ExecutionException =>
        val cause = Option(ex.getCause).getOrElse(ex)
        TestResult.Error(
          testPath,
          s"${cause.getClass.getSimpleName}: ${Option(cause.getMessage).getOrElse("")}".take(300),
          System.currentTimeMillis() - started
        )
      case _: InterruptedException =>
        future.cancel(true)
        Thread.currentThread().interrupt()
        TestResult.Timeout(testPath, System.currentTimeMillis() - started)
    }
  }

  // =========================================================================
  // Configuration
  // =========================================================================

  /** Parsed test262.conf configuration */
  case class Config(
      harnessDir: String,
      testDir: String,
      errorFile: String,
      reportFile: String,
      mode: String, // default, strict, nostrict, both
      handleAsync: Boolean,
      handleModule: Boolean,
      verbose: Boolean,
      features: Map[
        String,
        String
      ], // feature -> status ("" = supported, "skip" = skip)
      excludes: List[String], // excluded test paths/prefixes
      noStrict: Boolean,
      strict: Boolean
  )

  /** Test metadata parsed from YAML frontmatter */
  case class TestMeta(
      description: String,
      esid: Option[String],
      es5id: Option[String],
      es6id: Option[String],
      includes: List[String],
      flags: List[String],
      negative: Option[NegativeInfo],
      features: List[String],
      info: Option[String]
  )

  case class NegativeInfo(phase: String, errorType: String)

  /** Result of running a single test */
  enum TestResult {
    case Pass(testPath: String, durationMs: Long)
    case Fail(testPath: String, message: String, durationMs: Long)
    case Error(testPath: String, message: String, durationMs: Long)
    case Skip(testPath: String, reason: String)
    case Timeout(testPath: String, durationMs: Long)

    def isPass: Boolean = this match {
      case Pass(_, _) => true
      case _          => false
    }
  }

  /** Aggregate statistics */
  case class Stats(
      total: Int = 0,
      passed: Int = 0,
      failed: Int = 0,
      errors: Int = 0,
      skipped: Int = 0,
      timeouts: Int = 0
  ) {
    def passRate: Double = if total > 0 then
      val executed = total - skipped
      if executed == 0 then 0.0 else passed.toDouble / executed.toDouble * 100.0
    else 0.0
    def summary: String =
      s"Total: $total | Passed: $passed | Failed: $failed | Errors: $errors | Skipped: $skipped | Timeouts: $timeouts | Pass rate: ${f"$passRate%.1f"}%"
  }

  // =========================================================================
  // Config Parsing
  // =========================================================================

  def parseConfig(configPath: String): Config = {
    val source = Source.fromFile(configPath)
    val lines =
      try source.getLines().toList
      finally source.close()

    var section = ""
    var harnessDir = "test262/harness"
    var testDir = "test262/test"
    var errorFile = "test262_errors.txt"
    var reportFile = "test262_report.txt"
    var mode = "default"
    var handleAsync = false
    var handleModule = false
    var verbose = false
    var noStrict = true
    var strict = true
    val features = mutable.Map.empty[String, String]
    val excludes = mutable.ListBuffer.empty[String]

    for line <- lines do {
      val trimmed = line.trim
      if trimmed.startsWith("[") && trimmed.endsWith("]") then
        section = trimmed.substring(1, trimmed.length - 1)
      else if trimmed.nonEmpty && !trimmed.startsWith("#") && !trimmed
          .startsWith(";")
      then
        section match {
          case "config" =>
            trimmed.split("=", 2).map(_.trim) match {
              case Array("harnessdir", v) => harnessDir = v
              case Array("testdir", v)    => testDir = v
              case Array("errorfile", v)  => errorFile = v
              case Array("reportfile", v) => reportFile = v
              case Array("mode", v)       => mode = v
              case Array("async", v)      => handleAsync = v == "yes"
              case Array("module", v)     => handleModule = v == "yes"
              case Array("verbose", v)    => verbose = v == "yes"
              case Array("nostrict", v)   => noStrict = v == "yes"
              case Array("strict", v)     => strict = v == "yes"
              case _                      => ()
            }
          case "features" =>
            trimmed.split("=", 2).map(_.trim) match {
              case Array(feature, "skip") => features(feature) = "skip"
              case Array(feature)         => features(feature) = ""
              case _                      => ()
            }
          case "exclude" =>
            excludes += trimmed
          case _ => ()
        }
    }

    Config(
      harnessDir = harnessDir,
      testDir = testDir,
      errorFile = errorFile,
      reportFile = reportFile,
      mode = mode,
      handleAsync = handleAsync,
      handleModule = handleModule,
      verbose = verbose,
      features = features.toMap,
      excludes = excludes.toList,
      noStrict = noStrict,
      strict = strict
    )
  }

  // =========================================================================
  // YAML Frontmatter Parsing
  // =========================================================================

  // Parse the YAML-like frontmatter from a test262 test file.
  // Frontmatter is between the first /*--- and ---*\/ markers.
  def parseFrontmatter(source: String): (TestMeta, String) = {
    val frontmatterStart = source.indexOf("/*---")
    if frontmatterStart < 0 then
      return (TestMeta("", None, None, None, Nil, Nil, None, Nil, None), source)

    val contentStart = source.indexOf("---*/", frontmatterStart)
    if contentStart < 0 then
      return (TestMeta("", None, None, None, Nil, Nil, None, Nil, None), source)

    val fmBlock = source.substring(frontmatterStart + 5, contentStart).trim
    val remaining = source.substring(contentStart + 5)

    // Parse frontmatter lines
    var description = ""
    var esid: Option[String] = None
    var es5id: Option[String] = None
    var es6id: Option[String] = None
    var includes: List[String] = Nil
    var flags: List[String] = Nil
    var negative: Option[NegativeInfo] = None
    var features: List[String] = Nil
    var info: Option[String] = None

    var currentKey = ""
    var currentList = mutable.ListBuffer.empty[String]
    var inList = false
    var inNegative = false
    var negPhase = ""
    var negType = ""
    var inInfo = false
    val infoLines = mutable.ListBuffer.empty[String]

    val lines = fmBlock.split("\n")
    var i = 0
    var reprocessCurrent = false
    while i < lines.length do {
      val line = lines(i)
      val trimmed = line.trim

      if inInfo then
        // YAML info block: only indented lines are continuation; non-indented lines exit
        if trimmed.startsWith("---") then inInfo = false
        else if trimmed.startsWith(" ") || trimmed.startsWith("\t") then {
          if trimmed.nonEmpty then infoLines += trimmed
        } else {
          // Non-indented, non-empty line — exit info mode and reprocess
          inInfo = false
          reprocessCurrent = true
        }
      else if inNegative then {
        trimmed.split(":", 2).map(_.trim) match {
          case Array("phase", v) => negPhase = v
          case Array("type", v)  => negType = v
          case _                 => ()
        }
        if !line.startsWith(" ") && !line.startsWith(
            "\t"
          ) && trimmed.nonEmpty && !trimmed.startsWith("phase") && !trimmed
            .startsWith("type")
        then {
          negative = Some(NegativeInfo(negPhase, negType))
          inNegative = false
          currentKey = ""
          reprocessCurrent = true
        }
      } else if inList then {
        if trimmed.startsWith("-") then
          currentList += trimmed
            .substring(1)
            .trim
            .stripPrefix("\"")
            .stripSuffix("\"")
        else if trimmed.startsWith("]") then {
          inList = false
          currentKey match {
            case "includes" => includes = currentList.toList
            case "flags"    => flags = currentList.toList
            case "features" => features = currentList.toList
            case _          => ()
          }
          currentList.clear()
          currentKey = ""
        } else if trimmed.contains(",") then {
          // Support [a, b] on one line
          val items = trimmed
            .stripSuffix("]")
            .split(",")
            .map(_.trim.stripPrefix("\"").stripSuffix("\"").stripPrefix("["))
          items.filter(_.nonEmpty).foreach(currentList += _)
          inList = false
          currentKey match {
            case "includes" => includes = currentList.toList
            case "flags"    => flags = currentList.toList
            case "features" => features = currentList.toList
            case _          => ()
          }
          currentList.clear()
          currentKey = ""
        } else if trimmed.nonEmpty then {
          // YAML block sequences end when the next non-list key (or the
          // frontmatter terminator) begins; they do not have a closing `]`.
          inList = false
          currentKey match {
            case "includes" => includes = currentList.toList
            case "flags"    => flags = currentList.toList
            case "features" => features = currentList.toList
            case _          => ()
          }
          currentList.clear()
          currentKey = ""
          reprocessCurrent = true
        }
      } else
        trimmed.split(":", 2).map(_.trim) match {
          case Array("description", v)                   => description = v
          case Array("esid", v)                          => esid = Some(v)
          case Array("es5id", v)                         => es5id = Some(v)
          case Array("es6id", v)                         => es6id = Some(v)
          case Array("info", _)                          => inInfo = true
          case Array(key @ ("includes" | "flags" | "features"), "") =>
            currentKey = key
            inList = true
            currentList.clear()
          case Array("includes", v) if v.startsWith("[") =>
            currentKey = "includes"
            inList = true
            currentList.clear()
            val items = v.stripPrefix("[").stripSuffix("]")
            if items.nonEmpty then {
              items
                .split(",")
                .map(_.trim.stripPrefix("\"").stripSuffix("\""))
                .foreach(currentList += _)
              inList = false
              includes = currentList.toList
              currentList.clear()
            }
          case Array("flags", v) if v.startsWith("[") =>
            currentKey = "flags"
            inList = true
            currentList.clear()
            val items = v.stripPrefix("[").stripSuffix("]")
            if items.nonEmpty then {
              items
                .split(",")
                .map(_.trim.stripPrefix("\"").stripSuffix("\""))
                .foreach(currentList += _)
              inList = false
              flags = currentList.toList
              currentList.clear()
            }
          case Array("features", v) if v.startsWith("[") =>
            currentKey = "features"
            inList = true
            currentList.clear()
            val items = v.stripPrefix("[").stripSuffix("]")
            if items.nonEmpty then {
              items
                .split(",")
                .map(_.trim.stripPrefix("\"").stripSuffix("\""))
                .foreach(currentList += _)
              inList = false
              features = currentList.toList
              currentList.clear()
            }
          case Array("negative", _) =>
            inNegative = true
          case _ => ()
        }

      // Flush a block sequence that is the final frontmatter field.
      if inList then
        currentKey match {
          case "includes" => includes = currentList.toList
          case "flags"    => flags = currentList.toList
          case "features" => features = currentList.toList
          case _          => ()
        }

      // Flush any remaining negative
      if inNegative && negPhase.nonEmpty then
        negative = Some(NegativeInfo(negPhase, negType))

      if reprocessCurrent then
        reprocessCurrent = false // reprocess the current line, don't advance i
      else i += 1
    }

    (
      TestMeta(
        description,
        esid,
        es5id,
        es6id,
        includes,
        flags,
        negative,
        features,
        info.map(_ => infoLines.mkString("\n")).filter(_.nonEmpty)
      ),
      remaining
    )
  }

  // =========================================================================
  // Harness Loading
  // =========================================================================

  /** Cache of loaded harness files */
  private val harnessCache = TrieMap.empty[String, String]

  def loadHarness(harnessDir: String, filename: String): String =
    harnessCache.getOrElseUpdate(
      filename, {
        val file = Paths.get(harnessDir, filename)
        if Files.exists(file) then new String(Files.readAllBytes(file), "UTF-8")
        else {
          System.err.println(
            s"[test262] Warning: harness file not found: $file"
          )
          ""
        }
      }
    )

  /** Get the core harness files needed for most tests */
  def getCoreHarness(harnessDir: String): String =
    loadHarness(harnessDir, "assert.js") +
      loadHarness(harnessDir, "sta.js")

  def getAsyncHarness(harnessDir: String): String =
    loadHarness(harnessDir, "doneprintHandle.js")

  // =========================================================================
  // Test Execution
  // =========================================================================

  /** Check if a test should be skipped based on config */
  def shouldSkip(
      testPath: String,
      meta: TestMeta,
      config: Config,
      absolutePath: String = ""
  ): Option[String] = {
    // Check excludes — try matching against both relative and absolute paths
    val excludedByConfig = config.excludes
      .find { exclude =>
        val normalizedExclude = exclude.stripSuffix("/")
        testPath.startsWith(normalizedExclude) ||
        (absolutePath.nonEmpty && absolutePath.startsWith(normalizedExclude)) ||
        // Also try stripping testDir from the exclude pattern
        (normalizedExclude.startsWith(config.testDir) &&
          testPath.startsWith(
            normalizedExclude.stripPrefix(config.testDir).stripPrefix("/")
          ))
      }
      .map(exclude => s"excluded by config: $exclude")
    if excludedByConfig.isDefined then return excludedByConfig

    // Check features
    val excludedByFeature = meta.features.flatMap { feature =>
      config.features.get(feature) match {
        case Some("skip") => Some(s"feature '$feature' is skipped")
        case None         =>
          if feature.startsWith("Intl.") || feature == "Intl" then
            Some(s"feature '$feature' not supported (Intl)")
          else if feature.contains("TypedArray") || feature.contains(
              "ArrayBuffer"
            )
          then Some(s"feature '$feature' not supported (TypedArrays)")
          else None
        case _ => None
      }
    }.headOption
    if excludedByFeature.isDefined then return excludedByFeature

    // Skip module tests if not handled
    if meta.flags.contains("module") && !config.handleModule then
      return Some("module tests disabled")

    // Skip async tests if not handled
    if meta.flags.contains("async") && !config.handleAsync then
      return Some("async tests disabled")

    // Skip CanBlockIsFalse tests
    if meta.flags.contains("CanBlockIsFalse") then
      return Some("CanBlockIsFalse not supported")

    None
  }

  /** Run a single test and return the result */
  def runTest(
      testPath: String,
      testDir: String,
      config: Config
  ): TestResult = {
    val startTime = System.currentTimeMillis()
    // Compute relative path by finding testDir in the absolute path
    val relativePath = {
      val normalizedAbsPath = testPath.replace('\\', '/')
      val normalizedTestDir = testDir.replace('\\', '/')
      val idx = normalizedAbsPath.indexOf(normalizedTestDir)
      if idx >= 0 then
        normalizedAbsPath
          .substring(idx + normalizedTestDir.length)
          .stripPrefix("/")
      else
        testPath
          .stripPrefix(testDir)
          .stripPrefix("/")
          .stripPrefix(File.separator)
    }

    try {
      // Read the test file
      val source = new String(Files.readAllBytes(Paths.get(testPath)), "UTF-8")
      val (meta, testCode) = parseFrontmatter(source)

      // Check if should skip
      shouldSkip(relativePath, meta, config, testPath) match {
        case Some(reason) =>
          val elapsed = System.currentTimeMillis() - startTime
          return TestResult.Skip(relativePath, reason)
        case None => ()
      }

      // Assemble the full test script
      val harnessCode = new StringBuilder()

      val isRaw = meta.flags.contains("raw")
      if !isRaw then {
        // Core harness (assert.js, sta.js)
        harnessCode.append(getCoreHarness(config.harnessDir))
        harnessCode.append("\n")

        // Additional includes
        for inc <- meta.includes do {
          harnessCode.append(loadHarness(config.harnessDir, inc))
          harnessCode.append("\n")
        }
      }

      // Raw tests must execute exactly as supplied, without harness injection.
      val isAsync = meta.flags.contains("async") && !isRaw
      if isAsync then {
        harnessCode.append(getAsyncHarness(config.harnessDir))
        harnessCode.append("\n")
      }

      if !isRaw then
        harnessCode.append(
          "var __capturedPrint = ''; function print(msg) { __capturedPrint += msg + '\\n'; }\n"
        )

      // The test code itself
      harnessCode.append(testCode)
      harnessCode.append("\n")

      val fullScript = harnessCode.toString()

      val isModule = meta.flags.contains("module")
      val variants =
        if isModule then List("strict" -> fullScript)
        else if meta.flags.contains("onlyStrict") then
          List("strict" -> ("\"use strict\";\n" + fullScript))
        else if meta.flags.contains("noStrict") || meta.flags.contains("raw") then
          List("default" -> fullScript)
        else {
          val selected = mutable.ListBuffer.empty[(String, String)]
          if config.noStrict then selected += "default" -> fullScript
          if config.strict then selected += "strict" -> ("\"use strict\";\n" + fullScript)
          selected.toList
        }

      val variantResults = variants.map { case (variant, script) =>
        val result = runWithTimeout(relativePath) {
          meta.negative match {
            case Some(NegativeInfo(phase, errorType)) =>
              runNegativeTest(relativePath, script, phase, errorType, isModule, startTime)
            case None =>
              runRegularTest(relativePath, script, isAsync, isModule, testPath, startTime)
          }
        }
        result match {
          case TestResult.Fail(path, message, elapsed) =>
            TestResult.Fail(path, s"[$variant] $message", elapsed)
          case TestResult.Error(path, message, elapsed) =>
            TestResult.Error(path, s"[$variant] $message", elapsed)
          case TestResult.Timeout(path, elapsed) =>
            TestResult.Timeout(s"$path [$variant]", elapsed)
          case other => other
        }
      }
      variantResults.find(!_.isPass).getOrElse(
        TestResult.Pass(relativePath, System.currentTimeMillis() - startTime)
      )
    } catch {
      case ex: Exception =>
        val elapsed = System.currentTimeMillis() - startTime
        TestResult.Error(
          relativePath,
          s"Runner exception: ${ex.getMessage}",
          elapsed
        )
    }
  }

  /** Run a regular (non-negative) test */
  private def runRegularTest(
      testPath: String,
      script: String,
      isAsync: Boolean,
      isModule: Boolean,
      moduleName: String,
      startTime: Long
  ): TestResult =
    Try {
      val runtime = JSRuntime()
      given ctx: JSContext = JSContext(runtime)
      val loader = new quickjs.module.FileModuleLoader(
        Option(Paths.get(moduleName).toAbsolutePath.getParent).getOrElse(Paths.get(".").toAbsolutePath)
      )
      runtime.setModuleLoader(loader)
      StdLib.initialize(ctx, Some(loader))
      initializeTest262Host(ctx)
      // Also initialize JSON and Console (needed by harness files)
      quickjs.stdlib.JSON.initialize()
      quickjs.stdlib.Console.initialize()
      // Add $DONE for async tests
      if isAsync then
        ctx.global.set(
          "$DONE",
          quickjs.value.JSValue.Native(
            quickjs.value.NativeFunction(
              "$DONE",
              (args, _) =>
                val error = if args.length > 1 then Some(args(1)) else None
                error match {
                  case Some(e) if e != quickjs.value.JSValue.Undefined =>
                    throw new RuntimeException(s"Async test failure: $e")
                  case _ =>
                    quickjs.value.JSValue.Undefined
                }
            )
          )
        )(using ctx)

      executeScript(script, ctx, isModule, moduleName)
      val elapsed = System.currentTimeMillis() - startTime
      TestResult.Pass(testPath, elapsed)
    } match {
      case Success(result) => result
      case Failure(ex)     =>
        val elapsed = System.currentTimeMillis() - startTime
        val msg = ex.getMessage
        if msg != null && msg.contains("assertion failed") then
          TestResult.Fail(testPath, msg.take(300), elapsed)
        else
          TestResult.Error(
            testPath,
            s"${ex.getClass.getSimpleName}: ${Option(msg).getOrElse("")}"
              .take(300),
            elapsed
          )
    }

  /** Run a negative test (expected to fail) */
  private def runNegativeTest(
      testPath: String,
      script: String,
      phase: String,
      errorType: String,
      isModule: Boolean,
      startTime: Long
  ): TestResult =
    val attempt = Try {
      val runtime = JSRuntime()
      given ctx: JSContext = JSContext(runtime)
      val loader = new quickjs.module.FileModuleLoader(Paths.get(".").toAbsolutePath)
      runtime.setModuleLoader(loader)
      StdLib.initialize(ctx, Some(loader))
      initializeTest262Host(ctx)
      quickjs.stdlib.JSON.initialize()
      quickjs.stdlib.Console.initialize()
      if phase == "parse" || phase == "early" then
        compileScript(script, isModule, testPath)
      else executeScript(script, ctx, isModule, testPath)
    }
    attempt match {
      case Success(_) =>
        val elapsed = System.currentTimeMillis() - startTime
        TestResult.Fail(testPath, s"Expected $errorType during $phase but no error was thrown", elapsed)
      case Failure(ex) =>
        val elapsed = System.currentTimeMillis() - startTime
        // Check error type
        val msg = Option(ex.getMessage).getOrElse("")
        // $DONOTEVALUATE() means the engine should have caught this at parse/early time but didn't
        if msg.contains("This statement should not be evaluated") then
          TestResult.Fail(
            testPath,
            s"Expected $errorType (parse/early), but engine executed code that should have been rejected",
            elapsed
          )
        else if phase == "early" || phase == "parse" then
          TestResult.Pass(testPath, elapsed)
        else if msg.contains(errorType) then
          TestResult.Pass(testPath, elapsed)
        else
          TestResult.Fail(
            testPath,
            s"Expected $errorType but got ${ex.getClass.getSimpleName}: ${msg.take(200)}",
            elapsed
          )
    }

  /** Execute a JavaScript script in the engine */
  private def compileScript(
      source: String,
      isModule: Boolean,
      moduleName: String
  ): quickjs.bytecode.BytecodeFunction = {
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = new Parser(tokens, moduleMode = isModule)
    val ast = parser.parseScript()
    val compiler = Compiler()
    if isModule then compiler.compileModule(ast, moduleName)
    else compiler.compileScript(ast)
  }

  private def executeScript(
      source: String,
      ctx: JSContext,
      isModule: Boolean,
      moduleName: String
  ): Unit = {
    val bytecode = compileScript(source, isModule, moduleName)
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)(using ctx)
    // Run microtasks for async tests
    ctx.runMicrotasks()
  }

  /** Create a test262 host object (`$262`) for a context. A fresh realm is a
    * fresh runtime/context with its own intrinsics, which gives real
    * cross-realm semantics (distinct prototypes, `instanceof` boundaries).
    */
  private def createTest262Host(ctx: JSContext): JSValue = {
    given JSContext = ctx
    val host = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype)

    def makeDetach(): quickjs.value.NativeFunction =
      quickjs.value.NativeFunction(
        "detachArrayBuffer",
        (args, hostCtx) => {
          given JSContext = hostCtx
          val buffer =
            args.lift(1).orElse(args.headOption).getOrElse(JSValue.Undefined)
          quickjs.runtime.builtins.TypedArrayBuiltins.detachArrayBuffer(buffer)
          JSValue.Undefined
        }
      )

    def evalInRealm(source: String, realmCtx: JSContext): JSValue = {
      given JSContext = realmCtx
      val tokens = Lexer(source).tokenize()
      val ast = Parser(tokens).parseScript()
      val bytecode = Compiler().compileScript(ast)
      val result = Interpreter().call(bytecode, JSValue.Undefined, Array.empty)
      realmCtx.runMicrotasks()
      result
    }

    def makeCreateRealm(): quickjs.value.NativeFunction =
      quickjs.value.NativeFunction(
        "createRealm",
        (_, _) => {
          val runtime = JSRuntime()
          val realmCtx = JSContext(runtime)
          StdLib.initialize(realmCtx)
          JSON.initialize()(using realmCtx)
          Console.initialize()(using realmCtx)
          createTest262Host(realmCtx)
        }
      )

    val evalScript = quickjs.value.NativeFunction(
      "evalScript",
      (args, hostCtx) => {
        val source =
          if args.length > 1 then args(1).toString
          else if args.nonEmpty then args(0).toString
          else ""
        evalInRealm(source, ctx)
      }
    )

    host.defineProperty(
      "detachArrayBuffer",
      JSValue.Native(makeDetach()),
      enumerable = true
    )
    host.defineProperty(
      "evalScript",
      JSValue.Native(evalScript),
      enumerable = true
    )
    host.defineProperty(
      "global",
      JSValue.Object(ctx.global),
      enumerable = true
    )
    host.defineProperty(
      "createRealm",
      JSValue.Native(makeCreateRealm()),
      enumerable = true
    )
    JSValue.Object(host)
  }

  private def initializeTest262Host(ctx: JSContext): Unit = {
    given JSContext = ctx
    val hostValue = createTest262Host(ctx)
    ctx.global.defineProperty("$262", hostValue, enumerable = false)
  }

  // =========================================================================
  // Test Enumeration
  // =========================================================================

  /** Enumerate all test files from the test directory */
  def enumerateTests(testDir: String): List[String] = {
    val dir = new File(testDir)
    if !dir.exists() then {
      System.err.println(s"[test262] Test directory not found: $testDir")
      return Nil
    }

    def walk(f: File): List[String] =
      if f.isDirectory then f.listFiles().toList.sortBy(_.getName).flatMap(walk)
      else if f.getName.endsWith(".js") then List(f.getAbsolutePath)
      else Nil

    walk(dir)
  }

  // =========================================================================
  // Main Entry Point
  // =========================================================================

  /** Run the test262 suite.
    *
    * @param configPath
    *   path to test262.conf
    * @param maxTests
    *   limit number of tests (for quick runs)
    * @param filter
    *   only run tests whose path contains this string
    * @return
    *   (stats, list of non-passing results)
    */
  def run(
      configPath: String = "test262.conf",
      maxTests: Option[Int] = None,
      filter: Option[String] = None
  ): (Stats, List[TestResult]) = {
    println(s"[test262] Parsing config: $configPath")
    val config = parseConfig(configPath)
    println(
      s"[test262] Config loaded: testDir=${config.testDir}, harnessDir=${config.harnessDir}"
    )
    println(
      s"[test262] Features: ${config.features.count(_._2 != "skip")} supported, ${config.features.count(_._2 == "skip")} skipped"
    )
    println(
      s"[test262] Mode: ${config.mode}, async=${config.handleAsync}, module=${config.handleModule}"
    )

    println(s"[test262] Enumerating tests from: ${config.testDir}")
    var allTests = enumerateTests(config.testDir)

    // Apply filter
    filter.foreach { f =>
      val countBefore = allTests.size
      allTests = allTests.filter(_.contains(f))
      println(s"[test262] Filter '$f': $countBefore -> ${allTests.size} tests")
    }

    // Apply maxTests limit
    maxTests.foreach { max =>
      if allTests.size > max then {
        println(s"[test262] Limiting to $max tests (from ${allTests.size})")
        allTests = allTests.take(max)
      }
    }

    val requestedWorkers = Integer.getInteger("quickjs.test262.workers", 8).intValue()
    val parallelism = math.max(
      1,
      math.min(requestedWorkers, Runtime.getRuntime.availableProcessors())
    )
    println(s"[test262] Running ${allTests.size} tests with $parallelism workers...")

    val resultsByIndex = Array.ofDim[TestResult](allTests.size)
    val startTime = System.currentTimeMillis()
    val workers = Executors.newFixedThreadPool(parallelism, timeoutThreadFactory)
    val completion = new ExecutorCompletionService[(Int, TestResult)](workers)
    allTests.zipWithIndex.foreach { case (testPath, index) =>
      completion.submit(new Callable[(Int, TestResult)] {
        override def call(): (Int, TestResult) =
          index -> runTest(testPath, config.testDir, config)
      })
    }

    var count = 0
    var passedCount = 0
    var failedCount = 0
    var skippedCount = 0
    try
      while count < allTests.size do {
        val (index, result) = completion.take().get()
        resultsByIndex(index) = result
        count += 1
        result match {
          case TestResult.Pass(_, _) => passedCount += 1
          case TestResult.Skip(_, _) => skippedCount += 1
          case _ => failedCount += 1
        }

        if config.verbose && !result.isPass then
          result match {
            case TestResult.Fail(path, msg, _) => println(s"  FAIL: $path - $msg")
            case TestResult.Error(path, msg, _) => println(s"  ERROR: $path - $msg")
            case _ => ()
          }

        if count % 100 == 0 then {
          val elapsed = System.currentTimeMillis() - startTime
          println(
            s"[test262] Progress: $count/${allTests.size}, $passedCount passed, $failedCount failed/error, $skippedCount skipped (${elapsed}ms)"
          )
        }
      }
    finally workers.shutdownNow()

    val results = resultsByIndex.toList

    val elapsed = System.currentTimeMillis() - startTime

    // Compute stats
    val stats = Stats(
      total = results.size,
      passed =
        results.count { case TestResult.Pass(_, _) => true; case _ => false },
      failed = results.count {
        case TestResult.Fail(_, _, _) => true; case _ => false
      },
      errors = results.count {
        case TestResult.Error(_, _, _) => true; case _ => false
      },
      skipped =
        results.count { case TestResult.Skip(_, _) => true; case _ => false },
      timeouts = results.count {
        case TestResult.Timeout(_, _) => true; case _ => false
      }
    )

    println(s"\n[test262] Results:")
    println(s"[test262] ${stats.summary}")
    println(s"[test262] Time: ${elapsed}ms")

    writeReports(config, stats, elapsed, results.toList)

    // Print failures/errors
    val nonPassing = results.filter(!_.isPass)
    if nonPassing.nonEmpty then {
      val consoleLimit = 50
      println(
        s"\n[test262] ${nonPassing.size} non-passing tests " +
          s"(showing up to $consoleLimit; complete details in ${config.errorFile}):"
      )
      nonPassing.take(consoleLimit).foreach {
        case TestResult.Fail(path, msg, _)  => println(s"  FAIL: $path")
        case TestResult.Error(path, msg, _) => println(s"  ERROR: $path - $msg")
        case TestResult.Skip(path, reason)  => () // don't print skips
        case _                              => ()
      }
    }

    (stats, nonPassing.toList)
  }

  private def writeReports(
      config: Config,
      stats: Stats,
      elapsedMs: Long,
      results: List[TestResult]
  ): Unit = {
    def describe(result: TestResult): String = result match {
      case TestResult.Pass(path, duration) => s"PASS\t$path\t${duration}ms"
      case TestResult.Fail(path, message, duration) => s"FAIL\t$path\t${duration}ms\t$message"
      case TestResult.Error(path, message, duration) => s"ERROR\t$path\t${duration}ms\t$message"
      case TestResult.Skip(path, reason) => s"SKIP\t$path\t$reason"
      case TestResult.Timeout(path, duration) => s"TIMEOUT\t$path\t${duration}ms"
    }

    val report = (s"${stats.summary}\nTime: ${elapsedMs}ms\n" + results.map(describe).mkString("\n") + "\n")
    val errors = results.collect {
      case result: TestResult.Fail => describe(result)
      case result: TestResult.Error => describe(result)
      case result: TestResult.Timeout => describe(result)
    }.mkString("\n")
    Files.write(Paths.get(config.reportFile), report.getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get(config.errorFile), (errors + (if errors.nonEmpty then "\n" else "")).getBytes(StandardCharsets.UTF_8))
  }

  // =========================================================================
  // CLI-compatible main
  // =========================================================================

  def main(args: Array[String]): Unit = {
    val configPath = if args.length > 0 then args(0) else "test262.conf"
    val maxTests = if args.length > 1 then Some(args(1).toInt) else None
    val filter = if args.length > 2 then Some(args(2)) else None

    val (stats, _) = run(configPath = configPath, maxTests = maxTests, filter = filter)
    if stats.total == 0 then sys.exit(2)
    else if stats.failed > 0 || stats.errors > 0 || stats.timeouts > 0 then sys.exit(1)
  }
} // end Test262Runner
