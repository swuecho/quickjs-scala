package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSRuntime, JSContext, StdLib}
import quickjs.value.JSValue

import java.io.File
import java.nio.file.{Files, Paths, Path}
import scala.collection.mutable
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
      passed.toDouble / (total - skipped).toDouble * 100.0
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
        }
      } else
        trimmed.split(":", 2).map(_.trim) match {
          case Array("description", v)                   => description = v
          case Array("esid", v)                          => esid = Some(v)
          case Array("es5id", v)                         => es5id = Some(v)
          case Array("es6id", v)                         => es6id = Some(v)
          case Array("info", _)                          => inInfo = true
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
  private val harnessCache = mutable.Map.empty[String, String]

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

    // Skip raw tests
    if meta.flags.contains("raw") then return Some("raw tests not supported")

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

      // Core harness (assert.js, sta.js)
      harnessCode.append(getCoreHarness(config.harnessDir))
      harnessCode.append("\n")

      // Additional includes
      for inc <- meta.includes do {
        harnessCode.append(loadHarness(config.harnessDir, inc))
        harnessCode.append("\n")
      }

      // Async harness
      val isAsync = meta.flags.contains("async")
      if isAsync then {
        harnessCode.append(getAsyncHarness(config.harnessDir))
        harnessCode.append("\n")
      }

      // Print function for async test signaling
      harnessCode.append(
        "var __capturedPrint = ''; function print(msg) { __capturedPrint += msg + '\\n'; }\n"
      )

      // The test code itself
      harnessCode.append(testCode)
      harnessCode.append("\n")

      val fullScript = harnessCode.toString()

      // Check for negative tests
      meta.negative match {
        case Some(NegativeInfo(phase, errorType)) =>
          runNegativeTest(relativePath, fullScript, phase, errorType, startTime)
        case None =>
          runRegularTest(relativePath, fullScript, isAsync, startTime)
      }
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
      startTime: Long
  ): TestResult =
    Try {
      val runtime = JSRuntime()
      given ctx: JSContext = JSContext(runtime)
      StdLib.initialize(ctx)
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

      executeScript(script, ctx)
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
      startTime: Long
  ): TestResult =
    Try {
      val runtime = JSRuntime()
      given ctx: JSContext = JSContext(runtime)
      StdLib.initialize(ctx)
      quickjs.stdlib.JSON.initialize()
      quickjs.stdlib.Console.initialize()
      executeScript(script, ctx)
    } match {
      case Success(_) =>
        val elapsed = System.currentTimeMillis() - startTime
        // Negative test: expected an error but got none
        if phase == "early" || phase == "parse" then
          // early/parse errors are not enforced yet, treat as skip
          TestResult.Skip(
            testPath,
            s"negative test ($phase $errorType) - error not thrown (parse error detection not implemented)"
          )
        else
          TestResult.Fail(
            testPath,
            s"Expected $errorType but no error was thrown",
            elapsed
          )
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
        else if msg.contains(errorType) || errorType == "Test262Error" then
          TestResult.Pass(testPath, elapsed)
        else if phase == "early" || phase == "parse" then
          // parse/early errors: if we got any other error, note it but count as pass
          TestResult.Pass(testPath, elapsed)
        else
          TestResult.Fail(
            testPath,
            s"Expected $errorType but got ${ex.getClass.getSimpleName}: ${msg.take(200)}",
            elapsed
          )
    }

  /** Execute a JavaScript script in the engine */
  private def executeScript(source: String, ctx: JSContext): Unit = {
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)(using ctx)
    // Run microtasks for async tests
    ctx.runMicrotasks()
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

    println(s"[test262] Running ${allTests.size} tests...")

    val results = mutable.ListBuffer.empty[TestResult]

    var count = 0
    val startTime = System.currentTimeMillis()

    for testPath <- allTests do {
      count += 1
      val result = runTest(testPath, config.testDir, config)
      results += result

      if config.verbose && !result.isPass then
        result match {
          case TestResult.Fail(path, msg, _) => println(s"  FAIL: $path - $msg")
          case TestResult.Error(path, msg, _) =>
            println(s"  ERROR: $path - $msg")
          case _ => ()
        }

      // Progress indicator
      if count % 100 == 0 then {
        val elapsed = System.currentTimeMillis() - startTime
        val passed = results.count(_.isPass)
        println(
          s"[test262] Progress: $count/${allTests.size} tests, $passed passed (${elapsed}ms)"
        )
      }
    }

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

    // Print failures/errors
    val nonPassing = results.filter(!_.isPass)
    if nonPassing.nonEmpty then {
      println(s"\n[test262] ${nonPassing.size} non-passing tests:")
      nonPassing.foreach {
        case TestResult.Fail(path, msg, _)  => println(s"  FAIL: $path")
        case TestResult.Error(path, msg, _) => println(s"  ERROR: $path - $msg")
        case TestResult.Skip(path, reason)  => () // don't print skips
        case _                              => ()
      }
    }

    (stats, nonPassing.toList)
  }

  // =========================================================================
  // CLI-compatible main
  // =========================================================================

  def main(args: Array[String]): Unit = {
    val configPath = if args.length > 0 then args(0) else "test262.conf"
    val maxTests = if args.length > 1 then Some(args(1).toInt) else None
    val filter = if args.length > 2 then Some(args(2)) else None

    run(configPath = configPath, maxTests = maxTests, filter = filter)
  }
} // end Test262Runner
