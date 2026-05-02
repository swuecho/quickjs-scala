package quickjs.stdlib

import munit.FunSuite

/** Test262 conformance test suite for QuickJS-Scala.
  *
  * Runs the ECMAScript test262 test suite and reports results. The test262 test
  * suite must be available at the path specified in test262.conf.
  *
  * By default this runs a small smoke-test subset. To run more tests:
  *   - Increase maxTests
  *   - Use a filter for specific language features
  *   - Remove the maxTests limit entirely (WARNING: ~53,000 tests)
  */
class Test262Test extends FunSuite:

  // =========================================================================
  // Configuration
  // =========================================================================

  /** Path to test262.conf relative to project root */
  val configPath = "test262.conf"

  /** Check if test262 test suite is available */
  private def test262Available: Boolean =
    java.nio.file.Files.exists(java.nio.file.Paths.get("test262", "test"))

  /** Run test262 suite, or skip if not available */
  private def runIfAvailable(
      testName: String,
      filter: String,
      maxTests: Int = 50
  ): Unit =
    if !test262Available then
      println(
        s"[test262] SKIP: test262/test directory not found — clone test262 to enable ($testName)"
      )
      // Test passes (skip) — don't fail when test262 isn't available
    else
      val (stats, failures) = Test262Runner.run(
        configPath = configPath,
        maxTests = Some(maxTests),
        filter = Some(filter)
      )
      println(s"\n=== test262 $testName results ===")
      println(stats.summary)
      if failures.nonEmpty then
        failures.foreach {
          case Test262Runner.TestResult.Fail(path, msg, _) =>
            println(s"  FAIL: $path")
          case Test262Runner.TestResult.Error(path, msg, _) =>
            println(s"  ERROR: $path - $msg")
          case _ => ()
        }
      assert(
        stats.passed > 0,
        s"[$testName] Expected at least 1 passing test, got ${stats.passed} (${stats.summary})"
      )

  // =========================================================================
  // Smoke test - runs a small subset to verify the infrastructure works
  // =========================================================================

  test("test262 smoke test - built-ins/Array/isArray") {
    runIfAvailable("Array/isArray", "built-ins/Array/isArray")
  }

  test("test262 smoke test - built-ins/Object/assign") {
    runIfAvailable("Object/assign", "built-ins/Object/assign")
  }

  test("test262 smoke test - built-ins/Math") {
    runIfAvailable("Math", "built-ins/Math")
  }

  test("test262 smoke test - language/literals") {
    runIfAvailable("language/literals", "language/literals")
  }

  // =========================================================================
  // Feature suites — features we claim to support
  // =========================================================================

  test("test262 - built-ins/Symbol") {
    runIfAvailable("Symbol", "built-ins/Symbol", maxTests = 100)
  }

  test("test262 - built-ins/BigInt") {
    runIfAvailable("BigInt", "built-ins/BigInt", maxTests = 50)
  }

  test("test262 - built-ins/Map") {
    runIfAvailable("Map", "built-ins/Map", maxTests = 50)
  }

  test("test262 - built-ins/Set") {
    runIfAvailable("Set", "built-ins/Set", maxTests = 50)
  }

  test("test262 - built-ins/WeakMap") {
    runIfAvailable("WeakMap", "built-ins/WeakMap", maxTests = 30)
  }

  test("test262 - built-ins/WeakSet") {
    runIfAvailable("WeakSet", "built-ins/WeakSet", maxTests = 30)
  }

  test("test262 - built-ins/Promise") {
    runIfAvailable("Promise", "built-ins/Promise", maxTests = 50)
  }

  test("test262 - built-ins/Reflect") {
    runIfAvailable("Reflect", "built-ins/Reflect", maxTests = 50)
  }
