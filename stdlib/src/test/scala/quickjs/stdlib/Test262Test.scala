package quickjs.stdlib

import munit.FunSuite

/** Test262 conformance test suite for QuickJS-Scala.
  *
  * Runs the ECMAScript test262 test suite and reports results.
  * The test262 test suite must be available at the path specified in test262.conf.
  *
  * By default this runs a small smoke-test subset. To run more tests:
  * - Increase maxTests
  * - Use a filter for specific language features
  * - Remove the maxTests limit entirely (WARNING: ~53,000 tests)
  */
class Test262Test extends FunSuite:

  // =========================================================================
  // Configuration
  // =========================================================================

  /** Path to test262.conf relative to project root */
  val configPath = "test262.conf"

  /** Max number of tests to run (None = all, but very slow) */
  val maxTests: Option[Int] = Some(500)

  /** Only run tests whose path contains this string */
  val filter: Option[String] = None

  // =========================================================================
  // Smoke test - runs a small subset to verify the infrastructure works
  // =========================================================================

  test("test262 smoke test - built-ins/Array/isArray") {
    val (stats, failures) = Test262Runner.run(
      configPath = configPath,
      maxTests = Some(50),
      filter = Some("built-ins/Array/isArray")
    )
    println(s"\n=== test262 Array/isArray results ===")
    println(stats.summary)
    if failures.nonEmpty then
      failures.foreach {
        case Test262Runner.TestResult.Fail(path, msg, _) =>
          println(s"  FAIL: $path")
        case Test262Runner.TestResult.Error(path, msg, _) =>
          println(s"  ERROR: $path - $msg")
        case _ => ()
      }
    assert(stats.passed > 0, s"Expected at least 1 passing test, got ${stats.passed}")
  }

  test("test262 smoke test - built-ins/Object/assign") {
    val (stats, failures) = Test262Runner.run(
      configPath = configPath,
      maxTests = Some(50),
      filter = Some("built-ins/Object/assign")
    )
    println(s"\n=== test262 Object/assign results ===")
    println(stats.summary)
    assert(stats.passed > 0, s"Expected at least 1 passing test, got ${stats.passed}")
  }

  test("test262 smoke test - built-ins/Math") {
    val (stats, failures) = Test262Runner.run(
      configPath = configPath,
      maxTests = Some(50),
      filter = Some("built-ins/Math")
    )
    println(s"\n=== test262 Math results ===")
    println(stats.summary)
    assert(stats.passed > 0, s"Expected at least 1 passing test, got ${stats.passed}")
  }

  test("test262 smoke test - language/literals") {
    val (stats, failures) = Test262Runner.run(
      configPath = configPath,
      maxTests = Some(50),
      filter = Some("language/literals")
    )
    println(s"\n=== test262 language/literals results ===")
    println(stats.summary)
    assert(stats.passed > 0, s"Expected at least 1 passing test, got ${stats.passed}")
  }
