package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

/** Regression guard for interpreter throughput.
  *
  * `BytecodeLoop.run` used to be a single ~100KB method that exceeded
  * HotSpot's method-size limit, so the hot dispatch loop was never
  * JIT-compiled (~1M instructions/sec). It is now split into per-group
  * methods that C2 compiles; a tight loop runs ~15x faster. If a future change
  * re-merges the dispatch (or otherwise disables JIT), this test fails.
  */
class InterpreterPerfRegressionTest extends FunSuite:

  test("tight loop throughput") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)
    val source = "var s = 0; for (var i = 0; i < 1000000; i++) { s += i; } s"
    val tokens = Lexer(source).tokenize()
    val ast = Parser(tokens).parseScript()
    val bytecode = Compiler().compileScript(ast)
    // Warm up enough for the JIT compiler to finish, then take the best of
    // three measured runs. A single warmup call can still be measured before
    // C2 completes when the whole test suite is running in parallel.
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty)
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty)
    System.gc()
    // Measure CPU time, not wall time: suites run in parallel and wall time
    // would be inflated by contention while CPU time still distinguishes a
    // JIT-compiled dispatch (~0.3s) from an interpreted one (several seconds).
    val bean = java.lang.management.ManagementFactory.getThreadMXBean
    val cpuSupported = bean.isCurrentThreadCpuTimeSupported
    var bestMs = Double.MaxValue
    var run = 0
    while run < 3 && bestMs > 1000 do {
      val startCpu = if cpuSupported then bean.getCurrentThreadCpuTime else 0L
      val startWall = System.nanoTime()
      Interpreter().call(bytecode, JSValue.Undefined, Array.empty)
      val elapsedMs =
        if cpuSupported then (bean.getCurrentThreadCpuTime - startCpu) / 1e6
        else (System.nanoTime() - startWall) / 1e6
      bestMs = math.min(bestMs, elapsedMs)
      run += 1
    }
    assert(
      bestMs < 3000,
      f"1M-iteration loop took $bestMs%.0fms CPU, expected < 3000ms"
    )
  }
