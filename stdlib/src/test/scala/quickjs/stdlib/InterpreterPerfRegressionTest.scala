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
    val source = "var s = 0; for (var i = 0; i < 500000; i++) { s += i; } s"
    val tokens = Lexer(source).tokenize()
    val ast = Parser(tokens).parseScript()
    val bytecode = Compiler().compileScript(ast)
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty) // warmup
    val start = System.nanoTime()
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty)
    val elapsedMs = (System.nanoTime() - start) / 1e6
    // ~400ms with JIT; interpreted would be several seconds.
    assert(
      elapsedMs < 5000,
      f"500k-iteration loop took $elapsedMs%.0fms, expected < 5000ms"
    )
  }
