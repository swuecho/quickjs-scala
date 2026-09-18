package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import munit.*

/** Regression guard for parser performance.
  *
  * `Lexer.tokenize()` used to return a list-backed `Seq`, which made the
  * parser's `tokens(pos)` lookup O(n) and parsing quadratic. On a 400-line
  * harness file this made parsing take ~340ms instead of ~15ms, dominating the
  * test262 runner. These checks keep that from silently regressing.
  */
class ParserPerfRegressionTest extends FunSuite:

  private lazy val deepEqualSource: String =
    new String(
      java.nio.file.Files.readAllBytes(
        java.nio.file.Paths.get("test262/harness/deepEqual.js")
      ),
      "UTF-8"
    )

  test("tokenize returns an indexed sequence") {
    val tokens = Lexer("var x = 1 + 2;").tokenize()
    assert(
      tokens.isInstanceOf[scala.collection.IndexedSeq[_]],
      s"tokens must support O(1) indexing, got ${tokens.getClass}"
    )
  }

  test("parsing a large harness file stays fast") {
    assume(
      java.nio.file.Files.exists(
        java.nio.file.Paths.get("test262/harness/deepEqual.js")
      ),
      "test262 not checked out"
    )
    def parseOnce(): Unit = {
      val tokens = Lexer(deepEqualSource).tokenize()
      Parser(tokens).parseScript()
      ()
    }
    parseOnce() // warmup
    val start = System.nanoTime()
    var i = 0
    while i < 10 do { parseOnce(); i += 1 }
    val elapsedMs = (System.nanoTime() - start) / 1e6
    // Currently ~150ms for 10 parses; allow generous headroom for slow CI.
    assert(
      elapsedMs < 5000,
      f"parsing deepEqual.js 10x took $elapsedMs%.0fms, expected < 5000ms"
    )
  }

  test("compiling a large function body stays fast") {
    // Every `try`/`catch` statement is a block scope/jump-patching exercise.
    // Two quadratic costs used to live here: recomputing bytecode offsets by
    // folding over all instructions at each patch site, and never leaving the
    // catch clause's compile-time block scope (so every lookup scanned every
    // previous catch). 1,800 statements took ~22s; this guards against both.
    val source = "function f() {\n" + ("try {} catch (e) {}\n" * 3000) + "}\nf();"
    def compileOnce(): Unit = {
      val ast = Parser(Lexer(source).tokenize()).parseScript()
      Compiler().compileScript(ast)
      ()
    }
    compileOnce() // warmup
    val start = System.nanoTime()
    compileOnce()
    val elapsedMs = (System.nanoTime() - start) / 1e6
    assert(
      elapsedMs < 10000,
      f"compiling 3000 try/catch statements took $elapsedMs%.0fms, expected < 10000ms"
    )
  }
