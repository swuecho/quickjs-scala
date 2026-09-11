package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
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
