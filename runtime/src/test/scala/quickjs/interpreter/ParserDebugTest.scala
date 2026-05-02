package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.ast.*
import munit.*

class ParserDebugTest extends FunSuite {

  test("debug: check if arr[0] = 10 is parsed as assignment") {
    val source = "arr[0] = 10"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)

    println(s"\n=== Parsing '$source' ===")
    println(s"Tokens: ${tokens.take(10).mkString(", ")}")

    val ast = parser.parseScript()

    println(s"\n=== Statements found: ${ast.body.length} ===")
    for (stmt, i) <- ast.body.zipWithIndex do
      println(s"Statement $i: $stmt")

    // Should be exactly 1 statement (AssignmentExpression wrapped in ExpressionStatement)
    assert(ast.body.length == 1, s"Expected 1 statement, got ${ast.body.length}")

    val stmt = ast.body.head
    stmt match
      case exprStmt: ExpressionStatement =>
        exprStmt.expression match
          case assign: AssignmentExpression =>
            println(s"\n✓ Correctly parsed as AssignmentExpression")
            println(s"  Left: ${assign.left}")
            println(s"  Right: ${assign.right}")
          case _ =>
            assert(false, "Should be AssignmentExpression")
      case _ =>
        assert(false, "Should be ExpressionStatement")
  }
}
