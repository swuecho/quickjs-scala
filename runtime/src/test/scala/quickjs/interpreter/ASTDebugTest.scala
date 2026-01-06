package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.ast.*
import munit.*

class ASTDebugTest extends FunSuite {

  test("debug: check AST for arr[0] = 10") {
    val source = "arr[0] = 10"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    println(s"\n=== Tokens for '$source' ===")
    tokens.foreach(t => println(s"  $t"))

    val parser = Parser(tokens)
    val ast = parser.parseScript()

    println(s"\n=== AST for '$source' ===")
    ast.body.foreach { stmt =>
      println(s"  Statement: $stmt")
      stmt match
        case ExpressionStatement(expr, _) =>
          println(s"    Expression: $expr")
          expr match
            case AssignmentExpression(left, right, _) =>
              println(s"      Left: $left")
              println(s"      Right: $right")
              left match
                case MemberExpression(obj, prop, computed, _, optional) =>
                  println(s"        MemberExpression:")
                  println(s"          obj: $obj")
                  println(s"          prop: $prop")
                  println(s"          computed: $computed")
                  println(s"          optional: $optional")
                case _ =>
                  println(s"        Not a MemberExpression!")
            case _ =>
              println(s"    Not an AssignmentExpression!")
        case _ =>
          println(s"  Not an ExpressionStatement!")
    }
  }
}
