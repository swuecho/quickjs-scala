package quickjs.parser

import quickjs.lexer.Lexer
import quickjs.ast.*
import munit.*

class DebugFunctionCall extends FunSuite {

  test("debug function call parsing") {
    val source = "foo(1, 2)"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    println(s"\n=== Source: $source ===")
    println(s"Statements: ${script.body.length}")
    println(s"AST: ${script.body.head}")

    val stmt = script.body.head.asInstanceOf[ExpressionStatement]
    val call = stmt.expression.asInstanceOf[CallExpression]

    println(s"Function: ${call.callee}")
    println(s"Arguments: ${call.arguments.length}")
    call.arguments.zipWithIndex.foreach { case (arg, i) =>
      println(s"  Arg $i: $arg")
    }
  }
}
