package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import munit.*

class CheckParser extends FunSuite {

  test("debug parsing var i = 0, j = 10") {
    val source = "var i = 0, j = 10"
    val lexer = Lexer(source)
    val parser = Parser(lexer.tokenize())
    val ast = parser.parseScript()

    println(s"\n=== Parsing: $source ===")
    println(s"AST: $ast")
    println(s"Number of statements: ${ast.body.length}")

    ast.body.head match {
      case quickjs.ast.VariableDeclaration(_, declarators, _) =>
        println(s"Number of declarators: ${declarators.length}")
        declarators.foreach { d =>
          val name = d.id match
            case Identifier(idName, _) => idName
            case other => other.toString
          println(s"  Declarator: ${name} = ${d.init}")
        }
      case _ => println("Not a VariableDeclaration!")
    }
  }
}
