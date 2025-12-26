package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class ObjectDebugTest extends FunSuite {

  test("debug: tokenize {x: 1}") {
    val source = "{x: 1}"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    println(s"Tokens for '$source':")
    tokens.foreach(t => println(s"  $t"))
  }

  test("debug: parse {x: 1}") {
    val source = "({x: 1})"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    println(s"\nTokens: $tokens")
    val parser = Parser(tokens)
    try {
      val ast = parser.parseScript()
      println(s"AST: $ast")
    } catch {
      case e: Exception =>
        println(s"Error: ${e.getMessage}")
        e.printStackTrace()
        throw e
    }
  }
}
