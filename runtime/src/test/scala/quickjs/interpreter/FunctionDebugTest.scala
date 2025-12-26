package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class FunctionDebugTest extends FunSuite {

  test("debug: tokenize function()") {
    val source = "function() { return 1; }"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    println(s"Tokens for '$source':")
    tokens.foreach(t => println(s"  $t"))
  }

  test("debug: parse function()") {
    val source = "function() { return 1; }"
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

  test("debug: tokenize (function() {})") {
    val source = "(function() { return 1; })"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    println(s"\nTokens for '$source':")
    tokens.foreach(t => println(s"  $t"))
  }

  test("debug: parse (function() {})") {
    val source = "(function() { return 1; })"
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
