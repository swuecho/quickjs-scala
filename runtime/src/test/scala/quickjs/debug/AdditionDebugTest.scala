package quickjs.debug

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class AdditionDebugTest extends FunSuite:
  test("Debug simple 1") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "1"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    println(s"Source: $source")
    println(s"Tokens: ${tokens.map(_.toString).mkString(", ")}")

    val parser = Parser(tokens)
    val ast = parser.parseScript()
    println(s"AST: $ast")

    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
    println(s"Result: $result")
  }

  test("Debug 1 + 2") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "1 + 2"
    println(s"\nSource: $source")

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    println(s"Tokens: ${tokens.map(_.toString).mkString(", ")}")

    val parser = Parser(tokens)
    val ast = parser.parseScript()
    println(s"AST: $ast")

    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
    println(s"Result: $result")

    assertEquals(result, JSValue.fromInt(3))
  }
