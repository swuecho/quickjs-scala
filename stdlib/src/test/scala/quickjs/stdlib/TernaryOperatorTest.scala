package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class TernaryOperatorTest extends FunSuite {

  private def eval(source: String)(using JSContext): JSValue = {
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    println(s"Tokens: $tokens")
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    println(s"AST: $ast")
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    println(s"Bytecode length: ${bytecode.bytecode.length}")
    println(s"Bytecode (hex): ${bytecode.bytecode.map("%02X".format(_)).mkString(" ")}")
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)
  }

  test("ternary: simple true condition") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("true ? 1 : 0")
    assertEquals(result, JSValue.fromInt(1))
  }

  test("ternary: simple false condition") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("false ? 1 : 0")
    assertEquals(result, JSValue.fromInt(0))
  }

  test("ternary: with complex expressions") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("(1 + 2) > 2 ? 10 : 20")
    assertEquals(result, JSValue.fromInt(10))
  }

  test("ternary: with expressions") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Test with various expressions
    val result1 = eval("1 > 0 ? 10 : 20")
    assertEquals(result1, JSValue.fromInt(10))

    val result2 = eval("0 > 1 ? 10 : 20")
    assertEquals(result2, JSValue.fromInt(20))
  }

  test("ternary: with variables") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Note: Can't test variables yet as they're not fully supported
    // This will be added later
  }
}
