package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class GlobalScopeTest extends FunSuite:

  test("Function declaration and call") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = """
      |function add(a, b) {
      |  return a + b;
      |}
      |add(2, 3);
      |""".stripMargin

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    val hex = bytecode.bytecode.map("%02X".format(_)).mkString(" ")
    System.err.println(s"Bytecode: $hex")

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Should return 5
    assertEquals(result, JSValue.fromInt(5))
  }

  test("Function calling another function") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = """
      |function add(a, b) {
      |  return a + b;
      |}
      |function double(x) {
      |  return add(x, x);
      |}
      |double(5);
      |""".stripMargin

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Should return 10
    assertEquals(result, JSValue.fromInt(10))
  }
