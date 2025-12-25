package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class ObjectLiteralTest extends FunSuite:

  test("Object literal - simple object") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "var obj = {x: 1, y: 2};"

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }

  test("Object literal - property access") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = """
      |var obj = {x: 1, y: 2};
      |obj.x;
      |""".stripMargin

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }

  test("Object literal - nested object") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "var obj = {outer: {inner: 42}};"

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }
