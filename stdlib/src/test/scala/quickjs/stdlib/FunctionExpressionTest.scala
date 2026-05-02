package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSRuntime, JSContext, StdLib}
import quickjs.value.JSValue
import munit.*

class FunctionExpressionTest extends FunSuite:

  test("Array.map with simple function expression") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val source = """
      |var arr = [1, 2, 3];
      |var result = arr.map(function(x) { return x * 2; });
      |result
      |""".stripMargin

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    result match
      case arrVal: JSValue.JSArrayVal =>
        val arr = arrVal.value
        assertEquals(arr.getLength, 3)
        assertEquals(arr.get(0), JSValue.fromInt(2))
        assertEquals(arr.get(1), JSValue.fromInt(4))
        assertEquals(arr.get(2), JSValue.fromInt(6))
      case _ =>
        fail(s"Expected array but got $result")
  }

  test("Simple IIFE with function expression") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val source = """
      |(function() {
      |  return 42;
      |})()
      |""".stripMargin

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.fromInt(42))
  }
