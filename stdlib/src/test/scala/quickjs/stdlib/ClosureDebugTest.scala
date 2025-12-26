package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class ClosureDebugTest extends FunSuite:

  /** Helper to evaluate code and return result */
  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)


  test("debug: simple closure") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var x = 10;
      |  function g() {
      |    return x;
      |  }
      |  return g();
      |})()
      |""".stripMargin)

    println(s"Result: $result, expected: 10")
    assertEquals(result, JSValue.fromInt(10))
  }

  test("debug: closure with parameter") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  function g(d) {
      |    return d;
      |  }
      |  return g(4);
      |})()
      |""".stripMargin)

    println(s"Result: $result, expected: 4")
    assertEquals(result, JSValue.fromInt(4))
  }


  test("debug: nested closure capture - ONLY THIS TEST") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var x = 10;
      |  function g(d) {
      |    function h() {
      |      return d + x;
      |    }
      |    return h();
      |  }
      |  return g(4);
      |})()
      |""".stripMargin)

    println(s"Result: $result, expected: 14")
    assertEquals(result, JSValue.fromInt(14))
  }
