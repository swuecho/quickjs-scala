package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

/** Focused tests for Error stack behavior on throw. */
class QuickJSErrorStackTest extends FunSuite:

  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  private def assertJS(
      actual: JSValue,
      expected: JSValue,
      hint: String = ""
  ): Unit =
    if actual != expected then
      val msg = if hint.nonEmpty then s" ($hint)" else ""
      fail(s"assertion failed: got |$actual|, expected |$expected|$msg")

  test("throw attaches stack to Error objects missing stack") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |(function() {
      |  var err = new Error("boom");
      |  delete err.stack;
      |  try {
      |    throw err;
      |  } catch (e) {
      |    return (typeof e.stack === "string") && e.stack.length > 0;
      |  }
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromBoolean(true), "stack added on throw")
  }

  test("throw does not attach stack to non-Error values") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |(function() {
      |  var obj = { a: 1 };
      |  try {
      |    throw obj;
      |  } catch (e) {
      |    return e.stack === undefined;
      |  }
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromBoolean(true), "no stack for non-Error")
  }
