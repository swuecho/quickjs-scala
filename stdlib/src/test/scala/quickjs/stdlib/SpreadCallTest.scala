package quickjs.stdlib

import munit.FunSuite
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue

class SpreadCallTest extends FunSuite:

  private def eval(source: String)(using JSContext): JSValue =
    val tokens = Lexer(source).tokenize()
    val ast = Parser(tokens).parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty)

  private def freshContext(): JSContext =
    given rt: JSRuntime = JSRuntime()
    val ctx = JSContext(rt)
    StdLib.initialize(ctx)
    ctx

  test("regular function calls expand spread array arguments") {
    given JSContext = freshContext()
    val result = eval("""
      |function f(a, b, c) { return a + b + c; }
      |f(1, ...[2, 3]);
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(6))
  }

  test("method calls with spread preserve this binding") {
    given JSContext = freshContext()
    val result = eval("""
      |var obj = {
      |  base: 10,
      |  add: function(a, b) { return this.base + a + b; }
      |};
      |obj.add(...[1, 2]);
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(13))
  }

  test("new expressions expand spread constructor arguments") {
    given JSContext = freshContext()
    val result = eval("""
      |function Point(x, y) { this.x = x; this.y = y; }
      |var p = new Point(...[4, 5]);
      |p.x * 10 + p.y;
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(45))
  }

  test("optional calls expand spread arguments when callable") {
    given JSContext = freshContext()
    val result = eval("""
      |function f(a, b) { return a * b; }
      |f?.(...[6, 7]);
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(42))
  }

  test("optional calls with spread return undefined for nullish callee") {
    given JSContext = freshContext()
    val result = eval("""
      |var f = null;
      |f?.(...[1, 2]);
      |""".stripMargin)
    assertEquals(result, JSValue.Undefined)
  }

  test("spread call arguments expand strings") {
    given JSContext = freshContext()
    val result = eval("""
      |function f(a, b) { return a + b; }
      |f(..."ab");
      |""".stripMargin)
    assertEquals(result, JSValue.fromString("ab"))
  }

  test("QuickJS eval2 spread call shape") {
    given JSContext = freshContext()
    val result = eval("""
      |var g_call_count = 0;
      |var f1 = new Function("eval", "eval(1, 2)");
      |var f2 = new Function("eval", "eval(...[1, 2])");
      |function g(a, b) {
      |  if (a === 1 && b === 2) g_call_count++;
      |}
      |f1(g);
      |f2(g);
      |g_call_count;
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(2))
  }
