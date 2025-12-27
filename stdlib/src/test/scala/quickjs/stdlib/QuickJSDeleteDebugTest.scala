package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class QuickJSDeleteDebugTest extends FunSuite:

  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("delete basic property") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var a = {x: 1, y: 1};
      |delete a.x;
      |("x" in a);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  test("delete string index") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""delete "abc"[100];""")
    assertEquals(result, JSValue.Bool(true))
  }

  test("delete null property throws") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var err = false;
      |try { delete null.a; } catch (e) { err = (e instanceof TypeError); }
      |err;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("delete super property in method") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var err = false;
      |try {
      |  var a = { f() { delete super.a; } };
      |  a.f();
      |} catch (e) {
      |  err = (e instanceof ReferenceError);
      |}
      |err;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }
