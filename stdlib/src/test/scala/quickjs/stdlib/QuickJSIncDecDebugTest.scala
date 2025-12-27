package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class QuickJSIncDecDebugTest extends FunSuite:

  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("inc/dec identifiers") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var a, r;
      |a = 1;
      |r = a++;
      |r === 1 && a === 2;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("inc/dec object property") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var a, r;
      |a = {x:true};
      |r = a.x++;
      |""".stripMargin + "\"\" + r + \",\" + a.x;")
    assertEquals(result, JSValue.fromString("1,2"))
  }

  test("inc/dec array element") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var a, r;
      |a = [true];
      |r = a[0]++;
      |""".stripMargin + "\"\" + r + \",\" + a[0];")
    assertEquals(result, JSValue.fromString("1,2"))
  }
