package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class QuickJSModuleTest extends FunSuite:

  private def evalModule(name: String, source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileModule(ast, name)
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  private def evalScript(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("import default and named exports") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    evalModule(
      "m1",
      """
        |export const x = 1;
        |export function add(a, b) { return a + b; }
        |export default 5;
        |""".stripMargin
    )

    val result = evalScript(
      """
        |import { x, add as plus } from "m1";
        |import d from "m1";
        |plus(x, d);
        |""".stripMargin
    )

    assertEquals(result.toNumber, 6.0)
  }

  test("export star and namespace import") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    evalModule(
      "base",
      """
        |export const a = 2;
        |export const b = 3;
        |export default 99;
        |""".stripMargin
    )

    evalModule(
      "reexp",
      """
        |export * from "base";
        |export const extra = 4;
        |""".stripMargin
    )

    val result = evalScript(
      """
        |import * as ns from "reexp";
        |ns.a + ns.b + ns.extra;
        |""".stripMargin
    )

    assertEquals(result.toNumber, 9.0)
  }

  test("re-export named specifiers") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    evalModule(
      "src",
      """
        |export const a = 10;
        |export const b = 20;
        |""".stripMargin
    )

    evalModule(
      "alias",
      """
        |export { a as first, b } from "src";
        |""".stripMargin
    )

    val result = evalScript(
      """
        |import { first, b } from "alias";
        |first + b;
        |""".stripMargin
    )

    assertEquals(result.toNumber, 30.0)
  }
