package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class QuickJSModuleTest extends FunSuite:

  private def evalModule(name: String, source: String)(using
      JSContext
  ): JSValue =
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
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
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

  test("import.meta returns stable null-prototype module object") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    evalModule(
      "meta",
      """
        |import.meta.answer = 42;
        |export const same = import.meta === import.meta;
        |export const answer = import.meta.answer;
        |export const nullProto = Object.getPrototypeOf(import.meta) === null;
        |""".stripMargin
    )

    val result = evalScript(
      """
        |import { same, answer, nullProto } from "meta";
        |same && nullProto && answer === 42;
        |""".stripMargin
    )

    assertEquals(result, JSValue.Bool(true))
  }

  test("import.meta is rejected outside module compilation") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    intercept[RuntimeException] {
      evalScript("import.meta")
    }
  }

  test("dynamic import resolves module namespace object") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    evalModule(
      "dynamic-target",
      """
        |export const answer = 42;
        |export default 7;
        |""".stripMargin
    )

    val result = evalScript(
      """
        |var seen = [];
        |var p = import("dynamic-target");
        |seen.push(typeof p.then);
        |p.then(function(ns) {
        |  seen.push(ns.answer);
        |  seen.push(ns.default);
        |});
        |seen;
        |""".stripMargin
    )

    result match
      case JSValue.JSArrayVal(arr) =>
        assertEquals(arr.get(0), JSValue.fromString("function"))
        assertEquals(arr.get(1), JSValue.fromInt(42))
        assertEquals(arr.get(2), JSValue.fromInt(7))
      case other => fail(s"Expected array result, got $other")
  }

  test("dynamic import rejects promise for missing module") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = evalScript(
      """
        |var seen = [];
        |import("missing-module").catch(function(e) {
        |  seen.push(e.name);
        |});
        |seen;
        |""".stripMargin
    )

    result match
      case JSValue.JSArrayVal(arr) =>
        assertEquals(arr.get(0), JSValue.fromString("Error"))
      case other => fail(s"Expected array result, got $other")
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
