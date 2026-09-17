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
    val parser = new Parser(tokens, moduleMode = true)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileModule(ast, name)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
    // Module bodies are compiled async; surface evaluation failures the way a
    // loader would (rejections become throws).
    quickjs.module.ModuleEvaluation.settleAndCheck(result)
    result

  private def evalScript(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  /** Compile `source` as a module and return its completion value (the last
    * expression), after driving top-level await to settlement.
    */
  private def evalModuleValue(name: String, source: String)(using
      ctx: JSContext
  ): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = new Parser(tokens, moduleMode = true)
    val ast = parser.parseScript()
    val bytecode = Compiler().compileModule(ast, name)
    val result =
      Interpreter().call(bytecode, JSValue.Undefined, Array.empty)
    quickjs.module.ModuleEvaluation.settleAndCheck(result)
    result match {
      case JSValue.Object(obj) =>
        obj.getOwnProperty("__promise") match {
          case Some(promise: JSValue.Promise)
              if promise.state == JSValue.PromiseState.Fulfilled =>
            promise.result
          case _ => JSValue.Undefined
        }
      case other => other
    }

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

    val result = evalModuleValue("<test>", 
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

    val result = evalModuleValue("<test>", 
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

    val result = evalModuleValue("<test>", 
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

    val result = evalModuleValue("<test>", 
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

    val result = evalModuleValue("<test>", 
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

  test("arbitrary string module export names round-trip") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    evalModule(
      "str-export",
      """const value = 42;
        |export { value as 'module.exports', value as "quoted" };
        |""".stripMargin
    )

    val exports = summon[JSRuntime].getModuleExports("str-export").get
    assertEquals(exports.get("module.exports"), JSValue.fromInt(42))
    assertEquals(exports.get("quoted"), JSValue.fromInt(42))
  }

  test("string module import names bind through `as`") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    evalModule(
      "str-names",
      """const inner = 7;
        |export { inner as 'module.exports' };
        |""".stripMargin
    )
    evalModule(
      "str-consumer",
      """import { 'module.exports' as inner } from "str-names";
        |export const got = inner + 1;
        |""".stripMargin
    )

    val exports = summon[JSRuntime].getModuleExports("str-consumer").get
    assertEquals(exports.get("got"), JSValue.fromInt(8))
  }

  test("export async functions and export-star-as-namespace") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    evalModule(
      "star-src",
      """export const a = 1;
        |export default 2;
        |""".stripMargin
    )
    evalModule(
      "star-main",
      """export async function f() { return 3; }
        |export * as ns from 'star-src';
        |""".stripMargin
    )

    val exports = summon[JSRuntime].getModuleExports("star-main").get
    exports.get("f") match
      case _: JSValue.Function | JSValue.Native(_) => ()
      case other => fail(s"Expected exported function, got $other")
    exports.get("ns") match
      case JSValue.Object(ns) => assertEquals(ns.get("a"), JSValue.fromInt(1))
      case other              => fail(s"Expected namespace object, got $other")
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

    val result = evalModuleValue("<test>", 
      """
        |import { first, b } from "alias";
        |first + b;
        |""".stripMargin
    )

    assertEquals(result.toNumber, 30.0)
  }
