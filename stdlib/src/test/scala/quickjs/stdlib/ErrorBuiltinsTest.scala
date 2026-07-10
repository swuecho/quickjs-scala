package quickjs.stdlib

import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class ErrorBuiltinsTest extends FunSuite:

  private def eval(source: String): JSValue =
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val tokens = Lexer(source).tokenize()
    val ast = Parser(tokens).parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val result = Interpreter().call(bytecode, JSValue.Undefined, Array.empty)
    ctx.runMicrotasks()
    result

  test("EvalError and URIError constructors inherit from Error") {
    val result = eval("""
      |var e = new EvalError("bad eval");
      |var u = URIError("bad uri");
      |e.name === "EvalError" &&
      |e.message === "bad eval" &&
      |e instanceof EvalError &&
      |e instanceof Error &&
      |u.name === "URIError" &&
      |u.message === "bad uri" &&
      |u instanceof URIError &&
      |u instanceof Error &&
      |Error.prototype.toString.call(u) === "URIError: bad uri";
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Error constructors support cause option") {
    val result = eval("""
      |var e = new Error("outer", { cause: 42 });
      |var t = new TypeError("typed", { cause: "inner" });
      |e.cause === 42 && t.cause === "inner";
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("AggregateError stores iterable errors and inherits from Error") {
    val result = eval("""
      |var e = new AggregateError([1, 2], "many", { cause: "root" });
      |e.name === "AggregateError" &&
      |e.message === "many" &&
      |e.cause === "root" &&
      |e.errors.length === 2 &&
      |e.errors[0] === 1 &&
      |e.errors[1] === 2 &&
      |e instanceof AggregateError &&
      |e instanceof Error;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("AggregateError accepts call form and string iterables") {
    val result = eval("""
      |var e = AggregateError("ab", "letters");
      |e.message === "letters" &&
      |e.errors.length === 2 &&
      |e.errors[0] === "a" &&
      |e.errors[1] === "b" &&
      |e instanceof AggregateError;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("AggregateError rejects non-iterable undefined errors") {
    val result = eval("""
      |var ok = false;
      |try {
      |  new AggregateError(undefined, "bad");
      |} catch (e) {
      |  ok = e instanceof TypeError;
      |}
      |ok;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Promise.any rejects with AggregateError instance") {
    val result = eval("""
      |var ok = false;
      |Promise.any([Promise.reject("first"), Promise.reject("second")])
      |  .catch(function(e) {
      |    ok = e instanceof AggregateError &&
      |      e instanceof Error &&
      |      e.message === "All promises were rejected" &&
      |      e.errors.length === 2 &&
      |      e.errors[0] === "first" &&
      |      e.errors[1] === "second";
      |  });
      |__runMicrotasks();
      |ok;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }
