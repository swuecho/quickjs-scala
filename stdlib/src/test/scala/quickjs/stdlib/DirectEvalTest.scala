package quickjs.stdlib

import munit.FunSuite
import quickjs.value.JSValue
import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}

class DirectEvalTest extends FunSuite:

  private def eval(source: String)(using ctx: JSContext): JSValue =
    val tokens = Lexer(source).tokenize()
    val ast = Parser(tokens).parseScript()
    val bytecode = Compiler().compileScript(ast)
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty)

  test("direct eval var declarations are visible in caller scope") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |function f() {
      |  eval("var x = 41");
      |  return x + 1;
      |}
      |f();
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(42))
  }

  test("direct eval var declarations do not leak from function to global scope") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |function f() {
      |  eval("var hidden = 1");
      |  return hidden;
      |}
      |var value = f();
      |value === 1 && typeof hidden === "undefined";
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("strict direct eval var declarations stay inside eval scope") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |function f() {
      |  "use strict";
      |  eval("var x = 1");
      |  return typeof x;
      |}
      |f();
      |""".stripMargin)
    assertEquals(result, JSValue.fromString("undefined"))
  }

  test("direct eval assignments still update caller bindings") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |function f() {
      |  var x = 1;
      |  eval("x = 3");
      |  return x;
      |}
      |f();
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(3))
  }

  test("direct eval sees caller variables and arguments object") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |function f(code) {
      |  var x = 1;
      |  return eval(code);
      |}
      |var a = 2;
      |var r1 = eval("a");
      |eval("a = 3");
      |var r2 = a;
      |a = 4;
      |var r3 = f("a");
      |f("a = 5");
      |var r4 = a;
      |var r5 = f("arguments.length", 1);
      |var r6 = f("arguments[1]", 1);
      |r1 === 2 && r2 === 3 && r3 === 4 && r4 === 5 && r5 === 2 && r6 === 1;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("direct eval returns expressions after var declarations") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |function f() {
      |  return eval("var my_var = 2; my_var;");
      |}
      |f();
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(2))
  }

  test("strict direct eval returns expressions after var declarations") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |function f() {
      |  "use strict";
      |  return eval("var my_var = 2; my_var;");
      |}
      |f();
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(2))
  }

  test("direct eval returns statement completion values") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var r1 = eval("1 + 1;");
      |var r2 = eval("if (1) 2; else 3;");
      |var r3 = eval("if (0) 2; else 3;");
      |r1 === 2 && r2 === 2 && r3 === 3;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("direct eval in nested function sees outer function variables") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |function outer() {
      |  function f(code) {
      |    return eval(code);
      |  }
      |  var a = 4;
      |  var r1 = f("a");
      |  f("a = 5");
      |  return r1 === 4 && a === 5;
      |}
      |outer();
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("direct eval hoists var declarations from nested statements") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |function f() {
      |  eval("if (true) { var x = 1; } else { var y = 2; }");
      |  return x === 1 && typeof y === "undefined";
      |}
      |f();
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("direct eval function declarations are visible in caller scope") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |function f() {
      |  eval("function g() { return 41; }");
      |  return g() + 1;
      |}
      |typeof g === "undefined" && f() === 42 && typeof g === "undefined";
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }
