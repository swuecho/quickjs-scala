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

  test("direct eval resolves names through active with scopes") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var o1 = { x: "o1", y: "o1" };
      |var x = "local";
      |eval('var z="var_obj";');
      |var ok = z === "var_obj";
      |with (o1) {
      |  ok = ok && x === "o1";
      |  ok = ok && eval("x") === "o1";
      |  var f = function () {
      |    o2 = { x: "o2" };
      |    with (o2) {
      |      ok = ok && x === "o2";
      |      ok = ok && y === "o1";
      |      ok = ok && z === "var_obj";
      |      ok = ok && eval("x") === "o2";
      |      ok = ok && eval("y") === "o1";
      |      ok = ok && eval("z") === "var_obj";
      |      ok = ok && eval('eval("x")') === "o2";
      |    }
      |  };
      |  f();
      |}
      |ok;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("direct eval with scopes inside function-created closures") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |function test_with() {
      |  var o1 = { x: "o1", y: "o1" };
      |  var x = "local";
      |  eval('var z="var_obj";');
      |  var ok = z === "var_obj" ? "ok" : "z";
      |  with (o1) {
      |    if (ok === "ok" && !(x === "o1")) ok = "outer x";
      |    if (ok === "ok" && !(eval("x") === "o1")) ok = "outer eval x";
      |    var f = function () {
      |      o2 = { x: "o2" };
      |      with (o2) {
      |        if (ok === "ok" && !(x === "o2")) ok = "inner x";
      |        if (ok === "ok" && !(y === "o1")) ok = "inner y";
      |        if (ok === "ok" && !(z === "var_obj")) ok = "inner z";
      |        if (ok === "ok" && !(eval("x") === "o2")) ok = "inner eval x";
      |        if (ok === "ok" && !(eval("y") === "o1")) ok = "inner eval y";
      |        if (ok === "ok" && !(eval("z") === "var_obj")) ok = "inner eval z";
      |        if (ok === "ok" && !(eval('eval("x")') === "o2")) ok = "nested eval x";
      |      }
      |    };
      |    f();
      |  }
      |  return ok;
      |}
      |test_with();
      |""".stripMargin)
    assertEquals(result, JSValue.fromString("ok"))
  }

  test("named function expression name is read-only and private") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var f = function myfunc() {
      |  myfunc = 1;
      |  return myfunc;
      |};
      |f() === f && typeof myfunc === "undefined";
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("named function expression name is captured by nested arrows") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var f = function myfunc() {
      |  myfunc = 1;
      |  (() => { myfunc = 1; })();
      |  return myfunc;
      |};
      |f() === f;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("direct eval cannot overwrite named function expression name") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var f = function myfunc() {
      |  eval("myfunc = 1");
      |  return myfunc;
      |};
      |f() === f;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("strict named function expression name assignment throws") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var direct = false;
      |var arrow = false;
      |var viaEval = false;
      |
      |try {
      |  (function myfunc() { "use strict"; myfunc = 1; })();
      |} catch (e) {
      |  direct = e instanceof TypeError;
      |}
      |
      |try {
      |  (function myfunc() { "use strict"; (() => { myfunc = 1; })(); })();
      |} catch (e) {
      |  arrow = e instanceof TypeError;
      |}
      |
      |try {
      |  (function myfunc() { "use strict"; eval("myfunc = 1"); })();
      |} catch (e) {
      |  viaEval = e instanceof TypeError;
      |}
      |
      |direct && arrow && viaEval;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("direct eval in parameter defaults uses argument scope before body vars") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var c = "global";
      |
      |var f = (a = eval("var c = 1"), probe = () => c) => {
      |  var c = 2;
      |  return c === 2 && probe() === 1;
      |};
      |var r1 = f();
      |
      |f = function f(a = eval("var c = 1"), b = c, probe = () => c) {
      |  return b === 1 && c === 1 && probe() === 1;
      |};
      |var r2 = f();
      |
      |f = function f(a, b = c, probe = () => c) {
      |  eval("var c = 1");
      |  return c === 1 && b === "global" && probe() === "global";
      |};
      |var r3 = f();
      |
      |r1 && r2 && r3 && c === "global";
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("direct eval in parameter defaults can shadow arguments binding") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var f = function(a = eval("1"), b = arguments[0]) { return b; };
      |var r1 = f(12) === 12;
      |
      |f = function(a, b = arguments[0]) { return b; };
      |var r2 = f(12) === 12;
      |
      |f = function(a = eval("var arguments = 1"), probe = () => arguments) {
      |  var arguments = 2;
      |  return arguments === 2 && probe() === 1;
      |};
      |r1 && r2 && f();
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("indirect eval var declarations create configurable global properties") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var captured;
      |(1, eval)("var gvar1");
      |gvar1 = 1;
      |Object.defineProperty(globalThis, "gvar1", { writable: false });
      |gvar1 = 2;
      |var ok = gvar1 === 1;
      |
      |Object.defineProperty(globalThis, "gvar1", {
      |  get: function() { return "hello"; },
      |  set: function(v) { captured = v; },
      |  configurable: true
      |});
      |ok = ok && gvar1 === "hello";
      |gvar1 = 3;
      |ok = ok && captured === 3;
      |
      |Object.defineProperty(globalThis, "gvar1", {
      |  value: 4,
      |  writable: true,
      |  configurable: true
      |});
      |ok = ok && gvar1 === 4;
      |gvar1 = 6;
      |ok = ok && gvar1 === 6;
      |delete gvar1;
      |ok = ok && typeof gvar1 === "undefined";
      |ok;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }
