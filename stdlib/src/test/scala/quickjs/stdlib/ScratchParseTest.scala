package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

/** Probe tests for known conformance gaps found during the test262 sweep
  * (2026-09-11 full run). Each test is a minimal reproduction; tests for
  * un-fixed bugs are marked with `assume(false, ...)` so they are reported
  * as ignored while keeping the repro ready for the day we fix them.
  *
  * Full sweep results: test262_report.txt / test262_errors.txt
  *   Total 52896 | Passed 32639 | Failed 807 | Errors 5359 | Skipped 13994 | Timeouts 97
  */
class ScratchParseTest extends FunSuite:

  private def eval(source: String)(using ctx: JSContext): JSValue =
    StdLib.initialize(ctx)
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  // ----- Fixed / passing behavior -----

  test("member assignment with throw inside function expr") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    val src = """
      |function Test262Error(message) {
      |  this.message = message || "";
      |}
      |Test262Error.prototype.toString = function () {
      |  return "Test262Error: " + this.message;
      |};
      |Test262Error.thrower = function (message) {
      |  throw new Test262Error(message);
      |};
      |""".stripMargin
    eval(src)
  }

  test("array method length descriptor (writable=false, enumerable=false, configurable=true)") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("""
      |var d = Object.getOwnPropertyDescriptor(Array.prototype.every, 'length');
      |if (d.writable !== false || d.enumerable !== false || d.configurable !== true) throw new Error('bad length descriptor: ' + JSON.stringify(d));
      |""".stripMargin)
  }

  test("strict assignment to non-writable property throws") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("""
      |'use strict';
      |var o = {};
      |Object.defineProperty(o, 'x', { value: 1, writable: false });
      |var threw = false;
      |try { o.x = 2; } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('did not throw');
      |""".stripMargin)
  }

  test("plain-object prototype is inherited by instances") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("""
      |function foo() {}
      |foo.prototype = { every: function() { return 'ok'; } };
      |var f = new foo();
      |if (typeof f.every !== 'function') throw new Error('plain proto not inherited: ' + typeof f.every);
      |""".stripMargin)
  }

  test("Array.prototype.every via .call on array-like") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("""
      |var proto = new Array(1, 2, 3);
      |function foo() {}
      |foo.prototype = proto;
      |var f = new foo();
      |f.length = 0;
      |var i = Array.prototype.every.call(f, function () {});
      |if (i !== true) throw new Error('bad ' + i);
      |""".stripMargin)
  }

  test("compound shift assignments >>>= <<= >>=") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("""
      |var a = 8; a >>>= 2; if (a !== 2) throw new Error('>>>= ' + a);
      |var b = 1; b <<= 2; if (b !== 4) throw new Error('<<= ' + b);
      |var c = 4; c >>= 2; if (c !== 1) throw new Error('>>= ' + c);
      |""".stripMargin)
  }

  test("array literal spread with trailing comma is valid") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("""
      |var a = [1, 2]; var b = [3];
      |var x = [...a, ...b,];
      |if (x.length !== 3 || x[2] !== 3) throw new Error('bad ' + JSON.stringify(x));
      |var y = [...a, 9];
      |if (y.length !== 3 || y[2] !== 9) throw new Error('bad2 ' + JSON.stringify(y));
      |""".stripMargin)
  }

  test("numeric separators in all positions") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("""
      |if (1.0e-1_0 !== 1.0e-10) throw new Error('exp sep');
      |if (1_000 !== 1000) throw new Error('int sep');
      |if (1_000.5_5 !== 1000.55) throw new Error('frac sep');
      |if (0x1_0 !== 16) throw new Error('hex sep');
      |if (0b1_0 !== 2) throw new Error('bin sep');
      |if (0o1_0 !== 8) throw new Error('oct sep');
      |if (1_2n !== 12n) throw new Error('bigint sep');
      |""".stripMargin)
  }

  test("regexp literal early errors") {
    def parseOnly(src: String): Unit = {
      val tokens = Lexer(src).tokenize()
      Parser(tokens).parseScript()
      ()
    }
    def mustFail(src: String): Unit = {
      val failed =
        try { parseOnly(src); false }
        catch case _: Throwable => true
      if !failed then throw new Error(s"expected SyntaxError: $src")
    }
    mustFail("/./G;")
    mustFail("/./gig;")
    mustFail("/{2}/;")
    mustFail("/(?<a>a)(?<a>a)/;")
    mustFail("/(?<a>.)\\k<b>/;")
    mustFail("/(?<a>.)\\k/;")
    mustFail("/\\k(?<a>.)/;")
    mustFail("/.(?<=.)?/;")
    mustFail("/.(?<=.){2}/;")
    mustFail("/.(?=.)?/u;")
    mustFail("/\\c0/u;")
    mustFail("/\\M/u;")
    mustFail("/\\1/u;")
    mustFail("/\\8/u;")
    mustFail("/[\\d-a]/u;")
    mustFail("/[--\\d]/u;")
    mustFail("/\\u{110000}/u;")
    mustFail("/\\u{1,}/u;")
    mustFail("/{/u;")
    // Valid patterns must keep parsing (parse-only: the runtime regex engine
    // has its own, still incomplete, JS/regex translation layer).
    for p <- Seq(
        "/a{2}/;", "/(?:a){2}/;", "/a{2,}/;", "/a{2,3}?/;", "/(?=a)?/;",
        "/(?<=a)/;", "/[a-d]/;", "/[-a]/;", "/[a-]/;", "/[\\d]/;",
        "/[^\\d]/;", "/[💩-💫]/u;", "/\\k<a>(?<a>x)/;", "/\\0/u;",
        "/\\u{1F600}/u;", "/\\p{L}/u;", "/\\cA/u;", "/^$/;", "/\\b\\B/;",
        "/{/;", "/}/;", "/[{}]/;"
      )
    do parseOnly(p)
  }

  test("statement early errors") {
    def parseOnly(src: String): Unit = {
      val tokens = Lexer(src).tokenize()
      Parser(tokens).parseScript()
      ()
    }
    def mustFail(src: String): Unit = {
      val failed =
        try { parseOnly(src); false }
        catch case _: Throwable => true
      if !failed then throw new Error(s"expected SyntaxError: $src")
    }
    mustFail("return 1;")
    mustFail("break;")
    mustFail("continue;")
    mustFail("while (true) function f() {}")
    mustFail("while (true) let x;")
    mustFail("if (true) class C {}")
    mustFail("if (true) async function f() {}")
    mustFail("({ __proto__: 1, __proto__: 2 });")
    mustFail("({ __proto__: 1, '__proto__': 2 });")
    mustFail("({ get a(p = 1) {} });")
    mustFail("({ set a() {} });")
    mustFail("({ set a(x, y) {} });")
    mustFail("({ default });")
    mustFail("var x = ({ bre\\u0061k } = y);")
    mustFail("class let {}")
    mustFail("class static {}")
    mustFail("class yield {}")
    mustFail("(class implements {})")
    mustFail("'use strict'; with ({}) {}")
    mustFail("'use strict'; public = 1;")
    mustFail("'use strict'; (eval) = 20;")
    mustFail("'use strict'; eval = 20;")
    mustFail("'use strict'; arguments = 20;")
    // Still-valid forms.
    parseOnly("function f() { return 1; }")
    parseOnly("while (true) { break; }")
    parseOnly("for (;;) { continue; }")
    parseOnly("switch (1) { case 1: break; }")
    parseOnly("if (true) function f() {}")
    parseOnly("({ __proto__: 1 });")
    parseOnly("({ __proto__() {}, __proto__: 1 });")
    parseOnly("({ get a() {}, set a(v) {} });")
    parseOnly("({ __proto__ });")
    parseOnly("var o = { let: 1 };")
  }

  // ----- Known bugs (repros kept, currently ignored) -----

  test("FIXME: array instance as function.prototype is not inherited".ignore) {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("""
      |function foo() {}
      |foo.prototype = new Array(1, 2, 3);
      |var f = new foo();
      |if (typeof f.every !== 'function') throw new Error('array proto not inherited: ' + typeof f.every);
      |""".stripMargin)
  }

  test("conditional expression: nested ternary in alternate") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("""
      |var value = {};
      |var x = Symbol.toStringTag && Symbol.toStringTag in value ? 'a' : 'b';
      |var y = true ? 1 : false ? 2 : 3;
      |if (y !== 1) throw new Error('nested ternary: ' + y);
      |var z = false ? 1 : true ? 2 : 3;
      |if (z !== 2) throw new Error('nested ternary alt: ' + z);
      |""".stripMargin)
  }

  test("test262 deepEqual.js harness parses") {
    assume(
      java.nio.file.Files.exists(
        java.nio.file.Paths.get("test262/harness/deepEqual.js")
      ),
      "test262 not checked out"
    )
    val source = new String(
      java.nio.file.Files.readAllBytes(
        java.nio.file.Paths.get("test262/harness/deepEqual.js")
      ),
      java.nio.charset.StandardCharsets.UTF_8
    )
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    parser.parseScript()
  }

  test("FIXME: no early error for invalid assignment target".ignore) {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("1 = 2;")
  }

  test("FIXME: no early error for malformed numeric literal".ignore) {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("var x = 1__2;")
  }

  test("FIXME: no early error for invalid regexp literal".ignore) {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    eval("var r = /(/;")
  }
