package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.FunSuite

/** Regression tests for the conformance fixes made during the September 2026
  * test262 sweep. Each test is a minimal reproduction of a root cause behind a
  * large cluster of failing test262 tests.
  */
class ConformanceRegressionTest extends FunSuite:

  private def eval(source: String)(using ctx: JSContext): JSValue =
    StdLib.initialize(ctx)
    val tokens = Lexer(source).tokenize()
    val ast = Parser(tokens).parseScript()
    val bytecode = Compiler().compileScript(ast)
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty)

  private def assertEval(source: String, expected: String): Unit = {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    val out = eval(s"""
      |function assertEquals(actual, expected, label) {
      |  if (actual !== expected) throw new Error(label + ': got ' + actual + ', expected ' + expected);
      |}
      |$source
      |""".stripMargin)
  }

  private def run(js: String): Unit = {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    eval(js)
  }

  // --- with + PutValue reference semantics -------------------------------

  test("with compound assignment keeps the reference base when the binding is deleted") {
    run("""
      |var x = 0;
      |var scope = { get x() { delete this.x; return 2; } };
      |with (scope) { x ^= 3; }
      |if (scope.x !== 1) throw new Error("scope.x = " + scope.x);
      |if (x !== 0) throw new Error("x = " + x);
      |""".stripMargin)
  }

  test("with compound assignment inside nested functions") {
    run("""
      |function testFunction() {
      |  var x = 0;
      |  var scope = { get x() { delete this.x; return 2; } };
      |  with (scope) { x ^= 3; }
      |  if (scope.x !== 1) throw new Error("scope.x = " + scope.x);
      |}
      |testFunction();
      |""".stripMargin)
  }

  test("with increment keeps the reference base") {
    run("""
      |var x = 0;
      |var scope = { get x() { delete this.x; return 4; } };
      |with (scope) { x++; }
      |if (scope.x !== 5) throw new Error("scope.x = " + scope.x);
      |""".stripMargin)
  }

  test("with honors Symbol.unscopables") {
    run("""
      |var o = { x: 1, [Symbol.unscopables]: { x: true } };
      |var x = 42;
      |var result;
      |with (o) { result = x; }
      |if (result !== 42) throw new Error("result = " + result);
      |""".stripMargin)
  }

  test("strict assignment to an accessor without a setter throws TypeError") {
    run("""
      |"use strict";
      |var o = { get p() { return 1; } };
      |var threw = false;
      |try { o.p = 2; } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error("expected TypeError");
      |""".stripMargin)
  }

  // --- nested functions must not inherit outer abrupt-completion state ---

  test("a return inside a nested function does not close an outer for-of iterator") {
    run("""
      |var closeCount = 0;
      |var iterable = {
      |  [Symbol.iterator]() {
      |    var done = false;
      |    return {
      |      next() { if (done) return { done: true }; done = true; return { value: 1, done: false }; },
      |      return() { closeCount++; return {}; }
      |    };
      |  }
      |};
      |for (var v of iterable) {
      |  var f = function () { return 1; };
      |  f();
      |}
      |if (closeCount !== 0) throw new Error("closeCount = " + closeCount);
      |""".stripMargin)
  }

  // --- ToPrimitive in builtin conversions --------------------------------

  test("Number/String/builtins coerce objects with valueOf/toString") {
    run("""
      |if (Number({ valueOf: function () { return 3; } }) !== 3) throw new Error("Number");
      |if (String({ toString: function () { return "y"; } }) !== "y") throw new Error("String");
      |if (parseInt("11", { valueOf: function () { return 2; } }) !== 3) throw new Error("parseInt");
      |if (Number(new String("42")) !== 42) throw new Error("boxed");
      |""".stripMargin)
  }

  test("parseInt and parseInt.length are correct for global calls") {
    run("""
      |if (parseInt("11", 2) !== 3) throw new Error("binary");
      |if (parseInt("0x0", 0) !== 0) throw new Error("hex");
      |if (Number.parseInt("11", 16) !== 17) throw new Error("Number.parseInt");
      |if (parseInt.length !== 2) throw new Error("length");
      |""".stripMargin)
  }

  test("unary plus parses binary/octal string literals") {
    run("""
      |if (+"0b111" !== 7) throw new Error("binary");
      |if (+"0o123" !== 83) throw new Error("octal");
      |""".stripMargin)
  }

  // --- Math -------------------------------------------------------------

  test("Math round-trip edge cases") {
    run("""
      |if (!Number.isNaN(Math.abs())) throw new Error("Math.abs()");
      |if (Math.round(Infinity) !== Infinity) throw new Error("round +Inf");
      |if (1 / Math.round(-0.5) !== -Infinity) throw new Error("round -0.5");
      |if (Math.clz32(2147483648) !== 0) throw new Error("clz32");
      |if (Math.imul(0xB505, 0xB505) !== -2147479015) throw new Error("imul");
      |if (Math.sumPrecise([1, Number.EPSILON / 2, Number.MIN_VALUE]) !== 1.0000000000000002) throw new Error("sumPrecise");
      |if (Math.f16round(32767) !== 32768) throw new Error("f16round");
      |""".stripMargin)
  }

  // --- Date -------------------------------------------------------------

  test("Date setters coerce arguments with the spec order") {
    run("""
      |var log = [];
      |var d = new Date(0);
      |var o = function (tag) { return { valueOf: function () { log.push(tag); return 1; } }; };
      |d.setHours(o("hour"), o("min"), o("sec"), o("ms"));
      |if (log.join(",") !== "hour,min,sec,ms") throw new Error("order: " + log);
      |var threw = false;
      |try { d.setHours({ valueOf: function () { throw new RangeError("boom"); } }); }
      |catch (e) { threw = e instanceof RangeError; }
      |if (!threw) throw new Error("coercion errors must propagate");
      |""".stripMargin)
  }

  test("multi-argument Date constructor and Date.UTC follow MakeDay/MakeTime") {
    run("""
      |var d = new Date(2016, 6);
      |if (d.getFullYear() !== 2016 || d.getMonth() !== 6 || d.getDate() !== 1) throw new Error("ctor");
      |if (Date.UTC(2017, 9, 22, 18, 10, 11, 91) !== 1508695811091) throw new Error("UTC");
      |if (Date.UTC(1970, 0, 1, 80063993375, 29, 1, -288230376151711740) !== 29312) throw new Error("fp order");
      |""".stripMargin)
  }

  test("Date.prototype[Symbol.toPrimitive] distinguishes hints") {
    run("""
      |var d = new Date(0);
      |if (String(d) === d.valueOf().toString()) throw new Error("string hint");
      |if (typeof (d + 0) === "string" ? false : true) throw new Error("default hint");
      |var threw = false;
      |try { d[Symbol.toPrimitive]("bogus"); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error("invalid hint must throw");
      |""".stripMargin)
  }

  // --- Promise ----------------------------------------------------------

  test("Promise combinators use the receiver constructor") {
    run("""
      |var called = 0;
      |function C(executor) { called++; executor(function () {}, function () {}); }
      |C.resolve = function (v) { return v; };
      |Promise.allSettled.call(C, []);
      |Promise.all.call(C, []);
      |Promise.any.call(C, []);
      |Promise.race.call(C, []);
      |if (called !== 4) throw new Error("called = " + called);
      |""".stripMargin)
  }

  test("subclassing Promise works through super()") {
    run("""
      |class Custom extends Promise {}
      |var p = new Custom(function (resolve) { resolve(1); });
      |if (!(p instanceof Custom)) throw new Error("instance");
      |var Custom2 = Custom.resolve(1);
      |if (!(Custom2 instanceof Custom)) throw new Error("resolve instance");
      |""".stripMargin)
  }

  test("Promise.prototype.catch is generic and finally uses the species") {
    run("""
      |var count = 0;
      |Boolean.prototype.then = function () { count++; };
      |Promise.prototype.catch.call(true);
      |if (count !== 1) throw new Error("catch generic");
      |""".stripMargin)
  }

  // --- Proxy ------------------------------------------------------------

  test("function proxies inherit Function.prototype and honor the apply trap") {
    run("""
      |var target = function () {};
      |var ctxSeen = null;
      |var p = new Proxy(target, { apply: function (t, c, a) { ctxSeen = c; return a.length; } });
      |if (typeof p.call !== "function") throw new Error("p.call");
      |var context = {};
      |if (Function.prototype.call.call(p, context, 1, 2) !== 2) throw new Error("call result");
      |if (ctxSeen !== context) throw new Error("context");
      |""".stripMargin)
  }

  // --- RegExp -----------------------------------------------------------

  test("global empty matches advance correctly and terminate") {
    run("""
      |var m = "abc".match(/(?:)/g);
      |if (m.length !== 4) throw new Error("match length " + m.length);
      |if ("abc".replace(/(?:)/g, "X") !== "XaXbXcX") throw new Error("replace");
      |""".stripMargin)
  }

  test("unicode lastIndex mid-pair handling matches QuickJS") {
    run("""
      |var a = /(?:)/gu;
      |a.lastIndex = 1;
      |a.exec(String.fromCharCode(0xD83D, 0xDC31));
      |if (a.lastIndex !== 0) throw new Error("lastIndex = " + a.lastIndex);
      |a.lastIndex = 1;
      |a.exec("a\udc00");
      |if (a.lastIndex !== 1) throw new Error("lastIndex2 = " + a.lastIndex);
      |""".stripMargin)
  }

  // --- TypedArray -------------------------------------------------------

  test("TypedArray toLocaleString invokes each element method and BYTES_PER_ELEMENT is fixed") {
    run("""
      |var calls = 0;
      |Number.prototype.toLocaleString = function () { calls++; return "n" + this; };
      |var ta = new Int8Array([1, 2]);
      |if (ta.toLocaleString() !== "n1,n2") throw new Error("toLocaleString");
      |if (calls !== 2) throw new Error("calls = " + calls);
      |var d = Object.getOwnPropertyDescriptor(Int8Array, "BYTES_PER_ELEMENT");
      |if (d.configurable !== false) throw new Error("configurable");
      |""".stripMargin)
  }

  test("TypedArray ToInt32 wraps large values") {
    run("""
      |var a = new Int32Array(1);
      |a[0] = Math.pow(2, 32) - 1;
      |if (a[0] !== -1) throw new Error("a[0] = " + a[0]);
      |""".stripMargin)
  }

  // --- Class subclassing -------------------------------------------------

  test("native builtins initialize derived instances through super()") {
    run("""
      |class M extends Map {} var m = new M([["a", 1]]); m.set("b", 2);
      |if (m.size !== 2 || m.get("a") !== 1) throw new Error("Map");
      |class S extends Set {} var s = new S([1, 2, 2]); s.add(3);
      |if (s.size !== 3) throw new Error("Set");
      |class W extends WeakMap {} var wm = new W(); var k = {}; wm.set(k, 5);
      |if (wm.get(k) !== 5) throw new Error("WeakMap");
      |class Ws extends WeakSet {} var ws = new Ws(); ws.add(k);
      |if (!ws.has(k)) throw new Error("WeakSet");
      |class B extends Boolean {} if (new B(1).valueOf() !== true) throw new Error("Boolean");
      |class N extends Number {} if (new N(41).valueOf() !== 41) throw new Error("Number");
      |class Str extends String {} var str = new Str("hi");
      |if (str.valueOf() !== "hi" || str.length !== 2 || str[0] !== "h") throw new Error("String");
      |class E extends Error {} var e = new E("boom");
      |if (!(e instanceof E) || e.message !== "boom" || e.name !== "Error") throw new Error("Error");
      |class TE extends TypeError {} var te = new TE("bad");
      |if (!(te instanceof TE) || te.name !== "TypeError") throw new Error("TypeError");
      |class D extends Date {} if (new D(0).getTime() !== 0) throw new Error("Date");
      |class R extends RegExp {} var r = new R("a+", "gi");
      |if (!(r instanceof R) || r.source !== "a+" || r.flags !== "gi") throw new Error("RegExp");
      |class AB extends ArrayBuffer {} var ab = new AB(4);
      |if (ab.byteLength !== 4) throw new Error("ArrayBuffer");
      |class TA extends Int8Array {} var ta = new TA([1, 2, 3]);
      |if (!(ta instanceof TA) || ta.length !== 3 || ta[1] !== 2) throw new Error("TypedArray");
      |""".stripMargin)
  }

  test("Array subclassing builds an exotic Array receiver") {
    run("""
      |class Sub extends Array {}
      |var a1 = new Sub(42, "foo");
      |if (a1.length !== 2 || a1[0] !== 42 || a1[1] !== "foo") throw new Error("elements");
      |a1.push(true);
      |if (a1.length !== 3 || a1[2] !== true) throw new Error("push");
      |if (!Array.isArray(a1) || !(a1 instanceof Sub) || !(a1 instanceof Array)) throw new Error("isArray");
      |var a2 = new Sub(7);
      |if (a2.length !== 7) throw new Error("length");
      |class B extends Array { constructor() { super(); this[0] = 1; } }
      |var b = new B();
      |if (b.length !== 1 || b[0] !== 1) throw new Error("explicit super");
      |""".stripMargin)
  }

  test("derived constructors require super() and reject primitive returns") {
    run("""
      |class A extends Array { constructor() {} }
      |var threw = false;
      |try { new A(); } catch (e) { threw = e instanceof ReferenceError; }
      |if (!threw) throw new Error("missing super must throw ReferenceError");
      |
      |class Base { constructor() {} }
      |class D extends Base { constructor() { super(); return true; } }
      |threw = false;
      |try { new D(); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error("primitive return must throw TypeError");
      |
      |class D2 extends Base { constructor() { super(); try { return 0; } catch (e) { return; } } }
      |threw = false;
      |try { new D2(); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error("TypeError must not be catchable");
      |""".stripMargin)
  }

  test("class constructors cannot be invoked without new") {
    run("""
      |class C {}
      |var threw = false;
      |try { C(); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error("call without new");
      |class D extends C {}
      |threw = false;
      |try { D(); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error("derived call without new");
      |if (!(new C() instanceof C)) throw new Error("new works");
      |""".stripMargin)
  }

  test("extends null and invalid heritages") {
    run("""
      |class Foo extends null {}
      |if (Object.getPrototypeOf(Foo.prototype) !== null) throw new Error("proto");
      |if (Object.getPrototypeOf(Foo.prototype.constructor) !== Function.prototype) throw new Error("ctor proto");
      |var threw = false;
      |try { new Foo(); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error("default extends null construction");
      |
      |function* gen() {}
      |threw = false;
      |try { class G extends gen {} } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error("generator heritage");
      |
      |async function af() {}
      |threw = false;
      |try { class G extends af {} } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error("async heritage");
      |""".stripMargin)
  }

  test("async and generator functions follow prototype/callability rules") {
    run("""
      |async function af() {}
      |if (Object.getOwnPropertyDescriptor(af, "prototype") !== undefined) throw new Error("async prototype");
      |var threw = false;
      |try { new af(); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error("async new");
      |function* gen() {}
      |if (Object.getOwnPropertyDescriptor(gen, "prototype") === undefined) throw new Error("generator prototype");
      |threw = false;
      |try { new gen(); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error("generator new");
      |var arrow = () => {};
      |if (Object.getOwnPropertyDescriptor(arrow, "prototype") !== undefined) throw new Error("arrow prototype");
      |""".stripMargin)
  }
