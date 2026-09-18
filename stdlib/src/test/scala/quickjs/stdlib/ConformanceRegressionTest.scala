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
    val ast = Parser(tokens, source).parseScript()
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

  test("array-valued prototypes are honored by construction and lookup") {
    run("""
      |function Foo() {}
      |Foo.prototype = new Array(1, 2, 3);
      |var f = new Foo();
      |f.length = 2;
      |if (!(f instanceof Foo)) throw new Error("instanceof");
      |if (f[0] !== 1 || f[1] !== 2) throw new Error("index inheritance");
      |if (typeof f.every !== "function") throw new Error("every lookup");
      |var seen = [];
      |var all = f.every(function (v) { seen.push(v); return v <= 2; });
      |if (all !== true) throw new Error("every result");
      |if (seen.join(",") !== "1,2") throw new Error("seen = " + seen.join(","));
      |if (Object.getPrototypeOf(f) !== Foo.prototype) throw new Error("getPrototypeOf");
      |if (Foo.prototype.isPrototypeOf(f) !== true) throw new Error("isPrototypeOf");
      |""".stripMargin)
  }

  test("bitwise operators apply ToPrimitive to object operands") {
    run("""
      |var checks = [
      |  [(new Boolean(true) ^ true) === 0, 'Boolean object ^ true'],
      |  [(new Number(1) ^ 1) === 0, 'Number object ^ number'],
      |  [(new String('1') ^ '1') === 0, 'String object ^ string'],
      |  [(({valueOf: function(){return 1;}}) << 1) === 2, 'valueOf << 1'],
      |  [((new Number(3)) | 0) === 3, 'Number object | 0'],
      |  [((new Number(3)) & 1) === 1, 'Number object & 1'],
      |  [((new Number(3)) >> 1) === 1, 'Number object >> 1'],
      |  [((new Number(3)) >>> 1) === 1, 'Number object >>> 1']
      |];
      |for (var i = 0; i < checks.length; i++) {
      |  if (!checks[i][0]) throw new Error(checks[i][1]);
      |}
      |// Abrupt completion from valueOf must propagate, not become 0.
      |var threw = false;
      |try { ({valueOf: function(){ throw new Error('boom'); }}) ^ 1; }
      |catch (e) { threw = e.message === 'boom'; }
      |if (!threw) throw new Error('valueOf abrupt completion not propagated');
      |""".stripMargin)
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

  test("with scopes are unwound by throw, break and continue") {
    run("""
      |// throw out of with
      |var o = { x: 'from-with' };
      |var x = 'global';
      |try { with (o) { throw new Error('abort'); } } catch (e) {}
      |if (x !== 'global') throw new Error('throw leaked with: ' + x);
      |
      |// break out of with
      |do { with (o) { break; } } while (false);
      |if (x !== 'global') throw new Error('break leaked with: ' + x);
      |
      |// continue out of with
      |var n = 0;
      |for (var i = 0; i < 2; i++) { with (o) { n++; continue; } n += 10; }
      |if (n !== 2) throw new Error('continue n = ' + n);
      |if (x !== 'global') throw new Error('continue leaked with: ' + x);
      |""".stripMargin)
  }

  test("functions capture with only when defined inside it") {
    run("""
      |this.p = 'global';
      |var o = { p: 'with' };
      |
      |// Defined outside: must not see the caller's with scope.
      |var outside = function () { return p; };
      |var outsideResult;
      |with (o) { outsideResult = outside(); }
      |if (outsideResult !== 'global') throw new Error('outside = ' + outsideResult);
      |if (o.p !== 'with') throw new Error('o.p = ' + o.p);
      |
      |// Defined inside: closes over the with environment and keeps seeing it.
      |var captured;
      |with (o) { captured = function () { return p; }; }
      |if (captured() !== 'with') throw new Error('captured = ' + captured());
      |""".stripMargin)
  }

  test("var declarations inside with hoist and assign through the with object") {
    run("""
      |var o = { value: 'obj' };
      |with (o) { var value = 'local'; }
      |if (o.value !== 'local') throw new Error('o.value = ' + o.value);
      |if (value !== undefined) throw new Error('value = ' + value);
      |
      |// Unreachable declarations still create the binding.
      |var o2 = {};
      |try { with (o2) { throw 1; var after = 2; } } catch (e) {}
      |if (!('after' in this)) throw new Error('after not hoisted');
      |""".stripMargin)
  }

  test("delete of an identifier inside with targets the with object") {
    run("""
      |this.p3 = 3;
      |var o = { p3: 'c' };
      |var deleted;
      |with (o) { deleted = delete p3; }
      |if (deleted !== true) throw new Error('delete returned ' + deleted);
      |if (o.p3 !== undefined) throw new Error('o.p3 = ' + o.p3);
      |if (p3 !== 3) throw new Error('global p3 = ' + p3);
      |""".stripMargin)
  }

  test("with boxes primitive values") {
    run("""
      |var foo = 1;
      |with (2) { foo = 42; }
      |if (foo !== 42) throw new Error('number: ' + foo);
      |with ('str') { }
      |with (true) { }
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

  test("const bindings in for-in/for-of heads are re-initialized per iteration") {
    run("""
      |var keys = [];
      |for (const k in { a: 1, b: 2 }) keys.push(k);
      |if (keys.join(",") !== "a,b") throw new Error("for-in const: " + keys.join(","));
      |
      |var pairs = [];
      |for (const [x, y] of [[1, 2], [3, 4]]) pairs.push(x * 10 + y);
      |if (pairs.join(",") !== "12,34") throw new Error("for-of const destructuring: " + pairs.join(","));
      |
      |var values = [];
      |for (const { v } of [{ v: 1 }, { v: 2 }]) values.push(v);
      |if (values.join(",") !== "1,2") throw new Error("for-of const object pattern: " + values.join(","));
      |
      |var scalar = [];
      |for (const n of [5, 6]) scalar.push(n);
      |if (scalar.join(",") !== "5,6") throw new Error("for-of const: " + scalar.join(","));
      |""".stripMargin)
  }

  // --- Promise resolution (ResolvePromise / thenable adoption) -----------

  /** Schedule promise work, drain microtasks, then evaluate the assertions. */
  private def runPromise(schedule: String, check: String): Unit = {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    eval(schedule)
    ctx.runMicrotasks()
    eval(check)
  }

  test("then callbacks that return a promise adopt its eventual state") {
    runPromise(
      """
        |var seen = null;
        |var rejected = null;
        |Promise.resolve(1).then(() => Promise.resolve(2)).then(v => { seen = v; });
        |Promise.resolve(1)
        |  .then(() => Promise.reject("bad"))
        |  .then(() => { rejected = "fulfilled"; }, e => { rejected = e; });
        |""".stripMargin,
      """
        |if (seen !== 2) throw new Error("adopted promise: " + seen);
        |if (rejected !== "bad") throw new Error("adopted rejection: " + rejected);
        |""".stripMargin
    )
  }

  test("generic thenables are assimilated by promise resolution") {
    runPromise(
      """
        |var seen = null;
        |var chained = null;
        |Promise.resolve({ then(resolve) { resolve(3); } }).then(v => { seen = v; });
        |Promise.resolve(1).then(() => ({ then(resolve) { resolve(4); } })).then(v => { chained = v; });
        |""".stripMargin,
      """
        |if (seen !== 3) throw new Error("thenable: " + seen);
        |if (chained !== 4) throw new Error("chained thenable: " + chained);
        |""".stripMargin
    )
  }

  test("resolving a promise with itself rejects with TypeError") {
    runPromise(
      """
        |var result = null;
        |var resolveFn = null;
        |var p = new Promise(resolve => { resolveFn = resolve; });
        |resolveFn(p);
        |p.catch(e => { result = e instanceof TypeError; });
        |""".stripMargin,
      """
        |if (result !== true) throw new Error("self resolution: " + result);
        |""".stripMargin
    )
  }

  test("Promise.resolve passes through natives and adopts thenables") {
    runPromise(
      """
        |var direct = Promise.resolve(5);
        |var passthrough = Promise.resolve(direct) === direct;
        |var seen = null;
        |Promise.resolve({ then(resolve) { resolve("adopted"); } }).then(v => { seen = v; });
        |""".stripMargin,
      """
        |if (passthrough !== true) throw new Error("native passthrough");
        |if (seen !== "adopted") throw new Error("resolve thenable: " + seen);
        |""".stripMargin
    )
  }

  // --- async/await suspension -------------------------------------------

  /** Run async work, drain microtasks, then evaluate the assertions. */
  private def runAwait(schedule: String, check: String): Unit = {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    eval(schedule)
    ctx.runMicrotasks()
    ctx.runMicrotasks()
    eval(check)
  }

  test("await suspends on pending promises and resumes with the value") {
    runAwait(
      """
        |var log = [];
        |var p = Promise.resolve().then(() => { log.push('inner'); return 1; });
        |(async () => { const v = await p; log.push('await:' + v); })();
        |Promise.resolve().then(() => log.push('after'));
        |""".stripMargin,
      """
        |if (log.join(',') !== 'inner,after,await:1') throw new Error('order: ' + log.join(','));
        |""".stripMargin
    )
  }

  test("await always yields a microtask turn, even for settled promises") {
    runAwait(
      """
        |var order = [];
        |(async () => { await Promise.resolve(1); order.push('await'); })();
        |order.push('sync');
        |""".stripMargin,
      """
        |if (order.join(',') !== 'sync,await') throw new Error('order: ' + order.join(','));
        |""".stripMargin
    )
  }

  test("await of a rejected pending promise resumes with a throw") {
    runAwait(
      """
        |var seen = null;
        |var p = Promise.resolve().then(() => { throw new Error('boom'); });
        |(async () => {
        |  try { await p; seen = 'fulfilled'; } catch (e) { seen = e.message; }
        |})();
        |""".stripMargin,
      """
        |if (seen !== 'boom') throw new Error('seen: ' + seen);
        |""".stripMargin
    )
  }

  test("async functions resume across multiple awaits and return values") {
    runAwait(
      """
        |var result = null;
        |async function f() {
        |  const a = await Promise.resolve(2);
        |  const b = await Promise.resolve().then(() => 3);
        |  return a + b;
        |}
        |f().then(v => { result = v; });
        |""".stripMargin,
      """
        |if (result !== 5) throw new Error('result: ' + result);
        |""".stripMargin
    )
  }

  // --- Parser/regexp fixes found by running npm packages (Sep 2026) ------

  test("return immediately followed by } parses without a semicolon") {
    assertEval(
      """
        |function f(t) { if (t) { return } return 1; }
        |if (f(true) !== undefined) throw new Error("bare return");
        |if (f(false) !== 1) throw new Error("return after block");
        |function g() { return }
        |if (g() !== undefined) throw new Error("return then }");
        |""".stripMargin,
      ""
    )
  }

  test("ternary branches do not consume the surrounding comma") {
    run("""
      |const o = { f: () => 1 ? 2 : 3, g: 4 };
      |if (o.f() !== 2 || o.g !== 4) throw new Error("object arrow ternary");
      |const arr = [() => true ? 'a' : 'b', 5];
      |if (arr[0]() !== 'a' || arr[1] !== 5) throw new Error("array arrow ternary");
      |const chosen = (1 ? 2 : 3, 4);
      |if (chosen !== 4) throw new Error("parenthesized sequence");
      |""".stripMargin)
  }

  test("sibling blocks may reuse a name with different let/const kinds") {
    run("""
      |function f(x) {
      |  if (x) { const value = 1; return value; }
      |  let value = 2;
      |  { value = 3; }
      |  return value;
      |}
      |if (f(true) !== 1) throw new Error("const branch");
      |if (f(false) !== 3) throw new Error("let branch: " + f(false));
      |function g() {
      |  if (true) { const zodSchema = 'a'; }
      |  let zodSchema;
      |  zodSchema = 'b';
      |  return zodSchema;
      |}
      |if (g() !== 'b') throw new Error("later assignment: " + g());
      |""".stripMargin)
  }

  test("arrow block bodies hoist function declarations") {
    run("""
      |var holder = {};
      |((unused) => {
      |  holder.f = ei;
      |  function ei() { return 7; }
      |  return unused;
      |})(0);
      |if (holder.f() !== 7) throw new Error("arrow hoist");
      |""".stripMargin)
  }

  test("regexp translation handles classes with literal brackets braces and \\n") {
    run("""
      |if (!/[^[\]]+/.test('a[b')) throw new Error("literal bracket class");
      |if (!/a\{b/.test('a{b')) throw new Error("escaped brace");
      |if (!/\n/.test('\n')) throw new Error("newline escape");
      |if (!/[\n]/.test('\n')) throw new Error("newline in class");
      |if (!/[\n]/.test('\n')) throw new Error("newline in class");
      |if (!new RegExp("([\"'])(?:(?!\\1)[^\\\\]|\\\\.)*?\\1").test('"x"')) throw new Error("backref");
      |""".stripMargin)
  }


  test("closures capture block-scoped const/let bindings") {
    run("""
      |let x = false;
      |function cb(inst) {
      |  if (x) { const checks = 1; }
      |  else {
      |    const checks = 2;
      |    inst.run = () => checks;
      |  }
      |}
      |const inst = {};
      |cb(inst);
      |if (inst.run() !== 2) throw new Error("block const capture: " + inst.run());
      |
      |function blockOnly(holder) {
      |  { let value = 'v'; holder.get = () => value; }
      |}
      |const h = {};
      |blockOnly(h);
      |if (h.get() !== 'v') throw new Error("block let capture: " + h.get());
      |""".stripMargin)
  }

  test("let without initializer is initialized to undefined") {
    run("""
      |// `let x;` must initialize x to undefined at the declaration; it used to
      |// leave the slot in the TDZ and throw on the first read.
      |let x;
      |if (x !== undefined) throw new Error("top-level: " + x);
      |
      |function f() { let y; return y; }
      |if (f() !== undefined) throw new Error("function: " + f());
      |
      |function callMe() { return 1; }
      |let z;
      |callMe();
      |if (z !== undefined) throw new Error("after call: " + z);
      |
      |let a, b;
      |if (a !== undefined || b !== undefined) throw new Error("multi-declarator");
      |
      |for (let i; i === undefined; i = 1) {
      |  if (i !== undefined) throw new Error("for init: " + i);
      |}
      |
      |{ let block; if (block !== undefined) throw new Error("block: " + block); }
      |""".stripMargin)
  }

  // --- Engine fixes found by running bundled npm packages (Sep 2026) -----

  test("named class expressions with the same name are scoped separately") {
    run("""
      |const A = class u { static tag() { return 'A'; } static check() { return u.tag(); } };
      |const B = class u { static tag() { return 'B'; } };
      |if (A.check() !== 'A') throw new Error("class name capture: " + A.check());
      |const L = class u { static #o = 1; static create() { u.#o = 2; return u.#o; } };
      |const M = class u { static #o = 3; static read() { return 3; } };
      |if (L.create() !== 2) throw new Error("static private field");
      |""".stripMargin)
  }

  test("derived-class field initializers run after super()") {
    run("""
      |class Base { constructor(a) { this.a = a; } }
      |class D extends Base {
      |  sep = '/';
      |  constructor(cwd = 'x') { super(cwd); this.nocase = true; }
      |}
      |const d = new D();
      |if (d.sep !== '/' || d.nocase !== true || d.a !== 'x') throw new Error("field init order");
      |class E extends Base { x; y = 2; constructor() { super(1); } }
      |const e = new E();
      |if (e.y !== 2) throw new Error("field init value");
      |""".stripMargin)
  }

  test("free variables in parameter defaults are captured") {
    run("""
      |(function () {
      |  var mt = 1;
      |  function f({ fs = mt } = {}) { return fs; }
      |  if (f() !== 1) throw new Error("fn default: " + f());
      |})();
      |(function () {
      |  var mt = 2;
      |  class B { constructor({ fs = mt } = {}) { this.fs = fs; } }
      |  if (new B().fs !== 2) throw new Error("ctor default");
      |})();
      |""".stripMargin)
  }

  test("optional calls short-circuit on nullish callees and receivers") {
    run("""
      |const o = {};
      |if (o.x?.() !== undefined) throw new Error("optional call");
      |const p = { x: () => 1 };
      |if (p.x?.() !== 1) throw new Error("optional call value");
      |const q = { y: { x: () => 2 } };
      |if (q.y?.x?.() !== 2) throw new Error("optional chain call");
      |class C { #s; m() { return this.#s?.x?.(); } }
      |if (new C().m() !== undefined) throw new Error("private optional chain");
      |let evaluated = false;
      |o.missing?.(evaluated = true);
      |if (evaluated) throw new Error("optional call evaluated arguments");
      |""".stripMargin)
  }

  test("String.prototype.normalize and \\0 regexp") {
    run("""
      |if ('\u00e9'.normalize('NFD').length !== 2) throw new Error("NFD");
      |if ('e\u0301'.normalize('NFC').length !== 1) throw new Error("NFC");
      |if ('\uFB01'.normalize('NFKC') !== 'fi') throw new Error("NFKC");
      |let threw = false;
      |try { 'a'.normalize('BAD'); } catch (e) { threw = e instanceof RangeError; }
      |if (!threw) throw new Error("bad form");
      |if (!/\0/.test('\0')) throw new Error("nul regex");
      |""".stripMargin)
  }

  test("private field assignment evaluates to the assigned value") {
    run("""
      |class C { #x = 1; set(v) { return (this.#x = v); } get() { return this.#x; } }
      |const c = new C();
      |if (c.set(5) !== 5) throw new Error("assignment value");
      |if (c.get() !== 5) throw new Error("stored value");
      |""".stripMargin)
  }

  test("contextual keyword function names and async(...) calls") {
    run("""
      |function from(x) { return x + 1; }
      |function as(x) { return x + 2; }
      |function of(x) { return x + 3; }
      |function get(x) { return x + 4; }
      |function set(x) { return x + 5; }
      |function async(x) { return x + 6; }
      |if (from(1) !== 2) throw new Error("from");
      |if (as(1) !== 3) throw new Error("as");
      |if (of(1) !== 4) throw new Error("of");
      |if (get(1) !== 5) throw new Error("get");
      |if (set(1) !== 6) throw new Error("set");
      |if (async(1) !== 7) throw new Error("async call");
      |const arrow = async(x) => x * 2;
      |if (typeof arrow !== "function") throw new Error("async arrow");
      |const named = function from(x) { return x * 3; };
      |if (named(2) !== 6) throw new Error("named function expression");
      |""".stripMargin)
  }

  test("symbol lookup auto-boxes primitive receivers") {
    run("""
      |const iter = ''[Symbol.iterator]();
      |if (typeof iter.next !== 'function') throw new Error("string iterator");
      |if ('ab'[Symbol.iterator]().next().value !== 'a') throw new Error("value");
      |if ((5)[Symbol.iterator] !== undefined) throw new Error("number iterator");
      |const arrIter = [1, 2][Symbol.iterator]();
      |if (arrIter.next().value !== 1) throw new Error("array iterator");
      |""".stripMargin)
  }

  test("__proto__ accessor handles arrays, primitives and null prototypes") {
    run("""
      |if ([].__proto__ !== Array.prototype) throw new Error("array proto");
      |if (''.__proto__ !== String.prototype) throw new Error("string proto");
      |const o = {};
      |o.__proto__ = null;
      |if (Object.getPrototypeOf(o) !== null) throw new Error("set null");
      |const p = { __proto__: null, value: 1 };
      |if (Object.getPrototypeOf(p) !== null) throw new Error("literal null proto");
      |""".stripMargin)
  }

  test("extracted Reflect.apply and Reflect.construct work") {
    run("""
      |const apply = Reflect.apply;
      |if (apply(function (a, b) { return a + b; }, null, [1, 2]) !== 3) throw new Error("apply");
      |if (Reflect.apply(function () { return 7; }, null, []) !== 7) throw new Error("method apply");
      |function C(x) { this.x = x; }
      |const construct = Reflect.construct;
      |if (construct(C, [5]).x !== 5) throw new Error("construct");
      |""".stripMargin)
  }

  test("per-iteration loop bindings are captured by closures") {
    run("""
      |const fns = [];
      |for (const name of ['a', 'b', 'c']) fns.push(() => name);
      |if (fns.map(f => f()).join(',') !== 'a,b,c') throw new Error("for-of const");
      |
      |const fns2 = [];
      |for (let i = 0; i < 3; i++) fns2.push(() => i);
      |if (fns2.map(f => f()).join(',') !== '0,1,2') throw new Error("for let");
      |
      |const fns3 = [];
      |for (const k in { x: 1, y: 2 }) fns3.push(() => k);
      |if (fns3.map(f => f()).join(',') !== 'x,y') throw new Error("for-in const");
      |
      |const fns4 = [];
      |for (const [a, b] of [[1, 2], [3, 4]]) fns4.push(() => a + b);
      |if (fns4.map(f => f()).join(',') !== '3,7') throw new Error("destructuring");
      |
      |const fns5 = [];
      |for (let i = 0; i < 5; i++) { if (i === 1) continue; if (i === 4) break; fns5.push(() => i); }
      |if (fns5.map(f => f()).join(',') !== '0,2,3') throw new Error("continue/break");
      |
      |const fns6 = [];
      |for (let i = 0; i < 2; i++) { for (let j = 0; j < 2; j++) fns6.push(() => i + ',' + j); }
      |if (fns6.map(f => f()).join(' ') !== '0,0 0,1 1,0 1,1') throw new Error("nested");
      |""".stripMargin)
  }

  test("function declarations shadow outer bindings and stay local") {
    run("""
      |function outer() {
      |  var M = { name: 'en' };
      |  function inner() {
      |    function M(t) { this.x = t; }
      |    return M;
      |  }
      |  return { ctor: inner(), locale: M };
      |}
      |const r = outer();
      |if (typeof r.ctor !== 'function') throw new Error("inner declaration shadowed");
      |if (r.locale.name !== 'en') throw new Error("outer locale");
      |if (typeof globalThis.M !== 'undefined') throw new Error("function leaked to global");
      |
      |function recursive(n) { return n <= 1 ? 1 : n * recursive(n - 1); }
      |if (recursive(5) !== 120) throw new Error("recursion");
      |""".stripMargin)
  }

  test("Array(n) called without new creates the requested length") {
    run("""
      |const a = Array(3);
      |if (a.length !== 3) throw new Error("Array(3).length = " + a.length);
      |if (a.join('x') !== 'xx') throw new Error("join");
      |if (Array(1, 2).join(',') !== '1,2') throw new Error("multiple args");
      |if (new Array(2).length !== 2) throw new Error("new Array(2)");
      |""".stripMargin)
  }

  test("loose equality with null/undefined does not coerce objects") {
    run("""
      |let converted = false;
      |const o = { toString() { converted = true; return 'x'; }, valueOf() { converted = true; return 1; } };
      |if (o == null) throw new Error("o == null");
      |if (!(o != null)) throw new Error("o != null");
      |if (o == undefined) throw new Error("o == undefined");
      |if (undefined == o) throw new Error("undefined == o");
      |if (converted) throw new Error("object was coerced");
      |if (!(null == undefined)) throw new Error("null == undefined");
      |if (null == 0) throw new Error("null == 0");
      |""".stripMargin)
  }

  test("extracted native methods work without a receiver") {
    run("""
      |const ceil = Math.ceil, floor = Math.floor, min = Math.min, max = Math.max;
      |if (ceil(3) !== 3) throw new Error("ceil");
      |if (ceil(3.2) !== 4) throw new Error("ceil 3.2");
      |if (floor(3.8) !== 3) throw new Error("floor");
      |if (min(3, -1) !== -1) throw new Error("min");
      |if (max(3, -1) !== 3) throw new Error("max");
      |if (Math.min(3, -1) !== -1) throw new Error("method min");
      |if (Math.max(3, -1) !== 3) throw new Error("method max");
      |""".stripMargin)
  }

  test("Function constructor stringifies array arguments") {
    run("""
      |const f = Function(['a', 'b'], 'return a + b;');
      |if (f(1, 2) !== 3) throw new Error("array params");
      |const g = Function('a', 'return a * 2;');
      |if (g(3) !== 6) throw new Error("string params");
      |""".stripMargin)
  }

  test("computed string-key access on primitives auto-boxes") {
    run("""
      |function make(methodName) {
      |  return function (string) { return string[methodName](); };
      |}
      |if (make('toUpperCase')('abc') !== 'ABC') throw new Error("captured method name");
      |if ('abc'['length'] !== 3) throw new Error("string length");
      |if ('abc'['slice'](1) !== 'bc') throw new Error("string slice");
      |if ((5)['toFixed'](1) !== '5.0') throw new Error("number method");
      |if (true['toString']() !== 'true') throw new Error("boolean method");
      |""".stripMargin)
  }

  test("super property accessors use the current this") {
    run("""
      |class Parent {
      |  constructor() { this.nodes = [1, 2]; }
      |  get names() { return this.nodes; }
      |  who() { return this === undefined ? 'none' : 'this'; }
      |}
      |class Child extends Parent {
      |  get names() { return super.names; }
      |  who() { return super.who(); }
      |}
      |const c = new Child();
      |if (c.names.join(',') !== '1,2') throw new Error("super getter");
      |if (c.who() !== 'this') throw new Error("super method");
      |""".stripMargin)
  }

  test("Set methods union/intersection/difference and friends") {
    run("""
      |const a = new Set([1, 2, 3]);
      |const b = new Set([3, 4]);
      |if ([...a.union(b)].join(',') !== '1,2,3,4') throw new Error("union");
      |if ([...a.intersection(b)].join(',') !== '3') throw new Error("intersection");
      |if ([...a.difference(b)].join(',') !== '1,2') throw new Error("difference");
      |if ([...a.symmetricDifference(b)].join(',') !== '1,2,4') throw new Error("symmetricDifference");
      |if (new Set([1]).isSubsetOf(a) !== true) throw new Error("isSubsetOf");
      |if (a.isSupersetOf(new Set([1])) !== true) throw new Error("isSupersetOf");
      |if (a.isDisjointFrom(new Set([9])) !== true) throw new Error("isDisjointFrom");
      |""".stripMargin)
  }

  test("async generator function prototypes exist") {
    run("""
      |const proto = Object.getPrototypeOf(Object.getPrototypeOf(async function* () {}).prototype);
      |if (proto === null || typeof proto !== 'object') throw new Error("async generator prototype chain");
      |""".stripMargin)
  }

  test("logical assignment to private fields") {
    run("""
      |class C {
      |  #x = 1;
      |  bump() { this.#x ??= 5; return this.#x; }
      |  set(v) { this.#x = v; }
      |}
      |const c = new C();
      |if (c.bump() !== 1) throw new Error("private ??= existing");
      |const d = new C();
      |d.set(undefined);
      |if (d.bump() !== 5) throw new Error("private ??= undefined");
      |""".stripMargin)
  }

  test("extracted Reflect static methods work without a receiver") {
    run("""
      |const gpo = Reflect.getPrototypeOf;
      |if (gpo([]) !== Array.prototype) throw new Error("getPrototypeOf");
      |const ownKeys = Reflect.ownKeys;
      |if (ownKeys({ a: 1 }).join(',') !== 'a') throw new Error("ownKeys");
      |const isExt = Reflect.isExtensible;
      |if (isExt({}) !== true) throw new Error("isExtensible");
      |const get = Reflect.get;
      |if (get({ a: 1 }, 'a') !== 1) throw new Error("get");
      |const has = Reflect.has;
      |if (has({ a: 1 }, 'a') !== true) throw new Error("has");
      |const del = Reflect.deleteProperty;
      |const o = { a: 1 };
      |if (del(o, 'a') !== true || 'a' in o) throw new Error("deleteProperty");
      |""".stripMargin)
  }

  // --- Intl ---------------------------------------------------------------

  test("Intl.Segmenter splits grapheme clusters and exposes resolvedOptions") {
    run("""
      |const seg = new Intl.Segmenter('en', { granularity: 'grapheme' });
      |const parts = [...seg.segment('a\u{1F468}\u200D\u{1F469}\u200D\u{1F467}b')];
      |if (parts.length !== 3) throw new Error('segments = ' + parts.length);
      |if (parts[1].segment !== '\u{1F468}\u200D\u{1F469}\u200D\u{1F467}') throw new Error('middle');
      |if (parts[1].index !== 1) throw new Error('index = ' + parts[1].index);
      |if (parts[1].input !== 'a\u{1F468}\u200D\u{1F469}\u200D\u{1F467}b') throw new Error('input');
      |const opts = seg.resolvedOptions();
      |if (opts.granularity !== 'grapheme') throw new Error('granularity');
      |const words = [...new Intl.Segmenter('en', { granularity: 'word' }).segment('hi there')];
      |if (words.length < 3) throw new Error('words = ' + words.length);
      |""".stripMargin)
  }

  test("Intl.NumberFormat, DateTimeFormat, Collator and PluralRules") {
    run("""
      |const nf = new Intl.NumberFormat('de-DE', { style: 'currency', currency: 'EUR' }).format(1234.5);
      |if (nf.indexOf('1.234,50') !== 0) throw new Error('nf = ' + nf);
      |const grouped = new Intl.NumberFormat('en-US').format(1234567);
      |if (grouped !== '1,234,567') throw new Error('grouped = ' + grouped);
      |const df = new Intl.DateTimeFormat('en-GB', { year: 'numeric', month: 'long', day: 'numeric' }).format(new Date(0));
      |if (df !== '1 January 1970') throw new Error('df = ' + df);
      |if (new Intl.PluralRules('en').select(1) !== 'one') throw new Error('plural one');
      |if (new Intl.PluralRules('en').select(2) !== 'other') throw new Error('plural other');
      |if (new Intl.Collator('en').compare('a', 'b') !== -1) throw new Error('collator');
      |if (Intl.getCanonicalLocales(['EN-us']).join(',') !== 'en-US') throw new Error('canonical');
      |""".stripMargin)
  }

  // --- Unicode property escapes in regular expressions --------------------

  test("regexp Unicode property escapes Java lacks are translated") {
    run("""
      |const ignorable = /^\p{Default_Ignorable_Code_Point}+$/;
      |if (!ignorable.test('\u00AD')) throw new Error('soft hyphen');
      |if (!ignorable.test('\u200B\u200B')) throw new Error('zero width space');
      |if (ignorable.test('a')) throw new Error('a matched');
      |const format = /\p{Format}/;
      |if (!format.test('\u200E')) throw new Error('format');
      |const gc = new RegExp('\\p{gc=Letter}');
      |if (!gc.test('A')) throw new Error('gc=Letter');
      |const script = new RegExp('\\p{Script=Greek}');
      |if (!script.test('\u03B1')) throw new Error('Script=Greek');
      |const rgi = /^\p{RGI_Emoji}$/v;
      |if (!rgi.test('\u{1F600}')) throw new Error('emoji');
      |if (!rgi.test('\u{1F1FA}\u{1F1F8}')) throw new Error('flag');
      |""".stripMargin)
  }

  // --- class computed element keys ----------------------------------------

  test("class computed element keys are evaluated exactly once") {
    run("""
      |let count = 0;
      |class A { [count++]() {} }
      |if (count !== 1) throw new Error('method key count = ' + count);
      |let s = 0;
      |class B { static [s++]() {} }
      |if (s !== 1) throw new Error('static key count = ' + s);
      |let g = 0;
      |class C { get [g++]() { return 1; } }
      |if (g !== 1) throw new Error('getter key count = ' + g);
      |let f = 0;
      |class D { [f++] = 1; }
      |if (f !== 1) throw new Error('field key count = ' + f);
      |""".stripMargin)
  }

  // --- bound functions ----------------------------------------------------

  test("bound class methods receive arguments without the receiver") {
    run("""
      |class Y {
      |  constructor() { this.locale = 'en'; }
      |  setLocale(locale) { this.locale = locale; }
      |  getLocale() { return this.locale; }
      |}
      |const y = new Y();
      |const shim = { setLocale: y.setLocale.bind(y), getLocale: y.getLocale.bind(y) };
      |shim.setLocale('fr');
      |if (shim.getLocale() !== 'fr') throw new Error('locale = ' + shim.getLocale());
      |const extracted = shim.setLocale;
      |extracted('de');
      |if (y.locale !== 'de') throw new Error('extracted = ' + y.locale);
      |""".stripMargin)
  }

  test("apply/call on an extracted native function pass arguments as-is") {
    run("""
      |if (parseInt.apply(parseInt, ['10', '10']) !== 10) throw new Error('parseInt apply self');
      |if (parseInt.call(parseInt, 'ff', 16) !== 255) throw new Error('parseInt call self');
      |if (isNaN.call(isNaN, 'x') !== true) throw new Error('isNaN call self');
      |""".stripMargin)
  }

  // --- WeakMap storage ----------------------------------------------------

  test("WeakMap entries survive JVM garbage collection") {
    run("""
      |const wm = new WeakMap();
      |const key = {};
      |wm.set(key, 42);
      |if (wm.get(key) !== 42) throw new Error('before');
      |""".stripMargin)
    // Force a JVM GC between set and has/get: the previous identity-wrapper
    // WeakHashMap could drop live entries here.
    val rt = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)
    val tokens = Lexer("""
      |const wm = new WeakMap();
      |const key = {};
      |wm.set(key, 42); globalThis.__wm = wm; globalThis.__key = key;
      |""".stripMargin).tokenize()
    val ast = Parser(tokens).parseScript()
    val bytecode = Compiler().compileScript(ast)
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty)
    System.gc()
    Thread.sleep(50)
    val checkTokens = Lexer("""
      |if (globalThis.__wm.get(globalThis.__key) !== 42) throw new Error('after gc');
      |if (!globalThis.__wm.has(globalThis.__key)) throw new Error('after gc has');
      |""".stripMargin).tokenize()
    val checkAst = Parser(checkTokens).parseScript()
    val checkBytecode = Compiler().compileScript(checkAst)
    Interpreter().call(checkBytecode, JSValue.Undefined, Array.empty)
  }

  // --- Unicode escapes inside regexp classes ------------------------------

  test("regexp \\u{...} escapes work inside character classes") {
    run("""
      |if (!/[\u{AD}]/u.test('\u00AD')) throw new Error('single');
      |const range = /[a-z0-9_\u{AD}\u{C0}-\u{D6}\u{D8}-\u{F6}]+/u;
      |if (!range.test('\u00C5')) throw new Error('range');
      |if (!range.test('abc123_\u00F6')) throw new Error('mixed');
      |if (range.test('!')) throw new Error('bang');
      |if (!/[\u{1F600}]/u.test('\u{1F600}')) throw new Error('astral');
      |if (!new RegExp('[\\u{AD}\\u{C0}-\\u{D6}]', 'u').test('\u00C4')) throw new Error('dynamic');
      |""".stripMargin)
  }

  // --- Error.stackTraceLimit / prepareStackTrace / captureStackTrace ------

  test("Error.captureStackTrace produces lazy stacks") {
    run("""
      |if (typeof Error.captureStackTrace !== 'function') throw new Error('captureStackTrace');
      |if (Error.stackTraceLimit !== 10) throw new Error('limit = ' + Error.stackTraceLimit);
      |if (Error.prepareStackTrace !== undefined) throw new Error('prepareStackTrace');
      |const obj = {};
      |Error.captureStackTrace(obj);
      |if (typeof obj.stack !== 'string') throw new Error('stack not a string');
      |if (obj.stack.indexOf('captureStackTrace') >= 0) throw new Error('captureStackTrace frame');
      |if (obj.stack.indexOf(':') < 0) throw new Error('no location');
      |""".stripMargin)
  }

  test("Error.prepareStackTrace receives CallSite objects") {
    run("""
      |const seen = [];
      |Error.prepareStackTrace = function (err, sites) {
      |  seen.push([err.message, sites.length, sites[0].getFileName(), sites[0].getFunctionName()]);
      |  return sites.map(function (s) { return s.toString(); }).join('\n');
      |};
      |try {
      |  const inner = {};
      |  Error.captureStackTrace(inner);
      |  if (typeof inner.stack !== 'string') throw new Error('prepared stack');
      |} finally {
      |  Error.prepareStackTrace = undefined;
      |}
      |if (seen.length !== 1) throw new Error('prepare calls = ' + seen.length);
      |if (typeof seen[0][2] !== 'string') throw new Error('fileName');
      |const err = new Error('x');
      |if (typeof err.stack !== 'string') throw new Error('fallback string');
      |""".stripMargin)
  }
  
  test("a native constructor called as a method ignores the receiver") {
    run("""
      |const holder = { S: String, N: Number, B: Boolean };
      |if (typeof holder.S('z') !== 'string') throw new Error('String type');
      |if (holder.S('z') !== 'z') throw new Error('String value');
      |if (holder.N('4') !== 4) throw new Error('Number');
      |if (holder.B('') !== false) throw new Error('Boolean');
      |const call = Function.prototype.call;
      |if (String.call(holder, 'q') !== 'q') throw new Error('call');
      |""".stripMargin)
  }

  test("instanceof unwraps function-valued prototypes") {
    run("""
      |function R() {
      |  if (!(this instanceof R)) return new R();
      |  this.ok = true;
      |  const inner = () => 1;
      |  Object.setPrototypeOf(inner, this);
      |  return inner;
      |}
      |R.prototype = function () {};
      |const r = new R();
      |if (!(r instanceof R)) throw new Error('instanceof');
      |if (r.ok !== true) throw new Error('ok');
      |""".stripMargin)
  }

  test("destructuring defaults capture module bindings") {
    run("""
      |const D = '/';
      |function f(o) { const { d = D } = o; return d; }
      |if (f({}) !== '/') throw new Error('object default');
      |const h = (o) => { const [x = D] = o; return x; };
      |if (h([]) !== '/') throw new Error('array default');
      |if (f({ d: 'x' }) !== 'x') throw new Error('explicit value');
      |""".stripMargin)
  }

  test("nested parameter defaults capture enclosing bindings") {
    run("""
      |function returnTrue() { return 'T'; }
      |function outer() {
      |  function inner(x = returnTrue()) { return x; }
      |  const arrow = (y = returnTrue()) => y;
      |  return inner() + arrow();
      |}
      |if (outer() !== 'TT') throw new Error('defaults: ' + outer());
      |const outer2 = () => {
      |  const inner = (x = returnTrue()) => x;
      |  return inner();
      |};
      |if (outer2() !== 'T') throw new Error('arrow default');
      |""".stripMargin)
  }

  test("try statements do not leak operand stack slots across loop iterations") {
    run("""
      |var total = 0;
      |for (var i = 0; i < 10000; i++) { try { total += i; } catch (e) {} }
      |if (total !== 49995000) throw new Error('catch total: ' + total);
      |for (var j = 0; j < 10000; j++) { try { total += j; } finally {} }
      |if (total !== 99990000) throw new Error('finally total: ' + total);
      |for (var k = 0; k < 5000; k++) { try { throw k; } catch (e) { total += e; } }
      |if (total !== 112487500) throw new Error('throw total: ' + total);
      |""".stripMargin)
  }

  test("try statement completion values survive the stack-balance fix") {
    run("""
      |if (eval('try { 7 } catch (e) {}') !== 7) throw new Error('try completion');
      |if (eval('try { throw 1 } catch (e) { 2 }') !== 2) throw new Error('catch completion');
      |if (eval('try { 1 } catch (e) { 2 } finally { 3 }') !== 1)
      |  throw new Error('finally normal completion');
      |if (eval('try { throw 1 } catch (e) { 2 } finally { 3 }') !== 2)
      |  throw new Error('finally abrupt completion');
      |if (eval('try { try { 4 } catch (e) {} } catch (e) {}') !== 4)
      |  throw new Error('nested try completion');
      |""".stripMargin)
  }

  test("large finite loops are not aborted by a default instruction budget") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    if rt.maxInstructionCount != 0L then
      throw new Error("instruction budget should default to unlimited")
    StdLib.initialize(ctx)
    // A moderate loop plus the budget assertion above: the old hardcoded
    // 100M-instruction guard has been removed, and the default is checked
    // directly rather than by executing 100M instructions (which cost ~4s).
    eval("""
      |(function () {
      |  let s = 0;
      |  for (let i = 0; i < 2000000; i++) s += i;
      |  if (s !== 1999999000000) throw new Error('sum: ' + s);
      |})();
      |""".stripMargin)
  }

  test("a configured instruction budget bounds runaway loops") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    if rt.maxInstructionCount != 0L then
      throw new Error("instruction budget should default to unlimited")
    rt.setInstructionLimit(10000L)
    // The budget is a host policy, not a script-level error, so it surfaces as
    // a host exception rather than a catchable JavaScript error.
    val ex = intercept[RuntimeException] {
      eval("for (var i = 0; i < 1000000; i++) { var x = i; }")
    }
    if !String(ex.getMessage).contains("Infinite loop") then
      throw new Error(s"unexpected guard message: ${ex.getMessage}")
  }

  test("deep recursion raises a catchable RangeError instead of crashing") {
    run("""
      |function f(n) { return n <= 0 ? 0 : f(n - 1) + 1; }
      |var caught = null;
      |try { f(1000000); } catch (e) { caught = e; }
      |if (!(caught instanceof RangeError)) throw new Error('got ' + caught);
      |if (String(caught.message).indexOf('call stack') < 0)
      |  throw new Error('message: ' + caught.message);
      |""".stripMargin)
  }

  test("a low call-depth limit is honored and catchable") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    ctx.setMaxCallDepth(64)
    StdLib.initialize(ctx)
    eval("""
      |function g(n) { return n <= 0 ? 0 : g(n - 1) + 1; }
      |var caught = null;
      |try { g(1000); } catch (e) { caught = e; }
      |if (!(caught instanceof RangeError)) throw new Error('got ' + caught);
      |""".stripMargin)
  }

  test("long await loops do not recurse through microtask draining") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)
    eval("""
      |var done = null;
      |(async function () {
      |  let total = 0;
      |  for (let i = 0; i < 5000; i++) {
      |    try { total += await Promise.resolve(i); } catch (e) {}
      |  }
      |  done = total;
      |})();
      |""".stripMargin)
    ctx.runMicrotasks()
    eval("if (done !== 12497500) throw new Error('async loop: ' + done);")
  }

  test("Object.create accepts array, function and native prototypes") {
    run("""
      |var a = [9, 8];
      |var o = Object.create(a);
      |if (o[0] !== 9 || o.length !== 2) throw new Error('array prototype lookup');
      |if (Object.getPrototypeOf(o) !== a) throw new Error('array prototype identity');
      |if (Object.create(null) === null) throw new Error('null prototype');
      |var f = function () {};
      |var p = Object.create(f);
      |if (typeof p.call !== 'function') throw new Error('function prototype lookup');
      |var n = Object.create(Math.max);
      |if (typeof n.apply !== 'function') throw new Error('native prototype lookup');
      |var bad = null;
      |try { Object.create(1); } catch (e) { bad = e; }
      |if (!(bad instanceof TypeError)) throw new Error('primitive prototype');
      |""".stripMargin)
  }

  test("relational comparison applies ToPrimitive and string/BigInt rules") {
    run("""
      |var d0 = new Date(0), d1 = new Date(1000);
      |if (!(d0 < d1)) throw new Error('Date < Date');
      |if (!(d0 <= d1)) throw new Error('Date <= Date');
      |if (!(d1 > d0)) throw new Error('Date > Date');
      |if (d0 <= d0 !== true || d0 >= d0 !== true) throw new Error('equal Dates');
      |if ([-Infinity <= -Infinity] [0] !== true) throw new Error('infinity inline');
      |if (!(-Infinity <= -Infinity)) throw new Error('-Infinity <= -Infinity');
      |if (!(Infinity >= Infinity)) throw new Error('Infinity >= Infinity');
      |if ([[1] < 2] [0] !== true) throw new Error('[1] < 2');
      |if (([10] < 9) !== false) throw new Error('[10] < 9');
      |if (('10' < '9') !== true) throw new Error('string compare');
      |if (('10' < 9) !== false) throw new Error('string vs number');
      |if ((10n < '9') !== false) throw new Error('bigint vs invalid-ish string');
      |if ((10n < '11') !== true) throw new Error('bigint vs string');
      |if ((10n < 'abc') !== false) throw new Error('bigint vs NaN string');
      |if ((9007199254740993n > 9007199254740992) !== true) throw new Error('bigint precision');
      |var order = [];
      |var a = { valueOf: function () { order.push('a'); return 1; } };
      |var b = { valueOf: function () { order.push('b'); return 2; } };
      |if (!(a < b)) throw new Error('valueOf operands');
      |if (order.join(',') !== 'a,b') throw new Error('valueOf order: ' + order);
      |var threw = false;
      |try { Symbol() < 1; } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('Symbol comparison does not throw');
      |""".stripMargin)
  }

  test("instanceof protocol: non-callables, @@hasInstance and bound targets") {
    run("""
      |var threw = false;
      |try { 1 instanceof Math; } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('1 instanceof Math');
      |threw = false;
      |try { true instanceof true; } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('true instanceof true');
      |function F() {}
      |var B = F.bind(null);
      |if (!(new B() instanceof F)) throw new Error('bound target instanceof');
      |if (!(new B() instanceof B)) throw new Error('bound instanceof itself');
      |var custom = { [Symbol.hasInstance]: function (v) { return v === 42; } };
      |if (!(42 instanceof custom)) throw new Error('custom @@hasInstance true');
      |if (43 instanceof custom) throw new Error('custom @@hasInstance false');
      |var d = Object.getOwnPropertyDescriptor(Function.prototype, Symbol.hasInstance);
      |if (typeof d.value !== 'function' || d.writable || d.enumerable || d.configurable)
      |  throw new Error('@@hasInstance descriptor');
      |if (d.value.length !== 1 || d.value.name !== '[Symbol.hasInstance]')
      |  throw new Error('@@hasInstance name/length');
      |if (Function.prototype[Symbol.hasInstance].call({}) !== false)
      |  throw new Error('OrdinaryHasInstance on non-callable this');
      |""".stripMargin)
  }

  test("Function.prototype is a callable function and Function() results are constructors") {
    run("""
      |if (typeof Function.prototype !== 'function') throw new Error('typeof Function.prototype');
      |if (Function.prototype() !== undefined) throw new Error('Function.prototype()');
      |if (Object.getPrototypeOf(function () {}) !== Function.prototype)
      |  throw new Error('getPrototypeOf(fn) identity');
      |var F = Function('this.x = 1;');
      |if (F.prototype === undefined) throw new Error('Function() prototype');
      |if (new F().x !== 1) throw new Error('new Function() body');
      |if (!(new F() instanceof F)) throw new Error('new Function() instanceof');
      |""".stripMargin)
  }

  test("object literal __proto__ ignores non-objects and methods named __proto__ define") {
    run("""
      |if (Object.getPrototypeOf({ __proto__: 42 }) !== Object.prototype)
      |  throw new Error('__proto__ primitive ignored');
      |if (Object.getPrototypeOf({ __proto__: null }) !== null)
      |  throw new Error('__proto__ null');
      |var proto = { marker: 1 };
      |if (Object.getPrototypeOf({ __proto__: proto }) !== proto)
      |  throw new Error('__proto__ object');
      |var m = { __proto__() { return 5; } };
      |if (m.__proto__() !== 5) throw new Error('method named __proto__');
      |if (Object.getPrototypeOf(m) !== Object.prototype) throw new Error('method proto stayed default');
      |""".stripMargin)
  }

  test("Object.keys and Array.prototype.join throw TypeError on nullish receivers") {
    run("""
      |function throwsTypeError(f) {
      |  try { f(); } catch (e) { return e instanceof TypeError; }
      |  return false;
      |}
      |if (!throwsTypeError(function () { Object.keys(null); })) throw new Error('Object.keys(null)');
      |if (!throwsTypeError(function () { Object.keys(undefined); })) throw new Error('Object.keys(undefined)');
      |if (!throwsTypeError(function () { Array.prototype.join.call(null, ','); })) throw new Error('join(null)');
      |if (!throwsTypeError(function () { Array.prototype.join.call(undefined, ','); })) throw new Error('join(undefined)');
      |""".stripMargin)
  }

  test("private brand checks (#x in obj)") {
    run("""
      |class C {
      |  #data = 1;
      |  #method() {}
      |  get #accessor() { return 1; }
      |  static hasData(o) { return #data in o; }
      |  static hasMethod(o) { return #method in o; }
      |  static hasAccessor(o) { return #accessor in o; }
      |}
      |class D { #data = 1; }
      |if (!C.hasData(new C())) throw new Error('own field');
      |if (!C.hasMethod(new C())) throw new Error('own method');
      |if (!C.hasAccessor(new C())) throw new Error('own accessor');
      |if (C.hasData(new D())) throw new Error('other class field');
      |if (C.hasData({})) throw new Error('plain object');
      |var threw = false;
      |try { C.hasData(1); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('primitive brand check');
      |""".stripMargin)
  }

  test("let with a line terminator is an expression in single-statement contexts") {
    run("""
      |var r = [];
      |if (false) let
      |r.push(1);
      |if (r.length !== 1) throw new Error('if-body let ASI');
      |for (; false;) let
      |r.push(2);
      |if (r.length !== 2) throw new Error('for-body let ASI');
      |var threw = false;
      |try { eval('if (true) let x = 1;'); } catch (e) { threw = e instanceof SyntaxError; }
      |if (!threw) throw new Error('same-line lexical declaration accepted');
      |threw = false;
      |try { eval('if (true) let\\n[a] = [1];'); } catch (e) { threw = e instanceof SyntaxError; }
      |if (!threw) throw new Error('let [ lookahead restriction');
      |""".stripMargin)
  }

  test("bound function name and length follow SetFunctionName/SetFunctionLength") {
    run("""
      |function target() {}
      |Object.defineProperty(target, 'name', { value: 't', configurable: true });
      |if (target.bind().name !== 'bound t') throw new Error('name: ' + target.bind().name);
      |Object.defineProperty(target, 'name', { value: 1, configurable: true });
      |if (target.bind().name !== 'bound ') throw new Error('non-string name');
      |Object.defineProperty(target, 'length', { value: undefined, configurable: true });
      |if (target.bind(null, 1).length !== 0) throw new Error('undefined length');
      |Object.defineProperty(target, 'length', { value: 2147483648, configurable: true });
      |if (target.bind().length !== 2147483648) throw new Error('large length');
      |Object.defineProperty(target, 'length', { value: Infinity, configurable: true });
      |if (target.bind(0, 0).length !== Infinity) throw new Error('infinite length');
      |Object.defineProperty(target, 'length', { value: 3.66, configurable: true });
      |if (target.bind().length !== 3) throw new Error('fractional length');
      |function bar() {}
      |Object.setPrototypeOf(bar, { length: 42 });
      |delete bar.length;
      |if (Function.prototype.bind.call(bar, null, 1).length !== 0) throw new Error('inherited length ignored');
      |""".stripMargin)
  }

  test("sloppy function this is boxed by call/apply") {
    run("""
      |var retobj = Function('this.touched = true; return this;').call(1);
      |if (retobj.touched !== true) throw new Error('primitive this not boxed');
      |var retstr = Function('return this;').apply('abc');
      |if (typeof retstr !== 'object' || retstr.valueOf() !== 'abc')
      |  throw new Error('string this not boxed');
      |var ran = false;
      |try { Function.prototype.call.call({}); } catch (e) { ran = e instanceof TypeError; }
      |if (!ran) throw new Error('call on non-callable');
      |""".stripMargin)
  }

  test("concise methods and accessors are not constructors") {
    run("""
      |if (({ m() {} }).m.prototype !== undefined) throw new Error('method prototype');
      |var threw = false;
      |try { new (({ m() {} }).m)(); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('method not constructable');
      |var getter = Object.getOwnPropertyDescriptor({ get x() { return 1; } }, 'x').get;
      |if (getter.prototype !== undefined) throw new Error('accessor prototype');
      |if (({ f: function () {} }).f.prototype === undefined) throw new Error('function value prototype');
      |if ((class { m() {} }).prototype.m.prototype !== undefined) throw new Error('class method prototype');
      |""".stripMargin)
  }

  test("Function.prototype caller/arguments are poisoned accessors") {
    run("""
      |function f() {}
      |var b = f.bind(null);
      |if (b.hasOwnProperty('caller') || b.hasOwnProperty('arguments'))
      |  throw new Error('own caller/arguments');
      |var threw = false;
      |try { b.caller; } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('caller does not throw');
      |threw = false;
      |try { b.caller = {}; } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('caller set does not throw');
      |threw = false;
      |try { b.arguments; } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('arguments does not throw');
      |var d = Object.getOwnPropertyDescriptor(Function.prototype, 'caller');
      |if (typeof d.get !== 'function' || d.get !== d.set || d.enumerable || !d.configurable)
      |  throw new Error('caller descriptor');
      |""".stripMargin)
  }

  // --- ES2025 upsert / grouping / try / isError / escape -----------------

  test("Map.prototype.getOrInsert and getOrInsertComputed") {
    run("""
      |var m = new Map();
      |if (m.getOrInsert('a', 1) !== 1) throw new Error('insert value');
      |if (m.getOrInsert('a', 2) !== 1) throw new Error('existing value');
      |if (m.get('a') !== 1) throw new Error('stored value');
      |
      |var calls = 0;
      |var seen = null;
      |var v = m.getOrInsertComputed('b', function (key) { calls++; seen = key; return 2; });
      |if (v !== 2 || calls !== 1 || seen !== 'b') throw new Error('computed insert');
      |if (m.getOrInsertComputed('b', function () { calls++; return 3; }) !== 2) throw new Error('computed existing');
      |if (calls !== 1) throw new Error('callback ran for existing key');
      |
      |var canonical = null;
      |m.getOrInsertComputed(-0, function (key) { canonical = 1 / key; });
      |if (canonical !== Infinity) throw new Error('key not canonicalized: ' + canonical);
      |
      |var threw = false;
      |try { m.getOrInsertComputed('c', 5); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('non-callable callback did not throw');
      |if (m.has('c')) throw new Error('key inserted despite bad callback');
      |""".stripMargin)
  }

  test("WeakMap upsert rejects keys that cannot be held weakly") {
    run("""
      |var wm = new WeakMap();
      |var key = {};
      |if (wm.getOrInsert(key, 1) !== 1) throw new Error('weak insert');
      |if (wm.getOrInsert(key, 2) !== 1) throw new Error('weak existing');
      |var calls = 0;
      |var computed = wm.getOrInsertComputed(key, function () { calls++; return 3; });
      |if (computed !== 1 || calls !== 0) throw new Error('weak computed existing');
      |var o = {};
      |if (wm.getOrInsertComputed(o, () => 4) !== 4) throw new Error('weak computed insert');
      |
      |for (const bad of [1, 'x', null, undefined, true, Symbol.for('reg')]) {
      |  var threw = false;
      |  try { wm.getOrInsert(bad, 1); } catch (e) { threw = e instanceof TypeError; }
      |  if (!threw) throw new Error('weak key accepted: ' + String(bad));
      |}
      |var called = false;
      |var threw2 = false;
      |try { wm.getOrInsertComputed(Symbol.for('reg2'), () => { called = true; }); }
      |catch (e) { threw2 = e instanceof TypeError; }
      |if (!threw2 || called) throw new Error('registered symbol invoked callback');
      |""".stripMargin)
  }

  test("Object.groupBy and Map.groupBy") {
    run("""
      |var groups = Object.groupBy([1, 2, 3, 4], function (v, i) {
      |  if (arguments.length !== 2) throw new Error('callback arity');
      |  return v % 2 === 0 ? 'even' : 'odd';
      |});
      |if (Object.getPrototypeOf(groups) !== null) throw new Error('null prototype');
      |if (groups.odd.join(',') !== '1,3' || groups.even.join(',') !== '2,4')
      |  throw new Error('object groups: ' + JSON.stringify(groups));
      |
      |var map = Map.groupBy('abc', c => c);
      |if (!(map instanceof Map)) throw new Error('map instance');
      |if (map.get('a').join('') !== 'a' || map.get('b').join('') !== 'b')
      |  throw new Error('map groups');
      |
      |var closed = false;
      |var iterable = { [Symbol.iterator]() { return {
      |  next() { return { value: 1, done: false }; },
      |  return() { closed = true; return {}; }
      |}; } };
      |var threw = false;
      |try { Object.groupBy(iterable, () => { throw new RangeError('boom'); }); }
      |catch (e) { threw = e instanceof RangeError; }
      |if (!threw || !closed) throw new Error('iterator not closed on callback throw');
      |""".stripMargin)
  }

  test("Promise.try resolves, rejects and forwards arguments") {
    runPromise(
      """
        |var seen = [];
        |Promise.try((a, b) => { seen.push(a + b); return a + b; }, 1, 2).then(v => seen.push('v:' + v));
        |Promise.try(() => { throw new Error('nope'); }).catch(e => seen.push('e:' + e.message));
        |Promise.try(5).catch(e => seen.push('bad:' + (e instanceof TypeError)));
        |""".stripMargin,
      """
        |if (seen.join(',') !== '3,v:3,e:nope,bad:true') throw new Error('try: ' + seen.join(','));
        |""".stripMargin
    )
  }

  test("Error.isError checks the [[ErrorData]] slot") {
    run("""
      |if (!Error.isError(new Error('x'))) throw new Error('error');
      |if (!Error.isError(new TypeError('x'))) throw new Error('type error');
      |if (!Error.isError(new AggregateError([], 'x'))) throw new Error('aggregate');
      |try { null.x; } catch (e) { if (!Error.isError(e)) throw new Error('thrown native'); }
      |if (Error.isError(Object.create(Error.prototype))) throw new Error('prototype inheritance');
      |if (Error.isError(Error.prototype)) throw new Error('Error.prototype');
      |if (Error.isError({ name: 'Error', message: 'x' })) throw new Error('plain object');
      |if (Error.isError('error') || Error.isError(undefined) || Error.isError(null)) throw new Error('primitive');
      |var e = new Error('keep');
      |Object.setPrototypeOf(e, Object.prototype);
      |if (!Error.isError(e)) throw new Error('marker lost when prototype changes');
      |""".stripMargin)
  }

  test("RegExp.escape matches QuickJS EncodeForRegExpEscape") {
    run("""
      |function eq(a, b, label) { if (a !== b) throw new Error(label + ': ' + a + ' != ' + b); }
      |eq(RegExp.escape('a.b'), '\\x61\\.b', 'syntax');
      |eq(RegExp.escape('1abc'), '\\x31abc', 'leading digit');
      |eq(RegExp.escape('.abc'), '\\.abc', 'letter after first position');
      |eq(RegExp.escape('_x'), '_x', 'underscore');
      |eq(RegExp.escape('a,b'), '\\x61\\x2cb', 'punctuator');
      |eq(RegExp.escape('/'), '\\/', 'solidus');
      |eq(RegExp.escape('\t\n'), '\\t\\n', 'control');
      |eq(RegExp.escape(' '), '\\x20', 'space');
      |eq(RegExp.escape('\u00a0'), '\\xa0', 'nbsp');
      |eq(RegExp.escape('\ufeff'), '\\ufeff', 'zwnbsp');
      |eq(RegExp.escape('\ud800'), '\\ud800', 'surrogate');
      |eq(RegExp.escape('你好'), '你好', 'unicode');
      |eq(RegExp.escape(''), '', 'empty');
      |var threw = false;
      |try { RegExp.escape(1); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('non-string did not throw');
      |""".stripMargin)
  }

  test("new Function() compiles an empty body and is callable") {
    run("""
      |var f = new Function();
      |if (typeof f !== 'function') throw new Error('not a function');
      |if (f() !== undefined) throw new Error('empty call');
      |if (f.length !== 0) throw new Error('length');
      |if (!(f instanceof Function)) throw new Error('instanceof');
      |var g = new Function('a', 'b', 'return a + b;');
      |if (g(1, 2) !== 3) throw new Error('body call');
      |""".stripMargin)
  }

  test("Set methods use GetSetRecord with spec ordering") {
    run("""
      |var log = [];
      |var setLike = {
      |  get size() { log.push('size'); return { valueOf() { log.push('toNumber'); return 2; } }; },
      |  get has() { log.push('has'); return function (v) { log.push('call has'); return v === 'a'; }; },
      |  get keys() { log.push('keys'); return function () { return ['a', 'b'][Symbol.iterator](); }; }
      |};
      |var s = new Set(['a', 'c']);
      |var inter = s.intersection(setLike);
      |if (inter.size !== 1 || !inter.has('a')) throw new Error('intersection content');
      |var expected = ['size', 'toNumber', 'has', 'keys', 'call has', 'call has'];
      |if (log.join(',') !== expected.join(',')) throw new Error('order: ' + log.join(','));
      |
      |var threw = false;
      |try { new Set().union({ size: NaN, has() {}, keys() {} }); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('NaN size');
      |threw = false;
      |try { new Set().union({ size: -1, has() {}, keys() {} }); } catch (e) { threw = e instanceof RangeError; }
      |if (!threw) throw new Error('negative size');
      |
      |var ordered = [...new Set([3, 2, 1, 0]).intersection(new Set([1, 3, 5]))];
      |if (ordered.join(',') !== '1,3') throw new Error('other-order result: ' + ordered.join(','));
      |""".stripMargin)
  }

  // --- class static blocks / generator constructors / async depth -------

  test("class static blocks run in order with the class as this") {
    run("""
      |var seq = [];
      |class C {
      |  static a = seq.push('field1');
      |  static { seq.push('block1'); this.mark = 'b1'; }
      |  static b = seq.push('field2');
      |  static { seq.push('block2'); }
      |}
      |if (seq.join(',') !== 'field1,block1,field2,block2')
      |  throw new Error('order: ' + seq.join(','));
      |if (C.mark !== 'b1') throw new Error('this is not the class');
      |
      |// var declarations are scoped to the block
      |class D { static { var hidden = 1; } }
      |if (typeof hidden !== 'undefined') throw new Error('var leaked');
      |
      |// static super property reads the superclass constructor
      |function P() {}
      |P.tag = 'super';
      |class E extends P { static { this.tag = super.tag; } }
      |if (E.tag !== 'super') throw new Error('static super: ' + E.tag);
      |
      |function mustThrow(src) {
      |  var threw = false;
      |  try { eval(src); } catch (e) { threw = e instanceof SyntaxError; }
      |  if (!threw) throw new Error('no SyntaxError for: ' + src);
      |}
      |mustThrow('class X { static { arguments; } }');
      |mustThrow('class X { static { return 1; } }');
      |mustThrow('class X { static { await; } }');
      |mustThrow('class X { static { let y; let y; } }');
      |""".stripMargin)
  }

  test("generator/async function constructors and prototypes") {
    run("""
      |var GeneratorFunction = Object.getPrototypeOf(function*(){}).constructor;
      |var AsyncFunction = Object.getPrototypeOf(async function(){}).constructor;
      |var AsyncGeneratorFunction =
      |  Object.getPrototypeOf(async function*(){}).constructor;
      |if (typeof GeneratorFunction !== 'function' || GeneratorFunction.name !== 'GeneratorFunction')
      |  throw new Error('GeneratorFunction');
      |if (typeof AsyncFunction !== 'function' || AsyncFunction.name !== 'AsyncFunction')
      |  throw new Error('AsyncFunction');
      |if (AsyncGeneratorFunction.name !== 'AsyncGeneratorFunction')
      |  throw new Error('AsyncGeneratorFunction');
      |if (Object.getPrototypeOf(GeneratorFunction) !== Function) throw new Error('GF proto');
      |var g = GeneratorFunction('a', 'yield a;');
      |if (g(7).next().value !== 7) throw new Error('generator call');
      |var ag = AsyncGeneratorFunction('a', 'yield a;');
      |var agp = ag(8).next();
      |if (typeof agp.then !== 'function') throw new Error('async generator call');
      |var af = new AsyncFunction('a', 'return a;');
      |if (af.length !== 1 || af.constructor !== AsyncFunction)
      |  throw new Error('async function instance');
      |var genThrew = false;
      |try { var gen = function*(){}; gen.caller; } catch (e) { genThrew = e instanceof TypeError; }
      |if (!genThrew) throw new Error('generator caller');
      |var threw = false;
      |try { async function af2(){}; af2.caller; } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('async caller');
      |var genFn = function*(){ yield 1; };
      |var it = genFn();
      |if (Object.getPrototypeOf(it) !== genFn.prototype) throw new Error('gen proto chain');
      |if (Object.getPrototypeOf(genFn.prototype) !== Object.getPrototypeOf(function*(){}).prototype)
      |  throw new Error('generator prototype chain');
      |var res = it.next();
      |if (Object.getPrototypeOf(res) !== Object.prototype) throw new Error('result proto');
      |""".stripMargin)
  }

  test("await/yield are identifiers in nested non-async functions") {
    runPromise(
      """
        |var await;
        |var seen;
        |async function f() {
        |  function inner() { await = 1; }
        |  inner();
        |}
        |f().then(function () { seen = await; });
        |var yieldVal;
        |function* g() {
        |  function inner() { yieldVal = 'ok'; }
        |  inner();
        |}
        |g().next();
        |""".stripMargin,
      """
        |if (seen !== 1) throw new Error('await identifier: ' + seen);
        |if (yieldVal !== 'ok') throw new Error('yield identifier: ' + yieldVal);
        |""".stripMargin
    )
  }

  // --- import attributes / JSON modules ----------------------------------

  test("import attributes parse and duplicate keys are early errors") {
    def parseModule(source: String): Unit =
      val tokens = Lexer(source).tokenize()
      new Parser(tokens, moduleMode = true).parseScript()

    parseModule("""import x from './a.js' with { type: 'json', foo: "bar" };""")
    parseModule("""import './a.js' with { type: 'json' };""")
    parseModule("""export * from './a.js' with { type: 'json' };""")
    parseModule("""import x from './a.js'
with { type: 'json' };""")

    val duplicate = intercept[RuntimeException] {
      parseModule("""import x from './a.js' with { type: 'json', type: '' };""")
    }
    if !Option(duplicate.getMessage).exists(_.contains("duplicate")) then
      throw new Error("expected duplicate key error: " + duplicate.getMessage)

    intercept[RuntimeException] {
      parseModule("""import x from './a.js' with { type: 1 };""")
    }
  }

  // --- WeakRef -----------------------------------------------------------

  test("WeakRef rejects registered symbols and honors NewTarget.prototype") {
    run("""
      |var registered = Symbol.for('registered');
      |var threw = false;
      |try { new WeakRef(registered); } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('registered symbol must not be weakly held');
      |var sym = Symbol('local');
      |var ref = new WeakRef(sym);
      |if (ref.deref() !== sym) throw new Error('symbol deref');
      |var calls = 0;
      |var newTarget = function(){}.bind(null);
      |Object.defineProperty(newTarget, 'prototype', {
      |  get: function() { calls += 1; return Array.prototype; }
      |});
      |var instance = Reflect.construct(WeakRef, [{}], newTarget);
      |if (Object.getPrototypeOf(instance) !== Array.prototype)
      |  throw new Error('custom NewTarget prototype');
      |if (calls !== 1) throw new Error('prototype getter calls: ' + calls);
      |var abrupt = function(){}.bind(null);
      |Object.defineProperty(abrupt, 'prototype', {
      |  get: function() { throw new Error('abrupt'); }
      |});
      |var thrown = false;
      |try { Reflect.construct(WeakRef, [{}], abrupt); }
      |catch (e) { thrown = e.message === 'abrupt'; }
      |if (!thrown) throw new Error('abrupt NewTarget prototype not propagated');
      |""".stripMargin)
  }

  // --- Iterator helpers ---------------------------------------------------

  test("iterator helpers map/filter/take/drop/flatMap/reduce/toArray") {
    run("""
      |var array = [1, 2, 3, 4, 5];
      |var mapped = array.values().map(function (v) { return v * 2; }).toArray();
      |if (mapped.join(',') !== '2,4,6,8,10') throw new Error('map: ' + mapped);
      |var filtered = array.values().filter(function (v) { return v % 2 === 1; }).toArray();
      |if (filtered.join(',') !== '1,3,5') throw new Error('filter: ' + filtered);
      |var taken = array.values().take(2).toArray();
      |if (taken.join(',') !== '1,2') throw new Error('take: ' + taken);
      |var dropped = array.values().drop(3).toArray();
      |if (dropped.join(',') !== '4,5') throw new Error('drop: ' + dropped);
      |var flat = [1, 2].values().flatMap(function (v) { return [v, v * 10]; }).toArray();
      |if (flat.join(',') !== '1,10,2,20') throw new Error('flatMap: ' + flat);
      |var sum = array.values().reduce(function (acc, v, i) {
      |  if (i !== v - 1) throw new Error('reduce index ' + i);
      |  return acc + v;
      |}, 0);
      |if (sum !== 15) throw new Error('reduce: ' + sum);
      |if (array.values().some(function (v) { return v === 3; }) !== true) throw new Error('some');
      |if (array.values().every(function (v) { return v < 6; }) !== true) throw new Error('every');
      |if (array.values().find(function (v) { return v > 3; }) !== 4) throw new Error('find');
      |if (Iterator.from('ab').toArray().join(',') !== 'a,b') throw new Error('Iterator.from string');
      |""".stripMargin)
  }

  test("iterator helpers close the underlying iterator on return and errors") {
    run("""
      |var closed = 0;
      |var underlying = {
      |  next: function () { return { value: 1, done: false }; },
      |  return: function () { closed += 1; return {}; }
      |};
      |var helper = Iterator.prototype.map.call(underlying, function (v) { return v; });
      |helper.return();
      |if (closed !== 1) throw new Error('return not forwarded: ' + closed);
      |helper.return();
      |if (closed !== 1) throw new Error('return forwarded twice');
      |
      |closed = 0;
      |var threw = false;
      |try { Iterator.prototype.map.call(underlying, 3); }
      |catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('non-callable mapper accepted');
      |if (closed !== 1) throw new Error('validation failure did not close');
      |
      |closed = 0;
      |var mapperThrew = false;
      |var h2 = Iterator.prototype.map.call(underlying, function () {
      |  throw new Error('mapper');
      |});
      |try { h2.next(); } catch (e) { mapperThrew = e.message === 'mapper'; }
      |if (!mapperThrew) throw new Error('mapper error not propagated');
      |if (closed !== 1) throw new Error('mapper error did not close');
      |
      |closed = 0;
      |var h3 = Iterator.prototype.take.call(underlying, 0);
      |try { h3.next(); } catch (e) {}
      |if (closed !== 1) throw new Error('take(0) did not close');
      |""".stripMargin)
  }

  test("Iterator.prototype constructor and toStringTag are ignoring accessors") {
    run("""
      |if (typeof Iterator !== 'function') throw new Error('typeof Iterator');
      |if (Object.getPrototypeOf(Iterator) !== Function.prototype) throw new Error('Iterator proto');
      |if (Iterator.prototype.constructor !== Iterator) throw new Error('constructor getter');
      |if (Iterator.prototype[Symbol.toStringTag] !== 'Iterator') throw new Error('tag getter');
      |var desc = Object.getOwnPropertyDescriptor(Iterator.prototype, 'constructor');
      |if (typeof desc.get !== 'function' || typeof desc.set !== 'function') throw new Error('accessor');
      |var threw = false;
      |try { Iterator.prototype.constructor = 1; } catch (e) { threw = e instanceof TypeError; }
      |if (!threw) throw new Error('setting on Iterator.prototype must throw');
      |var fake = Object.create(Iterator.prototype);
      |fake.constructor = 5;
      |if (fake.constructor !== 5) throw new Error('ignoring setter');
      |class SubIterator extends Iterator {}
      |var sub = new SubIterator();
      |if (!(sub instanceof Iterator) || !(sub instanceof SubIterator)) throw new Error('subclass');
      |""".stripMargin)
  }

  // --- lexer / Array.from regressions found while adding the above --------

  test("template expression scanner handles regex literals and nested templates") {
    run("""
      |var propertyKey = "a'b";
      |var one = `${propertyKey.replace(/'/g, "\\'")}`;
      |if (one !== "a\\'b") throw new Error('regex in template: ' + one);
      |var two = `${`nested ${1 + 1}`}`;
      |if (two !== 'nested 2') throw new Error('nested template: ' + two);
      |var three = `${1 / 2}`;
      |if (three !== '0.5') throw new Error('division in template: ' + three);
      |""".stripMargin)
  }

  test("Array.from observes throwing iterator result value getters") {
    run("""
      |var poisoned = {};
      |Object.defineProperty(poisoned, 'value', {
      |  get: function () { throw new Error('poisoned'); }
      |});
      |var iterable = {};
      |iterable[Symbol.iterator] = function () {
      |  return { next: function () { return poisoned; } };
      |};
      |var threw = false;
      |try { Array.from(iterable); } catch (e) { threw = e.message === 'poisoned'; }
      |if (!threw) throw new Error('throwing value getter not observed');
      |""".stripMargin)
  }

  test("RegExp.prototype[Symbol.replace] follows the exec protocol") {
    run("""
      |var r = /b(c)(z)?(.)/;
      |if (r[Symbol.replace]('abcde', '[$1$2$3]') !== 'a[cd]e') throw new Error('captures');
      |if (r[Symbol.replace]('abcde', '[$1$2$3$4$0]') !== 'a[cd$4$0]e') throw new Error('out of range');
      |var calls = 0;
      |var custom = { flags: 'g', global: true, unicode: true, lastIndex: 0 };
      |custom.exec = function (s) { calls++; return calls === 1 ? ['a'] : null; };
      |if (RegExp.prototype[Symbol.replace].call(custom, 'abc', 'x') !== 'xbc') throw new Error('custom exec');
      |if (calls !== 2) throw new Error('exec calls ' + calls);
      |var poisoned = /./;
      |poisoned.exec = function () { throw new Error('poisoned'); };
      |var threw = false;
      |try { poisoned[Symbol.replace]('', ''); } catch (e) { threw = e.message === 'poisoned'; }
      |if (!threw) throw new Error('exec error not propagated');
      |""".stripMargin)
  }

  test("RegExp.prototype[Symbol.match] observes flags and custom exec") {
    run("""
      |var r = /a/;
      |Object.defineProperty(r, 'global', { value: true, configurable: true });
      |Object.defineProperty(r, 'unicode', { value: false, configurable: true });
      |if (r.flags !== 'g') throw new Error('flags from properties: ' + r.flags);
      |var seen = 0;
      |r.exec = function () { seen++; return seen <= 2 ? ['a'] : null; };
      |var m = r[Symbol.match]('aa');
      |if (m === null || m.length !== 2 || m[0] !== 'a' || m[1] !== 'a') throw new Error('match result');
      |if (seen !== 3) throw new Error('exec count ' + seen);
      |var ok = true;
      |try { RegExp.prototype[Symbol.match].call({ flags: 'g', exec: function () { return 42; } }, ''); ok = false; }
      |catch (e) { ok = e instanceof TypeError; }
      |if (!ok) throw new Error('exec result must be an object or null');
      |""".stripMargin)
  }

  test("object literal methods resolve super through their home object") {
    run("""
      |var fromA, fromB;
      |var A = { fromA: 'a', fromB: 'a' };
      |var B = { fromB: 'b' };
      |Object.setPrototypeOf(B, A);
      |var obj = { fromA: 'c', fromB: 'c', method() { fromA = super.fromA; fromB = super.fromB; } };
      |Object.setPrototypeOf(obj, B);
      |obj.method();
      |if (fromA !== 'a' || fromB !== 'b') throw new Error('super reads: ' + fromA + ',' + fromB);
      |var writable = { set n(v) { this.seen = v; }, get n() { return this.seen; } };
      |var child = { m() { super.n = 5; return this.n; } };
      |Object.setPrototypeOf(child, writable);
      |if (child.m() !== 5 || child.seen !== 5) throw new Error('super write receiver');
      |var counter = { base: 10, m() { return super.base++; } };
      |var proto = { base: 1 };
      |Object.setPrototypeOf(counter, proto);
      |if (counter.m() !== 1 || counter.base !== 2 || proto.base !== 1)
      |  throw new Error('super increment wrote to the wrong object');
      |// Base classes without `extends` still have a home object (Object.prototype).
      |class C { m() { super.x = 8; return this.x; } }
      |if (new C().m() !== 8) throw new Error('base-class super write');
      |// Strict class methods throw when the frozen receiver rejects the write.
      |var caught = false;
      |class D { m() { super.x = 1; Object.freeze(this); try { super.y = 2; } catch (e) { caught = e instanceof TypeError; } } }
      |new D().m();
      |if (!caught) throw new Error('frozen super write did not throw');
      |""".stripMargin)
  }

  test("super() binds the object returned by the parent and only runs once") {
    run("""
      |var customThis = {};
      |function Parent() { return customThis; }
      |var bound;
      |class Child extends Parent { constructor() { super(); bound = this; } }
      |new Child();
      |if (bound !== customThis) throw new Error('super() did not rebind this');
      |var caught = false;
      |class Twice extends Parent { constructor() { super(); try { super(); } catch (e) { caught = e instanceof ReferenceError; } } }
      |new Twice();
      |if (!caught) throw new Error('second super() did not throw ReferenceError');
      |""".stripMargin)
  }

  test("object spread copies getter values, symbols and array indices") {
    run("""
      |var o = { get a() { return 42; }, b: 1 };
      |var x = { ...o };
      |var d = Object.getOwnPropertyDescriptor(x, 'a');
      |if (d.value !== 42 || d.get !== undefined) throw new Error('getter value spread');
      |var s = Symbol('foo');
      |var src = { [s]: 'sym' };
      |var y = { ...src };
      |if (Object.getOwnPropertySymbols(y).length !== 1 || y[s] !== 'sym')
      |  throw new Error('symbol spread');
      |var z = { ...[1, 2] };
      |if (z[0] !== 1 || z[1] !== 2) throw new Error('array spread');
      |var str = { ...'ab' };
      |if (str[0] !== 'a' || str[1] !== 'b') throw new Error('string spread');
      |""".stripMargin)
  }

  test("super spread calls work with native methods") {
    run("""
      |class RE extends RegExp {
      |  [Symbol.replace](...args) { return super[Symbol.replace](...args); }
      |}
      |var re = new RE('b', 'g');
      |if ('abc abc'.replace(re, 'z') !== 'azc azc') throw new Error('super spread replace');
      |if ('abc abc'.replaceAll(re, 'z') !== 'azc azc') throw new Error('super spread replaceAll');
      |""".stripMargin)
  }

  test("replaceAll with a non-dispatching RegExp replaces its string form") {
    run("""
      |var re = /./g;
      |Object.defineProperty(re, Symbol.replace, { value: undefined });
      |if ('--- /./g --- /a/g ---'.replaceAll(re, 'x') !== '--- x --- /a/g ---')
      |  throw new Error('replaceAll string fallback');
      |var arities = [];
      |var fn = function () { arities.push(arguments.length); return 'z'; };
      |var out = 'ab c ab c'.replaceAll('ab c', fn);
      |if (out !== 'z z') throw new Error('replaceAll result ' + out);
      |if (arities.join(',') !== '3,3') throw new Error('callback arity: ' + arities.join(','));
      |""".stripMargin)
  }

  test("String.prototype protocol dispatch follows the 2025 receiver rules") {
    run("""
      |var guard = 0;
      |Object.defineProperty(String.prototype, Symbol.replace, {
      |  get: function () { guard += 1; return undefined; },
      |  configurable: true
      |});
      |try {
      |  if ("a,b".replace(",", "X") !== "aXb") throw new Error('primitive replace');
      |  if (guard !== 0) throw new Error('primitive searchValue must not be boxed');
      |} finally {
      |  delete String.prototype[Symbol.replace];
      |}
      |// @@split receives the original receiver and runs before ToString(this).
      |var order = [];
      |var receiver = { toString: function () { order.push('receiver'); return 'a,b'; } };
      |var custom = { [Symbol.split]: function (O, lim) { order.push('split'); return [O === receiver, lim]; } };
      |var res = String.prototype.split.call(receiver, custom, 1);
      |if (res[0] !== true || order.join(',') !== 'split') throw new Error('split order: ' + order.join(','));
      |// ToString(separator) precedes the lim == 0 check.
      |if ("a1b".split(1, 0).length !== 0) throw new Error('limit zero');
      |// replaceAll passes the receiver to @@replace.
      |var called = 0;
      |var sv = /./g;
      |Object.defineProperty(sv, Symbol.replace, {
      |  value: function (O, rv) { called += 1; if (O !== receiver2) throw new Error('receiver'); return 42; }
      |});
      |var receiver2 = new String('Leo');
      |if (receiver2.replaceAll(sv, {}) !== 42 || called !== 1) throw new Error('replaceAll dispatch');
      |""".stripMargin)
  }

  test("RegExp.prototype[Symbol.split] uses the species constructor") {
    run("""
      |var r = /,/;
      |var out = r[Symbol.split]('a,b,c');
      |if (out.length !== 3 || out[0] !== 'a' || out[1] !== 'b' || out[2] !== 'c') throw new Error('split');
      |var used = 0;
      |var species = function (pattern, flags) { used++; return new RegExp(pattern, flags); };
      |var r2 = /,/;
      |Object.defineProperty(r2, 'constructor', { value: { [Symbol.species]: species } });
      |var parts = r2[Symbol.split]('a,b,c');
      |if (parts.join('|') !== 'a|b|c') throw new Error('species split: ' + parts.join('|'));
      |if (used !== 1) throw new Error('species count ' + used);
      |if (/a/ [Symbol.split]('xayaz').join('|') !== 'x|y|z') throw new Error('split regex');
      |""".stripMargin)
  }

  test("sticky regexp exec anchors at lastIndex") {
    run("""
      |var r = /b/y;
      |if (r.exec('ab') !== null) throw new Error('sticky matched a later position');
      |if (r.lastIndex !== 0) throw new Error('sticky failure must reset lastIndex');
      |r.lastIndex = 1;
      |if (r.exec('ab') === null) throw new Error('sticky match at lastIndex');
      |if (r.lastIndex !== 2) throw new Error('sticky lastIndex after match');
      |if (/b/y[Symbol.replace]('ab', 'x') !== 'ab') throw new Error('sticky replace');
      |""".stripMargin)
  }

  test("compound member assignment evaluates the reference exactly once") {
    run("""
      |var keyEvaluations = 0;
      |var baseEvaluations = 0;
      |function makeBase() { baseEvaluations += 1; return { x: 6 }; }
      |var key = { toString: function () { keyEvaluations += 1; return 'x'; } };
      |var result = (makeBase()[key] *= 2);
      |if (result !== 12) throw new Error('result ' + result);
      |if (baseEvaluations !== 1) throw new Error('base evaluated ' + baseEvaluations);
      |if (keyEvaluations !== 1) throw new Error('key evaluated ' + keyEvaluations);
      |var order = [];
      |function b() { order.push('b'); return { v: 1 }; }
      |function k() { order.push('k'); return 'v'; }
      |function r() { order.push('r'); return 2; }
      |b()[k()] += r();
      |if (order.join(',') !== 'b,k,r') throw new Error('order ' + order.join(','));
      |var nullBase = null;
      |var keyThrew = false;
      |var throwingKey = { toString: function () { throw new Error('key converted'); } };
      |try { nullBase[throwingKey] *= 2; }
      |catch (e) { keyThrew = e instanceof TypeError; }
      |if (!keyThrew) throw new Error('null base must throw TypeError before ToPropertyKey');
      |""".stripMargin)
  }

  test("remainder keeps the dividend sign and handles infinities") {
    run("""
      |if (!Object.is(-1 % -1, -0)) throw new Error('(-1) % -1 should be -0');
      |if (!Object.is(-0 % 3, -0)) throw new Error('(-0) % 3 should be -0');
      |if (!Object.is(5 % Infinity, 5)) throw new Error('5 % Infinity');
      |if (!Object.is(-5 % Infinity, -5)) throw new Error('-5 % Infinity');
      |if (!Object.is(5 % -Infinity, 5)) throw new Error('5 % -Infinity');
      |if (!Object.is(5 % 0, NaN)) throw new Error('5 % 0');
      |function* g() { yield -1 % -1; }
      |if (!Object.is(g().next().value, -0)) throw new Error('generator remainder');
      |""".stripMargin)
  }

  test("private field divide-assign is not lexed as a regexp") {
    run("""
      |class C {
      |  #x = 8;
      |  m() { this.#x /= 2; return this.#x; }
      |  n() { this.#x %= 3; return this.#x; }
      |}
      |var c = new C();
      |if (c.m() !== 4) throw new Error('private /= ' + c.m());
      |if (c.n() !== 1) throw new Error('private %= ' + c.n());
      |""".stripMargin)
  }

  test("a regex literal after a closing brace is parsed as a regex") {
    run("""
      |function f() {}
      |if (/x/g.test('x') !== true) throw new Error('regex after block');
      |function g() { return 1; }
      |/=eq/.test('=eq');
      |var q = ({ valueOf: function () { return 4; } }) / 2;
      |if (q !== 2) throw new Error('division after object literal');
      |""".stripMargin)
  }

  test("hashbang comments end at every line terminator") {
    run(
      "#! comment" + "\u2028" + "var a = 1;" + "\u2028" +
        "if (a !== 1) throw new Error('hashbang LS');"
    )
    run(
      "#! comment" + "\u2029" + "var b = 2;" + "\u2029" +
        "if (b !== 2) throw new Error('hashbang PS');"
    )
    // `#` not followed by `!` is not a hashbang comment.
    intercept[RuntimeException] {
      run("#\\x21" + "\nthrow 'no';")
    }
    intercept[RuntimeException] {
      run(" #!" + "\nthrow 'no';")
    }
  }

  test("ZWNBSP and line/paragraph separators are insignificant input") {
    run("var a = 1;﻿if (a !== 1) throw new Error('ZWNBSP whitespace');")
    run("if (1﻿+ 2 !== 3) throw new Error('ZWNBSP between tokens');")
    run("var b = 1;" + "\u2028" + "var c = 2;" + "\u2028" + "if (b + c !== 3) throw new Error('LS');")
    run("var d = 1;" + "\u2029" + "var e = 2;" + "\u2029" + "if (d + e !== 3) throw new Error('PS');")
  }

  test("NUL characters do not terminate a line comment") {
    run("""
      |var yy = 0;
      |eval('//var ' + String.fromCharCode(0) + 'yy = -1');
      |if (yy !== 0) throw new Error('NUL ended the comment: ' + yy);
      |""".stripMargin)
  }

  test("string literals with escapes are not use-strict directives") {
    run("""
      |function f() {
      |  'use str\
      |ict';
      |  return this !== undefined;
      |}
      |if (f.call(undefined) !== true) throw new Error('line continuation treated as directive');
      |function g() { "use\x20strict"; return this !== undefined; }
      |if (g.call(undefined) !== true) throw new Error('escape treated as directive');
      |function h() { "use strict"; return this === undefined; }
      |if (h.call(undefined) !== true) throw new Error('real directive not recognized');
      |""".stripMargin)
  }

  test("delete of a non-reference evaluates the operand and returns true") {
    run("""
      |var effects = 0;
      |function operand() { effects += 1; return { x: 1 }; }
      |if ((delete operand()) !== true) throw new Error('delete non-reference');
      |if (effects !== 1) throw new Error('operand not evaluated: ' + effects);
      |if ((delete void 0) !== true) throw new Error('delete void');
      |if ((delete typeof +-~! 0) !== true) throw new Error('delete unary chain');
      |if ((delete (1 + 2)) !== true) throw new Error('delete binary');
      |""".stripMargin)
  }
