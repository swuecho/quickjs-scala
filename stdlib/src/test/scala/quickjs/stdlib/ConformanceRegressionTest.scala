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
}
