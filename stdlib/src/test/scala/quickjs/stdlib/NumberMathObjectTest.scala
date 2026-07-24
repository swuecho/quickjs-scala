package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class NumberMathObjectTest extends FunSuite:

  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("Math basics") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    assertEquals(eval("Math.abs(-5)").toNumber, 5.0)
    assertEquals(eval("Math.max(1, 3, 2)").toNumber, 3.0)
    assertEquals(eval("Math.min(1, 3, 2)").toNumber, 1.0)
    assertEquals(eval("Math.floor(2.9)").toNumber, 2.0)
    assertEquals(eval("Math.ceil(2.1)").toNumber, 3.0)
    assertEquals(eval("Math.round(2.5)").toNumber, 3.0)
    assertEquals(eval("Math.pow(2, 3)").toNumber, 8.0)
  }

  test("Number statics and parse") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    assertEquals(eval("Number.isInteger(3)").toBoolean, true)
    assertEquals(eval("Number.isInteger(3.2)").toBoolean, false)
    assertEquals(eval("Number.isSafeInteger(9007199254740991)").toBoolean, true)
    assertEquals(eval("parseInt('0x10')").toNumber, 16.0)
    assertEquals(eval("parseFloat('3.14x')").toNumber, 3.14)
  }

  test("Number and Boolean prototypes") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    assertEquals(eval("Number.prototype.toFixed.call(1.25, 1)").toString, "1.3")
    assertEquals(eval("Boolean.prototype.toString.call(true)").toString, "true")
  }

  test("Number String and Boolean constructors create boxed objects with new") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var number = new Number(3);
      |var string = new String('ab');
      |var boolean = new Boolean(false);
      |var keys = {};
      |Object.defineProperty(keys, new Number(123), {});
      |Object.defineProperty(keys, 1e21, {});
      |Object.defineProperty(keys, 1e-7, {});
      |number instanceof Number && string instanceof String && boolean instanceof Boolean &&
      |  number.valueOf() === 3 && string.valueOf() === 'ab' && boolean.valueOf() === false &&
      |  string.length === 2 && string[0] === 'a' && !!boolean &&
      |  Object.hasOwn(keys, '123') && Object.hasOwn(keys, '1e+21') && Object.hasOwn(keys, '1e-7');
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object assign and values") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    assertEquals(
      eval("var a={x:1}; var b={y:2}; Object.assign(a,b).x + a.y;").toNumber,
      3.0
    )
    assertEquals(eval("Object.values({a:1,b:2}).length").toNumber, 2.0)
    assertEquals(eval("Object.entries({a:1}).length").toNumber, 1.0)
  }

  test("Object.assign uses Proxy get trap for source values") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var seen = '';
      |var source = new Proxy({}, {
      |  ownKeys: function() { return ['a']; },
      |  getOwnPropertyDescriptor: function(target, prop) {
      |    return { enumerable: true, configurable: true };
      |  },
      |  get: function(target, prop, receiver) {
      |    seen = prop;
      |    return 42;
      |  }
      |});
      |var target = {};
      |Object.assign(target, source);
      |seen === 'a' && target.a === 42;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.assign uses Proxy target getter fallback for source values") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var backing = {};
      |Object.defineProperty(backing, 'a', {
      |  enumerable: true,
      |  configurable: true,
      |  get: function() { return this === source ? 42 : 1; }
      |});
      |var source = new Proxy(backing, {});
      |var target = {};
      |Object.assign(target, source);
      |target.a === 42;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.assign copies enumerable symbol keys from Proxy source") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('k');
      |var source = new Proxy({}, {
      |  ownKeys: function() { return [sym]; },
      |  getOwnPropertyDescriptor: function(target, prop) {
      |    return { enumerable: true, configurable: true };
      |  },
      |  get: function(target, prop) {
      |    return prop === sym ? 42 : 0;
      |  }
      |});
      |var target = {};
      |Object.assign(target, source);
      |target[sym] === 42 && Object.getOwnPropertySymbols(target)[0] === sym;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.assign skips non-enumerable Proxy source keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var source = new Proxy({}, {
      |  ownKeys: function() { return ['a']; },
      |  getOwnPropertyDescriptor: function() {
      |    return { enumerable: false, configurable: true };
      |  },
      |  get: function() { return 42; }
      |});
      |var target = {};
      |Object.assign(target, source);
      |target.a === undefined;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.values and entries use Proxy get trap for values") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var seen = '';
      |var proxy = new Proxy({}, {
      |  ownKeys: function() { return ['a']; },
      |  getOwnPropertyDescriptor: function() {
      |    return { enumerable: true, configurable: true };
      |  },
      |  get: function(target, prop) {
      |    seen = seen + prop;
      |    return 42;
      |  }
      |});
      |var values = Object.values(proxy);
      |var entries = Object.entries(proxy);
      |seen === 'aa' &&
      |  values.length === 1 &&
      |  values[0] === 42 &&
      |  entries.length === 1 &&
      |  entries[0][0] === 'a' &&
      |  entries[0][1] === 42;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.values and entries skip symbols and non-enumerable Proxy keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('k');
      |var proxy = new Proxy({}, {
      |  ownKeys: function() { return ['a', 'b', sym]; },
      |  getOwnPropertyDescriptor: function(target, prop) {
      |    return { enumerable: prop !== 'b', configurable: true };
      |  },
      |  get: function(target, prop) {
      |    return prop === 'a' ? 1 : 99;
      |  }
      |});
      |var values = Object.values(proxy);
      |var entries = Object.entries(proxy);
      |values.length === 1 &&
      |  values[0] === 1 &&
      |  entries.length === 1 &&
      |  entries[0][0] === 'a' &&
      |  entries[0][1] === 1;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.values and entries use Proxy target getter fallback") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var backing = {};
      |Object.defineProperty(backing, 'a', {
      |  enumerable: true,
      |  configurable: true,
      |  get: function() { return this === proxy ? 42 : 1; }
      |});
      |var proxy = new Proxy(backing, {});
      |var values = Object.values(proxy);
      |var entries = Object.entries(proxy);
      |values[0] === 42 && entries[0][1] === 42;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.values and entries handle string primitives") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var values = Object.values('ab');
      |var entries = Object.entries('ab');
      |values.length === 2 &&
      |  values[0] === 'a' &&
      |  values[1] === 'b' &&
      |  entries.length === 2 &&
      |  entries[0][0] === '0' &&
      |  entries[0][1] === 'a' &&
      |  entries[1][0] === '1' &&
      |  entries[1][1] === 'b';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.values and entries reject null and undefined") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var valuesNull = false;
      |var valuesUndefined = false;
      |var entriesNull = false;
      |var entriesUndefined = false;
      |try { Object.values(null); } catch (e) { valuesNull = e instanceof TypeError; }
      |try { Object.values(undefined); } catch (e) { valuesUndefined = e instanceof TypeError; }
      |try { Object.entries(null); } catch (e) { entriesNull = e instanceof TypeError; }
      |try { Object.entries(undefined); } catch (e) { entriesUndefined = e instanceof TypeError; }
      |valuesNull && valuesUndefined && entriesNull && entriesUndefined;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getPrototypeOf accepts primitives and rejects nullish values") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var nullRejected = false;
      |var undefinedRejected = false;
      |try { Object.getPrototypeOf(null); } catch (e) { nullRejected = e instanceof TypeError; }
      |try { Object.getPrototypeOf(undefined); } catch (e) { undefinedRejected = e instanceof TypeError; }
      |Object.getPrototypeOf(1) === Number.prototype &&
      |  Object.getPrototypeOf('x') === String.prototype &&
      |  Object.getPrototypeOf(true) === Boolean.prototype &&
      |  nullRejected &&
      |  undefinedRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getPrototypeOf uses Proxy getPrototypeOf trap") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var proto = {};
      |var target = {};
      |var called = false;
      |var proxy = new Proxy(target, {
      |  getPrototypeOf: function(obj) {
      |    called = obj === target;
      |    return proto;
      |  }
      |});
      |Object.getPrototypeOf(proxy) === proto && called;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getPrototypeOf rejects inconsistent Proxy prototype") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var proto = {};
      |var target = Object.create(null);
      |Object.preventExtensions(target);
      |var proxy = new Proxy(target, {
      |  getPrototypeOf: function() { return proto; }
      |});
      |var rejected = false;
      |try { Object.getPrototypeOf(proxy); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.setPrototypeOf accepts non-null primitives") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var rejected = false;
      |try { Object.setPrototypeOf(null, {}); } catch (e) { rejected = e instanceof TypeError; }
      |Object.setPrototypeOf(1, {}) === 1 && rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.setPrototypeOf uses Proxy setPrototypeOf trap") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var proto = {};
      |var target = {};
      |var called = false;
      |var proxy = new Proxy(target, {
      |  setPrototypeOf: function(obj, newProto) {
      |    called = obj === target && newProto === proto;
      |    return Reflect.setPrototypeOf(obj, newProto);
      |  }
      |});
      |Object.setPrototypeOf(proxy, proto) === proxy &&
      |  called &&
      |  Object.getPrototypeOf(target) === proto;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.setPrototypeOf rejects false or inconsistent Proxy result") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var proto = {};
      |var falseProxy = new Proxy({}, {
      |  setPrototypeOf: function() { return false; }
      |});
      |var target = Object.create(null);
      |Object.preventExtensions(target);
      |var inconsistentProxy = new Proxy(target, {
      |  setPrototypeOf: function() { return true; }
      |});
      |var falseRejected = false;
      |var inconsistentRejected = false;
      |try { Object.setPrototypeOf(falseProxy, proto); } catch (e) { falseRejected = e instanceof TypeError; }
      |try { Object.setPrototypeOf(inconsistentProxy, proto); } catch (e) { inconsistentRejected = e instanceof TypeError; }
      |falseRejected && inconsistentRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.hasOwn and hasOwnProperty use Proxy descriptor trap") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var seen = '';
      |var proxy = new Proxy({}, {
      |  getOwnPropertyDescriptor: function(target, prop) {
      |    seen = seen + prop;
      |    if (prop === 'a') return { value: 1, configurable: true };
      |  }
      |});
      |Object.hasOwn(proxy, 'a') === true &&
      |  Object.hasOwn(proxy, 'b') === false &&
      |  Object.prototype.hasOwnProperty.call(proxy, 'a') === true &&
      |  seen === 'aba';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.hasOwn and hasOwnProperty preserve symbol keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('k');
      |var obj = {};
      |Object.defineProperty(obj, sym, { value: 1 });
      |var proxy = new Proxy({}, {
      |  getOwnPropertyDescriptor: function(target, prop) {
      |    if (prop === sym) return { value: 2, configurable: true };
      |  }
      |});
      |Object.hasOwn(obj, sym) &&
      |  Object.prototype.hasOwnProperty.call(obj, sym) &&
      |  Object.hasOwn(proxy, sym) &&
      |  Object.prototype.hasOwnProperty.call(proxy, sym);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.hasOwn enforces Proxy descriptor invariants") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var target = {};
      |Object.defineProperty(target, 'x', { value: 1, configurable: false });
      |var proxy = new Proxy(target, {
      |  getOwnPropertyDescriptor: function() { return undefined; }
      |});
      |var rejected = false;
      |try { Object.hasOwn(proxy, 'x'); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.hasOwn handles string primitives and rejects nullish targets") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var nullRejected = false;
      |var undefinedRejected = false;
      |try { Object.hasOwn(null, 'x'); } catch (e) { nullRejected = e instanceof TypeError; }
      |try { Object.hasOwn(undefined, 'x'); } catch (e) { undefinedRejected = e instanceof TypeError; }
      |Object.hasOwn('ab', '0') &&
      |  Object.hasOwn('ab', 'length') &&
      |  !Object.hasOwn('ab', '2') &&
      |  Object.prototype.hasOwnProperty.call('ab', '1') &&
      |  nullRejected &&
      |  undefinedRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.create applies property descriptors") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var obj = Object.create(null, {
      |  x: { value: 3, enumerable: true },
      |  y: { get: function() { return 4; }, enumerable: true }
      |});
      |Object.getPrototypeOf(obj) === null &&
      |  obj.x === 3 &&
      |  obj.y === 4 &&
      |  Object.keys(obj).join(',') === 'x,y';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.create installs the requested prototype") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var prototype = { inherited: 1 };
      |var object = Object.create(prototype, {
      |  own: { value: 2, enumerable: true }
      |});
      |Object.getPrototypeOf(object) === prototype &&
      |  prototype.isPrototypeOf(object) &&
      |  object.inherited === 1 && object.own === 2;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperties uses own enumerable descriptor keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var inherited = { hidden: { value: 1, enumerable: true } };
      |var descriptors = Object.create(inherited);
      |Object.defineProperty(descriptors, 'x', {
      |  value: { value: 2, enumerable: true },
      |  enumerable: true
      |});
      |Object.defineProperty(descriptors, 'skip', {
      |  value: { value: 3, enumerable: true },
      |  enumerable: false
      |});
      |var target = {};
      |Object.defineProperties(target, descriptors);
      |target.x === 2 &&
      |  !Object.hasOwn(target, 'hidden') &&
      |  !Object.hasOwn(target, 'skip');
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperties converts every descriptor before defining any property") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var target = {};
      |var bad = {};
      |Object.defineProperty(bad, 'value', {
      |  get: function() { throw new TypeError('second descriptor'); }
      |});
      |var descriptors = {};
      |Object.defineProperty(descriptors, 'first', {
      |  value: { value: 1, enumerable: true }, enumerable: true
      |});
      |Object.defineProperty(descriptors, 'second', {
      |  value: bad, enumerable: true
      |});
      |var threw = false;
      |try { Object.defineProperties(target, descriptors); }
      |catch (e) { threw = e instanceof TypeError; }
      |threw && !Object.hasOwn(target, 'first') && !Object.hasOwn(target, 'second');
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperties preserves symbol descriptor keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('descriptor');
      |var descriptors = {};
      |Object.defineProperty(descriptors, sym, {
      |  value: { value: 7, enumerable: true },
      |  enumerable: true
      |});
      |var target = {};
      |Object.defineProperties(target, descriptors);
      |Object.getOwnPropertyDescriptor(target, sym).value === 7;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperties uses Proxy descriptor map traps") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var seen = '';
      |var descriptors = new Proxy({}, {
      |  ownKeys: function() { seen += 'o'; return ['x']; },
      |  getOwnPropertyDescriptor: function(target, prop) {
      |    seen += prop;
      |    return { enumerable: true, configurable: true };
      |  },
      |  get: function(target, prop) {
      |    seen += 'g';
      |    return { value: 9, enumerable: true };
      |  }
      |});
      |var target = {};
      |Object.defineProperties(target, descriptors);
      |target.x === 9 && seen === 'oxg';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.fromEntries defines enumerable writable configurable properties") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var obj = Object.fromEntries([['x', 1]]);
      |var desc = Object.getOwnPropertyDescriptor(obj, 'x');
      |obj.x === 1 &&
      |  desc.enumerable === true &&
      |  desc.writable === true &&
      |  desc.configurable === true;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.fromEntries preserves symbol keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('entry');
      |var obj = Object.fromEntries([[sym, 5]]);
      |obj[sym] === 5 &&
      |  Object.getOwnPropertySymbols(obj)[0] === sym;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.fromEntries consumes custom iterables") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var iterable = {};
      |Object.defineProperty(iterable, Symbol.iterator, {
      |  value: function() {
      |    var index = 0;
      |    return {
      |      next: function() {
      |        index = index + 1;
      |        if (index === 1) return { done: false, value: ['a', 2] };
      |        if (index === 2) return { done: false, value: ['b', 3] };
      |        return { done: true };
      |      }
      |    };
      |  }
      |});
      |var obj = Object.fromEntries(iterable);
      |obj.a === 2 && obj.b === 3;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.fromEntries rejects non-object entries and closes iterator") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var closed = false;
      |var iterable = {};
      |Object.defineProperty(iterable, Symbol.iterator, {
      |  value: function() {
      |    return {
      |      next: function() { return { done: false, value: 1 }; },
      |      return: function() { closed = true; return {}; }
      |    };
      |  }
      |});
      |var rejected = false;
      |try { Object.fromEntries(iterable); } catch (e) { rejected = e instanceof TypeError; }
      |rejected && closed;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.fromEntries requires an iterable argument") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var missingRejected = false;
      |var plainRejected = false;
      |try { Object.fromEntries(); } catch (e) { missingRejected = e instanceof TypeError; }
      |try { Object.fromEntries({}); } catch (e) { plainRejected = e instanceof TypeError; }
      |missingRejected && plainRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Array iterator values keys entries and Symbol.iterator") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var arr = [10, 20];
      |var values = arr.values();
      |var keys = arr.keys();
      |var entries = arr.entries();
      |var symIter = arr[Symbol.iterator]();
      |var firstEntry = entries.next().value;
      |values.next().value === 10 &&
      |  values.next().value === 20 &&
      |  values.next().done === true &&
      |  keys.next().value === 0 &&
      |  keys.next().value === 1 &&
      |  firstEntry[0] === 0 &&
      |  firstEntry[1] === 10 &&
      |  symIter.next().value === 10 &&
      |  symIter[Symbol.iterator]() === symIter;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Array iterator methods are non-enumerable") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var keys = Object.keys(Array.prototype).join(',');
      |keys.indexOf('values') === -1 &&
      |  keys.indexOf('keys') === -1 &&
      |  keys.indexOf('entries') === -1;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Array.from consumes custom iterables before array-like length") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var iterable = {
      |  length: 99
      |};
      |Object.defineProperty(iterable, Symbol.iterator, {
      |  value: function() {
      |    var index = 0;
      |    return {
      |      next: function() {
      |        index = index + 1;
      |        if (index === 1) return { done: false, value: 'a' };
      |        if (index === 2) return { done: false, value: 'b' };
      |        return { done: true };
      |      }
      |    };
      |  }
      |});
      |var arr = Array.from(iterable);
      |arr.length === 2 && arr[0] === 'a' && arr[1] === 'b';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Array.from maps iterator values with thisArg and index") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var receiver = { offset: 10 };
      |var indexes = '';
      |var arr = Array.from([2, 3].values(), function(value, index) {
      |  indexes = indexes + index;
      |  return this.offset + value;
      |}, receiver);
      |arr.length === 2 &&
      |  arr[0] === 12 &&
      |  arr[1] === 13 &&
      |  indexes === '01';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Array.from closes iterator when mapper throws") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var closed = false;
      |var iterable = {};
      |Object.defineProperty(iterable, Symbol.iterator, {
      |  value: function() {
      |    return {
      |      next: function() { return { done: false, value: 1 }; },
      |      return: function() { closed = true; return {}; }
      |    };
      |  }
      |});
      |var rejected = false;
      |try {
      |  Array.from(iterable, function() { throw new TypeError('boom'); });
      |} catch (e) {
      |  rejected = e instanceof TypeError;
      |}
      |rejected && closed;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Array.from rejects non-callable iterator method") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var obj = {};
      |Object.defineProperty(obj, Symbol.iterator, { value: 1 });
      |var rejected = false;
      |try { Array.from(obj); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Array.from rejects null and undefined sources") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var rejectedNull = false;
      |var rejectedUndefined = false;
      |try { Array.from(null); } catch (e) { rejectedNull = e instanceof TypeError; }
      |try { Array.from(undefined); } catch (e) { rejectedUndefined = e instanceof TypeError; }
      |rejectedNull && rejectedUndefined;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Array.from array-like path creates final length and holes") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var arr = Array.from({ 1: 'b', length: 3 });
      |arr.length === 3 &&
      |  arr[0] === undefined &&
      |  arr[1] === 'b' &&
      |  arr[2] === undefined;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Array.from uses constructor this value for array-like sources") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |function C(len) {
      |  this.createdLength = len;
      |}
      |var out = Array.from.call(C, { 0: 'x', 1: 'y', length: 2 });
      |out instanceof C &&
      |  out.createdLength === 2 &&
      |  out.length === 2 &&
      |  out[0] === 'x' &&
      |  out[1] === 'y';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Array.from uses constructor this value for iterable sources") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |function C() {
      |  this.createdLength = arguments.length;
      |}
      |var out = Array.from.call(C, ['a', 'b'].values());
      |out instanceof C &&
      |  out.createdLength === 0 &&
      |  out.length === 2 &&
      |  out[0] === 'a' &&
      |  out[1] === 'b';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertyDescriptor reports accessor get and set fields") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var obj = {};
      |function getter() { return 42; }
      |Object.defineProperty(obj, 'x', { get: getter, enumerable: true });
      |var desc = Object.getOwnPropertyDescriptor(obj, 'x');
      |desc.get === getter &&
      |  desc.set === undefined &&
      |  desc.hasOwnProperty('get') &&
      |  desc.hasOwnProperty('set') &&
      |  desc.enumerable === true &&
      |  desc.configurable === false;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.getOwnPropertyDescriptor reports accessor get and set fields") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var obj = {};
      |function setter(v) {}
      |Object.defineProperty(obj, 'x', { set: setter, configurable: true });
      |var desc = Reflect.getOwnPropertyDescriptor(obj, 'x');
      |desc.get === undefined &&
      |  desc.set === setter &&
      |  desc.hasOwnProperty('get') &&
      |  desc.hasOwnProperty('set') &&
      |  desc.enumerable === false &&
      |  desc.configurable === true;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperty rejects non-callable accessors") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var getterRejected = false;
      |var setterRejected = false;
      |try { Object.defineProperty({}, 'x', { get: 1 }); } catch (e) { getterRejected = e instanceof TypeError; }
      |try { Object.defineProperty({}, 'x', { set: 1 }); } catch (e) { setterRejected = e instanceof TypeError; }
      |getterRejected && setterRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperty reads inherited descriptor accessors in specification order") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var order = "";
      |var proto = {};
      |Object.defineProperty(proto, 'enumerable', { get: function() { order += 'e'; return true; } });
      |Object.defineProperty(proto, 'configurable', { get: function() { order += 'c'; return true; } });
      |Object.defineProperty(proto, 'value', { get: function() { order += 'v'; return 7; } });
      |Object.defineProperty(proto, 'writable', { get: function() { order += 'w'; return true; } });
      |var descriptor = Object.create(proto);
      |var target = {};
      |Object.defineProperty(target, 'x', descriptor);
      |var actual = Object.getOwnPropertyDescriptor(target, 'x');
      |order === 'ecvw' && actual.value === 7 && actual.enumerable &&
      |  actual.configurable && actual.writable;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperty rejects non-configurable descriptor kind changes") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var dataToAccessor = false;
      |var accessorToData = false;
      |var obj1 = {};
      |var obj2 = {};
      |Object.defineProperty(obj1, 'x', { value: 1, configurable: false });
      |Object.defineProperty(obj2, 'x', { get: function() { return 1; }, configurable: false });
      |try { Object.defineProperty(obj1, 'x', { get: function() { return 2; } }); } catch (e) { dataToAccessor = e instanceof TypeError; }
      |try { Object.defineProperty(obj2, 'x', { value: 2 }); } catch (e) { accessorToData = e instanceof TypeError; }
      |dataToAccessor && accessorToData && obj1.x === 1 && obj2.x === 1;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.defineProperty returns false for non-configurable descriptor kind changes") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var obj1 = {};
      |var obj2 = {};
      |Object.defineProperty(obj1, 'x', { value: 1, configurable: false });
      |Object.defineProperty(obj2, 'x', { get: function() { return 1; }, configurable: false });
      |Reflect.defineProperty(obj1, 'x', { get: function() { return 2; } }) === false &&
      |  Reflect.defineProperty(obj2, 'x', { value: 2 }) === false &&
      |  obj1.x === 1 &&
      |  obj2.x === 1;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("array index descriptors allow compatible non-configurable redefinition") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var arr = [];
      |Object.defineProperty(arr, '0', { value: 1, writable: false, enumerable: true, configurable: false });
      |Object.defineProperty(arr, '0', { value: 1 });
      |var same = Reflect.defineProperty(arr, '0', { value: 1 });
      |var writableRejected = Reflect.defineProperty(arr, '0', { writable: true });
      |var valueRejected = Reflect.defineProperty(arr, '0', { value: 2 });
      |same === true &&
      |writableRejected === false &&
      |valueRejected === false &&
      |arr[0] === 1 &&
      |arr.length === 1;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("array length descriptor shrinks elements and enforces writability") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var arr = [1, 2, 3];
      |Object.defineProperty(arr, 'length', { value: 2, writable: false });
      |var descriptor = Object.getOwnPropertyDescriptor(arr, 'length');
      |var rejected = false;
      |try { Object.defineProperty(arr, 'length', { value: 3 }); }
      |catch (e) { rejected = e instanceof TypeError; }
      |arr.length === 2 && arr[2] === undefined && rejected &&
      |  descriptor.value === 2 && descriptor.writable === false &&
      |  descriptor.enumerable === false && descriptor.configurable === false;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("non-configurable array index accessors preserve their fields") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var array = [];
      |function getter() { return 12; }
      |Object.defineProperty(array, "1", { get: getter });
      |var thrownName = "none";
      |try {
      |  Object.defineProperty(array, "1", { get: undefined });
      |} catch (error) {
      |  thrownName = error.name + ":" + (error instanceof TypeError);
      |}
      |var descriptor = Object.getOwnPropertyDescriptor(array, "1");
      |thrownName + "," + array.hasOwnProperty("1") + "," +
      |  (array[1] === 12) + "," + descriptor.hasOwnProperty("set") + "," +
      |  (descriptor.set === undefined) + "," + (descriptor.configurable === false);
      |""".stripMargin)

    assertEquals(result.toString, "TypeError:true,true,true,true,true,true")
  }

  test("array index definitions shadow inherited index properties") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |Object.defineProperty(Array.prototype, "0", {
      |  value: 11,
      |  configurable: true
      |});
      |var array = [];
      |Object.defineProperty(array, "0", { configurable: false });
      |var answer = array.hasOwnProperty("0") + "," +
      |  (Array.prototype[0] === 11) + "," + (typeof array[0]);
      |delete Array.prototype[0];
      |answer;
      |""".stripMargin)

    assertEquals(result.toString, "true,true,undefined")
  }

  test("non-strict arguments indices alias parameter bindings") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |function assignment(a) {
      |  arguments[0] = 2;
      |  var fromArguments = a;
      |  a = 3;
      |  return fromArguments + ":" + arguments[0];
      |}
      |function redefine(a) {
      |  Object.defineProperty(arguments, "0", { value: 4 });
      |  var defined = a;
      |  Object.defineProperty(arguments, "0", { value: 5, writable: false });
      |  a = 6;
      |  return defined + ":" + arguments[0] + ":" + a;
      |}
      |function remove(a) {
      |  delete arguments[0];
      |  a = 8;
      |  return arguments[0] + ":" + a;
      |}
      |assignment(1) + "," + redefine(1) + "," + remove(7);
      |""".stripMargin)

    assertEquals(result.toString, "2:3,4:5:6,undefined:8")
  }

  test("strict and non-simple arguments objects are unmapped") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |function strictMode(a) {
      |  "use strict";
      |  arguments[0] = 2;
      |  return a;
      |}
      |function defaultParameter(a = 1) {
      |  arguments[0] = 3;
      |  return a;
      |}
      |function duplicate(a, a) {
      |  arguments[0] = 4;
      |  var first = a;
      |  arguments[1] = 5;
      |  return first + ":" + a;
      |}
      |strictMode(1) + "," + defaultParameter(1) + "," + duplicate(1, 2);
      |""".stripMargin)

    assertEquals(result.toString, "1,1,2:5")
  }

  test("arguments objects expose length iterator and callee descriptors") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |function ordinary() { return arguments; }
      |var args = ordinary();
      |var lengthDesc = Object.getOwnPropertyDescriptor(args, "length");
      |var calleeDesc = Object.getOwnPropertyDescriptor(args, "callee");
      |(args.callee === ordinary) + "," +
      |  args.hasOwnProperty(Symbol.iterator) + "," +
      |  (!lengthDesc.enumerable && lengthDesc.writable && lengthDesc.configurable) + "," +
      |  (!calleeDesc.enumerable && calleeDesc.writable && calleeDesc.configurable);
      |""".stripMargin)

    assertEquals(result.toString, "true,true,true,true")
  }

  test("Reflect.defineProperty defines array indices") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var arr = [];
      |var defined = Reflect.defineProperty(arr, '0', {
      |  value: 42,
      |  writable: true,
      |  enumerable: true,
      |  configurable: true
      |});
      |defined === true && arr[0] === 42 && arr.length === 1;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperty honors explicit undefined accessor fields") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var obj = {};
      |function setter(v) {}
      |Object.defineProperty(obj, 'x', { get: function() { return 1; }, set: setter, configurable: true });
      |Object.defineProperty(obj, 'x', { get: undefined });
      |Object.defineProperty(obj, 'x', { set: undefined });
      |var desc = Object.getOwnPropertyDescriptor(obj, 'x');
      |desc.get === undefined &&
      |  desc.set === undefined &&
      |  desc.hasOwnProperty('get') &&
      |  desc.hasOwnProperty('set') &&
      |  obj.x === undefined;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperty rejects non-object target and descriptor") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var targetRejected = false;
      |var descRejected = false;
      |try { Object.defineProperty(1, 'x', { value: 1 }); } catch (e) { targetRejected = e instanceof TypeError; }
      |try { Object.defineProperty({}, 'x', 1); } catch (e) { descRejected = e instanceof TypeError; }
      |targetRejected && descRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperty uses Proxy defineProperty trap") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var target = {};
      |var called = false;
      |var proxy = new Proxy(target, {
      |  defineProperty: function(obj, prop, desc) {
      |    called = prop === 'x' && desc.value === 42;
      |    return true;
      |  }
      |});
      |Object.defineProperty(proxy, 'x', { value: 42 }) === proxy &&
      |  called &&
      |  target.x === undefined &&
      |  proxy.x === undefined;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperty passes symbol key to Proxy defineProperty trap") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('k');
      |var target = {};
      |var called = false;
      |var proxy = new Proxy(target, {
      |  defineProperty: function(obj, prop, desc) {
      |    called = prop === sym && desc.value === 42;
      |    return true;
      |  }
      |});
      |Object.defineProperty(proxy, sym, { value: 42 }) === proxy &&
      |  called &&
      |  target[sym] === undefined;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("in operator preserves symbol property keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var symbol = Symbol("key");
      |var object = {};
      |Object.defineProperty(object, symbol, {
      |  value: 1,
      |  configurable: true
      |});
      |var before = symbol in object;
      |delete object[symbol];
      |before + "," + (symbol in object);
      |""".stripMargin)

    assertEquals(result.toString, "true,false")
  }

  test("Object.getOwnPropertyDescriptor enforces Proxy descriptor invariants") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var target = {};
      |Object.defineProperty(target, 'x', { value: 1, configurable: false });
      |var proxy = new Proxy(target, {
      |  getOwnPropertyDescriptor: function() { return undefined; }
      |});
      |var rejected = false;
      |try { Object.getOwnPropertyDescriptor(proxy, 'x'); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertyDescriptor passes symbol key to Proxy trap") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('k');
      |var target = {};
      |var called = false;
      |var proxy = new Proxy(target, {
      |  getOwnPropertyDescriptor: function(obj, prop) {
      |    called = prop === sym;
      |    return { value: 42, configurable: true };
      |  }
      |});
      |var desc = Object.getOwnPropertyDescriptor(proxy, sym);
      |called && desc.value === 42;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertyDescriptor enforces Proxy symbol descriptor invariants") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('k');
      |var target = {};
      |Object.defineProperty(target, sym, { value: 1, configurable: false });
      |var proxy = new Proxy(target, {
      |  getOwnPropertyDescriptor: function() { return undefined; }
      |});
      |var rejected = false;
      |try { Object.getOwnPropertyDescriptor(proxy, sym); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertyNames and Object.keys use Proxy ownKeys trap") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var target = {};
      |var proxy = new Proxy(target, {
      |  ownKeys: function() { return ['a', 'b']; },
      |  getOwnPropertyDescriptor: function(obj, prop) {
      |    return { value: prop, enumerable: prop === 'a', configurable: true };
      |  }
      |});
      |var names = Object.getOwnPropertyNames(proxy);
      |var keys = Object.keys(proxy);
      |names.length === 2 &&
      |  names[0] === 'a' &&
      |  names[1] === 'b' &&
      |  keys.length === 1 &&
      |  keys[0] === 'a';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertyNames rejects duplicate Proxy ownKeys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var proxy = new Proxy({}, {
      |  ownKeys: function() { return ['a', 'a']; }
      |});
      |var rejected = false;
      |try { Object.getOwnPropertyNames(proxy); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertySymbols returns own symbol keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('k');
      |var obj = {};
      |obj.a = 1;
      |Object.defineProperty(obj, sym, { value: 2, enumerable: true });
      |var symbols = Object.getOwnPropertySymbols(obj);
      |symbols.length === 1 && symbols[0] === sym;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertySymbols includes non-enumerable symbol keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('hidden');
      |var obj = {};
      |Object.defineProperty(obj, sym, { value: 2, enumerable: false });
      |var symbols = Object.getOwnPropertySymbols(obj);
      |symbols.length === 1 && symbols[0] === sym;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertySymbols uses Proxy ownKeys trap") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('k');
      |var proxy = new Proxy({}, {
      |  ownKeys: function() { return ['a', sym]; }
      |});
      |var names = Object.getOwnPropertyNames(proxy);
      |var symbols = Object.getOwnPropertySymbols(proxy);
      |names.length === 1 &&
      |  names[0] === 'a' &&
      |  symbols.length === 1 &&
      |  symbols[0] === sym;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertyDescriptors includes symbol keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('k');
      |var obj = { a: 1 };
      |Object.defineProperty(obj, sym, { value: 2, enumerable: true });
      |var descs = Object.getOwnPropertyDescriptors(obj);
      |var symbols = Object.getOwnPropertySymbols(descs);
      |descs.a.value === 1 &&
      |  symbols.length === 1 &&
      |  symbols[0] === sym &&
      |  descs[sym].value === 2;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertyDescriptors includes non-enumerable symbol keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('hidden');
      |var obj = {};
      |Object.defineProperty(obj, sym, { value: 2, enumerable: false });
      |var descs = Object.getOwnPropertyDescriptors(obj);
      |var symbols = Object.getOwnPropertySymbols(descs);
      |symbols.length === 1 &&
      |  symbols[0] === sym &&
      |  descs[sym].value === 2 &&
      |  descs[sym].enumerable === false;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertyDescriptors uses Proxy traps and symbol keys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var sym = Symbol('k');
      |var seen = '';
      |var proxy = new Proxy({}, {
      |  ownKeys: function() { return ['a', sym]; },
      |  getOwnPropertyDescriptor: function(target, prop) {
      |    if (prop === 'a') {
      |      seen = seen + 'a';
      |      return { value: 1, enumerable: true, configurable: true };
      |    }
      |    if (prop === sym) {
      |      seen = seen + 's';
      |      return { value: 2, enumerable: true, configurable: true };
      |    }
      |  }
      |});
      |var descs = Object.getOwnPropertyDescriptors(proxy);
      |var symbols = Object.getOwnPropertySymbols(descs);
      |seen === 'as' &&
      |  descs.a.value === 1 &&
      |  symbols.length === 1 &&
      |  symbols[0] === sym &&
      |  descs[sym].value === 2;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertyDescriptors skips undefined Proxy descriptors") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var proxy = new Proxy({}, {
      |  ownKeys: function() { return ['a']; },
      |  getOwnPropertyDescriptor: function() { return undefined; }
      |});
      |var descs = Object.getOwnPropertyDescriptors(proxy);
      |Object.keys(descs).length === 0;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.getOwnPropertyDescriptors rejects duplicate Proxy ownKeys") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var proxy = new Proxy({}, {
      |  ownKeys: function() { return ['a', 'a']; }
      |});
      |var rejected = false;
      |try { Object.getOwnPropertyDescriptors(proxy); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Array length and indices use the full uint32 range") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var a = [];
      |Object.defineProperty(a, 'length', { value: 4294967294 });
      |Object.defineProperty(a, '4294967294', {
      |  value: 100, writable: true, enumerable: true, configurable: true
      |});
      |var highIndexWorks = a.length === 4294967295 &&
      |  a[4294967294] === 100 && Object.hasOwn(a, '4294967294');
      |
      |var b = [];
      |Object.defineProperty(b, '4294967295', { value: 101 });
      |Object.defineProperty(b, '4294967296', { value: 102 });
      |var ordinaryKeysWork = b.length === 0 &&
      |  b[4294967295] === 101 && b[4294967296] === 102 &&
      |  Object.hasOwn(b, '4294967295') && Object.hasOwn(b, '4294967296');
      |
      |var invalidLengths = 0;
      |try { b.length = -1; } catch (e) { if (e instanceof RangeError) invalidLengths++; }
      |try { b.length = 4294967296; } catch (e) { if (e instanceof RangeError) invalidLengths++; }
      |try { b.length = 1.5; } catch (e) { if (e instanceof RangeError) invalidLengths++; }
      |highIndexWorks && ordinaryKeysWork && invalidLengths === 3;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("array own index shadows an inherited index descriptor") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |try {
      |  Object.defineProperty(Array.prototype, '0', {
      |    value: 11, configurable: true
      |  });
      |  var array = [];
      |  Object.defineProperty(array, '0', { configurable: false });
      |  array.hasOwnProperty('0') &&
      |    Array.prototype[0] === 11 && typeof array[0] === 'undefined';
      |} finally {
      |  delete Array.prototype[0];
      |}
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("array named data and accessor descriptors retain ordinary semantics") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var array = [];
      |Object.defineProperty(array, 'data', {
      |  value: 1, writable: true, configurable: false
      |});
      |Object.defineProperty(array, 'data', { value: 2 });
      |var stored = 'initial';
      |Object.defineProperty(array, 'accessor', {
      |  get: function() { return stored; },
      |  set: function(value) { stored = value; },
      |  configurable: true
      |});
      |array.accessor = 'updated';
      |var inherited = [];
      |Object.defineProperty(Array.prototype, 'inheritedAccessor', {
      |  get: function() { return stored; },
      |  set: function(value) { stored = value; },
      |  configurable: true
      |});
      |inherited.inheritedAccessor = 'inherited';
      |var ok = array.data === 2 && array.accessor === 'inherited' &&
      |  inherited.inheritedAccessor === 'inherited' &&
      |  !Object.hasOwn(inherited, 'inheritedAccessor');
      |delete Array.prototype.inheritedAccessor;
      |ok;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.defineProperties invokes descriptor getters with the source receiver") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var functionSource = function() {};
      |var arraySource = [];
      |var argumentsSource = (function() { return arguments; })();
      |var seenFunction = false, seenArray = false, seenArguments = false;
      |Object.defineProperty(functionSource, 'a', {
      |  enumerable: true,
      |  get: function() { seenFunction = this instanceof Function; return {}; }
      |});
      |Object.defineProperty(arraySource, 'b', {
      |  enumerable: true,
      |  get: function() { seenArray = this instanceof Array; return {}; }
      |});
      |Object.defineProperty(argumentsSource, 'c', {
      |  enumerable: true,
      |  get: function() {
      |    seenArguments = Object.prototype.toString.call(this) === '[object Arguments]';
      |    return {};
      |  }
      |});
      |Object.defineProperties({}, functionSource);
      |Object.defineProperties({}, arraySource);
      |Object.defineProperties({}, argumentsSource);
      |(seenFunction ? 1 : 0) + (seenArray ? 2 : 0) + (seenArguments ? 4 : 0) +
      |  (functionSource instanceof Function ? 8 : 0) +
      |  (arraySource instanceof Array ? 16 : 0);
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(31))
  }

  test("global var creates an own global property over an inherited property") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |Object.defineProperty(Object.prototype, 'globalShadow', {
      |  value: 1, writable: false, configurable: true
      |});
      |var globalShadow = 2;
      |var ok = this.hasOwnProperty('globalShadow') &&
      |  this.globalShadow === 2 && globalShadow === 2;
      |delete Object.prototype.globalShadow;
      |ok;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("abstract equality preserves accessor function identity") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var getter = function() {};
      |var object = {};
      |Object.defineProperty(object, 'value', {
      |  get: getter, enumerable: true, configurable: true
      |});
      |var first = Object.getOwnPropertyDescriptor(object, 'value');
      |Object.defineProperty(object, 'value', {
      |  get: getter, enumerable: true, configurable: true
      |});
      |var second = Object.getOwnPropertyDescriptor(object, 'value');
      |first.get == second.get && first.get === getter;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("ordinary own-key consumers use integer string insertion symbol order") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var object = {};
      |var symbol = Symbol('key');
      |object.x = 1;
      |object["4294967295"] = 2;
      |object[10] = 3;
      |object["01"] = 4;
      |object[2] = 5;
      |object[symbol] = 6;
      |Object.defineProperty(object, "1", {
      |  value: 7, enumerable: false, configurable: true
      |});
      |var keys = Object.keys(object);
      |var names = Object.getOwnPropertyNames(object);
      |var own = Reflect.ownKeys(object);
      |var assigned = Object.assign({}, object);
      |keys.join(",") === "2,10,x,4294967295,01" &&
      |names.join(",") === "1,2,10,x,4294967295,01" &&
      |own.length === 7 && own.slice(0, 6).join(",") ===
      |  "1,2,10,x,4294967295,01" && own[6] === symbol &&
      |Object.keys(assigned).join(",") === "2,10,x,4294967295,01" &&
      |Object.getOwnPropertySymbols(assigned)[0] === symbol;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("array own-key consumers skip holes and preserve named properties") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var array = [];
      |array[3] = "three";
      |array[1] = "one";
      |array.extra = true;
      |Object.keys(array).join(",") === "1,3,extra" &&
      |Object.getOwnPropertyNames(array).join(",") === "1,3,length,extra" &&
      |Reflect.ownKeys(array).join(",") === "1,3,length,extra";
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Proxy ownKeys fallback uses ordinary integer string symbol order") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var target = {};
      |var symbol = Symbol('key');
      |target[symbol] = 1;
      |target.foo = 2;
      |target[10] = 3;
      |target[2] = 4;
      |var seen = [];
      |var proxy = new Proxy(target, {
      |  getOwnPropertyDescriptor: function(target, key) { seen.push(key); }
      |});
      |Object.defineProperties({}, proxy);
      |seen.length === 4 && seen[0] === '2' && seen[1] === '10' &&
      |  seen[2] === 'foo' && seen[3] === symbol;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("URI functions encode Unicode and preserve URI reserved characters") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |encodeURI(';/😀') === ';/%F0%9F%98%80' &&
      |encodeURIComponent(';/😀') === '%3B%2F%F0%9F%98%80' &&
      |decodeURI('%3B%2F%F0%9F%98%80') === '%3B%2F😀' &&
      |decodeURIComponent('%3B%2F%F0%9F%98%80') === ';/😀';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Date UTC fields and setters normalize overflowing calendar fields") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var date = new Date(Date.UTC(2020, 0, 31, 5, 6, 7, 8));
      |var getters = date.getUTCFullYear() === 2020 &&
      |  date.getUTCMonth() === 0 && date.getUTCDate() === 31 &&
      |  date.getUTCHours() === 5 && date.getUTCMinutes() === 6 &&
      |  date.getUTCSeconds() === 7 && date.getUTCMilliseconds() === 8;
      |date.setUTCMonth(1);
      |getters && date.getUTCMonth() === 2 && date.getUTCDate() === 2;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("RegExp fields are prototype accessors and descriptor enumeration respects proxies and symbols") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var regexp = /x/gim;
      |var accessor = Object.getOwnPropertyDescriptor(RegExp.prototype, 'source');
      |var symbol = Symbol('key');
      |var object = { key: 1 };
      |object[symbol] = 2;
      |var descriptors = Object.getOwnPropertyDescriptors(object);
      |var log = '';
      |var handler = {
      |  ownKeys: function(target) { log += 'k'; return ['key']; },
      |  getOwnPropertyDescriptor: function(target, key) {
      |    log += 'd'; return Object.getOwnPropertyDescriptor(target, key);
      |  }
      |};
      |var forwarding = new Proxy(handler, {
      |  get: function(target, key) { return target[key]; }
      |});
      |Object.getOwnPropertyDescriptors(new Proxy(object, forwarding));
      |accessor.get instanceof Function && !accessor.enumerable &&
      |  regexp.source === 'x' && regexp.global && regexp.ignoreCase && regexp.multiline &&
      |  Object.keys(descriptors).length === 1 &&
      |  Object.getOwnPropertySymbols(descriptors).length === 1 && log === 'kd';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("String codePointAt handles surrogate pairs and bounds") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var text = "\u{10ffff}";
      |text.codePointAt(0) === 0x10ffff &&
      |text.codePointAt(1) === 0xdfff &&
      |text.codePointAt(-1) === undefined &&
      |text.codePointAt(Infinity) === undefined &&
      |"A".codePointAt(NaN) === 65;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("RegExp exec preserves captures from the last quantified match") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var match = /(z)((a+)?(b+)?(c))*/.exec("zaacbbbcac");
      |match.length + "|" + match[0] + "|" + match[1] + "|" + match[2] +
      |  "|" + match[3] + "|" + match[4] + "|" + match[5];
      |""".stripMargin)
    assertEquals(
      result,
      JSValue.fromString("6|zaacbbbcac|z|ac|a|undefined|c")
    )
  }

  test("RegExp exec clears captures in quantified lookaheads") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var a = /(?:(?=(abc)))a/.exec("abc");
      |var b = /(?:(?=(abc)))?a/.exec("abc");
      |var c = /(?:(?=(abc))){0,2}a/.exec("abc");
      |a[0] + "|" + a[1] + "|" + b[0] + "|" + b[1] + "|" +
      |  c[0] + "|" + c[1];
      |""".stripMargin)
    assertEquals(result, JSValue.fromString("a|abc|a|undefined|a|undefined"))
  }

  test("RegExp exec handles empty alternatives before captures") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |var a = /(?:|[\w])+([0-9])/.exec("123a23");
      |a.length + "|" + a[0] + "|" + a[1] + "|" +
      |  (/()*?a/.exec(",") === null);
      |""".stripMargin)
    assertEquals(result, JSValue.fromString("2|123a23|3|true"))
  }

  test("Math trigonometric functions") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    assertEquals(eval("Math.sin(0)").toNumber, 0.0, 0.001)
    assertEquals(eval("Math.cos(0)").toNumber, 1.0, 0.001)
    assertEquals(eval("Math.tan(0)").toNumber, 0.0, 0.001)
    assertEquals(eval("Math.sin(Math.PI / 2)").toNumber, 1.0, 0.001)
  }

  test("Math constants") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    assertEquals(eval("Math.PI").toNumber, math.Pi, 0.001)
    assertEquals(eval("Math.E").toNumber, math.E, 0.001)
    assertEquals(eval("Math.SQRT2").toNumber, math.sqrt(2), 0.001)
    assertEquals(eval("Math.SQRT1_2").toNumber, 1.0 / math.sqrt(2), 0.001)
    assertEquals(eval("Math.LN2").toNumber, math.log(2), 0.001)
    assertEquals(eval("Math.LN10").toNumber, math.log(10), 0.001)
  }

  test("Math.sqrt") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    assertEquals(eval("Math.sqrt(16)").toNumber, 4.0)
    assertEquals(eval("Math.sqrt(2)").toNumber, math.sqrt(2), 0.001)
    assertEquals(eval("Math.sqrt(0)").toNumber, 0.0)
  }

  test("Math.random returns number between 0 and 1") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val rand = eval("Math.random()").toNumber
    assert(rand >= 0.0 && rand < 1.0, s"Math.random() returned $rand")
  }
