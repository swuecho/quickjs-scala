package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class ReflectTest extends FunSuite:

  private def eval(source: String): JSValue =
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  // ============================================================
  // Reflect.get Tests
  // ============================================================

  test("Reflect.get - basic property access") {
    val result = eval("""
      |var obj = { x: 42 };
      |Reflect.get(obj, 'x');
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(42))
  }

  test("Reflect.get - nested property") {
    val result = eval("""
      |var obj = { a: { b: 'nested' } };
      |Reflect.get(Reflect.get(obj, 'a'), 'b');
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("nested"))
  }

  test("Reflect.get - non-existent property returns undefined") {
    val result = eval("""
      |var obj = { x: 1 };
      |Reflect.get(obj, 'y');
      |""".stripMargin)
    assertEquals(result, JSValue.Undefined)
  }

  test("Reflect.get - from prototype chain") {
    val result = eval("""
      |var proto = { inherited: 'from proto' };
      |var obj = Object.create(proto);
      |Reflect.get(obj, 'inherited');
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("from proto"))
  }

  test("Reflect.get - symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var obj = {};
      |Object.defineProperty(obj, sym, { value: 42 });
      |Reflect.get(obj, sym);
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(42))
  }

  test("Reflect.get - Proxy receives symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var called = false;
      |var proxy = new Proxy({}, {
      |  get: function(target, prop, receiver) {
      |    called = prop === sym;
      |    return 42;
      |  }
      |});
      |Reflect.get(proxy, sym) === 42 && called;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.get - Proxy cannot report different value for frozen data property") {
    val result = eval("""
      |var target = {};
      |Object.defineProperty(target, 'x', { value: 1, writable: false, configurable: false });
      |var proxy = new Proxy(target, {
      |  get: function() { return 2; }
      |});
      |var rejected = false;
      |try { Reflect.get(proxy, 'x'); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Reflect.set Tests
  // ============================================================

  test("Reflect.set - basic property assignment") {
    val result = eval("""
      |var obj = {};
      |Reflect.set(obj, 'x', 100);
      |obj.x;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(100))
  }

  test("Reflect.set - returns true on success") {
    val result = eval("""
      |var obj = {};
      |Reflect.set(obj, 'prop', 'value');
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.set - overwrites existing property") {
    val result = eval("""
      |var obj = { x: 'old' };
      |Reflect.set(obj, 'x', 'new');
      |obj.x;
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("new"))
  }

  test("Reflect.set - symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var obj = {};
      |Reflect.set(obj, sym, 42) && Reflect.get(obj, sym) === 42;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.set - Proxy receives symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var called = false;
      |var proxy = new Proxy({}, {
      |  set: function(target, prop, value, receiver) {
      |    called = prop === sym && value === 42;
      |    return true;
      |  }
      |});
      |Reflect.set(proxy, sym, 42) && called;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.set - Proxy cannot change frozen data property") {
    val result = eval("""
      |var target = {};
      |Object.defineProperty(target, 'x', { value: 1, writable: false, configurable: false });
      |var proxy = new Proxy(target, {
      |  set: function() { return true; }
      |});
      |var rejected = false;
      |try { Reflect.set(proxy, 'x', 2); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Reflect.has Tests
  // ============================================================

  test("Reflect.has - own property") {
    val result = eval("""
      |var obj = { x: 1 };
      |Reflect.has(obj, 'x');
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.has - missing property") {
    val result = eval("""
      |var obj = { x: 1 };
      |Reflect.has(obj, 'y');
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  test("Reflect.has - inherited property") {
    val result = eval("""
      |var proto = { inherited: true };
      |var obj = Object.create(proto);
      |Reflect.has(obj, 'inherited');
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.has - equivalent to in operator") {
    val result = eval("""
      |var obj = { a: 1 };
      |Reflect.has(obj, 'a') === ('a' in obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.has - symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var obj = {};
      |Object.defineProperty(obj, sym, { value: 42 });
      |Reflect.has(obj, sym);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.has - Proxy receives symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var called = false;
      |var proxy = new Proxy({}, {
      |  has: function(target, prop) {
      |    called = prop === sym;
      |    return true;
      |  }
      |});
      |Reflect.has(proxy, sym) && called;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.has - Proxy cannot hide non-configurable property") {
    val result = eval("""
      |var target = {};
      |Object.defineProperty(target, 'x', { value: 1, configurable: false });
      |var proxy = new Proxy(target, {
      |  has: function() { return false; }
      |});
      |var rejected = false;
      |try { Reflect.has(proxy, 'x'); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Reflect.deleteProperty Tests
  // ============================================================

  test("Reflect.deleteProperty - delete existing property") {
    val result = eval("""
      |var obj = { x: 1, y: 2 };
      |Reflect.deleteProperty(obj, 'x');
      |'x' in obj;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  test("Reflect.deleteProperty - returns true on success") {
    val result = eval("""
      |var obj = { x: 1 };
      |Reflect.deleteProperty(obj, 'x');
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.deleteProperty - non-existent property returns true") {
    val result = eval("""
      |var obj = { x: 1 };
      |Reflect.deleteProperty(obj, 'y');
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.deleteProperty - symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var obj = {};
      |Object.defineProperty(obj, sym, { value: 42, configurable: true });
      |Reflect.deleteProperty(obj, sym) && !Reflect.has(obj, sym);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.deleteProperty - Proxy receives symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var called = false;
      |var proxy = new Proxy({}, {
      |  deleteProperty: function(target, prop) {
      |    called = prop === sym;
      |    return true;
      |  }
      |});
      |Reflect.deleteProperty(proxy, sym) && called;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.deleteProperty - Proxy cannot delete non-configurable property") {
    val result = eval("""
      |var target = {};
      |Object.defineProperty(target, 'x', { value: 1, configurable: false });
      |var proxy = new Proxy(target, {
      |  deleteProperty: function() { return true; }
      |});
      |var rejected = false;
      |try { Reflect.deleteProperty(proxy, 'x'); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Reflect.ownKeys Tests
  // ============================================================

  test("Reflect.ownKeys - returns own property names") {
    val result = eval("""
      |var obj = { a: 1, b: 2, c: 3 };
      |var keys = Reflect.ownKeys(obj);
      |keys.length;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(3))
  }

  test("Reflect.ownKeys - empty object") {
    val result = eval("""
      |var obj = {};
      |var keys = Reflect.ownKeys(obj);
      |keys.length;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(0))
  }

  test("Reflect.ownKeys - does not include inherited properties") {
    val result = eval("""
      |var proto = { inherited: true };
      |var obj = Object.create(proto);
      |obj.own = 'value';
      |var keys = Reflect.ownKeys(obj);
      |keys.length === 1 && keys[0] === 'own';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.ownKeys - includes symbol keys") {
    val result = eval("""
      |var sym = Symbol('k');
      |var obj = {};
      |obj.a = 1;
      |Object.defineProperty(obj, sym, { value: 2, enumerable: true });
      |var keys = Reflect.ownKeys(obj);
      |keys.length === 2 && keys[0] === 'a' && keys[1] === sym;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.ownKeys - includes non-enumerable symbol keys") {
    val result = eval("""
      |var sym = Symbol('hidden');
      |var obj = {};
      |Object.defineProperty(obj, sym, { value: 2, enumerable: false });
      |var keys = Reflect.ownKeys(obj);
      |keys.length === 1 && keys[0] === sym;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Reflect.getPrototypeOf Tests
  // ============================================================

  test("Reflect.getPrototypeOf - returns prototype") {
    val result = eval("""
      |var proto = { x: 1 };
      |var obj = Object.create(proto);
      |Reflect.getPrototypeOf(obj) === proto;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.getPrototypeOf - null prototype") {
    val result = eval("""
      |var obj = Object.create(null);
      |Reflect.getPrototypeOf(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Null)
  }

  test("Reflect.getPrototypeOf - uses Proxy getPrototypeOf trap") {
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
      |Reflect.getPrototypeOf(proxy) === proto && called;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.getPrototypeOf - rejects non-object Proxy prototype") {
    val result = eval("""
      |var proxy = new Proxy({}, {
      |  getPrototypeOf: function() { return 1; }
      |});
      |var rejected = false;
      |try { Reflect.getPrototypeOf(proxy); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.getPrototypeOf - rejects inconsistent Proxy prototype") {
    val result = eval("""
      |var proto = {};
      |var target = Object.create(null);
      |Object.preventExtensions(target);
      |var proxy = new Proxy(target, {
      |  getPrototypeOf: function() { return proto; }
      |});
      |var rejected = false;
      |try { Reflect.getPrototypeOf(proxy); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Reflect.setPrototypeOf Tests
  // ============================================================

  test("Reflect.setPrototypeOf - change prototype") {
    val result = eval("""
      |var proto = { x: 'from proto' };
      |var obj = {};
      |Reflect.setPrototypeOf(obj, proto);
      |obj.x;
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("from proto"))
  }

  test("Reflect.setPrototypeOf - returns true on success") {
    val result = eval("""
      |var obj = {};
      |var proto = {};
      |Reflect.setPrototypeOf(obj, proto);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.setPrototypeOf - set to null") {
    val result = eval("""
      |var obj = { x: 1 };
      |Reflect.setPrototypeOf(obj, null);
      |Reflect.getPrototypeOf(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Null)
  }

  test("Reflect.setPrototypeOf - uses Proxy setPrototypeOf trap") {
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
      |Reflect.setPrototypeOf(proxy, proto) === true &&
      |  called &&
      |  Reflect.getPrototypeOf(target) === proto;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.setPrototypeOf - returns false for false Proxy trap") {
    val result = eval("""
      |var proto = {};
      |var proxy = new Proxy({}, {
      |  setPrototypeOf: function() { return false; }
      |});
      |Reflect.setPrototypeOf(proxy, proto) === false;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.setPrototypeOf - rejects inconsistent Proxy prototype") {
    val result = eval("""
      |var proto = {};
      |var target = Object.create(null);
      |Object.preventExtensions(target);
      |var proxy = new Proxy(target, {
      |  setPrototypeOf: function() { return true; }
      |});
      |var rejected = false;
      |try { Reflect.setPrototypeOf(proxy, proto); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Reflect.defineProperty Tests
  // ============================================================

  test("Reflect.defineProperty - basic") {
    val result = eval("""
      |var obj = {};
      |Reflect.defineProperty(obj, 'x', { value: 42 });
      |obj.x;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(42))
  }

  test("Reflect.defineProperty - returns true on success") {
    val result = eval("""
      |var obj = {};
      |Reflect.defineProperty(obj, 'x', { value: 1 });
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.defineProperty - with enumerable") {
    val result = eval("""
      |var obj = {};
      |Reflect.defineProperty(obj, 'x', { value: 1, enumerable: true });
      |Object.keys(obj).length;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(1))
  }

  test("Reflect.defineProperty - non-enumerable") {
    val result = eval("""
      |var obj = {};
      |Reflect.defineProperty(obj, 'x', { value: 1, enumerable: false });
      |Object.keys(obj).length;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(0))
  }

  test("Reflect.defineProperty - symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var obj = {};
      |Reflect.defineProperty(obj, sym, { value: 42, enumerable: true });
      |obj[sym] === 42 &&
      |  obj['Symbol(' + sym.description + ')'] === undefined &&
      |  Object.getOwnPropertySymbols(obj)[0] === sym;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Reflect.getOwnPropertyDescriptor Tests
  // ============================================================

  test("Reflect.getOwnPropertyDescriptor - basic") {
    val result = eval("""
      |var obj = { x: 42 };
      |var desc = Reflect.getOwnPropertyDescriptor(obj, 'x');
      |desc.value;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(42))
  }

  test("Reflect.getOwnPropertyDescriptor - returns undefined for missing") {
    val result = eval("""
      |var obj = { x: 1 };
      |Reflect.getOwnPropertyDescriptor(obj, 'y');
      |""".stripMargin)
    assertEquals(result, JSValue.Undefined)
  }

  test("Reflect.getOwnPropertyDescriptor - includes enumerable") {
    val result = eval("""
      |var obj = {};
      |Object.defineProperty(obj, 'x', { value: 1, enumerable: true });
      |var desc = Reflect.getOwnPropertyDescriptor(obj, 'x');
      |desc.enumerable;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.getOwnPropertyDescriptor - symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var obj = {};
      |Object.defineProperty(obj, sym, { value: 99, configurable: true });
      |var desc = Reflect.getOwnPropertyDescriptor(obj, sym);
      |desc.value === 99 && desc.configurable === true;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Reflect.isExtensible Tests
  // ============================================================

  test("Reflect.isExtensible - extensible object") {
    val result = eval("""
      |var obj = {};
      |Reflect.isExtensible(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.isExtensible - uses Proxy isExtensible trap") {
    val result = eval("""
      |var target = {};
      |Object.preventExtensions(target);
      |var called = false;
      |var proxy = new Proxy(target, {
      |  isExtensible: function(obj) {
      |    called = obj === target;
      |    return false;
      |  }
      |});
      |Reflect.isExtensible(proxy) === false && called;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.isExtensible - rejects inconsistent Proxy result") {
    val result = eval("""
      |var proxy = new Proxy({}, {
      |  isExtensible: function() { return false; }
      |});
      |var rejected = false;
      |try { Reflect.isExtensible(proxy); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.preventExtensions - prevents adding properties") {
    val result = eval("""
      |var obj = {};
      |var prevented = Reflect.preventExtensions(obj);
      |obj.x = 1;
      |prevented === true && Reflect.isExtensible(obj) === false && obj.x === undefined;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.preventExtensions - uses Proxy preventExtensions trap") {
    val result = eval("""
      |var target = {};
      |var called = false;
      |var proxy = new Proxy(target, {
      |  preventExtensions: function(obj) {
      |    called = obj === target;
      |    Object.preventExtensions(obj);
      |    return true;
      |  }
      |});
      |Reflect.preventExtensions(proxy) === true &&
      |  called &&
      |  Reflect.isExtensible(proxy) === false;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.preventExtensions - returns false for false Proxy trap") {
    val result = eval("""
      |var proxy = new Proxy({}, {
      |  preventExtensions: function() { return false; }
      |});
      |Reflect.preventExtensions(proxy) === false;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.preventExtensions - rejects inconsistent Proxy result") {
    val result = eval("""
      |var proxy = new Proxy({}, {
      |  preventExtensions: function() { return true; }
      |});
      |var rejected = false;
      |try { Reflect.preventExtensions(proxy); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Reflect.apply Tests
  // ============================================================

  test("Reflect.apply - call function with this") {
    val result = eval("""
      |var obj = { value: 42 };
      |function getValue() { return this.value; }
      |Reflect.apply(getValue, obj, []);
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(42))
  }

  test("Reflect.apply - pass arguments") {
    val result = eval("""
      |function add(a, b) { return a + b; }
      |Reflect.apply(add, null, [2, 3]);
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(5))
  }

  test("Reflect.apply - multiple arguments") {
    val result = eval("""
      |function sum(a, b, c, d) { return a + b + c + d; }
      |Reflect.apply(sum, null, [1, 2, 3, 4]);
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(10))
  }

  test("Reflect.apply - with method") {
    val result = eval("""
      |var obj = {
      |  multiplier: 2,
      |  multiply: function(x) { return x * this.multiplier; }
      |};
      |Reflect.apply(obj.multiply, obj, [5]);
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(10))
  }

  // ============================================================
  // Reflect.construct Tests
  // ============================================================

  test("Reflect.construct - basic constructor") {
    val result = eval("""
      |function Person(name) {
      |  this.name = name;
      |}
      |var p = Reflect.construct(Person, ['John']);
      |p.name;
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("John"))
  }

  test("Reflect.construct - multiple arguments") {
    val result = eval("""
      |function Point(x, y) {
      |  this.x = x;
      |  this.y = y;
      |}
      |var p = Reflect.construct(Point, [3, 4]);
      |p.x + p.y;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(7))
  }

  test("Reflect.construct - returns instance") {
    val result = eval("""
      |function Foo() {
      |  this.value = 'instance';
      |}
      |var obj = Reflect.construct(Foo, []);
      |obj.value;
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("instance"))
  }

  test("Reflect.construct - prototype chain") {
    val result = eval("""
      |function Animal() {}
      |Animal.prototype.speak = function() { return 'sound'; };
      |var a = Reflect.construct(Animal, []);
      |a.speak();
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("sound"))
  }

  // ============================================================
  // Integration Tests
  // ============================================================

  test("Reflect - use with Proxy handler") {
    val result = eval("""
      |var target = { x: 10, y: 20 };
      |var handler = {
      |  get: function(obj, prop) {
      |    return Reflect.get(obj, prop) * 2;
      |  }
      |};
      |var proxy = new Proxy(target, handler);
      |proxy.x;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(20))
  }

  test("Proxy constructor rejects non-object target or handler") {
    val result = eval("""
      |var badTarget = false;
      |var badHandler = false;
      |try { new Proxy(1, {}); } catch (e) { badTarget = e instanceof TypeError; }
      |try { new Proxy({}, null); } catch (e) { badHandler = e instanceof TypeError; }
      |badTarget && badHandler;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Proxy.revocable throws after revoke for property operations") {
    val result = eval("""
      |var record = Proxy.revocable({ x: 1 }, {});
      |record.revoke();
      |var getRejected = false;
      |var setRejected = false;
      |var hasRejected = false;
      |var ownKeysRejected = false;
      |try { record.proxy.x; } catch (e) { getRejected = e instanceof TypeError; }
      |try { record.proxy.x = 2; } catch (e) { setRejected = e instanceof TypeError; }
      |try { Reflect.has(record.proxy, 'x'); } catch (e) { hasRejected = e instanceof TypeError; }
      |try { Reflect.ownKeys(record.proxy); } catch (e) { ownKeysRejected = e instanceof TypeError; }
      |getRejected && setRejected && hasRejected && ownKeysRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Proxy.revocable throws after revoke for call and construct") {
    val result = eval("""
      |function Target(x) { this.x = x; }
      |var callable = Proxy.revocable(function(x) { return x + 1; }, {});
      |var constructable = Proxy.revocable(Target, {});
      |callable.revoke();
      |constructable.revoke();
      |var callRejected = false;
      |var constructRejected = false;
      |try { callable.proxy(1); } catch (e) { callRejected = e instanceof TypeError; }
      |try { new constructable.proxy(1); } catch (e) { constructRejected = e instanceof TypeError; }
      |callRejected && constructRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Proxy apply trap is used for direct calls") {
    val result = eval("""
      |function add(a, b) { return a + b; }
      |var seenTarget = false;
      |var seenThis = false;
      |var proxy = new Proxy(add, {
      |  apply: function(target, thisArg, args) {
      |    seenTarget = target === add;
      |    seenThis = thisArg === undefined;
      |    return args[0] * args[1];
      |  }
      |});
      |proxy(3, 4) === 12 && seenTarget && seenThis;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.apply uses Proxy apply trap") {
    val result = eval("""
      |function add(a, b) { return a + b; }
      |var receiver = {};
      |var proxy = new Proxy(add, {
      |  apply: function(target, thisArg, args) {
      |    return target === add && thisArg === receiver ? args[0] - args[1] : 0;
      |  }
      |});
      |Reflect.apply(proxy, receiver, [9, 4]);
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(5))
  }

  test("Proxy apply fallback forwards to callable target with method this") {
    val result = eval("""
      |var obj = {
      |  factor: 5,
      |  method: new Proxy(function(x) { return this.factor * x; }, {})
      |};
      |obj.method(3);
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(15))
  }

  test("Proxy construct trap is used for new expressions") {
    val result = eval("""
      |function Target(x) { this.value = x; }
      |var proxy = new Proxy(Target, {
      |  construct: function(target, args, newTarget) {
      |    return { value: args[0] + 1, targetSeen: target === Target, newTargetSeen: newTarget === proxy };
      |  }
      |});
      |var obj = new proxy(4);
      |obj.value === 5 && obj.targetSeen && obj.newTargetSeen;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.construct uses Proxy construct trap") {
    val result = eval("""
      |function Target(x) { this.value = x; }
      |var proxy = new Proxy(Target, {
      |  construct: function(target, args, newTarget) {
      |    return { value: args[0] * 2, targetSeen: target === Target, newTargetSeen: newTarget === proxy };
      |  }
      |});
      |var obj = Reflect.construct(proxy, [7]);
      |obj.value === 14 && obj.targetSeen && obj.newTargetSeen;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Proxy construct fallback forwards to constructor target") {
    val result = eval("""
      |function Target(x) { this.value = x; }
      |var proxy = new Proxy(Target, {});
      |var obj = new proxy(6);
      |obj.value === 6 && obj instanceof Target;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Proxy construct trap must return an object") {
    val result = eval("""
      |function Target() {}
      |var proxy = new Proxy(Target, {
      |  construct: function() { return 1; }
      |});
      |var rejected = false;
      |try { new proxy(); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.defineProperty - uses Proxy defineProperty trap") {
    val result = eval("""
      |var target = {};
      |var called = false;
      |var proxy = new Proxy(target, {
      |  defineProperty: function(obj, prop, desc) {
      |    called = prop === 'x' && desc.value === 42;
      |    return true;
      |  }
      |});
      |Reflect.defineProperty(proxy, 'x', { value: 42 }) === true &&
      |  called &&
      |  target.x === undefined &&
      |  proxy.x === undefined;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.defineProperty - Proxy receives symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var target = {};
      |var called = false;
      |var proxy = new Proxy(target, {
      |  defineProperty: function(obj, prop, desc) {
      |    called = prop === sym && desc.value === 7;
      |    return true;
      |  }
      |});
      |Reflect.defineProperty(proxy, sym, { value: 7 }) === true &&
      |  called &&
      |  target[sym] === undefined;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.defineProperty - Proxy cannot add property to non-extensible target") {
    val result = eval("""
      |var target = {};
      |Object.preventExtensions(target);
      |var proxy = new Proxy(target, {
      |  defineProperty: function() { return true; }
      |});
      |var rejected = false;
      |try { Reflect.defineProperty(proxy, 'x', { value: 1 }); } catch (e) { rejected = e instanceof TypeError; }
      |rejected && target.x === undefined;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.getOwnPropertyDescriptor - Proxy cannot hide non-configurable property") {
    val result = eval("""
      |var target = {};
      |Object.defineProperty(target, 'x', { value: 1, configurable: false });
      |var proxy = new Proxy(target, {
      |  getOwnPropertyDescriptor: function() { return undefined; }
      |});
      |var rejected = false;
      |try { Reflect.getOwnPropertyDescriptor(proxy, 'x'); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.getOwnPropertyDescriptor - Proxy receives symbol property key") {
    val result = eval("""
      |var sym = Symbol('k');
      |var proxy = new Proxy({}, {
      |  getOwnPropertyDescriptor: function(obj, prop) {
      |    if (prop === sym) return { value: 8, configurable: true };
      |  }
      |});
      |Reflect.getOwnPropertyDescriptor(proxy, sym).value === 8;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.ownKeys - uses Proxy ownKeys trap and rejects duplicates") {
    val result = eval("""
      |var target = {};
      |var proxy = new Proxy(target, {
      |  ownKeys: function() { return ['a', 'b']; }
      |});
      |var dupProxy = new Proxy(target, {
      |  ownKeys: function() { return ['a', 'a']; }
      |});
      |var keys = Reflect.ownKeys(proxy);
      |var duplicateRejected = false;
      |try { Reflect.ownKeys(dupProxy); } catch (e) { duplicateRejected = e instanceof TypeError; }
      |keys.length === 2 && keys[0] === 'a' && keys[1] === 'b' && duplicateRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.ownKeys - Proxy must include non-configurable target keys") {
    val result = eval("""
      |var target = {};
      |Object.defineProperty(target, 'x', { value: 1, configurable: false });
      |var proxy = new Proxy(target, {
      |  ownKeys: function() { return []; }
      |});
      |var rejected = false;
      |try { Reflect.ownKeys(proxy); } catch (e) { rejected = e instanceof TypeError; }
      |rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect.ownKeys - Proxy preserves symbol keys") {
    val result = eval("""
      |var sym = Symbol('k');
      |var proxy = new Proxy({}, {
      |  ownKeys: function() { return ['a', sym]; }
      |});
      |var keys = Reflect.ownKeys(proxy);
      |keys.length === 2 && keys[0] === 'a' && keys[1] === sym;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Reflect - check existence before access") {
    val result = eval("""
      |var obj = { a: 1 };
      |var result = Reflect.has(obj, 'a') ? Reflect.get(obj, 'a') : 'default';
      |result;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(1))
  }

  test("Reflect - dynamic property manipulation") {
    val result = eval("""
      |var obj = {};
      |Reflect.set(obj, 'name', 'test');
      |Reflect.set(obj, 'count', 5);
      |Reflect.has(obj, 'name') && obj.count === 5;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }
