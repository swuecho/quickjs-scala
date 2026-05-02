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
