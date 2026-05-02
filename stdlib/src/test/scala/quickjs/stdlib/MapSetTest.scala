package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class MapSetTest extends FunSuite:

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
  // Map Tests
  // ============================================================

  test("Map - create empty map") {
    val result = eval("""
      |var m = new Map();
      |m.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(0))
  }

  test("Map - set and get") {
    val result = eval("""
      |var m = new Map();
      |m.set('a', 1);
      |m.get('a');
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(1))
  }

  test("Map - get non-existent key returns undefined") {
    val result = eval("""
      |var m = new Map();
      |m.get('missing');
      |""".stripMargin)
    assertEquals(result, JSValue.Undefined)
  }

  test("Map - has") {
    val result = eval("""
      |var m = new Map();
      |m.set('key', 'value');
      |var hasKey = m.has('key');
      |var hasMissing = m.has('missing');
      |hasKey && !hasMissing;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Map - size") {
    val result = eval("""
      |var m = new Map();
      |m.set('a', 1);
      |m.set('b', 2);
      |m.set('c', 3);
      |m.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(3))
  }

  test("Map - delete") {
    val result = eval("""
      |var m = new Map();
      |m.set('a', 1);
      |m.set('b', 2);
      |var deleted = m.delete('a');
      |var hasA = m.has('a');
      |var hasB = m.has('b');
      |deleted && !hasA && hasB;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Map - delete non-existent returns false") {
    val result = eval("""
      |var m = new Map();
      |m.delete('missing');
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  test("Map - clear") {
    val result = eval("""
      |var m = new Map();
      |m.set('a', 1);
      |m.set('b', 2);
      |m.clear();
      |m.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(0))
  }

  test("Map - overwrite existing key") {
    val result = eval("""
      |var m = new Map();
      |m.set('key', 'first');
      |m.set('key', 'second');
      |m.get('key');
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("second"))
  }

  test("Map - object as key") {
    val result = eval("""
      |var m = new Map();
      |var obj = { name: 'test' };
      |m.set(obj, 'value');
      |m.get(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("value"))
  }

  test("Map - different objects are different keys") {
    val result = eval("""
      |var m = new Map();
      |var obj1 = { x: 1 };
      |var obj2 = { x: 1 };
      |m.set(obj1, 'first');
      |m.set(obj2, 'second');
      |m.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(2))
  }

  test("Map - NaN as key") {
    val result = eval("""
      |var m = new Map();
      |m.set(NaN, 'nan-value');
      |m.get(NaN);
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("nan-value"))
  }

  test("Map - forEach") {
    val result = eval("""
      |var m = new Map();
      |m.set('a', 1);
      |m.set('b', 2);
      |m.set('c', 3);
      |var sum = 0;
      |m.forEach(function(value, key) {
      |  sum = sum + value;
      |});
      |sum;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(6))
  }

  test("Map - keys returns array") {
    val result = eval("""
      |var m = new Map();
      |m.set('a', 1);
      |m.set('b', 2);
      |var keys = m.keys();
      |keys.length;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(2))
  }

  test("Map - values returns array") {
    val result = eval("""
      |var m = new Map();
      |m.set('a', 10);
      |m.set('b', 20);
      |var values = m.values();
      |values[0] + values[1];
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(30))
  }

  test("Map - entries returns array of [key, value]") {
    val result = eval("""
      |var m = new Map();
      |m.set('x', 100);
      |var entries = m.entries();
      |entries[0][0] === 'x' && entries[0][1] === 100;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Map - constructor with iterable") {
    val result = eval("""
      |var m = new Map([['a', 1], ['b', 2]]);
      |m.size === 2 && m.get('a') === 1 && m.get('b') === 2;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Map - chaining set calls") {
    val result = eval("""
      |var m = new Map();
      |m.set('a', 1).set('b', 2).set('c', 3);
      |m.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(3))
  }

  test("Map - maintains insertion order") {
    val result = eval("""
      |var m = new Map();
      |m.set('c', 3);
      |m.set('a', 1);
      |m.set('b', 2);
      |var keys = m.keys();
      |keys[0] === 'c' && keys[1] === 'a' && keys[2] === 'b';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Set Tests
  // ============================================================

  test("Set - create empty set") {
    val result = eval("""
      |var s = new Set();
      |s.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(0))
  }

  test("Set - add and has") {
    val result = eval("""
      |var s = new Set();
      |s.add(1);
      |s.add(2);
      |s.has(1) && s.has(2) && !s.has(3);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Set - size") {
    val result = eval("""
      |var s = new Set();
      |s.add('a');
      |s.add('b');
      |s.add('c');
      |s.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(3))
  }

  test("Set - duplicates are ignored") {
    val result = eval("""
      |var s = new Set();
      |s.add(1);
      |s.add(1);
      |s.add(1);
      |s.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(1))
  }

  test("Set - delete") {
    val result = eval("""
      |var s = new Set();
      |s.add(1);
      |s.add(2);
      |var deleted = s.delete(1);
      |deleted && !s.has(1) && s.has(2);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Set - delete non-existent returns false") {
    val result = eval("""
      |var s = new Set();
      |s.delete(999);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  test("Set - clear") {
    val result = eval("""
      |var s = new Set();
      |s.add(1);
      |s.add(2);
      |s.add(3);
      |s.clear();
      |s.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(0))
  }

  test("Set - object as value") {
    val result = eval("""
      |var s = new Set();
      |var obj = { x: 1 };
      |s.add(obj);
      |s.has(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Set - different objects are different values") {
    val result = eval("""
      |var s = new Set();
      |var obj1 = { x: 1 };
      |var obj2 = { x: 1 };
      |s.add(obj1);
      |s.add(obj2);
      |s.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(2))
  }

  test("Set - NaN handling") {
    val result = eval("""
      |var s = new Set();
      |s.add(NaN);
      |s.add(NaN);
      |s.size === 1 && s.has(NaN);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Set - forEach") {
    val result = eval("""
      |var s = new Set();
      |s.add(1);
      |s.add(2);
      |s.add(3);
      |var sum = 0;
      |s.forEach(function(value) {
      |  sum = sum + value;
      |});
      |sum;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(6))
  }

  test("Set - values returns array") {
    val result = eval("""
      |var s = new Set();
      |s.add(10);
      |s.add(20);
      |var values = s.values();
      |values.length === 2;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Set - keys is alias for values") {
    val result = eval("""
      |var s = new Set();
      |s.add(1);
      |s.add(2);
      |var keys = s.keys();
      |var values = s.values();
      |keys.length === values.length;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Set - entries returns [value, value] pairs") {
    val result = eval("""
      |var s = new Set();
      |s.add('x');
      |var entries = s.entries();
      |entries[0][0] === 'x' && entries[0][1] === 'x';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Set - constructor with iterable array") {
    val result = eval("""
      |var s = new Set([1, 2, 3, 2, 1]);
      |s.size === 3 && s.has(1) && s.has(2) && s.has(3);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Set - constructor with string iterates characters") {
    val result = eval("""
      |var s = new Set('hello');
      |s.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(4)) // h, e, l, o (l is deduplicated)
  }

  test("Set - chaining add calls") {
    val result = eval("""
      |var s = new Set();
      |s.add(1).add(2).add(3);
      |s.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(3))
  }

  test("Set - maintains insertion order") {
    val result = eval("""
      |var s = new Set();
      |s.add('c');
      |s.add('a');
      |s.add('b');
      |var values = s.values();
      |values[0] === 'c' && values[1] === 'a' && values[2] === 'b';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Edge Cases
  // ============================================================

  test("Map - undefined as key") {
    val result = eval("""
      |var m = new Map();
      |m.set(undefined, 'undef-value');
      |m.get(undefined);
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("undef-value"))
  }

  test("Map - null as key") {
    val result = eval("""
      |var m = new Map();
      |m.set(null, 'null-value');
      |m.get(null);
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("null-value"))
  }

  test("Set - undefined in set") {
    val result = eval("""
      |var s = new Set();
      |s.add(undefined);
      |s.has(undefined);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Set - null in set") {
    val result = eval("""
      |var s = new Set();
      |s.add(null);
      |s.has(null);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Map - +0 and -0 are same key") {
    val result = eval("""
      |var m = new Map();
      |m.set(0, 'zero');
      |m.set(-0, 'negative-zero');
      |m.size === 1 && m.get(0) === 'negative-zero';
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Set - +0 and -0 are same value") {
    val result = eval("""
      |var s = new Set();
      |s.add(0);
      |s.add(-0);
      |s.size;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(1))
  }
