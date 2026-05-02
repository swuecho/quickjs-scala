package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class ObjectFreezeSealTest extends FunSuite:

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
  // Object.freeze Tests
  // ============================================================

  test("Object.freeze - returns the same object") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.freeze(obj) === obj;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.freeze - prevents property modification") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.freeze(obj);
      |obj.x = 2;
      |obj.x;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(1))
  }

  test("Object.freeze - prevents adding new properties") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.freeze(obj);
      |obj.y = 2;
      |obj.y;
      |""".stripMargin)
    assertEquals(result, JSValue.Undefined)
  }

  test("Object.freeze - prevents deleting properties") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.freeze(obj);
      |delete obj.x;
      |obj.x;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(1))
  }

  test("Object.freeze - Object.isFrozen returns true after freeze") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.freeze(obj);
      |Object.isFrozen(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.freeze - empty object") {
    val result = eval("""
      |var obj = {};
      |Object.freeze(obj);
      |Object.isFrozen(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Object.seal Tests
  // ============================================================

  test("Object.seal - returns the same object") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.seal(obj) === obj;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.seal - allows property modification") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.seal(obj);
      |obj.x = 2;
      |obj.x;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(2))
  }

  test("Object.seal - prevents adding new properties") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.seal(obj);
      |obj.y = 2;
      |obj.y;
      |""".stripMargin)
    assertEquals(result, JSValue.Undefined)
  }

  test("Object.seal - prevents deleting properties") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.seal(obj);
      |delete obj.x;
      |obj.x;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(1))
  }

  test("Object.seal - Object.isSealed returns true after seal") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.seal(obj);
      |Object.isSealed(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Object.isFrozen Tests
  // ============================================================

  test("Object.isFrozen - returns false for regular object") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.isFrozen(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  test("Object.isFrozen - returns true for frozen object") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.freeze(obj);
      |Object.isFrozen(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.isFrozen - returns true for primitives") {
    val result = eval("""
      |Object.isFrozen(42);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.isFrozen - sealed but writable is not frozen") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.seal(obj);
      |Object.isFrozen(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  // ============================================================
  // Object.isSealed Tests
  // ============================================================

  test("Object.isSealed - returns false for regular object") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.isSealed(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  test("Object.isSealed - returns true for sealed object") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.seal(obj);
      |Object.isSealed(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.isSealed - frozen objects are also sealed") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.freeze(obj);
      |Object.isSealed(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.isSealed - returns true for primitives") {
    val result = eval("""
      |Object.isSealed("hello");
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  // ============================================================
  // Object.preventExtensions Tests
  // ============================================================

  test("Object.preventExtensions - returns the same object") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.preventExtensions(obj) === obj;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.preventExtensions - prevents adding new properties") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.preventExtensions(obj);
      |obj.y = 2;
      |obj.y;
      |""".stripMargin)
    assertEquals(result, JSValue.Undefined)
  }

  test("Object.preventExtensions - allows modifying existing properties") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.preventExtensions(obj);
      |obj.x = 2;
      |obj.x;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(2))
  }

  test("Object.preventExtensions - allows deleting properties") {
    val result = eval("""
      |var obj = { x: 1, y: 2 };
      |Object.preventExtensions(obj);
      |delete obj.x;
      |obj.x;
      |""".stripMargin)
    assertEquals(result, JSValue.Undefined)
  }

  // ============================================================
  // Object.isExtensible Tests
  // ============================================================

  test("Object.isExtensible - returns true for regular object") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.isExtensible(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("Object.isExtensible - returns false after preventExtensions") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.preventExtensions(obj);
      |Object.isExtensible(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  test("Object.isExtensible - returns false for sealed object") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.seal(obj);
      |Object.isExtensible(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  test("Object.isExtensible - returns false for frozen object") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.freeze(obj);
      |Object.isExtensible(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  test("Object.isExtensible - returns false for primitives") {
    val result = eval("""
      |Object.isExtensible(42);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }

  // ============================================================
  // Integration Tests
  // ============================================================

  test("freeze then seal has no effect") {
    val result = eval("""
      |var obj = { x: 1 };
      |Object.freeze(obj);
      |Object.seal(obj);
      |Object.isFrozen(obj) && Object.isSealed(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("multiple properties freeze") {
    val result = eval("""
      |var obj = { a: 1, b: 2, c: 3 };
      |Object.freeze(obj);
      |obj.a = 10;
      |obj.b = 20;
      |obj.c = 30;
      |obj.a + obj.b + obj.c;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(6))
  }

  test("nested objects are not frozen") {
    val result = eval("""
      |var obj = { nested: { x: 1 } };
      |Object.freeze(obj);
      |obj.nested.x = 2;
      |obj.nested.x;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(2))
  }

  test("empty object is frozen after preventExtensions") {
    val result = eval("""
      |var obj = {};
      |Object.preventExtensions(obj);
      |Object.isFrozen(obj) && Object.isSealed(obj);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }
