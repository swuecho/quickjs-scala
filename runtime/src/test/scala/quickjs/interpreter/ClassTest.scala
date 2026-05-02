package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class ClassTest extends FunSuite:

  def withContext(f: (JSRuntime, JSContext) => Unit): Unit =
    val rt = JSRuntime()
    val ctx = JSContext(rt)
    StdLib.initialize(ctx)
    f(rt, ctx)

  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("basic class definition and instantiation") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class MyClass {
          constructor(x) {
            this.value = x;
          }
          getValue() {
            return this.value;
          }
        }
        var c = new MyClass(42);
        c.getValue();
      """)
      assertEquals(result, JSValue.Int32(42))
    }
  }

  test("class instance with method") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Counter {
          constructor() {
            this.value = 0;
          }
          increment() {
            this.value++;
          }
        }
        var c = new Counter();
        c.increment();
        c.value;
      """)
      assertEquals(result, JSValue.Int32(1))
    }
  }

  test("class with multiple methods") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class MultiMethod {
          constructor() {
            this.value = 0;
          }
          increment() {
            this.value++;
          }
          reset() {
            this.value = 0;
          }
        }
        var c = new MultiMethod();
        c.increment();
        c.increment();
        c.value;
      """)
      assertEquals(result, JSValue.Int32(2))
    }
  }

  test("class inheritance - basic") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Animal {
          constructor(name) {
            this.name = name;
          }
          speak() {
            return this.name + " makes noise";
          }
        }
        class Dog extends Animal {
          speak() {
            return this.name + " barks";
          }
        }
        var d = new Dog("Rex");
        d.speak();
      """)
      assertEquals(result, JSValue.fromString("Rex barks"))
    }
  }

  test("class inheritance - super constructor") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Animal {
          constructor(name) {
            this.name = name;
          }
        }
        class Dog extends Animal {
          constructor(name, breed) {
            super(name);
            this.breed = breed;
          }
        }
        var d = new Dog("Rex", "German Shepherd");
        d.name + ":" + d.breed;
      """)
      assertEquals(result, JSValue.fromString("Rex:German Shepherd"))
    }
  }

  test("class inheritance - super method") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Animal {
          speak() {
            return "noise";
          }
        }
        class Dog extends Animal {
          speak() {
            return "bark:" + super.speak();
          }
        }
        var d = new Dog();
        d.speak();
      """)
      assertEquals(result, JSValue.fromString("bark:noise"))
    }
  }

  test("class with getter") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Rectangle {
          constructor(w, h) {
            this.w = w;
            this.h = h;
          }
          get area() {
            return this.w * this.h;
          }
        }
        var r = new Rectangle(3, 4);
        r.area;
      """)
      assertEquals(result, JSValue.Int32(12))
    }
  }

  test("class with setter") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Person {
          constructor() {
            this._name = "";
          }
          set name(n) {
            this._name = n;
          }
          get name() {
            return this._name;
          }
        }
        var p = new Person();
        p.name = "Alice";
        p.name;
      """)
      assertEquals(result, JSValue.fromString("Alice"))
    }
  }

  test("class static method") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class MathUtil {
          static add(a, b) {
            return a + b;
          }
        }
        MathUtil.add(10, 20);
      """)
      assertEquals(result, JSValue.Int32(30))
    }
  }

  test("class instanceof") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Animal {}
        class Dog extends Animal {}
        var d = new Dog();
        d instanceof Dog && d instanceof Animal;
      """)
      assertEquals(result, JSValue.Bool(true))
    }
  }

  test("class with private field") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Counter {
          #count = 0;
          increment() {
            this.#count++;
          }
          getValue() {
            return this.#count;
          }
        }
        var c = new Counter();
        c.increment();
        c.increment();
        c.getValue();
      """)
      assertEquals(result, JSValue.Int32(2))
    }
  }

  test("class extends with private field") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Base {
          #secret = 42;
          getSecret() {
            return this.#secret;
          }
        }
        class Derived extends Base {
          getValue() {
            return this.getSecret();
          }
        }
        var d = new Derived();
        d.getValue();
      """)
      assertEquals(result, JSValue.Int32(42))
    }
  }
