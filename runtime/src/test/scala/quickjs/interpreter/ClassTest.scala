package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class ClassTest extends FunSuite:

  def withContext(testCode: (JSRuntime, JSContext) => Unit): Unit =
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)
    StdLib.initialize(ctx)
    testCode(runtime, ctx)

  def eval(source: String)(using ctx: JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("basic class declaration") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Point {
          constructor(x, y) {
            this.x = x;
            this.y = y;
          }
          getX() { return this.x; }
          getY() { return this.y; }
        }
        var p = new Point(10, 20);
        p.getX() + p.getY();
      """)
      assertEquals(result, JSValue.Int32(30))
    }
  }

  test("class with constructor return value") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Counter {
          constructor() {
            this.value = 0;
          }
          increment() {
            this.value++;
            return this.value;
          }
        }
        var c = new Counter();
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
        d.name + " - " + d.breed;
      """)
      assertEquals(result, JSValue.fromString("Rex - German Shepherd"))
    }
  }

  test("class inheritance - super method call") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Rectangle {
          constructor(w, h) {
            this.w = w;
            this.h = h;
          }
          area() {
            return this.w * this.h;
          }
        }
        class Square extends Rectangle {
          constructor(s) {
            super(s, s);
          }
          area() {
            return super.area();
          }
        }
        var s = new Square(5);
        s.area();
      """)
      assertEquals(result, JSValue.Int32(25))
    }
  }

  test("class static methods") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class MathUtils {
          static add(a, b) {
            return a + b;
          }
          static multiply(a, b) {
            return a * b;
          }
        }
        MathUtils.add(3, 4) + MathUtils.multiply(2, 5);
      """)
      assertEquals(result, JSValue.Int32(17))
    }
  }

  test("class with getters and setters") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Temperature {
          constructor(celsius) {
            this._celsius = celsius;
          }
          get celsius() {
            return this._celsius;
          }
          set celsius(value) {
            this._celsius = value;
          }
          get fahrenheit() {
            return this._celsius * 9/5 + 32;
          }
        }
        var t = new Temperature(100);
        t.celsius = 0;
        t.fahrenheit;
      """)
      assertEquals(result.toNumber, 32.0)
    }
  }

  test("class expression") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        var Person = class {
          constructor(name) {
            this.name = name;
          }
          greet() {
            return "Hello, " + this.name;
          }
        };
        var p = new Person("World");
        p.greet();
      """)
      assertEquals(result, JSValue.fromString("Hello, World"))
    }
  }

  test("class expression with binding") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        var MyClass = class Inner {
          static getName() {
            return Inner.name;
          }
        };
        MyClass.getName();
      """)
      assertEquals(result, JSValue.fromString("Inner"))
    }
  }

  test("class with instance fields") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Counter {
          count = 0;
          increment() {
            this.count++;
            return this.count;
          }
        }
        var c = new Counter();
        c.increment();
        c.increment();
        c.count;
      """)
      assertEquals(result, JSValue.Int32(2))
    }
  }

  test("class with static fields") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Counter {
          static total = 0;
          constructor() {
            Counter.total++;
          }
        }
        new Counter();
        new Counter();
        Counter.total;
      """)
      assertEquals(result, JSValue.Int32(2))
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

  test("class constructor returns object") {
    withContext { (_, ctx) =>
      given JSContext = ctx
      val result = eval("""
        class Factory {
          constructor() {
            return { created: true };
          }
        }
        var f = new Factory();
        f.created;
      """)
      assertEquals(result, JSValue.Bool(true))
    }
  }
