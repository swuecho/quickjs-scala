package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class ClassInheritanceTest extends FunSuite:

  private def eval(source: String): JSValue =
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("basic class definition") {
    val result = eval("""
      |class Animal {
      |  constructor(name) {
      |    this.name = name;
      |  }
      |  speak() {
      |    return this.name + " makes a sound";
      |  }
      |}
      |var a = new Animal("Dog");
      |a.speak();
      |""".stripMargin)
    assertEquals(result.toString, "Dog makes a sound")
  }

  test("class extends - basic") {
    val result = eval("""
      |class Animal {
      |  constructor(name) {
      |    this.name = name;
      |  }
      |}
      |class Dog extends Animal {
      |  constructor(name) {
      |    super(name);
      |  }
      |}
      |var d = new Dog("Buddy");
      |d.name;
      |""".stripMargin)
    assertEquals(result.toString, "Buddy")
  }

  test("super() in constructor") {
    val result = eval("""
      |class Animal {
      |  constructor(name) {
      |    this.name = name;
      |  }
      |}
      |class Dog extends Animal {
      |  constructor(name, breed) {
      |    super(name);
      |    this.breed = breed;
      |  }
      |}
      |var d = new Dog("Buddy", "Labrador");
      |d.name + " is a " + d.breed;
      |""".stripMargin)
    assertEquals(result.toString, "Buddy is a Labrador")
  }

  test("super.method() call") {
    val result = eval("""
      |class Animal {
      |  speak() {
      |    return "Animal speaks";
      |  }
      |}
      |class Dog extends Animal {
      |  speak() {
      |    return super.speak() + " - Woof!";
      |  }
      |}
      |var d = new Dog();
      |d.speak();
      |""".stripMargin)
    assertEquals(result.toString, "Animal speaks - Woof!")
  }

  test("super.property access") {
    val result = eval("""
      |class Animal {
      |  get type() {
      |    return "animal";
      |  }
      |}
      |class Dog extends Animal {
      |  get type() {
      |    return super.type + "/dog";
      |  }
      |}
      |var d = new Dog();
      |d.type;
      |""".stripMargin)
    assertEquals(result.toString, "animal/dog")
  }

  test("instanceof with inheritance") {
    val result = eval("""
      |class Animal {}
      |class Dog extends Animal {}
      |var d = new Dog();
      |[d instanceof Dog, d instanceof Animal];
      |""".stripMargin)
    result match
      case arr: JSValue.JSArrayVal =>
        assertEquals(arr.value.get(0), JSValue.Bool(true))
        assertEquals(arr.value.get(1), JSValue.Bool(true))
      case _ => fail("Expected array")
  }

  test("method override without super") {
    val result = eval("""
      |class Animal {
      |  speak() {
      |    return "generic sound";
      |  }
      |}
      |class Dog extends Animal {
      |  speak() {
      |    return "Woof!";
      |  }
      |}
      |var d = new Dog();
      |d.speak();
      |""".stripMargin)
    assertEquals(result.toString, "Woof!")
  }

  test("inherit method from parent") {
    val result = eval("""
      |class Animal {
      |  speak() {
      |    return "Animal speaks";
      |  }
      |}
      |class Dog extends Animal {
      |  bark() {
      |    return "Woof!";
      |  }
      |}
      |var d = new Dog();
      |d.speak() + " and " + d.bark();
      |""".stripMargin)
    assertEquals(result.toString, "Animal speaks and Woof!")
  }

  test("multi-level inheritance") {
    val result = eval("""
      |class Animal {
      |  constructor(name) {
      |    this.name = name;
      |  }
      |}
      |class Dog extends Animal {
      |  constructor(name, breed) {
      |    super(name);
      |    this.breed = breed;
      |  }
      |}
      |class Labrador extends Dog {
      |  constructor(name) {
      |    super(name, "Labrador");
      |  }
      |}
      |var l = new Labrador("Max");
      |l.name + " - " + l.breed;
      |""".stripMargin)
    assertEquals(result.toString, "Max - Labrador")
  }

  test("static method inheritance") {
    val result = eval("""
      |class Animal {
      |  static type() {
      |    return "animal";
      |  }
      |}
      |class Dog extends Animal {
      |  static type() {
      |    return super.type() + "/dog";
      |  }
      |}
      |Dog.type();
      |""".stripMargin)
    assertEquals(result.toString, "animal/dog")
  }
