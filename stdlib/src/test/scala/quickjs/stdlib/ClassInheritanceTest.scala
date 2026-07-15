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
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
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

  test("class methods use configurable non-enumerable writable descriptors") {
    val result = eval("""
      |class C {
      |  static async *stream() { yield 1; }
      |  static #hidden(value) { return value; }
      |  static method(value) { return this.#hidden(value); }
      |}
      |var staticDesc = Object.getOwnPropertyDescriptor(C, "stream");
      |staticDesc.configurable && !staticDesc.enumerable && staticDesc.writable &&
      |delete C.stream && !Object.prototype.hasOwnProperty.call(C, "stream");
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("private getters and setters install on each instance") {
    val result = eval("""
      |class C {
      |  #value = 1;
      |  get #accessor() { return this.#value; }
      |  set #accessor(value) { this.#value = value; }
      |  read() { return this.#accessor; }
      |  write(value) { this.#accessor = value; }
      |}
      |var c = new C();
      |c.write(42);
      |c.read();
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(42))
  }

  test("private methods are shared by instances and retain private names") {
    val result = eval("""
      |class C {
      |  #method() { return 1; }
      |  async #asyncMethod() { return 2; }
      |  *#generatorMethod() { yield 3; }
      |  methods() {
      |    return [this.#method, this.#asyncMethod, this.#generatorMethod];
      |  }
      |}
      |var first = new C().methods();
      |var second = new C().methods();
      |first[0] === second[0] && first[0].name === "#method" &&
      |first[1] === second[1] && first[1].name === "#asyncMethod" &&
      |first[2] === second[2] && first[2].name === "#generatorMethod";
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("private static accessors compile and share their private slot") {
    val result = eval("""
      |class C {
      |  static #value = 1;
      |  static get #accessor() { return this.#value; }
      |  static set #accessor(value) { this.#value = value; }
      |  static getAccessor() { return this.#accessor; }
      |  static setAccessor(value) { this.#accessor = value; }
      |}
      |C.setAccessor(42);
      |C.getAccessor() === 42;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("separate class evaluations create distinct private brands") {
    val result = eval("""
      |function makeClass() {
      |  return class {
      |    #method() { return 1; }
      |    method() { return this.#method; }
      |    static get #value() { return 1; }
      |    static set #value(value) {}
      |    static read() { return this.#value; }
      |    static write(value) { this.#value = value; }
      |  };
      |}
      |var C1 = makeClass();
      |var C2 = makeClass();
      |var instanceRejected = false;
      |var getterRejected = false;
      |var setterRejected = false;
      |try { C1.prototype.method.call(new C2()); }
      |catch (e) { instanceRejected = e instanceof TypeError; }
      |try { C1.read.call(C2); }
      |catch (e) { getterRejected = e instanceof TypeError; }
      |try { C1.write.call(C2, 2); }
      |catch (e) { setterRejected = e instanceof TypeError; }
      |instanceRejected + ":" + getterRejected + ":" + setterRejected;
      |""".stripMargin)
    assertEquals(result.toString, "true:true:true")
  }

  test("nested classes construct and shadow outer private names") {
    val result = eval("""
      |class Outer {
      |  #method() { return "outer"; }
      |  call() { return this.#method(); }
      |  Inner = class {
      |    #method() { return "inner"; }
      |    call(value) { return value.#method(); }
      |    write(value) { value.#method = 1; }
      |  };
      |  static #value = "outer static";
      |  static read() { return this.#value; }
      |  static Inner = class {
      |    static #value = "inner static";
      |    static read() { return this.#value; }
      |  };
      |}
      |var outer = new Outer();
      |var inner = new outer.Inner();
      |var writeRejected = false;
      |try { inner.write(inner); }
      |catch (e) { writeRejected = e instanceof TypeError; }
      |outer.call() + ":" + inner.call(inner) + ":" +
      |Outer.read() + ":" + Outer.Inner.read() + ":" + writeRejected;
      |""".stripMargin)
    assertEquals(
      result.toString,
      "outer:inner:outer static:inner static:true"
    )
  }

  test("computed instance fields evaluate key before value") {
    val result = eval("""
      |var order = "";
      |function key() { order += "k"; return "field"; }
      |function value() { order += "v"; return 42; }
      |class C { [key()] = value(); }
      |var c = new C();
      |var x = "b";
      |class D { [x] = 7; }
      |var d = new D();
      |order + ":" + c.field + ":" + d.b;
      |""".stripMargin)
    assertEquals(result.toString, "kv:42:7")
  }

  test("computed static and instance field keys follow source order") {
    val result = eval("""
      |var i = 0;
      |class C {
      |  [i++] = i++;
      |  static [i++] = i++;
      |  [i++] = i++;
      |}
      |var c = new C();
      |var staticDesc = Object.getOwnPropertyDescriptor(C, "1");
      |var enumerated = false;
      |for (var key in C) { if (key === "1") enumerated = true; }
      |i === 6 && c[0] === 4 && c[2] === 5 && C[1] === 3 &&
      |staticDesc.enumerable && staticDesc.writable && staticDesc.configurable &&
      |Object.keys(C).indexOf("1") !== -1 && enumerated;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("private generator methods retain generator semantics") {
    val result = eval("""
      |class C {
      |  *#values() { yield 1; yield* [2, 3]; }
      |  values() { return this.#values(); }
      |}
      |var iterator = new C().values();
      |var first = iterator.next();
      |var second = iterator.next();
      |var third = iterator.next();
      |var last = iterator.next();
      |first.value + ":" + first.done + ":" +
      |second.value + ":" + second.done + ":" +
      |third.value + ":" + third.done + ":" + last.done;
    """.stripMargin)
    assertEquals(result.toString, "1:false:2:false:3:false:true")
  }

  test("new.target is current for functions and lexical through arrows") {
    val result = eval("""
      |function Direct() { this.seen = new.target; }
      |function Arrow() { return (() => new.target)(); }
      |var direct = new Direct();
      |var arrow = new Arrow();
      |direct.seen === Direct && arrow === Arrow;
    """.stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("class field direct eval inherits field lexical context") {
    val result = eval("""
      |class Base {}
      |Base.prototype.answer = 35;
      |class C extends Base {
      |  #value = 7;
      |  get #accessor() { return this.#value; }
      |  result = (() => eval("this.#accessor + super.answer"))();
      |}
      |var argumentsRejected = false;
      |var superCallRejected = false;
      |try { class D { x = eval("arguments"); }; new D(); }
      |catch (e) { argumentsRejected = e instanceof SyntaxError; }
      |try { class E extends Base { x = eval("super()"); }; new E(); }
      |catch (e) { superCallRejected = e instanceof SyntaxError; }
      |new C().result === 42 && argumentsRejected && superCallRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("direct eval in class methods inherits private names") {
    val result = eval("""
      |class C {
      |  #value = 42;
      |  read() { return eval("this.#value"); }
      |  static #staticValue = 43;
      |  static read() { return eval("this.#staticValue"); }
      |}
      |new C().read() + ":" + C.read();
      |""".stripMargin)
    assertEquals(result.toString, "42:43")
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
