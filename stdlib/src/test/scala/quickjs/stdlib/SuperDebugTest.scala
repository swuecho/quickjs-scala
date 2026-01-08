package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib, JSException}
import quickjs.value.JSValue
import munit.*

class SuperDebugTest extends FunSuite:

  private def eval(source: String): JSValue =
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("simple extends without super") {
    val result = eval("""
      |class Animal {}
      |class Dog extends Animal {}
      |var d = new Dog();
      |"ok";
      |""".stripMargin)
    assertEquals(result.toString, "ok")
  }

  test("extends with explicit super()") {
    val result = eval("""
      |class Animal {
      |  constructor() {
      |    this.type = "animal";
      |  }
      |}
      |class Dog extends Animal {
      |  constructor() {
      |    super();
      |  }
      |}
      |var d = new Dog();
      |d.type;
      |""".stripMargin)
    assertEquals(result.toString, "animal")
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
