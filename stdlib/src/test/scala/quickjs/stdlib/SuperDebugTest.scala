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
    println(s"Bytecode: ${bytecode.bytecode.length} instructions")
    try
      interpreter.call(bytecode, JSValue.Undefined, Array.empty)
    catch
      case e: JSException =>
        println(s"JSException: ${e.getMessage}")
        e.getValue match
          case JSValue.Object(obj) =>
            println(s"  Error name: ${obj.get("name")}")
            println(s"  Error message: ${obj.get("message")}")
            println(s"  Error stack: ${obj.get("stack")}")
          case v =>
            println(s"  Value: $v")
        throw e

  test("debug: simple extends without super") {
    val result = eval("""
      |class Animal {}
      |class Dog extends Animal {}
      |var d = new Dog();
      |"ok";
      |""".stripMargin)
    assertEquals(result.toString, "ok")
  }

  test("debug: extends with explicit super()") {
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
