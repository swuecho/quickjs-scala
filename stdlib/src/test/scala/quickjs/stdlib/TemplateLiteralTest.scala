package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class TemplateLiteralTest extends FunSuite:

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

  test("simple template literal") {
    val result = eval("`hello world`")
    assertEquals(result, JSValue.JSStr("hello world"))
  }

  test("template literal with variable interpolation") {
    val result = eval("""
      |var name = "Alice";
      |`hello ${name}`;
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("hello Alice"))
  }

  test("template literal with expression") {
    val result = eval("`2 + 2 = ${2 + 2}`")
    assertEquals(result, JSValue.JSStr("2 + 2 = 4"))
  }

  test("template literal with multiple interpolations") {
    val result = eval("""
      |var a = 1;
      |var b = 2;
      |`${a} + ${b} = ${a + b}`;
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("1 + 2 = 3"))
  }

  test("template literal with nested template") {
    val result = eval("`outer ${`inner`} outer`")
    assertEquals(result, JSValue.JSStr("outer inner outer"))
  }

  test("template literal with object property") {
    val result = eval("""
      |var obj = {name: "Bob"};
      |`hello ${obj.name}`;
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("hello Bob"))
  }

  test("template literal with function call") {
    val result = eval("""
      |function greet(name) { return "Hi " + name; }
      |`${greet("Charlie")}!`;
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("Hi Charlie!"))
  }

  test("template literal with escaped characters") {
    val result = eval("`line1\\nline2`")
    assertEquals(result, JSValue.JSStr("line1\nline2"))
  }

  test("template literal with escaped backtick") {
    val result = eval("`hello \\`world\\``")
    assertEquals(result, JSValue.JSStr("hello `world`"))
  }

  test("template literal with escaped dollar sign") {
    val result = eval("`price: \\$10`")
    assertEquals(result, JSValue.JSStr("price: $10"))
  }

  test("empty template literal") {
    val result = eval("``")
    assertEquals(result, JSValue.JSStr(""))
  }

  test("template literal with only interpolation") {
    val result = eval("`${42}`")
    assertEquals(result, JSValue.JSStr("42"))
  }
