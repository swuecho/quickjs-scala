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
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
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

  test("tagged template passes template object and substitutions") {
    val result = eval("""
      |function tag(strings, value) {
      |  return strings[0] + value + strings[1];
      |}
      |tag`a${1}b`;
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("a1b"))
  }

  test("tagged template exposes raw strings") {
    val result = eval("""
      |function tag(strings) {
      |  return strings[0] + "|" + strings.raw[0];
      |}
      |tag`line\n`;
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("line\n|line\\n"))
  }

  test("String.raw consumes tagged template raw array") {
    val result = eval("String.raw `a\\n${1}b`;")
    assertEquals(result, JSValue.JSStr("a\\n1b"))
  }

  test("tagged template preserves member this binding") {
    val result = eval("""
      |var obj = {
      |  prefix: "ok:",
      |  tag: function(strings, value) {
      |    return this.prefix + strings[0] + value;
      |  }
      |};
      |obj.tag`v=${3}`;
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("ok:v=3"))
  }

  test("tagged template caches object per call site") {
    val result = eval("""
      |var first;
      |function tag(strings) {
      |  if (first === undefined) {
      |    first = strings;
      |    return "first";
      |  }
      |  return first === strings ? "same" : "different";
      |}
      |function again() { return tag`x`; }
      |again();
      |again();
      |""".stripMargin)
    assertEquals(result, JSValue.JSStr("same"))
  }

  test("tagged template uses distinct objects for distinct call sites") {
    val result = eval("""
      |function tag(strings) { return strings; }
      |var a = tag`x`;
      |var b = tag`x`;
      |a === b;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(false))
  }
