package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, JSException, StdLib}
import quickjs.value.JSValue
import munit.*

/** Ported error position checks from QuickJS test_builtin.js. */
class QuickJSBuiltinErrorTest extends FunSuite:

  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  private def assertJS(
      actual: JSValue,
      expected: JSValue,
      hint: String = ""
  ): Unit =
    if actual != expected then
      val msg = if hint.nonEmpty then s" ($hint)" else ""
      fail(s"assertion failed: got |$actual|, expected |$expected|$msg")

  private def stripAt(input: String): (String, Int, Int) =
    val idx = input.indexOf('@')
    if idx < 0 then throw new IllegalArgumentException("missing @ marker")
    var line = 1
    var col = 1
    var i = 0
    while i < idx do
      val ch = input.charAt(i)
      if ch == '\n' then
        line += 1
        col = 1
      else col += 1
      i += 1
    val stripped = input.substring(0, idx) + input.substring(idx + 1)
    (stripped, line, col)

  private def jsEscape(value: String): String =
    val sb = new StringBuilder(value.length + 8)
    value.foreach {
      case '\\'           => sb.append("\\\\")
      case '\''           => sb.append("\\'")
      case '\n'           => sb.append("\\n")
      case '\r'           => sb.append("\\r")
      case '\t'           => sb.append("\\t")
      case ch if ch < ' ' => sb.append(f"\\x${ch.toInt}%02x")
      case ch             => sb.append(ch)
    }
    sb.toString()

  private def assertJsonErrorStack(inputWithAt: String)(using JSContext): Unit =
    val (stripped, line, col) = stripAt(inputWithAt)
    val jsLiteral = jsEscape(stripped)
    val result =
      try eval(s"""
        |(function() {
        |  try {
        |    JSON.parse('$jsLiteral');
        |  } catch (e) {
        |    return e.stack;
        |  }
        |  return "no error";
        |})()
        |""".stripMargin)
      catch
        case ex: JSException =>
          val detail = ex.getValue match
            case JSValue.Object(obj) =>
              val msg = obj.get("message").toString
              val stack = obj.get("stack").toString
              s"message=$msg stack=$stack"
            case other =>
              s"value=$other"
          fail(s"JavaScript exception in test: $detail")
    val expected = s":$line:$col"
    val stackStr = result.toString
    if !stackStr.contains(expected) then
      fail(s"unexpected stack position. got |$stackStr| expected |$expected|")

  test("assert_json_error checks line/column from JSON.parse stack") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])
    JSON.initialize()
    assertJsonErrorStack("\n\"  \\@x\"")
    assertJsonErrorStack("\n{ \"a\": @x }\"")
  }
