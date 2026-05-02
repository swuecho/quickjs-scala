package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSException, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

/** Ported Date tests from QuickJS test_builtin.js. */
class QuickJSDateTest extends FunSuite:

  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("Date parsing and formatting basics") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result =
      try
        eval("""
      |(function() {
      |  function assertEq(a, b, msg) {
      |    if (a !== b) throw Error(msg + ": got " + a + " expected " + b);
      |  }
      |  function assertNaN(x, msg) {
      |    if (!(x !== x)) throw Error(msg + ": expected NaN");
      |  }
      |
      |  assertNaN(Date.parse(""), "Date.parse empty");
      |  assertEq(Date.parse("2000"), 946684800000, "Date.parse year");
      |  assertEq(Date.parse("2000-01"), 946684800000, "Date.parse year-month");
      |  assertEq(Date.parse("2000-01-01"), 946684800000, "Date.parse date");
      |  assertEq(Date.parse("2000-01-01T00:00Z"), 946684800000, "Date.parse time Z");
      |  assertEq(Date.parse("2000-01-01T00:00:00Z"), 946684800000, "Date.parse time Z full");
      |  assertEq(Date.parse("2000-01-01T00:00:00.1Z"), 946684800100, "Date.parse ms 1");
      |  assertEq(Date.parse("2000-01-01T00:00:00.10Z"), 946684800100, "Date.parse ms 10");
      |  assertEq(Date.parse("2000-01-01T00:00:00.100Z"), 946684800100, "Date.parse ms 100");
      |  assertEq(Date.parse("2000-01-01T00:00:00.1000Z"), 946684800100, "Date.parse ms 1000");
      |  assertEq(Date.parse("2000-01-01T00:00:00+00:00"), 946684800000, "Date.parse offset");
      |
      |  var d = new Date("2000T00:00");
      |  if (typeof d !== "object" || d.toString() === "Invalid Date")
      |    throw Error("Date constructor invalid");
      |  assertEq((new Date("Jan 1 2000")).toISOString(), d.toISOString(), "Jan 1 2000");
      |  assertEq((new Date("Jan 1 2000 00:00")).toISOString(), d.toISOString(), "Jan 1 2000 00:00");
      |  assertEq((new Date("Jan 1 2000 00:00:00")).toISOString(), d.toISOString(), "Jan 1 2000 00:00:00");
      |  assertEq((new Date("Jan 1 2000 00:00:00 GMT+0100")).toISOString(), "1999-12-31T23:00:00.000Z", "Jan 1 2000 GMT+0100");
      |  assertEq((new Date("Jan 1 2000 00:00:00 GMT+0200")).toISOString(), "1999-12-31T22:00:00.000Z", "Jan 1 2000 GMT+0200");
      |  assertEq((new Date("Sat Jan 1 2000")).toISOString(), d.toISOString(), "Sat Jan 1 2000");
      |  assertEq((new Date("Sat Jan 1 2000 00:00")).toISOString(), d.toISOString(), "Sat Jan 1 2000 00:00");
      |  assertEq((new Date("Sat Jan 1 2000 00:00:00")).toISOString(), d.toISOString(), "Sat Jan 1 2000 00:00:00");
      |  assertEq((new Date("Sat Jan 1 2000 00:00:00 GMT+0100")).toISOString(), "1999-12-31T23:00:00.000Z", "Sat Jan 1 2000 GMT+0100");
      |  assertEq((new Date("Sat Jan 1 2000 00:00:00 GMT+0200")).toISOString(), "1999-12-31T22:00:00.000Z", "Sat Jan 1 2000 GMT+0200");
      |
      |  var d2 = new Date(1506098258091);
      |  assertEq(d2.toISOString(), "2017-09-22T16:37:38.091Z", "toISOString epoch");
      |  d2.setUTCHours(18, 10, 11);
      |  assertEq(d2.toISOString(), "2017-09-22T18:10:11.091Z", "setUTCHours");
      |  var a = Date.parse(d2.toISOString());
      |  assertEq((new Date(a)).toISOString(), d2.toISOString(), "round trip");
      |
      |  assertEq((new Date("2020-01-01T01:01:01.123Z")).toISOString(), "2020-01-01T01:01:01.123Z", "ms 123");
      |  assertEq((new Date("2020-01-01T01:01:01.1Z")).toISOString(), "2020-01-01T01:01:01.100Z", "ms 1");
      |  assertEq((new Date("2020-01-01T01:01:01.12Z")).toISOString(), "2020-01-01T01:01:01.120Z", "ms 12");
      |  assertEq((new Date("2020-01-01T01:01:01.1234Z")).toISOString(), "2020-01-01T01:01:01.123Z", "ms 1234");
      |  assertEq((new Date("2020-01-01T01:01:01.12345Z")).toISOString(), "2020-01-01T01:01:01.123Z", "ms 12345");
      |  assertEq((new Date("2020-01-01T01:01:01.1235Z")).toISOString(), "2020-01-01T01:01:01.123Z", "ms 1235");
      |  assertEq((new Date("2020-01-01T01:01:01.9999Z")).toISOString(), "2020-01-01T01:01:01.999Z", "ms 9999");
      |
      |  assertEq(Date.UTC(2017), 1483228800000, "UTC year");
      |  assertEq(Date.UTC(2017, 9), 1506816000000, "UTC month");
      |  assertEq(Date.UTC(2017, 9, 22), 1508630400000, "UTC day");
      |  assertEq(Date.UTC(2017, 9, 22, 18), 1508695200000, "UTC hour");
      |  assertEq(Date.UTC(2017, 9, 22, 18, 10), 1508695800000, "UTC minute");
      |  assertEq(Date.UTC(2017, 9, 22, 18, 10, 11), 1508695811000, "UTC second");
      |  assertEq(Date.UTC(2017, 9, 22, 18, 10, 11, 91), 1508695811091, "UTC ms");
      |
      |  assertNaN(Date.UTC(NaN), "UTC NaN year");
      |  assertNaN(Date.UTC(2017, NaN), "UTC NaN month");
      |  assertNaN(Date.UTC(2017, 9, NaN), "UTC NaN day");
      |  assertNaN(Date.UTC(2017, 9, 22, NaN), "UTC NaN hour");
      |  assertNaN(Date.UTC(2017, 9, 22, 18, NaN), "UTC NaN minute");
      |  assertNaN(Date.UTC(2017, 9, 22, 18, 10, NaN), "UTC NaN second");
      |  assertNaN(Date.UTC(2017, 9, 22, 18, 10, 11, NaN), "UTC NaN ms");
      |  assertEq(Date.UTC(2017, 9, 22, 18, 10, 11, 91, NaN), 1508695811091, "UTC extra arg");
      |
      |  return true;
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

    assertEquals(result, JSValue.fromBoolean(true))
  }
