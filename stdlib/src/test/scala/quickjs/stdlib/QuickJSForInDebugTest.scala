package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class QuickJSForInDebugTest extends FunSuite:

  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("for-in builds array of keys") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var i, tab;
      |tab = [];
      |for (i in {x:1, y:2}) {
      |  tab.push(i);
      |}
      |tab.toString();
      |""".stripMargin)

    assertEquals(result, JSValue.fromString("x,y"))
  }

  test("__forInKeys helper") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |__forInKeys({x:1, y:2}).toString();
      |""".stripMargin)

    assertEquals(result, JSValue.fromString("x,y"))
  }

  test("for-in prototype chain order") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var i, tab, a, b;
      |a = {x:2, y: 2, "1": 3};
      |b = {"4" : 3 };
      |Object.setPrototypeOf(a, b);
      |tab = [];
      |for(i in a) {
      |  tab.push(i);
      |}
      |tab.toString();
      |""".stripMargin)

    assertEquals(result, JSValue.fromString("1,x,y,4"))
  }

  test("for-in non-enumerable hides prototype") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var i, tab, a, b;
      |a = {y: 2, "1": 3};
      |Object.defineProperty(a, "x", { value: 1 });
      |b = {"x" : 3 };
      |Object.setPrototypeOf(a, b);
      |tab = [];
      |for(i in a) {
      |  tab.push(i);
      |}
      |tab.toString();
      |""".stripMargin)

    assertEquals(result, JSValue.fromString("1,y"))
  }

  test("for-in array indices") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var i, tab, a;
      |a = [];
      |for(i = 0; i < 10; i++)
      |  a.push(i);
      |tab = [];
      |for(i in a) {
      |  tab.push(i);
      |}
      |tab.toString();
      |""".stripMargin)

    assertEquals(result, JSValue.fromString("0,1,2,3,4,5,6,7,8,9"))
  }

  test("for-in proxy enumerable filtering") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |let removed_key = "";
      |let target = {};
      |let proxy = new Proxy(target, {
      |  ownKeys: function() {
      |    return ["a", "b", "c"];
      |  },
      |  getOwnPropertyDescriptor: function(target, key) {
      |    if (removed_key != "" && key == removed_key)
      |      return undefined;
      |    else
      |      return { enumerable: true, configurable: true, value: this[key] };
      |  }
      |});
      |let str = "";
      |for (let o in proxy) {
      |  str += " " + o;
      |  if (o == "a")
      |    removed_key = "b";
      |}
      |str;
      |""".stripMargin)

    assertEquals(result, JSValue.fromString(" a c"))
  }

  test("spread array toString") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var x = [1, 2, ...[3, 4]];
      |x.toString();
      |""".stripMargin)

    assertEquals(result, JSValue.fromString("1,2,3,4"))
  }

  test("spread array getOwnPropertyNames") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var x = [ ...[ , ] ];
      |Object.getOwnPropertyNames(x).toString();
      |""".stripMargin)

    assertEquals(result, JSValue.fromString("0,length"))
  }

  test("bigint literal toString") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("1n.toString();")
    assertEquals(result, JSValue.fromString("1"))
  }

  test("while loop increments") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |var i = 0;
      |var c = 0;
      |while (i < 3) {
      |  c++;
      |  i++;
      |}
      |c;
      |""".stripMargin)

    assertEquals(result, JSValue.fromInt(3))
  }

  test("assert helper with while loop") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val result = eval("""
      |function assert(actual, expected, message) {
      |  if (arguments.length == 1)
      |    expected = true;
      |  if (actual === expected)
      |    return;
      |  if (actual !== null && expected !== null
      |  && typeof actual == 'object' && typeof expected == 'object'
      |  && actual.toString() === expected.toString())
      |    return;
      |  throw Error("assertion failed");
      |}
      |var i = 0;
      |var c = 0;
      |while (i < 3) {
      |  c++;
      |  i++;
      |}
      |assert(c === 3);
      |"ok";
      |""".stripMargin)

    assertEquals(result, JSValue.fromString("ok"))
  }
