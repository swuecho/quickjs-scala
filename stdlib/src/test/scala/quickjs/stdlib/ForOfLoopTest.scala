package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class ForOfLoopTest extends FunSuite:

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

  test("basic for-of with array") {
    val result = eval("""
      |var arr = [1, 2, 3];
      |var sum = 0;
      |for (var x of arr) {
      |  sum = sum + x;
      |}
      |sum;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(6))
  }

  test("for-of with const declaration") {
    val result = eval("""
      |var arr = [10, 20, 30];
      |var total = 0;
      |for (const val of arr) {
      |  total = total + val;
      |}
      |total;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(60))
  }

  test("for-of with let declaration") {
    val result = eval("""
      |var arr = [5, 10, 15];
      |var result = 0;
      |for (let n of arr) {
      |  result = result + n;
      |}
      |result;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(30))
  }

  test("for-of with string iterates characters") {
    val result = eval("""
      |var str = "abc";
      |var chars = [];
      |for (var c of str) {
      |  chars.push(c);
      |}
      |chars;
      |""".stripMargin)
    result match
      case arr: JSValue.JSArrayVal =>
        assertEquals(arr.value.get(0), JSValue.JSStr("a"))
        assertEquals(arr.value.get(1), JSValue.JSStr("b"))
        assertEquals(arr.value.get(2), JSValue.JSStr("c"))
      case _ => fail("Expected array")
  }

  test("for-of with break") {
    val result = eval("""
      |var arr = [1, 2, 3, 4, 5];
      |var sum = 0;
      |for (var x of arr) {
      |  if (x > 3) break;
      |  sum = sum + x;
      |}
      |sum;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(6))  // 1 + 2 + 3
  }

  test("for-of with continue") {
    val result = eval("""
      |var arr = [1, 2, 3, 4, 5];
      |var sum = 0;
      |for (var x of arr) {
      |  if (x % 2 === 0) continue;
      |  sum = sum + x;
      |}
      |sum;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(9))  // 1 + 3 + 5
  }

  test("nested for-of loops") {
    val result = eval("""
      |var arr1 = [1, 2];
      |var arr2 = [10, 20];
      |var sum = 0;
      |for (var a of arr1) {
      |  for (var b of arr2) {
      |    sum = sum + a * b;
      |  }
      |}
      |sum;
      |""".stripMargin)
    // (1*10 + 1*20) + (2*10 + 2*20) = 30 + 60 = 90
    assertEquals(result, JSValue.Int32(90))
  }

  test("labeled for-of with break") {
    val result = eval("""
      |var arr1 = [1, 2, 3];
      |var arr2 = [10, 20, 30];
      |var sum = 0;
      |outer: for (var a of arr1) {
      |  for (var b of arr2) {
      |    if (a === 2 && b === 20) break outer;
      |    sum = sum + 1;
      |  }
      |}
      |sum;
      |""".stripMargin)
    // First outer iteration: 3 inner iterations (b=10,20,30)
    // Second outer iteration: 1 inner iteration (b=10), then break outer at b=20
    assertEquals(result, JSValue.Int32(4))
  }

  test("labeled for-of with continue") {
    val result = eval("""
      |var arr1 = [1, 2, 3];
      |var arr2 = [10, 20];
      |var sum = 0;
      |outer: for (var a of arr1) {
      |  for (var b of arr2) {
      |    if (a === 2) continue outer;
      |    sum = sum + 1;
      |  }
      |}
      |sum;
      |""".stripMargin)
    // a=1: b=10 (sum++), b=20 (sum++) = 2
    // a=2: b=10 -> continue outer, skip rest
    // a=3: b=10 (sum++), b=20 (sum++) = 2
    assertEquals(result, JSValue.Int32(4))
  }

  test("for-of with empty array") {
    val result = eval("""
      |var arr = [];
      |var count = 0;
      |for (var x of arr) {
      |  count = count + 1;
      |}
      |count;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(0))
  }

  test("for-of with array of objects") {
    val result = eval("""
      |var arr = [{v: 1}, {v: 2}, {v: 3}];
      |var sum = 0;
      |for (var obj of arr) {
      |  sum = sum + obj.v;
      |}
      |sum;
      |""".stripMargin)
    assertEquals(result, JSValue.Int32(6))
  }

  test("for-of preserves array order") {
    val result = eval("""
      |var arr = [3, 1, 4, 1, 5];
      |var result = [];
      |for (var x of arr) {
      |  result.push(x);
      |}
      |result;
      |""".stripMargin)
    result match
      case arr: JSValue.JSArrayVal =>
        assertEquals(arr.value.get(0), JSValue.Int32(3))
        assertEquals(arr.value.get(1), JSValue.Int32(1))
        assertEquals(arr.value.get(2), JSValue.Int32(4))
        assertEquals(arr.value.get(3), JSValue.Int32(1))
        assertEquals(arr.value.get(4), JSValue.Int32(5))
      case _ => fail("Expected array")
  }
