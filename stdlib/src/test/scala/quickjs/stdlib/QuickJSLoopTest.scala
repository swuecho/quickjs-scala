package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

/** Port of QuickJS C test suite - test_loop.js
  *
  * These tests are adapted from the official QuickJS test suite
  * to validate loop control flow.
  *
  * Source: /home/hwu/dev/quickjs/tests/test_loop.js
  */
class QuickJSLoopTest extends FunSuite:

  /** Helper to evaluate code and return result */
  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  /** Helper to assert actual equals expected */
  private def assertJS(actual: JSValue, expected: JSValue, hint: String = "")(using JSContext): Unit =
    if actual != expected then
      val msg = if hint.nonEmpty then s" ($hint)" else ""
      fail(s"assertion failed: got |$actual|, expected |$expected|$msg")

  // ==================== test_while() ====================

  test("test_while: basic while loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i = 0;
      |  var c = 0;
      |  while (i < 3) {
      |    c++;
      |    i++;
      |  }
      |  return c;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(3), "while loop executed 3 times")
  }

  test("test_while: while loop with zero iterations") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i = 0;
      |  var c = 0;
      |  while (i < 0) {
      |    c++;
      |    i++;
      |  }
      |  return c;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(0), "while loop executed 0 times")
  }

  // ==================== test_while_break() ====================

  test("test_while_break: while loop with break") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i = 0;
      |  var c = 0;
      |  while (i < 3) {
      |    c++;
      |    if (i == 1)
      |      break;
      |    i++;
      |  }
      |  return { c: c, i: i };
      |})()
      |""".stripMargin)

    result match
      case JSValue.Object(obj) =>
        assertJS(obj.get("c"), JSValue.fromInt(2), "c === 2")
        assertJS(obj.get("i"), JSValue.fromInt(1), "i === 1")
      case _ =>
        fail(s"Expected object but got $result")
  }

  // ==================== test_do_while() ====================

  test("test_do_while: basic do-while loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i = 0;
      |  var c = 0;
      |  do {
      |    c++;
      |    i++;
      |  } while (i < 3);
      |  return { c: c, i: i };
      |})()
      |""".stripMargin)

    result match
      case JSValue.Object(obj) =>
        assertJS(obj.get("c"), JSValue.fromInt(3), "c === 3")
        assertJS(obj.get("i"), JSValue.fromInt(3), "i === 3")
      case _ =>
        fail(s"Expected object but got $result")
  }

  test("test_do_while: do-while with zero iterations") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i = 0;
      |  var c = 0;
      |  do {
      |    c++;
      |    i++;
      |  } while (i < 0);
      |  return { c: c, i: i };
      |})()
      |""".stripMargin)

    result match
      case JSValue.Object(obj) =>
        assertJS(obj.get("c"), JSValue.fromInt(1), "c === 1 (executed once)")
        assertJS(obj.get("i"), JSValue.fromInt(1), "i === 1")
      case _ =>
        fail(s"Expected object but got $result")
  }

  // ==================== test_for() ====================

  test("test_for: basic for loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var c = 0;
      |  for(var i = 0; i < 3; i++) {
      |    c++;
      |  }
      |  return { c: c, i: i };
      |})()
      |""".stripMargin)

    result match
      case JSValue.Object(obj) =>
        assertJS(obj.get("c"), JSValue.fromInt(3), "c === 3")
        assertJS(obj.get("i"), JSValue.fromInt(3), "i === 3")
      case _ =>
        fail(s"Expected object but got $result")
  }

  test("test_for: for loop with existing variable") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i;
      |  var c = 0;
      |  for(i = 0; i < 3; i++) {
      |    c++;
      |  }
      |  return { c: c, i: i };
      |})()
      |""".stripMargin)

    result match
      case JSValue.Object(obj) =>
        assertJS(obj.get("c"), JSValue.fromInt(3), "c === 3")
        assertJS(obj.get("i"), JSValue.fromInt(3), "i === 3")
      case _ =>
        fail(s"Expected object but got $result")
  }

  test("test_for: for loop with empty body") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i;
      |  for(i = 0; i < 10; i++) {}
      |  return i;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(10), "i === 10")
  }

  // ==================== test_for_break() ====================

  test("test_for_break: for loop with continue") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var c = 0;
      |  for(var i = 0; i < 3; i++) {
      |    c++;
      |    if (i == 0)
      |      continue;
      |    // No break, so loop continues normally
      |  }
      |  return { c: c, i: i };
      |})()
      |""".stripMargin)

    result match
      case JSValue.Object(obj) =>
        assertJS(obj.get("c"), JSValue.fromInt(3), "c === 3")
        assertJS(obj.get("i"), JSValue.fromInt(3), "i === 3")
      case _ =>
        fail(s"Expected object but got $result")
  }

  test("test_for_break: labeled break with nested loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i, c;
      |  c = 0;
      |  L1: for(i = 0; i < 3; i++) {
      |    c++;
      |    if (i == 0)
      |      continue;
      |    while (1) {
      |      break L1;
      |    }
      |  }
      |  return { c: c, i: i };
      |})()
      |""".stripMargin)

    result match
      case JSValue.Object(obj) =>
        assertJS(obj.get("c"), JSValue.fromInt(2), "c === 2")
        assertJS(obj.get("i"), JSValue.fromInt(1), "i === 1")
      case _ =>
        fail(s"Expected object but got $result")
  }

  // ==================== test_switch1() ====================

  test("test_switch1: basic switch statement") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i, a, s;
      |  s = "";
      |  for(i = 0; i < 3; i++) {
      |    a = "?";
      |    switch(i) {
      |    case 0:
      |      a = "a";
      |      break;
      |    case 1:
      |      a = "b";
      |      break;
      |    default:
      |      a = "c";
      |      break;
      |    }
      |    s += a;
      |  }
      |  return { s: s, i: i };
      |})()
      |""".stripMargin)

    result match
      case JSValue.Object(obj) =>
        assertJS(obj.get("s"), JSValue.fromString("abc"), "s === 'abc'")
        assertJS(obj.get("i"), JSValue.fromInt(3), "i === 3")
      case _ =>
        fail(s"Expected object but got $result")
  }

  test("test_switch1: switch with fallthrough (no break)") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var a;
      |  switch(1) {
      |  case 0:
      |    a = "zero";
      |  case 1:
      |    a = "one";
      |  default:
      |    a = "default";
      |  }
      |  return a;
      |})()
      |""".stripMargin)

    // Note: Fallthrough behavior - should execute case 1 and then default
    assertJS(result, JSValue.fromString("default"), "fallthrough to default")
  }

  // ==================== test_switch2() ====================

  test("test_switch2: switch with continue in loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i, a, s;
      |  s = "";
      |  for(i = 0; i < 4; i++) {
      |    a = "?";
      |    switch(i) {
      |    case 0:
      |      a = "a";
      |      break;
      |    case 1:
      |      a = "b";
      |      break;
      |    case 2:
      |      continue;
      |    default:
      |      a = "" + i;
      |      break;
      |    }
      |    s += a;
      |  }
      |  return { s: s, i: i };
      |})()
      |""".stripMargin)

    result match
      case JSValue.Object(obj) =>
        assertJS(obj.get("s"), JSValue.fromString("ab3"), "s === 'ab3'")
        assertJS(obj.get("i"), JSValue.fromInt(4), "i === 4")
      case _ =>
        fail(s"Expected object but got $result")
  }

  // ==================== Nested Loops ====================

  test("nested loops: double for loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var sum = 0;
      |  for (var i = 0; i < 3; i++) {
      |    for (var j = 0; j < 3; j++) {
      |      sum++;
      |    }
      |  }
      |  return sum;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(9), "3 * 3 = 9")
  }

  test("nested loops: break in inner loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var sum = 0;
      |  for (var i = 0; i < 3; i++) {
      |    for (var j = 0; j < 3; j++) {
      |      sum++;
      |      if (j == 1)
      |        break;
      |    }
      |  }
      |  return sum;
      |})()
      |""".stripMargin)

    // Each inner loop runs 2 times (j=0, j=1), so 3 * 2 = 6
    assertJS(result, JSValue.fromInt(6), "3 * 2 = 6")
  }

  test("nested loops: break in outer loop from inner") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var sum = 0;
      |  outer: for (var i = 0; i < 3; i++) {
      |    for (var j = 0; j < 3; j++) {
      |      sum++;
      |      if (i == 1 && j == 1)
      |        break outer;
      |    }
      |  }
      |  return sum;
      |})()
      |""".stripMargin)

    // i=0: j=0,1,2 (3); i=1: j=0,1 then break (2); total = 5
    assertJS(result, JSValue.fromInt(5), "break outer at (1,1)")
  }

  test("nested loops: continue in inner loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var sum = 0;
      |  for (var i = 0; i < 3; i++) {
      |    for (var j = 0; j < 3; j++) {
      |      if (j == 1)
      |        continue;
      |      sum++;
      |    }
      |  }
      |  return sum;
      |})()
      |""".stripMargin)

    // Each inner loop runs 2 times (j=0, j=2), so 3 * 2 = 6
    assertJS(result, JSValue.fromInt(6), "continue skips j=1")
  }

  // ==================== Loop with Condition Side Effects ====================

  test("while loop: condition with side effect") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i = 0;
      |  var c = 0;
      |  while (i++ < 3) {
      |    c++;
      |  }
      |  return { c: c, i: i };
      |})()
      |""".stripMargin)

    result match
      case JSValue.Object(obj) =>
        assertJS(obj.get("c"), JSValue.fromInt(3), "c === 3")
        assertJS(obj.get("i"), JSValue.fromInt(4), "i === 4")
      case _ =>
        fail(s"Expected object but got $result")
  }

  test("for loop: complex condition") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var sum = 0;
      |  for (var i = 0, j = 10; i < j && sum < 5; i++, j--) {
      |    sum += i;
      |  }
      |  return sum;
      |})()
      |""".stripMargin)

    // i=0, j=10: sum=0; i=1, j=9: sum=1; i=2, j=8: sum=3; i=3, j=7: sum=6 (stop because sum >= 5)
    assertJS(result, JSValue.fromInt(6), "sum stops at 6")
  }

  // ==================== Edge Cases ====================

  test("loop: return from inside loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  for (var i = 0; i < 10; i++) {
      |    if (i == 5)
      |      return i;
      |  }
      |  return -1;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(5), "returns 5")
  }

  test("loop: multiple breaks and continues") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var count = 0;
      |  for (var i = 0; i < 10; i++) {
      |    if (i < 3)
      |      continue;
      |    if (i > 6)
      |      break;
      |    count++;
      |  }
      |  return count;
      |})()
      |""".stripMargin)

    // Skips i=0,1,2; counts i=3,4,5,6; breaks at i=7
    assertJS(result, JSValue.fromInt(4), "count = 4")
  }

  test("loop: while(true) with break") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i = 0;
      |  while (true) {
      |    i++;
      |    if (i == 5)
      |      break;
      |  }
      |  return i;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(5), "i === 5")
  }

  test("loop: for with no initialization") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var i = 0;
      |  var c = 0;
      |  for (; i < 3; i++) {
      |    c++;
      |  }
      |  return c;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(3), "c === 3")
  }

  test("loop: for with no condition") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var c = 0;
      |  for (var i = 0; ; i++) {
      |    c++;
      |    if (i >= 2)
      |      break;
      |  }
      |  return c;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(3), "c === 3")
  }

  test("loop: for with no increment") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var c = 0;
      |  var i = 0;
      |  for (; i < 3; ) {
      |    c++;
      |    i++;
      |  }
      |  return c;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(3), "c === 3")
  }

  // ==================== test_try_catch*() ====================

  test("test_try_catch1: basic try/catch with throw") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  try {
      |    throw "hello";
      |  } catch (e) {
      |    return e === "hello";
      |  }
      |  return false;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromBoolean(true), "catch receives thrown value")
  }

  test("test_try_catch2: no exception in try") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var a;
      |  try {
      |    a = 1;
      |  } catch (e) {
      |    a = 2;
      |  }
      |  return a;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(1), "catch not executed")
  }

  test("test_try_catch2b: optional catch binding") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var s = "";
      |  try {
      |    throw "x";
      |  } catch {
      |    s += "c";
      |  }
      |  return s;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromString("c"), "catch without binding")
  }

  test("test_try_catch2c: catch destructuring") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  try {
      |    throw { a: 2, b: 3 };
      |  } catch ({ a, b }) {
      |    return a + b;
      |  }
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(5), "catch object pattern")
  }

  test("test_try_catch3: try/finally without throw") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var s = "";
      |  try {
      |    s += "t";
      |  } catch (e) {
      |    s += "c";
      |  } finally {
      |    s += "f";
      |  }
      |  return s;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromString("tf"), "finally executed")
  }

  test("test_try_catch4: try/catch/finally with throw") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var s = "";
      |  try {
      |    s += "t";
      |    throw "c";
      |  } catch (e) {
      |    s += e;
      |  } finally {
      |    s += "f";
      |  }
      |  return s;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromString("tcf"), "catch + finally")
  }

  test("test_try_catch5: finally runs on break") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var s = "";
      |  for(;;) {
      |    try {
      |      s += "t";
      |      break;
      |      s += "b";
      |    } finally {
      |      s += "f";
      |    }
      |  }
      |  return s;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromString("tf"), "finally runs on break")
  }

  test("test_try_catch6: finally runs on return") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  function f() {
      |    try {
      |      s += "t";
      |      return 1;
      |    } finally {
      |      s += "f";
      |    }
      |  }
      |  var s = "";
      |  var r = f();
      |  return r === 1 && s === "tf";
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromBoolean(true), "finally runs on return")
  }

  test("test_try_catch7: nested try/finally with throw") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var s = "";
      |  try {
      |    try {
      |      s += "t";
      |      throw "a";
      |    } finally {
      |      s += "f";
      |    }
      |  } catch(e) {
      |    s += e;
      |  } finally {
      |    s += "g";
      |  }
      |  return s;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromString("tfag"), "nested finally + catch")
  }

  test("test_try_catch8: try/catch/finally in for-in") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result = eval("""
      |(function() {
      |  var s = "";
      |  for (var i in {x:1, y:2}) {
      |    try {
      |      s += i;
      |      throw "a";
      |    } catch (e) {
      |      s += e;
      |    } finally {
      |      s += "f";
      |    }
      |  }
      |  return s;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromString("xafyaf"), "for-in finally order")
  }
