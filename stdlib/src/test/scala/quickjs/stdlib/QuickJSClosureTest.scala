package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

/** Port of QuickJS C test suite - test_closure.js
  *
  * These tests are adapted from the official QuickJS test suite
  * to validate closure behavior.
  *
  * Source: /home/hwu/dev/quickjs/tests/test_closure.js
  */
class QuickJSClosureTest extends FunSuite:

  /** Helper to create an initialized JSContext */
  private def createContext(): JSContext =
    val runtime = JSRuntime()
    val ctx = JSContext(runtime)
    StdLib.initialize(ctx)
    ctx

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

  // ==================== test_closure1() ====================

  test("test_closure1: nested function closure") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Note: The original test uses a log() function, which we don't have yet
    // Instead, we'll test a simpler version that just verifies the closure works
    val result = eval("""
      |(function() {
      |  var x = 10;
      |  function g(d) {
      |    function h() {
      |      return d + x;
      |    }
      |    return h();
      |  }
      |  return g(4);
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(14), "4 + 10 = 14")
  }

  // ==================== test_closure2() ====================

  test("test_closure2: object with closure methods") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var val = 1;
      |  function set(a) {
      |    val = a;
      |  }
      |  function get(a) {
      |    return val;
      |  }
      |  var obj = { "set": set, "get": get };
      |  obj.set(10);
      |  return obj.get();
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(10), "closure2")
  }

  test("test_closure2: independent closures") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Test that multiple closures don't share state
    val result1 = eval("""
      |(function() {
      |  var val = 1;
      |  return function() { return val; };
      |})()()
      |""".stripMargin)

    assertJS(result1, JSValue.fromInt(1), "first closure")

    val result2 = eval("""
      |(function() {
      |  var val = 2;
      |  return function() { return val; };
      |})()()
      |""".stripMargin)

    assertJS(result2, JSValue.fromInt(2), "second closure")
  }

  // ==================== test_closure3() - Recursive Functions ====================

  test("test_closure3: fibonacci with named function") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  function fib(n) {
      |    if (n <= 0)
      |      return 0;
      |    else if (n == 1)
      |      return 1;
      |    else
      |      return fib(n - 1) + fib(n - 2);
      |  }
      |  return fib(6);
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(8), "fib(6) = 8")
  }

  test("test_closure3: fibonacci with function expression") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Note: This test requires named function expressions with recursive calls
    // The current implementation may not fully support this
    val result = eval("""
      |(function() {
      |  var fib_func = function fib1(n) {
      |    if (n <= 0)
      |      return 0;
      |    else if (n == 1)
      |      return 1;
      |    else
      |      return fib1(n - 1) + fib1(n - 2);
      |  };
      |  return fib_func(6);
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(8), "fib_func(6) = 8")
  }

  // ==================== Multiple Closure Scenarios ====================

  test("closure: returns adder function") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    eval("function makeAdder(n) { return function(x) { return x + n; }; }")
    eval("var add5 = makeAdder(5)")
    assertJS(eval("add5(10)"), JSValue.fromInt(15), "add5(10) = 15")
  }

  test("closure: multiple closures share parent scope") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var count = 0;
      |  function inc() { return ++count; }
      |  function dec() { return --count; }
      |  inc();
      |  inc();
      |  dec();
      |  return count;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(1), "count after inc, inc, dec")
  }

  test("closure: closure preserves state across calls") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // First call: initialize counter
    eval("var counter = (function() { var n = 0; return function() { n++; return n; }; })();")
    assertJS(eval("counter()"), JSValue.fromInt(1), "first call")
    assertJS(eval("counter()"), JSValue.fromInt(2), "second call")
    assertJS(eval("counter()"), JSValue.fromInt(3), "third call")
  }

  test("closure: nested closures with different lifetimes") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var x = 10;
      |  var f1 = function() { return x; };
      |  var f2 = function() { return x * 2; };
      |  return f1() + f2();
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(30), "f1() + f2() = 10 + 20")
  }

  test("closure: closure with loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Create multiple closures in a loop
    val result = eval("""
      |(function() {
      |  var funcs = [];
      |  for (var i = 0; i < 3; i++) {
      |    (function(j) {
      |      funcs.push(function() { return j; });
      |    })(i);
      |  }
      |  return funcs[0]() + funcs[1]() + funcs[2]();
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(3), "sum of closures = 0 + 1 + 2")
  }

  test("closure: closure modifies captured variable") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var sum = 0;
      |  function add(n) { sum += n; return sum; }
      |  add(5);
      |  add(3);
      |  add(2);
      |  return sum;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(10), "sum after adding 5, 3, 2")
  }

  test("closure: returning closure from method") {
    given JSContext = createContext()

    val result = eval("""
      |(function() {
      |  var obj = {
      |    x: 10,
      |    getX: function() {
      |      return function() { return this.x; };
      |    }
      |  };
      |  var getter = obj.getX();
      |  return getter.call(obj);
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(10), "getX() returns 10")
  }

  // ==================== Advanced Closure Scenarios ====================

  test("closure: mutual recursion via closures") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  function isEven(n) {
      |    if (n === 0) return true;
      |    return function() { return isOdd(n - 1); }();
      |  }
      |  function isOdd(n) {
      |    if (n === 0) return false;
      |    return function() { return isEven(n - 1); }();
      |  }
      |  return isEven(4);
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromBoolean(true), "isEven(4)")
  }

  test("closure: factory function with private state") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    eval("""
      |function createCounter() {
      |  var count = 0;
      |  return {
      |    increment: function() { count++; return count; },
      |    decrement: function() { count--; return count; },
      |    getValue: function() { return count; }
      |  };
      |}
      |""".stripMargin)

    eval("var counter1 = createCounter()")
    eval("var counter2 = createCounter()")

    assertJS(eval("counter1.increment()"), JSValue.fromInt(1), "counter1 first increment")
    assertJS(eval("counter1.increment()"), JSValue.fromInt(2), "counter1 second increment")
    assertJS(eval("counter2.increment()"), JSValue.fromInt(1), "counter2 first increment (independent)")
    assertJS(eval("counter1.getValue()"), JSValue.fromInt(2), "counter1 still at 2")
    assertJS(eval("counter2.getValue()"), JSValue.fromInt(1), "counter2 still at 1")
  }

  test("closure: closure with object property access") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var obj = { x: 10, y: 20 };
      |  return function(prop) { return obj[prop]; };
      |})()("x")
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(10), "obj['x'] = 10")
  }

  // ==================== Closure with Arrays ====================

  test("closure: array of closures") {
    given JSContext = createContext()

    val result = eval("""
      |(function() {
      |  var arr = [1, 2, 3];
      |  var funcs = arr.map(function(x) {
      |    return function() { return x * 2; };
      |  });
      |  return funcs[0]() + funcs[1]() + funcs[2]();
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(12), "2 + 4 + 6 = 12")
  }

  test("closure: closure captures array by reference") {
    given JSContext = createContext()

    val result = eval("""
      |(function() {
      |  var arr = [1, 2, 3];
      |  function push(val) { arr.push(val); return arr.length; }
      |  push(4);
      |  push(5);
      |  return arr.length;
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(5), "array has 5 elements")
  }

  // ==================== Edge Cases ====================

  test("closure: empty closure") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  return function() { return 42; };
      |})()()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(42), "empty closure returns constant")
  }

  test("closure: deeply nested closures") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("""
      |(function() {
      |  var x = 1;
      |  return (function() {
      |    var y = 2;
      |    return (function() {
      |      var z = 3;
      |      return (function() {
      |        return x + y + z;
      |      })();
      |    })();
      |  })();
      |})()
      |""".stripMargin)

    assertJS(result, JSValue.fromInt(6), "1 + 2 + 3 = 6")
  }
