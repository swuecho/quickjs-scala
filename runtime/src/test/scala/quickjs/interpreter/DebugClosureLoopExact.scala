package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.runtime.StdLib
import quickjs.value.JSValue
import munit.*

class DebugClosureLoopExact extends FunSuite {

  test("debug closure loop - exact failing code") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    // Test 1: Single iteration
    val source1 = """
      |(function() {
      |  var funcs = [];
      |  for (var i = 0; i < 1; i++) {
      |    (function(j) {
      |      funcs.push(function() { return j; });
      |    })(i);
      |  }
      |  return funcs[0]();
      |})()
      |""".stripMargin
    val lexer1 = Lexer(source1)
    val parser1 = Parser(lexer1.tokenize())
    val ast1 = parser1.parseScript()
    val compiler1 = Compiler()
    val bytecode1 = compiler1.compileScript(ast1)
    val interpreter1 = Interpreter()
    val result1 = interpreter1.call(bytecode1, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 1: Single iteration ===")
    println(s"Result: $result1")
    println(s"Expected: 0")

    // Test 2: Check what's in the array
    val source2 = """
      |(function() {
      |  var funcs = [];
      |  for (var i = 0; i < 1; i++) {
      |    (function(j) {
      |      funcs.push(function() { return j; });
      |    })(i);
      |  }
      |  return funcs;
      |})()
      |""".stripMargin
    val lexer2 = Lexer(source2)
    val parser2 = Parser(lexer2.tokenize())
    val ast2 = parser2.parseScript()
    val compiler2 = Compiler()
    val bytecode2 = compiler2.compileScript(ast2)
    val interpreter2 = Interpreter()
    val result2 = interpreter2.call(bytecode2, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 2: Return the array ===")
    println(s"Result: $result2")
    println(s"Result type: ${result2.getClass}")

    result2 match {
      case arrVal: JSValue.JSArrayVal =>
        val arr = arrVal.value
        println(s"Array length: ${arr.getLength}")
        println(s"First element: ${arr.get(0)}")
        println(s"First element type: ${arr.get(0).getClass}")
      case other =>
        println(s"Not an array: $other")
    }

    // Test 3: Two iterations
    val source3 = """
      |(function() {
      |  var funcs = [];
      |  for (var i = 0; i < 2; i++) {
      |    (function(j) {
      |      funcs.push(function() { return j; });
      |    })(i);
      |  }
      |  return funcs[0]() + funcs[1]();
      |})()
      |""".stripMargin
    val lexer3 = Lexer(source3)
    val parser3 = Parser(lexer3.tokenize())
    val ast3 = parser3.parseScript()
    val compiler3 = Compiler()
    val bytecode3 = compiler3.compileScript(ast3)
    val interpreter3 = Interpreter()
    val result3 = interpreter3.call(bytecode3, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 3: Two iterations ===")
    println(s"Result: $result3")
    println(s"Expected: 1")

    // Test 4: The full test
    val source4 = """
      |(function() {
      |  var funcs = [];
      |  for (var i = 0; i < 3; i++) {
      |    (function(j) {
      |      funcs.push(function() { return j; });
      |    })(i);
      |  }
      |  return funcs[0]() + funcs[1]() + funcs[2]();
      |})()
      |""".stripMargin
    val lexer4 = Lexer(source4)
    val parser4 = Parser(lexer4.tokenize())
    val ast4 = parser4.parseScript()
    val compiler4 = Compiler()
    val bytecode4 = compiler4.compileScript(ast4)
    val interpreter4 = Interpreter()
    val result4 = interpreter4.call(bytecode4, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 4: Full test (3 iterations) ===")
    println(s"Result: $result4")
    println(s"Expected: 3")
  }
}
