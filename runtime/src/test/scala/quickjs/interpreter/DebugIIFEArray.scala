package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.runtime.StdLib
import quickjs.value.JSValue
import munit.*

class DebugIIFEArray extends FunSuite {

  test("debug IIFE with Array.push") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    // Test 1: Push function inside IIFE
    val source1 = """
      |(function() {
      |  var funcs = [];
      |  funcs.push(function() { return 42; });
      |  return funcs.length;
      |})()
      |""".stripMargin
    val lexer1 = Lexer(source1)
    val parser1 = Parser(lexer1.tokenize())
    val ast1 = parser1.parseScript()
    val compiler1 = Compiler()
    val bytecode1 = compiler1.compileScript(ast1)
    val interpreter1 = Interpreter()
    val result1 = interpreter1.call(bytecode1, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 1: Push function inside IIFE - check length ===")
    println(s"Result: $result1")
    println(s"Expected: 1")

    // Test 2: Get element from inside IIFE
    val source2 = """
      |(function() {
      |  var funcs = [];
      |  funcs.push(function() { return 42; });
      |  return funcs[0];
      |})()
      |""".stripMargin
    val lexer2 = Lexer(source2)
    val parser2 = Parser(lexer2.tokenize())
    val ast2 = parser2.parseScript()
    val compiler2 = Compiler()
    val bytecode2 = compiler2.compileScript(ast2)
    val interpreter2 = Interpreter()
    val result2 = interpreter2.call(bytecode2, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 2: Get element from inside IIFE ===")
    println(s"Result: $result2")
    println(s"Result type: ${result2.getClass}")

    // Test 3: Call the function
    val source3 = """
      |(function() {
      |  var funcs = [];
      |  funcs.push(function() { return 42; });
      |  return funcs[0]();
      |})()
      |""".stripMargin
    val lexer3 = Lexer(source3)
    val parser3 = Parser(lexer3.tokenize())
    val ast3 = parser3.parseScript()
    val compiler3 = Compiler()
    val bytecode3 = compiler3.compileScript(ast3)
    val interpreter3 = Interpreter()
    val result3 = interpreter3.call(bytecode3, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 3: Call the function ===")
    println(s"Result: $result3")
    println(s"Expected: 42")
  }
}
