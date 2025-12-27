package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class ArrayTraceTest extends FunSuite:

  test("trace: step through array creation and storage") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Test 1: Just create array (no variable)
    val source1 = "[1, 2, 3]"
    val lexer1 = Lexer(source1)
    val tokens1 = lexer1.tokenize()
    val parser1 = Parser(tokens1)
    val ast1 = parser1.parseScript()
    val compiler1 = Compiler()
    val bytecode1 = compiler1.withREPLMode { compiler1.compileScript(ast1) }

    val interpreter1 = Interpreter()
    val result1 = interpreter1.call(bytecode1, JSValue.Undefined, Array.empty)

    println(s"Test 1 - Array literal only:")
    println(s"  Result: $result1")
    println(s"  Result type: ${result1.getClass.getSimpleName}")
    println(s"  Result tag: ${result1.tag}")
    result1 match
      case JSValue.JSArrayVal(arr) =>
        println(s"  Array length: ${arr.length}")
        println(s"  Array contents: ${arr}")
        println(s"  Element 0: ${arr.get(0)}")
        println(s"  Element 1: ${arr.get(1)}")
        println(s"  Element 2: ${arr.get(2)}")
      case _ =>
        println(s"  NOT AN ARRAY!")

    // Test 2: Check if global scope has the array
    println(s"\nTest 2 - Check global scope:")
    val globalVar = summon[JSContext].globalScope.getVariable("arr")
    println(s"  Global 'arr': $globalVar")

    // Test 3: Variable declaration with array
    val source2 = "var x = [10, 20, 30]"
    val lexer2 = Lexer(source2)
    val tokens2 = lexer2.tokenize()
    val parser2 = Parser(tokens2)
    val ast2 = parser2.parseScript()
    val compiler2 = Compiler()
    val bytecode2 = compiler2.compileScript(ast2)

    val interpreter2 = Interpreter()
    interpreter2.call(bytecode2, JSValue.Undefined, Array.empty)

    println(s"\nTest 3 - After var x = [10, 20, 30]:")
    val xVar = summon[JSContext].globalScope.getVariable("x")
    println(s"  Global 'x': $xVar")
    xVar match
      case Some(JSValue.JSArrayVal(arr)) =>
        println(s"  x is array! Length: ${arr.length}")
        println(s"  x[0]: ${arr.get(0)}")
      case Some(v) =>
        println(s"  x is NOT array, it's: ${v.getClass.getSimpleName}")
      case None =>
        println(s"  x not found in global scope")
  }
