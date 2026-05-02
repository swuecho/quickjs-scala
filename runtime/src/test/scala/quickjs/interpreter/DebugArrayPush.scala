package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.runtime.StdLib
import quickjs.value.JSValue
import munit.*

class DebugArrayPush extends FunSuite {

  test("debug Array.push") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    // Test 1: Simple push
    val source1 = """
      |var arr = [];
      |arr.push(42);
      |arr.length
      |""".stripMargin
    val lexer1 = Lexer(source1)
    val parser1 = Parser(lexer1.tokenize())
    val ast1 = parser1.parseScript()
    val compiler1 = Compiler()
    val bytecode1 = compiler1.withREPLMode(compiler1.compileScript(ast1))
    val interpreter1 = Interpreter()
    val result1 = interpreter1.call(bytecode1, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 1: Simple push ===")
    println(s"Result: $result1")
    println(s"Expected: 1")

    // Test 2: Push function
    val source2 = """
      |var arr = [];
      |arr.push(function() { return 42; });
      |arr.length
      |""".stripMargin
    val lexer2 = Lexer(source2)
    val parser2 = Parser(lexer2.tokenize())
    val ast2 = parser2.parseScript()
    val compiler2 = Compiler()
    val bytecode2 = compiler2.withREPLMode(compiler2.compileScript(ast2))
    val interpreter2 = Interpreter()
    val result2 = interpreter2.call(bytecode2, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 2: Push function ===")
    println(s"Result: $result2")
    println(s"Expected: 1")

    // Test 3: Get element back
    val source3 = """
      |var arr = [];
      |arr.push(function() { return 42; });
      |arr[0]
      |""".stripMargin
    val lexer3 = Lexer(source3)
    val parser3 = Parser(lexer3.tokenize())
    val ast3 = parser3.parseScript()
    val compiler3 = Compiler()
    val bytecode3 = compiler3.withREPLMode(compiler3.compileScript(ast3))
    val interpreter3 = Interpreter()
    val result3 = interpreter3.call(bytecode3, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 3: Get element back ===")
    println(s"Result: $result3")
    println(s"Result type: ${result3.getClass}")
  }
}
