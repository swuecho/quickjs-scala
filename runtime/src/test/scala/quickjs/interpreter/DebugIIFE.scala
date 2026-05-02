package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class DebugIIFE extends FunSuite {

  test("debug IIFE - step by step") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // First test: Can we create a function expression?
    val source1 = "(function() { return 42; })"
    val lexer1 = Lexer(source1)
    val parser1 = Parser(lexer1.tokenize())
    val ast1 = parser1.parseScript()
    val compiler1 = Compiler()
    val bytecode1 = compiler1.withREPLMode(compiler1.compileScript(ast1))
    val interpreter1 = Interpreter()
    val result1 = interpreter1.call(bytecode1, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 1: Function expression only ===")
    println(s"Source: $source1")
    println(s"Result: $result1")
    println(s"Result type: ${result1.getClass}")

    result1 match {
      case func @ JSValue.Function(
            _,
            name,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        println(s"✓ Function created! Name: $name")
        println(s"Function details: $func")
      case other =>
        println(s"✗ Not a function! Got: $other")
    }

    // Second test: Can we call a stored function?
    val source2 = "var f = function() { return 42; }; f"
    val lexer2 = Lexer(source2)
    val parser2 = Parser(lexer2.tokenize())
    val ast2 = parser2.parseScript()
    val compiler2 = Compiler()
    val bytecode2 = compiler2.withREPLMode(compiler2.compileScript(ast2))
    val interpreter2 = Interpreter()
    val result2 = interpreter2.call(bytecode2, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 2: Function stored in variable ===")
    println(s"Source: $source2")
    println(s"Result: $result2")

    result2 match {
      case func @ JSValue.Function(
            _,
            name,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        println(s"✓ Function retrieved! Name: $name")
        println(s"Function details: $func")
      case other =>
        println(s"✗ Not a function! Got: $other")
    }

    // Third test: Full IIFE
    val source3 = "(function() { return 42; })()"
    val lexer3 = Lexer(source3)
    val parser3 = Parser(lexer3.tokenize())
    val ast3 = parser3.parseScript()
    val compiler3 = Compiler()
    val bytecode3 = compiler3.compileScript(ast3)
    val interpreter3 = Interpreter()
    val result3 = interpreter3.call(bytecode3, JSValue.Undefined, Array.empty)

    println(s"\n=== Test 3: Full IIFE ===")
    println(s"Source: $source3")
    println(s"Result: $result3")
  }
}
