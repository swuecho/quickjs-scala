package quickjs.repl

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class REPLPersistenceTest extends FunSuite:

  test("Variables persist across evaluations") {
    given JSRuntime = JSRuntime()
    val ctx = JSContext(summon[JSRuntime])

    val interpreter = Interpreter()

    // First evaluation: declare a variable
    val source1 = "var x = 42;"
    val lexer1 = Lexer(source1)
    val tokens1 = lexer1.tokenize()
    val parser1 = Parser(tokens1)
    val ast1 = parser1.parseScript()
    val compiler1 = Compiler()
    val bytecode1 = compiler1.compileScript(ast1)
    interpreter.call(bytecode1, JSValue.Undefined, Array.empty)(using ctx)

    // Second evaluation: use the variable
    val source2 = "x + 8;"
    val lexer2 = Lexer(source2)
    val tokens2 = lexer2.tokenize()
    val parser2 = Parser(tokens2)
    val ast2 = parser2.parseScript()
    val compiler2 = Compiler()
    val bytecode2 = compiler2.compileScript(ast2)
    val result = interpreter.call(bytecode2, JSValue.Undefined, Array.empty)(using ctx)

    // Expression returns the computed value
    assertEquals(result, JSValue.fromInt(50))
  }

  test("Functions persist across evaluations") {
    given JSRuntime = JSRuntime()
    val ctx = JSContext(summon[JSRuntime])

    val interpreter = Interpreter()

    // First evaluation: declare a function
    val source1 = "function add(a, b) { return a + b; }"
    val lexer1 = Lexer(source1)
    val tokens1 = lexer1.tokenize()
    val parser1 = Parser(tokens1)
    val ast1 = parser1.parseScript()
    val compiler1 = Compiler()
    val bytecode1 = compiler1.compileScript(ast1)
    interpreter.call(bytecode1, JSValue.Undefined, Array.empty)(using ctx)

    // Second evaluation: call the function
    val source2 = "add(5, 7);"
    val lexer2 = Lexer(source2)
    val tokens2 = lexer2.tokenize()
    val parser2 = Parser(tokens2)
    val ast2 = parser2.parseScript()
    val compiler2 = Compiler()
    val bytecode2 = compiler2.compileScript(ast2)
    val result = interpreter.call(bytecode2, JSValue.Undefined, Array.empty)(using ctx)

    // Expression returns the computed value
    assertEquals(result, JSValue.fromInt(12))
  }

  test("Multiple evaluations build up state") {
    given JSRuntime = JSRuntime()
    val ctx = JSContext(summon[JSRuntime])

    val interpreter = Interpreter()

    // First: declare function
    eval("function fib(n) { if (n <= 1) return n; return fib(n-1) + fib(n-2); }", interpreter, ctx)

    // Second: call it
    eval("var result = fib(10);", interpreter, ctx)

    // Third: check the result by using it in an expression
    val source3 = "result + 1;"
    val lexer3 = Lexer(source3)
    val tokens3 = lexer3.tokenize()
    val parser3 = Parser(tokens3)
    val ast3 = parser3.parseScript()
    val compiler3 = Compiler()
    val bytecode3 = compiler3.compileScript(ast3)
    val result = interpreter.call(bytecode3, JSValue.Undefined, Array.empty)(using ctx)

    // fib(10) = 55, so result + 1 = 56
    assertEquals(result, JSValue.fromInt(56))
  }

  test("Functions can call other functions declared earlier") {
    given JSRuntime = JSRuntime()
    val ctx = JSContext(summon[JSRuntime])

    val interpreter = Interpreter()

    // First: declare helper function
    eval("function square(x) { return x * x; }", interpreter, ctx)

    // Second: declare function that uses the first
    eval("function sumOfSquares(a, b) { return square(a) + square(b); }", interpreter, ctx)

    // Third: call it
    val source3 = "sumOfSquares(3, 4);"
    val lexer3 = Lexer(source3)
    val tokens3 = lexer3.tokenize()
    val parser3 = Parser(tokens3)
    val ast3 = parser3.parseScript()
    val compiler3 = Compiler()
    val bytecode3 = compiler3.compileScript(ast3)
    val result = interpreter.call(bytecode3, JSValue.Undefined, Array.empty)(using ctx)

    // 3*3 + 4*4 = 25
    assertEquals(result, JSValue.fromInt(25))
  }

  private def eval(source: String, interpreter: Interpreter, ctx: JSContext): Unit =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)(using ctx)
