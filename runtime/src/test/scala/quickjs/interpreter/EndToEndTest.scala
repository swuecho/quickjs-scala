package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class EndToEndTest extends FunSuite:

  test("end-to-end: 1 + 2") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "1 + 2"

    // Tokenize
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()

    // Parse
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    // Compile
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // The result should be undefined because ExpressionStatement drops the result
    assertEquals(result, JSValue.Undefined)
  }

  test("end-to-end: variable declaration") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "var x = 42;"

    // Tokenize
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()

    // Parse
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    // Compile
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }

  test("end-to-end: if statement") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "if (true) { 1 + 2; }"

    // Tokenize
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()

    // Parse
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    // Compile
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }

  test("end-to-end: while loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "var i = 0; while (i < 3) { i = i + 1; }"

    // Tokenize
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()

    // Parse
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    // Compile
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }

  test("end-to-end: function declaration") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "function add(a, b) { return a + b; }"

    // Tokenize
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()

    // Parse
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    // Compile
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Function declarations are compiled but not yet callable (need global scope)
    assertEquals(result, JSValue.Undefined)
  }

  test("end-to-end: for loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "for (var i = 0; i < 3; i = i + 1) { }"

    // Tokenize
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()

    // Parse
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    // Compile
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }

  test("end-to-end: complex arithmetic") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "(1 + 2) * 3 - 4 / 2"

    // Tokenize
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()

    // Parse
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    // Compile
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }

  test("end-to-end: logical operators") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "true && false || true"

    // Tokenize
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()

    // Parse
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    // Compile
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }
