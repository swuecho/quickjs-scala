package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class ArrayMethodsTest extends FunSuite:

  test("arr.push adds element to array") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    // Initialize Array methods
    ArrayStatics.initialize()

    val source = "var arr = [1, 2, 3]; arr.push(4); arr[3]"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.fromInt(4))
  }

  test("arr.pop removes and returns last element") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    // Initialize Array methods
    ArrayStatics.initialize()

    val source = "var arr = [1, 2, 3]; var popped = arr.pop(); popped"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.fromInt(3))
  }

  test("arr.push and arr.pop work together") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    // Initialize Array methods
    ArrayStatics.initialize()

    val source = """
      var arr = [1];
      arr.push(2);
      arr.push(3);
      var a = arr.pop();
      var b = arr.pop();
      a + b
    """
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.fromInt(5))  // 3 + 2
  }
