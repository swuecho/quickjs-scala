package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class ArrowFunctionTest extends FunSuite:

  test("Arrow function with Array.map") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Initialize standard library
    ArrayStatics.initialize()

    val source = """
      |var arr = [1, 2, 3];
      |var doubled = arr.map(x => x * 2);
      |doubled
      |""".stripMargin

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Result should be [2, 4, 6]
    result match
      case arrVal: JSValue.JSArrayVal =>
        val arr = arrVal.value
        assertEquals(arr.getLength, 3)
        assertEquals(arr.get(0), JSValue.fromInt(2))
        assertEquals(arr.get(1), JSValue.fromInt(4))
        assertEquals(arr.get(2), JSValue.fromInt(6))
      case _ =>
        fail(s"Expected array but got $result")
  }

  test("Arrow function with multiple parameters and Array.map") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Initialize standard library
    ArrayStatics.initialize()

    val source = """
      |var arr = [1, 2, 3];
      |var result = arr.map((x, i) => x + i);
      |result
      |""".stripMargin

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    val callResult = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Result should be [1+0, 2+1, 3+2] = [1, 3, 5]
    callResult match
      case arrVal: JSValue.JSArrayVal =>
        val arr = arrVal.value
        assertEquals(arr.getLength, 3)
        assertEquals(arr.get(0), JSValue.fromInt(1))
        assertEquals(arr.get(1), JSValue.fromInt(3))
        assertEquals(arr.get(2), JSValue.fromInt(5))
      case _ =>
        fail(s"Expected array but got $callResult")
  }

  test("Arrow function with block body and Array.map") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Initialize standard library
    ArrayStatics.initialize()

    val source = """
      |var arr = [1, 2, 3];
      |var result = arr.map(x => { return x * 3; });
      |result
      |""".stripMargin

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    val callResult = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Result should be [3, 6, 9]
    callResult match
      case arrVal: JSValue.JSArrayVal =>
        val arr = arrVal.value
        assertEquals(arr.getLength, 3)
        assertEquals(arr.get(0), JSValue.fromInt(3))
        assertEquals(arr.get(1), JSValue.fromInt(6))
        assertEquals(arr.get(2), JSValue.fromInt(9))
      case _ =>
        fail(s"Expected array but got $callResult")
  }
