package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class NumberMathObjectTest extends FunSuite:

  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("Math basics") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    assertEquals(eval("Math.abs(-5)").toNumber, 5.0)
    assertEquals(eval("Math.max(1, 3, 2)").toNumber, 3.0)
    assertEquals(eval("Math.min(1, 3, 2)").toNumber, 1.0)
    assertEquals(eval("Math.floor(2.9)").toNumber, 2.0)
    assertEquals(eval("Math.ceil(2.1)").toNumber, 3.0)
    assertEquals(eval("Math.round(2.5)").toNumber, 3.0)
    assertEquals(eval("Math.pow(2, 3)").toNumber, 8.0)
  }

  test("Number statics and parse") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    assertEquals(eval("Number.isInteger(3)").toBoolean, true)
    assertEquals(eval("Number.isInteger(3.2)").toBoolean, false)
    assertEquals(eval("Number.isSafeInteger(9007199254740991)").toBoolean, true)
    assertEquals(eval("parseInt('0x10')").toNumber, 16.0)
    assertEquals(eval("parseFloat('3.14x')").toNumber, 3.14)
  }

  test("Number and Boolean prototypes") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    assertEquals(eval("Number.prototype.toFixed.call(1.25, 1)").toString, "1.3")
    assertEquals(eval("Boolean.prototype.toString.call(true)").toString, "true")
  }

  test("Object assign and values") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    assertEquals(eval("var a={x:1}; var b={y:2}; Object.assign(a,b).x + a.y;").toNumber, 3.0)
    assertEquals(eval("Object.values({a:1,b:2}).length").toNumber, 2.0)
    assertEquals(eval("Object.entries({a:1}).length").toNumber, 1.0)
  }
