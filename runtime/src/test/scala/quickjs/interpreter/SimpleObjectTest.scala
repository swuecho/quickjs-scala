package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class SimpleObjectTest extends FunSuite:

  test("Single property object") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "var obj = {x: 1};"

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    System.err.println(s"Bytecode length: ${bytecode.bytecode.length}")
    System.err.println(s"Bytecode: ${bytecode.bytecode.take(100).map("%02X".format(_)).mkString(" ")}")

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }
