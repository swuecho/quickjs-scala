package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class TernaryOperatorTest extends FunSuite {

  private def eval(source: String)(using JSContext): JSValue = {
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    println(s"Tokens: $tokens")
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    println(s"AST: $ast")
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    println(s"Bytecode length: ${bytecode.bytecode.length}")
    println(s"Bytecode (hex): ${bytecode.bytecode.map("%02X".format(_)).mkString(" ")}")
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)
  }

  test("ternary: simple true condition") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = eval("true ? 1 : 0")
    println(s"Result: $result")
    println(s"Result type: ${result.getClass}")
    println(s"Result value: $result")
    result match {
      case JSValue.Int32(i) => println(s"Int32 value: $i")
      case _ => println(s"Not an Int32")
    }
    assertEquals(result, JSValue.fromInt(1))
  }
}
