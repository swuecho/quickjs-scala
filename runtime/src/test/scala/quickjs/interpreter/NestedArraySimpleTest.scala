package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class NestedArraySimpleTest extends FunSuite:

  test("debug: simple nested array - just one level") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Test just storing an array in another array
    val source = "var inner = [1, 2]; var outer = [inner]; outer[0]"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    println(s"Result: $result")
    println(s"Result class: ${result.getClass}")

    result match
      case JSValue.JSArrayVal(arr) =>
        println(s"It's an array!")
      case _ =>
        println(s"Not an array")

    // For now, just check it doesn't crash
    assert(true)
  }
