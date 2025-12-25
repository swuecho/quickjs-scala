package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class ObjectLiteralDebugTest extends FunSuite:

  test("Debug bytecode encoding") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "var obj = {x: 1};"

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    println(s"Tokens: $tokens")

    val parser = Parser(tokens)
    val ast = parser.parseScript()
    println(s"AST: $ast")

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    println(s"Bytecode length: ${bytecode.bytecode.length}")
    println(s"Bytecode hex: ${bytecode.bytecode.map("%02X".format(_)).mkString(" ")}")

    // Try to execute
    val interpreter = Interpreter()
    try {
      val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
      println(s"Result: $result")
    } catch {
      case e: Exception =>
        println(s"Error: ${e.getMessage}")
        e.printStackTrace()
    }
  }
