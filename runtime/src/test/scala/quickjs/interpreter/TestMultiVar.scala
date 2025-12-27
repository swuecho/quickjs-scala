package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class TestMultiVar extends FunSuite {

  test("multiple variable declarations") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "var i = 0, j = 10; i + j"
    val lexer = Lexer(source)
    val parser = Parser(lexer.tokenize())
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    println(s"\nTest result: $result")
    println(s"Expected: 10")

    assert(result.toNumber == 10.0, s"Expected 10 but got ${result.toNumber}")
  }

  test("for loop with multiple vars in init (no comma in update)") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = """
      |var sum = 0;
      |for (var i = 0, j = 10; i < 3; i++) {
      |  sum += i;
      |}
      |sum
      |""".stripMargin
    val lexer = Lexer(source)
    val parser = Parser(lexer.tokenize())
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    println(s"\nFor loop with multi-var init result: $result")
    println(s"Expected: 3 (0 + 1 + 2)")

    assert(result.toNumber == 3.0, s"Expected 3 but got ${result.toNumber}")
  }
}
