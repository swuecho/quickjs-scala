package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class ForLoopTest extends FunSuite {

  test("simple for loop without comma") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = """
      |var sum = 0;
      |for (var i = 0; i < 3; i++) {
      |  sum += i;
      |}
      |sum
      |""".stripMargin
    val lexer = Lexer(source)
    val parser = Parser(lexer.tokenize())
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    println(s"\nSimple for loop test result: $result")
    println(s"Expected: 3 (0 + 1 + 2)")

    // Should run 3 iterations: sum = 0 + 1 + 2 = 3
    assert(result.toNumber == 3.0, s"Expected 3 but got ${result.toNumber}")
  }

  test("for loop with two variables (no comma in update)") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = """
      |var sum = 0;
      |var j = 10;
      |for (var i = 0; i < 3; i++) {
      |  j--;
      |  sum += i;
      |}
      |sum
      |""".stripMargin
    val lexer = Lexer(source)
    val parser = Parser(lexer.tokenize())
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    println(s"\nFor loop with j in body test result: $result")
    println(s"Expected: 3 (0 + 1 + 2)")

    // Should run 3 iterations: sum = 0 + 1 + 2 = 3
    assert(result.toNumber == 3.0, s"Expected 3 but got ${result.toNumber}")
  }
}
