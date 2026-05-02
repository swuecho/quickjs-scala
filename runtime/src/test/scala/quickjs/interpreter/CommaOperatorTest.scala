package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class CommaOperatorTest extends FunSuite {

  test("comma operator: simple expression") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "var a = (1, 2); a"
    val lexer = Lexer(source)
    val parser = Parser(lexer.tokenize())
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    println(s"\nComma operator test: $source")
    println(s"Result: $result")
    println(s"Expected: ${JSValue.fromInt(2)}")

    // The comma operator should evaluate 1 (discard), then 2 (return)
    assert(result.toNumber == 2.0)
  }

  test("comma operator: with side effects") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = """
      |var x = 0;
      |var y = (x++, x + 5);
      |y
      |""".stripMargin
    val lexer = Lexer(source)
    val parser = Parser(lexer.tokenize())
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Check x was incremented
    val xVal = summon[JSContext].globalScope.getVariable("x")
    println(s"\nx after comma: $xVal")
    assert(xVal.isDefined && xVal.get.toNumber == 1.0)

    // y should be x + 5 = 6
    val yVal = summon[JSContext].globalScope.getVariable("y")
    println(s"y after comma: $yVal")
    assert(yVal.isDefined && yVal.get.toNumber == 6.0)
  }

  test("for loop with comma operator in update") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = """
      |var sum = 0;
      |for (var i = 0, j = 10; i < 3; i++, j--) {
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

    println(s"\nFor loop test result: $result")
    println(s"Expected: 3 (0 + 1 + 2)")

    // Should run 3 iterations: sum = 0 + 1 + 2 = 3
    assert(result.toNumber == 3.0)
  }
}
