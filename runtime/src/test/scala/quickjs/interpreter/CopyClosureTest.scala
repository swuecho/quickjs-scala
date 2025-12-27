package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.runtime.StdLib
import quickjs.value.JSValue
import munit.*

class CopyClosureTest extends FunSuite {

  /** Helper to create an initialized JSContext */
  private def createContext(): JSContext =
    val runtime = JSRuntime()
    val ctx = JSContext(runtime)
    StdLib.initialize(ctx)
    ctx

  /** Helper to evaluate code and return result */
  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("closure: closure with loop - EXACT COPY") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    // Create multiple closures in a loop
    val result = eval("""
      |(function() {
      |  var funcs = [];
      |  for (var i = 0; i < 3; i++) {
      |    (function(j) {
      |      funcs.push(function() { return j; });
      |    })(i);
      |  }
      |  return funcs[0]() + funcs[1]() + funcs[2]();
      |})()
      |""".stripMargin)

    println(s"\nResult: $result")
    println(s"Expected: 3")

    assertEquals(result.toNumber, 3.0, "sum of closures = 0 + 1 + 2")
  }
}
