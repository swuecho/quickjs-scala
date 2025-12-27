package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.runtime.StdLib
import quickjs.value.JSValue
import munit.*

class DebugClosureREPL extends FunSuite {

  test("debug closure loop - with REPL mode") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    // The exact test code with REPL mode
    val source = """
      |(function() {
      |  var funcs = [];
      |  for (var i = 0; i < 3; i++) {
      |    (function(j) {
      |      funcs.push(function() { return j; });
      |    })(i);
      |  }
      |  return funcs[0]() + funcs[1]() + funcs[2]();
      |})()
      |""".stripMargin

    val lexer = Lexer(source)
    val parser = Parser(lexer.tokenize())
    val ast = parser.parseScript()
    val compiler = Compiler()

    println(s"\n=== Testing WITHOUT REPL mode ===")
    val bytecode1 = compiler.compileScript(ast)
    val interpreter1 = Interpreter()
    val result1 = interpreter1.call(bytecode1, JSValue.Undefined, Array.empty)
    println(s"Result: $result1")
    println(s"Expected: 3")

    println(s"\n=== Testing WITH REPL mode ===")
    val bytecode2 = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter2 = Interpreter()
    val result2 = interpreter2.call(bytecode2, JSValue.Undefined, Array.empty)
    println(s"Result: $result2")
    println(s"Expected: 3")
  }
}
