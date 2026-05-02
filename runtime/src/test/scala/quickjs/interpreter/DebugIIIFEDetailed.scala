package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.bytecode.Opcode
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class DebugIIIFEDetailed extends FunSuite {

  test("debug IIFE with bytecode inspection") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "(function() { return 42; })()"

    val lexer = Lexer(source)
    val parser = Parser(lexer.tokenize())
    val ast = parser.parseScript()

    println(s"\n=== AST ===")
    println(s"AST: $ast")
    println(s"Statements: ${ast.body.length}")

    val compiler = Compiler()

    println(s"\n=== Compiling WITHOUT REPL mode ===")
    val bytecode1 = compiler.compileScript(ast)
    println(s"Bytecode length: ${bytecode1.bytecode.length}")
    println(s"Constants: ${bytecode1.constants.length}")

    // Print bytecode instructions
    var pc1 = 0
    println("\nBytecode (non-REPL):")
    while pc1 < bytecode1.bytecode.length do
      val op = bytecode1.bytecode(pc1).toInt & 0xff
      println(f"  $pc1%4d: $op%3d")
      pc1 += 1

    val interpreter1 = Interpreter()
    val result1 = interpreter1.call(bytecode1, JSValue.Undefined, Array.empty)
    println(s"\nResult (non-REPL): $result1")

    println(s"\n=== Compiling WITH REPL mode ===")
    val bytecode2 = compiler.withREPLMode(compiler.compileScript(ast))
    println(s"Bytecode length: ${bytecode2.bytecode.length}")

    // Print bytecode instructions
    var pc2 = 0
    println("\nBytecode (REPL):")
    while pc2 < bytecode2.bytecode.length do
      val op = bytecode2.bytecode(pc2).toInt & 0xff
      println(f"  $pc2%4d: $op%3d")
      pc2 += 1

    val interpreter2 = Interpreter()
    val result2 = interpreter2.call(bytecode2, JSValue.Undefined, Array.empty)
    println(s"\nResult (REPL): $result2")
  }
}
