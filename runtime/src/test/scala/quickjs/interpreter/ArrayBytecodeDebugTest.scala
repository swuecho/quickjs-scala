package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class ArrayBytecodeDebugTest extends FunSuite:

  test("debug: print bytecode for array literal") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "[1, 2, 3]"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    println(s"AST: $ast")

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    println(s"Bytecode length: ${bytecode.bytecode.length}")
    println("Bytecode bytes:")
    bytecode.bytecode.grouped(20).zipWithIndex.foreach { case (group, i) =>
      val hex = group.map(b => f"$b%02x").mkString(" ")
      println(f"  [$i] = $hex")
    }
  }

  test("debug: print bytecode for array access") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "var arr = [1, 2, 3]; arr[0]"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    println(s"AST: $ast")

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    println(s"Bytecode length: ${bytecode.bytecode.length}")
    println("Bytecode bytes:")
    bytecode.bytecode.grouped(20).zipWithIndex.foreach { case (group, i) =>
      val hex = group.map(b => f"$b%02x").mkString(" ")
      println(f"  [$i] = $hex")
    }
  }
