package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue

object ClassDebugInstanceOf {
  def main(args: Array[String]): Unit = {
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)
    StdLib.initialize(ctx)

    // Test instanceof
    val source = """
      class Animal {}
      class Dog extends Animal {}
      var d = new Dog();
      // Test instanceof
      "d instanceof Dog: " + (d instanceof Dog) + ", d instanceof Animal: " + (d instanceof Animal);
    """

    println("Test: instanceof debug")
    runTest(source)
  }

  def runTest(source: String)(using ctx: JSContext): Unit =
    try {
      val lexer = Lexer(source)
      val tokens = lexer.tokenize()
      val parser = Parser(tokens)
      val ast = parser.parseScript()

      val compiler = Compiler()
      val bytecode = compiler.compileScript(ast)

      val interpreter = Interpreter()
      val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
      println(s"Result: $result")
    } catch {
      case je: quickjs.runtime.JSException =>
        println(s"JSException: ${je.getMessage}")
        je.getValue match
          case JSValue.Object(obj) =>
            given JSContext = ctx
            val msg = obj.get("message")
            println(s"Error message: $msg")
            val stack = obj.get("stack")
            println(s"Stack: $stack")
          case other =>
            println(s"Error value: $other")
      case ex: Exception =>
        println(s"Exception: ${ex.getMessage}")
        ex.printStackTrace()
    }
}
