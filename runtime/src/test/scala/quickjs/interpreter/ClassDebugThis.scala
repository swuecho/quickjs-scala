package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue

object ClassDebugThis {
  def main(args: Array[String]): Unit = {
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)
    StdLib.initialize(ctx)

    // Test: inherited method accessing this.name (no explicit Dog constructor)
    val source = """
      class Animal {
        constructor(name) {
          this.name = name;
        }
        speak() {
          return this.name + " makes noise";
        }
      }
      class Dog extends Animal {
        speak() {
          return this.name + " barks";
        }
      }
      var d = new Dog("Rex");
      d.name;
    """"

    println("Test: Class inheritance - this binding in methods")
    runTest(source)
  }

  def runTest(source: String)(using ctx: JSContext): Unit = {
    try {
      val lexer = Lexer(source)
      val tokens = lexer.tokenize()
      val parser = Parser(tokens)
      val ast = parser.parseScript()
      println("AST parsed successfully")

      val compiler = Compiler()
      val bytecode = compiler.compileScript(ast)
      println("Bytecode compiled successfully")

      val interpreter = Interpreter()
      println("Calling interpreter...")
      val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
      println(s"Result type: ${result.getClass.getName}")
      println(s"Result value: '$result'")
      println(s"Result toString: ${result.toString}")
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
        println(s"Exception: ${ex.getClass.getName}: ${ex.getMessage}")
        ex.printStackTrace()
    }
  }
}
