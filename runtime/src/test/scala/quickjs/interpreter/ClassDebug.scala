package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue

object ClassDebug {
  def main(args: Array[String]): Unit = {
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)
    StdLib.initialize(ctx)

    // Test 1: Basic inheritance
    val source1 = """
      class Animal {
        constructor(name) {
          this.name = name;
        }
      }
      class Dog extends Animal {
        constructor(name, breed) {
          super(name);
          this.breed = breed;
        }
      }
      var d = new Dog("Rex", "German Shepherd");
      d.name;
    """

    println("Test 1: Class inheritance with super constructor")
    runTest(source1)

    // Test 2: super method call
    val source2 = """
      class Rectangle {
        constructor(w, h) {
          this.w = w;
          this.h = h;
        }
        area() {
          return this.w * this.h;
        }
      }
      class Square extends Rectangle {
        constructor(s) {
          super(s, s);
        }
        area() {
          return super.area();
        }
      }
      var s = new Square(5);
      s.area();
    """

    println("\nTest 2: super method call")
    runTest(source2)
  }

  def runTest(source: String)(using ctx: JSContext): Unit = {
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
}
