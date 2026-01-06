import quickjs.runtime.{JSRuntime, JSContext}
import quickjs.value.JSValue
import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter

@main def testRestDebug(): Unit =
  given runtime: JSRuntime = JSRuntime()
  given ctx: JSContext = JSContext(runtime)

  val code = """
  (function() {
    var [a, ...rest] = [1, 2, 3, 4, 5];
    return a;
  })()
  """

  try
    println("=== Parsing ===")
    val lexer = Lexer(code)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    println(s"AST: $ast")

    println("\n=== Compiling (REPL mode) ===")
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    println(s"Bytecode instructions: ${bytecode.instructions.length}")

    println("\n=== Executing ===")
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
    println(s"Result: $result")
    println(s"Expected: 1")
  catch
    case e: quickjs.runtime.JSException =>
      println(s"\n=== JSException ===")
      println(s"Exception value: ${e.value}")
      println(s"Message: ${e.getMessage}")
      e.printStackTrace()
    case e: Exception =>
      println(s"\n=== Other Exception ===")
      println(s"Error: ${e.getClass.getName}: ${e.getMessage}")
      e.printStackTrace()
