import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler

object TestNewCompile extends App:
  val code = "new F(2)"
  val lexer = Lexer(code)
  val tokens = lexer.tokenize()
  println("Tokens:")
  tokens.take(10).foreach(println)
  println()

  val parser = Parser(tokens)
  val ast = parser.parseScript()
  println("AST:")
  println(ast)
  println()

  val compiler = Compiler()
  val bytecode = compiler.compileScript(ast)
  println("Bytecode compiled successfully")
  println(s"Bytecode length: ${bytecode.bytecode.length}")
