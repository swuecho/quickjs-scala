package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.runtime.StdLib
import quickjs.value.JSValue

/** Shared test utilities to reduce code duplication across test files. */
object TestHarness:
  def freshContext(): (JSRuntime, JSContext) =
    val rt = JSRuntime()
    val ctx = JSContext(rt)
    StdLib.initialize(ctx)
    (rt, ctx)

  def eval(source: String)(using ctx: JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  def evalFresh(source: String): JSValue =
    val (rt, ctx) = freshContext()
    eval(source)(using ctx)
