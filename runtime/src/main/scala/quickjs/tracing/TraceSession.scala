package quickjs.tracing

import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue

final case class TraceResult(
  events: Vector[TraceEvent],
  json: String,
  instructions: Vector[InstructionInfo],
  bytecodeHex: String,
  bytecodeLength: Int,
  constantsCount: Int,
  functionName: String
)

object TraceSession:
  def run(source: String, replMode: Boolean = false)(using JSRuntime, JSContext): TraceResult =
    val tokens = Lexer(source).tokenize()
    val ast = Parser(tokens).parseScript()
    val compiler = Compiler()
    val func =
      if replMode then compiler.withREPLMode(compiler.compileScript(ast))
      else compiler.compileScript(ast)
    val tracer = TraceCollector()
    val interpreter = Interpreter()
    interpreter.call(func, JSValue.Undefined, Array.empty, trace = tracer)

    val events = tracer.getEvents
    val json = TraceJson.eventsToJson(events)
    val instructions = BytecodeDisassembler.disassemble(func.bytecode)
    TraceResult(
      events = events,
      json = json,
      instructions = instructions,
      bytecodeHex = toHex(func.bytecode),
      bytecodeLength = func.bytecode.length,
      constantsCount = func.constants.length,
      functionName = func.name
    )

  private def toHex(bytes: Array[Byte]): String =
    val sb = StringBuilder()
    var i = 0
    while i < bytes.length do
      if i > 0 then sb.append(' ')
      sb.append(f"${bytes(i) & 0xff}%02x")
      i += 1
    sb.toString
