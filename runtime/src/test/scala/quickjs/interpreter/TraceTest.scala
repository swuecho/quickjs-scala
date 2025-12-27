package quickjs.interpreter

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.compiler.Compiler
import quickjs.value.JSValue
import quickjs.runtime.*
import munit.*

class TraceTest extends FunSuite {

  test("trace execution with detailed logging") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // if (false) { 42; }
    val ast = Script(
      body = Seq(
        IfStatement(
          test = Literal(JSValue.Bool(false), Span(4, 9, 0, 4)),
          consequent = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(42), Span(13, 15, 0, 13)),
                span = Span(13, 15, 0, 13)
              )
            ),
            span = Span(11, 17, 0, 11)
          ),
          alternate = null,
          span = Span(0, 17, 0, 0)
        )
      ),
      span = Span(0, 17, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Create a custom interpreter with tracing
    val stack = new Array[JSValue](256)
    var stackTop = 0
    var pc = 0
    val bc = bytecode.bytecode

    val locals = new Array[JSValue](256)
    var localsCount = 0

    println(s"\n=== Execution Trace ===")
    var step = 0
    var result: JSValue = JSValue.Undefined

    try {
      while pc < bc.length do
        step += 1
        val opcode = Opcode.fromCode(bc(pc).toInt & 0xFF).getOrElse(Opcode.Invalid)
        println(f"[$step%3d] pc=$pc%2d $opcode%-20s | stackTop=$stackTop")

        opcode match
          case Opcode.PushFalse =>
            stack(stackTop) = JSValue.Bool(false)
            stackTop += 1
            pc += 1

          case Opcode.IfFalse =>
            val offset = readInt32(bc, pc + 1)
            val value = stack(stackTop - 1)
            println(f"     -> IfFalse: offset=$offset, value=$value")
            stackTop -= 1
            if !value.toBoolean then
              println(f"     -> Taking jump: pc=$pc + $offset = ${pc + offset}")
              pc += offset
            else
              println(f"     -> Not taking jump: pc=$pc + 5 = ${pc + 5}")
              pc += 5

          case Opcode.PushI32 =>
            val value = readInt32(bc, pc + 1)
            println(f"     -> PushI32: $value")
            stack(stackTop) = JSValue.fromInt(value)
            stackTop += 1
            pc += 5

          case Opcode.Drop =>
            stackTop -= 1
            pc += 1

          case Opcode.ReturnUndef =>
            result = JSValue.Undefined
            pc += 1

          case _ =>
            throw new RuntimeException(s"Unexpected opcode at pc=$pc: $opcode")
    } catch {
      case ex: Exception =>
        println(f"ERROR at step=$step, pc=$pc")
        throw ex
    }

    println(s"=======================\n")

    assert(result == JSValue.Undefined)
  }

  private def readInt32(buf: Array[Byte], pc: Int): Int =
    ((buf(pc) & 0xFF) << 24) | ((buf(pc + 1) & 0xFF) << 16) |
    ((buf(pc + 2) & 0xFF) << 8) | (buf(pc + 3) & 0xFF)
}
