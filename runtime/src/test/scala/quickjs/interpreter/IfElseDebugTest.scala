package quickjs.interpreter

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.compiler.Compiler
import quickjs.value.JSValue
import quickjs.runtime.*
import munit.*

class IfElseDebugTest extends FunSuite {

  test("debug if-else bytecode") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // if (false) { 1; } else { 2; }
    val ast = Script(
      body = Seq(
        IfStatement(
          test = Literal(JSValue.Bool(false), Span(4, 9, 0, 4)),
          consequent = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(1), Span(13, 14, 0, 13)),
                span = Span(13, 14, 0, 13)
              )
            ),
            span = Span(11, 16, 0, 11)
          ),
          alternate = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(2), Span(24, 25, 0, 24)),
                span = Span(24, 25, 0, 24)
              )
            ),
            span = Span(18, 27, 0, 18)
          ),
          span = Span(0, 27, 0, 0)
        )
      ),
      span = Span(0, 27, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    println(s"\n=== If-Else Bytecode Debug ===")
    println(s"Bytecode length: ${bytecode.bytecode.length}")
    println("Bytecode bytes:")
    bytecode.bytecode.zipWithIndex.foreach { case (b, i) =>
      println(f"  [$i%2d] = 0x$b%02x (${b & 0xFF}%3d)")
    }

    println("\nDisassembly:")
    var pc = 0
    while pc < bytecode.bytecode.length do
      val opcode = Opcode.fromCode(bytecode.bytecode(pc).toInt & 0xFF).getOrElse(Opcode.Invalid)
      println(f"  [$pc%2d] $opcode%-20s")

      opcode match
        case Opcode.PushI32 =>
          val value = readInt32(bytecode.bytecode, pc + 1)
          println(f"       -> value = $value")
          pc += 5
        case Opcode.PushFloat64 =>
          val value = java.lang.Double.longBitsToDouble(readInt64(bytecode.bytecode, pc + 1))
          println(f"       -> value = $value")
          pc += 9
        case Opcode.IfFalse | Opcode.IfTrue | Opcode.Goto =>
          val offset = readInt32(bytecode.bytecode, pc + 1)
          println(f"       -> offset = $offset")
          pc += 5
        case Opcode.GetLoc | Opcode.PutLoc =>
          val index = readInt32(bytecode.bytecode, pc + 1)
          println(f"       -> index = $index")
          pc += 5
        case _ =>
          pc += 1

    println("==============================\n")
  }

  private def readInt32(buf: Array[Byte], pc: Int): Int =
    ((buf(pc) & 0xFF) << 24) | ((buf(pc + 1) & 0xFF) << 16) |
    ((buf(pc + 2) & 0xFF) << 8) | (buf(pc + 3) & 0xFF)

  private def readInt64(buf: Array[Byte], pc: Int): Long =
    ((buf(pc).toLong & 0xFF) << 56) |
    ((buf(pc + 1).toLong & 0xFF) << 48) |
    ((buf(pc + 2).toLong & 0xFF) << 40) |
    ((buf(pc + 3).toLong & 0xFF) << 32) |
    ((buf(pc + 4).toLong & 0xFF) << 24) |
    ((buf(pc + 5).toLong & 0xFF) << 16) |
    ((buf(pc + 6).toLong & 0xFF) << 8) |
    (buf(pc + 7).toLong & 0xFF)
}
