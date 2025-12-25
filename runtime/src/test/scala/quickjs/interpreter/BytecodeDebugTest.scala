package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import quickjs.bytecode.Opcode
import munit.*

class BytecodeDebugTest extends FunSuite:

  test("Debug bytecode step by step") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val source = "var obj = {x: 1};"

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    println("=== AST ===")
    println(ast)

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    println(s"\n=== Bytecode (${bytecode.bytecode.length} bytes) ===")
    val hex = bytecode.bytecode.map("%02X".format(_)).mkString(" ")
    println(hex)

    // Decode the bytecode
    println("\n=== Decoded Instructions ===")
    var pc = 0
    while pc < bytecode.bytecode.length do
      val opcode = Opcode.fromCode(bytecode.bytecode(pc).toInt & 0xFF).getOrElse(Opcode.Invalid)
      println(s"PC $pc: $opcode")
      pc += 1
      opcode match
        case Opcode.PushI32 =>
          if pc + 4 <= bytecode.bytecode.length then
            val value = ((bytecode.bytecode(pc) & 0xFF) << 24) |
                       ((bytecode.bytecode(pc+1) & 0xFF) << 16) |
                       ((bytecode.bytecode(pc+2) & 0xFF) << 8) |
                       (bytecode.bytecode(pc+3) & 0xFF)
            println(s"  -> PushI32($value)")
            pc += 4
          else
            println(s"  -> ERROR: Not enough bytes for operand")
            pc = bytecode.bytecode.length
        case Opcode.PutLoc =>
          if pc + 4 <= bytecode.bytecode.length then
            val index = ((bytecode.bytecode(pc) & 0xFF) << 24) |
                        ((bytecode.bytecode(pc+1) & 0xFF) << 16) |
                        ((bytecode.bytecode(pc+2) & 0xFF) << 8) |
                        (bytecode.bytecode(pc+3) & 0xFF)
            println(s"  -> PutLoc($index)")
            pc += 4
          else
            println(s"  -> ERROR: Not enough bytes for operand")
            pc = bytecode.bytecode.length
        case Opcode.GetProp =>
          if pc + 4 <= bytecode.bytecode.length then
            val len = ((bytecode.bytecode(pc) & 0xFF) << 24) |
                      ((bytecode.bytecode(pc+1) & 0xFF) << 16) |
                      ((bytecode.bytecode(pc+2) & 0xFF) << 8) |
                      (bytecode.bytecode(pc+3) & 0xFF)
            println(s"  -> GetProp(length=$len)")
            if pc + 4 + len <= bytecode.bytecode.length then
              val propName = new String(bytecode.bytecode.slice(pc+4, pc+4+len), "UTF-8")
              println(s"  -> Property name: '$propName'")
              pc += 4 + len
            else
              println(s"  -> ERROR: Not enough bytes for string")
              pc = bytecode.bytecode.length
          else
            println(s"  -> ERROR: Not enough bytes for length")
            pc = bytecode.bytecode.length
        case Opcode.SetProp =>
          if pc + 4 <= bytecode.bytecode.length then
            val len = ((bytecode.bytecode(pc) & 0xFF) << 24) |
                      ((bytecode.bytecode(pc+1) & 0xFF) << 16) |
                      ((bytecode.bytecode(pc+2) & 0xFF) << 8) |
                      (bytecode.bytecode(pc+3) & 0xFF)
            println(s"  -> SetProp(length=$len)")
            if pc + 4 + len <= bytecode.bytecode.length then
              val propName = new String(bytecode.bytecode.slice(pc+4, pc+4+len), "UTF-8")
              println(s"  -> Property name: '$propName'")
              pc += 4 + len
            else
              println(s"  -> ERROR: Not enough bytes for string")
              pc = bytecode.bytecode.length
          else
            println(s"  -> ERROR: Not enough bytes for length")
            pc = bytecode.bytecode.length
        case Opcode.ReturnUndef =>
          println(s"  -> ReturnUndef")
        case Opcode.NewObject =>
          println(s"  -> NewObject")
        case Opcode.Dup =>
          println(s"  -> Dup")
        case Opcode.Drop =>
          println(s"  -> Drop")
        case _ =>
          println(s"  -> Unknown or unimplemented opcode")
          pc = bytecode.bytecode.length
  }
