import quickjs.ast.*
import quickjs.compiler.Compiler
import quickjs.value.JSValue
import quickjs.runtime.{JSRuntime, JSContext}

object debug_increment {
  def main(args: Array[String]): Unit = {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // var i = 0; while (i < 3) { ++i; }
    val ast = Script(
      body = Seq(
        VariableDeclaration(
          kind = VariableKind.Var,
          declarations = Seq(
            VariableDeclarator(
              id = Identifier("i", Span(0, 1, 0, 0)),
              init = Literal(JSValue.fromInt(0), Span(6, 7, 0, 6)),
              span = Span(0, 7, 0, 0)
            )
          ),
          span = Span(0, 7, 0, 0)
        ),
        WhileStatement(
          test = BinaryExpression(
            operator = BinaryOperator.Lt,
            left = Identifier("i", Span(16, 17, 0, 16)),
            right = Literal(JSValue.fromInt(3), Span(18, 19, 0, 18)),
            span = Span(16, 19, 0, 16)
          ),
          body = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                UnaryExpression(
                  operator = UnaryOperator.PreInc,
                  argument = Identifier("i", Span(0, 1, 0, 0)),
                  prefix = true,
                  span = Span(0, 3, 0, 0)
                ),
                span = Span(0, 3, 0, 0)
              )
            ),
            span = Span(21, 25, 0, 21)
          ),
          span = Span(9, 27, 0, 9)
        )
      ),
      span = Span(0, 27, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    println(s"Bytecode length: ${bytecode.bytecode.length}")

    // Proper disassembly that handles operand sizes
    def disassemble(): Unit =
      var pc = 0
      while pc < bytecode.bytecode.length do
        val opcode = bytecode.bytecode(pc).toInt & 0xFF
        quickjs.bytecode.Opcode.fromCode(opcode) match
          case Some(op) =>
            print(s"pc=$pc: $op")
            // Print operands based on opcode
            op match
              case quickjs.bytecode.Opcode.PushI32 =>
                val value = ((bytecode.bytecode(pc + 1) & 0xFF) << 24) |
                            ((bytecode.bytecode(pc + 2) & 0xFF) << 16) |
                            ((bytecode.bytecode(pc + 3) & 0xFF) << 8) |
                            (bytecode.bytecode(pc + 4) & 0xFF)
                print(s"($value)")
                pc += 5
              case quickjs.bytecode.Opcode.IfFalse | quickjs.bytecode.Opcode.IfTrue |
                   quickjs.bytecode.Opcode.Goto =>
                val offset = ((bytecode.bytecode(pc + 1) & 0xFF) << 24) |
                             ((bytecode.bytecode(pc + 2) & 0xFF) << 16) |
                             ((bytecode.bytecode(pc + 3) & 0xFF) << 8) |
                             (bytecode.bytecode(pc + 4) & 0xFF)
                print(s"(offset=$offset, target=${pc + 1 + offset})")
                pc += 5
              case quickjs.bytecode.Opcode.GetGlobal | quickjs.bytecode.Opcode.PutGlobal =>
                val len = ((bytecode.bytecode(pc + 1) & 0xFF) << 24) |
                          ((bytecode.bytecode(pc + 2) & 0xFF) << 16) |
                          ((bytecode.bytecode(pc + 3) & 0xFF) << 8) |
                          (bytecode.bytecode(pc + 4) & 0xFF)
                val name = new String(bytecode.bytecode, pc + 5, len)
                print(s"(\"$name\")")
                pc += 1 + 4 + len
              case quickjs.bytecode.Opcode.DefVar =>
                val len = ((bytecode.bytecode(pc + 1) & 0xFF) << 24) |
                          ((bytecode.bytecode(pc + 2) & 0xFF) << 16) |
                          ((bytecode.bytecode(pc + 3) & 0xFF) << 8) |
                          (bytecode.bytecode(pc + 4) & 0xFF)
                val name = new String(bytecode.bytecode, pc + 5, len)
                print(s"(\"$name\")")
                pc += 1 + 4 + len
              case quickjs.bytecode.Opcode.Lt =>
                pc += 1
              case quickjs.bytecode.Opcode.PreInc =>
                pc += 1
              case quickjs.bytecode.Opcode.Drop =>
                pc += 1
              case quickjs.bytecode.Opcode.ReturnUndef =>
                pc += 1
              case _ =>
                println(s" (unknown operand size, pc+=1)")
                pc += 1
            println()
          case None =>
            println(s"pc=$pc: UNKNOWN($opcode)")
            pc += 1

    disassemble()
  }
}
