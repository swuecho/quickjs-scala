package quickjs.bytecode

import scala.annotation.switch

/** Bytecode opcodes for the QuickJS-Scala virtual machine.
  *
  * Design:
  * - Stack-based virtual machine
  * - Compact encoding (1-5 bytes per instruction)
  * - Type-specialized opcodes for performance
  */
enum Opcode(val code: Int):
  // Invalid / nop
  case Invalid extends Opcode(0)
  case Nop extends Opcode(1)

  // Stack manipulation
  case PushI32 extends Opcode(2)      // push 32-bit integer constant
  case PushFloat64 extends Opcode(3)   // push 64-bit float constant
  case PushUndefined extends Opcode(4) // push undefined
  case PushNull extends Opcode(5)      // push null
  case PushTrue extends Opcode(6)      // push true
  case PushFalse extends Opcode(7)     // push false

  // Stack operations
  case Drop extends Opcode(8)          // drop top value
  case Dup extends Opcode(9)           // a -> a a

  // Arithmetic/logic
  case Neg extends Opcode(10)          // -x
  case Not extends Opcode(11)          // !x
  case LNot extends Opcode(12)         // ~x
  case Add extends Opcode(13)          // a + b
  case Sub extends Opcode(14)          // a - b
  case Mul extends Opcode(15)          // a * b
  case Div extends Opcode(16)          // a / b
  case Mod extends Opcode(17)          // a % b

  // Comparison
  case Lt extends Opcode(18)           // a < b
  case Lte extends Opcode(19)          // a <= b
  case Gt extends Opcode(20)           // a > b
  case Gte extends Opcode(21)          // a >= b
  case Eq extends Opcode(22)           // a == b
  case Neq extends Opcode(23)          // a != b
  case StrictEq extends Opcode(24)     // a === b
  case StrictNeq extends Opcode(25)    // a !== b

  // Bitwise
  case And extends Opcode(26)          // a & b
  case Or extends Opcode(27)           // a | b
  case Xor extends Opcode(28)          // a ^ b
  case Shl extends Opcode(29)          // a << b
  case Sar extends Opcode(30)          // a >> b
  case Shr extends Opcode(31)          // a >>> b

  // Logical
  case LogicalAnd extends Opcode(32)   // a && b
  case LogicalOr extends Opcode(33)    // a || b

  // Control flow
  case Return extends Opcode(34)       // return value
  case ReturnUndef extends Opcode(35)  // return undefined

object Opcode:
  val Count: Int = values.length

  def fromCode(code: Int): Option[Opcode] =
    if code >= 0 && code < Count then Some(values(code)) else None
