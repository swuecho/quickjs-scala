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

  // Variable access
  case GetLoc extends Opcode(10)       // get local variable
  case PutLoc extends Opcode(11)       // set local variable
  case GetArg extends Opcode(12)       // get argument
  case PutArg extends Opcode(13)       // set argument

  // Arithmetic/logic
  case Neg extends Opcode(14)          // -x
  case Not extends Opcode(15)          // !x
  case LNot extends Opcode(16)         // ~x
  case Add extends Opcode(17)          // a + b
  case Sub extends Opcode(18)          // a - b
  case Mul extends Opcode(19)          // a * b
  case Div extends Opcode(20)          // a / b
  case Mod extends Opcode(21)          // a % b

  // Comparison
  case Lt extends Opcode(22)           // a < b
  case Lte extends Opcode(23)          // a <= b
  case Gt extends Opcode(24)           // a > b
  case Gte extends Opcode(25)          // a >= b
  case Eq extends Opcode(26)           // a == b
  case Neq extends Opcode(27)          // a != b
  case StrictEq extends Opcode(28)     // a === b
  case StrictNeq extends Opcode(29)    // a !== b

  // Bitwise
  case And extends Opcode(30)          // a & b
  case Or extends Opcode(31)           // a | b
  case Xor extends Opcode(32)          // a ^ b
  case Shl extends Opcode(33)          // a << b
  case Sar extends Opcode(34)          // a >> b
  case Shr extends Opcode(35)          // a >>> b

  // Logical
  case LogicalAnd extends Opcode(36)   // a && b
  case LogicalOr extends Opcode(37)    // a || b

  // Control flow
  case IfFalse extends Opcode(38)      // if (value === false) goto label
  case IfTrue extends Opcode(39)       // if (value === true) goto label
  case Goto extends Opcode(40)         // goto label
  case Return extends Opcode(41)       // return value
  case ReturnUndef extends Opcode(42)  // return undefined

object Opcode:
  val Count: Int = values.length

  def fromCode(code: Int): Option[Opcode] =
    if code >= 0 && code < Count then Some(values(code)) else None
