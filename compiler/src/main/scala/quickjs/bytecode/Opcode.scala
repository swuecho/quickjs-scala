package quickjs.bytecode

import scala.annotation.switch

/** Bytecode opcodes for the QuickJS-Scala virtual machine.
  *
  * Design:
  *   - Stack-based virtual machine
  *   - Compact encoding (1-5 bytes per instruction)
  *   - Type-specialized opcodes for performance
  */
enum Opcode(val code: Int) {
  // Invalid / nop
  case Invalid extends Opcode(0)
  case Nop extends Opcode(1)

  // Stack manipulation
  case PushI32 extends Opcode(2) // push 32-bit integer constant
  case PushFloat64 extends Opcode(3) // push 64-bit float constant
  case PushUndefined extends Opcode(4) // push undefined
  case PushNull extends Opcode(5) // push null
  case PushTrue extends Opcode(6) // push true
  case PushFalse extends Opcode(7) // push false

  // Stack operations
  case Drop extends Opcode(8) // drop top value
  case Dup extends Opcode(9) // a -> a a
  case Swap extends Opcode(53) // a b -> b a
  case Rotate extends Opcode(54) // a b c -> b c a (rotate top 3)
  case Dup2 extends Opcode(104) // a b -> a b a b
  case Nip extends Opcode(105) // a b -> b

  // Variable access
  case GetLoc extends Opcode(10) // get local variable
  case PutLoc extends Opcode(11) // set local variable
  case GetArg extends Opcode(12) // get argument
  case PutArg extends Opcode(13) // set argument
  case GetThis extends Opcode(71) // get 'this' value

  // Arithmetic/logic
  case Neg extends Opcode(14) // -x
  case Pos extends Opcode(107) // +x (ToNumber)
  case Not extends Opcode(15) // !x
  case LNot extends Opcode(16) // ~x
  case PreInc extends Opcode(17) // ++x (increment then return)
  case PostInc extends Opcode(18) // x++ (return then increment)
  case PreDec extends Opcode(19) // --x (decrement then return)
  case PostDec extends Opcode(20) // x-- (return then decrement)
  case Add extends Opcode(21) // a + b
  case Sub extends Opcode(22) // a - b
  case Mul extends Opcode(23) // a * b
  case Div extends Opcode(24) // a / b
  case Mod extends Opcode(25) // a % b
  case Pow extends Opcode(70) // a ** b (exponentiation)

  // Comparison
  case Lt extends Opcode(26) // a < b
  case Lte extends Opcode(27) // a <= b
  case Gt extends Opcode(28) // a > b
  case Gte extends Opcode(29) // a >= b
  case Eq extends Opcode(30) // a == b
  case Neq extends Opcode(31) // a != b
  case StrictEq extends Opcode(32) // a === b
  case StrictNeq extends Opcode(33) // a !== b

  // Bitwise
  case And extends Opcode(34) // a & b
  case Or extends Opcode(35) // a | b
  case Xor extends Opcode(36) // a ^ b
  case Shl extends Opcode(37) // a << b
  case Sar extends Opcode(38) // a >> b
  case Shr extends Opcode(39) // a >>> b

  // Logical
  case LogicalAnd extends Opcode(40) // a && b
  case LogicalOr extends Opcode(41) // a || b

  // Control flow
  case IfFalse extends Opcode(42) // if (value === false) goto label
  case IfTrue extends Opcode(43) // if (value === true) goto label
  case Goto extends Opcode(44) // goto label
  case Break extends Opcode(45) // break from loop
  case Continue extends Opcode(46) // continue to next iteration
  case Return extends Opcode(47) // return value
  case ReturnUndef extends Opcode(48) // return undefined

  // Function calls
  case Call extends Opcode(49) // call function with argc (u16 operand)
  case CallMethod
      extends Opcode(
        64
      ) // call method with argc (u16 operand), 'this' is on stack
  case New extends Opcode(69) // new constructor with argc (u16 operand)

  // Objects and properties
  case NewObject extends Opcode(50) // create new object
  case GetProp extends Opcode(51) // get property (string name)
  case SetProp extends Opcode(52) // set property (string name)

  // Constants
  case GetConst extends Opcode(57) // get from constants table (i32 index)

  // Global scope
  case GetGlobal extends Opcode(56) // get from global scope (string name)
  case GetGlobalOrUndefined
      extends Opcode(
        93
      ) // get global/closure binding, or undefined if unresolved
  case PutGlobal
      extends Opcode(
        72
      ) // set variable in global scope (string name, stack has value)
  case DefVar
      extends Opcode(62) // define variable in global scope (string name)
  case DefFun
      extends Opcode(63) // define function in global scope (string name)

  // Scope management (for let/const block scoping)
  case EnterScope
      extends Opcode(74) // enter a new block scope (u16 operand = scope index)
  case LeaveScope
      extends Opcode(
        75
      ) // leave current block scope (u16 operand = scope index)

  // TDZ (Temporal Dead Zone) and const enforcement
  case SetLocUninitialized
      extends Opcode(
        76
      ) // mark local variable as uninitialized (TDZ) - u16 operand = var index
  case GetLocCheck
      extends Opcode(
        77
      ) // get local variable with TDZ check - u16 operand = var index
  case SetLocConst
      extends Opcode(
        83
      ) // mark local variable as const - u16 operand = var index
  case PushWith extends Opcode(84) // push with object
  case PopWith extends Opcode(85) // pop with object

  // Arrays
  case NewArray extends Opcode(58) // create new array with size (i32 operand)
  case GetElem extends Opcode(59) // get array element by index (computed)
  case SetElem
      extends Opcode(60) // set array element by index (computed, returns value)
  case InitElem
      extends Opcode(
        61
      ) // initialize array element (returns array, for literals)

  // Type operators
  case Typeof extends Opcode(65) // typeof operator - get type string
  case Delete extends Opcode(66) // delete operator - delete property
  case Instanceof
      extends Opcode(67) // instanceof operator - check prototype chain
  case In extends Opcode(68) // in operator - check if property exists

  // Comma operator
  case Comma
      extends Opcode(73) // comma operator: eval a, discard, eval b, return b

  // Exceptions
  case TryStart extends Opcode(78) // push try handler (catch pc, finally pc)
  case TryEnd extends Opcode(79) // pop try handler
  case Throw extends Opcode(80) // throw exception (value on stack)
  case GetException extends Opcode(81) // push last exception value
  case RethrowIfPending
      extends Opcode(82) // rethrow pending exception after finally

  // Generators
  case InitialYield extends Opcode(86) // first yield to return generator object
  case Yield
      extends Opcode(87) // suspend and yield value -> returns {value, done}
  case YieldStar extends Opcode(88) // delegate to another iterator (yield*)

  // Async functions
  case Await extends Opcode(89) // suspend until Promise resolves

  // Private class fields
  case GetPrivateField extends Opcode(90) // get private field (string name)
  case SetPrivateField extends Opcode(91) // set private field (string name)
  case DefinePrivateField
      extends Opcode(92) // define private field (string name)
  case GetRestArgs extends Opcode(106) // collect trailing arguments into an Array

  // `with` statement reference semantics: compound assignments and update
  // expressions must use the same object environment record base for the get
  // and the put, even if the binding is deleted in between.
  case GetGlobalWithBase extends Opcode(109) // value then base token
  case PutGlobalWithBase extends Opcode(108) // base token then value

  // Derived constructor `this` handling
  case GetThisUnchecked extends Opcode(110) // push this even before super()
  case MarkThisInitialized extends Opcode(111) // super() has initialized this
}

object Opcode {
  val Count: Int = values.length

  /** O(1) lookup table from opcode byte to Opcode enum value. Index 0..MaxCode
    * maps to the corresponding Opcode, or null for unused slots.
    */
  private val MaxCode: Int = values.map(_.code).max
  val lookup: Array[Opcode | Null] = {
    val arr = new Array[Opcode | Null](MaxCode + 1)
    values.foreach(op => arr(op.code) = op)
    arr
  }

  def fromCode(code: Int): Option[Opcode] =
    if code >= 0 && code < lookup.length then Option(lookup(code))
    else None
}
