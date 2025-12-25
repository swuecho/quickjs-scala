package quickjs.compiler

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.value.JSValue

import scala.collection.mutable

/** Minimal compiler for Phase 1.
  *
  * Compiles AST to bytecode for:
  * - Literals (numbers, booleans, undefined, null)
  * - Binary operations (arithmetic)
  */
class Compiler:
  import Compiler.*

  def compileScript(script: Script): BytecodeFunction =
    val bytecode = mutable.ArrayBuffer[Byte]()
    val constants = mutable.ArrayBuffer[AnyRef]()
    val instructions = mutable.ArrayBuffer[Instruction]()

    // Compile each statement
    for stmt <- script.body do
      compileStatement(stmt, instructions)

    // Add implicit return undefined
    instructions += Instruction.returnUndef()

    // Encode instructions to bytecode
    instructions.foreach(inst => bytecode ++= inst.encode())

    new BytecodeFunction(
      name = "<script>",
      bytecode = bytecode.toArray,
      constants = constants.toArray,
      stackSize = 256  // Fixed stack size for now
    )

  private def compileStatement(
    stmt: Statement,
    instructions: mutable.ArrayBuffer[Instruction]
  ): Unit = stmt match
    case ExpressionStatement(expr, _) =>
      compileExpression(expr, instructions)
      // Drop the result
      instructions += Instruction.drop()

    case _ =>
      throw new UnsupportedOperationException(s"Unsupported statement: $stmt")

  private def compileExpression(
    expr: Expression,
    instructions: mutable.ArrayBuffer[Instruction]
  ): Unit = expr match
    case Literal(value, _) =>
      compileLiteral(value, instructions)

    case BinaryExpression(op, left, right, _) =>
      compileExpression(left, instructions)
      compileExpression(right, instructions)
      instructions += Instruction.binary(binaryOpToOpcode(op))

    case UnaryExpression(op, argument, _, _) =>
      compileExpression(argument, instructions)
      instructions += Instruction.unary(unaryOpToOpcode(op))

    case Identifier(_, _) =>
      throw new UnsupportedOperationException("Identifiers not supported yet")

    case _ =>
      throw new UnsupportedOperationException(s"Unsupported expression: $expr")

  private def compileLiteral(
    value: JSValue,
    instructions: mutable.ArrayBuffer[Instruction]
  ): Unit = value match
    case JSValue.Undefined => instructions += Instruction.pushUndefined()
    case JSValue.Null => instructions += Instruction.pushNull()
    case JSValue.Bool(b) => if b then instructions += Instruction.pushTrue() else instructions += Instruction.pushFalse()
    case JSValue.Int32(i) => instructions += Instruction.pushI32(i)
    case JSValue.Float64(d) => instructions += Instruction.pushFloat64(d)
    case JSValue.JSStr(s) =>
      // For now, encode strings as constants (will be improved later)
      instructions += Instruction.pushI32(s.hashCode)  // Placeholder
    case _ =>
      throw new UnsupportedOperationException(s"Unsupported literal: $value")

  private def binaryOpToOpcode(op: quickjs.ast.BinaryOperator): BinaryOpcode = op match
    case quickjs.ast.BinaryOperator.Add => BinaryOpcode.Add
    case quickjs.ast.BinaryOperator.Sub => BinaryOpcode.Sub
    case quickjs.ast.BinaryOperator.Mul => BinaryOpcode.Mul
    case quickjs.ast.BinaryOperator.Div => BinaryOpcode.Div
    case quickjs.ast.BinaryOperator.Mod => BinaryOpcode.Mod
    case quickjs.ast.BinaryOperator.Lt => BinaryOpcode.Lt
    case quickjs.ast.BinaryOperator.Lte => BinaryOpcode.Lte
    case quickjs.ast.BinaryOperator.Gt => BinaryOpcode.Gt
    case quickjs.ast.BinaryOperator.Gte => BinaryOpcode.Gte
    case quickjs.ast.BinaryOperator.Eq => BinaryOpcode.Eq
    case quickjs.ast.BinaryOperator.Neq => BinaryOpcode.Neq
    case quickjs.ast.BinaryOperator.StrictEq => BinaryOpcode.StrictEq
    case quickjs.ast.BinaryOperator.StrictNeq => BinaryOpcode.StrictNeq
    case quickjs.ast.BinaryOperator.And => BinaryOpcode.And
    case quickjs.ast.BinaryOperator.Or => BinaryOpcode.Or
    case quickjs.ast.BinaryOperator.Xor => BinaryOpcode.Xor
    case quickjs.ast.BinaryOperator.LogicalAnd => BinaryOpcode.LogicalAnd
    case quickjs.ast.BinaryOperator.LogicalOr => BinaryOpcode.LogicalOr

  private def unaryOpToOpcode(op: quickjs.ast.UnaryOperator): UnaryOpcode = op match
    case quickjs.ast.UnaryOperator.Minus => UnaryOpcode.Neg
    case quickjs.ast.UnaryOperator.Not => UnaryOpcode.Not
    case quickjs.ast.UnaryOperator.BitwiseNot => UnaryOpcode.LNot
    case quickjs.ast.UnaryOperator.Typeof => throw new UnsupportedOperationException("typeof not supported yet")

object Compiler:
  def apply(): Compiler = new Compiler()
