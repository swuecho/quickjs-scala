package quickjs.compiler

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.value.JSValue

import scala.collection.mutable

/** Minimal compiler for Phase 2.
  *
  * Compiles AST to bytecode for:
  * - Literals (numbers, booleans, undefined, null)
  * - Binary operations (arithmetic)
  * - Variable declarations (var, let, const)
  * - Block statements
  * - If/else statements
  * - While loops
  * - Function declarations
  * - Function calls
  * - Return statements
  */
class Compiler:
  import Compiler.*

  // Scope for variable tracking
  private class Scope(val parent: Scope | Null):
    private val vars = mutable.HashMap[String, Int]()
    private var nextIndex = 0

    def declare(name: String): Int =
      if vars.contains(name) then
        vars(name)
      else
        val idx = nextIndex
        vars(name) = idx
        nextIndex += 1
        idx

    def lookup(name: String): Option[Int] =
      vars.get(name).orElse {
        if parent != null then parent.lookup(name) else None
      }

  // Current compilation scope
  private var currentScope: Scope = new Scope(null)

  /** Compile a function body to bytecode */
  private def compileFunctionBody(
    name: String,
    params: scala.collection.immutable.Seq[Identifier],
    body: Statement
  ): BytecodeFunction =
    // Create a new scope for the function (with parent as current scope for closures)
    val oldScope = currentScope
    currentScope = new Scope(currentScope)

    // Declare parameters as local variables
    for param <- params do
      currentScope.declare(param.name)

    val bytecode = mutable.ArrayBuffer[Byte]()
    val constants = mutable.ArrayBuffer[AnyRef]()
    val instructions = mutable.ArrayBuffer[Instruction]()

    // Compile the function body
    body match
      case block: BlockStatement =>
        // Compile each statement in the block
        for s <- block.statements do
          compileStatement(s, instructions, constants)
      case _ =>
        // Single statement body
        compileStatement(body, instructions, constants)

    // Add implicit return undefined
    instructions += Instruction.returnUndef()

    // Encode instructions to bytecode
    instructions.foreach(inst => bytecode ++= inst.encode())

    // Restore the parent scope
    currentScope = oldScope

    new BytecodeFunction(
      name = name,
      bytecode = bytecode.toArray,
      constants = constants.toArray,
      stackSize = 256  // Fixed stack size for now
    )

  def compileScript(script: Script): BytecodeFunction =
    val bytecode = mutable.ArrayBuffer[Byte]()
    val constants = mutable.ArrayBuffer[AnyRef]()
    val instructions = mutable.ArrayBuffer[Instruction]()

    // Compile each statement
    for stmt <- script.body do
      compileStatement(stmt, instructions, constants)

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
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = stmt match
    case ExpressionStatement(expr, _) =>
      compileExpression(expr, instructions)
      // Drop the result
      instructions += Instruction.drop()

    case VariableDeclaration(kind, declarations, _) =>
      for decl <- declarations do
        compileVariableDeclarator(decl, instructions)

    case BlockStatement(stmts, _) =>
      // Compile each statement in the block
      for s <- stmts do
        compileStatement(s, instructions, constants)

    case IfStatement(test, consequent, alternate, _) =>
      // Compile test
      compileExpression(test, instructions)

      // Reserve space for jump offset (track byte position, not instruction index)
      val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
      instructions += Instruction.ifFalse(0)  // Placeholder
      val ifFalseIdx = instructions.length - 1

      // Compile consequent
      compileStatement(consequent, instructions, constants)

      if alternate != null then
        // If we took the consequent, skip the alternate
        val jumpBytePos = instructions.foldLeft(0)(_ + _.size)
        instructions += Instruction.goto(0)  // Placeholder
        val gotoIdx = instructions.length - 1

        // Update the ifFalse jump to skip to after alternate (in bytes)
        val consequentEndBytePos = instructions.foldLeft(0)(_ + _.size)
        val ifFalseOffset = consequentEndBytePos - jumpIfFalseBytePos - 1
        instructions(ifFalseIdx) = Instruction.ifFalse(ifFalseOffset)

        // Compile alternate
        compileStatement(alternate, instructions, constants)

        // Update the jump to skip over alternate (in bytes)
        val alternateEndBytePos = instructions.foldLeft(0)(_ + _.size)
        val gotoOffset = alternateEndBytePos - jumpBytePos - 1
        instructions(gotoIdx) = Instruction.goto(gotoOffset)
      else
        // No alternate - just update the ifFalse jump (in bytes)
        val endBytePos = instructions.foldLeft(0)(_ + _.size)
        val ifFalseOffset = endBytePos - jumpIfFalseBytePos - 1
        instructions(ifFalseIdx) = Instruction.ifFalse(ifFalseOffset)

    case WhileStatement(test, body, _) =>
      val loopStartBytePos = instructions.foldLeft(0)(_ + _.size)

      // Compile test
      compileExpression(test, instructions)

      // Jump out if false
      val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
      instructions += Instruction.ifFalse(0)  // Placeholder
      val ifFalseIdx = instructions.length - 1

      // Compile body
      compileStatement(body, instructions, constants)

      // Jump back to loop start
      val currentBytePos = instructions.foldLeft(0)(_ + _.size)
      val backJumpOffset = loopStartBytePos - currentBytePos - 1
      instructions += Instruction.goto(backJumpOffset)

      // Update the ifFalse jump to exit loop
      val exitBytePos = instructions.foldLeft(0)(_ + _.size)
      val ifFalseOffset = exitBytePos - jumpIfFalseBytePos - 1
      instructions(ifFalseIdx) = Instruction.ifFalse(ifFalseOffset)

    case ForStatement(init, test, update, body, _) =>
      // Compile init (if present)
      if init != null then
        init match
          case decl: VariableDeclaration =>
            compileStatement(decl, instructions, constants)
          case expr: Expression =>
            compileExpression(expr, instructions)
            instructions += Instruction.drop()

      // Start of loop (before test)
      val loopStartBytePos = instructions.foldLeft(0)(_ + _.size)

      // Compile test (if present)
      if test != null then
        compileExpression(test, instructions)
      else
        // No test means always true - push true
        instructions += Instruction.pushTrue()

      // Jump out if false
      val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
      instructions += Instruction.ifFalse(0)  // Placeholder
      val ifFalseIdx = instructions.length - 1

      // Compile body
      compileStatement(body, instructions, constants)

      // Compile update (if present)
      if update != null then
        compileExpression(update, instructions)
        instructions += Instruction.drop()

      // Jump back to test
      val currentBytePos = instructions.foldLeft(0)(_ + _.size)
      val backJumpOffset = loopStartBytePos - currentBytePos - 1
      instructions += Instruction.goto(backJumpOffset)

      // Update the ifFalse jump to exit loop
      val exitBytePos = instructions.foldLeft(0)(_ + _.size)
      val ifFalseOffset = exitBytePos - jumpIfFalseBytePos - 1
      instructions(ifFalseIdx) = Instruction.ifFalse(ifFalseOffset)

    case FunctionDeclaration(id, params, body, _, _, _) =>
      // Compile the function body to bytecode
      val funcBytecode = compileFunctionBody(id.name, params, body)

      // Create a JSValue.Function with the compiled bytecode
      val funcValue = JSValue.Function(
        name = id.name,
        bytecode = funcBytecode.bytecode,
        constants = funcBytecode.constants,
        stackSize = funcBytecode.stackSize
      )

      // Add the function to the constants array
      val constIndex = constants.length
      constants += funcValue

      // Push the function from constants, then store it in global scope
      instructions += Instruction.getConst(constIndex)
      instructions += Instruction.defFun(id.name)

    case ReturnStatement(argument, _) =>
      if argument != null then
        compileExpression(argument, instructions)
        instructions += Instruction.returnInst()
      else
        instructions += Instruction.returnUndef()

    case BreakStatement(label, _) =>
      // For now, ignore labels (will need to implement for nested loops)
      instructions += Instruction.breakInst()

    case ContinueStatement(label, _) =>
      // For now, ignore labels (will need to implement for nested loops)
      instructions += Instruction.continueInst()

    case _ =>
      throw new UnsupportedOperationException(s"Unsupported statement: $stmt")

  private def compileVariableDeclarator(
    decl: VariableDeclarator,
    instructions: mutable.ArrayBuffer[Instruction]
  ): Unit =
    val index = currentScope.declare(decl.id.name)

    if decl.init != null then
      compileExpression(decl.init, instructions)
      instructions += Instruction.putLoc(index)
    else
      // Initialize to undefined
      instructions += Instruction.pushUndefined()
      instructions += Instruction.putLoc(index)

  private def compileExpression(
    expr: Expression,
    instructions: mutable.ArrayBuffer[Instruction]
  ): Unit = expr match
    case Literal(value, _) =>
      compileLiteral(value, instructions)

    case Identifier(name, _) =>
      // Look up variable in scope
      currentScope.lookup(name) match
        case Some(index) =>
          instructions += Instruction.getLoc(index)
        case None =>
          // Check if it's a global variable or function
          // For now, we'll emit a special instruction to get from global scope
          // In the future, this should be resolved at compile time
          instructions += Instruction.getGlobal(name)

    case BinaryExpression(op, left, right, _) =>
      compileExpression(left, instructions)
      compileExpression(right, instructions)
      instructions += Instruction.binary(binaryOpToOpcode(op))

    case UnaryExpression(op, argument, _, _) =>
      // Increment/decrement operators need special handling for identifiers
      (op, argument) match
        case (UnaryOperator.PreInc | UnaryOperator.PostInc | UnaryOperator.PreDec | UnaryOperator.PostDec, id: Identifier) =>
          // For increment/decrement on identifiers, we need to:
          // 1. Get the variable
          // 2. Perform the operation
          // 3. Store it back
          currentScope.lookup(id.name) match
            case Some(index) =>
              if op == UnaryOperator.PreInc then
                // PreInc: GetLoc, PreInc (modifies value), Dup, PutLoc
                instructions += Instruction.getLoc(index)
                instructions += Instruction.unary(UnaryOpcode.PreInc)
                instructions += Instruction.dup()
                instructions += Instruction.putLoc(index)
              else if op == UnaryOperator.PostInc then
                // PostInc: GetLoc, PostInc (leaves [x, x+1]), PutLoc (stores x+1, leaves [x]), Drop (removes x)
                instructions += Instruction.getLoc(index)
                instructions += Instruction.unary(UnaryOpcode.PostInc)
                instructions += Instruction.putLoc(index)  // Stores x+1, removes it
                instructions += Instruction.drop()         // Remove original x
              else if op == UnaryOperator.PreDec then
                // PreDec: GetLoc, PreDec, Dup, PutLoc
                instructions += Instruction.getLoc(index)
                instructions += Instruction.unary(UnaryOpcode.PreDec)
                instructions += Instruction.dup()
                instructions += Instruction.putLoc(index)
              else // PostDec
                // PostDec: GetLoc, PostDec (leaves [x, x-1]), PutLoc, Drop
                instructions += Instruction.getLoc(index)
                instructions += Instruction.unary(UnaryOpcode.PostDec)
                instructions += Instruction.putLoc(index)
                instructions += Instruction.drop()
            case None =>
              throw new RuntimeException(s"Undefined variable: ${id.name}")
        case _ =>
          // For other unary operators, use the standard path
          compileExpression(argument, instructions)
          if op != UnaryOperator.Plus then
            instructions += Instruction.unary(unaryOpToOpcode(op))
          // UnaryPlus is a no-op (just coerces to number, which happens automatically)

    case CallExpression(callee, arguments, _) =>
      // Compile callee and arguments in reverse order
      // Stack layout after compilation: [callee, arg1, arg2, ..., argN]
      compileExpression(callee, instructions)
      for arg <- arguments do
        compileExpression(arg, instructions)

      // Emit call instruction with argument count
      instructions += Instruction.call(arguments.length)

    case FunctionExpression(id, params, body, _, _, _) =>
      // TODO: Implement function expressions
      // For now, just push undefined as placeholder
      instructions += Instruction.pushUndefined()

    case AssignmentExpression(left, right, _) =>
      // Compile the right side first
      compileExpression(right, instructions)

      // For assignment to identifier, store it
      // Assignment returns the value, so we need to keep it on the stack
      left match
        case Identifier(name, _) =>
          currentScope.lookup(name) match
            case Some(index) =>
              // Duplicate the value so we can keep one on stack and store one
              instructions += Instruction.dup()
              instructions += Instruction.putLoc(index)
            case None =>
              throw new RuntimeException(s"Undefined variable: $name")
        case MemberExpression(obj, prop, computed, _) =>
          // For member assignment: obj.prop = value
          // Stack layout: [value, obj] (value is already on stack from right side)
          compileExpression(obj, instructions)
          // Now stack is: [value, obj]
          // Need to swap to get: [obj, value]
          instructions += Instruction.swap()
          // Get property name
          val propName = prop match
            case Identifier(name, _) => name
            case _ => throw new UnsupportedOperationException(s"Computed property names not supported yet")
          // Set property: [obj, value] -> obj.prop = value, [value]
          instructions += Instruction.setProp(propName)
        case _ =>
          throw new UnsupportedOperationException(s"Unsupported assignment target: $left")

    case ObjectLiteral(properties, _) =>
      // Create a new object
      instructions += Instruction.newObject()

      // Set each property
      for prop <- properties do
        // Duplicate the object reference
        instructions += Instruction.dup()

        // Compile the property value
        compileExpression(prop.value, instructions)

        // Get property name
        val propName = prop.key match
          case Identifier(name, _) => name
          case s: String => s
          case _ => throw new UnsupportedOperationException(s"Unsupported property key: ${prop.key}")

        // Set property (pops value, leaves object on stack)
        instructions += Instruction.setProp(propName)

    case ArrayLiteral(elements, _) =>
      // TODO: Implement array literals
      // For now, push undefined as placeholder
      instructions += Instruction.pushUndefined()

    case MemberExpression(obj, prop, computed, _) =>
      // Compile the object
      compileExpression(obj, instructions)

      // Get the property name
      val propName = prop match
        case Identifier(name, _) => name
        case _ => throw new UnsupportedOperationException(s"Computed property names not supported yet")

      // Get property
      instructions += Instruction.getProp(propName)

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
    case BinaryOperator.Add => BinaryOpcode.Add
    case BinaryOperator.Sub => BinaryOpcode.Sub
    case BinaryOperator.Mul => BinaryOpcode.Mul
    case BinaryOperator.Div => BinaryOpcode.Div
    case BinaryOperator.Mod => BinaryOpcode.Mod
    case BinaryOperator.Lt => BinaryOpcode.Lt
    case BinaryOperator.Lte => BinaryOpcode.Lte
    case BinaryOperator.Gt => BinaryOpcode.Gt
    case BinaryOperator.Gte => BinaryOpcode.Gte
    case BinaryOperator.Eq => BinaryOpcode.Eq
    case BinaryOperator.Neq => BinaryOpcode.Neq
    case BinaryOperator.StrictEq => BinaryOpcode.StrictEq
    case BinaryOperator.StrictNeq => BinaryOpcode.StrictNeq
    case BinaryOperator.And => BinaryOpcode.And
    case BinaryOperator.Or => BinaryOpcode.Or
    case BinaryOperator.Xor => BinaryOpcode.Xor
    case BinaryOperator.Shl => BinaryOpcode.Shl
    case BinaryOperator.Sar => BinaryOpcode.Sar
    case BinaryOperator.Shr => BinaryOpcode.Shr
    case BinaryOperator.LogicalAnd => BinaryOpcode.LogicalAnd
    case BinaryOperator.LogicalOr => BinaryOpcode.LogicalOr

  private def unaryOpToOpcode(op: quickjs.ast.UnaryOperator): UnaryOpcode = (op: @unchecked) match
    case UnaryOperator.Minus => UnaryOpcode.Neg
    case UnaryOperator.Not => UnaryOpcode.Not
    case UnaryOperator.BitwiseNot => UnaryOpcode.LNot
    case UnaryOperator.PreInc => UnaryOpcode.PreInc
    case UnaryOperator.PostInc => UnaryOpcode.PostInc
    case UnaryOperator.PreDec => UnaryOpcode.PreDec
    case UnaryOperator.PostDec => UnaryOpcode.PostDec
    case UnaryOperator.Typeof => throw new UnsupportedOperationException("typeof not supported yet")
    case UnaryOperator.Plus => UnaryOpcode.Neg  // UnaryPlus is a no-op, but we'll treat it as Neg for now (should be proper coercion)

object Compiler:
  def apply(): Compiler = new Compiler()