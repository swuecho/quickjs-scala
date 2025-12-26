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

  // REPL mode flag
  private var replMode: Boolean = false

  /** Compile in REPL mode (don't drop last expression) */
  def withREPLMode(compilation: => BytecodeFunction): BytecodeFunction =
    val oldMode = replMode
    replMode = true
    try compilation
    finally replMode = oldMode

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

    /** Check if variable is in this scope only (not parent scopes) */
    def isLocal(name: String): Boolean =
      vars.contains(name)

  // Current compilation scope
  private var currentScope: Scope = new Scope(null)

  /** Find all free variables in an expression, including those in nested function expressions (for closure analysis) */
  private def findFreeVariablesForClosure(expr: Expression): Set[String] = expr match
    case Identifier(name, _) => Set(name)
    case Literal(_, _) => Set.empty
    case BinaryExpression(_, left, right, _) =>
      findFreeVariablesForClosure(left) ++ findFreeVariablesForClosure(right)
    case UnaryExpression(_, argument, _, _) =>
      findFreeVariablesForClosure(argument)
    case CallExpression(callee, arguments, _) =>
      findFreeVariablesForClosure(callee) ++ arguments.flatMap(findFreeVariablesForClosure).toSet
    case MemberExpression(obj, prop, computed, _) =>
      findFreeVariablesForClosure(obj) ++ (if computed then findFreeVariablesForClosure(prop) else Set.empty)
    case AssignmentExpression(left, right, _) =>
      findFreeVariablesForClosure(left) ++ findFreeVariablesForClosure(right)
    case FunctionExpression(_, params, body, _, _, _) =>
      // For closure analysis, we need to look inside the function body
      // Find free variables in the body, excluding the function's own parameters
      val paramNames = params.map(_.name).toSet
      val bodyFree = findFreeVariablesForClosure(body)
      val localVars = findDeclaredVariables(body)
      // Exclude parameters and local variables - only return true free vars
      bodyFree -- paramNames -- localVars
    case ObjectLiteral(properties, _) =>
      properties.flatMap(p => findFreeVariablesForClosure(p.value)).toSet
    case ArrayLiteral(elements, _) =>
      elements.flatMap(findFreeVariablesForClosure).toSet
    case _ => Set.empty

  /** Find all free variables in an expression */
  private def findFreeVariables(expr: Expression): Set[String] = expr match
    case Identifier(name, _) => Set(name)
    case Literal(_, _) => Set.empty
    case BinaryExpression(_, left, right, _) =>
      findFreeVariables(left) ++ findFreeVariables(right)
    case UnaryExpression(_, argument, _, _) =>
      findFreeVariables(argument)
    case CallExpression(callee, arguments, _) =>
      findFreeVariables(callee) ++ arguments.flatMap(findFreeVariables).toSet
    case MemberExpression(obj, prop, computed, _) =>
      findFreeVariables(obj) ++ (if computed then findFreeVariables(prop) else Set.empty)
    case AssignmentExpression(left, right, _) =>
      findFreeVariables(left) ++ findFreeVariables(right)
    case FunctionExpression(_, _, _, _, _, _) =>
      // Function expressions create their own scope, so they don't directly
      // expose free variables from their body to the containing scope
      Set.empty
    case ObjectLiteral(properties, _) =>
      properties.flatMap(p => findFreeVariables(p.value)).toSet
    case ArrayLiteral(elements, _) =>
      elements.flatMap(findFreeVariables).toSet
    case _ => Set.empty

  /** Find all variables declared in a statement */
  private def findDeclaredVariables(stmt: Statement): Set[String] = stmt match
    case VariableDeclaration(_, declarations, _) =>
      declarations.map(_.id.name).toSet
    case BlockStatement(statements, _) =>
      statements.flatMap(findDeclaredVariables).toSet
    case IfStatement(test, consequent, alternate, _) =>
      findDeclaredVariables(consequent) ++
        (if alternate != null then findDeclaredVariables(alternate) else Set.empty)
    case WhileStatement(test, body, _) =>
      findDeclaredVariables(body)
    case ForStatement(init, test, update, body, _) =>
      val initDeclared = init match
        case vd: VariableDeclaration => findDeclaredVariables(vd)
        case _ => Set.empty
      initDeclared ++ findDeclaredVariables(body)
    case _ => Set.empty

  /** Find all free variables in a statement, including those in nested function expressions (for closure analysis) */
  private def findFreeVariablesForClosure(stmt: Statement): Set[String] = stmt match
    case ExpressionStatement(expr, _) => findFreeVariablesForClosure(expr)
    case VariableDeclaration(_, declarations, _) =>
      declarations.flatMap { d =>
        val initFree = if d.init != null then findFreeVariablesForClosure(d.init) else Set.empty
        // Exclude the variable being declared from free variables
        initFree - d.id.name
      }.toSet
    case BlockStatement(statements, _) =>
      statements.flatMap(findFreeVariablesForClosure).toSet
    case IfStatement(test, consequent, alternate, _) =>
      findFreeVariablesForClosure(test) ++ findFreeVariablesForClosure(consequent) ++
        (if alternate != null then findFreeVariablesForClosure(alternate) else Set.empty)
    case WhileStatement(test, body, _) =>
      findFreeVariablesForClosure(test) ++ findFreeVariablesForClosure(body)
    case ForStatement(init, test, update, body, _) =>
      val initFree = init match
        case e: Expression => findFreeVariablesForClosure(e)
        case s: Statement => findFreeVariablesForClosure(s)
        case null => Set.empty
      initFree ++ findFreeVariablesForClosure(test) ++
        findFreeVariablesForClosure(update) ++ findFreeVariablesForClosure(body)
    case FunctionDeclaration(_, _, _, _, _, _) =>
      // Function declarations create their own scope
      Set.empty
    case ReturnStatement(argument, _) =>
      if argument != null then findFreeVariablesForClosure(argument) else Set.empty
    case _ => Set.empty

  /** Find all free variables in a statement */
  private def findFreeVariables(stmt: Statement): Set[String] = stmt match
    case ExpressionStatement(expr, _) => findFreeVariables(expr)
    case VariableDeclaration(_, declarations, _) =>
      declarations.flatMap { d =>
        val initFree = if d.init != null then findFreeVariables(d.init) else Set.empty
        // Exclude the variable being declared from free variables
        initFree - d.id.name
      }.toSet
    case BlockStatement(statements, _) =>
      statements.flatMap(findFreeVariables).toSet
    case IfStatement(test, consequent, alternate, _) =>
      findFreeVariables(test) ++ findFreeVariables(consequent) ++
        (if alternate != null then findFreeVariables(alternate) else Set.empty)
    case WhileStatement(test, body, _) =>
      findFreeVariables(test) ++ findFreeVariables(body)
    case ForStatement(init, test, update, body, _) =>
      val initFree = init match
        case e: Expression => findFreeVariables(e)
        case s: Statement => findFreeVariables(s)
        case null => Set.empty
      initFree ++ findFreeVariables(test) ++
        findFreeVariables(update) ++ findFreeVariables(body)
    case FunctionDeclaration(_, _, _, _, _, _) =>
      // Function declarations create their own scope
      Set.empty
    case ReturnStatement(argument, _) =>
      if argument != null then findFreeVariables(argument) else Set.empty
    case _ => Set.empty

  /** Compile a function body to bytecode */
  private def compileFunctionBody(
    name: String,
    params: scala.collection.immutable.Seq[Identifier],
    body: Statement
  ): BytecodeFunction =
    // Create a new scope for the function (with parent as current scope for closures)
    val oldScope = currentScope
    currentScope = new Scope(currentScope)

    // Collect variables declared in this function (params and locals)
    val declaredVars = mutable.Set[String]()
    val paramNamesList = mutable.ArrayBuffer[String]()
    for param <- params do
      declaredVars += param.name
      paramNamesList += param.name
      currentScope.declare(param.name)

    // Also collect local variable declarations
    val localVars = findDeclaredVariables(body)
    declaredVars ++= localVars

    val bytecode = mutable.ArrayBuffer[Byte]()
    val constants = mutable.ArrayBuffer[AnyRef]()
    val instructions = mutable.ArrayBuffer[Instruction]()

    // Compile the function body
    body match
      case block: BlockStatement =>
        // Compile each statement in the block
        for s <- block.statements do
          compileStatement(s, instructions, constants, false)
      case _ =>
        // Single statement body
        compileStatement(body, instructions, constants, false)

    // Add implicit return undefined
    instructions += Instruction.returnUndef()

    // Encode instructions to bytecode
    instructions.foreach(inst => bytecode ++= inst.encode())

    // Find free variables (referenced but not declared in this function)
    // Use findFreeVariablesForClosure to look inside nested function expressions
    val allFreeVars = findFreeVariablesForClosure(body)
    val freeVarNames = allFreeVars.filterNot(declaredVars.contains).toArray

    // Restore the parent scope
    currentScope = oldScope

    new BytecodeFunction(
      name = name,
      bytecode = bytecode.toArray,
      constants = constants.toArray,
      stackSize = 256,
      freeVars = freeVarNames,
      paramNames = paramNamesList.toArray
    )

  def compileScript(script: Script): BytecodeFunction =
    // Reset scope for each script compilation (fixes test isolation issues)
    currentScope = new Scope(null)

    val bytecode = mutable.ArrayBuffer[Byte]()
    val constants = mutable.ArrayBuffer[AnyRef]()
    val instructions = mutable.ArrayBuffer[Instruction]()

    // Compile each statement
    for (stmt, index) <- script.body.zipWithIndex do
      val isLast = index == script.body.length - 1
      compileStatement(stmt, instructions, constants, isLast && replMode)

    // Add implicit return undefined (unless last expression already returns value)
    if script.body.isEmpty || !(script.body.last.isInstanceOf[ExpressionStatement] && replMode) then
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
    constants: mutable.ArrayBuffer[AnyRef],
    isLastREPLExpression: Boolean = false
  ): Unit = stmt match
    case ExpressionStatement(expr, _) =>
      compileExpression(expr, instructions, constants)
      // In REPL mode, don't drop the last expression's result
      if !isLastREPLExpression then
        instructions += Instruction.drop()
      else
        // Last expression in REPL mode: keep value on stack and return it
        instructions += Instruction.returnInst()

    case VariableDeclaration(kind, declarations, _) =>
      for decl <- declarations do
        compileVariableDeclarator(decl, instructions, constants)

    case BlockStatement(stmts, _) =>
      // Compile each statement in the block
      for s <- stmts do
        compileStatement(s, instructions, constants, false)

    case IfStatement(test, consequent, alternate, _) =>
      // Compile test
      compileExpression(test, instructions, constants)

      // Reserve space for jump offset (track byte position, not instruction index)
      val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
      instructions += Instruction.ifFalse(0)  // Placeholder
      val ifFalseIdx = instructions.length - 1

      // Compile consequent
      compileStatement(consequent, instructions, constants, false)

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
        compileStatement(alternate, instructions, constants, false)

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
      compileExpression(test, instructions, constants)

      // Jump out if false
      val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
      instructions += Instruction.ifFalse(0)  // Placeholder
      val ifFalseIdx = instructions.length - 1

      // Compile body
      compileStatement(body, instructions, constants, false)

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
            compileStatement(decl, instructions, constants, false)
          case expr: Expression =>
            compileExpression(expr, instructions, constants)
            instructions += Instruction.drop()

      // Start of loop (before test)
      val loopStartBytePos = instructions.foldLeft(0)(_ + _.size)

      // Compile test (if present)
      if test != null then
        compileExpression(test, instructions, constants)
      else
        // No test means always true - push true
        instructions += Instruction.pushTrue()

      // Jump out if false
      val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
      instructions += Instruction.ifFalse(0)  // Placeholder
      val ifFalseIdx = instructions.length - 1

      // Compile body
      compileStatement(body, instructions, constants, false)

      // Compile update (if present)
      if update != null then
        compileExpression(update, instructions, constants)
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

      // Store BytecodeFunction in constants array (will be converted to JSValue.Function at runtime with closure)
      val constIndex = constants.length
      constants += funcBytecode

      // Push the function from constants, then store it in global scope
      instructions += Instruction.getConst(constIndex)
      instructions += Instruction.defFun(id.name)

    case ReturnStatement(argument, _) =>
      if argument != null then
        compileExpression(argument, instructions, constants)
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
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    // Check if we're at the top level (script scope)
    val isTopLevel = currentScope.parent == null

    if isTopLevel then
      // Top-level variables go to BOTH local scope AND global scope
      // Declare in local scope (for increment/decrement to find it)
      val index = currentScope.declare(decl.id.name)

      if decl.init != null then
        compileExpression(decl.init, instructions, constants)
        // Duplicate for both local and global storage
        instructions += Instruction.dup()
        // Store one copy in local scope
        instructions += Instruction.putLoc(index)
        // Other copy remains for global scope storage
      else
        // Push undefined for both scopes
        instructions += Instruction.pushUndefined()
        instructions += Instruction.dup()
        // Store one in local scope
        instructions += Instruction.putLoc(index)
        // Other copy remains for global scope storage

      // Store in global scope (for persistence across evaluations)
      instructions += Instruction.defVar(decl.id.name)
    else
      // Local variables in functions only
      val index = currentScope.declare(decl.id.name)

      if decl.init != null then
        compileExpression(decl.init, instructions, constants)
        instructions += Instruction.putLoc(index)
      else
        // Initialize to undefined
        instructions += Instruction.pushUndefined()
        instructions += Instruction.putLoc(index)

  private def compileExpression(
    expr: Expression,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = expr match
    case Literal(value, _) =>
      compileLiteral(value, instructions, constants)

    case Identifier(name, _) =>
      // Look up variable in scope
      // Only use GetLoc for variables in the current function's immediate scope
      // For variables from outer scopes (closures), use GetGlobal which checks the closure at runtime
      if currentScope.isLocal(name) then
        val index = currentScope.lookup(name).get
        instructions += Instruction.getLoc(index)
      else
        // Variable is from outer scope or global - use GetGlobal
        // GetGlobal checks the closure first, then global scope
        instructions += Instruction.getGlobal(name)

    case BinaryExpression(op, left, right, _) =>
      compileExpression(left, instructions, constants)
      compileExpression(right, instructions, constants)
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
          compileExpression(argument, instructions, constants)
          if op != UnaryOperator.Plus then
            instructions += Instruction.unary(unaryOpToOpcode(op))
          // UnaryPlus is a no-op (just coerces to number, which happens automatically)

    case CallExpression(callee, arguments, _) =>
      // Check if this is a method call (callee is a MemberExpression)
      callee match
        case memberExpr: MemberExpression =>
          // Method call: obj.method(arg1, arg2, ...)
          // Stack layout should be: [this, func, arg1, arg2, ..., argN]

          // Compile the object part (for 'this' binding)
          val obj = memberExpr.obj
          compileExpression(obj, instructions, constants)
          // Stack now: [obj]

          // Get the method from the object
          compileExpression(memberExpr, instructions, constants)
          // Stack now: [obj, method]

          // Compile arguments
          for arg <- arguments do
            compileExpression(arg, instructions, constants)
          // Stack now: [obj, method, arg1, arg2, ..., argN]

          // Emit CallMethod instruction
          instructions += Instruction.callMethod(arguments.length)

        case _ =>
          // Regular function call: func(arg1, arg2, ...)
          // Stack layout: [func, arg1, arg2, ..., argN]
          compileExpression(callee, instructions, constants)
          for arg <- arguments do
            compileExpression(arg, instructions, constants)

          // Emit Call instruction with argument count
          instructions += Instruction.call(arguments.length)

    case FunctionExpression(id, params, body, _, _, _) =>
      // Compile function expression to bytecode
      val funcName = id match
        case Identifier(name, _) => name
        case null => "<anonymous>"

      val funcBytecode = compileFunctionBody(funcName, params, body)

      // Store BytecodeFunction in constants array (will be converted to JSValue.Function at runtime with closure)
      val constIndex = constants.length
      constants += funcBytecode

      // Push the function value onto the stack
      instructions += Instruction.getConst(constIndex)

      // For named function expressions, also define the function in global scope
      // This allows recursive calls (e.g., function fact(n) { return fact(n-1); })
      if id != null then
        instructions += Instruction.dup()  // Duplicate for DefFun
        instructions += Instruction.defFun(funcName)

    case AssignmentExpression(left, right, _) =>
      // Compile the right side first
      compileExpression(right, instructions, constants)

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
          if computed then
            // For computed member assignment: obj[prop] = value
            // Stack layout: [value] (from right side)
            compileExpression(obj, instructions, constants)
            // Now stack is: [value, obj]
            // Need to swap to get: [obj, value]
            instructions += Instruction.swap()
            // Compile the property expression
            compileExpression(prop, instructions, constants)
            // Now stack is: [obj, value, prop]
            // Need to swap value and prop to get: [obj, prop, value]
            instructions += Instruction.swap()
            // Set element: [obj, prop, value] -> obj[prop] = value, [value]
            instructions += Instruction.setElem()
          else
            // For member assignment: obj.prop = value
            // Stack layout: [value, obj] (value is already on stack from right side)
            compileExpression(obj, instructions, constants)
            // Now stack is: [value, obj]
            // Need to swap to get: [obj, value]
            instructions += Instruction.swap()
            // Get property name
            val propName = prop match
              case Identifier(name, _) => name
              case _ => throw new UnsupportedOperationException(s"Unsupported property key: $prop")
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
        compileExpression(prop.value, instructions, constants)

        // Get property name
        val propName = prop.key match
          case Identifier(name, _) => name
          case s: String => s
          case _ => throw new UnsupportedOperationException(s"Unsupported property key: ${prop.key}")

        // Set property (pops value, leaves object on stack)
        instructions += Instruction.setProp(propName)

    case ArrayLiteral(elements, _) =>
      // Create a new array with the given size
      instructions += Instruction.newArray(elements.length)

      // Initialize each element
      for (elem, index) <- elements.zipWithIndex do
        // Push the index
        instructions += Instruction.pushI32(index)  // [array, index]

        // Compile element expression
        // Stack: [array, index, value]
        compileExpression(elem, instructions, constants)

        // Initialize element
        // Note: InitElem pops [array, index, value] and pushes [array] back
        instructions += Instruction.initElem()

    case MemberExpression(obj, prop, computed, _) =>
      // Compile the object
      compileExpression(obj, instructions, constants)

      if computed then
        // Computed property access: obj[prop]
        // Compile the property expression
        compileExpression(prop, instructions, constants)
        // Get element with computed index
        instructions += Instruction.getElem()
      else
        // Regular property access: obj.prop
        // Get the property name
        val propName = prop match
          case Identifier(name, _) => name
          case _ => throw new UnsupportedOperationException(s"Unsupported property key: $prop")

        // Get property
        instructions += Instruction.getProp(propName)

    case _ =>
      throw new UnsupportedOperationException(s"Unsupported expression: $expr")

  private def compileLiteral(
    value: JSValue,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = value match
    case JSValue.Undefined => instructions += Instruction.pushUndefined()
    case JSValue.Null => instructions += Instruction.pushNull()
    case JSValue.Bool(b) => if b then instructions += Instruction.pushTrue() else instructions += Instruction.pushFalse()
    case JSValue.Int32(i) => instructions += Instruction.pushI32(i)
    case JSValue.Float64(d) => instructions += Instruction.pushFloat64(d)
    case JSValue.JSStr(s) =>
      // Store string in constants and load with GetConst
      val constIndex = constants.length
      constants += value  // Store the JSValue.JSStr directly
      instructions += Instruction.getConst(constIndex)
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