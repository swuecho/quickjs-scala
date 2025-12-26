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

  // Loop exit point stack for break/continue
  // Each entry contains (exitBytePos, continueBytePos, pendingBreaks, pendingContinues)
  // pendingBreaks and pendingContinues are ListBuffers of (instructionIndex, bytePosition) tuples
  private val loopStack: mutable.Stack[(Int, Int, mutable.ListBuffer[(Int, Int)], mutable.ListBuffer[(Int, Int)])] = mutable.Stack.empty

  /** Enter a loop and push its info onto the stack */
  private def enterLoop(): Unit =
    loopStack.push((-1, -1, mutable.ListBuffer.empty, mutable.ListBuffer.empty))

  /** Exit a loop and pop its info from the stack */
  private def exitLoop(): Unit =
    if loopStack.nonEmpty then
      loopStack.pop()
    ()

  /** Set the loop exit point (called after compiling the loop) */
  private def setLoopExit(exitBytePos: Int, instructions: mutable.ArrayBuffer[Instruction]): Unit =
    if loopStack.nonEmpty then
      val (oldExit, oldCont, pendingBreaks, pendingContinues) = loopStack.pop()
      loopStack.push((exitBytePos, oldCont, pendingBreaks, pendingContinues))
      // Fix up all pending break statements
      for (breakInstIdx, breakBytePos) <- pendingBreaks do
        val offset = exitBytePos - breakBytePos - 1
        instructions(breakInstIdx) = Instruction.goto(offset)
      ()

  /** Set the loop continue point (called at the position where continue should jump) */
  private def setLoopContinue(continueBytePos: Int, instructions: mutable.ArrayBuffer[Instruction]): Unit =
    if loopStack.nonEmpty then
      val (oldExit, oldCont, pendingBreaks, pendingContinues) = loopStack.pop()
      loopStack.push((oldExit, continueBytePos, pendingBreaks, pendingContinues))
      // Fix up all pending continue statements
      for (contInstIdx, contBytePos) <- pendingContinues do
        val offset = continueBytePos - contBytePos - 1
        instructions(contInstIdx) = Instruction.goto(offset)
      ()

  /** Get the current loop exit position (if known) */
  private def getCurrentLoopExit(): Option[Int] =
    if loopStack.nonEmpty then
      val (exit, _, _, _) = loopStack.top
      Some(exit)
    else
      None

  /** Get the current loop continue position (if known) */
  private def getCurrentLoopContinue(): Option[Int] =
    if loopStack.nonEmpty then
      val (_, cont, _, _) = loopStack.top
      Some(cont)
    else
      None

  /** Add a pending break statement (to be fixed up when exit is known) */
  private def addPendingBreak(instIdx: Int, bytePos: Int): Unit =
    if loopStack.nonEmpty then
      val (exit, cont, pendingBreaks, pendingContinues) = loopStack.pop()
      pendingBreaks += ((instIdx, bytePos))
      loopStack.push((exit, cont, pendingBreaks, pendingContinues))

  /** Add a pending continue statement (to be fixed up when continue point is known) */
  private def addPendingContinue(instIdx: Int, bytePos: Int): Unit =
    if loopStack.nonEmpty then
      val (exit, cont, pendingBreaks, pendingContinues) = loopStack.pop()
      pendingContinues += ((instIdx, bytePos))
      loopStack.push((exit, cont, pendingBreaks, pendingContinues))

  /** Find all free variables in an expression, including those in nested function expressions (for closure analysis) */
  private def findFreeVariablesForClosure(expr: Expression): Set[String] = expr match
    case Identifier(name, _) => Set(name)
    case Literal(_, _) => Set.empty
    case ThisExpression(_) => Set.empty  // 'this' is not a free variable
    case BinaryExpression(_, left, right, _) =>
      findFreeVariablesForClosure(left) ++ findFreeVariablesForClosure(right)
    case UnaryExpression(_, argument, _, _) =>
      findFreeVariablesForClosure(argument)
    case CallExpression(callee, arguments, _) =>
      findFreeVariablesForClosure(callee) ++ arguments.flatMap(findFreeVariablesForClosure).toSet
    case NewExpression(callee, arguments, _) =>
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
    case ArrowFunctionExpression(params, body, _, _) =>
      // Arrow functions are similar to function expressions for closure analysis
      val paramNames = params.map(_.name).toSet
      // Body can be Expression or BlockStatement
      val bodyFree = body match
        case Left(expr) => findFreeVariablesForClosure(expr)
        case Right(block) => findFreeVariablesForClosure(block)
      val localVars = body match
        case Left(_) => Set.empty[String]  // Expression bodies don't declare vars
        case Right(block) => findDeclaredVariables(block)
      // Exclude parameters and local variables
      bodyFree -- paramNames -- localVars
    case ObjectLiteral(properties, _) =>
      properties.flatMap { p =>
        val valueFree = findFreeVariablesForClosure(p.value)
        val keyFree = p.key match
          case expr: Expression => findFreeVariablesForClosure(expr)
          case _ => Set.empty[String]
        valueFree ++ keyFree
      }.toSet
    case ArrayLiteral(elements, _) =>
      elements.filter(_ != null).flatMap(findFreeVariablesForClosure).toSet
    case ConditionalExpression(test, consequent, alternate, _) =>
      findFreeVariablesForClosure(test) ++ findFreeVariablesForClosure(consequent) ++ findFreeVariablesForClosure(alternate)
    case _ => Set.empty

  /** Find all free variables in an expression */
  private def findFreeVariables(expr: Expression): Set[String] = expr match
    case Identifier(name, _) => Set(name)
    case Literal(_, _) => Set.empty
    case ThisExpression(_) => Set.empty  // 'this' is not a free variable
    case BinaryExpression(_, left, right, _) =>
      findFreeVariables(left) ++ findFreeVariables(right)
    case UnaryExpression(_, argument, _, _) =>
      findFreeVariables(argument)
    case CallExpression(callee, arguments, _) =>
      findFreeVariables(callee) ++ arguments.flatMap(findFreeVariables).toSet
    case NewExpression(callee, arguments, _) =>
      findFreeVariables(callee) ++ arguments.flatMap(findFreeVariables).toSet
    case MemberExpression(obj, prop, computed, _) =>
      findFreeVariables(obj) ++ (if computed then findFreeVariables(prop) else Set.empty)
    case AssignmentExpression(left, right, _) =>
      findFreeVariables(left) ++ findFreeVariables(right)
    case FunctionExpression(_, _, _, _, _, _) =>
      // Function expressions create their own scope, so they don't directly
      // expose free variables from their body to the containing scope
      Set.empty
    case ArrowFunctionExpression(_, _, _, _) =>
      // Arrow functions create their own scope
      Set.empty
    case ObjectLiteral(properties, _) =>
      properties.flatMap { p =>
        val valueFree = findFreeVariables(p.value)
        val keyFree = p.key match
          case expr: Expression => findFreeVariables(expr)
          case _ => Set.empty[String]
        valueFree ++ keyFree
      }.toSet
    case ArrayLiteral(elements, _) =>
      elements.filter(_ != null).flatMap(findFreeVariables).toSet
    case ConditionalExpression(test, consequent, alternate, _) =>
      findFreeVariables(test) ++ findFreeVariables(consequent) ++ findFreeVariables(alternate)
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
    case FunctionDeclaration(_, params, body, _, _, _) =>
      // Function declarations DO expose free variables from their body in nested scopes!
      // We need to look inside to find what variables the function uses
      val paramNames = params.map(_.name).toSet
      val bodyFree = findFreeVariablesForClosure(body)
      // Exclude parameters - they're not free variables
      bodyFree -- paramNames
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
    case FunctionDeclaration(_, params, body, _, _, _) =>
      // Function declarations DO expose free variables from their body in nested scopes!
      // We need to look inside to find what variables the function uses
      val paramNames = params.map(_.name).toSet
      val bodyFree = findFreeVariablesForClosure(body)
      // Exclude parameters - they're not free variables
      bodyFree -- paramNames
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

    // Also collect local variable declarations (excluding parameters)
    val localVarNamesList = mutable.ArrayBuffer[String]()
    val localVars = findDeclaredVariables(body)
    declaredVars ++= localVars

    // Track local variable names (for closure capture)
    localVarNamesList ++= (localVars -- paramNamesList.toSet)

    // Declare local variables in scope so GetLoc/PutLoc can find them
    for varName <- localVars do
      currentScope.declare(varName)

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
      paramNames = paramNamesList.toArray,
      localVarNames = localVarNamesList.toArray
    )

  /** Compile arrow function body */
  private def compileArrowFunctionBody(
    params: scala.collection.immutable.Seq[Identifier],
    body: Either[Expression, BlockStatement]
  ): BytecodeFunction =
    // Create a new scope for the arrow function
    val oldScope = currentScope
    currentScope = new Scope(currentScope)

    // Collect variables declared in this function (params and locals)
    val declaredVars = mutable.Set[String]()
    val paramNamesList = mutable.ArrayBuffer[String]()
    for param <- params do
      declaredVars += param.name
      paramNamesList += param.name
      currentScope.declare(param.name)

    // Also collect local variable declarations (excluding parameters)
    val localVarNamesList = mutable.ArrayBuffer[String]()
    val localVars = body match
      case Left(_) => Set.empty[String]  // Expression bodies don't declare vars
      case Right(block) => findDeclaredVariables(block)
    declaredVars ++= localVars

    // Track local variable names (for closure capture), excluding parameters
    localVarNamesList ++= (localVars -- paramNamesList.toSet)

    // Declare local variables in scope so GetLoc/PutLoc can find them
    for varName <- localVars do
      currentScope.declare(varName)

    val bytecode = mutable.ArrayBuffer[Byte]()
    val constants = mutable.ArrayBuffer[AnyRef]()
    val instructions = mutable.ArrayBuffer[Instruction]()

    // Compile the function body based on its type
    body match
      case Left(expr) =>
        // Concise body: expression is implicitly returned
        compileExpression(expr, instructions, constants)
        instructions += Instruction.returnInst()
      case Right(block) =>
        // Block body: compile statements and return undefined implicitly
        for s <- block.statements do
          compileStatement(s, instructions, constants, false)
        instructions += Instruction.returnUndef()

    // Encode instructions to bytecode
    instructions.foreach(inst => bytecode ++= inst.encode())

    // Find free variables (referenced but not declared in this function)
    val allFreeVars = body match
      case Left(expr) => findFreeVariablesForClosure(expr)
      case Right(block) => findFreeVariablesForClosure(block)
    val freeVarNames = allFreeVars.filterNot(declaredVars.contains).toArray

    // Restore the parent scope
    currentScope = oldScope

    new BytecodeFunction(
      name = "<arrow>",
      bytecode = bytecode.toArray,
      constants = constants.toArray,
      stackSize = 256,
      freeVars = freeVarNames,
      paramNames = paramNamesList.toArray,
      localVarNames = localVarNamesList.toArray
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
      stackSize = 256,  // Fixed stack size for now
      localVarNames = Array.empty  // Scripts don't have local variables
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
      enterLoop()
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

      // Set exit point (for break statements) - after the back-jump goto
      val exitBytePos = instructions.foldLeft(0)(_ + _.size)
      setLoopExit(exitBytePos, instructions)

      // Set continue point (for continue statements) - jump back to loop start
      setLoopContinue(loopStartBytePos, instructions)

      // Update the ifFalse jump to exit loop
      val ifFalseOffset = exitBytePos - jumpIfFalseBytePos - 1
      instructions(ifFalseIdx) = Instruction.ifFalse(ifFalseOffset)

      exitLoop()

    case ForStatement(init, test, update, body, _) =>
      // Follow QuickJS C pattern for for loops:
      // init
      // goto label_test
      // label_cont:           <-- continue jumps here
      // update
      // label_test:           <-- first iteration jumps here (skips update)
      // test
      // if_false goto label_break
      // goto label_body
      // label_body:
      // body
      // goto label_cont
      // label_break:          <-- break jumps here

      enterLoop()

      // Compile init (if present)
      if init != null then
        init match
          case decl: VariableDeclaration =>
            compileStatement(decl, instructions, constants, false)
          case expr: Expression =>
            compileExpression(expr, instructions, constants)
            instructions += Instruction.drop()

      // goto label_test (skip update on first iteration)
      val gotoTestIdx = instructions.length
      val gotoTestBytePos = instructions.foldLeft(0)(_ + _.size)
      instructions += Instruction.goto(0)  // Placeholder - will be fixed up

      // label_cont: (continue target)
      val labelContBytePos = instructions.foldLeft(0)(_ + _.size)

      // Compile update (if present)
      if update != null then
        compileExpression(update, instructions, constants)
        instructions += Instruction.drop()

      // label_test: (test target)
      val labelTestBytePos = instructions.foldLeft(0)(_ + _.size)

      // Fix up the initial goto to jump to label_test
      val gotoTestOffset = labelTestBytePos - gotoTestBytePos - 1
      instructions(gotoTestIdx) = Instruction.goto(gotoTestOffset)

      // Compile test (if present)
      if test != null then
        compileExpression(test, instructions, constants)
      else
        // No test means always true - push true
        instructions += Instruction.pushTrue()

      // Jump out if false -> goto label_break
      val jumpIfFalseIdx = instructions.length
      val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
      instructions += Instruction.ifFalse(0)  // Placeholder - will be fixed up

      // goto label_body
      val gotoBodyIdx = instructions.length
      val gotoBodyBytePos = instructions.foldLeft(0)(_ + _.size)
      instructions += Instruction.goto(0)  // Placeholder - will be fixed up

      // label_body:
      val labelBodyBytePos = instructions.foldLeft(0)(_ + _.size)

      // Fix up the goto label_body
      val gotoBodyOffset = labelBodyBytePos - gotoBodyBytePos - 1
      instructions(gotoBodyIdx) = Instruction.goto(gotoBodyOffset)

      // Compile body
      compileStatement(body, instructions, constants, false)

      // goto label_cont (jump back to update)
      val gotoContIdx = instructions.length
      val gotoContBytePos = instructions.foldLeft(0)(_ + _.size)
      instructions += Instruction.goto(0)  // Placeholder - will be fixed up

      // label_break: (break target)
      val labelBreakBytePos = instructions.foldLeft(0)(_ + _.size)

      // Fix up the goto label_cont
      val gotoContOffset = labelContBytePos - gotoContBytePos - 1
      instructions(gotoContIdx) = Instruction.goto(gotoContOffset)

      // Fix up the if_false goto label_break
      val ifFalseOffset = labelBreakBytePos - jumpIfFalseBytePos - 1
      instructions(jumpIfFalseIdx) = Instruction.ifFalse(ifFalseOffset)

      // Set loop exit and continue points for break/continue statements
      setLoopExit(labelBreakBytePos, instructions)
      setLoopContinue(labelContBytePos, instructions)

      exitLoop()

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
      // Generate goto to loop exit
      getCurrentLoopExit() match
        case Some(exitBytePos) if exitBytePos >= 0 =>
          // We know where the exit is, emit goto directly
          val breakBytePos = instructions.foldLeft(0)(_ + _.size)
          val offset = exitBytePos - breakBytePos - 1
          instructions += Instruction.goto(offset)
        case Some(_) =>
          // Exit position not set yet, emit placeholder and add to pending list
          val breakInstIdx = instructions.length
          val breakBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)  // Placeholder
          addPendingBreak(breakInstIdx, breakBytePos)
        case None =>
          // Not in a loop - this is a semantic error, but for now just generate break opcode
          instructions += Instruction.breakInst()

    case ContinueStatement(label, _) =>
      // For now, ignore labels (will need to implement for nested loops)
      // Generate goto to loop continue point
      getCurrentLoopContinue() match
        case Some(contBytePos) if contBytePos >= 0 =>
          // We know where the continue point is, emit goto directly
          val contBytePosCalc = instructions.foldLeft(0)(_ + _.size)
          val offset = contBytePos - contBytePosCalc - 1
          instructions += Instruction.goto(offset)
        case Some(_) =>
          // Continue position not set yet, emit placeholder and add to pending list
          val contInstIdx = instructions.length
          val contBytePosCalc = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)  // Placeholder
          addPendingContinue(contInstIdx, contBytePosCalc)
        case None =>
          // Not in a loop - this is a semantic error, but for now just generate continue opcode
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
      // Top-level variables go ONLY to global scope
      // Do NOT declare in local scope - the interpreter executes scripts with empty locals array
      if decl.init != null then
        compileExpression(decl.init, instructions, constants)
      else
        // Push undefined for global scope
        instructions += Instruction.pushUndefined()

      // Store in global scope
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

    case ThisExpression(_) =>
      // Push the 'this' value onto the stack
      instructions += Instruction.getThis()

    case BinaryExpression(op, left, right, _) =>
      compileExpression(left, instructions, constants)
      compileExpression(right, instructions, constants)
      instructions += Instruction.binary(binaryOpToOpcode(op))

    case UnaryExpression(op, argument, _, _) =>
      // Increment/decrement operators need special handling for identifiers
      // Delete operator needs special handling for member expressions
      (op, argument) match
        case (UnaryOperator.PreInc | UnaryOperator.PostInc | UnaryOperator.PreDec | UnaryOperator.PostDec, id: Identifier) =>
          // Increment/decrement on identifier - use helper that handles locals, globals, and closures
          compileIncrementDecrement(op, id, instructions, constants)

        case (UnaryOperator.Delete, memberExpr: MemberExpression) =>
          // Delete operator on member expression: delete obj.prop
          // Stack layout: [obj, prop] -> [successBoolean]
          memberExpr match
            case MemberExpression(obj, Identifier(propName, _), false, _) =>
              // Non-computed property access: delete obj.prop
              // 1. Compile object - leaves [obj]
              compileExpression(obj, instructions, constants)
              // 2. Push property name - leaves [obj, prop]
              val constIndex = constants.length
              constants += JSValue.fromString(propName)
              instructions += Instruction.getConst(constIndex)
              // 3. Apply delete
              instructions += Instruction.unary(UnaryOpcode.Delete)
            case _ =>
              // Computed property access: delete obj[expr]
              // For now, just compile normally (will be fixed later)
              compileExpression(argument, instructions, constants)
              instructions += Instruction.unary(unaryOpToOpcode(op))

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
          compileExpression(memberExpr.`object`, instructions, constants)
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

    case NewExpression(callee, arguments, _) =>
      // new Constructor(arg1, arg2, ...)
      // Stack layout: [constructor, arg1, arg2, ..., argN]

      // Compile the constructor
      compileExpression(callee, instructions, constants)

      // Compile arguments
      for arg <- arguments do
        compileExpression(arg, instructions, constants)

      // Emit New instruction with argument count
      instructions += Instruction.newInst(arguments.length)

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

    case ArrowFunctionExpression(params, body, _, _) =>
      // Arrow functions are always anonymous
      val funcBytecode = compileArrowFunctionBody(params, body)

      // Store BytecodeFunction in constants array
      val constIndex = constants.length
      constants += funcBytecode

      // Push the function value onto the stack
      instructions += Instruction.getConst(constIndex)

    case AssignmentExpression(left, right, _) =>
      // Compile the right side first
      compileExpression(right, instructions, constants)

      // For assignment to identifier, store it
      // Assignment returns the value, so we need to keep it on the stack
      left match
        case Identifier(name, _) =>
          val isGlobal = currentScope.parent == null  // Top-level variables are global

          if isGlobal then
            // Global variable - use PutGlobal
            instructions += Instruction.dup()
            instructions += Instruction.putGlobal(name)
          else
            // Check if variable is in current immediate scope (not parent scopes)
            if currentScope.isLocal(name) then
              // Variable is local to this function - use PutLoc
              val index = currentScope.lookup(name).get  // Safe because isLocal returned true
              // Duplicate the value so we can keep one on stack and store one
              instructions += Instruction.dup()
              instructions += Instruction.putLoc(index)
            else
              // Variable is in parent scope (closure) or undefined - use PutGlobal
              // PutGlobal will check the closure at runtime
              instructions += Instruction.dup()
              instructions += Instruction.putGlobal(name)
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

        // Handle property key - can be identifier, string, or computed expression
        prop.key match
          case Identifier(name, _) =>
            // Regular property: {name: value}
            instructions += Instruction.setProp(name)
          case s: String =>
            // String property: {"name": value}
            instructions += Instruction.setProp(s)
          case expr: Expression =>
            // Computed property: {[expr]: value}
            // Compile the key expression
            compileExpression(expr, instructions, constants)
            // Set element with computed key
            instructions += Instruction.setElem()

    case ArrayLiteral(elements, _) =>
      // Create a new array with the given size
      instructions += Instruction.newArray(elements.length)

      // Initialize each element
      for (elem, index) <- elements.zipWithIndex do
        elem match
          case null =>
            // Elision (empty slot) - skip initialization, array elements are undefined by default
            ()
          case expr: Expression =>
            // Push the index
            instructions += Instruction.pushI32(index)  // [array, index]

            // Compile element expression
            // Stack: [array, index, value]
            compileExpression(expr, instructions, constants)

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

    case ConditionalExpression(test, consequent, alternate, _) =>
      // Compile: condition ? trueExpr : falseExpr
      // This is equivalent to: if (condition) { trueExpr } else { falseExpr }

      // First, compile the test condition
      compileExpression(test, instructions, constants)

      // Calculate the current bytecode position (in bytes, not instructions)
      def getBytecodePos(): Int =
        instructions.map(_.size).sum

      // We need to jump over the consequent if the test is false
      // Emit a conditional jump instruction

      // Push a placeholder for the jump offset (will be fixed up later)
      // Track both instruction index and bytecode position
      val jumpIfFalseInstIndex = instructions.length
      val jumpIfFalseBytecodePos = getBytecodePos()
      instructions += Instruction.ifFalse(0)  // placeholder offset

      // Compile consequent (true branch)
      compileExpression(consequent, instructions, constants)

      // Jump over the alternate branch
      val jumpInstIndex = instructions.length
      val jumpBytecodePos = getBytecodePos()
      instructions += Instruction.goto(0)  // placeholder offset

      // Fix up the ifFalse offset to point to here (after the consequent and goto)
      // Offset is from the start of the ifFalse instruction (not including opcode)
      val afterConsequentPos = getBytecodePos()
      instructions(jumpIfFalseInstIndex) = Instruction.ifFalse(afterConsequentPos - jumpIfFalseBytecodePos - 1)

      // Compile alternate (false branch)
      compileExpression(alternate, instructions, constants)

      // Fix up the jump offset to skip the alternate
      // Offset is from the start of the goto instruction (not including opcode)
      val afterAlternatePos = getBytecodePos()
      instructions(jumpInstIndex) = Instruction.goto(afterAlternatePos - jumpBytecodePos - 1)

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
    case BinaryOperator.Pow => BinaryOpcode.Pow
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
    case BinaryOperator.Instanceof => BinaryOpcode.Instanceof
    case BinaryOperator.In => BinaryOpcode.In

  private def unaryOpToOpcode(op: quickjs.ast.UnaryOperator): UnaryOpcode = (op: @unchecked) match
    case UnaryOperator.Minus => UnaryOpcode.Neg
    case UnaryOperator.Not => UnaryOpcode.Not
    case UnaryOperator.BitwiseNot => UnaryOpcode.LNot
    case UnaryOperator.PreInc => UnaryOpcode.PreInc
    case UnaryOperator.PostInc => UnaryOpcode.PostInc
    case UnaryOperator.PreDec => UnaryOpcode.PreDec
    case UnaryOperator.PostDec => UnaryOpcode.PostDec
    case UnaryOperator.Typeof => UnaryOpcode.Typeof
    case UnaryOperator.Delete => UnaryOpcode.Delete
    case UnaryOperator.Plus => UnaryOpcode.Neg  // UnaryPlus is a no-op, but we'll treat it as Neg for now (should be proper coercion)

  /** Helper to compile increment/decrement operations based on variable storage type */
  private def compileIncrementDecrement(
    op: quickjs.ast.UnaryOperator,
    id: Identifier,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    // Determine how to access this variable: local, global, or closure
    currentScope.parent == null && currentScope.isLocal(id.name) match
      case true =>
        // Top-level variable - use GetGlobal/PutGlobal directly
        emitIncrementDecrement(op, id.name, instructions, useGetLoc = false)

      case false if currentScope.isLocal(id.name) =>
        // Local variable in current function - use GetLoc/PutLoc
        val index = currentScope.lookup(id.name).get  // Safe because we just checked isLocal
        emitIncrementDecrement(op, index, instructions, useGetLoc = true)

      case _ =>
        // Variable from closure or parent scope - use GetGlobal/PutGlobal (checks closure map)
        emitIncrementDecrement(op, id.name, instructions, useGetLoc = false)

  /** Emit increment/decrement bytecode for a specific variable access method */
  private def emitIncrementDecrement(
    op: quickjs.ast.UnaryOperator,
    varRef: String | Int,
    instructions: mutable.ArrayBuffer[Instruction],
    useGetLoc: Boolean
  ): Unit =
    // Helper functions to emit get/put instructions
    def emitGet(): Unit =
      if useGetLoc then
        instructions += Instruction.getLoc(varRef.asInstanceOf[Int])
      else
        instructions += Instruction.getGlobal(varRef.asInstanceOf[String])

    def emitPut(): Unit =
      if useGetLoc then
        instructions += Instruction.putLoc(varRef.asInstanceOf[Int])
      else
        instructions += Instruction.putGlobal(varRef.asInstanceOf[String])

    // Emit the operation-specific bytecode
    op match
      case UnaryOperator.PreInc =>
        // PreInc: Get, Inc, Dup, Put
        emitGet()
        instructions += Instruction.unary(UnaryOpcode.PreInc)
        instructions += Instruction.dup()
        emitPut()

      case UnaryOperator.PostInc =>
        // PostInc: Get, Dup, Inc, Put
        emitGet()
        instructions += Instruction.dup()
        instructions += Instruction.unary(UnaryOpcode.PostInc)
        emitPut()

      case UnaryOperator.PreDec =>
        // PreDec: Get, Dec, Dup, Put
        emitGet()
        instructions += Instruction.unary(UnaryOpcode.PreDec)
        instructions += Instruction.dup()
        emitPut()

      case UnaryOperator.PostDec =>
        // PostDec: Get, Dup, Dec, Put
        emitGet()
        instructions += Instruction.dup()
        instructions += Instruction.unary(UnaryOpcode.PostDec)
        emitPut()

object Compiler:
  def apply(): Compiler = new Compiler()