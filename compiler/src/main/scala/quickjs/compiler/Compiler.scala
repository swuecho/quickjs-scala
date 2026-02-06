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
  private var currentModuleName: String = "<script>"
  private var tempVarCounter: Int = 0
  private var currentSuperClass: Expression | Null = null
  private var currentSuperIsStatic: Boolean = false
  private var currentSuperCapture: Set[String] = Set.empty
  private var currentSuperVarName: Option[String] = None  // Name of variable holding superclass
  private var currentClassName: String | Null = null
  private var currentClassCapture: Boolean = true
  private var currentStaticFieldThis: Option[Int] = None
  private val unknownSpan: Span = Span(0, 0, 0, 0)
  private var currentSpan: Span = unknownSpan

  private final class InstructionBuffer extends mutable.ArrayBuffer[Instruction]:
    val spans: mutable.ArrayBuffer[Span] = mutable.ArrayBuffer.empty

    override def addOne(elem: Instruction): this.type =
      spans += currentSpan
      super.addOne(elem)
      this

  private def withSpan[T](span: Span)(body: => T): T =
    val prev = currentSpan
    currentSpan = span
    try body
    finally currentSpan = prev

  private def buildSpanMap(instructions: InstructionBuffer): Array[(Int, Int, Int)] =
    val map = mutable.ArrayBuffer[(Int, Int, Int)]()
    var offset = 0
    var lastLine = -1
    var lastCol = -1
    var i = 0
    while i < instructions.length do
      val span = instructions.spans(i)
      if span != unknownSpan then
        val line = span.line + 1
        val col = span.column + 1
        if line != lastLine || col != lastCol then
          map += ((offset, line, col))
          lastLine = line
          lastCol = col
      offset += instructions(i).size
      i += 1
    map.toArray

  private def withSuperContext[T](superClass: Expression | Null, isStatic: Boolean, superVarName: Option[String] = None)(f: => T): T =
    val prevSuper = currentSuperClass
    val prevStatic = currentSuperIsStatic
    val prevCapture = currentSuperCapture
    val prevVarName = currentSuperVarName
    currentSuperClass = superClass
    currentSuperIsStatic = isStatic
    currentSuperVarName = superVarName
    currentSuperCapture =
      if superVarName.isDefined then superVarName.toSet
      else if superClass != null then findFreeVariablesForClosure(superClass)
      else Set.empty
    try f
    finally
      currentSuperClass = prevSuper
      currentSuperIsStatic = prevStatic
      currentSuperCapture = prevCapture
      currentSuperVarName = prevVarName

  private def withClassContext[T](className: String | Null, captureInClosure: Boolean)(f: => T): T =
    val prevName = currentClassName
    val prevCapture = currentClassCapture
    currentClassName = className
    currentClassCapture = captureInClosure
    try f
    finally
      currentClassName = prevName
      currentClassCapture = prevCapture

  private def withStaticFieldThis[T](ctorIndex: Int)(f: => T): T =
    val prev = currentStaticFieldThis
    currentStaticFieldThis = Some(ctorIndex)
    try f
    finally currentStaticFieldThis = prev

  /** Compile in REPL mode (don't drop last expression) */
  def withREPLMode(compilation: => BytecodeFunction): BytecodeFunction =
    val oldMode = replMode
    replMode = true
    try compilation
    finally replMode = oldMode

  // Scope for variable tracking - following QuickJS C pattern
  // Key change: Variables now track their scope level for proper let/const scoping
  // Uses a list-based structure to support shadowing (multiple variables with same name at different scope levels)
  private class Scope(val parent: Scope | Null):
    // Store list of variable declarations: name -> List[(index, isLexical, isConst, scopeLevel))
    // The most recent (highest scope level) declaration should be at the HEAD of the list
    private val vars = mutable.HashMap[String, mutable.ListBuffer[(Int, Boolean, Boolean, Int)]]()
    private var nextIndex = 0
    // Track the current block scope level (0 = function/script level, 1+ = nested blocks)
    private var blockScopeLevel: Int = 0

    def declare(name: String, isLexical: Boolean = false, isConst: Boolean = false): Int =
      // Get or create the list of declarations for this name
      val declarations = vars.getOrElseUpdate(name, mutable.ListBuffer.empty)

      // Check if variable is already declared at the CURRENT block scope level
      val existingAtCurrentLevel = declarations.exists { case (_, _, _, level) => level == blockScopeLevel }

      if existingAtCurrentLevel then
        // Variable already declared in this block scope - return existing index
        declarations.find(_._4 == blockScopeLevel).get._1
      else
        // Create a new variable (either new name, or shadowing from outer scope)
        // Add to the HEAD of the list so most recent is first
        val idx = nextIndex
        declarations.prepend((idx, isLexical, isConst, blockScopeLevel))
        nextIndex += 1
        idx

    def lookup(name: String): Option[Int] =
      // Find the variable at the current block scope level
      // First, look for a variable declared at the current scope level
      val atCurrentLevel = vars.get(name).flatMap { declarations =>
        declarations.find { case (_, _, _, level) => level == blockScopeLevel }
      }

      if atCurrentLevel.isDefined then
        atCurrentLevel.map(_._1)
      else if vars.contains(name) then
        // If no variable at current scope level, look for the highest level ≤ current scope level
        val validVars = vars(name).filter { case (_, _, _, level) => level <= blockScopeLevel }
        if validVars.nonEmpty then
          // Find the one with the highest scope level
          Some(validVars.maxBy(_._4)._1)
        else if parent != null then
          parent.lookup(name)
        else
          None
      else if parent != null then
        parent.lookup(name)
      else
        None

    /** Check if a variable is const (for reassignment checks) */
    def isConst(name: String): Boolean =
      vars.get(name).exists { declarations =>
        declarations.find { case (_, _, _, level) => level == blockScopeLevel } match
          case Some((_, _, isConst, _)) => isConst
          case None => false
      }

    /** Check if a variable is lexical (let/const) for TDZ checks */
    def isLexical(name: String): Boolean =
      vars.get(name).exists { declarations =>
        declarations.find { case (_, _, _, level) => level == blockScopeLevel } match
          case Some((_, isLexical, _, _)) => isLexical
          case None => false
      }

    /** Check if variable is in this scope only (not parent scopes) */
    def isLocal(name: String): Boolean =
      // Check if variable exists at the current block scope level or any lower level
      // This is different from lookup which only finds variables at the current or lower levels
      vars.get(name).exists { declarations =>
        declarations.exists { case (_, _, _, level) => level <= blockScopeLevel }
      }

    /** Get the scope level of a variable if it's in this scope */
    def getVariableScopeLevel(name: String): Option[Int] =
      vars.get(name).map(_.head._4)

    /** Enter a new block scope (for let/const) */
    def enterBlockScope(): Int =
      blockScopeLevel += 1
      blockScopeLevel

    /** Leave the current block scope */
    def leaveBlockScope(): Int =
      if blockScopeLevel > 0 then
        blockScopeLevel -= 1
      blockScopeLevel

    /** Get current block scope level */
    def getBlockScopeLevel: Int = blockScopeLevel

    /** Get all local variable names in declaration order (by index) */
    def getAllLocalVarNames: Array[String] =
      // Collect all (name, index) pairs and sort by index
      val allVars = vars.flatMap { case (name, declarations) =>
        // Get the first (most recent) declaration for each name
        declarations.headOption.map { case (idx, _, _, _) => (name, idx) }
      }
      allVars.toArray.sortBy(_._2).map(_._1)

  // Current compilation scope
  private var currentScope: Scope = new Scope(null)

  // Loop/switch/labeled statement exit point stack for break/continue
  // Each entry contains (isLoop, labelName, exitBytePos, continueBytePos, pendingBreaks, pendingContinues, isRegularStmt)
  // isLoop: true for loops (for/while/do-while), false for switches/regular statements
  // labelName: Some(label) for labeled statements, None for unlabeled
  // pendingBreaks and pendingContinues are ListBuffers of (instructionIndex, bytePosition) tuples
  // Switches and regular statements only support break, not continue
  private val loopStack: mutable.Stack[(Boolean, Option[String], Int, Int, mutable.ListBuffer[(Int, Int)], mutable.ListBuffer[(Int, Int)], Boolean)] = mutable.Stack.empty

  // Stack of active finally blocks for control-flow unwinding (innermost first)
  private var finallyStack: List[Statement] = Nil

  /** Enter a loop and push its info onto the stack */
  private def enterLoop(labelName: Option[String] = None): Unit =
    loopStack.push((true, labelName, -1, -1, mutable.ListBuffer.empty, mutable.ListBuffer.empty, false))

  /** Enter a switch and push its info onto the stack (switches support break but not continue) */
  private def enterSwitch(labelName: Option[String] = None): Unit =
    loopStack.push((false, labelName, -1, -1, mutable.ListBuffer.empty, mutable.ListBuffer.empty, false))

  /** Enter a labeled regular statement and push its info onto the stack */
  private def enterLabeledStatement(labelName: String): Unit =
    loopStack.push((false, Some(labelName), -1, -1, mutable.ListBuffer.empty, mutable.ListBuffer.empty, true))

  /** Exit a loop/switch/labeled statement and pop its info from the stack */
  private def exitLoop(): Unit =
    if loopStack.nonEmpty then
      loopStack.pop()
    ()

  /** Set the loop/switch/labeled statement exit point (called after compiling the statement) */
  private def setLoopExit(exitBytePos: Int, instructions: mutable.ArrayBuffer[Instruction]): Unit =
    if loopStack.nonEmpty then
      val (isLoop, labelName, oldExit, oldCont, pendingBreaks, pendingContinues, isRegular) = loopStack.pop()
      loopStack.push((isLoop, labelName, exitBytePos, oldCont, pendingBreaks, pendingContinues, isRegular))
      // Fix up all pending break statements
      for (breakInstIdx, breakBytePos) <- pendingBreaks do
        val offset = exitBytePos - breakBytePos - 1
        instructions(breakInstIdx) = Instruction.goto(offset)
      ()

  /** Set the loop continue point (called at the position where continue should jump) */
  private def setLoopContinue(continueBytePos: Int, instructions: mutable.ArrayBuffer[Instruction]): Unit =
    if loopStack.nonEmpty then
      val (isLoop, labelName, oldExit, oldCont, pendingBreaks, pendingContinues, isRegular) = loopStack.pop()
      loopStack.push((isLoop, labelName, oldExit, continueBytePos, pendingBreaks, pendingContinues, isRegular))
      // Fix up all pending continue statements
      for (contInstIdx, contBytePos) <- pendingContinues do
        val offset = continueBytePos - contBytePos - 1
        instructions(contInstIdx) = Instruction.goto(offset)
      ()

  /** Get the current loop/switch/labeled statement exit position (if known) */
  private def getCurrentLoopExit(): Option[Int] =
    if loopStack.nonEmpty then
      val (_, _, exit, _, _, _, _) = loopStack.top
      Some(exit)
    else
      None

  /** Get the current loop continue position (if known) - skips switches and regular statements */
  private def getCurrentLoopContinue(): Option[Int] =
    // Find the innermost actual loop (skip switches and regular labeled statements)
    loopStack.find(_._1).map(_._4)

  /** Find a loop/switch/labeled statement by label name */
  private def findLabeledStatement(label: String): Option[(Boolean, Option[String], Int, Int, Boolean)] =
    loopStack.find { case (_, labelName, _, _, _, _, isRegular) =>
      labelName.exists(_ == label) && !isRegular
    }.map { case (isLoop, labelName, exit, cont, _, _, isRegular) =>
      (isLoop, labelName, exit, cont, isRegular)
    }

  /** Add a pending break statement (to be fixed up when exit is known) */
  private def addPendingBreak(instIdx: Int, bytePos: Int): Unit =
    if loopStack.nonEmpty then
      val (isLoop, labelName, exit, cont, pendingBreaks, pendingContinues, isRegular) = loopStack.pop()
      pendingBreaks += ((instIdx, bytePos))
      loopStack.push((isLoop, labelName, exit, cont, pendingBreaks, pendingContinues, isRegular))

  /** Add a pending continue statement (to be fixed up when continue point is known) */
  private def addPendingContinue(instIdx: Int, bytePos: Int): Unit =
    // Find the innermost actual loop (skip switches and regular labeled statements) and add to its pending continues
    val loopIdx = loopStack.indexWhere(_._1)
    if loopIdx >= 0 then
      val (isLoop, labelName, exit, cont, pendingBreaks, pendingContinues, isRegular) = loopStack(loopIdx)
      // Update that specific entry
      pendingContinues += ((instIdx, bytePos))
      loopStack(loopIdx) = (isLoop, labelName, exit, cont, pendingBreaks, pendingContinues, isRegular)

  private def allocateTempLocal(prefix: String): Int =
    val name = s"${prefix}_${tempVarCounter}"
    tempVarCounter += 1
    currentScope.declare(name)

  private def emitForInAssignment(
    target: VariableDeclaration | Expression,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    target match
      case decl: VariableDeclaration =>
        decl.declarations.head.id match
          case Identifier(name, _) =>
            if currentScope.isLocal(name) then
              val index = currentScope.lookup(name).get
              instructions += Instruction.putLoc(index)
            else
              instructions += Instruction.putGlobal(name)
          case pattern: BindingPattern =>
            emitDestructuring(pattern, isDeclaration = false, isGlobalVar = false, instructions, constants)
      case expr: Expression =>
        expr match
          case Identifier(name, _) =>
            if currentScope.isLocal(name) then
              val index = currentScope.lookup(name).get
              instructions += Instruction.putLoc(index)
            else
              instructions += Instruction.putGlobal(name)
          case MemberExpression(obj, prop, computed, _, _) =>
            if computed then
              compileExpression(obj, instructions, constants)
              instructions += Instruction.swap()
              compileExpression(prop, instructions, constants)
              instructions += Instruction.swap()
              instructions += Instruction.setElem()
              instructions += Instruction.drop()
            else
              compileExpression(obj, instructions, constants)
              instructions += Instruction.swap()
              val propName = prop match
                case Identifier(name, _) => name
                case _ => throw new UnsupportedOperationException(s"Unsupported property key: $prop")
              instructions += Instruction.setProp(propName)
              instructions += Instruction.drop()
          case _ =>
            throw new UnsupportedOperationException(s"Unsupported assignment target: $expr")

  // ===========================================================================
  // Free Variable Analysis Helpers
  // ===========================================================================

  /** Recursively find free variables in binary expression operands */
  private def findFreeVarsInBinary(left: Expression, right: Expression, recurse: Expression => Set[String]): Set[String] =
    recurse(left) ++ recurse(right)

  /** Recursively find free variables in unary expression operand */
  private def findFreeVarsInUnary(argument: Expression, recurse: Expression => Set[String]): Set[String] =
    recurse(argument)

  /** Recursively find free variables in call/expression */
  private def findFreeVarsInCall(callee: Expression, arguments: Seq[Expression], recurse: Expression => Set[String]): Set[String] =
    recurse(callee) ++ arguments.flatMap(recurse).toSet

  /** Recursively find free variables in member expression */
  private def findFreeVarsInMember(obj: Expression, prop: Expression, computed: Boolean, recurse: Expression => Set[String]): Set[String] =
    recurse(obj) ++ (if computed then recurse(prop) else Set.empty)

  /** Recursively find free variables in assignment expression */
  private def findFreeVarsInAssignment(left: Expression | BindingPattern, right: Expression, recurseExpr: Expression => Set[String], recursePattern: BindingPattern => Set[String]): Set[String] =
    val leftFree = left match
      case e: Expression => recurseExpr(e)
      case p: BindingPattern => recursePattern(p)
    leftFree ++ recurseExpr(right)

  /** Recursively find free variables in object literal properties */
  private def findFreeVarsInObjectLiteral(properties: Seq[Property | SpreadElement], recurse: Expression => Set[String]): Set[String] =
    properties.flatMap {
      case SpreadElement(argument, _) => recurse(argument)
      case p: Property =>
        val valueFree = recurse(p.value)
        val keyFree = p.key match
          case expr: Expression => recurse(expr)
          case _ => Set.empty[String]
        valueFree ++ keyFree
    }.toSet

  /** Recursively find free variables in array literal elements */
  private def findFreeVarsInArrayLiteral(elements: Seq[Expression | Null], recurse: Expression => Set[String]): Set[String] =
    elements.filter(_ != null).flatMap(e => recurse(e.asInstanceOf[Expression])).toSet

  /** Recursively find free variables in conditional expression */
  private def findFreeVarsInConditional(test: Expression, consequent: Expression, alternate: Expression, recurse: Expression => Set[String]): Set[String] =
    recurse(test) ++ recurse(consequent) ++ recurse(alternate)

  /** Find free variables in class expression */
  private def findFreeVarsInClass(superClass: Expression | Null, recurse: Expression => Set[String]): Set[String] =
    if superClass != null then recurse(superClass) else Set.empty

  /** Find free variables in function/arrow function for closure analysis.
    * This looks inside the function body and excludes parameters/local variables.
    */
  private def findFreeVarsInFunctionForClosure(
    params: Seq[BindingPattern],
    body: Expression | BlockStatement,
    recurseExpr: Expression => Set[String],
    recurseStmt: Statement => Set[String]
  ): Set[String] =
    val paramNames = params.flatMap(collectBindingNames).toSet
    val (bodyFree, localVars) = body match
      case e: Expression =>
        (recurseExpr(e), Set.empty[String])  // Expression bodies don't declare vars
      case b: BlockStatement =>
        (recurseStmt(b), findDeclaredVariables(b))
    // Exclude parameters and local variables - only return true free vars
    bodyFree -- paramNames -- localVars

  /** Find all free variables in an expression */
  private def findFreeVariablesForClosure(expr: Expression): Set[String] = expr match
    case Identifier(name, _) =>
      if currentClassName != null && !currentClassCapture && name == currentClassName then
        Set.empty
      else
        Set(name)
    case Literal(_, _) => Set.empty
    case ThisExpression(_) => Set.empty  // 'this' is not a free variable
    case SuperExpression(_) => currentSuperCapture
    case BinaryExpression(_, left, right, _) =>
      findFreeVarsInBinary(left, right, findFreeVariablesForClosure)
    case UnaryExpression(_, argument, _, _) =>
      findFreeVarsInUnary(argument, findFreeVariablesForClosure)
    case CallExpression(callee, arguments, _, _) =>
      findFreeVarsInCall(callee, arguments, findFreeVariablesForClosure)
    case NewExpression(callee, arguments, _) =>
      findFreeVarsInCall(callee, arguments, findFreeVariablesForClosure)
    case MemberExpression(obj, prop, computed, _, _) =>
      findFreeVarsInMember(obj, prop, computed, findFreeVariablesForClosure)
    case AssignmentExpression(left, right, _) =>
      findFreeVarsInAssignment(left, right, findFreeVariablesForClosure, findFreeVariablesInPattern)
    case FunctionExpression(_, params, body, _, _, _, _) =>
      findFreeVarsInFunctionForClosure(params, body, findFreeVariablesForClosure, findFreeVariablesForClosure)
    case ArrowFunctionExpression(params, body, _, _, _) =>
      // Arrow functions have Either[Expression, BlockStatement] for body
      val paramNames = params.flatMap(collectBindingNames).toSet
      val (bodyFree, localVars) = body match
        case Left(expr) =>
          (findFreeVariablesForClosure(expr), Set.empty[String])  // Expression bodies don't declare vars
        case Right(block) =>
          (findFreeVariablesForClosure(block), findDeclaredVariables(block))
      // Exclude parameters and local variables
      bodyFree -- paramNames -- localVars
    case ObjectLiteral(properties, _) =>
      findFreeVarsInObjectLiteral(properties, findFreeVariablesForClosure)
    case ClassExpression(_, superClass, _, _) =>
      findFreeVarsInClass(superClass, findFreeVariablesForClosure)
    case ArrayLiteral(elements, _) =>
      findFreeVarsInArrayLiteral(elements, findFreeVariablesForClosure)
    case SpreadElement(argument, _) =>
      findFreeVariablesForClosure(argument)
    case ConditionalExpression(test, consequent, alternate, _) =>
      findFreeVarsInConditional(test, consequent, alternate, findFreeVariablesForClosure)
    case _ => Set.empty

  /** Find all free variables in an expression (non-closure version - functions are boundaries) */
  private def findFreeVariables(expr: Expression): Set[String] = expr match
    case Identifier(name, _) => Set(name)
    case Literal(_, _) => Set.empty
    case ThisExpression(_) => Set.empty  // 'this' is not a free variable
    case SuperExpression(_) => Set.empty
    case BinaryExpression(_, left, right, _) =>
      findFreeVarsInBinary(left, right, findFreeVariables)
    case UnaryExpression(_, argument, _, _) =>
      findFreeVarsInUnary(argument, findFreeVariables)
    case CallExpression(callee, arguments, _, _) =>
      findFreeVarsInCall(callee, arguments, findFreeVariables)
    case NewExpression(callee, arguments, _) =>
      findFreeVarsInCall(callee, arguments, findFreeVariables)
    case MemberExpression(obj, prop, computed, _, _) =>
      findFreeVarsInMember(obj, prop, computed, findFreeVariables)
    case AssignmentExpression(left, right, _) =>
      findFreeVarsInAssignment(left, right, findFreeVariables, findFreeVariablesInPattern)
    case FunctionExpression(_, _, _, _, _, _, _) =>
      // Function expressions create their own scope, so they don't directly
      // expose free variables from their body to the containing scope
      Set.empty
    case ArrowFunctionExpression(_, _, _, _, _) =>
      // Arrow functions create their own scope
      Set.empty
    case ObjectLiteral(properties, _) =>
      findFreeVarsInObjectLiteral(properties, findFreeVariables)
    case ClassExpression(_, superClass, _, _) =>
      findFreeVarsInClass(superClass, findFreeVariables)
    case ArrayLiteral(elements, _) =>
      findFreeVarsInArrayLiteral(elements, findFreeVariables)
    case SpreadElement(argument, _) =>
      findFreeVariables(argument)
    case ConditionalExpression(test, consequent, alternate, _) =>
      findFreeVarsInConditional(test, consequent, alternate, findFreeVariables)
    case _ => Set.empty

  private def findFreeVariablesInPattern(pattern: BindingPattern): Set[String] = pattern match
    case Identifier(name, _) => Set(name)
    case BindingAssignment(target, defaultValue, _) =>
      findFreeVariablesInPattern(target) ++ findFreeVariables(defaultValue)
    case ArrayPattern(elements, _) =>
      elements.filter(_ != null).flatMap {
        case p: BindingPattern => findFreeVariablesInPattern(p)
        case _ => Set.empty[String]
      }.toSet
    case ObjectPattern(properties, rest, _) =>
      val propVars = properties.flatMap(p => findFreeVariablesInPattern(p.value)).toSet
      val restVars = if rest != null then findFreeVariablesInPattern(rest.argument) else Set.empty[String]
      propVars ++ restVars
    case RestElement(argument, _) =>
      findFreeVariablesInPattern(argument)

  /** Find all variables declared in a statement */
  private def findDeclaredVariables(stmt: Statement): Set[String] = stmt match
    case VariableDeclaration(_, declarations, _) =>
      val boundNames = declarations.flatMap(d => collectBindingNames(d.id))
      val classExprNames = declarations.flatMap { d =>
        d.init match
          case ClassExpression(id, _, _, _) if id != null => Seq(id.name)
          case _ => Seq.empty
      }
      (boundNames ++ classExprNames).toSet
    case ClassDeclaration(id, _, _, _) =>
      Set(id.name)
    case ImportDeclaration(specifiers, _, _) =>
      specifiers.map {
        case ImportNamedSpecifier(_, local, _) => local.name
        case ImportDefaultSpecifier(local, _) => local.name
        case ImportNamespaceSpecifier(local, _) => local.name
      }.toSet
    case ExportNamedDeclaration(decl, _, _, _) =>
      if decl != null then findDeclaredVariables(decl) else Set.empty
    case ExportDefaultDeclaration(decl, _) =>
      decl match
        case stmt: Statement => findDeclaredVariables(stmt)
        case _ => Set.empty
    case BlockStatement(statements, _) =>
      statements.flatMap(findDeclaredVariables).toSet
    case IfStatement(test, consequent, alternate, _) =>
      findDeclaredVariables(consequent) ++
        (if alternate != null then findDeclaredVariables(alternate) else Set.empty)
    case WhileStatement(test, body, _, _) =>
      findDeclaredVariables(body)
    case DoWhileStatement(body, test, _, _) =>
      findDeclaredVariables(body)
    case SwitchStatement(discriminant, cases, _) =>
      cases.flatMap(c => c.consequent.flatMap(findDeclaredVariables)).toSet
    case ForStatement(init, test, update, body, _, _) =>
      val initDeclared = init match
        case vd: VariableDeclaration => findDeclaredVariables(vd)
        case _ => Set.empty
      initDeclared ++ findDeclaredVariables(body)
    case ForInStatement(left, _, body, _, _) =>
      val leftDeclared = left match
        case vd: VariableDeclaration => findDeclaredVariables(vd)
        case _ => Set.empty
      leftDeclared ++ findDeclaredVariables(body)
    case ForOfStatement(left, _, body, _, _) =>
      val leftDeclared = left match
        case vd: VariableDeclaration => findDeclaredVariables(vd)
        case _ => Set.empty
      leftDeclared ++ findDeclaredVariables(body)
    case WithStatement(_, body, _) =>
      findDeclaredVariables(body)
    case TryStatement(block, handler, finalizer, _) =>
      val handlerDeclared = handler match
        case null => Set.empty
        case CatchClause(param, body, _) =>
          val paramNames = param match
            case pattern: BindingPattern => collectBindingNames(pattern)
            case null => Set.empty
          findDeclaredVariables(body) ++ paramNames
      val finalizerDeclared =
        if finalizer != null then findDeclaredVariables(finalizer) else Set.empty
      findDeclaredVariables(block) ++ handlerDeclared ++ finalizerDeclared
    case _ => Set.empty

  /** Find all free variables in a statement, including those in nested function expressions (for closure analysis) */
  private def findFreeVariablesForClosure(stmt: Statement): Set[String] = stmt match
    case ExpressionStatement(expr, _) => findFreeVariablesForClosure(expr)
    case ImportDeclaration(_, _, _) =>
      Set.empty
    case ExportDefaultDeclaration(decl, _) =>
      decl match
        case expr: Expression => findFreeVariablesForClosure(expr)
        case stmt: Statement => findFreeVariablesForClosure(stmt)
    case ExportNamedDeclaration(decl, specifiers, source, _) =>
      val declFree =
        if decl != null then findFreeVariablesForClosure(decl) else Set.empty
      val specFree =
        if source != null then Set.empty else specifiers.map(_.local.name).toSet
      declFree ++ specFree
    case ExportAllDeclaration(_, _) =>
      Set.empty
    case VariableDeclaration(_, declarations, _) =>
      declarations.flatMap { d =>
        val initFree = if d.init != null then findFreeVariablesForClosure(d.init) else Set.empty
        // Exclude the variable being declared from free variables
        initFree -- collectBindingNames(d.id)
      }.toSet
    case BlockStatement(statements, _) =>
      statements.flatMap(findFreeVariablesForClosure).toSet
    case IfStatement(test, consequent, alternate, _) =>
      findFreeVariablesForClosure(test) ++ findFreeVariablesForClosure(consequent) ++
        (if alternate != null then findFreeVariablesForClosure(alternate) else Set.empty)
    case WhileStatement(test, body, _, _) =>
      findFreeVariablesForClosure(test) ++ findFreeVariablesForClosure(body)
    case DoWhileStatement(body, test, _, _) =>
      findFreeVariablesForClosure(body) ++ findFreeVariablesForClosure(test)
    case SwitchStatement(discriminant, cases, _) =>
      val discFree = findFreeVariablesForClosure(discriminant)
      val casesFree = cases.flatMap { c =>
        c.test match
          case null => c.consequent.flatMap(findFreeVariablesForClosure)
          case testExpr => findFreeVariablesForClosure(testExpr) ++ c.consequent.flatMap(findFreeVariablesForClosure)
      }
      discFree ++ casesFree
    case ForStatement(init, test, update, body, _, _) =>
      val initFree = init match
        case e: Expression => findFreeVariablesForClosure(e)
        case s: Statement => findFreeVariablesForClosure(s)
        case null => Set.empty
      initFree ++ findFreeVariablesForClosure(test) ++
        findFreeVariablesForClosure(update) ++ findFreeVariablesForClosure(body)
    case ForInStatement(left, right, body, _, _) =>
      val leftFree = left match
        case e: Expression => findFreeVariablesForClosure(e)
        case s: Statement => findFreeVariablesForClosure(s)
      leftFree ++ findFreeVariablesForClosure(right) ++ findFreeVariablesForClosure(body)
    case ForOfStatement(left, right, body, _, _) =>
      val leftFree = left match
        case e: Expression => findFreeVariablesForClosure(e)
        case s: Statement => findFreeVariablesForClosure(s)
      leftFree ++ findFreeVariablesForClosure(right) ++ findFreeVariablesForClosure(body)
    case FunctionDeclaration(_, params, body, _, _, _, _) =>
      // Function declarations DO expose free variables from their body in nested scopes!
      // We need to look inside to find what variables the function uses
      val paramNames = params.flatMap(collectBindingNames).toSet
      val bodyFree = findFreeVariablesForClosure(body)
      // Exclude parameters - they're not free variables
      bodyFree -- paramNames
    case ReturnStatement(argument, _) =>
      if argument != null then findFreeVariablesForClosure(argument) else Set.empty
    case ThrowStatement(argument, _) =>
      findFreeVariablesForClosure(argument)
    case TryStatement(block, handler, finalizer, _) =>
      val handlerFree = handler match
        case null => Set.empty
        case CatchClause(param, body, _) =>
          val boundNames = param match
            case pattern: BindingPattern => collectBindingNames(pattern)
            case null => Set.empty
          findFreeVariablesForClosure(body) -- boundNames
      val finalizerFree =
        if finalizer != null then findFreeVariablesForClosure(finalizer) else Set.empty
      findFreeVariablesForClosure(block) ++ handlerFree ++ finalizerFree
    case WithStatement(obj, body, _) =>
      findFreeVariablesForClosure(obj) ++ findFreeVariablesForClosure(body)
    case _ => Set.empty

  private def collectBindingNames(pattern: BindingPattern): Set[String] = pattern match
    case Identifier(name, _) => Set(name)
    case BindingAssignment(target, _, _) => collectBindingNames(target)
    case ArrayPattern(elements, _) =>
      elements.filter(_ != null).flatMap {
        case p: BindingPattern => collectBindingNames(p)
        case _ => Set.empty[String]
      }.toSet
    case ObjectPattern(properties, rest, _) =>
      val propNames = properties.flatMap(p => collectBindingNames(p.value)).toSet
      val restNames = if rest != null then collectBindingNames(rest.argument) else Set.empty[String]
      propNames ++ restNames
    case RestElement(argument, _) =>
      collectBindingNames(argument)

  private def currentBytecodePos(instructions: mutable.ArrayBuffer[Instruction]): Int =
    instructions.map(_.size).sum

  private def emitActiveFinallyBlocks(
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    if finallyStack.nonEmpty then
      val saved = finallyStack
      finallyStack = Nil
      try
        for finalizer <- saved do
          compileStatement(finalizer, instructions, constants, false)
      finally
        finallyStack = saved

  private def emitDefaultIfUndefined(
    defaultValue: Expression,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    instructions += Instruction.dup()
    instructions += Instruction.pushUndefined()
    instructions += Instruction.binary(BinaryOpcode.StrictEq)
    val ifFalseInstIndex = instructions.length
    val ifFalseBytePos = currentBytecodePos(instructions)
    instructions += Instruction.ifFalse(0)  // placeholder
    instructions += Instruction.drop()
    compileExpression(defaultValue, instructions, constants)
    val endPos = currentBytecodePos(instructions)
    instructions(ifFalseInstIndex) = Instruction.ifFalse(endPos - ifFalseBytePos - 1)

  private def pushStringConst(
    value: String,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    val constIndex = constants.length
    constants += JSValue.fromString(value)
    instructions += Instruction.getConst(constIndex)

  private def emitModuleExportCall(
    exportName: String,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  )(emitValue: => Unit): Unit =
    instructions += Instruction.getGlobal("__moduleExport")
    pushStringConst(currentModuleName, instructions, constants)
    pushStringConst(exportName, instructions, constants)
    emitValue
    instructions += Instruction.call(3)
    instructions += Instruction.drop()

  private def emitLoadIdentifierValue(
    name: String,
    instructions: mutable.ArrayBuffer[Instruction]
  ): Unit =
    if currentScope.isLocal(name) then
      val index = currentScope.lookup(name).get
      if currentScope.isLexical(name) then
        instructions += Instruction.getLocCheck(index)
      else
        instructions += Instruction.getLoc(index)
    else
      instructions += Instruction.getGlobal(name)

  private def emitBindingStore(
    name: String,
    isDeclaration: Boolean,
    isGlobalVar: Boolean,
    instructions: mutable.ArrayBuffer[Instruction]
  ): Unit =
    if !isDeclaration && currentScope.isLocal(name) && currentScope.isConst(name) then
      throw new Exception(s"Cannot assign to const variable '$name'")

    if isDeclaration then
      if isGlobalVar then
        instructions += Instruction.defVar(name)
      else
        val index = currentScope.lookup(name).get
        instructions += Instruction.putLoc(index)
    else
      if currentScope.isLocal(name) then
        val index = currentScope.lookup(name).get
        instructions += Instruction.putLoc(index)
      else
        instructions += Instruction.putGlobal(name)

  private def emitDestructuring(
    pattern: BindingPattern,
    isDeclaration: Boolean,
    isGlobalVar: Boolean,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = pattern match
    case BindingAssignment(target, defaultValue, _) =>
      emitDefaultIfUndefined(defaultValue, instructions, constants)
      emitDestructuring(target, isDeclaration, isGlobalVar, instructions, constants)
    case Identifier(name, _) =>
      emitBindingStore(name, isDeclaration, isGlobalVar, instructions)
    case RestElement(argument, _) =>
      // RestElement should be handled by ArrayPattern/ObjectPattern cases
      // If we get here directly, just pass through to the argument
      emitDestructuring(argument, isDeclaration, isGlobalVar, instructions, constants)
    case ArrayPattern(elements, _) =>
      // Find the index of RestElement if present
      val restIndex = elements.indexWhere {
        case _: RestElement => true
        case _ => false
      }

      for ((elem, idx) <- elements.zipWithIndex) do
        elem match
          case null => ()
          case RestElement(argument, _) =>
            // Rest element: collect remaining elements using slice
            // Stack: [array]
            instructions += Instruction.dup()  // [array, array]
            instructions += Instruction.getProp("slice")  // [array, sliceFunc]
            // No swap needed - CallMethod expects [this, func, args...]
            instructions += Instruction.pushI32(idx)  // [array, sliceFunc, startIdx]
            instructions += Instruction.callMethod(1)  // [restArray]
            // NOTE: CallMethod consumed the original array (as 'this'), so stack is now [restArray]
            emitDestructuring(argument, isDeclaration, isGlobalVar, instructions, constants)
          case p: BindingPattern =>
            instructions += Instruction.dup()
            instructions += Instruction.pushI32(idx)
            instructions += Instruction.getElem()
            emitDestructuring(p, isDeclaration, isGlobalVar, instructions, constants)

      // Only drop the array if we didn't have a RestElement (which consumes it)
      if restIndex == -1 then
        instructions += Instruction.drop()
    case ObjectPattern(properties, rest, _) =>
      // Handle regular properties
      for prop <- properties do
        instructions += Instruction.dup()
        prop.key match
          case Identifier(name, _) =>
            instructions += Instruction.getProp(name)
          case s: String =>
            instructions += Instruction.getProp(s)
        emitDestructuring(prop.value, isDeclaration, isGlobalVar, instructions, constants)

      // Only drop the object if we didn't have a rest element
      // (rest element handling consumes the object via the call)
      if rest != null then
        // Create a new object with remaining properties using __objectRest helper
        // Stack: [sourceObj]
        val extractedKeys = properties.map { prop =>
          prop.key match
            case Identifier(name, _) => name
            case s: String => s
        }
        // Get __objectRest function and prepare call
        instructions += Instruction.getGlobal("__objectRest")  // [sourceObj, __objectRest]
        instructions += Instruction.swap()  // [__objectRest, sourceObj]

        // Build and push the keys array as second argument
        instructions += Instruction.newArray(extractedKeys.length)  // [__objectRest, sourceObj, keysArray]
        for (key, idx) <- extractedKeys.zipWithIndex do
          instructions += Instruction.pushI32(idx)  // [__objectRest, sourceObj, keysArray, idx]
          val constIdx = constants.length
          constants += JSValue.fromString(key)
          instructions += Instruction.getConst(constIdx)  // [__objectRest, sourceObj, keysArray, idx, keyStr]
          instructions += Instruction.initElem()  // [__objectRest, sourceObj, keysArray]

        // Call __objectRest(sourceObj, keysArray)
        // NOTE: Call consumes __objectRest, sourceObj, and keysArray from stack
        instructions += Instruction.call(2)  // [restObj]
        emitDestructuring(rest.argument, isDeclaration, isGlobalVar, instructions, constants)
      else
        // No rest element, drop the source object
        instructions += Instruction.drop()

  /** Find all free variables in a statement */
  private def findFreeVariables(stmt: Statement): Set[String] = stmt match
    case ExpressionStatement(expr, _) => findFreeVariables(expr)
    case ImportDeclaration(_, _, _) =>
      Set.empty
    case ExportDefaultDeclaration(decl, _) =>
      decl match
        case expr: Expression => findFreeVariables(expr)
        case stmt: Statement => findFreeVariables(stmt)
    case ExportNamedDeclaration(decl, specifiers, source, _) =>
      val declFree =
        if decl != null then findFreeVariables(decl) else Set.empty
      val specFree =
        if source != null then Set.empty else specifiers.map(_.local.name).toSet
      declFree ++ specFree
    case ExportAllDeclaration(_, _) =>
      Set.empty
    case VariableDeclaration(_, declarations, _) =>
      declarations.flatMap { d =>
        val initFree = if d.init != null then findFreeVariables(d.init) else Set.empty
        // Exclude the variable being declared from free variables
        initFree -- collectBindingNames(d.id)
      }.toSet
    case BlockStatement(statements, _) =>
      statements.flatMap(findFreeVariables).toSet
    case IfStatement(test, consequent, alternate, _) =>
      findFreeVariables(test) ++ findFreeVariables(consequent) ++
        (if alternate != null then findFreeVariables(alternate) else Set.empty)
    case WhileStatement(test, body, _, _) =>
      findFreeVariables(test) ++ findFreeVariables(body)
    case DoWhileStatement(body, test, _, _) =>
      findFreeVariables(body) ++ findFreeVariables(test)
    case SwitchStatement(discriminant, cases, _) =>
      val discFree = findFreeVariables(discriminant)
      val casesFree = cases.flatMap { c =>
        c.test match
          case null => c.consequent.flatMap(findFreeVariables)
          case testExpr => findFreeVariables(testExpr) ++ c.consequent.flatMap(findFreeVariables)
      }
      discFree ++ casesFree
    case ForStatement(init, test, update, body, _, _) =>
      val initFree = init match
        case e: Expression => findFreeVariables(e)
        case s: Statement => findFreeVariables(s)
        case null => Set.empty
      initFree ++ findFreeVariables(test) ++
        findFreeVariables(update) ++ findFreeVariables(body)
    case ForInStatement(left, right, body, _, _) =>
      val leftFree = left match
        case e: Expression => findFreeVariables(e)
        case s: Statement => findFreeVariables(s)
      leftFree ++ findFreeVariables(right) ++ findFreeVariables(body)
    case ForOfStatement(left, right, body, _, _) =>
      val leftFree = left match
        case e: Expression => findFreeVariables(e)
        case s: Statement => findFreeVariables(s)
      leftFree ++ findFreeVariables(right) ++ findFreeVariables(body)
    case FunctionDeclaration(_, params, body, _, _, _, _) =>
      // Function declarations DO expose free variables from their body in nested scopes!
      // We need to look inside to find what variables the function uses
      val paramNames = params.flatMap(collectBindingNames).toSet
      val bodyFree = findFreeVariablesForClosure(body)
      // Exclude parameters - they're not free variables
      bodyFree -- paramNames
    case ReturnStatement(argument, _) =>
      if argument != null then findFreeVariables(argument) else Set.empty
    case ThrowStatement(argument, _) =>
      findFreeVariables(argument)
    case TryStatement(block, handler, finalizer, _) =>
      val handlerFree = handler match
        case null => Set.empty
        case CatchClause(param, body, _) =>
          val boundNames = param match
            case pattern: BindingPattern => collectBindingNames(pattern)
            case null => Set.empty
          findFreeVariables(body) -- boundNames
      val finalizerFree =
        if finalizer != null then findFreeVariables(finalizer) else Set.empty
      findFreeVariables(block) ++ handlerFree ++ finalizerFree
    case WithStatement(obj, body, _) =>
      findFreeVariables(obj) ++ findFreeVariables(body)
    case _ => Set.empty

  /** Compile a function body to bytecode */
  private def compileFunctionBody(
    name: String,
    params: scala.collection.immutable.Seq[BindingPattern],
    body: Statement,
    isConstructor: Boolean = true,
    isStrict: Boolean = false
  ): BytecodeFunction =
    // Create a new scope for the function (with parent as current scope for closures)
    val oldScope = currentScope
    currentScope = new Scope(currentScope)

    // Collect variables declared in this function (params and locals)
    val declaredVars = mutable.Set[String]()
    val paramNamesList = mutable.ArrayBuffer[String]()
    val localVarNamesList = mutable.ArrayBuffer[String]()

    val paramSlots = params.zipWithIndex.map { (param, idx) =>
      val slotName = param match
        case Identifier(name, _) => name
        case BindingAssignment(target: Identifier, _, _) => target.name
        case _ => s"__param$idx"
      (param, slotName)
    }

    for (_, slotName) <- paramSlots do
      declaredVars += slotName
      paramNamesList += slotName
      currentScope.declare(slotName)

    val argumentsIndex =
      if paramNamesList.contains("arguments") then
        -1
      else
        currentScope.declare("arguments")
    if argumentsIndex >= 0 then
      declaredVars += "arguments"
      localVarNamesList += "arguments"

    val paramSlotSet = paramNamesList.toSet
    val paramBindingNames = params.flatMap(collectBindingNames).toSet
    for name <- paramBindingNames if !paramSlotSet.contains(name) do
      declaredVars += name
      localVarNamesList += name
      currentScope.declare(name)

    // Also collect local variable declarations (excluding parameters)
    val localVars = findDeclaredVariables(body)
    declaredVars ++= localVars

    // Track local variable names (for closure capture)
    localVarNamesList ++= (localVars -- paramNamesList.toSet)

    // Declare local variables in scope so GetLoc/PutLoc can find them
    for varName <- localVars do
      currentScope.declare(varName)

    val bytecode = mutable.ArrayBuffer[Byte]()
    val constants = mutable.ArrayBuffer[AnyRef]()
    val instructions = new InstructionBuffer()

    // Initialize parameter patterns and defaults
    for (param, slotName) <- paramSlots do
      withSpan(param.span) {
        val slotIndex = currentScope.lookup(slotName).get
        param match
          case Identifier(_, _) => ()
          case BindingAssignment(target: Identifier, defaultValue, _) if target.name == slotName =>
            instructions += Instruction.getLoc(slotIndex)
            emitDefaultIfUndefined(defaultValue, instructions, constants)
            instructions += Instruction.putLoc(slotIndex)
          case BindingAssignment(target, defaultValue, _) =>
            instructions += Instruction.getLoc(slotIndex)
            emitDefaultIfUndefined(defaultValue, instructions, constants)
            emitDestructuring(target, isDeclaration = false, isGlobalVar = false, instructions, constants)
          case _ =>
            instructions += Instruction.getLoc(slotIndex)
            emitDestructuring(param, isDeclaration = false, isGlobalVar = false, instructions, constants)
      }

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
    withSpan(body.span) {
      instructions += Instruction.returnUndef()
    }

    // Encode instructions to bytecode
    instructions.foreach(inst => bytecode ++= inst.encode())

    // Find free variables (referenced but not declared in this function)
    // Use findFreeVariablesForClosure to look inside nested function expressions
    val allFreeVars = findFreeVariablesForClosure(body)
    val freeVarNames = allFreeVars.filterNot(declaredVars.contains).toArray

    // Get all local variable names from the scope (includes temp vars declared during compilation)
    // This ensures internal variables like __super_N are available for closure capture
    val allLocalVarNames = currentScope.getAllLocalVarNames

    // Restore the parent scope
    currentScope = oldScope

    new BytecodeFunction(
      name = name,
      bytecode = bytecode.toArray,
      constants = constants.toArray,
      stackSize = 256,
      freeVars = freeVarNames,
      paramNames = paramNamesList.toArray,
      localVarNames = allLocalVarNames,
      argumentsIndex = argumentsIndex,
      isConstructor = isConstructor,
      length = computeFunctionLength(params),
      spanMap = buildSpanMap(instructions),
      isStrict = isStrict
    )

  private def compileClassDefinition(
    nameBinding: Option[(String, Int)],
    superClass: Expression | Null,
    body: ClassBody,
    exportToGlobal: Boolean,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    def keyName(key: Identifier | String | Expression): String = key match
      case Identifier(name, _) => name
      case s: String => s
      case _ => "<computed>"

    def emitPropertyKey(key: Identifier | String | Expression): Unit = key match
      case Identifier(name, _) =>
        val constIndex = constants.length
        constants += JSValue.fromString(name)
        instructions += Instruction.getConst(constIndex)
      case s: String =>
        val constIndex = constants.length
        constants += JSValue.fromString(s)
        instructions += Instruction.getConst(constIndex)
      case expr: Expression =>
        compileExpression(expr, instructions, constants)

    def emitDefineProperty(targetIndex: Int, key: Identifier | String | Expression, descIndex: Int): Unit =
      instructions += Instruction.getGlobal("Object")
      instructions += Instruction.getProp("defineProperty")
      instructions += Instruction.getLoc(targetIndex)
      emitPropertyKey(key)
      instructions += Instruction.getLoc(descIndex)
      instructions += Instruction.call(3)
      instructions += Instruction.drop()

    def buildFieldInitStatement(field: FieldDefinition): Statement =
      val thisExpr = ThisExpression(field.span)
      val member = field.key match
        case id: Identifier =>
          MemberExpression(thisExpr, id, computed = false, field.span)
        case s: String =>
          val literal = Literal(JSValue.fromString(s), field.span)
          MemberExpression(thisExpr, literal, computed = true, field.span)
        case expr: Expression =>
          MemberExpression(thisExpr, expr, computed = true, field.span)
      val valueExpr = if field.value != null then field.value else Literal(JSValue.Undefined, field.span)
      val assign = AssignmentExpression(member, valueExpr, field.span)
      ExpressionStatement(assign, field.span)

    val constructorMethod =
      body.elements.collectFirst {
        case m: MethodDefinition
            if !m.isStatic && m.kind == PropertyKind.Method && keyName(m.key) == "constructor" =>
          m
      }

    val instanceMethods =
      body.elements.collect {
        case m: MethodDefinition if !m.isStatic && keyName(m.key) != "constructor" => m
      }
    val staticMethods =
      body.elements.collect {
        case m: MethodDefinition if m.isStatic => m
      }
    val instanceFields =
      body.elements.collect {
        case f: FieldDefinition if !f.isStatic => f
      }
    val staticFields =
      body.elements.collect {
        case f: FieldDefinition if f.isStatic => f
      }

    val fieldInitStatements = instanceFields.map(buildFieldInitStatement)
    val ctorParams =
      constructorMethod match
        case Some(m) => m.params
        case None => Seq.empty

    val ctorBodyStatements =
      constructorMethod match
        case Some(m) =>
          val BlockStatement(stmts, span) = m.body
          fieldInitStatements ++ stmts
        case None =>
          if superClass != null then
            val superCall = ExpressionStatement(CallExpression(SuperExpression(body.span), Seq.empty, body.span), body.span)
            superCall +: fieldInitStatements
          else
            fieldInitStatements

    val ctorBody = BlockStatement(ctorBodyStatements, body.span)
    val className = nameBinding.map(_._1).getOrElse("<anonymous>")
    val captureClassName = !exportToGlobal

    // Evaluate superclass BEFORE compiling constructor so it can be captured
    val (superIndex, superVarName) =
      if superClass != null then
        // Create a unique variable name for the superclass that can be captured
        val varName = s"__super_${tempVarCounter}"
        tempVarCounter += 1
        val idx = currentScope.declare(varName)
        compileExpression(superClass, instructions, constants)
        instructions += Instruction.putLoc(idx)
        (Some(idx), Some(varName))
      else
        (None, None)

    val ctorFunc = withClassContext(className, captureClassName) {
      withSuperContext(superClass, isStatic = false, superVarName) {
        compileFunctionBody(className, ctorParams, ctorBody, isConstructor = true)
      }
    }

    val ctorConstIndex = constants.length
    constants += ctorFunc
    val ctorIndex = allocateTempLocal("__classCtor")
    instructions += Instruction.getConst(ctorConstIndex)
    instructions += Instruction.putLoc(ctorIndex)

    nameBinding.foreach { (name, idx) =>
      instructions += Instruction.getLoc(ctorIndex)
      instructions += Instruction.putLoc(idx)
      if exportToGlobal then
        instructions += Instruction.getLoc(ctorIndex)
        instructions += Instruction.putGlobal(name)
    }

    val protoIndex =
      superIndex match
        case Some(superIdx) =>
          val protoIdx = allocateTempLocal("__classProto")
          instructions += Instruction.newObject()
          instructions += Instruction.putLoc(protoIdx)

          instructions += Instruction.getGlobal("Object")
          instructions += Instruction.getProp("setPrototypeOf")
          instructions += Instruction.getLoc(ctorIndex)
          instructions += Instruction.getLoc(superIdx)
          instructions += Instruction.call(2)
          instructions += Instruction.drop()

          instructions += Instruction.getGlobal("Object")
          instructions += Instruction.getProp("setPrototypeOf")
          instructions += Instruction.getLoc(protoIdx)
          instructions += Instruction.getLoc(superIdx)
          instructions += Instruction.getProp("prototype")
          instructions += Instruction.call(2)
          instructions += Instruction.drop()

          instructions += Instruction.getLoc(ctorIndex)
          instructions += Instruction.getLoc(protoIdx)
          instructions += Instruction.setProp("prototype")
          instructions += Instruction.drop()

          instructions += Instruction.getLoc(protoIdx)
          instructions += Instruction.getLoc(ctorIndex)
          instructions += Instruction.setProp("constructor")
          instructions += Instruction.drop()

          Some(protoIdx)
        case None =>
          val protoIdx = allocateTempLocal("__classProto")
          instructions += Instruction.getLoc(ctorIndex)
          instructions += Instruction.getProp("prototype")
          instructions += Instruction.putLoc(protoIdx)
          Some(protoIdx)

    for method <- instanceMethods do
      val methodName = keyName(method.key)
      val funcName =
        method.kind match
          case PropertyKind.Getter => s"get $methodName"
          case PropertyKind.Setter => s"set $methodName"
          case _ => methodName
      val methodFunc = withClassContext(className, captureClassName) {
        withSuperContext(superClass, isStatic = false, superVarName) {
          compileFunctionBody(funcName, method.params, method.body, isConstructor = false)
        }
      }
      val constIndex = constants.length
      constants += methodFunc

      protoIndex.foreach { idx =>
        method.kind match
          case PropertyKind.Method =>
            instructions += Instruction.getLoc(idx)
            instructions += Instruction.getConst(constIndex)
            instructions += Instruction.setProp(methodName)
            instructions += Instruction.drop()
          case _ =>
            val descIndex = allocateTempLocal("__classDesc")
            instructions += Instruction.newObject()
            instructions += Instruction.putLoc(descIndex)
            instructions += Instruction.getLoc(descIndex)
            instructions += Instruction.getConst(constIndex)
            method.kind match
              case PropertyKind.Getter => instructions += Instruction.setProp("get")
              case PropertyKind.Setter => instructions += Instruction.setProp("set")
              case _ => instructions += Instruction.setProp("value")
            instructions += Instruction.drop()
            // Set configurable: true so getter/setter pairs can be merged
            instructions += Instruction.getLoc(descIndex)
            instructions += Instruction.pushTrue()
            instructions += Instruction.setProp("configurable")
            instructions += Instruction.drop()
            emitDefineProperty(idx, method.key, descIndex)
      }

    for method <- staticMethods do
      val methodName = keyName(method.key)
      val funcName =
        method.kind match
          case PropertyKind.Getter => s"get $methodName"
          case PropertyKind.Setter => s"set $methodName"
          case _ => methodName
      val methodFunc = withClassContext(className, captureClassName) {
        withSuperContext(superClass, isStatic = true, superVarName) {
          compileFunctionBody(funcName, method.params, method.body, isConstructor = false)
        }
      }
      val constIndex = constants.length
      constants += methodFunc
      method.kind match
        case PropertyKind.Method =>
          instructions += Instruction.getLoc(ctorIndex)
          instructions += Instruction.getConst(constIndex)
          instructions += Instruction.setProp(methodName)
          instructions += Instruction.drop()
        case _ =>
          val descIndex = allocateTempLocal("__classDesc")
          instructions += Instruction.newObject()
          instructions += Instruction.putLoc(descIndex)
          instructions += Instruction.getLoc(descIndex)
          instructions += Instruction.getConst(constIndex)
          method.kind match
            case PropertyKind.Getter => instructions += Instruction.setProp("get")
            case PropertyKind.Setter => instructions += Instruction.setProp("set")
            case _ => instructions += Instruction.setProp("value")
          instructions += Instruction.drop()
          // Set configurable: true so getter/setter pairs can be merged
          instructions += Instruction.getLoc(descIndex)
          instructions += Instruction.pushTrue()
          instructions += Instruction.setProp("configurable")
          instructions += Instruction.drop()
          emitDefineProperty(ctorIndex, method.key, descIndex)

    for field <- staticFields do
      field.key match
        case Identifier(name, _) =>
          instructions += Instruction.getLoc(ctorIndex)
          field.value match
            case null => instructions += Instruction.pushUndefined()
            case expr => withStaticFieldThis(ctorIndex) { compileExpression(expr, instructions, constants) }
          instructions += Instruction.setProp(name)
          instructions += Instruction.drop()
        case s: String =>
          instructions += Instruction.getLoc(ctorIndex)
          field.value match
            case null => instructions += Instruction.pushUndefined()
            case expr => withStaticFieldThis(ctorIndex) { compileExpression(expr, instructions, constants) }
          instructions += Instruction.setProp(s)
          instructions += Instruction.drop()
        case expr: Expression =>
          instructions += Instruction.getLoc(ctorIndex)
          compileExpression(expr, instructions, constants)
          field.value match
            case null => instructions += Instruction.pushUndefined()
            case valueExpr => withStaticFieldThis(ctorIndex) { compileExpression(valueExpr, instructions, constants) }
          instructions += Instruction.setElem()
          instructions += Instruction.drop()

    instructions += Instruction.getLoc(ctorIndex)

  private def computeFunctionLength(params: scala.collection.immutable.Seq[BindingPattern]): Int =
    val defaultIndex = params.indexWhere {
      case BindingAssignment(_, _, _) => true
      case _ => false
    }
    if defaultIndex == -1 then params.length else defaultIndex

  /** Compile arrow function body */
  private def compileArrowFunctionBody(
    params: scala.collection.immutable.Seq[BindingPattern],
    body: Either[Expression, BlockStatement],
    isStrict: Boolean = false
  ): BytecodeFunction =
    // Create a new scope for the arrow function
    val oldScope = currentScope
    currentScope = new Scope(currentScope)

    // Collect variables declared in this function (params and locals)
    val declaredVars = mutable.Set[String]()
    val paramNamesList = mutable.ArrayBuffer[String]()
    val localVarNamesList = mutable.ArrayBuffer[String]()

    val paramSlots = params.zipWithIndex.map { (param, idx) =>
      val slotName = param match
        case Identifier(name, _) => name
        case BindingAssignment(target: Identifier, _, _) => target.name
        case _ => s"__param$idx"
      (param, slotName)
    }

    for (_, slotName) <- paramSlots do
      declaredVars += slotName
      paramNamesList += slotName
      currentScope.declare(slotName)

    val paramSlotSet = paramNamesList.toSet
    val paramBindingNames = params.flatMap(collectBindingNames).toSet
    for name <- paramBindingNames if !paramSlotSet.contains(name) do
      declaredVars += name
      localVarNamesList += name
      currentScope.declare(name)
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
    val instructions = new InstructionBuffer()

    // Initialize parameter patterns and defaults
    for (param, slotName) <- paramSlots do
      withSpan(param.span) {
        val slotIndex = currentScope.lookup(slotName).get
        param match
          case Identifier(_, _) => ()
          case BindingAssignment(target: Identifier, defaultValue, _) if target.name == slotName =>
            instructions += Instruction.getLoc(slotIndex)
            emitDefaultIfUndefined(defaultValue, instructions, constants)
            instructions += Instruction.putLoc(slotIndex)
          case BindingAssignment(target, defaultValue, _) =>
            instructions += Instruction.getLoc(slotIndex)
            emitDefaultIfUndefined(defaultValue, instructions, constants)
            emitDestructuring(target, isDeclaration = false, isGlobalVar = false, instructions, constants)
          case _ =>
            instructions += Instruction.getLoc(slotIndex)
            emitDestructuring(param, isDeclaration = false, isGlobalVar = false, instructions, constants)
      }

    // Compile the function body based on its type
    body match
      case Left(expr) =>
        // Concise body: expression is implicitly returned
        compileExpression(expr, instructions, constants)
        withSpan(expr.span) {
          instructions += Instruction.returnInst()
        }
      case Right(block) =>
        // Block body: compile statements and return undefined implicitly
        for s <- block.statements do
          compileStatement(s, instructions, constants, false)
        withSpan(block.span) {
          instructions += Instruction.returnUndef()
        }

    // Encode instructions to bytecode
    instructions.foreach(inst => bytecode ++= inst.encode())

    // Find free variables (referenced but not declared in this function)
    val allFreeVars = body match
      case Left(expr) => findFreeVariablesForClosure(expr)
      case Right(block) => findFreeVariablesForClosure(block)
    val freeVarNames = allFreeVars.filterNot(declaredVars.contains).toArray

    // Get all local variable names from the scope (includes temp vars declared during compilation)
    val allLocalVarNames = currentScope.getAllLocalVarNames

    // Restore the parent scope
    currentScope = oldScope

    new BytecodeFunction(
      name = "<arrow>",
      bytecode = bytecode.toArray,
      constants = constants.toArray,
      stackSize = 256,
      freeVars = freeVarNames,
      paramNames = paramNamesList.toArray,
      localVarNames = allLocalVarNames,
      argumentsIndex = -1,
      isConstructor = false,
      length = computeFunctionLength(params),
      spanMap = buildSpanMap(instructions),
      isStrict = isStrict
    )

  def compileScript(script: Script): BytecodeFunction =
    // Reset scope for each script compilation (fixes test isolation issues)
    currentScope = new Scope(null)

    val bytecode = mutable.ArrayBuffer[Byte]()
    val constants = mutable.ArrayBuffer[AnyRef]()
    val instructions = new InstructionBuffer()

    // Compile each statement
    // Variables will be declared as we encounter them (not pre-declared)
    // This allows proper shadowing for let/const in block scopes
    for (stmt, index) <- script.body.zipWithIndex do
      val isLast = index == script.body.length - 1
      compileStatement(stmt, instructions, constants, isLast && replMode)

    // Add implicit return undefined (unless last expression already returns value)
    // In REPL mode, the last expression is returned
    if script.body.isEmpty || !(script.body.last.isInstanceOf[ExpressionStatement] && replMode) then
      withSpan(script.span) {
        instructions += Instruction.returnUndef()
      }

    // Encode instructions to bytecode
    instructions.foreach(inst => bytecode ++= inst.encode())

    // Get all local variable names from the scope (includes internal temp vars like __super_N)
    val localVarNames = currentScope.getAllLocalVarNames

    new BytecodeFunction(
      name = "<script>",
      bytecode = bytecode.toArray,
      constants = constants.toArray,
      stackSize = 256,  // Fixed stack size for now
      localVarNames = localVarNames,  // Scripts now have local variables for let/const scoping and internal temps
      spanMap = buildSpanMap(instructions),
      isStrict = script.strict
    )

  def compileModule(script: Script, moduleName: String): BytecodeFunction =
    val previous = currentModuleName
    currentModuleName = moduleName
    try compileScript(script)
    finally currentModuleName = previous

  private def compileStatement(
    stmt: Statement,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef],
    isLastREPLExpression: Boolean = false
  ): Unit =
    val prevSpan = currentSpan
    currentSpan = stmt.span
    try
      stmt match
        case ImportDeclaration(specifiers, source, _) =>
          if specifiers.isEmpty then
            instructions += Instruction.getGlobal("__moduleImport")
            pushStringConst(source, instructions, constants)
            instructions += Instruction.call(1)
            instructions += Instruction.drop()
          else
            instructions += Instruction.getGlobal("__moduleImport")
            pushStringConst(source, instructions, constants)
            instructions += Instruction.call(1)
            val moduleIndex = allocateTempLocal("__importModule")
            instructions += Instruction.putLoc(moduleIndex)
            for spec <- specifiers do
              spec match
                case ImportNamespaceSpecifier(local, _) =>
                  val index = currentScope.declare(local.name, isLexical = true, isConst = true)
                  instructions += Instruction.setLocUninitialized(index)
                  instructions += Instruction.setLocConst(index)
                  instructions += Instruction.getLoc(moduleIndex)
                  instructions += Instruction.putLoc(index)
                case ImportDefaultSpecifier(local, _) =>
                  val index = currentScope.declare(local.name, isLexical = true, isConst = true)
                  instructions += Instruction.setLocUninitialized(index)
                  instructions += Instruction.setLocConst(index)
                  instructions += Instruction.getLoc(moduleIndex)
                  instructions += Instruction.getProp("default")
                  instructions += Instruction.putLoc(index)
                case ImportNamedSpecifier(imported, local, _) =>
                  val index = currentScope.declare(local.name, isLexical = true, isConst = true)
                  instructions += Instruction.setLocUninitialized(index)
                  instructions += Instruction.setLocConst(index)
                  instructions += Instruction.getLoc(moduleIndex)
                  instructions += Instruction.getProp(imported.name)
                  instructions += Instruction.putLoc(index)

        case ExportDefaultDeclaration(declaration, _) =>
          declaration match
            case expr: Expression =>
              emitModuleExportCall("default", instructions, constants) {
                compileExpression(expr, instructions, constants)
              }
            case stmtDecl: Statement =>
              compileStatement(stmtDecl, instructions, constants, false)
              stmtDecl match
                case FunctionDeclaration(id, _, _, _, _, _, _) =>
                  emitModuleExportCall("default", instructions, constants) {
                    emitLoadIdentifierValue(id.name, instructions)
                  }
                case ClassDeclaration(id, _, _, _) =>
                  emitModuleExportCall("default", instructions, constants) {
                    emitLoadIdentifierValue(id.name, instructions)
                  }
                case _ => ()

        case ExportNamedDeclaration(declaration, specifiers, source, _) =>
          if declaration != null then
            compileStatement(declaration, instructions, constants, false)
            val exportNames = declaration match
              case VariableDeclaration(_, declarations, _) =>
                declarations.flatMap(d => collectBindingNames(d.id))
              case FunctionDeclaration(id, _, _, _, _, _, _) =>
                Seq(id.name)
              case ClassDeclaration(id, _, _, _) =>
                Seq(id.name)
              case _ =>
                Seq.empty
            for name <- exportNames do
              emitModuleExportCall(name, instructions, constants) {
                emitLoadIdentifierValue(name, instructions)
              }
          if specifiers.nonEmpty then
            source match
              case null =>
                for spec <- specifiers do
                  emitModuleExportCall(spec.exported.name, instructions, constants) {
                    emitLoadIdentifierValue(spec.local.name, instructions)
                  }
              case modName: String =>
                instructions += Instruction.getGlobal("__moduleImport")
                pushStringConst(modName, instructions, constants)
                instructions += Instruction.call(1)
                val moduleIndex = allocateTempLocal("__exportModule")
                instructions += Instruction.putLoc(moduleIndex)
                for spec <- specifiers do
                  emitModuleExportCall(spec.exported.name, instructions, constants) {
                    instructions += Instruction.getLoc(moduleIndex)
                    instructions += Instruction.getProp(spec.local.name)
                  }

        case ExportAllDeclaration(source, _) =>
          instructions += Instruction.getGlobal("__moduleExportAll")
          pushStringConst(currentModuleName, instructions, constants)
          pushStringConst(source, instructions, constants)
          instructions += Instruction.call(2)
          instructions += Instruction.drop()

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
            compileVariableDeclarator(decl, kind, instructions, constants)

        case ClassDeclaration(id, superClass, body, _) =>
          val index = currentScope.declare(id.name, isLexical = true, isConst = false)
          instructions += Instruction.setLocUninitialized(index)
          compileClassDefinition(Some(id.name -> index), superClass, body, exportToGlobal = false, instructions, constants)
          instructions += Instruction.drop()

        case BlockStatement(stmts, _) =>
          // Check if block contains any let/const declarations
          val hasLexicalDecls = stmts.exists {
            case VariableDeclaration(kind, _, _) =>
              kind == VariableKind.Let || kind == VariableKind.Const
            case _ => false
          }

          // Enter block scope if block contains let/const declarations
          if hasLexicalDecls then
            val scopeIndex = currentScope.enterBlockScope()
            instructions += Instruction.enterScope(scopeIndex)

          // Compile each statement in the block
          // If this block is the last expression in REPL mode, the last statement in the block
          // should also be treated as the last expression (so its value is returned)
          for (s, index) <- stmts.zipWithIndex do
            val isLastInBlock = index == stmts.length - 1
            val isLastREPLInBlock = isLastREPLExpression && isLastInBlock
            compileStatement(s, instructions, constants, isLastREPLInBlock)

          // Leave block scope if we entered one
          if hasLexicalDecls then
            val scopeIndex = currentScope.leaveBlockScope()
            instructions += Instruction.leaveScope(scopeIndex)

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

        case WhileStatement(test, body, label, _) =>
          val labelName = if label != null then Some(label.name) else None
          enterLoop(labelName)
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

        case DoWhileStatement(body, test, label, _) =>
          val labelName = if label != null then Some(label.name) else None
          enterLoop(labelName)
          val loopStartBytePos = instructions.foldLeft(0)(_ + _.size)

          // Compile body (do-while executes body at least once)
          compileStatement(body, instructions, constants, false)

          // Compile test
          compileExpression(test, instructions, constants)

          // Jump back to loop start if true
          val currentBytePos = instructions.foldLeft(0)(_ + _.size)
          val backJumpOffset = loopStartBytePos - currentBytePos - 1
          instructions += Instruction.ifTrue(backJumpOffset)

          // Set exit point (for break statements) - after the conditional jump
          val exitBytePos = instructions.foldLeft(0)(_ + _.size)
          setLoopExit(exitBytePos, instructions)

          // Set continue point (for continue statements) - jump to test
          setLoopContinue(loopStartBytePos, instructions)

          exitLoop()

        case SwitchStatement(discriminant, cases, _) =>
          // Switch statement compilation - simplified QuickJS pattern
          // Key: evaluate discriminant ONCE, use dup for each comparison

          enterSwitch()  // Switch statements support break but NOT continue

          def getBytecodePos(): Int =
            instructions.map(_.size).sum

          // Evaluate discriminant ONCE and leave on stack
          compileExpression(discriminant, instructions, constants)

          // Track jumps that need patching: (instructionIndex, caseIndex)
          val caseJumps = scala.collection.mutable.ArrayBuffer[(Int, Int)]()
          val defaultCaseIdx = cases.indexWhere(_.test == null)

          // Generate comparison code for each non-default case
          for (switchCase, caseIdx) <- cases.zipWithIndex do
            if switchCase.test != null then
              // dup discriminant and compare with case test
              instructions += Instruction.dup()
              compileExpression(switchCase.test, instructions, constants)
              instructions += Instruction.binary(BinaryOpcode.StrictEq)

              // If equal, jump to this case body (placeholder offset)
              caseJumps += ((instructions.length, caseIdx))
              instructions += Instruction.ifTrue(0)

          // If no case matched, jump to default or exit
          val fallThroughJumpIdx = instructions.length
          val fallThroughBytecodePos = getBytecodePos()
          instructions += Instruction.goto(0)  // placeholder

          // Record where each case body starts (in bytecode bytes)
          val caseBodyBytecodePos = scala.collection.mutable.ArrayBuffer[Int]()
          for (switchCase, caseIdx) <- cases.zipWithIndex do
            caseBodyBytecodePos += getBytecodePos()
            for stmt <- switchCase.consequent do
              compileStatement(stmt, instructions, constants, false)

          // Exit point for switch (where break statements jump to)
          val exitBytecodePos = getBytecodePos()
          setLoopExit(exitBytecodePos, instructions)

          // Patch all the case jumps
          for (jumpIdx, caseIdx) <- caseJumps do
            val targetBytecodePos = caseBodyBytecodePos(caseIdx)
            // Calculate offset: we need the position of the jump instruction
            val jumpBytecodePos = instructions.slice(0, jumpIdx).map(_.size).sum
            val offset = targetBytecodePos - jumpBytecodePos - 1
            instructions(jumpIdx) = Instruction.ifTrue(offset)

          // Patch the fall-through jump
          val fallThroughTarget = if defaultCaseIdx >= 0 then caseBodyBytecodePos(defaultCaseIdx) else exitBytecodePos
          val fallThroughOffset = fallThroughTarget - fallThroughBytecodePos - 1
          instructions(fallThroughJumpIdx) = Instruction.goto(fallThroughOffset)

          exitLoop()

        case ForStatement(init, test, update, body, label, _) =>
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

          val labelName = if label != null then Some(label.name) else None
          enterLoop(labelName)

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

        case ForInStatement(left, right, body, label, _) =>
          val labelName = if label != null then Some(label.name) else None
          enterLoop(labelName)

          left match
            case decl: VariableDeclaration =>
              val decls = decl.declarations.map(d => VariableDeclarator(d.id, null, d.span))
              val cleaned = VariableDeclaration(decl.kind, decls, decl.span)
              compileStatement(cleaned, instructions, constants, false)
            case _ => ()

          val keysIndex = allocateTempLocal("__forInKeys")
          val objIndex = allocateTempLocal("__forInObj")
          val indexIndex = allocateTempLocal("__forInIndex")

          compileExpression(right, instructions, constants)
          instructions += Instruction.putLoc(objIndex)
          instructions += Instruction.getGlobal("__forInKeys")
          instructions += Instruction.getLoc(objIndex)
          instructions += Instruction.call(1)
          instructions += Instruction.putLoc(keysIndex)
          instructions += Instruction.pushI32(0)
          instructions += Instruction.putLoc(indexIndex)

          val gotoTestIdx = instructions.length
          val gotoTestBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)

          val labelContBytePos = instructions.foldLeft(0)(_ + _.size)

          instructions += Instruction.getLoc(indexIndex)
          instructions += Instruction.pushI32(1)
          instructions += Instruction.binary(BinaryOpcode.Add)
          instructions += Instruction.putLoc(indexIndex)

          val labelTestBytePos = instructions.foldLeft(0)(_ + _.size)
          val gotoTestOffset = labelTestBytePos - gotoTestBytePos - 1
          instructions(gotoTestIdx) = Instruction.goto(gotoTestOffset)

          instructions += Instruction.getLoc(indexIndex)
          instructions += Instruction.getLoc(keysIndex)
          instructions += Instruction.getProp("length")
          instructions += Instruction.binary(BinaryOpcode.Lt)

          val jumpIfFalseIdx = instructions.length
          val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.ifFalse(0)

          val gotoBodyIdx = instructions.length
          val gotoBodyBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)

          val labelBodyBytePos = instructions.foldLeft(0)(_ + _.size)
          val gotoBodyOffset = labelBodyBytePos - gotoBodyBytePos - 1
          instructions(gotoBodyIdx) = Instruction.goto(gotoBodyOffset)

          instructions += Instruction.getLoc(keysIndex)
          instructions += Instruction.getLoc(indexIndex)
          instructions += Instruction.getElem()
          instructions += Instruction.getGlobal("__forInIsEnumerable")
          instructions += Instruction.getLoc(objIndex)
          instructions += Instruction.getLoc(keysIndex)
          instructions += Instruction.getLoc(indexIndex)
          instructions += Instruction.getElem()
          instructions += Instruction.call(2)
          val skipBodyIfFalseIdx = instructions.length
          val skipBodyIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.ifFalse(0)

          instructions += Instruction.getLoc(keysIndex)
          instructions += Instruction.getLoc(indexIndex)
          instructions += Instruction.getElem()
          emitForInAssignment(left, instructions, constants)

          compileStatement(body, instructions, constants, false)

          val gotoContIdx = instructions.length
          val gotoContBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)

          val labelBreakBytePos = instructions.foldLeft(0)(_ + _.size)
          val gotoContOffset = labelContBytePos - gotoContBytePos - 1
          instructions(gotoContIdx) = Instruction.goto(gotoContOffset)

          val ifFalseOffset = labelBreakBytePos - jumpIfFalseBytePos - 1
          instructions(jumpIfFalseIdx) = Instruction.ifFalse(ifFalseOffset)

          val skipBodyOffset = gotoContBytePos - skipBodyIfFalseBytePos - 1
          instructions(skipBodyIfFalseIdx) = Instruction.ifFalse(skipBodyOffset)

          setLoopExit(labelBreakBytePos, instructions)
          setLoopContinue(labelContBytePos, instructions)

          exitLoop()

        case ForOfStatement(left, right, body, label, _) =>
          // for-of iterates over values of an iterable (array, string, etc.)
          val labelName = if label != null then Some(label.name) else None
          enterLoop(labelName)

          // Declare variables if left is a variable declaration
          left match
            case decl: VariableDeclaration =>
              val decls = decl.declarations.map(d => VariableDeclarator(d.id, null, d.span))
              val cleaned = VariableDeclaration(decl.kind, decls, decl.span)
              compileStatement(cleaned, instructions, constants, false)
            case _ => ()

          // Allocate temp locals for iteration state
          val iterableIndex = allocateTempLocal("__forOfIterable")
          val indexIndex = allocateTempLocal("__forOfIndex")
          val lengthIndex = allocateTempLocal("__forOfLength")

          // Evaluate the iterable and store it
          compileExpression(right, instructions, constants)
          instructions += Instruction.putLoc(iterableIndex)

          // Get the length and store it
          instructions += Instruction.getLoc(iterableIndex)
          instructions += Instruction.getProp("length")
          instructions += Instruction.putLoc(lengthIndex)

          // Initialize index to 0
          instructions += Instruction.pushI32(0)
          instructions += Instruction.putLoc(indexIndex)

          // Jump to test
          val gotoTestIdx = instructions.length
          val gotoTestBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)

          // Continue label: increment index
          val labelContBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.getLoc(indexIndex)
          instructions += Instruction.pushI32(1)
          instructions += Instruction.binary(BinaryOpcode.Add)
          instructions += Instruction.putLoc(indexIndex)

          // Test: check if index < length
          val labelTestBytePos = instructions.foldLeft(0)(_ + _.size)
          val gotoTestOffset = labelTestBytePos - gotoTestBytePos - 1
          instructions(gotoTestIdx) = Instruction.goto(gotoTestOffset)

          instructions += Instruction.getLoc(indexIndex)
          instructions += Instruction.getLoc(lengthIndex)
          instructions += Instruction.binary(BinaryOpcode.Lt)

          // Jump to end if index >= length
          val jumpIfFalseIdx = instructions.length
          val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.ifFalse(0)

          // For const declarations, reset to uninitialized at start of each iteration
          // This allows re-assignment in the loop (each iteration has a fresh binding)
          left match
            case decl: VariableDeclaration if decl.kind == VariableKind.Const =>
              decl.declarations.head.id match
                case Identifier(name, _) =>
                  if currentScope.isLocal(name) then
                    val index = currentScope.lookup(name).get
                    instructions += Instruction.setLocUninitialized(index)
                case _ => ()
            case _ => ()

          // Body: get current value and assign to left
          instructions += Instruction.getLoc(iterableIndex)
          instructions += Instruction.getLoc(indexIndex)
          instructions += Instruction.getElem()
          emitForInAssignment(left, instructions, constants)  // Reuse for-in assignment logic

          // Compile the loop body
          compileStatement(body, instructions, constants, false)

          // Jump back to continue (increment)
          val gotoContIdx = instructions.length
          val gotoContBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)

          // Break label: end of loop
          val labelBreakBytePos = instructions.foldLeft(0)(_ + _.size)

          // Fix up jumps
          val gotoContOffset = labelContBytePos - gotoContBytePos - 1
          instructions(gotoContIdx) = Instruction.goto(gotoContOffset)

          val ifFalseOffset = labelBreakBytePos - jumpIfFalseBytePos - 1
          instructions(jumpIfFalseIdx) = Instruction.ifFalse(ifFalseOffset)

          setLoopExit(labelBreakBytePos, instructions)
          setLoopContinue(labelContBytePos, instructions)

          exitLoop()

        case FunctionDeclaration(id, params, body, isGenerator, _, strict, _) =>
          // Compile the function body to bytecode
          val funcBytecode = compileFunctionBody(id.name, params, body, isConstructor = !isGenerator, isStrict = strict)

          // Store BytecodeFunction in constants array (will be converted to JSValue.Function at runtime with closure)
          val constIndex = constants.length
          constants += funcBytecode

          // Push the function from constants, then store it in global scope
          instructions += Instruction.getConst(constIndex)
          instructions += Instruction.defFun(id.name)

        case ReturnStatement(argument, _) =>
          if finallyStack.nonEmpty then
            if argument != null then
              compileExpression(argument, instructions, constants)
              val retIndex = allocateTempLocal("__finallyRet")
              instructions += Instruction.putLoc(retIndex)
              emitActiveFinallyBlocks(instructions, constants)
              instructions += Instruction.getLoc(retIndex)
              instructions += Instruction.returnInst()
            else
              emitActiveFinallyBlocks(instructions, constants)
              instructions += Instruction.returnUndef()
          else if argument != null then
            compileExpression(argument, instructions, constants)
            instructions += Instruction.returnInst()
          else
            instructions += Instruction.returnUndef()

        case ThrowStatement(argument, _) =>
          compileExpression(argument, instructions, constants)
          instructions += Instruction.throwInst()

        case TryStatement(block, handler, finalizer, _) =>
          def bytePos: Int = instructions.foldLeft(0)(_ + _.size)

          val tryStartIdx = instructions.length
          instructions += Instruction.tryStart(0, 0)

          if finalizer != null then
            finallyStack = finalizer :: finallyStack

          compileStatement(block, instructions, constants, false)

          instructions += Instruction.tryEnd()

          val gotoAfterTryIdx =
            if handler != null then
              val gotoIdx = instructions.length
              instructions += Instruction.goto(0)
              gotoIdx
            else
              -1

          val catchBytePos = if handler != null then bytePos else -1
          var catchGotoFinallyIdx = -1
          var catchTryStartIdx = -1

          if handler != null then
            val CatchClause(param, body, _) = handler
            if finalizer != null then
              catchTryStartIdx = instructions.length
              instructions += Instruction.tryStart(-1, 0)

            val scopeIndex = currentScope.enterBlockScope()
            instructions += Instruction.enterScope(scopeIndex)
            param match
              case pattern: BindingPattern =>
                val boundNames = collectBindingNames(pattern)
                for name <- boundNames do
                  val index = currentScope.declare(name, isLexical = true, isConst = false)
                  instructions += Instruction.setLocUninitialized(index)
                instructions += Instruction.getException()
                emitDestructuring(pattern, isDeclaration = true, isGlobalVar = false, instructions, constants)
              case null =>
                ()

            compileStatement(body, instructions, constants, false)

            instructions += Instruction.leaveScope(scopeIndex)

            if finalizer != null then
              instructions += Instruction.tryEnd()
              catchGotoFinallyIdx = instructions.length
              instructions += Instruction.goto(0)

          if finalizer != null then
            finallyStack = finallyStack.tail

          val finallyBytePos = if finalizer != null then bytePos else -1
          if finalizer != null then
            compileStatement(finalizer, instructions, constants, false)
            instructions += Instruction.rethrowIfPending()

          val endBytePos = bytePos

          val patchedCatchPc = if handler != null then catchBytePos else -1
          val patchedFinallyPc = if finalizer != null then finallyBytePos else -1
          instructions(tryStartIdx) = Instruction.tryStart(patchedCatchPc, patchedFinallyPc)

          if catchTryStartIdx >= 0 then
            instructions(catchTryStartIdx) = Instruction.tryStart(-1, patchedFinallyPc)

          if gotoAfterTryIdx >= 0 then
            val gotoAfterTryBytePos = instructions.slice(0, gotoAfterTryIdx).map(_.size).sum
            val targetPos = if finalizer != null then finallyBytePos else endBytePos
            val offset = targetPos - gotoAfterTryBytePos - 1
            instructions(gotoAfterTryIdx) = Instruction.goto(offset)

          if catchGotoFinallyIdx >= 0 then
            val catchGotoBytePos = instructions.slice(0, catchGotoFinallyIdx).map(_.size).sum
            val offset = finallyBytePos - catchGotoBytePos - 1
            instructions(catchGotoFinallyIdx) = Instruction.goto(offset)

        case WithStatement(obj, body, _) =>
          compileExpression(obj, instructions, constants)
          instructions += Instruction.pushWith()
          compileStatement(body, instructions, constants, false)
          instructions += Instruction.popWith()

        case BreakStatement(label, _) =>
          emitActiveFinallyBlocks(instructions, constants)
          // Handle labeled and unlabeled break following QuickJS C pattern
          if label == null then
            // Unlabeled break: find innermost loop or switch (skip regular labeled statements)
            loopStack.indexWhere { case (_, _, _, _, _, _, isRegular) => !isRegular } match
              case -1 =>
                // Not in a loop or switch - semantic error
                instructions += Instruction.breakInst()
              case idx =>
                val (isLoop, labelName, exitBytePos, _, _, _, _) = loopStack(idx)
                if exitBytePos >= 0 then
                  // Exit point known, emit goto directly
                  val currentPos = instructions.foldLeft(0)(_ + _.size)
                  val offset = exitBytePos - currentPos - 1
                  instructions += Instruction.goto(offset)
                else
                  // Exit position not set yet, emit placeholder and add to pending list
                  val breakInstIdx = instructions.length
                  val breakBytePos = instructions.foldLeft(0)(_ + _.size)
                  instructions += Instruction.goto(0)
                  // Add to the target's pending breaks
                  val stackEntry = loopStack(idx)
                  val (isLoop2, labelName2, oldExit, cont, pendingBreaks, pendingContinues, isRegular) = stackEntry
                  pendingBreaks += ((breakInstIdx, breakBytePos))
                  loopStack(idx) = (isLoop2, labelName2, oldExit, cont, pendingBreaks, pendingContinues, isRegular)
          else
            // Labeled break: find the statement with matching label
            findLabeledStatement(label.name) match
              case None =>
                // Label not found - semantic error
                instructions += Instruction.breakInst()
              case Some((isLoop, _, exitBytePos, _, _)) =>
                if exitBytePos >= 0 then
                  val currentPos = instructions.foldLeft(0)(_ + _.size)
                  val offset = exitBytePos - currentPos - 1
                  instructions += Instruction.goto(offset)
                else
                  // Exit position not set yet, need to add to pending list of the specific labeled statement
                  val breakInstIdx = instructions.length
                  val breakBytePos = instructions.foldLeft(0)(_ + _.size)
                  instructions += Instruction.goto(0)
                  // Find the index of the labeled statement on the stack
                  loopStack.indexWhere { case (_, labelName, _, _, _, _, _) =>
                    labelName.exists(_ == label.name)
                  } match
                    case -1 =>
                      // Should not happen since we already found it
                      ()
                    case idx =>
                      // Add to the specific labeled statement's pending breaks
                      val stackEntry = loopStack(idx)
                      val (isLoop2, labelName2, oldExit, cont, pendingBreaks, pendingContinues, isRegular) = stackEntry
                      pendingBreaks += ((breakInstIdx, breakBytePos))
                      loopStack(idx) = (isLoop2, labelName2, oldExit, cont, pendingBreaks, pendingContinues, isRegular)

        case ContinueStatement(label, _) =>
          emitActiveFinallyBlocks(instructions, constants)
          // Handle labeled and unlabeled continue following QuickJS C pattern
          // Continue only works with loops (for/while/do-while), not switches or regular statements
          if label == null then
            // Unlabeled continue: find innermost loop (skip switches and regular statements)
            getCurrentLoopContinue() match
              case Some(contBytePos) if contBytePos >= 0 =>
                val currentPos = instructions.foldLeft(0)(_ + _.size)
                val offset = contBytePos - currentPos - 1
                instructions += Instruction.goto(offset)
              case Some(_) =>
                // Continue position not set yet, emit placeholder and add to pending list
                val contInstIdx = instructions.length
                val contBytePosCalc = instructions.foldLeft(0)(_ + _.size)
                instructions += Instruction.goto(0)
                addPendingContinue(contInstIdx, contBytePosCalc)
              case None =>
                // Not in a loop - semantic error
                instructions += Instruction.continueInst()
          else
            // Labeled continue: find the loop with matching label
            loopStack.indexWhere { case (isLoop, lbl, _, _, _, _, _) =>
              isLoop && lbl.exists(_ == label.name)
            } match
              case -1 =>
                // Label not found or not a loop - semantic error
                instructions += Instruction.continueInst()
              case idx =>
                val (_, _, _, contBytePos, _, _, _) = loopStack(idx)
                if contBytePos >= 0 then
                  val currentPos = instructions.foldLeft(0)(_ + _.size)
                  val offset = contBytePos - currentPos - 1
                  instructions += Instruction.goto(offset)
                else
                  // Continue position not set yet, emit placeholder and add to pending list of the specific labeled loop
                  val contInstIdx = instructions.length
                  val contBytePosCalc = instructions.foldLeft(0)(_ + _.size)
                  instructions += Instruction.goto(0)
                  // Add to the specific labeled loop's pending continues
                  val stackEntry = loopStack(idx)
                  val (isLoop2, labelName2, oldExit, oldCont, pendingBreaks, pendingContinues, isRegular) = stackEntry
                  pendingContinues += ((contInstIdx, contBytePosCalc))
                  loopStack(idx) = (isLoop2, labelName2, oldExit, oldCont, pendingBreaks, pendingContinues, isRegular)

        case _ =>
          throw new UnsupportedOperationException(s"Unsupported statement: $stmt")
    finally
      currentSpan = prevSpan
  private def compileVariableDeclarator(
    decl: VariableDeclarator,
    kind: VariableKind,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    // Check if we're at the top level (script scope)
    val isTopLevel = currentScope.parent == null
    val isModule = currentModuleName != "<script>"

    // Determine if this is a lexical variable (let/const) vs var
    val isLexical = kind == VariableKind.Let || kind == VariableKind.Const
    val isConst = kind == VariableKind.Const
    val isGlobalVar = isTopLevel && !isLexical && !isModule

    decl.id match
      case Identifier(name, _) =>
        if isGlobalVar then
          // Top-level var goes to global scope (for compatibility)
          if decl.init != null then
            compileExpression(decl.init, instructions, constants)
          else
            // Push undefined for global scope
            instructions += Instruction.pushUndefined()

          // Store in global scope
          instructions += Instruction.defVar(name)
        else
          // let/const (at any level) and var in functions use local variables
          // This enables proper shadowing for let/const
          val index = currentScope.declare(name, isLexical, isConst)

          if isConst then
            instructions += Instruction.setLocUninitialized(index)
            instructions += Instruction.setLocConst(index)

          if decl.init != null then
            compileExpression(decl.init, instructions, constants)
            instructions += Instruction.putLoc(index)
          else if isLexical then
            // For let/const without initializer, mark as uninitialized (TDZ)
            instructions += Instruction.setLocUninitialized(index)
          else
            // For var without initializer, initialize to undefined
            instructions += Instruction.pushUndefined()
            instructions += Instruction.putLoc(index)
      case pattern: BindingPattern =>
        if !isGlobalVar then
          for name <- collectBindingNames(pattern) do
            val index = currentScope.declare(name, isLexical, isConst)
            if isConst then
              instructions += Instruction.setLocUninitialized(index)
              instructions += Instruction.setLocConst(index)

        if decl.init != null then
          compileExpression(decl.init, instructions, constants)
        else
          if isConst then
            throw new Exception("Destructuring const declaration requires an initializer")
          instructions += Instruction.pushUndefined()
        emitDestructuring(pattern, isDeclaration = true, isGlobalVar, instructions, constants)

  private def compileExpression(
    expr: Expression,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    withSpan(expr.span):
      expr match
        case Literal(value, _) =>
          compileLiteral(value, instructions, constants)
    
        case Identifier(name, _) =>
          // Look up variable in scope
          // Only use GetLoc/GetLocCheck for variables in the current function's immediate scope
          // For variables from outer scopes (closures), use GetGlobal which checks the closure at runtime
          if currentScope.isLocal(name) then
            val index = currentScope.lookup(name).get
            // Use GetLocCheck for lexical variables (let/const) to enforce TDZ
            if currentScope.isLexical(name) then
              instructions += Instruction.getLocCheck(index)
            else
              instructions += Instruction.getLoc(index)
          else
            // Variable is from outer scope or global - use GetGlobal
            // GetGlobal checks the closure first, then global scope
            instructions += Instruction.getGlobal(name)
    
        case SuperExpression(_) =>
          if currentSuperClass == null then
            instructions += Instruction.getGlobal("ReferenceError")
            val msgIndex = constants.length
            constants += JSValue.fromString("super is not defined")
            instructions += Instruction.getConst(msgIndex)
            instructions += Instruction.call(1)
            instructions += Instruction.throwInst()
          else
            // Use the captured superclass variable if available, otherwise compile the expression
            currentSuperVarName match
              case Some(varName) =>
                instructions += Instruction.getGlobal(varName)
              case None =>
                compileExpression(currentSuperClass, instructions, constants)
            if !currentSuperIsStatic then
              instructions += Instruction.getProp("prototype")
    
        case ClassExpression(id, superClass, body, _) =>
          val nameBinding =
            if id != null then
              val index = currentScope.declare(id.name, isLexical = true, isConst = false)
              instructions += Instruction.setLocUninitialized(index)
              Some(id.name -> index)
            else
              None
          compileClassDefinition(nameBinding, superClass, body, exportToGlobal = id != null, instructions, constants)
    
        case ThisExpression(_) =>
          // Push the 'this' value onto the stack
          currentStaticFieldThis match
            case Some(index) => instructions += Instruction.getLoc(index)
            case None => instructions += Instruction.getThis()
    
        case BinaryExpression(op, left, right, _) =>
          op match
            case BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr =>
              compileExpression(left, instructions, constants)
              instructions += Instruction.dup()
              val jumpIdx = instructions.length
              val jumpBytePos = currentBytecodePos(instructions)
              if op == BinaryOperator.LogicalAnd then
                instructions += Instruction.ifFalse(0)
              else
                instructions += Instruction.ifTrue(0)
              instructions += Instruction.drop()
              compileExpression(right, instructions, constants)
              val endPos = currentBytecodePos(instructions)
              val offset = endPos - jumpBytePos - 1
              if op == BinaryOperator.LogicalAnd then
                instructions(jumpIdx) = Instruction.ifFalse(offset)
              else
                instructions(jumpIdx) = Instruction.ifTrue(offset)
            case BinaryOperator.NullishCoalesce =>
              // a ?? b - returns a if a is not null/undefined, otherwise b
              compileExpression(left, instructions, constants)
              instructions += Instruction.dup()  // [a, a]
              instructions += Instruction.pushNull()  // [a, a, null]
              instructions += Instruction.binary(BinaryOpcode.StrictEq)  // [a, a===null]
              val jumpIfNullIdx = instructions.length
              val jumpIfNullPos = currentBytecodePos(instructions)
              instructions += Instruction.ifTrue(0)  // placeholder

              instructions += Instruction.dup()  // [a, a]
              instructions += Instruction.pushUndefined()  // [a, a, undefined]
              instructions += Instruction.binary(BinaryOpcode.StrictEq)  // [a, a===undefined]
              val jumpIfUndefIdx = instructions.length
              val jumpIfUndefPos = currentBytecodePos(instructions)
              instructions += Instruction.ifTrue(0)  // placeholder

              // a is not null/undefined - keep a as result, skip b
              val jumpToEndIdx = instructions.length
              val jumpToEndPos = currentBytecodePos(instructions)
              instructions += Instruction.goto(0)  // placeholder

              // a is null/undefined - evaluate b
              val evalBPos = currentBytecodePos(instructions)
              instructions += Instruction.drop()  // remove a
              compileExpression(right, instructions, constants)

              val endPos = currentBytecodePos(instructions)

              // Fix up jumps
              instructions(jumpIfNullIdx) = Instruction.ifTrue(evalBPos - jumpIfNullPos - 1)
              instructions(jumpIfUndefIdx) = Instruction.ifTrue(evalBPos - jumpIfUndefPos - 1)
              instructions(jumpToEndIdx) = Instruction.goto(endPos - jumpToEndPos - 1)
            case _ =>
              compileExpression(left, instructions, constants)
              compileExpression(right, instructions, constants)
              instructions += Instruction.binary(binaryOpToOpcode(op))
    
        case UnaryExpression(op, argument, _, _) =>
          // Increment/decrement operators need special handling for identifiers
          // Delete operator needs special handling for member expressions
          (op, argument) match
            case (UnaryOperator.Void, _) =>
              // void expr: evaluate expression, discard result, push undefined
              compileExpression(argument, instructions, constants)
              instructions += Instruction.drop()
              instructions += Instruction.pushUndefined()
            case (UnaryOperator.PreInc | UnaryOperator.PostInc | UnaryOperator.PreDec | UnaryOperator.PostDec, id: Identifier) =>
              // Increment/decrement on identifier - use helper that handles locals, globals, and closures
              compileIncrementDecrement(op, id, instructions, constants)
    
            case (UnaryOperator.PreInc | UnaryOperator.PostInc | UnaryOperator.PreDec | UnaryOperator.PostDec, memberExpr: MemberExpression) =>
              compileMemberIncDec(op, memberExpr, instructions, constants)
    
            case (UnaryOperator.Delete, memberExpr: MemberExpression) =>
              // Delete operator on member expression: delete obj.prop
              // Stack layout: [obj, prop] -> [successBoolean]
              memberExpr match
                case MemberExpression(obj, Identifier(propName, _), false, _, _) =>
                  obj match
                    case Identifier(name, _) if name == "super" =>
                      // super is not supported - throw ReferenceError
                      instructions += Instruction.getGlobal("ReferenceError")
                      val msgIndex = constants.length
                      constants += JSValue.fromString("super is not defined")
                      instructions += Instruction.getConst(msgIndex)
                      instructions += Instruction.call(1)
                      instructions += Instruction.throwInst()
                    case _ =>
                      // Non-computed property access: delete obj.prop
                      // 1. Compile object - leaves [obj]
                      compileExpression(obj, instructions, constants)
                      // 2. Push property name - leaves [obj, prop]
                      val constIndex = constants.length
                      constants += JSValue.fromString(propName)
                      instructions += Instruction.getConst(constIndex)
                      // 3. Apply delete
                      instructions += Instruction.unary(UnaryOpcode.Delete)
                case MemberExpression(obj, prop, true, _, _) =>
                  // Computed property access: delete obj[expr]
                  compileExpression(obj, instructions, constants)
                  compileExpression(prop, instructions, constants)
                  instructions += Instruction.unary(UnaryOpcode.Delete)
                case _ =>
                  compileExpression(argument, instructions, constants)
                  instructions += Instruction.unary(unaryOpToOpcode(op))
    
            case _ =>
              // For other unary operators, use the standard path
              compileExpression(argument, instructions, constants)
              if op != UnaryOperator.Plus then
                instructions += Instruction.unary(unaryOpToOpcode(op))
              // UnaryPlus is a no-op (just coerces to number, which happens automatically)
    
        case CallExpression(callee, arguments, _, optional) =>
          // Check if this is a method call (callee is a MemberExpression)
          callee match
            case SuperExpression(_) =>
              if currentSuperClass == null then
                instructions += Instruction.getGlobal("ReferenceError")
                val msgIndex = constants.length
                constants += JSValue.fromString("super is not defined")
                instructions += Instruction.getConst(msgIndex)
                instructions += Instruction.call(1)
                instructions += Instruction.throwInst()
              else
                instructions += Instruction.getThis()
                // Use the captured superclass variable if available
                currentSuperVarName match
                  case Some(varName) =>
                    instructions += Instruction.getGlobal(varName)
                  case None =>
                    compileExpression(currentSuperClass, instructions, constants)
                for arg <- arguments do
                  compileExpression(arg, instructions, constants)
                instructions += Instruction.callMethod(arguments.length)
            case MemberExpression(SuperExpression(_), prop, computed, _, _) =>
              if currentSuperClass == null then
                instructions += Instruction.getGlobal("ReferenceError")
                val msgIndex = constants.length
                constants += JSValue.fromString("super is not defined")
                instructions += Instruction.getConst(msgIndex)
                instructions += Instruction.call(1)
                instructions += Instruction.throwInst()
              else
                instructions += Instruction.getThis()
                // Use the captured superclass variable if available
                currentSuperVarName match
                  case Some(varName) =>
                    instructions += Instruction.getGlobal(varName)
                  case None =>
                    compileExpression(currentSuperClass, instructions, constants)
                if !currentSuperIsStatic then
                  instructions += Instruction.getProp("prototype")
                if computed then
                  compileExpression(prop, instructions, constants)
                  instructions += Instruction.getElem()
                else
                  val propName = prop match
                    case Identifier(name, _) => name
                    case _ => throw new UnsupportedOperationException(s"Unsupported property key: $prop")
                  instructions += Instruction.getProp(propName)
                for arg <- arguments do
                  compileExpression(arg, instructions, constants)
                instructions += Instruction.callMethod(arguments.length)
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
              // For optional calls, check if callee is null/undefined first
              if optional then
                // Compile callee
                compileExpression(callee, instructions, constants)
                // Check for null/undefined
                instructions += Instruction.dup()  // [func, func]
                instructions += Instruction.pushNull()
                instructions += Instruction.binary(BinaryOpcode.StrictEq)
                val jumpIfNullIdx = instructions.length
                val jumpIfNullPos = currentBytecodePos(instructions)
                instructions += Instruction.ifTrue(0)  // placeholder

                instructions += Instruction.dup()
                instructions += Instruction.pushUndefined()
                instructions += Instruction.binary(BinaryOpcode.StrictEq)
                val jumpIfUndefIdx = instructions.length
                val jumpIfUndefPos = currentBytecodePos(instructions)
                instructions += Instruction.ifTrue(0)  // placeholder

                // Not null/undefined - call the function
                for arg <- arguments do
                  compileExpression(arg, instructions, constants)
                instructions += Instruction.call(arguments.length)

                val jumpToEndIdx = instructions.length
                val jumpToEndPos = currentBytecodePos(instructions)
                instructions += Instruction.goto(0)  // placeholder

                // Null/undefined path - replace func with undefined
                val nullPathPos = currentBytecodePos(instructions)
                instructions += Instruction.drop()  // remove func
                instructions += Instruction.pushUndefined()

                val endPos = currentBytecodePos(instructions)

                // Fix up jumps
                instructions(jumpIfNullIdx) = Instruction.ifTrue(nullPathPos - jumpIfNullPos - 1)
                instructions(jumpIfUndefIdx) = Instruction.ifTrue(nullPathPos - jumpIfUndefPos - 1)
                instructions(jumpToEndIdx) = Instruction.goto(endPos - jumpToEndPos - 1)
              else
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
    
        case FunctionExpression(id, params, body, isGenerator, _, strict, _) =>
          // Compile function expression to bytecode
          val funcName = id match
            case Identifier(name, _) => name
            case null => "<anonymous>"

          val funcBytecode = compileFunctionBody(funcName, params, body, isConstructor = !isGenerator, isStrict = strict)

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

        case ArrowFunctionExpression(params, body, _, strict, _) =>
          // Arrow functions are always anonymous
          val funcBytecode = compileArrowFunctionBody(params, body, isStrict = strict)
    
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
              // Check if this is a const variable (compile-time check for local const)
              if currentScope.isLocal(name) && currentScope.isConst(name) then
                // Compile-time error for const reassignment
                throw new Exception(s"Cannot assign to const variable '$name'")
    
              // Check if variable is in current immediate scope (not parent scopes)
              if currentScope.isLocal(name) then
                // Variable is local to this function/script - use PutLoc
                val index = currentScope.lookup(name).get  // Safe because isLocal returned true
                // Duplicate the value so we can keep one on stack and store one
                instructions += Instruction.dup()
                instructions += Instruction.putLoc(index)
              else
                // Variable is in parent scope (closure) or global - use PutGlobal
                // PutGlobal will check the closure at runtime
                instructions += Instruction.dup()
                instructions += Instruction.putGlobal(name)
            case pattern: BindingPattern =>
              // Destructuring assignment: keep a copy of RHS as the expression result
              instructions += Instruction.dup()
              emitDestructuring(pattern, isDeclaration = false, isGlobalVar = false, instructions, constants)
            case MemberExpression(obj, prop, computed, _, _) =>
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
          def emitPropertyKey(key: Identifier | String | Expression): Unit = key match
            case Identifier(name, _) =>
              val constIndex = constants.length
              constants += JSValue.fromString(name)
              instructions += Instruction.getConst(constIndex)
            case s: String =>
              val constIndex = constants.length
              constants += JSValue.fromString(s)
              instructions += Instruction.getConst(constIndex)
            case expr: Expression =>
              compileExpression(expr, instructions, constants)
    
          instructions += Instruction.newObject()
          val objIndex = allocateTempLocal("__objLit")
          instructions += Instruction.putLoc(objIndex)
    
          for propOrSpread <- properties do
            propOrSpread match
              case SpreadElement(argument, _) =>
                // Spread: copy all properties from argument to target
                instructions += Instruction.getGlobal("__objectSpread")
                instructions += Instruction.getLoc(objIndex)
                compileExpression(argument, instructions, constants)
                instructions += Instruction.call(2)
                instructions += Instruction.drop()

              case prop: Property =>
                prop.kind match
                  case PropertyKind.Getter | PropertyKind.Setter =>
                    val descIndex = allocateTempLocal("__objDesc")
                    instructions += Instruction.newObject()
                    instructions += Instruction.putLoc(descIndex)
                    instructions += Instruction.getLoc(descIndex)
                    compileExpression(prop.value, instructions, constants)
                    if prop.kind == PropertyKind.Getter then
                      instructions += Instruction.setProp("get")
                    else
                      instructions += Instruction.setProp("set")
                    instructions += Instruction.drop()

                    instructions += Instruction.getLoc(descIndex)
                    instructions += Instruction.pushTrue()
                    instructions += Instruction.setProp("enumerable")
                    instructions += Instruction.drop()

                    instructions += Instruction.getLoc(descIndex)
                    instructions += Instruction.pushTrue()
                    instructions += Instruction.setProp("configurable")
                    instructions += Instruction.drop()

                    instructions += Instruction.getGlobal("Object")
                    instructions += Instruction.getProp("defineProperty")
                    instructions += Instruction.getLoc(objIndex)
                    emitPropertyKey(prop.key)
                    instructions += Instruction.getLoc(descIndex)
                    instructions += Instruction.call(3)
                    instructions += Instruction.drop()

                  case _ =>
                    prop.key match
                      case Identifier(name, _) =>
                        instructions += Instruction.getLoc(objIndex)
                        compileExpression(prop.value, instructions, constants)
                        instructions += Instruction.setProp(name)
                        instructions += Instruction.drop()
                      case s: String =>
                        instructions += Instruction.getLoc(objIndex)
                        compileExpression(prop.value, instructions, constants)
                        instructions += Instruction.setProp(s)
                        instructions += Instruction.drop()
                      case expr: Expression =>
                        instructions += Instruction.getLoc(objIndex)
                        compileExpression(expr, instructions, constants)
                        compileExpression(prop.value, instructions, constants)
                        instructions += Instruction.setElem()
                        instructions += Instruction.drop()
    
          instructions += Instruction.getLoc(objIndex)
    
        case ArrayLiteral(elements, _) =>
          val hasSpread = elements.exists {
            case SpreadElement(_, _) => true
            case _ => false
          }
    
          if !hasSpread then
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
          else
            // Build array dynamically to support spread elements
            instructions += Instruction.newArray(0)
            for elem <- elements do
              elem match
                case null =>
                  // Elision becomes an explicit undefined entry for spread-mode arrays
                  instructions += Instruction.getGlobal("__arrayPush")
                  instructions += Instruction.swap()
                  instructions += Instruction.pushUndefined()
                  instructions += Instruction.call(2)
                case SpreadElement(argument, _) =>
                  instructions += Instruction.getGlobal("__arraySpread")
                  instructions += Instruction.swap()
                  compileExpression(argument, instructions, constants)
                  instructions += Instruction.call(2)
                case expr: Expression =>
                  instructions += Instruction.getGlobal("__arrayPush")
                  instructions += Instruction.swap()
                  compileExpression(expr, instructions, constants)
                  instructions += Instruction.call(2)
    
        case MemberExpression(obj, prop, computed, _, optional) =>
          // Compile the object
          compileExpression(obj, instructions, constants)

          if optional then
            // Optional chaining: obj?.prop or obj?.[expr]
            // If obj is null or undefined, return undefined without accessing property
            // Stack: [obj]
            instructions += Instruction.dup()  // [obj, obj]
            instructions += Instruction.pushNull()  // [obj, obj, null]
            instructions += Instruction.binary(BinaryOpcode.StrictEq)  // [obj, obj === null]
            val jumpIfNullIdx = instructions.length
            val jumpIfNullPos = currentBytecodePos(instructions)
            instructions += Instruction.ifTrue(0)  // placeholder

            instructions += Instruction.dup()  // [obj, obj]
            instructions += Instruction.pushUndefined()  // [obj, obj, undefined]
            instructions += Instruction.binary(BinaryOpcode.StrictEq)  // [obj, obj === undefined]
            val jumpIfUndefIdx = instructions.length
            val jumpIfUndefPos = currentBytecodePos(instructions)
            instructions += Instruction.ifTrue(0)  // placeholder

            // Not null/undefined - do normal property access
            if computed then
              compileExpression(prop, instructions, constants)
              instructions += Instruction.getElem()
            else
              val propName = prop match
                case Identifier(name, _) => name
                case _ => throw new UnsupportedOperationException(s"Unsupported property key: $prop")
              instructions += Instruction.getProp(propName)

            // Jump over the undefined result
            val jumpToEndIdx = instructions.length
            val jumpToEndPos = currentBytecodePos(instructions)
            instructions += Instruction.goto(0)  // placeholder

            // Null/undefined path - replace obj with undefined
            val nullPathPos = currentBytecodePos(instructions)
            instructions += Instruction.drop()  // remove obj
            instructions += Instruction.pushUndefined()  // push undefined as result

            val endPos = currentBytecodePos(instructions)

            // Fix up jumps
            instructions(jumpIfNullIdx) = Instruction.ifTrue(nullPathPos - jumpIfNullPos - 1)
            instructions(jumpIfUndefIdx) = Instruction.ifTrue(nullPathPos - jumpIfUndefPos - 1)
            instructions(jumpToEndIdx) = Instruction.goto(endPos - jumpToEndPos - 1)
          else
            // Regular member expression
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
    case JSValue.BigInt(_) =>
      val constIndex = constants.length
      constants += value
      instructions += Instruction.getConst(constIndex)
    case _ =>
      throw new UnsupportedOperationException(s"Unsupported literal: $value")

  private def binaryOpToOpcode(op: quickjs.ast.BinaryOperator): BinaryOpcode = op match
    case BinaryOperator.Comma => BinaryOpcode.Comma
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
        // Top-level lexical variables live in locals (not global scope).
        if currentScope.isLexical(id.name) then
          val index = currentScope.lookup(id.name).get
          emitIncrementDecrement(op, index, instructions, useGetLoc = true, useLocCheck = true)
        else
          // Top-level var uses global scope.
          emitIncrementDecrement(op, id.name, instructions, useGetLoc = false, useLocCheck = false)

      case false if currentScope.isLocal(id.name) =>
        // Local variable in current function - use GetLoc/PutLoc
        val index = currentScope.lookup(id.name).get  // Safe because we just checked isLocal
        val useLocCheck = currentScope.isLexical(id.name)
        emitIncrementDecrement(op, index, instructions, useGetLoc = true, useLocCheck = useLocCheck)

      case _ =>
        // Variable from closure or parent scope - use GetGlobal/PutGlobal (checks closure map)
        emitIncrementDecrement(op, id.name, instructions, useGetLoc = false, useLocCheck = false)

  /** Emit increment/decrement bytecode for a specific variable access method */
  private def emitIncrementDecrement(
    op: quickjs.ast.UnaryOperator,
    varRef: String | Int,
    instructions: mutable.ArrayBuffer[Instruction],
    useGetLoc: Boolean,
    useLocCheck: Boolean
  ): Unit =
    // Helper functions to emit get/put instructions
    def emitGet(): Unit =
      if useGetLoc then
        if useLocCheck then
          instructions += Instruction.getLocCheck(varRef.asInstanceOf[Int])
        else
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

  private def compileMemberIncDec(
    op: quickjs.ast.UnaryOperator,
    memberExpr: MemberExpression,
    instructions: mutable.ArrayBuffer[Instruction],
    constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    val objIndex = allocateTempLocal("__incObj")
    compileExpression(memberExpr.`object`, instructions, constants)
    instructions += Instruction.putLoc(objIndex)

    val isInc = op == UnaryOperator.PreInc || op == UnaryOperator.PostInc
    val newOp = if isInc then UnaryOpcode.PreInc else UnaryOpcode.PreDec
    val oldValIndex = allocateTempLocal("__incOld")
    val newValIndex = allocateTempLocal("__incNew")

    memberExpr match
      case MemberExpression(_, prop, true, _, _) =>
        val propIndex = allocateTempLocal("__incProp")
        compileExpression(prop, instructions, constants)
        instructions += Instruction.putLoc(propIndex)

        instructions += Instruction.getLoc(objIndex)
        instructions += Instruction.getLoc(propIndex)
        instructions += Instruction.getElem()
        instructions += Instruction.pushI32(0)
        instructions += Instruction.binary(BinaryOpcode.Add)
        instructions += Instruction.putLoc(oldValIndex)

        instructions += Instruction.getLoc(oldValIndex)
        instructions += Instruction.unary(newOp)
        instructions += Instruction.putLoc(newValIndex)

        instructions += Instruction.getLoc(objIndex)
        instructions += Instruction.getLoc(propIndex)
        instructions += Instruction.getLoc(newValIndex)
        instructions += Instruction.setElem()

        if op == UnaryOperator.PostInc || op == UnaryOperator.PostDec then
          instructions += Instruction.drop()
          instructions += Instruction.getLoc(oldValIndex)
      case MemberExpression(_, prop, false, _, _) =>
        val propName = prop match
          case Identifier(name, _) => name
          case _ => throw new UnsupportedOperationException(s"Unsupported property key: $prop")

        instructions += Instruction.getLoc(objIndex)
        instructions += Instruction.getProp(propName)
        instructions += Instruction.pushI32(0)
        instructions += Instruction.binary(BinaryOpcode.Add)
        instructions += Instruction.putLoc(oldValIndex)

        instructions += Instruction.getLoc(oldValIndex)
        instructions += Instruction.unary(newOp)
        instructions += Instruction.putLoc(newValIndex)

        instructions += Instruction.getLoc(objIndex)
        instructions += Instruction.getLoc(newValIndex)
        instructions += Instruction.setProp(propName)
        instructions += Instruction.drop()

        if op == UnaryOperator.PreInc || op == UnaryOperator.PreDec then
          instructions += Instruction.getLoc(newValIndex)
        else
          instructions += Instruction.getLoc(oldValIndex)

object Compiler:
  def apply(): Compiler = new Compiler()
