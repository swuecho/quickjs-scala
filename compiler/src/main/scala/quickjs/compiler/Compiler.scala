package quickjs.compiler

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.value.JSValue

import scala.collection.mutable

/** Minimal compiler for Phase 2.
  *
  * Compiles AST to bytecode for:
  *   - Literals (numbers, booleans, undefined, null)
  *   - Binary operations (arithmetic)
  *   - Variable declarations (var, let, const)
  *   - Block statements
  *   - If/else statements
  *   - While loops
  *   - Function declarations
  *   - Function calls
  *   - Return statements
  */
class Compiler {
  import Compiler.*

  // REPL mode flag
  private var replMode: Boolean = false
  private var directEvalMode: Boolean = false
  private var indirectEvalMode: Boolean = false
  private var withScopeDepth: Int = 0
  private var currentModuleName: String = "<script>"
  private var currentIsStrict: Boolean = false
  private var currentFunctionIsAsync: Boolean = false
  // Derived-class instance field/private-member initializers wait for the
  // first `super()` call; the super() compilation consumes this list.
  private var pendingDerivedFieldInits: List[Statement] = Nil
  private var tempVarCounter: Int = 0
  private var currentSuperClass: Expression | Null = null
  private var currentSuperIsStatic: Boolean = false
  private var currentSuperCapture: Set[String] = Set.empty
  private var currentSuperVarName: Option[String] =
    None // Name of variable holding superclass
  private var currentClassName: String | Null = null
  private var currentClassCapture: Boolean = true
  private var currentStaticFieldThis: Option[Int] = None
  private var classFieldEvalContextDepth: Int = 0
  private var currentClassPrivateNames: Set[String] = Set.empty
  private var currentClassPrivateBindings: Map[String, String] = Map.empty
  private val templateObjectPrefix: String =
    s"tmpl:${System.identityHashCode(this)}"
  private var templateObjectCounter: Int = 0
  private val unknownSpan: Span = Span(0, 0, 0, 0)
  private var currentSpan: Span = unknownSpan

  private final class InstructionBuffer
      extends mutable.ArrayBuffer[Instruction] {
    val spans: mutable.ArrayBuffer[Span] = mutable.ArrayBuffer.empty

    override def addOne(elem: Instruction): this.type = {
      spans += currentSpan
      super.addOne(elem)
      this
    }
  }

  private def withSpan[T](span: Span)(body: => T): T = {
    val prev = currentSpan
    currentSpan = span
    try body
    finally currentSpan = prev
  }

  private def buildSpanMap(
      instructions: InstructionBuffer
  ): Array[(Int, Int, Int)] = {
    val map = mutable.ArrayBuffer[(Int, Int, Int)]()
    var offset = 0
    var lastLine = -1
    var lastCol = -1
    var i = 0
    while i < instructions.length do {
      val span = instructions.spans(i)
      if span != unknownSpan then {
        val line = span.line + 1
        val col = span.column + 1
        if line != lastLine || col != lastCol then {
          map += ((offset, line, col))
          lastLine = line
          lastCol = col
        }
      }
      offset += instructions(i).size
      i += 1
    }
    map.toArray
  }

  private def withSuperContext[T](
      superClass: Expression | Null,
      isStatic: Boolean,
      superVarName: Option[String] = None
  )(f: => T): T = {
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
    finally {
      currentSuperClass = prevSuper
      currentSuperIsStatic = prevStatic
      currentSuperCapture = prevCapture
      currentSuperVarName = prevVarName
    }
  }

  private def withClassContext[T](
      className: String | Null,
      captureInClosure: Boolean
  )(f: => T): T = {
    val prevName = currentClassName
    val prevCapture = currentClassCapture
    currentClassName = className
    currentClassCapture = captureInClosure
    try f
    finally {
      currentClassName = prevName
      currentClassCapture = prevCapture
    }
  }

  private def withStaticFieldThis[T](ctorIndex: Int)(f: => T): T = {
    val prev = currentStaticFieldThis
    currentStaticFieldThis = Some(ctorIndex)
    try f
    finally currentStaticFieldThis = prev
  }

  private def withoutStaticFieldThis[T](f: => T): T = {
    val previous = currentStaticFieldThis
    currentStaticFieldThis = None
    try f
    finally currentStaticFieldThis = previous
  }

  private def withClassFieldEvalContext[T](f: => T): T = {
    classFieldEvalContextDepth += 1
    try f
    finally classFieldEvalContextDepth -= 1
  }

  private def withoutClassFieldEvalContext[T](f: => T): T = {
    val previous = classFieldEvalContextDepth
    classFieldEvalContextDepth = 0
    try f
    finally classFieldEvalContextDepth = previous
  }

  private def withClassPrivateNames[T](
      names: Set[String],
      bindings: Map[String, String] = Map.empty
  )(f: => T): T = {
    val previousNames = currentClassPrivateNames
    val previousBindings = currentClassPrivateBindings
    currentClassPrivateNames = previousNames ++ names
    currentClassPrivateBindings = previousBindings ++ bindings
    try f
    finally {
      currentClassPrivateNames = previousNames
      currentClassPrivateBindings = previousBindings
    }
  }

  private def privateOpcodeName(name: String): String =
    currentClassPrivateBindings.get(name) match {
      case Some(binding) => s"$name\u001f$binding"
      case None          => name
    }

  /** Compile in REPL mode (don't drop last expression) */
  def withREPLMode(compilation: => BytecodeFunction): BytecodeFunction = {
    val oldMode = replMode
    replMode = true
    try compilation
    finally replMode = oldMode
  }

  /** Compile direct eval code with eval-local top-level var declarations. */
  def withDirectEvalMode(compilation: => BytecodeFunction): BytecodeFunction = {
    val oldMode = directEvalMode
    directEvalMode = true
    try compilation
    finally directEvalMode = oldMode
  }

  def withEvalPrivateBindings(
      bindings: Map[String, String]
  )(compilation: => BytecodeFunction): BytecodeFunction =
    withClassPrivateNames(bindings.keySet, bindings)(compilation)

  /** Compile indirect eval code: top-level var creates configurable globals. */
  def withIndirectEvalMode(compilation: => BytecodeFunction): BytecodeFunction = {
    val oldMode = indirectEvalMode
    indirectEvalMode = true
    try compilation
    finally indirectEvalMode = oldMode
  }

  /** Compile direct eval source with the lexical `super` binding inherited
    * from a class field initializer.
    */
  def withEvalSuperContext(
      superVarName: String,
      isStatic: Boolean
  )(compilation: => BytecodeFunction): BytecodeFunction =
    withSuperContext(
      Identifier(superVarName, unknownSpan),
      isStatic,
      Some(superVarName)
    )(compilation)

  // Scope for variable tracking - following QuickJS C pattern
  // Key change: Variables now track their scope level for proper let/const scoping
  // Uses a list-based structure to support shadowing (multiple variables with same name at different scope levels)
  private class Scope(val parent: Scope | Null) {
    // Per-binding records: (slot, isLexical, isConst, blockId). blockId 0 is
    // the function/script scope; every entered block gets a fresh id so that
    // sibling blocks (which share a nesting depth) never collide. A slot is
    // only visible while its block is on the active chain.
    private val vars = mutable
      .HashMap[String, mutable.ListBuffer[(Int, Boolean, Boolean, Int)]]()
    private var nextIndex = 0
    private var blockScopeLevel: Int = 0
    private var nextBlockId: Int = 1
    private val activeBlocks = mutable.ArrayBuffer[Int](0)

    private def isVisible(blockId: Int): Boolean =
      blockId == 0 || activeBlocks.contains(blockId)

    private def innermost(
        name: String
    ): Option[(Int, Boolean, Boolean, Int)] =
      vars.get(name).flatMap { declarations =>
        declarations.filter(record => isVisible(record._4)).maxByOption(_._4)
      }

    def declare(
        name: String,
        isLexical: Boolean = false,
        isConst: Boolean = false
    ): Int = {
      val declarations = vars.getOrElseUpdate(name, mutable.ListBuffer.empty)
      if isLexical then {
        val currentBlock = activeBlocks.last
        declarations.find(record => record._4 == currentBlock) match {
          case Some(existing) => existing._1
          case None =>
            // The function-body pre-pass declares every collected name at
            // block 0 as non-lexical. Reclassify that slot for the real
            // let/const declaration instead of creating a duplicate: a
            // duplicate would make `localVarNames.indexOf(name)` point at the
            // pre-pass slot and closures would capture the wrong (empty) slot.
            val prePassIndex =
              declarations.indexWhere(record => record._4 == 0 && !record._2)
            if prePassIndex >= 0 then {
              val existing = declarations(prePassIndex)
              declarations(prePassIndex) =
                (existing._1, true, isConst, currentBlock)
              existing._1
            } else {
              val idx = nextIndex
              declarations.prepend((idx, true, isConst, currentBlock))
              nextIndex += 1
              idx
            }
        }
      } else {
        // var and compiler temporaries are function-scoped: reuse the visible
        // non-lexical binding, otherwise create one at the function scope.
        declarations.find(record => isVisible(record._4) && !record._2) match {
          case Some(existing) => existing._1
          case None =>
            val idx = nextIndex
            declarations.prepend((idx, false, false, 0))
            nextIndex += 1
            idx
        }
      }
    }

    def contains(name: String): Boolean = vars.contains(name)

    /** Slot of the innermost visible declaration in this scope only (no parent
      * fallback). Used for slot-aware closure capture.
      */
    def ownSlot(name: String): Option[Int] = innermost(name).map(_._1)

    def lookup(name: String): Option[Int] =
      innermost(name).map(_._1).orElse {
        if parent != null then parent.lookup(name) else None
      }

    /** Check if a variable is declared in this scope (not in parent scopes) */
    def hasVariable(name: String): Boolean = vars.contains(name)

    /** Check if a variable is const (for reassignment checks) */
    def isConst(name: String): Boolean = innermost(name).exists(_._3)

    /** Check if a variable is lexical (let/const) for TDZ checks */
    def isLexical(name: String): Boolean = innermost(name).exists(_._2)

    /** Check if variable is in this scope only (not parent scopes) */
    def isLocal(name: String): Boolean = innermost(name).isDefined

    /** Get the block id of a visible variable if it's in this scope */
    def getVariableScopeLevel(name: String): Option[Int] =
      innermost(name).map(_._4)

    /** Enter a new block scope (for let/const) */
    def enterBlockScope(): Int = {
      val id = nextBlockId
      nextBlockId += 1
      activeBlocks += id
      blockScopeLevel += 1
      blockScopeLevel
    }

    /** Leave the current block scope */
    def leaveBlockScope(): Int = {
      if activeBlocks.length > 1 then
        activeBlocks.remove(activeBlocks.length - 1)
      if blockScopeLevel > 0 then blockScopeLevel -= 1
      blockScopeLevel
    }

    /** Get current block scope level */
    def getBlockScopeLevel: Int = blockScopeLevel

    /** Get all local variable names in declaration order (by index) */
    def getAllLocalVarNames: Array[String] = {
      // Slot-indexed names: every declaration slot keeps its position so that
      // `localVarNames.indexOf(name)` equals the compiler slot. Shadowed
      // declarations get unique placeholder names.
      if vars.isEmpty then Array.empty
      else {
        val maxIndex = vars.valuesIterator.flatMap(_.map(_._1)).max
        val arr = new Array[String](maxIndex + 1)
        vars.foreach { case (name, declarations) =>
          // The runtime name lookup resolves by name; name the most recent
          // declaration so closures created in the latest sibling block
          // capture the slot they actually reference.
          val chosen = declarations.maxBy(_._4)
          arr(chosen._1) = name
        }
        var j = 0
        while j < arr.length do {
          if arr(j) == null then arr(j) = s"\u0000slot_$j"
          j += 1
        }
        arr
      }
    }
  }

  // Current compilation scope
  private var currentScope: Scope = new Scope(null)

  /** Check if a variable exists in any parent scope (for closure capture
    * decisions)
    */
  private def isVariableInParentScope(name: String): Boolean = {
    var scope = currentScope.parent
    while scope != null do {
      val found = scope.lookup(name)
      if found.isDefined then return true
      scope = scope.parent
    }
    false
  }

  // Loop/switch/labeled statement exit point stack for break/continue
  // Each entry contains (isLoop, labelName, exitBytePos, continueBytePos, pendingBreaks, pendingContinues, isRegularStmt)
  // isLoop: true for loops (for/while/do-while), false for switches/regular statements
  // labelName: Some(label) for labeled statements, None for unlabeled
  // pendingBreaks and pendingContinues are ListBuffers of (instructionIndex, bytePosition) tuples
  // Switches and regular statements only support break, not continue
  private var loopStack: mutable.Stack[
    (
        Boolean,
        Option[String],
        Int,
        Int,
        mutable.ListBuffer[(Int, Int)],
        mutable.ListBuffer[(Int, Int)],
        Boolean
    )
  ] = mutable.Stack.empty

  // Stack of active finally blocks for control-flow unwinding (innermost first)
  private var finallyStack: List[Statement] = Nil
  // Iterator locals for active for-of loops, innermost first. Abrupt control
  // flow must perform IteratorClose before leaving those loops.
  private var iteratorCloseStack: List[Int] = Nil
  // >0 while compiling the synthesized default constructor of a derived class
  private var defaultDerivedCtorDepth: Int = 0

  private def emitIteratorCloses(
      iterators: Iterable[Int],
      instructions: mutable.ArrayBuffer[Instruction]
  ): Unit = iterators.foreach { index =>
    instructions += Instruction.getGlobal("__iteratorClose")
    instructions += Instruction.getLoc(index)
    instructions += Instruction.call(1)
    instructions += Instruction.drop()
  }

  /** Enter a loop and push its info onto the stack */
  private def enterLoop(labelName: Option[String] = None): Unit =
    loopStack.push(
      (
        true,
        labelName,
        -1,
        -1,
        mutable.ListBuffer.empty,
        mutable.ListBuffer.empty,
        false
      )
    )

  /** Enter a switch and push its info onto the stack (switches support break
    * but not continue)
    */
  private def enterSwitch(labelName: Option[String] = None): Unit =
    loopStack.push(
      (
        false,
        labelName,
        -1,
        -1,
        mutable.ListBuffer.empty,
        mutable.ListBuffer.empty,
        false
      )
    )

  /** Enter a labeled regular statement and push its info onto the stack */
  private def enterLabeledStatement(labelName: String): Unit =
    loopStack.push(
      (
        false,
        Some(labelName),
        -1,
        -1,
        mutable.ListBuffer.empty,
        mutable.ListBuffer.empty,
        true
      )
    )

  /** Exit a loop/switch/labeled statement and pop its info from the stack */
  private def exitLoop(): Unit = {
    if loopStack.nonEmpty then loopStack.pop()
    ()
  }

  /** Set the loop/switch/labeled statement exit point (called after compiling
    * the statement)
    */
  private def setLoopExit(
      exitBytePos: Int,
      instructions: mutable.ArrayBuffer[Instruction]
  ): Unit =
    if loopStack.nonEmpty then {
      val (
        isLoop,
        labelName,
        oldExit,
        oldCont,
        pendingBreaks,
        pendingContinues,
        isRegular
      ) = loopStack.pop()
      loopStack.push(
        (
          isLoop,
          labelName,
          exitBytePos,
          oldCont,
          pendingBreaks,
          pendingContinues,
          isRegular
        )
      )
      // Fix up all pending break statements
      for (breakInstIdx, breakBytePos) <- pendingBreaks do {
        val offset = exitBytePos - breakBytePos - 1
        instructions(breakInstIdx) = Instruction.goto(offset)
      }
      ()
    }

  /** Set the loop continue point (called at the position where continue should
    * jump)
    */
  private def setLoopContinue(
      continueBytePos: Int,
      instructions: mutable.ArrayBuffer[Instruction]
  ): Unit =
    if loopStack.nonEmpty then {
      val (
        isLoop,
        labelName,
        oldExit,
        oldCont,
        pendingBreaks,
        pendingContinues,
        isRegular
      ) = loopStack.pop()
      loopStack.push(
        (
          isLoop,
          labelName,
          oldExit,
          continueBytePos,
          pendingBreaks,
          pendingContinues,
          isRegular
        )
      )
      // Fix up all pending continue statements
      for (contInstIdx, contBytePos) <- pendingContinues do {
        val offset = continueBytePos - contBytePos - 1
        instructions(contInstIdx) = Instruction.goto(offset)
      }
      ()
    }

  /** Get the current loop/switch/labeled statement exit position (if known) */
  private def getCurrentLoopExit(): Option[Int] =
    if loopStack.nonEmpty then {
      val (_, _, exit, _, _, _, _) = loopStack.top
      Some(exit)
    } else None

  /** Get the current loop continue position (if known) - skips switches and
    * regular statements
    */
  private def getCurrentLoopContinue(): Option[Int] =
    // Find the innermost actual loop (skip switches and regular labeled statements)
    loopStack.find(_._1).map(_._4)

  /** Find a loop/switch/labeled statement by label name */
  private def findLabeledStatement(
      label: String
  ): Option[(Boolean, Option[String], Int, Int, Boolean)] =
    loopStack
      .find { case (_, labelName, _, _, _, _, isRegular) =>
        labelName.exists(_ == label) && !isRegular
      }
      .map { case (isLoop, labelName, exit, cont, _, _, isRegular) =>
        (isLoop, labelName, exit, cont, isRegular)
      }

  /** Add a pending break statement (to be fixed up when exit is known) */
  private def addPendingBreak(instIdx: Int, bytePos: Int): Unit =
    if loopStack.nonEmpty then {
      val (
        isLoop,
        labelName,
        exit,
        cont,
        pendingBreaks,
        pendingContinues,
        isRegular
      ) = loopStack.pop()
      pendingBreaks += ((instIdx, bytePos))
      loopStack.push(
        (
          isLoop,
          labelName,
          exit,
          cont,
          pendingBreaks,
          pendingContinues,
          isRegular
        )
      )
    }

  /** Add a pending continue statement (to be fixed up when continue point is
    * known)
    */
  private def addPendingContinue(instIdx: Int, bytePos: Int): Unit = {
    // Find the innermost actual loop (skip switches and regular labeled statements) and add to its pending continues
    val loopIdx = loopStack.indexWhere(_._1)
    if loopIdx >= 0 then {
      val (
        isLoop,
        labelName,
        exit,
        cont,
        pendingBreaks,
        pendingContinues,
        isRegular
      ) = loopStack(loopIdx)
      // Update that specific entry
      pendingContinues += ((instIdx, bytePos))
      loopStack(loopIdx) = (
        isLoop,
        labelName,
        exit,
        cont,
        pendingBreaks,
        pendingContinues,
        isRegular
      )
    }
  }

  private def allocateTempLocal(prefix: String): Int = {
    val name = s"${prefix}_${tempVarCounter}"
    tempVarCounter += 1
    currentScope.declare(name)
  }

  private def emitForInAssignment(
      target: VariableDeclaration | Expression,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    target match {
      case decl: VariableDeclaration =>
        decl.declarations.head.id match {
          case Identifier(name, _) =>
            if currentScope.isLocal(name) then {
              val index = currentScope.lookup(name).get
              instructions += Instruction.putLoc(index)
            } else instructions += Instruction.putGlobal(name)
          case pattern: BindingPattern =>
            val isGlobalVar =
              currentScope.parent == null &&
                decl.kind == VariableKind.Var &&
                currentModuleName == "<script>" &&
                !directEvalMode
            emitDestructuring(
              pattern,
              isDeclaration = true,
              isGlobalVar = isGlobalVar,
              instructions,
              constants
            )
        }

      case expr: Expression =>
        expr match {
          case ArrayLiteral(elements, _, _) =>
            instructions += Instruction.getGlobal("__destructureArray")
            instructions += Instruction.swap()
            val restIndex = elements.indexWhere(_.isInstanceOf[SpreadElement])
            instructions += Instruction.pushI32(elements.length)
            if restIndex >= 0 then instructions += Instruction.pushTrue()
            else instructions += Instruction.pushFalse()
            instructions += Instruction.call(3)
            elements.zipWithIndex.foreach { case (element, index) =>
              element match {
                case null => ()
                case SpreadElement(argument, _) =>
                  instructions += Instruction.dup()
                  instructions += Instruction.getProp("slice")
                  instructions += Instruction.pushI32(index)
                  instructions += Instruction.callMethod(1)
                  emitForInAssignment(argument, instructions, constants)
                case AssignmentExpression(left, defaultValue, _) =>
                  instructions += Instruction.dup()
                  instructions += Instruction.pushI32(index)
                  instructions += Instruction.getElem()
                  emitDefaultIfUndefined(
                    defaultValue,
                    instructions,
                    constants,
                    left match {
                      case Identifier(name, _) => Some(name)
                      case _                   => None
                    }
                  )
                  left match {
                    case target: Expression =>
                      emitForInAssignment(target, instructions, constants)
                    case pattern: BindingPattern =>
                      emitDestructuring(pattern, false, false, instructions, constants)
                  }
                case target: Expression =>
                  instructions += Instruction.dup()
                  instructions += Instruction.pushI32(index)
                  instructions += Instruction.getElem()
                  emitForInAssignment(target, instructions, constants)
              }
            }
            if restIndex < 0 then instructions += Instruction.drop()
          case ObjectLiteral(properties, _) =>
            instructions += Instruction.getGlobal("__requireObjectCoercible")
            instructions += Instruction.swap()
            instructions += Instruction.call(1)
            val extractedKeys = mutable.ArrayBuffer.empty[Either[String, Int]]
            var hasRest = false
            properties.foreach {
              case Property(key, target, _, computed, _, _) =>
                instructions += Instruction.dup()
                key match {
                  case Identifier(name, _) if !computed =>
                    instructions += Instruction.getProp(name)
                    extractedKeys += Left(name)
                  case name: String if !computed =>
                    instructions += Instruction.getProp(name)
                    extractedKeys += Left(name)
                  case keyExpression: Expression =>
                    compileExpression(keyExpression, instructions, constants)
                    val keyIndex = allocateTempLocal("__computedAssignmentKey")
                    instructions += Instruction.dup()
                    instructions += Instruction.putLoc(keyIndex)
                    instructions += Instruction.getElem()
                    extractedKeys += Right(keyIndex)
                  case _ =>
                    throw new UnsupportedOperationException(
                      s"Unsupported destructuring property key: $key"
                    )
                }
                target match {
                  case AssignmentExpression(left, defaultValue, _) =>
                    emitDefaultIfUndefined(
                      defaultValue,
                      instructions,
                      constants,
                      left match {
                        case Identifier(name, _) => Some(name)
                        case _                   => None
                      }
                    )
                    left match {
                      case expression: Expression =>
                        emitForInAssignment(expression, instructions, constants)
                      case pattern: BindingPattern =>
                        emitDestructuring(pattern, false, false, instructions, constants)
                    }
                  case expression: Expression =>
                    emitForInAssignment(expression, instructions, constants)
                }
              case SpreadElement(argument, _) =>
                hasRest = true
                instructions += Instruction.getGlobal("__objectRest")
                instructions += Instruction.swap()
                instructions += Instruction.newArray(extractedKeys.length)
                for (key, index) <- extractedKeys.zipWithIndex do {
                  instructions += Instruction.pushI32(index)
                  key match {
                    case Left(name) =>
                      val constIndex = constants.length
                      constants += JSValue.fromString(name)
                      instructions += Instruction.getConst(constIndex)
                    case Right(localIndex) =>
                      instructions += Instruction.getLoc(localIndex)
                  }
                  instructions += Instruction.initElem()
                }
                instructions += Instruction.call(2)
                emitForInAssignment(argument, instructions, constants)
            }
            if !hasRest then instructions += Instruction.drop()
          case Identifier(name, _) =>
            if currentScope.isLocal(name) then {
              val index = currentScope.lookup(name).get
              instructions += Instruction.putLoc(index)
            } else instructions += Instruction.putGlobal(name)
          case MemberExpression(obj, prop, computed, _, _) =>
            if computed then {
              compileExpression(obj, instructions, constants)
              instructions += Instruction.swap()
              compileExpression(prop, instructions, constants)
              instructions += Instruction.swap()
              instructions += Instruction.setElem()
              instructions += Instruction.drop()
            } else {
              compileExpression(obj, instructions, constants)
              instructions += Instruction.swap()
              val propName = prop match {
                case Identifier(name, _) => name
                case _                   =>
                  throw new UnsupportedOperationException(
                    s"Unsupported property key: $prop"
                  )
              }
              instructions += Instruction.setProp(propName)
              instructions += Instruction.drop()
            }
          case _ =>
            throw new UnsupportedOperationException(
              s"Unsupported assignment target: $expr"
            )
        }
    }

  /** Local slots whose `let`/`const` bindings are copied to a fresh VarRef on
    * every loop iteration. A copy is only needed when user code in the body
    * can observe the binding (a closure over the name, or direct eval), so
    * plain arithmetic loops avoid the allocation.
    */
  private def perIterationBindingSlots(
      declaration: VariableDeclaration | Null,
      body: Statement
  ): Seq[Int] =
    if declaration == null || declaration.kind == VariableKind.Var then
      Seq.empty
    else {
      val names = declaration.declarations
        .flatMap(d => collectBindingNames(d.id))
        .distinct
      if names.isEmpty then Seq.empty
      else {
        val referenced = findFreeVariablesForClosure(body)
        if !names.exists(referenced.contains) && !containsDirectEval(body) then
          Seq.empty
        else
          names.flatMap(name =>
            if currentScope.isLocal(name) then currentScope.lookup(name)
            else None
          )
      }
    }

  private def emitForBindingDeclarations(
      declaration: VariableDeclaration,
      instructions: mutable.ArrayBuffer[Instruction]
  ): Unit = {
    val isLexical = declaration.kind != VariableKind.Var
    val isConst = declaration.kind == VariableKind.Const
    val isTopLevel = currentScope.parent == null
    val isModule = currentModuleName != "<script>"
    val isGlobalVar = isTopLevel && !isLexical && !isModule && !directEvalMode

    for declarator <- declaration.declarations;
        name <- collectBindingNames(declarator.id)
    do
      if isGlobalVar then {
        instructions += Instruction.pushUndefined()
        instructions += Instruction.defVar(name)
      } else {
        val index = currentScope.declare(name, isLexical, isConst)
        if isConst then {
          instructions += Instruction.setLocUninitialized(index)
          instructions += Instruction.setLocConst(index)
        } else if isLexical then
          instructions += Instruction.setLocUninitialized(index)
        else {
          instructions += Instruction.pushUndefined()
          instructions += Instruction.putLoc(index)
        }
      }
  }

  // ===========================================================================
  // Free Variable Analysis Helpers
  // ===========================================================================

  /** Recursively find free variables in binary expression operands */
  private def findFreeVarsInBinary(
      left: Expression,
      right: Expression,
      recurse: Expression => Set[String]
  ): Set[String] =
    recurse(left) ++ recurse(right)

  /** Recursively find free variables in unary expression operand */
  private def findFreeVarsInUnary(
      argument: Expression,
      recurse: Expression => Set[String]
  ): Set[String] =
    recurse(argument)

  /** Recursively find free variables in call/expression */
  private def findFreeVarsInCall(
      callee: Expression,
      arguments: Seq[Expression],
      recurse: Expression => Set[String]
  ): Set[String] =
    recurse(callee) ++ arguments.flatMap(recurse).toSet

  /** Recursively find free variables in member expression */
  private def findFreeVarsInMember(
      obj: Expression,
      prop: Expression,
      computed: Boolean,
      recurse: Expression => Set[String]
  ): Set[String] =
    recurse(obj) ++ (if computed then recurse(prop) else Set.empty)

  /** Recursively find free variables in assignment expression */
  private def findFreeVarsInAssignment(
      left: Expression | BindingPattern,
      right: Expression,
      recurseExpr: Expression => Set[String],
      recursePattern: BindingPattern => Set[String]
  ): Set[String] = {
    val leftFree = left match {
      case Identifier(name, _) =>
        // For closure analysis: we need to check if the variable is declared in the
        // CURRENT scope (local) vs an outer/runtime binding. Unresolved names are
        // captured as lazy GlobalRef values at runtime, which also lets nested
        // arrows see named-function-expression self bindings created in the
        // parent closure.
        val isInCurrentScope = currentScope.hasVariable(name)
        if !isInCurrentScope then Set(name) else Set.empty
      case e: Expression     => recurseExpr(e)
      case p: BindingPattern => recursePattern(p)
    }
    leftFree ++ recurseExpr(right)
  }

  /** Recursively find free variables in object literal properties */
  private def findFreeVarsInObjectLiteral(
      properties: Seq[Property | SpreadElement],
      recurse: Expression => Set[String]
  ): Set[String] =
    properties.flatMap {
      case SpreadElement(argument, _) => recurse(argument)
      case p: Property                =>
        val valueFree = recurse(p.value)
        val keyFree = p.key match {
          case expr: Expression => recurse(expr)
          case _                => Set.empty[String]
        }
        valueFree ++ keyFree
    }.toSet

  /** Recursively find free variables in array literal elements */
  private def findFreeVarsInArrayLiteral(
      elements: Seq[Expression | Null],
      recurse: Expression => Set[String]
  ): Set[String] =
    elements.collect { case e: Expression => e }.flatMap(recurse).toSet

  /** Recursively find free variables in conditional expression */
  private def findFreeVarsInConditional(
      test: Expression,
      consequent: Expression,
      alternate: Expression,
      recurse: Expression => Set[String]
  ): Set[String] =
    recurse(test) ++ recurse(consequent) ++ recurse(alternate)

  /** Find free variables in class expression */
  /** Free variables of a class definition: its heritage expression, computed
    * keys, field initializers and method bodies (methods close over the outer
    * scope, e.g. `class Agent extends events_1.EventEmitter`).
    */
  private def findFreeVarsInClass(
      superClass: Expression | Null,
      body: ClassBody,
      recurse: Expression => Set[String],
      recurseStmt: Statement => Set[String]
  ): Set[String] = {
    val superFree = if superClass != null then recurse(superClass) else Set.empty
    val membersFree = body.elements.flatMap {
      case m: MethodDefinition =>
        val keyFree = m.key match {
          case e: Expression => recurse(e)
          case _             => Set.empty[String]
        }
        keyFree ++ findFreeVarsInFunctionForClosure(
          m.params,
          m.body,
          recurse,
          recurseStmt
        )
      case f: FieldDefinition =>
        val keyFree = f.key match {
          case e: Expression => recurse(e)
          case _             => Set.empty[String]
        }
        val valueFree = f.value match {
          case e: Expression => recurse(e)
          case _             => Set.empty[String]
        }
        keyFree ++ valueFree
    }
    superFree ++ membersFree.toSet
  }

  /** Find free variables in function/arrow function for closure analysis. This
    * looks inside the function body and excludes parameters/local variables.
    */
  private def findFreeVarsInFunctionForClosure(
      params: Seq[BindingPattern],
      body: Expression | BlockStatement,
      recurseExpr: Expression => Set[String],
      recurseStmt: Statement => Set[String]
  ): Set[String] = {
    val paramNames = params.flatMap(collectBindingNames).toSet
    val (bodyFree, localVars) = body match {
      case e: Expression =>
        (
          recurseExpr(e),
          Set.empty[String]
        ) // Expression bodies don't declare vars
      case b: BlockStatement =>
        (recurseStmt(b), findDeclaredVariables(b))
    }
    // Exclude parameters and local variables - only return true free vars
    bodyFree -- paramNames -- localVars
  }

  /** Find all free variables in an expression */
  private def findFreeVariablesForClosure(expr: Expression): Set[String] =
    expr match {
      case Identifier(name, _) =>
        if currentClassName != null && !currentClassCapture && name == currentClassName
        then Set.empty
        else Set(name)
      case Literal(_, _)      => Set.empty
      case ThisExpression(_)  => Set.empty // 'this' is not a free variable
      case SuperExpression(_) => currentSuperCapture
      case NewTargetExpression(_) => Set.empty
      case ClassFieldInitializerExpression(expression, _) =>
        findFreeVariablesForClosure(expression)
      case ImportMetaExpression(_) => Set.empty
      case ComputedPropertyName(expression, _) =>
        findFreeVariablesForClosure(expression)
      case ImportCallExpression(arguments, _) =>
        arguments.flatMap(findFreeVariablesForClosure).toSet
      case BinaryExpression(_, left, right, _) =>
        findFreeVarsInBinary(left, right, findFreeVariablesForClosure)
      case UnaryExpression(_, argument, _, _) =>
        findFreeVarsInUnary(argument, findFreeVariablesForClosure)
      case CallExpression(callee, arguments, _, _) =>
        findFreeVarsInCall(callee, arguments, findFreeVariablesForClosure)
      case TaggedTemplateExpression(tag, template, _) =>
        findFreeVariablesForClosure(tag) ++ template.expressions.flatMap(
          findFreeVariablesForClosure
        )
      case NewExpression(callee, arguments, _) =>
        findFreeVarsInCall(callee, arguments, findFreeVariablesForClosure)
      case MemberExpression(obj, prop, computed, _, _) =>
        val memberFree =
          findFreeVarsInMember(obj, prop, computed, findFreeVariablesForClosure)
        prop match {
          case PrivateIdentifier(name, _) =>
            memberFree ++ currentClassPrivateBindings.get(name)
          case _ => memberFree
        }
      case AssignmentExpression(left, right, _) =>
        findFreeVarsInAssignment(
          left,
          right,
          findFreeVariablesForClosure,
          findFreeVariablesInPattern
        )
      case LogicalAssignmentExpression(_, left, right, _) =>
        findFreeVariablesForClosure(left) ++ findFreeVariablesForClosure(right)
      case FunctionExpression(_, params, body, _, _, _, _) =>
        findFreeVarsInFunctionForClosure(
          params,
          body,
          findFreeVariablesForClosure,
          findFreeVariablesForClosure
        )
      case ArrowFunctionExpression(params, body, _, _, _) =>
        // Arrow functions have Either[Expression, BlockStatement] for body
        val paramNames = params.flatMap(collectBindingNames).toSet
        val (bodyFree, localVars) = body match {
          case Left(expr) =>
            (
              findFreeVariablesForClosure(expr),
              Set.empty[String]
            ) // Expression bodies don't declare vars
          case Right(block) =>
            (findFreeVariablesForClosure(block), findDeclaredVariables(block))
        }
        // Exclude parameters and local variables
        bodyFree -- paramNames -- localVars
      case ObjectLiteral(properties, _) =>
        findFreeVarsInObjectLiteral(properties, findFreeVariablesForClosure)
      case ClassExpression(_, superClass, body, _) =>
        findFreeVarsInClass(
          superClass,
          body,
          findFreeVariablesForClosure,
          findFreeVariablesForClosure
        )
      case ArrayLiteral(elements, _, _) =>
        findFreeVarsInArrayLiteral(elements, findFreeVariablesForClosure)
      case SpreadElement(argument, _) =>
        findFreeVariablesForClosure(argument)
      case ConditionalExpression(test, consequent, alternate, _) =>
        findFreeVarsInConditional(
          test,
          consequent,
          alternate,
          findFreeVariablesForClosure
        )
      case AwaitExpression(argument, _) =>
        findFreeVariablesForClosure(argument)
      case YieldExpression(argument, _, _) =>
        if argument != null then findFreeVariablesForClosure(argument)
        else Set.empty
      case TemplateLiteral(_, expressions, _) =>
        expressions.flatMap(findFreeVariablesForClosure).toSet
      case _ => Set.empty
    }

  /** Find all free variables in an expression (non-closure version - functions
    * are boundaries)
    */
  private def findFreeVariables(expr: Expression): Set[String] = expr match {
    case Identifier(name, _) => Set(name)
    case Literal(_, _)       => Set.empty
    case ThisExpression(_)   => Set.empty // 'this' is not a free variable
    case SuperExpression(_)  => Set.empty
    case NewTargetExpression(_) => Set.empty
    case ClassFieldInitializerExpression(expression, _) =>
      findFreeVariablesForClosure(expression)
    case ImportMetaExpression(_) => Set.empty
    case ComputedPropertyName(expression, _) =>
      findFreeVariables(expression)
    case ImportCallExpression(arguments, _) =>
      arguments.flatMap(findFreeVariables).toSet
    case BinaryExpression(_, left, right, _) =>
      findFreeVarsInBinary(left, right, findFreeVariables)
    case UnaryExpression(_, argument, _, _) =>
      findFreeVarsInUnary(argument, findFreeVariables)
    case CallExpression(callee, arguments, _, _) =>
      findFreeVarsInCall(callee, arguments, findFreeVariables)
    case TaggedTemplateExpression(tag, template, _) =>
      findFreeVariables(tag) ++ template.expressions.flatMap(findFreeVariables)
    case NewExpression(callee, arguments, _) =>
      findFreeVarsInCall(callee, arguments, findFreeVariables)
    case MemberExpression(obj, prop, computed, _, _) =>
      findFreeVarsInMember(obj, prop, computed, findFreeVariables)
    case AssignmentExpression(left, right, _) =>
      findFreeVarsInAssignment(
        left,
        right,
        findFreeVariables,
        findFreeVariablesInPattern
      )
    case LogicalAssignmentExpression(_, left, right, _) =>
      findFreeVariables(left) ++ findFreeVariables(right)
    case FunctionExpression(_, _, _, _, _, _, _) =>
      // Function expressions create their own scope, so they don't directly
      // expose free variables from their body to the containing scope
      Set.empty
    case ArrowFunctionExpression(_, _, _, _, _) =>
      // Arrow functions create their own scope
      Set.empty
    case ObjectLiteral(properties, _) =>
      findFreeVarsInObjectLiteral(properties, findFreeVariables)
    case ClassExpression(_, superClass, body, _) =>
      findFreeVarsInClass(
        superClass,
        body,
        findFreeVariables,
        findFreeVariables
      )
    case ArrayLiteral(elements, _, _) =>
      findFreeVarsInArrayLiteral(elements, findFreeVariables)
    case SpreadElement(argument, _) =>
      findFreeVariables(argument)
    case ConditionalExpression(test, consequent, alternate, _) =>
      findFreeVarsInConditional(test, consequent, alternate, findFreeVariables)
    case _ => Set.empty
  }

  private def findFreeVariablesInPattern(pattern: BindingPattern): Set[String] =
    pattern match {
      case Identifier(name, _)                        => Set(name)
      case BindingAssignment(target, defaultValue, _) =>
        findFreeVariablesInPattern(target) ++ findFreeVariables(defaultValue)
      case ArrayPattern(elements, _) =>
        elements
          .filter(_ != null)
          .flatMap {
            case p: BindingPattern => findFreeVariablesInPattern(p)
            case _                 => Set.empty[String]
          }
          .toSet
      case ObjectPattern(properties, rest, _) =>
        val propVars =
          properties.flatMap { p =>
            val keyVars = p.key match {
              case _: Identifier | _: String => Set.empty[String]
              case expression: Expression   => findFreeVariables(expression)
            }
            keyVars ++ findFreeVariablesInPattern(p.value)
          }.toSet
        val restVars =
          if rest != null then findFreeVariablesInPattern(rest.argument)
          else Set.empty[String]
        propVars ++ restVars
      case RestElement(argument, _) =>
        findFreeVariablesInPattern(argument)
    }

  /** Free variables referenced by parameter defaults/computed keys (the bound
    * identifiers themselves are not free).
    */
  private def freeVarsInParamDefaults(pattern: BindingPattern): Set[String] =
    pattern match {
      case Identifier(_, _) => Set.empty
      case BindingAssignment(target, defaultValue, _) =>
        freeVarsInParamDefaults(target) ++ findFreeVariablesForClosure(defaultValue)
      case ArrayPattern(elements, _) =>
        elements
          .filter(_ != null)
          .flatMap {
            case p: BindingPattern => freeVarsInParamDefaults(p)
            case _                 => Set.empty[String]
          }
          .toSet
      case ObjectPattern(properties, rest, _) =>
        val propVars = properties.flatMap { p =>
          val keyVars = p.key match {
            case _: Identifier | _: String => Set.empty[String]
            case expression: Expression    => findFreeVariablesForClosure(expression)
          }
          keyVars ++ freeVarsInParamDefaults(p.value)
        }.toSet
        val restVars =
          if rest != null then freeVarsInParamDefaults(rest.argument) else Set.empty
        propVars ++ restVars
      case RestElement(argument, _) => freeVarsInParamDefaults(argument)
    }

  private def containsDirectEval(pattern: BindingPattern): Boolean =
    pattern match {
      case BindingAssignment(target, defaultValue, _) =>
        containsDirectEval(target) || containsDirectEval(defaultValue)
      case ArrayPattern(elements, _) =>
        elements.exists {
          case p: BindingPattern => containsDirectEval(p)
          case _                 => false
        }
      case ObjectPattern(properties, rest, _) =>
        properties.exists { p =>
          val keyContainsEval = p.key match {
            case _: Identifier | _: String => false
            case expression: Expression   => containsDirectEval(expression)
          }
          keyContainsEval || containsDirectEval(p.value)
        } ||
          (rest != null && containsDirectEval(rest.argument))
      case RestElement(argument, _) => containsDirectEval(argument)
      case _                        => false
    }

  /** Find all variables declared in a statement */
  private def findDeclaredVariables(stmt: Statement): Set[String] = stmt match {
    case VariableDeclaration(_, declarations, _) =>
      val boundNames = declarations.flatMap(d => collectBindingNames(d.id))
      val classExprNames = declarations.flatMap { d =>
        d.init match {
          case ClassExpression(id, _, _, _) if id != null => Seq(id.name)
          case _                                          => Seq.empty
        }
      }
      (boundNames ++ classExprNames).toSet
    case FunctionDeclaration(id, _, _, _, _, _, _) =>
      Set(id.name)
    case ClassDeclaration(id, _, _, _) =>
      Set(id.name)
    case ImportDeclaration(specifiers, _, _) =>
      specifiers.map {
        case ImportNamedSpecifier(_, local, _)  => local.name
        case ImportDefaultSpecifier(local, _)   => local.name
        case ImportNamespaceSpecifier(local, _) => local.name
      }.toSet
    case ExportNamedDeclaration(decl, _, _, _) =>
      if decl != null then findDeclaredVariables(decl) else Set.empty
    case ExportDefaultDeclaration(decl, _) =>
      decl match {
        case stmt: Statement => findDeclaredVariables(stmt)
        case FunctionExpression(id, _, _, _, _, _, _) if id != null =>
          Set(id.name)
        case ClassExpression(id, _, _, _) if id != null =>
          Set(id.name)
        case _ => Set.empty
      }
    case BlockStatement(statements, _) =>
      statements.flatMap(findDeclaredVariables).toSet
    case IfStatement(test, consequent, alternate, _) =>
      findDeclaredVariables(consequent) ++
        (if alternate != null then findDeclaredVariables(alternate)
         else Set.empty)
    case WhileStatement(test, body, _, _) =>
      findDeclaredVariables(body)
    case DoWhileStatement(body, test, _, _) =>
      findDeclaredVariables(body)
    case SwitchStatement(discriminant, cases, _) =>
      cases.flatMap(c => c.consequent.flatMap(findDeclaredVariables)).toSet
    case ForStatement(init, test, update, body, _, _) =>
      val initDeclared = init match {
        case vd: VariableDeclaration => findDeclaredVariables(vd)
        case _                       => Set.empty
      }
      initDeclared ++ findDeclaredVariables(body)
    case ForInStatement(left, _, body, _, _) =>
      val leftDeclared = left match {
        case vd: VariableDeclaration => findDeclaredVariables(vd)
        case _                       => Set.empty
      }
      leftDeclared ++ findDeclaredVariables(body)
    case ForOfStatement(left, _, body, _, _) =>
      val leftDeclared = left match {
        case vd: VariableDeclaration => findDeclaredVariables(vd)
        case _                       => Set.empty
      }
      leftDeclared ++ findDeclaredVariables(body)
    case ForAwaitOfStatement(left, _, body, _, _) =>
      val leftDeclared = left match {
        case vd: VariableDeclaration => findDeclaredVariables(vd)
        case _                       => Set.empty
      }
      leftDeclared ++ findDeclaredVariables(body)
    case WithStatement(_, body, _) =>
      findDeclaredVariables(body)
    case TryStatement(block, handler, finalizer, _) =>
      val handlerDeclared = handler match {
        case null                        => Set.empty
        case CatchClause(param, body, _) =>
          val paramNames = param match {
            case pattern: BindingPattern => collectBindingNames(pattern)
            case null                    => Set.empty
          }
          findDeclaredVariables(body) ++ paramNames
      }
      val finalizerDeclared =
        if finalizer != null then findDeclaredVariables(finalizer)
        else Set.empty
      findDeclaredVariables(block) ++ handlerDeclared ++ finalizerDeclared
    case _ => Set.empty
  }

  /** Find all free variables in a statement, including those in nested function
    * expressions (for closure analysis)
    */
  private def findFreeVariablesForClosure(stmt: Statement): Set[String] =
    stmt match {
      case ExpressionStatement(expr, _) => findFreeVariablesForClosure(expr)
      case ImportDeclaration(_, _, _)   =>
        Set.empty
      case ExportDefaultDeclaration(decl, _) =>
        decl match {
          case expr: Expression => findFreeVariablesForClosure(expr)
          case stmt: Statement  => findFreeVariablesForClosure(stmt)
        }
      case ExportNamedDeclaration(decl, specifiers, source, _) =>
        val declFree =
          if decl != null then findFreeVariablesForClosure(decl) else Set.empty
        val specFree =
          if source != null then Set.empty
          else specifiers.map(spec => moduleExportName(spec.local)).toSet
        declFree ++ specFree
      case ExportAllDeclaration(_, _, _) =>
        Set.empty
      case VariableDeclaration(_, declarations, _) =>
        declarations.flatMap { d =>
          val initFree =
            if d.init != null then findFreeVariablesForClosure(d.init)
            else Set.empty
          // Destructuring defaults and computed keys are free expressions.
          val patternFree = freeVarsInParamDefaults(d.id)
          // Exclude the variable being declared from free variables
          (initFree ++ patternFree) -- collectBindingNames(d.id)
        }.toSet
      case BlockStatement(statements, _) =>
        statements.flatMap(findFreeVariablesForClosure).toSet
      case IfStatement(test, consequent, alternate, _) =>
        findFreeVariablesForClosure(test) ++ findFreeVariablesForClosure(
          consequent
        ) ++
          (if alternate != null then findFreeVariablesForClosure(alternate)
           else Set.empty)
      case WhileStatement(test, body, _, _) =>
        findFreeVariablesForClosure(test) ++ findFreeVariablesForClosure(body)
      case DoWhileStatement(body, test, _, _) =>
        findFreeVariablesForClosure(body) ++ findFreeVariablesForClosure(test)
      case SwitchStatement(discriminant, cases, _) =>
        val discFree = findFreeVariablesForClosure(discriminant)
        val casesFree = cases.flatMap { c =>
          c.test match {
            case null     => c.consequent.flatMap(findFreeVariablesForClosure)
            case testExpr =>
              findFreeVariablesForClosure(testExpr) ++ c.consequent.flatMap(
                findFreeVariablesForClosure
              )
          }
        }
        discFree ++ casesFree
      case ForStatement(init, test, update, body, _, _) =>
        val initFree = init match {
          case e: Expression => findFreeVariablesForClosure(e)
          case s: Statement  => findFreeVariablesForClosure(s)
          case null          => Set.empty
        }
        initFree ++ findFreeVariablesForClosure(test) ++
          findFreeVariablesForClosure(update) ++ findFreeVariablesForClosure(
            body
          )
      case ForInStatement(left, right, body, _, _) =>
        val leftFree = left match {
          case e: Expression => findFreeVariablesForClosure(e)
          case s: Statement  => findFreeVariablesForClosure(s)
        }
        leftFree ++ findFreeVariablesForClosure(
          right
        ) ++ findFreeVariablesForClosure(body)
      case ForOfStatement(left, right, body, _, _) =>
        val leftFree = left match {
          case e: Expression => findFreeVariablesForClosure(e)
          case s: Statement  => findFreeVariablesForClosure(s)
        }
        leftFree ++ findFreeVariablesForClosure(
          right
        ) ++ findFreeVariablesForClosure(body)
      case ForAwaitOfStatement(left, right, body, _, _) =>
        val leftFree = left match {
          case e: Expression => findFreeVariablesForClosure(e)
          case s: Statement  => findFreeVariablesForClosure(s)
        }
        leftFree ++ findFreeVariablesForClosure(
          right
        ) ++ findFreeVariablesForClosure(body)
      case FunctionDeclaration(_, params, body, _, _, _, _) =>
        // Function declarations DO expose free variables from their body in nested scopes!
        // We need to look inside to find what variables the function uses
        val paramNames = params.flatMap(collectBindingNames).toSet
        val bodyFree = findFreeVariablesForClosure(body)
        // Exclude parameters - they're not free variables
        bodyFree -- paramNames
      case ClassDeclaration(_, superClass, body, _) =>
        findFreeVarsInClass(
          superClass,
          body,
          findFreeVariablesForClosure,
          findFreeVariablesForClosure
        )
      case ReturnStatement(argument, _) =>
        if argument != null then findFreeVariablesForClosure(argument)
        else Set.empty
      case ThrowStatement(argument, _) =>
        findFreeVariablesForClosure(argument)
      case TryStatement(block, handler, finalizer, _) =>
        val handlerFree = handler match {
          case null                        => Set.empty
          case CatchClause(param, body, _) =>
            val boundNames = param match {
              case pattern: BindingPattern => collectBindingNames(pattern)
              case null                    => Set.empty
            }
            findFreeVariablesForClosure(body) -- boundNames
        }
        val finalizerFree =
          if finalizer != null then findFreeVariablesForClosure(finalizer)
          else Set.empty
        findFreeVariablesForClosure(block) ++ handlerFree ++ finalizerFree
      case WithStatement(obj, body, _) =>
        findFreeVariablesForClosure(obj) ++ findFreeVariablesForClosure(body)
      case _ => Set.empty
    }

  private def containsDirectEval(expr: Expression): Boolean =
    expr match {
      case CallExpression(Identifier("eval", _), _, _, false) => true
      case CallExpression(callee, args, _, _) =>
        containsDirectEval(callee) || args.exists(containsDirectEval)
      case TaggedTemplateExpression(tag, template, _) =>
        containsDirectEval(tag) || template.expressions.exists(containsDirectEval)
      case BinaryExpression(_, left, right, _) =>
        containsDirectEval(left) || containsDirectEval(right)
      case AssignmentExpression(left, right, _) =>
        (left match {
          case e: Expression => containsDirectEval(e)
          case _             => false
        }) || containsDirectEval(right)
      case LogicalAssignmentExpression(_, left, right, _) =>
        containsDirectEval(left) || containsDirectEval(right)
      case MemberExpression(obj, prop, computed, _, _) =>
        containsDirectEval(obj) || (computed && containsDirectEval(prop))
      case ConditionalExpression(test, consequent, alternate, _) =>
        containsDirectEval(test) || containsDirectEval(consequent) ||
          containsDirectEval(alternate)
      case UnaryExpression(_, argument, _, _) => containsDirectEval(argument)
      case ImportMetaExpression(_)            => false
      case ComputedPropertyName(expression, _) => containsDirectEval(expression)
      case ClassFieldInitializerExpression(expression, _) =>
        containsDirectEval(expression)
      case ImportCallExpression(arguments, _) =>
        arguments.exists(containsDirectEval)
      case ArrayLiteral(elements, _, _) =>
        elements.exists(containsDirectEval)
      case ArrowFunctionExpression(_, body, _, _, _) =>
        body match {
          case Left(expression) => containsDirectEval(expression)
          case Right(block)     => containsDirectEval(block)
        }
      case FunctionExpression(_, _, body, _, _, _, _) =>
        containsDirectEval(body)
      case ObjectLiteral(properties, _) =>
        properties.exists {
          case SpreadElement(argument, _) => containsDirectEval(argument)
          case p: Property =>
            containsDirectEval(p.value) || (p.key match {
              case e: Expression => containsDirectEval(e)
              case _             => false
            })
        }
      case ClassExpression(_, superClass, body, _) =>
        (superClass != null && containsDirectEval(superClass)) ||
          body.elements.exists {
            case m: MethodDefinition => containsDirectEval(m.body)
            case f: FieldDefinition =>
              f.value != null && containsDirectEval(f.value)
          }
      case _ => false
    }

  private def containsDirectEval(stmt: Statement): Boolean =
    stmt match {
      case ExpressionStatement(expr, _) => containsDirectEval(expr)
      case ReturnStatement(argument, _) =>
        argument match {
          case e: Expression => containsDirectEval(e)
          case _             => false
        }
      case VariableDeclaration(_, declarations, _) =>
        declarations.exists(d =>
          (d.init != null && containsDirectEval(d.init)) ||
            containsDirectEval(d.id)
        )
      case BlockStatement(statements, _) =>
        statements.exists(containsDirectEval)
      case IfStatement(test, consequent, alternate, _) =>
        containsDirectEval(test) || containsDirectEval(consequent) ||
          (alternate != null && containsDirectEval(alternate))
      case WhileStatement(test, body, _, _) =>
        containsDirectEval(test) || containsDirectEval(body)
      case DoWhileStatement(body, test, _, _) =>
        containsDirectEval(body) || containsDirectEval(test)
      case ForStatement(init, test, update, body, _, _) =>
        val initHasEval = init match {
          case e: Expression          => containsDirectEval(e)
          case s: VariableDeclaration => containsDirectEval(s)
          case _                      => false
        }
        initHasEval ||
          (test != null && containsDirectEval(test)) ||
          (update != null && containsDirectEval(update)) ||
          containsDirectEval(body)
      case ThrowStatement(argument, _) => containsDirectEval(argument)
      case FunctionDeclaration(_, _, body, _, _, _, _) =>
        containsDirectEval(body)
      case ClassDeclaration(_, superClass, body, _) =>
        (superClass != null && containsDirectEval(superClass)) ||
          body.elements.exists {
            case m: MethodDefinition => containsDirectEval(m.body)
            case f: FieldDefinition =>
              f.value != null && containsDirectEval(f.value)
          }
      case SwitchStatement(discriminant, cases, _) =>
        containsDirectEval(discriminant) || cases.exists(c =>
          (c.test != null && containsDirectEval(c.test)) ||
            c.consequent.exists(containsDirectEval)
        )
      case ForInStatement(left, right, body, _, _) =>
        (left match {
          case e: Expression          => containsDirectEval(e)
          case s: VariableDeclaration => containsDirectEval(s)
        }) || containsDirectEval(right) || containsDirectEval(body)
      case ForOfStatement(left, right, body, _, _) =>
        (left match {
          case e: Expression          => containsDirectEval(e)
          case s: VariableDeclaration => containsDirectEval(s)
        }) || containsDirectEval(right) || containsDirectEval(body)
      case ForAwaitOfStatement(left, right, body, _, _) =>
        (left match {
          case e: Expression          => containsDirectEval(e)
          case s: VariableDeclaration => containsDirectEval(s)
        }) || containsDirectEval(right) || containsDirectEval(body)
      case TryStatement(block, handler, finalizer, _) =>
        containsDirectEval(block) ||
          (handler != null && containsDirectEval(handler.body)) ||
          (finalizer != null && containsDirectEval(finalizer))
      case _ => false
    }

  private def collectBindingNames(pattern: BindingPattern): Set[String] =
    pattern match {
      case Identifier(name, _)             => Set(name)
      case BindingAssignment(target, _, _) => collectBindingNames(target)
      case ArrayPattern(elements, _)       =>
        elements
          .filter(_ != null)
          .flatMap {
            case p: BindingPattern => collectBindingNames(p)
            case _                 => Set.empty[String]
          }
          .toSet
      case ObjectPattern(properties, rest, _) =>
        val propNames =
          properties.flatMap(p => collectBindingNames(p.value)).toSet
        val restNames =
          if rest != null then collectBindingNames(rest.argument)
          else Set.empty[String]
        propNames ++ restNames
      case RestElement(argument, _) =>
        collectBindingNames(argument)
    }

  private def currentBytecodePos(
      instructions: mutable.ArrayBuffer[Instruction]
  ): Int =
    instructions.map(_.size).sum

  private def emitActiveFinallyBlocks(
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    if finallyStack.nonEmpty then {
      val saved = finallyStack
      finallyStack = Nil
      try
        for finalizer <- saved do
          compileStatement(finalizer, instructions, constants, false)
      finally
        finallyStack = saved
    }

  private def emitDefaultIfUndefined(
      defaultValue: Expression,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef],
      inferredName: Option[String] = None
  ): Unit = {
    instructions += Instruction.dup()
    instructions += Instruction.pushUndefined()
    instructions += Instruction.binary(BinaryOpcode.StrictEq)
    val ifFalseInstIndex = instructions.length
    val ifFalseBytePos = currentBytecodePos(instructions)
    instructions += Instruction.ifFalse(0) // placeholder
    instructions += Instruction.drop()
    val shouldInferName = defaultValue match {
      case FunctionExpression(id, _, _, _, _, _, _) => id == null
      case _: ArrowFunctionExpression                => true
      case ClassExpression(id, _, _, _)              => id == null
      case _                                          => false
    }
    inferredName.filter(_ => shouldInferName) match {
      case Some(name) =>
        instructions += Instruction.getGlobal("__setFunctionName")
        compileExpression(defaultValue, instructions, constants)
        pushStringConst(name, instructions, constants)
        instructions += Instruction.call(2)
      case None =>
        compileExpression(defaultValue, instructions, constants)
    }
    val endPos = currentBytecodePos(instructions)
    instructions(ifFalseInstIndex) =
      Instruction.ifFalse(endPos - ifFalseBytePos - 1)
  }

  private def pushStringConst(
      value: String,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = {
    val constIndex = constants.length
    constants += JSValue.fromString(value)
    instructions += Instruction.getConst(constIndex)
  }

  /** Re-export top-level exported bindings at the end of a module body so the
    * namespace observes post-declaration assignments (live bindings).
    */
  private def emitFinalModuleExports(
      body: Seq[Statement],
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = {
    def reexport(exportName: String, localName: String): Unit =
      if currentScope.isLocal(localName) then
        emitModuleExportCall(exportName, instructions, constants) {
          emitLoadIdentifierValue(localName, instructions)
        }
    for stmt <- body do
      stmt match {
        case ExportNamedDeclaration(declaration, specifiers, source, _) =>
          if declaration != null then
            declaration match {
              case VariableDeclaration(_, declarations, _) =>
                for declaration <- declarations do
                  for name <- collectBindingNames(declaration.id) do reexport(name, name)
              case _ => ()
            }
          if specifiers.nonEmpty && source == null then
            for spec <- specifiers do
              reexport(moduleExportName(spec.exported), moduleExportName(spec.local))
        case _ => ()
      }
  }

  private def emitModuleExportCall(
      exportName: String,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  )(emitValue: => Unit): Unit = {
    instructions += Instruction.getGlobal("__moduleExport")
    pushStringConst(currentModuleName, instructions, constants)
    pushStringConst(exportName, instructions, constants)
    emitValue
    instructions += Instruction.call(3)
    instructions += Instruction.drop()
  }

  private def emitLoadIdentifierValue(
      name: String,
      instructions: mutable.ArrayBuffer[Instruction]
  ): Unit =
    if currentScope.isLocal(name) then {
      val index = currentScope.lookup(name).get
      if currentScope.isLexical(name) then
        instructions += Instruction.getLocCheck(index)
      else instructions += Instruction.getLoc(index)
    } else instructions += Instruction.getGlobal(name)

  private def emitBindingStore(
      name: String,
      isDeclaration: Boolean,
      isGlobalVar: Boolean,
      instructions: mutable.ArrayBuffer[Instruction]
  ): Unit = {
    if !isDeclaration && currentScope.isLocal(name) && currentScope.isConst(
        name
      )
    then throw new Exception(s"Cannot assign to const variable '$name'")

    if isDeclaration then
      if isGlobalVar then instructions += Instruction.defVar(name)
      else {
        val index = currentScope.lookup(name).get
        instructions += Instruction.putLoc(index)
      }
    else if currentScope.isLocal(name) then {
      val index = currentScope.lookup(name).get
      instructions += Instruction.putLoc(index)
    } else instructions += Instruction.putGlobal(name)
  }

  private def emitDestructuring(
      pattern: BindingPattern,
      isDeclaration: Boolean,
      isGlobalVar: Boolean,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = pattern match {
    case BindingAssignment(target, defaultValue, _) =>
      emitDefaultIfUndefined(
        defaultValue,
        instructions,
        constants,
        target match {
          case Identifier(name, _) => Some(name)
          case _                   => None
        }
      )
      emitDestructuring(
        target,
        isDeclaration,
        isGlobalVar,
        instructions,
        constants
      )
    case Identifier(name, _) =>
      emitBindingStore(name, isDeclaration, isGlobalVar, instructions)
    case RestElement(argument, _) =>
      // RestElement should be handled by ArrayPattern/ObjectPattern cases
      // If we get here directly, just pass through to the argument
      emitDestructuring(
        argument,
        isDeclaration,
        isGlobalVar,
        instructions,
        constants
      )
    case ArrayPattern(elements, _) =>
      instructions += Instruction.getGlobal("__destructureArray")
      instructions += Instruction.swap()

      // Find the index of RestElement if present
      val restIndex = elements.indexWhere {
        case _: RestElement => true
        case _              => false
      }
      // IteratorBindingInitialization advances once for every element and
      // elision. A rest binding consumes the iterator to completion; otherwise
      // the iterator must be closed after the requested prefix is collected.
      instructions += Instruction.pushI32(elements.length)
      if restIndex >= 0 then instructions += Instruction.pushTrue()
      else instructions += Instruction.pushFalse()
      instructions += Instruction.call(3)

      for (elem, idx) <- elements.zipWithIndex do
        elem match {
          case null                     => ()
          case RestElement(argument, _) =>
            // Rest element: collect remaining elements using slice
            // Stack: [array]
            instructions += Instruction.dup() // [array, array]
            instructions += Instruction.getProp("slice") // [array, sliceFunc]
            // No swap needed - CallMethod expects [this, func, args...]
            instructions += Instruction.pushI32(
              idx
            ) // [array, sliceFunc, startIdx]
            instructions += Instruction.callMethod(1) // [restArray]
            // NOTE: CallMethod consumed the original array (as 'this'), so stack is now [restArray]
            emitDestructuring(
              argument,
              isDeclaration,
              isGlobalVar,
              instructions,
              constants
            )
          case p: BindingPattern =>
            instructions += Instruction.dup()
            instructions += Instruction.pushI32(idx)
            instructions += Instruction.getElem()
            emitDestructuring(
              p,
              isDeclaration,
              isGlobalVar,
              instructions,
              constants
            )
        }

      // Only drop the array if we didn't have a RestElement (which consumes it)
      if restIndex == -1 then instructions += Instruction.drop()
    case ObjectPattern(properties, rest, _) =>
      // Object binding patterns apply RequireObjectCoercible even when empty.
      instructions += Instruction.getGlobal("__requireObjectCoercible")
      instructions += Instruction.swap()
      instructions += Instruction.call(1)
      val extractedKeys = mutable.ArrayBuffer.empty[Either[String, Int]]
      // Handle regular properties. Computed keys are saved because an object
      // rest pattern must exclude the exact same PropertyKey without evaluating
      // the key expression a second time.
      for prop <- properties do {
        instructions += Instruction.dup()
        prop.key match {
          case Identifier(name, _) =>
            instructions += Instruction.getProp(name)
            extractedKeys += Left(name)
          case s: String =>
            instructions += Instruction.getProp(s)
            extractedKeys += Left(s)
          case ComputedPropertyName(expression, _) =>
            compileExpression(expression, instructions, constants)
            val keyIndex = allocateTempLocal("__computedBindingKey")
            instructions += Instruction.dup()
            instructions += Instruction.putLoc(keyIndex)
            instructions += Instruction.getElem()
            extractedKeys += Right(keyIndex)
          case expression: Expression =>
            compileExpression(expression, instructions, constants)
            val keyIndex = allocateTempLocal("__computedBindingKey")
            instructions += Instruction.dup()
            instructions += Instruction.putLoc(keyIndex)
            instructions += Instruction.getElem()
            extractedKeys += Right(keyIndex)
        }
        emitDestructuring(
          prop.value,
          isDeclaration,
          isGlobalVar,
          instructions,
          constants
        )
      }

      // Only drop the object if we didn't have a rest element
      // (rest element handling consumes the object via the call)
      if rest != null then {
        // Create a new object with remaining properties using __objectRest helper
        // Stack: [sourceObj]
        // Get __objectRest function and prepare call
        instructions += Instruction.getGlobal(
          "__objectRest"
        ) // [sourceObj, __objectRest]
        instructions += Instruction.swap() // [__objectRest, sourceObj]

        // Build and push the keys array as second argument
        instructions += Instruction.newArray(
          extractedKeys.length
        ) // [__objectRest, sourceObj, keysArray]
        for (key, idx) <- extractedKeys.zipWithIndex do {
          instructions += Instruction.pushI32(
            idx
          ) // [__objectRest, sourceObj, keysArray, idx]
          key match {
            case Left(name) =>
              val constIdx = constants.length
              constants += JSValue.fromString(name)
              instructions += Instruction.getConst(constIdx)
            case Right(localIndex) =>
              instructions += Instruction.getLoc(localIndex)
          }
          instructions += Instruction
            .initElem() // [__objectRest, sourceObj, keysArray]
        }

        // Call __objectRest(sourceObj, keysArray)
        // NOTE: Call consumes __objectRest, sourceObj, and keysArray from stack
        instructions += Instruction.call(2) // [restObj]
        emitDestructuring(
          rest.argument,
          isDeclaration,
          isGlobalVar,
          instructions,
          constants
        )
      } else
        // No rest element, drop the source object
        instructions += Instruction.drop()
  }

  /** Find all free variables in a statement */
  private def findFreeVariables(stmt: Statement): Set[String] = stmt match {
    case ExpressionStatement(expr, _) => findFreeVariables(expr)
    case ImportDeclaration(_, _, _)   =>
      Set.empty
    case ExportDefaultDeclaration(decl, _) =>
      decl match {
        case expr: Expression => findFreeVariables(expr)
        case stmt: Statement  => findFreeVariables(stmt)
      }
    case ExportNamedDeclaration(decl, specifiers, source, _) =>
      val declFree =
        if decl != null then findFreeVariables(decl) else Set.empty
      val specFree =
        if source != null then Set.empty
        else specifiers.map(spec => moduleExportName(spec.local)).toSet
      declFree ++ specFree
    case ExportAllDeclaration(_, _, _) =>
      Set.empty
    case VariableDeclaration(_, declarations, _) =>
      declarations.flatMap { d =>
        val initFree =
          if d.init != null then findFreeVariables(d.init) else Set.empty
        val patternFree = freeVarsInParamDefaults(d.id)
        // Exclude the variable being declared from free variables
        (initFree ++ patternFree) -- collectBindingNames(d.id)
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
        c.test match {
          case null     => c.consequent.flatMap(findFreeVariables)
          case testExpr =>
            findFreeVariables(testExpr) ++ c.consequent.flatMap(
              findFreeVariables
            )
        }
      }
      discFree ++ casesFree
    case ForStatement(init, test, update, body, _, _) =>
      val initFree = init match {
        case e: Expression => findFreeVariables(e)
        case s: Statement  => findFreeVariables(s)
        case null          => Set.empty
      }
      initFree ++ findFreeVariables(test) ++
        findFreeVariables(update) ++ findFreeVariables(body)
    case ForInStatement(left, right, body, _, _) =>
      val leftFree = left match {
        case e: Expression => findFreeVariables(e)
        case s: Statement  => findFreeVariables(s)
      }
      leftFree ++ findFreeVariables(right) ++ findFreeVariables(body)
    case ForOfStatement(left, right, body, _, _) =>
      val leftFree = left match {
        case e: Expression => findFreeVariables(e)
        case s: Statement  => findFreeVariables(s)
      }
      leftFree ++ findFreeVariables(right) ++ findFreeVariables(body)
    case ForAwaitOfStatement(left, right, body, _, _) =>
      val leftFree = left match {
        case e: Expression => findFreeVariables(e)
        case s: Statement  => findFreeVariables(s)
      }
      leftFree ++ findFreeVariables(right) ++ findFreeVariables(body)
    case FunctionDeclaration(_, params, body, _, _, _, _) =>
      // Function declarations DO expose free variables from their body in nested scopes!
      // We need to look inside to find what variables the function uses
      val paramNames = params.flatMap(collectBindingNames).toSet
      val bodyFree = findFreeVariablesForClosure(body)
      // Exclude parameters - they're not free variables
      bodyFree -- paramNames
    case ClassDeclaration(_, superClass, body, _) =>
      findFreeVarsInClass(
        superClass,
        body,
        findFreeVariables,
        findFreeVariables
      )
    case ReturnStatement(argument, _) =>
      if argument != null then findFreeVariables(argument) else Set.empty
    case ThrowStatement(argument, _) =>
      findFreeVariables(argument)
    case TryStatement(block, handler, finalizer, _) =>
      val handlerFree = handler match {
        case null                        => Set.empty
        case CatchClause(param, body, _) =>
          val boundNames = param match {
            case pattern: BindingPattern => collectBindingNames(pattern)
            case null                    => Set.empty
          }
          findFreeVariables(body) -- boundNames
      }
      val finalizerFree =
        if finalizer != null then findFreeVariables(finalizer) else Set.empty
      findFreeVariables(block) ++ handlerFree ++ finalizerFree
    case WithStatement(obj, body, _) =>
      findFreeVariables(obj) ++ findFreeVariables(body)
    case _ => Set.empty
  }

  /** Compile a function body to bytecode */
  private def compileFunctionBody(
      name: String,
      params: scala.collection.immutable.Seq[BindingPattern],
      body: Statement,
      isConstructor: Boolean = true,
      isGenerator: Boolean = false,
      isAsync: Boolean = false,
      isStrict: Boolean = false,
      functionExpressionName: Option[String] = None,
      isClassConstructor: Boolean = false
  ): BytecodeFunction = {
    // Create a new scope for the function (with parent as current scope for closures)
    val oldScope = currentScope
    val oldIsStrict = currentIsStrict
    val oldIsAsync = currentFunctionIsAsync
    currentScope = new Scope(currentScope)
    currentIsStrict = isStrict
    currentFunctionIsAsync = isAsync

    // Nested functions must not inherit the enclosing function's control-flow
    // context: a `return`/`throw`/`break` inside this function does not unwind
    // outer finally blocks or for-of iterators.
    val savedLoopStack = loopStack
    val savedFinallyStack = finallyStack
    val savedIteratorCloseStack = iteratorCloseStack
    loopStack = mutable.Stack.empty
    finallyStack = Nil
    iteratorCloseStack = Nil

    // Collect variables declared in this function (params and locals)
    val declaredVars = mutable.Set[String]()
    val paramNamesList = mutable.ArrayBuffer[String]()
    val localVarNamesList = mutable.ArrayBuffer[String]()

    val paramSlots = params.zipWithIndex.map { (param, idx) =>
      val slotName = param match {
        case Identifier(name, _)                         => name
        case BindingAssignment(target: Identifier, _, _) => target.name
        case _                                           => s"__param$idx"
      }
      (param, slotName)
    }

    for (_, slotName) <- paramSlots do {
      // Check for invalid parameter names in strict mode
      if currentIsStrict && (slotName == "arguments" || slotName == "eval") then
        throw new RuntimeException(
          s"SyntaxError: invalid parameter name '$slotName' in strict mode"
        )
      declaredVars += slotName
      paramNamesList += slotName
      currentScope.declare(slotName)
    }

    val argumentsIndex =
      if paramNamesList.contains("arguments") then -1
      else currentScope.declare("arguments")
    if argumentsIndex >= 0 then {
      declaredVars += "arguments"
      localVarNamesList += "arguments"
    }

    val paramSlotSet = paramNamesList.toSet
    val paramBindingNames = params.flatMap(collectBindingNames).toSet
    for name <- paramBindingNames if !paramSlotSet.contains(name) do {
      declaredVars += name
      localVarNamesList += name
      currentScope.declare(name)
    }

    val localVars = findDeclaredVariables(body)
    declaredVars ++= localVars
    localVarNamesList ++= (localVars -- paramNamesList.toSet)
    val hasParameterExpressions = params.exists(bindingPatternHasInitializer)

    val bytecode = mutable.ArrayBuffer[Byte]()
    val constants = mutable.ArrayBuffer[AnyRef]()
    val instructions = new InstructionBuffer()

    // Formal bindings exist in a parameter environment before evaluation but
    // remain in the TDZ until initialized from the corresponding raw argument.
    val parameterLocalNames = (paramSlots.map(_._2) ++ paramBindingNames).distinct
    for name <- parameterLocalNames do
      instructions += Instruction.setLocUninitialized(
        currentScope.lookup(name).get
      )

    // Initialize parameter patterns and defaults
    for ((param, slotName), argumentIndex) <- paramSlots.zipWithIndex do
      withSpan(param.span) {
        val slotIndex = currentScope.lookup(slotName).get
        param match {
          case RestElement(argument, _) =>
            instructions += Instruction.getRestArgs(argumentIndex)
            emitDestructuring(
              argument,
              isDeclaration = false,
              isGlobalVar = false,
              instructions,
              constants
            )
          case Identifier(_, _) =>
            instructions += Instruction.getArg(argumentIndex)
            instructions += Instruction.putLoc(slotIndex)
          case BindingAssignment(target: Identifier, defaultValue, _)
              if target.name == slotName =>
            instructions += Instruction.getArg(argumentIndex)
            emitDefaultIfUndefined(
              defaultValue,
              instructions,
              constants,
              Some(target.name)
            )
            instructions += Instruction.putLoc(slotIndex)
          case BindingAssignment(target, defaultValue, _) =>
            instructions += Instruction.getArg(argumentIndex)
            emitDefaultIfUndefined(defaultValue, instructions, constants)
            emitDestructuring(
              target,
              isDeclaration = false,
              isGlobalVar = false,
              instructions,
              constants
            )
          case _ =>
            instructions += Instruction.getArg(argumentIndex)
            emitDestructuring(
              param,
              isDeclaration = false,
              isGlobalVar = false,
              instructions,
              constants
            )
        }
      }

    // Generator calls perform parameter initialization immediately, then
    // suspend before entering the body (QuickJS OP_initial_yield boundary).
    if isGenerator then instructions += Instruction.initialYield()

    val parameterScopeEndPc =
      if hasParameterExpressions then currentBytecodePos(instructions) else 0

    // Body var declarations are not visible while parameter default
    // expressions run. QuickJS models this with an argument scope; declaring
    // body locals after parameter initializers gives default expressions and
    // direct eval the same separation.
    for varName <- localVars do currentScope.declare(varName)

    // Function declarations in a function body are instantiated before any
    // statement is evaluated. Emit direct body declarations up front; their
    // source-position occurrences below are skipped. Emitting them in source
    // order also gives the last duplicate declaration the required binding.
    body match {
      case block: BlockStatement =>
        for declaration <- block.statements.collect {
            case function: FunctionDeclaration => function
          }
        do
          compileStatement(declaration, instructions, constants, false)
        // Compile each statement in the block
        for s <- block.statements if !s.isInstanceOf[FunctionDeclaration] do
          compileStatement(s, instructions, constants, false)
      case _ =>
        // Single statement body
        compileStatement(body, instructions, constants, false)
    }

    // Add implicit return undefined
    withSpan(body.span) {
      instructions += Instruction.returnUndef()
    }

    // Encode instructions to bytecode
    instructions.foreach(inst => bytecode ++= inst.encode())

    // Find free variables (referenced but not declared in this function)
    // Use findFreeVariablesForClosure to look inside nested function expressions
    val paramsContainDirectEval = params.exists(containsDirectEval)
    val paramFreeVars = params.flatMap(freeVarsInParamDefaults).toSet
    val allFreeVars = paramFreeVars ++ findFreeVariablesForClosure(body)
    val evalParentVars =
      if (containsDirectEval(body) || paramsContainDirectEval) && oldScope != null
      then
        oldScope.getAllLocalVarNames.toSet
      else Set.empty[String]
    val freeVarNames =
      (allFreeVars.filterNot(declaredVars.contains) ++ evalParentVars).toArray
    // Resolve free variables that live in the immediate parent's locals to
    // their slot index. Slot-based capture is unambiguous when several
    // block-scoped bindings share a name (sibling blocks, named class
    // expressions), where name lookup could pick another declaration.
    val freeVarSlots: Map[String, Int] =
      if oldScope == null then Map.empty
      else
        freeVarNames.iterator
          .flatMap(name => oldScope.ownSlot(name).map(name -> _))
          .toMap

    // Get all local variable names from the scope (includes temp vars declared during compilation)
    // This ensures internal variables like __super_N are available for closure capture
    val allLocalVarNames = currentScope.getAllLocalVarNames

    // Restore the parent scope
    currentScope = oldScope
    currentIsStrict = oldIsStrict
    currentFunctionIsAsync = oldIsAsync
    loopStack = savedLoopStack
    finallyStack = savedFinallyStack
    iteratorCloseStack = savedIteratorCloseStack

    new BytecodeFunction(
      name = name,
      bytecode = bytecode.toArray,
      constants = constants.toArray,
      stackSize = 4096,
      freeVars = freeVarNames,
      freeVarSlots = freeVarSlots,
      paramNames = paramNamesList.toArray,
      localVarNames = allLocalVarNames,
      argumentsIndex = argumentsIndex,
      isConstructor = isConstructor,
      isClassConstructor = isClassConstructor,
      isGenerator = isGenerator,
      isAsync = isAsync,
      length = computeFunctionLength(params),
      spanMap = buildSpanMap(instructions),
      isStrict = isStrict,
      functionExpressionName = functionExpressionName,
      parameterScopeEndPc = parameterScopeEndPc,
      captureParentClosure = paramsContainDirectEval || containsDirectEval(body)
    )
  }

  private def compileClassDefinition(
      nameBinding: Option[(String, Int)],
      superClass: Expression | Null,
      body: ClassBody,
      exportToGlobal: Boolean,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = {
    def keyName(
        key: Identifier | PrivateIdentifier | String | Expression
    ): String = key match {
      case Identifier(name, _)        => name
      case PrivateIdentifier(name, _) => s"#$name"
      case s: String                  => s
      case ComputedPropertyName(_, _) => "<computed>"
      case _                          => "<computed>"
    }

    // Computed names for every class element are evaluated once, in source
    // order, before static field initializers run. Keep both the enclosing
    // function local (for bytecode) and its name (for constructor capture).
    val computedClassKeys = mutable.ArrayBuffer.empty[(Expression, String, Int)]
    body.elements.foreach { element =>
      val key = element match {
        case method: MethodDefinition => method.key
        case field: FieldDefinition   => field.key
      }
      key match {
        case expression: Expression
            if !expression.isInstanceOf[Identifier] &&
              !expression.isInstanceOf[PrivateIdentifier] =>
          val tempName = s"__classElementKey_${tempVarCounter}"
          tempVarCounter += 1
          val tempIndex = currentScope.declare(tempName)
          computedClassKeys += ((expression, tempName, tempIndex))
        case _ => ()
      }
    }

    def computedKeyTemp(
        expression: Expression
    ): Option[(Expression, String, Int)] =
      // Callers pass either the `ComputedPropertyName` node or its inner
      // expression; unwrap both sides before the identity comparison so the
      // single evaluated key temp is reused instead of re-evaluating the key.
      def unwrap(expr: Expression): Expression = expr match {
        case ComputedPropertyName(inner, _) => inner
        case other                          => other
      }
      val raw = unwrap(expression)
      computedClassKeys.find { case (key, _, _) =>
        unwrap(key).asInstanceOf[AnyRef] eq raw.asInstanceOf[AnyRef]
      }

    def emitRawComputedPropertyKey(expression: Expression): Unit = {
      val rawExpression = expression match {
        case ComputedPropertyName(inner, _) => inner
        case other                          => other
      }
      compileExpression(rawExpression, instructions, constants)
      val rawKeyIndex = allocateTempLocal("__rawPropertyKey")
      instructions += Instruction.putLoc(rawKeyIndex)
      instructions += Instruction.getGlobal("__toPropertyKey")
      instructions += Instruction.getLoc(rawKeyIndex)
      instructions += Instruction.call(1)
    }

    def emitComputedPropertyKey(expression: Expression): Unit =
      computedKeyTemp(expression) match {
        case Some((_, _, tempIndex)) =>
          instructions += Instruction.getLoc(tempIndex)
        case None => emitRawComputedPropertyKey(expression)
      }

    def emitPropertyKey(key: Identifier | String | Expression): Unit =
      key match {
        case Identifier(name, _) =>
          val constIndex = constants.length
          constants += JSValue.fromString(name)
          instructions += Instruction.getConst(constIndex)
        case s: String =>
          val constIndex = constants.length
          constants += JSValue.fromString(s)
          instructions += Instruction.getConst(constIndex)
        case ComputedPropertyName(expression, _) =>
          emitComputedPropertyKey(expression)
        case expr: Expression =>
          emitComputedPropertyKey(expr)
      }

    def emitDefineProperty(
        targetIndex: Int,
        key: Identifier | String | Expression,
        descIndex: Int
    ): Unit = {
      instructions += Instruction.getGlobal("Object")
      instructions += Instruction.getProp("defineProperty")
      instructions += Instruction.getLoc(targetIndex)
      emitPropertyKey(key)
      instructions += Instruction.getLoc(descIndex)
      instructions += Instruction.call(3)
      instructions += Instruction.drop()
    }

    // Emit field initialization instructions directly (supports private fields)
    def emitFieldInit(
        field: FieldDefinition,
        instructions: mutable.ArrayBuffer[Instruction],
        constants: mutable.ArrayBuffer[AnyRef]
    ): Unit = {
      // Push this
      instructions += Instruction.getThis()
      field.key match {
        case PrivateIdentifier(name, _) =>
          if field.value != null then
            compileExpression(field.value, instructions, constants)
          else instructions += Instruction.pushUndefined()
          // Private field - use special opcode
          instructions += Instruction.setPrivateField(privateOpcodeName(name))
          instructions += Instruction
            .drop() // setPrivateField leaves object on stack
        case id: Identifier =>
          if field.value != null then
            compileExpression(field.value, instructions, constants)
          else instructions += Instruction.pushUndefined()
          instructions += Instruction.setProp(id.name)
          instructions += Instruction.drop()
        case s: String =>
          if field.value != null then
            compileExpression(field.value, instructions, constants)
          else instructions += Instruction.pushUndefined()
          instructions += Instruction.setProp(s)
          instructions += Instruction.drop()
        case expr: Expression =>
          // SetElem consumes object, key, value in that order. Computed names
          // are evaluated before their initializer value.
          compileExpression(expr, instructions, constants)
          if field.value != null then
            compileExpression(field.value, instructions, constants)
          else instructions += Instruction.pushUndefined()
          instructions += Instruction.setElem()
          instructions += Instruction.drop()
      }
    }

    def buildFieldInitStatement(field: FieldDefinition): Statement =
      def fieldInitializerExpression(expression: Expression): Expression =
        ClassFieldInitializerExpression(expression, expression.span)

      // For private fields, we create a special marker that will be handled during compilation
      field.key match {
        case priv: PrivateIdentifier =>
          // Return a placeholder - actual emission happens in emitFieldInit
          val thisExpr = ThisExpression(field.span)
          val member =
            MemberExpression(thisExpr, priv, computed = false, field.span)
          val valueExpr =
            if field.value != null then fieldInitializerExpression(field.value)
            else
              fieldInitializerExpression(
                Literal(JSValue.Undefined, field.span)
              )
          val assign = AssignmentExpression(member, valueExpr, field.span)
          ExpressionStatement(assign, field.span)
        case _ =>
          val thisExpr = ThisExpression(field.span)
          val member = field.key match {
            case id: Identifier =>
              MemberExpression(thisExpr, id, computed = false, field.span)
            case s: String =>
              val literal = Literal(JSValue.fromString(s), field.span)
              MemberExpression(thisExpr, literal, computed = true, field.span)
            case expr: Expression =>
              // Computed field names are evaluated once while defining the
              // class, then captured by the synthesized constructor.
              val (_, keyTempName, _) = computedKeyTemp(expr).getOrElse(
                throw new IllegalStateException("missing computed class key")
              )
              MemberExpression(
                thisExpr,
                Identifier(keyTempName, field.span),
                computed = true,
                field.span
              )
          }
          val valueExpr =
            if field.value != null then fieldInitializerExpression(field.value)
            else Literal(JSValue.Undefined, field.span)
          val assign = AssignmentExpression(member, valueExpr, field.span)
          ExpressionStatement(assign, field.span)
      }

    val constructorMethod =
      body.elements.collectFirst {
        case m: MethodDefinition
            if !m.isStatic && m.kind == PropertyKind.Method && keyName(
              m.key
            ) == "constructor" =>
          m
      }

    val classPrivateNames = body.elements.flatMap {
      case method: MethodDefinition =>
        method.key match {
          case PrivateIdentifier(name, _) => Some(name)
          case _                          => None
        }
      case field: FieldDefinition =>
        field.key match {
          case PrivateIdentifier(name, _) => Some(name)
          case _                          => None
      }
    }.toSet

    val classPrivateNameBindings = classPrivateNames.toSeq.sorted.map { name =>
      val bindingName = s"__privateName_${tempVarCounter}"
      tempVarCounter += 1
      val bindingIndex = currentScope.declare(bindingName)
      name -> (bindingName, bindingIndex)
    }.toMap
    val classPrivateBindingNames =
      classPrivateNameBindings.view.mapValues(_._1).toMap

    val instanceMethods =
      body.elements.collect {
        case m: MethodDefinition
            if !m.isStatic && keyName(m.key) != "constructor" && !m.key
              .isInstanceOf[PrivateIdentifier] =>
          m
      }
    val privateInstanceMethods =
      body.elements.collect {
        case m: MethodDefinition
            if !m.isStatic && m.key.isInstanceOf[
              PrivateIdentifier
            ] && m.kind == PropertyKind.Method =>
          m
      }
    val privateInstanceGetters =
      body.elements.collect {
        case m: MethodDefinition
            if !m.isStatic && m.key.isInstanceOf[
              PrivateIdentifier
            ] && m.kind == PropertyKind.Getter =>
          m
      }
    val privateInstanceSetters =
      body.elements.collect {
        case m: MethodDefinition
            if !m.isStatic && m.key.isInstanceOf[
              PrivateIdentifier
            ] && m.kind == PropertyKind.Setter =>
          m
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

    // QuickJS creates private method/accessor closures once while evaluating
    // the class and stores them in the surrounding class scope.  Instance
    // initialization only installs the class brand; it must not allocate a
    // fresh function for every constructed object.
    def allocatePrivateMemberBinding(
        prefix: String,
        method: MethodDefinition
    ): (MethodDefinition, String, Int) = {
      val tempName = s"${prefix}_${tempVarCounter}"
      tempVarCounter += 1
      (method, tempName, currentScope.declare(tempName))
    }

    val privateMethodBindings = privateInstanceMethods.map(
      allocatePrivateMemberBinding("__privateMethod", _)
    )
    val privateGetterBindings = privateInstanceGetters.map(
      allocatePrivateMemberBinding("__privateGetter", _)
    )
    val privateSetterBindings = privateInstanceSetters.map(
      allocatePrivateMemberBinding("__privateSetter", _)
    )

    val fieldInitStatements = instanceFields.map(buildFieldInitStatement)

    // Build private method init statements (function expressions assigned to private fields)
    val privateMethodInitStatements = privateMethodBindings.map {
      (method, bindingName, _) =>
      val PrivateIdentifier(methodName, span) = method.key: @unchecked
      val call = CallExpression(
        callee = Identifier("__initPrivateMethod__", span),
        arguments = Seq(
          ThisExpression(span),
          Identifier(classPrivateNameBindings(methodName)._1, span),
          Identifier(bindingName, span)
        ),
        optional = false,
        span = span
      )
      ExpressionStatement(call, span)
    }

    // Build private getter init statements using helper function
    val privateGetterInitStatements = privateGetterBindings.map {
      (method, bindingName, _) =>
      val PrivateIdentifier(methodName, span) = method.key: @unchecked
      // Call: __initPrivateGetter__(this, "#name", fn)
      val call = CallExpression(
        callee = Identifier("__initPrivateGetter__", span),
        arguments = Seq(
          ThisExpression(span),
          Identifier(classPrivateNameBindings(methodName)._1, span),
          Identifier(bindingName, span)
        ),
        optional = false,
        span = span
      )
      ExpressionStatement(call, span)
    }

    // Build private setter init statements using helper function
    val privateSetterInitStatements = privateSetterBindings.map {
      (method, bindingName, _) =>
      val PrivateIdentifier(methodName, span) = method.key: @unchecked
      // Call: __initPrivateSetter__(this, "#name", fn)
      val call = CallExpression(
        callee = Identifier("__initPrivateSetter__", span),
        arguments = Seq(
          ThisExpression(span),
          Identifier(classPrivateNameBindings(methodName)._1, span),
          Identifier(bindingName, span)
        ),
        optional = false,
        span = span
      )
      ExpressionStatement(call, span)
    }
    val ctorParams =
      constructorMethod match {
        case Some(m) => m.params
        case None    => Seq.empty
      }

    val allPrivateInits =
      privateMethodInitStatements ++ privateGetterInitStatements ++ privateSetterInitStatements

    val ctorBodyStatements =
      constructorMethod match {
        case Some(m) =>
          val BlockStatement(stmts, span) = m.body
          if superClass != null then stmts
          else allPrivateInits ++ fieldInitStatements ++ stmts
        case None =>
          if superClass != null then {
            // Generate __funcSpread(superClass, this, arguments) to forward all arguments
            val argsIdent = Identifier("arguments", body.span)
            // Create: __funcSpread(<superClass>, this, arguments)
            // The superClass expression will be compiled and captured in the closure
            val spreadCall = CallExpression(
              callee = Identifier("__funcSpread", body.span),
              arguments = Seq(superClass, ThisExpression(body.span), argsIdent),
              optional = false,
              span = body.span
            )
            ExpressionStatement(
              spreadCall,
              body.span
            ) +: (allPrivateInits ++ fieldInitStatements)
          } else allPrivateInits ++ fieldInitStatements
      }

    val ctorBody = BlockStatement(ctorBodyStatements, body.span)
    val className = nameBinding.map(_._1).getOrElse("<anonymous>")
    val captureClassName = !exportToGlobal

    // Evaluate superclass BEFORE compiling constructor so it can be captured
    val (superIndex, superVarName) =
      if superClass != null then {
        // Create a unique variable name for the superclass that can be captured
        val varName = s"__super_${tempVarCounter}"
        tempVarCounter += 1
        val idx = currentScope.declare(varName)
        compileExpression(superClass, instructions, constants)
        instructions += Instruction.putLoc(idx)
        // ClassDefinitionEvaluation requires the superclass to be a constructor
        // (unless it is `null`).
        val superIsNullLiteral = superClass match {
          case Literal(JSValue.Null, _) => true
          case _                        => false
        }
        if !superIsNullLiteral then {
          instructions += Instruction.getGlobal("__checkClassHeritage")
          instructions += Instruction.getLoc(idx)
          instructions += Instruction.call(1)
          instructions += Instruction.drop()
        }
        (Some(idx), Some(varName))
      } else (None, None)

    for (privateName, (_, bindingIndex)) <- classPrivateNameBindings do
      val nameConstIndex = constants.length
      constants += JSValue.fromString(privateName)
      instructions += Instruction.getGlobal("__newPrivateName__")
      instructions += Instruction.getConst(nameConstIndex)
      instructions += Instruction.call(1)
      instructions += Instruction.putLoc(bindingIndex)

    // Materialize each private member closure once per class evaluation, as
    // QuickJS does with its scoped private method/getter/setter bindings.
    for (method, _, bindingIndex) <-
        privateMethodBindings ++ privateGetterBindings ++ privateSetterBindings
    do
      val methodName = keyName(method.key)
      val funcName = method.kind match {
        case PropertyKind.Getter => s"get $methodName"
        case PropertyKind.Setter => s"set $methodName"
        case _                   => methodName
      }
      val methodFunc = withClassPrivateNames(
        classPrivateNames,
        classPrivateBindingNames
      ) {
        withClassContext(className, captureClassName) {
          withSuperContext(superClass, isStatic = false, superVarName) {
            withoutStaticFieldThis {
              compileFunctionBody(
                funcName,
                method.params,
                method.body,
                isConstructor = false,
                isGenerator = method.isGenerator,
                isAsync = method.isAsync
              )
            }
          }
        }
      }
      val constIndex = constants.length
      constants += methodFunc
      instructions += Instruction.getConst(constIndex)
      instructions += Instruction.putLoc(bindingIndex)

    // The synthesized default constructor of a derived class forwards the
    // (still uninitialized) receiver to the superclass before `this` is usable.
    val isDefaultDerivedCtor = constructorMethod.isEmpty && superClass != null
    if isDefaultDerivedCtor then defaultDerivedCtorDepth += 1
    val savedPendingInits = pendingDerivedFieldInits
    if superClass != null && constructorMethod.isDefined then
      pendingDerivedFieldInits = (allPrivateInits ++ fieldInitStatements).toList
    val ctorFunc =
      try
        withClassPrivateNames(
          classPrivateNames,
          classPrivateBindingNames
        ) {
          withClassContext(className, captureClassName) {
            withSuperContext(superClass, isStatic = false, superVarName) {
              withoutStaticFieldThis {
                compileFunctionBody(
                  className,
                  ctorParams,
                  ctorBody,
                  isConstructor = true,
                  isClassConstructor = true
                )
              }
            }
          }
        }
      finally {
        pendingDerivedFieldInits = savedPendingInits
        if isDefaultDerivedCtor then defaultDerivedCtorDepth -= 1
      }

    val ctorConstIndex = constants.length
    constants += ctorFunc
    val ctorIndex = allocateTempLocal("__classCtor")
    instructions += Instruction.getConst(ctorConstIndex)
    instructions += Instruction.putLoc(ctorIndex)

    // Mark class constructors with heritage (including `extends null`) so the
    // runtime can enforce derived-constructor `this` semantics.
    if superClass != null then {
      val descIndex = allocateTempLocal("__derivedDesc")
      instructions += Instruction.getGlobal("Object")
      instructions += Instruction.getProp("defineProperty")
      instructions += Instruction.getLoc(ctorIndex)
      pushStringConst("__derivedClass", instructions, constants)
      instructions += Instruction.newObject()
      instructions += Instruction.putLoc(descIndex)
      instructions += Instruction.getLoc(descIndex)
      instructions += Instruction.pushTrue()
      instructions += Instruction.setProp("value")
      instructions += Instruction.drop()
      instructions += Instruction.getLoc(descIndex)
      instructions += Instruction.call(3)
      instructions += Instruction.drop()
    }

    // QuickJS evaluates ClassElement keys by walking the original element
    // list, independent of whether each element is static or instance-side.
    for (expression, _, tempIndex) <- computedClassKeys do
      emitRawComputedPropertyKey(expression)
      instructions += Instruction.putLoc(tempIndex)

    nameBinding.foreach { (name, idx) =>
      instructions += Instruction.getLoc(ctorIndex)
      instructions += Instruction.putLoc(idx)
      if exportToGlobal then {
        instructions += Instruction.getLoc(ctorIndex)
        instructions += Instruction.putGlobal(name)
      }
    }

    val superIsNull = superClass match {
      case Literal(JSValue.Null, _) => true
      case _                        => false
    }

    val protoIndex =
      superIndex match {
        case Some(superIdx) if superIsNull =>
          // `class Foo extends null`: Foo.prototype.[[Prototype]] is null and
          // Foo.[[Prototype]] stays Function.prototype.
          val protoIdx = allocateTempLocal("__classProto")
          instructions += Instruction.newObject()
          instructions += Instruction.putLoc(protoIdx)

          instructions += Instruction.getGlobal("Object")
          instructions += Instruction.getProp("setPrototypeOf")
          instructions += Instruction.getLoc(protoIdx)
          instructions += Instruction.pushNull()
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
      }

    for method <- instanceMethods do {
      val methodName = keyName(method.key)
      val funcName =
        method.kind match {
          case PropertyKind.Getter => s"get $methodName"
          case PropertyKind.Setter => s"set $methodName"
          case _                   => methodName
        }
      val methodFunc = withClassPrivateNames(
        classPrivateNames,
        classPrivateBindingNames
      ) {
        withClassContext(className, captureClassName) {
          withSuperContext(superClass, isStatic = false, superVarName) {
            withoutStaticFieldThis {
              compileFunctionBody(
                funcName,
                method.params,
                method.body,
                isConstructor = false,
                isGenerator = method.isGenerator,
                isAsync = method.isAsync
              )
            }
          }
        }
      }
      val constIndex = constants.length
      constants += methodFunc

      protoIndex.foreach { idx =>
        method.kind match {
          case PropertyKind.Method =>
            val descIndex = allocateTempLocal("__classMethodDesc")
            instructions += Instruction.newObject()
            instructions += Instruction.putLoc(descIndex)
            instructions += Instruction.getLoc(descIndex)
            instructions += Instruction.getConst(constIndex)
            instructions += Instruction.setProp("value")
            instructions += Instruction.drop()
            instructions += Instruction.getLoc(descIndex)
            instructions += Instruction.pushTrue()
            instructions += Instruction.setProp("writable")
            instructions += Instruction.drop()
            instructions += Instruction.getLoc(descIndex)
            instructions += Instruction.pushTrue()
            instructions += Instruction.setProp("configurable")
            instructions += Instruction.drop()
            emitDefineProperty(idx, method.key, descIndex)
          case _ =>
            val descIndex = allocateTempLocal("__classDesc")
            instructions += Instruction.newObject()
            instructions += Instruction.putLoc(descIndex)
            instructions += Instruction.getLoc(descIndex)
            instructions += Instruction.getConst(constIndex)
            method.kind match {
              case PropertyKind.Getter =>
                instructions += Instruction.setProp("get")
              case PropertyKind.Setter =>
                instructions += Instruction.setProp("set")
              case _ => instructions += Instruction.setProp("value")
            }
            instructions += Instruction.drop()
            // Set configurable: true so getter/setter pairs can be merged
            instructions += Instruction.getLoc(descIndex)
            instructions += Instruction.pushTrue()
            instructions += Instruction.setProp("configurable")
            instructions += Instruction.drop()
            emitDefineProperty(idx, method.key, descIndex)
        }
      }
    }

    for method <- staticMethods do {
      val methodName = keyName(method.key)
      val funcName =
        method.kind match {
          case PropertyKind.Getter => s"get $methodName"
          case PropertyKind.Setter => s"set $methodName"
          case _                   => methodName
        }
      val methodFunc = withClassPrivateNames(
        classPrivateNames,
        classPrivateBindingNames
      ) {
        withClassContext(className, captureClassName) {
          withSuperContext(superClass, isStatic = true, superVarName) {
            withoutStaticFieldThis {
              compileFunctionBody(
                funcName,
                method.params,
                method.body,
                isConstructor = false,
                isGenerator = method.isGenerator,
                isAsync = method.isAsync
              )
            }
          }
        }
      }
      val constIndex = constants.length
      constants += methodFunc
      method.kind match {
        case PropertyKind.Method =>
          method.key match {
            case PrivateIdentifier(privateName, _) =>
              instructions += Instruction.getGlobal("__initPrivateMethod__")
              instructions += Instruction.getLoc(ctorIndex)
              instructions += Instruction.getLoc(
                classPrivateNameBindings(privateName)._2
              )
              instructions += Instruction.getConst(constIndex)
              instructions += Instruction.call(3)
              instructions += Instruction.drop()
            case _ =>
              val descIndex = allocateTempLocal("__classMethodDesc")
              instructions += Instruction.newObject()
              instructions += Instruction.putLoc(descIndex)
              instructions += Instruction.getLoc(descIndex)
              instructions += Instruction.getConst(constIndex)
              instructions += Instruction.setProp("value")
              instructions += Instruction.drop()
              instructions += Instruction.getLoc(descIndex)
              instructions += Instruction.pushTrue()
              instructions += Instruction.setProp("writable")
              instructions += Instruction.drop()
              instructions += Instruction.getLoc(descIndex)
              instructions += Instruction.pushTrue()
              instructions += Instruction.setProp("configurable")
              instructions += Instruction.drop()
              emitDefineProperty(ctorIndex, method.key, descIndex)
          }
        case _ =>
          method.key match {
            case PrivateIdentifier(privateName, _) =>
              val helperName = method.kind match {
                case PropertyKind.Getter => "__initPrivateGetter__"
                case PropertyKind.Setter => "__initPrivateSetter__"
                case _ =>
                  throw new IllegalStateException(
                    "unexpected private static member kind"
                  )
              }
              instructions += Instruction.getGlobal(helperName)
              instructions += Instruction.getLoc(ctorIndex)
              instructions += Instruction.getLoc(
                classPrivateNameBindings(privateName)._2
              )
              instructions += Instruction.getConst(constIndex)
              instructions += Instruction.call(3)
              instructions += Instruction.drop()
            case _ =>
              val descIndex = allocateTempLocal("__classDesc")
              instructions += Instruction.newObject()
              instructions += Instruction.putLoc(descIndex)
              instructions += Instruction.getLoc(descIndex)
              instructions += Instruction.getConst(constIndex)
              method.kind match {
                case PropertyKind.Getter =>
                  instructions += Instruction.setProp("get")
                case PropertyKind.Setter =>
                  instructions += Instruction.setProp("set")
                case _ => instructions += Instruction.setProp("value")
              }
              instructions += Instruction.drop()
              // Set configurable: true so getter/setter pairs can be merged
              instructions += Instruction.getLoc(descIndex)
              instructions += Instruction.pushTrue()
              instructions += Instruction.setProp("configurable")
              instructions += Instruction.drop()
              emitDefineProperty(ctorIndex, method.key, descIndex)
          }
      }
    }

    for field <- staticFields do
      field.key match {
        case Identifier(name, _) =>
          instructions += Instruction.getLoc(ctorIndex)
          field.value match {
            case null => instructions += Instruction.pushUndefined()
            case expr =>
              withClassPrivateNames(
                classPrivateNames,
                classPrivateBindingNames
              ) {
                withStaticFieldThis(ctorIndex) {
                  compileExpression(expr, instructions, constants)
                }
              }
          }
          instructions += Instruction.setProp(name)
          instructions += Instruction.drop()
        case PrivateIdentifier(name, _) =>
          // Private static field - use setPrivateField on the constructor
          instructions += Instruction.getLoc(ctorIndex)
          field.value match {
            case null => instructions += Instruction.pushUndefined()
            case expr =>
              withClassPrivateNames(
                classPrivateNames,
                classPrivateBindingNames
              ) {
                withStaticFieldThis(ctorIndex) {
                  compileExpression(expr, instructions, constants)
                }
              }
          }
          instructions += Instruction.definePrivateField(
            s"$name\u001f${classPrivateNameBindings(name)._1}"
          )
          instructions += Instruction.drop()
        case s: String =>
          instructions += Instruction.getLoc(ctorIndex)
          field.value match {
            case null => instructions += Instruction.pushUndefined()
            case expr =>
              withClassPrivateNames(
                classPrivateNames,
                classPrivateBindingNames
              ) {
                withStaticFieldThis(ctorIndex) {
                  compileExpression(expr, instructions, constants)
                }
              }
          }
          instructions += Instruction.setProp(s)
          instructions += Instruction.drop()
        case expr: Expression =>
          instructions += Instruction.getLoc(ctorIndex)
          emitComputedPropertyKey(expr)
          field.value match {
            case null      => instructions += Instruction.pushUndefined()
            case valueExpr =>
              withClassPrivateNames(
                classPrivateNames,
                classPrivateBindingNames
              ) {
                withStaticFieldThis(ctorIndex) {
                  compileExpression(valueExpr, instructions, constants)
                }
              }
          }
          instructions += Instruction.setElem()
          instructions += Instruction.drop()
      }

    instructions += Instruction.getLoc(ctorIndex)
  }

  private def bindingPatternHasInitializer(pattern: BindingPattern): Boolean =
    pattern match {
      case BindingAssignment(_, _, _) => true
      case RestElement(argument, _)   => bindingPatternHasInitializer(argument)
      case ArrayPattern(elements, _) =>
        elements.exists {
          case p: BindingPattern => bindingPatternHasInitializer(p)
          case _                 => false
        }
      case ObjectPattern(properties, rest, _) =>
        properties.exists(p => bindingPatternHasInitializer(p.value)) ||
          (rest != null && bindingPatternHasInitializer(rest.argument))
      case _ => false
    }

  private def computeFunctionLength(
      params: scala.collection.immutable.Seq[BindingPattern]
  ): Int = {
    val defaultIndex = params.indexWhere {
      case BindingAssignment(_, _, _) => true
      case RestElement(_, _)          => true
      case _                          => false
    }
    if defaultIndex == -1 then params.length else defaultIndex
  }

  /** Compile arrow function body */
  private def compileArrowFunctionBody(
      params: scala.collection.immutable.Seq[BindingPattern],
      body: Either[Expression, BlockStatement],
      isAsync: Boolean = false,
      isStrict: Boolean = false
  ): BytecodeFunction = {
    // Create a new scope for the arrow function
    val oldScope = currentScope
    val oldIsStrict = currentIsStrict
    val oldIsAsync = currentFunctionIsAsync
    currentScope = new Scope(currentScope)
    currentIsStrict = isStrict
    currentFunctionIsAsync = isAsync

    val savedLoopStack = loopStack
    val savedFinallyStack = finallyStack
    val savedIteratorCloseStack = iteratorCloseStack
    loopStack = mutable.Stack.empty
    finallyStack = Nil
    iteratorCloseStack = Nil

    // Collect variables declared in this function (params and locals)
    val declaredVars = mutable.Set[String]()
    val paramNamesList = mutable.ArrayBuffer[String]()
    val localVarNamesList = mutable.ArrayBuffer[String]()

    val paramSlots = params.zipWithIndex.map { (param, idx) =>
      val slotName = param match {
        case Identifier(name, _)                         => name
        case BindingAssignment(target: Identifier, _, _) => target.name
        case _                                           => s"__param$idx"
      }
      (param, slotName)
    }

    for (_, slotName) <- paramSlots do {
      // Check for invalid parameter names in strict mode
      if currentIsStrict && (slotName == "arguments" || slotName == "eval") then
        throw new RuntimeException(
          s"SyntaxError: invalid parameter name '$slotName' in strict mode"
        )
      declaredVars += slotName
      paramNamesList += slotName
      currentScope.declare(slotName)
    }

    val paramSlotSet = paramNamesList.toSet
    val paramBindingNames = params.flatMap(collectBindingNames).toSet
    for name <- paramBindingNames if !paramSlotSet.contains(name) do {
      declaredVars += name
      localVarNamesList += name
      currentScope.declare(name)
    }
    val localVars = body match {
      case Left(_) => Set.empty[String] // Expression bodies don't declare vars
      case Right(block) => findDeclaredVariables(block)
    }
    declaredVars ++= localVars
    localVarNamesList ++= (localVars -- paramNamesList.toSet)
    val hasParameterExpressions = params.exists(bindingPatternHasInitializer)

    val bytecode = mutable.ArrayBuffer[Byte]()
    val constants = mutable.ArrayBuffer[AnyRef]()
    val instructions = new InstructionBuffer()

    val parameterLocalNames = (paramSlots.map(_._2) ++ paramBindingNames).distinct
    for name <- parameterLocalNames do
      instructions += Instruction.setLocUninitialized(
        currentScope.lookup(name).get
      )

    // Initialize parameter patterns and defaults
    for ((param, slotName), argumentIndex) <- paramSlots.zipWithIndex do
      withSpan(param.span) {
        val slotIndex = currentScope.lookup(slotName).get
        param match {
          case RestElement(argument, _) =>
            instructions += Instruction.getRestArgs(argumentIndex)
            emitDestructuring(
              argument,
              isDeclaration = false,
              isGlobalVar = false,
              instructions,
              constants
            )
          case Identifier(_, _) =>
            instructions += Instruction.getArg(argumentIndex)
            instructions += Instruction.putLoc(slotIndex)
          case BindingAssignment(target: Identifier, defaultValue, _)
              if target.name == slotName =>
            instructions += Instruction.getArg(argumentIndex)
            emitDefaultIfUndefined(
              defaultValue,
              instructions,
              constants,
              Some(target.name)
            )
            instructions += Instruction.putLoc(slotIndex)
          case BindingAssignment(target, defaultValue, _) =>
            instructions += Instruction.getArg(argumentIndex)
            emitDefaultIfUndefined(defaultValue, instructions, constants)
            emitDestructuring(
              target,
              isDeclaration = false,
              isGlobalVar = false,
              instructions,
              constants
            )
          case _ =>
            instructions += Instruction.getArg(argumentIndex)
            emitDestructuring(
              param,
              isDeclaration = false,
              isGlobalVar = false,
              instructions,
              constants
            )
        }
      }

    val parameterScopeEndPc =
      if hasParameterExpressions then currentBytecodePos(instructions) else 0

    for varName <- localVars do currentScope.declare(varName)

    // Compile the function body based on its type
    body match {
      case Left(expr) =>
        // Concise body: expression is implicitly returned
        compileExpression(expr, instructions, constants)
        withSpan(expr.span) {
          instructions += Instruction.returnInst()
        }
      case Right(block) =>
        // Block body: instantiate function declarations before evaluation
        // (hoisting), then compile the remaining statements in order. This
        // mirrors compileFunctionBody: a reference before the declaration
        // (`it.f = ei; function ei() {}`) must resolve.
        for declaration <- block.statements.collect {
            case function: FunctionDeclaration => function
          }
        do
          compileStatement(declaration, instructions, constants, false)
        for s <- block.statements if !s.isInstanceOf[FunctionDeclaration] do
          compileStatement(s, instructions, constants, false)
        withSpan(block.span) {
          instructions += Instruction.returnUndef()
        }
    }

    // Encode instructions to bytecode
    instructions.foreach(inst => bytecode ++= inst.encode())

    // Find free variables (referenced but not declared in this function)
    val paramsContainDirectEval = params.exists(containsDirectEval)
    val paramFreeVars = params.flatMap(freeVarsInParamDefaults).toSet
    val allFreeVars = paramFreeVars ++ (body match {
      case Left(expr)   => findFreeVariablesForClosure(expr)
      case Right(block) => findFreeVariablesForClosure(block)
    })
    val freeVarNames = (allFreeVars ++ Set("$this", "$newTarget"))
      .filterNot(declaredVars.contains)
      .toArray
    val freeVarSlots: Map[String, Int] =
      if oldScope == null then Map.empty
      else
        freeVarNames.iterator
          .flatMap(name => oldScope.ownSlot(name).map(name -> _))
          .toMap

    // Get all local variable names from the scope (includes temp vars declared during compilation)
    val allLocalVarNames = currentScope.getAllLocalVarNames

    // Restore the parent scope
    currentScope = oldScope
    currentIsStrict = oldIsStrict
    currentFunctionIsAsync = oldIsAsync
    loopStack = savedLoopStack
    finallyStack = savedFinallyStack
    iteratorCloseStack = savedIteratorCloseStack

    new BytecodeFunction(
      name = "<arrow>",
      bytecode = bytecode.toArray,
      constants = constants.toArray,
      stackSize = 4096,
      freeVars = freeVarNames,
      freeVarSlots = freeVarSlots,
      paramNames = paramNamesList.toArray,
      localVarNames = allLocalVarNames,
      argumentsIndex = -1,
      isConstructor = false,
      isAsync = isAsync,
      length = computeFunctionLength(params),
      spanMap = buildSpanMap(instructions),
      isStrict = isStrict,
      parameterScopeEndPc = parameterScopeEndPc,
      captureParentClosure =
        paramsContainDirectEval || (body match {
          case Left(expr)   => containsDirectEval(expr)
          case Right(block) => containsDirectEval(block)
        })
    )
  }

  def compileScript(script: Script): BytecodeFunction = {
    // Reset scope for each script compilation (fixes test isolation issues)
    currentScope = new Scope(null)
    currentIsStrict = script.strict

    val bytecode = mutable.ArrayBuffer[Byte]()
    val constants = mutable.ArrayBuffer[AnyRef]()
    val instructions = new InstructionBuffer()

    // Global function declarations are instantiated before script evaluation.
    // Keep lexical declarations in source order so their TDZ behavior is not
    // affected by this declaration-instantiation pass.
    for declaration <- script.body.collect {
        case function: FunctionDeclaration => function
      }
    do
      compileStatement(declaration, instructions, constants, false)

    val executableBody = script.body.filterNot(_.isInstanceOf[FunctionDeclaration])
    for (stmt, index) <- executableBody.zipWithIndex do {
      val isLast = index == executableBody.length - 1
      // For the last statement, preserve its value so we can return it
      val preserveValue = isLast
      compileStatement(
        stmt,
        instructions,
        constants,
        isLast && replMode,
        preserveValue
      )
    }

    // Module exports of `var`/`let`/`const`/`class` bindings are live: a
    // binding can be assigned after its declaration (TypeScript enums do this
    // with `export var X; (function (X) { X.A = 1 })(X || (X = {}))`). The
    // initial `__moduleExport` call captures the declaration-time value, so
    // re-export every top-level exported binding once the module body has
    // finished evaluating.
    if currentModuleName != "<script>" then
      emitFinalModuleExports(script.body, instructions, constants)

    // Add implicit return (unless last expression already returns value)
    // In REPL mode, the last expression is returned
    // Also return the value if the last statement preserves its expression value
    val lastStmt = executableBody.lastOption
    lastStmt match {
      case Some(_: ExpressionStatement) =>
        if replMode then
          // REPL mode: ExpressionStatement already added Return
          ()
        else
          // Normal mode: ExpressionStatement preserved value, add Return
          instructions += Instruction.returnInst()
      case Some(_: TryStatement) =>
        // Try/catch preserves its final expression completion when present.
        instructions += Instruction.returnInst()
      case _ =>
        // Normal case: return undefined
        if script.body.nonEmpty then instructions += Instruction.returnUndef()
    }

    // Encode instructions to bytecode
    instructions.foreach(inst => bytecode ++= inst.encode())

    // Get all local variable names from the scope (includes internal temp vars like __super_N)
    val localVarNames = currentScope.getAllLocalVarNames

    new BytecodeFunction(
      name = "<script>",
      bytecode = bytecode.toArray,
      constants = constants.toArray,
      stackSize = 4096, // Fixed stack size until stack-depth analysis is added
      localVarNames =
        localVarNames, // Scripts now have local variables for let/const scoping and internal temps
      spanMap = buildSpanMap(instructions),
      isStrict = script.strict,
      globalVarConfigurable = indirectEvalMode,
      isModule = currentModuleName != "<script>"
    )
  }

  def compileModule(script: Script, moduleName: String): BytecodeFunction = {
    val previous = currentModuleName
    val previousAsync = currentFunctionIsAsync
    currentModuleName = moduleName
    // Top-level await has the same suspension semantics as await inside an
    // async function: compile the module body as async so the loader can
    // drive the evaluation promise to settlement.
    currentFunctionIsAsync = true
    try compileScript(script).withAsync(true)
    finally {
      currentModuleName = previous
      currentFunctionIsAsync = previousAsync
    }
  }

  /** Compiles a statement.
    *
    * @param stmt
    *   The statement to compile
    * @param instructions
    *   The instruction buffer
    * @param constants
    *   The constants buffer
    * @param isLastREPLExpression
    *   Whether this is the last expression in REPL mode (adds Return)
    * @param preserveExpressionValue
    *   If true, preserves the last expression value on stack without returning
    */
  private def compileStatement(
      stmt: Statement,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef],
      isLastREPLExpression: Boolean = false,
      preserveExpressionValue: Boolean = false
  ): Unit = {
    val prevSpan = currentSpan
    currentSpan = stmt.span
    try
      stmt match {
        case ImportDeclaration(specifiers, source, _) =>
          if specifiers.isEmpty then {
            instructions += Instruction.getGlobal("__moduleImport")
            pushStringConst(source, instructions, constants)
            instructions += Instruction.call(1)
            instructions += Instruction.drop()
          } else {
            instructions += Instruction.getGlobal("__moduleImport")
            pushStringConst(source, instructions, constants)
            instructions += Instruction.call(1)
            val moduleIndex = allocateTempLocal("__importModule")
            instructions += Instruction.putLoc(moduleIndex)
            for spec <- specifiers do
              spec match {
                case ImportNamespaceSpecifier(local, _) =>
                  val index = currentScope.declare(
                    local.name,
                    isLexical = true,
                    isConst = true
                  )
                  instructions += Instruction.setLocUninitialized(index)
                  instructions += Instruction.setLocConst(index)
                  instructions += Instruction.getLoc(moduleIndex)
                  instructions += Instruction.putLoc(index)
                case ImportDefaultSpecifier(local, _) =>
                  val index = currentScope.declare(
                    local.name,
                    isLexical = true,
                    isConst = true
                  )
                  instructions += Instruction.setLocUninitialized(index)
                  instructions += Instruction.setLocConst(index)
                  instructions += Instruction.getLoc(moduleIndex)
                  instructions += Instruction.getProp("default")
                  instructions += Instruction.putLoc(index)
                case ImportNamedSpecifier(imported, local, _) =>
                  val index = currentScope.declare(
                    local.name,
                    isLexical = true,
                    isConst = true
                  )
                  instructions += Instruction.setLocUninitialized(index)
                  instructions += Instruction.setLocConst(index)
                  instructions += Instruction.getLoc(moduleIndex)
                  instructions += Instruction.getProp(moduleExportName(imported))
                  instructions += Instruction.putLoc(index)
              }
          }

        case ExportDefaultDeclaration(declaration, _) =>
          declaration match {
            case expr: Expression =>
              // `export default function f(){}` / `export default class C{}`
              // also create a module-local binding for the name, which later
              // `export { f ... }` statements read.
              val declName = expr match {
                case FunctionExpression(id, _, _, _, _, _, _) if id != null =>
                  Some(id.name)
                case ClassExpression(id, _, _, _) if id != null =>
                  Some(id.name)
                case _ => None
              }
              declName match {
                case Some(name) =>
                  compileExpression(expr, instructions, constants)
                  val index = currentScope.declare(name)
                  instructions += Instruction.putLoc(index)
                  emitModuleExportCall("default", instructions, constants) {
                    instructions += Instruction.getLoc(index)
                  }
                case None =>
                  emitModuleExportCall("default", instructions, constants) {
                    compileExpression(expr, instructions, constants)
                  }
              }
            case stmtDecl: Statement =>
              compileStatement(stmtDecl, instructions, constants, false)
              stmtDecl match {
                case FunctionDeclaration(id, _, _, _, _, _, _) =>
                  emitModuleExportCall("default", instructions, constants) {
                    emitLoadIdentifierValue(id.name, instructions)
                  }
                case ClassDeclaration(id, _, _, _) =>
                  emitModuleExportCall("default", instructions, constants) {
                    emitLoadIdentifierValue(id.name, instructions)
                  }
                case _ => ()
              }
          }

        case ExportNamedDeclaration(declaration, specifiers, source, _) =>
          if declaration != null then {
            compileStatement(declaration, instructions, constants, false)
            val exportNames = declaration match {
              case VariableDeclaration(_, declarations, _) =>
                declarations.flatMap(d => collectBindingNames(d.id))
              case FunctionDeclaration(id, _, _, _, _, _, _) =>
                Seq(id.name)
              case ClassDeclaration(id, _, _, _) =>
                Seq(id.name)
              case _ =>
                Seq.empty
            }
            for name <- exportNames do
              emitModuleExportCall(name, instructions, constants) {
                emitLoadIdentifierValue(name, instructions)
              }
          }
          if specifiers.nonEmpty then
            source match {
              case null =>
                for spec <- specifiers do
                  emitModuleExportCall(
                    moduleExportName(spec.exported),
                    instructions,
                    constants
                  ) {
                    emitLoadIdentifierValue(
                      moduleExportName(spec.local),
                      instructions
                    )
                  }
              case modName: String =>
                instructions += Instruction.getGlobal("__moduleImport")
                pushStringConst(modName, instructions, constants)
                instructions += Instruction.call(1)
                val moduleIndex = allocateTempLocal("__exportModule")
                instructions += Instruction.putLoc(moduleIndex)
                for spec <- specifiers do
                  emitModuleExportCall(
                    moduleExportName(spec.exported),
                    instructions,
                    constants
                  ) {
                    instructions += Instruction.getLoc(moduleIndex)
                    instructions += Instruction.getProp(moduleExportName(spec.local))
                  }
            }

        case ExportAllDeclaration(source, namespace, _) =>
          // First import/load the source module
          instructions += Instruction.getGlobal("__moduleImport")
          pushStringConst(source, instructions, constants)
          instructions += Instruction.call(1)
          if namespace != null then {
            // `export * as name from 'source'`: export the namespace object.
            val nsIndex = allocateTempLocal("__exportNamespace")
            instructions += Instruction.putLoc(nsIndex)
            emitModuleExportCall(
              moduleExportName(namespace),
              instructions,
              constants
            ) {
              instructions += Instruction.getLoc(nsIndex)
            }
          } else {
            instructions += Instruction.drop()
            // Then re-export all its exports
            instructions += Instruction.getGlobal("__moduleExportAll")
            pushStringConst(currentModuleName, instructions, constants)
            pushStringConst(source, instructions, constants)
            instructions += Instruction.call(2)
            instructions += Instruction.drop()
          }

        case ExpressionStatement(expr, _) =>
          compileExpression(expr, instructions, constants)
          // Handle expression result based on context
          if isLastREPLExpression then
            // Last expression in REPL mode: keep value on stack and return it
            instructions += Instruction.returnInst()
          else if preserveExpressionValue then
            // Preserve value on stack (for try/catch expression results)
            () // Do nothing - leave value on stack
          else
            // Normal case: drop the expression result
            instructions += Instruction.drop()

        case VariableDeclaration(kind, declarations, _) =>
          for decl <- declarations do
            compileVariableDeclarator(decl, kind, instructions, constants)

        case ClassDeclaration(id, superClass, body, _) =>
          val index =
            currentScope.declare(id.name, isLexical = true, isConst = false)
          instructions += Instruction.setLocUninitialized(index)
          compileClassDefinition(
            Some(id.name -> index),
            superClass,
            body,
            exportToGlobal = false,
            instructions,
            constants
          )
          instructions += Instruction.drop()

        case BlockStatement(stmts, _) =>
          // Check if block contains any let/const declarations
          val hasLexicalDecls = stmts.exists {
            case VariableDeclaration(kind, _, _) =>
              kind == VariableKind.Let || kind == VariableKind.Const
            case _ => false
          }

          // Enter block scope if block contains let/const declarations
          if hasLexicalDecls then {
            val scopeIndex = currentScope.enterBlockScope()
            instructions += Instruction.enterScope(scopeIndex)
          }

          // Compile each statement in the block
          // If this block is the last expression in REPL mode, the last statement in the block
          // should also be treated as the last expression (so its value is returned)
          for (s, index) <- stmts.zipWithIndex do {
            val isLastInBlock = index == stmts.length - 1
            val isLastREPLInBlock = isLastREPLExpression && isLastInBlock
            val preserveInBlock = preserveExpressionValue && isLastInBlock
            compileStatement(
              s,
              instructions,
              constants,
              isLastREPLInBlock,
              preserveInBlock
            )
          }

          // Leave block scope if we entered one
          if hasLexicalDecls then {
            val scopeIndex = currentScope.leaveBlockScope()
            instructions += Instruction.leaveScope(scopeIndex)
          }

        case IfStatement(test, consequent, alternate, _) =>
          // Compile test
          compileExpression(test, instructions, constants)

          // Reserve space for jump offset (track byte position, not instruction index)
          val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.ifFalse(0) // Placeholder
          val ifFalseIdx = instructions.length - 1

          // Compile consequent
          compileStatement(
            consequent,
            instructions,
            constants,
            isLastREPLExpression,
            false
          )

          if alternate != null then {
            // If we took the consequent, skip the alternate
            val jumpBytePos = instructions.foldLeft(0)(_ + _.size)
            instructions += Instruction.goto(0) // Placeholder
            val gotoIdx = instructions.length - 1

            // Update the ifFalse jump to skip to after alternate (in bytes)
            val consequentEndBytePos = instructions.foldLeft(0)(_ + _.size)
            val ifFalseOffset = consequentEndBytePos - jumpIfFalseBytePos - 1
            instructions(ifFalseIdx) = Instruction.ifFalse(ifFalseOffset)

            // Compile alternate
            compileStatement(
              alternate,
              instructions,
              constants,
              isLastREPLExpression,
              false
            )

            // Update the jump to skip over alternate (in bytes)
            val alternateEndBytePos = instructions.foldLeft(0)(_ + _.size)
            val gotoOffset = alternateEndBytePos - jumpBytePos - 1
            instructions(gotoIdx) = Instruction.goto(gotoOffset)
          } else {
            // No alternate - just update the ifFalse jump (in bytes)
            val endBytePos = instructions.foldLeft(0)(_ + _.size)
            val ifFalseOffset = endBytePos - jumpIfFalseBytePos - 1
            instructions(ifFalseIdx) = Instruction.ifFalse(ifFalseOffset)
          }

        case WhileStatement(test, body, label, _) =>
          val labelName = if label != null then Some(label.name) else None
          enterLoop(labelName)
          val loopStartBytePos = instructions.foldLeft(0)(_ + _.size)

          // Compile test
          compileExpression(test, instructions, constants)

          // Jump out if false
          val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.ifFalse(0) // Placeholder
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

          // `continue` in a do-while must re-evaluate the test, so the
          // continue target is the test's byte position (the loop start is
          // wrong here: it would re-run the body forever without testing).
          val continueBytePos = instructions.foldLeft(0)(_ + _.size)
          setLoopContinue(continueBytePos, instructions)

          // Compile test
          compileExpression(test, instructions, constants)

          // Jump back to loop start if true
          val currentBytePos = instructions.foldLeft(0)(_ + _.size)
          val backJumpOffset = loopStartBytePos - currentBytePos - 1
          instructions += Instruction.ifTrue(backJumpOffset)

          // Set exit point (for break statements) - after the conditional jump
          val exitBytePos = instructions.foldLeft(0)(_ + _.size)
          setLoopExit(exitBytePos, instructions)

          exitLoop()

        case SwitchStatement(discriminant, cases, _) =>
          // Switch statement compilation - simplified QuickJS pattern
          // Key: evaluate discriminant ONCE, use dup for each comparison

          enterSwitch() // Switch statements support break but NOT continue

          def getBytecodePos(): Int =
            instructions.map(_.size).sum

          // Evaluate discriminant ONCE and leave on stack
          compileExpression(discriminant, instructions, constants)

          // Track jumps that need patching: (instructionIndex, caseIndex)
          val caseJumps = scala.collection.mutable.ArrayBuffer[(Int, Int)]()
          val defaultCaseIdx = cases.indexWhere(_.test == null)

          // Generate comparison code for each non-default case
          for (switchCase, caseIdx) <- cases.zipWithIndex do
            if switchCase.test != null then {
              // dup discriminant and compare with case test
              instructions += Instruction.dup()
              compileExpression(switchCase.test, instructions, constants)
              instructions += Instruction.binary(BinaryOpcode.StrictEq)

              // If equal, jump to this case body (placeholder offset)
              caseJumps += ((instructions.length, caseIdx))
              instructions += Instruction.ifTrue(0)
            }

          // If no case matched, jump to default or exit
          val fallThroughJumpIdx = instructions.length
          val fallThroughBytecodePos = getBytecodePos()
          instructions += Instruction.goto(0) // placeholder

          // Record where each case body starts (in bytecode bytes)
          val caseBodyBytecodePos = scala.collection.mutable.ArrayBuffer[Int]()
          for (switchCase, caseIdx) <- cases.zipWithIndex do {
            caseBodyBytecodePos += getBytecodePos()
            for stmt <- switchCase.consequent do
              compileStatement(stmt, instructions, constants, false)
          }

          // Exit point for switch (where break statements jump to)
          val exitBytecodePos = getBytecodePos()
          setLoopExit(exitBytecodePos, instructions)

          // Patch all the case jumps
          for (jumpIdx, caseIdx) <- caseJumps do {
            val targetBytecodePos = caseBodyBytecodePos(caseIdx)
            // Calculate offset: we need the position of the jump instruction
            val jumpBytecodePos = instructions.slice(0, jumpIdx).map(_.size).sum
            val offset = targetBytecodePos - jumpBytecodePos - 1
            instructions(jumpIdx) = Instruction.ifTrue(offset)
          }

          // Patch the fall-through jump
          val fallThroughTarget =
            if defaultCaseIdx >= 0 then caseBodyBytecodePos(defaultCaseIdx)
            else exitBytecodePos
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
            init match {
              case decl: VariableDeclaration =>
                compileStatement(decl, instructions, constants, false)
              case expr: Expression =>
                compileExpression(expr, instructions, constants)
                instructions += Instruction.drop()
            }

          // goto label_test (skip update on first iteration)
          val gotoTestIdx = instructions.length
          val gotoTestBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0) // Placeholder - will be fixed up

          // label_cont: (continue target)
          val labelContBytePos = instructions.foldLeft(0)(_ + _.size)

          // Fresh per-iteration bindings before the update: closures created in
          // the body keep the binding they captured.
          val forInitDecl = init match {
            case d: VariableDeclaration => d
            case _                      => null
          }
          for slot <- perIterationBindingSlots(forInitDecl, body) do
            instructions += Instruction.cloneLocRef(slot)

          // Compile update (if present)
          if update != null then {
            compileExpression(update, instructions, constants)
            instructions += Instruction.drop()
          }

          // label_test: (test target)
          val labelTestBytePos = instructions.foldLeft(0)(_ + _.size)

          // Fix up the initial goto to jump to label_test
          val gotoTestOffset = labelTestBytePos - gotoTestBytePos - 1
          instructions(gotoTestIdx) = Instruction.goto(gotoTestOffset)

          // Compile test (if present)
          if test != null then compileExpression(test, instructions, constants)
          else
            // No test means always true - push true
            instructions += Instruction.pushTrue()

          // Jump out if false -> goto label_break
          val jumpIfFalseIdx = instructions.length
          val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.ifFalse(
            0
          ) // Placeholder - will be fixed up

          // goto label_body
          val gotoBodyIdx = instructions.length
          val gotoBodyBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0) // Placeholder - will be fixed up

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
          instructions += Instruction.goto(0) // Placeholder - will be fixed up

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

          left match {
            case decl: VariableDeclaration =>
              emitForBindingDeclarations(decl, instructions)
            case _ => ()
          }

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

          // Fresh per-iteration bindings before the key is stored.
          val forInDecl = left match {
            case d: VariableDeclaration => d
            case _                      => null
          }
          for slot <- perIterationBindingSlots(forInDecl, body) do
            instructions += Instruction.cloneLocRef(slot)

          // Const bindings are re-initialized on every iteration, so reset
          // them (including destructured bindings) before storing the key.
          left match {
            case decl: VariableDeclaration if decl.kind == VariableKind.Const =>
              for name <- collectBindingNames(decl.declarations.head.id) do
                if currentScope.isLocal(name) then {
                  val index = currentScope.lookup(name).get
                  instructions += Instruction.setLocUninitialized(index)
                }
            case _ => ()
          }

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
          // for-of iterates over values of an iterable using the iterator protocol
          val labelName = if label != null then Some(label.name) else None
          enterLoop(labelName)

          // Declare variables if left is a variable declaration
          left match {
            case decl: VariableDeclaration =>
              emitForBindingDeclarations(decl, instructions)
            case _ => ()
          }

          // Allocate temp locals for iteration state
          val iteratorIndex = allocateTempLocal("__forOfIterator")
          val resultIndex = allocateTempLocal("__forOfResult")

          // Evaluate the iterable and create an iterator object
          // Call __createIterator(iterable) to get an iterator with its own index
          compileExpression(right, instructions, constants)
          instructions += Instruction.getGlobal("__createIterator")
          instructions += Instruction
            .swap() // Move iterator function below the iterable
          instructions += Instruction.call(1)
          instructions += Instruction.putLoc(iteratorIndex)

          // Jump to test
          val gotoTestIdx = instructions.length
          val gotoTestBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)

          // Continue label (unused for iterator-based for-of, but needed for continue)
          val labelContBytePos = instructions.foldLeft(0)(_ + _.size)

          // Test: call __forOfNext(iterator) and check done
          val labelTestBytePos = instructions.foldLeft(0)(_ + _.size)
          val gotoTestOffset = labelTestBytePos - gotoTestBytePos - 1
          instructions(gotoTestIdx) = Instruction.goto(gotoTestOffset)

          // Call __forOfNext(iterator)
          instructions += Instruction.getGlobal("__forOfNext")
          instructions += Instruction.getLoc(iteratorIndex)
          instructions += Instruction.call(1)
          // Store the result {value, done}
          instructions += Instruction.putLoc(resultIndex)

          // Check if done
          instructions += Instruction.getLoc(resultIndex)
          instructions += Instruction.getProp("done")
          // Jump to end if done is truthy
          val jumpIfFalseIdx = instructions.length
          val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.ifTrue(0)

          // Fresh per-iteration bindings before the value is assigned.
          val forOfDecl = left match {
            case d: VariableDeclaration => d
            case _                      => null
          }
          for slot <- perIterationBindingSlots(forOfDecl, body) do
            instructions += Instruction.cloneLocRef(slot)

          // For const declarations, reset every binding (including
          // destructured ones) to uninitialized at the start of each
          // iteration so it can be re-initialized.
          left match {
            case decl: VariableDeclaration if decl.kind == VariableKind.Const =>
              for name <- collectBindingNames(decl.declarations.head.id) do
                if currentScope.isLocal(name) then {
                  val index = currentScope.lookup(name).get
                  instructions += Instruction.setLocUninitialized(index)
                }
            case _ => ()
          }

          // IteratorBindingInitialization can invoke arbitrary user code. If
          // it throws, close the iterator while preserving that exception.
          val bindingTryIdx = instructions.length
          instructions += Instruction.tryStart(0, 0)
          instructions += Instruction.getLoc(resultIndex)
          instructions += Instruction.getProp("value")
          emitForInAssignment(
            left,
            instructions,
            constants
          ) // Reuse for-in assignment logic
          instructions += Instruction.tryEnd()
          val bindingDoneGotoIdx = instructions.length
          val bindingDoneGotoPos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)

          val bindingCatchPos = instructions.foldLeft(0)(_ + _.size)
          val bindingExceptionIndex = allocateTempLocal("__forOfBindingException")
          instructions += Instruction.getException()
          instructions += Instruction.putLoc(bindingExceptionIndex)
          instructions += Instruction.getGlobal("__iteratorCloseAbrupt")
          instructions += Instruction.getLoc(iteratorIndex)
          instructions += Instruction.call(1)
          instructions += Instruction.drop()
          instructions += Instruction.getLoc(bindingExceptionIndex)
          instructions += Instruction.throwInst()

          val bindingDonePos = instructions.foldLeft(0)(_ + _.size)
          instructions(bindingTryIdx) = Instruction.tryStart(bindingCatchPos, -1)
          instructions(bindingDoneGotoIdx) = Instruction.goto(
            bindingDonePos - bindingDoneGotoPos - 1
          )

          // Compile the loop body with its iterator visible to abrupt control
          // flow (break/return/throw).
          iteratorCloseStack = iteratorIndex :: iteratorCloseStack
          try compileStatement(body, instructions, constants, false)
          finally iteratorCloseStack = iteratorCloseStack.tail

          // Jump back to test
          val gotoTestIdx2 = instructions.length
          val gotoTestBytePos2 = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)

          // Break label: end of loop
          val labelBreakBytePos = instructions.foldLeft(0)(_ + _.size)

          // Fix up jumps
          val gotoTestOffset2 = labelTestBytePos - gotoTestBytePos2 - 1
          instructions(gotoTestIdx2) = Instruction.goto(gotoTestOffset2)

          val ifFalseOffset = labelBreakBytePos - jumpIfFalseBytePos - 1
          instructions(jumpIfFalseIdx) = Instruction.ifTrue(ifFalseOffset)

          setLoopExit(labelBreakBytePos, instructions)
          setLoopContinue(labelContBytePos, instructions)

          exitLoop()

        case ForAwaitOfStatement(left, right, body, label, _) =>
          // for-await-of iterates over async iterables, awaiting each value
          val labelName = if label != null then Some(label.name) else None
          enterLoop(labelName)

          // Declare variables if left is a variable declaration
          left match {
            case decl: VariableDeclaration =>
              emitForBindingDeclarations(decl, instructions)
            case _ => ()
          }

          // Allocate temp locals for iteration state
          val iteratorIndex = allocateTempLocal("__forOfIterator")
          val resultIndex = allocateTempLocal("__forOfResult")

          // Evaluate the async iterable and create an async iterator object:
          // __createAsyncIterator(iterable) prefers Symbol.asyncIterator and
          // falls back to a sync iterator wrapped for the await steps.
          compileExpression(right, instructions, constants)
          instructions += Instruction.getGlobal("__createAsyncIterator")
          instructions += Instruction.swap()
          instructions += Instruction.call(1)
          instructions += Instruction.putLoc(iteratorIndex)

          // Jump to test
          val gotoTestIdx = instructions.length
          val gotoTestBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)

          // Continue label
          val labelContBytePos = instructions.foldLeft(0)(_ + _.size)

          // Test: call __forOfNext(iterator) and check done
          val labelTestBytePos = instructions.foldLeft(0)(_ + _.size)
          val gotoTestOffset = labelTestBytePos - gotoTestBytePos - 1
          instructions(gotoTestIdx) = Instruction.goto(gotoTestOffset)

          // Call __forOfNext(iterator) - returns promise
          instructions += Instruction.getGlobal("__forOfNext")
          instructions += Instruction.getLoc(iteratorIndex)
          instructions += Instruction.call(1)
          // Await the result {value, done}
          instructions += Instruction.awaitAsyncInst()
          // Store the awaited result
          instructions += Instruction.putLoc(resultIndex)

          // Check if done
          instructions += Instruction.getLoc(resultIndex)
          instructions += Instruction.getProp("done")
          // Jump to end if done is truthy
          val jumpIfFalseIdx = instructions.length
          val jumpIfFalseBytePos = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.ifTrue(0)

          // Fresh per-iteration bindings before the awaited value is assigned.
          val forAwaitDecl = left match {
            case d: VariableDeclaration => d
            case _                      => null
          }
          for slot <- perIterationBindingSlots(forAwaitDecl, body) do
            instructions += Instruction.cloneLocRef(slot)

          // For const declarations, reset every binding (including
          // destructured ones) to uninitialized at the start of each
          // iteration so it can be re-initialized.
          left match {
            case decl: VariableDeclaration if decl.kind == VariableKind.Const =>
              for name <- collectBindingNames(decl.declarations.head.id) do
                if currentScope.isLocal(name) then {
                  val index = currentScope.lookup(name).get
                  instructions += Instruction.setLocUninitialized(index)
                }
            case _ => ()
          }

          // Body: get current value, await it, and assign to left
          instructions += Instruction.getLoc(resultIndex)
          instructions += Instruction.getProp("value")
          // Await the value (for async iterables, value might be a promise)
          instructions += Instruction.awaitAsyncInst()
          emitForInAssignment(left, instructions, constants)

          iteratorCloseStack = iteratorIndex :: iteratorCloseStack
          try compileStatement(body, instructions, constants, false)
          finally iteratorCloseStack = iteratorCloseStack.tail

          // Jump back to test
          val gotoTestIdx2 = instructions.length
          val gotoTestBytePos2 = instructions.foldLeft(0)(_ + _.size)
          instructions += Instruction.goto(0)

          // Break label: end of loop
          val labelBreakBytePos = instructions.foldLeft(0)(_ + _.size)

          // Fix up jumps
          val gotoTestOffset2 = labelTestBytePos - gotoTestBytePos2 - 1
          instructions(gotoTestIdx2) = Instruction.goto(gotoTestOffset2)

          val ifFalseOffset = labelBreakBytePos - jumpIfFalseBytePos - 1
          instructions(jumpIfFalseIdx) = Instruction.ifTrue(ifFalseOffset)

          setLoopExit(labelBreakBytePos, instructions)
          setLoopContinue(labelContBytePos, instructions)

          exitLoop()

        case FunctionDeclaration(
              id,
              params,
              body,
              isGenerator,
              isAsync,
              strict,
              _
            ) =>
          // Compile the function body to bytecode
          val funcBytecode = withoutClassFieldEvalContext {
            compileFunctionBody(
              id.name,
              params,
              body,
              isConstructor = !isGenerator && !isAsync,
              isGenerator = isGenerator,
              isAsync = isAsync,
              isStrict = strict
            )
          }

          // Store BytecodeFunction in constants array (will be converted to JSValue.Function at runtime with closure)
          val constIndex = constants.length
          constants += funcBytecode

          // Direct eval creates function declarations in the caller eval
          // environment instead of defining globals.
          instructions += Instruction.getConst(constIndex)
          // Script-level declarations live on the global object; function and
          // module bodies (and direct eval) keep the declaration as a local
          // binding so an inner `function f` shadows outer bindings instead of
          // leaking to `globalThis`.
          val isScriptGlobal =
            currentScope.parent == null && currentModuleName == "<script>" &&
              !directEvalMode
          if isScriptGlobal then instructions += Instruction.defFun(id.name)
          else {
            val index = currentScope.declare(id.name)
            instructions += Instruction.putLoc(index)
          }

        case ReturnStatement(argument, _) =>
          if finallyStack.nonEmpty then
            if argument != null then {
              compileExpression(argument, instructions, constants)
              val retIndex = allocateTempLocal("__finallyRet")
              instructions += Instruction.putLoc(retIndex)
              emitActiveFinallyBlocks(instructions, constants)
              instructions += Instruction.getLoc(retIndex)
              instructions += Instruction.returnInst()
            } else {
              emitActiveFinallyBlocks(instructions, constants)
              instructions += Instruction.returnUndef()
            }
          else if argument != null then {
            compileExpression(argument, instructions, constants)
            if iteratorCloseStack.nonEmpty then {
              val resultIndex = allocateTempLocal("__iteratorReturn")
              instructions += Instruction.putLoc(resultIndex)
              emitIteratorCloses(iteratorCloseStack, instructions)
              instructions += Instruction.getLoc(resultIndex)
            }
            instructions += Instruction.returnInst()
          } else {
            emitIteratorCloses(iteratorCloseStack, instructions)
            instructions += Instruction.returnUndef()
          }

        case ThrowStatement(argument, _) =>
          compileExpression(argument, instructions, constants)
          if iteratorCloseStack.nonEmpty then {
            val thrownIndex = allocateTempLocal("__iteratorThrow")
            instructions += Instruction.putLoc(thrownIndex)
            emitIteratorCloses(iteratorCloseStack, instructions)
            instructions += Instruction.getLoc(thrownIndex)
          }
          instructions += Instruction.throwInst()

        case TryStatement(block, handler, finalizer, _) =>
          def bytePos: Int = instructions.foldLeft(0)(_ + _.size)

          val tryStartIdx = instructions.length
          instructions += Instruction.tryStart(0, 0)

          if finalizer != null then finallyStack = finalizer :: finallyStack

          // Use preserveExpressionValue=true to keep the try block's result on the stack
          compileStatement(
            block,
            instructions,
            constants,
            false,
            preserveExpressionValue = true
          )

          instructions += Instruction.tryEnd()

          val gotoAfterTryIdx =
            if handler != null then {
              val gotoIdx = instructions.length
              instructions += Instruction.goto(0)
              gotoIdx
            } else -1

          val catchBytePos = if handler != null then bytePos else -1
          var catchGotoFinallyIdx = -1
          var catchTryStartIdx = -1

          if handler != null then {
            val CatchClause(param, body, _) = handler
            if finalizer != null then {
              catchTryStartIdx = instructions.length
              instructions += Instruction.tryStart(-1, 0)
            }

            val scopeIndex = currentScope.enterBlockScope()
            instructions += Instruction.enterScope(scopeIndex)
            param match {
              case pattern: BindingPattern =>
                val boundNames = collectBindingNames(pattern)
                for name <- boundNames do {
                  val index = currentScope.declare(
                    name,
                    isLexical = true,
                    isConst = false
                  )
                  instructions += Instruction.setLocUninitialized(index)
                }
                instructions += Instruction.getException()
                emitDestructuring(
                  pattern,
                  isDeclaration = true,
                  isGlobalVar = false,
                  instructions,
                  constants
                )
              case null =>
                // Optional catch binding - get and discard the exception
                instructions += Instruction.getException()
                instructions += Instruction.drop()
            }

            // Use preserveExpressionValue=true to keep the catch block's result on the stack
            compileStatement(
              body,
              instructions,
              constants,
              false,
              preserveExpressionValue = true
            )

            instructions += Instruction.leaveScope(scopeIndex)

            if finalizer != null then {
              instructions += Instruction.tryEnd()
              catchGotoFinallyIdx = instructions.length
              instructions += Instruction.goto(0)
            }
          }

          if finalizer != null then finallyStack = finallyStack.tail

          val finallyBytePos = if finalizer != null then bytePos else -1
          if finalizer != null then {
            compileStatement(finalizer, instructions, constants, false)
            instructions += Instruction.rethrowIfPending()
          }

          val endBytePos = bytePos

          val patchedCatchPc = if handler != null then catchBytePos else -1
          val patchedFinallyPc =
            if finalizer != null then finallyBytePos else -1
          instructions(tryStartIdx) =
            Instruction.tryStart(patchedCatchPc, patchedFinallyPc)

          if catchTryStartIdx >= 0 then
            instructions(catchTryStartIdx) =
              Instruction.tryStart(-1, patchedFinallyPc)

          if gotoAfterTryIdx >= 0 then {
            val gotoAfterTryBytePos =
              instructions.slice(0, gotoAfterTryIdx).map(_.size).sum
            val targetPos =
              if finalizer != null then finallyBytePos else endBytePos
            val offset = targetPos - gotoAfterTryBytePos - 1
            instructions(gotoAfterTryIdx) = Instruction.goto(offset)
          }

          if catchGotoFinallyIdx >= 0 then {
            val catchGotoBytePos =
              instructions.slice(0, catchGotoFinallyIdx).map(_.size).sum
            val offset = finallyBytePos - catchGotoBytePos - 1
            instructions(catchGotoFinallyIdx) = Instruction.goto(offset)
          }

        case WithStatement(obj, body, _) =>
          compileExpression(obj, instructions, constants)
          instructions += Instruction.pushWith()
          withScopeDepth += 1
          try compileStatement(body, instructions, constants, false)
          finally withScopeDepth -= 1
          instructions += Instruction.popWith()

        case BreakStatement(label, _) =>
          emitActiveFinallyBlocks(instructions, constants)
          // An unlabeled break exits the innermost active for-of loop. A
          // labeled break may cross multiple loops; closing every active
          // iterator is conservative and correct for the common labeled-loop
          // case until loop entries carry iterator metadata directly.
          if iteratorCloseStack.nonEmpty then
            emitIteratorCloses(
              if label == null then iteratorCloseStack.take(1)
              else iteratorCloseStack,
              instructions
            )
          // Handle labeled and unlabeled break following QuickJS C pattern
          if label == null then
            // Unlabeled break: find innermost loop or switch (skip regular labeled statements)
            loopStack.indexWhere { case (_, _, _, _, _, _, isRegular) =>
              !isRegular
            } match {
              case -1 =>
                // Not in a loop or switch - semantic error
                instructions += Instruction.breakInst()
              case idx =>
                val (isLoop, labelName, exitBytePos, _, _, _, _) = loopStack(
                  idx
                )
                if exitBytePos >= 0 then {
                  // Exit point known, emit goto directly
                  val currentPos = instructions.foldLeft(0)(_ + _.size)
                  val offset = exitBytePos - currentPos - 1
                  instructions += Instruction.goto(offset)
                } else {
                  // Exit position not set yet, emit placeholder and add to pending list
                  val breakInstIdx = instructions.length
                  val breakBytePos = instructions.foldLeft(0)(_ + _.size)
                  instructions += Instruction.goto(0)
                  // Add to the target's pending breaks
                  val stackEntry = loopStack(idx)
                  val (
                    isLoop2,
                    labelName2,
                    oldExit,
                    cont,
                    pendingBreaks,
                    pendingContinues,
                    isRegular
                  ) = stackEntry
                  pendingBreaks += ((breakInstIdx, breakBytePos))
                  loopStack(idx) = (
                    isLoop2,
                    labelName2,
                    oldExit,
                    cont,
                    pendingBreaks,
                    pendingContinues,
                    isRegular
                  )
                }
            }
          else
            // Labeled break: find the statement with matching label
            findLabeledStatement(label.name) match {
              case None =>
                // Label not found - semantic error
                instructions += Instruction.breakInst()
              case Some((isLoop, _, exitBytePos, _, _)) =>
                if exitBytePos >= 0 then {
                  val currentPos = instructions.foldLeft(0)(_ + _.size)
                  val offset = exitBytePos - currentPos - 1
                  instructions += Instruction.goto(offset)
                } else {
                  // Exit position not set yet, need to add to pending list of the specific labeled statement
                  val breakInstIdx = instructions.length
                  val breakBytePos = instructions.foldLeft(0)(_ + _.size)
                  instructions += Instruction.goto(0)
                  // Find the index of the labeled statement on the stack
                  loopStack.indexWhere { case (_, labelName, _, _, _, _, _) =>
                    labelName.exists(_ == label.name)
                  } match {
                    case -1 =>
                      // Should not happen since we already found it
                      ()
                    case idx =>
                      // Add to the specific labeled statement's pending breaks
                      val stackEntry = loopStack(idx)
                      val (
                        isLoop2,
                        labelName2,
                        oldExit,
                        cont,
                        pendingBreaks,
                        pendingContinues,
                        isRegular
                      ) = stackEntry
                      pendingBreaks += ((breakInstIdx, breakBytePos))
                      loopStack(idx) = (
                        isLoop2,
                        labelName2,
                        oldExit,
                        cont,
                        pendingBreaks,
                        pendingContinues,
                        isRegular
                      )
                  }
                }
            }

        case ContinueStatement(label, _) =>
          emitActiveFinallyBlocks(instructions, constants)
          // Handle labeled and unlabeled continue following QuickJS C pattern
          // Continue only works with loops (for/while/do-while), not switches or regular statements
          if label == null then
            // Unlabeled continue: find innermost loop (skip switches and regular statements)
            getCurrentLoopContinue() match {
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
            }
          else
            // Labeled continue: find the loop with matching label
            loopStack.indexWhere { case (isLoop, lbl, _, _, _, _, _) =>
              isLoop && lbl.exists(_ == label.name)
            } match {
              case -1 =>
                // Label not found or not a loop - semantic error
                instructions += Instruction.continueInst()
              case idx =>
                val (_, _, _, contBytePos, _, _, _) = loopStack(idx)
                if contBytePos >= 0 then {
                  val currentPos = instructions.foldLeft(0)(_ + _.size)
                  val offset = contBytePos - currentPos - 1
                  instructions += Instruction.goto(offset)
                } else {
                  // Continue position not set yet, emit placeholder and add to pending list of the specific labeled loop
                  val contInstIdx = instructions.length
                  val contBytePosCalc = instructions.foldLeft(0)(_ + _.size)
                  instructions += Instruction.goto(0)
                  // Add to the specific labeled loop's pending continues
                  val stackEntry = loopStack(idx)
                  val (
                    isLoop2,
                    labelName2,
                    oldExit,
                    oldCont,
                    pendingBreaks,
                    pendingContinues,
                    isRegular
                  ) = stackEntry
                  pendingContinues += ((contInstIdx, contBytePosCalc))
                  loopStack(idx) = (
                    isLoop2,
                    labelName2,
                    oldExit,
                    oldCont,
                    pendingBreaks,
                    pendingContinues,
                    isRegular
                  )
                }
            }

        case _ =>
          throw new UnsupportedOperationException(
            s"Unsupported statement: $stmt"
          )
      }
    finally
      currentSpan = prevSpan
  }
  private def compileVariableDeclarator(
      decl: VariableDeclarator,
      kind: VariableKind,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = {
    // Check if we're at the top level (script scope)
    val isTopLevel = currentScope.parent == null
    val isModule = currentModuleName != "<script>"

    // Determine if this is a lexical variable (let/const) vs var
    val isLexical = kind == VariableKind.Let || kind == VariableKind.Const
    val isConst = kind == VariableKind.Const
    val isGlobalVar = isTopLevel && !isLexical && !isModule && !directEvalMode

    decl.id match {
      case Identifier(name, _) =>
        // Check for invalid variable names in strict mode
        if currentIsStrict && (name == "arguments" || name == "eval") then
          throw new RuntimeException(
            s"SyntaxError: invalid variable name '$name' in strict mode"
          )
        if isGlobalVar then {
          // Top-level var goes to global scope (for compatibility)
          if decl.init != null then
            compileExpression(decl.init, instructions, constants)
          else
            // Push undefined for global scope
            instructions += Instruction.pushUndefined()

          // Store in global scope
          instructions += Instruction.defVar(name)
        } else {
          // let/const (at any level) and var in functions use local variables
          // This enables proper shadowing for let/const
          val alreadyDeclared = currentScope.contains(name)
          val index = currentScope.declare(name, isLexical, isConst)

          if isConst then {
            instructions += Instruction.setLocUninitialized(index)
            instructions += Instruction.setLocConst(index)
          }

          if decl.init != null then {
            compileExpression(decl.init, instructions, constants)
            instructions += Instruction.putLoc(index)
          } else if isLexical then
            // `let x;` initializes the binding to undefined when the
            // declaration is evaluated (const without initializer is a
            // syntax error). Emitting only SetLocUninitialized left the slot
            // in the TDZ forever, so any later read threw ReferenceError.
            instructions += Instruction.pushUndefined()
            instructions += Instruction.putLoc(index)
          else if !alreadyDeclared then {
            // For var without initializer, initialize to undefined only if not already declared
            // (avoids resetting parameters that are redeclared with 'var')
            instructions += Instruction.pushUndefined()
            instructions += Instruction.putLoc(index)
          }
        }
      case pattern: BindingPattern =>
        if !isGlobalVar then
          for name <- collectBindingNames(pattern) do {
            val index = currentScope.declare(name, isLexical, isConst)
            if isConst then {
              instructions += Instruction.setLocUninitialized(index)
              instructions += Instruction.setLocConst(index)
            }
          }

        if decl.init != null then
          compileExpression(decl.init, instructions, constants)
        else {
          if isConst then
            throw new Exception(
              "Destructuring const declaration requires an initializer"
            )
          instructions += Instruction.pushUndefined()
        }
        emitDestructuring(
          pattern,
          isDeclaration = true,
          isGlobalVar,
          instructions,
          constants
        )
    }
  }

  private def hasSpreadArguments(arguments: Seq[Expression]): Boolean =
    arguments.exists {
      case SpreadElement(_, _) => true
      case _                   => false
    }

  private def emitArgumentArray(
      arguments: Seq[Expression],
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = {
    instructions += Instruction.newArray(0)
    for arg <- arguments do
      arg match {
        case SpreadElement(argument, _) =>
          instructions += Instruction.getGlobal("__arraySpread")
          instructions += Instruction.swap()
          compileExpression(argument, instructions, constants)
          instructions += Instruction.call(2)
        case expr =>
          instructions += Instruction.getGlobal("__arrayPush")
          instructions += Instruction.swap()
          compileExpression(expr, instructions, constants)
          instructions += Instruction.call(2)
      }
  }

  private def emitLogicalAssignmentTest(
      operator: LogicalAssignmentOperator,
      instructions: mutable.ArrayBuffer[Instruction],
      assignBody: => Unit,
      cleanupPreservedReference: () => Unit
  ): Unit = {
    operator match {
      case LogicalAssignmentOperator.And | LogicalAssignmentOperator.Or =>
        instructions += Instruction.dup()
        val skipIdx = instructions.length
        val skipPos = currentBytecodePos(instructions)
        if operator == LogicalAssignmentOperator.And then
          instructions += Instruction.ifFalse(0)
        else instructions += Instruction.ifTrue(0)
        instructions += Instruction.drop()
        assignBody
        val endGotoIdx = instructions.length
        val endGotoPos = currentBytecodePos(instructions)
        instructions += Instruction.goto(0)
        val skipPosTarget = currentBytecodePos(instructions)
        if operator == LogicalAssignmentOperator.And then
          instructions(skipIdx) = Instruction.ifFalse(skipPosTarget - skipPos - 1)
        else instructions(skipIdx) = Instruction.ifTrue(skipPosTarget - skipPos - 1)
        cleanupPreservedReference()
        val endPos = currentBytecodePos(instructions)
        instructions(endGotoIdx) = Instruction.goto(endPos - endGotoPos - 1)

      case LogicalAssignmentOperator.Nullish =>
        instructions += Instruction.dup()
        instructions += Instruction.pushNull()
        instructions += Instruction.binary(BinaryOpcode.StrictEq)
        val nullAssignIdx = instructions.length
        val nullAssignPos = currentBytecodePos(instructions)
        instructions += Instruction.ifTrue(0)

        instructions += Instruction.dup()
        instructions += Instruction.pushUndefined()
        instructions += Instruction.binary(BinaryOpcode.StrictEq)
        val skipIdx = instructions.length
        val skipPos = currentBytecodePos(instructions)
        instructions += Instruction.ifFalse(0)

        val assignPos = currentBytecodePos(instructions)
        instructions(nullAssignIdx) =
          Instruction.ifTrue(assignPos - nullAssignPos - 1)
        instructions += Instruction.drop()
        assignBody
        val endGotoIdx = instructions.length
        val endGotoPos = currentBytecodePos(instructions)
        instructions += Instruction.goto(0)
        val skipPosTarget = currentBytecodePos(instructions)
        instructions(skipIdx) = Instruction.ifFalse(skipPosTarget - skipPos - 1)
        cleanupPreservedReference()
        val endPos = currentBytecodePos(instructions)
        instructions(endGotoIdx) = Instruction.goto(endPos - endGotoPos - 1)
    }
  }

  private def emitLogicalAssignment(
      operator: LogicalAssignmentOperator,
      left: Expression,
      right: Expression,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = {
    left match {
      case Identifier(name, _) =>
        if withScopeDepth == 0 && currentScope.isLocal(name) && currentScope.isConst(name) then
          throw new Exception(s"Cannot assign to const variable '$name'")

        compileExpression(left, instructions, constants)
        emitLogicalAssignmentTest(
          operator,
          instructions,
          {
            compileExpression(right, instructions, constants)
            instructions += Instruction.dup()
            if withScopeDepth == 0 && currentScope.isLocal(name) then
              instructions += Instruction.putLoc(currentScope.lookup(name).get)
            else instructions += Instruction.putGlobal(name)
          },
          () => ()
        )

      case MemberExpression(obj, prop, computed, _, _) =>
        if computed then {
          compileExpression(obj, instructions, constants)
          compileExpression(prop, instructions, constants)
          instructions += Instruction.dup2()
          instructions += Instruction.getElem()
          emitLogicalAssignmentTest(
            operator,
            instructions,
            {
              compileExpression(right, instructions, constants)
              instructions += Instruction.setElem()
            },
            () => {
              instructions += Instruction.nip()
              instructions += Instruction.nip()
            }
          )
        } else {
          compileExpression(obj, instructions, constants)
          instructions += Instruction.dup()
          prop match {
            case Identifier(name, _) =>
              instructions += Instruction.getProp(name)
            case PrivateIdentifier(name, _) =>
              instructions += Instruction.getPrivateField(
                privateOpcodeName(name)
              )
            case _ =>
              throw new UnsupportedOperationException(
                s"Unsupported logical assignment property key: $prop"
              )
          }
          emitLogicalAssignmentTest(
            operator,
            instructions,
            {
              compileExpression(right, instructions, constants)
              prop match {
                case Identifier(name, _) =>
                  instructions += Instruction.setProp(name)
                case PrivateIdentifier(name, _) =>
                  instructions += Instruction.setPrivateField(
                    privateOpcodeName(name)
                  )
                case _ => ()
              }
            },
            () => instructions += Instruction.nip()
          )
        }

      case _ =>
        throw new UnsupportedOperationException(
          s"Unsupported logical assignment target: $left"
        )
    }
  }

  private def emitTemplateObject(
      template: TemplateLiteral,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = {
    val templateObjectId = s"$templateObjectPrefix:${templateObjectCounter}"
    templateObjectCounter += 1

    def stringArray(values: Seq[String]): ArrayLiteral =
      ArrayLiteral(
        values.map(v => Literal(JSValue.fromString(v), template.span)),
        template.span
      )

    instructions += Instruction.getGlobal("__makeTemplateObject")
    compileLiteral(JSValue.fromString(templateObjectId), instructions, constants)
    compileExpression(
      stringArray(template.elements.map(_.cooked)),
      instructions,
      constants
    )
    compileExpression(
      stringArray(template.elements.map(_.raw)),
      instructions,
      constants
    )
    instructions += Instruction.call(3)
  }

  private def compileExpression(
      expr: Expression,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    withSpan(expr.span) {
      expr match {
        case Literal(value, _) =>
          compileLiteral(value, instructions, constants)

        case ComputedPropertyName(expression, _) =>
          compileExpression(expression, instructions, constants)

        case ClassFieldInitializerExpression(expression, _) =>
          withClassFieldEvalContext {
            compileExpression(expression, instructions, constants)
          }

        case Identifier(name, _) =>
          // Look up variable in scope
          // Only use GetLoc/GetLocCheck for variables in the current function's immediate scope
          // For variables from outer scopes (closures), use GetGlobal which checks the closure at runtime
          if withScopeDepth > 0 then
            instructions += Instruction.getGlobal(name)
          else if currentScope.isLocal(name) then {
            val index = currentScope.lookup(name).get
            // Use GetLocCheck for lexical variables (let/const) to enforce TDZ
            if currentScope.isLexical(name) then
              instructions += Instruction.getLocCheck(index)
            else instructions += Instruction.getLoc(index)
          } else
            // Variable is from outer scope or global - use GetGlobal
            // GetGlobal checks the closure first, then global scope
            instructions += Instruction.getGlobal(name)

      case SuperExpression(_) =>
          if currentSuperClass == null then {
            instructions += Instruction.getGlobal("ReferenceError")
            val msgIndex = constants.length
            constants += JSValue.fromString("super is not defined")
            instructions += Instruction.getConst(msgIndex)
            instructions += Instruction.call(1)
            instructions += Instruction.throwInst()
          } else {
            // Use the captured superclass variable if available, otherwise compile the expression
            currentSuperVarName match {
              case Some(varName) =>
                instructions += Instruction.getGlobal(varName)
              case None =>
              compileExpression(currentSuperClass, instructions, constants)
            }
            if !currentSuperIsStatic then
              instructions += Instruction.getProp("prototype")
          }

        case NewTargetExpression(_) =>
          instructions += Instruction.getGlobal("$newTarget")

        case ClassExpression(id, superClass, body, _) =>
          // A named class expression's name is scoped to the class body only.
          // Open a fresh block so two `class u {}` expressions in the same
          // scope get distinct slots instead of sharing one.
          currentScope.enterBlockScope()
          try {
            val nameBinding =
              if id != null then {
                val index =
                  currentScope.declare(id.name, isLexical = true, isConst = false)
                instructions += Instruction.setLocUninitialized(index)
                Some(id.name -> index)
              } else None
            compileClassDefinition(
              nameBinding,
              superClass,
              body,
              // A named class expression's binding belongs to the class's own
              // lexical environment; it must never be exported as a surrounding
              // or global binding.
              exportToGlobal = false,
              instructions,
              constants
            )
          } finally currentScope.leaveBlockScope()

        case ThisExpression(_) =>
          // Push the 'this' value onto the stack. Inside a synthesized default
          // derived constructor the receiver is still uninitialized while it is
          // forwarded to the superclass.
          currentStaticFieldThis match {
            case Some(index) => instructions += Instruction.getLoc(index)
            case None =>
              if defaultDerivedCtorDepth > 0 then
                instructions += Instruction.getThisUnchecked()
              else instructions += Instruction.getThis()
          }

        case ImportMetaExpression(_) =>
          if currentModuleName == "<script>" then
            throw new RuntimeException("import.meta only valid in module code")
          instructions += Instruction.getGlobal("__importMeta")
          pushStringConst(currentModuleName, instructions, constants)
          instructions += Instruction.call(1)

        case ImportCallExpression(arguments, _) =>
          instructions += Instruction.getGlobal("__dynamicImport")
          if arguments.isEmpty then instructions += Instruction.pushUndefined()
          else compileExpression(arguments.head, instructions, constants)
          pushStringConst(currentModuleName, instructions, constants)
          instructions += Instruction.call(2)

        case BinaryExpression(op, left, right, _) =>
          op match {
            case BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr =>
              compileExpression(left, instructions, constants)
              instructions += Instruction.dup()
              val jumpIdx = instructions.length
              val jumpBytePos = currentBytecodePos(instructions)
              if op == BinaryOperator.LogicalAnd then
                instructions += Instruction.ifFalse(0)
              else instructions += Instruction.ifTrue(0)
              instructions += Instruction.drop()
              compileExpression(right, instructions, constants)
              val endPos = currentBytecodePos(instructions)
              val offset = endPos - jumpBytePos - 1
              if op == BinaryOperator.LogicalAnd then
                instructions(jumpIdx) = Instruction.ifFalse(offset)
              else instructions(jumpIdx) = Instruction.ifTrue(offset)
            case BinaryOperator.NullishCoalesce =>
              // a ?? b - returns a if a is not null/undefined, otherwise b
              compileExpression(left, instructions, constants)
              instructions += Instruction.dup() // [a, a]
              instructions += Instruction.pushNull() // [a, a, null]
              instructions += Instruction.binary(
                BinaryOpcode.StrictEq
              ) // [a, a===null]
              val jumpIfNullIdx = instructions.length
              val jumpIfNullPos = currentBytecodePos(instructions)
              instructions += Instruction.ifTrue(0) // placeholder

              instructions += Instruction.dup() // [a, a]
              instructions += Instruction.pushUndefined() // [a, a, undefined]
              instructions += Instruction.binary(
                BinaryOpcode.StrictEq
              ) // [a, a===undefined]
              val jumpIfUndefIdx = instructions.length
              val jumpIfUndefPos = currentBytecodePos(instructions)
              instructions += Instruction.ifTrue(0) // placeholder

              // a is not null/undefined - keep a as result, skip b
              val jumpToEndIdx = instructions.length
              val jumpToEndPos = currentBytecodePos(instructions)
              instructions += Instruction.goto(0) // placeholder

              // a is null/undefined - evaluate b
              val evalBPos = currentBytecodePos(instructions)
              instructions += Instruction.drop() // remove a
              compileExpression(right, instructions, constants)

              val endPos = currentBytecodePos(instructions)

              // Fix up jumps
              instructions(jumpIfNullIdx) =
                Instruction.ifTrue(evalBPos - jumpIfNullPos - 1)
              instructions(jumpIfUndefIdx) =
                Instruction.ifTrue(evalBPos - jumpIfUndefPos - 1)
              instructions(jumpToEndIdx) =
                Instruction.goto(endPos - jumpToEndPos - 1)
            case _ =>
              compileExpression(left, instructions, constants)
              compileExpression(right, instructions, constants)
              instructions += Instruction.binary(binaryOpToOpcode(op))
          }

        case UnaryExpression(op, argument, _, _) =>
          // Increment/decrement operators need special handling for identifiers
          // Delete operator needs special handling for member expressions
          (op, argument) match {
            case (UnaryOperator.Typeof, Identifier(name, _)) =>
              if currentScope.isLocal(name) then
                compileExpression(argument, instructions, constants)
              else instructions += Instruction.getGlobalOrUndefined(name)
              instructions += Instruction.unary(UnaryOpcode.Typeof)

            case (UnaryOperator.Void, _) =>
              // void expr: evaluate expression, discard result, push undefined
              compileExpression(argument, instructions, constants)
              instructions += Instruction.drop()
              instructions += Instruction.pushUndefined()
            case (
                  UnaryOperator.PreInc | UnaryOperator.PostInc |
                  UnaryOperator.PreDec | UnaryOperator.PostDec,
                  id: Identifier
                ) =>
              // Increment/decrement on identifier - use helper that handles locals, globals, and closures
              compileIncrementDecrement(op, id, instructions, constants)

            case (
                  UnaryOperator.PreInc | UnaryOperator.PostInc |
                  UnaryOperator.PreDec | UnaryOperator.PostDec,
                  memberExpr: MemberExpression
                ) =>
              compileMemberIncDec(op, memberExpr, instructions, constants)

            case (UnaryOperator.Delete, Identifier(name, _)) =>
              if currentScope.isLocal(name) then
                instructions += Instruction.pushFalse()
              else {
                instructions += Instruction.getGlobal("globalThis")
                val constIndex = constants.length
                constants += JSValue.fromString(name)
                instructions += Instruction.getConst(constIndex)
                instructions += Instruction.unary(UnaryOpcode.Delete)
              }

            case (UnaryOperator.Delete, memberExpr: MemberExpression) =>
              val shortCircuitJumps =
                mutable.ArrayBuffer.empty[(Int, Int)]

              def emitOptionalAwareObject(expr: Expression): Unit =
                expr match {
                  case MemberExpression(obj, prop, computed, _, optional) =>
                    emitOptionalAwareObject(obj)
                    if optional then {
                      instructions += Instruction.dup()
                      instructions += Instruction.pushNull()
                      instructions += Instruction.binary(BinaryOpcode.StrictEq)
                      val jumpIfNullIdx = instructions.length
                      val jumpIfNullPos = currentBytecodePos(instructions)
                      instructions += Instruction.ifTrue(0)
                      shortCircuitJumps += ((jumpIfNullIdx, jumpIfNullPos))

                      instructions += Instruction.dup()
                      instructions += Instruction.pushUndefined()
                      instructions += Instruction.binary(BinaryOpcode.StrictEq)
                      val jumpIfUndefIdx = instructions.length
                      val jumpIfUndefPos = currentBytecodePos(instructions)
                      instructions += Instruction.ifTrue(0)
                      shortCircuitJumps += ((jumpIfUndefIdx, jumpIfUndefPos))
                    }

                    if computed then {
                      compileExpression(prop, instructions, constants)
                      instructions += Instruction.getElem()
                    } else
                      prop match {
                        case PrivateIdentifier(name, _) =>
                          instructions += Instruction.getPrivateField(privateOpcodeName(name))
                        case Identifier(name, _) =>
                          instructions += Instruction.getProp(name)
                        case _ =>
                          throw new UnsupportedOperationException(
                            s"Unsupported property key: $prop"
                          )
                      }
                  case _ =>
                    compileExpression(expr, instructions, constants)
              }

              memberExpr.`object` match {
                case Identifier(name, _) if name == "super" =>
                  instructions += Instruction.getGlobal("ReferenceError")
                  val msgIndex = constants.length
                  constants += JSValue.fromString("super is not defined")
                  instructions += Instruction.getConst(msgIndex)
                  instructions += Instruction.call(1)
                  instructions += Instruction.throwInst()
                case obj =>
                  emitOptionalAwareObject(obj)
                  if memberExpr.computed then
                    compileExpression(memberExpr.property, instructions, constants)
                  else
                    memberExpr.property match {
                      case Identifier(propName, _) =>
                        val constIndex = constants.length
                        constants += JSValue.fromString(propName)
                        instructions += Instruction.getConst(constIndex)
                      case _ =>
                        throw new UnsupportedOperationException(
                          s"Unsupported property key: ${memberExpr.property}"
                        )
                    }
                  instructions += Instruction.unary(UnaryOpcode.Delete)

                  if shortCircuitJumps.nonEmpty then {
                    val jumpToEndIdx = instructions.length
                    val jumpToEndPos = currentBytecodePos(instructions)
                    instructions += Instruction.goto(0)

                    val shortCircuitPos = currentBytecodePos(instructions)
                    instructions += Instruction.drop()
                    instructions += Instruction.pushTrue()

                    val endPos = currentBytecodePos(instructions)
                    instructions(jumpToEndIdx) =
                      Instruction.goto(endPos - jumpToEndPos - 1)
                    for (jumpIdx, jumpPos) <- shortCircuitJumps do
                      instructions(jumpIdx) =
                        Instruction.ifTrue(shortCircuitPos - jumpPos - 1)
                  }
              }

            case _ =>
              // For other unary operators, use the standard path
              compileExpression(argument, instructions, constants)
              instructions += Instruction.unary(unaryOpToOpcode(op))
          }

        case CallExpression(callee, arguments, _, optional) =>
          val hasSpread = hasSpreadArguments(arguments)
          val isDirectEval = callee match {
              case Identifier("eval", _) if !currentScope.isLocal("eval") => true
              case _ => false
            }
          val isDirectFieldEval =
            classFieldEvalContextDepth > 0 && isDirectEval
          val isDirectPrivateEval =
            isDirectEval && !isDirectFieldEval &&
              currentClassPrivateBindings.nonEmpty
          val directEvalHelper =
            if isDirectFieldEval then "__directEvalField"
            else if isDirectPrivateEval then "__directEvalPrivate"
            else "__directEval"
          // Check if this is a method call (callee is a MemberExpression)
          callee match {
            case SuperExpression(_) =>
              if currentSuperClass == null then {
                instructions += Instruction.getGlobal("ReferenceError")
                val msgIndex = constants.length
                constants += JSValue.fromString("super is not defined")
                instructions += Instruction.getConst(msgIndex)
                instructions += Instruction.call(1)
                instructions += Instruction.throwInst()
              } else {
                // A `super()` call may read the receiver before `this` is
                // initialized, then marks it initialized afterwards.
                instructions += Instruction.getThisUnchecked()
                // Use the captured superclass variable if available
                currentSuperVarName match {
                  case Some(varName) =>
                    instructions += Instruction.getGlobal(varName)
                  case None =>
                    compileExpression(
                      currentSuperClass,
                      instructions,
                      constants
                    )
                }
                if hasSpread then {
                  val thisIndex = allocateTempLocal("__superThis")
                  val funcIndex = allocateTempLocal("__superFunc")
                  instructions += Instruction.putLoc(funcIndex)
                  instructions += Instruction.putLoc(thisIndex)
                  instructions += Instruction.getGlobal("__funcSpread")
                  instructions += Instruction.getLoc(funcIndex)
                  instructions += Instruction.getLoc(thisIndex)
                  emitArgumentArray(arguments, instructions, constants)
                  // The parent constructor must see an initialized `this`.
                  instructions += Instruction.markThisInitialized()
                  instructions += Instruction.call(3)
                } else {
                  // Non-spread `super(...)` also routes through
                  // `__funcSpread`, which calls the superclass with the
                  // existing receiver (`superInitImpl` for native classes).
                  // A plain method call must not use that path — calling a
                  // native constructor as an ordinary method ignores `this`.
                  val thisIndex = allocateTempLocal("__superThis")
                  val funcIndex = allocateTempLocal("__superFunc")
                  instructions += Instruction.putLoc(funcIndex)
                  instructions += Instruction.putLoc(thisIndex)
                  instructions += Instruction.getGlobal("__funcSpread")
                  instructions += Instruction.getLoc(funcIndex)
                  instructions += Instruction.getLoc(thisIndex)
                  if arguments.isEmpty then instructions += Instruction.newArray(0)
                  else emitArgumentArray(arguments, instructions, constants)
                  instructions += Instruction.markThisInitialized()
                  instructions += Instruction.call(3)
                }
                if pendingDerivedFieldInits.nonEmpty then {
                  val inits = pendingDerivedFieldInits
                  pendingDerivedFieldInits = Nil
                  inits.foreach(stmt =>
                    compileStatement(stmt, instructions, constants, false)
                  )
                }
              }
            case MemberExpression(SuperExpression(_), prop, computed, _, _) =>
              if currentSuperClass == null then {
                instructions += Instruction.getGlobal("ReferenceError")
                val msgIndex = constants.length
                constants += JSValue.fromString("super is not defined")
                instructions += Instruction.getConst(msgIndex)
                instructions += Instruction.call(1)
                instructions += Instruction.throwInst()
              } else {
                instructions += Instruction.getThis()
                // Use the captured superclass variable if available
                currentSuperVarName match {
                  case Some(varName) =>
                    instructions += Instruction.getGlobal(varName)
                  case None =>
                    compileExpression(
                      currentSuperClass,
                      instructions,
                      constants
                    )
                }
                if !currentSuperIsStatic then
                  instructions += Instruction.getProp("prototype")
                if computed then {
                  compileExpression(prop, instructions, constants)
                  instructions += Instruction.getElem()
                } else {
                  val propName = prop match {
                    case Identifier(name, _) => name
                    case _                   =>
                      throw new UnsupportedOperationException(
                        s"Unsupported property key: $prop"
                      )
                  }
                  instructions += Instruction.getProp(propName)
                }
                if hasSpread then {
                  val thisIndex = allocateTempLocal("__superThis")
                  val funcIndex = allocateTempLocal("__superFunc")
                  instructions += Instruction.putLoc(funcIndex)
                  instructions += Instruction.putLoc(thisIndex)
                  instructions += Instruction.getGlobal("__funcSpread")
                  instructions += Instruction.getLoc(funcIndex)
                  instructions += Instruction.getLoc(thisIndex)
                  emitArgumentArray(arguments, instructions, constants)
                  instructions += Instruction.call(3)
                } else {
                  for arg <- arguments do
                    compileExpression(arg, instructions, constants)
                  instructions += Instruction.callMethod(arguments.length)
                }
              }
            case memberExpr: MemberExpression =>
              // Method call: obj.method(arg1, arg2, ...)
              // Stack layout should be: [this, func, arg1, arg2, ..., argN]

              if hasSpread then {
                instructions += Instruction.getGlobal("__callSpread")
                compileExpression(memberExpr.`object`, instructions, constants)
                instructions += Instruction.dup()
                if memberExpr.computed then {
                  compileExpression(memberExpr.property, instructions, constants)
                  instructions += Instruction.getElem()
                } else {
                  memberExpr.property match {
                    case Identifier(name, _) =>
                      instructions += Instruction.getProp(name)
                    case PrivateIdentifier(name, _) =>
                      instructions += Instruction.getPrivateField(privateOpcodeName(name))
                    case _ =>
                      throw new UnsupportedOperationException(
                        s"Unsupported property key: ${memberExpr.property}"
                      )
                  }
                }
                instructions += Instruction.swap()
                emitArgumentArray(arguments, instructions, constants)
                instructions += Instruction.pushTrue()
                instructions += Instruction.call(4)
              } else {
                // Compile the object part (for 'this' binding)
                compileExpression(memberExpr.`object`, instructions, constants)
                // Stack now: [obj]

                // `obj?.x()` / `obj?.x?.()`: the optional member
                // short-circuits before the property lookup and the call.
                var memberJumpNullIdx = -1
                var memberJumpNullPos = 0
                var memberJumpUndefIdx = -1
                var memberJumpUndefPos = 0
                if memberExpr.optional then {
                  instructions += Instruction.dup()
                  instructions += Instruction.pushNull()
                  instructions += Instruction.binary(BinaryOpcode.StrictEq)
                  memberJumpNullIdx = instructions.length
                  memberJumpNullPos = currentBytecodePos(instructions)
                  instructions += Instruction.ifTrue(0)

                  instructions += Instruction.dup()
                  instructions += Instruction.pushUndefined()
                  instructions += Instruction.binary(BinaryOpcode.StrictEq)
                  memberJumpUndefIdx = instructions.length
                  memberJumpUndefPos = currentBytecodePos(instructions)
                  instructions += Instruction.ifTrue(0)
                }

                // Preserve the already evaluated receiver for CallMethod and
                // perform property lookup on a duplicate. Recompiling the
                // MemberExpression would evaluate its object twice.
                instructions += Instruction.dup()
                if memberExpr.computed then {
                  compileExpression(memberExpr.property, instructions, constants)
                  instructions += Instruction.getElem()
                } else {
                  memberExpr.property match {
                    case Identifier(name, _) =>
                      instructions += Instruction.getProp(name)
                    case PrivateIdentifier(name, _) =>
                      instructions += Instruction.getPrivateField(privateOpcodeName(name))
                    case _ =>
                      throw new UnsupportedOperationException(
                        s"Unsupported property key: ${memberExpr.property}"
                      )
                  }
                }
                // Stack now: [obj, method]

                // `obj.x?.()` short-circuits when the method value is
                // null/undefined, without evaluating the arguments.
                var jumpNullIdx = -1
                var jumpNullPos = 0
                var jumpUndefIdx = -1
                var jumpUndefPos = 0
                var jumpEndIdx = -1
                var jumpEndPos = 0
                if optional then {
                  instructions += Instruction.dup()
                  instructions += Instruction.pushNull()
                  instructions += Instruction.binary(BinaryOpcode.StrictEq)
                  jumpNullIdx = instructions.length
                  jumpNullPos = currentBytecodePos(instructions)
                  instructions += Instruction.ifTrue(0)

                  instructions += Instruction.dup()
                  instructions += Instruction.pushUndefined()
                  instructions += Instruction.binary(BinaryOpcode.StrictEq)
                  jumpUndefIdx = instructions.length
                  jumpUndefPos = currentBytecodePos(instructions)
                  instructions += Instruction.ifTrue(0)
                }

                // Compile arguments
                for arg <- arguments do
                  compileExpression(arg, instructions, constants)
                // Stack now: [obj, method, arg1, arg2, ..., argN]

                // Emit CallMethod instruction
                instructions += Instruction.callMethod(arguments.length)

                if optional || memberExpr.optional then {
                  jumpEndIdx = instructions.length
                  jumpEndPos = currentBytecodePos(instructions)
                  instructions += Instruction.goto(0)

                  if optional then {
                    // Nullish method: drop [obj, method], yield undefined.
                    val nullPathPos = currentBytecodePos(instructions)
                    instructions += Instruction.drop()
                    instructions += Instruction.drop()
                    instructions += Instruction.pushUndefined()
                    instructions(jumpNullIdx) =
                      Instruction.ifTrue(nullPathPos - jumpNullPos - 1)
                    instructions(jumpUndefIdx) =
                      Instruction.ifTrue(nullPathPos - jumpUndefPos - 1)
                  }

                  if memberExpr.optional then {
                    // Nullish receiver: drop [obj], yield undefined.
                    val memberNullPathPos = currentBytecodePos(instructions)
                    instructions += Instruction.drop()
                    instructions += Instruction.pushUndefined()
                    instructions(memberJumpNullIdx) =
                      Instruction.ifTrue(memberNullPathPos - memberJumpNullPos - 1)
                    instructions(memberJumpUndefIdx) =
                      Instruction.ifTrue(memberNullPathPos - memberJumpUndefPos - 1)
                  }

                  val endPos = currentBytecodePos(instructions)
                  instructions(jumpEndIdx) =
                    Instruction.goto(endPos - jumpEndPos - 1)
                }
              }

            case _ =>
              // Regular function call: func(arg1, arg2, ...)
              // For optional calls, check if callee is null/undefined first
              if optional then {
                if hasSpread then {
                  val funcIndex = allocateTempLocal("__optionalCallFunc")
                  compileExpression(callee, instructions, constants)
                  instructions += Instruction.dup()
                  instructions += Instruction.putLoc(funcIndex)
                  instructions += Instruction.pushNull()
                  instructions += Instruction.binary(BinaryOpcode.StrictEq)
                  val jumpIfNullIdx = instructions.length
                  val jumpIfNullPos = currentBytecodePos(instructions)
                  instructions += Instruction.ifTrue(0)

                  instructions += Instruction.getLoc(funcIndex)
                  instructions += Instruction.pushUndefined()
                  instructions += Instruction.binary(BinaryOpcode.StrictEq)
                  val jumpIfUndefIdx = instructions.length
                  val jumpIfUndefPos = currentBytecodePos(instructions)
                  instructions += Instruction.ifTrue(0)

                  instructions += Instruction.getGlobal("__callSpread")
                  instructions += Instruction.getLoc(funcIndex)
                  instructions += Instruction.pushUndefined()
                  emitArgumentArray(arguments, instructions, constants)
                  instructions += Instruction.pushFalse()
                  instructions += Instruction.call(4)

                  val jumpToEndIdx = instructions.length
                  val jumpToEndPos = currentBytecodePos(instructions)
                  instructions += Instruction.goto(0)

                  val nullPathPos = currentBytecodePos(instructions)
                  instructions += Instruction.pushUndefined()

                  val endPos = currentBytecodePos(instructions)
                  instructions(jumpIfNullIdx) =
                    Instruction.ifTrue(nullPathPos - jumpIfNullPos - 1)
                  instructions(jumpIfUndefIdx) =
                    Instruction.ifTrue(nullPathPos - jumpIfUndefPos - 1)
                  instructions(jumpToEndIdx) =
                    Instruction.goto(endPos - jumpToEndPos - 1)
                } else {
                  // Compile callee
                  compileExpression(callee, instructions, constants)
                  // Check for null/undefined
                  instructions += Instruction.dup() // [func, func]
                  instructions += Instruction.pushNull()
                  instructions += Instruction.binary(BinaryOpcode.StrictEq)
                  val jumpIfNullIdx = instructions.length
                  val jumpIfNullPos = currentBytecodePos(instructions)
                  instructions += Instruction.ifTrue(0) // placeholder

                  instructions += Instruction.dup()
                  instructions += Instruction.pushUndefined()
                  instructions += Instruction.binary(BinaryOpcode.StrictEq)
                  val jumpIfUndefIdx = instructions.length
                  val jumpIfUndefPos = currentBytecodePos(instructions)
                  instructions += Instruction.ifTrue(0) // placeholder

                  // Not null/undefined - call the function
                  for arg <- arguments do
                    compileExpression(arg, instructions, constants)
                  instructions += Instruction.call(arguments.length)

                  val jumpToEndIdx = instructions.length
                  val jumpToEndPos = currentBytecodePos(instructions)
                  instructions += Instruction.goto(0) // placeholder

                  // Null/undefined path - replace func with undefined
                  val nullPathPos = currentBytecodePos(instructions)
                  instructions += Instruction.drop() // remove func
                  instructions += Instruction.pushUndefined()

                  val endPos = currentBytecodePos(instructions)

                  // Fix up jumps
                  instructions(jumpIfNullIdx) =
                    Instruction.ifTrue(nullPathPos - jumpIfNullPos - 1)
                  instructions(jumpIfUndefIdx) =
                    Instruction.ifTrue(nullPathPos - jumpIfUndefPos - 1)
                  instructions(jumpToEndIdx) =
                    Instruction.goto(endPos - jumpToEndPos - 1)
                }
              } else {
                if hasSpread then {
                  instructions += Instruction.getGlobal("__callSpread")
                  callee match {
                    case Identifier("eval", _) if !currentScope.isLocal("eval") =>
                      instructions += Instruction.getGlobal(directEvalHelper)
                    case _ =>
                      compileExpression(callee, instructions, constants)
                  }
                  instructions += Instruction.pushUndefined()
                  emitArgumentArray(arguments, instructions, constants)
                  instructions += Instruction.pushFalse()
                  instructions += Instruction.call(4)
                } else {
                  // Stack layout: [func, arg1, arg2, ..., argN]
                  callee match {
                    case Identifier("eval", _) if !currentScope.isLocal("eval") =>
                      instructions += Instruction.getGlobal(directEvalHelper)
                    case _ =>
                      compileExpression(callee, instructions, constants)
                  }
                  for arg <- arguments do
                    compileExpression(arg, instructions, constants)

                  if isDirectFieldEval || isDirectPrivateEval then
                    pushStringConst(
                      currentClassPrivateNames.toSeq.sorted.map { name =>
                        currentClassPrivateBindings.get(name) match {
                          case Some(binding) => s"$name\u001f$binding"
                          case None          => name
                        }
                      }.mkString("\u0000"),
                      instructions,
                      constants
                    )

                  // Emit Call instruction with argument count
                  instructions += Instruction.call(
                    arguments.length +
                      (if isDirectFieldEval || isDirectPrivateEval then 1 else 0)
                  )
                }
              }
          }

        case TaggedTemplateExpression(tag, template, _) =>
          val argCount = template.expressions.length + 1
          tag match {
            case memberExpr: MemberExpression =>
              compileExpression(memberExpr.`object`, instructions, constants)
              compileExpression(memberExpr, instructions, constants)
              emitTemplateObject(template, instructions, constants)
              template.expressions.foreach(expr =>
                compileExpression(expr, instructions, constants)
              )
              instructions += Instruction.callMethod(argCount)
            case _ =>
              compileExpression(tag, instructions, constants)
              emitTemplateObject(template, instructions, constants)
              template.expressions.foreach(expr =>
                compileExpression(expr, instructions, constants)
              )
              instructions += Instruction.call(argCount)
          }

        case NewExpression(callee, arguments, _) =>
          val hasSpread = hasSpreadArguments(arguments)
          // new Constructor(arg1, arg2, ...)
          // Stack layout: [constructor, arg1, arg2, ..., argN]

          if hasSpread then {
            instructions += Instruction.getGlobal("Reflect")
            instructions += Instruction.dup()
            instructions += Instruction.getProp("construct")
            compileExpression(callee, instructions, constants)
            emitArgumentArray(arguments, instructions, constants)
            instructions += Instruction.callMethod(2)
          } else {
            // Compile the constructor
            compileExpression(callee, instructions, constants)

            // Compile arguments
            for arg <- arguments do
              compileExpression(arg, instructions, constants)

            // Emit New instruction with argument count
            instructions += Instruction.newInst(arguments.length)
          }

        case FunctionExpression(
              id,
              params,
              body,
              isGenerator,
              isAsync,
              strict,
              _
            ) =>
          // Compile function expression to bytecode
          val funcName = id match {
            case Identifier(name, _) => name
            case null                => "<anonymous>"
          }

          val funcBytecode = withoutClassFieldEvalContext {
            compileFunctionBody(
              funcName,
              params,
              body,
              isConstructor = !isGenerator && !isAsync,
              isGenerator = isGenerator,
              isAsync = isAsync,
              isStrict = strict,
              functionExpressionName =
                if id != null then Some(funcName) else None
            )
          }

          // Store BytecodeFunction in constants array (will be converted to JSValue.Function at runtime with closure)
          val constIndex = constants.length
          constants += funcBytecode

          // Push the function value onto the stack
          instructions += Instruction.getConst(constIndex)

        case ArrowFunctionExpression(params, body, isAsync, strict, _) =>
          // Arrow functions are always anonymous
          val funcBytecode = compileArrowFunctionBody(
            params,
            body,
            isAsync = isAsync,
            isStrict = strict
          )

          // Store BytecodeFunction in constants array
          val constIndex = constants.length
          constants += funcBytecode

          // Push the function value onto the stack
          instructions += Instruction.getConst(constIndex)

        case AssignmentExpression(
              left,
              right @ BinaryExpression(op, l, rhs, _),
              _
            ) if withScopeDepth > 0 && (l eq left) &&
              left.isInstanceOf[Identifier] =>
          // Compound assignment inside `with`: the put must use the object
          // environment record captured by the get, even if the binding is
          // deleted while evaluating the right side.
          val name = left.asInstanceOf[Identifier].name
          val baseIndex = allocateTempLocal("__withBase")
          instructions += Instruction.getGlobalWithBase(name)
          instructions += Instruction.putLoc(baseIndex)
          compileExpression(rhs, instructions, constants)
          instructions += Instruction.binary(binaryOpToOpcode(op))
          instructions += Instruction.dup()
          instructions += Instruction.getLoc(baseIndex)
          instructions += Instruction.putGlobalWithBase(name)

        case AssignmentExpression(left, right, _) =>
          // Compile the right side first
          compileExpression(right, instructions, constants)

          // For assignment to identifier, store it
          // Assignment returns the value, so we need to keep it on the stack
          left match {
            case Identifier(name, _) =>
              // Check if this is a const variable (compile-time check for local const)
              if withScopeDepth == 0 && currentScope.isLocal(name) && currentScope.isConst(name) then
                // Compile-time error for const reassignment
                throw new Exception(s"Cannot assign to const variable '$name'")

              // Check if variable is in current immediate scope (not parent scopes)
              if withScopeDepth == 0 && currentScope.isLocal(name) then {
                // Variable is local to this function/script - use PutLoc
                val index = currentScope
                  .lookup(name)
                  .get // Safe because isLocal returned true
                // Duplicate the value so we can keep one on stack and store one
                instructions += Instruction.dup()
                instructions += Instruction.putLoc(index)
              } else {
                // Variable is in parent scope (closure) or global - use PutGlobal
                // PutGlobal will check the closure at runtime
                instructions += Instruction.dup()
                instructions += Instruction.putGlobal(name)
              }
            case pattern: BindingPattern =>
              // Destructuring assignment: keep a copy of RHS as the expression result
              instructions += Instruction.dup()
              emitDestructuring(
                pattern,
                isDeclaration = false,
                isGlobalVar = false,
                instructions,
                constants
              )
            case pattern @ (_: ObjectLiteral | _: ArrayLiteral) =>
              // Assignment patterns are represented as literals by the parser
              // when their targets are member expressions rather than bindings.
              instructions += Instruction.dup()
              emitForInAssignment(pattern, instructions, constants)
            case MemberExpression(obj, prop, computed, _, _) =>
              if computed then {
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
              } else {
                // For member assignment: obj.prop = value or obj.#field = value
                // Stack layout: [value, obj] (value is already on stack from right side)
                compileExpression(obj, instructions, constants)
                // Now stack is: [value, obj]
                // Need to swap to get: [obj, value]
                instructions += Instruction.swap()
                // Get property name and set property/private field
                prop match {
                  case PrivateIdentifier(name, _) =>
                    // Private field assignment
                    if right.isInstanceOf[ClassFieldInitializerExpression] then
                      instructions += Instruction.definePrivateField(
                        privateOpcodeName(name)
                      )
                    else
                      instructions += Instruction.setPrivateField(
                        privateOpcodeName(name)
                      )
                  case Identifier(name, _) =>
                    // Regular property assignment
                    instructions += Instruction.setProp(name)
                  case _ =>
                    throw new UnsupportedOperationException(
                      s"Unsupported property key: $prop"
                    )
                }
              }
            case _ =>
              throw new UnsupportedOperationException(
                s"Unsupported assignment target: $left"
              )
          }

        case LogicalAssignmentExpression(operator, left, right, _) =>
          emitLogicalAssignment(operator, left, right, instructions, constants)

        case ObjectLiteral(properties, _) =>
          // `{ a = 1 }` (CoverInitializedName) is only valid in a
          // destructuring pattern. Pattern consumers intercept it before
          // reaching object-literal expression compilation.
          properties.foreach {
            case Property(_, _: AssignmentExpression, _, _, _, true) =>
              throw new RuntimeException(
                "SyntaxError: shorthand property with initializer is only valid in a destructuring pattern"
              )
            case _ => ()
          }

          def emitPropertyKey(key: Identifier | String | Expression): Unit =
            key match {
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
            }

          instructions += Instruction.newObject()
          val objIndex = allocateTempLocal("__objLit")
          instructions += Instruction.putLoc(objIndex)

          for propOrSpread <- properties do
            propOrSpread match {
              case SpreadElement(argument, _) =>
                // Spread: copy all properties from argument to target
                instructions += Instruction.getGlobal("__objectSpread")
                instructions += Instruction.getLoc(objIndex)
                compileExpression(argument, instructions, constants)
                instructions += Instruction.call(2)
                instructions += Instruction.drop()

              case prop: Property =>
                val isComputed = prop.computed
                prop.kind match {
                  case PropertyKind.Getter | PropertyKind.Setter =>
                    val descIndex = allocateTempLocal("__objDesc")
                    instructions += Instruction.newObject()
                    instructions += Instruction.putLoc(descIndex)
                    instructions += Instruction.getLoc(descIndex)
                    compileExpression(prop.value, instructions, constants)
                    if prop.kind == PropertyKind.Getter then
                      instructions += Instruction.setProp("get")
                    else instructions += Instruction.setProp("set")
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
                    if isComputed then
                      compileExpression(
                        prop.key.asInstanceOf[Expression],
                        instructions,
                        constants
                      )
                    else emitPropertyKey(prop.key)
                    instructions += Instruction.getLoc(descIndex)
                    instructions += Instruction.call(3)
                    instructions += Instruction.drop()

                  case _ =>
                    if isComputed then {
                      // Computed property name: [expr]
                      instructions += Instruction.getLoc(objIndex)
                      compileExpression(
                        prop.key.asInstanceOf[Expression],
                        instructions,
                        constants
                      )
                      compileExpression(prop.value, instructions, constants)
                      instructions += Instruction.setElem()
                      instructions += Instruction.drop()
                    } else
                      prop.key match {
                        case Identifier(name, _) if name == "__proto__" =>
                          // __proto__: value sets the [[Prototype]] of the new object
                          instructions += Instruction.getGlobal("Object")
                          instructions += Instruction.getProp("setPrototypeOf")
                          instructions += Instruction.getLoc(objIndex)
                          compileExpression(prop.value, instructions, constants)
                          instructions += Instruction.call(2)
                          instructions += Instruction.drop()
                        case Identifier(name, _) =>
                          instructions += Instruction.getLoc(objIndex)
                          compileExpression(prop.value, instructions, constants)
                          instructions += Instruction.setProp(name)
                          instructions += Instruction.drop()
                        case s: String if s == "__proto__" =>
                          // __proto__: value sets the [[Prototype]] of the new object
                          instructions += Instruction.getGlobal("Object")
                          instructions += Instruction.getProp("setPrototypeOf")
                          instructions += Instruction.getLoc(objIndex)
                          compileExpression(prop.value, instructions, constants)
                          instructions += Instruction.call(2)
                          instructions += Instruction.drop()
                        case s: String =>
                          instructions += Instruction.getLoc(objIndex)
                          compileExpression(prop.value, instructions, constants)
                          instructions += Instruction.setProp(s)
                          instructions += Instruction.drop()
                      }
                }
            }

          instructions += Instruction.getLoc(objIndex)

        case ArrayLiteral(elements, _, _) =>
          val hasSpread = elements.exists {
            case SpreadElement(_, _) => true
            case _                   => false
          }

          if !hasSpread then {
            // Create a new array with the given size
            instructions += Instruction.newArray(elements.length)

            // Initialize each element
            for (elem, index) <- elements.zipWithIndex do
              elem match {
                case null =>
                  // Elision (empty slot) - skip initialization, array elements are undefined by default
                  ()
                case expr: Expression =>
                  // Push the index
                  instructions += Instruction.pushI32(index) // [array, index]

                  // Compile element expression
                  // Stack: [array, index, value]
                  compileExpression(expr, instructions, constants)

                  // Initialize element
                  // Note: InitElem pops [array, index, value] and pushes [array] back
                  instructions += Instruction.initElem()
              }
          } else {
            // Build array dynamically to support spread elements
            instructions += Instruction.newArray(0)
            for elem <- elements do
              elem match {
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
              }
          }

        case MemberExpression(SuperExpression(_), prop, computed, _, _)
            if currentSuperClass != null =>
          // `super.prop`: look up on the home object's prototype but invoke
          // accessors with `this` as the receiver.
          instructions += Instruction.getGlobal("__getSuperProp")
          currentSuperVarName match {
            case Some(varName) =>
              instructions += Instruction.getGlobal(varName)
            case None =>
              compileExpression(currentSuperClass, instructions, constants)
          }
          if !currentSuperIsStatic then
            instructions += Instruction.getProp("prototype")
          if computed then
            compileExpression(prop, instructions, constants)
          else
            prop match {
              case Identifier(name, _) =>
                pushStringConst(name, instructions, constants)
              case _ =>
                throw new UnsupportedOperationException(
                  s"Unsupported property key: $prop"
                )
            }
          instructions += Instruction.getThis()
          instructions += Instruction.call(3)

        case MemberExpression(obj, prop, computed, _, optional) =>
          // Compile the object
          compileExpression(obj, instructions, constants)

          if optional then {
            // Optional chaining: obj?.prop or obj?.[expr]
            // If obj is null or undefined, return undefined without accessing property
            // Stack: [obj]
            instructions += Instruction.dup() // [obj, obj]
            instructions += Instruction.pushNull() // [obj, obj, null]
            instructions += Instruction.binary(
              BinaryOpcode.StrictEq
            ) // [obj, obj === null]
            val jumpIfNullIdx = instructions.length
            val jumpIfNullPos = currentBytecodePos(instructions)
            instructions += Instruction.ifTrue(0) // placeholder

            instructions += Instruction.dup() // [obj, obj]
            instructions += Instruction.pushUndefined() // [obj, obj, undefined]
            instructions += Instruction.binary(
              BinaryOpcode.StrictEq
            ) // [obj, obj === undefined]
            val jumpIfUndefIdx = instructions.length
            val jumpIfUndefPos = currentBytecodePos(instructions)
            instructions += Instruction.ifTrue(0) // placeholder

            // Not null/undefined - do normal property access
            if computed then {
              compileExpression(prop, instructions, constants)
              instructions += Instruction.getElem()
            } else
              prop match {
                case PrivateIdentifier(name, _) =>
                  // Private field access
                  instructions += Instruction.getPrivateField(privateOpcodeName(name))
                case Identifier(name, _) =>
                  instructions += Instruction.getProp(name)
                case _ =>
                  throw new UnsupportedOperationException(
                    s"Unsupported property key: $prop"
                  )
              }

            // Jump over the undefined result
            val jumpToEndIdx = instructions.length
            val jumpToEndPos = currentBytecodePos(instructions)
            instructions += Instruction.goto(0) // placeholder

            // Null/undefined path - replace obj with undefined
            val nullPathPos = currentBytecodePos(instructions)
            instructions += Instruction.drop() // remove obj
            instructions += Instruction
              .pushUndefined() // push undefined as result

            val endPos = currentBytecodePos(instructions)

            // Fix up jumps
            instructions(jumpIfNullIdx) =
              Instruction.ifTrue(nullPathPos - jumpIfNullPos - 1)
            instructions(jumpIfUndefIdx) =
              Instruction.ifTrue(nullPathPos - jumpIfUndefPos - 1)
            instructions(jumpToEndIdx) =
              Instruction.goto(endPos - jumpToEndPos - 1)
          } else
            // Regular member expression
            if computed then {
              // Computed property access: obj[prop]
              // Compile the property expression
              compileExpression(prop, instructions, constants)
              // Get element with computed index
              instructions += Instruction.getElem()
            } else
              // Regular property access: obj.prop or obj.#field
              prop match {
                case PrivateIdentifier(name, _) =>
                  // Private field access
                  instructions += Instruction.getPrivateField(privateOpcodeName(name))
                case Identifier(name, _) =>
                  // Regular property access
                  instructions += Instruction.getProp(name)
                case _ =>
                  throw new UnsupportedOperationException(
                    s"Unsupported property key: $prop"
                  )
              }

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
          instructions += Instruction.ifFalse(0) // placeholder offset

          // Compile consequent (true branch)
          compileExpression(consequent, instructions, constants)

          // Jump over the alternate branch
          val jumpInstIndex = instructions.length
          val jumpBytecodePos = getBytecodePos()
          instructions += Instruction.goto(0) // placeholder offset

          // Fix up the ifFalse offset to point to here (after the consequent and goto)
          // Offset is from the start of the ifFalse instruction (not including opcode)
          val afterConsequentPos = getBytecodePos()
          instructions(jumpIfFalseInstIndex) =
            Instruction.ifFalse(afterConsequentPos - jumpIfFalseBytecodePos - 1)

          // Compile alternate (false branch)
          compileExpression(alternate, instructions, constants)

          // Fix up the jump offset to skip the alternate
          // Offset is from the start of the goto instruction (not including opcode)
          val afterAlternatePos = getBytecodePos()
          instructions(jumpInstIndex) =
            Instruction.goto(afterAlternatePos - jumpBytecodePos - 1)

        case YieldExpression(argument, delegate, _) =>
          // Compile the argument expression (if any)
          if argument != null then
            compileExpression(argument, instructions, constants)
          else instructions += Instruction.pushUndefined()

          // Emit the appropriate yield opcode
          if delegate then instructions += Instruction.yieldStar()
          else instructions += Instruction.yieldInst()

        case AwaitExpression(argument, _) =>
          // Compile the argument expression
          compileExpression(argument, instructions, constants)
          // Inside an async function awaits always suspend (microtask
          // ordering); top-level await keeps the synchronous unwrap.
          if currentFunctionIsAsync then
            instructions += Instruction.awaitAsyncInst()
          else instructions += Instruction.awaitInst()

        case _ =>
          throw new UnsupportedOperationException(
            s"Unsupported expression: $expr"
          )
      }
    }

  private def compileLiteral(
      value: JSValue,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = value match {
    case JSValue.Undefined => instructions += Instruction.pushUndefined()
    case JSValue.Null      => instructions += Instruction.pushNull()
    case JSValue.Bool(b)   =>
      if b then instructions += Instruction.pushTrue()
      else instructions += Instruction.pushFalse()
    case JSValue.Int32(i)   => instructions += Instruction.pushI32(i)
    case JSValue.Float64(d) => instructions += Instruction.pushFloat64(d)
    case JSValue.JSStr(s)   =>
      // Store string in constants and load with GetConst
      val constIndex = constants.length
      constants += value // Store the JSValue.JSStr directly
      instructions += Instruction.getConst(constIndex)
    case JSValue.BigInt(_) =>
      val constIndex = constants.length
      constants += value
      instructions += Instruction.getConst(constIndex)
    case _ =>
      throw new UnsupportedOperationException(s"Unsupported literal: $value")
  }

  private def binaryOpToOpcode(op: quickjs.ast.BinaryOperator): BinaryOpcode =
    op match {
      case BinaryOperator.Comma      => BinaryOpcode.Comma
      case BinaryOperator.Add        => BinaryOpcode.Add
      case BinaryOperator.Sub        => BinaryOpcode.Sub
      case BinaryOperator.Mul        => BinaryOpcode.Mul
      case BinaryOperator.Div        => BinaryOpcode.Div
      case BinaryOperator.Mod        => BinaryOpcode.Mod
      case BinaryOperator.Pow        => BinaryOpcode.Pow
      case BinaryOperator.Lt         => BinaryOpcode.Lt
      case BinaryOperator.Lte        => BinaryOpcode.Lte
      case BinaryOperator.Gt         => BinaryOpcode.Gt
      case BinaryOperator.Gte        => BinaryOpcode.Gte
      case BinaryOperator.Eq         => BinaryOpcode.Eq
      case BinaryOperator.Neq        => BinaryOpcode.Neq
      case BinaryOperator.StrictEq   => BinaryOpcode.StrictEq
      case BinaryOperator.StrictNeq  => BinaryOpcode.StrictNeq
      case BinaryOperator.And        => BinaryOpcode.And
      case BinaryOperator.Or         => BinaryOpcode.Or
      case BinaryOperator.Xor        => BinaryOpcode.Xor
      case BinaryOperator.Shl        => BinaryOpcode.Shl
      case BinaryOperator.Sar        => BinaryOpcode.Sar
      case BinaryOperator.Shr        => BinaryOpcode.Shr
      case BinaryOperator.LogicalAnd => BinaryOpcode.LogicalAnd
      case BinaryOperator.LogicalOr  => BinaryOpcode.LogicalOr
      case BinaryOperator.Instanceof => BinaryOpcode.Instanceof
      case BinaryOperator.In         => BinaryOpcode.In
    }

  private def unaryOpToOpcode(op: quickjs.ast.UnaryOperator): UnaryOpcode =
    (op: @unchecked) match {
      case UnaryOperator.Minus      => UnaryOpcode.Neg
      case UnaryOperator.Not        => UnaryOpcode.Not
      case UnaryOperator.BitwiseNot => UnaryOpcode.LNot
      case UnaryOperator.PreInc     => UnaryOpcode.PreInc
      case UnaryOperator.PostInc    => UnaryOpcode.PostInc
      case UnaryOperator.PreDec     => UnaryOpcode.PreDec
      case UnaryOperator.PostDec    => UnaryOpcode.PostDec
      case UnaryOperator.Typeof     => UnaryOpcode.Typeof
      case UnaryOperator.Delete     => UnaryOpcode.Delete
      case UnaryOperator.Plus       => UnaryOpcode.Pos
    }

  /** Helper to compile increment/decrement operations based on variable storage
    * type
    */
  private def compileIncrementDecrement(
      op: quickjs.ast.UnaryOperator,
      id: Identifier,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit =
    if withScopeDepth > 0 then {
      // Inside `with`, the binding is resolved dynamically; keep the object
      // environment record base for the put.
      val baseIndex = allocateTempLocal("__withBase")
      instructions += Instruction.getGlobalWithBase(id.name)
      instructions += Instruction.putLoc(baseIndex)
      op match {
        case UnaryOperator.PreInc =>
          instructions += Instruction.unary(UnaryOpcode.PreInc)
          instructions += Instruction.dup()
        case UnaryOperator.PostInc =>
          instructions += Instruction.unary(UnaryOpcode.PostInc)
        case UnaryOperator.PreDec =>
          instructions += Instruction.unary(UnaryOpcode.PreDec)
          instructions += Instruction.dup()
        case UnaryOperator.PostDec =>
          instructions += Instruction.unary(UnaryOpcode.PostDec)
        case _ => ()
      }
      instructions += Instruction.getLoc(baseIndex)
      instructions += Instruction.putGlobalWithBase(id.name)
      return
    }
    // Determine how to access this variable: local, global, or closure.
    // In direct eval, top-level `var`s are eval locals (not globals), so the
    // "top-level var uses global scope" heuristic must not apply.
    currentScope.parent == null && !directEvalMode && currentScope.isLocal(
      id.name
    ) match {
      case true =>
        // Top-level lexical variables live in locals (not global scope).
        if currentScope.isLexical(id.name) then {
          val index = currentScope.lookup(id.name).get
          emitIncrementDecrement(
            op,
            index,
            instructions,
            useGetLoc = true,
            useLocCheck = true
          )
        } else
          // Top-level var uses global scope.
          emitIncrementDecrement(
            op,
            id.name,
            instructions,
            useGetLoc = false,
            useLocCheck = false
          )

      case false if currentScope.isLocal(id.name) =>
        // Local variable in current function - use GetLoc/PutLoc
        val index = currentScope
          .lookup(id.name)
          .get // Safe because we just checked isLocal
        val useLocCheck = currentScope.isLexical(id.name)
        emitIncrementDecrement(
          op,
          index,
          instructions,
          useGetLoc = true,
          useLocCheck = useLocCheck
        )

      case _ =>
        // Variable from closure or parent scope - use GetGlobal/PutGlobal (checks closure map)
        emitIncrementDecrement(
          op,
          id.name,
          instructions,
          useGetLoc = false,
          useLocCheck = false
        )
    }

  /** Emit increment/decrement bytecode for a specific variable access method */
  private def emitIncrementDecrement(
      op: quickjs.ast.UnaryOperator,
      varRef: String | Int,
      instructions: mutable.ArrayBuffer[Instruction],
      useGetLoc: Boolean,
      useLocCheck: Boolean
  ): Unit = {
    // Helper functions to emit get/put instructions
    def emitGet(): Unit =
      if useGetLoc then
        varRef match {
          case i: Int =>
            if useLocCheck then instructions += Instruction.getLocCheck(i)
            else instructions += Instruction.getLoc(i)
          case s: String =>
            ()
        }
      else
        varRef match {
          case s: String =>
            instructions += Instruction.getGlobal(s)
          case _ =>
            ()
        }

    def emitPut(): Unit =
      if useGetLoc then
        varRef match {
          case i: Int =>
            instructions += Instruction.putLoc(i)
          case _ =>
            ()
        }
      else
        varRef match {
          case s: String =>
            instructions += Instruction.putGlobal(s)
          case _ =>
            ()
        }

    // Emit the operation-specific bytecode
    op match {
      case UnaryOperator.PreInc =>
        // PreInc: Get, Inc, Dup, Put
        emitGet()
        instructions += Instruction.unary(UnaryOpcode.PreInc)
        instructions += Instruction.dup()
        emitPut()

      case UnaryOperator.PostInc =>
        // PostInc: Get, PostInc (1->2: produces [old, new]), Put (stores new)
        emitGet()
        instructions += Instruction.unary(UnaryOpcode.PostInc)
        emitPut()

      case UnaryOperator.PreDec =>
        // PreDec: Get, Dec (1->1), Dup, Put
        emitGet()
        instructions += Instruction.unary(UnaryOpcode.PreDec)
        instructions += Instruction.dup()
        emitPut()

      case UnaryOperator.PostDec =>
        // PostDec: Get, PostDec (1->2: produces [old, new]), Put (stores new)
        emitGet()
        instructions += Instruction.unary(UnaryOpcode.PostDec)
        emitPut()
    }
  }

  private def compileMemberIncDec(
      op: quickjs.ast.UnaryOperator,
      memberExpr: MemberExpression,
      instructions: mutable.ArrayBuffer[Instruction],
      constants: mutable.ArrayBuffer[AnyRef]
  ): Unit = {
    val objIndex = allocateTempLocal("__incObj")
    compileExpression(memberExpr.`object`, instructions, constants)
    instructions += Instruction.putLoc(objIndex)

    val isInc = op == UnaryOperator.PreInc || op == UnaryOperator.PostInc
    val newOp = if isInc then UnaryOpcode.PreInc else UnaryOpcode.PreDec
    val oldValIndex = allocateTempLocal("__incOld")
    val newValIndex = allocateTempLocal("__incNew")

    memberExpr match {
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

        if op == UnaryOperator.PostInc || op == UnaryOperator.PostDec then {
          instructions += Instruction.drop()
          instructions += Instruction.getLoc(oldValIndex)
        }
      case MemberExpression(_, prop, false, _, _) =>
        prop match {
          case PrivateIdentifier(name, _) =>
            // Private field increment/decrement
            instructions += Instruction.getLoc(objIndex)
            instructions += Instruction.getPrivateField(privateOpcodeName(name))
            instructions += Instruction.pushI32(0)
            instructions += Instruction.binary(BinaryOpcode.Add)
            instructions += Instruction.putLoc(oldValIndex)

            instructions += Instruction.getLoc(oldValIndex)
            instructions += Instruction.unary(newOp)
            instructions += Instruction.putLoc(newValIndex)

            instructions += Instruction.getLoc(objIndex)
            instructions += Instruction.getLoc(newValIndex)
            instructions += Instruction.setPrivateField(privateOpcodeName(name))
            instructions += Instruction.drop()

            if op == UnaryOperator.PreInc || op == UnaryOperator.PreDec then
              instructions += Instruction.getLoc(newValIndex)
            else instructions += Instruction.getLoc(oldValIndex)
          case Identifier(propName, _) =>
            // Regular property increment/decrement
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
            else instructions += Instruction.getLoc(oldValIndex)
          case _ =>
            throw new UnsupportedOperationException(
              s"Unsupported property key: $prop"
            )
        }
    }
  }
}

object Compiler {
  def apply(): Compiler = new Compiler()
}
