package quickjs.bytecode

/** Static operand-stack depth analysis.
  *
  * The interpreter allocates one JSValue array per call, so sizing it exactly
  * matters for call throughput. Every `BytecodeFunction` used to request a
  * 4096-slot stack; this analysis computes the maximum stack depth reachable in
  * a function so the compiler can allocate only what is needed.
  *
  * The analysis is a worklist over instruction indices. It walks every
  * reachable path (including the handlers registered by `TryStart`, whose
  * catch/finally blocks are only reachable through an exception) and records
  * the highest depth seen. `compute` returns -1 when it encounters an opcode
  * it does not model, and the compiler then falls back to a large fixed stack.
  */
object StackAnalysis:

  /** Returns the maximum operand-stack depth of the function, or -1 if the
    * bytecode contains an opcode this analysis does not understand.
    */
  def compute(instructions: scala.collection.IndexedSeq[Instruction]): Int =
    val n = instructions.length
    if n == 0 then return 0

    // Byte offset of each instruction (plus the end offset).
    val offsets = new Array[Int](n + 1)
    var i = 0
    while i < n do
      offsets(i + 1) = offsets(i) + instructions(i).size
      i += 1
    val total = offsets(n)

    val indexAt = new Array[Int](total + 1)
    java.util.Arrays.fill(indexAt, -1)
    i = 0
    while i < n do
      indexAt(offsets(i)) = i
      i += 1
    indexAt(total) = n // virtual end-of-code target

    val depthAt = new Array[Int](n)
    java.util.Arrays.fill(depthAt, -1)
    val worklist = new java.util.ArrayDeque[Int]()
    depthAt(0) = 0
    worklist.add(0)
    var maxDepth = 0
    var processed = 0
    val stepLimit = 50 * n + 100000

    def enqueue(instructionIndex: Int, depth: Int): Unit =
      if instructionIndex < 0 || instructionIndex > n then ()
      else if instructionIndex == n then
        if depth > maxDepth then maxDepth = depth
      else if depthAt(instructionIndex) >= depth then ()
      else
        depthAt(instructionIndex) = depth
        worklist.add(instructionIndex)

    while !worklist.isEmpty do
      val index = worklist.poll()
      processed += 1
      // Safety net: a positive-depth cycle would otherwise keep raising the
      // depth forever. Valid compiler output is stack-balanced, so this only
      // trips on a compiler bug; fall back to the fixed size rather than hang.
      if processed > stepLimit || maxDepth > 4096 then return -1
      val instruction = instructions(index)
      val depth = depthAt(index)
      if depth > maxDepth then maxDepth = depth

      val effect = stackEffect(instruction)
      if effect == null then return -1
      val (pops, pushes) = effect
      val nextDepth = math.max(0, depth - pops + pushes)
      if nextDepth > maxDepth then maxDepth = nextDepth

      val fallthrough = index + 1
      instruction.opcode match
        case Opcode.Goto =>
          enqueue(indexAt(offsets(index) + instruction.operandInt(0) + 1), depth)
        case Opcode.IfFalse | Opcode.IfTrue =>
          enqueue(indexAt(offsets(index) + instruction.operandInt(0) + 1), nextDepth)
          enqueue(fallthrough, nextDepth)
        case Opcode.Return | Opcode.ReturnUndef | Opcode.Throw | Opcode.Break |
            Opcode.Continue =>
          () // terminal for this frame
        case Opcode.TryStart =>
          enqueue(fallthrough, nextDepth)
          val catchPc = instruction.operandInt(0)
          val finallyPc = instruction.operandInt(1)
          if catchPc >= 0 && catchPc <= total then
            enqueue(indexAt(catchPc), depth)
          if finallyPc >= 0 && finallyPc <= total then
            enqueue(indexAt(finallyPc), depth)
        case _ =>
          enqueue(fallthrough, nextDepth)
    end while

    maxDepth
  end compute

  /** (pops, pushes) for an instruction, or null when unknown. */
  private def stackEffect(instruction: Instruction): (Int, Int) | Null =
    instruction.opcode match
      // Constants and loads
      case Opcode.PushI32 | Opcode.PushFloat64 | Opcode.PushUndefined |
          Opcode.PushNull | Opcode.PushTrue | Opcode.PushFalse | Opcode.Dup |
          Opcode.GetLoc | Opcode.GetLocCheck | Opcode.GetArg | Opcode.GetThis |
          Opcode.GetThisUnchecked | Opcode.GetGlobal | Opcode.GetGlobalOrUndefined |
          Opcode.GetConst | Opcode.NewObject | Opcode.NewArray |
          Opcode.GetException | Opcode.GetRestArgs | Opcode.DeleteName |
          Opcode.GetPrivateField =>
        (0, 1)

      // Stores
      case Opcode.Drop | Opcode.PutLoc | Opcode.PutArg | Opcode.PutGlobal |
          Opcode.DefVar | Opcode.DefFun =>
        (1, 0)
      case Opcode.SetLocUninitialized | Opcode.SetLocConst | Opcode.CloneLocRef |
          Opcode.EnterScope | Opcode.LeaveScope | Opcode.Nop | Opcode.Invalid |
          Opcode.SetThis | Opcode.MarkThisInitialized | Opcode.MarkSuperCalled |
          Opcode.TryEnd | Opcode.TryStart | Opcode.PopWith =>
        (0, 0)
      case Opcode.PushWith => (1, 0)

      case Opcode.Dup2 => (0, 2)
      case Opcode.Nip    => (2, 1)
      case Opcode.Swap | Opcode.Rotate => (0, 0)

      // Unary / updates
      case Opcode.Neg | Opcode.Pos | Opcode.Not | Opcode.LNot | Opcode.PreInc |
          Opcode.PreDec | Opcode.Typeof | Opcode.GetProp | Opcode.PrivateIn |
          Opcode.Await | Opcode.AwaitAsync | Opcode.Yield =>
        (1, 1)
      // Post-increment/decrement leave both the old and the new value.
      case Opcode.PostInc | Opcode.PostDec => (1, 2)

      // Binary
      case Opcode.Add | Opcode.Sub | Opcode.Mul | Opcode.Div | Opcode.Mod |
          Opcode.Pow | Opcode.Lt | Opcode.Lte | Opcode.Gt | Opcode.Gte |
          Opcode.Eq | Opcode.Neq | Opcode.StrictEq | Opcode.StrictNeq |
          Opcode.And | Opcode.Or | Opcode.Xor | Opcode.Shl | Opcode.Sar |
          Opcode.Shr | Opcode.LogicalAnd | Opcode.LogicalOr | Opcode.Comma |
          Opcode.Instanceof | Opcode.In | Opcode.Delete | Opcode.GetElem =>
        (2, 1)

      case Opcode.GetGlobalWithBase => (0, 2)
      case Opcode.SetProp          => (2, 1)
      case Opcode.PutGlobalWithBase => (2, 0)
      case Opcode.SetElem | Opcode.InitElem          => (3, 1)
      case Opcode.SetPrivateField | Opcode.DefinePrivateField => (2, 1)

      // Calls: argc is the number of arguments on the stack.
      case Opcode.Call =>
        val argc = instruction.operandInt(0)
        (argc + 1, 1)
      case Opcode.CallMethod =>
        val argc = instruction.operandInt(0)
        (argc + 2, 1)
      case Opcode.New =>
        val argc = instruction.operandInt(0)
        (argc + 1, 1)

      case Opcode.YieldStar => (1, 1)
      case Opcode.InitialYield | Opcode.RethrowIfPending => (0, 0)

      // Control flow
      case Opcode.Goto | Opcode.Break | Opcode.Continue | Opcode.ReturnUndef =>
        (0, 0)
      case Opcode.Return | Opcode.Throw => (1, 0)
      case Opcode.IfFalse | Opcode.IfTrue => (1, 0)

      case _ => null
