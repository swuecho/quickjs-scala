package quickjs.bytecode

import scala.collection.mutable.{ArrayBuffer, StringBuilder}

/** Bytecode instruction encoding.
  *
  * Encoding format:
  *   - 1 byte: opcode
  *   - 0-8 bytes: operands (depending on opcode)
  *   - Operands can be: u8, i8, u16, i16, u32, i32, i64, f64
  */
final class Instruction(
    val opcode: Opcode,
    private val operands: Array[AnyRef]
) {
  def size: Int = 1 + operands.foldLeft(0)(_ + operandSize(_))

  private def operandSize(operand: AnyRef): Int = operand match {
    case _: java.lang.Integer => 4
    case _: java.lang.Long    => 8
    case _: java.lang.Double  => 8
    case s: String            => 4 + s.getBytes(java.nio.charset.StandardCharsets.UTF_8).length
    case _                    => 0
  }

  def encode(): Array[Byte] = {
    val buffer = ArrayBuffer[Byte]()
    buffer += opcode.code.toByte
    operands.foreach(encodeOperand(_, buffer))
    buffer.toArray
  }

  private def encodeOperand(operand: AnyRef, buffer: ArrayBuffer[Byte]): Unit =
    operand match {
      case i: java.lang.Integer =>
        val value = i.intValue()
        // DEBUG: Print encoding to catch any issues
        // println(s"Encoding Integer: $value -> bytes: ${((value >> 24) & 0xFF)}, ${((value >> 16) & 0xFF)}, ${((value >> 8) & 0xFF)}, ${(value & 0xFF)}")
        buffer += ((value >> 24) & 0xff).toByte
        buffer += ((value >> 16) & 0xff).toByte
        buffer += ((value >> 8) & 0xff).toByte
        buffer += (value & 0xff).toByte
      case l: java.lang.Long =>
        // Use big-endian to match Interpreter.readInt64
        val x = l.longValue()
        (0 until 8).foreach { i =>
          val shift = 56 - (i * 8)
          val byte = ((x >> shift) & 0xff).toByte
          buffer += byte
        }
      case d: java.lang.Double =>
        val bits = java.lang.Double.doubleToLongBits(d.doubleValue())
        encodeOperand(java.lang.Long.valueOf(bits), buffer)
      case s: String =>
        val bytes = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        val len = java.lang.Integer.valueOf(bytes.length)
        encodeOperand(len, buffer)
        buffer ++= bytes
      case _ =>
    }

  override def toString: String =
    s"$opcode${operands.mkString("(", ", ", ")")}"
}

object Instruction {
  def pushI32(value: Int): Instruction =
    new Instruction(
      Opcode.PushI32,
      Array[AnyRef](java.lang.Integer.valueOf(value))
    )

  def pushFloat64(value: Double): Instruction =
    new Instruction(
      Opcode.PushFloat64,
      Array[AnyRef](java.lang.Double.valueOf(value))
    )

  def pushUndefined(): Instruction =
    new Instruction(Opcode.PushUndefined, Array.empty)

  def pushNull(): Instruction =
    new Instruction(Opcode.PushNull, Array.empty)

  def pushTrue(): Instruction =
    new Instruction(Opcode.PushTrue, Array.empty)

  def pushFalse(): Instruction =
    new Instruction(Opcode.PushFalse, Array.empty)

  def drop(): Instruction =
    new Instruction(Opcode.Drop, Array.empty)

  def dup(): Instruction =
    new Instruction(Opcode.Dup, Array.empty)

  def dup2(): Instruction =
    new Instruction(Opcode.Dup2, Array.empty)

  def swap(): Instruction =
    new Instruction(Opcode.Swap, Array.empty)

  def rotate(): Instruction =
    new Instruction(Opcode.Rotate, Array.empty)

  def nip(): Instruction =
    new Instruction(Opcode.Nip, Array.empty)

  def unary(op: UnaryOpcode): Instruction =
    new Instruction(op.toOpcode, Array.empty)

  def binary(op: BinaryOpcode): Instruction =
    new Instruction(op.toOpcode, Array.empty)

  def returnUndef(): Instruction =
    new Instruction(Opcode.ReturnUndef, Array.empty)

  def returnInst(): Instruction =
    new Instruction(Opcode.Return, Array.empty)

  def getLoc(index: Int): Instruction =
    new Instruction(
      Opcode.GetLoc,
      Array[AnyRef](java.lang.Integer.valueOf(index))
    )

  def putLoc(index: Int): Instruction =
    new Instruction(
      Opcode.PutLoc,
      Array[AnyRef](java.lang.Integer.valueOf(index))
    )

  def getArg(index: Int): Instruction =
    new Instruction(
      Opcode.GetArg,
      Array[AnyRef](java.lang.Integer.valueOf(index))
    )

  def getRestArgs(index: Int): Instruction =
    new Instruction(
      Opcode.GetRestArgs,
      Array[AnyRef](java.lang.Integer.valueOf(index))
    )

  def getThis(): Instruction =
    new Instruction(Opcode.GetThis, Array.empty)

  def ifFalse(offset: Int): Instruction =
    new Instruction(
      Opcode.IfFalse,
      Array[AnyRef](java.lang.Integer.valueOf(offset))
    )

  def ifTrue(offset: Int): Instruction =
    new Instruction(
      Opcode.IfTrue,
      Array[AnyRef](java.lang.Integer.valueOf(offset))
    )

  def goto(offset: Int): Instruction =
    new Instruction(
      Opcode.Goto,
      Array[AnyRef](java.lang.Integer.valueOf(offset))
    )

  def breakInst(): Instruction =
    new Instruction(Opcode.Break, Array.empty)

  def continueInst(): Instruction =
    new Instruction(Opcode.Continue, Array.empty)

  def call(argc: Int): Instruction =
    new Instruction(Opcode.Call, Array[AnyRef](java.lang.Integer.valueOf(argc)))

  def callMethod(argc: Int): Instruction =
    new Instruction(
      Opcode.CallMethod,
      Array[AnyRef](java.lang.Integer.valueOf(argc))
    )

  def newInst(argc: Int): Instruction =
    new Instruction(Opcode.New, Array[AnyRef](java.lang.Integer.valueOf(argc)))

  def newObject(): Instruction =
    new Instruction(Opcode.NewObject, Array.empty)

  def getProp(name: String): Instruction =
    new Instruction(Opcode.GetProp, Array[AnyRef](name))

  def setProp(name: String): Instruction =
    new Instruction(Opcode.SetProp, Array[AnyRef](name))

  def defVar(name: String): Instruction =
    new Instruction(Opcode.DefVar, Array[AnyRef](name))

  def defFun(name: String): Instruction =
    new Instruction(Opcode.DefFun, Array[AnyRef](name))

  def getGlobal(name: String): Instruction =
    new Instruction(Opcode.GetGlobal, Array[AnyRef](name))

  def getGlobalOrUndefined(name: String): Instruction =
    new Instruction(Opcode.GetGlobalOrUndefined, Array[AnyRef](name))

  def putGlobal(name: String): Instruction =
    new Instruction(Opcode.PutGlobal, Array[AnyRef](name))

  def getThisUnchecked(): Instruction =
    new Instruction(Opcode.GetThisUnchecked, Array.empty)

  def markThisInitialized(): Instruction =
    new Instruction(Opcode.MarkThisInitialized, Array.empty)

  def getGlobalWithBase(name: String): Instruction =
    new Instruction(Opcode.GetGlobalWithBase, Array[AnyRef](name))

  def putGlobalWithBase(name: String): Instruction =
    new Instruction(Opcode.PutGlobalWithBase, Array[AnyRef](name))

  def enterScope(scopeIndex: Int): Instruction =
    new Instruction(
      Opcode.EnterScope,
      Array[AnyRef](java.lang.Integer.valueOf(scopeIndex))
    )

  def leaveScope(scopeIndex: Int): Instruction =
    new Instruction(
      Opcode.LeaveScope,
      Array[AnyRef](java.lang.Integer.valueOf(scopeIndex))
    )

  def setLocUninitialized(index: Int): Instruction =
    new Instruction(
      Opcode.SetLocUninitialized,
      Array[AnyRef](java.lang.Integer.valueOf(index))
    )

  def getLocCheck(index: Int): Instruction =
    new Instruction(
      Opcode.GetLocCheck,
      Array[AnyRef](java.lang.Integer.valueOf(index))
    )

  def setLocConst(index: Int): Instruction =
    new Instruction(
      Opcode.SetLocConst,
      Array[AnyRef](java.lang.Integer.valueOf(index))
    )

  /** Replace the local slot with a fresh `VarRef` for per-iteration loop
    * bindings; closures created before the clone keep the old binding.
    */
  def cloneLocRef(index: Int): Instruction =
    new Instruction(
      Opcode.CloneLocRef,
      Array[AnyRef](java.lang.Integer.valueOf(index))
    )

  def pushWith(): Instruction =
    new Instruction(Opcode.PushWith, Array.empty)

  def popWith(): Instruction =
    new Instruction(Opcode.PopWith, Array.empty)

  def getConst(index: Int): Instruction =
    new Instruction(
      Opcode.GetConst,
      Array[AnyRef](java.lang.Integer.valueOf(index))
    )

  def newArray(size: Int): Instruction =
    new Instruction(
      Opcode.NewArray,
      Array[AnyRef](java.lang.Integer.valueOf(size))
    )

  def getElem(): Instruction =
    new Instruction(Opcode.GetElem, Array.empty)

  def setElem(): Instruction =
    new Instruction(Opcode.SetElem, Array.empty)

  def initElem(): Instruction =
    new Instruction(Opcode.InitElem, Array.empty)

  def tryStart(catchPc: Int, finallyPc: Int): Instruction =
    new Instruction(
      Opcode.TryStart,
      Array[AnyRef](
        java.lang.Integer.valueOf(catchPc),
        java.lang.Integer.valueOf(finallyPc)
      )
    )

  def tryEnd(): Instruction =
    new Instruction(Opcode.TryEnd, Array.empty)

  def throwInst(): Instruction =
    new Instruction(Opcode.Throw, Array.empty)

  def getException(): Instruction =
    new Instruction(Opcode.GetException, Array.empty)

  def rethrowIfPending(): Instruction =
    new Instruction(Opcode.RethrowIfPending, Array.empty)

  // Generator opcodes
  def initialYield(): Instruction =
    new Instruction(Opcode.InitialYield, Array.empty)

  def yieldInst(): Instruction =
    new Instruction(Opcode.Yield, Array.empty)

  def yieldStar(): Instruction =
    new Instruction(Opcode.YieldStar, Array.empty)

  def awaitInst(): Instruction =
    new Instruction(Opcode.Await, Array.empty)

  def awaitAsyncInst(): Instruction =
    new Instruction(Opcode.AwaitAsync, Array.empty)

  // Private field access
  def getPrivateField(name: String): Instruction =
    new Instruction(Opcode.GetPrivateField, Array[AnyRef](name))

  def setPrivateField(name: String): Instruction =
    new Instruction(Opcode.SetPrivateField, Array[AnyRef](name))

  def definePrivateField(name: String): Instruction =
    new Instruction(Opcode.DefinePrivateField, Array[AnyRef](name))
}

enum UnaryOpcode {
  case Neg, Pos, Not, LNot
  case PreInc, PostInc, PreDec, PostDec
  case Typeof, Delete

  def toOpcode: Opcode = this match {
    case Neg     => Opcode.Neg
    case Pos     => Opcode.Pos
    case Not     => Opcode.Not
    case LNot    => Opcode.LNot
    case PreInc  => Opcode.PreInc
    case PostInc => Opcode.PostInc
    case PreDec  => Opcode.PreDec
    case PostDec => Opcode.PostDec
    case Typeof  => Opcode.Typeof
    case Delete  => Opcode.Delete
  }
}

enum BinaryOpcode {
  case Comma // Lowest precedence: eval left, discard, return right
  case Add, Sub, Mul, Div, Mod, Pow
  case Lt, Lte, Gt, Gte, Eq, Neq, StrictEq, StrictNeq
  case And, Or, Xor, Shl, Sar, Shr
  case LogicalAnd, LogicalOr
  case Instanceof, In

  def toOpcode: Opcode = this match {
    case Comma      => Opcode.Comma
    case Add        => Opcode.Add
    case Sub        => Opcode.Sub
    case Mul        => Opcode.Mul
    case Div        => Opcode.Div
    case Mod        => Opcode.Mod
    case Pow        => Opcode.Pow
    case Lt         => Opcode.Lt
    case Lte        => Opcode.Lte
    case Gt         => Opcode.Gt
    case Gte        => Opcode.Gte
    case Eq         => Opcode.Eq
    case Neq        => Opcode.Neq
    case StrictEq   => Opcode.StrictEq
    case StrictNeq  => Opcode.StrictNeq
    case And        => Opcode.And
    case Or         => Opcode.Or
    case Xor        => Opcode.Xor
    case Shl        => Opcode.Shl
    case Sar        => Opcode.Sar
    case Shr        => Opcode.Shr
    case LogicalAnd => Opcode.LogicalAnd
    case LogicalOr  => Opcode.LogicalOr
    case Instanceof => Opcode.Instanceof
    case In         => Opcode.In
  }
}

/** Bytecode function.
  */
final class BytecodeFunction(
    val name: String,
    val bytecode: Array[Byte],
    val constants: Array[AnyRef],
    val stackSize: Int,
    val freeVars: Array[String] =
      Array.empty, // Variables to capture from outer scope
    val freeVarSlots: Map[String, Int] =
      Map.empty, // Free vars resolved to parent local slots (slot-aware capture)
    val paramNames: Array[String] =
      Array.empty, // Parameter names in order (for closure capture)
    val localVarNames: Array[String] =
      Array.empty, // Local variable names (for closure capture)
    val argumentsIndex: Int = -1,
    val isConstructor: Boolean = true,
    val isClassConstructor: Boolean = false, // Class constructors require 'new'
    val isGenerator: Boolean = false, // True for function* declarations
    val isAsync: Boolean = false, // True for async function declarations
    val length: Int = 0,
    val spanMap: Array[(Int, Int, Int)] = Array.empty,
    val isStrict: Boolean = false,
    val functionExpressionName: Option[String] = None,
    val parameterScopeEndPc: Int = 0,
    val captureParentClosure: Boolean = false,
    val globalVarConfigurable: Boolean = false,
    val isModule: Boolean = false
) {
  def lineColForPc(pc: Int): Option[(Int, Int)] =
    if spanMap.isEmpty then None
    else {
      var idx = spanMap.length - 1
      while idx >= 0 && spanMap(idx)._1 > pc do idx -= 1
      if idx >= 0 then Some((spanMap(idx)._2, spanMap(idx)._3)) else None
    }

  /** Copy of this function with [[isAsync]] forced. Module bodies are compiled
    * async so top-level `await` suspends and resumes like an async function.
    */
  def withAsync(value: Boolean): BytecodeFunction =
    new BytecodeFunction(
      name = name,
      bytecode = bytecode,
      constants = constants,
      stackSize = stackSize,
      freeVars = freeVars,
      freeVarSlots = freeVarSlots,
      paramNames = paramNames,
      localVarNames = localVarNames,
      argumentsIndex = argumentsIndex,
      isConstructor = isConstructor,
      isClassConstructor = isClassConstructor,
      isGenerator = isGenerator,
      isAsync = value,
      length = length,
      spanMap = spanMap,
      isStrict = isStrict,
      functionExpressionName = functionExpressionName,
      parameterScopeEndPc = parameterScopeEndPc,
      captureParentClosure = captureParentClosure,
      globalVarConfigurable = globalVarConfigurable,
      isModule = isModule
    )

  override def toString: String =
    s"BytecodeFunction($name, ${bytecode.length} bytes, ${constants.length} constants, ${freeVars.length} free vars)"
}
