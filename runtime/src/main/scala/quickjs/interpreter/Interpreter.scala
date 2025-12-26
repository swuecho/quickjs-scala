package quickjs.interpreter

import quickjs.bytecode.*
import quickjs.value.JSValue
import quickjs.runtime.JSContext
import scala.util.control.Breaks.*
import scala.annotation.switch
import scala.util.control.ControlThrowable

// Control flow exceptions for break/continue
private case object BreakException extends ControlThrowable
private case object ContinueException extends ControlThrowable

/** Minimal bytecode interpreter for Phase 1.
  *
  * Design:
  * - Stack-based virtual machine
  * - Direct threading optimization (via @switch)
  * - Support for arithmetic operations
  */
final class Interpreter:
  import Interpreter.*

  def call(
    function: BytecodeFunction,
    thisArg: JSValue,
    args: Array[JSValue],
    closure: Map[String, JSValue] = Map.empty
  )(using ctx: JSContext): JSValue =

    val stack = new Array[JSValue](function.stackSize)
    var stackTop = 0
    var pc = 0
    val bytecode = function.bytecode

    // Local variables array (for Phase 2)
    val locals = new Array[JSValue](256)  // Fixed size for now
    var localsCount = 0

    // Copy arguments to local variables (arguments come first in locals)
    for i <- args.indices do
      locals(i) = args(i)
    localsCount = args.length

    // Copy closure values to local variables (after arguments)
    // For each captured variable, we need to know where to store it
    // For now, we'll just look them up dynamically from the closure map
    // when GetGlobal is called

    var result: JSValue = JSValue.Undefined

    breakable {
      while pc < bytecode.length do
        try {
          val opcode = Opcode.fromCode(bytecode(pc).toInt & 0xFF).getOrElse(Opcode.Invalid)

          (opcode: @switch) match
          case Opcode.Invalid =>
            throw new RuntimeException("Invalid opcode")

          case Opcode.Nop =>
            pc += 1

          case Opcode.PushI32 =>
            val value = readInt32(bytecode, pc + 1)
            stack(stackTop) = JSValue.fromInt(value)
            stackTop += 1
            pc += 5

          case Opcode.PushFloat64 =>
            val value = readDouble(bytecode, pc + 1)
            stack(stackTop) = JSValue.fromDouble(value)
            stackTop += 1
            pc += 9

          case Opcode.PushUndefined =>
            stack(stackTop) = JSValue.Undefined
            stackTop += 1
            pc += 1

          case Opcode.PushNull =>
            stack(stackTop) = JSValue.Null
            stackTop += 1
            pc += 1

          case Opcode.PushTrue =>
            stack(stackTop) = JSValue.Bool(true)
            stackTop += 1
            pc += 1

          case Opcode.PushFalse =>
            stack(stackTop) = JSValue.Bool(false)
            stackTop += 1
            pc += 1

          case Opcode.Drop =>
            stackTop -= 1
            pc += 1

          case Opcode.Dup =>
            stack(stackTop) = stack(stackTop - 1)
            stackTop += 1
            pc += 1

          case Opcode.GetLoc =>
            val index = readInt32(bytecode, pc + 1)
            stack(stackTop) = locals(index)
            stackTop += 1
            pc += 5

          case Opcode.PutLoc =>
            val index = readInt32(bytecode, pc + 1)
            stackTop -= 1
            locals(index) = stack(stackTop)
            if index >= localsCount then
              localsCount = index + 1
            pc += 5

          case Opcode.GetArg =>
            // For now, treat as GetLoc (arguments and locals in same array)
            val index = readInt32(bytecode, pc + 1)
            stack(stackTop) = locals(index)
            stackTop += 1
            pc += 5

          case Opcode.PutArg =>
            // For now, treat as PutLoc
            val index = readInt32(bytecode, pc + 1)
            stackTop -= 1
            locals(index) = stack(stackTop)
            if index >= localsCount then
              localsCount = index + 1
            pc += 5

          case Opcode.Neg =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val r = JSValue.Float64(-a.toNumber)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Not =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val r = JSValue.Bool(!a.toBoolean)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.LNot =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val r = JSValue.Int32(~a.toNumber.toInt)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.PreInc =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val r = JSValue.fromInt(a.toNumber.toInt + 1)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.PostInc =>
            // Post-increment: keep original value, push incremented value
            // Before: [x], After: [x, x+1]
            val a = stack(stackTop - 1)
            val r = JSValue.fromInt(a.toNumber.toInt + 1)
            stack(stackTop) = r  // Push incremented value
            stackTop += 1         // Stack grows by 1
            pc += 1

          case Opcode.PreDec =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val r = JSValue.fromInt(a.toNumber.toInt - 1)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.PostDec =>
            // Post-decrement: keep original value, push decremented value
            // Before: [x], After: [x, x-1]
            val a = stack(stackTop - 1)
            val r = JSValue.fromInt(a.toNumber.toInt - 1)
            stack(stackTop) = r  // Push decremented value
            stackTop += 1         // Stack grows by 1
            pc += 1

          case Opcode.Add =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.add(a, b)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Sub =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.subtract(a, b)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Mul =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.multiply(a, b)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Div =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.divide(a, b)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Mod =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Float64(math.IEEEremainder(a.toNumber, b.toNumber))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Lt =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(compare(a, b) < 0)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Lte =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(compare(a, b) <= 0)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Gt =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(compare(a, b) > 0)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Gte =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(compare(a, b) >= 0)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Eq =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(looseEqual(a, b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Neq =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(!looseEqual(a, b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.StrictEq =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(strictEqual(a, b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.StrictNeq =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(!strictEqual(a, b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.And =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(a.toNumber.toInt & b.toNumber.toInt)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Or =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(a.toNumber.toInt | b.toNumber.toInt)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Xor =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(a.toNumber.toInt ^ b.toNumber.toInt)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Shl =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(a.toNumber.toInt << (b.toNumber.toInt & 0x1F))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Sar =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(a.toNumber.toInt >> (b.toNumber.toInt & 0x1F))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Shr =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(a.toNumber.toInt >>> (b.toNumber.toInt & 0x1F))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.LogicalAnd =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(a.toBoolean && b.toBoolean)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.LogicalOr =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Bool(a.toBoolean || b.toBoolean)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.IfFalse =>
            val offset = readInt32(bytecode, pc + 1)
            val value = stack(stackTop - 1)
            stackTop -= 1
            if !value.toBoolean then
              pc += offset + 1
            else
              pc += 5

          case Opcode.IfTrue =>
            val offset = readInt32(bytecode, pc + 1)
            val value = stack(stackTop - 1)
            stackTop -= 1
            if value.toBoolean then
              pc += offset + 1
            else
              pc += 5

          case Opcode.Goto =>
            val offset = readInt32(bytecode, pc + 1)
            pc += offset + 1

          case Opcode.Break =>
            throw BreakException

          case Opcode.Continue =>
            throw ContinueException

          case Opcode.Return =>
            result = stack(stackTop - 1)
            break

          case Opcode.ReturnUndef =>
            result = JSValue.Undefined
            break

          case Opcode.Call =>
            val argc = readInt32(bytecode, pc + 1)
            // Stack layout: [func, arg1, arg2, ..., argN]
            // func is at stackTop - argc - 1
            val funcValue = stack(stackTop - argc - 1)
            val args = new Array[JSValue](argc)
            for i <- 0 until argc do
              args(i) = stack(stackTop - argc + i)

            // Pop func and arguments
            stackTop -= (argc + 1)

            // Call the function based on its type
            funcValue match
              case func: JSValue.Function =>
                // Create a temporary BytecodeFunction wrapper
                val bcFunc = new BytecodeFunction(
                  name = func.name,
                  bytecode = func.bytecode,
                  constants = func.constants,
                  stackSize = func.stackSize,
                  freeVars = Array.empty,  // Already captured in closure
                  paramNames = func.paramNames  // Copy paramNames for nested closures
                )
                val retValue = this.call(bcFunc, JSValue.Undefined, args, func.closure)
                stack(stackTop) = retValue
                stackTop += 1
              case _ =>
                throw new RuntimeException(s"Cannot call non-function value: $funcValue")
            pc += 5

          case Opcode.NewObject =>
            import quickjs.objmodel.JSObject
            val obj = JSObject(prototype = null, extensible = true)
            stack(stackTop) = JSValue.Object(obj)
            stackTop += 1
            pc += 1

          case Opcode.NewArray =>
            val size = readInt32(bytecode, pc + 1)
            import quickjs.objmodel.JSArray
            val arr = JSArray(size)
            stack(stackTop) = JSValue.JSArrayVal(arr)
            stackTop += 1
            pc += 5

          case Opcode.GetElem =>
            // Stack layout: [obj, index]
            val indexValue = stack(stackTop - 1)
            val objValue = stack(stackTop - 2)
            stackTop -= 2

            val result = (objValue, indexValue) match
              case (JSValue.JSArrayVal(arr), JSValue.Int32(i)) =>
                arr.get(i)
              case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) =>
                arr.get(d.toInt)
              case _ =>
                // For non-arrays or invalid indices, return undefined
                JSValue.Undefined

            stack(stackTop) = result
            stackTop += 1
            pc += 1

          case Opcode.SetElem =>
            // Stack layout: [obj, index, value]
            // For assignment expressions: returns the value (e.g., arr[0] = 5 evaluates to 5)
            val value = stack(stackTop - 1)
            val indexValue = stack(stackTop - 2)
            val objValue = stack(stackTop - 3)
            stackTop -= 3

            (objValue, indexValue) match
              case (JSValue.JSArrayVal(arr), JSValue.Int32(i)) =>
                arr.set(i, value)
              case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) =>
                arr.set(d.toInt, value)
              case _ =>
                // For non-arrays, ignore (could throw error in strict mode)
                ()

            // Leave value on stack (for assignment expressions)
            stack(stackTop) = value
            stackTop += 1
            pc += 1

          case Opcode.InitElem =>
            // Stack layout: [obj, index, value]
            // For array literal initialization: returns the array (not the value)
            val value = stack(stackTop - 1)
            val indexValue = stack(stackTop - 2)
            val objValue = stack(stackTop - 3)
            stackTop -= 3

            (objValue, indexValue) match
              case (JSValue.JSArrayVal(arr), JSValue.Int32(i)) =>
                arr.set(i, value)
              case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) =>
                arr.set(d.toInt, value)
              case _ =>
                // For non-arrays, ignore (could throw error in strict mode)
                ()

            // Leave object on stack (for array literal construction)
            stack(stackTop) = objValue
            stackTop += 1
            pc += 1

          case Opcode.GetProp =>
            val propName = readString(bytecode, pc + 1)
            val objValue = stack(stackTop - 1)
            stackTop -= 1

            val result = objValue match
              case JSValue.Object(obj) =>
                obj.get(propName)  // Already returns JSValue.Undefined if not found
              case _ =>
                // For non-objects, return undefined
                JSValue.Undefined

            stack(stackTop) = result
            stackTop += 1
            pc += 1 + 4 + propName.length  // opcode + length prefix + string bytes

          case Opcode.SetProp =>
            val propName = readString(bytecode, pc + 1)
            // Stack layout: [obj, value]
            val value = stack(stackTop - 1)
            val objValue = stack(stackTop - 2)
            stackTop -= 2

            objValue match
              case JSValue.Object(obj) =>
                obj.set(propName, value)
              case _ =>
                throw new RuntimeException(s"Cannot set property on non-object: $objValue")

            // Leave object on stack (for chained property sets in object literals)
            stack(stackTop) = objValue
            stackTop += 1
            pc += 1 + 4 + propName.length

          case Opcode.Swap =>
            val a = stack(stackTop - 1)
            val b = stack(stackTop - 2)
            stack(stackTop - 1) = b
            stack(stackTop - 2) = a
            pc += 1

          case Opcode.DefVar =>
            val varName = readString(bytecode, pc + 1)
            // Stack layout: [value]
            val value = stack(stackTop - 1)
            stackTop -= 1

            // Store in global scope
            ctx.globalScope.setVariable(varName, value)
            pc += 1 + 4 + varName.length

          case Opcode.DefFun =>
            val funName = readString(bytecode, pc + 1)
            // Stack layout: [value] (currently always undefined)
            val funcValue = stack(stackTop - 1)
            stackTop -= 1

            // Store in global scope
            // TODO: For now, we just store undefined. In the future, this should
            // create a proper function value from the compiled bytecode.
            ctx.globalScope.setFunction(funName, funcValue)
            pc += 1 + 4 + funName.length

          case Opcode.GetGlobal =>
            val varName = readString(bytecode, pc + 1)

            // Look up in closure first (for closures), then global scope
            val result = closure.get(varName).orElse {
              ctx.globalScope.getVariable(varName)
            }.getOrElse {
              // Try function
              ctx.globalScope.getFunction(varName).getOrElse(JSValue.Undefined)
            }

            stack(stackTop) = result
            stackTop += 1
            pc += 1 + 4 + varName.length

          case Opcode.GetConst =>
            val index = readInt32(bytecode, pc + 1)
            val constValue = function.constants(index)

            // If it's a BytecodeFunction, convert to JSValue.Function with captured closure
            val value = constValue match
              case bcFunc: BytecodeFunction =>
                // Capture closure from local scope, parent closure, and global scope
                val newClosure = bcFunc.freeVars.flatMap { varName =>
                  // First check if it's a parameter in the current (parent) function
                  val paramIndex = function.paramNames.indexOf(varName)

                  if paramIndex >= 0 && paramIndex < localsCount then
                    // Variable is a parameter in the parent function, capture from locals
                    Some(varName -> locals(paramIndex))
                  else
                    // Not a local parameter, check parent's closure (the 'closure' parameter)
                    val fromClosure = closure.get(varName)
                    if fromClosure.isDefined then
                      fromClosure.map(varName -> _)
                    else
                      // Not in closure either, try global scope
                      ctx.globalScope.getVariable(varName).map(varName -> _)
                }.toMap

                JSValue.Function(
                  name = bcFunc.name,
                  bytecode = bcFunc.bytecode,
                  constants = bcFunc.constants,
                  stackSize = bcFunc.stackSize,
                  closure = newClosure,
                  paramNames = bcFunc.paramNames  // Copy paramNames for nested closures
                )
              case jsValue: JSValue =>
                jsValue
              case _ =>
                JSValue.Undefined

            stack(stackTop) = value
            stackTop += 1
            pc += 5

          case _ =>
            throw new RuntimeException(s"Unimplemented opcode: $opcode")
        } catch {
          case BreakException =>
            break()
          case ContinueException =>
            // Continue to next iteration - fall through to next instruction
            // The compiler should generate proper bytecode where continue
            // targets the update/goto part of the loop
            ()
        }
    }

    result

  // Helper functions for comparisons
  private def compare(a: JSValue, b: JSValue): Double = (a, b) match
    case (_: JSValue.JSStr, _: JSValue.JSStr) =>
      a.toString.compareTo(b.toString).toDouble
    case _ =>
      val na = a.toNumber
      val nb = b.toNumber
      if na.isNaN || nb.isNaN then Double.NaN
      else na - nb

  private def looseEqual(a: JSValue, b: JSValue): Boolean = (a, b) match
    case (JSValue.Undefined, JSValue.Null) => true
    case (JSValue.Null, JSValue.Undefined) => true
    case (_: JSValue.JSStr, _: JSValue.JSStr) => a.toString == b.toString
    case (_: JSValue.Bool, _) | (_, _: JSValue.Bool) => a.toNumber == b.toNumber
    case (_: JSValue.Number, _) | (_, _: JSValue.Number) => a.toNumber == b.toNumber
    case _ => false

  private def strictEqual(a: JSValue, b: JSValue): Boolean = (a, b) match
    case (JSValue.Undefined, JSValue.Undefined) => true
    case (JSValue.Null, JSValue.Null) => true
    case (JSValue.Bool(x), JSValue.Bool(y)) => x == y
    case (JSValue.Int32(x), JSValue.Int32(y)) => x == y
    case (JSValue.Float64(x), JSValue.Float64(y)) => x == y
    case (JSValue.Int32(x), JSValue.Float64(y)) => x.toDouble == y
    case (JSValue.Float64(x), JSValue.Int32(y)) => x == y.toDouble
    case (_: JSValue.JSStr, _: JSValue.JSStr) => a.toString == b.toString
    case _ => false

object Interpreter:
  private def readInt32(buf: Array[Byte], pc: Int): Int =
    ((buf(pc) & 0xFF) << 24) | ((buf(pc + 1) & 0xFF) << 16) |
    ((buf(pc + 2) & 0xFF) << 8) | (buf(pc + 3) & 0xFF)

  private def readDouble(buf: Array[Byte], pc: Int): Double =
    java.lang.Double.longBitsToDouble(readInt64(buf, pc))

  private def readInt64(buf: Array[Byte], pc: Int): Long =
    ((buf(pc).toLong & 0xFF) << 56) |
    ((buf(pc + 1).toLong & 0xFF) << 48) |
    ((buf(pc + 2).toLong & 0xFF) << 40) |
    ((buf(pc + 3).toLong & 0xFF) << 32) |
    ((buf(pc + 4).toLong & 0xFF) << 24) |
    ((buf(pc + 5).toLong & 0xFF) << 16) |
    ((buf(pc + 6).toLong & 0xFF) << 8) |
    (buf(pc + 7).toLong & 0xFF)

  private def readString(buf: Array[Byte], pc: Int): String =
    val len = readInt32(buf, pc)
    val bytes = new Array[Byte](len)
    System.arraycopy(buf, pc + 4, bytes, 0, len)
    new String(bytes, java.nio.charset.StandardCharsets.UTF_8)

  def apply(): Interpreter = new Interpreter()
