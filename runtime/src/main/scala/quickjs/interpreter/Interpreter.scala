package quickjs.interpreter

import quickjs.bytecode.*
import quickjs.value.JSValue
import quickjs.runtime.JSContext
import scala.util.control.Breaks.*

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
    args: Array[JSValue]
  )(using ctx: JSContext): JSValue =
    val stack = new Array[JSValue](function.stackSize)
    var stackTop = 0
    var pc = 0
    val bytecode = function.bytecode

    var result: JSValue = JSValue.Undefined

    breakable {
      while pc < bytecode.length do
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
            val r = JSValue.Int32(~a.toInt)
            stack(stackTop) = r
            stackTop += 1
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
            val r = JSValue.Int32(a.toInt & b.toInt)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Or =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(a.toInt | b.toInt)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Xor =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(a.toInt ^ b.toInt)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Shl =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(a.toInt << (b.toInt & 0x1F))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Sar =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(a.toInt >> (b.toInt & 0x1F))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Shr =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(a.toInt >>> (b.toInt & 0x1F))
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

          case Opcode.Return =>
            result = stack(stackTop - 1)
            break

          case Opcode.ReturnUndef =>
            result = JSValue.Undefined
            break

          case _ =>
            throw new RuntimeException(s"Unimplemented opcode: $opcode")
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
    (buf(pc) & 0xFF) | ((buf(pc + 1) & 0xFF) << 8) |
    ((buf(pc + 2) & 0xFF) << 16) | ((buf(pc + 3) & 0xFF) << 24)

  private def readDouble(buf: Array[Byte], pc: Int): Double =
    java.lang.Double.longBitsToDouble(readInt64(buf, pc))

  private def readInt64(buf: Array[Byte], pc: Int): Long =
    (buf(pc).toLong & 0xFF) |
    ((buf(pc + 1).toLong & 0xFF) << 8) |
    ((buf(pc + 2).toLong & 0xFF) << 16) |
    ((buf(pc + 3).toLong & 0xFF) << 24) |
    ((buf(pc + 4).toLong & 0xFF) << 32) |
    ((buf(pc + 5).toLong & 0xFF) << 40) |
    ((buf(pc + 6).toLong & 0xFF) << 48) |
    ((buf(pc + 7).toLong & 0xFF) << 56)

  def apply(): Interpreter = new Interpreter()
