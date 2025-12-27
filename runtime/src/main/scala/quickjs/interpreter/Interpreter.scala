package quickjs.interpreter

import quickjs.bytecode.*
import quickjs.value.JSValue
import quickjs.runtime.JSContext
import scala.util.control.Breaks.*
import scala.annotation.switch
import scala.util.control.ControlThrowable
import scala.collection.mutable

// Import debug tracer
import quickjs.interpreter.DebugTracer

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
    closure: mutable.Map[String, JSValue.VarRef] = mutable.Map.empty
  )(using ctx: JSContext): JSValue =

    val stack = new Array[JSValue](function.stackSize)
    var stackTop = 0
    var pc = 0
    val bytecode = function.bytecode

    // Store 'this' value for GetThis opcode
    val thisValue: JSValue = thisArg

    // Local variables array using VarRef for pointer indirection (like QuickJS)
    // This enables closures to capture and mutate local variables
    val locals = new Array[JSValue.VarRef](256)  // Fixed size for now
    // Initialize all locals with VarRef(Undefined) to avoid nulls
    for i <- 0 until 256 do
      locals(i) = new JSValue.VarRef(JSValue.Undefined)
    var localsCount = 0

    // Copy arguments to local variables (arguments come first in locals)
    // Each argument is wrapped in a VarRef so closures can capture it
    for i <- args.indices do
      locals(i).set(args(i))
    localsCount = args.length

    // Copy closure values to local variables (after arguments)
    // For each captured variable, we need to know where to store it
    // For now, we'll just look them up dynamically from the closure map
    // when GetGlobal is called

    var result: JSValue = JSValue.Undefined

    // Safety check: prevent infinite loops (for debugging)
    var iterations = 0
    val maxIterations = 100000

    breakable {
      while pc < bytecode.length do
        iterations += 1
        if iterations > maxIterations then
          throw new RuntimeException(s"Infinite loop detected: executed $maxIterations instructions without terminating")
        try {
          val opcode = Opcode.fromCode(bytecode(pc).toInt & 0xFF).getOrElse(Opcode.Invalid)

          // DEBUG: Print all opcodes when closure is non-empty
          if closure.nonEmpty then

          // Debug tracing
          if DebugTracer.global.isEnabled then
            DebugTracer.global.traceInstruction(
              pc = pc,
              opcode = opcode,
              stack = stack,
              stackTop = stackTop,
              locals = locals,
              localsCount = localsCount
            )

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
            if index < 0 || index >= locals.length then
              throw new RuntimeException(s"GetLoc: Index $index out of bounds for locals array (length ${locals.length})")
            // Unwrap the VarRef to get the actual value
            stack(stackTop) = locals(index).get
            stackTop += 1
            pc += 5

          case Opcode.GetThis =>
            // Push the 'this' value onto the stack
            stack(stackTop) = thisValue
            stackTop += 1
            pc += 1

          case Opcode.PutLoc =>
            val index = readInt32(bytecode, pc + 1)
            if index < 0 || index >= locals.length then
              throw new RuntimeException(s"PutLoc: Index $index out of bounds for locals array (length ${locals.length})")
            stackTop -= 1
            // Update the VarRef with the new value
            locals(index).set(stack(stackTop))
            if index >= localsCount then
              localsCount = index + 1
            pc += 5

          case Opcode.GetArg =>
            // For now, treat as GetLoc (arguments and locals in same array)
            val index = readInt32(bytecode, pc + 1)
            // Unwrap the VarRef to get the actual value
            stack(stackTop) = locals(index).get
            stackTop += 1
            pc += 5

          case Opcode.PutArg =>
            // For now, treat as PutLoc
            val index = readInt32(bytecode, pc + 1)
            stackTop -= 1
            // Update the VarRef with the new value
            locals(index).set(stack(stackTop))
            if index >= localsCount then
              localsCount = index + 1
            pc += 5

          case Opcode.Neg =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val r = JSValue.fromDouble(-a.toNumber)
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

          case Opcode.Typeof =>
            val a = stack(stackTop - 1)
            stackTop -= 1
            val typeName = a match
              case JSValue.Undefined => "undefined"
              case JSValue.Null => "object"
              case _: JSValue.Bool => "boolean"
              case _: JSValue.Int32 | _: JSValue.Float64 => "number"
              case _: JSValue.JSStr => "string"
              case _: JSValue.Function => "function"
              case JSValue.Object(_) | _: JSValue.JSArrayVal => "object"
              case JSValue.Native(_) => "function"
            val r = JSValue.fromString(typeName)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Delete =>
            // Delete operator: obj.prop or obj[expr]
            // Stack: [obj, prop] -> [successBoolean]
            val propName = stack(stackTop - 1)
            val obj = stack(stackTop - 2)
            stackTop -= 2

            val prop = propName match
              case JSValue.JSStr(s) => s
              case _ => propName.toNumber.toInt.toString

            val r = obj match
              case JSValue.Object(o) =>
                JSValue.Bool(o.deleteProperty(prop)(using ctx))
              case _ =>
                // Can't delete properties on primitives
                JSValue.Bool(true)
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
            // JavaScript % is truncated remainder, not IEEE remainder
            // Result has same sign as dividend (a)
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val na = a.toNumber
            val nb = b.toNumber
            val truncated = na / nb
            // Truncate toward zero
            val truncatedInt = if truncated >= 0 then math.floor(truncated) else math.ceil(truncated)
            val r = JSValue.fromDouble(na - truncatedInt * nb)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Pow =>
            // Exponentiation: a ** b
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val na = a.toNumber
            val nb = b.toNumber
            val r = JSValue.fromDouble(math.pow(na, nb))
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
            val r = JSValue.Int32(toInt32(a) & toInt32(b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Or =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(toInt32(a) | toInt32(b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Xor =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(toInt32(a) ^ toInt32(b))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Shl =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(toInt32(a) << (toInt32(b) & 0x1F))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Sar =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = JSValue.Int32(toInt32(a) >> (toInt32(b) & 0x1F))
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Shr =>
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            // Unsigned right shift: result is unsigned 32-bit
            val shiftCount = toInt32(b) & 0x1F
            val unsignedResult = toInt32(a) >>> shiftCount
            // Convert to unsigned long for proper representation
            val asUnsigned = unsignedResult.toLong & 0xFFFFFFFFL
            val r = JSValue.fromDouble(asUnsigned.toDouble)
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.LogicalAnd =>
            // JavaScript: a && b returns a if falsy, else b
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = if a.toBoolean then b else a
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.LogicalOr =>
            // JavaScript: a || b returns a if truthy, else b
            val b = stack(stackTop - 1)
            val a = stack(stackTop - 2)
            stackTop -= 2
            val r = if a.toBoolean then a else b
            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.Instanceof =>
            // instanceof operator: obj instanceof constructor
            // Stack: [obj, constructor] -> [boolean]
            val constructor = stack(stackTop - 1)
            val obj = stack(stackTop - 2)
            stackTop -= 2

            // Get constructor's prototype property
            val ctorPrototype = constructor match
              case JSValue.Object(ctorObj) => ctorObj.get("prototype")
              case JSValue.Native(nativeCtor) =>
                nativeCtor match
                  case ctor: quickjs.value.NativeConstructor =>
                    JSValue.Object(ctor.prototype)
                  case _ => JSValue.Null
              case _ => JSValue.Null

            // Check if obj's prototype chain contains the constructor's prototype
            val r = obj match
              case JSValue.Object(objVal) =>
                var currentProto: quickjs.objmodel.JSObject | Null = objVal
                var found = false

                // Walk up the prototype chain
                while !found && (currentProto != null) do
                  // Check if current prototype matches constructor's prototype
                  ctorPrototype match
                    case JSValue.Object(protoObj) =>
                      if currentProto == protoObj then
                        found = true
                      else
                        currentProto = currentProto.getPrototype
                    case _ =>
                      currentProto = null

                JSValue.Bool(found)

              case _ => JSValue.Bool(false)

            stack(stackTop) = r
            stackTop += 1
            pc += 1

          case Opcode.In =>
            // in operator: prop in obj
            // Stack: [prop, obj] -> [boolean]
            // Note: compiler pushes left (prop) first, then right (obj)
            val propName = stack(stackTop - 2)  // First pushed = left = prop
            val objVal = stack(stackTop - 1)     // Second pushed = right = obj
            stackTop -= 2

            val prop = propName match
              case JSValue.JSStr(s) => s
              case _ => propName.toNumber.toInt.toString

            val r = objVal match
              case JSValue.Object(o) =>
                JSValue.Bool(o.hasProperty(prop))
              case _ =>
                JSValue.Bool(false)
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

            // DEBUG

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
                  paramNames = func.paramNames,  // Copy paramNames for nested closures
                  localVarNames = func.localVarNames  // Copy localVarNames for nested closures
                )
                val retValue = this.call(bcFunc, JSValue.Undefined, args, func.closure)
                stack(stackTop) = retValue
                stackTop += 1
              case JSValue.Native(nativeFuncWrapper) =>
                // Unwrap and call the native function or constructor
                nativeFuncWrapper match
                  case native: quickjs.value.NativeFunction =>
                    // Regular native function call
                    val retValue = native.call(args)
                    stack(stackTop) = retValue
                    stackTop += 1
                  case constructor: quickjs.value.NativeConstructor =>
                    // Constructor called without 'new' - use call mode
                    val retValue = constructor.call(args)(using ctx)
                    stack(stackTop) = retValue
                    stackTop += 1
                  case _ =>
                    throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
              case _ =>
                throw new RuntimeException(s"Cannot call non-function value: $funcValue")
            pc += 5

          case Opcode.CallMethod =>
            val argc = readInt32(bytecode, pc + 1)
            // Stack layout: [this, func, arg1, arg2, ..., argN]
            // this is at stackTop - argc - 2
            // func is at stackTop - argc - 1
            val thisValue = stack(stackTop - argc - 2)
            val funcValue = stack(stackTop - argc - 1)


            val args = new Array[JSValue](argc)
            for i <- 0 until argc do
              args(i) = stack(stackTop - argc + i)

            // Pop this, func and arguments
            stackTop -= (argc + 2)

            // Call the function with 'this' binding
            funcValue match
              case func: JSValue.Function =>
                // Create a temporary BytecodeFunction wrapper
                val bcFunc = new BytecodeFunction(
                  name = func.name,
                  bytecode = func.bytecode,
                  constants = func.constants,
                  stackSize = func.stackSize,
                  freeVars = Array.empty,  // Already captured in closure
                  paramNames = func.paramNames,  // Copy paramNames for nested closures
                  localVarNames = func.localVarNames  // Copy localVarNames for nested closures
                )
                val retValue = this.call(bcFunc, thisValue, args, func.closure)
                stack(stackTop) = retValue
                stackTop += 1
              case JSValue.Native(nativeFuncWrapper) =>
                // Unwrap and call the native function/constructor with 'this' binding
                nativeFuncWrapper match
                  case native: quickjs.value.NativeFunction =>
                    // For native functions, we need to handle 'this' binding
                    // The native function receives args, but we prepend 'this'
                    val argsWithThis = new Array[JSValue](argc + 1)
                    argsWithThis(0) = thisValue
                    Array.copy(args, 0, argsWithThis, 1, argc)
                    val retValue = native.call(argsWithThis)
                    stack(stackTop) = retValue
                    stackTop += 1
                  case constructor: quickjs.value.NativeConstructor =>
                    // Constructor called as method (rare) - use call mode with this binding
                    val retValue = constructor.call(args)(using ctx)
                    stack(stackTop) = retValue
                    stackTop += 1
                  case _ =>
                    throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
              case _ =>
                throw new RuntimeException(s"Cannot call non-function value: $funcValue")
            pc += 5

          case Opcode.New =>
            // new constructor: new Foo(arg1, arg2, ...)
            // Stack: [constructor, arg1, arg2, ..., argN]
            // constructor is at stackTop - argc - 1
            val argc = readInt32(bytecode, pc + 1)
            val constructorValue = stack(stackTop - argc - 1)
            val args = new Array[JSValue](argc)
            for i <- 0 until argc do
              args(i) = stack(stackTop - argc + i)

            // Pop constructor and arguments
            stackTop -= (argc + 1)

            // Call the constructor based on its type
            val result = constructorValue match
              case JSValue.Native(constructorWrapper) =>
                constructorWrapper match
                  case constructor: quickjs.value.NativeConstructor =>
                    // Native constructor - use construct mode
                    constructor.construct(args)(using ctx)
                  case _ =>
                    throw new RuntimeException(s"Cannot use 'new' with non-constructor: $constructorValue")
              case func: JSValue.Function =>
                // User-defined function - create object with function's prototype
                // Get the function's prototype
                val funcPrototype = func.closure.get("prototype") match
                  case Some(varRef) => varRef.get match
                    case JSValue.Object(proto) => proto
                    case _ => ctx.objectPrototype
                  case None => ctx.objectPrototype

                // Create new object with function's prototype
                import quickjs.objmodel.JSObject
                val newObj = JSObject(prototype = funcPrototype, extensible = true)

                // Call the function with 'this' bound to the new object
                val bcFunc = new BytecodeFunction(
                  name = func.name,
                  bytecode = func.bytecode,
                  constants = func.constants,
                  stackSize = func.stackSize,
                  freeVars = Array.empty,
                  paramNames = func.paramNames,
                  localVarNames = func.localVarNames  // Copy localVarNames for nested closures
                )
                val retValue = this.call(bcFunc, JSValue.Object(newObj), args, func.closure)

                // If function returns an object, return that; otherwise return new object
                retValue match
                  case JSValue.Object(_) => retValue
                  case _ => JSValue.Object(newObj)
              case _ =>
                throw new RuntimeException(s"Cannot use 'new' with non-constructor: $constructorValue")

            stack(stackTop) = result
            stackTop += 1
            pc += 5

          case Opcode.NewObject =>
            import quickjs.objmodel.JSObject
            val obj = JSObject(prototype = ctx.objectPrototype, extensible = true)
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
              case (JSValue.Object(obj), JSValue.JSStr(propName)) =>
                // Object property access with string key: obj["prop"]
                obj.get(propName)
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
              case arrVal: JSValue.JSArrayVal =>
                // For arrays, check special properties first
                if propName == "length" then
                  JSValue.fromInt(arrVal.value.length)
                else
                  // Look up methods from Array.prototype
                  // If stdlib is initialized, use arrayPrototype
                  // Otherwise fall back to looking in global Array object (backward compatibility)
                  val result = ctx.arrayPrototype.get(propName)(using ctx)
                  if result == JSValue.Undefined then
                    // Fall back to global Array object for backward compatibility
                    val arrayObj = ctx.global.get("Array")
                    arrayObj match
                      case JSValue.Object(obj) => obj.get(propName)
                      case _ => JSValue.Undefined
                  else
                    result
              case strVal: JSValue.JSStr =>
                // For strings, check special properties
                if propName == "length" then
                  JSValue.fromInt(strVal.value.length)
                else
                  // Look up methods from the global String object
                  val stringObj = ctx.global.get("String")
                  stringObj match
                    case JSValue.Object(obj) => obj.get(propName)
                    case _ => JSValue.Undefined
              case funcVal: JSValue.Function =>
                // For functions, look up methods from Function.prototype
                // This is a temporary solution until proper prototype chains are implemented
                val result = ctx.functionPrototype.get(propName)(using ctx)
                if result == JSValue.Undefined then
                  // Fall back to global Function object for backward compatibility
                  val funcObj = ctx.global.get("Function")
                  funcObj match
                    case JSValue.Object(obj) => obj.get(propName)
                    case _ => JSValue.Undefined
                else
                  result
              case JSValue.Native(nativeFuncWrapper) =>
                // For native functions/constructors, also look up methods from Function.prototype
                val result = ctx.functionPrototype.get(propName)(using ctx)
                if result == JSValue.Undefined then
                  // Fall back to global Function object for backward compatibility
                  val funcObj = ctx.global.get("Function")
                  funcObj match
                    case JSValue.Object(obj) => obj.get(propName)
                    case _ => JSValue.Undefined
                else
                  result
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

          case Opcode.Rotate =>
            // Rotate top 3 elements: a b c -> b c a
            val a = stack(stackTop - 1)
            val b = stack(stackTop - 2)
            val c = stack(stackTop - 3)
            stack(stackTop - 1) = c
            stack(stackTop - 2) = a
            stack(stackTop - 3) = b
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
            // Stack layout: [value] (the function value created by GetConst)
            val funcValue = stack(stackTop - 1)
            stackTop -= 1

            // Store the function in global scope
            // GetConst already converted BytecodeFunction to JSValue.Function with captured closure
            ctx.globalScope.setVariable(funName, funcValue)
            pc += 1 + 4 + funName.length

          case Opcode.PutGlobal =>
            val varName = readString(bytecode, pc + 1)
            // Stack layout: [value]
            val value = stack(stackTop - 1)
            stackTop -= 1

            // DEBUG: Print what we're putting

            // Check if variable exists in closure first (for closures that modify captured variables)
            closure.get(varName) match
              case Some(varRef) =>
                // Found VarRef in closure - update it
                varRef.get match
                  case JSValue.GlobalRef(refName) =>
                    // GlobalRef inside VarRef: update global scope AND promote the value in VarRef
                    ctx.globalScope.setVariable(refName, value)
                    varRef.set(value)  // Promote from GlobalRef to actual value
                  case _ =>
                    // Regular value in VarRef, just update it
                    varRef.set(value)
              case None =>
                // Not in closure, store in global scope
                ctx.globalScope.setVariable(varName, value)
            pc += 1 + 4 + varName.length

          case Opcode.GetGlobal =>
            val varName = readString(bytecode, pc + 1)

            // DEBUG

            // Look up in closure first (for closures)
            val result = closure.get(varName) match
              case Some(varRef) =>
                // Found VarRef in closure - unwrap to get the value
                varRef.get match
                  case JSValue.GlobalRef(refName) =>
                    // GlobalRef inside VarRef: lazily look up from global scope
                    ctx.globalScope.getVariable(refName).orElse {
                      // Try global object (for built-ins like console)
                      val globalVal = ctx.global.get(refName)
                      if globalVal != JSValue.Undefined then Some(globalVal) else None
                    }.getOrElse {
                      // Try function
                      ctx.globalScope.getFunction(refName).getOrElse(JSValue.Undefined)
                    }
                  case value =>
                    // Regular value, return it
                    value
              case None =>
                // Not in closure, check global scope
                ctx.globalScope.getVariable(varName).orElse {
                  // Try global object (for built-ins like console)
                  val globalVal = ctx.global.get(varName)
                  if globalVal != JSValue.Undefined then Some(globalVal) else None
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
                // Since locals is now Array[VarRef], we can directly share references!
                val newClosure = mutable.Map.empty[String, JSValue.VarRef]

                for varName <- bcFunc.freeVars do
                  // First check if it's a parameter in the current (parent) function
                  val paramIndex = function.paramNames.indexOf(varName)

                  if paramIndex >= 0 && paramIndex < localsCount && paramIndex < locals.length then
                    // Variable is a parameter in the parent function
                    // Since locals contains VarRef, just share the reference!
                    // This enables mutation sharing - both parent and child see the same VarRef
                    newClosure(varName) = locals(paramIndex)
                  else
                    // Not a parameter - check if it's a local variable in the parent function
                    // Look in the current function's localVarNames
                    val localVarIndex = function.localVarNames.indexOf(varName)
                    if localVarIndex >= 0 then
                      // It's a local variable (var x = ...) in the parent function
                      // Calculate actual index in locals array (parameters come first, then locals)
                      val actualIndex = function.paramNames.length + localVarIndex
                      if actualIndex < locals.length then
                        // Share the VarRef for this local variable!
                        // Both parent and child functions now see the SAME VarRef
                        newClosure(varName) = locals(actualIndex)
                      else
                        // Fallback to GlobalRef
                        newClosure(varName) = new JSValue.VarRef(JSValue.GlobalRef(varName))
                    else
                      // Not a local parameter, check parent's closure (the 'closure' parameter)
                      val fromClosure = closure.get(varName)
                      if fromClosure.isDefined then
                        // Share the same VarRef from parent closure (this enables mutation sharing!)
                        newClosure(varName) = fromClosure.get
                      else
                        // Not in parent's closure - use GlobalRef for lazy lookup from global scope
                        newClosure(varName) = new JSValue.VarRef(JSValue.GlobalRef(varName))

                JSValue.Function(
                  name = bcFunc.name,
                  bytecode = bcFunc.bytecode,
                  constants = bcFunc.constants,
                  stackSize = bcFunc.stackSize,
                  closure = newClosure,
                  paramNames = bcFunc.paramNames,  // Copy paramNames for nested closures
                  localVarNames = bcFunc.localVarNames,  // Copy localVarNames for nested closures
                  parentLocalVarNames = function.localVarNames  // Pass parent's localVarNames for capture
                )
              case jsValue: JSValue =>
                jsValue
              case _ =>
                JSValue.Undefined

            stack(stackTop) = value
            stackTop += 1
            pc += 5

          case Opcode.EnterScope =>
            val scopeIndex = readInt32(bytecode, pc + 1)
            // Enter a new block scope - for now, this is a no-op
            // The scope tracking is handled at compile time by the Scope class
            // In the future, this could be used for runtime scope validation
            pc += 5

          case Opcode.LeaveScope =>
            val scopeIndex = readInt32(bytecode, pc + 1)
            // Leave a block scope - for now, this is a no-op
            // The scope tracking is handled at compile time by the Scope class
            // In the future, this could be used for runtime scope validation
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
      // If both are strings, do lexicographic comparison
      a.toString.compareTo(b.toString).toDouble
    case _ =>
      // Otherwise, do numeric comparison
      val na = a.toNumber
      val nb = b.toNumber
      if na.isNaN || nb.isNaN then Double.NaN
      else na - nb

  private def looseEqual(a: JSValue, b: JSValue): Boolean = (a, b) match
    case (JSValue.Undefined, JSValue.Null) => true
    case (JSValue.Null, JSValue.Undefined) => true
    case (_: JSValue.JSStr, _: JSValue.JSStr) => a.toString == b.toString
    case (_: JSValue.Bool, _) | (_, _: JSValue.Bool) => a.toNumber == b.toNumber
    case (_: JSValue.JSStr, _: JSValue.Int32) => a.toNumber == b.toNumber
    case (_: JSValue.JSStr, _: JSValue.Float64) => a.toNumber == b.toNumber
    case (_: JSValue.Int32, _: JSValue.JSStr) => a.toNumber == b.toNumber
    case (_: JSValue.Float64, _: JSValue.JSStr) => a.toNumber == b.toNumber
    case (_: JSValue.Int32, _) | (_, _: JSValue.Int32) => a.toNumber == b.toNumber
    case (_: JSValue.Float64, _) | (_, _: JSValue.Float64) => a.toNumber == b.toNumber
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

  // JavaScript's ToInt32 abstract operation
  private def toInt32(v: JSValue): Int =
    val num = v.toNumber
    if num.isNaN || num.isInfinite then
      0  // JavaScript: ToInt32(NaN) = ToInt32(±Infinity) = +0
    else
      // Modulo 2^32, then convert to signed 32-bit
      val int32 = num.toLong % 4294967296L
      if int32 >= 2147483648L then
        (int32 - 4294967296L).toInt
      else
        int32.toInt

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
