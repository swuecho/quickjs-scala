# Native Function Delegation: How the VM Calls Scala

This document explains how QuickJS-Scala delegates JavaScript function calls to
native Scala implementations — the mechanism that powers all built-in objects
(`Object`, `Array`, `Promise`, `console.log`, etc.).

---

## Overview

The VM supports two kinds of callable values:

| Kind | Stored as | Execution |
|------|-----------|-----------|
| **Bytecode function** | `JSValue.Function` (holds bytecode, constants, closure) | Recursive VM invocation — the interpreter calls itself with a new frame |
| **Native function** | `JSValue.Native(AnyRef)` wrapping a `NativeFunction` or `NativeConstructor` | Direct Scala lambda invocation with no further interpretation |

This document focuses on the native path.

---

## Architecture (3 layers)

```
┌──────────────────────────────────────────────────────────────────┐
│  LAYER 1: Registration                                           │
│  Wrap Scala lambdas in NativeFunction / NativeConstructor,       │
│  store them as JSValue.Native on the global object or prototype. │
├──────────────────────────────────────────────────────────────────┤
│  LAYER 2: Compilation                                            │
│  Emit GetGlobal / GetProp / GetConst to push the function,       │
│  then Call / CallMethod / New to invoke it.                      │
├──────────────────────────────────────────────────────────────────┤
│  LAYER 3: Dispatch & Execution                                   │
│  BytecodeLoop.doCall() pattern-matches on JSValue.Native,        │
│  calls the Scala lambda directly through withNativeFrame().      │
└──────────────────────────────────────────────────────────────────┘
```

---

## Layer 1: Registration

### `NativeFunction` — regular functions

**File:** `runtime/src/main/scala/quickjs/value/NativeFunction.scala`

```scala
final case class NativeFunction(
  name: String,
  impl: (Array[JSValue], JSContext) => JSValue,  // (args, ctx) => result
  funcObj: JSObject = JSObject(),
  length: Int = 1
):
  def call(args: Array[JSValue])(using ctx: JSContext): JSValue = impl(args, ctx)
```

Used for: `console.log`, `parseInt`, `Array.prototype.push`, `JSON.parse`, etc.

### `NativeConstructor` — dual-mode constructors

**File:** `core/src/main/scala/quickjs/value/NativeConstructor.scala`

```scala
final case class NativeConstructor(
  name: String,
  callImpl: (Array[JSValue], JSContext) => JSValue,       // e.g. Object(42)
  constructImpl: (Array[JSValue], JSContext) => JSValue,   // e.g. new Object()
  prototype: JSObject,
  funcObj: JSObject = JSObject()
):
  def call(args: Array[JSValue])(using ctx: JSContext): JSValue = callImpl(args, ctx)
  def construct(args: Array[JSValue])(using ctx: JSContext): JSValue = constructImpl(args, ctx)
```

Used for: `Object`, `Array`, `String`, `Number`, `Promise`, `Map`, `Set`, `Error`, etc.

JavaScript constructors have two calling conventions:
- **Call mode:** `Object(42)` → coerces value to an object type
- **Construct mode:** `new Object()` → creates a new object with the constructor's prototype

`NativeConstructor` encodes both behaviors via separate lambdas.

### Registration pattern

```scala
// Example: registering global "print" (InternalHelpers.scala)
val printFunc = NativeFunction(
  name = "print",
  impl = (args, ctx) =>
    val msg = args.lift(1).map(_.toString).getOrElse("")
    System.out.println(msg)
    JSValue.Undefined
)
ctx.global.set("print", JSValue.Native(printFunc))
```

The lambda is stored inside `JSValue.Native`, which is the tagged-union variant:

```scala
// JSValue.scala
final case class Native(func: AnyRef) extends JSValue:
  def tag: Tag = Tag.Function
```

Note: the type is `AnyRef` to avoid circular dependencies between the `value` and
`runtime` modules. The runtime module defines `NativeFunction`; the core module
defines `JSValue`. `JSValue.Native` stores an opaque reference, and the
interpreter (in the runtime module) recovers the concrete type via pattern matching.

---

## Layer 2: Compilation

When the compiler encounters a call expression like `print("hello")`, it emits:

```
GetGlobal("print")     // push function value onto operand stack
PushJSStr("hello")     // push argument
Call(1)                // call with argc=1
```

The `Call` opcode is encoded as 5 bytes: `[opcode=49][argc: 4-byte int32]`.

### Call variants

| Opcode | Encoding | Usage |
|--------|----------|-------|
| `Call` | `49 + i32 argc` | Regular function call: `f(a, b)` |
| `CallMethod` | `64 + i32 argc` | Method call: `obj.f(a, b)` — `this` is the object before the function on the stack |
| `New` | `69 + i32 argc` | Constructor call: `new F(a, b)` |

### Stack layout at Call

```
Before Call(2):          After Call(2):
┌──────────────┐         ┌──────────────┐
│   func       │ ← top   │   result     │ ← top
├──────────────┤         └──────────────┘
│   arg0       │
├──────────────┤
│   arg1       │
└──────────────┘

The interpreter pops func + argc items, calls func(arg0, arg1), pushes result.
```

### CallMethod stack layout

```
Before CallMethod(1):    After CallMethod(1):
┌──────────────┐         ┌──────────────┐
│   method     │ ← top   │   result     │ ← top
├──────────────┤         └──────────────┘
│   this       │
├──────────────┤
│   arg0       │
└──────────────┘

this is passed as args(0) in the native call convention.
```

---

## Layer 3: Dispatch & Execution

### The dispatch point: `doCall()`

**File:** `runtime/src/main/scala/quickjs/interpreter/BytecodeLoop.scala` (line 228)

```scala
private def doCall(): Unit =
  val argc = readInt32(bytecode, pc + 1)
  val funcValue = stack(stackTop - argc - 1)
  val args = new Array[JSValue](argc)
  for i <- 0 until argc do args(i) = stack(stackTop - argc + i)
  stackTop -= (argc + 1)

  funcValue match
    // ── Bytecode function: recursive VM execution ──
    case func: JSValue.Function =>
      val bcFunc = new BytecodeFunction(...)
      val ret = interpreter.call(bcFunc, effectiveThis, args, func.closure, ...)
      stack(stackTop) = ret; stackTop += 1

    // ── Native function: direct Scala lambda call ──
    case JSValue.Native(nativeFuncWrapper) =>
      nativeFuncWrapper match
        case native: NativeFunction =>
          val ret = interpreter.withNativeFrame(native.name) {
            native.call(args)    // ← calls the Scala lambda!
          }
          stack(stackTop) = ret; stackTop += 1

        case constructor: NativeConstructor =>
          val ret = interpreter.withNativeFrame(constructor.name) {
            constructor.call(args)    // ← calls callImpl(args, ctx)
          }
          stack(stackTop) = ret; stackTop += 1

    // ── Error cases ──
    case JSValue.Undefined =>
      throw TypeError(s"Cannot call non-function value: undefined")
    case other =>
      throw TypeError(s"Cannot call non-function value: $other")
```

### The dispatch point: `doNew()`

For `new Constructor(args)`, the pattern is the same but calls `construct(args)`
instead of `call(args)`:

```scala
private def doNew(): Unit =
  ...
  constructorValue match
    case JSValue.Native(constructorWrapper) =>
      constructorWrapper match
        case constructor: NativeConstructor =>
          val ret = interpreter.withNativeFrame(constructor.name) {
            constructor.construct(args)  // ← constructImpl(args, ctx)
          }
    case func: JSValue.Function =>
      // Creates a new JSObject with func.prototype, then calls func as constructor
```

### Stack frame management: `withNativeFrame()`

**File:** `runtime/src/main/scala/quickjs/interpreter/Interpreter.scala`

Every native call is wrapped in `withNativeFrame`, which:

1. **Pushes a stack frame** on `ctx.stackTrace` for error reporting
2. **Catches `RuntimeException`** and converts to `JSException`:
   - Extracts the error message
   - Determines the error type (TypeError, RangeError, etc.)
   - Creates a proper JavaScript error object with stack trace
3. **Attaches stack traces** to error objects if the thrown value is an Error object

```scala
private[interpreter] def withNativeFrame[T](name: String)(body: => T)(using ctx: JSContext): T =
  ctx.withStackFrame(name, isNative = true) {
    try body
    catch
      case jsEx: JSException =>
        // Attach stack trace to Error objects
        throw jsEx
      case ex: RuntimeException =>
        // Convert Java exception to JSException with proper error object
        val err = runtimeExceptionToError(ex)
        throw new JSException(err)
  }
```

### Argument convention for method calls

When a native function is invoked as a method (e.g., `arr.push(42)` via
`CallMethod`), `this` is passed as `args(0)` and the real arguments start at
`args(1)`. The `BuiltinHelpers.nativeArgs` helper normalizes this:

```scala
def nativeArgs(args: Array[JSValue]): (JSValue, Array[JSValue]) =
  if args.isEmpty then (JSValue.Undefined, Array.empty)
  else (args(0), args.drop(1))  // (this, realArgs)
```

---

## Complete End-to-End Example: `print("hello")`

```
JavaScript source:        print("hello")
                                │
┌───────────────────────────────┘
│ 1. REGISTRATION (at startup)
│    InternalHelpers.scala:
│      val printFunc = NativeFunction("print",
│        impl = (args, ctx) => {
│          System.out.println(args.lift(1).map(_.toString).getOrElse(""))
│          JSValue.Undefined
│        })
│      ctx.global.set("print", JSValue.Native(printFunc))
│
│ 2. PARSING
│    Parser.scala:
│      CallExpression(
│        callee = Identifier("print"),
│        arguments = Seq(Literal(JSStr("hello")))
│      )
│
│ 3. COMPILATION
│    Compiler.scala:
│      emit(GetGlobal("print"))     → [0x38]["print"(5 bytes)]  → 6 bytes
│      emit(PushJSStr("hello"))    → [push str op] + ["hello"]  → varies
│      emit(Call(1))               → [0x31][0x00 0x00 0x00 0x01] → 5 bytes
│
│ 4. INTERPRETER (BytecodeLoop)
│
│    GetGlobal("print"):
│      ctx.global.get("print") → JSValue.Native(NativeFunction("print", lambda))
│      stack.push(result)
│      pc += 6
│
│    PushJSStr("hello"):
│      stack.push(JSValue.JSStr("hello"))
│      pc += ...
│
│    Call(1):
│      argc = 1
│      funcValue = stack.pop()  → JSValue.Native(...)
│      args(0)    = stack.pop() → JSValue.JSStr("hello")
│
│      match funcValue:
│        case JSValue.Native(nf: NativeFunction) =>
│          withNativeFrame("print") {
│            nf.impl(Array(JSStr("hello")), ctx)  // ← SCALA LAMBDA EXECUTES
│          }
│
│      stack.push(JSValue.Undefined)
│      pc += 5
│
│ 5. SCALA EXECUTION
│    lambda: (args, ctx) =>
│      val msg = args.lift(1).map(_.toString)  // "hello"
│      System.out.println("hello")              // prints to stdout
│      JSValue.Undefined                        // return value
│
│ 6. RESULT
│    Operand stack: [Undefined]
│    Program continues to next instruction...
```

---

## Constructor Example: `new Array(3)`

```
JavaScript:     new Array(3)
                    │
Compiler emits:  GetGlobal("Array"), PushI32(3), New(1)
                    │
                    ▼
doNew():         pops "Array" → JSValue.Native(NativeConstructor("Array", ...))
                 pops 3 → JSValue.Int32(3)
                    │
                 match:
                   case JSValue.Native(ctor: NativeConstructor) =>
                     ctor.constructImpl(Array(Int32(3)), ctx)
                       │
                       ▼
                     Creates new JSArray with length 3
                     Returns JSValue.JSArrayVal(newArray)
```

---

## Key Design Decisions

### 1. `AnyRef` erasure in `JSValue.Native`

`JSValue.Native(func: AnyRef)` uses `AnyRef` instead of a sealed trait to avoid
circular module dependencies. The `value` package (in `core/`) can't import
`NativeFunction` (in `runtime/`). The runtime module recovers the concrete type
via pattern matching:

```scala
case JSValue.Native(wrapper) =>
  wrapper match
    case nf: NativeFunction      => ...
    case nc: NativeConstructor   => ...
```

### 2. No JNI, no reflection

The delegation is a pure Scala function call. The `impl` lambda captures any
state it needs (e.g., references to `ctx.global`, prototype objects). There is
no serialization, no class loading, no method resolution at runtime.

### 3. `NativeConstructor.call()` vs `NativeConstructor.construct()`

JavaScript constructors must work in both call and construct modes with
different semantics:
- `Object(null)` → returns `{}` (empty object)
- `new Object(null)` → returns `{}` (empty object)
- `Array(3)` → returns `[empty × 3]` (array with length 3)
- `new Array(3)` → returns `[empty × 3]` (same, but different spec path)

The two-lambda design lets each mode implement its own specification logic.

### 4. Stack trace integration

`withNativeFrame` ensures native function calls appear in stack traces just
like bytecode functions. If a native function throws (e.g., `TypeError` from a
built-in), the error object gets a proper `.stack` property.

---

## Files Involved

| File | Role |
|------|------|
| `core/.../value/JSValue.scala` | `JSValue.Native(AnyRef)` tagged union variant |
| `core/.../value/NativeConstructor.scala` | `NativeConstructor` case class with `callImpl` + `constructImpl` |
| `runtime/.../value/NativeFunction.scala` | `NativeFunction` case class with `impl` lambda |
| `runtime/.../interpreter/BytecodeLoop.scala` | `doCall()`, `doNew()`, `doCallMethod()` dispatch |
| `runtime/.../interpreter/Interpreter.scala` | `withNativeFrame()`, `call()` entry point |
| `runtime/.../builtins/BuiltinHelpers.scala` | `nativeArgs()`, `functionToBytecode()`, descriptor parsing |
| `runtime/.../builtins/*.scala` | All built-in registration (Object, Array, Promise, etc.) |
| `compiler/.../bytecode/Opcode.scala` | `Call`, `CallMethod`, `New` opcode definitions |
