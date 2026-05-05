# Scala 3 Features in QuickJS-Scala: A Practical Tutorial

> A guided tour through the Scala 3 language features used to build a production-grade JavaScript engine on the JVM.

---

## Table of Contents

1. [Introduction](#introduction)
2. [Enums: Type-Safe ADTs](#1-enums-type-safe-adts)
3. [Union Types: `A | B`](#2-union-types-a--b)
4. [Contextual Abstractions: `given` / `using` / `summon`](#3-contextual-abstractions-given--using--summon)
5. [Sealed Traits & Exhaustive Pattern Matching](#4-sealed-traits--exhaustive-pattern-matching)
6. [New Control Flow Syntax](#5-new-control-flow-syntax)
7. [Annotations: `@switch`, `@tailrec`, `@targetName`](#6-annotations-switch-tailrec-targetname)
8. [Inline Methods](#7-inline-methods)
9. [Self-Type Annotations on Traits](#8-self-type-annotations-on-traits)
10. [`Either` for Two-Case Branching](#9-either-for-two-case-branching)
11. [`scala.compiletime.uninitialized`](#10-scalacompiletimeuninitialized)
12. [Backtick Identifiers for Java Interop](#11-backtick-identifiers-for-java-interop)
13. [Scala.js](#12-scalajs)
14. [Multi-Project sbt Builds](#13-multi-project-sbt-builds)
15. [Putting It All Together](#14-putting-it-all-together)

---

## Introduction

QuickJS-Scala is a JavaScript engine written in **Scala 3.7.4** for the JVM. It implements a stack-based bytecode interpreter, a recursive-descent parser, and a full ES2024+ standard library — all in idiomatic Scala 3. This tutorial walks through every language feature used in the codebase, explaining *what* it is, *why* it's used here, and showing real code from the project.

The codebase is ~20,000 lines of Scala across 6 subprojects:

| Module | Purpose | Lines (approx.) |
|---|---|---|
| `core` | Type system, object model, runtime context | ~1,400 |
| `parser` | Lexer + recursive-descent parser + AST | ~3,700 |
| `compiler` | AST → bytecode compilation | ~4,500 |
| `runtime` | Interpreter, stdlib builtins, REPL, tracing | ~9,600 |
| `stdlib` | Tests, runners, JSON, console | ~2,000 |
| `web` | Scala.js web frontend for tracing | ~400 |

---

## 1. Enums: Type-Safe ADTs

Scala 3 `enum` is one of the most impactful new features. It replaces both Scala 2 `sealed abstract class + case object` hierarchies and Java-style enums with a unified, concise syntax.

### 1.1 Simple Enums (Sum Types)

The simplest use: a fixed set of named constants.

**File:** `core/src/main/scala/quickjs/runtime/ErrorType.scala`

```scala
enum ErrorType {
  case TypeError
  case ReferenceError
  case SyntaxError
  case RangeError
  case Error

  /** Get the error name as a string. */
  def name: String = this.toString
}
```

Each case is a singleton value. Methods can be defined directly in the enum body. Used in the companion:

```scala
object ErrorType {
  def fromString(name: String): ErrorType =
    name match {
      case "TypeError"      => ErrorType.TypeError
      case "ReferenceError" => ErrorType.ReferenceError
      // ...
    }
}
```

### 1.2 Parameterized Enums (GADT-like)

Enums can carry data, making them algebraic data types.

**File:** `compiler/src/main/scala/quickjs/bytecode/Opcode.scala`

```scala
enum Opcode(val code: Int) {
  case Invalid       extends Opcode(0)
  case Nop           extends Opcode(1)
  case PushI32       extends Opcode(2)   // push 32-bit integer constant
  case PushFloat64   extends Opcode(3)   // push 64-bit float constant
  case Add           extends Opcode(21)  // a + b
  case Sub           extends Opcode(22)  // a - b
  case Call          extends Opcode(49)  // call function
  // ... 92 opcodes total
}
```

Each opcode carries an integer byte representation. The companion builds a fast lookup table:

```scala
object Opcode {
  private val MaxCode: Int = values.map(_.code).max
  val lookup: Array[Opcode | Null] = {
    val arr = new Array[Opcode | Null](MaxCode + 1)
    values.foreach(op => arr(op.code) = op)
    arr
  }

  def fromCode(code: Int): Option[Opcode] =
    if code >= 0 && code < lookup.length then Option(lookup(code))
    else None
}
```

`values` is an auto-generated method on all enums that returns all cases in definition order.

### 1.3 Enums with Methods

The `BinaryOpcode` enum shows how to add methods to each case and to the enum as a whole:

**File:** `compiler/src/main/scala/quickjs/bytecode/Instruction.scala`

```scala
enum BinaryOpcode {
  case Comma
  case Add, Sub, Mul, Div, Mod, Pow
  case Lt, Lte, Gt, Gte, Eq, Neq, StrictEq, StrictNeq
  case And, Or, Xor, Shl, Sar, Shr
  case LogicalAnd, LogicalOr
  case Instanceof, In

  def toOpcode: Opcode = this match {
    case Add        => Opcode.Add
    case Sub        => Opcode.Sub
    case Mul        => Opcode.Mul
    // ...
  }
}
```

### When to Use Enums

Use `enum` when:
- You have a fixed set of alternatives (opcodes, error types, token kinds, variable kinds)
- You want exhaustive pattern matching (the compiler warns on missing cases)
- You need the `values` auto-generated list

---

## 2. Union Types: `A | B`

Union types are one of Scala 3's most practical improvements. They replace the need for `Either` in many cases and read much more naturally.

### 2.1 Nullable Types Without `Option`

The most common pattern in the codebase is `T | Null` — a clean replacement for `Option[T]` when the semantics are genuinely "this value or null."

**File:** `core/src/main/scala/quickjs/objmodel/JSObject.scala`

```scala
final class JSObject private (
    private var properties: mutable.LinkedHashMap[String, JSValue],
    private var propertyAttributes: ...,
    private var prototype: JSObject | Null,  // ← Union type for nullable
    private var extensible: Boolean,
    ...
) {
  def getPrototype: JSObject | Null = prototype

  def setPrototype(proto: JSObject | Null): Unit =
    if !hasImmutablePrototype then prototype = proto
}
```

This is clearer than `Option[JSObject]` because the null case literally *is* null (matching JavaScript's semantics), and it interoperates cleanly with Java APIs.

### 2.2 Discriminated Unions in the AST

Union types are the backbone of the AST, where expression and pattern types mix:

**File:** `parser/src/main/scala/quickjs/ast/AST.scala`

```scala
// Assignment target can be an expression or a destructuring pattern
case class AssignmentExpression(
    left: Expression | BindingPattern,  // ← Union type
    right: Expression,
    span: Span
) extends Expression

// Arrow function body can be a single expression or a block
case class ArrowFunctionExpression(
    params: immutable.Seq[BindingPattern],
    body: Either[Expression, BlockStatement],  // ← Either for unambiguous cases
    isAsync: Boolean = false,
    strict: Boolean = false,
    span: Span
) extends Expression

// Optional superclass
case class ClassDeclaration(
    id: Identifier,
    superClass: Expression | Null,  // ← Union with Null
    body: ClassBody,
    span: Span
) extends Declaration
```

The compiler processes these with pattern matching:

```scala
// From Compiler.scala
arrow.body match {
  case Left(expr)  => // Concise arrow body: compile expression, add implicit return
    compileExpression(expr, instructions, constants)
    // ...
  case Right(block) => // Block body
    compileBlock(block, instructions, constants)
    // ...
}
```

### 2.3 Sentinel Arrays

The Opcode lookup table uses a nullable array for O(1) dispatch:

```scala
val lookup: Array[Opcode | Null] = {
  val arr = new Array[Opcode | Null](MaxCode + 1)  // ← Array of union type
  values.foreach(op => arr(op.code) = op)
  arr
}
```

### Union Type Best Practices

- Use `T | Null` when the value is genuinely nullable (matching Java interop or JS semantics)
- Use `Either[A, B]` when the two types carry distinct semantics and you want to pattern match on `Left`/`Right`
- Use union types in data structures where multiple AST node types can appear

---

## 3. Contextual Abstractions: `given` / `using` / `summon`

Scala 3 completely reimagines implicits. Instead of the old `implicit` keyword, we now have three clear concepts:

| Keyword | Purpose |
|---|---|
| `using` | Declare a parameter as contextual |
| `given` | Define a contextual value (in scope) |
| `summon[T]` | Explicitly retrieve a contextual value |

### 3.1 The Pervasive Pattern: `(using ctx: JSContext)`

Nearly every method that touches JavaScript objects needs a context. Instead of threading it manually, it's declared as a `using` parameter:

**File:** `core/src/main/scala/quickjs/objmodel/JSObject.scala`

```scala
def get(key: String)(using ctx: JSContext): JSValue =
  properties.get(key) match {
    case Some(value) => value
    case None =>
      prototype match {
        case null  => JSValue.Undefined
        case proto => proto.get(key)  // ctx is passed implicitly
      }
  }
```

The caller doesn't need to pass `ctx` explicitly when it's in scope:

```scala
given ctx: JSContext = JSContext(runtime)
// Now any call to obj.get("foo") automatically uses ctx
val result = obj.get("foo")  // ctx passed implicitly
```

### 3.2 Defining Contextual Instances

In tests and initializers, `given` makes a value available contextually:

**File:** `stdlib/src/test/scala/quickjs/stdlib/QuickJSJavaScriptTest.scala` (representative)

```scala
given JSRuntime = JSRuntime()
given JSContext = JSContext(summon[JSRuntime])
```

The pattern `JSContext(summon[JSRuntime])` is common: the runtime is summoned from the implicit scope to construct the context.

### 3.3 Aliasing Context in Method Bodies

When a method receives `ctx` as a `using` parameter but needs to call other methods that require it:

**File:** `runtime/src/main/scala/quickjs/runtime/builtins/ReflectBuiltins.scala`

```scala
object ReflectBuiltins {
  def initialize(ctx: JSContext): Unit = {
    given JSContext = ctx  // ← Make ctx available to all calls in this block

    val reflectGet = NativeFunction(
      name = "get",
      impl = (args, innerCtx) =>
        given JSContext = innerCtx  // ← The NativeFunction provides its own ctx
        obj.getPropertyDescriptorWithOwner(propertyKey) match {
          case Some((_, _, attrs)) if attrs.getter.isDefined =>
            // ctx is automatically available
            attrs.getter.get match {
              case func: JSValue.Function =>
                Interpreter().call(
                  functionToBytecode(func),
                  receiver,
                  Array.empty,
                  func.closure
                )
              // ...
            }
        }
    )
  }
}
```

### 3.4 The `summon` Function

`summon[T]` is the modern replacement for `implicitly[T]`:

```scala
given JSContext = JSContext(summon[JSRuntime])
// Equivalent to the old: implicit val ctx = JSContext(implicitly[JSRuntime])
```

It's also used inside builtin implementations where the context might come from different scopes:

**File:** `stdlib/src/main/scala/quickjs/stdlib/JSON.scala`

```scala
given JSContext = summon[JSContext]  // re-summon when shadowing might occur
```

### Why This Matters

In a JavaScript engine, **almost everything needs a context**: property access, type coercion, error creation, prototype chain walking, microtask scheduling. Without contextual abstractions, every method signature would be polluted with an extra `ctx` parameter, and every call site would pass it manually. Scala 3's `given`/`using` makes this invisible while maintaining full type safety.

---

## 4. Sealed Traits & Exhaustive Pattern Matching

### 4.1 The Core: `sealed trait JSValue`

The heart of the engine is a tagged union representing all JavaScript values:

**File:** `core/src/main/scala/quickjs/value/JSValue.scala`

```scala
sealed trait JSValue {
  def tag: Tag

  def toBoolean: Boolean = this match {
    case JSValue.Undefined | JSValue.Null => false
    case JSValue.Bool(b)                  => b
    case JSValue.Int32(i)                 => i != 0
    case JSValue.Float64(d)               => d != 0.0 && !d.isNaN
    case JSValue.BigInt(b)                => b.signum() != 0
    case JSValue.JSStr(s)                 => s.nonEmpty
    case _                                => true
  }
}

object JSValue {
  // Singleton values
  case object Undefined extends JSValue { def tag = Tag.Undefined }
  case object Null extends JSValue { def tag = Tag.Null }
  case object Uninitialized extends JSValue { def tag = Tag.Undefined }

  // Inline values (JVM primitives)
  final case class Bool(value: scala.Boolean) extends JSValue { def tag = Tag.Bool }
  final case class Int32(value: scala.Int) extends JSValue { def tag = Tag.Int32 }
  final case class Float64(value: scala.Double) extends JSValue { def tag = Tag.Float64 }

  // Reference values
  final case class JSStr(value: java.lang.String) extends JSValue { def tag = Tag.String }
  final case class Symbol(value: Int) extends JSValue { def tag = Tag.Symbol }
  final case class BigInt(value: java.math.BigInteger) extends JSValue { def tag = Tag.BigInt }
  final case class Object(value: quickjs.objmodel.JSObject) extends JSValue { def tag = Tag.Object }
  final case class JSArrayVal(value: quickjs.objmodel.JSArray) extends JSValue { def tag = Tag.Object }

  // Complex subtypes
  final case class Function(
      name: String, bytecode: Array[Byte], constants: Array[AnyRef],
      stackSize: Int, closure: mutable.Map[String, VarRef],
      paramNames: Array[String], localVarNames: Array[String],
      /* ... 15 fields total ... */
  ) extends JSValue { def tag = Tag.Function }

  final case class Native(func: AnyRef) extends JSValue { def tag = Tag.Function }
  final case class Generator(/* ... */) extends JSValue { def tag = Tag.Generator }
  final case class Promise(/* ... */) extends JSValue { def tag = Tag.Promise }
}
```

Because `JSValue` is `sealed`, the compiler knows all possible subtypes and can warn on non-exhaustive matches.

### 4.2 AST: `sealed trait AST`

**File:** `parser/src/main/scala/quickjs/ast/AST.scala`

```scala
sealed trait AST { val span: Span }
sealed trait Expression extends AST
sealed trait BindingPattern extends AST
sealed trait ClassElement extends AST

case class Literal(value: JSValue, span: Span) extends Expression
case class Identifier(name: String, span: Span) extends Expression, BindingPattern
case class BinaryExpression(operator: BinaryOperator, left: Expression,
    right: Expression, span: Span) extends Expression
// ... 40+ AST nodes
```

Note the compound `extends Expression, BindingPattern` — this is a Scala 3 feature where a class can extend multiple traits at once.

### 4.3 Pattern Matching in the Interpreter

The bytecode loop uses `@switch`-annotated matches for dispatch speed:

**File:** `runtime/src/main/scala/quickjs/interpreter/BytecodeLoop.scala`

```scala
private def run(): JSValue = {
  while true do {
    val op = Opcode.fromCode(bytecode(pc) & 0xff).getOrElse(Opcode.Invalid)
    (op: @switch) match {
      case Opcode.PushI32       => /* ... */
      case Opcode.Add           =>
        val b = stack(stackTop - 1); stackTop -= 1
        val a = stack(stackTop - 1)
        stack(stackTop - 1) = JSValue.add(a, b)
        pc += 1
      case Opcode.Call          => doCall()
      case Opcode.Return        => return stack(stackTop - 1)
      case Opcode.ReturnUndef   => return JSValue.Undefined
      // ... all 92 opcodes
    }
  }
}
```

### 4.4 Tuple Pattern Matching

The arithmetic operations use tuple deconstruction for elegant dispatch:

```scala
def add(a: JSValue, b: JSValue): JSValue = (a, b) match {
  case (BigInt(x), BigInt(y))      => BigInt(x.add(y))
  case (BigInt(_), _)              => throw new RuntimeException("TypeError: Cannot mix BigInt")
  case (_, BigInt(_))              => throw new RuntimeException("TypeError: Cannot mix BigInt")
  case (Int32(x), Int32(y))        => fromLong(x.toLong + y.toLong)
  case (Float64(x), Int32(y))      => Float64(x + y.toDouble)
  case (JSStr(x), _)               => JSStr(x + b.toString)
  case (_, JSStr(y))               => JSStr(a.toString + y)
  case _                           => fromDouble(a.toNumber + b.toNumber)
}
```

---

## 5. New Control Flow Syntax

Scala 3 introduced optional brace-less syntax with significant indentation. QuickJS-Scala uses **traditional braces**, but adopts the new keyword-based syntax:

### 5.1 `if ... then ... else`

```scala
// Old Scala 2: if (cond) expr else expr
// Scala 3:
if condition then expr1 else expr2
if s.isEmpty || s.trim.isEmpty then 0.0
else if s.startsWith("0x") || s.startsWith("0X") then
  try java.lang.Integer.decode(s).toDouble
  catch case _: NumberFormatException => Double.NaN
else try s.toDouble
  catch case _: NumberFormatException => Double.NaN
```

### 5.2 `while ... do`

The `do` keyword replaces the old `{}` for while loop bodies:

```scala
// Old Scala 2: while (cond) { body }
// Scala 3:
while current != null do {
  if current.eq(target) then return true
  current = current.getPrototype
}

while i < actualDelete do {
  // ...
  i += 1
}
```

### 5.3 `for ... do`

For comprehensions use `do` for side-effecting loops (not `yield`):

```scala
// Side-effecting loop
for (key, attrs) <- propertyAttributes do
  if attrs.getter.isEmpty && attrs.setter.isEmpty then
    propertyAttributes(key) = attrs.copy(writable = false, configurable = false)

// Initialization
for i <- 0 until 256 do locals(i) = new JSValue.VarRef(JSValue.Undefined)
```

### 5.4 Try/Catch

The syntax stays familiar but works with the new control keywords:

```scala
try body
catch {
  case jsEx: JSException => handle(jsEx.getValue)
  case ex: RuntimeException => handleRuntime(ex)
}
finally cleanup()
```

---

## 6. Annotations: `@switch`, `@tailrec`, `@targetName`

### 6.1 `@switch` — Optimized Match Dispatch

The `@switch` annotation tells the compiler to compile a match on an integer as a `tableswitch` or `lookupswitch` JVM instruction, avoiding the slower chain of `if/else` comparisons:

**File:** `runtime/src/main/scala/quickjs/interpreter/BytecodeLoop.scala`

```scala
import scala.annotation.switch

// In the main dispatch loop:
val op = bytecode(pc) & 0xff
(op: @switch) match {
  case 2  => handlePushI32()     // Opcode.PushI32.code
  case 21 => handleAdd()         // Opcode.Add.code
  case 49 => doCall()            // Opcode.Call.code
  // ...
}
```

This is critical for the bytecode interpreter's performance — this match is hit millions of times per second.

### 6.2 `@tailrec` — Guaranteed Tail Call Optimization

**File:** `parser/src/main/scala/quickjs/lexer/Lexer.scala`

```scala
import scala.annotation.tailrec

@tailrec
private def readIdentifierTail(sb: StringBuilder): String = {
  if Character.isUnicodeIdentifierPart(ch) || ch == '$' || ch == '_' || ch == '\\' then {
    sb.append(ch)
    advance()
    readIdentifierTail(sb)  // ← Must be in tail position
  } else sb.toString
}
```

The annotation causes a compile error if the recursion cannot be optimized.

### 6.3 `@targetName` — JVM-Compatible Names

When a Scala method name would produce an illegal JVM identifier, `@targetName` provides an alternative:

**File:** `core/src/main/scala/quickjs/value/JSValue.scala`

```scala
import scala.annotation.targetName

@targetName("add")
def add(a: JSValue, b: JSValue): JSValue = (a, b) match { /* ... */ }

@targetName("subtract")
def subtract(a: JSValue, b: JSValue): JSValue = /* ... */

@targetName("multiply")
def multiply(a: JSValue, b: JSValue): JSValue = /* ... */

@targetName("divide")
def divide(a: JSValue, b: JSValue): JSValue = /* ... */
```

This is particularly useful when overloading methods that need specific JVM signatures.

---

## 7. Inline Methods

Scala 3 `inline` is a soft modifier that guarantees the method body is inlined at every call site:

**File:** `runtime/src/main/scala/quickjs/interpreter/BytecodeLoop.scala`

```scala
private inline def stringOpSize(s: String): Int =
  4 + s.getBytes(java.nio.charset.StandardCharsets.UTF_8).length
```

This is used for small, frequently-called helper methods where the overhead of a method call would be noticeable. The `inline` modifier in Scala 3 is **guaranteed** (unlike `@inline` in Scala 2 which was a hint).

---

## 8. Self-Type Annotations on Traits

Self-types constrain which classes a trait can be mixed into:

**File:** `runtime/src/main/scala/quickjs/interpreter/PropertyAccess.scala`

```scala
private[interpreter] trait PropertyAccess {
  self: Interpreter =>  // ← This trait can ONLY be mixed into Interpreter

  def callAccessor(
      funcValue: JSValue,
      thisValue: JSValue,
      args: Array[JSValue],
      withStack: List[quickjs.objmodel.JSObject],
      trace: TraceRecorder
  )(using ctx: JSContext): JSValue =
    funcValue match {
      case func: JSValue.Function =>
        val bcFunc = new BytecodeFunction(/* ... */)
        // Can access Interpreter methods via `self`
        self.call(bcFunc, thisValue, args, func.closure, withObjects = withStack, trace = trace)
      // ...
    }
}
```

The `Interpreter` class mixes in this trait:

```scala
final class Interpreter extends PropertyAccess {  // self-type is satisfied
  // ... PropertyAccess methods are available here
}
```

This pattern is used to split the ~600-line `Interpreter` into focused, testable traits (`PropertyAccess`, `GeneratorSupport`, `BytecodeLoop`) without losing access to interpreter internals.

---

## 9. `Either` for Two-Case Branching

While union types handle many nullable cases, `Either` is used when the two alternatives have distinct, named semantics:

**File:** `parser/src/main/scala/quickjs/ast/AST.scala`

```scala
case class ArrowFunctionExpression(
    params: immutable.Seq[BindingPattern],
    body: Either[Expression, BlockStatement],  // Left = concise, Right = block
    isAsync: Boolean = false,
    strict: Boolean = false,
    span: Span
) extends Expression
```

Processing in the compiler:

```scala
body match {
  case Left(expr) =>
    // Concise body: `x => x + 1`
    compileExpression(expr, instructions, constants)
    addReturnInstruction(instructions)
  case Right(block) =>
    // Block body: `x => { return x + 1; }`
    compileBlock(block, instructions, constants)
}
```

The `Left`/`Right` naming makes the meaning clear at every match site, whereas a union type like `Expression | BlockStatement` would lose this semantic labeling.

---

## 10. `scala.compiletime.uninitialized`

For lazy or delayed initialization where `null` would be inappropriate:

**File:** `core/src/main/scala/quickjs/runtime/JSContext.scala`

```scala
import scala.compiletime.uninitialized

final class JSContext(private val runtime: JSRuntime) {
  var objectPrototype: quickjs.objmodel.JSObject = uninitialized
  var functionPrototype: quickjs.objmodel.JSObject = uninitialized
  var arrayPrototype: quickjs.objmodel.JSObject = uninitialized
  var symbolPrototype: quickjs.objmodel.JSObject = uninitialized
  var mapPrototype: quickjs.objmodel.JSObject = uninitialized
  var setPrototype: quickjs.objmodel.JSObject = uninitialized
  var weakMapPrototype: quickjs.objmodel.JSObject = uninitialized
  var weakSetPrototype: quickjs.objmodel.JSObject = uninitialized
  var promisePrototype: quickjs.objmodel.JSObject = uninitialized

  initializeIntrinsics()  // Sets all the above to real values
}
```

`uninitialized` is a sentinel value (of type `Nothing`) that:
- Satisfies any type `T` (so it compiles)
- Causes a runtime error if accessed before assignment
- Is clearer than `null.asInstanceOf[T]` or `var x: T = _` (which only works for `AnyRef` subtypes)

---

## 11. Backtick Identifiers for Java Interop

When field names collide with Scala keywords, backticks provide an escape hatch:

**File:** `compiler/src/main/scala/quickjs/compiler/Compiler.scala`

```scala
// AST node MemberExpression has a field named "object"
// In Scala, "object" is a keyword, so we use backticks:
compileExpression(memberExpr.`object`, instructions, constants)
```

Without backticks, `memberExpr.object` would be a compile error.

---

## 12. Scala.js

The project includes a Scala.js frontend for visualizing the JavaScript execution trace:

**File:** `build.sbt`

```scala
lazy val webFrontend = project
  .in(file("web"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "quickjs-web",
    scalaVersion := scala3Version,
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("quickjs.web.TraceApp"),
    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "16.0.0",
      "org.scala-js" %%% "scalajs-dom" % "2.8.0"
    )
  )
```

Note the `%%%` (triple percent) for Scala.js cross-built dependencies — this selects the `_sjs1_3` artifact for Scala.js + Scala 3.

---

## 13. Multi-Project sbt Builds

The build uses sbt's multi-project features to create a clear dependency graph:

```scala
lazy val core = project
  .settings(name := "quickjs-core", /* ... */)

lazy val parser = project
  .dependsOn(core)
  .settings(name := "quickjs-parser", /* ... */)

lazy val compiler = project
  .dependsOn(core, parser)          // ← Multi-dependency
  .settings(name := "quickjs-compiler", /* ... */)

lazy val runtime = project
  .dependsOn(core, compiler)
  .settings(name := "quickjs-runtime", /* ... */)

lazy val stdlib = project
  .dependsOn(runtime)
  .settings(
    Compile / mainClass := Some("quickjs.stdlib.Main"),
    /* ... */
  )

lazy val runner = project
  .dependsOn(stdlib)
  .enablePlugins(AssemblyPlugin)  // ← Creates a fat JAR
  .settings(
    assembly / mainClass := Some("quickjs.stdlib.Runner"),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    }
  )
```

The dependency chain is: `core → parser → compiler → runtime → stdlib → runner`

---

## 14. Putting It All Together

Here's a complete example showing how these features compose in practice — evaluating `"hello".length`:

```scala
import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue

// Top-level given definitions
given rt: JSRuntime = JSRuntime()
given ctx: JSContext = JSContext(summon[JSRuntime])

// Initialize standard library (Object, String, etc.)
StdLib.initialize(ctx)  // Uses given ctx implicitly

def eval(source: String): JSValue =
  val lexer = Lexer(source)
  val tokens = lexer.tokenize()
  val parser = Parser(tokens)
  val ast = parser.parseScript()          // Returns Script (sealed trait AST)
  val compiler = Compiler()
  val bytecode = compiler.compileScript(ast)  // Returns BytecodeFunction
  val interpreter = Interpreter()
  interpreter.call(bytecode, JSValue.Undefined, Array.empty)

// Execute!
eval("'hello'.length")  // Returns JSValue.Int32(5)
```

### Feature Map

| Feature | Where Used | Why |
|---|---|---|
| `enum` | Opcodes, error types, tokens, AST node kinds | Exhaustive matching, self-documenting codes |
| Union types `A \| B` | Nullable fields, AST mixed nodes | Cleaner than `Option`, more precise than `Any` |
| `given`/`using` | Every method with `JSContext` | Eliminates manual context threading |
| `sealed trait` | JSValue, AST, Token families | Compiler-verified exhaustive matching |
| `@switch` | Bytecode dispatch loop | Performance (tableswitch JVM instruction) |
| `@tailrec` | Lexer identifier parsing | Guaranteed stack safety |
| `@targetName` | Arithmetic operators on JSValue | JVM method name compatibility |
| `inline def` | Hot-path helpers in BytecodeLoop | Zero-overhead abstraction |
| Self-types | PropertyAccess trait | Safe modularization of ~600-line class |
| `Either` | Arrow function bodies | Semantic clarity (concise vs block) |
| `uninitialized` | Prototype chain fields | Type-safe delayed initialization |
| Backtick ids | `memberExpr.\`object\`` | Java/JS keyword collision avoidance |
| `if/then/while/do` | Everywhere | Readable, modern syntax |
| Tuple destructuring | `(a, b) match { ... }` | Elegant multi-value matching |
| `final case class` | Value types, AST nodes | Immutable, structural equality, efficient |
| Scala.js | Web tracing frontend | Share code between JVM and browser |

---

## 15. What's NOT Used (Yet)

Some Scala 3 features that could benefit the codebase:

- **Opaque types** — Could wrap `Int` for symbol IDs or atom IDs with zero runtime overhead
- **Extension methods** — Could add `.toJS` on Scala collections
- **`export` clauses** — Could simplify the `StdLib` delegation pattern
- **Significant indentation** — The project uses braces throughout (comment in build.sbt shows `-no-indent` was considered)
- **Match types** — For type-level operations (not needed yet)
- **`transparent inline`** — For macro-like code generation
- **Polymorphic function types** — For generic higher-order operations
- **`derives`** — Auto-derivation of typeclass instances (not needed for simple case classes)

---

## Summary

QuickJS-Scala is a real-world showcase of Scala 3's power. The language features work together to create a codebase that is:

- **Safe**: Sealed traits and exhaustive matching prevent entire categories of bugs
- **Concise**: Contextual abstractions eliminate boilerplate at every level
- **Performant**: `@switch` and `inline` enable a fast bytecode interpreter
- **Maintainable**: `enum`, union types, and self-types enable clean, modular design

The most impactful upgrade from Scala 2 is arguably the `given`/`using` system — it's used on virtually every method that touches `JSContext`, and without it the codebase would be significantly more verbose and error-prone.

---

*Generated from QuickJS-Scala commit history as of May 2026. Project: <https://github.com/user/quickjs-scala>*
