# Scala 3 Enums: Complete Guide

**Date**: 2025-12-29
**QuickJS-Scala Version**: Phase 2+ complete

---

## Overview

This document provides a comprehensive guide to Scala 3 enums, using examples from the QuickJS-Scala codebase. Enums are a powerful feature for representing fixed sets of values, with or without parameters.

**What are enums?**
- A type-safe way to represent a fixed set of constants
- Pattern matching with exhaustivity checking
- Can carry data (parameters)
- Auto-generated helper methods

---

## Table of Contents

1. [Basic Enums (No Parameters)](#1-basic-enums-no-parameters)
2. [Enums with Parameters](#2-enums-with-parameters)
3. [Mixed Enums (Some With Parameters)](#3-mixed-enums-some-with-parameters)
4. [Pattern Matching](#4-pattern-matching)
5. [Auto-Generated Methods](#5-auto-generated-methods)
6. [Companion Object Methods](#6-companion-object-methods)
7. [Enums vs Sealed Traits](#7-enums-vs-sealed-traits)
8. [Real-World Examples from QuickJS-Scala](#8-real-world-examples-from-quickjs-scala)
9. [Best Practices](#9-best-practices)
10. [Common Pitfalls](#10-common-pitfalls)

---

## 1. Basic Enums (No Parameters)

The simplest enum is just a list of named constants.

### Syntax

```scala
enum EnumName:
  case CaseName1, CaseName2, CaseName3
```

### Example from QuickJS-Scala

**File**: `/parser/src/main/scala/quickjs/lexer/Token.scala`

```scala
enum Keyword:
  case Var, Let, Const
  case If, Else
  case For, While, Do, Break, Continue
  case Switch, Case, Default
  case Return, Function, New
  case Class, Extends, Super
  case Try, Catch, Finally, Throw
  case With
  case Import, Export, From, As
  case True, False, Null, Undefined
  case This, Typeof, Instanceof, In, Delete, Void
```

**File**: `/parser/src/main/scala/quickjs/ast/AST.scala`

```scala
enum VariableKind:
  case Var, Let, Const

enum PropertyKind:
  case Value   // {a: 1}
  case Getter  // {get a() { return 1; }}
  case Setter  // {set a(v) { x = v; }}
  case Method  // {a() { return 1; }}
```

### Usage

```scala
// Create enum values
val kind: VariableKind = VariableKind.Let

// Pattern matching
kind match
  case VariableKind.Var => println("var keyword")
  case VariableKind.Let => println("let keyword")
  case VariableKind.Const => println("const keyword")

// Compare values
if kind == VariableKind.Let then
  println("Block-scoped variable")

// Get string representation
println(kind.toString)  // "Let"
```

---

## 2. Enums with Parameters

Enums can have parameters, allowing each case to carry data.

### Syntax

```scala
enum EnumName(param: Type):
  case CaseName1 extends EnumName(value1)
  case CaseName2 extends EnumName(value2)
```

### Example from QuickJS-Scala

**File**: `/compiler/src/main/scala/quickjs/bytecode/Opcode.scala`

```scala
enum Opcode(val code: Int):
  // Invalid / nop
  case Invalid extends Opcode(0)
  case Nop extends Opcode(1)

  // Stack manipulation
  case PushI32 extends Opcode(2)       // push 32-bit integer constant
  case PushFloat64 extends Opcode(3)   // push 64-bit float constant
  case PushUndefined extends Opcode(4)
  case PushNull extends Opcode(5)
  case PushTrue extends Opcode(6)
  case PushFalse extends Opcode(7)

  // Arithmetic
  case Add extends Opcode(21)
  case Sub extends Opcode(22)
  case Mul extends Opcode(23)
  case Div extends Opcode(24)
  // ... more cases
```

### Usage

```scala
// Access parameter
val op = Opcode.PushI32
println(op.code)  // prints: 2

// Pattern matching on parameter
op match
  case Opcode.PushI32 => println("Push 32-bit integer")
  case Opcode.PushFloat64 => println("Push 64-bit float")
  case _ => println("Other opcode")

// Find by parameter value
def findOpcode(code: Int): Option[Opcode] =
  Opcode.values.find(_.code == code)

findOpcode(2)  // Some(PushI32)
findOpcode(999)  // None
```

---

## 3. Mixed Enums (Some With Parameters)

Enums can have a mix of cases with and without parameters.

### Syntax

```scala
enum EnumName:
  case SimpleCase1           // No parameters
  case SimpleCase2           // No parameters
  case ParameterCase(field: Type)  // WITH parameter
```

### Example from QuickJS-Scala

**File**: `/runtime/src/main/scala/quickjs/interpreter/DebugMode.scala`

```scala
enum DebugCommand:
  case Help                        // No parameters
  case Quit                        // No parameters
  case Load(filename: String)      // WITH parameter!
  case Reset
  case TraceEnable
  case TraceDisable
  case TraceShow
  case Vars
  case VarsGlobal
  case StackTrace
  case Unknown
```

### Usage

```scala
def handleCommand(cmd: DebugCommand): Unit =
  cmd match
    case DebugCommand.Help =>
      println("Showing help...")

    case DebugCommand.Quit =>
      println("Quitting...")

    case DebugCommand.Load(filename) =>
      println(s"Loading: $filename")

    case DebugCommand.Unknown =>
      println("Unknown command")

    case _ =>
      println("Other command")

// Usage
handleCommand(DebugCommand.Help)
handleCommand(DebugCommand.Load("/path/to/file.js"))
```

---

## 4. Pattern Matching

Pattern matching on enums provides exhaustivity checking - the compiler will warn you if you forget a case.

### Basic Matching

```scala
val kind = VariableKind.Let

kind match
  case VariableKind.Var => println("var")
  case VariableKind.Let => println("let")
  case VariableKind.Const => println("const")
  // Compiler knows all cases are covered!
```

### Multiple Cases with `|`

```scala
val opcode = Opcode.Add

opcode match
  case Opcode.PushI32 | Opcode.PushFloat64 =>
    println("Push constant")
  case Opcode.Add | Opcode.Sub | Opcode.Mul | Opcode.Div =>
    println("Arithmetic operation")
  case _ =>
    println("Other")
```

### Guards

```scala
val op = Opcode.PushI32

op match
  case o if o.code < 10 => println("Stack manipulation")
  case o if o.code < 50 => println("Other operation")
  case _ => println("High opcode")
```

### Example from QuickJS-Scala

**File**: `/runtime/src/test/scala/quickjs/interpreter/BytecodeDumper.scala`

```scala
opcode match
  case Opcode.PushI32 =>
    val offset = readInt32(bytecode.bytecode, pc + 1)
    println(f"       -> offset = $offset")
    pc += 5

  case Opcode.IfFalse | Opcode.IfTrue | Opcode.Goto =>
    val offset = readInt32(bytecode.bytecode, pc + 1)
    println(f"       -> offset = $offset")
    pc += 5

  case Opcode.GetLoc | Opcode.PutLoc =>
    val index = readInt32(bytecode.bytecode, pc + 1)
    println(f"       -> index = $index")
    pc += 5

  case _ =>
    pc += 1
```

---

## 5. Auto-Generated Methods

Every Scala 3 enum automatically gets these methods in its companion object:

### `values: Array[EnumName]`

Returns an array of all enum cases, in declaration order.

```scala
Opcode.values
// Array(
//   Invalid, Nop, PushI32, PushFloat64,
//   PushUndefined, PushNull, PushTrue, PushFalse,
//   Drop, Dup, Swap, Rotate,
//   GetLoc, PutLoc, GetArg, PutArg,
//   Neg, Not, LNot,
//   PreInc, PostInc, PreDec, PostDec,
//   Add, Sub, Mul, Div, Mod, Pow,
//   Lt, Lte, Gt, Gte, Eq, Neq, StrictEq, StrictNeq,
//   And, Or, Xor, Shl, Sar, Shr,
//   LogicalAnd, LogicalOr,
//   IfFalse, IfTrue, Goto, Break, Continue, Return, ReturnUndef,
//   ...
// )
```

**Use case**: Iteration, counting, finding by predicate.

```scala
// Count all opcodes
val opcodeCount = Opcode.values.length

// Find all arithmetic opcodes
val arithmeticOpcodes = Opcode.values.filter { op =>
  op.code >= 21 && op.code <= 25  // Add, Sub, Mul, Div, Mod
}
```

### `valueOf(name: String): EnumName`

Get enum case by name (case-sensitive).

```scala
Opcode.valueOf("PushI32")   // Returns Opcode.PushI32
Opcode.valueOf("push_i32")  // Throws NoSuchElementException

VariableKind.valueOf("Let")  // Returns VariableKind.Let
```

**Use case**: Parsing strings to enums (e.g., configuration, user input).

---

## 6. Companion Object Methods

You can add custom methods to the enum's companion object.

### Example from QuickJS-Scala

**File**: `/compiler/src/main/scala/quickjs/bytecode/Opcode.scala`

```scala
enum Opcode(val code: Int):
  // ... cases

object Opcode:
  /** Total number of opcodes */
  val Count: Int = values.length

  /** Find opcode by numeric code */
  def fromCode(code: Int): Option[Opcode] =
    values.find(_.code == code)
```

### Usage

```scala
// Get count
println(s"Total opcodes: ${Opcode.Count}")  // Total opcodes: 85

// Find by code
Opcode.fromCode(2)   // Some(PushI32)
Opcode.fromCode(999) // None

// Common pattern: parse with fallback
val opcode = Opcode.fromCode(byte)
  .getOrElse(Opcode.Invalid)
```

---

## 7. Enums vs Sealed Traits

QuickJS-Scala uses both enums and sealed traits. Here's when to use each:

### Enums

**Use when:**
- Simple list of constants without data
- Each case is just a name (or has simple uniform parameters)
- You want exhaustivity checking in pattern matching

**Examples:**
```scala
enum Keyword:
  case Var, Let, Const, If, Else, // ...

enum VariableKind:
  case Var, Let, Const

enum PropertyKind:
  case Value, Getter, Setter, Method
```

### Sealed Traits

**Use when:**
- Each case carries different data
- You need hierarchical type relationships
- Cases have different fields/parameters

**Example from QuickJS-Scala:**

**File**: `/parser/src/main/scala/quickjs/lexer/Token.scala`

```scala
// Sealed trait hierarchy (for data-carrying types)
sealed trait Token:
  def span: Span

final case class NumberToken(value: Double, span: Span) extends Token
final case class StringToken(value: String, span: Span) extends Token
final case class IdentifierToken(name: String, span: Span) extends Token
final case class KeywordToken(kind: Keyword, span: Span) extends Token

case object EOF extends Token:
  def span: Span = Span(0, 0, 0, 0)
```

### Combining Both

You can use enums WITHIN sealed traits for the best of both worlds:

```scala
// Sealed trait for token hierarchy
sealed trait Token:
  def span: Span

// Enum for simple keyword categorization
enum Keyword:
  case Var, Let, Const, If, Else

// Case class that USES the enum
final case class KeywordToken(kind: Keyword, span: Span) extends Token
```

**This pattern is used extensively in QuickJS-Scala!**

---

## 8. Real-World Examples from QuickJS-Scala

### Example 1: Binary Operators

**File**: `/parser/src/main/scala/quickjs/ast/AST.scala`

```scala
enum BinaryOperator:
  case Comma        // Lowest precedence
  case Add, Sub, Mul, Div, Mod, Pow
  case Eq, Neq, StrictEq, StrictNeq
  case Lt, Lte, Gt, Gte
  case And, Or, Xor, Shl, Sar, Shr
  case LogicalAnd, LogicalOr
  case In, Instanceof
```

**Usage in AST:**
```scala
case class BinaryExpression(
  operator: BinaryOperator,
  left: Expression,
  right: Expression,
  span: Span
) extends Expression
```

### Example 2: Unary Operators

**File**: `/parser/src/main/scala/quickjs/ast/AST.scala`

```scala
enum UnaryOperator:
  case Minus, Plus, Not, BitwiseNot
  case PreInc, PostInc, PreDec, PostDec
  case Typeof, Delete, Void
```

**Usage in AST:**
```scala
case class UnaryExpression(
  operator: UnaryOperator,
  argument: Expression,
  prefix: Boolean = true,
  span: Span
) extends Expression
```

### Example 3: Punctuation Tokens

**File**: `/parser/src/main/scala/quickjs/lexer/Token.scala`

```scala
enum Punctuation:
  case Comma, Semicolon, Colon, Question
  case LeftParen, RightParen
  case LeftBracket, RightBracket
  case LeftBrace, RightBrace
```

### Example 4: Debug Commands with Parameters

**File**: `/runtime/src/main/scala/quickjs/interpreter/DebugMode.scala`

```scala
enum DebugCommand:
  case Help
  case Quit
  case Load(filename: String)  // Parameterized case
  case Reset
  case TraceEnable
  case TraceDisable
  case TraceShow
  case Vars
  case VarsGlobal
  case StackTrace
  case Unknown
```

**Parser for debug commands:**
```scala
object DebugCommand:
  def parse(input: String): DebugCommand =
    val parts = input.trim.split("\\s+", 2)
    val cmd = parts(0).toLowerCase

    cmd match
      case ".help" | ".h" => DebugCommand.Help
      case ".quit" | ".q" => DebugCommand.Quit
      case ".load" if parts.length > 1 => DebugCommand.Load(parts(1))
      case ".reset" => DebugCommand.Reset
      case ".debug" => DebugCommand.TraceEnable
      case _ => DebugCommand.Unknown
```

---

## 9. Best Practices

### DO ✅

1. **Use enums for fixed sets of constants**
   ```scala
   enum Color { case Red, Green, Blue }
   ```

2. **Use parameters for enum-specific data**
   ```scala
   enum Size(val multiplier: Int) {
     case Small extends Size(1)
     case Medium extends Size(2)
     case Large extends Size(3)
   }
   ```

3. **Add companion object methods for common operations**
   ```scala
   enum Opcode(val code: Int) { /* ... */ }
   object Opcode {
     def fromCode(code: Int): Option[Opcode] =
       values.find(_.code == code)
   }
   ```

4. **Pattern match exhaustively**
   ```scala
   kind match
     case VariableKind.Var => ...
     case VariableKind.Let => ...
     case VariableKind.Const => ...
     // Compiler: all cases covered!
   ```

### DON'T ❌

1. **Don't use enums for hierarchical data**
   ```scala
   // Bad - use sealed trait instead
   enum Animal:
     case Dog(name: String)
     case Cat(name: String, livesLeft: Int)

   // Good
   sealed trait Animal
   case class Dog(name: String) extends Animal
   case class Cat(name: String, livesLeft: Int) extends Animal
   ```

2. **Don't forget to handle all cases in pattern matching**
   ```scala
   // Bad - compiler will warn
   kind match
     case VariableKind.Var => ...
     // Missing: Let, Const!

   // Good - all cases covered
   kind match
     case VariableKind.Var => ...
     case VariableKind.Let => ...
     case VariableKind.Const => ...
   ```

3. **Don't use `valueOf` with user input without validation**
   ```scala
   // Bad - throws exception
   val kind = VariableKind.valueOf(userInput)

   // Good - safe parsing
   val kind = VariableKind.values.find(_.toString == userInput)
   ```

---

## 10. Common Pitfalls

### Pitfall 1: Case Sensitivity in `valueOf`

```scala
// This throws NoSuchElementException
VariableKind.valueOf("let")  // lowercase

// Correct
VariableKind.valueOf("Let")  // exact case match
```

**Solution**: Use a custom helper:
```scala
def parseVariableKind(s: String): Option[VariableKind] =
  VariableKind.values.find(_.toString.toLowerCase == s.toLowerCase)
```

### Pitfall 2: Pattern Matching Without Exhaustivity

```scala
// If you add a new case to VariableKind, this code won't warn you!
kind match
  case VariableKind.Var => "var"
  case _ => "other"  // Wildcard hides missing cases
```

**Solution**: Match explicitly or enable compiler warnings:
```scala
kind match
  case VariableKind.Var => "var"
  case VariableKind.Let => "let"
  case VariableKind.Const => "const"
  // Now adding a new case will cause a compile error here
```

### Pitfall 3: Forgetting Enum Values Order

```scala
// Don't assume index equals value
enum Opcode(val code: Int):
  case Invalid extends Opcode(0)
  case Nop extends Opcode(1)
  case PushI32 extends Opcode(2)  // Not index 2!

// Wrong
Opcode.values(2)  // Returns PushI32, but index is coincidence

// Correct
Opcode.values.find(_.code == 2)  // Find by actual code
```

### Pitfall 4: Using Enum as Integer

```scala
// Don't do this
val opcodeInt: Int = Opcode.PushI32.code  // Lose type safety

// Do this instead
val opcode: Opcode = Opcode.PushI32  // Keep enum type
```

---

## Summary: Quick Reference

| Feature | Syntax | Example |
|---------|--------|---------|
| **Basic enum** | `enum E { case A, B }` | `enum Color { case Red, Green, Blue }` |
| **Enum with param** | `enum E(p: T) { case A extends E(v) }` | `enum Opcode(val code: Int) { case Add extends Opcode(21) }` |
| **Mixed enum** | `enum E { case A; case B(x: T) }` | `enum Cmd { case Help; case Load(path: String) }` |
| **All values** | `E.values` | `Opcode.values` (returns `Array[Opcode]`) |
| **Find by name** | `E.valueOf("A")` | `VariableKind.valueOf("Let")` |
| **Pattern match** | `e match { case E.A => ... }` | `kind match { case VariableKind.Var => ... }` |
| **Multiple cases** | `case E.A \| E.B =>` | `case Opcode.Add | Opcode.Sub =>` |
| **Wild card** | `case _ =>` | `case _ => println("other")` |

---

## When to Use Enums vs Sealed Traits

| Aspect | Enums | Sealed Traits |
|--------|-------|---------------|
| **Syntax** | `enum E { case A, B }` | `sealed trait T; case class A() extends T` |
| **Data** | Uniform parameters per case | Different fields per case |
| **Hierarchy** | Flat (no inheritance) | Hierarchical (can extend) |
| **Pattern matching** | Exhaustivity checking | Exhaustivity checking |
| **Use case** | Constants, categories | Data structures, AST nodes |
| **QuickJS example** | `Keyword`, `Opcode` | `Token`, `JSValue`, `AST` |

---

## References

- Scala 3 Enum Documentation: https://docs.scala-lang.org/scala3/reference/enums/enums.html
- Pattern Matching: https://docs.scala-lang.org/scala3/reference/patterns/introduction.html
- QuickJS-Scala Codebase:
  - `/compiler/src/main/scala/quickjs/bytecode/Opcode.scala`
  - `/parser/src/main/scala/quickjs/lexer/Token.scala`
  - `/parser/src/main/scala/quickjs/ast/AST.scala`
  - `/runtime/src/main/scala/quickjs/interpreter/DebugMode.scala`

---

**Document Version**: 1.0
**Last Updated**: 2025-12-29
**Author**: Generated by Claude Code
