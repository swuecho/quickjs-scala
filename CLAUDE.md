# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

QuickJS-Scala is a JavaScript engine written in Scala 3 for the JVM, inspired by the QuickJS C implementation. The goal is to create a production-grade JavaScript engine with full ES2024+ support.

**Before implementing a feature, check the original C version first and follow a similar approach.**
**When fixing a bug and not sure about the approach, check the original QuickJS C version for ideas.**

Reference files:
- Original QuickJS: `quickjs.c` from the upstream QuickJS repository
- Opcodes reference: `quickjs-opcode.h` from the upstream QuickJS repository

## Build Commands

```bash
# Compile all modules
sbt compile

# Compile specific module
sbt "core/compile"
sbt "parser/compile"
sbt "compiler/compile"
sbt "runtime/compile"

# Run all tests
sbt test

# Run tests for specific module
sbt "core/test"
sbt "runtime/test"
sbt "stdlib/test"

# Run a single test class
sbt "stdlib/testOnly quickjs.stdlib.QuickJSLanguageTest"

# Run a single test by name pattern
sbt "stdlib/testOnly quickjs.stdlib.QuickJSLanguageTest -- --tests=*labeled*"

# Clean build
sbt clean

# Build fat JAR
sbt "runner/assembly"

# Web frontend development (Scala.js + Vite)
./scripts/dev-web.sh

# Start trace server
./scripts/dev-server.sh
```

## Architecture

### Core Design Decisions

1. **Stack-based bytecode interpreter** (not JVM bytecode generation) - matches QuickJS architecture
2. **JVM GC integration** (not custom mark-and-sweep) - leverages G1, ZGC, Shenandoah
3. **Tagged union type system** - sealed trait with case classes for JSValue
4. **Hand-written recursive descent parser** (not parser combinators)

### Module Structure

```
core/       → JSValue types, JSObject, JSRuntime, JSContext
parser/     → Lexer, AST nodes, recursive descent Parser
compiler/   → Opcode definitions, bytecode Compiler
runtime/    → Bytecode Interpreter
stdlib/     → Standard library built-ins and QuickJS test suite
runner/     → CLI runner with fat JAR assembly
web/        → Scala.js web frontend (Laminar)
```

### Key Files

- `core/src/main/scala/quickjs/value/JSValue.scala` - Tagged union value types
- `core/src/main/scala/quickjs/objmodel/JSObject.scala` - Object model with prototypes
- `core/src/main/scala/quickjs/runtime/JSContext.scala` - Execution context
- `parser/src/main/scala/quickjs/ast/AST.scala` - AST node definitions
- `parser/src/main/scala/quickjs/parser/Parser.scala` - Recursive descent parser
- `compiler/src/main/scala/quickjs/bytecode/Opcode.scala` - Bytecode opcodes (enum)
- `compiler/src/main/scala/quickjs/compiler/Compiler.scala` - AST to bytecode
- `runtime/src/main/scala/quickjs/interpreter/Interpreter.scala` - Bytecode execution

### Execution Pipeline

```
Source → Lexer → Parser → AST → Compiler → Bytecode → Interpreter → Result
```

## Implementation Details

### Type Boxing for Bytecode

Primitive types must be explicitly boxed when creating instructions with `Array[AnyRef]`:

```scala
// Correct
new Instruction(Opcode.PushI32, Array[AnyRef](java.lang.Integer.valueOf(value)))

// Incorrect - won't compile
new Instruction(Opcode.PushI32, Array(value))
```

### Pattern Matching on AnyRef

Use boxed types to avoid type refinement issues:

```scala
// Correct
operand match
  case i: java.lang.Integer => i.intValue()
  case l: java.lang.Long => l.longValue()

// Incorrect
operand match
  case i: Int => // Won't work
```

### Package Naming

- `quickjs.objmodel` (not `quickjs.object` - Scala keyword conflict)
- `JSStr` (not `String` - conflicts with Scala's String)
- Use fully qualified names for ambiguous types: `quickjs.ast.BinaryOperator`

### Division Semantics

`JSValue.divide(JSValue.fromInt(10), JSValue.fromInt(2))` may return `Int32(5)` instead of `Float64(5.0)` due to smart constructor optimization. Test with `.toNumber`:

```scala
assert(result.toNumber == 5.0)  // Works
```

## Common Patterns

### Adding a New Opcode

1. Add to enum in `compiler/src/main/scala/quickjs/bytecode/Opcode.scala`
2. Add factory in `compiler/src/main/scala/quickjs/bytecode/Instruction.scala`
3. Implement in `runtime/src/main/scala/quickjs/interpreter/Interpreter.scala`
4. Add compiler support in `compiler/src/main/scala/quickjs/compiler/Compiler.scala`

### Adding a New AST Node

1. Add to `parser/src/main/scala/quickjs/ast/AST.scala`
2. Add parser support in `parser/src/main/scala/quickjs/parser/Parser.scala`
3. Add compiler support in `compileExpression` or `compileStatement`
4. Add tests

### Using the Engine

```scala
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}

given runtime: JSRuntime = JSRuntime()
given ctx: JSContext = JSContext(runtime)

val parser = Parser()
val compiler = Compiler()
val interpreter = Interpreter()

val ast = parser.parse("1 + 2")
val bytecode = compiler.compileScript(ast)
val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
```

## Current Status

Implemented:
- Variables (var, let, const)
- Control flow (if/else, while, for, do-while, switch)
- Functions (declarations, expressions, arrows, closures)
- Labeled statements (break/continue with labels)
- Objects and arrays (literals, property access, methods)
- Operators (typeof, instanceof, in, delete, void)
- Error objects with stack traces
- Date, RegExp built-ins
- Array/String/Object prototype methods
- JSON.parse/stringify with full options

In progress:
- Module support (import/export AST parsed, compiler/runtime pending)
- Strict mode
