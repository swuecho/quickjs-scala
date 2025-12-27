# QuickJS-Scala - Claude Code Reference

## Project Overview

QuickJS-Scala is a JavaScript engine written in Scala 3 for the JVM, inspired by the QuickJS C implementation. The goal is to create a production-grade JavaScript engine with full ES2024+ support. **When not sure about the approach, check the original quickjs c version for ideas.**

**Current Status**: Phase 1 complete - Can evaluate arithmetic expressions like `1 + 2 = 3` through a full compile-execute pipeline.

## Architecture Overview

### Core Design Decisions

1. **Stack-based bytecode interpreter** (not JVM bytecode generation)
   - Matches QuickJS architecture for easier porting
   - Better control over semantics and debugging
   - Trade-off: Slower than direct JVM bytecode generation

2. **JVM GC integration** (not custom mark-and-sweep)
   - Eliminates 2,000+ lines of complex GC code
   - Leverages mature JVM garbage collectors (G1, ZGC, Shenandoah)
   - Trade-off: Less control over GC pauses

3. **Tagged union type system** for JavaScript values
   - Sealed trait with case classes for type safety
   - Smart constructors for type coercion and optimization
   - Inline storage for small values (Int32, Bool, Null, Undefined)

4. **Parser combinators** (fastparse library)
   - Declarative grammar rules
   - Good error recovery
   - Maintainable codebase

### Module Structure

```
quickjs-scala/
├── build.sbt                    # SBT multi-project build
├── core/                        # Core type system
│   └── src/main/scala/quickjs/
│       ├── value/               # JSValue tagged union
│       ├── runtime/             # JSRuntime, JSContext
│       ├── atom/                # Atom table (string interning)
│       └── objmodel/            # JSObject, properties
├── parser/                      # ES2024+ parser
│   └── src/main/scala/quickjs/
│       ├── ast/                 # AST nodes
│       ├── lexer/               # Lexer (not yet implemented)
│       └── parser/              # Parser combinators
├── compiler/                    # Bytecode compiler
│   └── src/main/scala/quickjs/
│       ├── bytecode/            # Opcode definitions
│       ├── emitter/             # Bytecode emitter
│       └── compiler/            # Compiler orchestration
├── runtime/                     # Interpreter
│   └── src/main/scala/quickjs/
│       └── interpreter/         # Bytecode interpreter
└── stdlib/                      # Standard library
    └── src/main/scala/quickjs/
        └── (to be implemented)
```

## Key Files and Their Purpose

### Core Type System

**`/core/src/main/scala/quickjs/value/JSValue.scala`**
- Foundation of the entire type system
- Tagged union representation: `sealed trait JSValue` with case classes
- Smart constructors: `fromInt`, `fromDouble`, `fromBoolean`, `fromString`
- Arithmetic operations: `add`, `subtract`, `multiply`, `divide`
- Type conversions: `toBoolean`, `toNumber`, `toString`

```scala
// Example: Creating and manipulating values
val a = JSValue.fromInt(1)
val b = JSValue.fromInt(2)
val result = JSValue.add(a, b)  // JSValue.Int32(3)
```

**`/core/src/main/scala/quickjs/objmodel/JSObject.scala`**
- JavaScript object model
- Property storage in mutable HashMap
- Prototype chain support
- Property descriptors and attributes
- Extensibility and sealing

**`/core/src/main/scala/quickjs/runtime/JSContext.scala`**
- Execution context (per-isolate resources)
- Global object management
- Exception handling
- Intrinsics (Object, Array, Function prototypes)

**`/core/src/main/scala/quickjs/runtime/JSRuntime.scala`**
- Runtime management (atom table, class registry)
- Job queue for Promises
- Module loading hooks

### Parser

**`/parser/src/main/scala/quickjs/ast/AST.scala`**
- AST node definitions for ES2024+ grammar
- Currently implements: literals, identifiers, binary/unary expressions
- Types: `Script`, `Statement`, `Expression`, `Literal`, `BinaryExpression`, `UnaryExpression`

### Compiler

**`/compiler/src/main/scala/quickjs/bytecode/Opcode.scala`**
- 35+ opcodes defined as enum
- Categories: stack manipulation, arithmetic, comparison, bitwise, logical, control flow

**`/compiler/src/main/scala/quickjs/bytecode/Instruction.scala`**
- Bytecode instruction encoding
- Factory methods for creating instructions
- Supports: I32, Float64 operands

**`/compiler/src/main/scala/quickjs/compiler/Compiler.scala`**
- Compiles AST to bytecode
- Scope analysis (not yet fully implemented)
- Label resolution (not yet implemented)
- Currently handles: literals, binary operations

### Interpreter

**`/runtime/src/main/scala/quickjs/interpreter/Interpreter.scala`**
- Stack-based bytecode interpreter
- Direct threading optimization via `@switch` annotation
- Implements all arithmetic and comparison opcodes
- Exception handling with `breakable`

## How the Pipeline Works

### Example: Evaluating `1 + 2 = 3`

```scala
// 1. Create AST
val ast = Script(
  body = Seq(
    ExpressionStatement(
      BinaryExpression(
        operator = BinaryOperator.Add,
        left = Literal(JSValue.fromInt(1), Span(0, 1, 0, 0)),
        right = Literal(JSValue.fromInt(2), Span(4, 5, 0, 4)),
        span = Span(0, 5, 0, 0)
      ),
      span = Span(0, 5, 0, 0)
    )
  ),
  span = Span(0, 5, 0, 0)
)

// 2. Compile to bytecode
val compiler = Compiler()
val bytecode = compiler.compileScript(ast)

// Generated bytecode:
// PushI32(1)
// PushI32(2)
// Add
// ReturnUndef

// 3. Execute bytecode
given ctx: JSContext = JSContext(JSRuntime())
val interpreter = Interpreter()
val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

// Result: JSValue.Undefined (because ExpressionStatement drops it)
```

## Important Implementation Details

### Type Boxing and Unboxing

When working with `Array[AnyRef]` in the bytecode encoder, primitive types must be explicitly boxed:

```scala
// Correct
new Instruction(Opcode.PushI32, Array[AnyRef](java.lang.Integer.valueOf(value)))
new Instruction(Opcode.PushFloat64, Array[AnyRef](java.lang.Double.valueOf(value)))

// Incorrect - won't compile
new Instruction(Opcode.PushI32, Array(value))
```

### Pattern Matching on AnyRef

When pattern matching on `AnyRef`, use boxed types to avoid type refinement issues:

```scala
// Correct
operand match
  case i: java.lang.Integer =>
    val value = i.intValue()
    // use value
  case l: java.lang.Long =>
    val value = l.longValue()
    // use value

// Incorrect - type errors
operand match
  case i: Int => // Won't work as expected
```

### Package Naming Conventions

- **`quickjs.object`** → renamed to **`quickjs.objmodel`** (avoid Scala keyword conflict)
- Use fully qualified names when there's ambiguity: `quickjs.ast.BinaryOperator`

### Number.toLong vs toInt

The `Number` trait in JSValue has both `toInt` and `toLong` methods. When accessing from the base `JSValue` trait, use:

```scala
// For numbers
value match
  case JSValue.Int32(i) => i
  case JSValue.Float64(d) => d.toInt
  case _ => 0

// Or use toNumber then convert
value.toNumber.toInt
```

## Known Issues and Workarounds

### 1. Division Test Failure

**Issue**: `JSValue.divide(JSValue.fromInt(10), JSValue.fromInt(2))` returns `Int32(5)` due to smart constructor optimization, not `Float64(5.0)`.

**Workaround**: Test using `toNumber` instead of exact match:
```scala
assert(result.toNumber == 5.0)  // Works
assert(result == JSValue.fromDouble(5.0))  // May fail due to optimization
```

### 2. String Type Naming

**Issue**: `String` conflicts with Scala's built-in `String` type.

**Solution**: Renamed to `JSStr` throughout the codebase:
```scala
case class JSStr(value: java.lang.String) extends JSValue
```

### 3. Object Package Naming

**Issue**: `object` is a reserved keyword in Scala.

**Solution**: Renamed package from `quickjs.object` to `quickjs.objmodel`.

## Build and Test Commands

```bash
# Compile all modules
sbt compile

# Compile specific module
sbt "core/compile"
sbt "parser/compile"
sbt "compiler/compile"
sbt "runtime/compile"

# Run tests
sbt test

# Run specific test
sbt "core/test"
sbt "runtime/test"

# Clean build
sbt clean
```

## Test Status

**Phase 1 Tests**: ✅ All 13 tests passing

- JSValue type system (creation, conversion, arithmetic)
- **"1 + 2 = 3"** - Full pipeline test

## Dependencies

```scala
libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "3.1.1",      // Parser combinators
  "org.scalameta" %% "munit" % "1.0.2" % Test, // Testing
  "org.jline" % "jline" % "3.26.1",            // REPL (future)
  "com.github.scopt" %% "scopt" % "4.1.0"      // CLI (future)
)
```

## Next Steps (Phase 2: Core Language)

### Priority Order

1. **Parser Enhancement**
   - Complete lexer with proper tokenization
   - Implement full expression parser
   - Add statement parser (if, while, for, functions)

2. **Variable Support**
   - Variable declarations (var, let, const)
   - Scope and hoisting
   - Variable lookup in interpreter

3. **Control Flow**
   - If/else statements
   - While loops
   - For loops
   - Break/continue

4. **Functions**
   - Function declarations and expressions
   - Function calls
   - Return statements
   - Arguments and parameters

5. **Object Literals**
   - Object property access
   - Array literals
   - Property assignment

### File Locations for Phase 2

Create/update these files:
- `/parser/src/main/scala/quickjs/lexer/Lexer.scala` - Tokenization
- `/parser/src/main/scala/quickjs/parser/ExpressionParser.scala` - Expression parsing
- `/parser/src/main/scala/quickjs/parser/StatementParser.scala` - Statement parsing
- `/compiler/src/main/scala/quickjs/compiler/ScopeAnalyzer.scala` - Scope analysis
- `/compiler/src/main/scala/quickjs/bytecode/Opcodes.scala` - Add control flow opcodes

## Quick Reference

### Creating a JSValue

```scala
// Primitives
JSValue.Undefined
JSValue.Null
JSValue.fromBoolean(true)
JSValue.fromInt(42)
JSValue.fromDouble(3.14)
JSValue.fromString("hello")

// Objects
import quickjs.objmodel.JSObject
val obj = JSObject(prototype = null, extensible = true)
JSValue.Object(obj)
```

### Using the Compiler

```scala
import quickjs.compiler.Compiler
import quickjs.ast.*

val compiler = Compiler()
val ast = Script(...)
val bytecode = compiler.compileScript(ast)
```

### Using the Interpreter

```scala
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}

given runtime: JSRuntime = JSRuntime()
given ctx: JSContext = JSContext(runtime)

val interpreter = Interpreter()
val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
```

## Performance Considerations

- Current implementation is **not optimized** for performance
- Focus is on correctness and maintainability for Phase 1
- Future optimizations:
  - Inline caching for property access
  - Peephole optimizer for bytecode
  - Escape analysis for stack allocation
  - Profile-guided optimization

## Documentation

- Full rewrite plan: `/home/hwu/dev/quickjs/docs/SCALA_REWRITE_PLAN.md`
- Original QuickJS reference: `/home/hwu/dev/quickjs/quickjs.c` (60,000 lines)
- Opcodes reference: `/home/hwu/dev/quickjs/quickjs-opcode.h`

## Common Patterns

### Adding a New Opcode

1. Add to `Opcode` enum in `/compiler/src/main/scala/quickjs/bytecode/Opcode.scala`
2. Add instruction factory in `/compiler/src/main/scala/quickjs/bytecode/Instruction.scala`
3. Implement in `/runtime/src/main/scala/quickjs/interpreter/Interpreter.scala`
4. Add compiler support in `/compiler/src/main/scala/quickjs/compiler/Compiler.scala`

### Adding a New AST Node

1. Add to `/parser/src/main/scala/quickjs/ast/AST.scala`
2. Add parser support (future)
3. Add compiler support in `compileExpression` or `compileStatement`
4. Add tests

## Contact and Contribution

This is a learning/educational project demonstrating Scala 3's capabilities for systems programming. The architecture prioritizes:
- Type safety
- Code clarity
- Maintainability
- Correctness

over raw performance, though performance optimizations are planned for later phases.
