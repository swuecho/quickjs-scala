# QuickJS-Scala - Claude Code Reference

## Project Overview

QuickJS-Scala is a JavaScript engine written in Scala 3 for the JVM, inspired by the QuickJS C implementation. The goal is to create a production-grade JavaScript engine with full ES2024+ support. 

**Before implement a feature, check the original c version first, should follow similar apparoch**
**When fixing a bug but not sure about the approach, check the original quickjs c version for ideas.**

**Current Status**: Phase 2+ complete - Full language support including variables, functions, control flow, closures, and labeled statements. Can evaluate complex JavaScript code through a complete compile-execute pipeline.

**Recent Progress (Dec 2025)**:
- Added error construction and stack trace formatting in JSContext; interpreter now records call frames and maps runtime exceptions to Error objects.
- Property storage moved to LinkedHashMap; descriptors now include writable/configurable and accessor semantics in JSObject.
- Implemented more Array methods (filter/forEach/reduce/includes/indexOf/splice) and improved map(thisArg).
- Implemented more String methods (split/replace/match/startsWith/endsWith/padStart/padEnd).
- JSON.parse/JSON.stringify now handle revivers, replacers, circular refs, toJSON, and insertion order; improved JSON error formatting.
- Object literal accessors now use defineProperty with enumerable/configurable set.
- QuickJS C test migration ongoing; `test_object_literal()` now passes, current failure is `test_argument_scope()` (strict mode not implemented).

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
│       ├── lexer/               # Lexer implementation
│       └── parser/              # Parser (hand-written, not combinators)
├── compiler/                    # Bytecode compiler
│   └── src/main/scala/quickjs/
│       ├── bytecode/            # Opcode definitions
│       └── compiler/            # Compiler orchestration
├── runtime/                     # Interpreter
│   └── src/main/scala/quickjs/
│       └── interpreter/         # Bytecode interpreter
└── stdlib/                      # Standard library & tests
    └── src/main/scala/quickjs/
        └── (tests ported from QuickJS C)
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
- Implements: literals, identifiers, binary/unary expressions, statements, control flow, functions
- Types: `Script`, `Statement`, `Expression`, `Literal`, `BinaryExpression`, `UnaryExpression`, `IfStatement`, `WhileStatement`, `ForStatement`, `DoWhileStatement`, `SwitchStatement`, `FunctionDeclaration`, `FunctionExpression`, `ArrowFunctionExpression`, `VariableDeclaration`, `BreakStatement`, `ContinueStatement`, `ReturnStatement`, `ObjectLiteral`, `ArrayLiteral`, `MemberExpression`, `CallExpression`

**`/parser/src/main/scala/quickjs/parser/Parser.scala`**
- Hand-written recursive descent parser (not parser combinators)
- Full JavaScript expression parsing with proper operator precedence
- Statement parsing including all control flow
- Labeled statement support (e.g., `label: for (...) { break label; }`)
- Handles all JavaScript syntax including arrow functions, object literals, array literals

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
- Full expression and statement compilation
- Labeled statement support with proper label resolution
- Loop stack management for break/continue
- Closure capture analysis
- Variable scope handling

### Interpreter

**`/runtime/src/main/scala/quickjs/interpreter/Interpreter.scala`**
- Stack-based bytecode interpreter
- Direct threading optimization via `@switch` annotation
- Implements all arithmetic, comparison, bitwise, logical, and control flow opcodes
- Exception handling with `breakable`
- Function call/return support
- Closure support with captured variables

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

**Current Test Count**: 316 tests total
- **stdlib**: 181 tests (171 passing, 10 failing)
- **runtime**: 135 tests (129 passing, 6 failing)

**Recently Added Tests** (December 2025):
- ✅ Labeled statement tests (4 tests) - QuickJS C test suite migration
- ✅ Closure state tests - Closure variable capture
- ✅ Comprehensive language tests - Full language feature coverage
- ✅ Loop tests - while, for, do-while, nested loops
- ✅ Operator tests - typeof, instanceof, in, delete
- ✅ Function expression tests - arrow functions, function expressions

**Key Test Suites**:
- `QuickJSLoopTest` - Loop control flow from QuickJS C
- `QuickJSLanguageTest` - Language features from QuickJS C
- `QuickJSClosureTest` - Closure behavior tests
- `ComprehensiveTest` - End-to-end language tests
- `FunctionExpressionTest` - Function expressions and closures

**Currently Failing Tests** (16 total):
- Function expression edge cases
- Array element assignment
- JSON.stringify edge cases
- Debug tracing features

## Dependencies

```scala
libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "3.1.1",      // Parser combinators
  "org.scalameta" %% "munit" % "1.0.2" % Test, // Testing
  "org.jline" % "jline" % "3.26.1",            // REPL (future)
  "com.github.scopt" %% "scopt" % "4.1.0"      // CLI (future)
)
```

## Next Steps

### Current Priorities (January 2025)

1. **Fix Failing Tests** (16 failures)
   - Array element assignment issues
   - Function expression edge cases
   - JSON.stringify completeness
   - Debug tracing implementation

2. **Standard Library**
   - Complete Array.prototype methods
   - String.prototype methods
   - Math functions
   - JSON.parse/stringify improvements

3. **Error Handling**
   - Proper JavaScript Error objects
   - Stack trace generation
   - Try/catch/finally statement support
   - Throw statements

4. **Object Model Enhancements**
   - Prototype chain resolution
   - Property descriptors (get/set/enumerable/etc)
   - Object.defineProperty
   - Object.freeze/seal/preventExtensions

### Completed Features

✅ **Phase 1**: Basic arithmetic and expressions
✅ **Phase 2a**: Variables (var, let, const)
✅ **Phase 2b**: Control flow (if/else, while, for, do-while, switch)
✅ **Phase 2c**: Functions (declarations, expressions, arrows, closures)
✅ **Phase 2d**: Labeled statements (break/continue with labels)
✅ **Phase 2e**: Objects and arrays (literals, property access, methods)
✅ **Phase 2f**: Operators (typeof, instanceof, in, delete, void)

## Recent Major Features

### Labeled Statements (December 2025)

Implemented full labeled statement support following QuickJS C implementation pattern:

**Supported Syntax**:
```javascript
// Labeled loops with break/continue
outer: for (var i = 0; i < 10; i++) {
  inner: for (var j = 0; j < 10; j++) {
    if (j === 5) break outer;  // Break out of outer loop
  }
}

// Labeled blocks
label: {
  console.log("executed");
  break label;  // Exit the labeled block
}

// Labeled continue
loop: while (condition) {
  if (skip) continue loop;  // Continue to loop test
}
```

**Implementation Details**:
- Labels stored in AST: `WhileStatement(test, body, label, span)`
- Loop stack tracks labels: `(isLoop, labelName, exitPos, continuePos, pendingBreaks, pendingContinues, isRegular)`
- `break labelName` searches stack for matching label
- `continue labelName` searches for loop with matching label
- Pending jumps patched when exit position known

**Key Files**:
- `/parser/src/main/scala/quickjs/parser/Parser.scala` - Label detection and parsing
- `/compiler/src/main/scala/quickjs/compiler/Compiler.scala` - Label stack management
- `/stdlib/src/test/scala/quickjs/stdlib/QuickJSLanguageTest.scala` - Labeled statement tests

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
