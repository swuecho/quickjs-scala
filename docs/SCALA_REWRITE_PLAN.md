# QuickJS Scala 3 Rewrite Plan

## Project Overview

Rewrite QuickJS (60,000+ lines of highly optimized C) in Scala 3 for the JVM to create a production-grade JavaScript engine with full ES2024+ support.

## User Requirements

- **Goal**: Production-grade JavaScript engine
- **Platform**: JVM (not Scala Native)
- **ES Support**: Full ES2024+ feature parity
- **Components**: All components (parser, compiler, interpreter, regex, Unicode, stdlib, WASM, workers)

## Architecture Overview

### Strategic Decisions

1. **Stack-based bytecode interpreter** (not JVM bytecode generation)
   - Easier to debug and maintain
   - Matches QuickJS architecture for easier porting
   - Better control over semantics

2. **JVM GC integration** (not custom mark-and-sweep)
   - Eliminates 2,000+ lines of complex GC code
   - Mature, optimized garbage collectors (G1, ZGC, Shenandoah)

3. **Hybrid regex approach** (java.util.regex + custom extensions)
   - Reuse well-tested JVM regex engine
   - Custom handling for JS-specific features

4. **Parser combinators** (fastparse library)
   - Declarative grammar rules
   - Good error recovery
   - Maintainable codebase

5. **Java's built-in Unicode support**
   - `java.text.Normalizer` for normalization
   - UTF-16 encoding matches JavaScript

## Project Structure

```
quickjs-scala/
├── build.sbt                    # SBT multi-project build
├── core/                        # Core type system
│   └── src/main/scala/quickjs/
│       ├── value/               # JSValue tagged union
│       ├── runtime/             # JSRuntime, JSContext
│       ├── atom/                # Atom table
│       └── object/              # JSObject, properties
├── parser/                      # ES2024+ parser
│   └── src/main/scala/quickjs/
│       ├── ast/                 # AST nodes
│       ├── lexer/               # Lexer
│       └── parser/              # Parser combinators
├── compiler/                    # Bytecode compiler
│   └── src/main/scala/quickjs/
│       ├── bytecode/            # Opcode definitions
│       ├── emitter/             # Bytecode emitter
│       └── compiler/            # Compiler orchestration
├── runtime/                     # Interpreter
│   └── src/main/scala/quickjs/
│       └── interpreter/         # Bytecode interpreter
├── stdlib/                      # Standard library
│   └── src/main/scala/quickjs/
│       ├── objects/             # Object, Array, etc.
│       ├── functions/           # Function, ArrowFunction
│       ├── primitives/          # Number, String, Boolean
│       ├── regexp/              # RegExp engine
│       ├── promise/             # Promise/async
│       └── modules/             # Module system
├── repl/                        # Interactive REPL
│   └── src/main/scala/quickjs/repl/
└── tools/                       # CLI tools
    └── src/main/scala/quickjs/
        ├── cli/                 # qjs command-line
        └── compiler/            # qjsc bytecode compiler
```

## Critical Files (Implementation Order)

### 1. Core Type System
- **`/core/src/main/scala/quickjs/value/JSValue.scala`** - Foundation for all JavaScript values (tagged union with primitives, objects, references)
- **`/core/src/main/scala/quickjs/object/JSObject.scala`** - Core object model (properties, prototypes, extensibility)
- **`/core/src/main/scala/quickjs/runtime/JSContext.scala`** - Execution context (exception handling, global object)
- **`/core/src/main/scala/quickjs/runtime/JSRuntime.scala`** - Runtime management (atom table, class registry, job queue)

### 2. Parser
- **`/parser/src/main/scala/quickjs/ast/AST.scala`** - AST node definitions for ES2024+
- **`/parser/src/main/scala/quickjs/parser/ESParser.scala`** - Main parser using fastparse

### 3. Compiler
- **`/compiler/src/main/scala/quickjs/bytecode/Opcode.scala`** - Opcode definitions (adapted from quickjs-opcode.h)
- **`/compiler/src/main/scala/quickjs/bytecode/Instruction.scala`** - Instruction encoding
- **`/compiler/src/main/scala/quickjs/compiler/Compiler.scala`** - Compiler orchestration (scope analysis, bytecode emission)

### 4. Interpreter
- **`/runtime/src/main/scala/quickjs/interpreter/Interpreter.scala`** - Core bytecode interpreter (most performance-critical code)

### 5. Standard Library
- **`/stdlib/src/main/scala/quickjs/stdlib/ObjectConstructor.scala`** - Object implementation
- **`/stdlib/src/main/scala/quickjs/stdlib/ArrayConstructor.scala`** - Array implementation
- **`/stdlib/src/main/scala/quickjs/regexp/RegExp.scala`** - RegExp engine

## Implementation Phases

### Phase 1: Foundation (Weeks 1-4)
**Goal**: Basic infrastructure for running simple expressions

- Project setup (SBT, build structure)
- JSValue type system (primitives, objects)
- JSRuntime and JSContext
- Lexer and AST for minimal grammar
- Basic parser for expressions
- Bytecode opcode definitions
- Minimal compiler (literals, binary ops)
- Minimal interpreter (arithmetic opcodes)

**Deliverable**: Can evaluate `1 + 2` and get `3`

### Phase 2: Core Language (Weeks 5-12)
**Goal**: Support most ES5.1 features

- Complete AST for statements
- Variable declarations (var, let, const)
- Function expressions and declarations
- Object and array literals
- Control flow (if/else, loops, switch)
- Variable scoping and hoisting
- Function calls and returns
- Object model (properties, prototype chains)
- Exception handling

**Deliverable**: Can run fibonnacci function

### Phase 3: ES6+ Features (Weeks 13-20)
**Goal**: ES2015-ES2020 feature parity

- Arrow functions
- Classes (extends, super)
- Template literals
- Destructuring
- Default parameters, rest/spread
- Symbols, iterators, generators
- Map, Set, WeakMap, WeakSet
- Promises (async/await)
- Proxy and Reflect

**Deliverable**: Can run modern ES6+ code

### Phase 4: Advanced Features (Weeks 21-28)
**Goal**: ES2021-ES2024 features

- Logical assignment operators
- Numeric separators
- Private class fields (#field)
- Top-level await
- New Array methods
- RegExp match indices

### Phase 5: Standard Library (Weeks 29-36)
**Goal**: Complete standard library

- Object, Array, String, Number APIs
- Math, Date, JSON APIs
- RegExp enhancements
- Intl API (optional)

### Phase 6: Modules & Tooling (Weeks 37-44)
**Goal**: ESM support and developer tools

- Module system (import, export)
- Module loader hooks
- Bytecode compiler (qjsc)
- REPL with advanced features

### Phase 7: Optimization & Polish (Weeks 45-52)
**Goal**: Performance and production readiness

- Inline caching for property access
- Peephole optimizer
- Performance benchmarks
- Test262 conformance (>95% pass rate)
- Documentation

## Performance Considerations

### JVM Optimizations
- **Inline caching** for property access
- **Escape analysis** for stack allocation
- **GraalVM** for better performance
- Profile-guided optimization

### Expected Performance vs QuickJS C
- Startup time: <2x slower
- Memory usage: <3x higher
- Throughput: >50% of C performance
- Test262 pass rate: >95%

## Dependencies

```scala
// build.sbt dependencies
libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "3.1.1",      // Parser combinators
  "org.scalameta" %% "munit" % "1.0.2" % Test, // Testing
  "org.jline" % "jline" % "3.26.1",            // REPL
  "com.github.scopt" %% "scopt" % "4.1.0"      // CLI parsing
)
```

## Testing Strategy

1. **Unit tests** for individual components (JSValue, parser, compiler, interpreter)
2. **Test262 integration** for ES conformance validation
3. **Benchmark suite** for performance tracking

## Success Criteria

### MVP (End of Phase 2 - Week 12)
- Parse and execute ES5.1 code
- Pass 50% of test262 ES5 tests
- REPL with basic functionality
- CLI tool to execute files

### Production Ready (End of Phase 7 - Week 52)
- Full ES2024+ support
- Pass >95% of test262 tests
- Performance within 2-5x of QuickJS C
- Comprehensive documentation

## Key Code Examples

### JSValue Type System

```scala
sealed trait JSValue:
  def tag: JSValue.Tag

object JSValue:
  enum Tag:
    case Undefined, Null, Bool, Int32, Float64, String, Symbol, BigInt, Object

  case object Undefined extends JSValue
  case object Null extends JSValue
  final case class Bool(value: scala.Boolean) extends JSValue
  final case class Int32(value: scala.Int) extends JSValue
  final case class Float64(value: scala.Double) extends JSValue
  final case class String(value: java.lang.String) extends JSValue
  final case class Object(value: JSObject) extends JSValue
```

### Bytecode Interpreter

```scala
final class Interpreter:
  private var stack: Array[JSValue] = _
  private var stackTop: Int = _
  private var pc: Int = _

  def call(function: BytecodeFunction, thisArg: JSValue, args: Array[JSValue]): JSValue = ???

  private def execute(frame: CallFrame): JSValue =
    while pc < bytecode.length do
      val opcode = Opcode.fromCode(bytecode(pc))
      // @switch dispatch for each opcode
```

## Risks and Mitigation

| Risk | Mitigation |
|------|-----------|
| Performance gap vs C | Focus on algorithmic efficiency; accept 2-5x slower |
| Semantic differences | Comprehensive test262 suite; continuous integration |
| Scope creep | Phased implementation with clear milestones |
| Maintenance burden | Clean architecture; comprehensive documentation |

## Timeline Estimate

**12 months** for a production-ready implementation, with incremental value delivered every 4-6 weeks.

## Reference QuickJS Files

These are the key QuickJS source files to reference during implementation:

- `quickjs.c` (59,540 lines) - Main engine
- `quickjs.h` - Public API
- `quickjs-opcode.h` - VM opcodes
- `libregexp.c` (3,448 lines) - Regex engine
- `libunicode.c` (2,123 lines) - Unicode support
- `quickjs-libc.c` - Standard library
- `repl.c` - REPL implementation
- `qjs.c` - Standalone interpreter
- `qjsc.c` - Bytecode compiler
