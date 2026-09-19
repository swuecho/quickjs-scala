# QuickJS-Scala Development Progress

> **Historical snapshot (2026-07-10)**: The numbers and roadmap below reflect
> the July 2026 state of the project. The canonical, up-to-date status — test
> counts, test262 sweep results and priorities — lives in
> [`../AGENTS.md`](../AGENTS.md). For context as of 2026-09-19: the suite has
> 1,341 tests (0 failures), top-level await and module instantiation are
> implemented, and the test262 sweep passes 93.7% of executed tests.

**Last Updated**: 2026-07-10
**Status**: Phase 3 — Substantial language support, most ES2024 features implemented

---

## Executive Summary

QuickJS-Scala is a JavaScript engine written in Scala 3 for the JVM, inspired by the QuickJS C implementation. The project uses a stack-based bytecode interpreter with JVM GC integration, prioritizing type safety, code clarity, and maintainability over raw performance.

### Current Status
- **748 tests passing, 0 failures, 0 errors** (including test262 smoke tests)
- **~22,700 lines of Scala** in main sources (across 77 files)
- **ES2024+ features**: ~85% implemented
- **5 QuickJS C test files** all passing

### Recent Achievements (Dec 2025 – May 2026)
1. ✅ Complete Promise support: constructor, then, catch, finally, resolve, reject, all, race, allSettled, any
2. ✅ Async/await: async functions, await expressions, for-await-of
3. ✅ Generators: function*, yield, yield*, Generator object, for-of iteration
4. ✅ Map, Set, WeakMap, WeakSet: all standard methods
5. ✅ Symbol: constructor, Symbol.for, Symbol.keyFor, well-known symbols
6. ✅ Reflect API: all 13 methods (apply, construct, defineProperty, deleteProperty, get, set, has, ownKeys, getPrototypeOf, setPrototypeOf, getOwnPropertyDescriptor, isExtensible, preventExtensions)
7. ✅ Proxy: constructor with get, set, has, deleteProperty, ownKeys, getOwnPropertyDescriptor, defineProperty traps
8. ✅ BigInt: complete arithmetic, comparison, bitwise, BigInt(), asIntN, asUintN, typeof, literal parsing
9. ✅ Classes: declarations, expressions, extends, super, static/instance methods & fields, getters/setters, private fields & methods
10. ✅ Full error stack traces with JSException
11. ✅ Regex: full support with all flags (global, ignoreCase, multiline, dotAll, unicode, sticky)
12. ✅ Module system: import/export with file-based module loading
13. ✅ Date: full date manipulation and formatting
14. ✅ Scala.js web frontend for bytecode trace visualization
15. ✅ AggregateError, EvalError, URIError, Error `cause`, and Promise.any AggregateError rejection
16. ✅ Array index property descriptor compatibility and Reflect.defineProperty support for arrays
17. ✅ `import.meta` for modules with cached null-prototype module meta objects
18. ✅ Dynamic `import()` with Promise resolution/rejection over existing module loaders
19. ✅ TypedArray indexed property descriptors, indexed `defineProperty`, and own-key enumeration
20. ✅ `TypedArray.from`/`of` generic constructor creation and result validation
21. ✅ `TypedArray` species construction for `map`, `filter`, and `slice`
22. ✅ `TypedArray.prototype.subarray` species construction with shared buffer/offset/length arguments

---

## Test Results Breakdown

### Overall Test Results: ✅ 748/748 (100%)

All Scala unit tests pass. QuickJS C test files are run as integration tests and all five pass their JS-level assertions.

### Test Distribution
- **stdlib**: 239 tests — language features, built-in objects, JSON, arrays, TypedArrays, modules, etc.
- **runtime**: 47 tests — interpreter correctness, closures, try/catch, classes, etc.
- **compiler**: 13 tests
- **parser**: 70 tests (lexer + parser + strict mode)
- **core**: 16 tests
- **REPL**: 23 tests
- **Various debug/trace tests**: ~98 tests

### QuickJS C Test File Status
| File | Status | Remaining Issue |
|---|---|---|
| `test_loop.js` | ✅ All pass | — |
| `test_closure.js` | ✅ All pass | — |
| `test_language.js` | ✅ All pass | — |
| `test_builtin.js` | ✅ All pass | — |
| `test_bigint.js` | ✅ All pass | — |

### Key Test Suites
All Scala test suites pass at 100%:
- QuickJSLanguageTest, QuickJSLoopTest, QuickJSClosureTest
- QuickJSRegExpTest, QuickJSDateTest, QuickJSModuleTest
- ArrayMethodsTest, MapSetTest, ReflectTest, ClassInheritanceTest
- Promise/async/await tests, Generator tests
- JSONTest, ConsoleTest, ObjectFreezeSealTest
- REPLTest suite (13 tests), Interpreter test suite (40 tests)

---

## Architecture Overview

### Module Structure

```
quickjs-scala/
├── core/                          # Core type system
│   └── src/main/scala/quickjs/
│       ├── value/                 # JSValue tagged union, NativeFunction, NativeConstructor
│       ├── runtime/               # JSRuntime, JSContext, ErrorType, GlobalScope
│       ├── objmodel/              # JSObject (279 lines), JSArray (124 lines)
│       └── atom/                  # Atom table (string interning)
│
├── parser/                        # ES2024+ parser
│   └── src/main/scala/quickjs/
│       ├── ast/                   # AST nodes (438 lines)
│       ├── lexer/                 # Lexer (749 lines), Token (102 lines)
│       └── parser/                # Hand-written recursive descent (2,184 lines)
│
├── compiler/                      # Bytecode compiler
│   └── src/main/scala/quickjs/
│       ├── bytecode/              # Opcode definitions (153 lines), Instruction (314 lines)
│       └── compiler/              # Compiler (3,616 lines)
│
├── runtime/                       # Interpreter & standard library
│   └── src/main/scala/quickjs/
│       ├── interpreter/           # Interpreter (293 lines), BytecodeLoop (1,559 lines),
│       │                          # Frame, GeneratorSupport (424 lines), PropertyAccess, DebugMode
│       ├── runtime/builtins/      # StdLib built-in objects (see below)
│       ├── module/                # Module loading
│       ├── repl/                  # REPL with completion
│       ├── tracing/               # Bytecode trace recording & visualization
│       └── diagnostic/            # Error formatting
│
├── stdlib/                        # Standard library helpers & test runner
│   └── src/main/scala/quickjs/stdlib/
│       ├── Runner.scala           # QuickJS C test runner
│       ├── ArrayStatics.scala     # Legacy array helpers
│       ├── JSON.scala, Console.scala, MathStatics.scala, etc.
│       └── Main.scala             # Entry point
│
└── web/                           # Scala.js web frontend
    └── src/main/scala/quickjs/web/
        ├── components/            # UI components (trace, stack, editor, bytecode)
        ├── state/                 # Redux-style state management
        └── features/              # Feature slices
```

### Standard Library Built-ins (runtime/src/main/scala/quickjs/runtime/builtins/)

| File | Lines | Implements |
|------|-------|------------|
| `NumberStringBuiltins.scala` | 992 | Number, String constructors & prototypes |
| `ArrayBuiltins.scala` | 975 | Array constructor & prototype (32 methods) |
| `MapSetBuiltins.scala` | 846 | Map, Set, WeakMap, WeakSet |
| `InternalHelpers.scala` | 710 | for-in, modules, test helpers |
| `ObjectBuiltins.scala` | 702 | Object static methods & prototype |
| `ReflectBuiltins.scala` | 575 | Reflect API (13 methods) |
| `PromiseBuiltins.scala` | 529 | Promise, async/await |
| `DateBuiltins.scala` | 385 | Date constructor & prototype |
| `FunctionBuiltins.scala` | 200 | Function constructor, call, apply, bind |
| `MathBuiltins.scala` | 217 | Math static methods & constants |
| `RegExpBuiltins.scala` | 133 | RegExp constructor & prototype |
| `SymbolBuiltins.scala` | 118 | Symbol constructor, well-known symbols |
| `ErrorBuiltins.scala` | 120 | Error, TypeError, ReferenceError, SyntaxError, RangeError |
| `BigIntBuiltins.scala` | 115 | BigInt constructor, asIntN, asUintN |
| `ProxyBuiltins.scala` | 31 | Proxy constructor with 7 traps |
| `StdLib.scala` | 55 | Initialization facade |

### Key Design Decisions

1. **Stack-based Bytecode Interpreter**
   - Matches QuickJS architecture for easier porting
   - Better control over semantics and debugging
   - Trade-off: Slower than direct JVM bytecode generation

2. **JVM GC Integration**
   - Eliminates 2,000+ lines of complex GC code
   - Leverages mature JVM garbage collectors (G1, ZGC, Shenandoah)
   - Trade-off: Less control over GC pauses

3. **Tagged Union Type System**
   - Sealed trait `JSValue` with case classes
   - Smart constructors for type coercion and optimization
   - Inline storage for small values (Int32, Bool, Null, Undefined)

4. **Hand-written Recursive Descent Parser**
   - Hand-written parser (not parser combinators)
   - Explicit grammar rules with proper operator precedence
   - Good error recovery
   - Supports full ES2024+ syntax

---

## Implementation Timeline

### Phase 1: Foundation ✅ Complete
- ✅ JSValue type system with smart constructors
- ✅ Lexer and parser (hand-written recursive descent)
- ✅ Basic compiler (literals, binary operations)
- ✅ Interpreter (arithmetic opcodes)
- ✅ "1 + 2 = 3" end-to-end test

### Phase 2: Core Language ✅ Complete
- ✅ Variables (var, let, const with block scoping and TDZ)
- ✅ Control flow (if/else, while, for, do-while, switch)
- ✅ Functions (declarations, expressions, closures, arrow functions)
- ✅ Objects (literals, property access, methods, prototypes)
- ✅ Arrays (literals, all ES2024 methods, element access)
- ✅ Operators (arithmetic, bitwise, logical, comparison, typeof, instanceof, in, delete, void, new)
- ✅ Labeled statements (break/continue with labels)
- ✅ try/catch/finally with stack traces
- ✅ with statement
- ✅ REPL with completion

### Phase 3: Advanced ES6+ Features ✅ Substantially Complete (~85%)
- ✅ Classes (declarations, expressions, extends, super, static/instance, getters/setters, private fields/methods)
- ✅ Arrow functions (concise and block body)
- ✅ Template literals (basic, interpolation, multi-line, tagged templates with `.raw`)
- ✅ Destructuring (array, object, nested, defaults, rest, parameter)
- ✅ Default parameters, rest/spread
- ✅ Symbol (constructor, Symbol.for, Symbol.keyFor, well-known symbols)
- ✅ Iterators and Generators (function*, yield, yield*, for-of)
- ✅ Map, Set, WeakMap, WeakSet
- ✅ Promise (constructor, then, catch, finally, resolve, reject, all, race, allSettled, any)
- ✅ Async/await (async functions, await, for-await-of)
- ✅ Proxy (constructor with 7 traps)
- ✅ Reflect (all 13 methods)
- ✅ BigInt (full arithmetic, comparison, bitwise)
- ✅ Optional chaining (?.)
- ✅ Nullish coalescing (??)
- ✅ Modules (import/export, file-based loading)
- ✅ Regex (full support with all flags)
- ✅ JSON (parse, stringify with reviver/replacer/space)
- ✅ Logical assignment (&&=, ||=, ??=)
- ✅ Tagged template literals
- ✅ Spread/rest in destructuring patterns
- ✅ WeakRef / FinalizationRegistry API surface
- ✅ AggregateError, EvalError, URIError
- ✅ import.meta
- ✅ Dynamic import()

### Phase 4: Binary Data & Completeness 🔜 Next Up
- ⚠️ TypedArrays / ArrayBuffer / DataView — implemented with remaining conformance gaps
- ❌ String.prototype.normalize() (wrappers java.text.Normalizer but needs validation)
- ❌ top-level await

### Phase 5: Optimization & Polish (Future)
- ❌ Inline caching for property access
- ❌ Peephole optimizer
- ❌ Performance benchmarks (JMH)
- ✅ **test262 conformance suite** — Integrated (see test results below)

---

## Remaining Work

### High Priority (Bug Fixes)

1. **Object/property descriptor conformance** — remaining edge cases across Object, Reflect, Proxy, and TypedArrays

2. **TypedArray, ArrayBuffer, and DataView conformance** — finish resizable/immutable buffer, deeper subclass species, iterator-closing, and detached-buffer edge cases

### Medium Priority (Missing ES Features)

3. **Top-level await** — Async module evaluation

4. **Module loading polish** — dynamic `import()` is implemented; remaining work is async module integration edge cases

### Lower Priority (Polish)

5. **Broaden source-location coverage** beyond the error paths exercised by the QuickJS regression suite
6. **Performance optimization** (inline caching, peephole optimizer)
7. **Expand test262 coverage** beyond smoke suites
8. **Code coverage measurement** (scoverage/JaCoCo)
9. **JMH benchmarks** for performance tracking

---

## Technical Highlights

### Performance Optimizations
- **Smart constructors**: `fromDouble()`, `fromLong()` optimize to Int32 when appropriate
- **Tagged union values**: Pattern matching with exhaustiveness checking
- **@switch dispatch**: Interpreter uses `@switch` annotation for fast opcode dispatch
- **JVM GC integration**: Leverages G1, ZGC, Shenandoah instead of custom mark-and-sweep

### Code Quality
- **Type safety**: Sealed traits prevent invalid states
- **Null safety**: Option types for optional values
- **Pattern matching**: Exhaustive checking prevents bugs
- **Test coverage**: 748 tests, 0 failures, 77 main source files (including test262 smoke tests)

### Known Limitations
1. **No performance optimization**: Focus is on correctness and feature completeness
2. **Partial line/column coverage**: parser, eval, calls, operators, and property access are covered; less common paths still need audit
3. **TypedArray conformance gaps**: resizable/immutable ArrayBuffer variants, deeper subclass species edge cases, iterator-closing paths, and detached-buffer checks remain
4. **Compiler.scala is monolithic**: 3,616 lines — needs phase splitting
5. **Some edge cases** with eval + arrow function bindings and strict mode argument scopes

---

## Key Files Reference

| File | Purpose | Lines | Status |
|------|---------|-------|--------|
| `compiler/.../Compiler.scala` | AST to bytecode compiler | 3,616 | ⚠️ Needs splitting |
| `parser/.../Parser.scala` | Hand-written recursive descent parser | 2,184 | ✅ Stable |
| `runtime/.../BytecodeLoop.scala` | Main interpreter dispatch loop | 1,559 | ✅ Stable |
| `runtime/.../NumberStringBuiltins.scala` | Number & String built-ins | 992 | ✅ Stable |
| `runtime/.../ArrayBuiltins.scala` | Array constructor & prototype (32 methods) | 975 | ✅ Stable |
| `runtime/.../MapSetBuiltins.scala` | Map, Set, WeakMap, WeakSet | 846 | ✅ Stable |
| `parser/.../Lexer.scala` | Lexer | 749 | ✅ Stable |
| `runtime/.../InternalHelpers.scala` | for-in, modules, test infra | 710 | ✅ Stable |
| `runtime/.../ObjectBuiltins.scala` | Object static methods & prototype | 702 | ✅ Stable |
| `stdlib/.../JSON.scala` | JSON.parse/stringify | 670 | ✅ Stable |
| `runtime/.../ReflectBuiltins.scala` | Reflect API | 575 | ✅ Stable |
| `runtime/.../PromiseBuiltins.scala` | Promise, async/await | 529 | ✅ Stable |
| `parser/.../AST.scala` | AST node definitions | 438 | ✅ Stable |
| `runtime/.../GeneratorSupport.scala` | Generator yield/resume | 424 | ✅ Stable |
| `core/.../JSValue.scala` | Tagged union type system | 396 | ✅ Stable |
| `runtime/.../DateBuiltins.scala` | Date constructor & prototype | 385 | ✅ Stable |
| `core/.../JSContext.scala` | Execution context | 318 | ✅ Stable |
| `compiler/.../Instruction.scala` | Bytecode instruction encoding | 314 | ✅ Stable |
| `runtime/.../Interpreter.scala` | Call entry point | 293 | ✅ Stable |
| `core/.../JSObject.scala` | Object model | 279 | ✅ Stable |
| `core/.../JSArray.scala` | Array storage | 124 | ✅ Stable |

---

## Build and Test

```bash
# Compile all modules
sbt compile

# Run all tests (748 tests, 0 failures)
sbt test

# Run specific test suite
sbt "stdlib/testOnly quickjs.stdlib.QuickJSLanguageTest"

# Run specific test by name
sbt "stdlib/testOnly quickjs.stdlib.QuickJSLanguageTest -- -z \"arithmetic\""
```

---

## Dependencies

```scala
// Scala 3.7.4
libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.0.2" % Test, // Testing
  "org.jline" % "jline" % "3.26.1"             // REPL
)
// Web frontend:
//   "com.raquo" %%% "laminar" % "16.0.0"
//   "org.scala-js" %%% "scalajs-dom" % "2.8.0"
```

No parser combinator libraries — the parser is hand-written for full control over grammar and error recovery.

---

## References

- **QuickJS C implementation**: `quickjs.c` from upstream QuickJS (60,000 lines)
- **QuickJS opcodes**: `quickjs-opcode.h` (upstream QuickJS)
- **Rewrite plan**: `docs/SCALA_REWRITE_PLAN.md`
- **Feature comparison**: `docs/QUICKJS_COMPARISON.md`
- **Parser comparison**: `docs/PARSER_COMPARISON.md`
- **Other docs**: `docs/CLOSURE_IMPLEMENTATION.md`, `docs/REPL.md`, `docs/ideas.md`

---

## Conclusion

QuickJS-Scala has achieved **substantial milestones**:
- ✅ Core language features fully working (all ES5.1 + most ES6+)
- ✅ Advanced ES2015-ES2024 features largely implemented (~85%)
- ✅ 748 tests passing, 0 failures (including test262 smoke tests)
- ✅ Solid architecture foundation with clean module separation
- ✅ Type-safe implementation leveraging Scala 3 sealed traits
- ✅ REPL with completion and debugging support
- ✅ Scala.js web frontend for bytecode trace visualization

The project is in **very good shape** for most JavaScript code. The main remaining gaps are TypedArray/ArrayBuffer conformance edge cases, top-level await, and performance optimization work.

**Current Status**: Phase 3 — Solid ES2024 engine with ~85% coverage. Suitable for most application-level JavaScript.
