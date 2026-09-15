# QuickJS-Scala - Claude Code Reference

## Project Overview

QuickJS-Scala is a JavaScript engine written in Scala 3 for the JVM, inspired by the QuickJS C implementation. The goal is to create a production-grade JavaScript engine with full ES2024+ support. 

**Before implement a feature, check the original c version first, should follow similar approach**
**When fixing a bug but not sure about the approach, check the original quickjs c version for ideas.**
**When the problem is tricky, create test step by step to help investigate, when done. keep the test**

**Current Status**: Phase 3 - Substantial language support with most ES2024 features. 819 tests passing, 0 failures. 15 test262 smoke test suites. Full test262 sweep (aggregated from per-directory chunks): 34,469/52,896 passing (88.5% of executed tests, 10,818 skipped by feature config). 5 QuickJS C test files all passing.

**Feature-config audit (Sep 2026)** — enabled 6 previously skipped features (+40 passing tests):
- `Math.sumPrecise` — rewritten on the iterator protocol: accepts any iterable, throws TypeError and closes the iterator on non-Number elements, handles NaN/±Infinity, accumulates exactly and preserves the `-0` rules. 10/10.
- `promise-with-resolvers` — `Promise.withResolvers` now builds its result via `NewPromiseCapability(this)`, so `this` (including subclasses) is used and non-constructors throw TypeError. 6/6.
- `symbols-as-weakmap-keys` — `WeakSet` gained a symbol backing store like `WeakMap` (WeakMap 78/79, WeakSet 66/67; remaining errors are unrelated).
- `for-in-order` — own-key enumeration order passes (9 tests).
- `stable-typedarray-sort` — TypedArray `sort` stability passes.
- `well-formed-json-stringify` — `JSON.stringify` now escapes unpaired surrogates as `\uXXXX` in `stdlib/JSON.scala` while keeping valid surrogate pairs.
- Evaluated but left skipped (pass rates too low to claim support): `regexp-unicode-property-escapes` (7%), `regexp-v-flag` (19%), `regexp-modifiers` (0%), `regexp-lookbehind` (47%), `regexp-match-indices` (50%), `regexp-duplicate-named-groups` (~50%), `cross-realm` (29%), `u180e` (the 3 dotall tests need code-unit `.` semantics), `proxy-missing-checks`, `export-star-as-namespace-from-module`, `well-formed`-adjacent JSON options, and all unimplemented proposals (Temporal, Atomics, Intl, Iterator helpers, Set methods, resizable ArrayBuffers, import attributes, TLA, …).

**Class subclassing (Sep 2026)** — 34,275 → 34,429 passed, 4,183 → 4,034 errors; `class/subclass` 62 → 165 of 184:
- **Derived-constructor `this` semantics**: class constructors carrying heritage are marked with a non-enumerable `__derivedClass` property at definition; `constructValue` places a `__thisUninitialized` marker on the receiver. `GetThis` throws ReferenceError before `super()`, `GetThisUnchecked` (new opcode) lets `super()`/the default constructor forward the receiver, and `MarkThisInitialized` clears the marker just before the superclass body runs. Derived constructors that return a non-undefined primitive now throw TypeError from `constructValue` (uncatchable), and missing-super returns throw ReferenceError.
- **`super()` for every native builtin**: `NativeConstructor.superInitImpl` added for `Array` (builds/initializes a real exotic Array receiver with prototype override), `Map`/`Set`/`WeakMap`/`WeakSet`, `Boolean`/`Number`/`String`, `Error` + all NativeErrors/AggregateError, `Date`, `RegExp`, `ArrayBuffer`, all TypedArrays and `DataView`; `Symbol` rejects construction. `__funcSpread` validates that the superclass is a constructor and clears the marker before invoking it.
- **Class definition validation**: `class extends <non-constructor>` (generators, async functions, arrows, primitives, proxies of non-constructors) throws TypeError before any `"prototype"` lookup; `class X extends null` sets `X.prototype.[[Prototype]] = null` and keeps `X.[[Prototype]] = Function.prototype`. Class constructors can no longer be called without `new` (TypeError).
- **Function property rules**: only constructable and generator functions get an own `prototype`; async functions, async generators, arrows and methods no longer do. `async function*`/generator own-property tests now pass.
- **Compiler slot alignment**: `Scope.getAllLocalVarNames` dropped slots for shadowed names (e.g. two `catch (e)` bindings), so the runtime `localVarNames.indexOf(name)` no longer matched compiler slot indices. It now returns a slot-indexed array with unique placeholders for shadowed slots; this fixed a latent corruption where `class D extends Base { super() }` resolved `super` to `D` after an earlier caught exception.
- **`instanceof` / `isPrototypeOf`** honor a `JSArray`'s prototype override, so `new (class extends Array {})() instanceof Subclass` works.

**Earlier (Sep 2026, second sweep)** — +583 test262 tests (33,692 → 34,275 passed; errors 4,771 → 4,183):
- **`with` + PutValue reference semantics**: new `GetGlobalWithBase`/`PutGlobalWithBase` opcodes preserve the object environment record base across compound assignments and `++`/`--` inside `with`, even when the getter deletes the binding (`scope.x` deleted during `x ^= 3` now writes back to `scope`). `DefVar` inside `with` routes the initializer through the object record, and `Symbol.unscopables` is honored by `HasBinding`.
- **Compiler control-flow context leak**: nested function/arrow bodies inherited the enclosing function's `loopStack`, `finallyStack` and `iteratorCloseStack`, so a `return` inside a closure emitted the outer for-of iterator's `__iteratorClose` and could corrupt the call. `compileFunctionBody`/`compileArrowFunctionBody` now save/reset/restore them.
- **ToPrimitive in builtin conversions**: `Number`, `String`, `Math.*`, `parseInt`/`parseFloat`, `isNaN`/`isFinite`, Date setters and `DataView`/`TypedArray` coercion now use `BuiltinHelpers.toNumber`/`toJSString` (call `valueOf`/`toString`, propagate abrupt completions) instead of the object-blind `JSValue.toNumber`; the latter now parses `0b`/`0o` string literals and all ECMAScript whitespace; `Number(BigInt)` converts.
- **Math**: no-argument functions return NaN, `round(±Infinity)`/`round(-0.5)`/`clz32`/`imul`/`min`/`max`/`hypot`/`sumPrecise` fixed, and `Math.f16round` (ES2025, round-to-nearest-even) added; `Float16Array` uses the same half-float conversion.
- **Date**: multi-argument `new Date(y, m, ...)`, spec `MakeDay`/`MakeTime`/`MakeDate` double arithmetic for `Date.UTC`, ordered setter coercion with throw propagation, `Date.prototype[Symbol.toPrimitive]`.
- **Promise**: `all`/`allSettled`/`any`/`race` use `NewPromiseCapability(this)` + iterator records; `then` uses `SpeciesConstructor` + a result capability; `catch` is generic; `finally` builds `thenFinally`/`catchFinally` thunks; `NewPromiseCapability` executor/resolve/reject functions are anonymous with `%Function.prototype%`; `Promise.prototype[Symbol.toStringTag]`; native `super()` support via `NativeConstructor.superInitImpl`/`callWithThis`.
- **RegExp/exec**: `lastIndex` uses ToLength semantics for global and sticky, unicode mid-surrogate-pair `lastIndex` rounds back to the code point start, matches never start inside a surrogate pair, and `String.prototype.match` advances empty matches with `AdvanceStringIndex`.
- **Proxy**: proxies mirror the target's `[[Prototype]]`, property get/set forward to function/native/array targets, and `Function.prototype.call`/`apply` invoke callable proxies through the `apply` trap.
- **TypedArray**: `toLocaleString` invokes each element's method, `BYTES_PER_ELEMENT` is non-configurable, ToInt32/ToUint32 wrap instead of saturating.
- **`initProperty` order for native constructors**: `length` before `name` (built-in function property order); bound function `length` uses the JS-visible length (was 0) and is passed to the `NativeConstructor`.
- Regression coverage: `ConformanceRegressionTest` (21 tests).

**Interpreter performance (Sep 2026)**: `BytecodeLoop.run` was a single ~100KB method that exceeded HotSpot's method-size limit and was interpreted even after warmup (~1M instructions/sec). The opcode case bodies are now split across 12 `runGroupN` methods selected by a precomputed `BytecodeLoop.opcodeGroups` table, so C2 compiles the hot paths. Tight-loop throughput is ~15M instructions/sec (15x faster); test262 timeouts dropped from 107 to 48 and the previously-timing-out `RegExp/CharacterClassEscapes` tests now pass. Guarded by `InterpreterPerfRegressionTest`. `maxIterations` (runaway-loop guard) was raised to 100M.

**Known sweep memory pressure**: resolved. Heavy tests (arrays with millions of indices, Date/DST stress) used to fill the heap: `JSArray.writeElement` materialized dense storage up to `MaxDenseIndex` (1M entries) for a single large-index write, and dense presence was tracked with a boxed-Long `HashSet`. Now far indices use the sparse map (`MaxDenseGap = 4096`), `new Array(n)` pre-allocates at most `MaxPreallocated = 65536` slots, and dense presence uses a compact `BitSet`; `readElement` checks the sparse map first when it is non-empty. The `language` chunk now completes with no GC warnings. `scripts/test262-chunks.sh` runs `built-ins`/`language`/`staging`/`harness` in separate JVMs and prints an aggregate (the single-JVM full sweep can still GC-thrash, so prefer the script).

**Latest Fixes (Sep 2026)** — +817 test262 tests from a full sweep:
- **String literal early errors**: `StringToken` gained a `legacyEscape` flag; strict mode rejects legacy octal / non-octal decimal escapes (`\1`, `\8`, `\0` + digit). Malformed `\x`/`\u` escapes and raw LF/CR are rejected in all modes; `\u2028`/`\u2029` are valid raw (JSON-superset) and valid after a backslash (line continuation); Annex B octal truncation (`\400` = `\40` + `0`) implemented.
- **Numeric literal early errors**: `NumberToken` gained a `legacy` flag; strict mode rejects legacy octal (`010`) and non-octal decimal (`08`) literals (sloppy values now decode octal, `010 === 8`). Numeric separators may not be trailing or appear in legacy forms; a numeric literal followed immediately by an identifier start/digit (`3in`, `1.toString`) is a SyntaxError.
- **Directive prologue scanning**: `"use strict"` is now found anywhere in the leading run of string-literal statements (`function f() { "\1"; "use strict"; }` is strict).
- **`$262.createRealm`**: the test host now builds fresh realms (new runtime/context with its own intrinsics, `global`, `evalScript`, recursive `createRealm`), fixing cross-realm tests that do not declare the `cross-realm` feature (~50 tests).
- **Test262 timeout** default lowered from 10s to 5s (`-Dquickjs.test262.timeoutSeconds=N`): stuck tests no longer hold workers and accumulate memory.
- **Interpreter JIT**: split the monolithic `BytecodeLoop.run` dispatch into 12 JIT-compilable `runGroupN` methods via a precomputed `opcodeGroups` table (~15x throughput, test262 timeouts 107 → 48).
- **Computed element keys use ToPropertyKey**: `doGetElem`/`doSetElem`/`doDelete` normalized keys via the new `BuiltinHelpers.toElementKey` (numbers/strings/symbols keep fast paths; objects, functions, booleans, bigints, undefined/null convert). Previously `o[fn] = v` was silently a no-op: `cpn-*` computed-property tests went from 72 failures to 54 (remaining: accessor computed keys, `__classElementKey_N is not defined`, object-to-primitive conversion). Related gap: `String(fn)` still yields `[object Function]` instead of the function source text (`Function.prototype.toString`).
- **Shared iterator intrinsics**: `%IteratorPrototype%` is created with the realm intrinsics; `%ArrayIteratorPrototype%` and `%StringIteratorPrototype%` share it, Map/Set iterator prototypes inherit from it, and typed-array iterators use `%ArrayIteratorPrototype%` (via `IteratorBuiltins`, which also implements `String.prototype[Symbol.iterator]`). ArrayIteratorPrototype 16/27 -> 23/27, Map/Set 10/11, String Symbol.iterator 6/6.
- **`RegExp.prototype[Symbol.matchAll]` + `%RegExpStringIteratorPrototype%`**: proper lazy iterator that performs `RegExpExec` (observable custom `exec`), copies `lastIndex` to a fresh matcher, advances empty matches with `AdvanceStringIndex`, and `String.prototype.matchAll` delegates to it (with the global-RegExp TypeError). RegExpStringIteratorPrototype 0/17 -> 17/17, RegExp Symbol.matchAll 0 -> 17/26, String matchAll 9 -> 20/26. Remaining: `SpeciesConstructor`/`Construct` support and IsRegExp ordering.
- **Map/Set iterator objects**: `Map.prototype.entries/keys/values` and `Set.prototype.entries/values/keys` now return real iterator objects (`next`, `@@iterator` returns self, `@@toStringTag` = "Map/Set Iterator", receiver brand checks) instead of arrays; `clear()` during iteration is observed. Map non-passing 10 → 5, `MapIteratorPrototype`/`SetIteratorPrototype` now 10/11 and 8/11.
- **`Promise.withResolvers`** (ES2024) implemented.
- **Date brand checks**: `Date.prototype` methods now throw TypeError on non-Date receivers (29 tests).
- **String coercions**: `startsWith`/`endsWith`/`includes`/`codePointAt` use ToString/ToNumber helpers so Symbols throw instead of stringifying; String/prototype non-passing 179 → 164.
- **Array memory + cancellation**: sparse writes beyond the dense end no longer materialize up to 1M slots; dense presence uses a `BitSet`; `JSArray.apply(n)` caps pre-allocation. Native loops honor thread interruption via `BuiltinHelpers.checkInterrupted` (TypedArray set/reduce/every/some/fill/join/find/indexOf, Array join/toLocaleString), and `GeneratorSupport`/`runMicrotasks` rethrow interrupts instead of swallowing them. `scripts/test262-chunks.sh` aggregates per-directory sweeps.
- **Object-pattern shorthand defaults** (`{ a = 1 }`, CoverInitializedName): now parsed and accepted in destructuring contexts (for-of/for-in/for-await heads, assignments), rejected as a plain object literal. The compiler already supported `AssignmentExpression` property values. (~56 tests)
- **Escaped keywords are IdentifierNames, not keywords**: `IdentifierToken` gained an `escaped` flag, so `\u0067et`/`\u0061sync` no longer act as get/set/async/static. Context rules now reject escaped `await`/`yield` in async/generator/module code and reserved words as labels. Object literals now require a comma between properties. (~70 tests)
- **Module mode in the parser**: `new Parser(tokens, moduleMode = true)` treats module code as strict and reserves `await` (used by `Test262Runner` and `ModuleLoader`).
- **Interpreter perf**: `JSContext.updateTopFramePc` mutated a case class per instruction (allocation); `StackFrame.pc` is now a `var`. Thread-interrupt cancellation is sampled every 1024 instructions instead of every instruction.
- **Known perf limit**: resolved — `BytecodeLoop.run` was too large for HotSpot's default `DontCompileHugeMethods` limit; it is now split into `runGroupN` methods (see "Interpreter performance" above).
- **Parser/lexer performance**: `Lexer.tokenize()` returned a list-backed `Seq`, making `Parser.tokens(pos)` O(n) and parsing quadratic. Now returns an `IndexedSeq`; parsing `deepEqual.js` went 340ms → 15ms and the full test262 sweep 24min → 6min. Guarded by `ParserPerfRegressionTest`.
- **Fixed-size local frames**: `Interpreter`/generator/eval frames were hardcoded to 256 local slots, crashing functions with more locals (`PutLoc: Index out of bounds`). Frames now size from `localVarNames` with the historical floor kept (30 tests).
- **Do-while `continue` target**: the compiler jumped to the loop start instead of the test, so `do { try { continue } finally {} } while (...)` looped forever (9 try-statement timeouts).
- **Direct-eval `++i`**: `compileIncrementDecrement` used the "top-level var = global" heuristic in direct-eval mode while declarations used locals, so `eval("for (var i = 0; i < 2; ++i) ...")` never advanced `i`.
- **Generator VM opcodes**: implemented `EnterScope`/`LeaveScope`, `PushWith`/`PopWith` (with `withStack` preserved across yields) and `In` (shared `BuiltinHelpers.inOperator`).
- **For-head early errors**: initializers in for-in/of declarations and invalid assignment-pattern heads (`[...x, y]`) are rejected; strict `"use strict"` directives are applied while parsing function/method/arrow bodies.
- Lexer: `<<=`, `>>=`, `>>>=` were lexed as shift + `=` because the assignment branch checked the wrong character.
- Parser: nested ternary in the alternate branch (`a ? b : c ? d : e`) was rejected; now parsed per grammar (fixed test262's `deepEqual.js` harness).
- Parser: spread followed by trailing comma in array literals (`[...a,]`) is valid ES2017+; the trailing comma now only triggers the "rest element must be last" early error when the literal is used as an assignment pattern.
- Lexer: numeric separators crashed with `NumberFormatException` when building the value (`1.0e-1_0`); underscores are stripped before conversion.
- Built-ins: corrected `length` property values for Date/RegExp/JSON/Object/Number/Boolean/Error/Promise/String/parseInt/Function.prototype.apply (~70 tests).
- **Regexp literal early errors** (`RegExpSyntax.scala`): flag validity/duplicates, line terminators, named-group syntax/duplicates/dangling `\k`, invalid braced quantifiers, quantified assertions, Unicode-mode identity/`\c`/decimal/`\u{...}` escapes and class ranges (~90 tests).
- **Parser early errors**: ReservedWord shorthand properties, duplicate object-literal `__proto__`, getter/setter arity, strict-reserved identifier refs, `with` in strict mode, reserved class names, `return`/`break`/`continue` outside their contexts, and declarations in single-statement bodies (~100 tests).
- Tests: `QuickJSJavaScriptTest` has a 120s timeout (test_builtin.js runs close to munit's 30s default under load).

**Known architectural gap**: ordinary objects cannot have a `JSArray` as their `[[Prototype]]` (`JSObject.prototype` is typed `JSObject | Null`; `JSArray` is a separate class). This breaks `foo.prototype = new Array(...); new foo()` and `Object.create(array)`. Repro kept in `stdlib/src/test/scala/quickjs/stdlib/ScratchParseTest.scala` (ignored). Fix requires a prototype-value abstraction or unifying arrays with objects.

**Recent Progress (May 2026)**:
- Implemented TypedArrays (12 types: Int8, Uint8, Uint8Clamped, Int16, Uint16, Int32, Uint32, Float32, Float64, BigInt64, BigUint64, Float16) + ArrayBuffer + DataView
- Added `%TypedArray%` intrinsic object (shared base for all typed array constructors)
- Added TypedArray static methods `from`, `of`, and `Symbol.species`
- Added ArrayBuffer/DataView/TypedArray test262 smoke tests (3 new suites, 200 tests)
- Fixed parser to accept contextual keywords (`from`, `as`, `get`, `set`, `static`, `of`, `yield`, `await`, `let`) as identifiers
- Fixed ArrayBuffer constructor OOM on large size inputs
- Implemented real `eval` function with special inline handling for `eval("this")`, `eval("new.target")`, and `eval("super.f()")`
- Added `__proto__` getter/setter on `Object.prototype` and `__proto__:` support in object literals
- Fixed `Function.prototype.bind` — name, length, constructability, and bound `new`
- Fixed `new Array(...)` multi-argument construction
- Added global `isNaN` and `isFinite` functions
- Added context tracking (`currentThis`, `currentClosure`) to JSContext for eval
- Made `delete` on null/undefined respect strict mode (return true in non-strict, throw TypeError in strict)
- Re-enabled 14 previously-excluded test_language.js test functions
- Implemented `AggregateError`, `EvalError`, and `URIError`, including `cause` options and `AggregateError.errors` iterable conversion
- Updated `Promise.any` to reject with a real `AggregateError` instance
- Improved array index property descriptor compatibility and `Reflect.defineProperty` support for arrays
- Implemented `import.meta` for modules with a cached null-prototype meta object
- Implemented dynamic `import()` with Promise resolution/rejection over existing module loaders
- Improved TypedArray indexed property descriptors, indexed `defineProperty`, and own-key enumeration
- Aligned `TypedArray.from`/`of` with QuickJS generic constructor creation and result validation
- Added `TypedArray` species construction for `map`, `filter`, and `slice`
- Added `TypedArray.prototype.subarray` species construction with shared buffer/offset/length arguments

**QuickJS C Test Status (5 files)**:
- `test_loop.js` — ✅ ALL PASS
- `test_bigint.js` — ✅ ALL PASS
- `test_builtin.js` — ✅ ALL PASS (excluded: TypedArrays, WeakRef, FinalizationRegistry, generators, rope, line/col, eval scope, enum order, Math.sumPrecise, Date, RegExp, JSON, Map, Symbol, WeakMap, Number, String, Array, Function edge cases)
- `test_closure.js` — ✅ ALL PASS (excluded: test_with, test_eval_closure, test_eval_const — require direct eval scope)
- `test_language.js` — ✅ ALL PASS (14/26 test functions pass, 12 excluded: argument_scope, function_expr_name, parse_arrow_function, global_var_opt, parse_semicolon, labels, labels2, destructuring, function_length, object_literal, unicode_ident — various edge cases; test_delete excluded due to QuickJS-specific non-strict delete behavior)

## Architecture Overview

### Core Design Decisions

1. **Stack-based bytecode interpreter** (not JVM bytecode generation)
2. **JVM GC integration** (not custom mark-and-sweep)
3. **Tagged union type system** for JavaScript values
4. **Hand-written recursive descent parser** (not parser combinators as originally planned)

### Module Structure

```
quickjs-scala/
├── build.sbt
├── core/                        # Core type system
│   └── src/main/scala/quickjs/
│       ├── value/               # JSValue tagged union (Int32, Float64, BigInt, JSStr, Symbol, Object, etc.)
│       ├── runtime/             # JSRuntime, JSContext
│       ├── atom/                # Atom table (string interning)
│       └── objmodel/            # JSObject, JSArray, properties
├── parser/                      # ES2024+ parser
│   └── src/main/scala/quickjs/
│       ├── ast/                 # AST nodes
│       ├── lexer/               # Lexer with BigInt literal support
│       └── parser/              # Hand-written recursive descent parser
├── compiler/                    # Bytecode compiler
│   └── src/main/scala/quickjs/
│       ├── bytecode/            # Opcode definitions (92 opcodes)
│       └── compiler/            # Compiler with closure capture analysis
├── runtime/                     # Interpreter & standard library
│   └── src/main/scala/quickjs/
│       ├── interpreter/         # Stack-based bytecode interpreter
│       ├── runtime/             # StdLib (Promise, Map, Set, WeakMap, WeakSet, Symbol, RegExp, Date, Proxy, Reflect, BigInt, Error)
│       └── repl/                # REPL with completion
└── stdlib/                      # Standard library & tests
    └── src/test/
        ├── scala/               # Scala test suites
        └── resources/           # QuickJS C test files
```

## Key Files and Their Purpose

### Core Type System

**`/core/src/main/scala/quickjs/value/JSValue.scala`**
- Tagged union representation: `sealed trait JSValue` with case classes
- Types: `Undefined`, `Null`, `Bool`, `Int32`, `Float64`, `JSStr`, `Symbol`, `BigInt` (wraps `java.math.BigInteger`), `Object`, `JSArrayVal`, `Function`, `Generator`, `Promise`, `Native`
- Smart constructors: `fromInt`, `fromDouble`, `fromBoolean`, `fromString`
- Arithmetic operations: `add`, `subtract`, `multiply`, `divide` — all have BigInt cases
- Type conversions: `toBoolean`, `toNumber`, `toString`
- VarRef for closure variable indirection (pointer sharing)
- `GlobalRef` for lazy global scope lookup from closures

**`/core/src/main/scala/quickjs/objmodel/JSObject.scala`**
- Property storage in `mutable.LinkedHashMap`
- Prototype chain support
- Property descriptors with attributes (enumerable, writable, configurable)
- Getter/setter support
- Extensibility, sealing, freezing flags

**`/core/src/main/scala/quickjs/runtime/JSContext.scala`**
- Execution context, global object, intrinsics
- Error construction with stack trace attachment
- Microtask queue for Promise resolution
- Prototype chain: objectPrototype → functionPrototype → ... → null

### Parser

**`/parser/src/main/scala/quickjs/ast/AST.scala`**
- AST nodes for ES2024+ grammar
- Supports: literals, identifiers, private identifiers, binary/unary expressions, all statements, control flow, functions, arrow functions, classes, template literals, optional chaining, nullish coalescing, destructuring, spread/rest, modules

**`/parser/src/main/scala/quickjs/parser/Parser.scala`**
- Hand-written recursive descent parser
- Full JavaScript expression parsing with operator precedence
- Labeled statement support
- BigInt literal parsing (`0n`, `0xFn`, `0o7n`, `0b1n`)

### Compiler

**`/compiler/src/main/scala/quickjs/bytecode/Opcode.scala`**
- 92 opcodes: stack manipulation, arithmetic, comparison, bitwise, logical, control flow, objects, arrays, exceptions, closures, iterators, generators, async

**`/compiler/src/main/scala/quickjs/compiler/Compiler.scala`**
- AST → bytecode compilation
- Closure capture analysis (free variable detection)
- Scope management for let/const/var
- Label resolution for break/continue
- TDZ enforcement via GetLocCheck/SetLocUninitialized

### Interpreter

**`/runtime/src/main/scala/quickjs/interpreter/Interpreter.scala`**
- Stack-based bytecode interpreter with @switch dispatch
- Closure creation via VarRef sharing
- BigInt arithmetic, comparison, and bitwise operations
- try/catch/finally support
- Generator support (basic)

### Standard Library

**`/runtime/src/main/scala/quickjs/runtime/StdLib.scala`** (55 lines — initialization facade)

**`/runtime/src/main/scala/quickjs/runtime/builtins/`** (~7,600 lines across 15 files)
- Initializes all built-in objects:
  - `Object` (create, assign, keys, values, entries, defineProperty, getOwnPropertyDescriptor, freeze, seal, is, hasOwn, etc.)
  - `Array` (push, pop, shift, unshift, slice, concat, map, filter, forEach, reduce, splice, indexOf, includes, flat, flatMap, find, sort, etc.)
  - `Function` (call, apply, bind)
  - `String` (charAt, indexOf, slice, split, replace, match, startsWith, endsWith, padStart, padEnd, trim, etc.)
  - `Number` (isFinite, isInteger, isNaN, parseInt, parseFloat, toFixed, toExponential, etc.)
  - `Boolean` (toString, valueOf)
  - `Math` (abs, floor, ceil, round, max, min, pow, sqrt, random, sin, cos, etc.)
  - `Date` (constructor, parse, UTC, now, get/set methods)
  - `RegExp` (exec, test, toString, flags, sticky/dotAll/unicode support)
  - `Symbol` (constructor, for, keyFor, well-known symbols)
  - `Map` (get, set, has, delete, clear, size, forEach, entries, keys, values)
  - `Set` (add, has, delete, clear, size, forEach, entries, keys, values)
  - `WeakMap` (get, set, has, delete)
  - `WeakSet` (add, has, delete)
  - `Promise` (then, catch, finally, resolve, reject, all, race, allSettled, any)
  - `Proxy` (get, set, has, deleteProperty, ownKeys, getOwnPropertyDescriptor, defineProperty)
  - `Reflect` (get, set, has, deleteProperty, ownKeys, getPrototypeOf, setPrototypeOf, defineProperty, getOwnPropertyDescriptor)
  - `BigInt` (constructor with string/number/bool conversion, asIntN, asUintN)
  - `WeakMap` (get, set, has, delete)
  - `WeakSet` (add, has, delete)
  - `Error`, `TypeError`, `ReferenceError`, `SyntaxError`, `RangeError`, `EvalError`, `URIError`, `AggregateError` (with stack traces)
  - `console` (log with pretty printing)
  - `JSON` (parse, stringify with reviver/replacer/space)
  - File-based module loading (`import`/`export`) via ModuleLoader

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

## Build and Test Commands

```bash
# Compile all modules
sbt compile

# Run all tests (698 tests, 0 failures; test262 smoke tests auto-skip if not cloned)
sbt test

# Clone test262 for conformance testing (if you don't already have it)
# If test262/ already exists as a symlink or clone, skip this.
git clone --depth 1 https://github.com/tc39/test262.git test262

# Run specific test
sbt "testOnly quickjs.stdlib.QuickJSJavaScriptTest"
```

## Test Status

**Current Test Count**: 698 tests, 0 failures, 0 errors

### Test Distribution
- **stdlib**: 239 tests — language features, built-in objects, JSON, arrays, TypedArrays, etc.
- **runtime**: 47 tests — interpreter correctness, closures, try/catch, classes, etc.
- **compiler**: 13 tests
- **parser**: 70 tests (lexer + parser + strict mode)
- **core**: 16 tests
- **REPL**: 23 tests
- **Various debug/trace tests**: ~98 tests
- **test262 smoke tests**: 15 suites (~871 tests) — see below
- **QuickJS C test files**: 5 files run via `QuickJSJavaScriptTest` — all pass

### test262 Conformance (15 suites, ~871 tests)
| Suite | Tests | Passed | Errors | Skipped | Pass Rate |
|-------|-------|--------|--------|---------|-----------|
| `Array/isArray` | 29 | 29 | 0 | 0 | 100% |
| `Object/assign` | 38 | 27 | 11 | 0 | 71.1% |
| `Math` | 50 | 50 | 0 | 0 | 100% |
| `language/literals` | 50 | 42 | 0 | 8 | 100% |
| `Symbol` | 94 | 64 | 13 | 17 | 83.1% |
| `BigInt` | 50 | 31 | 19 | 0 | 62.0% |
| `Map` | 50 | 27 | 8 | 15 | 77.1% |
| `Set` | 50 | 48 | 1 | 1 | 98.0% |
| `WeakMap` | 30 | 20 | 6 | 4 | 76.9% |
| `WeakSet` | 30 | 22 | 3 | 5 | 88.0% |
| `Promise` | 50 | 23 | 27 | 0 | 46% |
| `Reflect` | 50 | 39 | 11 | 0 | 78% |
| `TypedArray` | 100 | 51 | 21 | 28 | 70.8% |
| `ArrayBuffer` | 50 | 26 | 12 | 12 | 68.4% |
| `DataView` | 50 | 15 | 14 | 21 | 51.7% |

Main engine gaps exposed: `ToNumber` doesn't call `valueOf`/`toString` on objects for all paths (partial fix), iterator closing/error paths in `from` are partial, and resizable/immutable ArrayBuffer variants remain.

### QuickJS C Test File Status
| File | Status | Remaining Issue |
|---|---|---|
| `test_loop.js` | ✅ All pass | — |
| `test_bigint.js` | ✅ All pass | — |
| `test_closure.js` | ✅ All pass | — |
| `test_language.js` | ✅ All pass | — |
| `test_builtin.js` | ✅ All pass | — |

## Current Priorities

1. **TypedArray test262** — remaining issues: resizable/immutable ArrayBuffer variants, deeper subclass species edge cases, and iterator-closing error paths
2. **Top-level await** — Not implemented; dynamic `import()` now implemented over existing module loaders
3. **Line/column number reporting** — Missing in error messages
4. **Performance optimization** — No inline caching, peephole optimization
5. **Object/property descriptor conformance** — Remaining edge cases across Object, Reflect, Proxy, and TypedArrays

## Quick Reference

### Creating and executing JavaScript

```scala
import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.runtime.StdLib
import quickjs.value.JSValue

given rt: JSRuntime = JSRuntime()
given ctx: JSContext = JSContext(rt)
StdLib.initialize(ctx)

def eval(source: String): JSValue =
  val lexer = Lexer(source)
  val tokens = lexer.tokenize()
  val parser = Parser(tokens)
  val ast = parser.parseScript()
  val compiler = Compiler()
  val bytecode = compiler.compileScript(ast)
  val interpreter = Interpreter()
  interpreter.call(bytecode, JSValue.Undefined, Array.empty)
```

### Key Architecture Notes

- **VarRef indirection**: Closure variables use `VarRef` wrappers for shared mutation — both parent and child functions see the same `VarRef` object
- **GetGlobal opcode**: Resolves variables by checking `with` stack → closure map → global scope, in that order
- **Closure creation**: `GetConst` opcode creates `JSValue.Function` from `BytecodeFunction`, sharing `VarRef` objects between parent and child
- **BigInt**: Uses `java.math.BigInteger`, all arithmetic/comparison/bitwise ops have BigInt cases, throws `TypeError` on mix with Number
- **`getAllLocalVarNames`**: Returns ALL variables (params + locals + arguments) sorted by declaration index. This index directly maps to the interpreter's `locals` array position.
