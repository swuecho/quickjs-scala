# QuickJS-Scala vs QuickJS C - Feature Parity

## Overview

This document compares QuickJS-Scala with the original QuickJS C implementation, tracking feature parity and implementation gaps.

**Last Updated**: 2026-09-18

> **Note**: This document trails `AGENTS.md`, which is the living status log.
> Sections below were written in July 2026 and some entries (dynamic import,
> top-level await, direct eval) are now implemented; see `AGENTS.md` for the
> current feature matrix and test262 numbers.

## Feature Parity Summary

| Category | QuickJS C | QuickJS-Scala | Status |
|----------|-----------|---------------|--------|
| Core Language | 100% | ~90% | Good, with remaining eval/scope and syntax gaps |
| Classes | 100% | ~85% | Good (incl. private fields/methods) |
| Async/Await | 100% | ~85% | **Implemented** |
| Generators | 100% | ~85% | **Implemented** |
| Promises | 100% | ~90% | **Implemented** |
| Symbol | 100% | ~80% | Good |
| Map/Set | 100% | ~90% | **Implemented** |
| WeakMap/WeakSet | 100% | ~90% | **Implemented** |
| Proxy/Reflect | 100% | ~85% | Good, needs invariant and descriptor polish |
| Modules | 100% | ~70% | Static import/export, file loading, import.meta, dynamic import() |
| TypedArrays | 100% | ~78% | Implemented, indexed descriptor/key conformance improved |
| BigInt | 100% | ~90% | Good |

---

## Implemented Features

### Core Language Syntax

| Feature | Status | Notes |
|---------|--------|-------|
| `var`, `let`, `const` | ✅ | Full support with TDZ |
| `if`/`else` | ✅ | |
| `while`, `do-while` | ✅ | |
| `for` (C-style) | ✅ | |
| `for-in` | ✅ | |
| `for-of` | ✅ | Full iterable support |
| `switch`/`case` | ✅ | |
| `break`/`continue` | ✅ | Including labeled |
| `try`/`catch`/`finally` | ✅ | With stack traces |
| `throw` | ✅ | |
| `with` | ✅ | For compatibility |

### Functions

| Feature | Status | Notes |
|---------|--------|-------|
| Function declarations | ✅ | |
| Function expressions | ✅ | |
| Arrow functions | ✅ | Concise and block body |
| Default parameters | ✅ | |
| Rest parameters | ✅ | |
| Spread call/new arguments | ✅ | Includes `fn(...args)` and `new C(...args)` |
| Closures | ✅ | Full variable capture via VarRef |
| `this` binding | ✅ | |
| `call`/`apply`/`bind` | ✅ | |

### Classes (ES6)

| Feature | Status | Notes |
|---------|--------|-------|
| Class declarations | ✅ | |
| Class expressions | ✅ | |
| Constructor | ✅ | |
| Instance methods | ✅ | |
| Static methods | ✅ | |
| Static fields | ✅ | |
| Static initialization blocks (`static { }`) | ✅ | Own var/lexical scope, `this` is the class, runs in source order with static fields, `super.x` allowed |
| Getters/Setters | ✅ | |
| `extends` | ✅ | |
| `super()` calls | ✅ | |
| `super.method()` | ✅ | |
| Private fields (`#field`) | ✅ | |
| Private methods | ✅ | |
| Private static fields | ✅ | |

### Operators

| Feature | Status | Notes |
|---------|--------|-------|
| Arithmetic (`+`,`-`,`*`,`/`,`%`,`**`) | ✅ | |
| Comparison (`<`,`<=`,`>`,`>=`,`==`,`!=`,`===`,`!==`) | ✅ | |
| Logical (`&&`, `\|\|`, `!`) | ✅ | |
| Bitwise (`&`,`\|`,`^`,`~`,`<<`,`>>`,`>>>`) | ✅ | |
| Nullish coalescing (`??`) | ✅ | |
| Optional chaining (`?.`) | ✅ | Property, method, bracket |
| `typeof` | ✅ | |
| `instanceof` | ✅ | |
| `in` | ✅ | |
| `delete` | ✅ | |
| `void` | ✅ | |
| `new` | ✅ | |
| Compound assignment (`+=`, etc.) | ✅ | |
| Logical assignment (`&&=`, `\|\|=`, `??=`) | ✅ | Identifier and member targets, short-circuit semantics |

### Destructuring

| Feature | Status | Notes |
|---------|--------|-------|
| Array destructuring | ✅ | Uses iterable/array-like collection, including rest |
| Object destructuring | ✅ | Includes rest properties |
| Default values | ✅ | |
| Rest elements | ✅ | Rest-last syntax validation |
| Nested destructuring | ✅ | |
| Function parameter destructuring | ✅ | |

### Template Literals

| Feature | Status | Notes |
|---------|--------|-------|
| Basic template strings | ✅ | |
| Expression interpolation (`${}`) | ✅ | |
| Multi-line strings | ✅ | |
| Tagged templates | ✅ | Template object, `.raw`, substitutions, member `this`, call-site caching |

### Modules

| Feature | Status | Notes |
|---------|--------|-------|
| `import` declarations | ✅ | Parsed and executed |
| `export` declarations | ✅ | Named and default |
| `export * from` | ✅ | Re-exports |
| File-based module loading | ✅ | |
| Dynamic `import()` | ❌ | Not implemented |
| `import.meta` | ✅ | Module-only, cached null-prototype meta object |
| Top-level await | ❌ | Not implemented |

---

## Major Missing Features

### 1. Object Model & Descriptor Correctness (Highest Priority)

Many remaining test262 and QuickJS C compatibility failures depend on exact
ECMAScript object semantics rather than missing surface APIs. This should be the
first conformance focus because Object, Array, TypedArray, Proxy, Reflect,
classes, modules, and built-ins all share it.

**Remaining work:**
- Broaden descriptor conformance for accessor/data transitions across Object,
  Reflect, typed arrays, and proxies
- Complete `[[DefineOwnProperty]]` validation for data/accessor transitions,
  non-configurable properties, and typed arrays
- Enforce Proxy invariants for `getOwnPropertyDescriptor`, `defineProperty`,
  `ownKeys`, `getPrototypeOf`, and `setPrototypeOf`
- Match ECMAScript property enumeration order for integer indices, strings, and symbols
- Tighten `Object.freeze`, `Object.seal`, `preventExtensions`, and related queries
- Continue iterator consumer conformance after the initial Array iterator and
  `Array.from` iterator/constructor work

### 2. TypedArrays & Binary Data Conformance (High Priority)

TypedArrays, ArrayBuffer, and DataView are implemented, including the 12 typed
array constructors, `%TypedArray%`, iterator methods, and common prototype
methods such as `at`, `fill`, `copyWithin`, `reverse`, `includes`, `indexOf`,
`lastIndexOf`, `join`, `toString`, `toLocaleString`, `with`, `toReversed`,
`sort`, and `toSorted`, plus callback methods such as `find`, `findIndex`,
`findLast`, `findLastIndex`, `forEach`, `every`, `some`, `map`, `filter`,
`reduce`, and `reduceRight`. Fixed-length `ArrayBuffer.prototype.transfer`,
`transferToFixedLength`, `detached`, wrapper identity caching, and detached
typed-array length/byte offset reporting are implemented. DataView supports
big-endian and little-endian integer, float, BigInt, and Float16 accessors, and
throws on detached buffers for byte-length/offset and element access. Several
conformance gaps remain:
```javascript
const buffer = new ArrayBuffer(16);
const view = new DataView(buffer);
const arr = new Uint8Array(buffer);
```

**Remaining work:** resizable/immutable ArrayBuffer variants, deeper subclass
species edge cases, iterator-closing error paths, detached-buffer checks inside
all prototype algorithms, and remaining prototype method edge cases.

### 3. Direct Eval & Dynamic Scope Semantics (High Priority)

QuickJS C compatibility still depends on exact direct `eval` behavior:
- Top-level `var` declarations in non-strict direct eval now merge into the
  caller eval environment, and strict eval keeps them local
- Eval-bearing functions now capture parent locals so eval strings can see
  outer variables, matching QuickJS C's conservative eval closure behavior
- QuickJS C `test_eval2()` now passes, including `eval(...[1, 2])` through a
  non-direct eval parameter
- Non-strict direct eval now collects nested `var` declarations and function
  declarations for caller-scope merging, and QuickJS C `test_eval_closure()`
  plus `test_eval_const()` now run in the imported closure test
- Dynamic `with` scope lookup now routes through direct eval, so QuickJS C
  `test_with()` also runs in the imported closure test
- Named function expressions now keep their self-name binding private and
  read-only, including nested arrow and direct eval assignments, so QuickJS C
  `test_function_expr_name()` runs in the imported language test
- Parameter default expressions now run before body `var` declarations enter
  scope, so direct eval in the argument-scope cases from QuickJS C
  `test_argument_scope()` runs in the imported language test
- Direct eval and indirect eval are now separated: syntactic `eval(...)` uses
  caller scope, while `(1, eval)(...)` creates configurable global properties,
  so QuickJS C `test_global_var_opt()` runs in the imported language test
- Remaining direct eval gaps are deeper test262 argument-scope edge cases not
  covered by QuickJS C tests
- `this`, `super`, `new.target`, and closure variables must be visible through eval
- default parameter and argument-scope interactions need to match QuickJS/test262

### 4. Logical Assignment Operators

Implemented for identifiers and member expressions with left-reference
evaluation and short-circuit semantics matching QuickJS:
```javascript
x &&= y;  // x && (x = y)
x ||= y;  // x || (x = y)
x ??= y;  // x ?? (x = y)
```

### 5. Top-Level Await (Medium Priority)

```javascript
const module = await import('./module.js');
```
Dynamic `import()` is implemented with Promise resolution/rejection over the
existing module loaders. Top-level await still requires async module evaluation
infrastructure.

### 6. Memory Management Features (Low Priority)

```javascript
const ref = new WeakRef(obj);
const registry = new FinalizationRegistry(callback);
```

WeakRef and FinalizationRegistry constructors/prototypes are implemented with
JVM-backed weak references. Finalization callback delivery is still limited by
JVM GC/reference-queue integration and is not deterministic.

### 9. Edge Case Bugs

| Bug | Test File |
|-----|-----------|
| Arrow function `this`/`new.target`/`super` through `eval()` | `test_closure.js` |
| Strict mode argument scope isolation with `eval` in default params | `test_language.js` |
| `Object.isExtensible`/`preventExtensions` edge case | `test_builtin.js` |

---

## Built-in Objects Comparison

### Fully Implemented

| Object | Methods |
|--------|---------|
| **Object** | `keys`, `values`, `entries`, `assign`, `create`, `defineProperty`, `defineProperties`, `getPrototypeOf`, `setPrototypeOf`, `getOwnPropertyDescriptor`, `getOwnPropertyDescriptors`, `getOwnPropertyNames`, `fromEntries`, `groupBy`, `is`, `hasOwn`, `freeze`, `seal`, `isFrozen`, `isSealed`, `isExtensible`, `preventExtensions` |
| **Array** | `push`, `pop`, `shift`, `unshift`, `slice`, `splice`, `concat`, `map`, `filter`, `forEach`, `reduce`, `reduceRight`, `includes`, `indexOf`, `lastIndexOf`, `every`, `some`, `find`, `findIndex`, `reverse`, `fill`, `at`, `copyWithin`, `sort`, `join`, `flat`, `flatMap`, `from`, `of`, `isArray` |
| **String** | `charAt`, `charCodeAt`, `indexOf`, `lastIndexOf`, `slice`, `substring`, `toLowerCase`, `toUpperCase`, `trim`, `trimStart`, `trimEnd`, `split`, `replace`, `replaceAll`, `includes`, `match`, `matchAll`, `search`, `padStart`, `padEnd`, `repeat`, `startsWith`, `endsWith`, `at`, `normalize` |
| **Number** | `isNaN`, `isFinite`, `isInteger`, `isSafeInteger`, `parseFloat`, `parseInt`, `toFixed`, `toExponential`, `toPrecision`, constants |
| **Math** | All standard methods and constants |
| **Date** | Full date manipulation and formatting |
| **RegExp** | Full regex support with all flags, `RegExp.escape` |
| **JSON** | `parse`, `stringify` with reviver/replacer/space |
| **Error** | `Error`, `TypeError`, `ReferenceError`, `SyntaxError`, `RangeError`, `EvalError`, `URIError`, `AggregateError` with stack traces, `cause`, `AggregateError.errors`, `Error.isError` |
| **console** | `log`, `error`, `warn`, `info`, `debug` |
| **Promise** | Constructor, `then`, `catch`, `finally`, `resolve`, `reject`, `try`, `all`, `race`, `allSettled`, `any`, `withResolvers` |
| **Map** | Constructor, `get`, `set`, `has`, `delete`, `clear`, `size`, `forEach`, `keys`, `values`, `entries`, `getOrInsert`, `getOrInsertComputed`, `groupBy` |
| **Set** | Constructor, `add`, `has`, `delete`, `clear`, `size`, `forEach`, `keys`, `values`, `entries`, ES2025 `union`, `intersection`, `difference`, `symmetricDifference`, `isSubsetOf`, `isSupersetOf`, `isDisjointFrom` (with `GetSetRecord` set-like arguments) |
| **WeakMap** | Constructor, `get`, `set`, `has`, `delete`, `getOrInsert`, `getOrInsertComputed` |
| **WeakSet** | Constructor, `add`, `has`, `delete` |
| **Symbol** | Constructor, `for`, `keyFor`, well-known symbols (`iterator`, `asyncIterator`, `toStringTag`, `hasInstance`, `species`) |
| **Reflect** | `apply`, `construct`, `defineProperty`, `deleteProperty`, `get`, `set`, `has`, `ownKeys`, `getPrototypeOf`, `setPrototypeOf`, `getOwnPropertyDescriptor`, `isExtensible`, `preventExtensions` |
| **BigInt** | Constructor, `asIntN`, `asUintN`, full arithmetic/comparison/bitwise |
| **Proxy** | Constructor with `get`, `set`, `has`, `deleteProperty`, `ownKeys`, `getOwnPropertyDescriptor`, `defineProperty` traps |
| **Function** | `call`, `apply`, `bind` |

### Not Implemented

| Object | Priority |
|--------|----------|
| **SharedArrayBuffer** | Low |
| **Atomics** | Low |

### Implemented With Known Conformance Gaps

| Object | Remaining Work |
|--------|----------------|
| **ArrayBuffer** | Resizable/immutable variants skipped; deeper transfer/species edge cases remain |
| **DataView** | Conversion and resizable-buffer edge cases |
| **TypedArrays** (12 variants) | Deeper subclass species edge cases, iterator-closing paths, detached-buffer checks in all algorithms, and remaining prototype method edge cases |
| **WeakRef / FinalizationRegistry** | JVM GC timing means cleanup callback scheduling is not deterministic yet |

---

## Implementation Priority Roadmap

### ✅ Phase 1-3: Foundation, Core Language, ES6+ — COMPLETED
All core language features, classes (incl. private fields/methods), arrow functions, destructuring,
template literals, optional chaining, nullish coalescing, Map, Set, WeakMap, WeakSet, Symbol,
Promise, async/await, generators, Proxy, Reflect, BigInt, modules (static and dynamic import), RegExp, JSON.

### 🔜 Phase 4: Conformance & Completeness
1. **Object model correctness** - descriptors, accessors, property definition, enumeration order
2. **Proxy/Reflect invariants** - reject invalid trap results and preserve target invariants
3. **TypedArray/DataView/ArrayBuffer conformance** - finish edge cases after object model fixes
4. **Direct eval semantics** - caller scope, `this`, `super`, `new.target`, strict/non-strict behavior
5. **Top-level await** and async module evaluation

### Future: Performance & Polish
6. **SharedArrayBuffer** / **Atomics**
7. **Performance optimization** (inline caching, peephole optimizer)
8. **Expand test262 from smoke suites toward full QuickJS-style conformance tracking**

---

## Architecture Comparison

### Value Representation

| Aspect | QuickJS C | QuickJS-Scala |
|--------|-----------|---------------|
| Type System | NaN boxing (64-bit union) | Sealed trait + case classes |
| Number Storage | Inline in union | Separate `Int32`, `Float64` |
| Object Storage | Pointer in union | Case class wrapping `JSObject` |
| Memory | Manual ref counting + GC | JVM GC (G1, ZGC, Shenandoah) |

### Compiler Design

| Aspect | QuickJS C | QuickJS-Scala |
|--------|-----------|---------------|
| Phases | 3-phase (parse → resolve → emit) | Single-pass with scope tracking |
| Optimization | Peephole optimizer, inline caching | Minimal optimization |
| Bytecode | Compact encoding, short opcodes | Similar but less optimized |

### Key Design Decisions

1. **JVM GC Integration**: Eliminates ~2000 lines of manual GC code
2. **Type-safe values**: Leverages Scala's type system for safety
3. **Modular architecture**: Separate modules vs monolithic `quickjs.c`
4. **Simplified compiler**: Suitable for current feature set

---

## Test Compatibility

QuickJS-Scala runs a subset of the original QuickJS test suite:

| Test File | Status | Notes |
|-----------|--------|-------|
| `test_closure.js` | ✅ Pass | Imported QuickJS C test file |
| `test_loop.js` | ✅ Pass | Loop control flow |
| `test_language.js` | ✅ Pass | Imported QuickJS C test file |
| `test_builtin.js` | ✅ Pass | Complete upstream file runs unchanged in the default suite |
| `test_bigint.js` | ✅ Pass | BigInt operations |

---

## Conclusion

QuickJS-Scala has achieved broad coverage of JavaScript features (~85%) and is suitable for
many application use cases. All major ES6+ features (classes, promises, async/await,
generators, Map/Set/WeakMap/WeakSet, Symbol, Proxy, Reflect, BigInt) are implemented and
tested. The main gaps are:

- **TypedArray conformance edges** — resizable/immutable buffers and deeper detached-buffer cases
- **Top-level await** — not implemented
- **Deeper test262 coverage** — the imported QuickJS regression files pass,
  while broader specification coverage still exposes edge cases
