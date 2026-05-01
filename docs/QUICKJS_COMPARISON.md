# QuickJS-Scala vs QuickJS C - Feature Parity

## Overview

This document compares QuickJS-Scala with the original QuickJS C implementation, tracking feature parity and implementation gaps.

**Last Updated**: 2026-05-01

## Feature Parity Summary

| Category | QuickJS C | QuickJS-Scala | Status |
|----------|-----------|---------------|--------|
| Core Language | 100% | ~90% | Good |
| Classes | 100% | ~85% | Good (incl. private fields/methods) |
| Async/Await | 100% | ~85% | **Implemented** |
| Generators | 100% | ~85% | **Implemented** |
| Promises | 100% | ~90% | **Implemented** |
| Symbol | 100% | ~80% | Good |
| Map/Set | 100% | ~90% | **Implemented** |
| WeakMap/WeakSet | 100% | ~90% | **Implemented** |
| Proxy/Reflect | 100% | ~85% | Good |
| Modules | 100% | ~60% | Partial (static only) |
| TypedArrays | 100% | 0% | **Not Started** |
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
| Logical assignment (`&&=`, `\|\|=`, `??=`) | ❌ | Not implemented |

### Destructuring

| Feature | Status | Notes |
|---------|--------|-------|
| Array destructuring | ✅ | |
| Object destructuring | ✅ | |
| Default values | ✅ | |
| Rest elements | ✅ | |
| Nested destructuring | ✅ | |
| Function parameter destructuring | ✅ | |

### Template Literals

| Feature | Status | Notes |
|---------|--------|-------|
| Basic template strings | ✅ | |
| Expression interpolation (`${}`) | ✅ | |
| Multi-line strings | ✅ | |
| Tagged templates | ❌ | Not implemented |

### Modules

| Feature | Status | Notes |
|---------|--------|-------|
| `import` declarations | ✅ | Parsed and executed |
| `export` declarations | ✅ | Named and default |
| `export * from` | ✅ | Re-exports |
| File-based module loading | ✅ | |
| Dynamic `import()` | ❌ | Not implemented |
| `import.meta` | ❌ | Not implemented |
| Top-level await | ❌ | Not implemented |

---

## Major Missing Features

### 1. TypedArrays & Binary Data (High Priority)

The largest remaining feature gap:
```javascript
// Not yet supported in QuickJS-Scala
const buffer = new ArrayBuffer(16);
const view = new DataView(buffer);
const arr = new Uint8Array(buffer);
```

**Missing:** `ArrayBuffer`, `SharedArrayBuffer`, `DataView`, all TypedArray variants
(`Int8Array`, `Uint8Array`, `Uint8ClampedArray`, `Int16Array`, `Uint16Array`,
`Int32Array`, `Uint32Array`, `BigInt64Array`, `BigUint64Array`,
`Float32Array`, `Float64Array`, `Float16Array`), `Atomics`

### 2. Logical Assignment Operators (Medium Priority)

Parsed in the AST but not yet compiled to bytecode:
```javascript
x &&= y;  // x && (x = y)
x ||= y;  // x || (x = y)
x ??= y;  // x ?? (x = y)
```

### 3. Tagged Template Literals (Medium Priority)

```javascript
const result = myTag`hello ${name}`;
```

### 4. Dynamic import() (Medium Priority)

```javascript
const module = await import('./module.js');
```
Requires async module loading infrastructure.

### 5. Missing Error Types (Low Priority)

`AggregateError`, `EvalError`, `URIError` — the main 5 error types exist, but these 3 are missing.

### 6. Memory Management Features (Low Priority)

```javascript
const ref = new WeakRef(obj);
const registry = new FinalizationRegistry(callback);
```

### 7. Edge Case Bugs

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
| **Object** | `keys`, `values`, `entries`, `assign`, `create`, `defineProperty`, `defineProperties`, `getPrototypeOf`, `setPrototypeOf`, `getOwnPropertyDescriptor`, `getOwnPropertyDescriptors`, `getOwnPropertyNames`, `fromEntries`, `is`, `hasOwn`, `freeze`, `seal`, `isFrozen`, `isSealed`, `isExtensible`, `preventExtensions` |
| **Array** | `push`, `pop`, `shift`, `unshift`, `slice`, `splice`, `concat`, `map`, `filter`, `forEach`, `reduce`, `reduceRight`, `includes`, `indexOf`, `lastIndexOf`, `every`, `some`, `find`, `findIndex`, `reverse`, `fill`, `at`, `copyWithin`, `sort`, `join`, `flat`, `flatMap`, `from`, `of`, `isArray` |
| **String** | `charAt`, `charCodeAt`, `indexOf`, `lastIndexOf`, `slice`, `substring`, `toLowerCase`, `toUpperCase`, `trim`, `trimStart`, `trimEnd`, `split`, `replace`, `replaceAll`, `includes`, `match`, `matchAll`, `search`, `padStart`, `padEnd`, `repeat`, `startsWith`, `endsWith`, `at`, `normalize` |
| **Number** | `isNaN`, `isFinite`, `isInteger`, `isSafeInteger`, `parseFloat`, `parseInt`, `toFixed`, `toExponential`, `toPrecision`, constants |
| **Math** | All standard methods and constants |
| **Date** | Full date manipulation and formatting |
| **RegExp** | Full regex support with all flags |
| **JSON** | `parse`, `stringify` with reviver/replacer/space |
| **Error** | `Error`, `TypeError`, `ReferenceError`, `SyntaxError`, `RangeError` with stack traces |
| **console** | `log`, `error`, `warn`, `info`, `debug` |
| **Promise** | Constructor, `then`, `catch`, `finally`, `resolve`, `reject`, `all`, `race`, `allSettled`, `any` |
| **Map** | Constructor, `get`, `set`, `has`, `delete`, `clear`, `size`, `forEach`, `keys`, `values`, `entries` |
| **Set** | Constructor, `add`, `has`, `delete`, `clear`, `size`, `forEach`, `keys`, `values`, `entries` |
| **WeakMap** | Constructor, `get`, `set`, `has`, `delete` |
| **WeakSet** | Constructor, `add`, `has`, `delete` |
| **Symbol** | Constructor, `for`, `keyFor`, well-known symbols (`iterator`, `asyncIterator`, `toStringTag`, `hasInstance`, `species`) |
| **Reflect** | `apply`, `construct`, `defineProperty`, `deleteProperty`, `get`, `set`, `has`, `ownKeys`, `getPrototypeOf`, `setPrototypeOf`, `getOwnPropertyDescriptor`, `isExtensible`, `preventExtensions` |
| **BigInt** | Constructor, `asIntN`, `asUintN`, full arithmetic/comparison/bitwise |
| **Proxy** | Constructor with `get`, `set`, `has`, `deleteProperty`, `ownKeys`, `getOwnPropertyDescriptor`, `defineProperty` traps |
| **Function** | `call`, `apply`, `bind` |

### Not Implemented

| Object | Priority |
|--------|----------|
| **ArrayBuffer** | High |
| **DataView** | High |
| **TypedArrays** (12 variants) | High |
| **SharedArrayBuffer** | Low |
| **Atomics** | Low |
| **WeakRef** | Low |
| **FinalizationRegistry** | Low |
| **AggregateError, EvalError, URIError** | Low |

---

## Implementation Priority Roadmap

### ✅ Phase 1-3: Foundation, Core Language, ES6+ — COMPLETED
All core language features, classes (incl. private fields/methods), arrow functions, destructuring,
template literals, optional chaining, nullish coalescing, Map, Set, WeakMap, WeakSet, Symbol,
Promise, async/await, generators, Proxy, Reflect, BigInt, modules (static), RegExp, JSON.

### 🔜 Phase 4: Binary Data & Completeness
1. **ArrayBuffer** - Raw binary buffer
2. **TypedArrays** - All 12 typed array views
3. **DataView** - Low-level byte access
4. **Logical assignment** (`&&=`, `||=`, `??=`)
5. **Tagged templates**
6. **Missing error types** (AggregateError, EvalError, URIError)

### Future: Performance & Polish
7. **Dynamic import()** + `import.meta`
8. **Top-level await**
9. **WeakRef** / **FinalizationRegistry**
10. **Performance optimization** (inline caching, peephole optimizer)
11. **Test262 integration**

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
| `test_closure.js` | ⚠️ Failing | Arrow function `this`/`new.target`/`super` through eval |
| `test_loop.js` | ✅ Pass | Loop control flow |
| `test_language.js` | ⚠️ Failing | `test_argument_scope()` strict mode |
| `test_builtin.js` | ⚠️ Failing | `Object.isExtensible`/`preventExtensions` edge case |
| `test_bigint.js` | ✅ Pass | BigInt operations |

---

## Conclusion

QuickJS-Scala has achieved broad coverage of JavaScript features (~85%) and is suitable for
many application use cases. All major ES6+ features (classes, promises, async/await,
generators, Map/Set/WeakMap/WeakSet, Symbol, Proxy, Reflect, BigInt) are implemented and
tested. The main gaps are:

- **TypedArrays & binary data** — largest missing feature block
- **Dynamic import / tagged templates / logical assignment** — parsed but not compiled
- **3 QuickJS C test edge cases** — strict mode arg scope, arrow+eval bindings, `Object.isExtensible`
