# QuickJS-Scala vs QuickJS C - Feature Parity

## Overview

This document compares QuickJS-Scala with the original QuickJS C implementation, tracking feature parity and implementation gaps.

**Last Updated**: January 2026

## Feature Parity Summary

| Category | QuickJS C | QuickJS-Scala | Status |
|----------|-----------|---------------|--------|
| Core Language | 100% | ~85% | Good |
| Classes | 100% | ~80% | Good |
| Async/Await | 100% | 0% | **Not Started** |
| Generators | 100% | 0% | **Not Started** |
| Promises | 100% | 0% | **Not Started** |
| Symbol | 100% | ~20% | Partial |
| Map/Set | 100% | ~90% | **Implemented** |
| WeakMap/WeakSet | 100% | 0% | **Not Started** |
| Proxy/Reflect | 100% | ~60% | Partial |
| Modules | 100% | ~50% | Partial |
| TypedArrays | 100% | 0% | **Not Started** |
| BigInt | 100% | ~30% | Partial |

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
| `for-of` | ✅ | Arrays and strings |
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
| Rest parameters | ✅ | In destructuring |
| Closures | ✅ | Full variable capture |
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
| Private fields (`#field`) | ❌ | Not implemented |
| Private methods | ❌ | Not implemented |

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
| `import` declarations | ✅ | Parsed, runtime partial |
| `export` declarations | ✅ | Named and default |
| `export * from` | ✅ | Re-exports |
| File-based module loading | ✅ | |
| Dynamic `import()` | ❌ | Not implemented |
| `import.meta` | ❌ | Not implemented |
| Top-level await | ❌ | Not implemented |

---

## Major Missing Features

### 1. Async Programming (High Priority)

QuickJS C has full async support:
```javascript
// Not yet supported in QuickJS-Scala
async function fetchData() {
    const result = await fetch(url);
    return result.json();
}
```

**Missing components:**
- `Promise` object and methods (`then`, `catch`, `finally`, `all`, `race`, `allSettled`, `any`)
- `async`/`await` syntax
- Microtask queue
- `for-await-of` loops

### 2. Generators & Iterators (High Priority)

QuickJS C has full generator support:
```javascript
// Not yet supported in QuickJS-Scala
function* range(start, end) {
    for (let i = start; i < end; i++) {
        yield i;
    }
}
```

**Missing components:**
- `function*` generators
- `yield` / `yield*` expressions
- Iterator protocol (`Symbol.iterator`)
- `Generator` object
- Async generators (`async function*`)

### 3. Collections

```javascript
// Map and Set are now supported!
const map = new Map([['a', 1], ['b', 2]]);
const set = new Set([1, 2, 3]);

// WeakMap/WeakSet not yet supported
const weakMap = new WeakMap();
const weakSet = new WeakSet();
```

**Implemented:**
- `Map` - `new Map()`, `get`, `set`, `has`, `delete`, `clear`, `size`, `forEach`, `keys`, `values`, `entries`
- `Set` - `new Set()`, `add`, `has`, `delete`, `clear`, `size`, `forEach`, `keys`, `values`, `entries`

**Missing:**
- `WeakMap`
- `WeakSet`

### 4. Symbol (Medium Priority)

```javascript
// Partial support in QuickJS-Scala
const sym = Symbol('description');
const obj = { [Symbol.iterator]: function* () { yield 1; } };
```

**Missing:**
- `Symbol` primitive (full support)
- `Symbol.for()` / `Symbol.keyFor()`
- Well-known symbols:
  - `Symbol.iterator`
  - `Symbol.asyncIterator`
  - `Symbol.toStringTag`
  - `Symbol.hasInstance`
  - `Symbol.toPrimitive`
  - `Symbol.species`
  - And others...

### 5. Reflect API (Medium Priority)

```javascript
// Not yet supported in QuickJS-Scala
Reflect.get(obj, 'prop');
Reflect.set(obj, 'prop', value);
Reflect.construct(Class, args);
```

**Missing all Reflect methods:**
- `Reflect.apply()`
- `Reflect.construct()`
- `Reflect.defineProperty()`
- `Reflect.deleteProperty()`
- `Reflect.get()` / `Reflect.set()`
- `Reflect.getOwnPropertyDescriptor()`
- `Reflect.getPrototypeOf()` / `Reflect.setPrototypeOf()`
- `Reflect.has()`
- `Reflect.isExtensible()` / `Reflect.preventExtensions()`
- `Reflect.ownKeys()`

### 6. TypedArrays & Buffers (Medium Priority)

```javascript
// Not yet supported in QuickJS-Scala
const buffer = new ArrayBuffer(16);
const view = new DataView(buffer);
const arr = new Uint8Array(buffer);
```

**Missing:**
- `ArrayBuffer`
- `SharedArrayBuffer`
- `DataView`
- All TypedArray variants:
  - `Int8Array`, `Uint8Array`, `Uint8ClampedArray`
  - `Int16Array`, `Uint16Array`
  - `Int32Array`, `Uint32Array`
  - `BigInt64Array`, `BigUint64Array`
  - `Float32Array`, `Float64Array`, `Float16Array`
- `Atomics` API

### 7. Private Class Fields (Low Priority)

```javascript
// Not yet supported in QuickJS-Scala
class Counter {
    #count = 0;
    #increment() { this.#count++; }
    get value() { return this.#count; }
}
```

### 8. Memory Management Features (Low Priority)

```javascript
// Not yet supported in QuickJS-Scala
const ref = new WeakRef(obj);
const registry = new FinalizationRegistry(callback);
```

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
| **JSON** | `parse`, `stringify` with options |
| **Error** | `Error`, `TypeError`, `ReferenceError`, `SyntaxError` with stack traces |
| **console** | `log`, `error`, `warn`, `info`, `debug` |
| **Map** | `new Map()`, `get`, `set`, `has`, `delete`, `clear`, `size`, `forEach`, `keys`, `values`, `entries` |
| **Set** | `new Set()`, `add`, `has`, `delete`, `clear`, `size`, `forEach`, `keys`, `values`, `entries` |

### Partially Implemented

| Object | Status | Missing |
|--------|--------|---------|
| **Proxy** | ~60% | Some traps may be incomplete |
| **BigInt** | ~30% | Limited arithmetic operations |

### Not Implemented

| Object | Priority |
|--------|----------|
| **Promise** | High |
| **WeakMap** | Medium |
| **WeakSet** | Medium |
| **Symbol** | Medium |
| **Reflect** | Medium |
| **ArrayBuffer** | Medium |
| **DataView** | Medium |
| **TypedArrays** | Medium |
| **SharedArrayBuffer** | Low |
| **Atomics** | Low |
| **WeakRef** | Low |
| **FinalizationRegistry** | Low |
| **Intl** | Low (intentionally excluded in QuickJS C too) |

---

## Implementation Priority Roadmap

### ~~Phase 1: Foundation for Iteration~~ (Partial - Map/Set work without full Symbol support)

### ~~Phase 2: Collections~~ ✅ COMPLETED
- ✅ **Map** - Key-value collection with all core methods
- ✅ **Set** - Unique value collection with all core methods
- ⏳ **WeakMap** / **WeakSet** - Pending (requires proper GC integration)

### Phase 3: Async Foundation
7. **Promise** - Async primitive
8. **Microtask queue** - Promise resolution

### Phase 4: Generators
9. **Generator functions** - `function*` and `yield`
10. **Generator protocol** - Iterator integration

### Phase 5: Async/Await
11. **async/await** - Built on Promise + generators
12. **Async iterators** - `for-await-of`

### Phase 6: Binary Data
13. **ArrayBuffer** - Raw binary buffer
14. **TypedArrays** - Typed views
15. **DataView** - Low-level access

### Phase 7: Completeness
16. **Reflect API** - Metaprogramming
17. **Private fields** - Class encapsulation
18. **Tagged templates** - Advanced string processing

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
| `test_closure.js` | ✅ Pass | Closure semantics |
| `test_loop.js` | ✅ Pass | Loop control flow |
| `test_language.js` | ⚠️ Partial | Some failures in edge cases |
| `test_builtin.js` | ⚠️ Partial | Missing built-ins cause failures |
| `test_bigint.js` | ⚠️ Partial | Limited BigInt support |

---

## Conclusion

QuickJS-Scala has achieved good coverage of core JavaScript features (~85%) and is suitable for many use cases. The main gaps are in advanced ES6+ features:

- **Async programming** (Promise, async/await)
- **Generators and iterators**
- **Collections** (Map, Set)
- **Symbol system**

These features build on each other, so implementation should follow the priority roadmap above.
