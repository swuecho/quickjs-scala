# QuickJS-Scala vs Original QuickJS (C) Design Comparison

## Overview

This document compares the Scala implementation of QuickJS with the original C implementation by Fabrice Bellard, highlighting design differences and compatibility decisions.

## Core Architecture

### Original QuickJS (C)
- **Single file implementation**: ~60,000 lines in `quickjs.c`
- **NaN boxing**: Values stored as 64-bit unions with tag bits
- **Manual memory management**: Custom reference counting and GC
- **Stack-based bytecode interpreter**: Direct threading optimization
- **Compiler phases**: 3-phase compilation (parse → resolve scopes → emit bytecode)

### QuickJS-Scala
- **Modular multi-project**: Separate modules for core, parser, compiler, runtime
- **Tagged union types**: Sealed trait `JSValue` with case classes
- **JVM GC integration**: Leverages JVM garbage collectors (G1, ZGC, Shenandoah)
- **Stack-based bytecode interpreter**: Similar design but Scala-idiomatic
- **Simplified compiler**: Single-pass compilation with scope tracking

## Key Design Differences

### 1. Value Representation

| Aspect | Original QuickJS | QuickJS-Scala | Notes |
|--------|------------------|---------------|-------|
| **Type System** | NaN boxing (64-bit union) | Sealed trait + case classes | Scala's type system is safer |
| **Number Storage** | Inline in union | Separate case classes (Int32, Float64) | More type-safe, less compact |
| **Object Storage** | Pointer in union | Case class wrapping JSObject | Similar semantics |
| **Memory** | Manual ref counting | JVM GC | Eliminates 2,000+ lines of GC code |

**Rationale**: JVM GC eliminates complexity while providing production-grade garbage collection.

### 2. Bytecode Opcodes

#### Increment/Decrement Operators

| Opcode | Original | QuickJS-Scala | Difference |
|--------|----------|---------------|------------|
| `inc` | Modifies value in-place, pushes 1 | Same | ✓ Compatible |
| `dec` | Modifies value in-place, pushes 1 | Same | ✓ Compatible |
| `post_inc` | Pushes 2 values (original + incremented) | Pushes 1 value (simplified) | ⚠ Different |
| `post_dec` | Pushes 2 values (original + decremented) | Pushes 1 value (simplified) | ⚠ Different |

**Original QuickJS sequence**:
```c
get_loc x      // Push x
post_inc       // Push x, x+1 (2 values on stack)
put_loc x      // Store x+1 to x
drop           // Drop original x
```

**QuickJS-Scala simplified sequence**:
```scala
getLoc x       // Push x
PreInc         // Modify to x+1, push result
dup            // Duplicate result
putLoc x       // Store x+1 to x (consumes one copy)
```

**Rationale**: For Phase 2, the simplified approach works correctly. The full post_inc semantics can be added later.

#### Missing Opcodes

The following opcodes from QuickJS are **not yet implemented** in QuickJS-Scala:

1. **Short opcodes** (optimizations):
   - `push_0`, `push_1`, `push_2`, etc.
   - `get_loc0`, `get_loc1`, `get_loc2`, `get_loc3`
   - `put_loc0`, `put_loc1`, `put_loc2`, `put_loc3`
   - Rationale: Can be added as optimization pass later

2. **Specialized increment/decrement**:
   - `inc_loc`, `dec_loc`, `add_loc`
   - Rationale: Compiler generates equivalent sequences

3. **Object/Property operations**:
   - `get_field`, `put_field`, `define_field`
   - `get_array_el`, `put_array_el`
   - `set_name`, `set_proto`
   - Rationale: Not yet implemented in Phase 2

4. **Function-related**:
   - `fclosure`, `call_constructor`, `call_method`
   - `return`, `check_ctor_return`, `init_ctor`
   - Rationale: Function call support pending

5. **Control flow**:
   - `catch`, `gosub`, `ret` (exception handling)
   - `for_in_start`, `for_of_start` (iteration)
   - Rationale: Not yet implemented

6. **Other operators**:
   - `typeof`, `delete`, `in`, `instanceof`
   - `plus` (unary plus)
   - Rationale: Can be added incrementally

#### Extra Opcodes in QuickJS-Scala

| Opcode | Purpose | Status |
|--------|---------|--------|
| `Break` | Break from loop | ✓ Implemented |
| `Continue` | Continue to next iteration | ✓ Implemented |

**Note**: Original QuickJS doesn't have dedicated break/continue opcodes - they're handled via goto.

### 3. Variable Access

| Aspect | Original QuickJS | QuickJS-Scala | Notes |
|--------|------------------|---------------|-------|
| **Local variables** | `get_loc`, `put_loc`, `set_loc` | `GetLoc`, `PutLoc` | Similar |
| **Arguments** | `get_arg`, `put_arg`, `set_arg` | `GetArg`, `PutArg` | Similar |
| **Closure variables** | `get_var_ref`, `put_var_ref` | Not yet implemented | Phase 3+ |
| **Scope resolution** | Multi-phase (enter_scope, leave_scope) | Single-pass with scope tracking | Simplified but works |

### 4. Stack Operations

| Opcode | Original | QuickJS-Scala | Match |
|--------|----------|---------------|-------|
| `drop` | 1, 1, 0 (pops 1, pushes 0) | Same | ✓ |
| `dup` | 1, 1, 2 (pops 1, pushes 2) | Same | ✓ |
| `nip` | 2, 1, 1 | Not implemented | Phase 3+ |
| `swap` | 2, 2, 2 | Not implemented | Phase 3+ |
| `rot3l`, `rot3r` | Stack rotations | Not implemented | Phase 3+ |

### 5. Compiler Design

**Original QuickJS**:
- **3-phase compilation**:
  1. Parse to AST
  2. Resolve scopes (enter_scope/leave_scope)
  3. Emit bytecode with optimizations

**QuickJS-Scala**:
- **Single-pass compilation**:
  - Parse to AST
  - Compile with inline scope tracking
  - Emit bytecode directly

**Rationale**: Simplified compiler suitable for Phase 2. Can be enhanced to multi-pass later.

## Compatibility Assessment

### ✓ What Matches Well

1. **Opcode encoding**: Similar 1-5 byte instruction encoding
2. **Stack-based execution**: Same fundamental model
3. **Local variable access**: Same get/put patterns
4. **Control flow**: if_false/if_true/goto semantics match
5. **Arithmetic operations**: Same add/sub/mul/div/mod semantics
6. **Comparison operations**: Same lt/lte/gt/gte/eq/neq semantics

### ⚠ What Differs (Acceptable for Phase 2)

1. **Post-increment semantics**: Simplified (1 value vs 2 values)
2. **Compiler phases**: Single-pass vs multi-pass
3. **Optimization passes**: Missing (can be added later)
4. **Exception handling**: Not yet implemented
5. **Object model**: Partially implemented

### ❌ What's Missing (To Be Implemented)

1. **Function calls**: Only stub implementation
2. **Object literals**: Not yet implemented
3. **Array operations**: Not yet implemented
4. **Closures**: Not yet implemented
5. **Exception handling**: try/catch/finally
6. **Iterators**: for-in, for-of loops
7. **Classes**: ES6 class syntax
8. **Modules**: import/export

## Recommendations

### Short-term (Phase 2-3)

1. **Complete post-inc/dec semantics**: Make PostInc/PostDec match QuickJS behavior
2. **Add unary plus**: Implement `plus` opcode
3. **Function calls**: Implement proper `call` and `fclosure` opcodes
4. **Object literals**: Implement `object`, `get_field`, `put_field` opcodes

### Medium-term (Phase 4-5)

1. **Add short opcodes**: Implement optimization opcodes
2. **Exception handling**: Implement catch/gosub/ret
3. **Closures**: Implement closure variable access
4. **Iterators**: Implement for-in/for-of loops

### Long-term (Phase 6+)

1. **Multi-pass compiler**: Align with QuickJS 3-phase design
2. **Optimization passes**: Peephole optimizer, inline caching
3. **Async/await**: Implement yield/await opcodes
4. **Classes**: Implement full ES6 class support

## Conclusion

The QuickJS-Scala implementation maintains **architectural compatibility** with the original QuickJS while making deliberate simplifications suitable for Phase 2:

- **Same fundamental design**: Stack-based bytecode interpreter
- **Compatible opcode semantics**: Core operations match QuickJS
- **Type-safe implementation**: Leverages Scala's type system
- **JVM GC integration**: Eliminates GC complexity

The differences are:
- **Intentional simplifications** for Phase 2 goals
- **Scala-idiomatic choices** (sealed traits vs NaN boxing)
- **Missing features** that can be added incrementally

Overall, the design is **compatible enough** that QuickJS bytecode patterns can be understood and ported, while **different enough** to leverage JVM strengths.
