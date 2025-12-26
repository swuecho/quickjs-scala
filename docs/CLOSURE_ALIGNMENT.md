# Closure Implementation Alignment: Scala vs QuickJS C

## Overview

This document shows how the Scala implementation of closures aligns with the QuickJS C reference implementation.

## Core Concepts

### QuickJS C Implementation

```c
typedef struct JSVarRef {
    JSValue *pvalue;  // Pointer to the actual value
    // ... other fields
} JSVarRef;
```

**Key insight:** `JSVarRef` uses pointer indirection (`pvalue`) to enable shared mutable storage between closures.

### Scala Implementation

```scala
final class VarRef(var value: JSValue):
  def get: JSValue = value
  def set(v: JSValue): Unit = value = v
```

**Key insight:** `VarRef` is a mutable wrapper class that provides the same indirection as C pointers.

## Alignment Comparison

### 1. Variable Storage

| Aspect | QuickJS C | Scala |
|--------|-----------|-------|
| Stack frame layout | Separate `arg_buf` and `var_buf` arrays | Unified `locals: Array[VarRef]` |
| Local variable access | `sf->var_buf[var_idx]` | `locals(paramIndex + localVarIndex)` |
| Indirection | Pointer `JSValue *pvalue` | Reference wrapper `VarRef` |

**✅ Aligned**: Both use indirection to enable shared mutable storage.

---

### 2. Creating VarRefs for Locals

#### QuickJS C (quickjs.c:16612-16672)

```c
static JSVarRef *get_var_ref(JSContext *ctx, JSStackFrame *sf, int var_idx, BOOL is_arg)
{
    JSValue *pvalue;

    if (is_arg) {
        pvalue = &sf->arg_buf[var_idx];  // Point to argument
    } else {
        pvalue = &sf->var_buf[var_idx];  // Point to local variable
    }

    var_ref = sf->var_refs[var_ref_idx];
    if (var_ref) {
        // Reuse existing VarRef (reference sharing!)
        var_ref->header.ref_count++;
        return var_ref;
    }

    // Create new VarRef
    var_ref = js_malloc(ctx, sizeof(JSVarRef));
    var_ref->pvalue = pvalue;  // Point to stack location!
    return var_ref;
}
```

#### Scala (Interpreter.scala:999-1015)

```scala
// In GetConst - capturing local variable from parent function
val localVarIndex = function.localVarNames.indexOf(varName)
if localVarIndex >= 0 then
  val actualIndex = function.paramNames.length + localVarIndex
  if actualIndex < locals.length then
    // Share the VarRef for this local variable!
    newClosure(varName) = locals(actualIndex)  // Reference sharing!
```

**✅ Aligned**: Both directly share the VarRef/pointer to the variable's storage location.

---

### 3. Reading Closure Variables

#### QuickJS C (quickjs.c:18067-18084)

```c
CASE(OP_get_var):
{
    int idx = get_u16(pc);
    JSValue val = *var_refs[idx]->pvalue;  // Dereference pointer!
    sp[0] = JS_DupValue(ctx, val);
    sp++;
}
```

#### Scala (Interpreter.scala:135-142)

```scala
case Opcode.GetLoc =>
  val index = readInt32(bytecode, pc + 1)
  // Unwrap the VarRef to get the actual value
  stack(stackTop) = locals(index).get  // Dereference VarRef!
  stackTop += 1
```

**✅ Aligned**: Both dereference the indirection (`*pvalue` vs `.get`).

---

### 4. Writing Closure Variables

#### QuickJS C (quickjs.c:18219-18226)

```c
CASE(OP_put_var_ref0):
    set_value(ctx, var_refs[0]->pvalue, *--sp);  // Write through pointer!
    BREAK;
```

#### Scala (Interpreter.scala:154-159)

```scala
case Opcode.PutLoc =>
  val index = readInt32(bytecode, pc + 1)
  stackTop -= 1
  // Update the VarRef with the new value
  locals(index).set(stack(stackTop))  // Write through VarRef!
```

**✅ Aligned**: Both write through the indirection (`*pvalue = ...` vs `.set(...)`).

---

### 5. Closure Variable Capture Types

#### QuickJS C Closure Types (quickjs.c:16892-16946)

```c
enum {
    JS_CLOSURE_LOCAL,      // Local variable in current function
    JS_CLOSURE_ARG,        // Function argument
    JS_CLOSURE_REF,        // From parent closure (shared)
    JS_CLOSURE_GLOBAL,     // Global variable
    JS_CLOSURE_GLOBAL_REF  // Global with lazy binding
};

switch(cv->closure_type) {
    case JS_CLOSURE_LOCAL:
        var_ref = get_var_ref(ctx, sf, cv->var_idx, FALSE);
        break;
    case JS_CLOSURE_ARG:
        var_ref = get_var_ref(ctx, sf, cv->var_idx, TRUE);
        break;
    case JS_CLOSURE_REF:
        var_ref = cur_var_refs[cv->var_idx];  // Share parent's VarRef!
        var_ref->header.ref_count++;
        break;
    case JS_CLOSURE_GLOBAL:
        var_ref = js_closure_global_var(ctx, cv);
        break;
}
```

#### Scala (Interpreter.scala:995-1027)

```scala
for varName <- bcFunc.freeVars do
  val paramIndex = function.paramNames.indexOf(varName)

  if paramIndex >= 0 then
    // JS_CLOSURE_ARG: Parameter in parent function
    newClosure(varName) = locals(paramIndex)  // Share VarRef
  else
    val localVarIndex = function.localVarNames.indexOf(varName)
    if localVarIndex >= 0 then
      // JS_CLOSURE_LOCAL: Local variable in parent function
      newClosure(varName) = locals(actualIndex)  // Share VarRef
    else
      val fromClosure = closure.get(varName)
      if fromClosure.isDefined then
        // JS_CLOSURE_REF: From parent's closure (nested closures)
        newClosure(varName) = fromClosure.get  // Share VarRef
      else
        // JS_CLOSURE_GLOBAL: Use GlobalRef for lazy lookup
        newClosure(varName) = new JSValue.VarRef(JSValue.GlobalRef(varName))
```

**✅ Aligned**: Both handle the same cases (args, locals, parent closure, globals).

---

### 6. Compiler Fix: Assignment to Closure Variables

#### QuickJS C Approach

QuickJS doesn't have this issue because:
1. It has separate `OP_get_var` (for closure variables) vs `OP_get_loc` (for locals)
2. The compiler generates the correct opcode based on variable scope

#### Scala Bug and Fix

**Bug**: Compiler was generating `PutLoc` for closure variables.

```scala
// WRONG: Using lookup() searches parent scopes
currentScope.lookup(name) match
  case Some(index) =>
    instructions += Instruction.putLoc(index)  // Wrong index!
```

**Fix**: Use `isLocal()` to check immediate scope only.

```scala
// CORRECT: Only use PutLoc for immediate scope
if currentScope.isLocal(name) then
  val index = currentScope.lookup(name).get
  instructions += Instruction.putLoc(index)
else
  // Variable in parent scope - use PutGlobal (checks closure at runtime)
  instructions += Instruction.putGlobal(name)
```

**✅ Aligned**: Now generates correct bytecode based on scope, similar to QuickJS's separate opcodes.

---

## Key Differences

### 1. Memory Management

| Aspect | QuickJS C | Scala |
|--------|-----------|-------|
| Allocation | Manual `js_malloc` / `free_var_ref` | JVM GC |
| Reference counting | Explicit `header.ref_count` | JVM references |
| Detached VarRefs | Complex logic for async | Not needed (JVM handles) |

### 2. Stack Frame Layout

| Aspect | QuickJS C | Scala |
|--------|-----------|-------|
| Arguments | Separate `arg_buf` array | Part of unified `locals` array |
| Locals | Separate `var_buf` array | Part of unified `locals` array |
| VarRefs | Separate `var_refs` array | Embedded in `locals` as `VarRef` |

**✅ Both correct**: The layout difference is implementation detail, the semantic behavior is identical.

### 3. Opcode Design

| Aspect | QuickJS C | Scala |
|--------|-----------|-------|
| Closure variables | `OP_get_var` / `OP_put_var` | `GetGlobal` / `PutGlobal` (checks closure) |
| Local variables | `OP_get_loc` / `OP_put_loc` | `GetLoc` / `PutLoc` |
| VarRef fast-path | `OP_get_var_ref0`..`3` | Not yet optimized |

**⚠️ Optimization opportunity**: Scala could add fast-path opcodes for first N closure variables.

---

## Test Results

### Before Fix
- Total: 51 tests
- Passed: 49 (96.1%)
- Failed: 2 closure tests

### After Fix
- Total: 51 tests
- Passed: 51 (100%)
- Failed: 0

**All closure tests now passing:**
```
PASSED: simple add closure => 8
PASSED: counter first call => 1
PASSED: counter second call => 2
```

---

## Conclusion

The Scala implementation is **correctly aligned** with the QuickJS C reference implementation:

✅ **Core mechanism**: Both use pointer/VarRef indirection for shared mutable storage
✅ **Capture semantics**: Both share VarRefs from parent scope
✅ **Read/Write operations**: Both dereference through indirection
✅ **Closure types**: Both handle args, locals, parent closures, and globals
✅ **Compiler fix**: Now generates correct bytecode for closure variable assignment

The key insight from QuickJS is that **`JSVarRef->pvalue` points directly to the variable's storage location**. Our Scala `VarRef` class provides the same indirection, enabling closures to correctly share and mutate variables across function calls.
