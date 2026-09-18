# QuickJS Closure Implementation Analysis

## Overview
This document analyzes how the QuickJS C implementation handles closures and variable capture, based on the source code in the upstream QuickJS repository.

## Key Data Structures

### 1. JSVarRef - The Magic Indirect Reference
```c
typedef struct JSVarRef {
    JSGCObjectHeader header;  // GC header
    JSValue *pvalue;          // Pointer to the actual value
    union {
        JSValue value;        // Used when is_detached = TRUE
        struct {
            uint16_t var_ref_idx;     // Index in stack frame's var_refs[]
            JSStackFrame *stack_frame; // Parent stack frame
        };
    };
} JSVarRef;
```

**Key insight**: `JSVarRef` provides **indirection** - closures capture a pointer to the variable's storage, not the value itself. This allows:
- Mutations to be visible across all closures
- Proper sharing of variables between nested functions

### 2. Closure Variable Types
```c
typedef enum {
    JS_CLOSURE_LOCAL,      // Capture local var from parent function
    JS_CLOSURE_ARG,        // Capture argument from parent function
    JS_CLOSURE_REF,        // Capture from parent's closure (nested closures)
    JS_CLOSURE_GLOBAL_REF, // Capture global variable reference
    JS_CLOSURE_GLOBAL_DECL, // Global declaration (eval only)
    JS_CLOSURE_GLOBAL,     // Global variable (eval only)
    JS_CLOSURE_MODULE_DECL, // Module declaration
    JS_CLOSURE_MODULE_IMPORT, // Module import
} JSClosureTypeEnum;
```

### 3. JSClosureVar - Closure Variable Descriptor
```c
typedef struct JSClosureVar {
    JSClosureTypeEnum closure_type : 3;
    uint8_t is_lexical : 1;
    uint8_t is_const : 1;
    uint8_t var_kind : 4;
    uint16_t var_idx;  // Index in parent function's locals/args/closure
    JSAtom var_name;
} JSClosureVar;
```

## Closure Creation Process

### Step 1: Bytecode Compilation (Parser/Compiler)
When compiling a function that references variables from outer scopes:
1. **Analyze free variables** - Find variables used but not declared in the function
2. **Create `JSClosureVar[]` array** - Each entry describes how to capture one variable
3. **Store in `JSFunctionBytecode.closure_var[]`**

### Step 2: Function Creation (Runtime)
When `OP_fclosure` is executed:
```c
CASE(OP_fclosure):
    JSValue bfunc = JS_DupValue(ctx, b->cpool[get_u32(pc)]);
    pc += 4;
    *sp++ = js_closure(ctx, bfunc, var_refs, sf, FALSE);
    BREAK;
```

The `js_closure()` function:
1. Creates a function object
2. Calls `js_closure2()` to capture variables
3. For each closure variable:
   ```c
   for(i = 0; i < b->closure_var_count; i++) {
       JSClosureVar *cv = &b->closure_var[i];
       JSVarRef *var_ref;

       switch(cv->closure_type) {
       case JS_CLOSURE_LOCAL:
           // Get reference to local variable from parent stack frame
           var_ref = get_var_ref(ctx, sf, cv->var_idx, FALSE);
           break;
       case JS_CLOSURE_ARG:
           // Get reference to argument from parent stack frame
           var_ref = get_var_ref(ctx, sf, cv->var_idx, TRUE);
           break;
       case JS_CLOSURE_REF:
           // Reuse var_ref from parent closure
           var_ref = cur_var_refs[cv->var_idx];
           var_ref->header.ref_count++;
           break;
       // ... other cases
       }
       var_refs[i] = var_ref;
   }
   ```

### Step 3: Variable Access
When accessing a closure variable:
1. `get_var_ref` opcode gets the `JSVarRef*` from the function's `var_refs[]` array
2. Dereferences `pvalue` to get the actual `JSValue`
3. Reads/writes through this pointer

## Stack Frame Structure

```c
typedef struct JSStackFrame {
    JSValue *buf_ptr;           // Stack pointer
    JSValue *cur_sp;            // Current stack pointer
    JSValue *arg_buf;           // Arguments array
    int arg_count;
    JSVarRef **var_refs;        // Array of var_ref pointers for this frame
    // ... other fields
} JSStackFrame;
```

**Key point**: Each stack frame has a `var_refs` array containing pointers to all `JSVarRef` objects used by that function.

## Closure Lifecycle

### Creation
1. Parse/compile: Identify free variables, create `JSClosureVar[]` descriptors
2. Function creation: Allocate `JSVarRef*[]` array, fill with references
3. Runtime: Each function call creates a new stack frame with its own `var_refs`

### Access
1. `get_var_ref <idx>`: Load `var_refs[idx]`, push onto stack
2. `get_var`: Dereference the var_ref to get actual value
3. `put_var`: Write through the var_ref pointer

### GC
- `JSVarRef` is a GC object with reference counting
- When a closure is created, it increments var_ref refcounts
- When closure is GC'd, decrements refcounts
- When refcount reaches 0, var_ref can be freed

## Comparison with Scala Implementation

### What We Have (Current)
```scala
case class BytecodeFunction(
  name: String,
  bytecode: Array[Byte],
  constants: Array[Any],
  stackSize: Int,
  freeVars: Array[String],  // ✅ We track variable names
  paramNames: Array[String],
  localVarNames: Array[String]
)

// Function value has closure
case class Function(
  bytecode: BytecodeFunction,
  closure: mutable.Map[String, JSValue.VarRef]  // ✅ We have closure map
) extends JSValue
```

### What's Missing
1. **No capture at function creation**: When compiling a `FunctionExpression`, we don't capture variables from the parent scope
2. **No `fclosure` opcode**: We use `getConst` but don't create closure with captured variables
3. **No var_ref opcodes**: We use `getGlobal` for everything, no `get_var_ref`
4. **freeVars not populated**: The array exists but is always empty

## Implementation Plan

### Phase 1: Compiler Changes
1. **Populate `freeVars`** in `compileFunctionBody()`:
   ```scala
   val freeVars = findFreeVariablesForClosure(body).toArray
   ```

2. **Add `fclosure` opcode**: Instead of `getConst`, emit:
   ```scala
   instructions += Instruction.pushConst(constIndex)  // bytecode
   instructions += Instruction.fclosure(freeVars.length)
   ```

### Phase 2: Interpreter Changes
1. **Add `fclosure` handler**:
   ```scala
   case Opcode.FClosure =>
     val bcFunc = stack(stackTop - 1) match
       case f: JSValue.Function => f.bytecode
     // Capture variables from current locals into closure
     val captured = mutable.Map.empty[String, JSValue.VarRef]
     for varName <- bcFunc.freeVars do
       // Find var in current scope
       captured(varName) = findVarRef(varName)
     // Create new function with captured closure
     val newFunc = JSValue.Function(bcFunc, captured)
     stack(stackTop - 1) = newFunc
     pc += 2
   ```

2. **Variable lookup**: When `getGlobal` is called, check:
   - Local variables first
   - Then closure environment
   - Then global scope

### Phase 3: Testing
- Test nested closures
- Test closure variable mutation
- Test closure return values
- Test closure with loops

## Key Differences from QuickJS C

### Simplifications in Scala
1. **No JSVarRef indirection yet**: We use `VarRef` class but don't have full pointer semantics
2. **No distinction between local/arg/ref**: All captured variables go in same closure map
3. **Simplified GC**: We rely on JVM GC instead of manual refcounting

### Advantages in Scala
1. **Type safety**: Compiler can catch many errors
2. **Pattern matching**: Easier to destructure AST
3. **JVM GC**: No manual memory management

## Conclusion

The QuickJS closure implementation is elegant and efficient:
- **Compile-time**: Analyze and mark which variables to capture
- **Runtime**: Capture by reference through `JSVarRef` indirection
- **Access**: Direct pointer dereferencing for fast reads/writes

Our Scala implementation can follow the same pattern:
1. **Identify free variables at compile time** (we already do this!)
2. **Capture them in `fclosure` opcode** (need to add)
3. **Access through closure map** (partially works)

The main fix is to **actually capture variables when creating the function**, not just pass an empty closure map.
