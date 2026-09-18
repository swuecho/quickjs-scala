# Labeled Statements Implementation

## Overview

This document describes the implementation of labeled statements (break/continue with labels) in QuickJS-Scala, following the architecture and patterns from the original QuickJS C implementation.

**Date**: December 2025
**Status**: ✅ Complete and tested
**Test Results**: All labeled statement tests passing

## Problem Statement

JavaScript allows labeling loops and blocks, then using `break label;` and `continue label;` to control which loop/block to exit or continue. This is useful for nested loops:

```javascript
// Without labels: break only exits innermost loop
for (var i = 0; i < 10; i++) {
  for (var j = 0; j < 10; j++) {
    if (condition) break;  // Only exits inner loop
  }
}

// With labels: break can exit outer loop
outer: for (var i = 0; i < 10; i++) {
  for (var j = 0; j < 10; j++) {
    if (condition) break outer;  // Exits outer loop
  }
}
```

## QuickJS C Reference Architecture

The QuickJS C implementation uses a label stack to track labeled loops and blocks:

```c
// Label stack entry
typedef struct JSLabelEntry {
    JSAtom label_name;       // Label name (or NULL if unlabeled)
    int is_break;            // // Can break to this label
    int is_continue;         // // Can continue to this label (loops only)
    int *patch_list;         // List of jump offsets to patch
    int8_t is_loop;          // // Is this a loop (for continue)
} JSLabelEntry;

// Function definition with label stack
typedef struct JSFunctionDef {
    JSLabelEntry *label_stack;  // Stack of labels
    int label_stack_size;       // Current stack size
    // ...
} JSFunctionDef;
```

## Scala Implementation

### 1. AST Support for Labels

**File**: `parser/src/main/scala/quickjs/ast/AST.scala`

All loop and block statements now support optional labels:

```scala
case class WhileStatement(
  test: Expression,
  body: Statement,
  label: Option[String],  // Label for this loop
  span: Span
) extends Statement

case class ForStatement(
  init: Option[ForInit],
  test: Option[Expression],
  update: Option[Expression],
  body: Statement,
  label: Option[String],  // Label for this loop
  span: Span
) extends Statement

case class DoWhileStatement(
  body: Statement,
  test: Expression,
  label: Option[String],  // Label for this loop
  span: Span
) extends Statement

case class BlockStatement(
  statements: Seq[Statement],
  label: Option[String],  // Label for this block
  span: Span
) extends Statement
```

### 2. Parser Label Detection

**File**: `parser/src/main/scala/quickjs/parser/Parser.scala`

The parser detects labels before statements:

```scala
// Parse a label (identifier followed by colon)
private def parseLabel(): Option[String] =
  peek(2) match
    case Some((IdentifierToken(name, _), ColonToken())) =>
      consume(2)  // consume identifier and colon
      Some(name)
    case _ => None

// Parse labeled statements
private def parseLabeledStatement(): Statement =
  val label = parseLabel()
  parseStatement(label)  // Pass label to statement parser
```

Labels are attached to loops and blocks:
```javascript
// Parsed as:
// ForStatement(
//   label = Some("outer"),
//   ...
// )

outer: for (var i = 0; i < 10; i++) {
  inner: for (var j = 0; j < 10; j++) {
    if (j === 5) break outer;
  }
}
```

### 3. Compiler Label Stack

**File**: `compiler/src/main/scala/quickjs/compiler/Compiler.scala`

The compiler maintains a stack of labeled loops/blocks:

```scala
// Label stack entry
case class LabelEntry(
  isLoop: Boolean,           // True if this is a loop (for continue)
  labelName: Option[String], // Label name (if any)
  exitPos: Int,              // Position to jump to on break
  continuePos: Int,          // Position to jump to on continue (loops only)
  pendingBreaks: List[Int],  // List of break jumps to patch
  pendingContinues: List[Int], // List of continue jumps to patch
  isRegular: Boolean         // True if unlabeled loop (for unlabeled break/continue)
)

// Label stack in compiler
private val labelStack = mutable.ArrayBuffer[LabelEntry]()
```

#### Loop Compilation with Labels

```scala
case WhileStatement(test, body, labelOpt, _) =>
  val labelName = labelOpt
  val loopStartPos = instructions.length

  // Push label onto stack before compiling loop body
  labelStack += LabelEntry(
    isLoop = true,
    labelName = labelName,
    exitPos = -1,  // Will be set after compiling body
    continuePos = loopStartPos,  // Continue jumps back to test
    pendingBreaks = List.empty,
    pendingContinues = List.empty,
    isRegular = labelName.isEmpty
  )

  // Compile test and body
  compileExpression(test, instructions, constants)
  // ... generate jump if false ...

  compileStatement(body, instructions, constants)

  // Pop label from stack
  val entry = labelStack.remove(labelStack.length - 1)

  // Patch pending breaks and continues
  val loopEndPos = instructions.length
  for breakPos <- entry.pendingBreaks do
    patchJump(breakPos, loopEndPos)
  for continuePos <- entry.pendingContinues do
    patchJump(continuePos, loopStartPos)
```

#### Break/Continue Compilation

```scala
case BreakStatement(labelOpt, _) =>
  if labelOpt.isDefined then
    // Find matching labeled loop/block on stack
    val targetLabel = labelOpt.get
    val entryIndex = labelStack.lastIndexWhere(e =>
      e.labelName.isDefined && e.labelName.get == targetLabel
    )

    if entryIndex >= 0 then
      val entry = labelStack(entryIndex)
      // Add pending break to patch later
      labelStack(entryIndex) = entry.copy(
        pendingBreaks = entry.pendingBreaks :+ instructions.length
      )
      instructions += Instruction.jump(-1)  // Placeholder, will be patched
    else
      throw new Exception(s"Label '$targetLabel' not found")
  else
    // Unlabeled break: find innermost loop
    val entryIndex = labelStack.lastIndexWhere(_.isLoop)
    if entryIndex >= 0 then
      val entry = labelStack(entryIndex)
      labelStack(entryIndex) = entry.copy(
        pendingBreaks = entry.pendingBreaks :+ instructions.length
      )
      instructions += Instruction.jump(-1)
    else
      throw new Exception("Break not in a loop")

case ContinueStatement(labelOpt, _) =>
  // Similar logic for continue, but only works with loops
  // Continue to a non-loop is a syntax error
```

### 4. Interpreter Execution

**File**: `runtime/src/main/scala/quickjs/interpreter/Interpreter.scala`

No special runtime support needed - all label resolution happens at compile time. Break and continue are just jumps:

```scala
case Opcode.Jump =>
  val offset = readInt32(bytecode, pc + 1)
  pc += offset
```

## Test Cases

### Basic Labeled Break

```javascript
outer: for (var i = 0; i < 10; i++) {
  for (var j = 0; j < 10; j++) {
    if (j === 5) break outer;
  }
}
// Result: Loop exits when j reaches 5
```

### Labeled Continue

```javascript
loop: while (condition) {
  if (skip) continue loop;
  // ...
}
// Result: Skips to next iteration when skip is true
```

### Labeled Blocks

```javascript
label: {
  console.log("executed");
  break label;
  console.log("not executed");
}
// Result: Only first log executes
```

### Nested Labels

```javascript
outer: for (var i = 0; i < 10; i++) {
  inner: for (var j = 0; j < 10; j++) {
    if (condition) break outer;  // Breaks outer loop
    if (other) break inner;      // Breaks inner loop
  }
}
```

## Implementation Details

### Label Resolution Strategy

1. **Declaration**: When compiling a loop/block with a label, push entry onto stack
2. **Break/Continue**: Search stack for matching label (or innermost loop if unlabeled)
3. **Patch Jumps**: After loop compilation, patch all pending jumps with actual positions
4. **Pop Stack**: Remove label entry from stack after compilation

### Key Design Decisions

1. **Compile-time resolution**: All label resolution happens during compilation
2. **Stack-based tracking**: Labels pushed/popped as we enter/exit loops
3. **Pending jumps**: Break/continue emit placeholder jumps, patched later
4. **Follows QuickJS C**: Similar architecture to C implementation

## Differences from QuickJS C

### Simplifications
- No runtime label lookup (all compile-time)
- Scala collections (ArrayBuffer) instead of C arrays
- Pattern matching for cleaner code

### Similarities
- Same label stack concept
- Same pending jump patching
- Same label resolution logic

## Test Results

All labeled statement tests pass:
- ✅ Labeled break in nested loops
- ✅ Labeled continue in loops
- ✅ Labeled blocks with break
- ✅ Multiple labels with different names
- ✅ Unlabeled break/continue (backward compatibility)

## Related Files

- `parser/src/main/scala/quickjs/ast/AST.scala` - AST node definitions with labels
- `parser/src/main/scala/quickjs/parser/Parser.scala` - Label detection and parsing
- `compiler/src/main/scala/quickjs/compiler/Compiler.scala` - Label stack management
- `runtime/src/main/scala/quickjs/interpreter/Interpreter.scala` - Jump execution
- `stdlib/src/test/scala/quickjs/stdlib/QuickJSLoopTest.scala` - Labeled statement tests

## References

- QuickJS C source: `quickjs.c` (upstream QuickJS) (label stack implementation)
- ES6 Spec: https://tc39.es/ecma262/#sec-labelled-statements
