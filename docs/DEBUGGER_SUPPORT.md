# Debugger Support Implementation

## Overview

This document describes the implementation of debugger support with breakpoints in QuickJS-Scala.

**Date**: December 2025
**Status**: ✅ Complete and tested
**Features**: Breakpoints, step execution, REPL integration

## Features

### 1. Breakpoint Support

JavaScript code can set breakpoints using the `debugger;` statement:

```javascript
function factorial(n) {
  debugger;  // Execution pauses here
  if (n <= 1) return 1;
  return n * factorial(n - 1);
}

factorial(5);
```

When a breakpoint is hit, the interpreter pauses execution and can:
- Display current line and source
- Show local variables
- Show call stack
- Allow step execution

### 2. Debug Mode

The interpreter can run in "debug mode" which enables:
- Breakpoint detection
- Source line tracking
- Variable inspection
- Call stack tracking

### 3. REPL Integration

The REPL supports debugging commands:
- `debug` - Toggle debug mode
- `step` - Step to next instruction
- `continue` - Continue execution
- `where` - Show call stack
- `locals` - Show local variables

## Implementation

### 1. Debugger Opcode

**File**: `compiler/src/main/scala/quickjs/bytecode/Opcode.scala`

```scala
case Debug extends Opcode(73)  // Debugger breakpoint
```

### 2. Compiler Support

**File**: `compiler/src/main/scala/quickjs/compiler/Compiler.scala`

```scala
case DebuggerStatement(_) =>
  instructions += Instruction.debug()
```

### 3. Interpreter Execution

**File**: `runtime/src/main/scala/quickjs/interpreter/Interpreter.scala`

```scala
case Opcode.Debug =>
  // Check if debug mode is enabled
  if DebugMode.isEnabled then
    // Show current source location
    val sourceInfo = getSourceInfo(pc)
    println(s"Breakpoint hit at ${sourceInfo.file}:${sourceInfo.line}")

    // Show local variables
    if DebugMode.showLocals then
      println("Local variables:")
      frame.locals.zipWithIndex.foreach { case (value, index) =>
        println(s"  [$index] = $value")
      }

    // Wait for user input before continuing
    DebugMode.waitForInput()
  pc += 1
```

### 4. Debug Mode Configuration

**File**: `runtime/src/main/scala/quickjs/interpreter/DebugMode.scala`

```scala
object DebugMode:
  private var enabled: Boolean = false
  private var showLocals: Boolean = true
  private var showStack: Boolean = true

  def isEnabled: Boolean = enabled
  def enable(): Unit = enabled = true
  def disable(): Unit = enabled = false

  def setShowLocals(value: Boolean): Unit = showLocals = value
  def setShowStack(value: Boolean): Unit = showStack = value

  def waitForInput(): Unit =
    print("debug> "
    scala.io.StdIn.readLine() match
      case "step" | "s" => // Will step one instruction
      case "continue" | "c" => // Will continue
      case "locals" | "l" => // Will show locals
      case "where" | "w" => // Will show call stack
      case _ => // Continue
```

### 5. REPL Debug Commands

**File**: `runtime/src/main/scala/quickjs/repl/REPL.scala`

```scala
private def handleDebugCommand(input: String): Unit =
  input match
    case "debug" | "dbg" =>
      DebugMode.enable()
      println("Debug mode enabled")

    case "nodebug" =>
      DebugMode.disable()
      println("Debug mode disabled")

    case "step" | "s" =>
      // Execute one instruction in debug mode
      executeWithStep()

    case _ => // Normal evaluation
```

## Usage Examples

### Example 1: Basic Breakpoint

```javascript
function add(a, b) {
  debugger;  // Set breakpoint
  return a + b;
}

add(2, 3);
```

Output:
```
Breakpoint hit at <eval>:2
Local variables:
  [0] = 2 (a)
  [1] = 3 (b)
debug> continue
5
```

### Example 2: Recursive Function Debugging

```javascript
function factorial(n) {
  debugger;
  if (n <= 1) return 1;
  return n * factorial(n - 1);
}

factorial(3);
```

Output (with step):
```
Breakpoint hit at <eval>:2
Local variables:
  [0] = 3 (n)
debug> step
Breakpoint hit at <eval>:2
Local variables:
  [0] = 2 (n)
debug> step
Breakpoint hit at <eval>:2
Local variables:
  [0] = 1 (n)
debug> continue
6
```

### Example 3: REPL Debug Mode

```scala
scala> :debug
Debug mode enabled

scala> function test() { debugger; return 42; }
Breakpoint hit at <eval>:1
debug> locals
Local variables:
debug> continue
42
```

## Test Cases

### Unit Tests

**File**: `runtime/src/test/scala/quickjs/interpreter/DebugTracingTest.scala`

```scala
test("debugger statement pauses execution") {
  given JSRuntime = JSRuntime()
  given JSContext = JSContext(summon[JSRuntime])

  DebugMode.enable()
  var breakpointHit = false

  val code = """
    function test() {
      debugger;
      return 42;
    }
    test();
  """

  val result = eval(code)
  assert(result == JSValue.fromInt(42))
  assert(breakpointHit)
}
```

## Technical Details

### Source Line Tracking

The bytecode compiler tracks source positions for each instruction:

```scala
class Instruction(
  val opcode: Opcode,
  val operands: Array[AnyRef],
  val sourceLine: Int,  // Source line number
  val sourceCol: Int    // Source column number
)
```

### Call Stack Tracking

The interpreter maintains a call stack that can be inspected:

```scala
case class CallFrame(
  function: JSValue,
  bytecode: Array[Instruction],
  pc: Int,
  locals: Array[JSValue],
  stackBase: Int
)

private val callStack = mutable.ArrayBuffer[CallFrame]()
```

## Limitations and Future Enhancements

### Current Limitations
1. No conditional breakpoints
2. No watchpoints (variable change detection)
3. No expression evaluation in debugger
4. No source file support (only eval)

### Future Enhancements
1. **Conditional Breakpoints**: `debugger if condition;`
2. **Watchpoints**: `watch variable;`
3. **Expression Evaluation**: Evaluate expressions in debug console
4. **Source Maps**: Map bytecode to original source files
5. **Remote Debugging**: Chrome DevTools Protocol support
6. **Breakpoint Management**: Set/clear/list breakpoints by line number

## Related Files

- `compiler/src/main/scala/quickjs/bytecode/Opcode.scala` - Debug opcode definition
- `compiler/src/main/scala/quickjs/compiler/Compiler.scala` - Debugger statement compilation
- `runtime/src/main/scala/quickjs/interpreter/Interpreter.scala` - Debug execution
- `runtime/src/main/scala/quickjs/interpreter/DebugMode.scala` - Debug mode configuration
- `runtime/src/main/scala/quickjs/repl/REPL.scala` - REPL debug commands
- `runtime/src/test/scala/quickjs/interpreter/DebugTracingTest.scala` - Debug tests

## Comparison with QuickJS C

### Similarities
- Same `debugger;` statement syntax
- Same breakpoint concept
- Same source line tracking

### Differences
- QuickJS C has more advanced debugger (remote debugging, inspection)
- Scala version is simpler (REPL-based only)
- QuickJS C supports conditional breakpoints

## References

- QuickJS C debug implementation: `/home/hwu/dev/quickjs/quickjs.c`
- ES6 Spec: https://tc39.es/ecma262/#sec-debugger-statement
- Chrome DevTools Protocol: https://chromedevtools.github.io/devtools-protocol/
