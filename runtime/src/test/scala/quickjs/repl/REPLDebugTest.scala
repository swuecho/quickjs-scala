package quickjs.repl

import quickjs.runtime.{JSRuntime, JSContext}
import quickjs.interpreter.{DebugTracer, VariableInspector, DebugCommand}
import munit.*

class REPLDebugTest extends FunSuite {

  test("DebugCommand.parse - help command") {
    val cmd = DebugCommand.parse(".help")
    assert(cmd == DebugCommand.Help)
  }

  test("DebugCommand.parse - debug enable") {
    val cmd = DebugCommand.parse(".debug")
    assert(cmd == DebugCommand.TraceEnable)
  }

  test("DebugCommand.parse - debug disable") {
    val cmd = DebugCommand.parse(".nodebug")
    assert(cmd == DebugCommand.TraceDisable)
  }

  test("DebugCommand.parse - trace enable") {
    val cmd = DebugCommand.parse(".trace")
    assert(cmd == DebugCommand.TraceEnable)
  }

  test("DebugCommand.parse - vars command") {
    val cmd = DebugCommand.parse(".vars")
    assert(cmd == DebugCommand.Vars)
  }

  test("DebugCommand.parse - unknown command") {
    val cmd = DebugCommand.parse(".unknown")
    assert(cmd == DebugCommand.Unknown)
  }

  test("DebugTracer - enable/disable") {
    val tracer = new DebugTracer()

    assert(!tracer.isEnabled)

    tracer.enable()
    assert(tracer.isEnabled)

    tracer.disable()
    assert(!tracer.isEnabled)
  }

  test("DebugTracer - global instance") {
    val global = DebugTracer.global

    assert(!global.isEnabled)

    global.enable()
    assert(global.isEnabled)

    global.disable()
    assert(!global.isEnabled)

    // Clean up
    global.clear()
  }

  test("DebugTracer - trace instruction") {
    val tracer = new DebugTracer()
    tracer.enable()

    val stack = Array[quickjs.value.JSValue](
      quickjs.value.JSValue.fromInt(1),
      quickjs.value.JSValue.fromInt(2)
    )
    val locals = Array[quickjs.value.JSValue.VarRef](
      new quickjs.value.JSValue.VarRef(quickjs.value.JSValue.fromInt(42))
    )

    tracer.traceInstruction(
      pc = 0,
      opcode = quickjs.bytecode.Opcode.Add,
      stack = stack,
      stackTop = 2,
      locals = locals,
      localsCount = 1
    )

    val output = tracer.getOutput
    assert(output.nonEmpty)
    assert(output.contains("Add"))
  }

  test("VariableInspector - format values") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val inspector = VariableInspector()

    // Test integer
    val intVal = quickjs.value.JSValue.fromInt(42)
    val intStr = formatViaInspector(intVal)
    assert(intStr.contains("42"))

    // Test string
    val strVal = quickjs.value.JSValue.fromString("hello")
    val strStr = formatViaInspector(strVal)
    assert(strStr.contains("\"hello\""))

    // Test boolean
    val boolVal = quickjs.value.JSValue.Bool(true)
    val boolStr = formatViaInspector(boolVal)
    assert(boolStr.contains("true"))

    // Test undefined
    val undefStr = formatViaInspector(quickjs.value.JSValue.Undefined)
    assert(undefStr.contains("undefined"))

    // Test null
    val nullStr = formatViaInspector(quickjs.value.JSValue.Null)
    assert(nullStr.contains("null"))
  }

  test("VariableInspector - inspect globals") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val inspector = VariableInspector()
    val globals = inspector.inspectGlobals

    assert(globals.nonEmpty)
    assert(globals.contains("Global variables:"))
  }

  // Helper function to call formatValue via inspectLocals
  private def formatViaInspector(value: quickjs.value.JSValue): String =
    val locals = Array[quickjs.value.JSValue.VarRef](new quickjs.value.JSValue.VarRef(value))
    val inspector = VariableInspector()
    val output = inspector.inspectLocals(locals, 1)
    output.linesIterator.drop(1).next()  // Skip header, get first variable line
      .replaceAll("\\u001B\\[[0-9;]+m", "")  // Remove ANSI colors
      .trim
      .drop(4)  // Remove "[0] " prefix
}
