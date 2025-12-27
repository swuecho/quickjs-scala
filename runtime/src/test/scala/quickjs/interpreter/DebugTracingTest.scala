package quickjs.interpreter

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.compiler.Compiler
import quickjs.value.JSValue
import quickjs.runtime.{JSContext, JSRuntime}
import munit.*

class DebugTracingTest extends FunSuite {

  override def beforeEach(context: BeforeEach): Unit =
    // Ensure tracer is disabled before each test
    DebugTracer.global.disable()
    DebugTracer.global.clear()

  override def afterEach(context: AfterEach): Unit =
    // Clean up after each test
    DebugTracer.global.disable()
    DebugTracer.global.clear()

  test("Debug tracer captures instructions") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Enable debug tracing
    DebugTracer.global.enable()
    DebugTracer.global.clear()

    // Compile and execute: 1 + 2
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

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Get trace output
    val trace = DebugTracer.global.getOutput

    // Verify trace contains expected instructions
    assert(trace.nonEmpty, "Trace should not be empty")
    assert(trace.contains("PushI32"), "Trace should contain PushI32")
    assert(trace.contains("Add"), "Trace should contain Add")

    // Clean up
    DebugTracer.global.disable()
    DebugTracer.global.clear()
  }

  test("Debug tracer shows stack values") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    DebugTracer.global.enable()
    DebugTracer.global.clear()

    // Execute: 1 + 2 + 3
    val ast = Script(
      body = Seq(
        ExpressionStatement(
          BinaryExpression(
            operator = BinaryOperator.Add,
            left = BinaryExpression(
              operator = BinaryOperator.Add,
              left = Literal(JSValue.fromInt(1), Span(0, 1, 0, 0)),
              right = Literal(JSValue.fromInt(2), Span(4, 5, 0, 4)),
              span = Span(0, 5, 0, 0)
            ),
            right = Literal(JSValue.fromInt(3), Span(8, 9, 0, 8)),
            span = Span(0, 9, 0, 0)
          ),
          span = Span(0, 9, 0, 0)
        )
      ),
      span = Span(0, 9, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    val trace = DebugTracer.global.getOutput

    // Verify trace shows stack values
    assert(trace.contains("stack:"), "Trace should show stack")

    DebugTracer.global.disable()
    DebugTracer.global.clear()
  }

  test("Debug tracer can be disabled") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Make sure it's disabled
    DebugTracer.global.disable()
    DebugTracer.global.clear()

    // Execute code
    val ast = Script(
      body = Seq(
        ExpressionStatement(
          Literal(JSValue.fromInt(42), Span(0, 2, 0, 0)),
          span = Span(0, 2, 0, 0)
        )
      ),
      span = Span(0, 2, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Trace should be empty when disabled
    val trace = DebugTracer.global.getOutput
    assert(trace.isEmpty, "Trace should be empty when disabled")
  }
}
