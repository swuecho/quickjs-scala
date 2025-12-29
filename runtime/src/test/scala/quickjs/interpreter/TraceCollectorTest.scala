package quickjs.interpreter

import quickjs.ast.*
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.tracing.{InstructionTrace, ReturnTrace, TraceCollector}
import quickjs.value.JSValue
import munit.*

class TraceCollectorTest extends FunSuite {

  test("trace collector captures instruction and return events") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

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
    val tracer = TraceCollector()

    interpreter.call(bytecode, JSValue.Undefined, Array.empty, trace = tracer)

    val events = tracer.getEvents
    assert(events.exists(_.isInstanceOf[InstructionTrace]))
    assert(events.exists(_.isInstanceOf[ReturnTrace]))
  }
}
