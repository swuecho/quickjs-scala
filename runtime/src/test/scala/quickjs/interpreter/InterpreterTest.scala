package quickjs.interpreter

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.compiler.Compiler
import quickjs.value.JSValue
import quickjs.runtime.*
import munit.*

class InterpreterTest extends FunSuite:

  test("evaluate 1 + 2 = 3") =>
    // Create runtime and context
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)

    // Build AST for "1 + 2"
    val ast = Script(
      body = Seq(
        ExpressionStatement(
          BinaryExpression(
            operator = ast.BinaryOperator.Add,
            left = Literal(JSValue.fromInt(1), Span(0, 1, 0, 0)),
            right = Literal(JSValue.fromInt(2), Span(4, 5, 0, 4)),
            span = Span(0, 5, 0, 0)
          ),
          span = Span(0, 5, 0, 0)
        )
      ),
      span = Span(0, 5, 0, 0)
    )

    // Compile AST to bytecode
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute bytecode
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Check result (will be Undefined due to ExpressionStatement dropping the result)
    assert(result == JSValue.Undefined)

  test("evaluate arithmetic operations") =>
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)

    val operations = Seq(
      (BinaryOperator.Add, 1, 2, 3),
      (BinaryOperator.Sub, 5, 3, 2),
      (BinaryOperator.Mul, 3, 4, 12),
      (BinaryOperator.Div, 10, 2, 5),
      (BinaryOperator.Mod, 10, 3, 1)
    )

    for (op, a, b, expected) <- operations do
      val ast = Script(
        body = Seq(
          ExpressionStatement(
            BinaryExpression(
              operator = op,
              left = Literal(JSValue.fromInt(a), Span(0, 1, 0, 0)),
              right = Literal(JSValue.fromInt(b), Span(0, 1, 0, 0)),
              span = Span(0, 1, 0, 0)
            ),
            span = Span(0, 1, 0, 0)
          )
        ),
        span = Span(0, 1, 0, 0)
      )

      val compiler = Compiler()
      val bytecode = compiler.compileScript(ast)
      val interpreter = Interpreter()
      val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

      // Result is undefined because ExpressionStatement drops it
      assert(result == JSValue.Undefined)

  test("JSValue arithmetic operations") =>
    val result = JSValue.add(
      JSValue.fromInt(1),
      JSValue.fromInt(2)
    )
    assert(result == JSValue.fromInt(3))

    val result2 = JSValue.subtract(
      JSValue.fromInt(5),
      JSValue.fromInt(3)
    )
    assert(result2 == JSValue.fromInt(2))

    val result3 = JSValue.multiply(
      JSValue.fromInt(3),
      JSValue.fromInt(4)
    )
    assert(result3 == JSValue.fromInt(12))

    val result4 = JSValue.divide(
      JSValue.fromInt(10),
      JSValue.fromInt(2)
    )
    assert(result4 == JSValue.fromInt(5))
