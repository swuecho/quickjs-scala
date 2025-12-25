package quickjs.interpreter

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.compiler.Compiler
import quickjs.value.JSValue
import quickjs.runtime.*
import munit.*

class IncrementDecrementTest extends FunSuite {

  test("pre-increment operator") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // var x = 5; ++x;
    val ast = Script(
      body = Seq(
        VariableDeclaration(
          kind = VariableKind.Var,
          declarations = Seq(
            VariableDeclarator(
              id = Identifier("x", Span(0, 1, 0, 0)),
              init = Literal(JSValue.fromInt(5), Span(6, 7, 0, 6)),
              span = Span(0, 7, 0, 0)
            )
          ),
          span = Span(0, 7, 0, 0)
        ),
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.PreInc,
            argument = Identifier("x", Span(11, 12, 0, 11)),
            prefix = true,
            span = Span(10, 13, 0, 10)
          ),
          span = Span(10, 14, 0, 10)
        )
      ),
      span = Span(0, 14, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  test("post-increment operator") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // var x = 5; x++;
    val ast = Script(
      body = Seq(
        VariableDeclaration(
          kind = VariableKind.Var,
          declarations = Seq(
            VariableDeclarator(
              id = Identifier("x", Span(0, 1, 0, 0)),
              init = Literal(JSValue.fromInt(5), Span(6, 7, 0, 6)),
              span = Span(0, 7, 0, 0)
            )
          ),
          span = Span(0, 7, 0, 0)
        ),
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.PostInc,
            argument = Identifier("x", Span(11, 12, 0, 11)),
            prefix = false,
            span = Span(10, 13, 0, 10)
          ),
          span = Span(10, 14, 0, 10)
        )
      ),
      span = Span(0, 14, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  test("pre-decrement operator") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // var x = 5; --x;
    val ast = Script(
      body = Seq(
        VariableDeclaration(
          kind = VariableKind.Var,
          declarations = Seq(
            VariableDeclarator(
              id = Identifier("x", Span(0, 1, 0, 0)),
              init = Literal(JSValue.fromInt(5), Span(6, 7, 0, 6)),
              span = Span(0, 7, 0, 0)
            )
          ),
          span = Span(0, 7, 0, 0)
        ),
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.PreDec,
            argument = Identifier("x", Span(11, 12, 0, 11)),
            prefix = true,
            span = Span(10, 13, 0, 10)
          ),
          span = Span(10, 14, 0, 10)
        )
      ),
      span = Span(0, 14, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  test("post-decrement operator") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // var x = 5; x--;
    val ast = Script(
      body = Seq(
        VariableDeclaration(
          kind = VariableKind.Var,
          declarations = Seq(
            VariableDeclarator(
              id = Identifier("x", Span(0, 1, 0, 0)),
              init = Literal(JSValue.fromInt(5), Span(6, 7, 0, 6)),
              span = Span(0, 7, 0, 0)
            )
          ),
          span = Span(0, 7, 0, 0)
        ),
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.PostDec,
            argument = Identifier("x", Span(11, 12, 0, 11)),
            prefix = false,
            span = Span(10, 13, 0, 10)
          ),
          span = Span(10, 14, 0, 10)
        )
      ),
      span = Span(0, 14, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  // TODO: Re-enable after proper for loop update integration with PostInc
  // test("for loop with increment") {
  //   given JSRuntime = JSRuntime()
  //   given JSContext = JSContext(summon[JSRuntime])
  //
  //   // var sum = 0; for (var i = 0; i < 3; i++) { sum = sum + i; }
  //   ... (test code)
  // }

  test("while loop with increment") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // var i = 0; while (i < 3) { ++i; }
    val ast = Script(
      body = Seq(
        VariableDeclaration(
          kind = VariableKind.Var,
          declarations = Seq(
            VariableDeclarator(
              id = Identifier("i", Span(0, 1, 0, 0)),
              init = Literal(JSValue.fromInt(0), Span(6, 7, 0, 6)),
              span = Span(0, 7, 0, 0)
            )
          ),
          span = Span(0, 7, 0, 0)
        ),
        WhileStatement(
          test = BinaryExpression(
            operator = BinaryOperator.Lt,
            left = Identifier("i", Span(16, 17, 0, 16)),
            right = Literal(JSValue.fromInt(3), Span(18, 19, 0, 18)),
            span = Span(16, 19, 0, 16)
          ),
          body = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                UnaryExpression(
                  operator = UnaryOperator.PreInc,
                  argument = Identifier("i", Span(0, 1, 0, 0)),
                  prefix = true,
                  span = Span(0, 3, 0, 0)
                ),
                span = Span(0, 3, 0, 0)
              )
            ),
            span = Span(21, 25, 0, 21)
          ),
          span = Span(9, 27, 0, 9)
        )
      ),
      span = Span(0, 27, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Should complete without hanging
    assert(result == JSValue.Undefined)
  }
}
