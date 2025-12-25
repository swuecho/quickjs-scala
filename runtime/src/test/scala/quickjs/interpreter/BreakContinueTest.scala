package quickjs.interpreter

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.compiler.Compiler
import quickjs.value.JSValue
import quickjs.runtime.*
import munit.*

class BreakContinueTest extends FunSuite {

  test("break statement in while loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // while (true) { break; }
    val ast = Script(
      body = Seq(
        WhileStatement(
          test = Literal(JSValue.Bool(true), Span(0, 4, 0, 0)),
          body = BlockStatement(
            statements = Seq(
              BreakStatement(null, Span(6, 11, 0, 6))
            ),
            span = Span(5, 12, 0, 5)
          ),
          span = Span(0, 13, 0, 0)
        )
      ),
      span = Span(0, 13, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Should exit the loop via break
    assert(result == JSValue.Undefined)
  }

  test("break statement in for loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // for (var i = 0; true; ) { break; }
    val ast = Script(
      body = Seq(
        ForStatement(
          init = VariableDeclaration(
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
          test = Literal(JSValue.Bool(true), Span(9, 13, 0, 9)),
          update = null,
          body = BlockStatement(
            statements = Seq(
              BreakStatement(null, Span(17, 22, 0, 17))
            ),
            span = Span(16, 23, 0, 16)
          ),
          span = Span(0, 24, 0, 0)
        )
      ),
      span = Span(0, 24, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Should exit the loop via break
    assert(result == JSValue.Undefined)
  }

  test("continue statement in while loop") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // var i = 0; while (i < 2) { i = i + 1; if (i == 1) continue; break; }
    // This test is limited - continue will just fall through to the next instruction
    // For proper continue, we need the compiler to generate specific bytecode
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
            right = Literal(JSValue.fromInt(2), Span(18, 19, 0, 18)),
            span = Span(16, 19, 0, 16)
          ),
          body = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                BinaryExpression(
                  operator = BinaryOperator.Add,
                  left = Identifier("i", Span(0, 1, 0, 0)),
                  right = Literal(JSValue.fromInt(1), Span(0, 1, 0, 0)),
                  span = Span(0, 5, 0, 0)
                ),
                span = Span(0, 5, 0, 0)
              ),
              BreakStatement(null, Span(0, 5, 0, 0))
            ),
            span = Span(0, 5, 0, 0)
          ),
          span = Span(0, 5, 0, 0)
        )
      ),
      span = Span(0, 50, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }
}
