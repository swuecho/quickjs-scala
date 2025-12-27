package quickjs.interpreter

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.compiler.Compiler
import quickjs.value.JSValue
import quickjs.runtime.*
import munit.*

class ForLoopTest extends FunSuite {

  test("for loop with initialization only") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // for (var x = 0; ; ) { break; }  // Can't break yet, so use false test
    // for (var x = 0; false; ) { 42; }
    val ast = Script(
      body = Seq(
        ForStatement(
          init = VariableDeclaration(
            kind = VariableKind.Var,
            declarations = Seq(
              VariableDeclarator(
                id = Identifier("x", Span(0, 1, 0, 0)),
                init = Literal(JSValue.fromInt(0), Span(6, 7, 0, 6)),
                span = Span(0, 7, 0, 0)
              )
            ),
            span = Span(0, 7, 0, 0)
          ),
          test = Literal(JSValue.Bool(false), Span(9, 14, 0, 9)),
          update = null,
          body = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(42), Span(18, 20, 0, 18)),
                span = Span(18, 20, 0, 18)
              )
            ),
            span = Span(16, 22, 0, 16)
          ),
          label = null,
          span = Span(0, 22, 0, 0)
        )
      ),
      span = Span(0, 22, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  test("for loop with test and update") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // var x = 5; for (; false; -x) { 42; }
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
        ForStatement(
          init = null,
          test = Literal(JSValue.Bool(false), Span(6, 11, 0, 6)),
          update = UnaryExpression(
            operator = UnaryOperator.Minus,  // Using unary minus instead of ++ for now
            argument = Identifier("x", Span(14, 15, 0, 14)),
            prefix = false,
            span = Span(14, 15, 0, 14)
          ),
          body = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(42), Span(19, 21, 0, 19)),
                span = Span(19, 21, 0, 19)
              )
            ),
            span = Span(17, 23, 0, 17)
          ),
          label = null,
          span = Span(0, 23, 0, 0)
        )
      ),
      span = Span(0, 23, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  // TODO: Re-enable after implementing break statement
  // test("for loop without test (infinite)") {
  //   given JSRuntime = JSRuntime()
  //   given JSContext = JSContext(summon[JSRuntime])
  //
  //   // for (;;) { break; }  // Can't test without break
  //   // Just test bytecode generation for now
  //   val ast = Script(
  //     body = Seq(
  //       ForStatement(
  //         init = null,
  //         test = null,
  //         update = null,
  //         body = BlockStatement(
  //           statements = Seq(
  //             ExpressionStatement(
  //               Literal(JSValue.fromInt(42), Span(7, 9, 0, 7)),
  //               span = Span(7, 9, 0, 7)
  //             )
  //           ),
  //           span = Span(5, 11, 0, 5)
  //         ),
  //         span = Span(0, 11, 0, 0)
  //       )
  //     ),
  //     span = Span(0, 11, 0, 0)
  //   )
  //
  //   val compiler = Compiler()
  //   val bytecode = compiler.compileScript(ast)
  //
  //   // Verify bytecode was generated - DON'T EXECUTE (will loop infinitely without break)
  //   assert(bytecode.bytecode.length > 0)
  //   // Check for Goto instruction (backjump)
  //   assert(bytecode.bytecode.exists(b => (b & 0xFF) == Opcode.Goto.ordinal))
  // }

  test("for loop with expression init") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // for (1 + 1; false; ) { 42; }
    val ast = Script(
      body = Seq(
        ForStatement(
          init = BinaryExpression(
            operator = BinaryOperator.Add,
            left = Literal(JSValue.fromInt(1), Span(1, 2, 0, 1)),
            right = Literal(JSValue.fromInt(1), Span(3, 4, 0, 3)),
            span = Span(1, 4, 0, 1)
          ),
          test = Literal(JSValue.Bool(false), Span(6, 11, 0, 6)),
          update = null,
          body = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(42), Span(15, 17, 0, 15)),
                span = Span(15, 17, 0, 15)
              )
            ),
            span = Span(13, 19, 0, 13)
          ),
          label = null,
          span = Span(0, 19, 0, 0)
        )
      ),
      span = Span(0, 19, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  // TODO: This test needs ++ operator to work properly
  // test("simple counting for loop") {
  //   given JSRuntime = JSRuntime()
  //   given JSContext = JSContext(summon[JSRuntime])
  //
  //   // for (var i = 0; i < 3; i++) { ... }
  //   // Using i - 1 as update since ++ isn't implemented
  //   val ast = Script(
  //     body = Seq(
  //       ForStatement(
  //         init = VariableDeclaration(
  //           kind = VariableKind.Var,
  //           declarations = Seq(
  //             VariableDeclarator(
  //               id = Identifier("i", Span(0, 1, 0, 0)),
  //               init = Literal(JSValue.fromInt(0), Span(6, 7, 0, 6)),
  //               span = Span(0, 7, 0, 0)
  //             )
  //           ),
  //           span = Span(0, 7, 0, 0)
  //         ),
  //         test = BinaryExpression(
  //           operator = BinaryOperator.Lt,
  //           left = Identifier("i", Span(9, 10, 0, 9)),
  //           right = Literal(JSValue.fromInt(3), Span(11, 12, 0, 11)),
  //           span = Span(9, 12, 0, 9)
  //         ),
  //         update = UnaryExpression(
  //           operator = UnaryOperator.Minus,
  //           argument = Identifier("i", Span(15, 16, 0, 15)),
  //           prefix = false,
  //           span = Span(15, 16, 0, 15)
  //         ),
  //         body = BlockStatement(
  //           statements = Seq(
  //             ExpressionStatement(
  //               Identifier("i", Span(19, 20, 0, 19)),
  //               span = Span(19, 20, 0, 19)
  //             )
  //           ),
  //           span = Span(17, 22, 0, 17)
  //         ),
  //         span = Span(0, 22, 0, 0)
  //       )
  //     ),
  //     span = Span(0, 22, 0, 0)
  //   )
  //
  //   val compiler = Compiler()
  //   val bytecode = compiler.compileScript(ast)
  //   val interpreter = Interpreter()
  //   val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
  //
  //   // Note: This won't actually loop correctly without ++ and proper variable access
  //   // But it should compile and execute without crashing
  //   assert(result == JSValue.Undefined)
  // }
}
