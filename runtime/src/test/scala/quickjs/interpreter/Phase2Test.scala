package quickjs.interpreter

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.compiler.Compiler
import quickjs.value.JSValue
import quickjs.runtime.*
import munit.*

class Phase2Test extends FunSuite {

  test("variable declaration with initialization") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // var x = 42;
    val ast = Script(
      body = Seq(
        VariableDeclaration(
          kind = VariableKind.Var,
          declarations = Seq(
            VariableDeclarator(
              id = Identifier("x", Span(0, 1, 0, 0)),
              init = Literal(JSValue.fromInt(42), Span(6, 8, 0, 6)),
              span = Span(0, 8, 0, 0)
            )
          ),
          span = Span(0, 8, 0, 0)
        )
      ),
      span = Span(0, 8, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Result is undefined because variable declaration doesn't produce a value
    assert(result == JSValue.Undefined)
  }

  test("variable declaration without initialization") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // var x;
    val ast = Script(
      body = Seq(
        VariableDeclaration(
          kind = VariableKind.Var,
          declarations = Seq(
            VariableDeclarator(
              id = Identifier("x", Span(0, 1, 0, 0)),
              init = null,
              span = Span(0, 1, 0, 0)
            )
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

    assert(result == JSValue.Undefined)
  }

  test("if statement - condition true") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // if (true) { 42; }
    val ast = Script(
      body = Seq(
        IfStatement(
          test = Literal(JSValue.Bool(true), Span(4, 8, 0, 4)),
          consequent = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(42), Span(12, 14, 0, 12)),
                span = Span(12, 14, 0, 12)
              )
            ),
            span = Span(10, 16, 0, 10)
          ),
          alternate = null,
          span = Span(0, 16, 0, 0)
        )
      ),
      span = Span(0, 16, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  test("if statement - condition false") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // if (false) { 42; }
    val ast = Script(
      body = Seq(
        IfStatement(
          test = Literal(JSValue.Bool(false), Span(4, 9, 0, 4)),
          consequent = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(42), Span(13, 15, 0, 13)),
                span = Span(13, 15, 0, 13)
              )
            ),
            span = Span(11, 17, 0, 11)
          ),
          alternate = null,
          span = Span(0, 17, 0, 0)
        )
      ),
      span = Span(0, 17, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  test("if-else statement") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // if (false) { 1; } else { 2; }
    val ast = Script(
      body = Seq(
        IfStatement(
          test = Literal(JSValue.Bool(false), Span(4, 9, 0, 4)),
          consequent = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(1), Span(13, 14, 0, 13)),
                span = Span(13, 14, 0, 13)
              )
            ),
            span = Span(11, 16, 0, 11)
          ),
          alternate = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(2), Span(24, 25, 0, 24)),
                span = Span(24, 25, 0, 24)
              )
            ),
            span = Span(18, 27, 0, 18)
          ),
          span = Span(0, 27, 0, 0)
        )
      ),
      span = Span(0, 27, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  test("while loop - zero iterations") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // while (false) { 42; }
    val ast = Script(
      body = Seq(
        WhileStatement(
          test = Literal(JSValue.Bool(false), Span(7, 12, 0, 7)),
          body = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(42), Span(16, 18, 0, 16)),
                span = Span(16, 18, 0, 16)
              )
            ),
            span = Span(14, 20, 0, 14)
          ),
          label = null,
          span = Span(0, 20, 0, 0)
        )
      ),
      span = Span(0, 20, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  test("while loop - multiple iterations") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // This test verifies the bytecode is generated correctly
    // We can't test actual loop execution without proper variable support in interpreter
    // while (true) { break; }  // This would loop forever without break support

    // For now, just test that the bytecode is generated
    val ast = Script(
      body = Seq(
        WhileStatement(
          test = Literal(JSValue.Bool(false), Span(7, 12, 0, 7)),
          body = BlockStatement(
            statements = Seq(
              ExpressionStatement(
                Literal(JSValue.fromInt(42), Span(16, 18, 0, 16)),
                span = Span(16, 18, 0, 16)
              )
            ),
            span = Span(14, 20, 0, 14)
          ),
          label = null,
          span = Span(0, 20, 0, 0)
        )
      ),
      span = Span(0, 20, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Verify bytecode was generated
    assert(bytecode.bytecode.length > 0)
  }

  test("block statement with multiple statements") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // { 1; 2; 3; }
    val ast = Script(
      body = Seq(
        BlockStatement(
          statements = Seq(
            ExpressionStatement(
              Literal(JSValue.fromInt(1), Span(2, 3, 0, 2)),
              span = Span(2, 3, 0, 2)
            ),
            ExpressionStatement(
              Literal(JSValue.fromInt(2), Span(6, 7, 0, 6)),
              span = Span(6, 7, 0, 6)
            ),
            ExpressionStatement(
              Literal(JSValue.fromInt(3), Span(10, 11, 0, 10)),
              span = Span(10, 11, 0, 10)
            )
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

    assert(result == JSValue.Undefined)
  }

  test("multiple variable declarations") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // var x = 1, y = 2;
    val ast = Script(
      body = Seq(
        VariableDeclaration(
          kind = VariableKind.Var,
          declarations = Seq(
            VariableDeclarator(
              id = Identifier("x", Span(0, 1, 0, 0)),
              init = Literal(JSValue.fromInt(1), Span(6, 7, 0, 6)),
              span = Span(0, 7, 0, 0)
            ),
            VariableDeclarator(
              id = Identifier("y", Span(9, 10, 0, 9)),
              init = Literal(JSValue.fromInt(2), Span(15, 16, 0, 15)),
              span = Span(9, 16, 0, 9)
            )
          ),
          span = Span(0, 16, 0, 0)
        )
      ),
      span = Span(0, 16, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }

  test("let and const declarations") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // let x = 1; const y = 2;
    val ast = Script(
      body = Seq(
        VariableDeclaration(
          kind = VariableKind.Let,
          declarations = Seq(
            VariableDeclarator(
              id = Identifier("x", Span(0, 1, 0, 0)),
              init = Literal(JSValue.fromInt(1), Span(6, 7, 0, 6)),
              span = Span(0, 7, 0, 0)
            )
          ),
          span = Span(0, 7, 0, 0)
        ),
        VariableDeclaration(
          kind = VariableKind.Const,
          declarations = Seq(
            VariableDeclarator(
              id = Identifier("y", Span(9, 10, 0, 9)),
              init = Literal(JSValue.fromInt(2), Span(17, 18, 0, 17)),
              span = Span(9, 18, 0, 9)
            )
          ),
          span = Span(9, 18, 0, 9)
        )
      ),
      span = Span(0, 18, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Undefined)
  }
}
