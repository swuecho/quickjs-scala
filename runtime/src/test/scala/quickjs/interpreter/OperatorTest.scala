package quickjs.interpreter

import quickjs.ast.*
import quickjs.bytecode.*
import quickjs.compiler.Compiler
import quickjs.value.JSValue
import quickjs.runtime.*
import quickjs.objmodel.JSObject
import munit.*

class OperatorTest extends FunSuite {

  test("typeof operator - number") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // typeof 42
    val ast = Script(
      body = Seq(
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.Typeof,
            argument = Literal(JSValue.fromInt(42), Span(7, 9, 0, 7)),
            prefix = true,
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

    assert(result == JSValue.fromString("number"))
  }

  test("typeof operator - string") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // typeof "hello"
    val ast = Script(
      body = Seq(
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.Typeof,
            argument = Literal(JSValue.fromString("hello"), Span(7, 14, 0, 7)),
            prefix = true,
            span = Span(0, 14, 0, 0)
          ),
          span = Span(0, 14, 0, 0)
        )
      ),
      span = Span(0, 14, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.fromString("string"))
  }

  test("typeof operator - boolean") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // typeof true
    val ast = Script(
      body = Seq(
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.Typeof,
            argument = Literal(JSValue.Bool(true), Span(7, 11, 0, 7)),
            prefix = true,
            span = Span(0, 11, 0, 0)
          ),
          span = Span(0, 11, 0, 0)
        )
      ),
      span = Span(0, 11, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.fromString("boolean"))
  }

  test("typeof operator - object") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // typeof {}
    val ast = Script(
      body = Seq(
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.Typeof,
            argument = ObjectLiteral(
              properties = Seq.empty,
              span = Span(7, 9, 0, 7)
            ),
            prefix = true,
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

    assert(result == JSValue.fromString("object"))
  }

  test("typeof operator - undefined") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // typeof undefined
    val ast = Script(
      body = Seq(
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.Typeof,
            argument = Literal(JSValue.Undefined, Span(7, 17, 0, 7)),
            prefix = true,
            span = Span(0, 17, 0, 0)
          ),
          span = Span(0, 17, 0, 0)
        )
      ),
      span = Span(0, 17, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.fromString("undefined"))
  }

  test("in operator - property exists") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // "x" in obj
    // First create obj = {x: 1}
    val obj = JSObject(prototype = null, extensible = true)
    obj.set("x", JSValue.fromInt(1))
    summon[JSContext].global.set("obj", JSValue.Object(obj))

    val ast = Script(
      body = Seq(
        ExpressionStatement(
          BinaryExpression(
            operator = BinaryOperator.In,
            left = Literal(JSValue.fromString("x"), Span(0, 3, 0, 0)),
            right = Identifier("obj", Span(7, 10, 0, 7)),
            span = Span(0, 10, 0, 0)
          ),
          span = Span(0, 10, 0, 0)
        )
      ),
      span = Span(0, 10, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Bool(true))
  }

  test("in operator - property does not exist") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // "y" in obj
    // First create obj = {x: 1}
    val obj = JSObject(prototype = null, extensible = true)
    obj.set("x", JSValue.fromInt(1))
    summon[JSContext].global.set("obj", JSValue.Object(obj))

    val ast = Script(
      body = Seq(
        ExpressionStatement(
          BinaryExpression(
            operator = BinaryOperator.In,
            left = Literal(JSValue.fromString("y"), Span(0, 3, 0, 0)),
            right = Identifier("obj", Span(7, 10, 0, 7)),
            span = Span(0, 10, 0, 0)
          ),
          span = Span(0, 10, 0, 0)
        )
      ),
      span = Span(0, 10, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Bool(false))
  }

  test("delete operator - delete existing property") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // delete obj.x
    val obj = JSObject(prototype = null, extensible = true)
    obj.set("x", JSValue.fromInt(1))
    summon[JSContext].global.set("obj", JSValue.Object(obj))

    val ast = Script(
      body = Seq(
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.Delete,
            argument = MemberExpression(
              `object` = Identifier("obj", Span(7, 10, 0, 7)),
              property = Identifier("x", Span(11, 12, 0, 11)),
              computed = false,
              span = Span(7, 12, 0, 7)
            ),
            prefix = true,
            span = Span(0, 12, 0, 0)
          ),
          span = Span(0, 12, 0, 0)
        )
      ),
      span = Span(0, 12, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Bool(true))
  }

  test("instanceof operator - basic test") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // obj instanceof Function
    val obj = JSObject(prototype = null, extensible = true)
    summon[JSContext].global.set("obj", JSValue.Object(obj))

    val ast = Script(
      body = Seq(
        ExpressionStatement(
          BinaryExpression(
            operator = BinaryOperator.Instanceof,
            left = Identifier("obj", Span(0, 3, 0, 0)),
            right = Identifier("Function", Span(15, 23, 0, 15)),
            span = Span(0, 23, 0, 0)
          ),
          span = Span(0, 23, 0, 0)
        )
      ),
      span = Span(0, 23, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.Bool(false))
  }

  test("typeof operator - array") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // typeof []
    val ast = Script(
      body = Seq(
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.Typeof,
            argument = ArrayLiteral(
              elements = Seq.empty,
              span = Span(7, 9, 0, 7)
            ),
            prefix = true,
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

    assert(result == JSValue.fromString("object"))
  }

  test("typeof operator - function") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // typeof function() {}
    val ast = Script(
      body = Seq(
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.Typeof,
            argument = FunctionExpression(
              id = null,
              params = Seq.empty,
              body = BlockStatement(
                statements = Seq.empty,
                span = Span(16, 18, 0, 16)
              ),
              isGenerator = false,
              isAsync = false,
              span = Span(7, 18, 0, 7)
            ),
            prefix = true,
            span = Span(0, 18, 0, 0)
          ),
          span = Span(0, 18, 0, 0)
        )
      ),
      span = Span(0, 18, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.fromString("function"))
  }

  test("typeof operator - null") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // typeof null
    val ast = Script(
      body = Seq(
        ExpressionStatement(
          UnaryExpression(
            operator = UnaryOperator.Typeof,
            argument = Literal(JSValue.Null, Span(7, 11, 0, 7)),
            prefix = true,
            span = Span(0, 11, 0, 0)
          ),
          span = Span(0, 11, 0, 0)
        )
      ),
      span = Span(0, 11, 0, 0)
    )

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assert(result == JSValue.fromString("object"))
  }
}
