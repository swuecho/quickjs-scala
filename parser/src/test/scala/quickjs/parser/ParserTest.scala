package quickjs.parser

import quickjs.lexer.*
import quickjs.ast.*
import quickjs.value.JSValue
import munit.*

class ParserTest extends FunSuite:

  test("parse number literal") {
    val lexer = Lexer("42")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val stmt = script.body(0).asInstanceOf[ExpressionStatement]
    val lit = stmt.expression.asInstanceOf[Literal]
    assertEquals(lit.value, JSValue.fromInt(42))
  }

  test("parse simple addition") {
    val lexer = Lexer("1 + 2")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val stmt = script.body(0).asInstanceOf[ExpressionStatement]
    val bin = stmt.expression.asInstanceOf[BinaryExpression]
    assertEquals(bin.operator, BinaryOperator.Add)
  }

  test("parse variable declaration") {
    val lexer = Lexer("var x = 5;")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val decl = script.body(0).asInstanceOf[VariableDeclaration]
    assertEquals(decl.kind, VariableKind.Var)
    assertEquals(decl.declarations.length, 1)
    decl.declarations(0).id match
      case Identifier(name, _) => assertEquals(name, "x")
      case other => fail(s"Expected identifier declarator, got $other")
  }

  test("parse if statement") {
    val lexer = Lexer("if (x > 0) { return x; }")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val ifStmt = script.body(0).asInstanceOf[IfStatement]
    assert(ifStmt.test != null)
    assert(ifStmt.consequent != null)
    assert(ifStmt.alternate == null)
  }

  test("parse while loop") {
    val lexer = Lexer("while (i < 10) { i = i + 1; }")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val whileStmt = script.body(0).asInstanceOf[WhileStatement]
    assert(whileStmt.test != null)
    assert(whileStmt.body != null)
  }

  test("parse for loop") {
    val lexer = Lexer("for (var i = 0; i < 10; i = i + 1) { }")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val forStmt = script.body(0).asInstanceOf[ForStatement]
    assert(forStmt.init != null)
    assert(forStmt.test != null)
    assert(forStmt.update != null)
  }

  test("parse function call") {
    val lexer = Lexer("foo(1, 2)")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val stmt = script.body(0).asInstanceOf[ExpressionStatement]
    val call = stmt.expression.asInstanceOf[CallExpression]
    assertEquals(call.arguments.length, 2)
  }

  test("parse function declaration") {
    val lexer = Lexer("function add(a, b) { return a + b; }")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val func = script.body(0).asInstanceOf[FunctionDeclaration]
    assertEquals(func.id.name, "add")
    assertEquals(func.params.length, 2)
    func.params(0) match
      case Identifier(name, _) => assertEquals(name, "a")
      case other => fail(s"Expected identifier param, got $other")
    func.params(1) match
      case Identifier(name, _) => assertEquals(name, "b")
      case other => fail(s"Expected identifier param, got $other")
  }

  test("parse return statement") {
    val lexer = Lexer("return 42;")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val ret = script.body(0).asInstanceOf[ReturnStatement]
    assert(ret.argument != null)
  }

  test("parse block statement") {
    val lexer = Lexer("{ var x = 1; x + 2; }")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val block = script.body(0).asInstanceOf[BlockStatement]
    assertEquals(block.statements.length, 2)
  }

  test("parse logical operators") {
    val lexer = Lexer("true && false || true")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val stmt = script.body(0).asInstanceOf[ExpressionStatement]
    val bin = stmt.expression.asInstanceOf[BinaryExpression]
    // Due to precedence, should parse as (true && false) || true
    assertEquals(bin.operator, BinaryOperator.LogicalOr)
  }

  test("parse unary minus") {
    val lexer = Lexer("-42")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val stmt = script.body(0).asInstanceOf[ExpressionStatement]
    val unary = stmt.expression.asInstanceOf[UnaryExpression]
    assertEquals(unary.operator, UnaryOperator.Minus)
  }

  test("parse pre-increment") {
    val lexer = Lexer("++x")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val stmt = script.body(0).asInstanceOf[ExpressionStatement]
    val unary = stmt.expression.asInstanceOf[UnaryExpression]
    assertEquals(unary.operator, UnaryOperator.PreInc)
  }

  test("parse string literal") {
    val lexer = Lexer("\"hello\"")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val stmt = script.body(0).asInstanceOf[ExpressionStatement]
    val lit = stmt.expression.asInstanceOf[Literal]
    assertEquals(lit.value, JSValue.fromString("hello"))
  }

  test("parse boolean literals") {
    val lexer1 = Lexer("true")
    val tokens1 = lexer1.tokenize()
    val parser1 = Parser(tokens1)
    val script1 = parser1.parseScript()

    assertEquals(script1.body.length, 1)
    val stmt1 = script1.body(0).asInstanceOf[ExpressionStatement]
    val lit1 = stmt1.expression.asInstanceOf[Literal]
    assertEquals(lit1.value, JSValue.Bool(true))

    val lexer2 = Lexer("false")
    val tokens2 = lexer2.tokenize()
    val parser2 = Parser(tokens2)
    val script2 = parser2.parseScript()

    assertEquals(script2.body.length, 1)
    val stmt2 = script2.body(0).asInstanceOf[ExpressionStatement]
    val lit2 = stmt2.expression.asInstanceOf[Literal]
    assertEquals(lit2.value, JSValue.Bool(false))
  }

  test("parse null and undefined") {
    val lexer1 = Lexer("null")
    val tokens1 = lexer1.tokenize()
    val parser1 = Parser(tokens1)
    val script1 = parser1.parseScript()

    assertEquals(script1.body.length, 1)
    val stmt1 = script1.body(0).asInstanceOf[ExpressionStatement]
    val lit1 = stmt1.expression.asInstanceOf[Literal]
    assertEquals(lit1.value, JSValue.Null)

    val lexer2 = Lexer("undefined")
    val tokens2 = lexer2.tokenize()
    val parser2 = Parser(tokens2)
    val script2 = parser2.parseScript()

    assertEquals(script2.body.length, 1)
    val stmt2 = script2.body(0).asInstanceOf[ExpressionStatement]
    val lit2 = stmt2.expression.asInstanceOf[Literal]
    assertEquals(lit2.value, JSValue.Undefined)
  }

  test("parse parentheses expression") {
    val lexer = Lexer("(1 + 2) * 3")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val stmt = script.body(0).asInstanceOf[ExpressionStatement]
    val mul = stmt.expression.asInstanceOf[BinaryExpression]
    assertEquals(mul.operator, BinaryOperator.Mul)
    // Left operand should be (1 + 2)
    val add = mul.left.asInstanceOf[BinaryExpression]
    assertEquals(add.operator, BinaryOperator.Add)
  }

  test("parse assignment expression") {
    val lexer = Lexer("x = 10")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val stmt = script.body(0).asInstanceOf[ExpressionStatement]
    val assign = stmt.expression.asInstanceOf[AssignmentExpression]
    assert(assign.left != null)
    assert(assign.right != null)
  }

  test("parse comparison operators") {
    val lexer = Lexer("x < 5 && x >= 1")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    val stmt = script.body(0).asInstanceOf[ExpressionStatement]
    val logAnd = stmt.expression.asInstanceOf[BinaryExpression]
    assertEquals(logAnd.operator, BinaryOperator.LogicalAnd)
  }
