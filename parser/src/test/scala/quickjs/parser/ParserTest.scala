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
    script.body(0) match
      case stmt: ExpressionStatement =>
        stmt.expression match
          case lit: Literal => assertEquals(lit.value, JSValue.fromInt(42))
          case other => fail(s"Expected Literal, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }

  test("parse simple addition") {
    val lexer = Lexer("1 + 2")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case stmt: ExpressionStatement =>
        stmt.expression match
          case bin: BinaryExpression => assertEquals(bin.operator, BinaryOperator.Add)
          case other => fail(s"Expected BinaryExpression, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }

  test("parse variable declaration") {
    val lexer = Lexer("var x = 5;")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case decl: VariableDeclaration =>
        assertEquals(decl.kind, VariableKind.Var)
        assertEquals(decl.declarations.length, 1)
        decl.declarations(0).id match
          case Identifier(name, _) => assertEquals(name, "x")
          case other => fail(s"Expected identifier declarator, got $other")
      case other => fail(s"Expected VariableDeclaration, got $other")
  }

  test("parse if statement") {
    val lexer = Lexer("if (x > 0) { return x; }")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case ifStmt: IfStatement =>
        assert(ifStmt.test != null)
        assert(ifStmt.consequent != null)
        assert(ifStmt.alternate == null)
      case other => fail(s"Expected IfStatement, got $other")
  }

  test("parse while loop") {
    val lexer = Lexer("while (i < 10) { i = i + 1; }")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case whileStmt: WhileStatement =>
        assert(whileStmt.test != null)
        assert(whileStmt.body != null)
      case other => fail(s"Expected WhileStatement, got $other")
  }

  test("parse for loop") {
    val lexer = Lexer("for (var i = 0; i < 10; i = i + 1) { }")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case forStmt: ForStatement =>
        assert(forStmt.init != null)
        assert(forStmt.test != null)
        assert(forStmt.update != null)
      case other => fail(s"Expected ForStatement, got $other")
  }

  test("parse function call") {
    val lexer = Lexer("foo(1, 2)")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case stmt: ExpressionStatement =>
        stmt.expression match
          case call: CallExpression => assertEquals(call.arguments.length, 2)
          case other => fail(s"Expected CallExpression, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }

  test("parse function declaration") {
    val lexer = Lexer("function add(a, b) { return a + b; }")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case func: FunctionDeclaration =>
        assertEquals(func.id.name, "add")
        assertEquals(func.params.length, 2)
        func.params(0) match
          case Identifier(name, _) => assertEquals(name, "a")
          case other => fail(s"Expected identifier param, got $other")
        func.params(1) match
          case Identifier(name, _) => assertEquals(name, "b")
          case other => fail(s"Expected identifier param, got $other")
      case other => fail(s"Expected FunctionDeclaration, got $other")
  }

  test("parse return statement") {
    val lexer = Lexer("return 42;")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case ret: ReturnStatement => assert(ret.argument != null)
      case other => fail(s"Expected ReturnStatement, got $other")
  }

  test("parse block statement") {
    val lexer = Lexer("{ var x = 1; x + 2; }")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case block: BlockStatement => assertEquals(block.statements.length, 2)
      case other => fail(s"Expected BlockStatement, got $other")
  }

  test("parse logical operators") {
    val lexer = Lexer("true && false || true")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case stmt: ExpressionStatement =>
        stmt.expression match
          case bin: BinaryExpression =>
            // Due to precedence, should parse as (true && false) || true
            assertEquals(bin.operator, BinaryOperator.LogicalOr)
          case other => fail(s"Expected BinaryExpression, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }

  test("parse unary minus") {
    val lexer = Lexer("-42")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case stmt: ExpressionStatement =>
        stmt.expression match
          case unary: UnaryExpression => assertEquals(unary.operator, UnaryOperator.Minus)
          case other => fail(s"Expected UnaryExpression, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }

  test("parse pre-increment") {
    val lexer = Lexer("++x")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case stmt: ExpressionStatement =>
        stmt.expression match
          case unary: UnaryExpression => assertEquals(unary.operator, UnaryOperator.PreInc)
          case other => fail(s"Expected UnaryExpression, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }

  test("parse string literal") {
    val lexer = Lexer("\"hello\"")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case stmt: ExpressionStatement =>
        stmt.expression match
          case lit: Literal => assertEquals(lit.value, JSValue.fromString("hello"))
          case other => fail(s"Expected Literal, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }

  test("parse boolean literals") {
    val lexer1 = Lexer("true")
    val tokens1 = lexer1.tokenize()
    val parser1 = Parser(tokens1)
    val script1 = parser1.parseScript()

    assertEquals(script1.body.length, 1)
    script1.body(0) match
      case stmt1: ExpressionStatement =>
        stmt1.expression match
          case lit1: Literal => assertEquals(lit1.value, JSValue.Bool(true))
          case other => fail(s"Expected Literal, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")

    val lexer2 = Lexer("false")
    val tokens2 = lexer2.tokenize()
    val parser2 = Parser(tokens2)
    val script2 = parser2.parseScript()

    assertEquals(script2.body.length, 1)
    script2.body(0) match
      case stmt2: ExpressionStatement =>
        stmt2.expression match
          case lit2: Literal => assertEquals(lit2.value, JSValue.Bool(false))
          case other => fail(s"Expected Literal, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }

  test("parse null and undefined") {
    val lexer1 = Lexer("null")
    val tokens1 = lexer1.tokenize()
    val parser1 = Parser(tokens1)
    val script1 = parser1.parseScript()

    assertEquals(script1.body.length, 1)
    script1.body(0) match
      case stmt1: ExpressionStatement =>
        stmt1.expression match
          case lit1: Literal => assertEquals(lit1.value, JSValue.Null)
          case other => fail(s"Expected Literal, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")

    val lexer2 = Lexer("undefined")
    val tokens2 = lexer2.tokenize()
    val parser2 = Parser(tokens2)
    val script2 = parser2.parseScript()

    assertEquals(script2.body.length, 1)
    script2.body(0) match
      case stmt2: ExpressionStatement =>
        stmt2.expression match
          case lit2: Literal => assertEquals(lit2.value, JSValue.Undefined)
          case other => fail(s"Expected Literal, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }

  test("parse parentheses expression") {
    val lexer = Lexer("(1 + 2) * 3")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case stmt: ExpressionStatement =>
        stmt.expression match
          case mul: BinaryExpression =>
            assertEquals(mul.operator, BinaryOperator.Mul)
            // Left operand should be (1 + 2)
            mul.left match
              case add: BinaryExpression => assertEquals(add.operator, BinaryOperator.Add)
              case other => fail(s"Expected BinaryExpression for add, got $other")
          case other => fail(s"Expected BinaryExpression for mul, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }

  test("parse assignment expression") {
    val lexer = Lexer("x = 10")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case stmt: ExpressionStatement =>
        stmt.expression match
          case assign: AssignmentExpression =>
            assert(assign.left != null)
            assert(assign.right != null)
          case other => fail(s"Expected AssignmentExpression, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }

  test("parse comparison operators") {
    val lexer = Lexer("x < 5 && x >= 1")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case stmt: ExpressionStatement =>
        stmt.expression match
          case logAnd: BinaryExpression => assertEquals(logAnd.operator, BinaryOperator.LogicalAnd)
          case other => fail(s"Expected BinaryExpression, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }
