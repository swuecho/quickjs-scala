package quickjs.parser

import quickjs.lexer.*
import quickjs.ast.*
import quickjs.value.JSValue
import munit.*

class ParserTest extends FunSuite:

  private def parse(source: String): Script =
    Parser(Lexer(source).tokenize()).parseScript()

  test("parse Unicode identifiers from ID_Start and Other_ID_Start") {
    parse("var ℘ = 1; var " + "\\" + "u212E = 2;")
    parse("var 𐐀 = 1; var " + "\\" + "u{10401} = 2;")
  }

  test("parse direct and escaped Unicode private identifiers") {
    parse("class Direct { #𐐀; }")
    parse("class Escaped { #" + "\\" + "u{10401}; }")
  }

  test("reserved words cannot be variable binding identifiers") {
    intercept[RuntimeException](parse("var case = 1;"))
    intercept[RuntimeException](parse("var enum = 1;"))
    intercept[RuntimeException](parse("var " + "\\" + "u{63}ase = 1;"))
    intercept[RuntimeException](parse("var c" + "\\" + "u0061se = 1;"))
  }

  test("strict reserved words are rejected only in strict bindings") {
    parse("var yield = 1; var implements = 2;")
    intercept[RuntimeException](parse("'use strict'; var yield = 1;"))
    intercept[RuntimeException](parse("'use strict'; var implements = 1;"))
  }

  test("contextual words remain valid binding identifiers") {
    parse("var async = 1, from = 2, as = 3, undefined = 4;")
  }

  test("class bodies enforce constructor and reserved element names") {
    intercept[RuntimeException](
      parse("class C { constructor() {} constructor() {} }")
    )
    intercept[RuntimeException](parse("class C { async constructor() {} }"))
    intercept[RuntimeException](parse("class C { *constructor() {} }"))
    intercept[RuntimeException](parse("class C { get constructor() {} }"))
    intercept[RuntimeException](parse("class C { constructor; }"))
    intercept[RuntimeException](parse("class C { #constructor; }"))
    intercept[RuntimeException](parse("class C { static prototype() {} }"))
    intercept[RuntimeException](parse("class C { static prototype; }"))
  }

  test("class bodies enforce private-name uniqueness") {
    intercept[RuntimeException](parse("class C { #x; #x; }"))
    intercept[RuntimeException](parse("class C { #x() {} static #x() {} }"))
    parse("class C { get #x() {} set #x(value) {} }")
  }

  test("same-line class fields require explicit separators") {
    intercept[RuntimeException](parse("class C { x y }"))
    parse("class C { x; y }")
    parse("class C { x\n y }")
  }

  test("class methods use strict and async binding contexts") {
    intercept[RuntimeException](parse("class C { m() { var yield; } }"))
    intercept[RuntimeException](
      parse("class C { async m(await) {} }")
    )
    intercept[RuntimeException](
      parse("class C { async m() { var await; } }")
    )
    parse("class C { m() { var await; } }")
  }

  test("class field initializers reject lexical arguments and super calls") {
    intercept[RuntimeException](parse("class C { x = arguments; }"))
    intercept[RuntimeException](parse("class C { x = () => arguments; }"))
    intercept[RuntimeException](parse("class C { x = super(); }"))
    intercept[RuntimeException](parse("class C { x = () => super(); }"))
  }

  test("ordinary functions delimit class field initializer checks") {
    parse("class C { x = function() { return arguments; }; }")
  }

  test("super calls are restricted to derived constructors") {
    intercept[RuntimeException](parse("class C { constructor() { super(); } }"))
    intercept[RuntimeException](parse("class C extends B { m() { super(); } }"))
    intercept[RuntimeException](
      parse("class C extends B { static m() { super(); } }")
    )
    intercept[RuntimeException](
      parse("class C extends B { constructor() { function f() { super(); } } }")
    )
    parse("class C extends B { constructor() { super(); } }")
    parse("class C extends B { constructor() { (() => super())(); } }")
  }

  test("private references resolve against lexical class environments") {
    intercept[RuntimeException](
      parse("class C { method() { return this.#missing; } }")
    )
    intercept[RuntimeException](parse("class C extends obj.#missing {}"))
    parse("class C { method() { return this.#x; } #x; }")
    parse("class Outer { method() { class Inner { m(o) { return o.#x; } } } #x; }")
  }

  test("function and arrow parameter lists accept a trailing comma") {
    parse("function f(a = 1,) {}\nconst g = (a = 1,) => a;")
  }

  test("commas after assignment defaults delimit formal parameters") {
    val script = parse("function f(a = x += 1, b = y += 1, c) {}")
    val function = script.body.head.asInstanceOf[FunctionDeclaration]
    assertEquals(function.params.length, 3)
  }

  test("parse generator and async generator object methods") {
    val script = parse("({ *gen() { yield 1; }, async *stream() { yield 2; } });")
    val obj = script.body.head
      .asInstanceOf[ExpressionStatement]
      .expression
      .asInstanceOf[ObjectLiteral]
    val methods = obj.properties.collect { case p: Property => p }
    assertEquals(methods.length, 2, clues(methods))
    val generator = methods.find {
      case Property(Identifier("gen", _), _, _, _, _) => true
      case _ => false
    }.get.value.asInstanceOf[FunctionExpression]
    val asyncGenerator = methods.find {
      case Property(Identifier("stream", _), _, _, _, _) => true
      case _ => false
    }.get.value.asInstanceOf[FunctionExpression]
    assert(generator.isGenerator)
    assert(!generator.isAsync)
    assert(asyncGenerator.isGenerator)
    assert(asyncGenerator.isAsync)
  }

  test("parse instance and static generator class methods") {
    val script = parse(
      "class C { *gen() { yield 1; } static *ids() { yield 2; } async *stream() { yield 3; } }"
    )
    val cls = script.body.head.asInstanceOf[ClassDeclaration]
    val methods = cls.body.elements.collect { case m: MethodDefinition => m }
    assert(methods(0).isGenerator)
    assert(!methods(0).isStatic)
    assert(methods(1).isGenerator)
    assert(methods(1).isStatic)
    assert(methods(2).isGenerator)
    assert(methods(2).isAsync)
  }

  test("yield delegation forbids a preceding line terminator") {
    parse("function* g() { yield* [1]; }")
    intercept[RuntimeException](parse("function* g() { yield\n* [1]; }"))
  }

  test("yield operand does not consume an object literal separator") {
    parse("function* g() { yield { ...yield, y: 1, ...yield yield, }; }")
    parse(
      "async function* g() { yield { ...yield, y: 1, ...yield yield, }; }"
    )
  }

  test("new.target is parsed only in function contexts and is not assignable") {
    parse("function F() { return new.target; }")
    parse("function F() { return () => new.target; }")
    intercept[RuntimeException](parse("new.target;"))
    intercept[RuntimeException](parse("() => new.target;"))
    intercept[RuntimeException](parse("function F() { new.target = 1; }"))
    intercept[RuntimeException](parse("function F() { ++new.target; }"))
    intercept[RuntimeException](parse("function F() { new.target++; }"))
  }

  test("new consumes member and parenthesized constructor expressions") {
    val memberExpr = parse("new holder.C();").body.head
      .asInstanceOf[ExpressionStatement]
      .expression
      .asInstanceOf[NewExpression]
    assert(memberExpr.callee.isInstanceOf[MemberExpression])

    val computedExpr = parse("new holder[key]();").body.head
      .asInstanceOf[ExpressionStatement]
      .expression
      .asInstanceOf[NewExpression]
    assert(computedExpr.callee.asInstanceOf[MemberExpression].computed)

    val parenthesized = parse("new (factory())();").body.head
      .asInstanceOf[ExpressionStatement]
      .expression
      .asInstanceOf[NewExpression]
    assert(parenthesized.callee.isInstanceOf[CallExpression])
  }

  test("eval parser context enforces class field super and arguments rules") {
    intercept[RuntimeException](parse("super.x"))
    parse("class C { x = super.x; m() { return super.x; } }")
    parse("({ m() { return super.x; } })")
    intercept[RuntimeException](parse("function f() { return super.x; }"))

    def parseFieldEval(source: String): Script =
      new Parser(
        tokens = Lexer(source).tokenize(),
        allowNewTargetAtTopLevel = true,
        classFieldInitializerAtTopLevel = true,
        allowSuperPropertyAtTopLevel = true
      ).parseScript()

    parseFieldEval("super.x")
    intercept[RuntimeException](parseFieldEval("super()"))
    intercept[RuntimeException](parseFieldEval("arguments"))
    intercept[RuntimeException](parseFieldEval("function f() { super.x; }"))
  }

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
          case other        => fail(s"Expected Literal, got $other")
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
          case bin: BinaryExpression =>
            assertEquals(bin.operator, BinaryOperator.Add)
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
      case other                => fail(s"Expected ReturnStatement, got $other")
  }

  test("parse block statement") {
    val lexer = Lexer("{ var x = 1; x + 2; }")
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assertEquals(script.body.length, 1)
    script.body(0) match
      case block: BlockStatement => assertEquals(block.statements.length, 2)
      case other                 => fail(s"Expected BlockStatement, got $other")
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
          case unary: UnaryExpression =>
            assertEquals(unary.operator, UnaryOperator.Minus)
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
          case unary: UnaryExpression =>
            assertEquals(unary.operator, UnaryOperator.PreInc)
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
          case lit: Literal =>
            assertEquals(lit.value, JSValue.fromString("hello"))
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
          case other         => fail(s"Expected Literal, got $other")
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
          case other         => fail(s"Expected Literal, got $other")
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
          case other         => fail(s"Expected Literal, got $other")
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
          case other         => fail(s"Expected Literal, got $other")
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
              case add: BinaryExpression =>
                assertEquals(add.operator, BinaryOperator.Add)
              case other =>
                fail(s"Expected BinaryExpression for add, got $other")
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
          case logAnd: BinaryExpression =>
            assertEquals(logAnd.operator, BinaryOperator.LogicalAnd)
          case other => fail(s"Expected BinaryExpression, got $other")
      case other => fail(s"Expected ExpressionStatement, got $other")
  }
