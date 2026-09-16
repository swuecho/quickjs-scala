package quickjs.lexer

import quickjs.ast.Span
import munit.*

class LexerTest extends FunSuite:

  test("tokenize number") {
    val lexer = Lexer("123")
    lexer.nextToken() match
      case t: NumberToken => assertEquals(t.value, 123.0)
      case other          => fail(s"Expected NumberToken, got $other")
  }

  test("tokenize floating point number") {
    val lexer = Lexer("3.14")
    lexer.nextToken() match
      case t: NumberToken => assertEquals(t.value, 3.14)
      case other          => fail(s"Expected NumberToken, got $other")
  }

  test("tokenize string") {
    val lexer = Lexer("\"hello\"")
    lexer.nextToken() match
      case t: StringToken => assertEquals(t.value, "hello")
      case other          => fail(s"Expected StringToken, got $other")
  }

  test("tokenize string containing a literal NUL") {
    val lexer = Lexer("\"\u0000\"")
    lexer.nextToken() match
      case t: StringToken => assertEquals(t.value, "\u0000")
      case other          => fail(s"Expected StringToken, got $other")
  }

  test("tokenize identifier") {
    val lexer = Lexer("foo")
    lexer.nextToken() match
      case t: IdentifierToken => assertEquals(t.name, "foo")
      case other              => fail(s"Expected IdentifierToken, got $other")
  }

  test("tokenize keyword - var") {
    val lexer = Lexer("var")
    lexer.nextToken() match
      case t: KeywordToken => assertEquals(t.kind, Keyword.Var)
      case other           => fail(s"Expected KeywordToken, got $other")
  }

  test("tokenize keyword - if") {
    val lexer = Lexer("if")
    lexer.nextToken() match
      case t: KeywordToken => assertEquals(t.kind, Keyword.If)
      case other           => fail(s"Expected KeywordToken, got $other")
  }

  test("tokenize operator - +") {
    val lexer = Lexer("+")
    lexer.nextToken() match
      case t: OperatorToken => assertEquals(t.op, Operator.Add)
      case other            => fail(s"Expected OperatorToken, got $other")
  }

  test("tokenize operator - ==") {
    val lexer = Lexer("==")
    lexer.nextToken() match
      case t: OperatorToken => assertEquals(t.op, Operator.Eq)
      case other            => fail(s"Expected OperatorToken, got $other")
  }

  test("tokenize operator - &&") {
    val lexer = Lexer("&&")
    lexer.nextToken() match
      case t: OperatorToken => assertEquals(t.op, Operator.LogicalAnd)
      case other            => fail(s"Expected OperatorToken, got $other")
  }

  test("tokenize punctuation - semicolon") {
    val lexer = Lexer(";")
    lexer.nextToken() match
      case t: PunctuationToken => assertEquals(t.punct, Punctuation.Semicolon)
      case other               => fail(s"Expected PunctuationToken, got $other")
  }

  test("tokenize simple expression") {
    val lexer = Lexer("var x = 1 + 2;")
    val tokens = lexer.tokenize()

    assertEquals(tokens.length, 8) // var, x, =, 1, +, 2, ;, EOF

    tokens(0) match
      case t: KeywordToken => assertEquals(t.kind, Keyword.Var)
      case other           => fail(s"Expected KeywordToken, got $other")

    tokens(1) match
      case t: IdentifierToken => assertEquals(t.name, "x")
      case other              => fail(s"Expected IdentifierToken, got $other")

    tokens(2) match
      case t: OperatorToken => assertEquals(t.op, Operator.Assign)
      case other            => fail(s"Expected OperatorToken, got $other")

    tokens(3) match
      case t: NumberToken => assertEquals(t.value, 1.0)
      case other          => fail(s"Expected NumberToken, got $other")

    tokens(4) match
      case t: OperatorToken => assertEquals(t.op, Operator.Add)
      case other            => fail(s"Expected OperatorToken, got $other")

    tokens(5) match
      case t: NumberToken => assertEquals(t.value, 2.0)
      case other          => fail(s"Expected NumberToken, got $other")

    tokens(6) match
      case t: PunctuationToken => assertEquals(t.punct, Punctuation.Semicolon)
      case other               => fail(s"Expected PunctuationToken, got $other")

    assertEquals(tokens(7), EOF)
  }

  test("tokenize with whitespace") {
    val lexer = Lexer("  42  ")
    lexer.nextToken() match
      case t: NumberToken => assertEquals(t.value, 42.0)
      case other          => fail(s"Expected NumberToken, got $other")
  }

  test("tokenize line comment") {
    val lexer = Lexer("// this is a comment\n42")
    lexer.nextToken() match
      case t: NumberToken => assertEquals(t.value, 42.0)
      case other          => fail(s"Expected NumberToken, got $other")
  }

  test("tokenize boolean literals") {
    val lexer1 = Lexer("true")
    lexer1.nextToken() match
      case t: KeywordToken => assertEquals(t.kind, Keyword.True)
      case other           => fail(s"Expected KeywordToken, got $other")

    val lexer2 = Lexer("false")
    lexer2.nextToken() match
      case t: KeywordToken => assertEquals(t.kind, Keyword.False)
      case other           => fail(s"Expected KeywordToken, got $other")
  }

  test("tokenize null and undefined") {
    val lexer1 = Lexer("null")
    lexer1.nextToken() match
      case t: KeywordToken => assertEquals(t.kind, Keyword.Null)
      case other           => fail(s"Expected KeywordToken, got $other")

    val lexer2 = Lexer("undefined")
    lexer2.nextToken() match
      case t: KeywordToken => assertEquals(t.kind, Keyword.Undefined)
      case other           => fail(s"Expected KeywordToken, got $other")
  }

  test("tokenize increment operators") {
    val lexer1 = Lexer("++")
    lexer1.nextToken() match
      case t: OperatorToken => assertEquals(t.op, Operator.PreInc)
      case other            => fail(s"Expected OperatorToken, got $other")

    val lexer2 = Lexer("--")
    lexer2.nextToken() match
      case t: OperatorToken => assertEquals(t.op, Operator.PreDec)
      case other            => fail(s"Expected OperatorToken, got $other")
  }

  test("tokenize comparison operators") {
    val lexer1 = Lexer("<=")
    lexer1.nextToken() match
      case t: OperatorToken => assertEquals(t.op, Operator.Lte)
      case other            => fail(s"Expected OperatorToken, got $other")

    val lexer2 = Lexer(">=")
    lexer2.nextToken() match
      case t: OperatorToken => assertEquals(t.op, Operator.Gte)
      case other            => fail(s"Expected OperatorToken, got $other")

    val lexer3 = Lexer("===")
    lexer3.nextToken() match
      case t: OperatorToken => assertEquals(t.op, Operator.StrictEq)
      case other            => fail(s"Expected OperatorToken, got $other")

    val lexer4 = Lexer("!==")
    lexer4.nextToken() match
      case t: OperatorToken => assertEquals(t.op, Operator.StrictNeq)
      case other            => fail(s"Expected OperatorToken, got $other")
  }

  test("tokenize parentheses and braces") {
    val lexer = Lexer("(){}")
    val tokens = lexer.tokenize()

    tokens(0) match
      case t: PunctuationToken => assertEquals(t.punct, Punctuation.LeftParen)
      case other               => fail(s"Expected PunctuationToken, got $other")

    tokens(1) match
      case t: PunctuationToken => assertEquals(t.punct, Punctuation.RightParen)
      case other               => fail(s"Expected PunctuationToken, got $other")

    tokens(2) match
      case t: PunctuationToken => assertEquals(t.punct, Punctuation.LeftBrace)
      case other               => fail(s"Expected PunctuationToken, got $other")

    tokens(3) match
      case t: PunctuationToken => assertEquals(t.punct, Punctuation.RightBrace)
      case other               => fail(s"Expected PunctuationToken, got $other")
  }

  test("tokenize brackets") {
    val lexer = Lexer("[]")
    val tokens = lexer.tokenize()

    tokens(0) match
      case t: PunctuationToken => assertEquals(t.punct, Punctuation.LeftBracket)
      case other               => fail(s"Expected PunctuationToken, got $other")

    tokens(1) match
      case t: PunctuationToken =>
        assertEquals(t.punct, Punctuation.RightBracket)
      case other => fail(s"Expected PunctuationToken, got $other")
  }

  test("tokenize comma") {
    val lexer = Lexer(",")
    lexer.nextToken() match
      case t: OperatorToken => assertEquals(t.op, Operator.Comma)
      case other            => fail(s"Expected OperatorToken, got $other")
  }

  test("escaped let remains an identifier") {
    Lexer("l" + "\\u0065" + "t").nextToken() match
      case IdentifierToken(name, _, _) => assertEquals(name, "let")
      case other                    => fail(s"Expected IdentifierToken, got $other")
  }

  test("block comments advance line and column tracking") {
    val tokens = Lexer("/* one\ntwo\nthree */\nconst x = 1;").tokenize()
    tokens.head match
      case KeywordToken(_, span) =>
        // Lexer line numbers are 0-based internally.
        assertEquals(span.line, 3)
        assertEquals(span.column, 0)
      case other => fail(s"Expected KeywordToken, got $other")
  }

  test("line comments advance line tracking") {
    val tokens = Lexer("// one\nconst x = 1;").tokenize()
    tokens.head match
      case KeywordToken(_, span) =>
        assertEquals(span.line, 1)
        assertEquals(span.column, 0)
      case other => fail(s"Expected KeywordToken, got $other")
  }
