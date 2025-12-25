package quickjs.lexer

import quickjs.ast.Span
import munit.*

class LexerTest extends FunSuite:

  test("tokenize number") {
    val lexer = Lexer("123")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[NumberToken])
    assert(token.asInstanceOf[NumberToken].value == 123.0)
  }

  test("tokenize floating point number") {
    val lexer = Lexer("3.14")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[NumberToken])
    assert(token.asInstanceOf[NumberToken].value == 3.14)
  }

  test("tokenize string") {
    val lexer = Lexer("\"hello\"")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[StringToken])
    assert(token.asInstanceOf[StringToken].value == "hello")
  }

  test("tokenize identifier") {
    val lexer = Lexer("foo")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[IdentifierToken])
    assert(token.asInstanceOf[IdentifierToken].name == "foo")
  }

  test("tokenize keyword - var") {
    val lexer = Lexer("var")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[KeywordToken])
    assert(token.asInstanceOf[KeywordToken].kind == Keyword.Var)
  }

  test("tokenize keyword - if") {
    val lexer = Lexer("if")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[KeywordToken])
    assert(token.asInstanceOf[KeywordToken].kind == Keyword.If)
  }

  test("tokenize operator - +") {
    val lexer = Lexer("+")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[OperatorToken])
    assert(token.asInstanceOf[OperatorToken].op == Operator.Add)
  }

  test("tokenize operator - ==") {
    val lexer = Lexer("==")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[OperatorToken])
    assert(token.asInstanceOf[OperatorToken].op == Operator.Eq)
  }

  test("tokenize operator - &&") {
    val lexer = Lexer("&&")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[OperatorToken])
    assert(token.asInstanceOf[OperatorToken].op == Operator.LogicalAnd)
  }

  test("tokenize punctuation - semicolon") {
    val lexer = Lexer(";")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[PunctuationToken])
    assert(token.asInstanceOf[PunctuationToken].punct == Punctuation.Semicolon)
  }

  test("tokenize simple expression") {
    val lexer = Lexer("var x = 1 + 2;")
    val tokens = lexer.tokenize()

    assertEquals(tokens.length, 8) // var, x, =, 1, +, 2, ;, EOF

    assert(tokens(0).isInstanceOf[KeywordToken])
    assert(tokens(0).asInstanceOf[KeywordToken].kind == Keyword.Var)

    assert(tokens(1).isInstanceOf[IdentifierToken])
    assert(tokens(1).asInstanceOf[IdentifierToken].name == "x")

    assert(tokens(2).isInstanceOf[OperatorToken])
    assert(tokens(2).asInstanceOf[OperatorToken].op == Operator.Assign)

    assert(tokens(3).isInstanceOf[NumberToken])
    assert(tokens(3).asInstanceOf[NumberToken].value == 1.0)

    assert(tokens(4).isInstanceOf[OperatorToken])
    assert(tokens(4).asInstanceOf[OperatorToken].op == Operator.Add)

    assert(tokens(5).isInstanceOf[NumberToken])
    assert(tokens(5).asInstanceOf[NumberToken].value == 2.0)

    assert(tokens(6).isInstanceOf[PunctuationToken])
    assert(tokens(6).asInstanceOf[PunctuationToken].punct == Punctuation.Semicolon)

    assert(tokens(7) == EOF)
  }

  test("tokenize with whitespace") {
    val lexer = Lexer("  42  ")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[NumberToken])
    assert(token.asInstanceOf[NumberToken].value == 42.0)
  }

  test("tokenize line comment") {
    val lexer = Lexer("// this is a comment\n42")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[NumberToken])
    assert(token.asInstanceOf[NumberToken].value == 42.0)
  }

  test("tokenize boolean literals") {
    val lexer1 = Lexer("true")
    val token1 = lexer1.nextToken()
    assert(token1.isInstanceOf[KeywordToken])
    assert(token1.asInstanceOf[KeywordToken].kind == Keyword.True)

    val lexer2 = Lexer("false")
    val token2 = lexer2.nextToken()
    assert(token2.isInstanceOf[KeywordToken])
    assert(token2.asInstanceOf[KeywordToken].kind == Keyword.False)
  }

  test("tokenize null and undefined") {
    val lexer1 = Lexer("null")
    val token1 = lexer1.nextToken()
    assert(token1.isInstanceOf[KeywordToken])
    assert(token1.asInstanceOf[KeywordToken].kind == Keyword.Null)

    val lexer2 = Lexer("undefined")
    val token2 = lexer2.nextToken()
    assert(token2.isInstanceOf[KeywordToken])
    assert(token2.asInstanceOf[KeywordToken].kind == Keyword.Undefined)
  }

  test("tokenize increment operators") {
    val lexer1 = Lexer("++")
    val token1 = lexer1.nextToken()
    assert(token1.isInstanceOf[OperatorToken])
    assert(token1.asInstanceOf[OperatorToken].op == Operator.PreInc)

    val lexer2 = Lexer("--")
    val token2 = lexer2.nextToken()
    assert(token2.isInstanceOf[OperatorToken])
    assert(token2.asInstanceOf[OperatorToken].op == Operator.PreDec)
  }

  test("tokenize comparison operators") {
    val lexer1 = Lexer("<=")
    val token1 = lexer1.nextToken()
    assert(token1.isInstanceOf[OperatorToken])
    assert(token1.asInstanceOf[OperatorToken].op == Operator.Lte)

    val lexer2 = Lexer(">=")
    val token2 = lexer2.nextToken()
    assert(token2.isInstanceOf[OperatorToken])
    assert(token2.asInstanceOf[OperatorToken].op == Operator.Gte)

    val lexer3 = Lexer("===")
    val token3 = lexer3.nextToken()
    assert(token3.isInstanceOf[OperatorToken])
    assert(token3.asInstanceOf[OperatorToken].op == Operator.StrictEq)

    val lexer4 = Lexer("!==")
    val token4 = lexer4.nextToken()
    assert(token4.isInstanceOf[OperatorToken])
    assert(token4.asInstanceOf[OperatorToken].op == Operator.StrictNeq)
  }

  test("tokenize parentheses and braces") {
    val lexer = Lexer("(){}")
    val tokens = lexer.tokenize()

    assert(tokens(0).isInstanceOf[PunctuationToken])
    assert(tokens(0).asInstanceOf[PunctuationToken].punct == Punctuation.LeftParen)

    assert(tokens(1).isInstanceOf[PunctuationToken])
    assert(tokens(1).asInstanceOf[PunctuationToken].punct == Punctuation.RightParen)

    assert(tokens(2).isInstanceOf[PunctuationToken])
    assert(tokens(2).asInstanceOf[PunctuationToken].punct == Punctuation.LeftBrace)

    assert(tokens(3).isInstanceOf[PunctuationToken])
    assert(tokens(3).asInstanceOf[PunctuationToken].punct == Punctuation.RightBrace)
  }

  test("tokenize brackets") {
    val lexer = Lexer("[]")
    val tokens = lexer.tokenize()

    assert(tokens(0).isInstanceOf[PunctuationToken])
    assert(tokens(0).asInstanceOf[PunctuationToken].punct == Punctuation.LeftBracket)

    assert(tokens(1).isInstanceOf[PunctuationToken])
    assert(tokens(1).asInstanceOf[PunctuationToken].punct == Punctuation.RightBracket)
  }

  test("tokenize comma") {
    val lexer = Lexer(",")
    val token = lexer.nextToken()
    assert(token.isInstanceOf[PunctuationToken])
    assert(token.asInstanceOf[PunctuationToken].punct == Punctuation.Comma)
  }
