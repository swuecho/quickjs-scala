package quickjs.lexer

import scala.annotation.tailrec
import quickjs.ast.Span

/** Lexical analyzer for JavaScript.
  *
  * Tokenizes JavaScript source code into tokens for the parser.
  * Minimal implementation for Phase 2a.
  */
class Lexer(input: String):
  private var pos = 0
  private val length = input.length
  private var line = 0
  private var column = 0

  /** Get the current character */
  private def ch: Char = if pos < length then input(pos) else '\u0000'

  /** Peek at the next character without advancing */
  private def peek: Char = if pos + 1 < length then input(pos + 1) else '\u0000'

  /** Peek n characters ahead */
  private def peek(n: Int): String =
    if pos + n < length then input.substring(pos, pos + n + 1) else ""

  /** Advance to next character */
  private def advance(): Unit =
    if pos < length then
      pos += 1
      column += 1

  /** Skip whitespace */
  private def skipWhitespace(): Unit =
    while Character.isWhitespace(ch) do
      if ch == '\n' then
        line += 1
        column = 0
      advance()

  /** Read a number literal */
  private def readNumber(): Token =
    val start = pos
    val startLine = line
    val startCol = column

    // Read integer part
    while Character.isDigit(ch) do advance()

    // Read fractional part
    if ch == '.' then
      advance()
      while Character.isDigit(ch) do advance()

    // Read exponent
    if ch == 'e' || ch == 'E' then
      advance()
      if ch == '+' || ch == '-' then advance()
      while Character.isDigit(ch) do advance()

    val span = Span(start, pos, startLine, startCol)
    val value = input.substring(start, pos).toDouble
    NumberToken(value, span)

  /** Read a string literal */
  private def readString(quote: Char): Token =
    val start = pos
    val startLine = line
    val startCol = column
    advance() // Skip opening quote

    val sb = new StringBuilder()
    while ch != quote && ch != '\u0000' do
      if ch == '\\' then
        // Handle escape sequences
        advance()
        ch match
          case 'n' => sb.append('\n')
          case 't' => sb.append('\t')
          case 'r' => sb.append('\r')
          case '"' => sb.append('"')
          case '\'' => sb.append('\'')
          case '\\' => sb.append('\\')
          case _ => // Ignore unknown escapes
      else
        sb.append(ch)
      advance()

    advance() // Skip closing quote
    val span = Span(start, pos, startLine, startCol)
    StringToken(sb.toString, span)

  /** Read an identifier or keyword */
  private def readIdentifier(): Token =
    val start = pos
    val startLine = line
    val startCol = column

    // Read first character (must be letter, _, or $)
    if ch == '_' || ch == '$' || Character.isLetter(ch) then
      advance()
      while ch == '_' || ch == '$' || Character.isLetterOrDigit(ch) do
        advance()

    val text = input.substring(start, pos)
    val span = Span(start, pos, startLine, startCol)

    // Check if it's a keyword
    text.toUpperCase match
      case "VAR" => KeywordToken(Keyword.Var, span)
      case "LET" => KeywordToken(Keyword.Let, span)
      case "CONST" => KeywordToken(Keyword.Const, span)
      case "IF" => KeywordToken(Keyword.If, span)
      case "ELSE" => KeywordToken(Keyword.Else, span)
      case "FOR" => KeywordToken(Keyword.For, span)
      case "WHILE" => KeywordToken(Keyword.While, span)
      case "BREAK" => KeywordToken(Keyword.Break, span)
      case "CONTINUE" => KeywordToken(Keyword.Continue, span)
      case "RETURN" => KeywordToken(Keyword.Return, span)
      case "FUNCTION" => KeywordToken(Keyword.Function, span)
      case "TRUE" => KeywordToken(Keyword.True, span)
      case "FALSE" => KeywordToken(Keyword.False, span)
      case "NULL" => KeywordToken(Keyword.Null, span)
      case "UNDEFINED" => KeywordToken(Keyword.Undefined, span)
      case "THIS" => KeywordToken(Keyword.This, span)
      case "TYPEOF" => KeywordToken(Keyword.Typeof, span)
      case "INSTANCEOF" => KeywordToken(Keyword.Instanceof, span)
      case "IN" => KeywordToken(Keyword.In, span)
      case _ => IdentifierToken(text, span)

  /** Read an operator or punctuation */
  private def readOperatorOrPunctuation(): Token =
    val start = pos
    val startLine = line
    val startCol = column

    // Helper to check if the next character matches
    def nextIs(c: Char): Boolean =
      val s = peek(1)
      s.length >= 2 && s.charAt(1) == c

    // Multi-character operators
    // Check for assignment operators (op=)
    if nextIs('=') && (ch == '+' || ch == '-' || ch == '*' || ch == '/' ||
                        ch == '%' || ch == '&' || ch == '|' || ch == '^') then
      // Save the operator character before advancing
      val opChar = ch
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return opChar match
        case '+' => OperatorToken(Operator.AddAssign, span)
        case '-' => OperatorToken(Operator.SubAssign, span)
        case '*' => OperatorToken(Operator.MulAssign, span)
        case '/' => OperatorToken(Operator.DivAssign, span)
        case '%' => OperatorToken(Operator.ModAssign, span)
        case '&' => OperatorToken(Operator.BitwiseAnd, span)  // &= not fully implemented
        case '|' => OperatorToken(Operator.BitwiseOr, span)  // |= not fully implemented
        case '^' => OperatorToken(Operator.Xor, span)        // ^= not fully implemented
        case _ => throw new RuntimeException(s"Unexpected operator: $opChar")

    // Check for comparison operators
    if ch == '=' && nextIs('=') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      if ch == '=' then
        advance() // ===
        return OperatorToken(Operator.StrictEq, span)
      else
        return OperatorToken(Operator.Eq, span)  // ==

    if ch == '!' && nextIs('=') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      if ch == '=' then
        advance() // !==
        return OperatorToken(Operator.StrictNeq, span)
      else
        return OperatorToken(Operator.Neq, span)  // !=

    // Check for shift operators (must check >>> before >>, << before <)
    if ch == '<' && nextIs('<') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.LeftShift, span)  // <<

    if ch == '>' && nextIs('>') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      if ch == '>' then
        advance()  // >>>
        return OperatorToken(Operator.UnsignedRightShift, span)
      else
        return OperatorToken(Operator.RightShift, span)  // >>

    // Check for comparison operators (after shift operators)
    if ch == '<' && nextIs('=') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.Lte, span)

    if ch == '>' && nextIs('=') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.Gte, span)

    // Check for exponentiation operator (**)
    if ch == '*' && nextIs('*') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.Pow, span)  // **

    // Check for logical operators
    if ch == '&' && nextIs('&') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.LogicalAnd, span)

    if ch == '|' && nextIs('|') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.LogicalOr, span)

    // Check for increment/decrement
    if ch == '+' && nextIs('+') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.PreInc, span)

    if ch == '-' && nextIs('-') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.PreDec, span)
    // Check for arrow operator (=>)
    if ch == '=' && nextIs('>') then
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.Arrow, span)
    // Single-character operators and punctuation
    advance()
    val c = input(start)
    val span = Span(start, pos, startLine, startCol)

    c match
      case '+' => OperatorToken(Operator.Add, span)
      case '-' => OperatorToken(Operator.Sub, span)
      case '*' => OperatorToken(Operator.Mul, span)
      case '/' => OperatorToken(Operator.Div, span)
      case '%' => OperatorToken(Operator.Mod, span)
      case '=' => OperatorToken(Operator.Assign, span)
      case '<' => OperatorToken(Operator.Lt, span)
      case '>' => OperatorToken(Operator.Gt, span)
      case '!' => OperatorToken(Operator.Not, span)
      case '&' => OperatorToken(Operator.BitwiseAnd, span)
      case '|' => OperatorToken(Operator.BitwiseOr, span)
      case '^' => OperatorToken(Operator.Xor, span)
      case '~' => OperatorToken(Operator.BitwiseNot, span)
      case ',' => PunctuationToken(Punctuation.Comma, span)
      case ';' => PunctuationToken(Punctuation.Semicolon, span)
      case ':' => PunctuationToken(Punctuation.Colon, span)
      case '?' => PunctuationToken(Punctuation.Question, span)
      case '(' => PunctuationToken(Punctuation.LeftParen, span)
      case ')' => PunctuationToken(Punctuation.RightParen, span)
      case '[' => PunctuationToken(Punctuation.LeftBracket, span)
      case ']' => PunctuationToken(Punctuation.RightBracket, span)
      case '{' => PunctuationToken(Punctuation.LeftBrace, span)
      case '}' => PunctuationToken(Punctuation.RightBrace, span)
      case '.' => OperatorToken(Operator.Dot, span)
      case _ => throw new RuntimeException(s"Unexpected character: '$c' at $startLine:$startCol")

  /** Skip a line comment (// ...) */
  private def skipLineComment(): Unit =
    while ch != '\n' && ch != '\u0000' do advance()

  /** Get the next token */
  @tailrec
  final def nextToken(): Token =
    skipWhitespace()

    if pos >= length then
      return EOF

    ch match
      case '/' =>
        // Check for line comment
        val next = peek(1)
        if next.length >= 2 && next.charAt(1) == '/' then
          advance(); advance()
          skipLineComment()
          nextToken()  // Recursively get next token after comment
        else
          readOperatorOrPunctuation()

      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        readNumber()

      case '"' | '\'' =>
        readString(ch)

      case '_' | '$' |  // Can start with _ or $
           'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' |
           'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' |
           'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' |
           'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' =>
        readIdentifier()

      case '+' | '-' | '*' | '/' | '%' | '=' | '<' | '>' | '!' | '&' | '|' | '~' | '^' |
           ',' | ';' | ':' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '.' =>
        readOperatorOrPunctuation()

      case _ =>
        val span = Span(pos, pos + 1, line, column)
        throw new RuntimeException(s"Unexpected character: '$ch' at line ${span.line}:${span.column}")

  /** Get all tokens as a sequence */
  def tokenize(): Seq[Token] =
    val tokens = scala.collection.mutable.ArrayBuffer[Token]()
    var token = nextToken()
    while token != EOF do
      tokens += token
      token = nextToken()
    tokens += token
    tokens.toSeq

object Lexer:
  def apply(input: String): Lexer = new Lexer(input)
