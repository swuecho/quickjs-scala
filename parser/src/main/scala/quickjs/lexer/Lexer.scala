package quickjs.lexer

import scala.annotation.tailrec
import quickjs.ast.Span
import scala.collection.mutable.{ArrayBuffer, Queue}
import com.ibm.icu.lang.{UCharacter, UProperty}

/** Lexical analyzer for JavaScript.
  *
  * Tokenizes JavaScript source code into tokens for the parser. Minimal
  * implementation for Phase 2a.
  */
class Lexer(input: String) {
  private var pos = 0
  private val length = input.length
  private var line = 0
  private var column = 0
  private val pendingTokens = Queue.empty[Token]
  private var lastToken: Option[Token] = None

  /** Get the current character */
  private def ch: Char = if pos < length then input(pos) else '\u0000'

  /** Peek at the next character without advancing */
  private def peek: Char = if pos + 1 < length then input(pos + 1) else '\u0000'

  /** Peek n characters ahead */
  private def peek(n: Int): String =
    if pos + n < length then input.substring(pos, pos + n + 1) else ""

  /** Advance to next character */
  private def advance(): Unit =
    if pos < length then {
      pos += 1
      column += 1
    }

  /** Skip whitespace */
  private def skipWhitespace(): Unit =
    while Character.isWhitespace(ch) || Character.isSpaceChar(ch) do {
      if ch == '\n' then {
        line += 1
        // advance() increments the column after consuming the newline.
        column = -1
      }
      advance()
    }

  /** Read a number literal */
  private def readNumber(): Token = {
    val start = pos
    val startLine = line
    val startCol = column
    def isHexDigit(c: Char): Boolean =
      (c >= '0' && c <= '9') ||
        (c >= 'a' && c <= 'f') ||
        (c >= 'A' && c <= 'F')
    def isOctalDigit(c: Char): Boolean = c >= '0' && c <= '7'
    def isBinaryDigit(c: Char): Boolean = c == '0' || c == '1'

    /** Read digits with numeric separator (_) support. Returns (digitString,
      * trailingSeparator) where trailingSeparator is true if the last character
      * read was a separator.
      */
    def readDigits(isValidDigit: Char => Boolean): (String, Boolean) = {
      val sb = new StringBuilder()
      var lastWasSeparator = false
      while isValidDigit(ch) || ch == '_' do
        if ch == '_' then {
          if lastWasSeparator then
            throw new RuntimeException(
              "SyntaxError: Numeric separator must not be adjacent to another separator"
            )
          if sb.isEmpty then
            throw new RuntimeException(
              "SyntaxError: Numeric separator must not be at the start of a number"
            )
          lastWasSeparator = true
          advance()
        } else {
          sb.append(ch)
          lastWasSeparator = false
          advance()
        }
      (sb.toString, lastWasSeparator)
    }

    // Check for 0x/0X hex integer
    if ch == '0' && (peek == 'x' || peek == 'X') then {
      advance() // skip '0'
      advance() // skip 'x'/'X'
      val (digits, trailingSep) = readDigits(isHexDigit)
      if digits.isEmpty then
        throw new RuntimeException("SyntaxError: Invalid hex integer literal")
      val span = Span(start, pos, startLine, startCol)
      if ch == 'n' then {
        advance()
        if trailingSep then
          throw new RuntimeException(
            "SyntaxError: Numeric separator must not be adjacent to BigInt suffix"
          )
        val bigValue = new java.math.BigInteger(digits, 16)
        return BigIntToken(bigValue, span)
      } else {
        val value = new java.math.BigInteger(digits, 16).doubleValue()
        return NumberToken(value, span)
      }
    }

    // Check for 0o/0O octal integer
    if ch == '0' && (peek == 'o' || peek == 'O') then {
      advance()
      advance()
      val (digits, trailingSep) = readDigits(isOctalDigit)
      if digits.isEmpty then
        throw new RuntimeException("SyntaxError: Invalid octal integer literal")
      val span = Span(start, pos, startLine, startCol)
      if ch == 'n' then {
        advance()
        if trailingSep then
          throw new RuntimeException(
            "SyntaxError: Numeric separator must not be adjacent to BigInt suffix"
          )
        val bigValue = new java.math.BigInteger(digits, 8)
        return BigIntToken(bigValue, span)
      } else {
        val value = new java.math.BigInteger(digits, 8).doubleValue()
        return NumberToken(value, span)
      }
    }

    // Check for 0b/0B binary integer
    if ch == '0' && (peek == 'b' || peek == 'B') then {
      advance()
      advance()
      val (digits, trailingSep) = readDigits(isBinaryDigit)
      if digits.isEmpty then
        throw new RuntimeException(
          "SyntaxError: Invalid binary integer literal"
        )
      val span = Span(start, pos, startLine, startCol)
      if ch == 'n' then {
        advance()
        if trailingSep then
          throw new RuntimeException(
            "SyntaxError: Numeric separator must not be adjacent to BigInt suffix"
          )
        val bigValue = new java.math.BigInteger(digits, 2)
        return BigIntToken(bigValue, span)
      } else {
        val value = new java.math.BigInteger(digits, 2).doubleValue()
        return NumberToken(value, span)
      }
    }

    // Decimal number (or legacy octal / non-octal decimal)
    // Read integer part with numeric separator support
    val (integerPart, intTrailingSep) = readDigits(Character.isDigit)

    // Check for BigInt suffix on decimal integer
    if ch == 'n' then {
      advance()
      if intTrailingSep then
        throw new RuntimeException(
          "SyntaxError: Numeric separator must not be adjacent to BigInt suffix"
        )
      // Validate decimal BigInt literal:
      // Only "0n" or "NonZeroDigit DecimalDigits_opt n" is valid per spec.
      // "00n", "01n", "08n" etc. (legacy octal / non-octal decimal) are not valid.
      if integerPart.startsWith("0") && integerPart.length > 1 then
        throw new RuntimeException("SyntaxError: Invalid BigInt literal")
      val span = Span(start, pos, startLine, startCol)
      val bigValue =
        if integerPart.isEmpty then new java.math.BigInteger("0")
        else new java.math.BigInteger(integerPart, 10)
      return BigIntToken(bigValue, span)
    }

    // Read fractional part
    if ch == '.' then {
      advance()
      val (fracPart, _) = readDigits(Character.isDigit)
      // Note: fractional digits are optional (e.g., "1." is valid)
    }

    // Read exponent
    if ch == 'e' || ch == 'E' then {
      advance()
      if ch == '+' || ch == '-' then advance()
      val (expPart, _) = readDigits(Character.isDigit)
      if expPart.isEmpty then
        throw new RuntimeException("SyntaxError: Invalid numeric literal")
    }

    // BigInt suffix is not allowed after fraction or exponent
    if ch == 'n' then
      throw new RuntimeException("SyntaxError: Invalid BigInt literal")

    val span = Span(start, pos, startLine, startCol)
    // Strip numeric separators before converting; `input.substring` keeps the
    // underscores that `readDigits` validated.
    val raw = input.substring(start, pos)
    val cleaned = if raw.indexOf('_') >= 0 then raw.replace("_", "") else raw
    val value = cleaned.toDouble
    NumberToken(value, span)
  }

  /** Read a string literal */
  /** Read a hex digit and return its value */
  private def readHexDigit(): Int = {
    val c = ch
    if c >= '0' && c <= '9' then {
      advance()
      c - '0'
    } else if c >= 'a' && c <= 'f' then {
      advance()
      c - 'a' + 10
    } else if c >= 'A' && c <= 'F' then {
      advance()
      c - 'A' + 10
    } else -1
  }

  /** Read a Unicode escape sequence (\uHHHH or \u{H...}).
    * Assumes 'u' has already been consumed (i.e., called after the 'u' in \u).
    * Returns Some(decodedString) on success, None on failure.
    */
  private def readUnicodeEscapeString(): Option[String] =
    if ch == '{' then {
      // Code point escape \u{...}
      advance()
      var codePoint = 0
      var digits = 0
      while ch != '}' && ch != '\u0000' && digits < 8 do {
        val d = readHexDigit()
        if d >= 0 then {
          codePoint = codePoint * 16 + d
          digits += 1
        } else return None
      }
      if ch == '}' then advance()
      if digits == 0 then None
      else {
        if codePoint > 0x10ffff then codePoint = 0x10ffff
        Some(new String(Character.toChars(codePoint)))
      }
    } else {
      // \uHHHH
      val h = readHexDigit()
      if h >= 0 then {
        val h2 = readHexDigit()
        val h3 = readHexDigit()
        val h4 = readHexDigit()
        if h2 >= 0 && h3 >= 0 && h4 >= 0 then {
          val cp = h * 4096 + h2 * 256 + h3 * 16 + h4
          Some(cp.toChar.toString)
        } else None
      } else None
    }

  /** Read an escape sequence (expects to be called after '\\'). Returns the
    * character to append.
    */
  private def readEscapeSequence(): String =
    ch match {
      case 'n'  => advance(); "\n"
      case 't'  => advance(); "\t"
      case 'r'  => advance(); "\r"
      case 'b'  => advance(); "\b"
      case 'f'  => advance(); "\f"
      case 'v'  => advance(); "\u000b"
      case '"'  => advance(); "\""
      case '\'' => advance(); "'"
      case '\\' => advance(); "\\"
      case '0'  =>
        // Null character \0
        advance()
        if pos < length && ch >= '0' && ch <= '9' then
          // It's an octal escape, but \0 followed by non-octal is just null
          "\u0000"
        else "\u0000"
      case 'x' =>
        // Hex escape \xHH
        advance()
        val h1 = readHexDigit()
        val h2 = readHexDigit()
        if h1 >= 0 && h2 >= 0 then ((h1 * 16 + h2).toChar).toString
        else "x"
      case 'u' =>
        advance()
        readUnicodeEscapeString() match
          case Some(s) => s
          case None => "u"
      case '\r' =>
        // Line continuation: \ followed by newline
        advance()
        if ch == '\n' then advance()
        ""
      case '\n' =>
        advance()
        ""
      case c if c >= '1' && c <= '9' =>
        // Legacy octal escape (non-strict) - read up to 3 octal digits
        // For simplicity, just treat as literal
        advance()
        ch.toString
      case _ =>
        // Unknown escape - keep the character after backslash
        val r = ch.toString
        advance()
        r
    }

  private def readString(quote: Char): Token = {
    val start = pos
    val startLine = line
    val startCol = column
    advance() // Skip opening quote

    val sb = new StringBuilder()
    // NUL is valid source text inside a string. `ch` also uses NUL as its
    // out-of-input sentinel, so position is the authoritative EOF check.
    while pos < length && ch != quote do
      if ch == '\\' then {
        advance()
        sb.append(readEscapeSequence())
      } else {
        sb.append(ch)
        advance()
      }

    if pos >= length then
      throw new RuntimeException("SyntaxError: Unterminated string literal")
    advance() // Skip closing quote
    val span = Span(start, pos, startLine, startCol)
    StringToken(sb.toString, span)
  }

  /** ECMAScript IdentifierStart is Unicode ID_Start plus `$` and `_`. */
  private def isIdentifierStart(codePoint: Int): Boolean =
    codePoint == '_' || codePoint == '$' ||
      (codePoint >= 0 && UCharacter.hasBinaryProperty(codePoint, UProperty.ID_START))

  /** ECMAScript IdentifierPart is Unicode ID_Continue plus `$`, `_`, ZWNJ,
    * and ZWJ.
    */
  private def isIdentifierPart(codePoint: Int): Boolean =
    codePoint == '_' || codePoint == '$' || codePoint == 0x200c ||
      codePoint == 0x200d || isIdentifierStart(codePoint) ||
      (codePoint >= 0 && UCharacter.hasBinaryProperty(codePoint, UProperty.ID_CONTINUE))

  private def currentCodePoint: Int =
    if pos >= length then -1 else Character.codePointAt(input, pos)

  private def appendCurrentCodePoint(sb: StringBuilder): Unit = {
    val cp = currentCodePoint
    sb.append(new String(Character.toChars(cp)))
    advance()
    if Character.isSupplementaryCodePoint(cp) then advance()
  }

  /** Read an identifier or keyword. Handles \uXXXX and \u{XXXXX} Unicode escapes. */
  private def readIdentifier(): Token = {
    val start = pos
    val startLine = line
    val startCol = column
    val sb = new StringBuilder()
    var hadEscape = false

    // Read first character (must be letter, _, $, ZWNJ, ZWJ, or \uXXXX with valid start)
    if ch == '\\' then {
      hadEscape = true
      // Unicode escape at start of identifier
      advance() // skip \
      if ch == 'u' then {
        advance() // skip u
        readUnicodeEscapeString() match
          case Some(s) =>
            if s.isEmpty || !isIdentifierStart(s.codePointAt(0)) then
              throw new RuntimeException(
                s"Invalid identifier start: Unicode escape at line $startLine:$startCol"
              )
            sb.append(s)
          case None =>
            throw new RuntimeException(
              s"Invalid Unicode escape in identifier at line $startLine:$startCol"
            )
      } else {
        throw new RuntimeException(
          s"Unexpected character '${ch}' after backslash in identifier at line $startLine:$startCol"
        )
      }
    } else if isIdentifierStart(currentCodePoint) then {
      appendCurrentCodePoint(sb)
    }

    // Read remaining characters
    var continue = true
    while continue && pos < length do {
      if ch == '\\' then {
        hadEscape = true
        // Possible Unicode escape within identifier
        val savedPos = pos
        val savedLine = line
        val savedCol = column
        advance() // skip \
        if ch == 'u' then {
          advance() // skip u
          val savedPos2 = pos
          val savedLine2 = line
          val savedCol2 = column
          readUnicodeEscapeString() match
            case Some(s) =>
              if s.nonEmpty && isIdentifierPart(s.codePointAt(0)) then
                sb.append(s)
              else
                // Not a valid identifier part, rewind to before the \
                pos = savedPos
                line = savedLine
                column = savedCol
                continue = false
            case None =>
              // Failed to parse, rewind to before the \
              pos = savedPos
              line = savedLine
              column = savedCol
              continue = false
        } else {
          // Backslash not followed by u, rewind
          pos = savedPos
          line = savedLine
          column = savedCol
          continue = false
        }
      } else if isIdentifierPart(currentCodePoint) then {
        appendCurrentCodePoint(sb)
      } else {
        continue = false
      }
    }

    val text = sb.toString
    val span = Span(start, pos, startLine, startCol)

    // Check if it's a keyword
    text match {
      case "var"        => KeywordToken(Keyword.Var, span)
      // `let` is contextual grammar, not a ReservedWord. An escaped spelling
      // therefore remains an IdentifierName and must not start a declaration.
      case "let" if hadEscape => IdentifierToken(text, span)
      case "let"        => KeywordToken(Keyword.Let, span)
      case "const"      => KeywordToken(Keyword.Const, span)
      case "if"         => KeywordToken(Keyword.If, span)
      case "else"       => KeywordToken(Keyword.Else, span)
      case "for"        => KeywordToken(Keyword.For, span)
      case "while"      => KeywordToken(Keyword.While, span)
      case "do"         => KeywordToken(Keyword.Do, span)
      case "break"      => KeywordToken(Keyword.Break, span)
      case "continue"   => KeywordToken(Keyword.Continue, span)
      case "switch"     => KeywordToken(Keyword.Switch, span)
      case "case"       => KeywordToken(Keyword.Case, span)
      case "default"    => KeywordToken(Keyword.Default, span)
      case "return"     => KeywordToken(Keyword.Return, span)
      case "function"   => KeywordToken(Keyword.Function, span)
      case "new"        => KeywordToken(Keyword.New, span)
      case "class"      => KeywordToken(Keyword.Class, span)
      case "extends"    => KeywordToken(Keyword.Extends, span)
      case "super"      => KeywordToken(Keyword.Super, span)
      case "try"        => KeywordToken(Keyword.Try, span)
      case "catch"      => KeywordToken(Keyword.Catch, span)
      case "finally"    => KeywordToken(Keyword.Finally, span)
      case "throw"      => KeywordToken(Keyword.Throw, span)
      case "with"       => KeywordToken(Keyword.With, span)
      case "import"     => KeywordToken(Keyword.Import, span)
      case "export"     => KeywordToken(Keyword.Export, span)
      case "from"       => KeywordToken(Keyword.From, span)
      case "as"         => KeywordToken(Keyword.As, span)
      case "true"       => KeywordToken(Keyword.True, span)
      case "false"      => KeywordToken(Keyword.False, span)
      case "null"       => KeywordToken(Keyword.Null, span)
      case "undefined"  => KeywordToken(Keyword.Undefined, span)
      case "this"       => KeywordToken(Keyword.This, span)
      case "typeof"     => KeywordToken(Keyword.Typeof, span)
      case "instanceof" => KeywordToken(Keyword.Instanceof, span)
      case "in"         => KeywordToken(Keyword.In, span)
      case "delete"     => KeywordToken(Keyword.Delete, span)
      case "void"       => KeywordToken(Keyword.Void, span)
      case "yield"      => KeywordToken(Keyword.Yield, span)
      case "async"      => KeywordToken(Keyword.Async, span)
      case "await"      => KeywordToken(Keyword.Await, span)
      case _            => IdentifierToken(text, span)
    }
  }

  /** Read a private identifier (#field) */
  private def readPrivateIdentifier(): Token = {
    val start = pos
    val startLine = line
    val startCol = column

    // Skip the #
    advance()

    val name = new StringBuilder()
    def readEscaped(predicate: Int => Boolean): Boolean = {
      val savedPos = pos
      val savedColumn = column
      if ch != '\\' then false
      else {
        advance()
        if ch != 'u' then {
          pos = savedPos
          column = savedColumn
          false
        } else {
          advance()
          readUnicodeEscapeString() match {
            case Some(s) if s.nonEmpty && predicate(s.codePointAt(0)) =>
              name.append(s)
              true
            case _ =>
              pos = savedPos
              column = savedColumn
              false
          }
        }
      }
    }

    if ch == '\\' then {
      if !readEscaped(isIdentifierStart) then
        throw new RuntimeException(
          s"Invalid private identifier start at line $startLine:$startCol"
        )
    } else if isIdentifierStart(currentCodePoint) then
      appendCurrentCodePoint(name)
    else
      throw new RuntimeException(
        s"Invalid private identifier start at line $startLine:$startCol"
      )

    var continue = true
    while continue && pos < length do
      if ch == '\\' then continue = readEscaped(isIdentifierPart)
      else if isIdentifierPart(currentCodePoint) then appendCurrentCodePoint(name)
      else continue = false

    val text = name.toString
    val span = Span(start, pos, startLine, startCol)

    PrivateIdentifierToken(text, span)
  }

  /** Read an operator or punctuation */
  private def readOperatorOrPunctuation(): Token = {
    val start = pos
    val startLine = line
    val startCol = column

    // Helper to check if the next character matches
    def nextIs(c: Char): Boolean = {
      val s = peek(1)
      s.length >= 2 && s.charAt(1) == c
    }

    // Multi-character operators
    // Check for assignment operators (op=)
    if nextIs('=') && (ch == '+' || ch == '-' || ch == '*' || ch == '/' ||
        ch == '%' || ch == '&' || ch == '|' || ch == '^')
    then {
      // Save the operator character before advancing
      val opChar = ch
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return opChar match {
        case '+' => OperatorToken(Operator.AddAssign, span)
        case '-' => OperatorToken(Operator.SubAssign, span)
        case '*' => OperatorToken(Operator.MulAssign, span)
        case '/' => OperatorToken(Operator.DivAssign, span)
        case '%' => OperatorToken(Operator.ModAssign, span)
        case '&' => OperatorToken(Operator.BitwiseAndAssign, span)
        case '|' => OperatorToken(Operator.BitwiseOrAssign, span)
        case '^' => OperatorToken(Operator.XorAssign, span)
        case _   => throw new RuntimeException(s"Unexpected operator: $opChar")
      }
    }

    // Check for <<=, >>=, >>>=
    // '=' is the third character for <<= and >>=, the fourth for >>>=,
    // so nextIs('=') (which looks at the second character) must not be used.
    if ch == '<' && peek == '<' && pos + 2 < length && input(pos + 2) == '='
    then {
      advance(); advance(); advance() // consume <<=
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.LeftShiftAssign, span)
    }
    if ch == '>' && peek == '>' then {
      if pos + 3 < length && input(pos + 2) == '>' && input(
          pos + 3
        ) == '='
      then {
        advance(); advance(); advance(); advance() // consume >>>=
        val span = Span(start, pos, startLine, startCol)
        return OperatorToken(Operator.UnsignedRightShiftAssign, span)
      }
      if pos + 2 < length && input(pos + 2) == '=' then {
        advance(); advance(); advance() // consume >>=
        val span = Span(start, pos, startLine, startCol)
        return OperatorToken(Operator.RightShiftAssign, span)
      }
    }

    // Check for **=
    if ch == '*' && nextIs('*') && pos + 2 < input.length && input(
        pos + 2
      ) == '='
    then {
      advance(); advance(); advance() // consume * * =
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.PowAssign, span)
    }

    // Check for comparison operators
    if ch == '=' && nextIs('=') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      if ch == '=' then {
        advance() // ===
        return OperatorToken(Operator.StrictEq, span)
      } else return OperatorToken(Operator.Eq, span) // ==
    }

    if ch == '!' && nextIs('=') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      if ch == '=' then {
        advance() // !==
        return OperatorToken(Operator.StrictNeq, span)
      } else return OperatorToken(Operator.Neq, span) // !=
    }

    // Check for shift operators (must check >>> before >>, << before <)
    if ch == '<' && nextIs('<') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.LeftShift, span) // <<
    }

    if ch == '>' && nextIs('>') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      if ch == '>' then {
        advance() // >>>
        return OperatorToken(Operator.UnsignedRightShift, span)
      } else return OperatorToken(Operator.RightShift, span) // >>
    }

    // Check for comparison operators (after shift operators)
    if ch == '<' && nextIs('=') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.Lte, span)
    }

    if ch == '>' && nextIs('=') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.Gte, span)
    }

    // Check for exponentiation operator (**)
    if ch == '*' && nextIs('*') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.Pow, span) // **
    }

    // Check for spread operator (...)
    if ch == '.' && nextIs('.') then {
      val third = peek(2)
      if third.length >= 3 && third.charAt(2) == '.' then {
        advance(); advance(); advance()
        val span = Span(start, pos, startLine, startCol)
        return OperatorToken(Operator.Spread, span)
      }
    }

    // Check for logical assignment operators
    if ch == '&' && nextIs('&') && pos + 2 < input.length && input(pos + 2) == '=' then {
      advance(); advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.LogicalAndAssign, span)
    }

    if ch == '|' && nextIs('|') && pos + 2 < input.length && input(pos + 2) == '=' then {
      advance(); advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.LogicalOrAssign, span)
    }

    if ch == '?' && nextIs('?') && pos + 2 < input.length && input(pos + 2) == '=' then {
      advance(); advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.NullishCoalesceAssign, span)
    }

    // Check for logical operators
    if ch == '&' && nextIs('&') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.LogicalAnd, span)
    }

    if ch == '|' && nextIs('|') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.LogicalOr, span)
    }

    // Check for nullish coalescing operator (??)
    if ch == '?' && nextIs('?') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.NullishCoalesce, span)
    }

    // Check for increment/decrement
    if ch == '+' && nextIs('+') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.PreInc, span)
    }

    if ch == '-' && nextIs('-') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.PreDec, span)
    }
    // Check for arrow operator (=>)
    if ch == '=' && nextIs('>') then {
      advance(); advance()
      val span = Span(start, pos, startLine, startCol)
      return OperatorToken(Operator.Arrow, span)
    }
    // Single-character operators and punctuation
    advance()
    val c = input(start)
    val span = Span(start, pos, startLine, startCol)

    c match {
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
      case ',' =>
        OperatorToken(
          Operator.Comma,
          span
        ) // Comma is an operator (for comma expressions)
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
      case _   =>
        throw new RuntimeException(
          s"Unexpected character: '$c' at $startLine:$startCol"
        )
    }
  }

  /** Skip a line comment (// ...) */
  private def skipLineComment(): Unit =
    while ch != '\n' && ch != '\u0000' do advance()

  /** Skip a block comment (/* ... */) */
  private def skipBlockComment(): Unit = {
    val startLine = line
    val startColumn = column
    advance() // consume '*'
    while pos < length do
      if ch == '*' && peek(1).length >= 2 && peek(1).charAt(1) == '/' then {
        advance() // consume '*'
        advance() // consume '/'
        return
      } else advance()
    throw new RuntimeException(
      s"SyntaxError: Unterminated block comment at ${startLine + 1}:$startColumn"
    )
  }

  private def readQuotedLiteralRaw(quote: Char): String = {
    val sb = new StringBuilder()
    sb.append(quote)
    advance()
    while ch != '\u0000' do
      if ch == '\\' then {
        sb.append('\\')
        advance()
        if ch != '\u0000' then {
          sb.append(ch)
          advance()
        }
      } else if ch == quote then {
        sb.append(quote)
        advance()
        return sb.toString
      } else {
        sb.append(ch)
        advance()
      }
    sb.toString
  }

  private def readLineCommentRaw(): String = {
    val sb = new StringBuilder()
    sb.append('/')
    sb.append('/')
    advance()
    advance()
    while ch != '\n' && ch != '\u0000' do {
      sb.append(ch)
      advance()
    }
    sb.toString
  }

  private def readBlockCommentRaw(): String = {
    val sb = new StringBuilder()
    sb.append('/')
    sb.append('*')
    advance()
    advance()
    while ch != '\u0000' do
      if ch == '*' && peek == '/' then {
        sb.append('*')
        sb.append('/')
        advance()
        advance()
        return sb.toString
      } else {
        sb.append(ch)
        advance()
      }
    sb.toString
  }

  private def readTemplateLiteralRaw(): String = {
    val sb = new StringBuilder()
    sb.append('`')
    advance()
    while ch != '\u0000' do
      if ch == '`' then {
        sb.append('`')
        advance()
        return sb.toString
      } else if ch == '$' && peek == '{' then {
        sb.append('$')
        sb.append('{')
        advance()
        advance()
        val expr = readTemplateExpressionSource()
        sb.append(expr)
        sb.append('}')
      } else if ch == '\\' then {
        sb.append('\\')
        advance()
        if ch != '\u0000' then {
          sb.append(ch)
          advance()
        }
      } else {
        sb.append(ch)
        advance()
      }
    sb.toString
  }

  private def readTemplateExpressionSource(): String = {
    val sb = new StringBuilder()
    var depth = 1
    while ch != '\u0000' && depth > 0 do
      ch match {
        case '\'' | '"' =>
          sb.append(readQuotedLiteralRaw(ch))
        case '`' =>
          sb.append(readTemplateLiteralRaw())
        case '/' =>
          val next = peek
          if next == '/' then sb.append(readLineCommentRaw())
          else if next == '*' then sb.append(readBlockCommentRaw())
          else {
            sb.append(ch)
            advance()
          }
        case '{' =>
          depth += 1
          sb.append(ch)
          advance()
        case '}' =>
          depth -= 1
          if depth == 0 then advance()
          else {
            sb.append(ch)
            advance()
          }
        case _ =>
          sb.append(ch)
          advance()
      }
    sb.toString
  }

  private def readTemplateLiteralTokens(): Unit = {
    val start = pos
    val startLine = line
    val startCol = column
    val tagged = isTaggedTemplateStart
    advance() // consume opening `

    val cookedParts = ArrayBuffer.empty[String]
    val rawParts = ArrayBuffer.empty[String]
    val expressions = ArrayBuffer.empty[String]
    val cooked = new StringBuilder()
    val raw = new StringBuilder()

    while ch != '\u0000' do
      if ch == '`' then {
        advance()
        cookedParts += cooked.toString
        rawParts += raw.toString
        val span = Span(start, pos, startLine, startCol)
        if tagged then {
          val parts = cookedParts.zip(rawParts).map { case (c, r) =>
            TemplatePart(c, r)
          }
          pendingTokens += TemplateToken(parts.toSeq, expressions.toSeq, span)
        } else {
          pendingTokens ++= buildTemplateTokens(
            templatePartsToConcatParts(cookedParts.toSeq, expressions.toSeq),
            span
          )
        }
        return
      } else if ch == '$' && peek == '{' then {
        advance() // $
        advance() // {
        cookedParts += cooked.toString
        rawParts += raw.toString
        cooked.clear()
        raw.clear()
        val expr = readTemplateExpressionSource()
        expressions += expr
      } else if ch == '\\' then {
        raw.append('\\')
        advance()
        ch match {
          case 'n'  => cooked.append('\n')
          case 't'  => cooked.append('\t')
          case 'r'  => cooked.append('\r')
          case '`'  => cooked.append('`')
          case '$'  => cooked.append('$')
          case '\\' => cooked.append('\\')
          case _    => cooked.append(ch)
        }
        if ch != '\u0000' then raw.append(ch)
        advance()
      } else {
        cooked.append(ch)
        raw.append(ch)
        advance()
      }

    throw new RuntimeException("Unterminated template literal")
  }

  private def templatePartsToConcatParts(
      cookedParts: Seq[String],
      expressions: Seq[String]
  ): Seq[Either[String, String]] = {
    val parts = ArrayBuffer.empty[Either[String, String]]
    for i <- cookedParts.indices do {
      parts += Left(cookedParts(i))
      if i < expressions.length then parts += Right(expressions(i))
    }
    parts.toSeq
  }

  private def isTaggedTemplateStart: Boolean =
    lastToken match {
      case Some(_: IdentifierToken)                            => true
      case Some(_: PrivateIdentifierToken)                     => true
      case Some(_: StringToken)                                => true
      case Some(_: NumberToken)                                => true
      case Some(_: BigIntToken)                                => true
      case Some(KeywordToken(Keyword.This, _))                 => true
      case Some(KeywordToken(Keyword.Super, _))                => true
      case Some(PunctuationToken(Punctuation.RightParen, _))   => true
      case Some(PunctuationToken(Punctuation.RightBracket, _)) => true
      case _                                                   => false
    }

  private def isRegexpAllowed(): Boolean =
    lastToken match {
      case None                                                => true
      case Some(_: NumberToken)                                => false
      case Some(_: BigIntToken)                                => false
      case Some(_: StringToken)                                => false
      case Some(_: RegexToken)                                 => false
      case Some(_: IdentifierToken)                            => false
      case Some(KeywordToken(Keyword.True, _))                 => false
      case Some(KeywordToken(Keyword.False, _))                => false
      case Some(KeywordToken(Keyword.Null, _))                 => false
      case Some(KeywordToken(Keyword.Undefined, _))            => false
      case Some(KeywordToken(Keyword.This, _))                 => false
      case Some(PunctuationToken(Punctuation.RightParen, _))   => false
      case Some(PunctuationToken(Punctuation.RightBracket, _)) => false
      case Some(PunctuationToken(Punctuation.RightBrace, _))   => false
      case Some(OperatorToken(Operator.PreInc, _))             => false
      case Some(OperatorToken(Operator.PreDec, _))             => false
      case _                                                   => true
    }

  private def readRegExpLiteral(): Token = {
    val start = pos
    val startLine = line
    val startCol = column
    advance() // consume '/'

    val body = new StringBuilder()
    var inClass = false

    while pos < length do
      if ch == '\n' || ch == '\r' then
        throw new RuntimeException("Unexpected line terminator in regexp")
      else if ch == '/' && !inClass then {
        advance() // consume closing '/'
        val flags = new StringBuilder()
        while Character.isLetterOrDigit(ch) || ch == '_' || ch == '$' do {
          flags.append(ch)
          advance()
        }
        val span = Span(start, pos, startLine, startCol)
        return RegexToken(body.toString, flags.toString, span)
      } else if ch == '[' then {
        inClass = true
        body.append(ch)
        advance()
      } else if ch == ']' then {
        inClass = false
        body.append(ch)
        advance()
      } else if ch == '\\' then {
        body.append(ch)
        advance()
        if pos >= length then
          throw new RuntimeException("Unexpected end of regexp")
        body.append(ch)
        advance()
      } else {
        body.append(ch)
        advance()
      }

    throw new RuntimeException("Unexpected end of regexp")
  }

  private def emit(token: Token): Token = {
    lastToken = Some(token)
    token
  }

  private def buildTemplateTokens(
      parts: Seq[Either[String, String]],
      span: Span
  ): Seq[Token] = {
    val tokens = ArrayBuffer.empty[Token]
    val leftParen = PunctuationToken(Punctuation.LeftParen, span)
    val rightParen = PunctuationToken(Punctuation.RightParen, span)
    val plus = OperatorToken(Operator.Add, span)

    def addExprTokens(expr: String): Unit = {
      tokens += IdentifierToken("__templateToString", span)
      tokens += leftParen
      tokens += leftParen
      val exprTokens = Lexer(expr).tokenize().filter(_ != EOF)
      tokens ++= exprTokens
      tokens += rightParen
      tokens += rightParen
    }

    tokens += leftParen
    parts match {
      case Seq(Left(str)) =>
        tokens += StringToken(str, span)
      case _ =>
        var first = true
        parts.foreach {
          case Left(str) =>
            if !first then tokens += plus
            tokens += StringToken(str, span)
            first = false
          case Right(expr) =>
            if !first then tokens += plus
            addExprTokens(expr)
            first = false
        }
    }
    tokens += rightParen
    tokens.toSeq
  }

  /** Get the next token */
  @tailrec
  final def nextToken(): Token = {
    if pendingTokens.nonEmpty then return emit(pendingTokens.dequeue())

    skipWhitespace()

    if pos >= length then return emit(EOF)

    ch match {
      case '/' =>
        // Check for line comment
        val next = peek(1)
        if next.length >= 2 && next.charAt(1) == '/' then {
          advance(); advance()
          skipLineComment()
          nextToken() // Recursively get next token after comment
        } else if next.length >= 2 && next.charAt(1) == '*' then {
          advance()
          skipBlockComment()
          nextToken() // Recursively get next token after comment
        } else if isRegexpAllowed() then emit(readRegExpLiteral())
        else emit(readOperatorOrPunctuation())

      case '`' =>
        readTemplateLiteralTokens()
        nextToken()

      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        emit(readNumber())

      case '"' | '\'' =>
        emit(readString(ch))

      case '\\' =>
        // Check for Unicode escape starting an identifier (\uXXXX or \u{...})
        val next = peek
        if next == 'u' then
          emit(readIdentifier())
        else
          emit(readOperatorOrPunctuation())

      case '_' | '$' | // Can start with _ or $
          'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' |
          'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' |
          'w' | 'x' | 'y' | 'z' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' |
          'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' |
          'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' =>
        emit(readIdentifier())
      case _ if isIdentifierStart(currentCodePoint) =>
        emit(readIdentifier())

      case '.' if Character.isDigit(peek) =>
        // Number literal starting with . (e.g., .5)
        emit(readNumber())
      case '+' | '-' | '*' | '/' | '%' | '=' | '<' | '>' | '!' | '&' | '|' |
          '~' | '^' | ',' | ';' | ':' | '?' | '(' | ')' | '[' | ']' | '{' |
          '}' | '.' =>
        emit(readOperatorOrPunctuation())

      case '#' =>
        // Private identifier (#field)
        emit(readPrivateIdentifier())

      case _ =>
        val span = Span(pos, pos + 1, line, column)
        throw new RuntimeException(
          s"Unexpected character: '$ch' at line ${span.line}:${span.column}"
        )
    }
  }

  /** Get all tokens as a sequence */
  def tokenize(): Seq[Token] = {
    // Hashbang comment support: skip #!... at the start
    if pos == 0 && input.startsWith("#!") then {
      while pos < length && input(pos) != '\n' && input(pos) != '\r' do pos += 1
      // Skip the newline too
      if pos < length && input(pos) == '\r' then pos += 1
      if pos < length && input(pos) == '\n' then pos += 1
    }
    val tokens = scala.collection.mutable.ArrayBuffer[Token]()
    var token = nextToken()
    while token != EOF do {
      tokens += token
      token = nextToken()
    }
    tokens += token
    tokens.toSeq
  }
}

object Lexer {
  def apply(input: String): Lexer = new Lexer(input)
}
