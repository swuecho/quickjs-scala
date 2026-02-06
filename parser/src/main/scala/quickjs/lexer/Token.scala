package quickjs.lexer

import quickjs.ast.Span
import java.math.BigInteger

/** Tokens produced by the JavaScript lexer.
  *
  * Covers a minimal subset of JavaScript for Phase 2a:
  * - Whitespace (skipped)
  * - Number literals
  * - String literals
  * - Identifiers and keywords
  * - Basic operators
  * - Punctuation
  */
sealed trait Token:
  def span: Span

// Literals
final case class NumberToken(value: Double, span: Span) extends Token
final case class StringToken(value: String, span: Span) extends Token
final case class RegexToken(body: String, flags: String, span: Span) extends Token
final case class BigIntToken(value: BigInteger, span: Span) extends Token

// Identifiers and keywords
final case class IdentifierToken(name: String, span: Span) extends Token

// Keywords
final case class KeywordToken(kind: Keyword, span: Span) extends Token

enum Keyword:
  case Var, Let, Const
  case If, Else
  case For, While, Do, Break, Continue
  case Switch, Case, Default
  case Return, Function, New
  case Class, Extends, Super
  case Try, Catch, Finally, Throw
  case With
  case Import, Export, From, As
  case True, False, Null, Undefined
  case This, Typeof, Instanceof, In, Delete, Void, Yield

// Operators
final case class OperatorToken(op: Operator, span: Span) extends Token

enum Operator:
  // Arithmetic
  case Add, Sub, Mul, Div, Mod, Pow  // Pow = ** (exponentiation)
  // Increment/decrement
  case PreInc, PostInc, PreDec, PostDec
  // Comparison
  case Eq, Neq, StrictEq, StrictNeq, Lt, Lte, Gt, Gte
  // Logical
  case LogicalAnd, LogicalOr, Not
  // Nullish coalescing
  case NullishCoalesce  // ??
  // Bitwise
  case BitwiseAnd, BitwiseOr, BitwiseNot, Xor
  // Shift
  case LeftShift, RightShift, UnsignedRightShift  // <<, >>, >>>
  // Assignment
  case Assign
  // Compound assignment
  case AddAssign, SubAssign, MulAssign, DivAssign, ModAssign
  // Relational
  case Instanceof, In
  // Other
  case Dot, Arrow, Spread, Comma  // Comma = comma operator (eval left, discard, return right)

// Punctuation
final case class PunctuationToken(punct: Punctuation, span: Span) extends Token

enum Punctuation:
  case Comma, Semicolon, Colon, Question
  case LeftParen, RightParen
  case LeftBracket, RightBracket
  case LeftBrace, RightBrace

// End of input
case object EOF extends Token:
  def span: Span = Span(0, 0, 0, 0)

object Token:
  def show(token: Token): String = token match
    case NumberToken(v, _) => s"$v"
    case StringToken(v, _) => s"\"$v\""
    case RegexToken(body, flags, _) => s"/$body/$flags"
    case BigIntToken(v, _) => s"${v.toString}n"
    case IdentifierToken(n, _) => n
    case KeywordToken(k, _) => k.toString.toLowerCase
    case OperatorToken(o, _) => o.toString
    case PunctuationToken(p, _) => p.toString
    case EOF => "<EOF>"
