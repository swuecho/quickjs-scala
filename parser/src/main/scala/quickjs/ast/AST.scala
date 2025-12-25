package quickjs.ast

import quickjs.value.JSValue
import scala.collection.immutable

/** Abstract Syntax Tree nodes for JavaScript expressions.
  *
  * This is a minimal AST for Phase 1 (literals and binary operations).
  */
sealed trait AST:
  val span: Span

sealed trait Expression extends AST

// Literals
case class Literal(value: JSValue, span: Span) extends Expression

// Identifiers
case class Identifier(name: String, span: Span) extends Expression

// Binary expressions
case class BinaryExpression(
  operator: BinaryOperator,
  left: Expression,
  right: Expression,
  span: Span
) extends Expression

enum BinaryOperator:
  case Add, Sub, Mul, Div, Mod
  case Eq, Neq, StrictEq, StrictNeq
  case Lt, Lte, Gt, Gte
  case And, Or, Xor
  case LogicalAnd, LogicalOr

// Unary expressions
case class UnaryExpression(
  operator: UnaryOperator,
  argument: Expression,
  prefix: Boolean = true,
  span: Span
) extends Expression

enum UnaryOperator:
  case Minus, Plus, Not, BitwiseNot
  case Typeof

// Source location
case class Span(
  start: Int,
  end: Int,
  line: Int,
  column: Int
)

// Script (top-level)
case class Script(
  body: immutable.Seq[Statement],
  span: Span
) extends AST

sealed trait Statement extends AST

case class ExpressionStatement(
  expression: Expression,
  span: Span
) extends Statement
