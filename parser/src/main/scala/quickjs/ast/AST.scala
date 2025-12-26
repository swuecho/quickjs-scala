package quickjs.ast

import quickjs.value.JSValue
import scala.collection.immutable

/** Abstract Syntax Tree nodes for JavaScript expressions.
  *
  * This is a minimal AST for Phase 1 (literals and binary operations)
  * and Phase 2 (variables, functions, control flow).
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

// Assignment expressions
case class AssignmentExpression(
  left: Expression,
  right: Expression,
  span: Span
) extends Expression

enum BinaryOperator:
  case Add, Sub, Mul, Div, Mod, Pow
  case Eq, Neq, StrictEq, StrictNeq
  case Lt, Lte, Gt, Gte
  case And, Or, Xor, Shl, Sar, Shr
  case LogicalAnd, LogicalOr
  case In, Instanceof

// Unary expressions
case class UnaryExpression(
  operator: UnaryOperator,
  argument: Expression,
  prefix: Boolean = true,
  span: Span
) extends Expression

enum UnaryOperator:
  case Minus, Plus, Not, BitwiseNot
  case PreInc, PostInc, PreDec, PostDec
  case Typeof, Delete

// Variable declarations
case class VariableDeclaration(
  kind: VariableKind,
  declarations: immutable.Seq[VariableDeclarator],
  span: Span
) extends Declaration

enum VariableKind:
  case Var, Let, Const

case class VariableDeclarator(
  id: Identifier,
  init: Expression | Null,
  span: Span
) extends AST

// Function expressions and declarations
case class FunctionExpression(
  id: Identifier | Null,
  params: immutable.Seq[Identifier],
  body: BlockStatement,
  isGenerator: Boolean = false,
  isAsync: Boolean = false,
  span: Span
) extends Expression
// Arrow function expressions (ES6+)
case class ArrowFunctionExpression(
  params: immutable.Seq[Identifier],
  body: Either[Expression, BlockStatement],  // Concise body or block body
  isAsync: Boolean = false,
  span: Span
) extends Expression


case class FunctionDeclaration(
  id: Identifier,
  params: immutable.Seq[Identifier],
  body: BlockStatement,
  isGenerator: Boolean = false,
  isAsync: Boolean = false,
  span: Span
) extends Declaration

// Call expressions
case class CallExpression(
  callee: Expression,
  arguments: immutable.Seq[Expression],
  span: Span
) extends Expression

// Object literals
case class ObjectLiteral(
  properties: immutable.Seq[Property],
  span: Span
) extends Expression

case class Property(
  key: Identifier | String,  // Identifier or computed key (string for now)
  value: Expression,
  kind: PropertyKind = PropertyKind.Value,
  span: Span
) extends AST

enum PropertyKind:
  case Value  // {a: 1}
  case Getter // {get a() { return 1; }}
  case Setter // {set a(v) { x = v; }}
  case Method // {a() { return 1; }}

// Array literals
case class ArrayLiteral(
  elements: immutable.Seq[Expression],
  span: Span
) extends Expression

// Member expression (property access)
case class MemberExpression(
  `object`: Expression,
  property: Expression,  // For now, only identifier
  computed: Boolean = false,
  span: Span
) extends Expression

// Control flow statements
case class BlockStatement(
  statements: immutable.Seq[Statement],
  span: Span
) extends Statement

case class IfStatement(
  test: Expression,
  consequent: Statement,
  alternate: Statement | Null,
  span: Span
) extends Statement

case class WhileStatement(
  test: Expression,
  body: Statement,
  span: Span
) extends Statement

case class ForStatement(
  init: VariableDeclaration | Expression | Null,
  test: Expression | Null,
  update: Expression | Null,
  body: Statement,
  span: Span
) extends Statement

case class ReturnStatement(
  argument: Expression | Null,
  span: Span
) extends Statement

case class BreakStatement(
  label: Identifier | Null,
  span: Span
) extends Statement

case class ContinueStatement(
  label: Identifier | Null,
  span: Span
) extends Statement

// Statements
case class ExpressionStatement(
  expression: Expression,
  span: Span
) extends Statement

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
sealed trait Declaration extends Statement
