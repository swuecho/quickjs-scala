package quickjs.ast

import quickjs.value.JSValue
import scala.collection.immutable

/** Abstract Syntax Tree nodes for JavaScript expressions.
  *
  * This is a minimal AST for Phase 1 (literals and binary operations) and Phase
  * 2 (variables, functions, control flow).
  */
sealed trait AST {
  val span: Span
}

sealed trait Expression extends AST

sealed trait BindingPattern extends AST

// Literals
case class Literal(value: JSValue, span: Span) extends Expression

// Identifiers
case class Identifier(name: String, span: Span)
    extends Expression,
      BindingPattern

// Private identifier (#field)
case class PrivateIdentifier(name: String, span: Span) extends Expression

/** Preserves the syntactic distinction between `[name]` and literal `name`
  * property keys.
  */
case class ComputedPropertyName(expression: Expression, span: Span)
    extends Expression

// This expression
case class ThisExpression(span: Span) extends Expression

// Super expression
case class SuperExpression(span: Span) extends Expression

// new.target meta-property
case class NewTargetExpression(span: Span) extends Expression

/** Compiler-internal marker for the lexical class-field initializer context.
  * The parser never emits this node; synthesized class constructors use it so
  * direct eval inside nested arrows retains the field initializer restrictions.
  */
case class ClassFieldInitializerExpression(
    expression: Expression,
    span: Span
) extends Expression

// import.meta
case class ImportMetaExpression(span: Span) extends Expression

// Binary expressions
case class BinaryExpression(
    operator: BinaryOperator,
    left: Expression,
    right: Expression,
    span: Span
) extends Expression

// Assignment expressions
case class AssignmentExpression(
    left: Expression | BindingPattern,
    right: Expression,
    span: Span
) extends Expression

case class LogicalAssignmentExpression(
    operator: LogicalAssignmentOperator,
    left: Expression,
    right: Expression,
    span: Span
) extends Expression

enum LogicalAssignmentOperator {
  case And, Or, Nullish
}

enum BinaryOperator {
  case Comma // Lowest precedence: evaluates left, discards, returns right
  case Add, Sub, Mul, Div, Mod, Pow
  case Eq, Neq, StrictEq, StrictNeq
  case Lt, Lte, Gt, Gte
  case And, Or, Xor, Shl, Sar, Shr
  case LogicalAnd, LogicalOr, NullishCoalesce // ?? operator
  case In, Instanceof
}

// Unary expressions
case class UnaryExpression(
    operator: UnaryOperator,
    argument: Expression,
    prefix: Boolean = true,
    span: Span
) extends Expression

enum UnaryOperator {
  case Minus, Plus, Not, BitwiseNot
  case PreInc, PostInc, PreDec, PostDec
  case Typeof, Delete, Void
}

// Conditional (ternary) expression: condition ? trueExpr : falseExpr
case class ConditionalExpression(
    test: Expression,
    consequent: Expression,
    alternate: Expression,
    span: Span
) extends Expression

// Variable declarations
case class VariableDeclaration(
    kind: VariableKind,
    declarations: immutable.Seq[VariableDeclarator],
    span: Span
) extends Declaration

enum VariableKind {
  case Var, Let, Const
}

case class VariableDeclarator(
    id: BindingPattern,
    init: Expression | Null,
    span: Span
) extends AST

// Function expressions and declarations
case class FunctionExpression(
    id: Identifier | Null,
    params: immutable.Seq[BindingPattern],
    body: BlockStatement,
    isGenerator: Boolean = false,
    isAsync: Boolean = false,
    strict: Boolean = false,
    span: Span
) extends Expression
// Arrow function expressions (ES6+)
case class ArrowFunctionExpression(
    params: immutable.Seq[BindingPattern],
    body: Either[Expression, BlockStatement], // Concise body or block body
    isAsync: Boolean = false,
    strict: Boolean = false,
    span: Span
) extends Expression

case class FunctionDeclaration(
    id: Identifier,
    params: immutable.Seq[BindingPattern],
    body: BlockStatement,
    isGenerator: Boolean = false,
    isAsync: Boolean = false,
    strict: Boolean = false,
    span: Span
) extends Declaration

// Class declarations and expressions
case class ClassDeclaration(
    id: Identifier,
    superClass: Expression | Null,
    body: ClassBody,
    span: Span
) extends Declaration

case class ClassExpression(
    id: Identifier | Null,
    superClass: Expression | Null,
    body: ClassBody,
    span: Span
) extends Expression

case class ClassBody(
    elements: immutable.Seq[ClassElement],
    span: Span
) extends AST

sealed trait ClassElement extends AST

case class MethodDefinition(
    key: Identifier | PrivateIdentifier | String | Expression,
    params: immutable.Seq[BindingPattern],
    body: BlockStatement,
    isStatic: Boolean,
    kind: PropertyKind,
    span: Span,
    isGenerator: Boolean = false,
    isAsync: Boolean = false
) extends ClassElement

case class FieldDefinition(
    key: Identifier | PrivateIdentifier | String | Expression,
    value: Expression | Null,
    isStatic: Boolean,
    span: Span
) extends ClassElement

// Destructuring/binding patterns
case class BindingAssignment(
    target: BindingPattern,
    defaultValue: Expression,
    span: Span
) extends BindingPattern

case class ArrayPattern(
    elements: immutable.Seq[BindingPattern | Null],
    span: Span
) extends BindingPattern

case class BindingProperty(
    key: Expression | String,
    value: BindingPattern,
    span: Span
) extends AST

case class ObjectPattern(
    properties: immutable.Seq[BindingProperty],
    rest: RestElement | Null = null, // ...rest in object pattern
    span: Span
) extends BindingPattern

// Rest element for destructuring: ...identifier
case class RestElement(
    argument: BindingPattern,
    span: Span
) extends BindingPattern

// Call expressions
case class CallExpression(
    callee: Expression,
    arguments: immutable.Seq[Expression],
    span: Span,
    optional: Boolean = false // true for foo?.()
) extends Expression

case class ImportCallExpression(
    arguments: immutable.Seq[Expression],
    span: Span
) extends Expression

case class TemplateElement(cooked: String, raw: String)

case class TemplateLiteral(
    elements: immutable.Seq[TemplateElement],
    expressions: immutable.Seq[Expression],
    span: Span
) extends Expression

case class TaggedTemplateExpression(
    tag: Expression,
    template: TemplateLiteral,
    span: Span
) extends Expression

// New expressions (new Constructor())
case class NewExpression(
    callee: Expression,
    arguments: immutable.Seq[Expression],
    span: Span
) extends Expression

// Object literals
case class ObjectLiteral(
    properties: immutable.Seq[Property | SpreadElement],
    span: Span
) extends Expression

case class Property(
    key: Identifier | String |
      Expression, // Identifier, string, or computed key (expression)
    value: Expression,
    kind: PropertyKind = PropertyKind.Value,
    computed: Boolean = false, // true for computed property name [expr]
    span: Span
) extends AST

enum PropertyKind {
  case Value // {a: 1}
  case Getter // {get a() { return 1; }}
  case Setter // {set a(v) { x = v; }}
  case Method // {a() { return 1; }}
}

// Array literals
case class ArrayLiteral(
    elements: immutable.Seq[
      Expression | Null
    ], // Null represents elision (empty slot)
    span: Span
) extends Expression

case class SpreadElement(
    argument: Expression,
    span: Span
) extends Expression

// Yield expression (for generator functions)
case class YieldExpression(
    argument: Expression | Null, // null for `yield` without value
    delegate: Boolean = false, // true for `yield*`
    span: Span
) extends Expression

// Await expression (for async functions)
case class AwaitExpression(
    argument: Expression,
    span: Span
) extends Expression

// Member expression (property access)
case class MemberExpression(
    `object`: Expression,
    property: Expression, // For now, only identifier
    computed: Boolean = false,
    span: Span,
    optional: Boolean = false // true for foo?.bar or foo?.[expr]
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
    label: Identifier | Null, // Label for break/continue, null if unlabeled
    span: Span
) extends Statement

case class DoWhileStatement(
    body: Statement,
    test: Expression,
    label: Identifier | Null, // Label for break/continue, null if unlabeled
    span: Span
) extends Statement

case class ForStatement(
    init: VariableDeclaration | Expression | Null,
    test: Expression | Null,
    update: Expression | Null,
    body: Statement,
    label: Identifier | Null, // Label for break/continue, null if unlabeled
    span: Span
) extends Statement

case class ForInStatement(
    left: VariableDeclaration | Expression,
    right: Expression,
    body: Statement,
    label: Identifier | Null, // Label for break/continue, null if unlabeled
    span: Span
) extends Statement

case class ForOfStatement(
    left: VariableDeclaration | Expression,
    right: Expression,
    body: Statement,
    label: Identifier | Null, // Label for break/continue, null if unlabeled
    span: Span
) extends Statement

case class ForAwaitOfStatement(
    left: VariableDeclaration | Expression,
    right: Expression,
    body: Statement,
    label: Identifier | Null, // Label for break/continue, null if unlabeled
    span: Span
) extends Statement

case class ReturnStatement(
    argument: Expression | Null,
    span: Span
) extends Statement

case class ThrowStatement(
    argument: Expression,
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

// Switch statement
case class SwitchStatement(
    discriminant: Expression,
    cases: immutable.Seq[SwitchCase],
    span: Span
) extends Statement

case class CatchClause(
    param: BindingPattern | Null,
    body: BlockStatement,
    span: Span
) extends AST

case class TryStatement(
    block: BlockStatement,
    handler: CatchClause | Null,
    finalizer: BlockStatement | Null,
    span: Span
) extends Statement

case class WithStatement(
    obj: Expression,
    body: Statement,
    span: Span
) extends Statement

// Module declarations
sealed trait ImportSpecifier extends AST
case class ImportNamedSpecifier(
    imported: Identifier,
    local: Identifier,
    span: Span
) extends ImportSpecifier
case class ImportDefaultSpecifier(
    local: Identifier,
    span: Span
) extends ImportSpecifier
case class ImportNamespaceSpecifier(
    local: Identifier,
    span: Span
) extends ImportSpecifier

case class ImportDeclaration(
    specifiers: immutable.Seq[ImportSpecifier],
    source: String,
    span: Span
) extends Statement

case class ExportSpecifier(
    local: Identifier,
    exported: Identifier,
    span: Span
) extends AST

case class ExportNamedDeclaration(
    declaration: Statement | Null,
    specifiers: immutable.Seq[ExportSpecifier],
    source: String | Null,
    span: Span
) extends Statement

case class ExportDefaultDeclaration(
    declaration: Expression | Statement,
    span: Span
) extends Statement

case class ExportAllDeclaration(
    source: String,
    span: Span
) extends Statement

case class SwitchCase(
    test: Expression | Null, // Null for default case
    consequent: immutable.Seq[Statement],
    span: Span
) extends AST

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
    strict: Boolean = false,
    span: Span
) extends AST

sealed trait Statement extends AST
sealed trait Declaration extends Statement
