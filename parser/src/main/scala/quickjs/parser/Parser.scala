package quickjs.parser

import quickjs.lexer.*
import quickjs.ast.*
import quickjs.value.JSValue
import scala.collection.mutable.ArrayBuffer

/** Minimal parser for JavaScript.
  *
  * Parses a sequence of tokens into an AST.
  * Supports a minimal subset for Phase 2.
  */
class Parser(tokens: Seq[Token]):
  private var pos = 0

  /** Get the current token */
  private def current: Token =
    if pos < tokens.length then tokens(pos)
    else EOF

  /** Peek at the next token without advancing */
  private def peek(offset: Int = 1): Token =
    val p = pos + offset
    if p < tokens.length then tokens(p)
    else EOF

  /** Check if the current token is of a specific type */
  private def isToken(token: Token): Boolean = current == token

  /** Check if the current token is a specific keyword */
  private def isKeyword(keyword: Keyword): Boolean = current match
    case KeywordToken(k, _) => k == keyword
    case _ => false

  /** Check if the current token is a specific operator */
  private def isOperator(op: Operator): Boolean = current match
    case OperatorToken(o, _) => o == op
    case _ => false

  /** Check if the current token is a specific punctuation */
  private def isPunctuation(punct: Punctuation): Boolean = current match
    case PunctuationToken(p, _) => p == punct
    case _ => false

  /** Parse function/method arguments (arg1, arg2, ...) */
  private def parseArguments(): Seq[Expression] =
    val arguments = scala.collection.mutable.ArrayBuffer[Expression]()
    if !isPunctuation(Punctuation.RightParen) then
      var more = true
      while more do
        arguments += parseAssignmentExpression()
        if isPunctuation(Punctuation.Comma) then
          advance()
        else
          more = false
    arguments.toSeq

  /** Advance to the next token */
  private def advance(): Unit =
    if pos < tokens.length then
      pos += 1

  /** Expect a specific token or throw an error */
  private def expectToken(token: Token): Unit =
    if !isToken(token) then
      throw new RuntimeException(s"Expected $token but got ${current}")

  /** Expect a specific keyword or throw an error */
  private def expectKeyword(keyword: Keyword): Unit =
    if !isKeyword(keyword) then
      throw new RuntimeException(s"Expected keyword $keyword but got ${current}")

  /** Expect a specific punctuation or throw an error */
  private def expectPunctuation(punct: Punctuation): Unit =
    if !isPunctuation(punct) then
      throw new RuntimeException(s"Expected punctuation $punct but got ${current}")
    else
      advance()  // Consume the punctuation token

  /** Parse a script */
  def parseScript(): Script =
    val body = ArrayBuffer[Statement]()

    while current != EOF do
      body += parseStatement()

    val span = Span(0, 0, 0, 0)  // TODO: compute actual span
    Script(body.toSeq, span)

  /** Parse a statement */
  private def parseStatement(): Statement =
    val result = current match
      case KeywordToken(k, _) if k == Keyword.Var || k == Keyword.Let || k == Keyword.Const =>
        parseVariableDeclaration()
      case KeywordToken(Keyword.If, _) =>
        parseIfStatement()
      case KeywordToken(Keyword.While, _) =>
        parseWhileStatement()
      case KeywordToken(Keyword.Do, _) =>
        parseDoWhileStatement()
      case KeywordToken(Keyword.For, _) =>
        parseForStatement()
      case KeywordToken(Keyword.Return, _) =>
        parseReturnStatement()
      case KeywordToken(Keyword.Break, _) =>
        parseBreakStatement()
      case KeywordToken(Keyword.Continue, _) =>
        parseContinueStatement()
      case KeywordToken(Keyword.Function, _) =>
        parseFunctionDeclaration()
      case PunctuationToken(Punctuation.LeftBrace, _) =>
        parseBlockStatement()
      case _ =>
        // Try to parse as expression statement
        val expr = parseExpression()
        ExpressionStatement(expr, expr.span)

    // Consume optional semicolon after statement
    if isPunctuation(Punctuation.Semicolon) then
      advance()

    result

  /** Parse a variable declaration */
  private def parseVariableDeclaration(): VariableDeclaration =
    val kindToken = current
    val kind = kindToken match
      case KeywordToken(k, _) => k match
        case Keyword.Var => VariableKind.Var
        case Keyword.Let => VariableKind.Let
        case Keyword.Const => VariableKind.Const
        case _ => throw new RuntimeException(s"Expected variable keyword")
      case _ => throw new RuntimeException(s"Expected variable keyword")

    advance()
    val declarators = ArrayBuffer[VariableDeclarator]()
    val span = kindToken.span

    var more = true
    while more do
      declarators += parseVariableDeclarator()
      if isPunctuation(Punctuation.Comma) then
        advance()
      else
        more = false

    VariableDeclaration(kind, declarators.toSeq, span)

  /** Parse a variable declarator */
  private def parseVariableDeclarator(): VariableDeclarator =
    val id = parseIdentifier()
    val init = if isOperator(Operator.Assign) then
      advance()
      Some(parseAssignmentExpression())
    else
      None

    val span = id.span
    VariableDeclarator(id, init.orNull, span)

  /** Parse an if statement */
  private def parseIfStatement(): IfStatement =
    val startSpan = current.span
    expectKeyword(Keyword.If)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()
    val test = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()
    val consequent = parseStatement()
    val alternate = if isKeyword(Keyword.Else) then
      advance()
      parseStatement()
    else
      null

    val span = startSpan
    IfStatement(test, consequent, alternate, span)

  /** Parse a while statement */
  private def parseWhileStatement(): WhileStatement =
    val startSpan = current.span
    expectKeyword(Keyword.While)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()
    val test = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()
    val body = parseStatement()

    val span = startSpan
    WhileStatement(test, body, span)

  /** Parse a do-while statement */
  private def parseDoWhileStatement(): DoWhileStatement =
    val startSpan = current.span
    expectKeyword(Keyword.Do)
    advance()
    val body = parseStatement()
    expectKeyword(Keyword.While)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()
    val test = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()

    val span = startSpan
    DoWhileStatement(body, test, span)

  /** Parse a for statement */
  private def parseForStatement(): ForStatement =
    val startSpan = current.span
    expectKeyword(Keyword.For)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()

    // Check if init is a variable declaration
    val isVarDecl: Boolean = current match
      case KeywordToken(k, _) =>
        k == Keyword.Var || k == Keyword.Let || k == Keyword.Const
      case _ => false

    // Parse init and ensure proper typing
    val initResult =
      if isVarDecl then
        Left(parseVariableDeclaration())
      else if !isPunctuation(Punctuation.Semicolon) then
        Right(parseExpression())
      else
        Right(null)

    val init: VariableDeclaration | Expression | Null = initResult match
      case Left(vd) => vd
      case Right(e) => e

    if isPunctuation(Punctuation.Semicolon) then advance()

    val test =
      if !isPunctuation(Punctuation.Semicolon) then
        parseExpression()
      else
        null

    if isPunctuation(Punctuation.Semicolon) then advance()

    val update =
      if !isPunctuation(Punctuation.RightParen) then
        parseExpression()
      else
        null

    expectPunctuation(Punctuation.RightParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()
    val body = parseStatement()

    val span = startSpan
    ForStatement(init, test, update, body, span)

  /** Parse a return statement */
  private def parseReturnStatement(): ReturnStatement =
    val startSpan = current.span
    expectKeyword(Keyword.Return)
    advance()
    val argument = if !isPunctuation(Punctuation.Semicolon) && current != EOF then
      Some(parseExpression())
    else
      None
    val span = startSpan
    ReturnStatement(argument.orNull, span)

  /** Parse a break statement */
  private def parseBreakStatement(): BreakStatement =
    val startSpan = current.span
    expectKeyword(Keyword.Break)
    advance()
    // Optional label (not implemented yet)
    val span = startSpan
    BreakStatement(null, span)

  /** Parse a continue statement */
  private def parseContinueStatement(): ContinueStatement =
    val startSpan = current.span
    expectKeyword(Keyword.Continue)
    advance()
    // Optional label (not implemented yet)
    val span = startSpan
    ContinueStatement(null, span)

  /** Parse a function declaration */
  private def parseFunctionDeclaration(): FunctionDeclaration =
    val startSpan = current.span
    expectKeyword(Keyword.Function)
    advance()
    val id = parseIdentifier()

    expectPunctuation(Punctuation.LeftParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()
    val params = ArrayBuffer[Identifier]()
    if !isPunctuation(Punctuation.RightParen) then
      var more = true
      while more do
        params += parseIdentifier()
        if isPunctuation(Punctuation.Comma) then
          advance()
        else
          more = false
    expectPunctuation(Punctuation.RightParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()

    val body = parseBlockStatement()

    val span = startSpan
    FunctionDeclaration(id, params.toSeq, body, false, false, span)

  /** Parse a function expression */
  private def parseFunctionExpression(): FunctionExpression =
    val startSpan = current.span
    expectKeyword(Keyword.Function)
    advance()

    // Optional identifier (anonymous functions have null id)
    val id = current match
      case IdentifierToken(_, _) => parseIdentifier()
      case _ => null

    expectPunctuation(Punctuation.LeftParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()
    val params = ArrayBuffer[Identifier]()
    if !isPunctuation(Punctuation.RightParen) then
      var more = true
      while more do
        params += parseIdentifier()
        if isPunctuation(Punctuation.Comma) then
          advance()
        else
          more = false
    expectPunctuation(Punctuation.RightParen)
    // NOTE: expectPunctuation already advances, so no need for extra advance()

    val body = parseBlockStatement()

    val span = startSpan
    FunctionExpression(id, params.toSeq, body, false, false, span)

  /** Parse a block statement */
  private def parseBlockStatement(): BlockStatement =
    val startSpan = current.span
    expectPunctuation(Punctuation.LeftBrace)
    // NOTE: expectPunctuation already advances, so no need for extra advance()
    val statements = ArrayBuffer[Statement]()
    while !isPunctuation(Punctuation.RightBrace) && current != EOF do
      statements += parseStatement()
    expectPunctuation(Punctuation.RightBrace)
    // NOTE: expectPunctuation already advances, so no need for extra advance()
    val span = startSpan
    BlockStatement(statements.toSeq, span)

  /** Parse an expression */
  private def parseExpression(): Expression =
    parseAssignmentExpression()

  /** Parse an assignment expression (includes ternary operator) */
  private def parseAssignmentExpression(): Expression =
    // First check for ternary operator
    val left = parseConditionalExpression()

    // Check for compound assignment operators
    val op = current match
      case OperatorToken(op, _) if op == Operator.Assign ||
                                op == Operator.AddAssign ||
                                op == Operator.SubAssign ||
                                op == Operator.MulAssign ||
                                op == Operator.DivAssign ||
                                op == Operator.ModAssign => Some(op)
      case _ => None

    if op.isDefined then
      advance()
      val right = parseAssignmentExpression()  // Right side can include ternary
      val span = left.span

      // Desugar compound assignment: x += y  ->  x = x + y
      op.get match
        case Operator.Assign =>
          AssignmentExpression(left, right, span)
        case Operator.AddAssign =>
          AssignmentExpression(left, BinaryExpression(BinaryOperator.Add, left, right, span), span)
        case Operator.SubAssign =>
          AssignmentExpression(left, BinaryExpression(BinaryOperator.Sub, left, right, span), span)
        case Operator.MulAssign =>
          AssignmentExpression(left, BinaryExpression(BinaryOperator.Mul, left, right, span), span)
        case Operator.DivAssign =>
          AssignmentExpression(left, BinaryExpression(BinaryOperator.Div, left, right, span), span)
        case Operator.ModAssign =>
          AssignmentExpression(left, BinaryExpression(BinaryOperator.Mod, left, right, span), span)
        case _ =>
          left  // Should not happen
    else
      left

  /** Parse assignment expression WITHOUT ternary (for ternary branches) */
  private def parseAssignmentExpressionNoTernary(): Expression =
    // Parse the left side (logical OR only, no ternary)
    val left = parseLogicalOrExpression()

    // Check for compound assignment operators
    val op = current match
      case OperatorToken(op, _) if op == Operator.Assign ||
                                op == Operator.AddAssign ||
                                op == Operator.SubAssign ||
                                op == Operator.MulAssign ||
                                op == Operator.DivAssign ||
                                op == Operator.ModAssign => Some(op)
      case _ => None

    if op.isDefined then
      advance()
      val right = parseAssignmentExpressionNoTernary()  // Right side also no ternary
      val span = left.span

      // Desugar compound assignment
      op.get match
        case Operator.Assign =>
          AssignmentExpression(left, right, span)
        case Operator.AddAssign =>
          AssignmentExpression(left, BinaryExpression(BinaryOperator.Add, left, right, span), span)
        case Operator.SubAssign =>
          AssignmentExpression(left, BinaryExpression(BinaryOperator.Sub, left, right, span), span)
        case Operator.MulAssign =>
          AssignmentExpression(left, BinaryExpression(BinaryOperator.Mul, left, right, span), span)
        case Operator.DivAssign =>
          AssignmentExpression(left, BinaryExpression(BinaryOperator.Div, left, right, span), span)
        case Operator.ModAssign =>
          AssignmentExpression(left, BinaryExpression(BinaryOperator.Mod, left, right, span), span)
        case _ =>
          left
    else
      left

  /** Parse conditional (ternary) expression: condition ? trueExpr : falseExpr */
  private def parseConditionalExpression(): Expression =
    // Parse the condition (logical OR and below)
    var result = parseLogicalOrExpression()

    // Check for ternary operator (right-associative)
    if isPunctuation(Punctuation.Question) then
      advance()  // consume '?'
      // Consequent can include full assignment expressions (including nested ternary)
      val consequent = parseAssignmentExpression()
      expectPunctuation(Punctuation.Colon)
      // Alternate CANNOT include ternary at this level (prevents infinite recursion)
      val alternate = parseAssignmentExpressionNoTernary()
      val span = Span(result.span.start, alternate.span.end, result.span.line, result.span.column)
      result = ConditionalExpression(result, consequent, alternate, span)

    result

  /** Parse a logical OR expression */
  private def parseLogicalOrExpression(): Expression =
    var left = parseLogicalAndExpression()
    while isOperator(Operator.LogicalOr) do
      advance()
      val right = parseLogicalAndExpression()
      val span = left.span
      left = BinaryExpression(BinaryOperator.LogicalOr, left, right, span)
    left

  /** Parse a logical AND expression */
  private def parseLogicalAndExpression(): Expression =
    var left = parseBitwiseOrExpression()
    while isOperator(Operator.LogicalAnd) do
      advance()
      val right = parseBitwiseOrExpression()
      val span = left.span
      left = BinaryExpression(BinaryOperator.LogicalAnd, left, right, span)
    left

  /** Parse a bitwise OR expression */
  private def parseBitwiseOrExpression(): Expression =
    var left = parseBitwiseXorExpression()
    while isOperator(Operator.BitwiseOr) do
      advance()
      val right = parseBitwiseXorExpression()
      val span = left.span
      left = BinaryExpression(BinaryOperator.Or, left, right, span)
    left

  /** Parse a bitwise XOR expression */
  private def parseBitwiseXorExpression(): Expression =
    var left = parseBitwiseAndExpression()
    while isOperator(Operator.Xor) do
      advance()
      val right = parseBitwiseAndExpression()
      val span = left.span
      left = BinaryExpression(BinaryOperator.Xor, left, right, span)
    left

  /** Parse a bitwise AND expression */
  private def parseBitwiseAndExpression(): Expression =
    var left = parseEqualityExpression()
    while isOperator(Operator.BitwiseAnd) do
      advance()
      val right = parseEqualityExpression()
      val span = left.span
      left = BinaryExpression(BinaryOperator.And, left, right, span)
    left

  /** Parse an equality expression */
  private def parseEqualityExpression(): Expression =
    var left = parseRelationalExpression()
    while isOperator(Operator.Eq) || isOperator(Operator.Neq) ||
          isOperator(Operator.StrictEq) || isOperator(Operator.StrictNeq) do
      val op = current match
        case OperatorToken(o, _) => o match
          case Operator.Eq => BinaryOperator.Eq
          case Operator.Neq => BinaryOperator.Neq
          case Operator.StrictEq => BinaryOperator.StrictEq
          case Operator.StrictNeq => BinaryOperator.StrictNeq
          case _ => throw new RuntimeException(s"Expected equality operator")
        case _ => throw new RuntimeException(s"Expected equality operator")
      advance()
      val right = parseRelationalExpression()
      val span = left.span
      left = BinaryExpression(op, left, right, span)
    left

  /** Parse a relational expression */
  private def parseRelationalExpression(): Expression =
    var left = parseShiftExpression()
    while isOperator(Operator.Lt) || isOperator(Operator.Lte) ||
          isOperator(Operator.Gt) || isOperator(Operator.Gte) ||
          isKeyword(Keyword.Instanceof) || isKeyword(Keyword.In) do
      val op = current match
        case OperatorToken(o, _) => o match
          case Operator.Lt => BinaryOperator.Lt
          case Operator.Lte => BinaryOperator.Lte
          case Operator.Gt => BinaryOperator.Gt
          case Operator.Gte => BinaryOperator.Gte
          case _ => throw new RuntimeException(s"Expected relational operator")
        case KeywordToken(k, _) => k match
          case Keyword.Instanceof => BinaryOperator.Instanceof
          case Keyword.In => BinaryOperator.In
          case _ => throw new RuntimeException(s"Expected relational operator")
        case _ => throw new RuntimeException(s"Expected relational operator")
      advance()
      val right = parseShiftExpression()
      val span = left.span
      left = BinaryExpression(op, left, right, span)
    left

  /** Parse an additive expression */
  private def parseAdditiveExpression(): Expression =
    var left = parseMultiplicativeExpression()
    while isOperator(Operator.Add) || isOperator(Operator.Sub) do
      val op = current match
        case OperatorToken(o, _) => o match
          case Operator.Add => BinaryOperator.Add
          case Operator.Sub => BinaryOperator.Sub
          case _ => throw new RuntimeException(s"Expected additive operator")
        case _ => throw new RuntimeException(s"Expected additive operator")
      advance()
      val right = parseMultiplicativeExpression()
      val span = left.span
      left = BinaryExpression(op, left, right, span)
    left

  /** Parse a shift expression */
  private def parseShiftExpression(): Expression =
    var left = parseAdditiveExpression()
    while isOperator(Operator.LeftShift) || isOperator(Operator.RightShift) ||
          isOperator(Operator.UnsignedRightShift) do
      val op = current match
        case OperatorToken(o, _) => o match
          case Operator.LeftShift => BinaryOperator.Shl
          case Operator.RightShift => BinaryOperator.Sar
          case Operator.UnsignedRightShift => BinaryOperator.Shr
          case _ => throw new RuntimeException(s"Expected shift operator")
        case _ => throw new RuntimeException(s"Expected shift operator")
      advance()
      val right = parseAdditiveExpression()
      val span = left.span
      left = BinaryExpression(op, left, right, span)
    left

  /** Parse a multiplicative expression */
  private def parseMultiplicativeExpression(): Expression =
    var left = parseExponentiationExpression()
    while isOperator(Operator.Mul) || isOperator(Operator.Div) || isOperator(Operator.Mod) do
      val op = current match
        case OperatorToken(o, _) => o match
          case Operator.Mul => BinaryOperator.Mul
          case Operator.Div => BinaryOperator.Div
          case Operator.Mod => BinaryOperator.Mod
          case _ => throw new RuntimeException(s"Expected multiplicative operator")
        case _ => throw new RuntimeException(s"Expected multiplicative operator")
      advance()
      val right = parseExponentiationExpression()
      val span = left.span
      left = BinaryExpression(op, left, right, span)
    left

  /** Parse an exponentiation expression (right-associative) */
  private def parseExponentiationExpression(): Expression =
    var left = parseUnaryExpression()
    if isOperator(Operator.Pow) then
      advance()
      val right = parseExponentiationExpression()  // Right-recursive for right-associativity
      val span = left.span
      left = BinaryExpression(BinaryOperator.Pow, left, right, span)
    left

  /** Parse a new expression (new Constructor()) */
  private def parseNewExpression(): Expression =
    // Check if we have 'new' keyword
    current match
      case KeywordToken(Keyword.New, span) =>
        advance()

        // Parse the constructor - should be a primary expression (not including calls)
        // For now, we'll check if the next token is an identifier or another 'new'
        val callee = current match
          case IdentifierToken(_, _) => parsePrimaryExpression()
          case KeywordToken(Keyword.New, _) => parseNewExpression()  // new new Foo()
          case _ => throw new RuntimeException(s"Expected constructor after 'new', got: $current")

        // Parse arguments for new Constructor(arg1, arg2, ...)
        val arguments = current match
          case PunctuationToken(Punctuation.LeftParen, _) =>
            advance()  // Skip '('
            val args = parseArguments()
            advance()  // Skip ')'
            args
          case _ =>
            Seq.empty  // new Foo without arguments

        NewExpression(callee, arguments, span)

      case _ =>
        // Not a new expression, parse as postfix expression
        parsePostfixExpression()

  /** Parse a unary expression */
  private def parseUnaryExpression(): Expression =
    current match
      case OperatorToken(op, _) if op == Operator.Add || op == Operator.Sub ||
                                   op == Operator.Not || op == Operator.BitwiseNot =>
        val unaryOp = op match
          case Operator.Add => UnaryOperator.Plus
          case Operator.Sub => UnaryOperator.Minus
          case Operator.Not => UnaryOperator.Not
          case Operator.BitwiseNot => UnaryOperator.BitwiseNot
          case _ => throw new RuntimeException(s"Expected unary operator")
        advance()
        val argument = parseUnaryExpression()
        val span = argument.span
        UnaryExpression(unaryOp, argument, true, span)
      case OperatorToken(op, _) if op == Operator.PreInc || op == Operator.PreDec ||
                                   op == Operator.PostInc || op == Operator.PostDec =>
        val unaryOp = op match
          case Operator.PreInc => UnaryOperator.PreInc
          case Operator.PreDec => UnaryOperator.PreDec
          case Operator.PostInc => UnaryOperator.PostInc
          case Operator.PostDec => UnaryOperator.PostDec
          case _ => throw new RuntimeException(s"Expected increment/decrement operator")
        advance()
        val argument = parseUnaryExpression()
        val span = argument.span
        UnaryExpression(unaryOp, argument, true, span)
      case KeywordToken(Keyword.Typeof, _) =>
        advance()
        val argument = parseUnaryExpression()
        val span = argument.span
        UnaryExpression(UnaryOperator.Typeof, argument, true, span)
      case _ =>
        parseNewExpression()

  /** Parse a postfix expression */
  private def parsePostfixExpression(): Expression =
    var left = parsePrimaryExpression()

    var continue = true
    while continue do
      // Check for postfix increment/decrement
      // Note: Lexer returns PreInc/PreDec for both prefix and postfix
      // We need to check for them here to handle postfix (a++, a--)
      if isOperator(Operator.PreInc) || isOperator(Operator.PreDec) ||
         isOperator(Operator.PostInc) || isOperator(Operator.PostDec) then
        val op = current match
          case OperatorToken(o, _) => o match
            case Operator.PreInc => UnaryOperator.PostInc
            case Operator.PreDec => UnaryOperator.PostDec
            case Operator.PostInc => UnaryOperator.PostInc
            case Operator.PostDec => UnaryOperator.PostDec
            case _ => throw new RuntimeException(s"Expected postfix operator")
          case _ => throw new RuntimeException(s"Expected postfix operator")
        advance()
        val span = left.span
        left = UnaryExpression(op, left, false, span)
      // Check for function call
      else if isPunctuation(Punctuation.LeftParen) then
        advance()
        val arguments = ArrayBuffer[Expression]()
        if !isPunctuation(Punctuation.RightParen) then
          var more = true
          while more do
            arguments += parseAssignmentExpression()
            if isPunctuation(Punctuation.Comma) then
              advance()
            else
              more = false
        expectPunctuation(Punctuation.RightParen)
        // NOTE: expectPunctuation already advances, so no need for extra advance()
        val span = left.span
        left = CallExpression(left, arguments.toSeq, span)
      // Check for member expression (dot notation)
      else if isOperator(Operator.Dot) then
        advance()
        val property = current match
          case IdentifierToken(name, span) =>
            advance()
            Identifier(name, span)
          case _ =>
            throw new RuntimeException(s"Expected identifier after '.'")
        val span = left.span
        left = MemberExpression(left, property, computed = false, span)
      // Check for member expression (bracket notation)
      else if isPunctuation(Punctuation.LeftBracket) then
        advance()
        val property = parseExpression()
        expectPunctuation(Punctuation.RightBracket)
        advance()
        val span = left.span
        left = MemberExpression(left, property, computed = true, span)
      else
        continue = false

    left

  /** Parse an object literal */
  private def parseObjectLiteral(): ObjectLiteral =
    val startSpan = current.span
    expectPunctuation(Punctuation.LeftBrace)
    // NOTE: expectPunctuation already advances, so no need for extra advance()

    val properties = ArrayBuffer[Property]()
    while !isPunctuation(Punctuation.RightBrace) && current != EOF do
      properties += parseProperty()
      if isPunctuation(Punctuation.Comma) then
        advance()

    expectPunctuation(Punctuation.RightBrace)
    // NOTE: expectPunctuation already advances, so no need for extra advance()

    ObjectLiteral(properties.toSeq, startSpan)

  /** Parse a property in an object literal */
  private def parseProperty(): Property =
    // Parse key (identifier, string, or computed property)
    val (key, keySpan) = current match
      case IdentifierToken(name, span) =>
        advance()
        (Identifier(name, span), span)
      case StringToken(value, span) =>
        advance()
        (value, span)
      case PunctuationToken(Punctuation.LeftBracket, span) =>
        // Computed property name: [expr]
        advance()
        val keyExpr = parseExpression()
        expectPunctuation(Punctuation.RightBracket)
        advance()
        (keyExpr, span)
      case _ =>
        throw new RuntimeException(s"Expected property key (identifier, string, or computed property) but got $current")

    // Expect colon (unless it's a method or shorthand, which we'll add later)
    if !isPunctuation(Punctuation.Colon) then
      throw new RuntimeException(s"Expected ':' after property key")

    advance()

    // Parse value
    val value = parseAssignmentExpression()

    Property(key, value, PropertyKind.Value, keySpan)

  /** Parse an array literal */
  private def parseArrayLiteral(): ArrayLiteral =
    val startSpan = current.span
    expectPunctuation(Punctuation.LeftBracket)
    // NOTE: expectPunctuation already advances, so no need for extra advance()

    val elements = ArrayBuffer[Expression | Null]()
    while !isPunctuation(Punctuation.RightBracket) && current != EOF do
      // Check if there's an elision (empty element) indicated by leading comma
      if isPunctuation(Punctuation.Comma) then
        elements += null  // Elision
        advance()
      else if isPunctuation(Punctuation.RightBracket) then
        // Trailing comma - will exit loop
        ()
      else
        // Parse the element expression
        elements += parseAssignmentExpression()
        // Check for comma after element
        if isPunctuation(Punctuation.Comma) then
          advance()
          // Check if there's another comma (elision) or right bracket (trailing)
          if isPunctuation(Punctuation.Comma) then
            elements += null  // Elision
            advance()
          // Continue to next element

    expectPunctuation(Punctuation.RightBracket)
    // NOTE: expectPunctuation already advances, so no need for extra advance()

    ArrayLiteral(elements.toSeq, startSpan)

  /** Parse a primary expression */
  private def parsePrimaryExpression(): Expression = current match
    case NumberToken(v, span) =>
      advance()
      Literal(JSValue.fromDouble(v), span)

    case StringToken(v, span) =>
      advance()
      Literal(JSValue.fromString(v), span)

    case KeywordToken(Keyword.True, span) =>
      advance()
      Literal(JSValue.Bool(true), span)

    case KeywordToken(Keyword.False, span) =>
      advance()
      Literal(JSValue.Bool(false), span)

    case KeywordToken(Keyword.Null, span) =>
      advance()
      Literal(JSValue.Null, span)

    case KeywordToken(Keyword.Undefined, span) =>
      advance()
      Literal(JSValue.Undefined, span)

    case KeywordToken(Keyword.This, span) =>
      advance()
      ThisExpression(span)

    case IdentifierToken(name, span) =>
      // Check for arrow function: x => body (single parameter without parens)
      // Peek to see if next token is =>
      val nextTok = peek()
      nextTok match
        case OperatorToken(Operator.Arrow, _) =>
          advance() // consume identifier
          advance() // consume =>
          val params = Seq(Identifier(name, span))
          val body = parseArrowFunctionBody()
          ArrowFunctionExpression(params, body, false, span)
        case _ =>
          advance()
          Identifier(name, span)

    case PunctuationToken(Punctuation.LeftParen, _) =>
      // Check for arrow function: (params) => body
      // But first check if this is (function ...) which is NOT an arrow function
      val nextTok = peek()
      nextTok match
        case KeywordToken(Keyword.Function, _) =>
          // This is (function ...), parse as grouped expression (probably an IIFE)
          advance() // consume (
          val expr = parseExpression()
          expectPunctuation(Punctuation.RightParen)
          // NOTE: expectPunctuation already advances, so no need for extra advance()
          expr
        case _ =>
          // Not (function ...), check if it's an arrow function
          val saved = pos
          advance() // consume (
          val maybeArrow = try {
            // Try to parse parameters
            val params = parseArrowFunctionParams()
            // Check for arrow: need ) followed by =>
            if isPunctuation(Punctuation.RightParen) then
              // Peek to see if next token is =>
              val nextTok = peek()
              nextTok match
                case OperatorToken(Operator.Arrow, _) =>
                  advance() // consume )
                  advance() // consume =>
                  // It's an arrow function!
                  val body = parseArrowFunctionBody()
                  ArrowFunctionExpression(params, body, false, current.span)
                case _ =>
                  null
              else
                null
            } catch {
              case _: Exception =>
                // Not an arrow function
                null
            }

            if maybeArrow != null then
              maybeArrow
            else
              // Not an arrow function, parse as regular parenthesized expression
              pos = saved
              advance()
              val expr = parseExpression()
              expectPunctuation(Punctuation.RightParen)
              // NOTE: expectPunctuation already advances, so no need for extra advance()
              expr

    case PunctuationToken(Punctuation.LeftBrace, _) =>
      parseObjectLiteral()

    case PunctuationToken(Punctuation.LeftBracket, _) =>
      parseArrayLiteral()

    case KeywordToken(Keyword.Function, _) =>
      parseFunctionExpression()

    case _ =>
      throw new RuntimeException(s"Unexpected token in expression: $current")

  /** Parse an identifier */
  private def parseIdentifier(): Identifier = current match
    case IdentifierToken(name, span) =>
      advance()
      Identifier(name, span)
    case _ =>
      throw new RuntimeException(s"Expected identifier but got $current")

  /** Parse arrow function parameters */
  private def parseArrowFunctionParams(): Seq[Identifier] =
    val params = ArrayBuffer[Identifier]()

    // Parse parameters: can be identifiers or patterns (for now, just identifiers)
    if !isPunctuation(Punctuation.RightParen) then
      var more = true
      while more do
        current match
          case IdentifierToken(name, span) =>
            params += Identifier(name, span)
            advance()
          case _ =>
            throw new RuntimeException(s"Expected identifier in arrow function parameters but got $current")

        if isPunctuation(Punctuation.Comma) then
          advance()
        else
          more = false

    params.toSeq

  /** Parse arrow function body */
  private def parseArrowFunctionBody(): Either[Expression, BlockStatement] =
    // Check if it's a block body: { ... }
    if isPunctuation(Punctuation.LeftBrace) then
      Right(parseBlockStatement())
    else
      // Concise body: just an expression
      Left(parseAssignmentExpression())

object Parser:
  def apply(tokens: Seq[Token]): Parser = new Parser(tokens)
