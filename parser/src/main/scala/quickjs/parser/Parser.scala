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
  private var allowInOperator = true

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
        // Don't parse comma operator - comma in function arguments is a separator
        arguments += parseAssignmentExpressionWithoutComma()
        if isOperator(Operator.Comma) then
          advance()
        else
          more = false
    arguments.toSeq

  /** Advance to the next token */
  private def advance(): Unit =
    if pos < tokens.length then
      pos += 1

  private def withInOperatorAllowed[T](allowed: Boolean)(f: => T): T =
    val old = allowInOperator
    allowInOperator = allowed
    try f
    finally allowInOperator = old

  private def isForInAhead(): Boolean =
    var i = pos
    var depth = 0
    while i < tokens.length do
      tokens(i) match
        case PunctuationToken(Punctuation.LeftParen, _) |
             PunctuationToken(Punctuation.LeftBracket, _) |
             PunctuationToken(Punctuation.LeftBrace, _) =>
          depth += 1
        case PunctuationToken(Punctuation.RightParen, _) =>
          if depth == 0 then return false
          depth -= 1
        case PunctuationToken(Punctuation.RightBracket, _) |
             PunctuationToken(Punctuation.RightBrace, _) =>
          depth -= 1
        case PunctuationToken(Punctuation.Semicolon, _) =>
          if depth == 0 then return false
        case KeywordToken(Keyword.In, _) =>
          if depth == 0 then return true
        case _ => ()
      i += 1
    false

  /** Expect a specific token or throw an error */
  private def expectToken(token: Token): Unit =
    if !isToken(token) then
      throw new RuntimeException(s"Expected $token but got ${current}")

  /** Expect a specific keyword or throw an error */
  private def expectKeyword(keyword: Keyword): Unit =
    if !isKeyword(keyword) then
      throw new RuntimeException(s"Expected keyword $keyword but got ${current}")

  /** Expect a specific punctuation or throw an error (does NOT advance) */
  private def expectPunctuation(punct: Punctuation): Unit =
    if !isPunctuation(punct) then
      throw new RuntimeException(s"Expected punctuation $punct but got ${current}")

  /** Parse a script */
  def parseScript(): Script =
    val body = ArrayBuffer[Statement]()

    while current != EOF do
      body += parseStatement()

    val span = Span(0, 0, 0, 0)  // TODO: compute actual span
    Script(body.toSeq, span)

  /** Check if current token is a label (identifier followed by colon) */
  private def isLabel(): Boolean =
    current match
      case IdentifierToken(_, _) =>
        // Peek at next token to see if it's a colon
        peek() match
          case PunctuationToken(Punctuation.Colon, _) => true
          case _ => false
      case _ => false

  /** Parse a statement */
  private def parseStatement(): Statement =
    // Check for labeled statement
    if isLabel() then
      // Parse the label
      val labelToken = current
      advance()  // consume identifier
      expectPunctuation(Punctuation.Colon)  // check for :
      advance()  // consume :

      // Check what kind of statement follows
      val statement = current match
        case KeywordToken(Keyword.For, _) =>
          parseLabeledForStatement(labelToken)
        case KeywordToken(Keyword.While, _) =>
          parseLabeledWhileStatement(labelToken)
        case KeywordToken(Keyword.Do, _) =>
          parseLabeledDoWhileStatement(labelToken)
        case PunctuationToken(Punctuation.LeftBrace, _) =>
          // Labeled block statement: label: { ... }
          parseLabeledBlockStatement(labelToken)
        case _ =>
          // Labeled single statement (rare but valid)
          // Parse as a regular labeled statement - the label is stored but not used for control flow
          val body = parseStatement()
          // For non-loop labeled statements, we don't store the label in the AST
          // since break/continue only work with loops
          body

      statement
    else
      val result = current match
        case KeywordToken(k, _) if k == Keyword.Var || k == Keyword.Let || k == Keyword.Const =>
          parseVariableDeclaration()
        case KeywordToken(Keyword.If, _) =>
          parseIfStatement()
        case KeywordToken(Keyword.While, _) =>
          parseWhileStatement()
        case KeywordToken(Keyword.Do, _) =>
          parseDoWhileStatement()
        case KeywordToken(Keyword.Switch, _) =>
          parseSwitchStatement()
        case KeywordToken(Keyword.For, _) =>
          parseForStatement()
        case KeywordToken(Keyword.Return, _) =>
          parseReturnStatement()
        case KeywordToken(Keyword.Throw, _) =>
          parseThrowStatement()
        case KeywordToken(Keyword.Break, _) =>
          parseBreakStatement()
        case KeywordToken(Keyword.Continue, _) =>
          parseContinueStatement()
        case KeywordToken(Keyword.Try, _) =>
          parseTryStatement()
        case KeywordToken(Keyword.With, _) =>
          parseWithStatement()
        case KeywordToken(Keyword.Function, _) =>
          // Check if next token is an identifier (function declaration) or ( (function expression)
          // Function declarations require a name, function expressions can be anonymous
          peek() match
            case IdentifierToken(_, _) =>
              // function name() {} - function declaration
              parseFunctionDeclaration()
            case OperatorToken(Operator.Mul, _) =>
              peek(2) match
                case IdentifierToken(_, _) =>
                  // function *name() {} - generator function declaration
                  parseFunctionDeclaration()
                case _ =>
                  // function *() {} - anonymous generator function expression
                  val funcExpr = parseFunctionExpression()
                  ExpressionStatement(funcExpr, funcExpr.span)
            case _ =>
              // function() {} - anonymous function expression
              // Parse as expression and wrap in ExpressionStatement
              val funcExpr = parseFunctionExpression()
              ExpressionStatement(funcExpr, funcExpr.span)
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
      if isOperator(Operator.Comma) then
        advance()
      else
        more = false

    VariableDeclaration(kind, declarators.toSeq, span)

  /** Parse a variable declarator */
  private def parseVariableDeclarator(): VariableDeclarator =
    val id = parseIdentifier()
    val init = if isOperator(Operator.Assign) then
      advance()
      // Don't parse comma operator here - comma in variable declarations is a separator
      Some(parseAssignmentExpressionWithoutComma())
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
    advance()  // consume (
    val test = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    advance()  // consume )
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
    advance()  // consume (
    val test = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    advance()  // consume )
    val body = parseStatement()

    val span = startSpan
    WhileStatement(test, body, null, span)

  /** Parse a labeled while statement */
  private def parseLabeledWhileStatement(labelToken: Token): WhileStatement =
    val label = labelToken match
      case IdentifierToken(name, _) => Identifier(name, labelToken.span)
      case _ => throw new RuntimeException(s"Expected identifier as label, got $labelToken")

    expectKeyword(Keyword.While)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    advance()  // consume (
    val test = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    advance()  // consume )
    val body = parseStatement()

    val span = labelToken.span
    WhileStatement(test, body, label, span)

  /** Parse a do-while statement */
  private def parseDoWhileStatement(): DoWhileStatement =
    parseDoWhileStatementInternal(null)

  /** Parse a labeled do-while statement */
  private def parseLabeledDoWhileStatement(labelToken: Token): DoWhileStatement =
    val label = labelToken match
      case IdentifierToken(name, _) => Identifier(name, labelToken.span)
      case _ => throw new RuntimeException(s"Expected identifier as label, got $labelToken")
    parseDoWhileStatementInternal(label)

  /** Parse a labeled block statement: label: { ... } */
  private def parseLabeledBlockStatement(labelToken: Token): BlockStatement =
    // Labeled blocks are just regular blocks - the label is stored for potential break statements
    // but we don't need to store it in the AST since blocks don't use it for control flow
    parseBlockStatement()

  /** Internal method to parse do-while with optional label */
  private def parseDoWhileStatementInternal(label: Identifier | Null): DoWhileStatement =
    val startSpan = current.span
    expectKeyword(Keyword.Do)
    advance()
    val body = parseStatement()
    expectKeyword(Keyword.While)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    advance()  // consume (
    val test = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    advance()  // consume )

    val span = startSpan
    DoWhileStatement(body, test, label, span)

  /** Parse a switch statement */
  private def parseSwitchStatement(): SwitchStatement =
    val startSpan = current.span
    expectKeyword(Keyword.Switch)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    advance()  // consume (
    val discriminant = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    advance()  // consume )
    expectPunctuation(Punctuation.LeftBrace)
    advance()  // consume {

    val cases = ArrayBuffer[SwitchCase]()

    // Parse cases - use a simpler loop structure
    var caseCount = 0
    while current != EOF && !isPunctuation(Punctuation.RightBrace) do
      // Check if we have a case clause
      if isKeyword(Keyword.Case) then
        advance()  // consume 'case'
        val test = parseExpression()
        expectPunctuation(Punctuation.Colon)  // check for :
        advance()  // consume :

        // Parse statements for this case - stop at next case/default or closing brace
        val consequent = ArrayBuffer[Statement]()
        var stmtCount = 0
        while current != EOF &&
              !isPunctuation(Punctuation.RightBrace) &&
              !isKeyword(Keyword.Case) &&
              !isKeyword(Keyword.Default) do
          consequent += parseStatement()
          stmtCount += 1

        cases += SwitchCase(test, consequent.toSeq, test.span)
        caseCount += 1
      // Check if we have a default clause
      else if isKeyword(Keyword.Default) then
        advance()  // consume 'default'
        expectPunctuation(Punctuation.Colon)  // check for :
        advance()  // consume :

        // Parse statements for default case - stop at next case or closing brace
        val consequent = ArrayBuffer[Statement]()
        while current != EOF &&
              !isPunctuation(Punctuation.RightBrace) &&
              !isKeyword(Keyword.Case) do
          consequent += parseStatement()

        cases += SwitchCase(null, consequent.toSeq, startSpan)
      else
        val tok = current
        val isCase = isKeyword(Keyword.Case)
        val isDefault = isKeyword(Keyword.Default)
        val isRightBrace = isPunctuation(Punctuation.RightBrace)
        throw new RuntimeException(s"Expected 'case' or 'default' in switch statement but got ${tok} at ${tok.span} (isCase=$isCase, isDefault=$isDefault, isRightBrace=$isRightBrace, caseCount=$caseCount)")

    expectPunctuation(Punctuation.RightBrace)
    advance()  // consume }

    SwitchStatement(discriminant, cases.toSeq, startSpan)

  /** Parse a for statement */
  private def parseForStatement(): Statement =
    val startSpan = current.span
    expectKeyword(Keyword.For)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    advance()  // consume (

    // Check if init is a variable declaration
    val isVarDecl: Boolean = current match
      case KeywordToken(k, _) =>
        k == Keyword.Var || k == Keyword.Let || k == Keyword.Const
      case _ => false

    val forInAhead = isForInAhead()
    // Parse init and ensure proper typing
    val initResult =
      if isVarDecl then
        if forInAhead then
          Left(withInOperatorAllowed(false) { parseVariableDeclaration() })
        else
          Left(parseVariableDeclaration())
      else if !isPunctuation(Punctuation.Semicolon) then
        if forInAhead then
          Right(withInOperatorAllowed(false) { parseAssignmentExpressionWithoutComma() })
        else
          Right(parseExpression())
      else
        Right(null)

    val init: VariableDeclaration | Expression | Null = initResult match
      case Left(vd) => vd
      case Right(e) => e

    if isKeyword(Keyword.In) && forInAhead then
      if init == null then
        throw new RuntimeException("Expected left-hand side in for-in")
      advance()
      val right = parseExpression()
      expectPunctuation(Punctuation.RightParen)
      advance()  // consume )
      val body = parseStatement()
      val span = startSpan
      return ForInStatement(init.asInstanceOf[VariableDeclaration | Expression], right, body, null, span)

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
    advance()  // consume )
    val body = parseStatement()

    val span = startSpan
    ForStatement(init, test, update, body, null, span)

  /** Parse a labeled for statement */
  private def parseLabeledForStatement(labelToken: Token): Statement =
    val label = labelToken match
      case IdentifierToken(name, _) => Identifier(name, labelToken.span)
      case _ => throw new RuntimeException(s"Expected identifier as label, got $labelToken")

    expectKeyword(Keyword.For)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    advance()  // consume (

    // Check if init is a variable declaration
    val isVarDecl: Boolean = current match
      case KeywordToken(k, _) =>
        k == Keyword.Var || k == Keyword.Let || k == Keyword.Const
      case _ => false

    val forInAhead = isForInAhead()
    // Parse init and ensure proper typing
    val initResult =
      if isVarDecl then
        if forInAhead then
          Left(withInOperatorAllowed(false) { parseVariableDeclaration() })
        else
          Left(parseVariableDeclaration())
      else if !isPunctuation(Punctuation.Semicolon) then
        if forInAhead then
          Right(withInOperatorAllowed(false) { parseAssignmentExpressionWithoutComma() })
        else
          Right(parseExpression())
      else
        Right(null)

    val init: VariableDeclaration | Expression | Null = initResult match
      case Left(vd) => vd
      case Right(e) => e

    if isKeyword(Keyword.In) && forInAhead then
      if init == null then
        throw new RuntimeException("Expected left-hand side in for-in")
      advance()
      val right = parseExpression()
      expectPunctuation(Punctuation.RightParen)
      advance()  // consume )
      val body = parseStatement()
      val span = labelToken.span
      return ForInStatement(init.asInstanceOf[VariableDeclaration | Expression], right, body, label, span)

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
    advance()  // consume )
    val body = parseStatement()

    val span = labelToken.span
    ForStatement(init, test, update, body, label, span)

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

  private def parseThrowStatement(): ThrowStatement =
    val startSpan = current.span
    expectKeyword(Keyword.Throw)
    advance()
    val argument = parseExpression()
    val span = startSpan
    ThrowStatement(argument, span)

  private def parseTryStatement(): TryStatement =
    val startSpan = current.span
    expectKeyword(Keyword.Try)
    advance()
    val block = parseBlockStatement()
    var handler: CatchClause | Null = null
    var finalizer: BlockStatement | Null = null

    if isKeyword(Keyword.Catch) then
      advance()
      expectPunctuation(Punctuation.LeftParen)
      advance()
      val param = parseIdentifier()
      expectPunctuation(Punctuation.RightParen)
      advance()
      val body = parseBlockStatement()
      handler = CatchClause(param, body, param.span)

    if isKeyword(Keyword.Finally) then
      advance()
      finalizer = parseBlockStatement()

    if handler == null && finalizer == null then
      throw new RuntimeException("try statement must have catch or finally")

    TryStatement(block, handler, finalizer, startSpan)

  private def parseWithStatement(): WithStatement =
    val startSpan = current.span
    expectKeyword(Keyword.With)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    advance()
    val obj = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    advance()
    val body = parseStatement()
    WithStatement(obj, body, startSpan)

  /** Parse a break statement */
  private def parseBreakStatement(): BreakStatement =
    val startSpan = current.span
    expectKeyword(Keyword.Break)
    advance()
    // Optional label
    val label = current match
      case IdentifierToken(name, _) =>
        val labelIdent = Identifier(name, current.span)
        advance()  // consume the label identifier
        labelIdent
      case _ =>
        null
    val span = startSpan
    BreakStatement(label, span)

  /** Parse a continue statement */
  private def parseContinueStatement(): ContinueStatement =
    val startSpan = current.span
    expectKeyword(Keyword.Continue)
    advance()
    // Optional label
    val label = current match
      case IdentifierToken(name, _) =>
        val labelIdent = Identifier(name, current.span)
        advance()  // consume the label identifier
        labelIdent
      case _ =>
        null
    val span = startSpan
    ContinueStatement(label, span)

  /** Parse a function declaration */
  private def parseFunctionDeclaration(): FunctionDeclaration =
    val startSpan = current.span
    expectKeyword(Keyword.Function)
    advance()
    val isGenerator = isOperator(Operator.Mul)
    if isGenerator then advance()
    val id = parseIdentifier()
    val params = parseFunctionParams()

    val body = parseBlockStatement()

    val span = startSpan
    FunctionDeclaration(id, params.toSeq, body, isGenerator, false, span)

  /** Parse a function expression */
  private def parseFunctionExpression(): FunctionExpression =
    val startSpan = current.span
    expectKeyword(Keyword.Function)
    advance()
    val isGenerator = isOperator(Operator.Mul)
    if isGenerator then advance()

    // Optional identifier (anonymous functions have null id)
    val id = current match
      case IdentifierToken(_, _) => parseIdentifier()
      case _ => null

    val params = parseFunctionParams()

    val body = parseBlockStatement()

    val span = startSpan
    FunctionExpression(id, params.toSeq, body, isGenerator, false, span)

  private def parseFunctionParams(): Seq[Identifier] =
    expectPunctuation(Punctuation.LeftParen)
    advance()  // consume (
    val params = ArrayBuffer[Identifier]()
    if !isPunctuation(Punctuation.RightParen) then
      var more = true
      while more do
        current match
          case IdentifierToken(_, _) =>
            params += parseIdentifier()
          case StringToken(value, span) =>
            advance()
            params += Identifier(value, span)
          case _ =>
            throw new RuntimeException(s"Expected identifier in function parameters but got $current")
        if isOperator(Operator.Comma) then
          advance()
        else
          more = false
    expectPunctuation(Punctuation.RightParen)
    advance()  // consume )
    params.toSeq

  private def parseMethodFunction(): FunctionExpression =
    val params = parseFunctionParams()
    val body = parseBlockStatement()
    FunctionExpression(null, params, body, false, false, body.span)

  /** Parse a block statement */
  private def parseBlockStatement(): BlockStatement =
    val startSpan = current.span
    expectPunctuation(Punctuation.LeftBrace)
    advance()  // consume {
    val statements = ArrayBuffer[Statement]()
    while !isPunctuation(Punctuation.RightBrace) && current != EOF do
      statements += parseStatement()
    expectPunctuation(Punctuation.RightBrace)
    advance()  // consume }
    val span = startSpan
    BlockStatement(statements.toSeq, span)

  /** Parse an expression */
  private def parseExpression(): Expression =
    parseAssignmentExpression()

  /** Parse an assignment expression (includes ternary operator) */
  private def parseAssignmentExpression(): Expression =
    // First parse assignment (including +=, -=, etc.)
    var left = parseAssignmentExpressionWithoutComma()

    // Check for comma operator (lowest precedence, can chain)
    while isOperator(Operator.Comma) do
      advance()  // consume comma
      val right = parseAssignmentExpressionWithoutComma()
      val span = left.span
      left = BinaryExpression(BinaryOperator.Comma, left, right, span)

    left

  /** Parse assignment expression without comma operator */
  private def parseAssignmentExpressionWithoutComma(): Expression =
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
      val right = parseAssignmentExpression()  // Right side can include ternary and comma
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
      expectPunctuation(Punctuation.Colon)  // check for :
      advance()  // consume :
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
          isKeyword(Keyword.Instanceof) || (allowInOperator && isKeyword(Keyword.In)) do
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
          case KeywordToken(Keyword.Function, _) => parseFunctionExpression()
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
      case KeywordToken(Keyword.Delete, _) =>
        advance()
        val argument = parseUnaryExpression()
        val span = argument.span
        UnaryExpression(UnaryOperator.Delete, argument, true, span)
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
            // Don't parse comma operator - comma in function arguments is a separator
            arguments += parseAssignmentExpressionWithoutComma()
            if isOperator(Operator.Comma) then
              advance()
            else
              more = false
        expectPunctuation(Punctuation.RightParen)
        advance()  // consume )
        val span = left.span
        left = CallExpression(left, arguments.toSeq, span)
      // Check for member expression (dot notation)
      else if isOperator(Operator.Dot) then
        advance()
        val property = current match
          case IdentifierToken(name, span) =>
            advance()
            Identifier(name, span)
          case KeywordToken(kind, span) =>
            advance()
            Identifier(kind.toString.toLowerCase, span)
          case _ =>
            throw new RuntimeException(s"Expected identifier after '.'")
        val span = left.span
        left = MemberExpression(left, property, computed = false, span)
      // Check for member expression (bracket notation)
      else if isPunctuation(Punctuation.LeftBracket) then
        advance()
        val property = parseExpression()
        expectPunctuation(Punctuation.RightBracket)
        advance()  // consume ]
        val span = left.span
        left = MemberExpression(left, property, computed = true, span)
      else
        continue = false

    left

  /** Parse an object literal */
  private def parseObjectLiteral(): ObjectLiteral =
    val startSpan = current.span
    expectPunctuation(Punctuation.LeftBrace)
    advance()  // consume {

    val properties = ArrayBuffer[Property]()
    while !isPunctuation(Punctuation.RightBrace) && current != EOF do
      properties += parseProperty()
      if isOperator(Operator.Comma) then
        advance()

    expectPunctuation(Punctuation.RightBrace)
    advance()  // consume }

    ObjectLiteral(properties.toSeq, startSpan)

  /** Parse a property in an object literal */
  private def parseProperty(): Property =
    def parsePropertyKey(): (Identifier | String | Expression, Span) =
      current match
        case IdentifierToken(name, span) =>
          advance()
          (Identifier(name, span), span)
        case KeywordToken(kind, span) =>
          advance()
          (Identifier(kind.toString.toLowerCase, span), span)
        case StringToken(value, span) =>
          advance()
          (value, span)
        case PunctuationToken(Punctuation.LeftBracket, span) =>
          // Computed property name: [expr]
          advance()
          val keyExpr = parseAssignmentExpressionWithoutComma()  // Don't parse comma in computed property
          expectPunctuation(Punctuation.RightBracket)
          advance()
          (keyExpr, span)
        case _ =>
          throw new RuntimeException(s"Expected property key (identifier, string, or computed property) but got $current")

    def isAccessorCandidate: Boolean =
      current match
        case IdentifierToken(name, _) if name == "get" || name == "set" =>
          peek() match
            case IdentifierToken(_, _) | StringToken(_, _) =>
              peek(2) match
                case PunctuationToken(Punctuation.LeftParen, _) => true
                case _ => false
            case _ => false
        case _ => false

    if isAccessorCandidate then
      val accessorName = current.asInstanceOf[IdentifierToken].name
      advance()
      val (accessorKey, keySpan) = parsePropertyKey()
      val func = parseMethodFunction()
      val kind = if accessorName == "get" then PropertyKind.Getter else PropertyKind.Setter
      return Property(accessorKey, func, kind, keySpan)

    val (key, keySpan) = parsePropertyKey()

    if isPunctuation(Punctuation.LeftParen) then
      val func = parseMethodFunction()
      return Property(key, func, PropertyKind.Method, keySpan)

    if isPunctuation(Punctuation.Colon) then
      advance()
      val value = parseAssignmentExpressionWithoutComma()
      return Property(key, value, PropertyKind.Value, keySpan)

    key match
      case Identifier(name, span) =>
        val value = Identifier(name, span)
        Property(key, value, PropertyKind.Value, keySpan)
      case _ =>
        throw new RuntimeException(s"Expected ':' after property key")

  /** Parse an array literal */
  private def parseArrayLiteral(): ArrayLiteral =
    val startSpan = current.span
    expectPunctuation(Punctuation.LeftBracket)
    advance()  // consume [

    val elements = ArrayBuffer[Expression | Null]()
    while !isPunctuation(Punctuation.RightBracket) && current != EOF do
      // Check if there's an elision (empty element) indicated by leading comma
      if isOperator(Operator.Comma) then
        elements += null  // Elision
        advance()
      else if isPunctuation(Punctuation.RightBracket) then
        // Trailing comma - will exit loop
        ()
      else
        // Parse the element expression (don't parse comma - comma is a separator)
        elements += parseAssignmentExpressionWithoutComma()
        // Check for comma after element
        if isOperator(Operator.Comma) then
          advance()
          // Check if there's another comma (elision) or right bracket (trailing)
          if isOperator(Operator.Comma) then
            elements += null  // Elision
            advance()
          // Continue to next element

    expectPunctuation(Punctuation.RightBracket)
    advance()  // consume ]

    ArrayLiteral(elements.toSeq, startSpan)

  /** Parse a primary expression */
  private def parsePrimaryExpression(): Expression = current match
    case NumberToken(v, span) =>
      advance()
      Literal(JSValue.fromDouble(v), span)

    case BigIntToken(v, span) =>
      advance()
      Literal(JSValue.BigInt(v), span)

    case StringToken(v, span) =>
      advance()
      Literal(JSValue.fromString(v), span)

    case RegexToken(body, flags, span) =>
      advance()
      Literal(JSValue.fromString(s"/$body/$flags"), span)

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
          advance()  // consume )
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
              advance()  // consume )
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

        if isOperator(Operator.Comma) then
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
