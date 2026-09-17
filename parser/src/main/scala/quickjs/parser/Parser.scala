package quickjs.parser

import quickjs.lexer.*
import quickjs.ast.*
import quickjs.value.JSValue
import scala.collection.mutable.ArrayBuffer

/** Minimal parser for JavaScript.
  *
  * Parses a sequence of tokens into an AST. Supports a minimal subset for Phase
  * 2.
  */
class Parser(
    tokens: Seq[Token],
    allowNewTargetAtTopLevel: Boolean = false,
    classFieldInitializerAtTopLevel: Boolean = false,
    allowSuperPropertyAtTopLevel: Boolean = false,
    allowedPrivateNamesAtTopLevel: Set[String] = Set.empty,
    allowTopLevelReturn: Boolean = false,
    // Module code is always strict and reserves `await` as an identifier.
    moduleMode: Boolean = false,
    // Strict code that is not module code (e.g. direct eval inside a strict
    // function): reserved words are rejected but `await` stays an identifier.
    strictMode: Boolean = false
) {
  private var pos = 0
  private var allowInOperator = true
  // Track current strict mode (inherited from enclosing context)
  private var currentStrictMode: Boolean = false
  private var generatorFunctionDepth: Int = 0
  private var asyncFunctionDepth: Int = 0
  private var functionDepth: Int = 0
  // Import/export declarations are ModuleItems, not Statements: they are
  // legal only at the top level of module code. The flag is set by
  // `parseScript` before each top-level statement and cleared as soon as a
  // statement starts, so nested positions (blocks, function bodies,
  // control-flow headers, class methods) reject them.
  private var moduleItemAllowed: Boolean = false
  // Labels of the labeled statements currently being parsed in this function.
  // Used for the early errors on duplicate labels and on `break`/`continue`
  // referencing a label that does not exist.
  private val labelStack = scala.collection.mutable.ArrayBuffer.empty[String]

  /** `await` is an AwaitExpression in async functions and at the top level of
    * module code. The Await capability does not propagate into the body or
    * parameters of a non-async nested function, where `await` is an ordinary
    * IdentifierReference.
    */
  private def awaitExpressionAllowed: Boolean =
    asyncFunctionDepth > 0 || (moduleMode && functionDepth == 0)
  private var iterationDepth: Int = 0
  private var switchDepth: Int = 0
  private var newTargetContextDepth: Int =
    if allowNewTargetAtTopLevel then 1 else 0
  private var classFieldInitializerDepth: Int =
    if classFieldInitializerAtTopLevel then 1 else 0
  private var fieldInitializerFunctionBoundaryDepth: Int = 0
  private var currentClassHasHeritage: Boolean = false
  private var superCallAllowedDepth: Int = 0
  private var superCallNestedFunctionDepth: Int = 0
  private var superPropertyContextDepth: Int =
    if allowSuperPropertyAtTopLevel then 1 else 0
  private var superPropertyNestedFunctionDepth: Int = 0
  private final case class PrivateEnvironment(
      declared: scala.collection.mutable.Set[String] =
        scala.collection.mutable.HashSet.empty,
      referenced: scala.collection.mutable.Set[String] =
        scala.collection.mutable.HashSet.empty
  )
  private var privateEnvironmentStack: List[PrivateEnvironment] = Nil

  private def referencePrivateName(name: String): Unit =
    privateEnvironmentStack match {
      case env :: _ => env.referenced += name
      case Nil if allowedPrivateNamesAtTopLevel.contains(name) => ()
      case Nil =>
        throw new RuntimeException(
          s"Private name #$name is not declared in an enclosing class"
        )
    }

  /** Get the current token */
  private def current: Token =
    if pos < tokens.length then tokens(pos)
    else EOF

  /** Peek at the next token without advancing */
  private def peek(offset: Int = 1): Token = {
    val p = pos + offset
    if p < tokens.length then tokens(p)
    else EOF
  }

  /** Check if a line terminator occurred before the current token (ASI helper).
    * Returns true if previous token is on a different line than current token.
    */
  private def wasLineTerminatorBefore: Boolean =
    if pos > 0 && pos < tokens.length then
      current.span.line != tokens(pos - 1).span.line
    else false

  /** True when the property key beginning at token index `keyPos` is followed
    * by `(`, i.e. it starts a method definition. Computed keys `[...]` are
    * scanned to their matching `]`, so `async [name] () {}` is recognised.
    */
  private def keyFollowedByParen(keyPos: Int): Boolean = {
    def isParen(i: Int): Boolean =
      i < tokens.length && (tokens(i) match {
        case PunctuationToken(Punctuation.LeftParen, _) => true
        case _                                          => false
      })
    if keyPos >= tokens.length then false
    else
      tokens(keyPos) match {
        case PunctuationToken(Punctuation.LeftBracket, _) =>
          var index = keyPos + 1
          var depth = 1
          while index < tokens.length && depth > 0 do {
            tokens(index) match {
              case PunctuationToken(Punctuation.LeftBracket, _)  => depth += 1
              case PunctuationToken(Punctuation.RightBracket, _) => depth -= 1
              case _                                             => ()
            }
            index += 1
          }
          depth == 0 && isParen(index)
        case _ => isParen(keyPos + 1)
      }
  }

  /** Check if the current token is of a specific type */
  private def isToken(token: Token): Boolean = current == token

  /** Check if the current token is a specific keyword */
  private def isKeyword(keyword: Keyword): Boolean = current match {
    case KeywordToken(k, _) => k == keyword
    case _                  => false
  }

  /** Check if the current token is a specific operator */
  private def isOperator(op: Operator): Boolean = current match {
    case OperatorToken(o, _) => o == op
    case _                   => false
  }

  /** Check if the current token is a specific punctuation */
  private def isPunctuation(punct: Punctuation): Boolean = current match {
    case PunctuationToken(p, _) => p == punct
    case _                      => false
  }

  /** Check if the current token is a specific identifier */
  private def isIdentifier(name: String): Boolean = current match {
    case IdentifierToken(n, _, _) => n == name
    case _                     => false
  }

  /** Parse function/method arguments (arg1, arg2, ...) */
  private def parseArguments(): Seq[Expression] = {
    val arguments = scala.collection.mutable.ArrayBuffer[Expression]()
    if !isPunctuation(Punctuation.RightParen) then {
      var more = true
      while more do {
        if isOperator(Operator.Spread) then {
          val spreadSpan = current.span
          advance()
          arguments += SpreadElement(
            parseAssignmentExpressionWithoutComma(),
            spreadSpan
          )
        } else {
          // Don't parse comma operator - comma in function arguments is a separator
          arguments += parseAssignmentExpressionWithoutComma()
        }
        if isOperator(Operator.Comma) then {
          advance()
          // Trailing comma is allowed: if next token is ), end the argument list
          if isPunctuation(Punctuation.RightParen) then more = false
        } else more = false
      }
    }
    arguments.toSeq
  }

  /** Advance to the next token */
  private def advance(): Unit =
    if pos < tokens.length then pos += 1

  private def withInOperatorAllowed[T](allowed: Boolean)(f: => T): T = {
    val old = allowInOperator
    allowInOperator = allowed
    try f
    finally allowInOperator = old
  }

  private def withFunctionGrammarContext[T](
      isGenerator: Boolean,
      isAsync: Boolean,
      isArrow: Boolean = false,
      isMethodRoot: Boolean = false
  )(f: => T): T = {
    functionDepth += 1
    if isGenerator then generatorFunctionDepth += 1
    if isAsync then asyncFunctionDepth += 1
    if !isArrow then newTargetContextDepth += 1
    val isFieldBoundary = classFieldInitializerDepth > 0 && !isArrow
    if isFieldBoundary then fieldInitializerFunctionBoundaryDepth += 1
    val isNestedSuperBoundary =
      superCallAllowedDepth > 0 && !isArrow && !isMethodRoot
    if isNestedSuperBoundary then superCallNestedFunctionDepth += 1
    if isMethodRoot then superPropertyContextDepth += 1
    val isNestedSuperPropertyBoundary =
      superPropertyContextDepth > 0 && !isArrow && !isMethodRoot
    if isNestedSuperPropertyBoundary then
      superPropertyNestedFunctionDepth += 1
    // Labels and loop/switch contexts are function-scoped: a `break` inside a
    // nested function cannot see the enclosing function's loops or labels.
    val savedLabels = labelStack.toList
    val savedIterationDepth = iterationDepth
    val savedSwitchDepth = switchDepth
    labelStack.clear()
    iterationDepth = 0
    switchDepth = 0
    try f
    finally {
      labelStack.clear()
      labelStack ++= savedLabels
      iterationDepth = savedIterationDepth
      switchDepth = savedSwitchDepth
      if isNestedSuperPropertyBoundary then
        superPropertyNestedFunctionDepth -= 1
      if isMethodRoot then superPropertyContextDepth -= 1
      if isNestedSuperBoundary then superCallNestedFunctionDepth -= 1
      if isFieldBoundary then fieldInitializerFunctionBoundaryDepth -= 1
      if isGenerator then generatorFunctionDepth -= 1
      if isAsync then asyncFunctionDepth -= 1
      if !isArrow then newTargetContextDepth -= 1
      functionDepth -= 1
    }
  }

  /** Parse the body of a loop, tracking iteration context for `break`/
    * `continue` early errors.
    */
  private def parseLoopBody(): Statement = {
    iterationDepth += 1
    try parseStatement()
    finally iterationDepth -= 1
  }

  private def withSwitchContext[T](f: => T): T = {
    switchDepth += 1
    try f
    finally switchDepth -= 1
  }

  /** Parse the body of a labeled statement, rejecting duplicate labels in the
    * same function.
    */
  private def withLabel[T](name: String)(f: => T): T = {
    if name.nonEmpty && labelStack.contains(name) then
      throw new RuntimeException(s"SyntaxError: label '$name' has already been declared")
    labelStack += name
    try f
    finally labelStack.remove(labelStack.length - 1)
  }

  /** Cheap token-level lookahead: is the bracketed group starting at the
    * current `{`/`[` immediately followed by an assignment operator? Used to
    * avoid speculative pattern parsing for ordinary literals.
    */
  private def isBracketedGroupFollowedByAssign(): Boolean = {
    var i = pos
    var depth = 0
    while i < tokens.length do
      tokens(i) match {
        case PunctuationToken(Punctuation.LeftParen, _) |
            PunctuationToken(Punctuation.LeftBracket, _) |
            PunctuationToken(Punctuation.LeftBrace, _) =>
          depth += 1
        case PunctuationToken(Punctuation.RightParen, _) |
            PunctuationToken(Punctuation.RightBracket, _) |
            PunctuationToken(Punctuation.RightBrace, _) =>
          depth -= 1
          if depth == 0 then
            return i + 1 < tokens.length && (tokens(i + 1) match {
              case OperatorToken(Operator.Assign, _) => true
              case _                                 => false
            })
        case _ => ()
      }
      i += 1
    false
  }

  /** Cheap token-level lookahead for `( ... ) =>`. Avoids the costly
    * exception-based speculative parameter parse for every parenthesized
    * expression, which is a major parser hot spot.
    */
  private def isArrowFunctionAhead(): Boolean = {
    var i = pos
    var depth = 0
    while i < tokens.length do
      tokens(i) match {
        case PunctuationToken(Punctuation.LeftParen, _) |
            PunctuationToken(Punctuation.LeftBracket, _) |
            PunctuationToken(Punctuation.LeftBrace, _) =>
          depth += 1
        case PunctuationToken(Punctuation.RightParen, _) |
            PunctuationToken(Punctuation.RightBracket, _) |
            PunctuationToken(Punctuation.RightBrace, _) =>
          depth -= 1
          if depth == 0 then
            return i + 1 < tokens.length && (tokens(i + 1) match {
              case OperatorToken(Operator.Arrow, _) => true
              case _                                => false
            })
        case _ => ()
      }
      i += 1
    false
  }

  /** Validate a left-hand side that must be an assignment pattern (assignment
    * targets, `for (x of ...)`/`for (x in ...)` heads): rest elements must be
    * last and `eval`/`arguments` cannot be assigned in strict mode.
    */
  private def validateAssignmentPatternTarget(left: Expression): Unit =
    left match {
      case ObjectLiteral(properties, _)
          if properties.dropRight(1).exists(_.isInstanceOf[SpreadElement]) =>
        throw new RuntimeException("assignment rest property must be last")
      case ArrayLiteral(elements, _, trailingCommaAfterSpread)
          if trailingCommaAfterSpread || elements.dropRight(1).exists {
            case _: SpreadElement => true
            case _                => false
          } =>
        throw new RuntimeException("assignment rest element must be last")
      case Identifier("eval" | "arguments", _) if currentStrictMode =>
        throw new RuntimeException(
          "SyntaxError: cannot assign to 'eval' or 'arguments' in strict mode"
        )
      case _ => ()
    }

  /** Validate a for-in/for-of loop head. Declarations may not have an
    * initializer (except Annex B `for (var x = 1 in y)` in sloppy mode), and
    * expression heads must be valid assignment patterns.
    */
  private def validateForInOfHead(
      forInit: VariableDeclaration | Expression,
      isForOf: Boolean
  ): Unit =
    forInit match {
      case decl: VariableDeclaration =>
        if decl.declarations.exists(_.init != null) then {
          val annexBAllowed =
            !isForOf && decl.kind == VariableKind.Var && !currentStrictMode
          if !annexBAllowed then
            throw new RuntimeException(
              "for-in/of loop variable declaration may not have an initializer"
            )
        }
      case expr: Expression => validateAssignmentPatternTarget(expr)
    }

  /** Check if this is a for-in or for-of loop (returns "in", "of", or null) */
  private def isForInOrOfAhead(): String | Null = {
    var i = pos
    var depth = 0
    while i < tokens.length do {
      tokens(i) match {
        case PunctuationToken(Punctuation.LeftParen, _) |
            PunctuationToken(Punctuation.LeftBracket, _) |
            PunctuationToken(Punctuation.LeftBrace, _) =>
          depth += 1
        case PunctuationToken(Punctuation.RightParen, _) =>
          if depth == 0 then return null
          depth -= 1
        case PunctuationToken(Punctuation.RightBracket, _) |
            PunctuationToken(Punctuation.RightBrace, _) =>
          depth -= 1
        case PunctuationToken(Punctuation.Semicolon, _) =>
          if depth == 0 then return null
        case KeywordToken(Keyword.In, _) =>
          if depth == 0 then return "in"
        case IdentifierToken("of", _, _) =>
          if depth == 0 then return "of"
        case _ => ()
      }
      i += 1
    }
    null
  }

  private def isForInAhead(): Boolean = isForInOrOfAhead() == "in"

  /** Expect a specific token or throw an error */
  private def expectToken(token: Token): Unit =
    if !isToken(token) then
      throw new RuntimeException(s"Expected $token but got ${current}")

  /** Expect a specific keyword or throw an error */
  private def expectKeyword(keyword: Keyword): Unit =
    if !isKeyword(keyword) then
      throw new RuntimeException(
        s"Expected keyword $keyword but got ${current}"
      )

  /** Expect a specific punctuation or throw an error (does NOT advance) */
  private def expectPunctuation(punct: Punctuation): Unit =
    if !isPunctuation(punct) then
      throw new RuntimeException(
        s"Expected punctuation $punct but got ${current}"
      )

  /** Parse a script */
  def parseScript(): Script = {
    val body = ArrayBuffer[Statement]()

    // Module code is always strict.
    if moduleMode then currentStrictMode = true
    if strictMode then currentStrictMode = true

    // Peek ahead to check if first statement is "use strict" directive
    // so that currentStrictMode is set before parsing inner functions
    if isUseStrictDirectiveAhead() then currentStrictMode = true

    while current != EOF do {
      moduleItemAllowed = moduleMode
      body += parseStatement()
    }
    moduleItemAllowed = false

    val span = Span(0, 0, 0, 0) // TODO: compute actual span
    val (isStrict, remainingBody) = Parser.extractStrictMode(body.toSeq)
    currentStrictMode = isStrict
    val script = Script(remainingBody, isStrict || moduleMode || strictMode, span)
    validateStatementList(
      script.body,
      blockScope = moduleMode,
      isStrict || moduleMode || strictMode
    )
    if moduleMode then {
      validateModuleExportNames(script.body)
      validateModuleExportBindings(script.body)
    }
    script
  }

  /** Enforce the declaration-name early errors whose scope is a StatementList.
    * Keeping this as a post-parse pass mirrors the specification's static
    * semantics and, importantly, lets var declarations nested in statements be
    * checked against lexical declarations in an enclosing block.
    */
  private def validateStatementList(
      statements: Seq[Statement],
      blockScope: Boolean,
      strict: Boolean
  ): Unit = {
    val lexical = scala.collection.mutable.HashMap.empty[String, String]

    def addLexical(name: String, kind: String): Unit =
      lexical.get(name) match {
        case Some(previous)
            if strict || previous != "function" || kind != "function" =>
          throw new RuntimeException(s"Identifier '$name' has already been declared")
        case Some(_) => () // Annex B permits duplicate sloppy block functions.
        case None    => lexical(name) = kind
      }

    statements.foreach {
      case VariableDeclaration(kind, declarations, _)
          if kind != VariableKind.Var =>
        declarations.foreach(d => bindingNames(d.id).foreach(addLexical(_, "lexical")))
      case ClassDeclaration(id, _, _, _) => addLexical(id.name, "lexical")
      case FunctionDeclaration(id, _, _, isGenerator, isAsync, _, _) if blockScope =>
        addLexical(id.name, if !isGenerator && !isAsync then "function" else "lexical")
      case ExportNamedDeclaration(declaration: Statement, _, _, _) =>
        directLexicalDeclarations(declaration, blockScope).foreach { case (name, kind) =>
          addLexical(name, kind)
        }
      case ExportDefaultDeclaration(declaration, _) =>
        declaration match {
          case FunctionExpression(id, _, _, _, _, _, _) if id != null =>
            addLexical(id.name, "lexical")
          case ClassExpression(id, _, _, _) if id != null =>
            addLexical(id.name, "lexical")
          case statement: Statement =>
            directLexicalDeclarations(statement, blockScope).foreach { case (name, kind) =>
              addLexical(name, kind)
            }
          case _ => ()
        }
      case ImportDeclaration(specifiers, _, _) =>
        specifiers.foreach {
          case ImportNamedSpecifier(_, local, _) => addLexical(local.name, "lexical")
          case ImportDefaultSpecifier(local, _) => addLexical(local.name, "lexical")
          case ImportNamespaceSpecifier(local, _) => addLexical(local.name, "lexical")
        }
      case _ => ()
    }

    val varNames = statements.flatMap(collectVarDeclaredNames).toSet
    lexical.keys.find(varNames.contains).foreach { name =>
      throw new RuntimeException(s"Identifier '$name' has already been declared")
    }

    statements.foreach(validateNestedStatement(_, strict))
  }

  /** Module-only early errors: exported names must be unique. */
  private def validateModuleExportNames(statements: Seq[Statement]): Unit = {
    val seen = scala.collection.mutable.HashSet.empty[String]
    def add(name: String): Unit =
      if !seen.add(name) then
        throw new RuntimeException(s"Duplicate export name '$name'")
    def exportedName(value: Identifier | String): String = value match {
      case id: Identifier => id.name
      case s: String      => s
    }
    statements.foreach {
      case ExportDefaultDeclaration(_, _) => add("default")
      case ExportNamedDeclaration(declaration: Statement, _, _, _) =>
        declaration match {
          case VariableDeclaration(_, declarations, _) =>
            declarations.foreach(d => bindingNames(d.id).foreach(add))
          case FunctionDeclaration(id, _, _, _, _, _, _) => add(id.name)
          case ClassDeclaration(id, _, _, _)               => add(id.name)
          case _                                           => ()
        }
      case ExportNamedDeclaration(null, specifiers, _, _) =>
        specifiers.foreach(s => add(exportedName(s.exported)))
      case ExportAllDeclaration(_, namespace, _) =>
        if namespace != null then add(namespace.name)
      case _ => ()
    }
  }

  /** Module-only early error: every locally exported name (an
    * `export { name }` without a `from` clause) must be declared at module
    * scope. Collecting the whole module first allows forward references such
    * as `export { f }; function f() {}`.
    */
  private def validateModuleExportBindings(statements: Seq[Statement]): Unit = {
    val declared = scala.collection.mutable.HashSet.empty[String]

    def addStatementDeclarations(statement: Statement): Unit = statement match {
      case VariableDeclaration(_, declarations, _) =>
        declarations.foreach(d => declared ++= bindingNames(d.id))
      case FunctionDeclaration(id, _, _, _, _, _, _) => declared += id.name
      case ClassDeclaration(id, _, _, _)               => declared += id.name
      case _                                           => ()
    }

    statements.foreach {
      case statement @ (_: VariableDeclaration | _: FunctionDeclaration |
          _: ClassDeclaration) =>
        addStatementDeclarations(statement)
      case ImportDeclaration(specifiers, _, _) =>
        specifiers.foreach {
          case ImportNamedSpecifier(_, local, _)   => declared += local.name
          case ImportDefaultSpecifier(local, _)    => declared += local.name
          case ImportNamespaceSpecifier(local, _)  => declared += local.name
        }
      case ExportNamedDeclaration(declaration: Statement, _, _, _) =>
        addStatementDeclarations(declaration)
      case ExportDefaultDeclaration(declaration, _) =>
        declaration match {
          case FunctionExpression(id, _, _, _, _, _, _) if id != null =>
            declared += id.name
          case ClassExpression(id, _, _, _) if id != null =>
            declared += id.name
          case _ => ()
        }
      case _ => ()
    }
    // `var` declarations nested in statements are still module-level.
    declared ++= statements.flatMap(collectVarDeclaredNames)

    def exportedName(value: Identifier | String): String = value match {
      case id: Identifier => id.name
      case s: String      => s
    }
    statements.foreach {
      case ExportNamedDeclaration(null, specifiers, null, _) =>
        specifiers.foreach { spec =>
          val localName = exportedName(spec.local)
          if !declared.contains(localName) then
            throw new RuntimeException(
              s"Exported binding '$localName' is not declared in this module"
            )
        }
      case _ => ()
    }
  }

  private def directLexicalDeclarations(
      statement: Statement,
      blockScope: Boolean
  ): Seq[(String, String)] = statement match {
    case VariableDeclaration(kind, declarations, _) if kind != VariableKind.Var =>
      declarations.flatMap(d => bindingNames(d.id)).map(_ -> "lexical")
    case ClassDeclaration(id, _, _, _) => Seq(id.name -> "lexical")
    case FunctionDeclaration(id, _, _, isGenerator, isAsync, _, _) if blockScope =>
      Seq(id.name -> (if !isGenerator && !isAsync then "function" else "lexical"))
    case _ => Seq.empty
  }

  private def bindingNames(pattern: BindingPattern): Seq[String] = pattern match {
    case Identifier(name, _)                 => Seq(name)
    case BindingAssignment(target, _, _)     => bindingNames(target)
    case ArrayPattern(elements, _)           => elements.flatMap(e => Option(e).toSeq.flatMap(bindingNames))
    case ObjectPattern(properties, rest, _)  =>
      properties.flatMap(p => bindingNames(p.value)) ++ Option(rest).toSeq.flatMap(bindingNames)
    case RestElement(argument, _)            => bindingNames(argument)
  }

  private def collectVarDeclaredNames(statement: Statement): Seq[String] = statement match {
    case VariableDeclaration(VariableKind.Var, declarations, _) =>
      declarations.flatMap(d => bindingNames(d.id))
    case _: FunctionDeclaration | _: ClassDeclaration => Seq.empty
    case BlockStatement(statements, _) => statements.flatMap(collectVarDeclaredNames)
    case IfStatement(_, consequent, alternate, _) =>
      collectVarDeclaredNames(consequent) ++ Option(alternate).toSeq.flatMap(collectVarDeclaredNames)
    case WhileStatement(_, body, _, _)       => collectVarDeclaredNames(body)
    case DoWhileStatement(body, _, _, _)     => collectVarDeclaredNames(body)
    case ForStatement(init, _, _, body, _, _) =>
      Option(init).toSeq.collect { case v: VariableDeclaration => v }.flatMap(collectVarDeclaredNames) ++
        collectVarDeclaredNames(body)
    case ForInStatement(left, _, body, _, _) =>
      (left match { case v: VariableDeclaration => collectVarDeclaredNames(v); case _ => Seq.empty }) ++
        collectVarDeclaredNames(body)
    case ForOfStatement(left, _, body, _, _) =>
      (left match { case v: VariableDeclaration => collectVarDeclaredNames(v); case _ => Seq.empty }) ++
        collectVarDeclaredNames(body)
    case ForAwaitOfStatement(left, _, body, _, _) =>
      (left match { case v: VariableDeclaration => collectVarDeclaredNames(v); case _ => Seq.empty }) ++
        collectVarDeclaredNames(body)
    case SwitchStatement(_, cases, _) => cases.flatMap(_.consequent).flatMap(collectVarDeclaredNames)
    case TryStatement(block, handler, finalizer, _) =>
      collectVarDeclaredNames(block) ++ Option(handler).toSeq.flatMap(h => collectVarDeclaredNames(h.body)) ++
        Option(finalizer).toSeq.flatMap(collectVarDeclaredNames)
    case WithStatement(_, body, _) => collectVarDeclaredNames(body)
    case ExportNamedDeclaration(declaration: Statement, _, _, _) => collectVarDeclaredNames(declaration)
    case ExportDefaultDeclaration(declaration: Statement, _) => collectVarDeclaredNames(declaration)
    case _ => Seq.empty
  }

  private def validateNestedStatement(statement: Statement, strict: Boolean): Unit = statement match {
    case BlockStatement(statements, _) => validateStatementList(statements, blockScope = true, strict)
    case FunctionDeclaration(_, _, body, _, _, functionStrict, _) =>
      validateStatementList(body.statements, blockScope = false, strict || functionStrict)
    case IfStatement(_, consequent, alternate, _) =>
      rejectDeclarationAsSingleStatement(consequent, strict, allowAnnexBFunction = true)
      Option(alternate).foreach(
        rejectDeclarationAsSingleStatement(_, strict, allowAnnexBFunction = true)
      )
      validateNestedStatement(consequent, strict)
      Option(alternate).foreach(validateNestedStatement(_, strict))
    case WhileStatement(_, body, _, _) =>
      rejectDeclarationAsSingleStatement(body, strict, allowAnnexBFunction = false)
      validateNestedStatement(body, strict)
    case DoWhileStatement(body, _, _, _) =>
      rejectDeclarationAsSingleStatement(body, strict, allowAnnexBFunction = false)
      validateNestedStatement(body, strict)
    case ForStatement(_, _, _, body, _, _) =>
      rejectDeclarationAsSingleStatement(body, strict, allowAnnexBFunction = false)
      validateNestedStatement(body, strict)
    case ForInStatement(_, _, body, _, _) =>
      rejectDeclarationAsSingleStatement(body, strict, allowAnnexBFunction = false)
      validateNestedStatement(body, strict)
    case ForOfStatement(_, _, body, _, _) =>
      rejectDeclarationAsSingleStatement(body, strict, allowAnnexBFunction = false)
      validateNestedStatement(body, strict)
    case ForAwaitOfStatement(_, _, body, _, _) =>
      rejectDeclarationAsSingleStatement(body, strict, allowAnnexBFunction = false)
      validateNestedStatement(body, strict)
    case SwitchStatement(_, cases, _) =>
      val statements = cases.flatMap(_.consequent)
      validateStatementList(statements, blockScope = true, strict)
    case TryStatement(block, handler, finalizer, _) =>
      validateNestedStatement(block, strict)
      Option(handler).foreach { clause =>
        val bodyLexical = clause.body.statements.flatMap(directLexicalDeclarations(_, blockScope = true)).map(_._1).toSet
        Option(clause.param).toSeq.flatMap(bindingNames).find(bodyLexical.contains).foreach { name =>
          throw new RuntimeException(s"Identifier '$name' has already been declared")
        }
        validateNestedStatement(clause.body, strict)
      }
      Option(finalizer).foreach(validateNestedStatement(_, strict))
    case WithStatement(_, body, _) =>
      rejectDeclarationAsSingleStatement(body, strict, allowAnnexBFunction = false)
      validateNestedStatement(body, strict)
    case ExportNamedDeclaration(declaration: Statement, _, _, _) => validateNestedStatement(declaration, strict)
    case ExportDefaultDeclaration(declaration: Statement, _) => validateNestedStatement(declaration, strict)
    case _ => ()
  }

  private def rejectDeclarationAsSingleStatement(
      statement: Statement,
      strict: Boolean,
      allowAnnexBFunction: Boolean
  ): Unit =
    statement match {
      case VariableDeclaration(kind, _, _) if kind != VariableKind.Var =>
        throw new RuntimeException(
          "Lexical declaration is not allowed in a single-statement context"
        )
      case _: ClassDeclaration =>
        throw new RuntimeException(
          "Class declaration is not allowed in a single-statement context"
        )
      case fn: FunctionDeclaration
          if fn.isAsync || fn.isGenerator || strict || !allowAnnexBFunction =>
        throw new RuntimeException(
          "Function declaration is not allowed in a single-statement context"
        )
      case _ => ()
    }

  /** Peek ahead to check if the first statement is a "use strict" directive.
    * This is needed to set strict mode BEFORE parsing inner function
    * expressions. When called before parsing a function body, the current token
    * should be '{'.
    */
  private def isUseStrictDirectiveAhead(): Boolean = {
    val savedPos = pos
    try {
      // Skip past '{' if present (for function bodies)
      var lookPos = pos
      tokens(lookPos) match {
        case PunctuationToken(Punctuation.LeftBrace, _) => lookPos += 1
        case _                                          => ()
      }

      // Scan the whole Directive Prologue: a run of leading string-literal
      // expression statements. `"use strict"` may appear after other
      // directives and still makes the enclosing code strict.
      var found = false
      var scanning = true
      while scanning && lookPos < tokens.length do
        tokens(lookPos) match {
          case StringToken(value, span, _) =>
            if value == "use strict" then found = true
            val nextPos = lookPos + 1
            if nextPos >= tokens.length then scanning = false
            else
              tokens(nextPos) match {
                case PunctuationToken(Punctuation.Semicolon, _) =>
                  lookPos = nextPos + 1
                case next if next == EOF || next.span.line != span.line =>
                  lookPos = nextPos // ASI
                case _ =>
                  // Not a directive-only statement (e.g. `"a" + b`); the
                  // prologue ends here.
                  scanning = false
              }
          case _ => scanning = false
        }
      found
    } finally pos = savedPos
  }

  /** Check if current token is a label (identifier followed by colon) */
  private def isLabel(): Boolean =
    current match {
      case IdentifierToken(_, _, _) =>
        // Peek at next token to see if it's a colon
        peek() match {
          case PunctuationToken(Punctuation.Colon, _) => true
          case _                                      => false
        }
      case _ => false
    }

  /** Parse a statement */
  private def parseStatement(): Statement = {
    // Only the top-level statement of a module body may be an import/export
    // declaration. Any recursive call (block, body, method, ...) sees false.
    val allowModuleItem = moduleItemAllowed
    moduleItemAllowed = false
    // `debugger` is a reserved word with no AST node; it is a no-op.
    current match {
      case IdentifierToken("debugger", span, false) =>
        advance()
        if isPunctuation(Punctuation.Semicolon) then advance()
        return ExpressionStatement(Literal(JSValue.Undefined, span), span)
      case _ => ()
    }
    // Check for labeled statement
    if isLabel() then {
      // Parse the label
      val labelToken = current
      labelToken match {
        case IdentifierToken(name, _, _)
            if Parser.isReservedWordForIdentifierReference(
              name,
              currentStrictMode
            ) || (name == "await" && awaitExpressionAllowed) ||
              (name == "yield" && generatorFunctionDepth > 0) =>
          throw new RuntimeException(
            s"SyntaxError: '$name' cannot be used as a label"
          )
        case _ => ()
      }
      advance() // consume identifier
      expectPunctuation(Punctuation.Colon) // check for :
      advance() // consume :

      val labelName = labelToken match {
        case IdentifierToken(name, _, _) => name
        case _                           => ""
      }
      withLabel(labelName) {
        // Check what kind of statement follows
        current match {
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
            rejectDeclarationAsSingleStatement(
              body,
              currentStrictMode,
              allowAnnexBFunction = true
            )
            // For non-loop labeled statements, we don't store the label in the AST
            // since break/continue only work with loops
            body
        }
      }
    } else {
      val result = current match {
        case PunctuationToken(Punctuation.Semicolon, span) =>
          advance()
          return BlockStatement(Seq.empty, span)
        case KeywordToken(k, _)
            if k == Keyword.Var || k == Keyword.Let || k == Keyword.Const =>
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
        case KeywordToken(Keyword.Import, _)
            if allowModuleItem && !isImportMetaStart && !isDynamicImportStart =>
          parseImportDeclaration()
        case KeywordToken(Keyword.Export, _) if allowModuleItem =>
          parseExportDeclaration()
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
          peek() match {
            case IdentifierToken(_, _, _) | KeywordToken(_, _) =>
              // function name() {} - function declaration. Contextual keywords
              // (from/as/of/...) are valid names; reserved words are rejected
              // by validateBindingIdentifier.
              parseFunctionDeclaration()
            case OperatorToken(Operator.Mul, _) =>
              peek(2) match {
                case IdentifierToken(_, _, _) | KeywordToken(_, _) =>
                  // function *name() {} - generator function declaration
                  parseFunctionDeclaration()
                case _ =>
                  // function *() {} - anonymous generator function expression
                  val funcExpr = parseFunctionExpression()
                  ExpressionStatement(funcExpr, funcExpr.span)
              }
            case _ =>
              // function() {} - anonymous function expression
              // Parse as expression and wrap in ExpressionStatement
              val funcExpr = parseFunctionExpression()
              ExpressionStatement(funcExpr, funcExpr.span)
          }
        case KeywordToken(Keyword.Async, _) =>
          // async function - check if followed by function keyword
          peek() match {
            case KeywordToken(Keyword.Function, _) =>
              // async function name() {} - async function declaration
              parseFunctionDeclaration()
            case _ =>
              // async followed by something else - parse as expression (could be async arrow)
              val expr = parseExpression()
              ExpressionStatement(expr, expr.span)
          }
        case KeywordToken(Keyword.Class, _) =>
          parseClassDeclaration()
        case PunctuationToken(Punctuation.LeftBrace, _) =>
          parseBlockStatement()
        case _ =>
          // Try to parse as expression statement
          val expr = parseExpression()
          requireStatementEnd()
          ExpressionStatement(expr, expr.span)
      }

      // Consume optional semicolon after statement
      if isPunctuation(Punctuation.Semicolon) then advance()

      result
    }

  /** Parse a variable declaration */
  }
  private def parseVariableDeclaration(
      allowConstWithoutInitializer: Boolean = false
  ): VariableDeclaration = {
    val kindToken = current
    val kind = kindToken match {
      case KeywordToken(k, _) =>
        k match {
          case Keyword.Var   => VariableKind.Var
          case Keyword.Let   => VariableKind.Let
          case Keyword.Const => VariableKind.Const
          case _ => throw new RuntimeException(s"Expected variable keyword")
        }
      case _ => throw new RuntimeException(s"Expected variable keyword")
    }

    advance()
    val declarators = ArrayBuffer[VariableDeclarator]()
    val span = kindToken.span

    var more = true
    while more do {
      val declarator = parseVariableDeclarator()
      if kind != VariableKind.Var && bindingNames(declarator.id).contains("let") then
        throw new RuntimeException("Lexical declaration cannot bind 'let'")
      if kind == VariableKind.Const && declarator.init == null &&
        !allowConstWithoutInitializer
      then throw new RuntimeException("Missing initializer in const declaration")
      declarators += declarator
      if isOperator(Operator.Comma) then advance()
      else more = false
    }

    VariableDeclaration(kind, declarators.toSeq, span)
  }

  /** Parse a variable declarator */
  private def parseVariableDeclarator(): VariableDeclarator = {
    val id = parseBindingPattern(allowDefault = false)
    val init = if isOperator(Operator.Assign) then {
      advance()
      // Don't parse comma operator here - comma in variable declarations is a separator
      Some(parseAssignmentExpressionWithoutComma())
    } else None

    val span = id.span
    VariableDeclarator(id, init.orNull, span)
  }

  /** Parse an if statement */
  private def parseIfStatement(): IfStatement = {
    val startSpan = current.span
    expectKeyword(Keyword.If)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    advance() // consume (
    val test = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    advance() // consume )
    val consequent = parseStatement()
    val alternate = if isKeyword(Keyword.Else) then {
      advance()
      parseStatement()
    } else null

    val span = startSpan
    IfStatement(test, consequent, alternate, span)
  }

  /** Parse a while statement */
  private def parseWhileStatement(): WhileStatement = {
    val startSpan = current.span
    expectKeyword(Keyword.While)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    advance() // consume (
    val test = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    advance() // consume )
    val body = parseLoopBody()

    val span = startSpan
    WhileStatement(test, body, null, span)
  }

  /** Parse a labeled while statement */
  private def parseLabeledWhileStatement(labelToken: Token): WhileStatement = {
    val label = labelToken match {
      case IdentifierToken(name, _, _) => Identifier(name, labelToken.span)
      case _                        =>
        throw new RuntimeException(
          s"Expected identifier as label, got $labelToken"
        )
    }

    expectKeyword(Keyword.While)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    advance() // consume (
    val test = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    advance() // consume )
    val body = parseLoopBody()

    val span = labelToken.span
    WhileStatement(test, body, label, span)
  }

  /** Parse a do-while statement */
  private def parseDoWhileStatement(): DoWhileStatement =
    parseDoWhileStatementInternal(null)

  /** Parse a labeled do-while statement */
  private def parseLabeledDoWhileStatement(
      labelToken: Token
  ): DoWhileStatement = {
    val label = labelToken match {
      case IdentifierToken(name, _, _) => Identifier(name, labelToken.span)
      case _                        =>
        throw new RuntimeException(
          s"Expected identifier as label, got $labelToken"
        )
    }
    parseDoWhileStatementInternal(label)
  }

  /** Parse a labeled block statement: label: { ... } */
  private def parseLabeledBlockStatement(labelToken: Token): BlockStatement =
    // Labeled blocks are just regular blocks - the label is stored for potential break statements
    // but we don't need to store it in the AST since blocks don't use it for control flow
    parseBlockStatement()

  /** Internal method to parse do-while with optional label */
  private def parseDoWhileStatementInternal(
      label: Identifier | Null
  ): DoWhileStatement = {
    val startSpan = current.span
    expectKeyword(Keyword.Do)
    advance()
    val body = parseLoopBody()
    expectKeyword(Keyword.While)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    advance() // consume (
    val test = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    advance() // consume )

    val span = startSpan
    DoWhileStatement(body, test, label, span)
  }

  /** Parse a switch statement */
  private def parseSwitchStatement(): SwitchStatement = {
    val startSpan = current.span
    expectKeyword(Keyword.Switch)
    advance()
    expectPunctuation(Punctuation.LeftParen)
    advance() // consume (
    val discriminant = parseExpression()
    expectPunctuation(Punctuation.RightParen)
    advance() // consume )
    expectPunctuation(Punctuation.LeftBrace)
    advance() // consume {

    val cases = ArrayBuffer[SwitchCase]()

    // Parse cases - use a simpler loop structure
    var caseCount = 0
    withSwitchContext {
      while current != EOF && !isPunctuation(Punctuation.RightBrace) do
      // Check if we have a case clause
      if isKeyword(Keyword.Case) then {
        advance() // consume 'case'
        val test = parseExpression()
        expectPunctuation(Punctuation.Colon) // check for :
        advance() // consume :

        // Parse statements for this case - stop at next case/default or closing brace
        val consequent = ArrayBuffer[Statement]()
        var stmtCount = 0
        while current != EOF &&
          !isPunctuation(Punctuation.RightBrace) &&
          !isKeyword(Keyword.Case) &&
          !isKeyword(Keyword.Default)
        do {
          consequent += parseStatement()
          stmtCount += 1
        }

        cases += SwitchCase(test, consequent.toSeq, test.span)
        caseCount += 1
      }
      // Check if we have a default clause
      else if isKeyword(Keyword.Default) then {
        advance() // consume 'default'
        expectPunctuation(Punctuation.Colon) // check for :
        advance() // consume :

        // Parse statements for default case - stop at next case or closing brace
        val consequent = ArrayBuffer[Statement]()
        while current != EOF &&
          !isPunctuation(Punctuation.RightBrace) &&
          !isKeyword(Keyword.Case)
        do consequent += parseStatement()

        cases += SwitchCase(null, consequent.toSeq, startSpan)
      } else {
        val tok = current
        val isCase = isKeyword(Keyword.Case)
        val isDefault = isKeyword(Keyword.Default)
        val isRightBrace = isPunctuation(Punctuation.RightBrace)
        throw new RuntimeException(
          s"Expected 'case' or 'default' in switch statement but got ${tok} at ${tok.span} (isCase=$isCase, isDefault=$isDefault, isRightBrace=$isRightBrace, caseCount=$caseCount)"
        )
      }
    }

    expectPunctuation(Punctuation.RightBrace)
    advance() // consume }

    SwitchStatement(discriminant, cases.toSeq, startSpan)
  }

  /** Parse a for statement */
  private def parseForStatement(): Statement = {
    val startSpan = current.span
    expectKeyword(Keyword.For)
    advance()

    // Check for 'await' keyword (for await...of)
    val isForAwait = isKeyword(Keyword.Await)
    if isForAwait then advance() // consume 'await'

    expectPunctuation(Punctuation.LeftParen)
    advance() // consume (

    // Check if init is a variable declaration
    val isVarDecl: Boolean = current match {
      case KeywordToken(k, _) =>
        k == Keyword.Var || k == Keyword.Let || k == Keyword.Const
      case _ => false
    }

    val forInOrOf = isForInOrOfAhead()
    val isForInOrOfLoop = forInOrOf != null
    // Parse init and ensure proper typing
    val initResult =
      if isVarDecl then
        if isForInOrOfLoop then
          Left(withInOperatorAllowed(false)(parseVariableDeclaration(allowConstWithoutInitializer = true)))
        else Left(parseVariableDeclaration())
      else if !isPunctuation(Punctuation.Semicolon) then
        if isForInOrOfLoop then
          Right(withInOperatorAllowed(false) {
            parseAssignmentExpressionWithoutComma()
          })
        else Right(parseExpression())
      else Right(null)

    val init: VariableDeclaration | Expression | Null = initResult match {
      case Left(vd) => vd
      case Right(e) => e
    }

    // Handle for-in loop
    if isKeyword(Keyword.In) && forInOrOf == "in" then {
      val forInit: VariableDeclaration | Expression = init match {
        case v: VariableDeclaration => v
        case e: Expression          => e
        case null                   =>
          throw new RuntimeException("Expected left-hand side in for-in")
      }
      validateForInOfHead(forInit, isForOf = false)
      advance()
      val right = parseExpression()
      expectPunctuation(Punctuation.RightParen)
      advance() // consume )
      val body = parseLoopBody()
      val span = startSpan
      return ForInStatement(forInit, right, body, null, span)
    }

    // Handle for-of loop (or for-await-of if isForAwait is true)
    if isIdentifier("of") && forInOrOf == "of" then {
      val forInit: VariableDeclaration | Expression = init match {
        case v: VariableDeclaration => v
        case e: Expression          => e
        case null                   =>
          throw new RuntimeException("Expected left-hand side in for-of")
      }
      validateForInOfHead(forInit, isForOf = true)
      advance() // consume 'of'
      val right = parseAssignmentExpressionWithoutComma()
      expectPunctuation(Punctuation.RightParen)
      advance() // consume )
      val body = parseLoopBody()
      val span = startSpan
      if isForAwait then
        return ForAwaitOfStatement(forInit, right, body, null, span)
      else return ForOfStatement(forInit, right, body, null, span)
    }

    if isPunctuation(Punctuation.Semicolon) then advance()

    val test =
      if !isPunctuation(Punctuation.Semicolon) then parseExpression()
      else null

    if isPunctuation(Punctuation.Semicolon) then advance()

    val update =
      if !isPunctuation(Punctuation.RightParen) then parseExpression()
      else null

    expectPunctuation(Punctuation.RightParen)
    advance() // consume )
    val body = parseLoopBody()

    val span = startSpan
    ForStatement(init, test, update, body, null, span)
  }

  /** Parse a labeled for statement */
  private def parseLabeledForStatement(labelToken: Token): Statement = {
    val label = labelToken match {
      case IdentifierToken(name, _, _) => Identifier(name, labelToken.span)
      case _                        =>
        throw new RuntimeException(
          s"Expected identifier as label, got $labelToken"
        )
    }

    expectKeyword(Keyword.For)
    advance()

    // Check for 'await' keyword (for await...of)
    val isForAwait = isKeyword(Keyword.Await)
    if isForAwait then advance() // consume 'await'

    expectPunctuation(Punctuation.LeftParen)
    advance() // consume (

    // Check if init is a variable declaration
    val isVarDecl: Boolean = current match {
      case KeywordToken(k, _) =>
        k == Keyword.Var || k == Keyword.Let || k == Keyword.Const
      case _ => false
    }

    val forInOrOf = isForInOrOfAhead()
    val isForInOrOfLoop = forInOrOf != null
    // Parse init and ensure proper typing
    val initResult =
      if isVarDecl then
        if isForInOrOfLoop then
          Left(withInOperatorAllowed(false)(parseVariableDeclaration(allowConstWithoutInitializer = true)))
        else Left(parseVariableDeclaration())
      else if !isPunctuation(Punctuation.Semicolon) then
        if isForInOrOfLoop then
          Right(withInOperatorAllowed(false) {
            parseAssignmentExpressionWithoutComma()
          })
        else Right(parseExpression())
      else Right(null)

    val init: VariableDeclaration | Expression | Null = initResult match {
      case Left(vd) => vd
      case Right(e) => e
    }

    // Handle for-in loop
    if isKeyword(Keyword.In) && forInOrOf == "in" then {
      val forInit: VariableDeclaration | Expression = init match {
        case v: VariableDeclaration => v
        case e: Expression          => e
        case null                   =>
          throw new RuntimeException("Expected left-hand side in for-in")
      }
      validateForInOfHead(forInit, isForOf = false)
      advance()
      val right = parseExpression()
      expectPunctuation(Punctuation.RightParen)
      advance() // consume )
      val body = parseLoopBody()
      val span = labelToken.span
      return ForInStatement(forInit, right, body, label, span)
    }

    // Handle for-of loop (or for-await-of if isForAwait is true)
    if isIdentifier("of") && forInOrOf == "of" then {
      val forInit: VariableDeclaration | Expression = init match {
        case v: VariableDeclaration => v
        case e: Expression          => e
        case null                   =>
          throw new RuntimeException("Expected left-hand side in for-of")
      }
      validateForInOfHead(forInit, isForOf = true)
      advance() // consume 'of'
      val right = parseAssignmentExpressionWithoutComma()
      expectPunctuation(Punctuation.RightParen)
      advance() // consume )
      val body = parseLoopBody()
      val span = labelToken.span
      if isForAwait then
        return ForAwaitOfStatement(forInit, right, body, label, span)
      else return ForOfStatement(forInit, right, body, label, span)
    }

    if isPunctuation(Punctuation.Semicolon) then advance()

    val test =
      if !isPunctuation(Punctuation.Semicolon) then parseExpression()
      else null

    if isPunctuation(Punctuation.Semicolon) then advance()

    val update =
      if !isPunctuation(Punctuation.RightParen) then parseExpression()
      else null

    expectPunctuation(Punctuation.RightParen)
    advance() // consume )
    val body = parseLoopBody()

    val span = labelToken.span
    ForStatement(init, test, update, body, label, span)
  }

  /** Parse a return statement */
  private def parseReturnStatement(): ReturnStatement = {
    if functionDepth == 0 && !allowTopLevelReturn then
      throw new RuntimeException(
        "SyntaxError: return statement is not allowed outside of a function"
      )
    val startSpan = current.span
    expectKeyword(Keyword.Return)
    advance()
    val argument =
      if current != EOF && current.span.line == startSpan.line &&
          !isPunctuation(Punctuation.Semicolon) &&
          !isPunctuation(Punctuation.RightBrace)
      then Some(parseExpression())
      else None
    val span = startSpan
    ReturnStatement(argument.orNull, span)
  }

  private def parseThrowStatement(): ThrowStatement = {
    val startSpan = current.span
    expectKeyword(Keyword.Throw)
    advance()
    if current == EOF || current.span.line != startSpan.line then
      throw new RuntimeException("Unexpected line terminator after throw")
    val argument = parseExpression()
    val span = startSpan
    ThrowStatement(argument, span)
  }

  /** Enforce that a statement ends here: either a `;`, the end of input, the
    * enclosing `}`, or a line terminator (automatic semicolon insertion).
    */
  private def requireStatementEnd(): Unit =
    if !isPunctuation(Punctuation.Semicolon) &&
        current != EOF &&
        !isPunctuation(Punctuation.RightBrace) &&
        pos > 0 &&
        tokens(pos - 1).span.line == current.span.line
    then
      throw new RuntimeException(s"Unexpected token $current after statement")

  private def parseImportDeclaration(): ImportDeclaration = {
    val result = parseImportDeclarationCore()
    requireStatementEnd()
    result
  }

  private def parseImportDeclarationCore(): ImportDeclaration = {
    val startSpan = current.span
    expectKeyword(Keyword.Import)
    advance()
    current match {
      case StringToken(source, _, _) =>
        advance()
        ImportDeclaration(Seq.empty, source, startSpan)
      case _ =>
        val specifiers = ArrayBuffer.empty[ImportSpecifier]
        if current.isInstanceOf[IdentifierToken] then {
          val local = parseIdentifier()
          validateBindingIdentifier(local.name, local.span)
          specifiers += ImportDefaultSpecifier(local, local.span)
          if isPunctuation(Punctuation.Comma) then advance()
          else if isOperator(Operator.Comma) then advance()
        }
        if isOperator(Operator.Mul) then {
          advance()
          if !isKeyword(Keyword.As) then
            throw new RuntimeException("Expected 'as' in namespace import")
          advance()
          val local = parseIdentifier()
          validateBindingIdentifier(local.name, local.span)
          specifiers += ImportNamespaceSpecifier(local, local.span)
        } else if isPunctuation(Punctuation.LeftBrace) then {
          advance()
          while !isPunctuation(Punctuation.RightBrace) do {
            val imported = parseModuleExportName()
            val local =
              if isKeyword(Keyword.As) then {
                advance()
                parseIdentifier()
              } else imported match {
                case id: Identifier => id
                case _ =>
                  throw new RuntimeException(
                    "String import names require an 'as' binding"
                  )
              }
            val importedSpan = imported match {
              case id: Identifier => id.span
              case _              => local.span
            }
            validateBindingIdentifier(local.name, local.span)
            specifiers += ImportNamedSpecifier(imported, local, importedSpan)
            if isPunctuation(Punctuation.Comma) then advance()
            else if isOperator(Operator.Comma) then advance()
            else ()
          }
          expectPunctuation(Punctuation.RightBrace)
          advance()
        }

        if !isKeyword(Keyword.From) then
          throw new RuntimeException("Expected 'from' in import declaration")
        advance()
        current match {
          case StringToken(source, _, _) =>
            advance()
            ImportDeclaration(specifiers.toSeq, source, startSpan)
          case _ =>
            throw new RuntimeException(
              "Expected string literal in import declaration"
            )
        }
    }
  }

  private def parseExportDeclaration(): Statement =
    parseExportDeclarationCore()

  private def parseExportDeclarationCore(): Statement = {
    val startSpan = current.span
    expectKeyword(Keyword.Export)
    advance()
    if isKeyword(Keyword.Default) then {
      advance()
      current match {
        case KeywordToken(Keyword.Function, _) =>
          val funcExpr = parseFunctionExpression()
          ExportDefaultDeclaration(funcExpr, startSpan)
        case KeywordToken(Keyword.Class, _) =>
          val classExpr = parseClassExpression()
          ExportDefaultDeclaration(classExpr, startSpan)
        case _ =>
          val expr = parseAssignmentExpression()
          requireStatementEnd()
          ExportDefaultDeclaration(expr, startSpan)
      }
    } else
      current match {
        case KeywordToken(Keyword.Var, _) | KeywordToken(Keyword.Let, _) |
            KeywordToken(Keyword.Const, _) =>
          val decl = parseVariableDeclaration()
          requireStatementEnd()
          ExportNamedDeclaration(decl, Seq.empty, null, startSpan)
        case KeywordToken(Keyword.Function, _) =>
          val decl = parseFunctionDeclaration()
          ExportNamedDeclaration(decl, Seq.empty, null, startSpan)
        case KeywordToken(Keyword.Async, _) =>
          // export async function / export async function*
          val decl = parseFunctionDeclaration()
          ExportNamedDeclaration(decl, Seq.empty, null, startSpan)
        case KeywordToken(Keyword.Class, _) =>
          val decl = parseClassDeclaration()
          ExportNamedDeclaration(decl, Seq.empty, null, startSpan)
        case OperatorToken(Operator.Mul, _) =>
          advance()
          // `export * from '...'` or `export * as name from '...'`
          var namespace: Identifier | Null = null
          if isKeyword(Keyword.As) then {
            advance()
            namespace = parseIdentifier()
          }
          if !isKeyword(Keyword.From) then
            throw new RuntimeException("Expected 'from' in export declaration")
          advance()
          current match {
            case StringToken(source, _, _) =>
              advance()
              requireStatementEnd()
              ExportAllDeclaration(source, namespace, startSpan)
            case _ =>
              throw new RuntimeException(
                "Expected string literal in export declaration"
              )
          }
        case PunctuationToken(Punctuation.LeftBrace, _) =>
          advance()
          val specifiers = ArrayBuffer.empty[ExportSpecifier]
          while !isPunctuation(Punctuation.RightBrace) do {
            val local = parseModuleExportName()
            val exported =
              if isKeyword(Keyword.As) then {
                advance()
                parseModuleExportName()
              } else local
            val specSpan = local match {
              case id: Identifier => id.span
              case _              => startSpan
            }
            specifiers += ExportSpecifier(local, exported, specSpan)
            if isPunctuation(Punctuation.Comma) then advance()
            else if isOperator(Operator.Comma) then advance()
            else ()
          }
          expectPunctuation(Punctuation.RightBrace)
          advance()

          var source: String | Null = null
          if isKeyword(Keyword.From) then {
            advance()
            current match {
              case StringToken(modName, _, _) =>
                advance()
                source = modName
              case _ =>
                throw new RuntimeException(
                  "Expected string literal in export declaration"
                )
            }
          }

          requireStatementEnd()
          ExportNamedDeclaration(null, specifiers.toSeq, source, startSpan)
        case _ =>
          throw new RuntimeException("Unsupported export declaration")
      }
  }

  private def parseTryStatement(): TryStatement = {
    val startSpan = current.span
    expectKeyword(Keyword.Try)
    advance()
    val block = parseBlockStatement()
    var handler: CatchClause | Null = null
    var finalizer: BlockStatement | Null = null

    if isKeyword(Keyword.Catch) then {
      advance()
      val param =
        if isPunctuation(Punctuation.LeftParen) then {
          advance()
          val pattern = parseBindingPattern(allowDefault = false)
          expectPunctuation(Punctuation.RightParen)
          advance()
          pattern
        } else null
      val body = parseBlockStatement()
      val catchSpan = param match {
        case pattern: BindingPattern => pattern.span
        case null                    => body.span
      }
      handler = CatchClause(param, body, catchSpan)
    }

    if isKeyword(Keyword.Finally) then {
      advance()
      finalizer = parseBlockStatement()
    }

    if handler == null && finalizer == null then
      throw new RuntimeException("try statement must have catch or finally")

    TryStatement(block, handler, finalizer, startSpan)
  }

  private def parseWithStatement(): WithStatement = {
    if currentStrictMode then
      throw new RuntimeException(
        "SyntaxError: 'with' statements are not allowed in strict mode"
      )
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
  }

  /** Parse a break statement */
  private def parseBreakStatement(): BreakStatement = {
    val startSpan = current.span
    expectKeyword(Keyword.Break)
    advance()
    // Optional label
    val label = current match {
      case IdentifierToken(name, _, _) if current.span.line == startSpan.line =>
        val labelIdent = Identifier(name, current.span)
        advance() // consume the label identifier
        labelIdent
      case _ =>
        null
    }
    val span = startSpan
    if label == null && iterationDepth == 0 && switchDepth == 0 then
      throw new RuntimeException(
        "SyntaxError: break statement is not allowed outside of a loop or switch"
      )
    if label != null && !labelStack.contains(label.name) then
      throw new RuntimeException(
        s"SyntaxError: undefined label '${label.name}'"
      )
    BreakStatement(label, span)
  }

  /** Parse a continue statement */
  private def parseContinueStatement(): ContinueStatement = {
    val startSpan = current.span
    expectKeyword(Keyword.Continue)
    advance()
    // Optional label
    val label = current match {
      case IdentifierToken(name, _, _) if current.span.line == startSpan.line =>
        val labelIdent = Identifier(name, current.span)
        advance() // consume the label identifier
        labelIdent
      case _ =>
        null
    }
    val span = startSpan
    if label == null && iterationDepth == 0 then
      throw new RuntimeException(
        "SyntaxError: continue statement is not allowed outside of a loop"
      )
    if label != null && !labelStack.contains(label.name) then
      throw new RuntimeException(
        s"SyntaxError: undefined label '${label.name}'"
      )
    ContinueStatement(label, span)
  }

  /** Parse a function declaration */
  private def parseFunctionDeclaration(): FunctionDeclaration = {
    val startSpan = current.span

    // Check for async keyword
    val isAsync = isKeyword(Keyword.Async)
    if isAsync then advance()

    expectKeyword(Keyword.Function)
    advance()
    val isGenerator = isOperator(Operator.Mul)
    if isGenerator then advance()
    withFunctionGrammarContext(isGenerator, isAsync) {
      val id = parseIdentifier()
      validateBindingIdentifier(id.name, id.span)
      val params = parseFunctionParams()

      // Apply a leading "use strict" directive before parsing the body so
      // that early errors (for example `with` or reserved identifiers) are
      // reported while parsing.
      val savedStrict = currentStrictMode
      val hasUseStrictDirective = isUseStrictDirectiveAhead()
      if hasUseStrictDirective then currentStrictMode = true
      val body = parseBlockStatement()
      val (bodyStrict, remainingBody) = Parser.extractStrictMode(body.statements)
      val finalBody = BlockStatement(remainingBody.toSeq, body.span)
      val isStrict = savedStrict || hasUseStrictDirective || bodyStrict
      currentStrictMode = savedStrict
      validateFormalParameters(
        params,
        isStrict,
        hasUseStrictDirective || bodyStrict,
        forceUnique = isGenerator || isAsync,
        body = finalBody
      )

      FunctionDeclaration(
        id,
        params.toSeq,
        finalBody,
        isGenerator,
        isAsync,
        isStrict,
        startSpan
      )
    }
  }

  /** Parse a function expression */
  private def parseFunctionExpression(): FunctionExpression = {
    val startSpan = current.span

    // Check for async keyword
    val isAsync = isKeyword(Keyword.Async)
    if isAsync then advance()

    expectKeyword(Keyword.Function)
    advance()
    val isGenerator = isOperator(Operator.Mul)
    if isGenerator then advance()
    withFunctionGrammarContext(isGenerator, isAsync) {
      val id = current match {
        case IdentifierToken(_, _, _) | KeywordToken(_, _) =>
          val parsed = parseIdentifier()
          validateBindingIdentifier(parsed.name, parsed.span)
          parsed
        case _ => null
      }

      val params = parseFunctionParams()
      val savedStrict = currentStrictMode
      val hasUseStrictDirective = isUseStrictDirectiveAhead()
      if hasUseStrictDirective then currentStrictMode = true

      val body = parseBlockStatement()
      val (bodyStrict, remainingBody) = Parser.extractStrictMode(body.statements)
      val finalBody = BlockStatement(remainingBody.toSeq, body.span)
      val isStrict = savedStrict || hasUseStrictDirective || bodyStrict
      currentStrictMode = savedStrict
      validateFormalParameters(
        params,
        isStrict,
        hasUseStrictDirective || bodyStrict,
        forceUnique = isGenerator || isAsync,
        body = finalBody
      )

      FunctionExpression(
        id,
        params.toSeq,
        finalBody,
        isGenerator,
        isAsync,
        isStrict,
        startSpan
      )
    }
  }

  /** Class definitions are always strict mode code, so their binding name may
    * not be a strict-reserved word.
    */
  private def validateClassName(id: Identifier): Unit =
    if Parser.isReservedWordForIdentifierReference(id.name, strict = true) then
      throw new RuntimeException(
        s"SyntaxError: '${id.name}' is not a valid class name"
      )

  private def parseClassDeclaration(): ClassDeclaration = {
    val startSpan = current.span
    expectKeyword(Keyword.Class)
    advance()
    val id = parseIdentifier()
    validateClassName(id)
    val superClass =
      if isKeyword(Keyword.Extends) then {
        advance()
        rejectUnparenthesizedArrowClassHeritage()
        parsePostfixExpression()
      } else null
    val body = parseClassBody(superClass != null)
    ClassDeclaration(id, superClass, body, startSpan)
  }

  private def parseClassExpression(): ClassExpression = {
    val startSpan = current.span
    expectKeyword(Keyword.Class)
    advance()
    val id =
      current match {
        case IdentifierToken(_, _, _) =>
          val identifier = parseIdentifier()
          validateClassName(identifier)
          identifier
        case _ => null
      }
    val superClass =
      if isKeyword(Keyword.Extends) then {
        advance()
        rejectUnparenthesizedArrowClassHeritage()
        parsePostfixExpression()
      } else null
    val body = parseClassBody(superClass != null)
    ClassExpression(id, superClass, body, startSpan)
  }

  /** Scan from the opening-paren token at `openIndex` to its matching `)` and
    * report whether the following token is `=>`. Used to decide between an
    * async arrow head and a call to a function named `async`.
    */
  private def arrowAfterMatchingParen(openIndex: Int): Boolean = {
    var index = openIndex
    var depth = 0
    while index < tokens.length do {
      tokens(index) match {
        case PunctuationToken(Punctuation.LeftParen, _) => depth += 1
        case PunctuationToken(Punctuation.RightParen, _) =>
          depth -= 1
          if depth == 0 then
            return index + 1 < tokens.length && (tokens(index + 1) match {
              case OperatorToken(Operator.Arrow, _) => true
              case _                                => false
            })
        case _ => ()
      }
      index += 1
    }
    false
  }

  private def rejectUnparenthesizedArrowClassHeritage(): Unit = {
    val isArrow = current match {
      case IdentifierToken(_, _, _) =>
        peek() match {
          case OperatorToken(Operator.Arrow, _) => true
          case _                                => false
        }
      case PunctuationToken(Punctuation.LeftParen, _) =>
        arrowAfterMatchingParen(pos)
      case KeywordToken(Keyword.Async, _) =>
        peek() match {
          case IdentifierToken(_, _, _) =>
            peek(2) match {
              case OperatorToken(Operator.Arrow, _) => true
              case _                                => false
            }
          case PunctuationToken(Punctuation.LeftParen, _) =>
            arrowAfterMatchingParen(pos + 1)
          case _ => false
        }
      case _ => false
    }
    if isArrow then
      throw new RuntimeException(
        "An unparenthesized arrow function cannot be a class heritage expression"
      )
  }

  private def parseClassBody(hasHeritage: Boolean): ClassBody = {
    val startSpan = current.span
    val savedStrict = currentStrictMode
    val savedClassHasHeritage = currentClassHasHeritage
    currentStrictMode = true // All class code is strict mode code.
    currentClassHasHeritage = hasHeritage
    val privateEnvironment = PrivateEnvironment()
    privateEnvironmentStack = privateEnvironment :: privateEnvironmentStack
    expectPunctuation(Punctuation.LeftBrace)
    advance() // consume {
    val elements = ArrayBuffer[ClassElement]()
    while !isPunctuation(Punctuation.RightBrace) && current != EOF do
      if isPunctuation(Punctuation.Semicolon) then advance()
      else {
        val element = parseClassElement()
        elements += element
        if isPunctuation(Punctuation.Semicolon) then advance()
        else if element.isInstanceOf[FieldDefinition] &&
          !isPunctuation(Punctuation.RightBrace) && current != EOF &&
          current.span.line == element.span.line
        then
          throw new RuntimeException(
            "Class fields on the same line must be separated by a semicolon"
          )
      }
    expectPunctuation(Punctuation.RightBrace)
    advance() // consume }
    validateClassElements(elements.toSeq)
    val unresolved =
      privateEnvironment.referenced.diff(privateEnvironment.declared)
    privateEnvironmentStack = privateEnvironmentStack.tail
    privateEnvironmentStack match {
      case outer :: _ => outer.referenced ++= unresolved
      case Nil if unresolved.nonEmpty =>
        throw new RuntimeException(
          s"Private name #${unresolved.head} is not declared"
        )
      case _ => ()
    }
    currentStrictMode = savedStrict
    currentClassHasHeritage = savedClassHasHeritage
    ClassBody(elements.toSeq, startSpan)
  }

  private def validateClassElements(elements: Seq[ClassElement]): Unit = {
    def literalName(key: Identifier | PrivateIdentifier | String | Expression)
        : Option[(String, Boolean)] = key match {
      case Identifier(name, _)        => Some(name -> false)
      case PrivateIdentifier(name, _) => Some(name -> true)
      case name: String               => Some(name -> false)
      case _                          => None
    }

    var constructorCount = 0
    val privateDefinitions = scala.collection.mutable.HashMap
      .empty[String, ArrayBuffer[(PropertyKind | Null, Boolean)]]

    elements.foreach {
      case method: MethodDefinition =>
        literalName(method.key).foreach { case (name, isPrivate) =>
          if isPrivate then {
            if name == "constructor" then
              throw new RuntimeException("Private class elements cannot be named #constructor")
            privateDefinitions
              .getOrElseUpdate(name, ArrayBuffer.empty)
              .addOne(method.kind -> method.isStatic)
          } else {
            if method.isStatic && name == "prototype" then
              throw new RuntimeException("Static class methods cannot be named prototype")
            if !method.isStatic && name == "constructor" then {
              val isOrdinaryConstructor =
                method.kind == PropertyKind.Method &&
                  !method.isGenerator && !method.isAsync
              if !isOrdinaryConstructor then
                throw new RuntimeException(
                  "A class constructor cannot be an accessor, generator, or async method"
                )
              constructorCount += 1
              if constructorCount > 1 then
                throw new RuntimeException("A class may only have one constructor")
            }
          }
        }
      case field: FieldDefinition =>
        literalName(field.key).foreach { case (name, isPrivate) =>
          if name == "constructor" then
            throw new RuntimeException("Class fields cannot be named constructor")
          if field.isStatic && !isPrivate && name == "prototype" then
            throw new RuntimeException("Static class fields cannot be named prototype")
          if isPrivate then
            privateDefinitions
              .getOrElseUpdate(name, ArrayBuffer.empty)
              .addOne((null, field.isStatic))
        }
    }

    privateDefinitions.foreach { case (name, kinds) =>
      val isAccessorPair =
        kinds.length == 2 && kinds.map(_._1).contains(PropertyKind.Getter) &&
          kinds.map(_._1).contains(PropertyKind.Setter) &&
          kinds(0)._2 == kinds(1)._2
      if kinds.length > 1 && !isAccessorPair then
        throw new RuntimeException(s"Duplicate private class element #$name")
    }
  }

  private def parseClassElement(): ClassElement = {
    def parseMethodParts(
        isGenerator: Boolean,
        isAsync: Boolean,
        allowsSuperCall: Boolean = false
    ): (Seq[BindingPattern], BlockStatement) = {
      if allowsSuperCall then superCallAllowedDepth += 1
      try {
        withFunctionGrammarContext(
          isGenerator,
          isAsync,
          isMethodRoot = true
        ) {
          val params = parseFunctionParams()
          val body = parseBlockStatement()
          val (bodyStrict, _) = Parser.extractStrictMode(body.statements)
          validateFormalParameters(
            params,
            strict = true,
            hasUseStrictDirective = bodyStrict,
            forceUnique = true,
            body = body
          )
          (params, body)
        }
      } finally {
        if allowsSuperCall then superCallAllowedDepth -= 1
      }
    }

    def isPropertyKeyToken(tok: Token): Boolean = tok match {
      case IdentifierToken(_, _, _)        => true
      case PrivateIdentifierToken(_, _) => true // Private fields
      case KeywordToken(_, _)           => true
      case StringToken(_, _, _)            => true
      case NumberToken(_, _, _)            => true
      case BigIntToken(_, _)            => true
      case PunctuationToken(Punctuation.LeftBracket, _) => true
      case _                                            => false
    }

    def parsePropertyKey()
        : (Identifier | PrivateIdentifier | String | Expression, Span) =
      current match {
        case PrivateIdentifierToken(name, span) =>
          advance()
          privateEnvironmentStack.head.declared += name
          (PrivateIdentifier(name, span), span)
        case IdentifierToken(name, span, _) =>
          advance()
          (Identifier(name, span), span)
        case KeywordToken(kind, span) =>
          advance()
          (Identifier(kind.toString.toLowerCase, span), span)
        case StringToken(value, span, _) =>
          advance()
          (value, span)
        case NumberToken(value, span, _) =>
          advance()
          (numberPropertyKey(value), span)
        case BigIntToken(value, span) =>
          advance()
          (value.toString, span)
        case PunctuationToken(Punctuation.LeftBracket, span) =>
          advance()
          val keyExpr = parseAssignmentExpressionWithoutComma()
          expectPunctuation(Punctuation.RightBracket)
          advance()
          (ComputedPropertyName(keyExpr, span), span)
        case _ =>
          throw new RuntimeException(
            s"Expected class element key but got $current"
          )
      }

    val isStatic =
      current match {
        case IdentifierToken(name, _, false) if name == "static" =>
          peek() match {
            case PunctuationToken(Punctuation.LeftParen, _) => false
            case OperatorToken(Operator.Mul, _)             => true
            case next if isPropertyKeyToken(next)           => true
            case _                                          => false
          }
        case _ => false
      }

    if isStatic then advance()

    val isAsyncMethod =
      current match {
        case IdentifierToken("async", _, false) | KeywordToken(Keyword.Async, _) =>
          peek() match {
            case OperatorToken(Operator.Mul, _) => true
            case next if isPropertyKeyToken(next) =>
              keyFollowedByParen(pos + 1)
            case _ => false
          }
        case _ => false
      }
    if isAsyncMethod then advance()

    val isGenerator = isOperator(Operator.Mul)
    if isGenerator then advance()

    def isAccessorCandidate: Boolean =
      current match {
        case IdentifierToken(name, _, false) if name == "get" || name == "set" =>
          peek() match {
            case PunctuationToken(Punctuation.LeftBracket, _) =>
              var index = pos + 1
              var depth = 0
              var found = false
              while index < tokens.length && !found do {
                tokens(index) match {
                  case PunctuationToken(Punctuation.LeftBracket, _) => depth += 1
                  case PunctuationToken(Punctuation.RightBracket, _) =>
                    depth -= 1
                    if depth == 0 then found = true
                  case _ => ()
                }
                index += 1
              }
              found && index < tokens.length && (tokens(index) match {
                case PunctuationToken(Punctuation.LeftParen, _) => true
                case _ => false
              })
            case tok if isPropertyKeyToken(tok) =>
              peek(2) match {
                case PunctuationToken(Punctuation.LeftParen, _) => true
                case _                                          => false
              }
            case _ => false
          }
        case _ => false
      }

    if isAccessorCandidate then {
      val accessorName = current match {
        case IdentifierToken(name, _, _) => name
        case _                        => ""
      }
      advance()
      val (accessorKey, keySpan) = parsePropertyKey()
      val (params, body) = parseMethodParts(false, false)
      val kind =
        if accessorName == "get" then PropertyKind.Getter
        else PropertyKind.Setter
      validateAccessorParams(kind, params.toSeq)
      return MethodDefinition(
        accessorKey,
        params.toSeq,
        body,
        isStatic,
        kind,
        keySpan,
        isGenerator = false,
        isAsync = false
      )
    }

    val (key, keySpan) = parsePropertyKey()
    if isPunctuation(Punctuation.LeftParen) then {
      val isDerivedConstructor =
        !isStatic && !isGenerator && !isAsyncMethod &&
          (key match {
            case Identifier("constructor", _) => true
            case "constructor"                => true
            case _                            => false
          }) && currentClassHasHeritage
      val (params, body) =
        parseMethodParts(isGenerator, isAsyncMethod, isDerivedConstructor)
      MethodDefinition(
        key,
        params.toSeq,
        body,
        isStatic,
        PropertyKind.Method,
        keySpan,
        isGenerator = isGenerator,
        isAsync = isAsyncMethod
      )
    } else {
      val value =
        if isOperator(Operator.Assign) then {
          advance()
          classFieldInitializerDepth += 1
          try parseAssignmentExpressionWithoutComma()
          finally classFieldInitializerDepth -= 1
        } else null
      FieldDefinition(key, value, isStatic, keySpan)
    }
  }

  private def parseFunctionParams(): Seq[BindingPattern] = {
    expectPunctuation(Punctuation.LeftParen)
    advance() // consume (
    val params = ArrayBuffer[BindingPattern]()
    if !isPunctuation(Punctuation.RightParen) then {
      var more = true
      while more do {
        if isOperator(Operator.Spread) then {
          val spreadSpan = current.span
          advance()
          params += RestElement(parseBindingPatternBase(), spreadSpan)
          if isOperator(Operator.Comma) then
            throw new RuntimeException("rest parameter must be the last parameter")
          more = false
        } else params += parseBindingPattern()
        if more && isOperator(Operator.Comma) then {
          advance()
          // A trailing comma terminates the formal parameter list.
          if isPunctuation(Punctuation.RightParen) then more = false
        }
        else more = false
      }
    }
    expectPunctuation(Punctuation.RightParen)
    advance() // consume )
    params.toSeq
  }

  private def validateFormalParameters(
      params: Seq[BindingPattern],
      strict: Boolean,
      hasUseStrictDirective: Boolean,
      forceUnique: Boolean = false,
      body: BlockStatement | Null = null
  ): Unit = {
    val simple = params.forall(_.isInstanceOf[Identifier])
    if hasUseStrictDirective && !simple then
      throw new RuntimeException(
        "use strict directive is not allowed with a non-simple parameter list"
      )
    val names = params.flatMap(bindingNames)
    if (strict || forceUnique || !simple) && names.distinct.length != names.length
    then throw new RuntimeException("duplicate formal parameter")
    if body != null then {
      val lexicalNames = body.statements
        .flatMap(directLexicalDeclarations(_, blockScope = false))
        .map(_._1)
        .toSet
      names.find(lexicalNames.contains).foreach { name =>
        throw new RuntimeException(
          s"formal parameter '$name' conflicts with a lexical declaration"
        )
      }
    }
  }

  /** Validate the formal parameter list of a getter/setter method. */
  private def validateAccessorParams(
      kind: PropertyKind,
      params: Seq[BindingPattern]
  ): Unit =
    kind match {
      case PropertyKind.Getter =>
        if params.nonEmpty then
          throw new RuntimeException(
            "SyntaxError: getter must not have any formal parameters"
          )
      case PropertyKind.Setter =>
        if params.length != 1 || params.head.isInstanceOf[RestElement] then
          throw new RuntimeException(
            "SyntaxError: setter must have exactly one formal parameter"
          )
      case _ => ()
    }

  private def parseMethodFunction(
      isGenerator: Boolean = false,
      isAsync: Boolean = false
  ): FunctionExpression = {
    withFunctionGrammarContext(
      isGenerator,
      isAsync,
      isMethodRoot = true
    ) {
      val params = parseFunctionParams()
      val savedStrict = currentStrictMode
      val hasUseStrictDirective = isUseStrictDirectiveAhead()
      if hasUseStrictDirective then currentStrictMode = true
      val body = parseBlockStatement()
      val (bodyStrict, remainingStatements) =
        Parser.extractStrictMode(body.statements)
      val finalBody = BlockStatement(remainingStatements.toSeq, body.span)
      val isStrict = savedStrict || hasUseStrictDirective || bodyStrict
      currentStrictMode = savedStrict
      validateFormalParameters(
        params,
        isStrict,
        bodyStrict,
        forceUnique = true,
        body = finalBody
      )
      FunctionExpression(
        null,
        params,
        finalBody,
        isGenerator,
        isAsync,
        isStrict,
        body.span
      )
    }
  }

  /** Parse a block statement */
  private def parseBlockStatement(): BlockStatement = {
    val startSpan = current.span
    expectPunctuation(Punctuation.LeftBrace)
    advance() // consume {
    val statements = ArrayBuffer[Statement]()
    while !isPunctuation(Punctuation.RightBrace) && current != EOF do
      statements += parseStatement()
    expectPunctuation(Punctuation.RightBrace)
    advance() // consume }
    val span = startSpan
    BlockStatement(statements.toSeq, span)
  }

  /** Parse an expression */
  private def parseExpression(): Expression =
    parseAssignmentExpression()

  /** Parse an assignment expression (includes ternary operator) */
  private def parseAssignmentExpression(): Expression = {
    // First parse assignment (including +=, -=, etc.)
    var left = parseAssignmentExpressionWithoutComma()

    // Check for comma operator (lowest precedence, can chain)
    while isOperator(Operator.Comma) do {
      advance() // consume comma
      val right = parseAssignmentExpressionWithoutComma()
      val span = left.span
      left = BinaryExpression(BinaryOperator.Comma, left, right, span)
    }

    left
  }

  /** Parse assignment expression without comma operator */
  private def parseAssignmentExpressionWithoutComma(): Expression = {
    // YieldExpression is an AssignmentExpression production. It is not a
    // general UnaryExpression, so constructs such as `void yield` are syntax
    // errors inside generators.
    if isKeyword(Keyword.Yield) && generatorFunctionDepth > 0 then
      return parseYieldExpression()

    // Check for destructuring assignment patterns on the left. Only attempt
    // the speculative pattern parse when the bracketed group is followed by
    // `=`; otherwise the exception-based backtracking would run for every
    // object/array literal, which is a major parser hot spot.
    if (isPunctuation(Punctuation.LeftBrace) || isPunctuation(
        Punctuation.LeftBracket
      )) && isBracketedGroupFollowedByAssign()
    then {
      val savedPos = pos
      try {
        val pattern = parseBindingPattern(allowDefault = false)
        if isOperator(Operator.Assign) then {
          advance()
          val right = parseAssignmentExpressionWithoutComma()
          return AssignmentExpression(pattern, right, pattern.span)
        } else pos = savedPos
      } catch {
        case _: Exception =>
          pos = savedPos
      }
    }

    // First check for ternary operator
    val left = parseConditionalExpression()

    // Check for compound assignment operators
    val op = current match {
      case OperatorToken(op, _)
          if op == Operator.Assign ||
            op == Operator.AddAssign ||
            op == Operator.SubAssign ||
            op == Operator.MulAssign ||
            op == Operator.DivAssign ||
            op == Operator.ModAssign ||
            op == Operator.BitwiseAndAssign ||
            op == Operator.BitwiseOrAssign ||
            op == Operator.XorAssign ||
            op == Operator.LeftShiftAssign ||
            op == Operator.RightShiftAssign ||
            op == Operator.UnsignedRightShiftAssign ||
            op == Operator.PowAssign ||
            op == Operator.LogicalAndAssign ||
            op == Operator.LogicalOrAssign ||
            op == Operator.NullishCoalesceAssign =>
        Some(op)
      case _ => None
    }

    if op.isDefined then {
      if left.isInstanceOf[NewTargetExpression] then
        throw new RuntimeException("new.target is not an assignment target")
      validateAssignmentPatternTarget(left)
      val assignmentSpan = current.span
      advance()
      val right =
        // AssignmentExpression is right-associative but does not include the
        // comma operator; comma belongs to the enclosing Expression grammar.
        parseAssignmentExpressionWithoutComma()
      val span = assignmentSpan

      // Desugar compound assignment: x += y  ->  x = x + y
      op.get match {
        case Operator.Assign =>
          AssignmentExpression(left, right, left.span)
        case Operator.AddAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.Add, left, right, span),
            span
          )
        case Operator.SubAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.Sub, left, right, span),
            span
          )
        case Operator.MulAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.Mul, left, right, span),
            span
          )
        case Operator.DivAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.Div, left, right, span),
            span
          )
        case Operator.ModAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.Mod, left, right, span),
            span
          )
        case Operator.BitwiseAndAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.And, left, right, span),
            span
          )
        case Operator.BitwiseOrAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.Or, left, right, span),
            span
          )
        case Operator.XorAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.Xor, left, right, span),
            span
          )
        case Operator.LeftShiftAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.Shl, left, right, span),
            span
          )
        case Operator.RightShiftAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.Sar, left, right, span),
            span
          )
        case Operator.UnsignedRightShiftAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.Shr, left, right, span),
            span
          )
        case Operator.PowAssign =>
          AssignmentExpression(
            left,
            BinaryExpression(BinaryOperator.Pow, left, right, span),
            span
          )
        case Operator.LogicalAndAssign =>
          LogicalAssignmentExpression(LogicalAssignmentOperator.And, left, right, span)
        case Operator.LogicalOrAssign =>
          LogicalAssignmentExpression(LogicalAssignmentOperator.Or, left, right, span)
        case Operator.NullishCoalesceAssign =>
          LogicalAssignmentExpression(LogicalAssignmentOperator.Nullish, left, right, span)
        case _ =>
          left // Should not happen
      }
    } else left
  }

  /** Parse conditional (ternary) expression: condition ? trueExpr : falseExpr
    */
  private def parseConditionalExpression(): Expression = {
    // Parse the condition (logical OR and below)
    var result = parseLogicalOrExpression()

    // Check for ternary operator (right-associative)
    if isPunctuation(Punctuation.Question) then {
      advance() // consume '?'
      // Consequent and alternate are AssignmentExpressions per the grammar
      // (not the wider Expression), so an unparenthesized comma terminates
      // the ternary. Using the comma-including parser here made
      // `{ f: () => a ? b : c, g: 2 }` swallow `, g` into the alternate.
      val consequent = parseAssignmentExpressionWithoutComma()
      expectPunctuation(Punctuation.Colon) // check for :
      advance() // consume :
      // Nested ternaries remain valid: a ? b : c ? d : e parses
      // right-associatively as a ? b : (c ? d : e).
      val alternate = parseAssignmentExpressionWithoutComma()
      val span = Span(
        result.span.start,
        alternate.span.end,
        result.span.line,
        result.span.column
      )
      result = ConditionalExpression(result, consequent, alternate, span)
    }

    result
  }

  /** Parse a logical OR or nullish coalescing expression */
  private def parseLogicalOrExpression(): Expression = {
    var left = parseLogicalAndExpression()
    while isOperator(Operator.LogicalOr) || isOperator(Operator.NullishCoalesce)
    do {
      val op = current match {
        case OperatorToken(Operator.LogicalOr, _) => BinaryOperator.LogicalOr
        case OperatorToken(Operator.NullishCoalesce, _) =>
          BinaryOperator.NullishCoalesce
        case _ => throw new RuntimeException("Expected || or ??")
      }
      advance()
      val right = parseLogicalAndExpression()
      val span = left.span
      left = BinaryExpression(op, left, right, span)
    }
    left
  }

  /** Parse a logical AND expression */
  private def parseLogicalAndExpression(): Expression = {
    var left = parseBitwiseOrExpression()
    while isOperator(Operator.LogicalAnd) do {
      advance()
      val right = parseBitwiseOrExpression()
      val span = left.span
      left = BinaryExpression(BinaryOperator.LogicalAnd, left, right, span)
    }
    left
  }

  /** Parse a bitwise OR expression */
  private def parseBitwiseOrExpression(): Expression = {
    var left = parseBitwiseXorExpression()
    while isOperator(Operator.BitwiseOr) do {
      advance()
      val right = parseBitwiseXorExpression()
      val span = left.span
      left = BinaryExpression(BinaryOperator.Or, left, right, span)
    }
    left
  }

  /** Parse a bitwise XOR expression */
  private def parseBitwiseXorExpression(): Expression = {
    var left = parseBitwiseAndExpression()
    while isOperator(Operator.Xor) do {
      advance()
      val right = parseBitwiseAndExpression()
      val span = left.span
      left = BinaryExpression(BinaryOperator.Xor, left, right, span)
    }
    left
  }

  /** Parse a bitwise AND expression */
  private def parseBitwiseAndExpression(): Expression = {
    var left = parseEqualityExpression()
    while isOperator(Operator.BitwiseAnd) do {
      advance()
      val right = parseEqualityExpression()
      val span = left.span
      left = BinaryExpression(BinaryOperator.And, left, right, span)
    }
    left
  }

  /** Parse an equality expression */
  private def parseEqualityExpression(): Expression = {
    var left = parseRelationalExpression()
    while isOperator(Operator.Eq) || isOperator(Operator.Neq) ||
      isOperator(Operator.StrictEq) || isOperator(Operator.StrictNeq)
    do {
      val operatorSpan = current.span
      val op = current match {
        case OperatorToken(o, _) =>
          o match {
            case Operator.Eq        => BinaryOperator.Eq
            case Operator.Neq       => BinaryOperator.Neq
            case Operator.StrictEq  => BinaryOperator.StrictEq
            case Operator.StrictNeq => BinaryOperator.StrictNeq
            case _ => throw new RuntimeException(s"Expected equality operator")
          }
        case _ => throw new RuntimeException(s"Expected equality operator")
      }
      advance()
      val right = parseRelationalExpression()
      val span = left.span
      left = BinaryExpression(op, left, right, span)
    }
    left
  }

  /** Parse a relational expression */
  private def parseRelationalExpression(): Expression = {
    var left = parseShiftExpression()
    while isOperator(Operator.Lt) || isOperator(Operator.Lte) ||
      isOperator(Operator.Gt) || isOperator(Operator.Gte) ||
      isKeyword(Keyword.Instanceof) || (allowInOperator && isKeyword(
        Keyword.In
      ))
    do {
      val op = current match {
        case OperatorToken(o, _) =>
          o match {
            case Operator.Lt  => BinaryOperator.Lt
            case Operator.Lte => BinaryOperator.Lte
            case Operator.Gt  => BinaryOperator.Gt
            case Operator.Gte => BinaryOperator.Gte
            case _            =>
              throw new RuntimeException(s"Expected relational operator")
          }
        case KeywordToken(k, _) =>
          k match {
            case Keyword.Instanceof => BinaryOperator.Instanceof
            case Keyword.In         => BinaryOperator.In
            case _                  =>
              throw new RuntimeException(s"Expected relational operator")
          }
        case _ => throw new RuntimeException(s"Expected relational operator")
      }
      advance()
      val right = parseShiftExpression()
      val span = left.span
      left = BinaryExpression(op, left, right, span)
    }
    left
  }

  /** Parse an additive expression */
  private def parseAdditiveExpression(): Expression = {
    var left = parseMultiplicativeExpression()
    // `+`/`-` continue the expression across a line break (ASI only applies
    // when the operator cannot continue the expression). An arrow function is
    // only an AssignmentExpression, so `() => {}\n+1` is two statements.
    while (isOperator(Operator.Add) || isOperator(Operator.Sub)) &&
      !(wasLineTerminatorBefore && left.isInstanceOf[ArrowFunctionExpression])
    do {
      val op = current match {
        case OperatorToken(o, _) =>
          o match {
            case Operator.Add => BinaryOperator.Add
            case Operator.Sub => BinaryOperator.Sub
            case _ => throw new RuntimeException(s"Expected additive operator")
          }
        case _ => throw new RuntimeException(s"Expected additive operator")
      }
      advance()
      val right = parseMultiplicativeExpression()
      val span = left.span
      left = BinaryExpression(op, left, right, span)
    }
    left
  }

  /** Parse a shift expression */
  private def parseShiftExpression(): Expression = {
    var left = parseAdditiveExpression()
    while isOperator(Operator.LeftShift) || isOperator(Operator.RightShift) ||
      isOperator(Operator.UnsignedRightShift)
    do {
      val op = current match {
        case OperatorToken(o, _) =>
          o match {
            case Operator.LeftShift          => BinaryOperator.Shl
            case Operator.RightShift         => BinaryOperator.Sar
            case Operator.UnsignedRightShift => BinaryOperator.Shr
            case _ => throw new RuntimeException(s"Expected shift operator")
          }
        case _ => throw new RuntimeException(s"Expected shift operator")
      }
      advance()
      val right = parseAdditiveExpression()
      val span = left.span
      left = BinaryExpression(op, left, right, span)
    }
    left
  }

  /** Parse a multiplicative expression */
  private def parseMultiplicativeExpression(): Expression = {
    var left = parseExponentiationExpression()
    while isOperator(Operator.Mul) || isOperator(Operator.Div) || isOperator(
        Operator.Mod
      )
    do {
      val operatorSpan = current.span
      val op = current match {
        case OperatorToken(o, _) =>
          o match {
            case Operator.Mul => BinaryOperator.Mul
            case Operator.Div => BinaryOperator.Div
            case Operator.Mod => BinaryOperator.Mod
            case _            =>
              throw new RuntimeException(s"Expected multiplicative operator")
          }
        case _ =>
          throw new RuntimeException(s"Expected multiplicative operator")
      }
      advance()
      val right = parseExponentiationExpression()
      left = BinaryExpression(op, left, right, operatorSpan)
    }
    left
  }

  /** Parse an exponentiation expression (right-associative) */
  private def parseExponentiationExpression(): Expression = {
    var left = parseUnaryExpression()
    if isOperator(Operator.Pow) then {
      val operatorSpan = current.span
      advance()
      val right =
        parseExponentiationExpression() // Right-recursive for right-associativity
      left = BinaryExpression(BinaryOperator.Pow, left, right, operatorSpan)
    }
    left
  }

  /** Parse a new expression (new Constructor()) */
  private def parseNewExpression(): Expression =
    // Check if we have 'new' keyword
    current match {
      case KeywordToken(Keyword.New, span) =>
        advance()

        if isOperator(Operator.Dot) then {
          advance()
          current match {
            case IdentifierToken("target", _, _) => advance()
            case _ =>
              throw new RuntimeException("Expected target after new.")
          }
          if newTargetContextDepth == 0 then
            throw new RuntimeException(
              "new.target is only valid inside a function"
            )
          return parsePostfixTail(NewTargetExpression(span))
        }

        // NewExpression consumes a MemberExpression before its Arguments.
        // In particular, `new obj.C()` means `new (obj.C)()`, not
        // `(new obj).C()`. Calls are deliberately excluded from this tail.
        val calleeBase = current match {
          case KeywordToken(Keyword.New, _) =>
            parseNewExpression() // new new Foo()
          case IdentifierToken(_, _, _) |
              KeywordToken(Keyword.Function | Keyword.Class | Keyword.This, _) |
              PunctuationToken(
                Punctuation.LeftParen | Punctuation.LeftBracket |
                  Punctuation.LeftBrace,
                _
              ) =>
            parsePrimaryExpression()
          case _ =>
            throw new RuntimeException(
              s"Expected constructor after 'new', got: $current"
            )
        }

        var callee = calleeBase
        var parseMembers = true
        while parseMembers do
          if isOperator(Operator.Dot) then {
            advance()
            val property = current match {
              case PrivateIdentifierToken(name, propertySpan) =>
                advance()
                referencePrivateName(name)
                PrivateIdentifier(name, propertySpan)
              case IdentifierToken(name, propertySpan, _) =>
                advance()
                Identifier(name, propertySpan)
              case KeywordToken(kind, propertySpan) =>
                advance()
                Identifier(kind.toString.toLowerCase, propertySpan)
              case _ =>
                throw new RuntimeException("Expected identifier after '.'")
            }
            callee = MemberExpression(
              callee,
              property,
              computed = false,
              property.span
            )
          } else if isPunctuation(Punctuation.LeftBracket) then {
            advance()
            val property = parseExpression()
            expectPunctuation(Punctuation.RightBracket)
            advance()
            callee = MemberExpression(
              callee,
              property,
              computed = true,
              property.span
            )
          } else parseMembers = false

        // Parse arguments for new Constructor(arg1, arg2, ...)
        val arguments = current match {
          case PunctuationToken(Punctuation.LeftParen, _) =>
            advance() // Skip '('
            val args = parseArguments()
            advance() // Skip ')'
            args
          case _ =>
            Seq.empty // new Foo without arguments
        }

        val expr = NewExpression(callee, arguments, span)
        // Allow member access/calls after `new` (e.g., new P().static())
        parsePostfixTail(expr)

      case _ =>
        // Not a new expression, parse as postfix expression
        parsePostfixExpression()
    }

  /** Parse a unary expression */
  private def isSimpleAssignmentTarget(expression: Expression): Boolean =
    expression match {
      case _: Identifier => true
      case member: MemberExpression => !member.optional
      case _ => false
    }

  private def parseUnaryExpression(): Expression =
    current match {
      case OperatorToken(op, _)
          if op == Operator.Add || op == Operator.Sub ||
            op == Operator.Not || op == Operator.BitwiseNot =>
        val operatorSpan = current.span
        val unaryOp = op match {
          case Operator.Add        => UnaryOperator.Plus
          case Operator.Sub        => UnaryOperator.Minus
          case Operator.Not        => UnaryOperator.Not
          case Operator.BitwiseNot => UnaryOperator.BitwiseNot
          case _ => throw new RuntimeException(s"Expected unary operator")
        }
        advance()
        val argument = parseUnaryExpression()
        UnaryExpression(unaryOp, argument, true, operatorSpan)
      case OperatorToken(op, _)
          if op == Operator.PreInc || op == Operator.PreDec ||
            op == Operator.PostInc || op == Operator.PostDec =>
        val operatorSpan = current.span
        val unaryOp = op match {
          case Operator.PreInc  => UnaryOperator.PreInc
          case Operator.PreDec  => UnaryOperator.PreDec
          case Operator.PostInc => UnaryOperator.PostInc
          case Operator.PostDec => UnaryOperator.PostDec
          case _                =>
            throw new RuntimeException(s"Expected increment/decrement operator")
        }
        advance()
        val argument = parseUnaryExpression()
        if !isSimpleAssignmentTarget(argument) then
          throw new RuntimeException("invalid update target")
        UnaryExpression(unaryOp, argument, true, operatorSpan)
      case KeywordToken(Keyword.Typeof, _) =>
        advance()
        val argument = parseUnaryExpression()
        val span = argument.span
        UnaryExpression(UnaryOperator.Typeof, argument, true, span)
      case KeywordToken(Keyword.Void, _) =>
        advance()
        val argument = parseUnaryExpression()
        val span = argument.span
        UnaryExpression(UnaryOperator.Void, argument, true, span)
      case KeywordToken(Keyword.Delete, _) =>
        advance()
        val argument = parseUnaryExpression()
        val span = argument.span
        UnaryExpression(UnaryOperator.Delete, argument, true, span)
      case KeywordToken(Keyword.Yield, _) =>
        if generatorFunctionDepth > 0 || currentStrictMode then
          throw new RuntimeException(
            "yield cannot be used as an identifier reference here"
          )
        val span = current.span
        advance()
        Identifier("yield", span)
      case KeywordToken(Keyword.Await, _) =>
        if awaitExpressionAllowed then parseAwaitExpression()
        else {
          // The module top-level Await capability does not reach nested
          // non-async functions: `await` is an IdentifierReference there.
          val span = current.span
          advance()
          Identifier("await", span)
        }
      case _ =>
        parseNewExpression()
    }

  /** Parse a yield expression yield yield expression yield* expression (yield
    * delegation)
    */
  private def parseYieldExpression(): Expression = {
    val startSpan = current.span
    expectKeyword(Keyword.Yield)
    advance()
    val hasLineTerminator = wasLineTerminatorBefore
    // Check for yield* (yield delegation)
    val isDelegate = isOperator(Operator.Mul)
    if isDelegate && hasLineTerminator then
      throw new RuntimeException("Line terminator is not allowed before yield*")
    if isDelegate then advance()
    // Check if there's an argument
    val argument =
      if !hasLineTerminator && isExpressionStart() then
        // YieldExpression's operand is an AssignmentExpression, which does
        // not include the comma operator. In particular, the comma after
        // `...yield yield,` belongs to the surrounding object literal.
        Some(parseAssignmentExpressionWithoutComma())
      else None
    val span = startSpan
    YieldExpression(argument.orNull, isDelegate, span)
  }

  /** Parse an await expression await expression
    */
  private def parseAwaitExpression(): Expression = {
    val startSpan = current.span
    expectKeyword(Keyword.Await)
    advance()
    // Parse the argument (unary expression for correct precedence)
    val argument = parseUnaryExpression()
    AwaitExpression(argument, startSpan)
  }

  /** Check if the current token can start an expression */
  private def isExpressionStart(): Boolean = current match {
    case NumberToken(_, _, _) | StringToken(_, _, _) | RegexToken(_, _, _) |
        BigIntToken(_, _) =>
      true
    case IdentifierToken(_, _, _) => true
    case KeywordToken(k, _)    =>
      k match {
        case Keyword.Function | Keyword.New | Keyword.This | Keyword.Typeof |
            Keyword.Void | Keyword.Delete | Keyword.Yield | Keyword.Await |
            Keyword.True | Keyword.False | Keyword.Null | Keyword.Undefined =>
          true
        case _ => false
      }
    case OperatorToken(op, _) =>
      op match {
        case Operator.Add | Operator.Sub | Operator.Not | Operator.BitwiseNot |
            Operator.PreInc | Operator.PreDec =>
          true
        case _ => false
      }
    case PunctuationToken(p, _) =>
      p match {
        case Punctuation.LeftParen | Punctuation.LeftBracket |
            Punctuation.LeftBrace =>
          true
        case _ => false
      }
    case _ => false
  }

  /** Parse a postfix expression */
  private def parsePostfixExpression(): Expression = {
    val left = parsePrimaryExpression()
    parsePostfixTail(left)
  }

  private def parsePostfixTail(start: Expression): Expression = {
    var left = start

    var continue = true
    while continue do
      // Check for postfix increment/decrement
      // Note: Lexer returns PreInc/PreDec for both prefix and postfix
      // We need to check for them here to handle postfix (a++, a--)
      // A postfix operator cannot follow a line terminator: `x\n++y` is two
      // statements (ASI), so only take the postfix path when `++`/`--` is on
      // the same line as the preceding expression.
      if !wasLineTerminatorBefore &&
        (isOperator(Operator.PreInc) || isOperator(Operator.PreDec) ||
          isOperator(Operator.PostInc) || isOperator(Operator.PostDec))
      then {
        if !isSimpleAssignmentTarget(left) then
          throw new RuntimeException("invalid update target")
        val operatorSpan = current.span
        val op = current match {
          case OperatorToken(o, _) =>
            o match {
              case Operator.PreInc  => UnaryOperator.PostInc
              case Operator.PreDec  => UnaryOperator.PostDec
              case Operator.PostInc => UnaryOperator.PostInc
              case Operator.PostDec => UnaryOperator.PostDec
              case _ => throw new RuntimeException(s"Expected postfix operator")
            }
          case _ => throw new RuntimeException(s"Expected postfix operator")
        }
        advance()
        left = UnaryExpression(op, left, false, operatorSpan)
      }
      // A line terminator before `(` does not prevent a call: `f\n(x)` is a
      // call expression per the grammar. An arrow function cannot be called
      // without parentheses, so `() => {}\n() => {}` stays two statements.
      else if isPunctuation(Punctuation.LeftParen) &&
        !(wasLineTerminatorBefore && left.isInstanceOf[ArrowFunctionExpression])
      then {
        val callSiteSpan = current.span
        if left.isInstanceOf[SuperExpression] then {
          if classFieldInitializerDepth > 0 &&
            fieldInitializerFunctionBoundaryDepth == 0
          then
            throw new RuntimeException(
              "super() is not allowed in a class field initializer"
            )
          if superCallAllowedDepth == 0 || superCallNestedFunctionDepth > 0 then
            throw new RuntimeException(
              "super() is only allowed directly in a derived constructor"
            )
        }
        advance()
        val arguments = parseCallArguments()
        expectPunctuation(Punctuation.RightParen)
        advance() // consume )
        left = CallExpression(left, arguments.toSeq, callSiteSpan)
      }
      // Tagged template literal: tag`a${b}c`
      else if current.isInstanceOf[TemplateToken] then {
        val template = parseTemplateLiteralToken()
        left = TaggedTemplateExpression(left, template, left.span)
      }
      // Optional chaining: ?. ?.[ ?.(
      else if isPunctuation(Punctuation.Question) then
        peek() match {
          case OperatorToken(Operator.Dot, _) =>
            advance() // consume ?
            advance() // consume .
            if isPunctuation(Punctuation.LeftBracket) then {
              // ?.[ - optional computed member access
              advance()
              val property = parseExpression()
              expectPunctuation(Punctuation.RightBracket)
              advance() // consume ]
              val span = property.span
              left = MemberExpression(
                left,
                property,
                computed = true,
                span,
                optional = true
              )
            } else if isPunctuation(Punctuation.LeftParen) then {
              // ?.( - optional call expression
              advance()
              val arguments = parseCallArguments()
              expectPunctuation(Punctuation.RightParen)
              advance() // consume )
              val span = left.span
              left =
                CallExpression(left, arguments.toSeq, span, optional = true)
            } else {
              // ?. - optional property access
              val property = current match {
                case PrivateIdentifierToken(name, span) =>
                  // Private field access (obj?.#field)
                  advance()
                  referencePrivateName(name)
                  PrivateIdentifier(name, span)
                case IdentifierToken(name, span, _) =>
                  advance()
                  Identifier(name, span)
                case KeywordToken(kind, span) =>
                  advance()
                  Identifier(kind.toString.toLowerCase, span)
                case _ =>
                  throw new RuntimeException(s"Expected identifier after '?.'")
              }
              val span = property.span
              left = MemberExpression(
                left,
                property,
                computed = false,
                span,
                optional = true
              )
            }
          case _ =>
            continue = false
        }
      // Check for member expression (dot notation)
      else if isOperator(Operator.Dot) then {
        val accessSpan = current.span
        advance()
        val property = current match {
          case PrivateIdentifierToken(name, span) =>
            // Private field access (this.#field)
            advance()
            referencePrivateName(name)
            PrivateIdentifier(name, span)
          case IdentifierToken(name, span, _) =>
            advance()
            Identifier(name, span)
          case KeywordToken(kind, span) =>
            advance()
            Identifier(kind.toString.toLowerCase, span)
          case _ =>
            throw new RuntimeException(s"Expected identifier after '.'")
        }
        left = MemberExpression(left, property, computed = false, accessSpan)
      }
      // Check for member expression (bracket notation)
      else if isPunctuation(Punctuation.LeftBracket) then {
        val accessSpan = current.span
        advance()
        val property = parseExpression()
        expectPunctuation(Punctuation.RightBracket)
        advance() // consume ]
        left = MemberExpression(left, property, computed = true, accessSpan)
      } else continue = false

    left
  }

  private def parseTemplateLiteralToken(): TemplateLiteral = current match {
    case TemplateToken(parts, expressions, span) =>
      advance()
      TemplateLiteral(
        parts.map(p => TemplateElement(p.cooked, p.raw)).toSeq,
        expressions.map(parseTemplateExpressionSource).toSeq,
        span
      )
    case _ => throw new RuntimeException(s"Expected template literal")
  }

  private def parseTemplateExpressionSource(source: String): Expression = {
    val tokens = quickjs.lexer.Lexer(source).tokenize()
    val parser = Parser(tokens)
    val expr = parser.parseExpression()
    parser.expectToken(EOF)
    expr
  }

  private def parseCallArguments(): ArrayBuffer[Expression] = {
    val arguments = ArrayBuffer[Expression]()
    if !isPunctuation(Punctuation.RightParen) then {
      var more = true
      while more do {
        if isOperator(Operator.Spread) then {
          val spreadSpan = current.span
          advance()
          arguments += SpreadElement(
            parseAssignmentExpressionWithoutComma(),
            spreadSpan
          )
        } else {
          // Don't parse comma operator - comma in function arguments is a separator
          arguments += parseAssignmentExpressionWithoutComma()
        }
        if isOperator(Operator.Comma) then {
          advance()
          // Trailing comma is allowed: if next token is ), end the argument list
          if isPunctuation(Punctuation.RightParen) then more = false
        } else more = false
      }
    }
    arguments
  }

  /** Parse an object literal */
  private def parseObjectLiteral(): ObjectLiteral = {
    val startSpan = current.span
    expectPunctuation(Punctuation.LeftBrace)
    advance() // consume {

    val properties = ArrayBuffer[Property | SpreadElement]()
    var hasProtoDataProperty = false
    while !isPunctuation(Punctuation.RightBrace) && current != EOF do {
      if isOperator(Operator.Spread) then {
        val spreadSpan = current.span
        advance() // consume ...
        val argument = parseAssignmentExpressionWithoutComma()
        properties += SpreadElement(argument, spreadSpan)
      } else {
        val property = parseProperty()
        property match {
          case Property(key, _, PropertyKind.Value, false, _, false)
              if key match {
                case Identifier("__proto__", _) => true
                case s: String                   => s == "__proto__"
                case _                           => false
              } =>
            if hasProtoDataProperty then
              throw new RuntimeException(
                "SyntaxError: duplicate __proto__ fields are not allowed in object literals"
              )
            hasProtoDataProperty = true
          case _ => ()
        }
        properties += property
      }
      if isOperator(Operator.Comma) then advance()
      else if !isPunctuation(Punctuation.RightBrace) then
        throw new RuntimeException(
          s"Expected ',' or '}' after object property but got $current"
        )
    }

    expectPunctuation(Punctuation.RightBrace)
    advance() // consume }

    ObjectLiteral(properties.toSeq, startSpan)
  }

  /** Parse a property in an object literal */
  private def parseProperty(): Property = {
    def parsePropertyKey(): (Identifier | String | Expression, Boolean, Span) =
      current match {
        case IdentifierToken(name, span, _) =>
          advance()
          (Identifier(name, span), false, span)
        case KeywordToken(kind, span) =>
          advance()
          (Identifier(kind.toString.toLowerCase, span), false, span)
        case StringToken(value, span, _) =>
          advance()
          (value, false, span)
        case NumberToken(v, span, _) =>
          advance()
          // Convert number to string property key (integer values without decimal)
          val keyStr = numberPropertyKey(v)
          (keyStr, false, span)
        case BigIntToken(v, span) =>
          advance()
          (v.toString, false, span)
        case PunctuationToken(Punctuation.LeftBracket, span) =>
          // Computed property name: [expr]
          advance()
          val keyExpr =
            parseAssignmentExpressionWithoutComma() // Don't parse comma in computed property
          expectPunctuation(Punctuation.RightBracket)
          advance()
          (keyExpr, true, span)
        case _ =>
          throw new RuntimeException(
            s"Expected property key (identifier, string, or computed property) but got $current"
          )
      }

    def isAccessorCandidate: Boolean =
      current match {
        case IdentifierToken(name, _, false) if name == "get" || name == "set" =>
          peek() match {
            case PunctuationToken(Punctuation.LeftBracket, _) => true
            case IdentifierToken(_, _, _) | KeywordToken(_, _) |
                StringToken(_, _, _) | NumberToken(_, _, _) | BigIntToken(_, _) =>
              peek(2) match {
                case PunctuationToken(Punctuation.LeftParen, _) => true
                case _                                          => false
              }
            case _ => false
          }
        case _ => false
      }

    val isAsyncMethod =
      current match {
        case IdentifierToken("async", _, false) | KeywordToken(Keyword.Async, _) =>
          peek() match {
            case OperatorToken(Operator.Mul, _) => true
            case IdentifierToken(_, _, _) | KeywordToken(_, _) |
                StringToken(_, _, _) | NumberToken(_, _, _) | BigIntToken(_, _) |
                PunctuationToken(Punctuation.LeftBracket, _) =>
              keyFollowedByParen(pos + 1)
            case _ => false
          }
        case _ => false
      }
    if isAsyncMethod then advance()

    val isGenerator = isOperator(Operator.Mul)
    if isGenerator then advance()

    if !isAsyncMethod && !isGenerator && isAccessorCandidate then {
      val accessorName = current match {
        case IdentifierToken(name, _, _) => name
        case _                        => ""
      }
      advance()
      val (accessorKey, computed, keySpan) = parsePropertyKey()
      val func = parseMethodFunction()
      val kind =
        if accessorName == "get" then PropertyKind.Getter
        else PropertyKind.Setter
      validateAccessorParams(kind, func.params)
      return Property(accessorKey, func, kind, computed, keySpan)
    }

    val (key, computed, keySpan) = parsePropertyKey()

    if isPunctuation(Punctuation.LeftParen) then {
      val func = parseMethodFunction(isGenerator, isAsyncMethod)
      return Property(key, func, PropertyKind.Method, computed, keySpan)
    }

    if isPunctuation(Punctuation.Colon) then {
      advance()
      val value = parseAssignmentExpressionWithoutComma()
      return Property(key, value, PropertyKind.Value, computed, keySpan)
    }

    key match {
      case Identifier(name, span) =>
        // Shorthand properties are IdentifierReferences, which may not be
        // ReservedWords (unlike a property name with a colon).
        if Parser.isReservedWordForIdentifierReference(name, currentStrictMode)
        then
          throw new RuntimeException(
            s"SyntaxError: '$name' is not a valid identifier reference"
          )
        val value = Identifier(name, span)
        if isOperator(Operator.Assign) then {
          // CoverInitializedName: `{ a = 1 }`. Valid only when the literal is
          // used as a destructuring pattern; the compiler rejects it when it
          // reaches object-literal expression evaluation.
          advance()
          val defaultValue = parseAssignmentExpressionWithoutComma()
          Property(
            key,
            AssignmentExpression(value, defaultValue, span),
            PropertyKind.Value,
            computed,
            keySpan,
            shorthand = true
          )
        } else
          Property(
            key,
            value,
            PropertyKind.Value,
            computed,
            keySpan,
            shorthand = true
          )
      case _ =>
        throw new RuntimeException(s"Expected ':' after property key")
    }
  }

  /** Parse an array literal */
  private def parseArrayLiteral(): ArrayLiteral = {
    val startSpan = current.span
    expectPunctuation(Punctuation.LeftBracket)
    advance() // consume [

    val elements = ArrayBuffer[Expression | Null]()
    var trailingCommaAfterSpread = false
    while !isPunctuation(Punctuation.RightBracket) && current != EOF do
      // Check if there's an elision (empty element) indicated by leading comma
      if isOperator(Operator.Comma) then {
        elements += null // Elision
        advance()
      } else if isOperator(Operator.Spread) then {
        val spreadSpan = current.span
        advance()
        val argument = parseAssignmentExpressionWithoutComma()
        elements += SpreadElement(argument, spreadSpan)
        // A trailing comma (and any following elisions) is valid after a
        // spread element in an array literal, e.g. [...a,]. If the literal is
        // later used as an assignment pattern the trailing comma is rejected
        // (see the assignment-target validation).
        if isOperator(Operator.Comma) then {
          advance()
          if isPunctuation(Punctuation.RightBracket) then
            trailingCommaAfterSpread = true
          else if isOperator(Operator.Comma) then {
            elements += null // Elision
            advance()
          }
        }
      } else if isPunctuation(Punctuation.RightBracket) then
        // Trailing comma - will exit loop
        ()
      else {
        // Parse the element expression (don't parse comma - comma is a separator)
        elements += parseAssignmentExpressionWithoutComma()
        // Check for comma after element
        if isOperator(Operator.Comma) then {
          advance()
          // Check if there's another comma (elision) or right bracket (trailing)
          if isOperator(Operator.Comma) then {
            elements += null // Elision
            advance()
          }
          // Continue to next element
        }
      }

    expectPunctuation(Punctuation.RightBracket)
    advance() // consume ]

    ArrayLiteral(elements.toSeq, startSpan, trailingCommaAfterSpread)
  }

  private def parseBindingPattern(
      allowDefault: Boolean = true
  ): BindingPattern = {
    val target = parseBindingPatternBase()
    if allowDefault && isOperator(Operator.Assign) then {
      advance()
      val defaultValue = parseAssignmentExpressionWithoutComma()
      BindingAssignment(target, defaultValue, target.span)
    } else target
  }

  private def parseBindingPatternBase(): BindingPattern = current match {
    case IdentifierToken(name, span, _) =>
      validateBindingIdentifier(name, span)
      advance()
      Identifier(name, span)
    case KeywordToken(k, span) =>
      val name = k.toString.toLowerCase
      validateBindingIdentifier(name, span)
      advance()
      Identifier(name, span)
    case StringToken(value, span, _) =>
      advance()
      Identifier(value, span)
    case PunctuationToken(Punctuation.LeftBracket, _) =>
      parseArrayPattern()
    case PunctuationToken(Punctuation.LeftBrace, _) =>
      parseObjectPattern()
    case _ =>
      throw new RuntimeException(s"Expected binding pattern but got $current")
  }

  private def parseArrayPattern(): ArrayPattern = {
    val startSpan = current.span
    expectPunctuation(Punctuation.LeftBracket)
    advance() // consume [

    val elements = ArrayBuffer[BindingPattern | Null]()
    while !isPunctuation(Punctuation.RightBracket) && current != EOF do
      if isOperator(Operator.Comma) then {
        elements += null
        advance()
      } else if isPunctuation(Punctuation.RightBracket) then ()
      else if isOperator(Operator.Spread) then {
        // Rest element: ...pattern
        val spreadSpan = current.span
        advance() // consume ...
        val argument = parseBindingPatternBase()
        elements += RestElement(argument, spreadSpan)
        if !isPunctuation(Punctuation.RightBracket) then
          throw new RuntimeException("rest element must be the last one")
      } else {
        elements += parseBindingPattern()
        if isOperator(Operator.Comma) then advance()
      }

    expectPunctuation(Punctuation.RightBracket)
    advance() // consume ]

    ArrayPattern(elements.toSeq, startSpan)
  }

  private def parseObjectPattern(): ObjectPattern = {
    val startSpan = current.span
    expectPunctuation(Punctuation.LeftBrace)
    advance() // consume {

    val properties = ArrayBuffer[BindingProperty]()
    var restElement: RestElement | Null = null
    while !isPunctuation(Punctuation.RightBrace) && current != EOF do {
      if isOperator(Operator.Spread) then {
        // Rest element: ...identifier
        val spreadSpan = current.span
        advance() // consume ...
        val argument = parseBindingPatternBase()
        restElement = RestElement(argument, spreadSpan)
        if !isPunctuation(Punctuation.RightBrace) then
          throw new RuntimeException("assignment rest property must be last")
      } else {
        val (key, keySpan) = current match {
          case IdentifierToken(name, span, _) =>
            advance()
            (Identifier(name, span), span)
          case KeywordToken(kind, span) =>
            advance()
            (Identifier(kind.toString.toLowerCase, span), span)
          case StringToken(value, span, _) =>
            advance()
            (value, span)
          case NumberToken(v, span, _) =>
            advance()
            val keyStr = numberPropertyKey(v)
            (keyStr, span)
          case BigIntToken(v, span) =>
            advance()
            (v.toString, span)
          case PunctuationToken(Punctuation.LeftBracket, span) =>
            advance()
            val expression = parseAssignmentExpressionWithoutComma()
            expectPunctuation(Punctuation.RightBracket)
            advance()
            (ComputedPropertyName(expression, span), span)
          case _ =>
            throw new RuntimeException(
              s"Expected property key in object pattern but got $current"
            )
        }

        val value =
          if isPunctuation(Punctuation.Colon) then {
            advance()
            parseBindingPattern()
          } else if isOperator(Operator.Assign) then
            key match {
              case id: Identifier =>
                advance()
                val defaultValue = parseAssignmentExpressionWithoutComma()
                BindingAssignment(id, defaultValue, id.span)
              case _ =>
                throw new RuntimeException(
                  "Invalid default assignment in object pattern"
                )
            }
          else
            key match {
              case id: Identifier =>
                validateBindingIdentifier(id.name, id.span)
                id
              case _              =>
                throw new RuntimeException(
                  "Invalid shorthand property in object pattern"
                )
            }

        properties += BindingProperty(key, value, keySpan)
      }
      if isOperator(Operator.Comma) then advance()
    }

    expectPunctuation(Punctuation.RightBrace)
    advance() // consume }

    ObjectPattern(properties.toSeq, restElement, startSpan)
  }

  /** Parse a primary expression */
  private def parsePrimaryExpression(): Expression = current match {
    case NumberToken(v, span, legacy) =>
      if legacy && currentStrictMode then
        throw new RuntimeException(
          "SyntaxError: Legacy octal literals are not allowed in strict mode"
        )
      advance()
      Literal(JSValue.fromDouble(v), span)

    case BigIntToken(v, span) =>
      advance()
      Literal(JSValue.BigInt(v), span)

    case StringToken(v, span, legacyEscape) =>
      if legacyEscape && currentStrictMode then
        throw new RuntimeException(
          "SyntaxError: Legacy octal escape sequences are not allowed in strict mode"
        )
      advance()
      Literal(JSValue.fromString(v), span)

    case RegexToken(body, flags, span) =>
      if flags.exists(flag => flag == 'u' || flag == 'v') then {
        var escaped = false
        var classDepth = 0
        body.foreach { current =>
          if escaped then escaped = false
          else if current == '\\' then escaped = true
          else if current == '[' then classDepth += 1
          else if current == ']' then
            if classDepth == 0 then
              throw new RuntimeException(
                s"SyntaxError: unmatched ']' in regular expression at $span"
              )
            else classDepth -= 1
        }
      }
      advance()
      val patternLiteral = Literal(JSValue.fromString(body), span)
      val args =
        if flags.nonEmpty then
          Seq(patternLiteral, Literal(JSValue.fromString(flags), span))
        else Seq(patternLiteral)
      NewExpression(Identifier("RegExp", span), args, span)

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

    case KeywordToken(Keyword.Super, span) =>
      val allowedByFieldInitializer =
        classFieldInitializerDepth > 0 &&
          fieldInitializerFunctionBoundaryDepth == 0
      val allowedByMethod =
        superPropertyContextDepth > 0 &&
          superPropertyNestedFunctionDepth == 0
      if !allowedByFieldInitializer && !allowedByMethod then
        throw new RuntimeException(
          "super is not allowed outside a method or class field initializer"
        )
      advance()
      SuperExpression(span)

    case KeywordToken(Keyword.Async, startSpan) =>
      // Check for async function expressions and async arrows. A line terminator
      // between `async` and `function` is rejected earlier by the lexer/parser's
      // normal expression boundary handling.
      peek() match {
        case KeywordToken(Keyword.Function, _) =>
          parseFunctionExpression()
        case PunctuationToken(Punctuation.LeftParen, _) =>
          // `async(args)` is an ordinary call unless the matching `)` is
          // followed by `=>` (and there is no line terminator after `async`).
          if current.span.line != peek().span.line ||
            !arrowAfterMatchingParen(pos + 1)
          then {
            advance()
            Identifier("async", startSpan)
          } else withFunctionGrammarContext(false, true, isArrow = true) {
            advance() // consume async
            advance() // consume (
            val params = parseArrowFunctionParams()
            expectPunctuation(Punctuation.RightParen)
            advance() // consume )
            if !isOperator(Operator.Arrow) then
              throw new RuntimeException(
                s"Expected => after async arrow function parameters, got: $current"
              )
            advance() // consume =>
            val (body, bodyStrict) = parseArrowFunctionBodyWithStrict()
            val isStrict = currentStrictMode || bodyStrict
            validateArrowParameters(params, body, isStrict, bodyStrict)
            ArrowFunctionExpression(
              params,
              body,
              true,
              isStrict,
              startSpan
            )
          }
        case IdentifierToken(name, nameSpan, _) =>
          // Could be: async x => body or async function ...
          peek(2) match {
            case OperatorToken(Operator.Arrow, _) =>
              // async x => body
              withFunctionGrammarContext(false, true, isArrow = true) {
                validateBindingIdentifier(name, nameSpan)
                advance() // consume async
                advance() // consume identifier
                advance() // consume =>
                val params = Seq(Identifier(name, nameSpan))
                val (body, bodyStrict) = parseArrowFunctionBodyWithStrict()
                val isStrict = currentStrictMode || bodyStrict
                validateArrowParameters(params, body, isStrict, bodyStrict)
                ArrowFunctionExpression(
                  params,
                  body,
                  true,
                  isStrict,
                  startSpan
                )
              }
            case _ =>
              // Not an async arrow function - treat async as identifier (e.g., var async = 1)
              advance()
              Identifier("async", startSpan)
          }
        case _ =>
          // Not an async arrow function - treat async as identifier
          advance()
          Identifier("async", startSpan)
      }

    case IdentifierToken(name, span, _) =>
      // Check for arrow function: x => body (single parameter without parens)
      // Peek to see if next token is =>
      val nextTok = peek()
      nextTok match {
        case OperatorToken(Operator.Arrow, _) =>
          advance() // consume identifier
          advance() // consume =>
          val params = Seq(Identifier(name, span))
          val (body, bodyStrict) = parseArrowFunctionBodyWithStrict()
          val isStrict = currentStrictMode || bodyStrict
          validateArrowParameters(params, body, isStrict, bodyStrict)
          ArrowFunctionExpression(params, body, false, isStrict, span)
        case _ =>
          if name == "arguments" && classFieldInitializerDepth > 0 &&
            fieldInitializerFunctionBoundaryDepth == 0
          then
            throw new RuntimeException(
              "arguments is not allowed in a class field initializer"
            )
          if Parser.isReservedWordForIdentifierReference(name, currentStrictMode)
          then
            throw new RuntimeException(
              s"SyntaxError: '$name' is not a valid identifier"
            )
          if name == "await" && awaitExpressionAllowed then
            throw new RuntimeException(
              "SyntaxError: 'await' is not a valid identifier in this context"
            )
          if name == "yield" && generatorFunctionDepth > 0 then
            throw new RuntimeException(
              "SyntaxError: 'yield' is not a valid identifier in a generator"
            )
          advance()
          Identifier(name, span)
      }

    case PunctuationToken(Punctuation.LeftParen, _) =>
      // Check for arrow function: (params) => body
      // But first check if this is (function ...) which is NOT an arrow function
      val nextTok = peek()
      nextTok match {
        case KeywordToken(Keyword.Function, _) =>
          // This is (function ...), parse as grouped expression (probably an IIFE)
          advance() // consume (
          val expr = parseExpression()
          expectPunctuation(Punctuation.RightParen)
          advance() // consume )
          expr
        case _ =>
          // Not (function ...), check if it's an arrow function. Only attempt
          // the speculative parameter parse when the tokens actually end with
          // `) =>`; otherwise parse a plain parenthesized expression.
          if !isArrowFunctionAhead() then {
            advance() // consume (
            val expr = parseExpression()
            expectPunctuation(Punctuation.RightParen)
            advance() // consume )
            expr
          } else {
            val saved = pos
            advance() // consume (
            val maybeArrow =
              try {
              // Try to parse parameters
              val params = parseArrowFunctionParams()
              // Check for arrow: need ) followed by =>
              if isPunctuation(Punctuation.RightParen) then {
                // Peek to see if next token is =>
                val nextTok2 = peek()
                // println(s"[DEBUG] arrow check: after ')' peek=$nextTok2")
                nextTok2 match {
                  case OperatorToken(Operator.Arrow, _) =>
                    advance() // consume )
                    advance() // consume =>
                    // It's an arrow function!
                    val (body, bodyStrict) = parseArrowFunctionBodyWithStrict()
                    val isStrict = currentStrictMode || bodyStrict
                    validateArrowParameters(params, body, isStrict, bodyStrict)
                    ArrowFunctionExpression(
                      params,
                      body,
                      false,
                      isStrict,
                      current.span
                    )
                  case _ =>
                    // println(s"[DEBUG] arrow check FAILED: next=$nextTok2")
                    null
                }
              } else null
            } catch {
              case _: Exception =>
                // Not an arrow function
                null
            }

          if maybeArrow != null then maybeArrow
          else {
            // Not an arrow function, parse as regular parenthesized expression
            pos = saved
            advance()
            val expr = parseExpression()
            expectPunctuation(Punctuation.RightParen)
            advance() // consume )
            expr
          }
          }
      }

    case PunctuationToken(Punctuation.LeftBrace, _) =>
      parseObjectLiteral()

    case PunctuationToken(Punctuation.LeftBracket, _) =>
      parseArrayLiteral()

    case KeywordToken(Keyword.Function, _) =>
      parseFunctionExpression()

    case KeywordToken(Keyword.Class, _) =>
      parseClassExpression()

    case KeywordToken(Keyword.Import, span) if isDynamicImportStart =>
      advance()
      expectPunctuation(Punctuation.LeftParen)
      advance()
      val arguments = parseArguments()
      expectPunctuation(Punctuation.RightParen)
      advance()
      if arguments.isEmpty || arguments.length > 2 then
        throw new RuntimeException("import() requires one or two arguments")
      ImportCallExpression(arguments.toSeq, span)

    case KeywordToken(Keyword.Import, span) if isImportMetaStart =>
      advance()
      if !isOperator(Operator.Dot) then
        throw new RuntimeException("Expected . after import")
      advance()
      current match {
        case IdentifierToken("meta", _, _) =>
          advance()
          ImportMetaExpression(span)
        case KeywordToken(kind, _) if kind.toString.toLowerCase == "meta" =>
          advance()
          ImportMetaExpression(span)
        case _ =>
          throw new RuntimeException("Expected meta after import.")
      }

    case KeywordToken(Keyword.Import, _) =>
      throw new RuntimeException("import must be followed by '(' or '.meta'")

    // Contextual keywords that can be used as identifiers in expressions
    // e.g., `from` (import keyword), `as` (import/export), `get`/`set` (object literal),
    // `static` (class), `of` (for-of), `yield` (generator), `let` (non-strict),
    // `await` (module), `target` (new.target)
    case KeywordToken(kind, span) =>
      advance()
      Identifier(kind.toString.toLowerCase, span)

    case _ =>
      throw new RuntimeException(s"Unexpected token in expression: $current")
  }

  private def isImportMetaStart: Boolean =
    current match {
      case KeywordToken(Keyword.Import, _) =>
        peek() match {
          case OperatorToken(Operator.Dot, _) => true
          case _                              => false
        }
      case _ => false
    }

  private def isDynamicImportStart: Boolean =
    current match {
      case KeywordToken(Keyword.Import, _) =>
        peek() match {
          case PunctuationToken(Punctuation.LeftParen, _) => true
          case _                                          => false
        }
      case _ => false
    }

  /** Parse an identifier */
  private val alwaysReservedBindingWords = Set(
    "break", "case", "catch", "class", "const", "continue", "debugger",
    "default", "delete", "do", "else", "enum", "export", "extends",
    "false", "finally", "for", "function", "if", "import", "in",
    "instanceof", "new", "null", "return", "super", "switch", "this",
    "throw", "true", "try", "typeof", "var", "void", "while", "with"
  )

  private val strictReservedBindingWords = Set(
    "implements", "interface", "let", "package", "private", "protected",
    "public", "static", "yield"
  )

  private def validateBindingIdentifier(name: String, span: Span): Unit =
    if alwaysReservedBindingWords.contains(name) ||
      (currentStrictMode && strictReservedBindingWords.contains(name)) ||
      (currentStrictMode && (name == "eval" || name == "arguments")) ||
      (generatorFunctionDepth > 0 && name == "yield") ||
      (name == "await" && awaitExpressionAllowed)
    then
      throw new RuntimeException(
        s"Reserved word '$name' cannot be used as a binding identifier at $span"
      )

  private def numberPropertyKey(value: Double): String = {
    val abs = math.abs(value)
    if value == 0.0 then "0"
    else if value.isFinite && abs >= 1e-6 && abs < 1e21 then
      JSValue.fromDouble(value).toString
    else {
      val raw = java.lang.Double.toString(value).toLowerCase
      val exponentAt = raw.indexOf('e')
      if exponentAt < 0 then raw
      else {
        val mantissa = raw.substring(0, exponentAt).stripSuffix(".0")
        val exponent0 = raw.substring(exponentAt + 1)
        val negative = exponent0.startsWith("-")
        val positive = exponent0.startsWith("+")
        val digits =
          exponent0.stripPrefix("-").stripPrefix("+").dropWhile(_ == '0') match {
            case "" => "0"
            case s  => s
          }
        val sign = if negative then "-" else if positive || !negative then "+" else ""
        mantissa + "e" + sign + digits
      }
    }
  }

  /** Parse a ModuleExportName: an IdentifierName or a StringLiteral (ES2022
    * arbitrary module namespace names).
    */
  private def parseModuleExportName(): Identifier | String = current match {
    case IdentifierToken(name, span, _) =>
      advance()
      Identifier(name, span)
    case KeywordToken(kind, span) =>
      advance()
      Identifier(kind.toString.toLowerCase, span)
    case StringToken(value, _, _) =>
      advance()
      value
    case _ =>
      throw new RuntimeException(
        s"Expected identifier or string but got $current"
      )
  }

  private def parseIdentifier(): Identifier = current match {
    case IdentifierToken(name, span, _) =>
      advance()
      Identifier(name, span)
    case KeywordToken(kind, span) =>
      // Allow contextual keywords as identifiers (e.g., `from`, `as`, `async`, `get`, `set`)
      advance()
      Identifier(kind.toString.toLowerCase, span)
    case _ =>
      throw new RuntimeException(s"Expected identifier but got $current")
  }

  /** Parse arrow function parameters */
  private def parseArrowFunctionParams(): Seq[BindingPattern] = {
    val params = ArrayBuffer[BindingPattern]()

    // Parse parameters: can be identifiers or patterns (for now, just identifiers)
    if !isPunctuation(Punctuation.RightParen) then {
      var more = true
      while more do {
        if isOperator(Operator.Spread) then {
          val spreadSpan = current.span
          advance()
          params += RestElement(parseBindingPatternBase(), spreadSpan)
          if isOperator(Operator.Comma) then
            throw new RuntimeException("rest parameter must be the last parameter")
          more = false
        } else params += parseBindingPattern()

        if more && isOperator(Operator.Comma) then {
          advance()
          if isPunctuation(Punctuation.RightParen) then more = false
        }
        else more = false
      }
    }

    params.toSeq
  }

  private def validateArrowParameters(
      params: Seq[BindingPattern],
      body: Either[Expression, BlockStatement],
      strict: Boolean,
      hasUseStrictDirective: Boolean
  ): Unit =
    validateFormalParameters(
      params,
      strict,
      hasUseStrictDirective,
      forceUnique = true,
      body = body.toOption.orNull
    )

  /** Parse arrow function body and extract strict mode */
  private def parseArrowFunctionBodyWithStrict()
      : (Either[Expression, BlockStatement], Boolean) = {
    // Arrow bodies are function bodies for `return` early-error purposes; the
    // parenthesized arrow path does not go through
    // withFunctionGrammarContext, so track the depth here.
    functionDepth += 1
    try {
      // Check if it's a block body: { ... }
      if isPunctuation(Punctuation.LeftBrace) then {
        val savedStrict = currentStrictMode
        val hasUseStrictDirective = isUseStrictDirectiveAhead()
        if hasUseStrictDirective then currentStrictMode = true
        val block = parseBlockStatement()
        val (isStrict, remainingStatements) =
          Parser.extractStrictMode(block.statements)
        val finalBlock = BlockStatement(remainingStatements.toSeq, block.span)
        currentStrictMode = savedStrict
        (Right(finalBlock), hasUseStrictDirective || isStrict)
      } else
        // ConciseBody is an AssignmentExpression, not the wider Expression
        // grammar. In particular, an unparenthesized comma terminates the
        // arrow body (for example in an object literal property list).
        (Left(parseAssignmentExpressionWithoutComma()), false)
    } finally functionDepth -= 1
  }
}

object Parser {
  def apply(tokens: Seq[Token]): Parser = new Parser(tokens)

  /** Words that can never be used as an IdentifierReference. */
  private val AlwaysReserved: Set[String] = Set(
    "break", "case", "catch", "class", "const", "continue", "debugger",
    "default", "delete", "do", "else", "enum", "export", "extends",
    "false", "finally", "for", "function", "if", "import", "in",
    "instanceof", "new", "null", "return", "super", "switch", "this",
    "throw", "true", "try", "typeof", "var", "void", "while", "with"
  )

  /** Words reserved only in strict mode (plus `let`/`yield`). */
  private val StrictReserved: Set[String] = Set(
    "implements", "interface", "let", "package", "private", "protected",
    "public", "static", "yield"
  )

  /** Whether `name` may not appear as an IdentifierReference in the given
    * strictness context. Used for shorthand destructuring/property positions
    * where a reserved word cannot be written even via an escape sequence.
    */
  def isReservedWordForIdentifierReference(
      name: String,
      strict: Boolean
  ): Boolean =
    AlwaysReserved.contains(name) || (strict && StrictReserved.contains(name))

  /** Check if a statement is a "use strict" directive. A directive is an
    * expression statement containing a string literal.
    */
  def isUseStrictDirective(stmt: Statement): Boolean = stmt match {
    case ExpressionStatement(Literal(JSValue.JSStr(s), _), _) =>
      s == "use strict"
    case _ => false
  }

  /** Extract strict mode from the beginning of a statement sequence. Returns
    * (isStrict, remainingStatements)
    */
  def extractStrictMode(statements: Seq[Statement]): (Boolean, Seq[Statement]) =
    statements.headOption match {
      case Some(first) if isUseStrictDirective(first) =>
        (true, statements.tail)
      case _ =>
        (false, statements)
    }
}
