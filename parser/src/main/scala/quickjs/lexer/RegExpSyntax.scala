package quickjs.lexer

import com.ibm.icu.lang.{UCharacter, UProperty}

import scala.collection.mutable

/** Early-error (parse-time) validation for regular expression literals.
  *
  * Per ECMAScript, invalid regular expression literals must throw a
  * SyntaxError during parsing, before any code runs. The runtime regex engine
  * cannot provide that guarantee (and Java's regex syntax differs from
  * JavaScript's), so this validator checks the static semantics that can be
  * determined from the pattern text alone:
  *
  *   - flag validity and duplicates (including `u`/`v` exclusivity)
  *   - line terminators (raw or escaped) in the pattern
  *   - named capture group syntax, duplicates and dangling `\k<name>`
  *     backreferences
  *   - invalid braced quantifiers and quantified assertions in Unicode mode
  *   - Unicode-mode identity escapes, `\c` escapes, decimal escapes and
  *     `\u{...}` escapes
  *   - non-empty character class ranges involving class escapes in Unicode mode
  *
  * Validation is intentionally conservative: when in doubt the pattern is
  * accepted, so a valid pattern is never rejected.
  */
object RegExpSyntax {

  private val ValidFlags = "dgimsuvy"
  private val SyntaxCharacters = "^$\\.*+?()[]{}|"

  private def isLineTerminator(cp: Int): Boolean =
    cp == '\n' || cp == '\r' || cp == 0x2028 || cp == 0x2029

  private def isIdentifierStart(cp: Int): Boolean =
    cp == '$' || cp == '_' ||
      (cp >= 0 && UCharacter.hasBinaryProperty(cp, UProperty.ID_START))

  private def isIdentifierPart(cp: Int): Boolean =
    cp == '$' || cp == '_' || cp == 0x200c || cp == 0x200d ||
      isIdentifierStart(cp) ||
      (cp >= 0 && UCharacter.hasBinaryProperty(cp, UProperty.ID_CONTINUE))

  private def isDigit(cp: Int): Boolean = cp >= '0' && cp <= '9'
  private def isHexDigit(cp: Int): Boolean =
    isDigit(cp) || (cp >= 'a' && cp <= 'f') || (cp >= 'A' && cp <= 'F')
  private def isAsciiLetter(cp: Int): Boolean =
    (cp >= 'a' && cp <= 'z') || (cp >= 'A' && cp <= 'Z')

  private def syntaxError(message: String): Nothing =
    throw new RuntimeException(s"SyntaxError: $message")

  def validate(pattern: String, flags: String): Unit = {
    validateFlags(flags)
    val unicode = flags.contains('u')
    val unicodeSets = flags.contains('v')
    new Validator(pattern, unicode, unicodeSets).run()
  }

  private def validateFlags(flags: String): Unit = {
    val seen = mutable.HashSet.empty[Char]
    var i = 0
    while i < flags.length do {
      val c = flags.charAt(i)
      if ValidFlags.indexOf(c) < 0 then
        syntaxError(s"Invalid regular expression flag '$c'")
      if !seen.add(c) then
        syntaxError(s"Duplicate regular expression flag '$c'")
      i += 1
    }
    if seen.contains('u') && seen.contains('v') then
      syntaxError("Regular expression flags 'u' and 'v' cannot be combined")
  }

  private final class Validator(
      pattern: String,
      unicode: Boolean,
      unicodeSets: Boolean
  ) {
    private val length = pattern.length
    private var i = 0

    private val groupNames = mutable.LinkedHashSet.empty[String]
    private val pendingReferences = mutable.ListBuffer.empty[String]
    private var bareNamedBackreference = false

    // Whether each currently open group is an assertion (lookaround).
    // Values match PrevAtom / PrevLookahead / PrevLookbehind to restore the
    // state after ')'.
    private val groupStack = mutable.Stack.empty[Int]
    private val captureCount = countCapturingGroups()

    /** Count capturing groups so Unicode-mode decimal escapes can be checked.
      * Approximate but safe: over-counting only makes the validator more
      * permissive.
      */
    private def countCapturingGroups(): Int = {
      var count = 0
      var index = 0
      var inCls = false
      while index < length do
        val cp = codePointAt(index)
        if cp == '\\' then index += 1 + (if index + 1 < length then 1 else 0)
        else if cp == '[' then { inCls = true; index += 1 }
        else if cp == ']' then { inCls = false; index += 1 }
        else if cp == '(' && !inCls then {
          val isSpecial =
            index + 1 < length && codePointAt(index + 1) == '?'
          if isSpecial then {
            val third = if index + 2 < length then codePointAt(index + 2) else -1
            val fourth = if index + 3 < length then codePointAt(index + 3) else -1
            val isLookaround =
              third == '=' || third == '!' ||
                (third == '<' && (fourth == '=' || fourth == '!'))
            if !(third == ':' || isLookaround) then count += 1
          } else count += 1
          index += 1
        } else index += charCountAt(index)
      count
    }

    // Tracks what precedes the current position in the current alternative:
    // 0 = nothing yet (start, after '(' or '|'), 1 = quantifiable atom,
    // 2 = quantifiable assertion (Annex B lookahead), 3 = non-quantifiable
    // assertion (lookbehind, ^, $, \b, \B).
    private val PrevNone = 0
    private val PrevAtom = 1
    private val PrevLookahead = 2
    private val PrevLookbehind = 3
    private var prevKind = PrevNone

    private def codePointAt(index: Int): Int = pattern.codePointAt(index)
    private def charCountAt(index: Int): Int =
      java.lang.Character.charCount(pattern.codePointAt(index))

    def run(): Unit = {
      while i < length do
        val cp = codePointAt(i)
        if isLineTerminator(cp) then
          syntaxError("Line terminator in regular expression literal")
        else if cp == '\\' then handleEscape(inClass = false)
        else if cp == '[' then { i += 1; handleClass() }
        else if cp == '(' then handleGroup()
        else if cp == ')' then {
          prevKind = if groupStack.nonEmpty then groupStack.pop() else PrevAtom
          i += 1
        }
        else if cp == '|' then { prevKind = PrevNone; i += 1 }
        else if cp == '^' || cp == '$' then { prevKind = PrevLookbehind; i += 1 }
        else if cp == '{' then handleBrace()
        else if cp == '*' || cp == '+' || cp == '?' then handleQuantifier()
        else {
          prevKind = PrevAtom
          i += charCountAt(i)
        }

      if bareNamedBackreference && groupNames.nonEmpty then
        syntaxError("Invalid named backreference")
      pendingReferences.foreach { name =>
        if !groupNames.contains(name) then
          syntaxError(s"Invalid named capture referenced: $name")
      }
    }

    /** Handle a `[...]` character class (the leading `[` is already consumed).
      */
    private def handleClass(): Unit = {
      var lastWasClassEscape = false
      var classEscapeBeforeDash = false
      if i < length && codePointAt(i) == '^' then i += 1
      while i < length && codePointAt(i) != ']' do
        val cp = codePointAt(i)
        if cp == '\\' then {
          val escaped = handleEscape(inClass = true)
          lastWasClassEscape = escaped
          if classEscapeBeforeDash && unicode then
            syntaxError("Invalid character class range")
          classEscapeBeforeDash = false
        } else if cp == '-' then {
          // A '-' directly after a class escape starts a range whose start
          // atom is not a single character; that is invalid in Unicode mode
          // unless it is the last atom before ']'.
          val next = if i + 1 < length then codePointAt(i + 1) else -1
          if unicode && lastWasClassEscape && next != ']' then
            syntaxError("Invalid character class range")
          classEscapeBeforeDash = true
          lastWasClassEscape = false
          i += 1
        } else {
          classEscapeBeforeDash = false
          lastWasClassEscape = false
          prevKind = PrevAtom
          i += charCountAt(i)
        }
      if i < length then i += 1 // consume ']'
      prevKind = PrevAtom
    }

    /** Handle `(` group openings. Returns after consuming what it understands.
      */
    private def handleGroup(): Unit = {
      i += 1 // consume '('
      prevKind = PrevNone
      if i < length && codePointAt(i) == '?' then {
        i += 1
        if i < length && codePointAt(i) == '<' then {
          i += 1
          if i < length && (codePointAt(i) == '=' || codePointAt(i) == '!') then {
            i += 1 // lookbehind: not quantifiable even via Annex B
            groupStack.push(PrevLookbehind)
          } else {
            // Named capture group: (?<name>
            val (name, next) = readGroupName(i)
            i = next
            if i >= length || codePointAt(i) != '>' then
              syntaxError("Invalid named capture group")
            i += 1
            if !groupNames.add(name) then
              syntaxError(s"Duplicate named capture group: $name")
            groupStack.push(PrevAtom)
          }
        } else if i < length && codePointAt(i) == ':' then {
          i += 1
          groupStack.push(PrevAtom)
        } else if i < length && (codePointAt(i) == '=' || codePointAt(i) == '!')
        then {
          i += 1 // lookahead: quantifiable only in Annex B (non-Unicode)
          groupStack.push(PrevLookahead)
        } else syntaxError("Invalid group")
      } else groupStack.push(PrevAtom)
    }

    /** Read a RegExpIdentifierName starting at `start`. Returns the decoded
      * name and the index just past it.
      */
    private def readGroupName(start: Int): (String, Int) = {
      val sb = new StringBuilder
      var index = start
      var first = true
      var done = false
      while !done && index < length do
        val cp = codePointAt(index)
        if cp == '>' then done = true
        else {
          var decoded =
            if cp == '\\' then {
              val (value, next) = readUnicodeEscape(index)
              index = next
              value
            } else {
              index += charCountAt(index)
              cp
            }
          // A surrogate pair may be written as two escapes; combine it so the
          // identifier validation sees the code point.
          if Character.isHighSurrogate(decoded.toChar) && index < length &&
              pattern.charAt(index) == '\\'
          then {
            val save = index
            try {
              val (low, next) = readUnicodeEscape(index)
              if low >= 0xDC00 && low <= 0xDFFF then {
                decoded = Character.toCodePoint(decoded.toChar, low.toChar)
                index = next
              } else index = save
            } catch case _: RuntimeException => index = save
          }
          val valid = if first then isIdentifierStart(decoded)
          else isIdentifierPart(decoded)
          if !valid then
            syntaxError("Invalid named capture group identifier")
          sb.appendAll(Character.toChars(decoded))
          first = false
        }
      if first then syntaxError("Empty named capture group")
      if done then (sb.toString, index)
      else syntaxError("Invalid named capture group")
    }

    /** Read a `\uXXXX` or `\u{...}` escape starting at the backslash. Returns
      * the decoded code point and the index after the escape.
      */
    private def readUnicodeEscape(start: Int): (Int, Int) = {
      var index = start + 1
      if index >= length || codePointAt(index) != 'u' then
        syntaxError("Invalid escape in named capture group")
      index += 1
      if index < length && codePointAt(index) == '{' then {
        // Braced escapes are valid inside group names regardless of the `u`
        // flag (RegExpIdentifierName grammar).
        index += 1
        var value = 0
        var digits = 0
        while index < length && codePointAt(index) != '}' do
          val cp = codePointAt(index)
          if !isHexDigit(cp) then
            syntaxError("Invalid Unicode escape")
          value = value * 16 + Character.digit(cp, 16)
          digits += 1
          index += 1
        if index >= length || codePointAt(index) != '}' || digits == 0 then
          syntaxError("Invalid Unicode escape")
        if value > 0x10ffff then syntaxError("Unicode escape out of range")
        (value, index + 1)
      } else {
        var value = 0
        var digits = 0
        while digits < 4 && index < length && isHexDigit(codePointAt(index)) do
          value = value * 16 + Character.digit(codePointAt(index), 16)
          digits += 1
          index += 1
        if digits < 4 then syntaxError("Invalid Unicode escape")
        (value, index)
      }
    }

    /** Handle a `\` escape at `i`. Returns true when the escape represents a
      * CharacterClassEscape (`\d`, `\s`, `\w`, `\p{...}` and negations).
      */
    private def handleEscape(inClass: Boolean): Boolean = {
      i += 1 // consume backslash
      if i >= length then syntaxError("Trailing backslash in regular expression")
      val cp = codePointAt(i)
      if isLineTerminator(cp) then
        syntaxError("Line terminator in regular expression literal")
      else if cp == 'k' then {
        // Named backreference
        i += 1
        if i < length && codePointAt(i) == '<' then {
          i += 1
          val (name, next) = readGroupName(i)
          i = next
          if i >= length || codePointAt(i) != '>' then
            syntaxError("Invalid named backreference")
          i += 1
          pendingReferences += name
        } else {
          // Bare `\k` is an Annex B identity escape only when the pattern
          // contains no named group at all (which may be declared later).
          if unicode then syntaxError("Invalid named backreference")
          bareNamedBackreference = true
          prevKind = PrevAtom
          return false
        }
        prevKind = PrevAtom
        false
      } else if cp == 'c' then {
        val next = if i + 1 < length then codePointAt(i + 1) else -1
        if unicode && !isAsciiLetter(next) then
          syntaxError("Invalid control escape")
        if isAsciiLetter(next) then i += 2 else i += 1
        prevKind = 1
        false
      } else if cp == 'u' then {
        if i + 1 < length && codePointAt(i + 1) == '{' && unicode then {
          val (_, next) = readUnicodeEscape(i - 1)
          i = next
        } else if i + 4 < length && (1 to 4).forall(k => isHexDigit(codePointAt(i + k)))
        then i += 5 // \uXXXX
        else if unicode then syntaxError("Invalid Unicode escape")
        else i += 1 // Annex B identity escape 'u'
        prevKind = 1
        false
      } else if cp == 'x' then {
        var index = i + 1
        var digits = 0
        while digits < 2 && index < length && isHexDigit(codePointAt(index)) do
          digits += 1
          index += 1
        if digits < 2 && unicode then syntaxError("Invalid hex escape")
        i = if digits == 2 then index else i + 1
        prevKind = 1
        false
      } else if (cp == 'p' || cp == 'P') && !unicodeSets then {
        if i + 1 < length && codePointAt(i + 1) == '{' then i = skipPropertyEscape(i + 2)
        else i += 1
        prevKind = 1
        true
      } else if unicodeSets && (cp == 'p' || cp == 'P' || cp == 'q') &&
          i + 1 < length && codePointAt(i + 1) == '{'
      then {
        // v-mode set notation: \p{...} and \q{...} contain arbitrary text.
        i = skipPropertyEscape(i + 2)
        prevKind = 1
        cp == 'p' || cp == 'P'
      } else if cp == 'f' || cp == 'n' || cp == 'r' || cp == 't' ||
          cp == 'v'
      then {
        // CharacterEscape: valid in both modes (and in classes).
        i += 1
        prevKind = 1
        false
      } else if cp == 'd' || cp == 'D' || cp == 's' || cp == 'S' || cp == 'w' ||
          cp == 'W'
      then {
        i += 1
        prevKind = 1
        true
      } else if cp == 'b' then {
        // Word boundary outside a class, backspace inside
        i += 1
        if inClass then { prevKind = PrevAtom; false }
        else { prevKind = PrevLookbehind; false }
      } else if cp == 'B' then {
        i += 1
        prevKind = PrevLookbehind
        false
      } else if isDigit(cp) && cp != '0' then {
        // Decimal escape / backreference
        var value = 0
        var index = i
        while index < length && isDigit(codePointAt(index)) do
          value = value * 10 + (codePointAt(index) - '0')
          index += 1
        if unicode then {
          if cp == '8' || cp == '9' || value > captureCount then
            syntaxError("Invalid decimal escape")
        }
        i = index
        prevKind = PrevAtom
        false
      } else if cp == '0' then {
        // \0 is NUL unless followed by a digit (legacy octal); under u a
        // following digit is invalid.
        val nextIsDigit = i + 1 < length && isDigit(codePointAt(i + 1))
        if unicode && nextIsDigit then syntaxError("Invalid decimal escape")
        i = if nextIsDigit then i + 1 else i + 1
        prevKind = PrevAtom
        false
      } else {
        // Identity escape: allowed characters under Unicode mode are the
        // syntax characters plus '/'. In non-Unicode mode everything is
        // allowed (Annex B).
        if unicode && !SyntaxCharacters.contains(cp.toChar) && cp != '/' &&
            !(inClass && cp == '-')
        then syntaxError(s"Invalid identity escape '\\${new String(Character.toChars(cp))}'")
        i += charCountAt(i)
        prevKind = PrevAtom
        false
      }
    }

    private def skipPropertyEscape(start: Int): Int = {
      var index = start
      while index < length && codePointAt(index) != '}' do index += 1
      index + 1
    }

    /** Handle `{n}`, `{n,}`, `{n,m}` quantifiers and invalid braced
      * quantifiers (leading `{...}` is always a SyntaxError; in Unicode mode
      * any non-quantifier `{` is invalid).
      */
    private def handleBrace(): Unit = {
      val start = i
      var index = i + 1
      val lowerStart = index
      while index < length && isDigit(codePointAt(index)) do index += 1
      val hasLower = index > lowerStart
      var hasComma = false
      var upperStart = index
      if index < length && codePointAt(index) == ',' then {
        hasComma = true
        index += 1
        upperStart = index
        while index < length && isDigit(codePointAt(index)) do index += 1
      }
      val hasUpper = index > upperStart
      val closed = index < length && codePointAt(index) == '}'
      val isQuantifier = hasLower && closed
      if isQuantifier then {
        if prevKind == PrevNone then syntaxError("Nothing to repeat")
        // Lookbehind is never quantifiable; lookahead only via Annex B.
        if prevKind == PrevLookbehind ||
            (unicode && prevKind == PrevLookahead)
        then syntaxError("Invalid quantifier on assertion")
        i = index + 1
        prevKind = PrevAtom
      } else {
        if unicode then syntaxError("Invalid extended pattern character")
        // Literal '{'
        i = start + 1
        prevKind = PrevAtom
      }
    }

    private def handleQuantifier(): Unit = {
      if prevKind == PrevNone then syntaxError("Nothing to repeat")
      if prevKind == PrevLookbehind ||
          (unicode && prevKind == PrevLookahead)
      then syntaxError("Invalid quantifier on assertion")
      i += 1
      // Consume a following lazy modifier
      if i < length && codePointAt(i) == '?' then i += 1
      prevKind = PrevAtom
    }
  }
}
