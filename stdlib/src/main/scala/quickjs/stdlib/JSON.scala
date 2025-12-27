package quickjs.stdlib

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.bytecode.BytecodeFunction
import quickjs.runtime.JSContext
import quickjs.objmodel.{JSObject, JSArray}
import quickjs.interpreter.Interpreter
import scala.collection.mutable

/** JavaScript JSON object implementation.
  *
  * Provides JSON.parse() and JSON.stringify() methods.
  */
object JSON:
  /** Initialize JSON object in the given context */
  def initialize()(using ctx: JSContext): Unit =
    val jsonObj = JSObject(prototype = null, extensible = true)

    // JSON.parse(text, reviver) - parse JSON string to JavaScript value
    val parseFunc = NativeFunction("parse", (args, context) =>
      // Find the string argument (skip 'this' if it's an object)
      val textArg = args.find(_.isString).getOrElse(return JSValue.Undefined)
      val text = textArg match
        case JSValue.JSStr(s) => s
        case _ => return JSValue.Undefined

      try
        given JSContext = context  // For operations that need context
        val parser = new JSONParser(text)
        parser.parseValue()
      catch
        case ex: JSONParseException =>
          JSValue.Undefined  // Or could throw with error message
    )
    jsonObj.set("parse", JSValue.Native(parseFunc))

    // JSON.stringify(value, replacer, space) - convert JavaScript value to JSON string
    val stringifyFunc = NativeFunction("stringify", (args, context) =>
      // Calling convention for method calls: args = [this, value, ...optional]
      // We always skip args(0) when there's more than one argument
      val value = if args.length >= 2 then args(1) else args(0)
      val replacerIdx = if args.length >= 2 then 2 else 1

      val replacer = if args.length > replacerIdx then Some(args(replacerIdx)) else None
      val space = if args.length > replacerIdx + 1 then Some(args(replacerIdx + 1)) else None

      try
        given JSContext = context  // For stringifier operations
        val stringifier = new JSONStringifier()
        val result = stringifier.stringify(value, replacer, space)
        JSValue.fromString(result)
      catch
        case ex: Exception =>
          JSValue.Undefined
    )
    jsonObj.set("stringify", JSValue.Native(stringifyFunc))

    ctx.global.set("JSON", JSValue.Object(jsonObj))

  /** JSON parser using recursive descent */
  private class JSONParser(input: String)(using ctx: JSContext):
    private var pos = 0
    private val length = input.length

    def parseValue(): JSValue =
      skipWhitespace()
      if pos >= length then
        throw new JSONParseException("Unexpected end of input")

      input.charAt(pos) match
        case '"' => parseString()
        case '{' => parseObject()
        case '[' => parseArray()
        case 't' => parseTrue()
        case 'f' => parseFalse()
        case 'n' => parseNull()
        case c if c == '-' || (c >= '0' && c <= '9') => parseNumber()
        case c => throw new JSONParseException(s"Unexpected character: $c")

    def parseString(): JSValue.JSStr =
      pos += 1  // Skip opening quote
      val sb = StringBuilder()

      while pos < length do
        val c = input.charAt(pos)
        c match
          case '"' =>
            pos += 1  // Skip closing quote
            return JSValue.JSStr(sb.toString())
          case '\\' =>
            pos += 1
            if pos < length then
              val escape = input.charAt(pos)
              val decoded = escape match
                case '"' => '"'
                case '\\' => '\\'
                case '/' => '/'
                case 'b' => '\b'
                case 'f' => '\f'
                case 'n' => '\n'
                case 'r' => '\r'
                case 't' => '\t'
                case 'u' =>
                  // Unicode escape
                  pos += 1
                  if pos + 3 < length then
                    val hex = input.substring(pos, pos + 4)
                    val code = Integer.parseInt(hex, 16)
                    pos += 3
                    code.toChar
                  else
                    throw new JSONParseException("Invalid Unicode escape")
                case _ =>
                  throw new JSONParseException(s"Invalid escape sequence: \\$escape")
              sb.append(decoded)
            pos += 1
          case c =>
            sb.append(c)
            pos += 1

      throw new JSONParseException("Unterminated string")

    def parseNumber(): JSValue =
      val start = pos

      // Optional minus
      if pos < length && input.charAt(pos) == '-' then
        pos += 1

      // Integer part
      if pos < length && input.charAt(pos) == '0' then
        pos += 1
      else if pos < length && input.charAt(pos) >= '1' && input.charAt(pos) <= '9' then
        while pos < length && input.charAt(pos) >= '0' && input.charAt(pos) <= '9' do
          pos += 1

      // Fraction part
      if pos < length && input.charAt(pos) == '.' then
        pos += 1
        while pos < length && input.charAt(pos) >= '0' && input.charAt(pos) <= '9' do
          pos += 1

      // Exponent part
      if pos < length && (input.charAt(pos) == 'e' || input.charAt(pos) == 'E') then
        pos += 1
        if pos < length && (input.charAt(pos) == '+' || input.charAt(pos) == '-') then
          pos += 1
        while pos < length && input.charAt(pos) >= '0' && input.charAt(pos) <= '9' do
          pos += 1

      val numStr = input.substring(start, pos)

      // Try to parse as Int first, then Double
      try
        if numStr.contains(".") || numStr.contains("e") || numStr.contains("E") then
          JSValue.fromDouble(numStr.toDouble)
        else
          val longVal = numStr.toLong
          if longVal >= Int.MinValue.toLong && longVal <= Int.MaxValue.toLong then
            JSValue.fromInt(longVal.toInt)
          else
            JSValue.fromDouble(longVal.toDouble)
      catch
        case _: NumberFormatException =>
          throw new JSONParseException(s"Invalid number: $numStr")

    def parseObject(): JSValue.Object =
      pos += 1  // Skip opening brace
      skipWhitespace()

      val obj = JSObject(prototype = null, extensible = true)

      if pos < length && input.charAt(pos) == '}' then
        pos += 1  // Empty object
        return JSValue.Object(obj)

      while pos < length do
        skipWhitespace()

        // Parse key (must be string)
        val key = parseString() match
          case JSValue.JSStr(s) => s
          case _ => throw new JSONParseException("Object key must be a string")

        skipWhitespace()

        // Expect colon
        if pos >= length || input.charAt(pos) != ':' then
          throw new JSONParseException("Expected ':' after object key")
        pos += 1

        // Parse value
        val value = parseValue()

        // Set property
        obj.set(key, value)

        skipWhitespace()

        // Check for comma or closing brace
        if pos < length && input.charAt(pos) == ',' then
          pos += 1
        else if pos < length && input.charAt(pos) == '}' then
          pos += 1
          return JSValue.Object(obj)
        else
          throw new JSONParseException("Expected ',' or '}' in object")

      throw new JSONParseException("Unterminated object")

    def parseArray(): JSValue =
      pos += 1  // Skip opening bracket
      skipWhitespace()

      val arr = JSArray.empty()

      if pos < length && input.charAt(pos) == ']' then
        pos += 1  // Empty array
        return JSValue.JSArrayVal(arr)

      while pos < length do
        skipWhitespace()

        // Parse value
        val value = parseValue()
        arr.push(value)

        skipWhitespace()

        // Check for comma or closing bracket
        if pos < length && input.charAt(pos) == ',' then
          pos += 1
        else if pos < length && input.charAt(pos) == ']' then
          pos += 1
          return JSValue.JSArrayVal(arr)
        else
          throw new JSONParseException("Expected ',' or ']' in array")

      throw new JSONParseException("Unterminated array")

    def parseTrue(): JSValue.Bool =
      if pos + 3 < length && input.substring(pos, pos + 4) == "true" then
        pos += 4
        JSValue.Bool(true)
      else
        throw new JSONParseException("Invalid token")

    def parseFalse(): JSValue.Bool =
      if pos + 4 < length && input.substring(pos, pos + 5) == "false" then
        pos += 5
        JSValue.Bool(false)
      else
        throw new JSONParseException("Invalid token")

    def parseNull(): JSValue.Null.type =
      if pos + 3 < length && input.substring(pos, pos + 4) == "null" then
        pos += 4
        JSValue.Null
      else
        throw new JSONParseException("Invalid token")

    def skipWhitespace(): Unit =
      while pos < length && (input.charAt(pos) == ' ' ||
                             input.charAt(pos) == '\n' ||
                             input.charAt(pos) == '\r' ||
                             input.charAt(pos) == '\t') do
        pos += 1

  /** JSON stringifier */
  private class JSONStringifier(using ctx: JSContext):
    private val seen = mutable.HashSet[AnyRef]()  // For circular reference detection
    private val interpreter = Interpreter()

    private def callGetter(getter: JSValue, receiver: JSObject): JSValue =
      getter match
        case func: JSValue.Function =>
          val bcFunc = new BytecodeFunction(
            name = func.name,
            bytecode = func.bytecode,
            constants = func.constants,
            stackSize = func.stackSize,
            freeVars = Array.empty,
            paramNames = func.paramNames,
            localVarNames = func.localVarNames,
            argumentsIndex = func.argumentsIndex,
            isConstructor = func.isConstructor
          )
          interpreter.call(bcFunc, JSValue.Object(receiver), Array.empty, func.closure)
        case JSValue.Native(nativeFuncWrapper) =>
          nativeFuncWrapper match
            case native: NativeFunction =>
              native.call(Array(JSValue.Object(receiver)))
            case _ =>
              JSValue.Undefined
        case _ =>
          JSValue.Undefined

    def stringify(value: JSValue, replacer: Option[JSValue], space: Option[JSValue]): String =
      seen.clear()
      val gap = space match
        case Some(JSValue.JSStr(s)) => s.take(10)
        case Some(JSValue.Int32(i)) if i > 0 => " " * i.min(10)
        case Some(JSValue.Float64(d)) if d > 0 => " " * d.toInt.min(10)
        case _ => ""

      val result = stringifyValue(value, replacer, gap, "")
      seen.clear()
      result

    private def stringifyValue(value: JSValue, replacer: Option[JSValue], gap: String, indent: String): String =
      value match
        case JSValue.Null => "null"
        case JSValue.Bool(b) => if b then "true" else "false"
        case JSValue.JSStr(s) => quoteString(s)
        case JSValue.Int32(i) => i.toString
        case JSValue.Float64(d) =>
          if d.isNaN then "null"
          else if d.isInfinite then "null"
          else d.toString
        case JSValue.JSArrayVal(arr) =>
          if seen.contains(arr) then
            throw new Exception("Circular reference")
          seen.add(arr)
          val result = stringifyArray(arr, replacer, gap, indent)
          seen.remove(arr)
          result
        case JSValue.Object(obj) =>
          // Check if this is actually an array
          if obj.isArray then
            // This is an array stored as an object - stringify as array
            if seen.contains(obj) then
              throw new Exception("Circular reference")
            seen.add(obj)

            given JSContext = summon[JSContext]
            val len = obj.get("length").toNumber.toInt
            val elements = (0 until len).map(i => obj.get(i.toString)).toArray

            // Create array JSON string
            val sb = StringBuilder()
            if len == 0 then return "[]"

            val newIndent = indent + gap
            if gap.nonEmpty then
              sb.append("[\n")
            else
              sb.append("[")

            for i <- 0 until len do
              if gap.nonEmpty then
                sb.append(newIndent)

              val elemStr = stringifyValue(elements(i), replacer, gap, newIndent)

              if i > 0 then
                if gap.nonEmpty then
                  sb.append(",\n").append(newIndent)
                else
                  sb.append(",")

              sb.append(elemStr)

            if gap.nonEmpty then
              sb.append("\n").append(indent).append("]")
            else
              sb.append("]")

            seen.remove(obj)
            sb.toString
          else
            // Regular object
            if seen.contains(obj) then
              throw new Exception("Circular reference")
            seen.add(obj)
            val result = stringifyObject(obj, replacer, gap, indent)
            seen.remove(obj)
            result
        case JSValue.Function(_, _, _, _, _, _, _, _, _, _, _) => "undefined"  // Functions are not valid JSON
        case JSValue.Native(_) => "undefined"  // Native functions are not valid JSON
        case JSValue.Symbol(_) => "undefined"  // Symbols are not valid JSON
        case JSValue.BigInt(_) => "undefined"  // BigInt is not valid JSON
        case JSValue.Undefined => "undefined"

    private def stringifyArray(arr: JSArray, replacer: Option[JSValue], gap: String, indent: String): String =
      if arr.length == 0 then return "[]"

      val newIndent = indent + gap
      val sb = StringBuilder()

      if gap.nonEmpty then
        sb.append("[\n")
      else
        sb.append("[")

      for i <- 0 until arr.length.toInt do
        if gap.nonEmpty then
          sb.append(newIndent)

        val value = arr.get(i)
        val str = stringifyValue(value, replacer, gap, newIndent)

        if i > 0 then
          if gap.nonEmpty then
            sb.append(",\n").append(newIndent)
          else
            sb.append(",")

        sb.append(str)

      if gap.nonEmpty then
        sb.append("\n").append(indent).append("]")
      else
        sb.append("]")

      sb.toString()

    private def stringifyObject(obj: JSObject, replacer: Option[JSValue], gap: String, indent: String): String =
      val keys = obj.getOwnPropertyKeys().filter(isValidJSONKey).sorted

      if keys.isEmpty then return "{}"

      val newIndent = indent + gap
      val sb = StringBuilder()

      if gap.nonEmpty then
        sb.append("{\n")
      else
        sb.append("{")

      var first = true
      for key <- keys do
        val value =
          obj.getOwnPropertyDescriptor(key) match
            case Some((value, attrs)) if attrs.getter.isDefined =>
              callGetter(attrs.getter.get, obj)
            case Some((value, _)) => value
            case None => JSValue.Undefined

        // Skip undefined values
        if value != JSValue.Undefined then
          val str = stringifyValue(value, replacer, gap, newIndent)

          if str != "undefined" then
            if !first then
              if gap.nonEmpty then
                sb.append(",\n").append(newIndent)
              else
                sb.append(",")

            if gap.nonEmpty then
              sb.append(newIndent)
            else if !first then
              sb.append(" ")

            sb.append(quoteString(key)).append(":")
            if gap.nonEmpty then
              sb.append(" ")
            sb.append(str)

            first = false

      if first then
        return "{}"

      if gap.nonEmpty then
        sb.append("\n").append(indent).append("}")
      else
        sb.append("}")

      sb.toString()

    private def quoteString(s: String): String =
      val sb = StringBuilder("\"")

      for c <- s do
        c match
          case '"' => sb.append("\\\"")
          case '\\' => sb.append("\\\\")
          case '\b' => sb.append("\\b")
          case '\f' => sb.append("\\f")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case '\t' => sb.append("\\t")
          case c if c < ' ' =>
            // Control characters
            sb.append(f"\\u$c%04x")
          case c => sb.append(c)

      sb.append("\"").toString()

    private def isValidJSONKey(key: String): Boolean =
      // Skip internal properties and callable properties for now
      !key.startsWith("__") && key != "prototype" && key != "constructor"

  /** Exception for JSON parsing errors */
  private class JSONParseException(message: String) extends Exception(message)
