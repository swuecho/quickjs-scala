package quickjs.repl

import org.jline.reader.Completer
import org.jline.reader.Candidate
import org.jline.reader.LineReader
import org.jline.reader.ParsedLine
import scala.collection.JavaConverters.*
import java.util.List

/** Enhanced tab completer for REPL commands.
  *
  * Completes:
  *   - REPL commands (.help, .quit, .load, .reset, etc.)
  *   - JavaScript keywords
  *   - Common global objects and their properties
  *   - Object properties (e.g., Math.| shows abs, random, etc.)
  */
class REPLCompleter extends Completer {

  private val commands = Seq(
    ".help",
    ".h",
    ".quit",
    ".exit",
    ".q",
    ".load",
    ".reset",
    ".clear",
    ".debug",
    ".trace",
    ".nodebug",
    ".notrace",
    ".trace show",
    ".vars",
    ".v",
    ".vars global",
    ".vg",
    ".bt",
    ".backtrace",
    ".stack"
  )

  private val keywords = Seq(
    "var",
    "let",
    "const",
    "function",
    "return",
    "if",
    "else",
    "while",
    "for",
    "break",
    "continue",
    "true",
    "false",
    "null",
    "undefined",
    "typeof",
    "instanceof",
    "in",
    "delete",
    "new",
    "_"
  )

  private val globals = Seq(
    "console",
    "Array",
    "Object",
    "String",
    "Math",
    "Function"
  )

  // Common object properties for tab completion
  private val objectProperties = Seq(
    "toString",
    "valueOf",
    "hasOwnProperty",
    "propertyIsEnumerable"
  )

  private val arrayMethods = Seq(
    "length",
    "push",
    "pop",
    "shift",
    "unshift",
    "slice",
    "splice",
    "map",
    "filter",
    "reduce",
    "forEach",
    "find",
    "indexOf",
    "includes",
    "join",
    "sort",
    "reverse",
    "concat",
    "flat",
    "flatMap"
  )

  private val stringMethods = Seq(
    "length",
    "charAt",
    "charCodeAt",
    "concat",
    "includes",
    "endsWith",
    "indexOf",
    "lastIndexOf",
    "match",
    "replace",
    "search",
    "slice",
    "split",
    "startsWith",
    "substring",
    "toLowerCase",
    "toUpperCase",
    "trim",
    "trimLeft",
    "trimRight",
    "padStart",
    "padEnd"
  )

  private val mathProperties = Seq(
    "E",
    "LN10",
    "LN2",
    "LOG10E",
    "LOG2E",
    "PI",
    "SQRT1_2",
    "SQRT2",
    "abs",
    "acos",
    "acosh",
    "asin",
    "asinh",
    "atan",
    "atan2",
    "atanh",
    "cbrt",
    "ceil",
    "clz32",
    "cos",
    "cosh",
    "exp",
    "expm1",
    "floor",
    "fround",
    "hypot",
    "imul",
    "log",
    "log10",
    "log1p",
    "log2",
    "max",
    "min",
    "pow",
    "random",
    "round",
    "sign",
    "sin",
    "sinh",
    "sqrt",
    "tan",
    "tanh",
    "trunc"
  )

  private val consoleMethods = Seq(
    "log",
    "info",
    "warn",
    "error",
    "debug",
    "trace",
    "table",
    "count",
    "countReset",
    "group",
    "groupCollapsed",
    "groupEnd",
    "time",
    "timeLog",
    "timeEnd",
    "assert",
    "clear",
    "dir",
    "dirxml",
    "exception",
    "profile"
  )

  private val numberPrototype = Seq(
    "toExponential",
    "toFixed",
    "toPrecision",
    "toString",
    "valueOf"
  )

  override def complete(
      reader: LineReader,
      line: ParsedLine,
      candidates: List[Candidate]
  ): Unit = {
    val word = line.word()

    if word.startsWith(".") then {
      // Complete REPL commands
      val matching = commands.filter(_.startsWith(word))
      for cmd <- matching do
        candidates.add(new Candidate(cmd, cmd, null, null, null, null, true))
    }
    else if word.contains(".") then {
      // Complete object properties (e.g., Math.ab|, console.lo|, arr.ma|)
      val parts = word.split("\\.", 2)
      if parts.length == 2 then {
        val objName = parts(0)
        val propPrefix = parts(1)
        val properties = getPropertiesForObject(objName)
        val matching = properties.filter(_.startsWith(propPrefix))
        for prop <- matching do
          candidates.add(
            new Candidate(s"$objName.$prop", prop, null, null, null, null, true)
          )
      }
    }
    else {
      // Complete JavaScript keywords and globals
      val matching = (keywords ++ globals).filter(_.startsWith(word))
      for kw <- matching do
        candidates.add(new Candidate(kw, kw, null, null, null, null, true))
    }
  }

  /** Get properties for a known object.
    *
    * @param objName
    *   Object name (e.g., "Math", "Array", "console")
    * @return
    *   List of property names
    */
  private def getPropertiesForObject(objName: String): Seq[String] =
    objName.toLowerCase match {
      case "math"    => mathProperties
      case "array"   => objectProperties ++ arrayMethods
      case "string"  => objectProperties ++ stringMethods
      case "console" => consoleMethods
      case "number"  => numberPrototype
      case _         => objectProperties
    }
}
