package quickjs.util

import quickjs.value.JSValue
import quickjs.objmodel.JSArray
import quickjs.objmodel.JSObject

/** Pretty printer for JavaScript values.
  *
  * Formats JSValue for human-readable display.
  */
object PrettyPrinter:
  /** Format a JSValue as a string for display */
  def format(value: JSValue, indent: Int = 0): String =
    val ind = "  " * indent
    value match
      case JSValue.Undefined => "undefined"
      case JSValue.Null => "null"
      case JSValue.Bool(b) => b.toString
      case JSValue.Int32(i) => i.toString
      case JSValue.Float64(d) =>
        if d.isNaN then "NaN"
        else if d == Double.PositiveInfinity then "Infinity"
        else if d == Double.NegativeInfinity then "-Infinity"
        else if d == d.toLong.toDouble then d.toLong.toString
        else d.toString
      case JSValue.JSStr(s) => escapeString(s)
      case JSValue.JSArrayVal(arr) => formatArray(arr, indent)
      case JSValue.Object(obj) => formatObject(obj, indent)
      case JSValue.Function(name, _, _, _, _, _) => s"[Function: $name]"
      case JSValue.Native(nativeFunc) => s"[NativeFunction: $nativeFunc]"
      case JSValue.Symbol(id) => s"Symbol($id)"
      case JSValue.BigInt(value) => s"${value}n"

  private def formatArray(arr: JSArray, indent: Int): String =
    val ind = "  " * indent
    val contents = arr.getElements.map(format(_, indent + 1)).mkString(", ")
    s"[$contents]"

  private def formatObject(obj: JSObject, indent: Int): String =
    val ind = "  " * indent
    val props = obj.getAllProperties
    if props.isEmpty then
      "{}"
    else
      val propList = props.take(10).map { (key, value) =>
        s"$ind  $key: ${format(value, indent + 1)}"
      }.toList
      val propStr = if props.size > 10 then propList :+ s"$ind  ..." else propList
      s"{\n${propStr.mkString(",\n")}\n$ind}"

  private def escapeString(s: String): String =
    s.map {
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '"' => "\\\""
      case '\\' => "\\\\"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c => c.toString
    }.mkString("\"", "", "\"")

  /** Short format for concise display (used in REPL) */
  def shortFormat(value: JSValue): String = value match
    case JSValue.JSArrayVal(arr) =>
      val contents = arr.getElements.map(shortFormat).mkString(", ")
      s"[$contents]"
    case JSValue.Object(obj) =>
      val props = obj.getAllProperties
      if props.size <= 5 then
        val propStr = props.map { (k, v) => s"$k: ${shortFormat(v)}" }.mkString(", ")
        s"{$propStr}"
      else
        s"{...${props.size} props...}"
    case _ => format(value)
