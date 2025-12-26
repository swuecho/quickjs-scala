package quickjs.interpreter

import quickjs.bytecode.*
import quickjs.value.JSValue
import quickjs.runtime.JSContext
import scala.collection.mutable

/** Debug tracer for the interpreter.
  *
  * Provides execution tracing with:
  * - Instruction-level tracing
  * - Stack state visualization
  * - Variable inspection
  * - Function call tracking
  */
class DebugTracer:
  import DebugTracer.*

  private var enabled = false
  private var indentLevel = 0
  private val output = StringBuilder()

  /** Enable tracing.
    */
  def enable(): Unit =
    enabled = true

  /** Disable tracing.
    */
  def disable(): Unit =
    enabled = false

  /** Check if tracing is enabled.
    *
    * @return true if enabled
    */
  def isEnabled: Boolean = enabled

  /** Trace an instruction execution.
    *
    * @param pc Program counter
    * @param opcode Opcode being executed
    * @param stack Current stack (top values)
    * @param locals Current local variables
    */
  def traceInstruction(
    pc: Int,
    opcode: Opcode,
    stack: Array[JSValue],
    stackTop: Int,
    locals: Array[JSValue],
    localsCount: Int
  ): Unit =
    if !enabled then return

    val indent = "  " * indentLevel
    output.append(s"$indent[$pc] $opcode")

    // Show stack values (top 3)
    val stackVals = (Math.max(0, stackTop - 3) until stackTop).reverse.map { i =>
      stack(i) match
        case JSValue.Undefined => "undefined"
        case JSValue.Null => "null"
        case JSValue.Bool(b) => b.toString
        case JSValue.Int32(i) => i.toString
        case JSValue.Float64(d) =>
          if d == d.toLong then d.toLong.toString
          else f"$d%.2f"
        case JSValue.JSStr(s) => if s.length > 20 then s"\"${s.take(20)}...\"" else s"\"$s\""
        case _ => "?"
    }

    if stackVals.nonEmpty then
      output.append(s" | stack: [${stackVals.mkString(", ")}]")

    // Show locals if any
    if localsCount > 0 then
      val localVals = (0 until localsCount).map { i =>
        locals(i) match
          case JSValue.Undefined => "undefined"
          case JSValue.Null => "null"
          case JSValue.Bool(b) => b.toString
          case JSValue.Int32(i) => i.toString
          case JSValue.Float64(d) =>
            if d == d.toLong then d.toLong.toString
            else f"$d%.2f"
          case JSValue.JSStr(s) => if s.length > 15 then s"\"${s.take(15)}...\"" else s"\"$s\""
          case _ => "?"
      }
      output.append(s" | locals: [$localVals]")

    output.append("\n")

  /** Trace a function call.
    *
    * @param functionName Name of function being called
    * @param args Arguments being passed
    */
  def traceCall(functionName: String, args: Array[JSValue]): Unit =
    if !enabled then return

    val indent = "  " * indentLevel
    val argStr = args.take(3).map(formatValue).mkString(", ")
    output.append(s"$indent→ call $functionName($argStr)\n")
    indentLevel += 1

  /** Trace a function return.
    *
    * @param functionName Name of function returning
    * @param returnValue Return value
    */
  def traceReturn(functionName: String, returnValue: JSValue): Unit =
    if !enabled then return

    indentLevel = Math.max(0, indentLevel - 1)
    val indent = "  " * indentLevel
    output.append(s"$indent← return $functionName => ${formatValue(returnValue)}\n")

  /** Get accumulated trace output.
    *
    * @return Trace output string
    */
  def getOutput: String = output.toString

  /** Clear trace output.
    */
  def clear(): Unit =
    output.clear()
    indentLevel = 0

  /** Format a JSValue concisely.
    */
  private def formatValue(value: JSValue): String =
    value match
      case JSValue.Undefined => "undefined"
      case JSValue.Null => "null"
      case JSValue.Bool(b) => b.toString
      case JSValue.Int32(i) => i.toString
      case JSValue.Float64(d) =>
        if d == d.toLong then d.toLong.toString
        else f"$d%.2f"
      case JSValue.JSStr(s) => if s.length > 15 then s"\"${s.take(15)}...\"" else s"\"$s\""
      case _ => "?"

/** Variable inspector for debugging.
  *
  * Shows variables in current scope.
  */
class VariableInspector:
  import VariableInspector.*

  /** Get all variables from a scope.
    *
    * @param locals Local variables
    * @param localsCount Number of active locals
    * @param ctx JS context for globals
    * @return Formatted variable list
    */
  def inspectLocals(
    locals: Array[JSValue],
    localsCount: Int
  ): String =
    val sb = StringBuilder()
    sb.append("\u001B[36mLocal variables:\u001B[0m\n")

    if localsCount == 0 then
      sb.append("  (none)\n")
    else
      for i <- 0 until localsCount do
        val value = locals(i)
        val formatted = formatValue(value)
        sb.append(s"  [$i] $formatted\n")

    sb.toString()

  /** Get all global variables.
    *
    * @param ctx JS context
    * @return Formatted global variable list
    */
  def inspectGlobals(using ctx: JSContext): String =
    val sb = StringBuilder()
    sb.append("\u001B[36mGlobal variables:\u001B[0m\n")

    val globals = ctx.global
    // Get common global variables
    val commonGlobals = Seq("console", "Array", "Object", "String", "Math", "Function")

    for name <- commonGlobals do
      val value = globals.get(name)
      value match
        case JSValue.Undefined => ()
        case _ =>
          val formatted = formatValue(value)
          sb.append(s"  $name = $formatted\n")

    sb.toString()

  /** Format a value for inspection.
    */
  private def formatValue(value: JSValue, maxLength: Int = 50): String =
    value match
      case JSValue.Undefined => "\u001B[90mundefined\u001B[0m"
      case JSValue.Null => "\u001B[90mnull\u001B[0m"
      case JSValue.Bool(b) => s"\u001B[33m$b\u001B[0m"
      case JSValue.Int32(i) => s"\u001B[34m$i\u001B[0m"
      case JSValue.Float64(d) =>
        val valStr = if d == d.toLong then d.toLong.toString else f"$d%.4f"
        s"\u001B[34m$valStr\u001B[0m"
      case JSValue.JSStr(s) =>
        val display = if s.length > maxLength then s.take(maxLength) + "..." else s
        s"\u001B[32m\"$display\"\u001B[0m"
      case arr: JSValue.JSArrayVal =>
        val len = arr.value.length
        s"\u001B[35mArray($len)\u001B[0m"
      case JSValue.Object(_) =>
        s"\u001B[35mObject\u001B[0m"
      case JSValue.Native(func) =>
        s"\u001B[36mNativeFunction(<native>)\u001B[0m"
      case JSValue.Function(_, _, _, _, _, _) =>
        s"\u001B[36mFunction(<js>)\u001B[0m"

object DebugTracer:
  /** Global debug tracer instance.
    */
  val global = new DebugTracer()

object VariableInspector:
  /** Global variable inspector instance.
    */
  val global = new VariableInspector()

/** Debug commands for the REPL.
  */
enum DebugCommand:
  case Help
  case Quit
  case TraceEnable
  case TraceDisable
  case TraceShow
  case Vars
  case VarsGlobal
  case StackTrace
  case Unknown

object DebugCommand:
  /** Parse a debug command.
    *
    * @param input Command string
    * @return Parsed command
    */
  def parse(input: String): DebugCommand =
    input.toLowerCase match
      case ".help" | ".h" => DebugCommand.Help
      case ".debug" | ".trace" => DebugCommand.TraceEnable
      case ".nodebug" | ".notrace" => DebugCommand.TraceDisable
      case ".trace show" => DebugCommand.TraceShow
      case ".vars" | ".v" => DebugCommand.Vars
      case ".vars global" | ".vg" => DebugCommand.VarsGlobal
      case ".bt" | ".backtrace" | ".stack" => DebugCommand.StackTrace
      case _ if input.startsWith(".") => DebugCommand.Unknown
      case _ => DebugCommand.Unknown  // Not a command
