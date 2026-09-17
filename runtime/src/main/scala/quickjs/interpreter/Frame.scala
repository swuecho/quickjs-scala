package quickjs.interpreter

import quickjs.value.JSValue
import quickjs.runtime.JSContext
import scala.collection.mutable

/** Mutable execution frame state for the bytecode interpreter. Bundles all
  * mutable state that was previously local variables inside `call()`.
  */
private[interpreter] final class Frame(
    val stack: Array[JSValue],
    var stackTop: Int,
    var pc: Int,
    val bytecode: Array[Byte],
    val args: Array[JSValue],
    val locals: Array[JSValue.VarRef],
    var localsCount: Int,
    val thisValue: JSValue,
    val closure: mutable.Map[String, JSValue.VarRef],
    val withStack: mutable.ArrayBuffer[quickjs.objmodel.JSObject],
    val tryStack: mutable.ArrayBuffer[TryHandler],
    var lastException: JSValue,
    var pendingException: Option[JSValue],
    var result: JSValue,
    var lastResolvedName: String,
    var lastResolvedKind: String,
    var iterations: Long
)

private[interpreter] case class TryHandler(
    catchPc: Int,
    finallyPc: Int,
    stackTop: Int,
    // `with` scopes pushed when the try block was entered. Abrupt completion
    // (throw / yield-through) must unwind any with scopes opened inside the
    // protected range, or later lookups keep resolving through them.
    withStackDepth: Int = 0
)
