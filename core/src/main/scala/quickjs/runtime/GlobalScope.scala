package quickjs.runtime

import quickjs.value.JSValue
import scala.collection.mutable

/** Global scope for storing variables and functions.
 *
 * Provides a global namespace where:
 * - Function declarations are stored
 * - Variables (var/let/const at script level) are stored
 * - Lookups resolve to stored values
 */
class GlobalScope:
  import GlobalScope.*

  private val variables = mutable.HashMap[String, JSValue]()

  /** Store a variable in the global scope (including functions) */
  def setVariable(name: String, value: JSValue): Unit =
    variables(name) = value

  /** Look up a variable in the global scope */
  def getVariable(name: String): Option[JSValue] =
    variables.get(name)

  /** Store a function in the global scope (as JSValue.Function) */
  def setFunction(name: String, value: JSValue): Unit =
    variables(name) = value

  /** Look up a function in the global scope */
  def getFunction(name: String): Option[JSValue] =
    variables.get(name).filter {
      case JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => true
      case _ => false
    }

  /** Check if a name exists (variable or function) */
  def has(name: String): Boolean =
    variables.contains(name)

  /** Get all variable names */
  def variableNames: Set[String] = variables.keySet.toSet

  /** Get all function names */
  def functionNames: Set[String] =
    variables.filter { case (_, v) =>
      v match
        case JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => true
        case _ => false
    }.keySet.toSet

  /** Clear all variables and functions */
  def clear(): Unit =
    variables.clear()

  /** Get the number of items in scope */
  def size: Int = variables.size

object GlobalScope:
  def apply(): GlobalScope = new GlobalScope()
