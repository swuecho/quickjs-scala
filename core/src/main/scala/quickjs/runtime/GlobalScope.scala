package quickjs.runtime

import quickjs.bytecode.*
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
  private val functions = mutable.HashMap[String, BytecodeFunction]()

  /** Store a variable in the global scope */
  def setVariable(name: String, value: JSValue): Unit =
    variables(name) = value

  /** Look up a variable in the global scope */
  def getVariable(name: String): Option[JSValue] =
    variables.get(name)

  /** Store a function in the global scope */
  def setFunction(name: String, bytecode: BytecodeFunction): Unit =
    functions(name) = bytecode

  /** Look up a function in the global scope */
  def getFunction(name: String): Option[BytecodeFunction] =
    functions.get(name)

  /** Check if a name exists (variable or function) */
  def has(name: String): Boolean =
    variables.contains(name) || functions.contains(name)

  /** Get all variable names */
  def variableNames: Set[String] = variables.keySet.toSet

  /** Get all function names */
  def functionNames: Set[String] = functions.keySet.toSet

  /** Clear all variables and functions */
  def clear(): Unit =
    variables.clear()
    functions.clear()

  /** Get the number of items in scope */
  def size: Int = variables.size + functions.size

object GlobalScope:
  def apply(): GlobalScope = new GlobalScope()
