package quickjs.runtime

import quickjs.atom.JSAtomTable
import quickjs.value.JSValue
import quickjs.objmodel.JSObject

import scala.collection.mutable

/** JavaScript runtime.
  *
  * Manages global resources:
  * - Atom table
  * - Memory management
  * - Class definitions
  */
final class JSRuntime:
  private val atomTable: JSAtomTable = JSAtomTable.initialize()
  private val classes: mutable.ArrayBuffer[JSClassDef] = mutable.ArrayBuffer.empty
  private val moduleExports: mutable.HashMap[String, JSObject] = mutable.HashMap.empty

  // Atoms
  def atom(str: String): Int = atomTable.atom(str)
  def atomString(atom: Int): String = atomTable.string(atom)

  // Classes
  def newClassID(): Int = classes.size
  def registerClass(classID: Int, classDef: JSClassDef): Unit =
    while classes.size <= classID do classes += null
    classes(classID) = classDef

  def getClass(classID: Int): Option[JSClassDef] =
    if classID >= 0 && classID < classes.size then Option(classes(classID))
    else None

  def getModuleExports(name: String): Option[JSObject] =
    moduleExports.get(name)

  def ensureModuleExports(name: String)(using ctx: JSContext): JSObject =
    moduleExports.getOrElseUpdate(name, JSObject.createOrdinary())

object JSRuntime:
  def apply(): JSRuntime = new JSRuntime()

/** Class definition for JavaScript classes.
  */
final class JSClassDef(
  val name: String,
  val classID: Int,
  val prototype: quickjs.objmodel.JSObject
)
