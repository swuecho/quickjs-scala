package quickjs.runtime

import quickjs.atom.JSAtomTable
import quickjs.value.JSValue
import quickjs.objmodel.JSObject
import quickjs.module.ModuleLoader

import scala.collection.mutable

/** JavaScript runtime.
  *
  * Manages global resources:
  *   - Atom table
  *   - Memory management
  *   - Class definitions
  *   - Module exports and loader
  */
final class JSRuntime {
  private val atomTable: JSAtomTable = JSAtomTable.initialize()
  private val classes: mutable.ArrayBuffer[JSClassDef] =
    mutable.ArrayBuffer.empty
  private val moduleExports: mutable.HashMap[String, JSObject] =
    mutable.HashMap.empty
  private val moduleMetaObjects: mutable.HashMap[String, JSObject] =
    mutable.HashMap.empty
  private var moduleLoader: Option[ModuleLoader] = None

  // Atoms
  def atom(str: String): Int = atomTable.atom(str)
  def atomString(atom: Int): String = atomTable.string(atom)

  // Classes
  def newClassID(): Int = classes.size
  def registerClass(classID: Int, classDef: JSClassDef): Unit = {
    while classes.size <= classID do classes += null
    classes(classID) = classDef
  }

  def getClass(classID: Int): Option[JSClassDef] =
    if classID >= 0 && classID < classes.size then Option(classes(classID))
    else None

  // Module loader
  def setModuleLoader(loader: ModuleLoader): Unit =
    moduleLoader = Some(loader)

  def getModuleLoader: Option[ModuleLoader] = moduleLoader

  def resolveModule(specifier: String, referrer: String): String =
    moduleLoader match {
      case Some(loader) => loader.resolve(specifier, referrer)
      case None         => specifier
    }

  /** Get the module loader if configured */
  def getModuleLoaderOption: Option[ModuleLoader] = moduleLoader

  // Module exports
  def getModuleExports(name: String): Option[JSObject] =
    moduleExports.get(name)

  def ensureModuleExports(name: String)(using ctx: JSContext): JSObject =
    moduleExports.getOrElseUpdate(name, JSObject.createOrdinary())

  def ensureModuleMeta(name: String)(using ctx: JSContext): JSObject =
    moduleMetaObjects.getOrElseUpdate(
      name, {
        val meta = JSObject(prototype = null, extensible = true)
        val url =
          try
            val path = java.nio.file.Paths.get(name)
            if path.isAbsolute then path.toUri.toString else name
          catch case _: Exception => name
        meta.defineProperty(
          "url",
          JSValue.fromString(url),
          enumerable = true,
          writable = true,
          configurable = true
        )
        meta
      }
    )

  def clearModuleExports(name: String): Unit =
    moduleExports.remove(name)

  def clearAllModuleExports(): Unit =
    moduleExports.clear()

  def clearModuleMeta(name: String): Unit =
    moduleMetaObjects.remove(name)

  def clearAllModuleMeta(): Unit =
    moduleMetaObjects.clear()
}

object JSRuntime {
  def apply(): JSRuntime = new JSRuntime()
}

/** Class definition for JavaScript classes.
  */
final class JSClassDef(
    val name: String,
    val classID: Int,
    val prototype: quickjs.objmodel.JSObject
)
