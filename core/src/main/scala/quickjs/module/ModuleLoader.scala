package quickjs.module

import quickjs.objmodel.JSObject
import quickjs.value.JSValue

/** Module load result containing source code and optional metadata */
case class ModuleLoadResult(
    source: String,
    isModule: Boolean = true
)

/** Trait for resolving and loading ES modules */
trait ModuleLoader:
  /** Resolve a module specifier relative to a referrer module
    *
    * @param specifier
    *   The module specifier (e.g., "./foo.js" or "bar")
    * @param referrer
    *   The referrer module name (for relative resolution)
    * @return
    *   The resolved module name/URL
    */
  def resolve(specifier: String, referrer: String): String

  /** Load a module by its resolved name
    *
    * @param name
    *   The resolved module name
    * @return
    *   The module source code and metadata
    */
  def load(name: String): ModuleLoadResult

/** Simple in-memory module loader for testing */
class InMemoryModuleLoader extends ModuleLoader:
  private val modules = scala.collection.mutable.HashMap[String, String]()

  def register(name: String, source: String): this.type =
    modules(name) = source
    this

  override def resolve(specifier: String, referrer: String): String =
    // Simple resolution: if specifier starts with ./ or ../, resolve relative to referrer
    // Otherwise, treat as absolute/name
    if specifier.startsWith("./") || specifier.startsWith("../") then
      resolveRelative(specifier, referrer)
    else specifier

  override def load(name: String): ModuleLoadResult =
    modules.get(name) match
      case Some(source) => ModuleLoadResult(source, isModule = true)
      case None => throw new RuntimeException(s"Module not found: $name")

  private def resolveRelative(specifier: String, referrer: String): String =
    // Simple relative path resolution
    val referrerDir = referrer.lastIndexOf('/') match
      case -1  => ""
      case idx => referrer.substring(0, idx)

    if specifier.startsWith("./") then
      val rest = specifier.substring(2)
      if referrerDir.isEmpty then rest else s"$referrerDir/$rest"
    else if specifier.startsWith("../") then
      val parentDir = referrerDir.lastIndexOf('/') match
        case -1  => ""
        case idx => referrerDir.substring(0, idx)
      val rest = specifier.substring(3)
      if parentDir.isEmpty then rest else s"$parentDir/$rest"
    else specifier
