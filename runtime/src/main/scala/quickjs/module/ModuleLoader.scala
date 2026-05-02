package quickjs.module

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import quickjs.objmodel.JSObject

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

/** Module loading status for handling circular dependencies */
enum ModuleStatus {
  case Unloaded
  case Loading
  case Loaded
  case Failed(error: String)
}

/** Represents a loaded module with its metadata */
case class LoadedModule(
    path: String,
    exports: JSObject,
    status: ModuleStatus
)

/** File-based module loader with file system integration.
  *
  * Handles:
  *   - Module resolution (relative paths, file extensions)
  *   - File reading
  *   - Module caching
  *   - Circular dependency detection
  */
class FileModuleLoader(basePath: Path = Paths.get(".").toAbsolutePath.normalize)
    extends ModuleLoader {

  /** Track module loading status for circular dependency detection */
  private val moduleStatus: mutable.HashMap[String, ModuleStatus] =
    mutable.HashMap.empty

  /** Cache of resolved paths to avoid repeated resolution */
  private val resolvedPaths: mutable.HashMap[(String, String), String] =
    mutable.HashMap.empty

  /** Supported file extensions in order of preference */
  private val extensions = Seq(".js", ".mjs", "")

  /** Resolve a module specifier to an absolute file path.
    *
    * @param specifier
    *   The module specifier (e.g., "./foo", "../bar", "lodash")
    * @param fromPath
    *   The path of the module doing the import (for relative resolution)
    * @return
    *   The resolved absolute path
    */
  def resolveModule(specifier: String, fromPath: String): String = {
    val cacheKey = (specifier, fromPath)
    resolvedPaths.getOrElseUpdate(cacheKey, doResolve(specifier, fromPath))
  }

  private def doResolve(specifier: String, fromPath: String): String =
    if specifier.startsWith("./") || specifier.startsWith("../") then {
      // Relative import
      val fromDir =
        if fromPath.isEmpty then basePath else Paths.get(fromPath).getParent
      val baseDir = if fromDir == null then basePath else fromDir
      resolveWithExtensions(baseDir.resolve(specifier).normalize)
    }
    else if specifier.startsWith("/") then
      // Absolute import
      resolveWithExtensions(Paths.get(specifier))
    else
      // Bare specifier - could be node_modules or built-in
      // For now, treat as relative to base path
      resolveWithExtensions(basePath.resolve(specifier).normalize)

  private def resolveWithExtensions(path: Path): String =
    // If path already has extension and exists, use it
    if Files.exists(path) && Files.isRegularFile(path) then
      path.toAbsolutePath.normalize.toString
    else {
      // Try adding extensions
      val withExtension = extensions.iterator
        .map(ext => Paths.get(path.toString + ext))
        .find(p => Files.exists(p) && Files.isRegularFile(p))

      withExtension match {
        case Some(p) => p.toAbsolutePath.normalize.toString
        case None    =>
          // Try index.js in directory
          if Files.isDirectory(path) then {
            val indexPath = path.resolve("index.js")
            if Files.exists(indexPath) then
              indexPath.toAbsolutePath.normalize.toString
            else path.toAbsolutePath.normalize.toString
          }
          else
            // Return the path as-is (will fail later if not found)
            path.toAbsolutePath.normalize.toString
      }
    }

  override def resolve(specifier: String, referrer: String): String =
    resolveModule(specifier, referrer)

  /** Load module source code from file */
  def loadSource(path: String): Either[String, String] =
    try {
      val filePath = Paths.get(path)
      if !Files.exists(filePath) then Left(s"Module not found: $path")
      else if !Files.isRegularFile(filePath) then Left(s"Not a file: $path")
      else Right(Files.readString(filePath))
    }
    catch {
      case e: Exception =>
        Left(s"Failed to read module $path: ${e.getMessage}")
    }

  override def load(name: String): ModuleLoadResult =
    loadSource(name) match {
      case Right(source) => ModuleLoadResult(source, isModule = true)
      case Left(error)   => throw new RuntimeException(error)
    }

  /** Check if a module is currently being loaded (circular dependency) */
  def isLoading(path: String): Boolean =
    moduleStatus.get(path).contains(ModuleStatus.Loading)

  /** Check if a module has been loaded */
  def isLoaded(path: String): Boolean =
    moduleStatus.get(path).contains(ModuleStatus.Loaded)

  /** Mark module as loading */
  def markLoading(path: String): Unit =
    moduleStatus(path) = ModuleStatus.Loading

  /** Mark module as loaded */
  def markLoaded(path: String): Unit =
    moduleStatus(path) = ModuleStatus.Loaded

  /** Mark module as failed */
  def markFailed(path: String, error: String): Unit =
    moduleStatus(path) = ModuleStatus.Failed(error)

  /** Get module status */
  def getStatus(path: String): ModuleStatus =
    moduleStatus.getOrElse(path, ModuleStatus.Unloaded)

  /** Load and execute a module, returning its exports.
    *
    * @param specifier
    *   The module specifier
    * @param fromPath
    *   The path of the importing module
    * @param ctx
    *   The JavaScript context
    * @return
    *   The module's exports object
    */
  def loadModule(specifier: String, fromPath: String)(using
      ctx: JSContext
  ): JSValue = {
    val resolvedPath = resolveModule(specifier, fromPath)

    // Check cache first
    ctx.rt.getModuleExports(resolvedPath) match {
      case Some(exports) if isLoaded(resolvedPath) =>
        return JSValue.Object(exports)
      case _ => ()
    }

    // Check for circular dependency
    if isLoading(resolvedPath) then {
      // Return partial exports for circular dependency
      val exports = ctx.rt.ensureModuleExports(resolvedPath)
      return JSValue.Object(exports)
    }

    // Load the module
    loadSource(resolvedPath) match {
      case Left(error) =>
        markFailed(resolvedPath, error)
        ctx.throwError(
          "Error",
          s"Cannot find module '$specifier' (resolved: $resolvedPath)"
        )
      case Right(source) =>
        markLoading(resolvedPath)
        try {
          // Parse
          val lexer = Lexer(source)
          val tokens = lexer.tokenize()
          val parser = Parser(tokens)
          val ast = parser.parseScript()

          // Compile as module
          val compiler = Compiler()
          val bytecode = compiler.compileModule(ast, resolvedPath)

          // Execute with module path context
          val interpreter = Interpreter()
          val previousPath = ctx.currentModulePath
          ctx.currentModulePath = resolvedPath
          try
            interpreter.call(bytecode, JSValue.Undefined, Array.empty)
          finally
            ctx.currentModulePath = previousPath

          markLoaded(resolvedPath)
          val exports = ctx.rt.ensureModuleExports(resolvedPath)
          JSValue.Object(exports)
        }
        catch {
          case e: quickjs.runtime.JSException =>
            markFailed(resolvedPath, e.getMessage)
            throw e
          case e: Exception =>
            markFailed(resolvedPath, e.getMessage)
            ctx.throwError(
              "Error",
              s"Failed to load module '$specifier': ${e.getMessage}"
            )
        }
    }
  }
}

object FileModuleLoader {
  def apply(
      basePath: Path = Paths.get(".").toAbsolutePath.normalize
  ): FileModuleLoader =
    new FileModuleLoader(basePath)
}
