package quickjs.module

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.runtime.builtins.BuiltinHelpers
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

/** Raised when module instantiation (linking) fails: a requested name is not
  * exported by its dependency, an indirect export cannot be resolved, or a
  * dependency fails to parse. Callers surface this as a JavaScript
  * `SyntaxError` at resolution time.
  */
final class ModuleLinkException(message: String)
    extends RuntimeException(message)

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

  // ---- Static module records (instantiation / linking) -------------------

  private enum ExportTarget:
    // Declared by this module; `localName` is the binding that provides the
    // value (which may itself be an imported binding).
    case Local(localName: String)
    // Re-exported from another module: `specifier` and the name to look up
    // there ("*" denotes the whole namespace object, for `export * as ns`).
    case Indirect(
        specifier: String,
        sourceName: String,
        attributes: Map[String, String]
    )

  private case class ImportRequest(
      specifier: String,
      names: List[String],
      attributes: Map[String, String]
  )

  private case class ModuleRecord(
      path: String,
      explicit: Map[String, ExportTarget],
      stars: List[(String, Map[String, String])],
      requests: List[ImportRequest],
      // Local name -> (specifier, imported name); a re-exported imported
      // binding resolves through the module it was imported from.
      imports: Map[String, (String, String)]
  )

  private val moduleRecords: mutable.HashMap[String, ModuleRecord] =
    mutable.HashMap.empty
  private val parsedModules: mutable.HashMap[String, quickjs.ast.Script] =
    mutable.HashMap.empty
  private val instantiated: mutable.HashSet[String] = mutable.HashSet.empty
  private val instantiating: mutable.HashSet[String] = mutable.HashSet.empty

  /** Resolved module paths that have been classified as JSON modules, and the
    * parsed JSON value shared by every import site (ES ParseJSONModule).
    */
  private val jsonModulePaths: mutable.HashSet[String] = mutable.HashSet.empty
  private val jsonModuleValues: mutable.HashMap[String, JSValue] =
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
    } else if specifier.startsWith("/") then
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
          } else
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
    } catch {
      case e: Exception =>
        Left(s"Failed to read module $path: ${e.getMessage}")
    }

  override def load(name: String): ModuleLoadResult =
    loadSource(name) match {
      case Right(source) => ModuleLoadResult(source, isModule = true)
      case Left(error)   => throw new RuntimeException(error)
    }

  /** Whether static instantiation/link validation runs for this loader. Node's
    * loader synthesizes builtin/CommonJS namespaces at runtime, so its ESM
    * graphs are left to the runtime interop layer.
    */
  protected def staticLinkingEnabled: Boolean = true

  /** Check if a module is currently being loaded (circular dependency) */
  def isLoading(path: String): Boolean =
    moduleStatus.get(path).contains(ModuleStatus.Loading)

  private def moduleExportName(value: quickjs.ast.Identifier | String): String =
    value match {
      case id: quickjs.ast.Identifier => id.name
      case s: String                  => s
    }

  private def attributeMap(
      attributes: Seq[quickjs.ast.ImportAttribute]
  ): Map[String, String] =
    attributes.map(a => a.key -> a.value).toMap

  /** A module is loaded as JSON when the `with` clause requests
    * `type: "json"` or the resolved file name ends in `.json` (matching the
    * quickjs C loader's `js_module_test_json` behavior).
    */
  private def isJsonModule(path: String, attributes: Map[String, String]): Boolean =
    attributes.get("type").contains("json") || path.endsWith(".json") ||
      jsonModulePaths.contains(path)

  /** Parse a JSON module body with `%JSON.parse%` and remember the resulting
    * value so repeated imports observe the same object identity.
    */
  private def parseJsonModule(path: String, source: String)(using
      ctx: JSContext
  ): JSValue =
    jsonModuleValues.getOrElseUpdate(
      path,
      try {
        val parse = BuiltinHelpers
          .extractJSObject(ctx.global.get("JSON"))
          .map(_.get("parse"))
          .getOrElse(JSValue.Undefined)
        parse match {
          case _: JSValue.Function | JSValue.Native(_) =>
            BuiltinHelpers.callFunctionWithThis(
              parse,
              JSValue.Undefined,
              Array(JSValue.fromString(source))
            )(using ctx)
          case _ =>
            linkError(
              s"Cannot parse JSON module '$path': JSON.parse is unavailable"
            )
        }
      } catch {
        case e: ModuleLinkException => throw e
        case e: quickjs.runtime.JSException =>
          jsonModuleValues.remove(path)
          linkError(Option(e.getMessage).getOrElse(s"Invalid JSON module '$path'"))
        case e: Exception =>
          jsonModuleValues.remove(path)
          linkError(
            Option(e.getMessage).getOrElse(s"Invalid JSON module '$path'")
          )
      }
    )

  private def bindingNames(pattern: quickjs.ast.BindingPattern): Seq[String] =
    pattern match {
      case quickjs.ast.Identifier(name, _)            => Seq(name)
      case quickjs.ast.BindingAssignment(target, _, _) => bindingNames(target)
      case quickjs.ast.ArrayPattern(elements, _)      =>
        elements.flatMap(e => Option(e).toSeq.flatMap(bindingNames))
      case quickjs.ast.ObjectPattern(properties, rest, _) =>
        properties.flatMap(p => bindingNames(p.value)) ++
          Option(rest).toSeq.flatMap(bindingNames)
      case quickjs.ast.RestElement(argument, _) => bindingNames(argument)
    }

  /** Build the static export/import record of a parsed module. */
  private def buildRecord(path: String, ast: quickjs.ast.Script): ModuleRecord = {
    val explicit = mutable.LinkedHashMap[String, ExportTarget]()
    val stars = mutable.ListBuffer[(String, Map[String, String])]()
    val requests = mutable.ListBuffer[ImportRequest]()
    val imports = mutable.LinkedHashMap[String, (String, String)]()

    ast.body.foreach {
      case quickjs.ast.ImportDeclaration(specifiers, source, attrs, _) =>
        val names = specifiers.collect {
          case quickjs.ast.ImportNamedSpecifier(imported, _, _) =>
            moduleExportName(imported)
          case quickjs.ast.ImportDefaultSpecifier(_, _) => "default"
        }
        requests += ImportRequest(source, names.toList, attributeMap(attrs))
        specifiers.foreach {
          case quickjs.ast.ImportNamedSpecifier(imported, local, _) =>
            imports(local.name) = (source, moduleExportName(imported))
          case quickjs.ast.ImportDefaultSpecifier(local, _) =>
            imports(local.name) = (source, "default")
          case quickjs.ast.ImportNamespaceSpecifier(local, _) =>
            imports(local.name) = (source, "*")
        }
      case quickjs.ast.ExportNamedDeclaration(
            declaration,
            specifiers,
            source,
            attrs,
            _
          ) =>
        if declaration != null then
          declaration match {
            case quickjs.ast.VariableDeclaration(_, declarations, _) =>
              declarations.foreach(d =>
                bindingNames(d.id).foreach(n =>
                  explicit(n) = ExportTarget.Local(n)
                )
              )
            case quickjs.ast.FunctionDeclaration(id, _, _, _, _, _, _) =>
              explicit(id.name) = ExportTarget.Local(id.name)
            case quickjs.ast.ClassDeclaration(id, _, _, _) =>
              explicit(id.name) = ExportTarget.Local(id.name)
            case _ => ()
          }
        specifiers.foreach { spec =>
          explicit(moduleExportName(spec.exported)) =
            if source == null then
              ExportTarget.Local(moduleExportName(spec.local))
            else
              ExportTarget.Indirect(
                source,
                moduleExportName(spec.local),
                attributeMap(attrs)
              )
        }
      case quickjs.ast.ExportDefaultDeclaration(_, _) =>
        explicit("default") = ExportTarget.Local("default")
      case quickjs.ast.ExportAllDeclaration(source, namespace, attrs, _) =>
        if namespace != null then
          explicit(moduleExportName(namespace)) =
            ExportTarget.Indirect(source, "*", attributeMap(attrs))
        else stars += ((source, attributeMap(attrs)))
      case _ => ()
    }

    ModuleRecord(path, explicit.toMap, stars.toList, requests.toList, imports.toMap)
  }

  /** Resolve `name` in the module at `path`. Returns the set of binding
    * identities the name resolves to: empty when not found, more than one
    * when the resolution is ambiguous (two star exports provide the name).
    */
  private def resolveExport(
      path: String,
      name: String,
      seen: Set[(String, String)]
  ): Set[String] =
    if name == "*" then Set(path + "#*")
    else if seen.contains((path, name)) then Set.empty
    else
      moduleRecords.get(path) match {
        case None => Set.empty
        case Some(record) =>
          record.explicit.get(name) match {
            case Some(ExportTarget.Local(localName)) =>
              record.imports.get(localName) match {
                case Some((specifier, importedName)) =>
                  val dep = resolveModule(specifier, path)
                  resolveExport(dep, importedName, seen + ((path, name)))
                case None => Set(path + "#" + localName)
              }
            case Some(ExportTarget.Indirect(specifier, sourceName, _)) =>
              val dep = resolveModule(specifier, path)
              resolveExport(dep, sourceName, seen + ((path, name)))
            case None =>
              // `export *` never forwards `default`.
              if name == "default" then Set.empty
              else
                record.stars.flatMap { case (specifier, _) =>
                  val dep = resolveModule(specifier, path)
                  resolveExport(dep, name, seen + ((path, name)))
                }.toSet
          }
      }

  private def linkError(message: String): Nothing =
    throw new ModuleLinkException(message)

  private def parseModule(path: String, source: String): quickjs.ast.Script =
    parsedModules.getOrElseUpdate(
      path,
      try {
        val tokens = Lexer(source).tokenize()
        new Parser(tokens, moduleMode = true, source = source).parseScript()
      } catch {
        case e: RuntimeException =>
          linkError(
            Option(e.getMessage).getOrElse(s"Failed to parse module $path")
          )
      }
    )

  /** Instantiate (parse, record and validate) a module and, recursively, all
    * of its requested modules, without evaluating any body. Mirrors ES
    * `ModuleDeclarationInstantiation`.
    */
  private def instantiateModule(
      path: String,
      attributes: Map[String, String]
  )(using ctx: JSContext): Unit = {
    if !staticLinkingEnabled then return
    if instantiated.contains(path) || instantiating.contains(path) then return

    // JSON modules have no dependencies and a single synthetic `default`
    // export; their body is parsed (and validated) at instantiation time so
    // invalid JSON is a resolution error before any body runs.
    if isJsonModule(path, attributes) then {
      jsonModulePaths += path
      if !moduleRecords.contains(path) then {
        loadSource(path) match {
          case Left(error)   => linkError(error)
          case Right(source) => parseJsonModule(path, source)
        }
        moduleRecords(path) = ModuleRecord(
          path,
          explicit = Map("default" -> ExportTarget.Local("default")),
          stars = Nil,
          requests = Nil,
          imports = Map.empty
        )
      }
      instantiated += path
      return
    }

    val ast = parsedModules.getOrElse(
      path,
      loadSource(path) match {
        case Left(error)   => linkError(error)
        case Right(source) => parseModule(path, source)
      }
    )

    if moduleRecords.contains(path) then return
    val record = buildRecord(path, ast)
    moduleRecords(path) = record
    instantiating += path
    try {
      // Records of every requested module must exist before any resolution
      // runs: instantiate (in source order) dependencies, then explicit
      // indirect sources, then star sources.
      record.requests.foreach(request =>
        instantiateModule(
          resolveModule(request.specifier, path),
          request.attributes
        )
      )
      record.explicit.foreach {
        case (_, ExportTarget.Indirect(specifier, _, attributes)) =>
          instantiateModule(resolveModule(specifier, path), attributes)
        case _ => ()
      }
      record.stars.foreach { case (specifier, attributes) =>
        instantiateModule(resolveModule(specifier, path), attributes)
      }

      // Requested names must resolve uniquely in their dependency.
      record.requests.foreach { request =>
        val dep = resolveModule(request.specifier, path)
        request.names.foreach { name =>
          val resolved = resolveExport(dep, name, Set.empty)
          if resolved.isEmpty then
            linkError(
              s"The requested module '${request.specifier}' does not provide an export named '$name'"
            )
          if resolved.size > 1 then
            linkError(
              s"The requested module '${request.specifier}' has ambiguous exports named '$name'"
            )
        }
      }
      // Indirect export entries must resolve unambiguously.
      record.explicit.foreach {
        case (name, ExportTarget.Indirect(specifier, sourceName, _)) =>
          val dep = resolveModule(specifier, path)
          val resolved = resolveExport(dep, sourceName, Set((path, name)))
          if resolved.isEmpty then
            linkError(
              s"The requested module '$specifier' does not provide an export named '$sourceName'"
            )
          if resolved.size > 1 then
            linkError(
              s"The requested module '$specifier' has ambiguous exports named '$sourceName'"
            )
        case _ => ()
      }
    } finally instantiating -= path
    instantiated += path
  }

  /** Instantiate a requested module and check the requested names against its
    * exports. Throws [[ModuleLinkException]] on any resolution failure.
    */
  def instantiate(
      specifier: String,
      fromPath: String,
      names: Seq[String],
      attributes: Map[String, String] = Map.empty
  )(using ctx: JSContext): Unit = {
    if !staticLinkingEnabled then return
    val resolvedPath = resolveModule(specifier, fromPath)
    instantiateModule(resolvedPath, attributes)
    names.foreach { name =>
      val resolved = resolveExport(resolvedPath, name, Set.empty)
      if resolved.isEmpty then
        linkError(
          s"The requested module '$specifier' does not provide an export named '$name'"
        )
      if resolved.size > 1 then
        linkError(
          s"The requested module '$specifier' has ambiguous exports named '$name'"
        )
    }
  }

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
  def loadModule(
      specifier: String,
      fromPath: String,
      attributes: Map[String, String] = Map.empty
  )(using ctx: JSContext): JSValue = {
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
          // JSON modules: parse the body and expose a single `default` export.
          // The parsed value is cached so every import site sees the same
          // object.
          if isJsonModule(resolvedPath, attributes) then {
            jsonModulePaths += resolvedPath
            val value = parseJsonModule(resolvedPath, source)
            val exports = ctx.rt.ensureModuleExports(resolvedPath)
            exports.set("default", value)
            markLoaded(resolvedPath)
            return JSValue.Object(exports)
          }

          // Parse and link (instantiate) the whole requested-module graph
          // before evaluating any body; named imports and indirect exports
          // are validated against the static export records.
          try instantiateModule(resolvedPath, attributes)
          catch {
            case e: ModuleLinkException =>
              markFailed(resolvedPath, e.getMessage)
              ctx.throwError("SyntaxError", e.getMessage)
          }

          // Parse
          val ast = parseModule(resolvedPath, source)

          // Compile as module
          val compiler = Compiler()
          val bytecode = compiler.compileModule(ast, resolvedPath)

          // Execute with module path context
          val interpreter = Interpreter()
          val previousPath = ctx.currentModulePath
          ctx.currentModulePath = resolvedPath
          try {
            val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
            // The module body is compiled async: wait for top-level await to
            // settle (driving timers / async I/O) before exposing exports.
            ModuleEvaluation.settleAndCheck(result)
          } finally
            ctx.currentModulePath = previousPath

          markLoaded(resolvedPath)
          val exports = ctx.rt.ensureModuleExports(resolvedPath)
          JSValue.Object(exports)
        } catch {
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
