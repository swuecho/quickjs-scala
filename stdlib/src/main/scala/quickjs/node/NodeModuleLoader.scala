package quickjs.node

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.module.{FileModuleLoader, ModuleLoadResult}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.value.{JSValue, NativeFunction}
import quickjs.objmodel.{JSArray, JSObject}

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

/** Options controlling the Node compatibility layer. */
final case class NodeOptions(
    argv: Vector[String] = Vector("node"),
    cwd: Path = Paths.get("").toAbsolutePath.normalize,
    env: Map[String, String] = sys.env,
    version: String = "v22.0.0"
)

/** Result of resolving a `require`/`import` specifier. */
enum NodeResolution {
  case Builtin(value: JSValue)
  case JsonFile(path: String)
  case CjsFile(path: String)
  case EsmFile(path: String)
}

/** Node-compatible module loader: `require()`, `node_modules` lookup,
  * `package.json` (`main`, `exports`, `type`), JSON modules and CommonJS
  * execution, layered over the existing ESM [[FileModuleLoader]].
  */
final class NodeModuleLoader(
    basePath: Path = Paths.get("").toAbsolutePath.normalize,
    val options: NodeOptions = NodeOptions()
) extends FileModuleLoader(basePath) {

  private val builtins = mutable.LinkedHashMap.empty[String, JSValue]

  /** `require.extensions` (shared by every `require` and the `module` builtin). */
  private var requireExtensionsObj: JSObject = null
  private val registeredExtensions = mutable.LinkedHashSet.empty[String]
  private val defaultExtensionHandlers = mutable.HashMap.empty[String, JSValue]

  // CommonJS module cache: resolved path -> module.exports value.
  private val cjsCache = mutable.HashMap.empty[String, JSValue]
  private val cjsLoading = mutable.HashSet.empty[String]
  private val partialModules = mutable.HashMap.empty[String, JSObject]
  private val packageJsonCache =
    mutable.HashMap.empty[String, Option[MiniJson.JsObject]]

  /** Register a built-in module (e.g. "fs", "path"). */
  def registerBuiltin(name: String, exports: JSValue): Unit = {
    builtins(name) = exports
    builtins(s"node:$name") = exports
  }

  def isBuiltin(specifier: String): Boolean =
    builtins.contains(specifier) ||
      (specifier.startsWith("node:") && builtins.contains(specifier.drop(5)))

  def builtin(specifier: String): Option[JSValue] =
    builtins
      .get(specifier)
      .orElse(if specifier.startsWith("node:") then
        builtins.get(specifier.drop(5))
      else None)

  // =========================================================================
  // Resolution
  // =========================================================================

  /** Resolve a specifier for `require`/`import`, throwing a JS error when the
    * module cannot be found. Prefer [[tryResolve]] from contexts without a
    * JSContext.
    */
  def resolveSpecifier(specifier: String, fromFile: String)(using
      ctx: JSContext
  ): NodeResolution = {
    extensionObject()
    tryResolve(specifier, fromFile) match {
      case Right(resolved) => resolved
      case Left(message)   =>
        NodeHelpers.throwCoded("Error", message, "MODULE_NOT_FOUND")
    }
  }

  /** Pure resolution (no JSContext); returns a message on failure. */
  def tryResolve(
      specifier: String,
      fromFile: String
  ): Either[String, NodeResolution] = {
    builtin(specifier) match {
      case Some(value) => return Right(NodeResolution.Builtin(value))
      case None        => ()
    }

    val fromDir =
      if fromFile == null || fromFile.isEmpty then basePath
      else Option(Paths.get(fromFile).getParent).getOrElse(basePath)

    val candidate: Option[Path] =
      if specifier.startsWith("./") || specifier.startsWith("../") then
        resolveAsFileOrDirectory(fromDir.resolve(specifier).normalize)
      else if specifier.startsWith("/") then
        resolveAsFileOrDirectory(Paths.get(specifier).normalize)
      else if specifier.startsWith("#") then
        resolvePackageImport(specifier, fromDir)
      else if specifier.startsWith("file:") then
        try {
          val urlPath = NodeUrl.fileUrlStringToPath(specifier)
          resolveAsFileOrDirectory(Paths.get(urlPath).normalize)
        } catch case _: IllegalArgumentException => None
      else resolveBare(specifier, fromDir)

    candidate match {
      case None =>
        Left(s"Cannot find module '$specifier' required from '$fromFile'")
      case Some(path) =>
        if path.toString.endsWith(".json") then
          Right(NodeResolution.JsonFile(path.toString))
        else
          kindOf(path) match {
            case ModuleKind.Esm => Right(NodeResolution.EsmFile(path.toString))
            case ModuleKind.Cjs => Right(NodeResolution.CjsFile(path.toString))
          }
    }
  }

  private enum ModuleKind {
    case Cjs, Esm
  }

  private def kindOf(path: Path): ModuleKind = {
    val name = path.getFileName.toString
    if name.endsWith(".mjs") then ModuleKind.Esm
    else if name.endsWith(".cjs") || name.endsWith(".json") then ModuleKind.Cjs
    else {
      // Nearest package.json "type" wins; a package.json without a `type`
      // field means CommonJS and stops the lookup (it must not inherit an
      // outer package's `type`).
      var dir = Option(path.getParent).getOrElse(basePath)
      var result: Option[ModuleKind] = None
      while dir != null && result.isEmpty do {
        packageJsonFor(dir) match {
          case Some(pkg) =>
            pkg.fields.get("type") match {
              case Some(MiniJson.JsString("module")) => result = Some(ModuleKind.Esm)
              case Some(MiniJson.JsString(_))        => result = Some(ModuleKind.Cjs)
              case _                                 => result = Some(ModuleKind.Cjs)
            }
          case None => ()
        }
        dir = dir.getParent
      }
      result.getOrElse(ModuleKind.Cjs)
    }
  }

  /** Resolve a `#subpath` import declared in the nearest package.json's
    * `imports` field, as used by packages such as chalk.
    */
  private def resolvePackageImport(
      specifier: String,
      fromDir: Path
  ): Option[Path] = {
    var dir: Path | Null = fromDir
    while dir != null do {
      packageJsonFor(dir) match {
        case Some(pkg) =>
          pkg.fields.get("imports") match {
            case Some(MiniJson.JsObject(imports)) =>
              imports.get(specifier) match {
                case Some(target) =>
                  return resolveCondition(target).flatMap { relative =>
                    resolveAsFileOrDirectory(dir.resolve(relative.stripPrefix("./")))
                  }
                case None => return None
              }
            case _ => return None
          }
        case None => dir = dir.getParent
      }
    }
    None
  }

  /** Resolve a package specifier through `node_modules`, walking up from the
    * requiring directory.
    */
  private def resolveBare(specifier: String, fromDir: Path): Option[Path] = {
    val (packageName, subpath) = splitPackageSpecifier(specifier)
    var dir: Path | Null = fromDir
    var result: Option[Path] = None
    while dir != null && result.isEmpty do {
      val nodeModules = dir.resolve("node_modules")
      if Files.isDirectory(nodeModules) then
        result = resolvePackage(nodeModules.resolve(packageName), subpath)
      dir = dir.getParent
    }
    result
  }

  private def splitPackageSpecifier(specifier: String): (String, String) = {
    val parts = specifier.split("/")
    if specifier.startsWith("@") && parts.length >= 2 then
      (
        s"${parts(0)}/${parts(1)}",
        if parts.length > 2 then parts.drop(2).mkString("/") else ""
      )
    else
      (parts(0), if parts.length > 1 then parts.drop(1).mkString("/") else "")
  }

  private def resolvePackage(
      packageDir: Path,
      subpath: String
  ): Option[Path] = {
    if !Files.isDirectory(packageDir) then return None
    packageJsonFor(packageDir) match {
      case Some(pkg) =>
        val exportsTarget =
          pkg.fields.get("exports").flatMap(exports => resolveExports(exports, subpath))
        exportsTarget match {
          case Some(relative) =>
            val target = packageDir.resolve(relative.stripPrefix("./"))
            return resolveAsFileOrDirectory(target)
          case None => ()
        }
        if subpath.nonEmpty then
          resolveAsFileOrDirectory(packageDir.resolve(subpath))
        else {
          pkg.fields.get("main") match {
            case Some(MiniJson.JsString(main)) if main.nonEmpty =>
              resolveAsFileOrDirectory(packageDir.resolve(main))
                .orElse(resolveAsDirectory(packageDir))
            case _ => resolveAsDirectory(packageDir)
          }
        }
      case None =>
        if subpath.nonEmpty then
          resolveAsFileOrDirectory(packageDir.resolve(subpath))
        else resolveAsDirectory(packageDir)
    }
  }

  /** Very small subset of package `exports` support: string targets, `"."`
    * subpath maps, condition objects with `require`/`import`/`node`/`default`,
    * and arrays. `require` and `import` both accept any condition that is not
    * explicitly the other one.
    */
  private def resolveExports(
      exports: MiniJson.Value,
      subpath: String
  ): Option[String] = {
    val key = if subpath.isEmpty then "." else s"./$subpath"
    exports match {
      case MiniJson.JsString(target) if subpath.isEmpty =>
        normalizeExportTarget(target)
      case MiniJson.JsObject(fields)
          if fields.keys.exists(_.startsWith(".")) =>
        fields.get(key).flatMap(resolveCondition)
      case MiniJson.JsObject(fields) if subpath.isEmpty =>
        resolveCondition(MiniJson.JsObject(fields))
      case MiniJson.JsArray(values) =>
        values.iterator
          .flatMap(value => resolveExports(value, subpath).iterator)
          .nextOption()
      case _ => None
    }
  }

  private def resolveCondition(value: MiniJson.Value): Option[String] =
    value match {
      case MiniJson.JsString(target) => normalizeExportTarget(target)
      case MiniJson.JsArray(values)  =>
        values.iterator.flatMap(v => resolveCondition(v).iterator).nextOption()
      case MiniJson.JsObject(fields) =>
        val preferred = Seq("node", "require", "import", "default")
        preferred.iterator
          .flatMap(key => fields.get(key).flatMap(resolveCondition).iterator)
          .nextOption()
      case _ => None
    }

  private def normalizeExportTarget(target: String): Option[String] =
    if target.startsWith("./") then Some(target)
    else None

  private def resolveAsFileOrDirectory(path: Path): Option[Path] = {
    resolveAsFile(path).orElse(resolveAsDirectory(path))
  }

  private def resolveAsFile(path: Path): Option[Path] = {
    if Files.isRegularFile(path) then Some(path.toAbsolutePath.normalize)
    else {
      // `require.extensions` keys participate in resolution (ts-node's `.ts`).
      val candidates =
        (registeredExtensions.toSeq ++ Seq(".js", ".json", ".cjs", ".mjs")).distinct
      candidates.iterator
        .map(ext => Paths.get(path.toString + ext))
        .find(Files.isRegularFile(_))
        .map(_.toAbsolutePath.normalize)
        .orElse {
          if Files.exists(path) && path.toString.endsWith(".node") then
            throw new RuntimeException(
              s"Native addon modules (.node) are not supported: $path"
            )
          None
        }
    }
  }

  private def resolveAsDirectory(dir: Path): Option[Path] = {
    if !Files.isDirectory(dir) then None
    else {
      packageJsonFor(dir) match {
        case Some(pkg) =>
          pkg.fields.get("main") match {
            case Some(MiniJson.JsString(main)) if main.nonEmpty =>
              resolveAsFileOrDirectory(dir.resolve(main))
                .orElse(resolveIndex(dir))
            case _ => resolveIndex(dir)
          }
        case None => resolveIndex(dir)
      }
    }
  }

  private def resolveIndex(dir: Path): Option[Path] = {
    Seq("index.js", "index.json", "index.cjs", "index.mjs").iterator
      .map(name => dir.resolve(name))
      .find(Files.isRegularFile(_))
      .map(_.toAbsolutePath.normalize)
  }

  private def packageJsonFor(dir: Path): Option[MiniJson.JsObject] =
    packageJsonCache.getOrElseUpdate(
      dir.toString, {
        val file = dir.resolve("package.json")
        if Files.isRegularFile(file) then
          try {
            MiniJson.parse(Files.readString(file)) match {
              case obj: MiniJson.JsObject => Some(obj)
              case _                      => None
            }
          } catch case _: Throwable => None
        else None
      }
    )

  override def resolveModule(specifier: String, fromPath: String): String =
    tryResolve(specifier, fromPath) match {
      case Right(NodeResolution.JsonFile(path)) => path
      case Right(NodeResolution.CjsFile(path))  => path
      case Right(NodeResolution.EsmFile(path))  => path
      case _                                    => specifier
    }

  override def resolve(specifier: String, referrer: String): String =
    resolveModule(specifier, referrer)

  // =========================================================================
  // Loading
  // =========================================================================

  override def load(name: String): ModuleLoadResult =
    if isBuiltin(name) then
      throw new RuntimeException(s"Built-in module '$name' has no source text")
    else
      loadSource(name) match {
        case Right(source) =>
          ModuleLoadResult(source, isModule = kindOf(Paths.get(name)) == ModuleKind.Esm)
        case Left(error) => throw new RuntimeException(error)
      }

  /** Static module linking is disabled: Node resolves builtins and CommonJS
    * interop at runtime, so export-name checks would reject valid imports.
    */
  override protected def staticLinkingEnabled: Boolean = false

  override def loadModule(
      specifier: String,
      fromPath: String,
      attributes: Map[String, String] = Map.empty
  )(using ctx: JSContext): JSValue =
    resolveSpecifier(specifier, fromPath) match {
      case NodeResolution.Builtin(value) => builtinNamespace(value)
      case NodeResolution.JsonFile(path) =>
        jsonNamespace(MiniJson.parse(Files.readString(Paths.get(path))))
      case NodeResolution.CjsFile(path) =>
        cjsNamespace(loadCJSModule(path))
      case NodeResolution.EsmFile(path) =>
        super.loadModule(path, fromPath, attributes)
    }

  /** `require()` a specifier relative to `fromFile`. */
  def requireFrom(specifier: String, fromFile: String)(using
      ctx: JSContext
  ): JSValue =
    resolveSpecifier(specifier, fromFile) match {
      case NodeResolution.Builtin(value) => value
      case NodeResolution.JsonFile(path) =>
        MiniJson.toJSValue(MiniJson.parse(Files.readString(Paths.get(path))))
      case NodeResolution.CjsFile(path) =>
        loadCJSModule(path)
      case NodeResolution.EsmFile(path) =>
        // require(esm) returns the module namespace object, unless the module
        // declares a `'module.exports'` export (Node's CJS interop marker) or
        // is a pure CJS re-export wrapper like
        // `export { default } from './index.cjs'; export * from './index.cjs'`
        // (mocha), in which case the CJS value is returned.
        val ns = super.loadModule(path, fromFile)
        ns match {
          case JSValue.Object(obj) =>
            obj.getOwnProperty("module.exports")(using ctx) match {
              case Some(value) if value != JSValue.Undefined => value
              case _ =>
                cjsReexportDefault(obj).getOrElse(ns)
            }
          case _ => ns
        }
    }

  /** Detect `export { default } from './x.cjs'; export * from './x.cjs'`
    * wrappers: there is at least one named export and every named export
    * mirrors a property of `default`, which is how Node's `require(esm)`
    * recognizes a CJS module re-exported through an ESM facade.
    */
  private def cjsReexportDefault(ns: JSObject)(using
      ctx: JSContext
  ): Option[JSValue] = {
    val default =
      BuiltinHelpers.getPropertyWithGetter(JSValue.Object(ns), "default")
    if default == JSValue.Undefined || default == JSValue.Null then None
    else {
      val keys = ns
        .getAllOwnPropertyKeys()
        .collect { case key: String => key }
        .filterNot(key => key == "default" || key == "__esModule")
      if keys.isEmpty then None
      else {
        val mirrors = keys.forall { key =>
          val value =
            BuiltinHelpers.getPropertyWithGetter(JSValue.Object(ns), key)
          value != JSValue.Undefined &&
          BuiltinHelpers.getPropertyWithGetter(default, key) == value
        }
        if mirrors then Some(default) else None
      }
    }
  }

  private def jsonNamespace(value: MiniJson.Value)(using ctx: JSContext): JSValue = {
    val parsed = MiniJson.toJSValue(value)
    val ns = JSObject(prototype = null)
    ns.set("default", parsed)
    parsed match {
      case JSValue.Object(obj) =>
        obj.getAllOwnPropertyKeys().foreach { key =>
          obj.getOwnPropertyDescriptor(key).foreach { case (v, attrs) =>
            if attrs.enumerable && !key.startsWith("__") then ns.set(key, v)
          }
        }
      case _ => ()
    }
    JSValue.Object(ns)
  }

  /** Wrap a CommonJS/built-in exports value in an ESM namespace object:
    * `default` is the value itself and enumerable own properties become named
    * exports (Node's CJS->ESM interop).
    */
  private def builtinNamespace(exports: JSValue)(using ctx: JSContext): JSValue =
    cjsNamespace(exports)

  /** Wrap a CommonJS `module.exports` value in an ESM namespace object. */
  private def cjsNamespace(exports: JSValue)(using ctx: JSContext): JSValue = {
    val ns = JSObject(prototype = null)
    ns.set("default", exports)
    // Native functions/constructors expose their module surface on funcObj.
    BuiltinHelpers.extractJSObject(exports).foreach { obj =>
      // `import * as ns from ...; ns.method(...)` passes the namespace object
      // as the receiver; module methods strip the canonical object instead.
      NodeHelpers.registerReceiverAlias(ns, obj)
      obj.getAllOwnPropertyKeys().foreach { key =>
        obj.getOwnPropertyDescriptor(key).foreach { case (_, attrs) =>
          // Read through `[[Get]]`: TypeScript-compiled CJS packages define
          // their exports as enumerable getters
          // (`Object.defineProperty(exports, "map", { enumerable: true, get })`),
          // so the descriptor's value is not the export value.
          if attrs.enumerable && !key.startsWith("__") then
            // Invoke accessors: TypeScript-compiled CJS packages define their
            // exports as enumerable getters, and the raw object lookup does
            // not trigger them.
            ns.set(
              key,
              BuiltinHelpers.getPropertyWithGetter(JSValue.Object(obj), key)
            )
        }
      }
    }
    JSValue.Object(ns)
  }

  /** Load a CommonJS module from an absolute path (cache + circular-dependency
    * handling).
    */
  def loadCJSModule(absPath: String)(using ctx: JSContext): JSValue = {
    cjsCache.get(absPath) match {
      case Some(value) => value
      case None =>
        if cjsLoading.contains(absPath) then
          partialModules
            .get(absPath)
            .map(module => module.get("exports"))
            .getOrElse(JSValue.Undefined)
        else {
          val path = Paths.get(absPath)
          customExtensionHandler(extensionOf(absPath)) match {
            case Some(handler) =>
              val (moduleObj, _) = makeModuleObject(absPath)
              cjsLoading += absPath
              partialModules(absPath) = moduleObj
              try
                BuiltinHelpers.callFunctionWithThis(
                  handler,
                  JSValue.Undefined,
                  Array(
                    JSValue.Object(moduleObj),
                    JSValue.fromString(absPath)
                  )
                )
              finally {
                cjsLoading -= absPath
                partialModules -= absPath
              }
              moduleObj.set("loaded", JSValue.Bool(true))
              val result = moduleObj.get("exports")
              cjsCache(absPath) = result
              result
            case None =>
              val source =
                try Files.readString(path)
                catch {
                  case e: Exception =>
                    NodeHelpers.throwCoded(
                      "Error",
                      s"Cannot find module '$absPath': ${e.getMessage}",
                      "MODULE_NOT_FOUND"
                    )
                }
              executeCJS(source, absPath)(using ctx)
          }
        }
    }
  }

  private def extensionOf(path: String): String = {
    val name = Paths.get(path).getFileName.toString
    val dot = name.lastIndexOf('.')
    if dot > 0 then name.substring(dot) else ""
  }

  /** The shared `require.extensions` object, with Node's default handlers.
    * Refreshes the extension list used by resolution.
    */
  private[node] def extensionObject()(using ctx: JSContext): JSObject = {
    if requireExtensionsObj == null then {
      val obj = JSObject(prototype = ctx.objectPrototype)
      def stripSelf(args: Array[JSValue]): Array[JSValue] =
        args.headOption match {
          case Some(JSValue.Object(o)) if o eq obj => args.drop(1)
          case _                                   => args
        }
      val jsHandler = NativeFunction(
        name = ".js",
        length = 2,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val rest = stripSelf(args)
          rest.headOption match {
            case Some(JSValue.Object(moduleObj)) =>
              val filename = rest
                .lift(1)
                .map(NodeHelpers.toStr(_))
                .getOrElse(moduleObj.get("filename").toString)
              val source =
                try Files.readString(Paths.get(filename))
                catch
                  case e: Exception =>
                    NodeHelpers.throwCoded(
                      "Error",
                      s"Cannot find module '$filename': ${e.getMessage}",
                      "MODULE_NOT_FOUND"
                    )
              val compile = BuiltinHelpers.getPropertyWithGetter(
                JSValue.Object(moduleObj),
                "_compile"
              )
              BuiltinHelpers.callFunctionWithThis(
                compile,
                JSValue.Object(moduleObj),
                Array(
                  JSValue.fromString(source),
                  JSValue.fromString(filename)
                )
              )
              JSValue.Undefined
            case _ => JSValue.Undefined
          }
        }
      )
      val jsonHandler = NativeFunction(
        name = ".json",
        length = 2,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val rest = stripSelf(args)
          rest.headOption match {
            case Some(JSValue.Object(moduleObj)) =>
              val filename = rest
                .lift(1)
                .map(NodeHelpers.toStr(_))
                .getOrElse(moduleObj.get("filename").toString)
              val source = Files.readString(Paths.get(filename))
              val json = callCtx.global.get("JSON")
              val parse =
                BuiltinHelpers.getPropertyWithGetter(json, "parse")
              val parsed = BuiltinHelpers.callFunctionWithThis(
                parse,
                json,
                Array(JSValue.fromString(source))
              )
              moduleObj.set("exports", parsed)
              JSValue.Undefined
            case _ => JSValue.Undefined
          }
        }
      )
      obj.set(".js", JSValue.Native(jsHandler))
      obj.set(".json", JSValue.Native(jsonHandler))
      defaultExtensionHandlers(".js") = JSValue.Native(jsHandler)
      defaultExtensionHandlers(".json") = JSValue.Native(jsonHandler)
      requireExtensionsObj = obj
    }
    registeredExtensions.clear()
    requireExtensionsObj.getAllOwnPropertyKeys().foreach {
      case key: String if key.startsWith(".") => registeredExtensions += key
      case _                                  => ()
    }
    requireExtensionsObj
  }

  /** A user-registered handler for `ext`, if it is not one of our defaults. */
  private def customExtensionHandler(ext: String)(using
      ctx: JSContext
  ): Option[JSValue] =
    if ext.isEmpty then None
    else {
      val obj = extensionObject()
      obj.getOwnProperty(ext) match {
        case Some(handler) if BuiltinHelpers.isCallable(handler) =>
          val isDefault = defaultExtensionHandlers
            .get(ext)
            .exists(default => NodeHelpers.sameValue(default, handler))
          if isDefault then None else Some(handler)
        case _ => None
      }
    }

  /** Build the CommonJS module object (`exports`, `require`, `_compile`, ...). */
  private def makeModuleObject(filename: String)(using
      ctx: JSContext
  ): (JSObject, JSValue) = {
    val moduleObj = JSObject(prototype = ctx.objectPrototype)
    val exportsObj = JSObject(prototype = ctx.objectPrototype)
    moduleObj.initProperty("exports", JSValue.Object(exportsObj), enumerable = true, writable = true, configurable = false)
    moduleObj.initProperty("filename", JSValue.fromString(filename), enumerable = true, writable = true, configurable = false)
    moduleObj.initProperty("id", JSValue.fromString(filename), enumerable = true, writable = true, configurable = false)
    moduleObj.initProperty("loaded", JSValue.Bool(false), enumerable = true, writable = true, configurable = false)
    val parentDir = Option(Paths.get(filename).getParent).getOrElse(basePath)
    moduleObj.initProperty("path", JSValue.fromString(parentDir.toString), enumerable = true, writable = true, configurable = false)
    val requireFn = createRequire(filename)
    moduleObj.set("require", requireFn)
    val compileFn = NativeFunction(
      name = "_compile",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = args.headOption match {
          case Some(JSValue.Object(o)) if o eq moduleObj => args.drop(1)
          case _                                         => args
        }
        val source = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
        val compileFilename = rest
          .lift(1)
          .map(NodeHelpers.toStr(_))
          .getOrElse(filename)
        runModuleSource(source, compileFilename, moduleObj, requireFn)
        JSValue.Undefined
      }
    )
    moduleObj.set("_compile", JSValue.Native(compileFn))
    (moduleObj, requireFn)
  }

  /** Run CommonJS source inside an existing module object. */
  private def runModuleSource(
      source0: String,
      filename: String,
      moduleObj: JSObject,
      requireFn: JSValue
  )(using ctx: JSContext): Unit = {
    val source =
      if source0.startsWith("#!") then "//" + source0.substring(2) else source0
    val parentDir = Option(Paths.get(filename).getParent).getOrElse(basePath)
    val wrapper =
      try compileWrapper(source)
      catch {
        case e: Exception =>
          throw new RuntimeException(
            s"Failed to parse CommonJS module $filename: ${e.getMessage}",
            e
          )
      }
    val thisArg = moduleObj.get("exports")
    BuiltinHelpers.callFunctionWithThis(
      wrapper,
      thisArg,
      Array(
        thisArg,
        requireFn,
        JSValue.Object(moduleObj),
        JSValue.fromString(filename),
        JSValue.fromString(parentDir.toString)
      )
    )
  }

  /** Execute CommonJS source as if it were the body of Node's module wrapper. */
  def executeCJS(source0: String, filename: String)(using
      ctx: JSContext
  ): JSValue = {
    val (moduleObj, requireFn) = makeModuleObject(filename)
    cjsLoading += filename
    partialModules(filename) = moduleObj
    try runModuleSource(source0, filename, moduleObj, requireFn)
    finally {
      cjsLoading -= filename
      partialModules -= filename
    }
    moduleObj.set("loaded", JSValue.Bool(true))
    val result = moduleObj.get("exports")
    cjsCache(filename) = result
    result
  }

  /** Compile `(function (exports, require, module, __filename, __dirname) {
    * ...source... })` and return the resulting function value. The wrapper is
    * kept on the first line so reported line numbers stay aligned with the
    * original file.
    */
  private def compileWrapper(source: String)(using ctx: JSContext): JSValue = {
    val prefix =
      "(function (exports, require, module, __filename, __dirname) {"
    val wrapped = prefix + source + "\n})"
    val tokens = Lexer(wrapped).tokenize()
    val ast = new Parser(tokens).parseScript()
    val bytecode = Compiler().compileScript(ast)
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty)
  }

  /** The `module` builtin: `createRequire`/`createRequireFromPath`,
    * `builtinModules` and `isBuiltin`.
    */
  def createModuleBuiltin()(using ctx: JSContext): JSObject = {
    val moduleObj = JSObject(prototype = ctx.objectPrototype)

    def pathArgument(args: Array[JSValue]): String = {
      val rest = NodeHelpers.stripReceiver(args, moduleObj)
      rest.headOption match {
        case Some(JSValue.JSStr(s)) if s.startsWith("file:") =>
          try java.nio.file.Paths.get(java.net.URI.create(s)).toString
          catch case _: Throwable => s
        case Some(other) => NodeHelpers.toPath(other)
        case None        => basePath.toString
      }
    }

    val createRequireFn = NativeFunction(
      name = "createRequire",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        createRequire(pathArgument(args))
      }
    )
    moduleObj.defineProperty(
      "createRequire",
      JSValue.Native(createRequireFn),
      enumerable = true
    )
    moduleObj.defineProperty(
      "createRequireFromPath",
      JSValue.Native(createRequireFn),
      enumerable = true
    )

    val isBuiltinFn = NativeFunction(
      name = "isBuiltin",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        args.headOption match {
          case Some(JSValue.JSStr(s)) => JSValue.Bool(isBuiltin(s))
          case _                      => JSValue.Bool(false)
        }
      }
    )
    moduleObj.defineProperty(
      "isBuiltin",
      JSValue.Native(isBuiltinFn),
      enumerable = true
    )
    val extensions = extensionObject()
    moduleObj.defineProperty("extensions", JSValue.Object(extensions), enumerable = true)
    moduleObj.defineProperty("_extensions", JSValue.Object(extensions), enumerable = true)
    moduleObj.defineProperty("_cache", JSValue.Object(cacheObject), enumerable = true)

    // The `Module` class surface ts-node / source-map-support poke at.
    val moduleClass = JSObject(prototype = ctx.objectPrototype)
    moduleClass.set("_extensions", JSValue.Object(extensions))
    moduleClass.set("_cache", JSValue.Object(cacheObject))
    moduleClass.set(
      "wrap",
      JSValue.Native(
        NativeFunction(
          name = "wrap",
          length = 1,
          impl = (args, _) => {
            val rest = NodeHelpers.stripReceiver(args, moduleClass)
            val source = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            JSValue.fromString(
              "(function (exports, require, module, __filename, __dirname) {" +
                source + "\n})"
            )
          }
        )
      )
    )
    moduleClass.set(
      "_preloadModules",
      JSValue.Native(
        NativeFunction(
          name = "_preloadModules",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            NodeHelpers.stripReceiver(args, moduleClass).headOption.foreach {
              case JSValue.JSArrayVal(arr) =>
                var i = 0
                while i < arr.getLength do {
                  requireFrom(
                    NodeHelpers.toStr(arr.get(i)),
                    basePath.resolve("__preload__").toString
                  )
                  i += 1
                }
              case other =>
                requireFrom(
                  NodeHelpers.toStr(other),
                  basePath.resolve("__preload__").toString
                )
            }
            JSValue.Undefined
          }
        )
      )
    )
    moduleClass.set(
      "_resolveFilename",
      JSValue.Native(
        NativeFunction(
          name = "_resolveFilename",
          length = 4,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = NodeHelpers.stripReceiver(args, moduleClass)
            val request = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            val parent = rest.lift(1)
            val fromFile = parent match {
              case Some(JSValue.Object(obj)) =>
                obj.get("filename") match {
                  case JSValue.JSStr(s) => s
                  case _                => basePath.resolve("__index__").toString
                }
              case Some(JSValue.JSStr(s)) => s
              case _ => basePath.resolve("__index__").toString
            }
            builtin(request) match {
              case Some(_) => JSValue.fromString(request)
              case None =>
                tryResolve(request, fromFile) match {
                  case Right(NodeResolution.JsonFile(path)) =>
                    JSValue.fromString(path)
                  case Right(NodeResolution.CjsFile(path)) =>
                    JSValue.fromString(path)
                  case Right(NodeResolution.EsmFile(path)) =>
                    JSValue.fromString(path)
                  case Right(NodeResolution.Builtin(_)) =>
                    JSValue.fromString(request)
                  case Left(message) =>
                    NodeHelpers.throwCoded("Error", message, "MODULE_NOT_FOUND")
                }
            }
          }
        )
      )
    )
    moduleClass.set(
      "_findPath",
      JSValue.Native(
        NativeFunction(
          name = "_findPath",
          length = 3,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = NodeHelpers.stripReceiver(args, moduleClass)
            val request = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            tryResolve(request, basePath.resolve("__index__").toString) match {
              case Right(NodeResolution.JsonFile(path)) =>
                JSValue.fromString(path)
              case Right(NodeResolution.CjsFile(path)) =>
                JSValue.fromString(path)
              case Right(NodeResolution.EsmFile(path)) =>
                JSValue.fromString(path)
              case _ => JSValue.Bool(false)
            }
          }
        )
      )
    )
    moduleClass.set(
      "createRequire",
      JSValue.Native(
        NativeFunction(
          name = "createRequire",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = NodeHelpers.stripReceiver(args, moduleClass)
            val from = rest.headOption match {
              case Some(JSValue.JSStr(s)) if s.startsWith("file:") =>
                try java.nio.file.Paths.get(java.net.URI.create(s)).toString
                catch case _: Throwable => s
              case Some(value) => NodeHelpers.toPath(value)
              case None        => basePath.toString
            }
            createRequire(from)
          }
        )
      )
    )
    moduleObj.set("Module", JSValue.Object(moduleClass))

    val builtinModules = JSArray.empty()
    builtins.keysIterator
      .filterNot(_.startsWith("node:"))
      .toSeq
      .sorted
      .foreach(name => builtinModules.push(JSValue.fromString(name)))
    moduleObj.defineProperty(
      "builtinModules",
      JSValue.JSArrayVal(builtinModules),
      enumerable = true
    )
    moduleObj
  }

  /** Build the module-scoped `require` function. */
  private def createRequire(fromFile: String)(using
      ctx: JSContext
  ): JSValue = {
    val requireFn = NativeFunction(
      name = "require",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        // `module.require('x')` is a method call, so the module object is
        // prepended; a specifier is always a string.
        val specifier =
          args.headOption match {
            case Some(JSValue.JSStr(s)) => s
            case Some(_) if args.length > 1 =>
              NodeHelpers.toStr(args(1))
            case Some(other) => NodeHelpers.toStr(other)
            case None        => ""
          }
        requireFrom(specifier, fromFile)
      }
    )
    requireFn.funcObj.set("extensions", JSValue.Object(extensionObject()))

    val resolveFn = NativeFunction(
      name = "resolve",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = NodeHelpers.stripReceiver(args, requireFn)
        val specifier =
          rest.headOption match {
            case Some(JSValue.JSStr(s)) => s
            case Some(other)            => NodeHelpers.toStr(other)
            case None                   => ""
          }
        resolveSpecifier(specifier, fromFile) match {
          case NodeResolution.Builtin(_) => JSValue.fromString(specifier)
          case NodeResolution.JsonFile(path) => JSValue.fromString(path)
          case NodeResolution.CjsFile(path)  => JSValue.fromString(path)
          case NodeResolution.EsmFile(path)  => JSValue.fromString(path)
        }
      }
    )
    requireFn.funcObj.defineProperty(
      "resolve",
      JSValue.Native(resolveFn),
      enumerable = true,
      writable = true,
      configurable = true
    )
    requireFn.funcObj.defineProperty(
      "cache",
      JSValue.Object(cacheObject),
      enumerable = true,
      writable = true,
      configurable = true
    )
    requireFn.funcObj.defineProperty(
      "main",
      JSValue.Undefined,
      enumerable = true,
      writable = true,
      configurable = true
    )
    JSValue.Native(requireFn)
  }

  private lazy val cacheObject: JSObject =
    JSObject(prototype = null)

  /** Run a file as the main CommonJS module or ESM entry, depending on its
    * extension and nearest package.json.
    */
  def runMainFile(path: Path)(using ctx: JSContext): JSValue = {
    val abs = path.toAbsolutePath.normalize.toString
    kindOf(path) match {
      case ModuleKind.Cjs => loadCJSModule(abs)
      case ModuleKind.Esm => loadModule(abs, abs)
    }
  }
}

/** Minimal JSON reader used for `package.json` and JSON modules. It only
  * implements the JSON grammar (no comments) and produces Scala values.
  */
private[node] object MiniJson {
  sealed trait Value
  final case class JsString(value: String) extends Value
  final case class JsNumber(value: Double) extends Value
  final case class JsBool(value: Boolean) extends Value
  case object JsNull extends Value
  final case class JsArray(values: List[Value]) extends Value
  final case class JsObject(fields: scala.collection.immutable.ListMap[String, Value])
      extends Value

  def parse(text: String): Value = {
    val parser = new Parser(text)
    val value = parser.parseValue()
    parser.skipWhitespace()
    if !parser.atEnd then parser.fail("Unexpected trailing content")
    value
  }

  def toJSValue(value: Value): JSValue = value match {
    case JsString(s) => JSValue.fromString(s)
    case JsNumber(n) =>
      if n == math.floor(n) && !n.isInfinite && math.abs(n) < Int.MaxValue then
        JSValue.fromInt(n.toInt)
      else JSValue.fromDouble(n)
    case JsBool(b) => JSValue.Bool(b)
    case JsNull    => JSValue.Null
    case JsArray(values) =>
      val arr = JSArray.empty()
      values.foreach(v => arr.push(toJSValue(v)))
      JSValue.JSArrayVal(arr)
    case JsObject(fields) =>
      val obj = JSObject()
      fields.foreach { case (k, v) =>
        obj.initProperty(
          k,
          toJSValue(v),
          enumerable = true,
          writable = true,
          configurable = true
        )
      }
      JSValue.Object(obj)
  }

  private final class Parser(text: String) {
    private var pos = 0

    def atEnd: Boolean = pos >= text.length

    def fail(message: String): Nothing =
      throw new RuntimeException(s"$message at offset $pos")

    def skipWhitespace(): Unit =
      while pos < text.length && {
          val c = text.charAt(pos)
          c == ' ' || c == '\t' || c == '\n' || c == '\r'
        }
      do pos += 1

    private def expect(c: Char): Unit = {
      if pos >= text.length || text.charAt(pos) != c then
        fail(s"Expected '$c'")
      pos += 1
    }

    def parseValue(): Value = {
      skipWhitespace()
      if atEnd then fail("Unexpected end of input")
      text.charAt(pos) match {
        case '{' => parseObject()
        case '[' => parseArray()
        case '"' => JsString(parseString())
        case 't' => parseKeyword("true", JsBool(true))
        case 'f' => parseKeyword("false", JsBool(false))
        case 'n' => parseKeyword("null", JsNull)
        case _   => parseNumber()
      }
    }

    private def parseKeyword(word: String, value: Value): Value = {
      if !text.startsWith(word, pos) then fail(s"Invalid literal")
      pos += word.length
      value
    }

    private def parseObject(): Value = {
      expect('{')
      skipWhitespace()
      val fields = mutable.LinkedHashMap.empty[String, Value]
      if !atEnd && text.charAt(pos) == '}' then {
        pos += 1
        return JsObject(scala.collection.immutable.ListMap(fields.toSeq*))
      }
      var continue = true
      while continue do {
        skipWhitespace()
        val key = parseString()
        skipWhitespace()
        expect(':')
        fields(key) = parseValue()
        skipWhitespace()
        if atEnd then fail("Unterminated object")
        text.charAt(pos) match {
          case ',' => pos += 1
          case '}' => pos += 1; continue = false
          case _   => fail("Expected ',' or '}'")
        }
      }
      JsObject(scala.collection.immutable.ListMap(fields.toSeq*))
    }

    private def parseArray(): Value = {
      expect('[')
      skipWhitespace()
      val values = mutable.ListBuffer.empty[Value]
      if !atEnd && text.charAt(pos) == ']' then {
        pos += 1
        return JsArray(values.toList)
      }
      var continue = true
      while continue do {
        values += parseValue()
        skipWhitespace()
        if atEnd then fail("Unterminated array")
        text.charAt(pos) match {
          case ',' => pos += 1
          case ']' => pos += 1; continue = false
          case _   => fail("Expected ',' or ']'")
        }
      }
      JsArray(values.toList)
    }

    private def parseString(): String = {
      expect('"')
      val sb = new StringBuilder
      while pos < text.length && text.charAt(pos) != '"' do {
        val c = text.charAt(pos)
        if c == '\\' then {
          pos += 1
          if atEnd then fail("Unterminated escape")
          text.charAt(pos) match {
            case '"'  => sb.append('"')
            case '\\' => sb.append('\\')
            case '/'  => sb.append('/')
            case 'b'  => sb.append('\b')
            case 'f'  => sb.append('\f')
            case 'n'  => sb.append('\n')
            case 'r'  => sb.append('\r')
            case 't'  => sb.append('\t')
            case 'u'  =>
              if pos + 4 >= text.length then fail("Invalid unicode escape")
              val hex = text.substring(pos + 1, pos + 5)
              sb.append(Integer.parseInt(hex, 16).toChar)
              pos += 4
            case other => fail(s"Invalid escape '\\$other'")
          }
        } else sb.append(c)
        pos += 1
      }
      expect('"')
      sb.toString
    }

    private def parseNumber(): Value = {
      val start = pos
      if !atEnd && (text.charAt(pos) == '-' || text.charAt(pos) == '+') then
        pos += 1
      while pos < text.length && {
          val c = text.charAt(pos)
          (c >= '0' && c <= '9') || c == '.' || c == 'e' || c == 'E' ||
          c == '+' || c == '-'
        }
      do pos += 1
      val raw = text.substring(start, pos)
      try JsNumber(raw.toDouble)
      catch case _: NumberFormatException => fail(s"Invalid number '$raw'")
    }
  }
}
