package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.module.{ModuleLoader, FileModuleLoader}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers.{callFunctionWithThis, callFunctionValue}
import scala.collection.mutable

/** Internal runtime helpers: for-in, module import, array spread. */
object InternalHelpers:
  import quickjs.objmodel.{JSObject, JSArray}

  def initializeForInHelpers(ctx: JSContext): Unit =
    val forInKeys = NativeFunction(
      name = "__forInKeys",
      impl = (args, ctx) =>
        val seen = mutable.LinkedHashSet.empty[String]
        val resultKeys = mutable.ArrayBuffer.empty[String]

        def addObjectKeys(obj: quickjs.objmodel.JSObject | Null): Unit =
          if obj != null then
            val keys = obj.getAllProperties.keys.toVector
            val (indexKeys, otherKeys) =
              keys.partition { key =>
                key.nonEmpty &&
                key.forall(_.isDigit) &&
                (key.length == 1 || key.charAt(0) != '0')
              }
            val orderedKeys =
              indexKeys.map(_.toInt).sorted.map(_.toString) ++ otherKeys
            orderedKeys.foreach { key =>
              if !seen.contains(key) then
                seen += key
                val enumerable = obj.getPropertyAttributes(key) match
                  case Some(attrs) => attrs.enumerable
                  case None => true
                if enumerable then resultKeys += key
            }
            addObjectKeys(obj.getPrototype)

        args.headOption match
          case Some(JSValue.Object(obj)) if obj.getOwnProperty("__proxy_handler")(using ctx).isDefined =>
            val handlerValue = obj.getOwnProperty("__proxy_handler")(using ctx).getOrElse(JSValue.Undefined)
            val targetValue = obj.getOwnProperty("__proxy_target")(using ctx).getOrElse(JSValue.Undefined)
            handlerValue match
              case JSValue.Object(handlerObj) =>
                val ownKeysFunc = handlerObj.get("ownKeys")(using ctx)
                val keysValue =
                  if ownKeysFunc != JSValue.Undefined then
                    callFunctionWithThis(ownKeysFunc, JSValue.Object(handlerObj), Array(targetValue))(using ctx)
                  else
                    targetValue match
                      case JSValue.Object(targetObj) =>
                        JSValue.JSArrayVal({
                          val arr = quickjs.objmodel.JSArray.empty()
                          targetObj.getOwnPropertyKeys().foreach(k => arr.push(JSValue.fromString(k)))
                          arr
                        })
                      case _ => JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())

                keysValue match
                  case JSValue.JSArrayVal(arr) =>
                    var i = 0
                    while i < arr.getLength do
                      val keyValue = arr.get(i)
                      val key = keyValue.toString
                      val descFunc = handlerObj.get("getOwnPropertyDescriptor")(using ctx)
                      val include =
                        if descFunc != JSValue.Undefined then
                          val descValue = callFunctionWithThis(
                            descFunc,
                            JSValue.Object(handlerObj),
                            Array(targetValue, JSValue.fromString(key))
                          )(using ctx)
                          descValue match
                            case JSValue.Undefined => false
                            case JSValue.Object(descObj) =>
                              descObj.get("enumerable")(using ctx) match
                                case JSValue.Bool(b) => b
                                case _ => true
                            case _ => true
                        else
                          true
                      if include && !seen.contains(key) then
                        seen += key
                        resultKeys += key
                      i += 1
                  case _ => ()
              case _ => ()
          case Some(JSValue.Object(obj)) =>
            addObjectKeys(obj)
          case Some(JSValue.JSArrayVal(arr)) =>
            var i = 0
            while i < arr.getLength do
              val key = i.toString
              if !seen.contains(key) then
                seen += key
                resultKeys += key
              i += 1
          case _ => ()

        val result = quickjs.objmodel.JSArray.empty()
        for key <- resultKeys do
          result.push(JSValue.fromString(key))
        JSValue.JSArrayVal(result)
    )

    val forInIsEnumerable = NativeFunction(
      name = "__forInIsEnumerable",
      impl = (args, ctx) =>
        if args.length < 2 then
          JSValue.Bool(false)
        else
          val key = args(1).toString
          args(0) match
            case JSValue.Object(obj) if obj.getOwnProperty("__proxy_handler")(using ctx).isDefined =>
              val handlerValue = obj.getOwnProperty("__proxy_handler")(using ctx).getOrElse(JSValue.Undefined)
              val targetValue = obj.getOwnProperty("__proxy_target")(using ctx).getOrElse(JSValue.Undefined)
              handlerValue match
                case JSValue.Object(handlerObj) =>
                  val descFunc = handlerObj.get("getOwnPropertyDescriptor")(using ctx)
                  if descFunc != JSValue.Undefined then
                    val descValue = callFunctionWithThis(
                      descFunc,
                      JSValue.Object(handlerObj),
                      Array(targetValue, JSValue.fromString(key))
                    )(using ctx)
                    descValue match
                      case JSValue.Undefined => JSValue.Bool(false)
                      case JSValue.Object(descObj) =>
                        descObj.get("enumerable")(using ctx) match
                          case JSValue.Bool(b) => JSValue.Bool(b)
                          case _ => JSValue.Bool(true)
                      case _ => JSValue.Bool(true)
                  else
                    JSValue.Bool(true)
                case _ =>
                  JSValue.Bool(true)
            case _ =>
              JSValue.Bool(true)
    )

    given JSContext = ctx
    ctx.globalScope.setVariable("__forInKeys", JSValue.Native(forInKeys))
    ctx.globalScope.setVariable("__forInIsEnumerable", JSValue.Native(forInIsEnumerable))

    // __createIterator(obj) - creates an iterator object for arrays/strings
    // Returns the object itself if it already has a next method (generator/iterator)
    // Otherwise wraps arrays/strings in an iterator object with __iterTarget and __iterIndex
    val createIterator = NativeFunction(
      name = "__createIterator",
      impl = (args, ctx) =>
        given JSContext = ctx
        val obj = args.headOption.getOrElse(JSValue.Undefined)

        obj match
          case JSValue.Object(obj) =>
            // Check if it already has a next method (generator/iterator)
            val nextMethod = obj.get("next")(using ctx)
            if nextMethod != JSValue.Undefined then
              // Already an iterator, return as-is
              JSValue.Object(obj)
            else
              // Create an iterator wrapper object
              val iterObj = quickjs.objmodel.JSObject()
              iterObj.defineProperty("__iterTarget", JSValue.Object(obj), enumerable = false, writable = false)
              iterObj.defineProperty("__iterIndex", JSValue.Int32(0), enumerable = false, writable = true)
              JSValue.Object(iterObj)

          case JSValue.JSArrayVal(arr) =>
            // Create an iterator wrapper for JSArrayVal
            val iterObj = quickjs.objmodel.JSObject()
            iterObj.defineProperty("__iterArray", JSValue.JSArrayVal(arr), enumerable = false, writable = false)
            iterObj.defineProperty("__iterIndex", JSValue.Int32(0), enumerable = false, writable = true)
            JSValue.Object(iterObj)

          case JSValue.JSStr(str) =>
            // Create an iterator wrapper for strings
            val iterObj = quickjs.objmodel.JSObject()
            iterObj.defineProperty("__iterString", JSValue.JSStr(str), enumerable = false, writable = false)
            iterObj.defineProperty("__iterIndex", JSValue.Int32(0), enumerable = false, writable = true)
            JSValue.Object(iterObj)

          case _ =>
            // Not iterable, return undefined
            JSValue.Undefined
    )
    ctx.globalScope.setVariable("__createIterator", JSValue.Native(createIterator))

    // __forOfNext(iterator) - iterator protocol helper for for-of loops
    // Returns {value: ..., done: boolean} by calling iterator.next()
    // Works with iterator objects created by __createIterator or generators
    val forOfNext = NativeFunction(
      name = "__forOfNext",
      impl = (args, ctx) =>
        given JSContext = ctx
        // When called via Call opcode: args(0) is the iterator (no 'this' prepended for global calls)
        // When called via callFunctionValue: args(1) is the first actual arg
        // Try args(0) first, fall back to args(1) for compatibility
        val iterator = args.headOption.getOrElse(JSValue.Undefined)

        iterator match
          case JSValue.Object(obj) =>
            // Check if it's an iterator wrapper (has __iterIndex)
            obj.getOwnProperty("__iterIndex") match
              case Some(JSValue.Int32(currentIndex)) =>
                // It's an iterator wrapper
                val resultObj = quickjs.objmodel.JSObject()

                // Check what type of target we're iterating
                obj.getOwnProperty("__iterArray") match
                  case Some(JSValue.JSArrayVal(arr)) =>
                    // Iterating a JSArrayVal
                    val length = arr.getLength
                    if currentIndex < length then
                      val value = arr.get(currentIndex)
                      obj.defineProperty("__iterIndex", JSValue.Int32(currentIndex + 1), enumerable = false, writable = true)
                      resultObj.defineProperty("value", value, enumerable = true)
                      resultObj.defineProperty("done", JSValue.Bool(false), enumerable = true)
                      JSValue.Object(resultObj)
                    else
                      resultObj.defineProperty("value", JSValue.Undefined, enumerable = true)
                      resultObj.defineProperty("done", JSValue.Bool(true), enumerable = true)
                      JSValue.Object(resultObj)

                  case _ =>
                    // Check for string iteration
                    obj.getOwnProperty("__iterString") match
                      case Some(JSValue.JSStr(str)) =>
                        val length = str.length
                        if currentIndex < length then
                          val charStr = str.substring(currentIndex, currentIndex + 1)
                          obj.defineProperty("__iterIndex", JSValue.Int32(currentIndex + 1), enumerable = false, writable = true)
                          resultObj.defineProperty("value", JSValue.JSStr(charStr), enumerable = true)
                          resultObj.defineProperty("done", JSValue.Bool(false), enumerable = true)
                          JSValue.Object(resultObj)
                        else
                          resultObj.defineProperty("value", JSValue.Undefined, enumerable = true)
                          resultObj.defineProperty("done", JSValue.Bool(true), enumerable = true)
                          JSValue.Object(resultObj)

                      case _ =>
                        // Check for object iteration (array-like with length)
                        obj.getOwnProperty("__iterTarget") match
                          case Some(JSValue.Object(targetObj)) =>
                            val length = targetObj.get("length")(using ctx) match
                              case JSValue.Int32(len) => len
                              case JSValue.Float64(len) => len.toInt
                              case _ => 0

                            if currentIndex < length then
                              val value = targetObj.get(currentIndex.toString)(using ctx)
                              obj.defineProperty("__iterIndex", JSValue.Int32(currentIndex + 1), enumerable = false, writable = true)
                              resultObj.defineProperty("value", value, enumerable = true)
                              resultObj.defineProperty("done", JSValue.Bool(false), enumerable = true)
                              JSValue.Object(resultObj)
                            else
                              resultObj.defineProperty("value", JSValue.Undefined, enumerable = true)
                              resultObj.defineProperty("done", JSValue.Bool(true), enumerable = true)
                              JSValue.Object(resultObj)

                          case _ =>
                            // Unknown iterator type
                            resultObj.defineProperty("value", JSValue.Undefined, enumerable = true)
                            resultObj.defineProperty("done", JSValue.Bool(true), enumerable = true)
                            JSValue.Object(resultObj)

              case _ =>
                // No __iterIndex, so it's a generator/iterator with a next method
                val nextMethod = obj.get("next")(using ctx)
                nextMethod match
                  case JSValue.Native(_) =>
                    // It's a native iterator, call next()
                    val result = callFunctionValue(nextMethod, iterator, Array.empty)
                    result match
                      case JSValue.Object(resultObj) =>
                        result
                      case _ =>
                        val resultObj = quickjs.objmodel.JSObject()
                        resultObj.defineProperty("value", JSValue.Undefined, enumerable = true)
                        resultObj.defineProperty("done", JSValue.Bool(true), enumerable = true)
                        JSValue.Object(resultObj)
                  case _: JSValue.Function =>
                    // It's a bytecode function
                    val result = callFunctionValue(nextMethod, iterator, Array.empty)
                    result match
                      case JSValue.Object(resultObj) =>
                        result
                      case _ =>
                        val resultObj = quickjs.objmodel.JSObject()
                        resultObj.defineProperty("value", JSValue.Undefined, enumerable = true)
                        resultObj.defineProperty("done", JSValue.Bool(true), enumerable = true)
                        JSValue.Object(resultObj)
                  case _ =>
                    // No next method, return done
                    val resultObj = quickjs.objmodel.JSObject()
                    resultObj.defineProperty("value", JSValue.Undefined, enumerable = true)
                    resultObj.defineProperty("done", JSValue.Bool(true), enumerable = true)
                    JSValue.Object(resultObj)

          case _ =>
            // Not an iterator or array, return done
            val resultObj = quickjs.objmodel.JSObject()
            resultObj.defineProperty("value", JSValue.Undefined, enumerable = true)
            resultObj.defineProperty("done", JSValue.Bool(true), enumerable = true)
            JSValue.Object(resultObj)
    )
    ctx.globalScope.setVariable("__forOfNext", JSValue.Native(forOfNext))

    // __initPrivateGetter__(obj, name, getterFn) - initialize a private getter
    val initPrivateGetter = NativeFunction(
      name = "__initPrivateGetter__",
      impl = (args, ctx) =>
        given JSContext = ctx
        // args(0) = this, args(1) = name, args(2) = getterFn
        val obj = args(1)
        val name = args(2).toString
        val getterFn = args(3)

        obj match
          case JSValue.Object(o) =>
            // Get or create __privateGetters__ map
            val gettersMap = o.getOwnProperty("__privateGetters__") match
              case Some(JSValue.Object(gm)) => gm
              case _ =>
                val gm = quickjs.objmodel.JSObject(prototype = null, extensible = true)
                o.defineProperty("__privateGetters__", JSValue.Object(gm), enumerable = false, writable = false, configurable = false)
                gm
            gettersMap.set(name, getterFn)
          case f: JSValue.Function =>
            // Handle Function's funcObj
            val gettersMap = f.funcObj.getOwnProperty("__privateGetters__") match
              case Some(JSValue.Object(gm)) => gm
              case _ =>
                val gm = quickjs.objmodel.JSObject(prototype = null, extensible = true)
                f.funcObj.defineProperty("__privateGetters__", JSValue.Object(gm), enumerable = false, writable = false, configurable = false)
                gm
            gettersMap.set(name, getterFn)
          case _ =>
            ctx.throwTypeError("Cannot define private getter on non-object")
        JSValue.Undefined
    )
    ctx.globalScope.setVariable("__initPrivateGetter__", JSValue.Native(initPrivateGetter))

    // __initPrivateSetter__(obj, name, setterFn) - initialize a private setter
    val initPrivateSetter = NativeFunction(
      name = "__initPrivateSetter__",
      impl = (args, ctx) =>
        given JSContext = ctx
        // args(0) = this, args(1) = name, args(2) = setterFn
        val obj = args(1)
        val name = args(2).toString
        val setterFn = args(3)

        obj match
          case JSValue.Object(o) =>
            // Get or create __privateSetters__ map
            val settersMap = o.getOwnProperty("__privateSetters__") match
              case Some(JSValue.Object(sm)) => sm
              case _ =>
                val sm = quickjs.objmodel.JSObject(prototype = null, extensible = true)
                o.defineProperty("__privateSetters__", JSValue.Object(sm), enumerable = false, writable = false, configurable = false)
                sm
            settersMap.set(name, setterFn)
          case f: JSValue.Function =>
            // Handle Function's funcObj
            val settersMap = f.funcObj.getOwnProperty("__privateSetters__") match
              case Some(JSValue.Object(sm)) => sm
              case _ =>
                val sm = quickjs.objmodel.JSObject(prototype = null, extensible = true)
                f.funcObj.defineProperty("__privateSetters__", JSValue.Object(sm), enumerable = false, writable = false, configurable = false)
                sm
            settersMap.set(name, setterFn)
          case _ =>
            ctx.throwTypeError("Cannot define private setter on non-object")
        JSValue.Undefined
    )
    ctx.globalScope.setVariable("__initPrivateSetter__", JSValue.Native(initPrivateSetter))


  def initializeModuleHelpers(ctx: JSContext, loader: Option[ModuleLoader]): Unit =
    def loadModuleWithLoader(loader: ModuleLoader, specifier: String, context: JSContext): JSValue =
      given JSContext = context
      loader match
        case fileLoader: FileModuleLoader =>
          fileLoader.loadModule(specifier, context.currentModulePath)
        case _ =>
          val fromPath = context.currentModulePath
          val resolvedName = loader.resolve(specifier, fromPath)

          context.rt.getModuleExports(resolvedName) match
            case Some(exports) =>
              JSValue.Object(exports)
            case None =>
              val loadResult =
                try loader.load(resolvedName)
                catch
                  case e: Exception =>
                    context.throwError("Error", s"Cannot find module '$specifier': ${e.getMessage}")

              val lexer = quickjs.lexer.Lexer(loadResult.source)
              val tokens = lexer.tokenize()
              val parser = quickjs.parser.Parser(tokens)
              val ast = parser.parseScript()
              val compiler = quickjs.compiler.Compiler()
              val bytecode = compiler.compileModule(ast, resolvedName)
              val interpreter = Interpreter()

              val previousPath = context.currentModulePath
              context.currentModulePath = resolvedName
              try
                interpreter.call(bytecode, JSValue.Undefined, Array.empty)
              finally
                context.currentModulePath = previousPath

              JSValue.Object(context.rt.ensureModuleExports(resolvedName))

    // Capture the loader in a local val so closures use the correct instance
    val capturedLoader = loader

    val moduleImport = NativeFunction(
      name = "__moduleImport",
      impl = (args, context) =>
        given JSContext = context
        val specifier = args.headOption match
          case Some(JSValue.JSStr(s)) => s
          case Some(other) => other.toString
          case None => ""

        capturedLoader.orElse(context.rt.getModuleLoaderOption) match
          case Some(loader) =>
            loadModuleWithLoader(loader, specifier, context)
          case None =>
            context.rt.getModuleExports(specifier) match
              case Some(exportsObj) => JSValue.Object(exportsObj)
              case None =>
                context.throwError("Error", s"Cannot import module '$specifier': no module loader configured")
    )

    val moduleExport = NativeFunction(
      name = "__moduleExport",
      impl = (args, context) =>
        given JSContext = context
        val moduleName = args.headOption match
          case Some(JSValue.JSStr(s)) => s
          case Some(other) => other.toString
          case None => ""
        val exportName = args.drop(1).headOption match
          case Some(JSValue.JSStr(s)) => s
          case Some(other) => other.toString
          case None => ""
        val value =
          if args.length > 2 then args(2)
          else JSValue.Undefined
        val exportsObj = context.rt.ensureModuleExports(moduleName)
        exportsObj.set(exportName, value)
        value
    )

    val moduleExportAll = NativeFunction(
      name = "__moduleExportAll",
      impl = (args, context) =>
        given JSContext = context
        val moduleName = args.headOption match
          case Some(JSValue.JSStr(s)) => s
          case Some(other) => other.toString
          case None => ""
        val sourceSpecifier = args.drop(1).headOption match
          case Some(JSValue.JSStr(s)) => s
          case Some(other) => other.toString
          case None => ""

        // First, load the source module if using file-based loading
        val sourceObj = capturedLoader.orElse(context.rt.getModuleLoaderOption) match
          case Some(loader) =>
            loadModuleWithLoader(loader, sourceSpecifier, context) match
              case JSValue.Object(obj) => obj
              case _ => context.rt.ensureModuleExports(context.rt.resolveModule(sourceSpecifier, context.currentModulePath))
          case None =>
            context.rt.getModuleExports(sourceSpecifier) match
              case Some(obj) => obj
              case None =>
                context.throwError("Error", s"Cannot export from module '$sourceSpecifier': no module loader configured")

        val exportsObj = context.rt.ensureModuleExports(moduleName)
        val keys = sourceObj.getOwnPropertyKeys()
        for key <- keys if key != "default" do
          sourceObj.getOwnProperty(key) match
            case Some(value) => exportsObj.set(key, value)
            case None => ()
        JSValue.Undefined
    )

    ctx.globalScope.setVariable("__moduleImport", JSValue.Native(moduleImport))
    ctx.globalScope.setVariable("__moduleExport", JSValue.Native(moduleExport))
    ctx.globalScope.setVariable("__moduleExportAll", JSValue.Native(moduleExportAll))


  def initializeArrayHelpers(ctx: JSContext): Unit =
    val arrayPush = NativeFunction(
      name = "__arrayPush",
      impl = (args, ctx) =>
        if args.length < 2 then
          JSValue.Undefined
        else
          args(0) match
            case arrVal: JSValue.JSArrayVal =>
              arrVal.value.push(args(1))
              arrVal
            case _ => JSValue.Undefined
    )

    val arraySpread = NativeFunction(
      name = "__arraySpread",
      impl = (args, ctx) =>
        if args.length < 2 then
          JSValue.Undefined
        else
          (args(0), args(1)) match
            case (arrVal: JSValue.JSArrayVal, srcVal: JSValue.JSArrayVal) =>
              val src = srcVal.value
              var i = 0
              while i < src.getLength do
                arrVal.value.push(src.get(i))
                i += 1
              arrVal
            case (arrVal: JSValue.JSArrayVal, _) =>
              arrVal
            case _ =>
              JSValue.Undefined
    )

    // Helper for object spread: __objectSpread(target, source)
    // Copies all enumerable own properties from source to target
    val objectSpread = NativeFunction(
      name = "__objectSpread",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 2 then
          JSValue.Undefined
        else
          val target = args(0)
          val source = args(1)

          (target, source) match
            case (JSValue.Object(targetObj), JSValue.Object(srcObj)) =>
              for key <- srcObj.getOwnPropertyKeys() do
                val value = srcObj.get(key)
                targetObj.set(key, value)
              target
            case (JSValue.Object(targetObj), JSValue.Null | JSValue.Undefined) =>
              // Spreading null/undefined is a no-op
              target
            case (JSValue.Object(_), _) =>
              // For non-object sources, no properties are copied
              target
            case _ =>
              JSValue.Undefined
    )

    // Helper for spreading arguments in super() calls: __funcSpread(superFunc, thisObj, argsArray)
    // Calls superFunc with thisObj and spreads argsArray as individual arguments
    val funcSpread = NativeFunction(
      name = "__funcSpread",
      impl = (args, ctx) =>
        import quickjs.bytecode.BytecodeFunction
        given JSContext = ctx
        if args.length < 3 then
          JSValue.Undefined
        else
          val func = args(0)
          val thisObj = args(1)
          val argsArray = args(2)

          // Extract arguments from array
          val callArgs = argsArray match
            case JSValue.JSArrayVal(arr) =>
              (0 until arr.getLength).map(i => arr.get(i)).toArray
            case _ =>
              Array.empty[JSValue]

          // Call the function with extracted arguments
          func match
            case f: JSValue.Function =>
              try
                val bcFunc = BuiltinHelpers.functionToBytecode(f)
                val result = quickjs.interpreter.Interpreter().call(bcFunc, thisObj, callArgs, f.closure)
                if f.isConstructor then thisObj else result
              catch case e: Exception => JSValue.Undefined
            case _ => JSValue.Undefined
    )

    // Helper for object rest destructuring: __objectRest(source, excludeKeys)
    // Returns a new object with all properties except those in excludeKeys
    val objectRest = NativeFunction(
      name = "__objectRest",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 2 then
          ctx.throwTypeError("__objectRest requires 2 arguments")
        val source = args(0)
        val excludeKeys = args(1)

        // Get the exclude keys as a set
        val excludeSet = excludeKeys match
          case arrVal: JSValue.JSArrayVal =>
            val arr = arrVal.value
            val keys = scala.collection.mutable.Set[String]()
            var i = 0
            while i < arr.getLength do
              arr.get(i) match
                case JSValue.JSStr(s) => keys += s
                case other =>
                  // Skip non-string keys
                  ()
              i += 1
            keys.toSet
          case _ =>
            ctx.throwTypeError("__objectRest: second argument must be an array")

        // Create a new object with remaining properties
        source match
          case JSValue.Object(srcObj) =>
            val result = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
            for key <- srcObj.getOwnPropertyKeys() do
              if !excludeSet.contains(key) then
                val value = srcObj.get(key)
                result.set(key, value)
            JSValue.Object(result)
          case other =>
            ctx.throwTypeError(s"__objectRest: first argument must be an object, got $other")
    )

    given JSContext = ctx
    ctx.globalScope.setVariable("__arrayPush", JSValue.Native(arrayPush))
    ctx.globalScope.setVariable("__arraySpread", JSValue.Native(arraySpread))
    ctx.globalScope.setVariable("__objectSpread", JSValue.Native(objectSpread))
    ctx.globalScope.setVariable("__funcSpread", JSValue.Native(funcSpread))
    ctx.globalScope.setVariable("__objectRest", JSValue.Native(objectRest))

  /** For-of iteration index tracking */
  private val forOfIndices = mutable.Map[Int, Int]()  // identityHashCode -> currentIndex

  /** Test helpers: eval, __loadScript, __runMicrotasks, queueMicrotask */
  def initializeTestHelpers(ctx: JSContext): Unit =
    val loadScript = NativeFunction(
      name = "__loadScript",
      impl = (_, _) => JSValue.Undefined
    )
    given JSContext = ctx
    ctx.global.set("__loadScript", JSValue.Native(loadScript))

    val evalFunc = NativeFunction(
      name = "eval",
      impl = (args, _) =>
        if args.nonEmpty then args(0) else JSValue.Undefined
    )
    ctx.global.set("eval", JSValue.Native(evalFunc))

    // __runMicrotasks - runs all pending microtasks
    val runMicrotasksFunc = NativeFunction(
      name = "__runMicrotasks",
      impl = (_, ctx) =>
        given JSContext = ctx
        ctx.runMicrotasks()
        JSValue.Undefined
    )
    ctx.global.set("__runMicrotasks", JSValue.Native(runMicrotasksFunc))

    // queueMicrotask - queues a microtask
    val queueMicrotaskFunc = NativeFunction(
      name = "queueMicrotask",
      impl = (args, ctx) =>
        given JSContext = ctx
        val callback = args.lift(1).getOrElse(JSValue.Undefined)
        ctx.queueMicrotask { () =>
          BuiltinHelpers.callFunctionValue(callback, JSValue.Undefined, Array.empty)
        }
        JSValue.Undefined
    )
    ctx.global.set("queueMicrotask", JSValue.Native(queueMicrotaskFunc))

    // print() - used by test262 async tests (doneprintHandle.js)
    val printFunc = NativeFunction(
      name = "print",
      impl = (args, ctx) =>
        val msg = args.lift(1).map(_.toString).getOrElse("")
        System.out.println(msg)
        JSValue.Undefined
    )
    ctx.global.set("print", JSValue.Native(printFunc))

