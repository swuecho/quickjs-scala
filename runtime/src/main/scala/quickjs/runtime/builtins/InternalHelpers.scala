package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.module.{ModuleLoader, FileModuleLoader}
import quickjs.runtime.{JSContext, JSException}
import quickjs.runtime.builtins.BuiltinHelpers.{
  callFunctionWithThis,
  callFunctionValue,
  toPropertyKey,
  toJSString,
  wrapPromise
}
import scala.collection.mutable

/** Internal runtime helpers: for-in, module import, array spread. */
object InternalHelpers {
  import quickjs.objmodel.{JSObject, JSArray}

  def initializeForInHelpers(ctx: JSContext): Unit = {
    var nextPrivateNameId = 0L
    val newPrivateName = NativeFunction(
      name = "__newPrivateName__",
      impl = (args, _) => {
        val description = args.headOption.getOrElse(JSValue.Undefined).toString
        nextPrivateNameId += 1
        JSValue.fromString(
          s"$description\u0000${System.identityHashCode(ctx)}:$nextPrivateNameId"
        )
      }
    )
    ctx.globalScope.setVariable(
      "__newPrivateName__",
      JSValue.Native(newPrivateName)
    )

    val toPropertyKeyHelper = NativeFunction(
      name = "__toPropertyKey",
      impl = (args, helperCtx) =>
        given JSContext = helperCtx
        toPropertyKey(args.headOption.getOrElse(JSValue.Undefined))
    )
    ctx.globalScope.setVariable(
      "__toPropertyKey",
      JSValue.Native(toPropertyKeyHelper)
    )

    val templateToStringHelper = NativeFunction(
      name = "__templateToString",
      impl = (args, helperCtx) =>
        given JSContext = helperCtx
        JSValue.fromString(toJSString(args.headOption.getOrElse(JSValue.Undefined)))
    )
    ctx.globalScope.setVariable(
      "__templateToString",
      JSValue.Native(templateToStringHelper)
    )

    val forInKeys = NativeFunction(
      name = "__forInKeys",
      impl = (args, ctx) =>
        val seen = mutable.LinkedHashSet.empty[String]
        val resultKeys = mutable.ArrayBuffer.empty[String]

        def addObjectKeys(obj: quickjs.objmodel.JSObject | Null): Unit =
          if obj != null then {
            val keys = obj.getAllOwnStringPropertyKeys().toVector
            val (indexKeys, otherKeys) =
              keys.partition { key =>
                key.nonEmpty &&
                key.forall(_.isDigit) &&
                (key.length == 1 || key.charAt(0) != '0')
              }
            val orderedKeys =
              indexKeys.map(_.toInt).sorted.map(_.toString) ++ otherKeys
            orderedKeys.foreach { key =>
              if !seen.contains(key) then {
                seen += key
                val enumerable = obj.getPropertyAttributes(key) match {
                  case Some(attrs) => attrs.enumerable
                  case None        => true
                }
                if enumerable then resultKeys += key
              }
            }
            addObjectKeys(obj.getPrototype)
          }

        args.headOption match {
          case Some(JSValue.Object(obj))
              if obj.getOwnProperty("__proxy_handler")(using ctx).isDefined =>
            val handlerValue = obj
              .getOwnProperty("__proxy_handler")(using ctx)
              .getOrElse(JSValue.Undefined)
            val targetValue = obj
              .getOwnProperty("__proxy_target")(using ctx)
              .getOrElse(JSValue.Undefined)
            handlerValue match {
              case JSValue.Object(handlerObj) =>
                val ownKeysFunc = handlerObj.get("ownKeys")(using ctx)
                val keysValue =
                  if ownKeysFunc != JSValue.Undefined then
                    callFunctionWithThis(
                      ownKeysFunc,
                      JSValue.Object(handlerObj),
                      Array(targetValue)
                    )(using ctx)
                  else
                    targetValue match {
                      case JSValue.Object(targetObj) =>
                        JSValue.JSArrayVal {
                          val arr = quickjs.objmodel.JSArray.empty()
                          targetObj
                            .getOwnPropertyKeys()
                            .foreach(k => arr.push(JSValue.fromString(k)))
                          arr
                        }
                      case _ =>
                        JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
                    }

                keysValue match {
                  case JSValue.JSArrayVal(arr) =>
                    var i = 0
                    while i < arr.getLength do {
                      val keyValue = arr.get(i)
                      val key = keyValue.toString
                      val descFunc =
                        handlerObj.get("getOwnPropertyDescriptor")(using ctx)
                      val include =
                        if descFunc != JSValue.Undefined then {
                          val descValue = callFunctionWithThis(
                            descFunc,
                            JSValue.Object(handlerObj),
                            Array(targetValue, JSValue.fromString(key))
                          )(using ctx)
                          descValue match {
                            case JSValue.Undefined       => false
                            case JSValue.Object(descObj) =>
                              descObj.get("enumerable")(using ctx) match {
                                case JSValue.Bool(b) => b
                                case _               => true
                              }
                            case _ => true
                          }
                        }
                        else true
                      if include && !seen.contains(key) then {
                        seen += key
                        resultKeys += key
                      }
                      i += 1
                    }
                  case _ => ()
                }
              case _ => ()
            }
          case Some(JSValue.Object(obj)) =>
            addObjectKeys(obj)
          case Some(function: JSValue.Function) =>
            addObjectKeys(function.funcObj)
          case Some(JSValue.Native(function: NativeFunction)) =>
            addObjectKeys(function.funcObj)
          case Some(
                JSValue.Native(function: quickjs.value.NativeConstructor)
              ) =>
            addObjectKeys(function.funcObj)
          case Some(JSValue.JSArrayVal(arr)) =>
            arr.getOwnIndexKeys.foreach { i =>
              val key = i.toString
              if !seen.contains(key) then {
                seen += key
                if arr.getOwnIndexDescriptor(i).exists(_._2.enumerable) then
                  resultKeys += key
              }
            }
            arr.getEnumerableOwnPropertyKeys.foreach { key =>
              if !seen.contains(key) then {
                seen += key
                resultKeys += key
              }
            }
          case _ => ()
        }

        val result = quickjs.objmodel.JSArray.empty()
        for key <- resultKeys do result.push(JSValue.fromString(key))
        JSValue.JSArrayVal(result)
    )

    val forInIsEnumerable = NativeFunction(
      name = "__forInIsEnumerable",
      impl = (args, ctx) =>
        if args.length < 2 then JSValue.Bool(false)
        else {
          val key = args(1).toString
          args(0) match {
            case JSValue.Object(obj)
                if obj.getOwnProperty("__proxy_handler")(using ctx).isDefined =>
              val handlerValue = obj
                .getOwnProperty("__proxy_handler")(using ctx)
                .getOrElse(JSValue.Undefined)
              val targetValue = obj
                .getOwnProperty("__proxy_target")(using ctx)
                .getOrElse(JSValue.Undefined)
              handlerValue match {
                case JSValue.Object(handlerObj) =>
                  val descFunc =
                    handlerObj.get("getOwnPropertyDescriptor")(using ctx)
                  if descFunc != JSValue.Undefined then {
                    val descValue = callFunctionWithThis(
                      descFunc,
                      JSValue.Object(handlerObj),
                      Array(targetValue, JSValue.fromString(key))
                    )(using ctx)
                    descValue match {
                      case JSValue.Undefined       => JSValue.Bool(false)
                      case JSValue.Object(descObj) =>
                        descObj.get("enumerable")(using ctx) match {
                          case JSValue.Bool(b) => JSValue.Bool(b)
                          case _               => JSValue.Bool(true)
                        }
                      case _ => JSValue.Bool(true)
                    }
                  }
                  else JSValue.Bool(true)
                case _ =>
                  JSValue.Bool(true)
              }
            case _ =>
              JSValue.Bool(true)
          }
        }
    )

    given JSContext = ctx
    ctx.globalScope.setVariable("__forInKeys", JSValue.Native(forInKeys))
    ctx.globalScope.setVariable(
      "__forInIsEnumerable",
      JSValue.Native(forInIsEnumerable)
    )

    // __createIterator(obj) - creates an iterator object for arrays/strings
    // Returns the object itself if it already has a next method (generator/iterator)
    // Otherwise wraps arrays/strings in an iterator object with __iterTarget and __iterIndex
    val createIterator = NativeFunction(
      name = "__createIterator",
      impl = (args, ctx) =>
        given JSContext = ctx
        val obj = args.headOption.getOrElse(JSValue.Undefined)

        obj match {
          case JSValue.Object(obj) =>
            val iteratorSymbol = ctx.global.get("Symbol") match {
              case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                nc.funcObj.get("iterator")(using ctx)
              case _ => JSValue.Undefined
            }
            val iteratorMethod = iteratorSymbol match {
              case JSValue.Symbol(id) =>
                obj.getSymbolPropertyDescriptorWithOwner(id) match {
                  case Some((_, stored, attrs)) if attrs.getter.isDefined =>
                    callFunctionWithThis(attrs.getter.get, JSValue.Object(obj), Array.empty)
                  case Some((_, stored, _)) => stored
                  case None                 => JSValue.Undefined
                }
              case _ => JSValue.Undefined
            }
            if iteratorMethod == JSValue.Undefined || iteratorMethod == JSValue.Null then
              ctx.throwTypeError("value is not iterable")
            if !BuiltinHelpers.isCallable(iteratorMethod) then
              ctx.throwTypeError("iterator method is not callable")
            val iterator = callFunctionWithThis(
              iteratorMethod,
              JSValue.Object(obj),
              Array.empty
            )
            if !iterator.isObject then
              ctx.throwTypeError("iterator is not an object")
            iterator

          case JSValue.JSArrayVal(arr) =>
            // Create an iterator wrapper for JSArrayVal
            val iterObj = quickjs.objmodel.JSObject()
            iterObj.defineProperty(
              "__iterArray",
              JSValue.JSArrayVal(arr),
              enumerable = false,
              writable = false
            )
            iterObj.defineProperty(
              "__iterIndex",
              JSValue.Int32(0),
              enumerable = false,
              writable = true
            )
            JSValue.Object(iterObj)

          case JSValue.JSStr(str) =>
            // Create an iterator wrapper for strings
            val iterObj = quickjs.objmodel.JSObject()
            iterObj.defineProperty(
              "__iterString",
              JSValue.JSStr(str),
              enumerable = false,
              writable = false
            )
            iterObj.defineProperty(
              "__iterIndex",
              JSValue.Int32(0),
              enumerable = false,
              writable = true
            )
            JSValue.Object(iterObj)

          case _ =>
            // Not iterable, return undefined
            JSValue.Undefined
        }
    )
    ctx.globalScope.setVariable(
      "__createIterator",
      JSValue.Native(createIterator)
    )

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

        iterator match {
          case JSValue.Object(obj) =>
            // Check if it's an iterator wrapper (has __iterIndex)
            obj.getOwnProperty("__iterIndex") match {
              case Some(JSValue.Int32(currentIndex)) =>
                // It's an iterator wrapper
                val resultObj = quickjs.objmodel.JSObject()

                // Check what type of target we're iterating
                obj.getOwnProperty("__iterArray") match {
                  case Some(JSValue.JSArrayVal(arr)) =>
                    // Iterating a JSArrayVal
                    val length = arr.getLength
                    if currentIndex < length then {
                      val value = arr.getIndexAttributes(currentIndex).flatMap(_.getter) match {
                        case Some(getter) => callFunctionWithThis(
                          getter,
                          JSValue.JSArrayVal(arr),
                          Array.empty
                        )
                        case None => arr.get(currentIndex)
                      }
                      obj.defineProperty(
                        "__iterIndex",
                        JSValue.Int32(currentIndex + 1),
                        enumerable = false,
                        writable = true
                      )
                      resultObj.defineProperty(
                        "value",
                        value,
                        enumerable = true
                      )
                      resultObj.defineProperty(
                        "done",
                        JSValue.Bool(false),
                        enumerable = true
                      )
                      JSValue.Object(resultObj)
                    }
                    else {
                      resultObj.defineProperty(
                        "value",
                        JSValue.Undefined,
                        enumerable = true
                      )
                      resultObj.defineProperty(
                        "done",
                        JSValue.Bool(true),
                        enumerable = true
                      )
                      JSValue.Object(resultObj)
                    }

                  case _ =>
                    // Check for string iteration
                    obj.getOwnProperty("__iterString") match {
                      case Some(JSValue.JSStr(str)) =>
                        val length = str.length
                        if currentIndex < length then {
                          val charStr =
                            str.substring(currentIndex, currentIndex + 1)
                          obj.defineProperty(
                            "__iterIndex",
                            JSValue.Int32(currentIndex + 1),
                            enumerable = false,
                            writable = true
                          )
                          resultObj.defineProperty(
                            "value",
                            JSValue.JSStr(charStr),
                            enumerable = true
                          )
                          resultObj.defineProperty(
                            "done",
                            JSValue.Bool(false),
                            enumerable = true
                          )
                          JSValue.Object(resultObj)
                        }
                        else {
                          resultObj.defineProperty(
                            "value",
                            JSValue.Undefined,
                            enumerable = true
                          )
                          resultObj.defineProperty(
                            "done",
                            JSValue.Bool(true),
                            enumerable = true
                          )
                          JSValue.Object(resultObj)
                        }

                      case _ =>
                        // Check for object iteration (array-like with length)
                        obj.getOwnProperty("__iterTarget") match {
                          case Some(JSValue.Object(targetObj)) =>
                            val length =
                              targetObj.get("length")(using ctx) match {
                                case JSValue.Int32(len)   => len
                                case JSValue.Float64(len) => len.toInt
                                case _                    => 0
                              }

                            if currentIndex < length then {
                              val value =
                                targetObj.get(currentIndex.toString)(using ctx)
                              obj.defineProperty(
                                "__iterIndex",
                                JSValue.Int32(currentIndex + 1),
                                enumerable = false,
                                writable = true
                              )
                              resultObj.defineProperty(
                                "value",
                                value,
                                enumerable = true
                              )
                              resultObj.defineProperty(
                                "done",
                                JSValue.Bool(false),
                                enumerable = true
                              )
                              JSValue.Object(resultObj)
                            }
                            else {
                              resultObj.defineProperty(
                                "value",
                                JSValue.Undefined,
                                enumerable = true
                              )
                              resultObj.defineProperty(
                                "done",
                                JSValue.Bool(true),
                                enumerable = true
                              )
                              JSValue.Object(resultObj)
                            }

                          case _ =>
                            // Unknown iterator type
                            resultObj.defineProperty(
                              "value",
                              JSValue.Undefined,
                              enumerable = true
                            )
                            resultObj.defineProperty(
                              "done",
                              JSValue.Bool(true),
                              enumerable = true
                            )
                            JSValue.Object(resultObj)
                        }
                    }
                }

              case _ =>
                // No __iterIndex, so it's a generator/iterator with a next method
                val nextMethod = obj.get("next")(using ctx)
                nextMethod match {
                  case JSValue.Native(_) =>
                    // It's a native iterator, call next()
                    val result =
                      callFunctionValue(nextMethod, iterator, Array.empty)
                    result match {
                      case JSValue.Object(resultObj) =>
                        result
                      case _ =>
                        ctx.throwTypeError("iterator result is not an object")
                    }
                  case _: JSValue.Function =>
                    // It's a bytecode function
                    val result =
                      callFunctionValue(nextMethod, iterator, Array.empty)
                    result match {
                      case JSValue.Object(resultObj) =>
                        result
                      case _ =>
                        ctx.throwTypeError("iterator result is not an object")
                    }
                  case _ =>
                    ctx.throwTypeError("iterator next is not callable")
                }
            }

          case _ =>
            // Not an iterator or array, return done
            val resultObj = quickjs.objmodel.JSObject()
            resultObj.defineProperty(
              "value",
              JSValue.Undefined,
              enumerable = true
            )
            resultObj.defineProperty(
              "done",
              JSValue.Bool(true),
              enumerable = true
            )
            JSValue.Object(resultObj)
        }
    )
    ctx.globalScope.setVariable("__forOfNext", JSValue.Native(forOfNext))

    // __createAsyncIterator(value) - iterator protocol helper for for-await-of.
    // Prefers Symbol.asyncIterator, falls back to the sync iterator protocol
    // (the compiler awaits every step), and finally accepts an object that
    // already exposes a `next` method.
    val createAsyncIterator = NativeFunction(
      name = "__createAsyncIterator",
      impl = (args, ctx) =>
        given JSContext = ctx
        val source = args.headOption.getOrElse(JSValue.Undefined)

        def methodNamed(name: String): JSValue =
          val id = BuiltinHelpers.wellKnownSymbolId(name)
          BuiltinHelpers.getSymbolPropertyWithGetter(source, id)

        source match {
          case _: JSValue.JSArrayVal | _: JSValue.JSStr =>
            createIterator.call(Array(source))
          case _ =>
            val asyncMethod = methodNamed("asyncIterator")
            if BuiltinHelpers.isCallable(asyncMethod) then {
              val iterator =
                callFunctionWithThis(asyncMethod, source, Array.empty)
              if !iterator.isObject then
                ctx.throwTypeError(
                  "Result of Symbol.asyncIterator is not an object"
                )
              iterator
            } else {
              val syncMethod = methodNamed("iterator")
              if BuiltinHelpers.isCallable(syncMethod) then {
                val iterator =
                  callFunctionWithThis(syncMethod, source, Array.empty)
                if !iterator.isObject then
                  ctx.throwTypeError("Result of Symbol.iterator is not an object")
                iterator
              } else if BuiltinHelpers.isCallable(
                  BuiltinHelpers.getPropertyWithGetter(source, "next")
                )
              then source
              else createIterator.call(Array(source))
            }
        }
    )
    ctx.globalScope.setVariable(
      "__createAsyncIterator",
      JSValue.Native(createAsyncIterator)
    )

    val iteratorClose = NativeFunction(
      name = "__iteratorClose",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val iterator = args.headOption.getOrElse(JSValue.Undefined)
        iterator match {
          case JSValue.Object(obj) =>
            val returnMethod = obj.get("return")(using ctx)
            if returnMethod == JSValue.Undefined || returnMethod == JSValue.Null then
              JSValue.Undefined
            else {
              if !BuiltinHelpers.isCallable(returnMethod) then
                ctx.throwTypeError("iterator return is not callable")
              val result = callFunctionWithThis(returnMethod, iterator, Array.empty)
              if !result.isObject then
                ctx.throwTypeError("iterator return result is not an object")
              JSValue.Undefined
            }
          case _ => ctx.throwTypeError("iterator is not an object")
        }
    )
    ctx.globalScope.setVariable("__iteratorClose", JSValue.Native(iteratorClose))

    val iteratorCloseAbrupt = NativeFunction(
      name = "__iteratorCloseAbrupt",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        try {
          iteratorClose.call(args)
          JSValue.Undefined
        } catch {
          // When the incoming completion is a throw completion, errors from
          // retrieving/calling return() and validating its result do not
          // replace the original exception.
          case _: Throwable => JSValue.Undefined
        }
    )
    ctx.globalScope.setVariable(
      "__iteratorCloseAbrupt",
      JSValue.Native(iteratorCloseAbrupt)
    )

    // Private methods are class-scoped values, distinct from writable private
    // data fields. Keeping them in their own table lets writes enforce the
    // spec's method [[Set]] TypeError path.
    val initPrivateMethod = NativeFunction(
      name = "__initPrivateMethod__",
      impl = (args, ctx) =>
        given JSContext = ctx
        val obj = args(0)
        val name = args(1).toString
        val methodFn = args(2)
        val target = obj match {
          case JSValue.Object(o) => o
          case f: JSValue.Function => f.funcObj
          case _ =>
            ctx.throwTypeError("Cannot define private method on non-object")
        }
        val methodsMap = target.getOwnProperty("__privateMethods__") match {
          case Some(JSValue.Object(mm)) => mm
          case _ =>
            val mm =
              quickjs.objmodel.JSObject(prototype = null, extensible = true)
            target.defineProperty(
              "__privateMethods__",
              JSValue.Object(mm),
              enumerable = false,
              writable = false,
              configurable = false
            )
            mm
        }
        methodsMap.set(name, methodFn)
        JSValue.Undefined
    )
    ctx.globalScope.setVariable(
      "__initPrivateMethod__",
      JSValue.Native(initPrivateMethod)
    )

    // __initPrivateGetter__(obj, name, getterFn) - initialize a private getter
    val initPrivateGetter = NativeFunction(
      name = "__initPrivateGetter__",
      impl = (args, ctx) =>
        given JSContext = ctx
        // Native helper calls receive only the explicit arguments.
        val obj = args(0)
        val name = args(1).toString
        val getterFn = args(2)

        obj match {
          case JSValue.Object(o) =>
            // Get or create __privateGetters__ map
            val gettersMap = o.getOwnProperty("__privateGetters__") match {
              case Some(JSValue.Object(gm)) => gm
              case _                        =>
                val gm =
                  quickjs.objmodel.JSObject(prototype = null, extensible = true)
                o.defineProperty(
                  "__privateGetters__",
                  JSValue.Object(gm),
                  enumerable = false,
                  writable = false,
                  configurable = false
                )
                gm
            }
            gettersMap.set(name, getterFn)
          case f: JSValue.Function =>
            // Handle Function's funcObj
            val gettersMap =
              f.funcObj.getOwnProperty("__privateGetters__") match {
                case Some(JSValue.Object(gm)) => gm
                case _                        =>
                  val gm = quickjs.objmodel
                    .JSObject(prototype = null, extensible = true)
                  f.funcObj.defineProperty(
                    "__privateGetters__",
                    JSValue.Object(gm),
                    enumerable = false,
                    writable = false,
                    configurable = false
                  )
                  gm
              }
            gettersMap.set(name, getterFn)
          case _ =>
            ctx.throwTypeError("Cannot define private getter on non-object")
        }
        JSValue.Undefined
    )
    ctx.globalScope.setVariable(
      "__initPrivateGetter__",
      JSValue.Native(initPrivateGetter)
    )

    // __initPrivateSetter__(obj, name, setterFn) - initialize a private setter
    val initPrivateSetter = NativeFunction(
      name = "__initPrivateSetter__",
      impl = (args, ctx) =>
        given JSContext = ctx
        // Native helper calls receive only the explicit arguments.
        val obj = args(0)
        val name = args(1).toString
        val setterFn = args(2)

        obj match {
          case JSValue.Object(o) =>
            // Get or create __privateSetters__ map
            val settersMap = o.getOwnProperty("__privateSetters__") match {
              case Some(JSValue.Object(sm)) => sm
              case _                        =>
                val sm =
                  quickjs.objmodel.JSObject(prototype = null, extensible = true)
                o.defineProperty(
                  "__privateSetters__",
                  JSValue.Object(sm),
                  enumerable = false,
                  writable = false,
                  configurable = false
                )
                sm
            }
            settersMap.set(name, setterFn)
          case f: JSValue.Function =>
            // Handle Function's funcObj
            val settersMap =
              f.funcObj.getOwnProperty("__privateSetters__") match {
                case Some(JSValue.Object(sm)) => sm
                case _                        =>
                  val sm = quickjs.objmodel
                    .JSObject(prototype = null, extensible = true)
                  f.funcObj.defineProperty(
                    "__privateSetters__",
                    JSValue.Object(sm),
                    enumerable = false,
                    writable = false,
                    configurable = false
                  )
                  sm
              }
            settersMap.set(name, setterFn)
          case _ =>
            ctx.throwTypeError("Cannot define private setter on non-object")
        }
        JSValue.Undefined
    )
    ctx.globalScope.setVariable(
      "__initPrivateSetter__",
      JSValue.Native(initPrivateSetter)
    )
  }

  /** `__getSuperProp(proto, key, receiver)`: property lookup for `super.prop`,
    * which must invoke accessors with the current `this` as receiver. Plain
    * property access on the superclass prototype would bind `this` to the
    * prototype instead (ajv's `super.names`).
    */
  def initializeSuperHelpers(ctx: JSContext): Unit = {
    val getSuperProp = NativeFunction(
      name = "__getSuperProp",
      impl = (args, callCtx) =>
        given JSContext = callCtx
        val proto = args.headOption.getOrElse(JSValue.Undefined)
        val key = args.lift(1).getOrElse(JSValue.Undefined)
        val receiver = args.lift(2).getOrElse(JSValue.Undefined)
        val noop = quickjs.tracing.TraceRecorder.Noop
        val interpreter = Interpreter()
        key match {
          case JSValue.Symbol(id) =>
            proto match {
              case JSValue.Object(obj) =>
                interpreter.getPropertyValueBySymbol(
                  obj,
                  receiver,
                  id,
                  Nil,
                  noop
                )
              case _ => JSValue.Undefined
            }
          case _ =>
            val keyName = key match {
              case JSValue.JSStr(s) => s
              case other            => other.toString
            }
            proto match {
              case JSValue.Object(obj) =>
                interpreter.getPropertyValue(
                  obj,
                  receiver,
                  keyName,
                  Nil,
                  noop
                )
              case _ => JSValue.Undefined
            }
        }
    )
    ctx.globalScope.setVariable("__getSuperProp", JSValue.Native(getSuperProp))
  }

  def initializeModuleHelpers(
      ctx: JSContext,
      loader: Option[ModuleLoader]
  ): Unit = {
    def loadModuleWithLoader(
        loader: ModuleLoader,
        specifier: String,
        context: JSContext
    ): JSValue = {
      given JSContext = context
      loader match {
        case fileLoader: FileModuleLoader =>
          fileLoader.loadModule(specifier, context.currentModulePath)
        case _ =>
          val fromPath = context.currentModulePath
          val resolvedName = loader.resolve(specifier, fromPath)

          context.rt.getModuleExports(resolvedName) match {
            case Some(exports) =>
              JSValue.Object(exports)
            case None =>
              val loadResult =
                try loader.load(resolvedName)
                catch {
                  case e: Exception =>
                    context.throwError(
                      "Error",
                      s"Cannot find module '$specifier': ${e.getMessage}"
                    )
                }

              val lexer = quickjs.lexer.Lexer(loadResult.source)
              val tokens = lexer.tokenize()
              val parser =
                new quickjs.parser.Parser(tokens, moduleMode = true)
              val ast = parser.parseScript()
              val compiler = quickjs.compiler.Compiler()
              val bytecode = compiler.compileModule(ast, resolvedName)
              val interpreter = Interpreter()

              val previousPath = context.currentModulePath
              context.currentModulePath = resolvedName
              try {
                val result =
                  interpreter.call(bytecode, JSValue.Undefined, Array.empty)
                quickjs.module.ModuleEvaluation.settleAndCheck(result)(
                  using context
                )
              } finally
                context.currentModulePath = previousPath

              JSValue.Object(context.rt.ensureModuleExports(resolvedName))
          }
      }
    }

    // Capture the loader in a local val so closures use the correct instance
    val capturedLoader = loader

    val moduleImport = NativeFunction(
      name = "__moduleImport",
      impl = (args, context) =>
        given JSContext = context
        val specifier = args.headOption match {
          case Some(JSValue.JSStr(s)) => s
          case Some(other)            => other.toString
          case None                   => ""
        }

        capturedLoader.orElse(context.rt.getModuleLoaderOption) match {
          case Some(loader) =>
            loadModuleWithLoader(loader, specifier, context)
          case None =>
            context.rt.getModuleExports(specifier) match {
              case Some(exportsObj) => JSValue.Object(exportsObj)
              case None             =>
                context.throwError(
                  "Error",
                  s"Cannot import module '$specifier': no module loader configured"
                )
            }
        }
    )

    val moduleInstantiate = NativeFunction(
      name = "__moduleInstantiate",
      impl = (args, context) =>
        given JSContext = context
        val specifier = args.headOption match {
          case Some(JSValue.JSStr(s)) => s
          case Some(other)            => other.toString
          case None                   => ""
        }
        val names = args.drop(1).map {
          case JSValue.JSStr(s) => s
          case other            => other.toString
        }
        capturedLoader.orElse(context.rt.getModuleLoaderOption) match {
          case Some(fileLoader: quickjs.module.FileModuleLoader) =>
            try
              fileLoader.instantiate(specifier, context.currentModulePath, names)
            catch {
              case e: quickjs.module.ModuleLinkException =>
                context.throwError(
                  "SyntaxError",
                  Option(e.getMessage).getOrElse("module link error")
                )
            }
          case _ => ()
        }
        JSValue.Undefined
    )

    def fulfilledPromise(value: JSValue)(using JSContext): JSValue =
      wrapPromise(
        JSValue.Promise(
          state = JSValue.PromiseState.Fulfilled,
          result = value
        )
      )

    def rejectedPromise(reason: JSValue)(using JSContext): JSValue =
      wrapPromise(
        JSValue.Promise(
          state = JSValue.PromiseState.Rejected,
          result = reason
        )
      )

    val dynamicImport = NativeFunction(
      name = "__dynamicImport",
      impl = (args, context) =>
        given JSContext = context
        try {
          val specifier =
            toJSString(args.headOption.getOrElse(JSValue.Undefined))
          val fromModule = args.drop(1).headOption match {
            case Some(JSValue.JSStr(s)) => s
            case Some(other) if other != JSValue.Undefined => other.toString
            case _                                         => context.currentModulePath
          }
          val previousPath = context.currentModulePath
          if fromModule.nonEmpty then context.currentModulePath = fromModule
          try {
            val namespace =
              capturedLoader.orElse(context.rt.getModuleLoaderOption) match {
                case Some(loader) =>
                  loadModuleWithLoader(loader, specifier, context)
                case None =>
                  context.rt.getModuleExports(specifier) match {
                    case Some(exportsObj) => JSValue.Object(exportsObj)
                    case None =>
                      context.throwError(
                        "Error",
                        s"Cannot import module '$specifier': no module loader configured"
                      )
                  }
              }
            fulfilledPromise(namespace)
          } finally context.currentModulePath = previousPath
        } catch {
          case e: JSException => rejectedPromise(e.getValue)
          case e: Exception =>
            rejectedPromise(context.createError("Error", e.getMessage))
        }
    )

    val moduleExport = NativeFunction(
      name = "__moduleExport",
      impl = (args, context) =>
        given JSContext = context
        val moduleName = args.headOption match {
          case Some(JSValue.JSStr(s)) => s
          case Some(other)            => other.toString
          case None                   => ""
        }
        val exportName = args.drop(1).headOption match {
          case Some(JSValue.JSStr(s)) => s
          case Some(other)            => other.toString
          case None                   => ""
        }
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
        val moduleName = args.headOption match {
          case Some(JSValue.JSStr(s)) => s
          case Some(other)            => other.toString
          case None                   => ""
        }
        val sourceSpecifier = args.drop(1).headOption match {
          case Some(JSValue.JSStr(s)) => s
          case Some(other)            => other.toString
          case None                   => ""
        }

        // First, load the source module if using file-based loading
        val sourceObj = capturedLoader.orElse(
          context.rt.getModuleLoaderOption
        ) match {
          case Some(loader) =>
            loadModuleWithLoader(loader, sourceSpecifier, context) match {
              case JSValue.Object(obj) => obj
              case _                   =>
                context.rt.ensureModuleExports(
                  context.rt
                    .resolveModule(sourceSpecifier, context.currentModulePath)
                )
            }
          case None =>
            context.rt.getModuleExports(sourceSpecifier) match {
              case Some(obj) => obj
              case None      =>
                context.throwError(
                  "Error",
                  s"Cannot export from module '$sourceSpecifier': no module loader configured"
                )
            }
        }

        val exportsObj = context.rt.ensureModuleExports(moduleName)
        val keys = sourceObj.getOwnPropertyKeys()
        for key <- keys if key != "default" do
          sourceObj.getOwnProperty(key) match {
            case Some(value) => exportsObj.set(key, value)
            case None        => ()
          }
        JSValue.Undefined
    )

    val importMeta = NativeFunction(
      name = "__importMeta",
      impl = (args, context) =>
        given JSContext = context
        val moduleName = args.headOption match {
          case Some(JSValue.JSStr(s)) if s.nonEmpty => s
          case Some(other) if other != JSValue.Undefined && other.toString.nonEmpty =>
            other.toString
          case _ => context.currentModulePath
        }
        if moduleName.isEmpty || moduleName == "<script>" then
          context.throwTypeError("import.meta not supported in this context")
        JSValue.Object(context.rt.ensureModuleMeta(moduleName))
    )

    ctx.globalScope.setVariable("__moduleImport", JSValue.Native(moduleImport))
    ctx.globalScope.setVariable(
      "__moduleInstantiate",
      JSValue.Native(moduleInstantiate)
    )
    ctx.globalScope.setVariable("__dynamicImport", JSValue.Native(dynamicImport))
    ctx.globalScope.setVariable("__moduleExport", JSValue.Native(moduleExport))
    ctx.globalScope.setVariable(
      "__moduleExportAll",
      JSValue.Native(moduleExportAll)
    )
    ctx.globalScope.setVariable("__importMeta", JSValue.Native(importMeta))
  }

  def initializeArrayHelpers(ctx: JSContext): Unit = {
    def extractArrayElements(value: JSValue)(using JSContext): Array[JSValue] =
      value match {
        case JSValue.JSArrayVal(arr) =>
          (0 until arr.getLength).map(arr.get).toArray
        case JSValue.Object(obj) =>
          obj.get("length") match {
            case JSValue.Int32(len) if len > 0 =>
              (0 until len).map(i => obj.get(i.toString)).toArray
            case JSValue.Float64(len) if len > 0 =>
              (0 until len.toInt).map(i => obj.get(i.toString)).toArray
            case _ => Array.empty[JSValue]
          }
        case _ => Array.empty[JSValue]
      }

    def isProxyValue(v: JSValue)(using
        JSContext
    ): Option[(JSValue, JSObject)] =
      v match {
        case JSValue.Object(obj) =>
          val targetOpt = obj.getOwnProperty("__proxy_target")
          val handlerOpt = obj.getOwnProperty("__proxy_handler")
          val revoked = obj.getOwnProperty("__proxy_revoked") match {
            case Some(JSValue.Bool(true)) => true
            case _                        => false
          }
          if (targetOpt.isDefined || handlerOpt.isDefined) &&
              (revoked || targetOpt.contains(JSValue.Null) || handlerOpt
                .contains(JSValue.Null))
          then ctx.throwTypeError("Cannot perform operation on a revoked proxy")
          (targetOpt, handlerOpt) match {
            case (Some(target), Some(JSValue.Object(handler))) =>
              Some((target, handler))
            case _ => None
          }
        case _ => None
      }

    def isCallable(value: JSValue)(using JSContext): Boolean =
      value match {
        case _: JSValue.Function => true
        case JSValue.Native(_: quickjs.value.NativeFunction) => true
        case JSValue.Native(_: quickjs.value.NativeConstructor) => true
        case proxy @ JSValue.Object(_) =>
          isProxyValue(proxy).exists { case (target, _) => isCallable(target) }
        case _ => false
      }

    def getWellKnownSymbol(name: String)(using ctx: JSContext): JSValue =
      ctx.global.get("Symbol") match {
        case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
          nc.funcObj.get(name)(using ctx)
        case _ => JSValue.Undefined
      }

    def getProperty(value: JSValue, key: JSValue)(using ctx: JSContext): JSValue =
      def fromObject(obj: JSObject, receiver: JSValue): JSValue =
        key match {
          case JSValue.Symbol(sym) =>
            obj.getSymbolPropertyDescriptorWithOwner(sym) match {
              case Some((_, stored, attrs)) =>
                attrs.getter match {
                  case Some(getter) =>
                    callFunctionWithThis(getter, receiver, Array.empty)
                  case None => stored
                }
              case None => JSValue.Undefined
            }
          case _ =>
            obj.getPropertyDescriptorWithOwner(key.toString) match {
              case Some((_, stored, attrs)) =>
                attrs.getter match {
                  case Some(getter) =>
                    callFunctionWithThis(getter, receiver, Array.empty)
                  case None => stored
                }
              case None => JSValue.Undefined
            }
        }

      value match {
        case JSValue.JSArrayVal(arr) =>
          key match {
            case JSValue.Symbol(sym) =>
              fromObject(ctx.arrayPrototype, value)
            case JSValue.JSStr("length") => JSValue.fromInt(arr.getLength)
            case JSValue.JSStr(s) if s.forall(_.isDigit) && s.nonEmpty =>
              arr.get(s.toInt)
            case JSValue.Int32(i) => arr.get(i)
            case JSValue.Float64(d) if d.isValidInt => arr.get(d.toInt)
            case JSValue.JSStr(s) => arr.getProperty(s).getOrElse(JSValue.Undefined)
            case _                => JSValue.Undefined
          }
        case JSValue.JSStr(str) =>
          key match {
            case JSValue.Symbol(_) =>
              JSValue.Native(
                NativeFunction(
                  name = "[Symbol.iterator]",
                  length = 0,
                  impl = (callArgs, innerCtx) =>
                    given JSContext = innerCtx
                    val text = callArgs.headOption match {
                      case Some(JSValue.JSStr(value)) => value
                      case _                         => str
                    }
                    var index = 0
                    val iterator = JSObject(
                      prototype = innerCtx.objectPrototype,
                      extensible = true
                    )
                    iterator.defineProperty(
                      "next",
                      JSValue.Native(
                        NativeFunction(
                          name = "next",
                          length = 0,
                          impl = (_, _) =>
                            val result = JSObject(
                              prototype = innerCtx.objectPrototype,
                              extensible = true
                            )
                            if index >= text.length then {
                              result.set("value", JSValue.Undefined)
                              result.set("done", JSValue.Bool(true))
                            } else {
                              val cp = text.codePointAt(index)
                              index += Character.charCount(cp)
                              result.set(
                                "value",
                                JSValue.fromString(new String(Character.toChars(cp)))
                              )
                              result.set("done", JSValue.Bool(false))
                            }
                            JSValue.Object(result)
                        )
                      ),
                      enumerable = false
                    )
                    JSValue.Object(iterator)
                )
              )
            case JSValue.JSStr("length") => JSValue.fromInt(str.length)
            case JSValue.JSStr(s) if s.forall(_.isDigit) && s.nonEmpty =>
              val i = s.toInt
              if i >= 0 && i < str.length then JSValue.JSStr(str.charAt(i).toString)
              else JSValue.Undefined
            case JSValue.Int32(i) if i >= 0 && i < str.length =>
              JSValue.JSStr(str.charAt(i).toString)
            case _ => JSValue.Undefined
          }
        case JSValue.Object(obj) =>
          fromObject(obj, value)
        case func: JSValue.Function =>
          fromObject(func.funcObj, value)
        case JSValue.Native(nf: quickjs.value.NativeFunction) =>
          fromObject(nf.funcObj, value)
        case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
          fromObject(nc.funcObj, value)
        case _ => JSValue.Undefined
      }

    def toArgArray(values: Array[JSValue]): JSValue = {
      val arr = JSArray.empty()
      values.foreach(arr.push)
      JSValue.JSArrayVal(arr)
    }

    def callSpreadTarget(
        target: JSValue,
        thisValue: JSValue,
        callArgs: Array[JSValue],
        isMethod: Boolean
    )(using JSContext): JSValue =
      target match {
        case proxy @ JSValue.Object(_) if isProxyValue(proxy).isDefined =>
          val (proxyTarget, handler) = isProxyValue(proxy).get
          if !isCallable(proxyTarget) then
            ctx.throwTypeError("proxy target is not callable")
          handler.get("apply") match {
            case JSValue.Undefined =>
              callSpreadTarget(proxyTarget, thisValue, callArgs, isMethod)
            case trap =>
              callFunctionWithThis(
                trap,
                JSValue.Object(handler),
                Array(proxyTarget, thisValue, toArgArray(callArgs))
              )
          }
        case func: JSValue.Function =>
          Interpreter()
            .call(
              BuiltinHelpers.functionToBytecode(func),
              thisValue,
              callArgs,
              func.closure
            )
        case JSValue.Native(native: quickjs.value.NativeFunction) =>
          if isMethod then {
            val argsWithThis = new Array[JSValue](callArgs.length + 1)
            argsWithThis(0) = thisValue
            Array.copy(callArgs, 0, argsWithThis, 1, callArgs.length)
            native.call(argsWithThis)
          } else native.call(callArgs)
        case JSValue.Native(constructor: quickjs.value.NativeConstructor) =>
          constructor.call(callArgs)
        case _ =>
          ctx.throwTypeError(s"Cannot call non-function value: $target")
      }

    def appendSpreadSource(target: JSArray, source: JSValue)(using
        JSContext
    ): Unit =
      source match {
        case JSValue.JSArrayVal(src) =>
          var i = 0
          while i < src.getLength do {
            target.push(src.get(i))
            i += 1
          }
        case JSValue.JSStr(str) =>
          str.foreach(ch => target.push(JSValue.JSStr(ch.toString)))
        case JSValue.Object(obj) =>
          val iteratorMethod = getWellKnownSymbol("iterator") match {
            case JSValue.Symbol(sym) => getProperty(source, JSValue.Symbol(sym))
            case _                   => JSValue.Undefined
          }
          if iteratorMethod != JSValue.Undefined && iteratorMethod != JSValue.Null
          then {
            if !isCallable(iteratorMethod) then
              ctx.throwTypeError("value is not iterable")
            val iterator =
              callFunctionWithThis(iteratorMethod, source, Array.empty)
            val nextMethod = getProperty(iterator, JSValue.fromString("next"))
            if !isCallable(nextMethod) then
              ctx.throwTypeError("iterator next is not callable")
            var done = false
            while !done do {
              callFunctionWithThis(nextMethod, iterator, Array.empty) match {
                case JSValue.Object(resultObj) =>
                  resultObj.get("done") match {
                    case JSValue.Bool(true) => done = true
                    case _ =>
                      target.push(resultObj.get("value"))
                  }
                case _ => ctx.throwTypeError("iterator result is not an object")
              }
            }
          } else if obj.get("next") != JSValue.Undefined then {
            val nextMethod = obj.get("next")
            if !isCallable(nextMethod) then
              ctx.throwTypeError("iterator next is not callable")
            var done = false
            while !done do {
              callFunctionWithThis(nextMethod, source, Array.empty) match {
                case JSValue.Object(resultObj) =>
                  resultObj.get("done") match {
                    case JSValue.Bool(true) => done = true
                    case _                  => target.push(resultObj.get("value"))
                  }
                case _ => ctx.throwTypeError("iterator result is not an object")
              }
            }
          } else {
            obj.get("length") match {
              case JSValue.Int32(len) if len >= 0 =>
                var i = 0
                while i < len do {
                  target.push(obj.get(i.toString))
                  i += 1
                }
              case JSValue.Float64(len) if len >= 0 =>
                var i = 0
                while i < len.toInt do {
                  target.push(obj.get(i.toString))
                  i += 1
                }
              case _ => ctx.throwTypeError("value is not iterable")
            }
          }
        case _ => ctx.throwTypeError("value is not iterable")
      }

    val arrayPush = NativeFunction(
      name = "__arrayPush",
      impl = (args, ctx) =>
        if args.length < 2 then JSValue.Undefined
        else
          args(0) match {
            case arrVal: JSValue.JSArrayVal =>
              arrVal.value.push(args(1))
              arrVal
            case _ => JSValue.Undefined
          }
    )

    val arraySpread = NativeFunction(
      name = "__arraySpread",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 2 then JSValue.Undefined
        else
          (args(0), args(1)) match {
            case (arrVal: JSValue.JSArrayVal, srcVal: JSValue.JSArrayVal) =>
              appendSpreadSource(arrVal.value, srcVal)
              arrVal
            case (arrVal: JSValue.JSArrayVal, source) =>
              appendSpreadSource(arrVal.value, source)
              arrVal
            case _ =>
              JSValue.Undefined
          }
    )

    val destructureArray = NativeFunction(
      name = "__destructureArray",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.isEmpty then ctx.throwTypeError("value is not iterable")
        else {
          val source = args(0)
          val requested = args.lift(1) match {
            case Some(JSValue.Int32(value)) => math.max(0, value)
            case Some(value) => math.max(0, value.toNumber.toInt)
            case None => Int.MaxValue
          }
          val consumeRest = args.lift(2).exists(_.toBoolean)
          val result = JSArray.empty()

          val iteratorKey = getWellKnownSymbol("iterator")
          val iteratorMethod = getProperty(source, iteratorKey)
          if !isCallable(iteratorMethod) then
            ctx.throwTypeError("value is not iterable")
          val iterator = callFunctionWithThis(iteratorMethod, source, Array.empty)
          if !iterator.isObject then
            ctx.throwTypeError("iterator is not an object")
          val nextMethod = getProperty(iterator, JSValue.fromString("next"))
          if !isCallable(nextMethod) then
            ctx.throwTypeError("iterator next is not callable")

          var done = false
          var count = 0
          while !done && (consumeRest || count < requested) do {
            val nextResult =
              callFunctionWithThis(nextMethod, iterator, Array.empty)
            if !nextResult.isObject then
              ctx.throwTypeError("iterator result is not an object")
            done = getProperty(nextResult, JSValue.fromString("done")).toBoolean
            if !done then {
              result.push(getProperty(nextResult, JSValue.fromString("value")))
              count += 1
            }
          }

          // Abrupt completion of IteratorStep/IteratorValue marks the iterator
          // record done and does not perform IteratorClose. Only normal early
          // completion of a non-rest pattern invokes return().
          if !done && !consumeRest then {
            val returnMethod =
              getProperty(iterator, JSValue.fromString("return"))
            if returnMethod != JSValue.Undefined && returnMethod != JSValue.Null
            then {
              if !isCallable(returnMethod) then
                ctx.throwTypeError("iterator return is not callable")
              val closeResult =
                callFunctionWithThis(returnMethod, iterator, Array.empty)
              if !closeResult.isObject then
                ctx.throwTypeError("iterator return result is not an object")
            }
          }
          JSValue.JSArrayVal(result)
        }
    )

    val requireObjectCoercible = NativeFunction(
      name = "__requireObjectCoercible",
      length = 1,
      impl = (args, ctx) =>
        val value = args.headOption.getOrElse(JSValue.Undefined)
        value match {
          case JSValue.Null | JSValue.Undefined =>
            ctx.throwTypeError("Cannot destructure null or undefined")
          case _ => value
        }
    )

    val setFunctionName = NativeFunction(
      name = "__setFunctionName",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = args.headOption.getOrElse(JSValue.Undefined)
        val name = args.lift(1) match {
          case Some(JSValue.JSStr(text)) => text
          case Some(other)               => other.toString
          case None                      => ""
        }
        value match {
          case fn: JSValue.Function =>
            // An anonymous class can define a static `name` element while its
            // body is evaluated. NamedEvaluation must not replace that class
            // element with the inferred binding name.
            fn.funcObj.getOwnPropertyDescriptor("name") match {
              case Some((_: JSValue.JSStr, _)) =>
                fn.funcObj.defineProperty(
                  "name",
                  JSValue.fromString(name),
                  enumerable = false,
                  writable = false,
                  configurable = true
                )
                fn.copy(name = name)
              case Some(_) => fn
              case None =>
                fn.funcObj.defineProperty(
                  "name",
                  JSValue.fromString(name),
                  enumerable = false,
                  writable = false,
                  configurable = true
                )
                fn.copy(name = name)
            }
          case other => other
        }
    )

    val makeTemplateObject = NativeFunction(
      name = "__makeTemplateObject",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 3 then
          ctx.throwTypeError(
            "__makeTemplateObject requires id, cooked and raw arrays"
          )
        val cacheKey = args(0) match {
          case JSValue.JSStr(id) => id
          case _ => ctx.throwTypeError("__makeTemplateObject id must be a string")
        }
        ctx.templateObjectCache.get(cacheKey) match {
          case Some(templateObject) => templateObject
          case None =>
            def copyFrozenArray(value: JSValue): JSArray =
              value match {
                case JSValue.JSArrayVal(src) =>
                  val dst = JSArray.empty()
                  var i = 0
                  while i < src.getLength do {
                    dst.defineIndexProperty(
                      i,
                      src.get(i),
                      enumerable = true,
                      writable = false,
                      configurable = false
                    )
                    i += 1
                  }
                  dst.isExtensible = false
                  dst
                case _ =>
                  ctx.throwTypeError(
                    "__makeTemplateObject arguments must be arrays"
                  )
              }

            val cooked = copyFrozenArray(args(1))
            val raw = copyFrozenArray(args(2))
            cooked.setProperty("raw", JSValue.JSArrayVal(raw))
            cooked.isExtensible = false
            val templateObject = JSValue.JSArrayVal(cooked)
            ctx.templateObjectCache(cacheKey) = templateObject
            templateObject
        }
    )

    // Helper for the object literal `__proto__: value` form (B.3.1): unlike
    // Object.setPrototypeOf, non-object and non-null values are ignored.
    val objectSetProto = NativeFunction(
      name = "__objectSetProto",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 2 then JSValue.Undefined
        else {
          val target = args(0)
          val proto = args(1)
          val isProtoValue =
            proto == JSValue.Null || BuiltinHelpers.isObjectLikeValue(proto)
          if isProtoValue then
            target match {
              case JSValue.Object(obj) =>
                proto match {
                  case JSValue.Object(p) =>
                    obj.setPrototypeValue(null)
                    obj.setPrototype(p)
                  case JSValue.Null =>
                    obj.setPrototypeValue(null)
                    obj.setPrototype(null)
                  case JSValue.JSArrayVal(_) =>
                    obj.setPrototypeValue(proto)
                  case f: JSValue.Function =>
                    obj.setPrototypeValue(JSValue.Object(f.funcObj))
                  case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                    obj.setPrototypeValue(JSValue.Object(nf.funcObj))
                  case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                    obj.setPrototypeValue(JSValue.Object(nc.funcObj))
                  case _ => ()
                }
              case _ => ()
            }
          target
        }
    )

    // Helper for defining an own data property (used for concise methods and
    // accessors named "__proto__", which must not trigger the inherited
    // __proto__ setter).
    val defineOwn = NativeFunction(
      name = "__defineOwn",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 3 then JSValue.Undefined
        else {
          val target = args(0)
          val key = args(1)
          val value = args(2)
          BuiltinHelpers.extractJSObject(target).foreach { obj =>
            key match {
              case JSValue.Symbol(id) =>
                obj.defineSymbolDataProperty(
                  id,
                  Some(value),
                  Some(true),
                  Some(true),
                  Some(true)
                )
              case other =>
                obj.defineProperty(
                  BuiltinHelpers.toJSString(other),
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )
            }
          }
          value
        }
    )

    // Helper for object spread: __objectSpread(target, source)
    // Copies all enumerable own properties from source to target
    val objectSpread = NativeFunction(
      name = "__objectSpread",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 2 then JSValue.Undefined
        else {
          val target = args(0)
          val source = args(1)

          (target, source) match {
            case (JSValue.Object(targetObj), JSValue.Object(srcObj)) =>
              for key <- srcObj.getOwnPropertyKeys() do {
                val value = srcObj.get(key)
                targetObj.set(key, value)
              }
              target
            case (
                  JSValue.Object(targetObj),
                  JSValue.Null | JSValue.Undefined
                ) =>
              // Spreading null/undefined is a no-op
              target
            case (JSValue.Object(_), _) =>
              // For non-object sources, no properties are copied
              target
            case _ =>
              JSValue.Undefined
          }
        }
    )

    // Helper for spreading arguments in super() calls: __funcSpread(superFunc, thisObj, argsArray)
    // Calls superFunc with thisObj and spreads argsArray as individual arguments
    val funcSpread = NativeFunction(
      name = "__funcSpread",
      impl = (args, ctx) =>
        import quickjs.bytecode.BytecodeFunction
        given JSContext = ctx
        if args.length < 3 then JSValue.Undefined
        else {
          val func = args(0)
          val thisObj = args(1)
          val argsArray = args(2)

          // Extract arguments from array or array-like object (e.g., 'arguments')
          val callArgs: Array[JSValue] = argsArray match {
            case JSValue.JSArrayVal(arr) =>
              (0 until arr.getLength).map(i => arr.get(i)).toArray
            case JSValue.Object(obj) =>
              // Handle array-like objects (e.g., the 'arguments' object)
              obj.get("length")(using ctx) match {
                case JSValue.Int32(len) if len > 0 =>
                  (0 until len).map { i =>
                    obj.get(i.toString)(using ctx)
                  }.toArray
                case JSValue.Float64(len) if len > 0 =>
                  (0 until len.toInt).map { i =>
                    obj.get(i.toString)(using ctx)
                  }.toArray
                case _ => Array.empty[JSValue]
              }
            case _ =>
              Array.empty[JSValue]
          }

          // The superclass constructor sees an initialized `this`.
          thisObj match {
            case JSValue.Object(obj) =>
              obj.deleteProperty("__thisUninitialized")
            case JSValue.JSArrayVal(arr) =>
              arr.deleteProperty("__thisUninitialized")
            case _ => ()
          }
          // Call the function with extracted arguments
          val spreadResult = func match {
            case f: JSValue.Function =>
              try {
                val bcFunc = BuiltinHelpers.functionToBytecode(f)
                val result = quickjs.interpreter
                  .Interpreter()
                  .call(bcFunc, thisObj, callArgs, f.closure)
                if f.isConstructor then thisObj else result
              }
              catch {
                case e: Exception =>
                  // Propagate exceptions from constructor calls
                  throw e
              }
            case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
              // Native superclass constructor (e.g. Promise/Array subclasses):
              // initialize the already-created derived `this`.
              nc.callWithThis(thisObj, callArgs)(using ctx)
              thisObj
            case _ =>
              ctx.throwTypeError("Super constructor is not a constructor")
          }
          spreadResult
        }
    )

    // ClassDefinitionEvaluation step: the heritage value must be a constructor.
    val checkClassHeritage = NativeFunction(
      name = "__checkClassHeritage",
      impl = (args, ctx) =>
        given JSContext = ctx
        def isConstructorValue(v: JSValue): Boolean =
          v match {
            case f: JSValue.Function => f.isConstructor
            case JSValue.Native(nc: quickjs.value.NativeConstructor) => true
            case JSValue.Native(_: quickjs.value.NativeFunction)     => false
            case JSValue.Object(obj) =>
              obj.getOwnProperty("__proxy_target") match {
                case Some(JSValue.Null) => false
                case Some(target)       => isConstructorValue(target)
                case None               => false
              }
            case _ => false
          }
        val value = args.headOption.getOrElse(JSValue.Undefined)
        if !isConstructorValue(value) then
          ctx.throwTypeError("Class extends value is not a constructor")
        JSValue.Undefined
    )
    ctx.globalScope.setVariable("__checkClassHeritage", JSValue.Native(checkClassHeritage))

    val callSpread = NativeFunction(
      name = "__callSpread",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 4 then
          ctx.throwTypeError("__callSpread requires function, this, args, flag")
        val target = args(0)
        val thisValue = args(1)
        val callArgs = extractArrayElements(args(2))
        val isMethod = args(3).toBoolean
        callSpreadTarget(target, thisValue, callArgs, isMethod)
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
        val (excludedStrings, excludedSymbols) = excludeKeys match {
          case arrVal: JSValue.JSArrayVal =>
            val arr = arrVal.value
            val strings = scala.collection.mutable.Set[String]()
            val symbols = scala.collection.mutable.Set[Int]()
            var i = 0
            while i < arr.getLength do {
              toPropertyKey(arr.get(i)) match {
                case JSValue.JSStr(s)   => strings += s
                case JSValue.Symbol(id) => symbols += id
                case _                  => ()
              }
              i += 1
            }
            (strings.toSet, symbols.toSet)
          case _ =>
            ctx.throwTypeError("__objectRest: second argument must be an array")
        }

        // Create a new object with remaining properties
        val boxedSource = BuiltinHelpers.toObject(source)
        boxedSource match {
          case JSValue.Object(srcObj) =>
            val result = quickjs.objmodel
              .JSObject(prototype = ctx.objectPrototype, extensible = true)
            for key <- srcObj.getOwnPropertyKeys() do
              if !excludedStrings.contains(key) &&
                srcObj.getOwnPropertyDescriptor(key).exists(_._2.enumerable)
              then {
                val value = BuiltinHelpers.getPropertyWithGetter(boxedSource, key)
                result.defineProperty(
                  key, value, enumerable = true, writable = true,
                  configurable = true
                )
              }
            for symbol <- srcObj.getOwnSymbolPropertyIds() do
              srcObj.getOwnSymbolPropertyDescriptor(symbol) match {
                case Some((_, attrs))
                    if !excludedSymbols.contains(symbol) && attrs.enumerable =>
                  val value = srcObj.getOwnSymbolPropertyDescriptor(symbol) match {
                    case Some((_, attrs)) if attrs.getter.isDefined =>
                      callFunctionWithThis(attrs.getter.get, boxedSource, Array.empty)
                    case Some((propertyValue, _)) => propertyValue
                    case None => JSValue.Undefined
                  }
                  result.initSymbolProperty(
                    symbol, value, enumerable = true, writable = true,
                    configurable = true
                  )
                case _ => ()
              }
            JSValue.Object(result)
          case other =>
            ctx.throwTypeError(
              s"__objectRest: first argument must be an object, got $other"
            )
        }
    )

    given JSContext = ctx
    ctx.globalScope.setVariable("__arrayPush", JSValue.Native(arrayPush))
    ctx.globalScope.setVariable("__arraySpread", JSValue.Native(arraySpread))
    ctx.globalScope
      .setVariable("__destructureArray", JSValue.Native(destructureArray))
    ctx.globalScope.setVariable(
      "__requireObjectCoercible",
      JSValue.Native(requireObjectCoercible)
    )
    ctx.globalScope
      .setVariable("__setFunctionName", JSValue.Native(setFunctionName))
    ctx.globalScope
      .setVariable("__makeTemplateObject", JSValue.Native(makeTemplateObject))
    ctx.globalScope.setVariable("__defineOwn", JSValue.Native(defineOwn))
    ctx.globalScope.setVariable("__objectSetProto", JSValue.Native(objectSetProto))
    ctx.globalScope.setVariable("__objectSpread", JSValue.Native(objectSpread))
    ctx.globalScope.setVariable("__funcSpread", JSValue.Native(funcSpread))
    ctx.globalScope.setVariable("__callSpread", JSValue.Native(callSpread))
    ctx.globalScope.setVariable("__objectRest", JSValue.Native(objectRest))
  }

  /** For-of iteration index tracking */
  private val forOfIndices =
    mutable.Map[Int, Int]() // identityHashCode -> currentIndex

  /** Test helpers: eval, __loadScript, __runMicrotasks, queueMicrotask */
  def initializeTestHelpers(ctx: JSContext): Unit = {
    val loadScript = NativeFunction(
      name = "__loadScript",
      impl = (_, _) => JSValue.Undefined
    )
    given JSContext = ctx
    ctx.global.set("__loadScript", JSValue.Native(loadScript))
    val finalizationJobs =
      mutable.ArrayBuffer.empty[(JSValue, JSValue)]
    val timerJobs = mutable.ArrayBuffer.empty[JSValue]
    ctx.global.set(
      "__finalizationJobs",
      JSValue.Native(finalizationJobs)
    )
    val std = quickjs.objmodel.JSObject(
      prototype = ctx.objectPrototype,
      extensible = true
    )
    std.defineProperty(
      "gc",
      JSValue.Native(
        NativeFunction(
          name = "gc",
          impl = (_, _) =>
            System.gc()
            ctx.global.get("__weakRefs") match {
              case JSValue.Native(refs: mutable.ArrayBuffer[?]) =>
                refs
                  .asInstanceOf[mutable.ArrayBuffer[
                    java.lang.ref.WeakReference[JSValue]
                  ]]
                  .foreach(_.clear())
              case _ => ()
            }
            finalizationJobs.foreach { case (callback, heldValue) =>
              BuiltinHelpers.callFunctionWithThis(
                callback,
                JSValue.Undefined,
                Array(heldValue)
              )
            }
            finalizationJobs.clear()
            JSValue.Undefined
        )
      ),
      enumerable = true
    )
    ctx.global.set("std", JSValue.Object(std))
    val os = quickjs.objmodel.JSObject(
      prototype = ctx.objectPrototype,
      extensible = true
    )
    os.defineProperty(
      "platform",
      JSValue.fromString("linux"),
      enumerable = true
    )
    os.defineProperty(
      "setTimeout",
      JSValue.Native(
        NativeFunction(
          name = "setTimeout",
          impl = (args, _) =>
            val callback =
              args.lift(if args.length >= 2 then 1 else 0)
                .getOrElse(JSValue.Undefined)
            timerJobs += callback
            JSValue.Undefined
        )
      ),
      enumerable = true
    )
    ctx.global.set("os", JSValue.Object(os))

    val evalFunc = NativeFunction(
      name = "eval",
      impl = (args, evalCtx) =>
        given JSContext = evalCtx
        if args.isEmpty then JSValue.Undefined
        else
          args(0) match
            case JSValue.JSStr(source) =>
              def syntaxLocation(message: String): (Int, Int) = {
                val spanPattern =
                  raw"""Span\(\d+,\d+,(\d+),(\d+)\)""".r
                spanPattern.findFirstMatchIn(message) match {
                  case Some(m) =>
                    val zeroBasedLine = m.group(1).toInt
                    val rawColumn = m.group(2).toInt
                    (
                      zeroBasedLine + 1,
                      rawColumn + 1
                    )
                  case None =>
                    val offset =
                      if message.contains("comment") then source.indexOf("/*")
                      else if message.contains("regexp") then
                        source.indexOf('/')
                      else math.max(0, source.length - 1)
                    val prefix = source.take(math.max(0, offset))
                    val line = prefix.count(_ == '\n') + 1
                    val lastNewline = prefix.lastIndexOf('\n')
                    val column = offset - lastNewline
                    (line, math.max(1, column))
                }
              }

              def throwEvalSyntaxError(message: String): Nothing = {
                val (line, column) = syntaxLocation(message)
                evalCtx.createError("SyntaxError", message, 0) match {
                  case value @ JSValue.Object(error) =>
                    error.defineProperty(
                      "lineNumber",
                      JSValue.fromInt(line),
                      enumerable = false
                    )
                    error.defineProperty(
                      "columnNumber",
                      JSValue.fromInt(column),
                      enumerable = false
                    )
                    evalCtx.installLazyStack(
                      error,
                      quickjs.runtime.JSContext.CapturedFrame(
                        "<eval>",
                        "<eval>",
                        line,
                        column,
                        isNative = false
                      ) :: evalCtx.captureFrames()
                    )
                    throw quickjs.runtime.JSException(value)
                  case value => throw quickjs.runtime.JSException(value)
                }
              }

              try {
                // Parse the source code
                val lexer = quickjs.lexer.Lexer(source)
                val tokens = lexer.tokenize()
                val parser = quickjs.parser.Parser(tokens)
                val ast =
                  try parser.parseScript()
                  catch
                    case error: RuntimeException =>
                      throwEvalSyntaxError(
                        Option(error.getMessage).getOrElse("Invalid eval source")
                      )
                val compiler = quickjs.compiler.Compiler()
                val bytecode =
                  compiler.withIndirectEvalMode(compiler.compileScript(ast))
                // Execute the compiled bytecode using the interpreter
                // with the captured `this` and closure from the calling context.
                val interpreter = quickjs.interpreter.Interpreter()
                val capturedThis = evalCtx.currentThis
                val capturedClosure = evalCtx.currentClosure
                val result = interpreter.call(
                  bytecode,
                  capturedThis,
                  Array.empty,
                  capturedClosure
                )
                result
              } catch {
                case e: quickjs.runtime.JSException =>
                  // Re-throw JS exceptions unchanged (preserving Error type)
                  throw e
                case e: RuntimeException =>
                  // Wrap parsing/compilation errors as SyntaxError or re-throw
                  val msg = e.getMessage
                  if msg != null && (msg.contains("SyntaxError") ||
                      msg.contains("Unexpected") || msg.contains("new.target") ||
                      msg.contains("comment") || msg.contains("regexp")) then
                    throwEvalSyntaxError(msg)
                  else throw e
              }
            case _ =>
              // Non-string argument: return unchanged (per ES spec)
              args(0)
    )
    ctx.global.set("eval", JSValue.Native(evalFunc))
    ctx.globalScope.setVariable(
      "__directEval",
      JSValue.Native(evalFunc.copy(name = "__directEval"))
    )
    ctx.globalScope.setVariable(
      "__directEvalField",
      JSValue.Native(evalFunc.copy(name = "__directEvalField"))
    )
    ctx.globalScope.setVariable(
      "__directEvalPrivate",
      JSValue.Native(evalFunc.copy(name = "__directEvalPrivate"))
    )

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
          BuiltinHelpers.callFunctionValue(
            callback,
            JSValue.Undefined,
            Array.empty
          )
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
  }
}
