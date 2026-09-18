package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext

/** Error built-in: Error and native error constructors.
  */
object ErrorBuiltins {
  import quickjs.objmodel.JSObject

  def initialize(ctx: JSContext): Unit = {
    def defineCause(obj: JSObject, args: Array[JSValue], index: Int)(using
        JSContext
    ): Unit =
      args.lift(index) match {
        case Some(JSValue.Object(options)) if options.hasProperty("cause") =>
          obj.defineProperty(
            "cause",
            options.get("cause"),
            enumerable = false,
            writable = true,
            configurable = true
          )(using ctx)
        case _ => ()
      }

    def setupErrorObject(
        obj: JSObject,
        name: String,
        args: Array[JSValue],
        messageIndex: Int
    )(using JSContext): JSValue = {
      obj.markErrorData()
      obj.defineProperty(
        "name",
        JSValue.fromString(name),
        enumerable = false,
        writable = true,
        configurable = true
      )(using ctx)
      args.lift(messageIndex) match {
        case Some(value) if value != JSValue.Undefined =>
          obj.defineProperty(
            "message",
            JSValue.fromString(BuiltinHelpers.toJSString(value)),
            enumerable = false,
            writable = true,
            configurable = true
          )(using ctx)
        case _ => ()
      }
      defineCause(obj, args, messageIndex + 1)
      ctx.attachStack(obj, skipFrames = 1)
      JSValue.Object(obj)
    }

    def buildError(
        proto: JSObject,
        name: String,
        args: Array[JSValue],
        messageIndex: Int = 0
    )(using
        JSContext
    ): JSValue = {
      val obj = JSObject(prototype = proto, extensible = true)
      setupErrorObject(obj, name, args, messageIndex)
    }

    def arrayFrom(value: JSValue)(using JSContext): JSValue =
      ctx.global.get("Array") match {
        case JSValue.Native(arrayCtor: NativeConstructor) =>
          arrayCtor.funcObj.get("from") match {
            case from if from != JSValue.Undefined =>
              BuiltinHelpers.callFunctionWithThis(
                from,
                JSValue.Native(arrayCtor),
                Array(value)
              )
            case _ =>
              ctx.throwTypeError("Array.from is not available")
          }
        case _ =>
          ctx.throwTypeError("Array constructor is not available")
      }

    def buildAggregateError(proto: JSObject, args: Array[JSValue])(using
        JSContext
    ): JSValue = {
      val errorList = arrayFrom(args.headOption.getOrElse(JSValue.Undefined))
      buildError(proto, "AggregateError", args, messageIndex = 1) match {
        case JSValue.Object(obj) =>
          obj.set("errors", errorList)
          JSValue.Object(obj)
        case other => other
      }
    }

    def defineNativeError(
        name: String,
        parentPrototype: JSObject,
        length: Int = 1,
        builder: Option[(JSObject, Array[JSValue], JSContext) => JSValue] =
          None
    )(using JSContext): JSObject = {
      val prototype =
        JSObject(prototype = parentPrototype, extensible = true)
      prototype.set("name", JSValue.fromString(name))
      val errorBuilder =
        builder.getOrElse { (proto, args, callCtx) =>
          given JSContext = callCtx
          buildError(proto, name, args)
        }
      val constructor = NativeConstructor(
        name = name,
        callImpl = (args, callCtx) => errorBuilder(prototype, args, callCtx),
        constructImpl = (args, callCtx) =>
          errorBuilder(prototype, args, callCtx),
        prototype = prototype,
        superInitImpl = Some((thisValue, args, callCtx) => {
          given JSContext = callCtx
          thisValue match {
            case JSValue.Object(obj) =>
              if name == "AggregateError" then {
                val errorList =
                  arrayFrom(args.headOption.getOrElse(JSValue.Undefined))
                setupErrorObject(obj, name, args, 1)
                obj.set("errors", errorList)
                JSValue.Object(obj)
              } else setupErrorObject(obj, name, args, 0)
            case _ =>
              callCtx.throwTypeError(s"Constructor $name requires 'new'")
          }
        })
      )
      BuiltinHelpers.initConstructor(constructor, length = length)
      prototype.defineProperty(
        "constructor",
        JSValue.Native(constructor),
        enumerable = false
      )(using ctx)
      ctx.global.set(name, JSValue.Native(constructor))
      prototype
    }

    given JSContext = ctx

    val errorPrototype =
      JSObject(prototype = ctx.objectPrototype, extensible = true)
    errorPrototype.set("name", JSValue.fromString("Error"))
    val errorConstructor = quickjs.value.NativeConstructor(
      name = "Error",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(errorPrototype, "Error", args)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(errorPrototype, "Error", args)
      ,
      prototype = errorPrototype,
      superInitImpl = Some((thisValue, args, initCtx) => {
        given JSContext = initCtx
        thisValue match {
          case JSValue.Object(obj) => setupErrorObject(obj, "Error", args, 0)
          case _ => initCtx.throwTypeError("Constructor Error requires 'new'")
        }
      })
    )
    BuiltinHelpers.initConstructor(errorConstructor, length = 1)
    errorPrototype.defineProperty(
      "constructor",
      JSValue.Native(errorConstructor),
      enumerable = false
    )(using ctx)
    ctx.global.set("Error", JSValue.Native(errorConstructor))

    // Error.isError(value) - checks the [[ErrorData]] slot.
    val errorIsError = NativeFunction(
      name = "isError",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        JSValue.Bool(
          args.lift(1).exists {
            case JSValue.Object(obj) => obj.hasErrorData
            case _                   => false
          }
        )
    )
    errorConstructor.funcObj.defineProperty(
      "isError",
      JSValue.Native(errorIsError),
      enumerable = false,
      writable = true,
      configurable = true
    )

    defineNativeError("EvalError", errorPrototype)
    defineNativeError("RangeError", errorPrototype)
    defineNativeError("ReferenceError", errorPrototype)
    defineNativeError("SyntaxError", errorPrototype)
    defineNativeError("TypeError", errorPrototype)
    defineNativeError("URIError", errorPrototype)
    defineNativeError(
      "AggregateError",
      errorPrototype,
      length = 2,
      builder = Some((proto, args, callCtx) =>
        given JSContext = callCtx
        buildAggregateError(proto, args)
      )
    )

    val errorPrototypeToString = NativeFunction(
      name = "toString",
      length = 0,
      impl = (args, ctx) =>
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        thisValue match {
          case JSValue.Object(obj) =>
            val nameValue = obj.get("name")(using ctx)
            val nameStr =
              if nameValue == JSValue.Undefined then "Error"
              else nameValue.toString
            val msgValue = obj.get("message")(using ctx)
            val msgStr =
              if msgValue == JSValue.Undefined then "" else msgValue.toString
            if nameStr.nonEmpty && msgStr.nonEmpty then
              JSValue.fromString(s"$nameStr: $msgStr")
            else if nameStr.nonEmpty then JSValue.fromString(nameStr)
            else JSValue.fromString(msgStr)
          case _ =>
            ctx.throwTypeError("Error.prototype.toString called on non-object")
        }
    )
    errorPrototype.defineProperty(
      "toString",
      JSValue.Native(errorPrototypeToString),
      enumerable = false
    )(using ctx)

    // =========================================================================
    // V8 stack introspection: stackTraceLimit, prepareStackTrace, CallSite
    // =========================================================================
    errorConstructor.funcObj.defineProperty(
      "stackTraceLimit",
      JSValue.fromInt(10),
      enumerable = false,
      writable = true,
      configurable = true
    )(using ctx)
    errorConstructor.funcObj.defineProperty(
      "prepareStackTrace",
      JSValue.Undefined,
      enumerable = false,
      writable = true,
      configurable = true
    )(using ctx)

    val callSiteProto =
      JSObject(prototype = ctx.objectPrototype, extensible = true)

    def callSiteField(site: JSObject, name: String): JSValue =
      site.getOwnPropertyRaw(name).getOrElse(JSValue.Undefined)

    def callSiteString(site: JSObject, name: String): String =
      callSiteField(site, name) match {
        case JSValue.JSStr(s) => s
        case _                => ""
      }

    def callSiteInt(site: JSObject, name: String): Int =
      callSiteField(site, name) match {
        case JSValue.Int32(n)   => n
        case JSValue.Float64(d) => d.toInt
        case _                  => -1
      }

    def defineCallSiteMethod(name: String, arity: Int)(
        impl: (JSObject, Array[JSValue], JSContext) => JSValue
    ): Unit = {
      val fn = NativeFunction(
        name = name,
        length = arity,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val site = args.headOption match {
            case Some(JSValue.Object(obj)) => obj
            case _ =>
              callCtx.throwTypeError(
                s"CallSite.prototype.$name called on non-object"
              )
          }
          impl(site, args, callCtx)
        }
      )
      callSiteProto.defineProperty(
        name,
        JSValue.Native(fn),
        enumerable = false
      )(using ctx)
    }

    defineCallSiteMethod("getThis", 0)((_, _, _) => JSValue.Undefined)
    defineCallSiteMethod("getTypeName", 0)((_, _, _) => JSValue.fromString(""))
    defineCallSiteMethod("getFunction", 0)((_, _, _) => JSValue.Undefined)
    defineCallSiteMethod("getFunctionName", 0) { (site, _, _) =>
      val name = callSiteString(site, "__csName")
      if name.isEmpty || name == "<anonymous>" then JSValue.Null
      else JSValue.fromString(name)
    }
    defineCallSiteMethod("getMethodName", 0)((_, _, _) => JSValue.Null)
    defineCallSiteMethod("getFileName", 0) { (site, _, _) =>
      val source = callSiteString(site, "__csSource")
      if source.isEmpty then JSValue.Null else JSValue.fromString(source)
    }
    defineCallSiteMethod("getScriptNameOrSourceURL", 0) { (site, _, _) =>
      val source = callSiteString(site, "__csSource")
      if source.isEmpty then JSValue.Null else JSValue.fromString(source)
    }
    defineCallSiteMethod("getLineNumber", 0) { (site, _, _) =>
      val line = callSiteInt(site, "__csLine")
      if line >= 0 then JSValue.fromInt(line) else JSValue.Null
    }
    defineCallSiteMethod("getColumnNumber", 0) { (site, _, _) =>
      val col = callSiteInt(site, "__csCol")
      if col >= 0 then JSValue.fromInt(col) else JSValue.Null
    }
    defineCallSiteMethod("getEvalOrigin", 0)((_, _, _) => JSValue.Undefined)
    defineCallSiteMethod("isToplevel", 0)((_, _, _) => JSValue.Bool(false))
    defineCallSiteMethod("isEval", 0)((_, _, _) => JSValue.Bool(false))
    defineCallSiteMethod("isNative", 0) { (site, _, _) =>
      callSiteField(site, "__csNative")
    }
    defineCallSiteMethod("isConstructor", 0)((_, _, _) => JSValue.Bool(false))
    defineCallSiteMethod("isAsync", 0)((_, _, _) => JSValue.Bool(false))
    defineCallSiteMethod("isPromiseAll", 0)((_, _, _) => JSValue.Bool(false))
    defineCallSiteMethod("getPromiseIndex", 0)((_, _, _) => JSValue.Null)
    defineCallSiteMethod("getPosition", 0)((_, _, _) => JSValue.fromInt(0))
    defineCallSiteMethod("getEnclosingLineNumber", 0)((_, _, _) =>
      JSValue.fromInt(0)
    )
    defineCallSiteMethod("getEnclosingColumnNumber", 0)((_, _, _) =>
      JSValue.fromInt(0)
    )
    defineCallSiteMethod("toString", 0) { (site, _, _) =>
      val name = callSiteString(site, "__csName")
      val source = callSiteString(site, "__csSource")
      val line = callSiteInt(site, "__csLine")
      val col = callSiteInt(site, "__csCol")
      val native = callSiteField(site, "__csNative") == JSValue.Bool(true)
      val location =
        if native then "native"
        else if line >= 0 then s"$source:$line:${Math.max(1, col)}"
        else if source.nonEmpty then source
        else if name.nonEmpty then name
        else "<anonymous>"
      val isAnonymous = name.isEmpty || name == "<anonymous>"
      JSValue.fromString(
        if isAnonymous then location else s"$name ($location)"
      )
    }

    def makeCallSite(frame: JSContext.CapturedFrame): JSValue = {
      val site = JSObject(prototype = callSiteProto, extensible = true)
      site.defineProperty(
        "__csName",
        JSValue.fromString(frame.name),
        enumerable = false,
        configurable = true
      )(using ctx)
      site.defineProperty(
        "__csSource",
        JSValue.fromString(frame.source),
        enumerable = false,
        configurable = true
      )(using ctx)
      site.defineProperty(
        "__csLine",
        JSValue.fromInt(frame.line),
        enumerable = false,
        configurable = true
      )(using ctx)
      site.defineProperty(
        "__csCol",
        JSValue.fromInt(frame.col),
        enumerable = false,
        configurable = true
      )(using ctx)
      site.defineProperty(
        "__csNative",
        JSValue.Bool(frame.isNative),
        enumerable = false,
        configurable = true
      )(using ctx)
      JSValue.Object(site)
    }

    ctx.setStackGetterFactory { (obj, frames) =>
      JSValue.Native(NativeFunction(
        name = "get stack",
        length = 0,
        impl = (_, callCtx) => {
          given JSContext = callCtx
          callCtx.stackValueFor(obj, frames)
        }
      ))
    }

    ctx.setStackFormatter { (errorObj, frames) =>
      given JSContext = ctx
      val prepare = errorConstructor.funcObj.get("prepareStackTrace")
      if BuiltinHelpers.isCallable(prepare) then {
        val sites = quickjs.objmodel.JSArray.empty()
        frames.foreach(frame => sites.push(makeCallSite(frame)))
        BuiltinHelpers.callFunctionWithThis(
          prepare,
          JSValue.Undefined,
          Array(JSValue.Object(errorObj), JSValue.JSArrayVal(sites))
        )
      } else JSValue.fromString(ctx.formatFrames(frames))
    }

    val captureStackTraceFn = NativeFunction(
      name = "captureStackTrace",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        // Method call `Error.captureStackTrace(...)` prepends the Error
        // constructor; extracted calls pass the target first.
        val rest = args.headOption match {
          case Some(JSValue.Native(nc: NativeConstructor))
              if nc.asInstanceOf[AnyRef] eq errorConstructor.asInstanceOf[AnyRef] =>
            args.drop(1)
          case _ => args
        }
        rest.headOption match {
          case Some(JSValue.Object(target)) =>
            var frames = callCtx.captureFrames(skipFrames = 1)
            rest.lift(1) match {
              case Some(value) if BuiltinHelpers.isCallable(value) =>
                val ctorName = value match {
                  case f: JSValue.Function                  => f.name
                  case JSValue.Native(nf: NativeFunction)   => nf.name
                  case JSValue.Native(nc: NativeConstructor) => nc.name
                  case _                                    => ""
                }
                val index = frames.indexWhere(_.name == ctorName)
                if index >= 0 then frames = frames.drop(index + 1)
              case _ => ()
            }
            callCtx.installLazyStack(target, frames)
            JSValue.Undefined
          case _ =>
            callCtx.throwTypeError(
              "Error.captureStackTrace called on non-object"
            )
        }
      }
    )
    errorConstructor.funcObj.defineProperty(
      "captureStackTrace",
      JSValue.Native(captureStackTraceFn),
      enumerable = false,
      writable = true,
      configurable = true
    )(using ctx)
  }
}
