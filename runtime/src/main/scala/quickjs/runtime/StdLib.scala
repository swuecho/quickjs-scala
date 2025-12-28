package quickjs.runtime

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import scala.collection.mutable
import java.math.{BigDecimal, BigInteger, MathContext, RoundingMode}
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale

/** Standard library initialization.
  *
  * Initializes built-in methods like Function.prototype.call, etc.
  * This is in a separate module to avoid circular dependencies between core and runtime.
  */
object StdLib:
  private def initConstructor(
    constructor: quickjs.value.NativeConstructor,
    length: Int
  )(using ctx: JSContext): Unit =
    constructor.funcObj.setPrototype(ctx.functionPrototype)
    constructor.funcObj.defineProperty("prototype", JSValue.Object(constructor.prototype), enumerable = false)
    constructor.funcObj.defineProperty("length", JSValue.fromInt(length), enumerable = false)
    constructor.funcObj.defineProperty("name", JSValue.fromString(constructor.name), enumerable = false)

  private def callFunctionWithThis(
    funcValue: JSValue,
    thisValue: JSValue,
    args: Array[JSValue]
  )(using ctx: JSContext): JSValue =
    funcValue match
      case func: JSValue.Function =>
        val bcFunc = new BytecodeFunction(
          name = func.name,
          bytecode = func.bytecode,
          constants = func.constants,
          stackSize = func.stackSize,
          freeVars = Array.empty,
          paramNames = func.paramNames,
          localVarNames = func.localVarNames,
          argumentsIndex = func.argumentsIndex,
          isConstructor = func.isConstructor
        )
        val interpreter = Interpreter()
        interpreter.call(bcFunc, thisValue, args, func.closure)
      case JSValue.Native(nativeFuncWrapper) =>
        nativeFuncWrapper match
          case native: NativeFunction =>
            val argsWithThis = new Array[JSValue](args.length + 1)
            argsWithThis(0) = thisValue
            Array.copy(args, 0, argsWithThis, 1, args.length)
            ctx.withStackFrame(native.name, isNative = true) {
              native.call(argsWithThis)
            }
          case constructor: quickjs.value.NativeConstructor =>
            ctx.withStackFrame(constructor.name, isNative = true) {
              constructor.call(args)(using ctx)
            }
          case _ =>
            throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
      case _ =>
        throw new RuntimeException(s"Cannot call non-function value: $funcValue")

  private def initializeForInHelpers(ctx: JSContext): Unit =
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

  private def initializeModuleHelpers(ctx: JSContext): Unit =
    val moduleImport = NativeFunction(
      name = "__moduleImport",
      impl = (args, context) =>
        given JSContext = context
        val name = args.headOption match
          case Some(JSValue.JSStr(s)) => s
          case Some(other) => other.toString
          case None => ""
        val exportsObj = context.rt.ensureModuleExports(name)
        JSValue.Object(exportsObj)
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
        val sourceName = args.drop(1).headOption match
          case Some(JSValue.JSStr(s)) => s
          case Some(other) => other.toString
          case None => ""
        val exportsObj = context.rt.ensureModuleExports(moduleName)
        val sourceObj = context.rt.ensureModuleExports(sourceName)
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

  private def initializeArrayHelpers(ctx: JSContext): Unit =
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
            case _ => JSValue.Undefined
    )

    given JSContext = ctx
    ctx.globalScope.setVariable("__arrayPush", JSValue.Native(arrayPush))
    ctx.globalScope.setVariable("__arraySpread", JSValue.Native(arraySpread))

  private def initializeObjectStatics(ctx: JSContext): Unit =
    val setPrototypeOf = NativeFunction(
      name = "setPrototypeOf",
      impl = (args, ctx) =>
        if args.length < 2 then
          JSValue.Undefined
        else
          val offset = if args.length >= 3 then 1 else 0
          val target = args(offset)
          val proto = args(offset + 1)
          (target, proto) match
            case (JSValue.Object(obj), JSValue.Object(protoObj)) =>
              obj.setPrototype(protoObj)
              target
            case (JSValue.Object(obj), JSValue.Null) =>
              obj.setPrototype(null)
              target
            case (func: JSValue.Function, JSValue.Object(protoObj)) =>
              func.funcObj.setPrototype(protoObj)
              target
            case (func: JSValue.Function, superFunc: JSValue.Function) =>
              func.funcObj.setPrototype(superFunc.funcObj)
              target
            case (func: JSValue.Function, JSValue.Null) =>
              func.funcObj.setPrototype(null)
              target
            case (_: JSValue.Object | _: JSValue.Function, _) =>
              ctx.throwTypeError("Object.setPrototypeOf called with invalid prototype")
            case _ =>
              ctx.throwTypeError("Object.setPrototypeOf called on non-object")
    )

    val defineProperty = NativeFunction(
      name = "defineProperty",
      impl = (args, ctx) =>
        if args.length < 3 then
          JSValue.Undefined
        else
          val offset = if args.length >= 4 then 1 else 0
          val target = args(offset)
          val propKey = args(offset + 1).toString
          val descriptor = args(offset + 2)
          def applyDefine(obj: quickjs.objmodel.JSObject): JSValue =
            val existingDesc = obj.getOwnPropertyDescriptor(propKey)(using ctx)
            val (enumerableOpt, writableOpt, configurableOpt, getterOpt, setterOpt, valueOpt, hasWritableProp) =
              descriptor match
                case JSValue.Object(descObj) =>
                  val enumerableOpt =
                    descObj.getOwnProperty("enumerable")(using ctx) match
                      case Some(JSValue.Bool(b)) => Some(b)
                      case Some(_) => Some(false)
                      case None => None
                  val writableOpt =
                    descObj.getOwnProperty("writable")(using ctx) match
                      case Some(JSValue.Bool(b)) => Some(b)
                      case Some(_) => Some(false)
                      case None => None
                  val configurableOpt =
                    descObj.getOwnProperty("configurable")(using ctx) match
                      case Some(JSValue.Bool(b)) => Some(b)
                      case Some(_) => Some(false)
                      case None => None
                  val getterOpt =
                    descObj.getOwnProperty("get")(using ctx) match
                      case Some(JSValue.Undefined) | None => None
                      case Some(v) => Some(v)
                  val setterOpt =
                    descObj.getOwnProperty("set")(using ctx) match
                      case Some(JSValue.Undefined) | None => None
                      case Some(v) => Some(v)
                  val valueOpt = descObj.getOwnProperty("value")(using ctx)
                  val hasWritableProp = descObj.getOwnProperty("writable")(using ctx).isDefined
                  (enumerableOpt, writableOpt, configurableOpt, getterOpt, setterOpt, valueOpt, hasWritableProp)
                case _ =>
                  (None, None, None, None, None, None, false)

            val hasAccessor = getterOpt.isDefined || setterOpt.isDefined
            val hasValue = valueOpt.isDefined || hasWritableProp
            if hasAccessor && hasValue then
              ctx.throwTypeError("Invalid property descriptor. Cannot have both accessors and a value")

            val enumerable = enumerableOpt.getOrElse(existingDesc.map(_._2.enumerable).getOrElse(false))
            val writable = writableOpt.getOrElse(existingDesc.map(_._2.writable).getOrElse(false))
            val configurable = configurableOpt.getOrElse(existingDesc.map(_._2.configurable).getOrElse(false))
            val value = valueOpt.getOrElse(obj.get(propKey)(using ctx))
            val ok =
              if hasAccessor then
                val getter = getterOpt.orElse(existingDesc.flatMap(_._2.getter))
                val setter = setterOpt.orElse(existingDesc.flatMap(_._2.setter))
                obj.defineAccessorProperty(propKey, getter, setter, enumerable, configurable)(using ctx)
              else
                obj.defineProperty(propKey, value, enumerable, writable, configurable)(using ctx)
            if !ok then
              ctx.throwTypeError("Cannot define property")
            target

          target match
            case JSValue.Object(obj) => applyDefine(obj)
            case func: JSValue.Function => applyDefine(func.funcObj)
            case _ => target
    )

    val objectIs = NativeFunction(
      name = "is",
      impl = (args, ctx) =>
        if args.length < 2 then
          JSValue.Bool(false)
        else
          val offset = if args.length >= 3 then 1 else 0
          val a = args(offset)
          val b = args(offset + 1)

          def sameValue(x: JSValue, y: JSValue): Boolean = (x, y) match
            case (JSValue.Float64(dx), JSValue.Float64(dy)) =>
              if java.lang.Double.isNaN(dx) && java.lang.Double.isNaN(dy) then
                true
              else
                java.lang.Double.doubleToRawLongBits(dx) == java.lang.Double.doubleToRawLongBits(dy)
            case (JSValue.Int32(ix), JSValue.Int32(iy)) =>
              ix == iy
            case (JSValue.Int32(ix), JSValue.Float64(dy)) =>
              if java.lang.Double.isNaN(dy) then
                false
              else if ix == 0 && java.lang.Double.doubleToRawLongBits(dy) == java.lang.Double.doubleToRawLongBits(-0.0) then
                false
              else
                ix.toDouble == dy
            case (JSValue.Float64(dx), JSValue.Int32(iy)) =>
              if java.lang.Double.isNaN(dx) then
                false
              else if iy == 0 && java.lang.Double.doubleToRawLongBits(dx) == java.lang.Double.doubleToRawLongBits(-0.0) then
                false
              else
                dx == iy.toDouble
            case (JSValue.BigInt(bx), JSValue.BigInt(by)) =>
              bx == by
            case _ =>
              x == y

          JSValue.Bool(sameValue(a, b))
    )

    val objectGetOwnPropertyNames = NativeFunction(
      name = "getOwnPropertyNames",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else
          val offset = if args.length >= 2 then 1 else 0
          args(offset) match
            case JSValue.Object(obj) =>
              val result = quickjs.objmodel.JSArray.empty()
              obj.getAllProperties.keys.foreach { key =>
                result.push(JSValue.fromString(key))
              }
              JSValue.JSArrayVal(result)
            case func: JSValue.Function =>
              val result = quickjs.objmodel.JSArray.empty()
              func.funcObj.getAllProperties.keys.foreach { key =>
                result.push(JSValue.fromString(key))
              }
              JSValue.JSArrayVal(result)
            case JSValue.JSArrayVal(arr) =>
              val result = quickjs.objmodel.JSArray.empty()
              var i = 0
              while i < arr.getLength do
                result.push(JSValue.fromString(i.toString))
                i += 1
              result.push(JSValue.fromString("length"))
              JSValue.JSArrayVal(result)
            case _ =>
              JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
    )

    val objectGetOwnPropertyDescriptor = NativeFunction(
      name = "getOwnPropertyDescriptor",
      impl = (args, ctx) =>
        if args.length < 2 then
          JSValue.Undefined
        else
          val offset = if args.length >= 3 then 1 else 0
          val propKey = args(offset + 1).toString
          def buildDescriptor(
            desc: Option[(JSValue, quickjs.objmodel.JSObject.PropertyAttributes)]
          ): JSValue =
            desc match
              case Some((value, attrs)) =>
                val descObj = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
                if attrs.getter.isDefined || attrs.setter.isDefined then
                  attrs.getter.foreach(v => descObj.set("get", v)(using ctx))
                  attrs.setter.foreach(v => descObj.set("set", v)(using ctx))
                else
                  descObj.set("value", value)(using ctx)
                  descObj.set("writable", JSValue.fromBoolean(attrs.writable))(using ctx)
                descObj.set("enumerable", JSValue.fromBoolean(attrs.enumerable))(using ctx)
                descObj.set("configurable", JSValue.fromBoolean(attrs.configurable))(using ctx)
                JSValue.Object(descObj)
              case None =>
                JSValue.Undefined

          args(offset) match
            case JSValue.Object(obj) =>
              buildDescriptor(obj.getOwnPropertyDescriptor(propKey)(using ctx))
            case func: JSValue.Function =>
              buildDescriptor(func.funcObj.getOwnPropertyDescriptor(propKey)(using ctx))
            case _ =>
              JSValue.Undefined
    )

    val objectKeys = NativeFunction(
      name = "keys",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else
          val offset = if args.length >= 2 then 1 else 0
          args(offset) match
            case JSValue.Object(obj) =>
              val result = quickjs.objmodel.JSArray.empty()
              obj.getOwnPropertyKeys().foreach { key =>
                result.push(JSValue.fromString(key))
              }
              JSValue.JSArrayVal(result)
            case func: JSValue.Function =>
              val result = quickjs.objmodel.JSArray.empty()
              func.funcObj.getOwnPropertyKeys().foreach { key =>
                result.push(JSValue.fromString(key))
              }
              JSValue.JSArrayVal(result)
            case JSValue.JSArrayVal(arr) =>
              val result = quickjs.objmodel.JSArray.empty()
              var i = 0
              while i < arr.getLength do
                result.push(JSValue.fromString(i.toString))
                i += 1
              JSValue.JSArrayVal(result)
            case _ =>
              JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
    )

    val objectGetPrototypeOf = NativeFunction(
      name = "getPrototypeOf",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.Undefined
        else
          val offset = if args.length >= 2 then 1 else 0
          args(offset) match
            case JSValue.Object(obj) =>
              val proto = obj.getPrototype
              if proto == null then JSValue.Null else JSValue.Object(proto)
            case func: JSValue.Function =>
              val proto = func.funcObj.getPrototype
              if proto == null then JSValue.Null else JSValue.Object(proto)
            case JSValue.JSArrayVal(_) =>
              JSValue.Object(ctx.arrayPrototype)
            case _ =>
              JSValue.Undefined
    )

    val objectAssign = NativeFunction(
      name = "assign",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.Undefined
        else
          val offset = if args.length >= 2 then 1 else 0
          val target = args(offset)

          def setTargetProp(key: String, value: JSValue): Unit =
            target match
              case JSValue.Object(obj) =>
                obj.set(key, value)(using ctx)
              case func: JSValue.Function =>
                func.funcObj.set(key, value)(using ctx)
              case JSValue.JSArrayVal(arr) =>
                if key.forall(_.isDigit) then
                  arr.set(key.toInt, value)
                else
                  arr.setProperty(key, value)
              case _ => ()

          for i <- (offset + 1) until args.length do
            args(i) match
              case JSValue.Object(obj) =>
                obj.getOwnPropertyKeys().foreach { key =>
                  setTargetProp(key, obj.get(key)(using ctx))
                }
              case func: JSValue.Function =>
                func.funcObj.getOwnPropertyKeys().foreach { key =>
                  setTargetProp(key, func.funcObj.get(key)(using ctx))
                }
              case JSValue.JSArrayVal(arr) =>
                var idx = 0
                while idx < arr.getLength do
                  setTargetProp(idx.toString, arr.get(idx))
                  idx += 1
              case _ => ()

          target
    )

    val objectCreate = NativeFunction(
      name = "create",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        val proto = if args.length > offset then args(offset) else JSValue.Null
        val obj =
          proto match
            case JSValue.Null =>
              quickjs.objmodel.JSObject(prototype = null, extensible = true)
            case JSValue.Object(p) =>
              quickjs.objmodel.JSObject(prototype = p, extensible = true)
            case func: JSValue.Function =>
              quickjs.objmodel.JSObject(prototype = func.funcObj, extensible = true)
            case _ =>
              ctx.throwTypeError("Object.create called with invalid prototype")
        JSValue.Object(obj)
    )

    val objectValues = NativeFunction(
      name = "values",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else
          val offset = if args.length >= 2 then 1 else 0
          val result = quickjs.objmodel.JSArray.empty()
          args(offset) match
            case JSValue.Object(obj) =>
              obj.getOwnPropertyKeys().foreach { key =>
                result.push(obj.get(key)(using ctx))
              }
            case func: JSValue.Function =>
              func.funcObj.getOwnPropertyKeys().foreach { key =>
                result.push(func.funcObj.get(key)(using ctx))
              }
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do
                result.push(arr.get(i))
                i += 1
            case _ => ()
          JSValue.JSArrayVal(result)
    )

    val objectEntries = NativeFunction(
      name = "entries",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else
          val offset = if args.length >= 2 then 1 else 0
          val result = quickjs.objmodel.JSArray.empty()
          def pushEntry(key: String, value: JSValue): Unit =
            val pair = quickjs.objmodel.JSArray.empty()
            pair.push(JSValue.fromString(key))
            pair.push(value)
            result.push(JSValue.JSArrayVal(pair))

          args(offset) match
            case JSValue.Object(obj) =>
              obj.getOwnPropertyKeys().foreach { key =>
                pushEntry(key, obj.get(key)(using ctx))
              }
            case func: JSValue.Function =>
              func.funcObj.getOwnPropertyKeys().foreach { key =>
                pushEntry(key, func.funcObj.get(key)(using ctx))
              }
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do
                pushEntry(i.toString, arr.get(i))
                i += 1
            case _ => ()
          JSValue.JSArrayVal(result)
    )

    val objectPrototypeToString = NativeFunction(
      name = "toString",
      impl = (_, _) => JSValue.fromString("[object Object]")
    )

    given JSContext = ctx
    val objectConstructorOpt =
      ctx.global.get("Object") match
        case JSValue.Native(cons: quickjs.value.NativeConstructor) => Some(cons)
        case _ => None

    objectConstructorOpt.foreach { cons =>
      cons.funcObj.set("setPrototypeOf", JSValue.Native(setPrototypeOf))
      cons.funcObj.set("defineProperty", JSValue.Native(defineProperty))
      cons.funcObj.set("is", JSValue.Native(objectIs))
      cons.funcObj.set("getPrototypeOf", JSValue.Native(objectGetPrototypeOf))
      cons.funcObj.set("getOwnPropertyDescriptor", JSValue.Native(objectGetOwnPropertyDescriptor))
      cons.funcObj.set("getOwnPropertyNames", JSValue.Native(objectGetOwnPropertyNames))
      cons.funcObj.set("keys", JSValue.Native(objectKeys))
      cons.funcObj.set("assign", JSValue.Native(objectAssign))
      cons.funcObj.set("create", JSValue.Native(objectCreate))
      cons.funcObj.set("values", JSValue.Native(objectValues))
      cons.funcObj.set("entries", JSValue.Native(objectEntries))
    }
    ctx.objectPrototype.defineProperty("toString", JSValue.Native(objectPrototypeToString), enumerable = false)

  private def initializeNumberString(ctx: JSContext): Unit =
    val numberPrototype = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
    val stringPrototype = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
    val booleanPrototype = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)

    val numberConstructor = quickjs.value.NativeConstructor(
      name = "Number",
      callImpl = (args, _) =>
        if args.isEmpty then JSValue.fromInt(0)
        else JSValue.fromDouble(args(0).toNumber),
      constructImpl = (args, _) =>
        if args.isEmpty then JSValue.fromInt(0)
        else JSValue.fromDouble(args(0).toNumber),
      prototype = numberPrototype
    )

    val stringConstructor = quickjs.value.NativeConstructor(
      name = "String",
      callImpl = (args, _) =>
        if args.isEmpty then JSValue.fromString("")
        else JSValue.fromString(args(0).toString),
      constructImpl = (args, _) =>
        if args.isEmpty then JSValue.fromString("")
        else JSValue.fromString(args(0).toString),
      prototype = stringPrototype
    )

    val booleanConstructor = quickjs.value.NativeConstructor(
      name = "Boolean",
      callImpl = (args, _) =>
        if args.isEmpty then JSValue.fromBoolean(false)
        else JSValue.fromBoolean(args(0).toBoolean),
      constructImpl = (args, _) =>
        if args.isEmpty then JSValue.fromBoolean(false)
        else JSValue.fromBoolean(args(0).toBoolean),
      prototype = booleanPrototype
    )

    given JSContext = ctx
    initConstructor(numberConstructor, length = 1)
    initConstructor(stringConstructor, length = 1)
    initConstructor(booleanConstructor, length = 1)
    ctx.global.set("Number", JSValue.Native(numberConstructor))
    ctx.global.set("String", JSValue.Native(stringConstructor))
    ctx.global.set("Boolean", JSValue.Native(booleanConstructor))

    def requireThisNumber(args: Array[JSValue], method: String)(using JSContext): Double =
      if args.isEmpty then
        ctx.throwTypeError(s"Number.prototype.$method called on null or undefined")
      else
        args(0) match
          case JSValue.Null | JSValue.Undefined =>
            ctx.throwTypeError(s"Number.prototype.$method called on null or undefined")
          case other => other.toNumber

    def requireThisBoolean(args: Array[JSValue], method: String)(using JSContext): Boolean =
      if args.isEmpty then
        ctx.throwTypeError(s"Boolean.prototype.$method called on null or undefined")
      else
        args(0) match
          case JSValue.Bool(b) => b
          case JSValue.Null | JSValue.Undefined =>
            ctx.throwTypeError(s"Boolean.prototype.$method called on null or undefined")
          case _ =>
            ctx.throwTypeError("not a boolean")

    def numberToString(value: Double, radix: Int): String =
      if value.isNaN || value.isInfinite then
        value.toString
      else if radix == 10 then
        value.toString.replace("E", "e")
      else
        val rounded = value.toLong
        if value == rounded.toDouble then
          java.lang.Long.toString(rounded, radix)
        else
          value.toString.replace("E", "e")

    def parseIntString(input: String, radixRaw: Int): Double =
      var s = input.dropWhile(_.isWhitespace)
      if s.isEmpty then
        Double.NaN
      else
        var sign = 1
        if s.head == '+' || s.head == '-' then
          if s.head == '-' then sign = -1
          s = s.tail
        var radix = radixRaw
        if radix == 0 then
          if s.startsWith("0x") || s.startsWith("0X") then
            radix = 16
            s = s.drop(2)
          else
            radix = 10
        else if radix == 16 && (s.startsWith("0x") || s.startsWith("0X")) then
          s = s.drop(2)
        if radix < 2 || radix > 36 then
          Double.NaN
        else
          var value = BigInteger.ZERO
          var digits = 0
          var i = 0
          var done = false
          while i < s.length && !done do
            val d = Character.digit(s.charAt(i), radix)
            if d < 0 then
              done = true
            else
              value = value.multiply(BigInteger.valueOf(radix.toLong)).add(BigInteger.valueOf(d.toLong))
              digits += 1
              i += 1
          if digits == 0 then
            Double.NaN
          else
            value.multiply(BigInteger.valueOf(sign.toLong)).doubleValue()

    def parseFloatString(input: String): Double =
      val trimmed = input.dropWhile(_.isWhitespace)
      if trimmed.startsWith("Infinity") then Double.PositiveInfinity
      else if trimmed.startsWith("+Infinity") then Double.PositiveInfinity
      else if trimmed.startsWith("-Infinity") then Double.NegativeInfinity
      else
        val pattern = """^[+-]?((\d+(\.\d*)?)|(\.\d+))([eE][+-]?\d+)?""".r
        pattern.findPrefixOf(trimmed) match
          case Some(prefix) =>
            try prefix.toDouble
            catch case _: NumberFormatException => Double.NaN
          case None => Double.NaN

    val numberIsNaN = NativeFunction(
      name = "isNaN",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        args.lift(offset) match
          case Some(JSValue.Float64(d)) => JSValue.fromBoolean(d.isNaN)
          case Some(JSValue.Int32(_)) => JSValue.fromBoolean(false)
          case _ => JSValue.fromBoolean(false)
    )

    val numberIsFinite = NativeFunction(
      name = "isFinite",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        args.lift(offset) match
          case Some(JSValue.Float64(d)) => JSValue.fromBoolean(java.lang.Double.isFinite(d))
          case Some(JSValue.Int32(_)) => JSValue.fromBoolean(true)
          case _ => JSValue.fromBoolean(false)
    )

    val numberIsInteger = NativeFunction(
      name = "isInteger",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        args.lift(offset) match
          case Some(JSValue.Int32(_)) => JSValue.fromBoolean(true)
          case Some(JSValue.Float64(d)) =>
            JSValue.fromBoolean(java.lang.Double.isFinite(d) && math.floor(d) == d)
          case _ => JSValue.fromBoolean(false)
    )

    val numberIsSafeInteger = NativeFunction(
      name = "isSafeInteger",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        val limit = 9007199254740991.0
        args.lift(offset) match
          case Some(JSValue.Int32(i)) => JSValue.fromBoolean(math.abs(i.toLong) <= limit)
          case Some(JSValue.Float64(d)) =>
            JSValue.fromBoolean(java.lang.Double.isFinite(d) && math.floor(d) == d && math.abs(d) <= limit)
          case _ => JSValue.fromBoolean(false)
    )

    val numberPrototypeToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "toString")
        val radix =
          if args.length > 1 then args(1).toNumber.toInt
          else 10
        if radix < 2 || radix > 36 then
          ctx.throwRangeError("radix must be between 2 and 36")
        JSValue.fromString(numberToString(value, radix))
    )

    val numberPrototypeToFixed = NativeFunction(
      name = "toFixed",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "toFixed")
        val digits = if args.length > 1 then args(1).toNumber.toInt else 0
        if digits < 0 || digits > 100 then
          ctx.throwRangeError("invalid number of digits")
        if value.isNaN || value.isInfinite then
          JSValue.fromString(value.toString)
        else
          val bd = BigDecimal.valueOf(value).setScale(digits, RoundingMode.HALF_UP)
          JSValue.fromString(bd.toPlainString)
    )

    val numberPrototypeToExponential = NativeFunction(
      name = "toExponential",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "toExponential")
        if value.isNaN || value.isInfinite then
          JSValue.fromString(value.toString)
        else
          val hasDigits = args.length > 1 && args(1) != JSValue.Undefined
          val digits = if hasDigits then args(1).toNumber.toInt else 0
          if hasDigits && (digits < 0 || digits > 100) then
            ctx.throwRangeError("invalid number of digits")
          if !hasDigits then
            JSValue.fromString(value.toString.replace("E", "e"))
          else
            val pattern = "0." + ("0" * digits) + "E0"
            val fmt = new DecimalFormat(pattern, new DecimalFormatSymbols(Locale.US))
            fmt.setRoundingMode(RoundingMode.HALF_UP)
            JSValue.fromString(fmt.format(value).replace("E", "e"))
    )

    val numberPrototypeToPrecision = NativeFunction(
      name = "toPrecision",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "toPrecision")
        if args.length < 2 || args(1) == JSValue.Undefined then
          JSValue.fromString(value.toString.replace("E", "e"))
        else if value.isNaN || value.isInfinite then
          JSValue.fromString(value.toString)
        else
          val precision = args(1).toNumber.toInt
          if precision < 1 || precision > 100 then
            ctx.throwRangeError("invalid number of digits")
          val mc = MathContext(precision, RoundingMode.HALF_UP)
          val bd = BigDecimal.valueOf(value).round(mc)
          JSValue.fromString(bd.toString.replace("E", "e"))
    )

    val numberPrototypeValueOf = NativeFunction(
      name = "valueOf",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "valueOf")
        JSValue.fromDouble(value)
    )

    val numberPrototypeToLocaleString = NativeFunction(
      name = "toLocaleString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisNumber(args, "toLocaleString")
        JSValue.fromString(value.toString.replace("E", "e"))
    )

    val booleanPrototypeToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisBoolean(args, "toString")
        JSValue.fromString(if value then "true" else "false")
    )

    val booleanPrototypeValueOf = NativeFunction(
      name = "valueOf",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = requireThisBoolean(args, "valueOf")
        JSValue.fromBoolean(value)
    )

    val parseIntFunc = NativeFunction(
      name = "parseInt",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        val input = if args.length > offset then args(offset).toString else ""
        val radix =
          if args.length > offset + 1 then args(offset + 1).toNumber.toInt
          else 0
        JSValue.fromDouble(parseIntString(input, radix))
    )

    val parseFloatFunc = NativeFunction(
      name = "parseFloat",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        val input = if args.length > offset then args(offset).toString else ""
        JSValue.fromDouble(parseFloatString(input))
    )

    numberConstructor.funcObj.defineProperty("MAX_VALUE", JSValue.fromDouble(1.7976931348623157e+308), enumerable = false)
    numberConstructor.funcObj.defineProperty("MIN_VALUE", JSValue.fromDouble(5e-324), enumerable = false)
    numberConstructor.funcObj.defineProperty("NaN", JSValue.Float64(Double.NaN), enumerable = false)
    numberConstructor.funcObj.defineProperty("NEGATIVE_INFINITY", JSValue.Float64(Double.NegativeInfinity), enumerable = false)
    numberConstructor.funcObj.defineProperty("POSITIVE_INFINITY", JSValue.Float64(Double.PositiveInfinity), enumerable = false)
    numberConstructor.funcObj.defineProperty("EPSILON", JSValue.fromDouble(2.220446049250313e-16), enumerable = false)
    numberConstructor.funcObj.defineProperty("MAX_SAFE_INTEGER", JSValue.fromDouble(9007199254740991.0), enumerable = false)
    numberConstructor.funcObj.defineProperty("MIN_SAFE_INTEGER", JSValue.fromDouble(-9007199254740991.0), enumerable = false)
    numberConstructor.funcObj.set("parseInt", JSValue.Native(parseIntFunc))
    numberConstructor.funcObj.set("parseFloat", JSValue.Native(parseFloatFunc))
    numberConstructor.funcObj.set("isNaN", JSValue.Native(numberIsNaN))
    numberConstructor.funcObj.set("isFinite", JSValue.Native(numberIsFinite))
    numberConstructor.funcObj.set("isInteger", JSValue.Native(numberIsInteger))
    numberConstructor.funcObj.set("isSafeInteger", JSValue.Native(numberIsSafeInteger))

    numberPrototype.defineProperty("toString", JSValue.Native(numberPrototypeToString), enumerable = false)
    numberPrototype.defineProperty("toFixed", JSValue.Native(numberPrototypeToFixed), enumerable = false)
    numberPrototype.defineProperty("toExponential", JSValue.Native(numberPrototypeToExponential), enumerable = false)
    numberPrototype.defineProperty("toPrecision", JSValue.Native(numberPrototypeToPrecision), enumerable = false)
    numberPrototype.defineProperty("valueOf", JSValue.Native(numberPrototypeValueOf), enumerable = false)
    numberPrototype.defineProperty("toLocaleString", JSValue.Native(numberPrototypeToLocaleString), enumerable = false)

    booleanPrototype.defineProperty("toString", JSValue.Native(booleanPrototypeToString), enumerable = false)
    booleanPrototype.defineProperty("valueOf", JSValue.Native(booleanPrototypeValueOf), enumerable = false)

    ctx.global.set("parseInt", JSValue.Native(parseIntFunc))
    ctx.global.set("parseFloat", JSValue.Native(parseFloatFunc))

    def requireThisString(args: Array[JSValue], method: String)(using JSContext): String =
      if args.isEmpty then
        ctx.throwTypeError(s"String.prototype.$method called on null or undefined")
      else
        args(0) match
          case JSValue.Null | JSValue.Undefined =>
            ctx.throwTypeError(s"String.prototype.$method called on null or undefined")
          case other =>
            other.toString

    val stringPrototypeSplit = NativeFunction(
      name = "split",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "split")
        val separator = if args.length > 1 then args(1) else JSValue.Undefined
        val limit =
          if args.length > 2 then math.max(0, args(2).toNumber.toInt)
          else Int.MaxValue
        val result = quickjs.objmodel.JSArray.empty()
        if limit == 0 then
          JSValue.JSArrayVal(result)
        else if separator == JSValue.Undefined then
          result.push(JSValue.fromString(str))
          JSValue.JSArrayVal(result)
        else
          val sepStr = separator.toString
          if sepStr.isEmpty then
            var i = 0
            while i < str.length && i < limit do
              result.push(JSValue.fromString(str.charAt(i).toString))
              i += 1
          else
            val parts = str.split(java.util.regex.Pattern.quote(sepStr), if limit == Int.MaxValue then 0 else limit)
            var i = 0
            while i < parts.length && i < limit do
              result.push(JSValue.fromString(parts(i)))
              i += 1
          JSValue.JSArrayVal(result)
    )

    val stringPrototypeReplace = NativeFunction(
      name = "replace",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "replace")
        if args.length < 2 then
          JSValue.fromString(str)
        else
          val search = args(1).toString
          val replacement = if args.length > 2 then args(2).toString else ""
          val idx = str.indexOf(search)
          if idx < 0 then
            JSValue.fromString(str)
          else
            val updated = str.substring(0, idx) + replacement + str.substring(idx + search.length)
            JSValue.fromString(updated)
    )

    val stringPrototypeMatch = NativeFunction(
      name = "match",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "match")
        val pattern = if args.length > 1 then args(1) else JSValue.Undefined
        if pattern == JSValue.Undefined then
          val arr = quickjs.objmodel.JSArray.empty()
          arr.push(JSValue.fromString(str))
          JSValue.JSArrayVal(arr)
        else
          val needle = pattern.toString
          val idx = str.indexOf(needle)
          if idx < 0 then
            JSValue.Null
          else
            val arr = quickjs.objmodel.JSArray.empty()
            arr.push(JSValue.fromString(needle))
            JSValue.JSArrayVal(arr)
    )

    val stringPrototypeIndexOf = NativeFunction(
      name = "indexOf",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "indexOf")
        val search =
          if args.length > 1 then args(1).toString else "undefined"
        val rawPos =
          if args.length > 2 then args(2).toNumber.toInt else 0
        val pos = math.min(math.max(rawPos, 0), str.length)
        JSValue.fromInt(str.indexOf(search, pos))
    )

    val stringPrototypeSlice = NativeFunction(
      name = "slice",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "slice")
        val len = str.length
        val startRaw = if args.length > 1 then args(1).toNumber.toInt else 0
        val endRaw = if args.length > 2 then args(2).toNumber.toInt else len
        def clampIndex(idx: Int): Int =
          if idx < 0 then math.max(len + idx, 0) else math.min(idx, len)
        val start = clampIndex(startRaw)
        val end = clampIndex(endRaw)
        if end <= start then JSValue.fromString("")
        else JSValue.fromString(str.substring(start, end))
    )

    val stringPrototypeStartsWith = NativeFunction(
      name = "startsWith",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "startsWith")
        val search = if args.length > 1 then args(1).toString else ""
        val position = if args.length > 2 then math.max(0, args(2).toNumber.toInt) else 0
        JSValue.fromBoolean(str.startsWith(search, position))
    )

    val stringPrototypeEndsWith = NativeFunction(
      name = "endsWith",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "endsWith")
        val search = if args.length > 1 then args(1).toString else ""
        val endPos =
          if args.length > 2 then args(2).toNumber.toInt
          else str.length
        val clamped = math.min(math.max(endPos, 0), str.length)
        JSValue.fromBoolean(str.substring(0, clamped).endsWith(search))
    )

    val stringPrototypePadStart = NativeFunction(
      name = "padStart",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "padStart")
        val targetLength = if args.length > 1 then args(1).toNumber.toInt else 0
        val padString =
          if args.length > 2 && args(2) != JSValue.Undefined then args(2).toString else " "
        if targetLength <= str.length || padString.isEmpty then
          JSValue.fromString(str)
        else
          val padNeeded = targetLength - str.length
          val repeatCount = (padNeeded + padString.length - 1) / padString.length
          val pad = padString.repeat(repeatCount).substring(0, padNeeded)
          JSValue.fromString(pad + str)
    )

    val stringPrototypePadEnd = NativeFunction(
      name = "padEnd",
      impl = (args, ctx) =>
        given JSContext = ctx
        val str = requireThisString(args, "padEnd")
        val targetLength = if args.length > 1 then args(1).toNumber.toInt else 0
        val padString =
          if args.length > 2 && args(2) != JSValue.Undefined then args(2).toString else " "
        if targetLength <= str.length || padString.isEmpty then
          JSValue.fromString(str)
        else
          val padNeeded = targetLength - str.length
          val repeatCount = (padNeeded + padString.length - 1) / padString.length
          val pad = padString.repeat(repeatCount).substring(0, padNeeded)
          JSValue.fromString(str + pad)
    )

    stringPrototype.defineProperty("split", JSValue.Native(stringPrototypeSplit), enumerable = false)
    stringPrototype.defineProperty("replace", JSValue.Native(stringPrototypeReplace), enumerable = false)
    stringPrototype.defineProperty("match", JSValue.Native(stringPrototypeMatch), enumerable = false)
    stringPrototype.defineProperty("indexOf", JSValue.Native(stringPrototypeIndexOf), enumerable = false)
    stringPrototype.defineProperty("slice", JSValue.Native(stringPrototypeSlice), enumerable = false)
    stringPrototype.defineProperty("startsWith", JSValue.Native(stringPrototypeStartsWith), enumerable = false)
    stringPrototype.defineProperty("endsWith", JSValue.Native(stringPrototypeEndsWith), enumerable = false)
    stringPrototype.defineProperty("padStart", JSValue.Native(stringPrototypePadStart), enumerable = false)
    stringPrototype.defineProperty("padEnd", JSValue.Native(stringPrototypePadEnd), enumerable = false)

    val stringRaw = NativeFunction(
      name = "raw",
      impl = (args, _) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then
          JSValue.fromString("")
        else
          JSValue.fromString(args(offset).toString)
    )
    ctx.functionPrototype.set("raw", JSValue.Native(stringRaw))

  private def initializeProxy(ctx: JSContext): Unit =
    val proxyConstructor = quickjs.value.NativeConstructor(
      name = "Proxy",
      callImpl = (_, _) =>
        throw new RuntimeException("Proxy constructor must be called with 'new'"),
      constructImpl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Proxy constructor requires target and handler")
        else
          val target = args(0)
          val handler = args(1)
          import quickjs.objmodel.JSObject
          val proxyObj = JSObject(prototype = ctx.objectPrototype, extensible = true)
          given JSContext = ctx
          proxyObj.defineProperty("__proxy_target", target, enumerable = false)
          proxyObj.defineProperty("__proxy_handler", handler, enumerable = false)
          JSValue.Object(proxyObj),
      prototype = ctx.objectPrototype
    )

    given JSContext = ctx
    initConstructor(proxyConstructor, length = 2)
    ctx.global.set("Proxy", JSValue.Native(proxyConstructor))

  private def initializeDate(ctx: JSContext): Unit =
    import java.time.{Instant, LocalDateTime, ZoneId, ZoneOffset, ZonedDateTime}
    import java.time.format.DateTimeFormatter
    import java.util.Locale

    val datePrototype = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
    given JSContext = ctx

    def toMillisOrNaN(value: JSValue): Double =
      value match
        case JSValue.Int32(i) => i.toDouble
        case JSValue.Float64(d) => d
        case _ => value.toNumber

    def setDateValue(obj: quickjs.objmodel.JSObject, millis: Double)(using JSContext): Unit =
      obj.defineProperty("__dateValue", JSValue.fromDouble(millis), enumerable = false, writable = true, configurable = false)

    def getDateValue(obj: quickjs.objmodel.JSObject)(using JSContext): Double =
      obj.getOwnProperty("__dateValue") match
        case Some(value) => value.toNumber
        case None => Double.NaN

    def newDateObject(millis: Double)(using JSContext): JSValue =
      val obj = quickjs.objmodel.JSObject(prototype = datePrototype, extensible = true)
      setDateValue(obj, millis)
      JSValue.Object(obj)

    def parseFractionalMillis(raw: String): Int =
      if raw.isEmpty then 0
      else
        val digits = if raw.length >= 3 then raw.substring(0, 3) else raw.padTo(3, '0')
        digits.toInt

    def parseIso(input: String): Option[Double] =
      val isoRegex =
        """^([+-]?\d{4,6})(?:-(\d{2})(?:-(\d{2}))?)?(?:T(\d{2}):(\d{2})(?::(\d{2})(?:\.(\d+))?)?)?(?:Z|([+-])(\d{2}):?(\d{2}))?$""".r
      input match
        case isoRegex(yearStr, monthStr, dayStr, hourStr, minuteStr, secondStr, fracStr, tzSign, tzHourStr, tzMinStr) =>
          val year = yearStr.toInt
          val month = if monthStr == null then 1 else monthStr.toInt
          val day = if dayStr == null then 1 else dayStr.toInt
          val hasTime = hourStr != null
          val hour = if hourStr == null then 0 else hourStr.toInt
          val minute = if minuteStr == null then 0 else minuteStr.toInt
          val second = if secondStr == null then 0 else secondStr.toInt
          val millis = if fracStr == null then 0 else parseFractionalMillis(fracStr)
          val hasTz = tzSign != null || input.endsWith("Z")
          val isLocal = hasTime && !hasTz
          val zone =
            if hasTz then
              if input.endsWith("Z") then ZoneOffset.UTC
              else
                val sign = if tzSign == "-" then -1 else 1
                val tzHour = tzHourStr.toInt
                val tzMin = tzMinStr.toInt
                ZoneOffset.ofHoursMinutes(sign * tzHour, sign * tzMin)
            else if !hasTime then
              ZoneOffset.UTC
            else
              ZoneId.systemDefault
          val ldt = LocalDateTime.of(year, month, day, hour, minute, second, millis * 1000000)
          val instant =
            if isLocal then ldt.atZone(ZoneId.systemDefault).toInstant
            else ldt.atZone(zone).toInstant
          Some(instant.toEpochMilli.toDouble)
        case _ =>
          None

    def parseMonth(token: String): Option[Int] =
      val months = Array("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
      val idx = months.indexOf(token.toLowerCase(Locale.ROOT))
      if idx >= 0 then Some(idx + 1) else None

    def parseTextDate(input: String): Option[Double] =
      val cleaned = input.trim.replaceAll("\\s+", " ")
      if cleaned.isEmpty then return None
      val tokens = cleaned.split(" ").toList
      val weekdays = Set("mon", "tue", "wed", "thu", "fri", "sat", "sun")
      val withoutWeekday =
        tokens match
          case head :: tail if weekdays.contains(head.take(3).toLowerCase(Locale.ROOT)) => tail
          case _ => tokens
      if withoutWeekday.length < 3 then return None
      val monthOpt = parseMonth(withoutWeekday.head)
      if monthOpt.isEmpty then return None
      val month = monthOpt.get
      val day = withoutWeekday(1).toInt
      val year = withoutWeekday(2).toInt
      var hour = 0
      var minute = 0
      var second = 0
      var millis = 0
      var zone: ZoneId | ZoneOffset = ZoneId.systemDefault
      if withoutWeekday.length >= 4 then
        val timeToken = withoutWeekday(3)
        if timeToken.contains(":") then
          val parts = timeToken.split(":")
          if parts.length >= 2 then
            hour = parts(0).toInt
            minute = parts(1).toInt
          if parts.length >= 3 then
            val secPart = parts(2)
            val secSplit = secPart.split("\\.")
            second = secSplit(0).toInt
            if secSplit.length > 1 then
              millis = parseFractionalMillis(secSplit(1))
      if withoutWeekday.length >= 5 then
        val tzToken = withoutWeekday(4)
        if tzToken.startsWith("GMT") && tzToken.length >= 8 then
          val sign = if tzToken.charAt(3) == '-' then -1 else 1
          val hh = tzToken.substring(4, 6).toInt
          val mm = tzToken.substring(6, 8).toInt
          zone = ZoneOffset.ofHoursMinutes(sign * hh, sign * mm)
      val ldt = LocalDateTime.of(year, month, day, hour, minute, second, millis * 1000000)
      val instant = ldt.atZone(zone).toInstant
      Some(instant.toEpochMilli.toDouble)

    def parseDateString(input: String): Double =
      val trimmed = input.trim
      if trimmed.isEmpty then Double.NaN
      else
        parseIso(trimmed)
          .orElse {
            if trimmed.matches("""^[+-]?\d{4,6}T.*""") then
              parseIso(trimmed.replaceFirst("T", "-01-01T"))
            else None
          }
          .orElse(parseTextDate(trimmed))
          .getOrElse(Double.NaN)

    def formatToISOString(millis: Double): String =
      val instant = Instant.ofEpochMilli(millis.toLong)
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").withZone(ZoneOffset.UTC)
      formatter.format(instant)

    def formatToString(millis: Double): String =
      val formatter = DateTimeFormatter.ofPattern("EEE MMM dd yyyy HH:mm:ss 'GMT'XXX", Locale.ENGLISH)
      val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(millis.toLong), ZoneId.systemDefault)
      formatter.format(zdt)

    def requireDateObject(args: Array[JSValue], method: String)(using JSContext): (quickjs.objmodel.JSObject, Double) =
      if args.isEmpty then
        ctx.throwTypeError(s"Date.prototype.$method called on undefined")
      args(0) match
        case JSValue.Object(obj) =>
          val value = getDateValue(obj)
          (obj, value)
        case _ =>
          ctx.throwTypeError(s"Date.prototype.$method called on non-object")

    val dateConstructor = quickjs.value.NativeConstructor(
      name = "Date",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        val now = System.currentTimeMillis().toDouble
        JSValue.fromString(formatToString(now)),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val millis =
          if args.isEmpty then
            System.currentTimeMillis().toDouble
          else
            args(0) match
              case JSValue.Object(obj) if !getDateValue(obj).isNaN =>
                getDateValue(obj)
              case JSValue.JSStr(s) =>
                parseDateString(s)
              case _ =>
                toMillisOrNaN(args(0))
        newDateObject(millis),
      prototype = datePrototype
    )
    initConstructor(dateConstructor, length = 7)

    val dateNow = NativeFunction(
      name = "now",
      impl = (_, _) => JSValue.fromDouble(System.currentTimeMillis().toDouble)
    )
    val dateParse = NativeFunction(
      name = "parse",
      impl = (args, _) =>
        val actualArgs = if args.length >= 2 then args.drop(1) else args
        if actualArgs.isEmpty then JSValue.fromDouble(Double.NaN)
        else JSValue.fromDouble(parseDateString(actualArgs(0).toString))
    )
    val dateUTC = NativeFunction(
      name = "UTC",
      impl = (args, _) =>
        val actualArgs = if args.length >= 2 then args.drop(1) else args
        if actualArgs.isEmpty then JSValue.fromDouble(Double.NaN)
        else
          val nums = actualArgs.take(7).map(toMillisOrNaN)
          if nums.exists(_.isNaN) then JSValue.fromDouble(Double.NaN)
          else
            val yearRaw = nums(0).toInt
            val year = if yearRaw >= 0 && yearRaw <= 99 then yearRaw + 1900 else yearRaw
            val month = if nums.length > 1 then nums(1).toInt else 0
            val day = if nums.length > 2 then nums(2).toInt else 1
            val hour = if nums.length > 3 then nums(3).toInt else 0
            val minute = if nums.length > 4 then nums(4).toInt else 0
            val second = if nums.length > 5 then nums(5).toInt else 0
            val ms = if nums.length > 6 then nums(6).toInt else 0
            val ldt = LocalDateTime.of(year, month + 1, day, hour, minute, second, ms * 1000000)
            JSValue.fromDouble(ldt.toInstant(ZoneOffset.UTC).toEpochMilli.toDouble)
    )

    val dateToISOString = NativeFunction(
      name = "toISOString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "toISOString")
        if value.isNaN then ctx.throwRangeError("Invalid time value")
        JSValue.fromString(formatToISOString(value))
    )

    val dateToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "toString")
        if value.isNaN then JSValue.fromString("Invalid Date")
        else JSValue.fromString(formatToString(value))
    )

    val dateGetTime = NativeFunction(
      name = "getTime",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getTime")
        JSValue.fromDouble(value)
    )

    val dateValueOf = NativeFunction(
      name = "valueOf",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "valueOf")
        JSValue.fromDouble(value)
    )

    val dateSetUTCHours = NativeFunction(
      name = "setUTCHours",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (obj, value) = requireDateObject(args, "setUTCHours")
        if value.isNaN then
          setDateValue(obj, Double.NaN)
          JSValue.fromDouble(Double.NaN)
        else
          val instant = Instant.ofEpochMilli(value.toLong)
          val base = ZonedDateTime.ofInstant(instant, ZoneOffset.UTC)
          val hour = if args.length > 1 then toMillisOrNaN(args(1)).toInt else base.getHour
          val minute = if args.length > 2 then toMillisOrNaN(args(2)).toInt else base.getMinute
          val second = if args.length > 3 then toMillisOrNaN(args(3)).toInt else base.getSecond
          val ms = if args.length > 4 then toMillisOrNaN(args(4)).toInt else base.getNano / 1000000
          val updated = base
            .withHour(hour)
            .withMinute(minute)
            .withSecond(second)
            .withNano(ms * 1000000)
          val newMillis = updated.toInstant.toEpochMilli.toDouble
          setDateValue(obj, newMillis)
          JSValue.fromDouble(newMillis)
    )

    datePrototype.defineProperty("toISOString", JSValue.Native(dateToISOString), enumerable = false)
    datePrototype.defineProperty("toString", JSValue.Native(dateToString), enumerable = false)
    datePrototype.defineProperty("getTime", JSValue.Native(dateGetTime), enumerable = false)
    datePrototype.defineProperty("valueOf", JSValue.Native(dateValueOf), enumerable = false)
    datePrototype.defineProperty("setUTCHours", JSValue.Native(dateSetUTCHours), enumerable = false)

    dateConstructor.funcObj.defineProperty("now", JSValue.Native(dateNow), enumerable = false)
    dateConstructor.funcObj.defineProperty("parse", JSValue.Native(dateParse), enumerable = false)
    dateConstructor.funcObj.defineProperty("UTC", JSValue.Native(dateUTC), enumerable = false)

    datePrototype.defineProperty("constructor", JSValue.Native(dateConstructor), enumerable = false)(using ctx)
    ctx.global.set("Date", JSValue.Native(dateConstructor))

  private def initializeTestHelpers(ctx: JSContext): Unit =
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

  private def initializeError(ctx: JSContext): Unit =
    def buildError(proto: quickjs.objmodel.JSObject, name: String, args: Array[JSValue])(using JSContext): JSValue =
      val obj = quickjs.objmodel.JSObject(prototype = proto, extensible = true)
      obj.set("name", JSValue.fromString(name))
      if args.nonEmpty then
        obj.set("message", args(0))
      ctx.attachStack(obj, skipFrames = 1)
      JSValue.Object(obj)

    given JSContext = ctx

    val errorPrototype = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
    errorPrototype.set("name", JSValue.fromString("Error"))
    val errorConstructor = quickjs.value.NativeConstructor(
      name = "Error",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(errorPrototype, "Error", args),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(errorPrototype, "Error", args),
      prototype = errorPrototype
    )
    initConstructor(errorConstructor, length = 1)
    errorPrototype.defineProperty("constructor", JSValue.Native(errorConstructor), enumerable = false)(using ctx)
    ctx.global.set("Error", JSValue.Native(errorConstructor))

    val typeErrorPrototype = quickjs.objmodel.JSObject(prototype = errorPrototype, extensible = true)
    typeErrorPrototype.set("name", JSValue.fromString("TypeError"))
    val typeErrorConstructor = quickjs.value.NativeConstructor(
      name = "TypeError",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(typeErrorPrototype, "TypeError", args),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(typeErrorPrototype, "TypeError", args),
      prototype = typeErrorPrototype
    )
    initConstructor(typeErrorConstructor, length = 1)
    typeErrorPrototype.defineProperty("constructor", JSValue.Native(typeErrorConstructor), enumerable = false)(using ctx)
    ctx.global.set("TypeError", JSValue.Native(typeErrorConstructor))

    val referenceErrorPrototype = quickjs.objmodel.JSObject(prototype = errorPrototype, extensible = true)
    referenceErrorPrototype.set("name", JSValue.fromString("ReferenceError"))
    val referenceErrorConstructor = quickjs.value.NativeConstructor(
      name = "ReferenceError",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(referenceErrorPrototype, "ReferenceError", args),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(referenceErrorPrototype, "ReferenceError", args),
      prototype = referenceErrorPrototype
    )
    initConstructor(referenceErrorConstructor, length = 1)
    referenceErrorPrototype.defineProperty("constructor", JSValue.Native(referenceErrorConstructor), enumerable = false)(using ctx)
    ctx.global.set("ReferenceError", JSValue.Native(referenceErrorConstructor))

    val syntaxErrorPrototype = quickjs.objmodel.JSObject(prototype = errorPrototype, extensible = true)
    syntaxErrorPrototype.set("name", JSValue.fromString("SyntaxError"))
    val syntaxErrorConstructor = quickjs.value.NativeConstructor(
      name = "SyntaxError",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(syntaxErrorPrototype, "SyntaxError", args),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(syntaxErrorPrototype, "SyntaxError", args),
      prototype = syntaxErrorPrototype
    )
    initConstructor(syntaxErrorConstructor, length = 1)
    syntaxErrorPrototype.defineProperty("constructor", JSValue.Native(syntaxErrorConstructor), enumerable = false)(using ctx)
    ctx.global.set("SyntaxError", JSValue.Native(syntaxErrorConstructor))

    val rangeErrorPrototype = quickjs.objmodel.JSObject(prototype = errorPrototype, extensible = true)
    rangeErrorPrototype.set("name", JSValue.fromString("RangeError"))
    val rangeErrorConstructor = quickjs.value.NativeConstructor(
      name = "RangeError",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(rangeErrorPrototype, "RangeError", args),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildError(rangeErrorPrototype, "RangeError", args),
      prototype = rangeErrorPrototype
    )
    initConstructor(rangeErrorConstructor, length = 1)
    rangeErrorPrototype.defineProperty("constructor", JSValue.Native(rangeErrorConstructor), enumerable = false)(using ctx)
    ctx.global.set("RangeError", JSValue.Native(rangeErrorConstructor))

    val errorPrototypeToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        thisValue match
          case JSValue.Object(obj) =>
            val nameValue = obj.get("name")(using ctx)
            val nameStr = if nameValue == JSValue.Undefined then "Error" else nameValue.toString
            val msgValue = obj.get("message")(using ctx)
            val msgStr = if msgValue == JSValue.Undefined then "" else msgValue.toString
            if nameStr.nonEmpty && msgStr.nonEmpty then
              JSValue.fromString(s"$nameStr: $msgStr")
            else if nameStr.nonEmpty then
              JSValue.fromString(nameStr)
            else
              JSValue.fromString(msgStr)
          case _ =>
            ctx.throwTypeError("Error.prototype.toString called on non-object")
    )
    errorPrototype.defineProperty("toString", JSValue.Native(errorPrototypeToString), enumerable = false)(using ctx)
  /** Initialize Function.prototype methods */
  def initializeFunctionPrototype(ctx: JSContext): Unit =
    // Function.prototype.call(thisArg, arg1, arg2, ...)
    val functionPrototypeCall = NativeFunction(
      name = "call",
      impl = (args, ctx) =>
        // When called as a method, args(0) is the function (this value)
        // args(1) is the thisArg, args(2...) are the actual arguments
        if args.isEmpty then
          throw new RuntimeException("Function.prototype.call called on non-function")
        else
          val func = args(0)  // The function to call
          val thisArg = if args.length > 1 then args(1) else JSValue.Undefined
          val actualArgs = if args.length > 2 then args.slice(2, args.length) else Array.empty[JSValue]

          func match
            case f: JSValue.Function =>
              // Call the bytecode function with the custom this binding
              given JSContext = ctx
              val interpreter = Interpreter()
              // Create a temporary BytecodeFunction wrapper
              val bcFunc = new BytecodeFunction(
                name = f.name,
                bytecode = f.bytecode,
                constants = f.constants,
                stackSize = f.stackSize,
                freeVars = Array.empty,
                paramNames = f.paramNames,
                localVarNames = f.localVarNames,
                argumentsIndex = f.argumentsIndex,
                isConstructor = f.isConstructor
              )
              interpreter.call(bcFunc, thisArg, actualArgs, f.closure)
            case JSValue.Native(nativeFuncWrapper) =>
              // Call the native function with the custom this binding
              nativeFuncWrapper match
                case native: NativeFunction =>
                  // Prepend thisArg to arguments for native functions that expect it
                  val argsWithThis = new Array[JSValue](actualArgs.length + 1)
                  argsWithThis(0) = thisArg
                  Array.copy(actualArgs, 0, argsWithThis, 1, actualArgs.length)
                  given JSContext = ctx
                  native.call(argsWithThis)
                case constructor: quickjs.value.NativeConstructor =>
                  // Native constructor called with .call()
                  given JSContext = ctx
                  constructor.call(actualArgs)
                case _ =>
                  throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
            case _ =>
              throw new RuntimeException(s"Function.prototype.call called on non-function: $func")
    )

    // Add methods to Function.prototype
    given JSContext = ctx
    ctx.functionPrototype.set("call", JSValue.Native(functionPrototypeCall))

    val functionPrototypeToString = NativeFunction(
      name = "toString",
      impl = (_, _) => JSValue.fromString("[object Function]")
    )
    ctx.functionPrototype.set("toString", JSValue.Native(functionPrototypeToString))

  /** Initialize Array.prototype methods */
  def initializeArrayPrototype(ctx: JSContext): Unit =
    def strictEquals(a: JSValue, b: JSValue): Boolean = (a, b) match
      case (JSValue.Int32(x), JSValue.Int32(y)) => x == y
      case (JSValue.Float64(x), JSValue.Float64(y)) =>
        !x.isNaN && !y.isNaN && x == y
      case (JSValue.Int32(x), JSValue.Float64(y)) =>
        !y.isNaN && x.toDouble == y
      case (JSValue.Float64(x), JSValue.Int32(y)) =>
        !x.isNaN && x == y.toDouble
      case _ => a == b

    def sameValueZero(a: JSValue, b: JSValue): Boolean = (a, b) match
      case (JSValue.Float64(x), JSValue.Float64(y)) if x.isNaN && y.isNaN => true
      case _ => strictEquals(a, b)
    // Array.prototype.push(element1, ..., elementN)
    // Appends elements to the end of an array and returns the new length
    val arrayPrototypePush = NativeFunction(
      name = "push",
      impl = (args, ctx) =>
        // args(0) is the array (this value)
        // args(1...) are the elements to push
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.push called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              // Add each element to the array using the push method
              for i <- 1 until args.length do
                arr.push(args(i))
              JSValue.fromInt(arr.getLength)
            case _ =>
              throw new RuntimeException(s"Array.prototype.push called on non-array: $arrValue")
    )

    // Array.prototype.map(callback)
    // Creates a new array with the results of calling a provided function on every element
    val arrayPrototypeMap = NativeFunction(
      name = "map",
      impl = (args, ctx) =>
        // args(0) is the array (this value)
        // args(1) is the callback function
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.map requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val resultArr = quickjs.objmodel.JSArray.empty()
              var index = 0

              // Call callback for each element
              while index < arr.getLength do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                val result = callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx)
                resultArr.push(result)
                index += 1

              JSValue.JSArrayVal(resultArr)
            case _ =>
              throw new RuntimeException(s"Array.prototype.map called on non-array: $arrValue")
    )

    val arrayPrototypeFilter = NativeFunction(
      name = "filter",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.filter requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val resultArr = quickjs.objmodel.JSArray.empty()
              var index = 0
              while index < arr.getLength do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                val keep = callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx).toBoolean
                if keep then resultArr.push(elem)
                index += 1
              JSValue.JSArrayVal(resultArr)
            case _ =>
              throw new RuntimeException(s"Array.prototype.filter called on non-array: $arrValue")
    )

    val arrayPrototypeForEach = NativeFunction(
      name = "forEach",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.forEach requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              var index = 0
              while index < arr.getLength do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx)
                index += 1
              JSValue.Undefined
            case _ =>
              throw new RuntimeException(s"Array.prototype.forEach called on non-array: $arrValue")
    )

    val arrayPrototypeReduce = NativeFunction(
      name = "reduce",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.reduce requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val len = arr.getLength
              val hasInitial = args.length > 2
              if len == 0 && !hasInitial then
                throw new RuntimeException("TypeError: Reduce of empty array with no initial value")
              var acc =
                if hasInitial then args(2)
                else arr.get(0)
              var index = if hasInitial then 0 else 1
              while index < len do
                val elem = arr.get(index)
                val callbackArgs = Array(acc, elem, JSValue.fromInt(index), arrVal)
                acc = callFunctionWithThis(callback, JSValue.Undefined, callbackArgs)(using ctx)
                index += 1
              acc
            case _ =>
              throw new RuntimeException(s"Array.prototype.reduce called on non-array: $arrValue")
    )

    val arrayPrototypeIncludes = NativeFunction(
      name = "includes",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.includes called on non-array")
        else
          val arrValue = args(0)
          val search = if args.length > 1 then args(1) else JSValue.Undefined
          val fromIndex =
            if args.length > 2 then args(2).toNumber.toInt
            else 0
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              var k = if fromIndex < 0 then math.max(len + fromIndex, 0) else fromIndex
              var found = false
              while k < len && !found do
                if sameValueZero(arr.get(k), search) then
                  found = true
                k += 1
              JSValue.fromBoolean(found)
            case _ =>
              throw new RuntimeException(s"Array.prototype.includes called on non-array: $arrValue")
    )

    val arrayPrototypeIndexOf = NativeFunction(
      name = "indexOf",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.indexOf called on non-array")
        else
          val arrValue = args(0)
          val search = if args.length > 1 then args(1) else JSValue.Undefined
          val fromIndex =
            if args.length > 2 then args(2).toNumber.toInt
            else 0
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              var k = if fromIndex < 0 then math.max(len + fromIndex, 0) else fromIndex
              var idx = -1
              while k < len && idx < 0 do
                if strictEquals(arr.get(k), search) then
                  idx = k
                k += 1
              JSValue.fromInt(idx)
            case _ =>
              throw new RuntimeException(s"Array.prototype.indexOf called on non-array: $arrValue")
    )

    val arrayPrototypeSplice = NativeFunction(
      name = "splice",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.splice called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              val startRaw = if args.length > 1 then args(1).toNumber.toInt else 0
              val actualStart =
                if startRaw < 0 then math.max(len + startRaw, 0)
                else math.min(startRaw, len)
              val deleteCountRaw =
                if args.length > 2 then args(2).toNumber.toInt
                else len - actualStart
              val actualDelete = math.max(0, math.min(deleteCountRaw, len - actualStart))
              val items =
                if args.length > 3 then args.slice(3, args.length).toSeq
                else Seq.empty
              val removed = arr.splice(actualStart, actualDelete, items)
              JSValue.JSArrayVal(removed)
            case _ =>
              throw new RuntimeException(s"Array.prototype.splice called on non-array: $arrValue")
    )

    // Array.prototype.pop()
    // Removes the last element from an array and returns that element
    val arrayPrototypePop = NativeFunction(
      name = "pop",
      impl = (args, ctx) =>
        // args(0) is the array (this value)
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.pop called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              arr.pop()
            case _ =>
              throw new RuntimeException(s"Array.prototype.pop called on non-array: $arrValue")
    )

    // Array.prototype.toString()
    // Joins elements with commas
    val arrayPrototypeToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        if args.isEmpty then
          JSValue.fromString("")
        else
          args(0) match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val sb = new StringBuilder()
              var i = 0
              while i < arr.getLength do
                if i > 0 then sb.append(",")
                sb.append(arr.get(i).toString)
                i += 1
              JSValue.fromString(sb.toString)
            case _ =>
              JSValue.fromString("")
    )

    // Array.prototype.concat(value1, value2, ..., valueN)
    // Returns a new array comprised of this array joined with other array(s) and/or value(s)
    val arrayPrototypeConcat = NativeFunction(
      name = "concat",
      impl = (args, ctx) =>
        // args(0) is the array (this value)
        // args(1...) are values/arrays to concatenate
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.concat called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val resultArr = quickjs.objmodel.JSArray.empty()

              // Copy all elements from this array
              var i = 0
              while i < arr.getLength do
                resultArr.push(arr.get(i))
                i += 1

              // Concatenate additional arguments
              for j <- 1 until args.length do
                args(j) match
                  case otherArr: JSValue.JSArrayVal =>
                    // Concatenate array elements
                    var k = 0
                    while k < otherArr.value.getLength do
                      resultArr.push(otherArr.value.get(k))
                      k += 1
                  case elem =>
                    // Concatenate single element
                    resultArr.push(elem)

              JSValue.JSArrayVal(resultArr)
            case _ =>
              throw new RuntimeException(s"Array.prototype.concat called on non-array: $arrValue")
    )

    // Array.prototype.slice(begin, end)
    // Returns a shallow copy of a portion of an array
    val arrayPrototypeSlice = NativeFunction(
      name = "slice",
      impl = (args, ctx) =>
        // args(0) is the array (this value)
        // args(1) is begin (optional)
        // args(2) is end (optional)
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.slice called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val length = arr.getLength

              // Parse begin parameter
              val begin = if args.length > 1 then
                args(1) match
                  case JSValue.Int32(i) => i
                  case JSValue.Float64(d) => d.toInt
                  case _ => 0
              else
                0

              // Handle negative begin
              val start = if begin < 0 then
                val normalized = length + begin
                if normalized < 0 then 0 else normalized
              else
                if begin > length then length else begin

              // Parse end parameter
              val end = if args.length > 2 then
                args(2) match
                  case JSValue.Int32(i) => i
                  case JSValue.Float64(d) => d.toInt
                  case _ => length
              else
                length

              // Handle negative end
              val stop = if end < 0 then
                val normalized = length + end
                if normalized < 0 then 0 else normalized
              else
                if end > length then length else end

              // Create result array with sliced elements
              val resultArr = quickjs.objmodel.JSArray.empty()
              var i = start
              while i < stop do
                resultArr.push(arr.get(i))
                i += 1

              JSValue.JSArrayVal(resultArr)
            case _ =>
              throw new RuntimeException(s"Array.prototype.slice called on non-array: $arrValue")
    )

    // Add methods to Array.prototype
    given JSContext = ctx
    ctx.arrayPrototype.set("push", JSValue.Native(arrayPrototypePush))
    ctx.arrayPrototype.set("pop", JSValue.Native(arrayPrototypePop))
    ctx.arrayPrototype.set("map", JSValue.Native(arrayPrototypeMap))
    ctx.arrayPrototype.set("filter", JSValue.Native(arrayPrototypeFilter))
    ctx.arrayPrototype.set("forEach", JSValue.Native(arrayPrototypeForEach))
    ctx.arrayPrototype.set("reduce", JSValue.Native(arrayPrototypeReduce))
    ctx.arrayPrototype.set("includes", JSValue.Native(arrayPrototypeIncludes))
    ctx.arrayPrototype.set("indexOf", JSValue.Native(arrayPrototypeIndexOf))
    ctx.arrayPrototype.set("splice", JSValue.Native(arrayPrototypeSplice))
    ctx.arrayPrototype.set("toString", JSValue.Native(arrayPrototypeToString))
    ctx.arrayPrototype.set("concat", JSValue.Native(arrayPrototypeConcat))
    ctx.arrayPrototype.set("slice", JSValue.Native(arrayPrototypeSlice))

  /** Initialize all standard library methods */
  def initialize(ctx: JSContext): Unit =
    initializeFunctionPrototype(ctx)
    initializeArrayPrototype(ctx)
    initializeForInHelpers(ctx)
    initializeModuleHelpers(ctx)
    initializeArrayHelpers(ctx)
    initializeObjectStatics(ctx)
    initializeMath(ctx)
    initializeNumberString(ctx)
    initializeDate(ctx)
    initializeProxy(ctx)
    initializeTestHelpers(ctx)
    initializeError(ctx)
