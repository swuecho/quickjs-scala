package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.interpreter.{Interpreter, PropertyAccess}
import quickjs.runtime.builtins.BuiltinHelpers.{
  extractJSObject,
  functionToBytecode,
  parsePropertyDescriptor,
  buildPropertyDescriptorObject,
  nativeArgs
}

/** Object static methods (Object.keys, Object.defineProperty, freeze, seal,
  * etc.).
  */
object ObjectBuiltins {
  import quickjs.objmodel.{JSObject, JSArray}

  /** Extract the underlying JSObject, falling back to Function.funcObj. */
  /** Call a proxy trap on a handler object. */
  private def callProxyTrap(
      handlerObj: JSObject,
      trapName: String,
      args: Array[JSValue]
  )(using ctx: JSContext): JSValue = {
    val desc = handlerObj.getOwnPropertyDescriptor(trapName) match {
      case Some((trap: JSValue.Function, _)) => Some(trap)
      case Some((JSValue.Native(nf: quickjs.value.NativeFunction), _)) =>
        Some(JSValue.Native(nf))
      case _ => None
    }
    desc match {
      case Some(trap) =>
        BuiltinHelpers.callFunctionValue(trap, JSValue.Object(handlerObj), args)
      case None => JSValue.Undefined
    }
  }

  /** Check if a value is a proxy and return (target, handler). */
  private def isProxyValue(v: JSValue)(using
      ctx: JSContext
  ): Option[(JSValue, JSObject)] =
    v match {
      case JSValue.Object(obj) =>
        (
          obj.getOwnProperty("__proxy_target"),
          obj.getOwnProperty("__proxy_handler")
        ) match {
          case (Some(target), Some(JSValue.Object(handler))) =>
            Some((target, handler))
          case _ => None
        }
      case _ => None
    }

  private def objOf(value: JSValue): Option[JSObject] = extractJSObject(value)

  /** Convert a value to an object per the ToObject abstract operation. Throws
    * TypeError for null/undefined. Wraps primitives in objects.
    */
  private def toObjectForAssign(value: JSValue, ctx: JSContext): JSValue = {
    given JSContext = ctx
    value match {
      case JSValue.Null | JSValue.Undefined =>
        ctx.throwTypeError(s"Cannot convert ${value.toString} to object")
      case JSValue.Object(_) | _: JSValue.Function | JSValue.JSArrayVal(_) =>
        value
      case JSValue.JSStr(s) =>
        // Wrap string in a String object with proper valueOf/toString
        val strProto = ctx.global.get("String") match {
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            nc.prototype
          case _ => ctx.objectPrototype
        }
        val obj = JSObject(prototype = strProto, extensible = true)
        var i = 0
        while i < s.length do {
          obj.defineProperty(
            i.toString,
            JSValue.fromString(s.charAt(i).toString),
            enumerable = true,
            writable = false
          )
          i += 1
        }
        obj.defineProperty(
          "length",
          JSValue.fromInt(s.length),
          enumerable = false,
          writable = false
        )
        obj.defineProperty(
          "valueOf",
          JSValue.Native(
            quickjs.value.NativeFunction("valueOf", (_, _) => value)
          ),
          enumerable = false
        )
        obj.defineProperty(
          "toString",
          JSValue.Native(
            quickjs.value.NativeFunction("toString", (_, _) => value)
          ),
          enumerable = false
        )
        JSValue.Object(obj)
      case v @ (_: JSValue.Int32 | _: JSValue.Float64) =>
        val numProto = ctx.global.get("Number") match {
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            nc.prototype
          case _ => ctx.objectPrototype
        }
        val wrapper = JSObject(prototype = numProto, extensible = true)
        wrapper.initProperty(
          "__primitive",
          v,
          enumerable = false,
          writable = false,
          configurable = false
        )
        JSValue.Object(wrapper)
      case v @ JSValue.Bool(_) =>
        val boolProto = ctx.global.get("Boolean") match {
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            nc.prototype
          case _ => ctx.objectPrototype
        }
        val wrapper = JSObject(prototype = boolProto, extensible = true)
        wrapper.initProperty(
          "__primitive",
          v,
          enumerable = false,
          writable = false,
          configurable = false
        )
        JSValue.Object(wrapper)
      case v @ JSValue.Symbol(_) =>
        val wrapper =
          JSObject(prototype = ctx.symbolPrototype, extensible = true)
        wrapper.initProperty(
          "__primitive",
          v,
          enumerable = false,
          writable = false,
          configurable = false
        )
        JSValue.Object(wrapper)
      case v @ JSValue.BigInt(_) =>
        val biProto = ctx.global.get("BigInt") match {
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            nc.prototype
          case _ => ctx.objectPrototype
        }
        val wrapper = JSObject(prototype = biProto, extensible = true)
        wrapper.initProperty(
          "__primitive",
          v,
          enumerable = false,
          writable = false,
          configurable = false
        )
        JSValue.Object(wrapper)
      case _ =>
        JSValue.Object(
          JSObject(prototype = ctx.objectPrototype, extensible = true)
        )
    }
  }

  /** Invoke a getter function and return its result. */
  private def invokeGetter(getter: JSValue, thisValue: JSValue)(using
      ctx: JSContext
  ): JSValue =
    getter match {
      case func: JSValue.Function =>
        Interpreter().call(
          functionToBytecode(func),
          thisValue,
          Array.empty,
          func.closure
        )
      case JSValue.Native(native: quickjs.value.NativeFunction) =>
        native.call(Array(thisValue))
      case _ => JSValue.Undefined
    }

  /** Invoke a setter function with a value. */
  private def invokeSetter(setter: JSValue, thisValue: JSValue, value: JSValue)(
      using ctx: JSContext
  ): Unit =
    setter match {
      case func: JSValue.Function =>
        Interpreter().call(
          functionToBytecode(func),
          thisValue,
          Array(value),
          func.closure
        )
      case JSValue.Native(native: quickjs.value.NativeFunction) =>
        native.call(Array(thisValue, value))
      case _ => ()
    }

  def initialize(ctx: JSContext): Unit = {
    def isArrayIndexKey(key: String): Boolean =
      key.nonEmpty && key
        .forall(_.isDigit) && (key.length == 1 || key.charAt(0) != '0')

    def hasOwnKey(target: JSValue, key: String)(using JSContext): Boolean =
      target match {
        case JSValue.Object(obj) =>
          obj.getOwnProperty(key).isDefined
        case func: JSValue.Function =>
          func.funcObj.getOwnProperty(key).isDefined
        case JSValue.Native(nw) =>
          nw match {
            case c: quickjs.value.NativeConstructor =>
              c.funcObj.getOwnProperty(key).isDefined
            case nf: quickjs.value.NativeFunction =>
              nf.funcObj.getOwnProperty(key).isDefined
            case _ => false
          }
        case JSValue.JSArrayVal(arr) =>
          if key == "length" then true
          else if isArrayIndexKey(key) then {
            val idx = key.toInt
            idx >= 0 && idx < arr.getLength
          }
          else arr.getOwnProperty(key).isDefined
        case _ => false
      }

    def hasOwnSymbolKey(target: JSValue, symbolId: Int)(using
        JSContext
    ): Boolean =
      target match {
        case JSValue.Object(obj) =>
          obj.getOwnSymbolProperty(symbolId).isDefined
        case func: JSValue.Function =>
          func.funcObj.getOwnSymbolProperty(symbolId).isDefined
        case JSValue.Native(nw) =>
          nw match {
            case c: quickjs.value.NativeConstructor =>
              c.funcObj.getOwnSymbolProperty(symbolId).isDefined
            case nf: quickjs.value.NativeFunction =>
              nf.funcObj.getOwnSymbolProperty(symbolId).isDefined
            case _ => false
          }
        case _ => false
      }

    def definePropertyOnTarget(
        target: JSValue,
        propKey: String,
        descriptor: JSValue
    )(using JSContext): JSValue = {
      val pd = parsePropertyDescriptor(descriptor)
      target match {
        case JSValue.JSArrayVal(arr) if isArrayIndexKey(propKey) =>
          // Handle array index property
          val idx = propKey.toInt
          val existingDesc = arr.getIndexAttributes(idx).map { attrs =>
            (arr.getRaw(idx), attrs)
          }
          if pd.isAccessor && pd.hasValueField then
            ctx.throwTypeError(
              "Invalid property descriptor. Cannot have both accessors and a value"
            )
          val enumerable = pd.enumerable.getOrElse(
            existingDesc.map(_._2.enumerable).getOrElse(false)
          )
          val writable = pd.writable.getOrElse(
            existingDesc.map(_._2.writable).getOrElse(false)
          )
          val configurable = pd.configurable.getOrElse(
            existingDesc.map(_._2.configurable).getOrElse(false)
          )
          val ok = if pd.isAccessor then {
            val getter = pd.getter.orElse(existingDesc.flatMap(_._2.getter))
            val setter = pd.setter.orElse(existingDesc.flatMap(_._2.setter))
            arr.defineIndexAccessor(
              idx,
              getter,
              setter,
              enumerable,
              configurable
            )
          }
          else {
            val value = pd.value.getOrElse(arr.getRaw(idx))
            arr.defineIndexProperty(
              idx,
              value,
              enumerable,
              writable,
              configurable
            )
          }
          if !ok then ctx.throwTypeError("Cannot define property")
          target
        case _ =>
          def applyDefine(obj: JSObject): JSValue = {
            val existingDesc = obj.getOwnPropertyDescriptor(propKey)
            if pd.isAccessor && pd.hasValueField then
              ctx.throwTypeError(
                "Invalid property descriptor. Cannot have both accessors and a value"
              )
            val enumerable = pd.enumerable.getOrElse(
              existingDesc.map(_._2.enumerable).getOrElse(false)
            )
            val writable = pd.writable.getOrElse(
              existingDesc.map(_._2.writable).getOrElse(false)
            )
            val configurable = pd.configurable.getOrElse(
              existingDesc.map(_._2.configurable).getOrElse(false)
            )
            val value = pd.value.getOrElse(obj.get(propKey))
            val ok = if pd.isAccessor then {
              val getter = pd.getter.orElse(existingDesc.flatMap(_._2.getter))
              val setter = pd.setter.orElse(existingDesc.flatMap(_._2.setter))
              obj.defineAccessorProperty(
                propKey,
                getter,
                setter,
                enumerable,
                configurable
              )
            }
            else
              obj.defineProperty(
                propKey,
                value,
                enumerable,
                writable,
                configurable
              )
            if !ok then ctx.throwTypeError("Cannot define property")
            target
          }
          objOf(target).map(applyDefine).getOrElse(target)
      }
    }

    val setPrototypeOf = NativeFunction(
      name = "setPrototypeOf",
      impl = (args, ctx) =>
        if args.length < 2 then JSValue.Undefined
        else {
          val offset = if args.length >= 3 then 1 else 0
          val target = args(offset)
          val proto = args(offset + 1)
          (target, proto) match {
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
              ctx.throwTypeError(
                "Object.setPrototypeOf called with invalid prototype"
              )
            case _ =>
              ctx.throwTypeError("Object.setPrototypeOf called on non-object")
          }
        }
    )

    val defineProperty = NativeFunction(
      name = "defineProperty",
      impl = (args, ctx) =>
        if args.length < 3 then JSValue.Undefined
        else {
          val offset = if args.length >= 4 then 1 else 0
          val target = args(offset)
          val rawKey = args(offset + 1)
          val descriptor = args(offset + 2)
          given JSContext = ctx
          rawKey match {
            case JSValue.Symbol(sym) =>
              val pd = parsePropertyDescriptor(descriptor)
              objOf(target) match {
                case Some(obj) =>
                  val existingDesc = obj.getOwnSymbolPropertyDescriptor(sym)
                  if pd.isAccessor && pd.hasValueField then
                    ctx.throwTypeError("Invalid property descriptor")
                  val enumerable = pd.enumerable.getOrElse(
                    existingDesc.map(_._2.enumerable).getOrElse(false)
                  )
                  val configurable = pd.configurable.getOrElse(
                    existingDesc.map(_._2.configurable).getOrElse(false)
                  )
                  if pd.isAccessor then {
                    val getter =
                      pd.getter.orElse(existingDesc.flatMap(_._2.getter))
                    val setter =
                      pd.setter.orElse(existingDesc.flatMap(_._2.setter))
                    val ok = obj.defineSymbolAccessorProperty(
                      sym,
                      getter,
                      setter,
                      enumerable,
                      configurable
                    )
                    if !ok then ctx.throwTypeError("Cannot define property")
                  }
                  else {
                    val writable = pd.writable.getOrElse(
                      existingDesc.map(_._2.writable).getOrElse(false)
                    )
                    val value = pd.value.getOrElse(obj.getSymbol(sym))
                    val ok = obj.defineSymbolProperty(
                      sym,
                      value,
                      enumerable,
                      writable,
                      configurable
                    )
                    if !ok then ctx.throwTypeError("Cannot define property")
                  }
                  target
                case None => target
              }
            case _ =>
              definePropertyOnTarget(target, rawKey.toString, descriptor)
          }
        }
    )

    val objectIs = NativeFunction(
      name = "is",
      impl = (args, ctx) =>
        if args.length < 2 then JSValue.Bool(false)
        else {
          val offset = if args.length >= 3 then 1 else 0
          val a = args(offset)
          val b = args(offset + 1)

          def sameValue(x: JSValue, y: JSValue): Boolean = (x, y) match {
            case (JSValue.Float64(dx), JSValue.Float64(dy)) =>
              if java.lang.Double.isNaN(dx) && java.lang.Double.isNaN(dy) then
                true
              else
                java.lang.Double.doubleToRawLongBits(dx) == java.lang.Double
                  .doubleToRawLongBits(dy)
            case (JSValue.Int32(ix), JSValue.Int32(iy)) =>
              ix == iy
            case (JSValue.Int32(ix), JSValue.Float64(dy)) =>
              if java.lang.Double.isNaN(dy) then false
              else if ix == 0 && java.lang.Double.doubleToRawLongBits(
                  dy
                ) == java.lang.Double.doubleToRawLongBits(-0.0)
              then false
              else ix.toDouble == dy
            case (JSValue.Float64(dx), JSValue.Int32(iy)) =>
              if java.lang.Double.isNaN(dx) then false
              else if iy == 0 && java.lang.Double.doubleToRawLongBits(
                  dx
                ) == java.lang.Double.doubleToRawLongBits(-0.0)
              then false
              else dx == iy.toDouble
            case (JSValue.BigInt(bx), JSValue.BigInt(by)) =>
              bx == by
            case _ =>
              x == y
          }

          JSValue.Bool(sameValue(a, b))
        }
    )

    val objectGetOwnPropertyNames = NativeFunction(
      name = "getOwnPropertyNames",
      impl = (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        objOf(target) match {
          case Some(o) =>
            val result = JSArray.empty()
            o.getAllProperties.keys
              .filterNot(k => k.startsWith("__"))
              .foreach(k => result.push(JSValue.fromString(k)))
            JSValue.JSArrayVal(result)
          case None =>
            target match {
              case JSValue.JSArrayVal(arr) =>
                val result = JSArray.empty()
                for i <- 0 until arr.getLength do
                  result.push(JSValue.fromString(i.toString))
                result.push(JSValue.fromString("length"))
                JSValue.JSArrayVal(result)
              case _ => JSValue.JSArrayVal(JSArray.empty())
            }
        }
    )

    val objectGetOwnPropertyDescriptor = NativeFunction(
      name = "getOwnPropertyDescriptor",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 2 then JSValue.Undefined
        else {
          val offset = if args.length >= 3 then 1 else 0
          val target = args(offset)
          val rawKey = args(offset + 1)
          rawKey match {
            case JSValue.Symbol(sym) =>
              // Symbol-keyed property
              objOf(target) match {
                case Some(o) =>
                  buildPropertyDescriptorObject(
                    s"Symbol(${sym})",
                    o.getOwnSymbolPropertyDescriptor(sym)
                  )
                case None => JSValue.Undefined
              }
            case _ =>
              val propKey = rawKey.toString
              objOf(target) match {
                case Some(o) =>
                  buildPropertyDescriptorObject(
                    propKey,
                    o.getOwnPropertyDescriptor(propKey)
                  )
                case None =>
                  target match {
                    case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                      val synthesized = nf.funcObj
                        .getOwnPropertyDescriptor(propKey)
                        .orElse(
                          propKey match {
                            case "length" =>
                              Some(
                                (
                                  JSValue.fromInt(nf.length),
                                  JSObject.PropertyAttributes(
                                    enumerable = false,
                                    writable = false,
                                    configurable = true
                                  )
                                )
                              )
                            case "name" =>
                              Some(
                                (
                                  JSValue.fromString(nf.name),
                                  JSObject.PropertyAttributes(
                                    enumerable = false,
                                    writable = false,
                                    configurable = true
                                  )
                                )
                              )
                            case _ => None
                          }
                        )
                      buildPropertyDescriptorObject(propKey, synthesized)
                    case _ => JSValue.Undefined
                  }
              }
          }
        }
    )

    val objectGetOwnPropertyDescriptors = NativeFunction(
      name = "getOwnPropertyDescriptors",
      impl = (args, ctx) =>
        given JSContext = ctx
        val offset = if args.length >= 2 then 1 else 0
        val target =
          if args.length > offset then args(offset) else JSValue.Undefined
        val result =
          JSObject(prototype = ctx.objectPrototype, extensible = true)
        def pushDescriptor(
            key: String,
            desc: Option[(JSValue, JSObject.PropertyAttributes)]
        ): Unit =
          buildPropertyDescriptorObject(key, desc) match {
            case JSValue.Object(descObj) =>
              result.set(key, JSValue.Object(descObj))
            case _ => ()
          }
        objOf(target) match {
          case Some(o) =>
            o.getAllProperties.keys.foreach(k =>
              pushDescriptor(k, o.getOwnPropertyDescriptor(k))
            )
          case None =>
            target match {
              case JSValue.JSArrayVal(arr) =>
                for i <- 0 until arr.getLength do
                  pushDescriptor(
                    i.toString,
                    Some(
                      arr.get(i) -> JSObject.PropertyAttributes(enumerable =
                        true
                      )
                    )
                  )
                pushDescriptor(
                  "length",
                  Some(
                    JSValue
                      .fromInt(arr.getLength) -> JSObject.PropertyAttributes(
                      enumerable = false,
                      writable = true,
                      configurable = false
                    )
                  )
                )
                arr.getOwnPropertyKeys.foreach(k =>
                  pushDescriptor(
                    k,
                    Some(
                      arr
                        .getProperty(k)
                        .getOrElse(JSValue.Undefined) -> JSObject
                        .PropertyAttributes(enumerable = true)
                    )
                  )
                )
              case _ => ()
            }
        }
        JSValue.Object(result)
    )

    val objectKeys = NativeFunction(
      name = "keys",
      impl = (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        objOf(target) match {
          case Some(o) =>
            val result = JSArray.empty()
            o.getOwnPropertyKeys()
              .foreach(k => result.push(JSValue.fromString(k)))
            JSValue.JSArrayVal(result)
          case None =>
            target match {
              case JSValue.JSArrayVal(arr) =>
                val result = JSArray.empty()
                for i <- 0 until arr.getLength do
                  result.push(JSValue.fromString(i.toString))
                JSValue.JSArrayVal(result)
              case _ => JSValue.JSArrayVal(JSArray.empty())
            }
        }
    )

    val objectGetPrototypeOf = NativeFunction(
      name = "getPrototypeOf",
      impl = (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        objOf(target) match {
          case Some(o) =>
            val proto = o.getPrototype
            if proto == null then JSValue.Null else JSValue.Object(proto)
          case None =>
            target match {
              case JSValue.JSArrayVal(_) => JSValue.Object(ctx.arrayPrototype)
              case _                     => JSValue.Undefined
            }
        }
    )

    val objectAssign = NativeFunction(
      name = "assign",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 1 then JSValue.Undefined
        else {
          val offset = if args.length >= 2 then 1 else 0
          // ToObject: throw TypeError for null/undefined, wrap primitives
          val target = toObjectForAssign(args(offset), ctx)

          def setTargetProp(key: String, value: JSValue): Unit = {
            val ok = target match {
              case JSValue.Object(obj) =>
                // Check for setter on target
                obj.getOwnPropertyDescriptor(key) match {
                  case Some((_, attrs)) if attrs.setter.isDefined =>
                    invokeSetter(attrs.setter.get, target, value)
                    true
                  case _ =>
                    obj.set(key, value)(using ctx)
                }
              case func: JSValue.Function =>
                func.funcObj.getOwnPropertyDescriptor(key) match {
                  case Some((_, attrs)) if attrs.setter.isDefined =>
                    invokeSetter(attrs.setter.get, func, value)
                    true
                  case _ =>
                    func.funcObj.set(key, value)(using ctx)
                }
              case JSValue.JSArrayVal(arr) =>
                if key.forall(_.isDigit) then {
                  arr.set(key.toInt, value)
                  true
                }
                else {
                  arr.setProperty(key, value)
                  true
                }
              case _ => false
            }
            // Object.assign throws TypeError if [[Set]] fails (strict mode semantics)
            if !ok then
              ctx.throwTypeError(s"Cannot set property '$key' on target object")
          }

          def setTargetSymbolProp(symbolId: Int, value: JSValue): Unit = {
            val ok = target match {
              case JSValue.Object(obj) =>
                obj.getOwnSymbolPropertyDescriptor(symbolId) match {
                  case Some((_, attrs)) if attrs.setter.isDefined =>
                    invokeSetter(attrs.setter.get, target, value)
                    true
                  case _ =>
                    obj.setSymbol(symbolId, value)(using ctx)
                }
              case func: JSValue.Function =>
                func.funcObj.getOwnSymbolPropertyDescriptor(symbolId) match {
                  case Some((_, attrs)) if attrs.setter.isDefined =>
                    invokeSetter(attrs.setter.get, func, value)
                    true
                  case _ =>
                    func.funcObj.setSymbol(symbolId, value)(using ctx)
                }
              case _ => false
            }
            if !ok then
              ctx.throwTypeError("Cannot set symbol property on target object")
          }

          for i <- (offset + 1) until args.length do
            args(i) match {
              case src @ JSValue.Object(obj) =>
                isProxyValue(src) match {
                  case Some((proxyTarget, handler)) =>
                    // Proxy source: use ownKeys and getOwnPropertyDescriptor traps
                    val keysResult =
                      callProxyTrap(handler, "ownKeys", Array(proxyTarget))
                    keysResult match {
                      case JSValue.JSArrayVal(keysArr) =>
                        var ki = 0
                        while ki < keysArr.getLength do {
                          val key = keysArr.get(ki)
                          val keyStr = key.toString
                          val descResult = callProxyTrap(
                            handler,
                            "getOwnPropertyDescriptor",
                            Array(proxyTarget, key)
                          )
                          descResult match {
                            case JSValue.Object(descObj) =>
                              descObj.getOwnProperty("enumerable") match {
                                case Some(JSValue.Bool(true)) =>
                                  // Get value via get trap or descriptor value
                                  val value = descObj
                                    .getOwnProperty("value")
                                    .getOrElse(JSValue.Undefined)
                                  key match {
                                    case JSValue.Symbol(symId) =>
                                      setTargetSymbolProp(symId, value)
                                    case _ => setTargetProp(keyStr, value)
                                  }
                                case _ => ()
                              }
                            case _ => ()
                          }
                          ki += 1
                        }
                      case _ => ()
                    }
                  case None =>
                    // Regular object source
                    val stringKeys = obj
                      .getOwnPropertyKeys()
                      .filter(k => obj.isEncodedSymbolKey(k).isEmpty)
                    val symbolIds = obj.getOwnSymbolPropertyIds()
                    for key <- stringKeys do {
                      val value = obj.getOwnPropertyDescriptor(key) match {
                        case Some((_, attrs)) if attrs.getter.isDefined =>
                          invokeGetter(attrs.getter.get, JSValue.Object(obj))
                        case Some((v, _)) => v
                        case None         => JSValue.Undefined
                      }
                      setTargetProp(key, value)
                    }
                    for symId <- symbolIds do {
                      val value =
                        obj.getOwnSymbolPropertyDescriptor(symId) match {
                          case Some((_, attrs)) if attrs.getter.isDefined =>
                            invokeGetter(attrs.getter.get, JSValue.Object(obj))
                          case Some((v, _)) => v
                          case None         => JSValue.Undefined
                        }
                      setTargetSymbolProp(symId, value)
                    }
                }
              case func: JSValue.Function =>
                val stringKeys = func.funcObj
                  .getOwnPropertyKeys()
                  .filter(k => func.funcObj.isEncodedSymbolKey(k).isEmpty)
                val symbolIds = func.funcObj.getOwnSymbolPropertyIds()
                for key <- stringKeys do {
                  val value = func.funcObj.getOwnPropertyDescriptor(key) match {
                    case Some((_, attrs)) if attrs.getter.isDefined =>
                      invokeGetter(attrs.getter.get, func)
                    case Some((v, _)) => v
                    case None         => JSValue.Undefined
                  }
                  setTargetProp(key, value)
                }
                for symId <- symbolIds do {
                  val value =
                    func.funcObj.getOwnSymbolPropertyDescriptor(symId) match {
                      case Some((_, attrs)) if attrs.getter.isDefined =>
                        invokeGetter(attrs.getter.get, func)
                      case Some((v, _)) => v
                      case None         => JSValue.Undefined
                    }
                  setTargetSymbolProp(symId, value)
                }
              case JSValue.JSArrayVal(arr) =>
                var idx = 0
                while idx < arr.getLength do {
                  if arr.hasIndex(idx) then
                    setTargetProp(idx.toString, arr.get(idx))
                  idx += 1
                }
              case JSValue.JSStr(s) =>
                // String primitives contribute their indexed characters
                var idx = 0
                while idx < s.length do {
                  setTargetProp(
                    idx.toString,
                    JSValue.fromString(s.charAt(idx).toString)
                  )
                  idx += 1
                }
              case _ => ()
            }

          target
        }
    )

    val objectCreate = NativeFunction(
      name = "create",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        val proto = if args.length > offset then args(offset) else JSValue.Null
        val obj =
          proto match {
            case JSValue.Null =>
              quickjs.objmodel.JSObject(prototype = null, extensible = true)
            case JSValue.Object(p) =>
              quickjs.objmodel.JSObject(prototype = p, extensible = true)
            case func: JSValue.Function =>
              quickjs.objmodel
                .JSObject(prototype = func.funcObj, extensible = true)
            case _ =>
              ctx.throwTypeError("Object.create called with invalid prototype")
          }
        JSValue.Object(obj)
    )

    val objectDefineProperties = NativeFunction(
      name = "defineProperties",
      impl = (args, ctx) =>
        if args.length < 3 then JSValue.Undefined
        else {
          val offset = if args.length >= 4 then 1 else 0
          val target = args(offset)
          val descriptors = args(offset + 1)
          given JSContext = ctx
          descriptors match {
            case JSValue.Object(descObj) =>
              descObj.getAllProperties.keys.foreach { key =>
                val descriptor = descObj.get(key)
                definePropertyOnTarget(target, key, descriptor)
              }
            case func: JSValue.Function =>
              func.funcObj.getAllProperties.keys.foreach { key =>
                val descriptor = func.funcObj.get(key)
                definePropertyOnTarget(target, key, descriptor)
              }
            case _ => ()
          }
          target
        }
    )

    val objectHasOwn = NativeFunction(
      name = "hasOwn",
      impl = (args, ctx) =>
        if args.length < 2 then JSValue.Bool(false)
        else {
          val offset = if args.length >= 3 then 1 else 0
          val target = args(offset)
          val key = args(offset + 1).toString
          given JSContext = ctx
          JSValue.fromBoolean(hasOwnKey(target, key))
        }
    )

    val objectFromEntries = NativeFunction(
      name = "fromEntries",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then
          JSValue.Object(
            quickjs.objmodel
              .JSObject(prototype = ctx.objectPrototype, extensible = true)
          )
        else {
          val result = quickjs.objmodel
            .JSObject(prototype = ctx.objectPrototype, extensible = true)
          args(offset) match {
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do {
                arr.get(i) match {
                  case JSValue.JSArrayVal(pair) =>
                    if pair.getLength >= 2 then {
                      val key = pair.get(0).toString
                      val value = pair.get(1)
                      result.set(key, value)(using ctx)
                    }
                  case _ => ()
                }
                i += 1
              }
            case _ =>
              ctx.throwTypeError("Object.fromEntries expects an array")
          }
          JSValue.Object(result)
        }
    )

    val objectPrototypeHasOwnProperty = NativeFunction(
      name = "hasOwnProperty",
      impl = (args, ctx) =>
        if args.isEmpty then
          ctx.throwTypeError(
            "Object.prototype.hasOwnProperty called on null or undefined"
          )
        else {
          val rawKey =
            if args.length > 1 then args(1) else JSValue.fromString("undefined")
          val target = args(0)
          target match {
            case JSValue.Null | JSValue.Undefined =>
              ctx.throwTypeError(
                "Object.prototype.hasOwnProperty called on null or undefined"
              )
            case _ =>
              given JSContext = ctx
              rawKey match {
                case JSValue.Symbol(sym) =>
                  JSValue.fromBoolean(hasOwnSymbolKey(target, sym))
                case _ =>
                  JSValue.fromBoolean(hasOwnKey(target, rawKey.toString))
              }
          }
        }
    )

    val objectValues = NativeFunction(
      name = "values",
      impl = (args, ctx) =>
        given JSContext = ctx
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        val result = JSArray.empty()
        objOf(target) match {
          case Some(o) =>
            o.getOwnPropertyKeys().foreach(k => result.push(o.get(k)))
          case None =>
            target match {
              case JSValue.JSArrayVal(arr) =>
                for i <- 0 until arr.getLength do result.push(arr.get(i))
              case _ => ()
            }
        }
        JSValue.JSArrayVal(result)
    )

    val objectEntries = NativeFunction(
      name = "entries",
      impl = (args, ctx) =>
        given JSContext = ctx
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        val result = JSArray.empty()
        def pushEntry(key: String, value: JSValue): Unit = {
          val pair = JSArray.empty(); pair.push(JSValue.fromString(key));
          pair.push(value)
          result.push(JSValue.JSArrayVal(pair))
        }
        objOf(target) match {
          case Some(o) =>
            o.getOwnPropertyKeys().foreach(k => pushEntry(k, o.get(k)))
          case None =>
            target match {
              case JSValue.JSArrayVal(arr) =>
                for i <- 0 until arr.getLength do
                  pushEntry(i.toString, arr.get(i))
              case _ => ()
            }
        }
        JSValue.JSArrayVal(result)
    )

    val objectPrototypeToString = NativeFunction(
      name = "toString",
      impl = (_, _) => JSValue.fromString("[object Object]")
    )

    // Object.freeze/seal/isFrozen/isSealed/preventExtensions/isExtensible — extract common pattern
    val objectFreeze = NativeFunction(
      "freeze",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        objOf(target).foreach(_.freeze())
        target
    )
    val objectSeal = NativeFunction(
      "seal",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        objOf(target).foreach(_.seal())
        target
    )
    val objectPreventExtensions = NativeFunction(
      "preventExtensions",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        objOf(target).foreach(_.preventExtensions())
        target
    )
    val objectIsFrozen = NativeFunction(
      "isFrozen",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        JSValue.Bool(objOf(target).map(_.checkFrozen()).getOrElse(true))
    )
    val objectIsSealed = NativeFunction(
      "isSealed",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        JSValue.Bool(objOf(target).map(_.checkSealed()).getOrElse(true))
    )
    val objectIsExtensible = NativeFunction(
      "isExtensible",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        JSValue.Bool(objOf(target).map(_.isExtensible).getOrElse(false))
    )

    given JSContext = ctx
    val objectConstructorOpt =
      ctx.global.get("Object") match {
        case JSValue.Native(cons: quickjs.value.NativeConstructor) => Some(cons)
        case _                                                     => None
      }

    objectConstructorOpt.foreach { cons =>
      cons.funcObj.set("setPrototypeOf", JSValue.Native(setPrototypeOf))
      cons.funcObj.set("defineProperty", JSValue.Native(defineProperty))
      cons.funcObj.set(
        "defineProperties",
        JSValue.Native(objectDefineProperties)
      )
      def reg(name: String, f: NativeFunction): Unit =
        cons.funcObj.defineProperty(
          name,
          JSValue.Native(f),
          enumerable = false,
          writable = true,
          configurable = true
        )(using ctx)
      reg("is", objectIs)
      reg("getPrototypeOf", objectGetPrototypeOf)
      reg("getOwnPropertyDescriptor", objectGetOwnPropertyDescriptor)
      reg("getOwnPropertyDescriptors", objectGetOwnPropertyDescriptors)
      reg("getOwnPropertyNames", objectGetOwnPropertyNames)
      reg("keys", objectKeys)
      reg("assign", objectAssign)
      reg("create", objectCreate)
      reg("values", objectValues)
      reg("entries", objectEntries)
      reg("hasOwn", objectHasOwn)
      reg("fromEntries", objectFromEntries)
      // ES5 freeze/seal methods
      reg("freeze", objectFreeze)
      reg("seal", objectSeal)
      reg("isFrozen", objectIsFrozen)
      reg("isSealed", objectIsSealed)
      reg("preventExtensions", objectPreventExtensions)
      reg("isExtensible", objectIsExtensible)
    }
    ctx.objectPrototype.defineProperty(
      "toString",
      JSValue.Native(objectPrototypeToString),
      enumerable = false
    )
    ctx.objectPrototype.defineProperty(
      "hasOwnProperty",
      JSValue.Native(objectPrototypeHasOwnProperty),
      enumerable = false
    )

    val objectPrototypePropertyIsEnumerable = NativeFunction(
      name = "propertyIsEnumerable",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 1 then JSValue.Bool(false)
        else {
          val target = args(0)
          val rawKey = if args.length > 1 then args(1) else JSValue.Undefined
          target match {
            case JSValue.Object(obj) =>
              rawKey match {
                case JSValue.Symbol(sym) =>
                  obj.getOwnSymbolPropertyDescriptor(sym) match {
                    case Some((_, attrs)) => JSValue.Bool(attrs.enumerable)
                    case None             => JSValue.Bool(false)
                  }
                case _ =>
                  obj.getOwnPropertyDescriptor(rawKey.toString) match {
                    case Some((_, attrs)) => JSValue.Bool(attrs.enumerable)
                    case None             => JSValue.Bool(false)
                  }
              }
            case _ => JSValue.Bool(false)
          }
        }
    )
    ctx.objectPrototype.defineProperty(
      "propertyIsEnumerable",
      JSValue.Native(objectPrototypePropertyIsEnumerable),
      enumerable = false
    )

    // __proto__ accessor: getter returns Object.getPrototypeOf(this),
    // setter calls Object.setPrototypeOf(this, value)
    val protoGetter = NativeFunction(
      name = "get __proto__",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.lift(0) match
          case Some(JSValue.Object(obj)) =>
            obj.getPrototype match
              case null  => JSValue.Null
              case proto => JSValue.Object(proto)
          case _ =>
            ctx.throwTypeError("Object.prototype.__proto__ getter called on non-object")
    )
    val protoSetter = NativeFunction(
      name = "set __proto__",
      impl = (args, ctx) =>
        given JSContext = ctx
        val obj = args.lift(0)
        val valArg = args.lift(1).getOrElse(JSValue.Undefined)
        obj match
          case Some(JSValue.Object(o)) =>
            valArg match
              case JSValue.Null => o.setPrototype(null)
              case JSValue.Object(proto) => o.setPrototype(proto)
              case _ =>
                ctx.throwTypeError("Object.prototype.__proto__ setter: prototype must be object or null")
          case _ =>
            ctx.throwTypeError("Object.prototype.__proto__ setter called on non-object")
        JSValue.Undefined
    )
    ctx.objectPrototype.defineAccessorProperty(
      "__proto__",
      getter = Some(JSValue.Native(protoGetter)),
      setter = Some(JSValue.Native(protoSetter)),
      enumerable = false,
      configurable = true
    )
  }
}
