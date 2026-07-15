package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.interpreter.{Interpreter, PropertyAccess}
import quickjs.runtime.builtins.BuiltinHelpers.{
  extractJSObject,
  functionToBytecode,
  parsePropertyDescriptor,
  buildPropertyDescriptorObject,
  nativeArgs,
  callFunctionWithThis,
  toPropertyKey
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
        val targetOpt = obj.getOwnProperty("__proxy_target")
        val handlerOpt = obj.getOwnProperty("__proxy_handler")
        val revoked = obj.getOwnProperty("__proxy_revoked") match {
          case Some(JSValue.Bool(true)) => true
          case _                        => false
        }
        if (targetOpt.isDefined || handlerOpt.isDefined) &&
            (revoked || targetOpt.contains(JSValue.Null) || handlerOpt.contains(
              JSValue.Null
            ))
        then ctx.throwTypeError("Cannot perform operation on a revoked proxy")
        (
          targetOpt,
          handlerOpt
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

    def proxyTrap(
        handler: JSObject,
        trapName: String,
        args: Array[JSValue]
    )(using JSContext): Option[JSValue] =
      handler.get(trapName) match {
        case JSValue.Undefined => None
        case trap =>
          Some(
            BuiltinHelpers.callFunctionWithThis(
              trap,
              JSValue.Object(handler),
              args
            )
          )
      }

    def ownStringDescriptor(
        target: JSValue,
        key: String
    )(using JSContext): Option[(JSValue, JSObject.PropertyAttributes)] =
      objOf(target).flatMap { obj =>
        TypedArrayBuiltins
          .typedArrayIndexDescriptor(obj, key)
          .orElse(obj.getOwnPropertyDescriptor(key))
      }

    def ownKeyDescriptor(
        target: JSValue,
        key: JSValue
    )(using JSContext): Option[(JSValue, JSObject.PropertyAttributes)] =
      key match {
        case JSValue.Symbol(sym) =>
          objOf(target).flatMap(_.getOwnSymbolPropertyDescriptor(sym))
        case _ => ownStringDescriptor(target, key.toString)
      }

    def keyIdentity(key: JSValue): String =
      key match {
        case JSValue.Symbol(sym) => s"@@symbol:$sym"
        case _                   => s"string:${key.toString}"
      }

    def targetExtensible(target: JSValue): Boolean =
      objOf(target).exists(_.isExtensible)

    def validateProxyDefineProperty(
        target: JSValue,
        key: JSValue,
        pd: BuiltinHelpers.ParsedDescriptor
    )(using JSContext): Unit = {
      val existing = ownKeyDescriptor(target, key)
      val settingNotConfigurable = pd.configurable.contains(false)
      existing match {
        case None =>
          if !targetExtensible(target) || settingNotConfigurable then
            ctx.throwTypeError("proxy: inconsistent defineProperty")
        case Some((currentValue, attrs)) =>
          val existingIsAccessor =
            attrs.isAccessor || attrs.getter.isDefined || attrs.setter.isDefined
          if !attrs.configurable then {
            if pd.configurable.contains(true) then
              ctx.throwTypeError("proxy: inconsistent defineProperty")
            if pd.enumerable.exists(_ != attrs.enumerable) then
              ctx.throwTypeError("proxy: inconsistent defineProperty")
            if existingIsAccessor && pd.hasValueField then
              ctx.throwTypeError("proxy: inconsistent defineProperty")
            if !existingIsAccessor && pd.isAccessor then
              ctx.throwTypeError("proxy: inconsistent defineProperty")
            if existingIsAccessor then {
              if pd.hasGetter && pd.getter != attrs.getter then
                ctx.throwTypeError("proxy: inconsistent defineProperty")
              if pd.hasSetter && pd.setter != attrs.setter then
                ctx.throwTypeError("proxy: inconsistent defineProperty")
            }
            else if !attrs.writable then {
              if pd.writable.contains(true) then
                ctx.throwTypeError("proxy: inconsistent defineProperty")
              if pd.hasValue && pd.value.exists(_ != currentValue) then
                ctx.throwTypeError("proxy: inconsistent defineProperty")
            }
          }
          if attrs.configurable && settingNotConfigurable then
            ctx.throwTypeError("proxy: inconsistent defineProperty")
          if !existingIsAccessor && attrs.configurable && attrs.writable && pd.writable
              .contains(false)
          then ctx.throwTypeError("proxy: inconsistent defineProperty")
      }
    }

    def parsedDescriptorToObject(pd: BuiltinHelpers.ParsedDescriptor)(using
        JSContext
    ): JSValue = {
      val obj = JSObject(prototype = ctx.objectPrototype, extensible = true)
      pd.value.foreach(v => obj.set("value", v))
      pd.writable.foreach(v => obj.set("writable", JSValue.fromBoolean(v)))
      if pd.hasGetter then obj.set("get", pd.getter.getOrElse(JSValue.Undefined))
      if pd.hasSetter then obj.set("set", pd.setter.getOrElse(JSValue.Undefined))
      pd.enumerable.foreach(v => obj.set("enumerable", JSValue.fromBoolean(v)))
      pd.configurable.foreach(v =>
        obj.set("configurable", JSValue.fromBoolean(v))
      )
      JSValue.Object(obj)
    }

    def collectOwnKeys(value: JSValue)(using JSContext): Vector[JSValue] =
      value match {
        case JSValue.Object(o) =>
          val stringKeys =
            o.getAllProperties.keys
              .filterNot(_.startsWith("__proxy_"))
              .map(JSValue.fromString)
              .toVector
          val symbolKeys =
            o.getAllOwnSymbolPropertyIds().map(JSValue.Symbol.apply).toVector
          stringKeys ++ symbolKeys
        case JSValue.JSArrayVal(arr) =>
          arr.getOwnIndexKeys.map(i => JSValue.fromString(i.toString)) :+
            JSValue.fromString("length")
        case _ => Vector.empty
      }

    def validateProxyOwnKeys(
        target: JSValue,
        trapKeys: Vector[JSValue]
    )(using JSContext): Unit = {
      val trapIds = trapKeys.map(keyIdentity)
      if trapIds.distinct.length != trapIds.length then
        ctx.throwTypeError("proxy: duplicate property")
      val targetKeys = collectOwnKeys(target)
      val targetIds = targetKeys.map(keyIdentity).toSet
      val nonConfigurableKeys = targetKeys.filter(key =>
        ownKeyDescriptor(target, key).exists { case (_, attrs) =>
          !attrs.configurable
        }
      )
      nonConfigurableKeys.foreach { key =>
        if !trapIds.contains(keyIdentity(key)) then
          ctx.throwTypeError(
            "proxy: target property must be present in proxy ownKeys"
          )
      }
      if !targetExtensible(target) then
        trapKeys.foreach { key =>
          if !targetIds.contains(keyIdentity(key)) then
            ctx.throwTypeError(
              "proxy: property not present in target were returned by non extensible proxy"
            )
        }
    }

    def proxyOwnKeys(
        target: JSValue,
        handler: JSObject
    )(using JSContext): Vector[JSValue] =
      proxyTrap(handler, "ownKeys", Array(target)) match {
        case Some(JSValue.JSArrayVal(keysArr)) =>
          val keys = (0 until keysArr.getLength).map { i =>
            keysArr.get(i) match {
              case s @ JSValue.JSStr(_)  => s
              case s @ JSValue.Symbol(_) => s
              case _ =>
                ctx.throwTypeError("proxy: properties must be strings or symbols")
            }
          }.toVector
          validateProxyOwnKeys(target, keys)
          keys
        case Some(JSValue.Object(obj)) =>
          val length = obj.get("length").toNumber.toInt
          val keys = (0 until length).map { i =>
            obj.get(i.toString) match {
              case s @ JSValue.JSStr(_)  => s
              case s @ JSValue.Symbol(_) => s
              case _ =>
                ctx.throwTypeError("proxy: properties must be strings or symbols")
            }
          }.toVector
          validateProxyOwnKeys(target, keys)
          keys
        case Some(_) =>
          ctx.throwTypeError("proxy: ownKeys trap result is not an object")
        case None =>
          collectOwnKeys(target)
      }

    def proxyGetOwnPropertyDescriptor(
        target: JSValue,
        handler: JSObject,
        key: JSValue
    )(using JSContext): JSValue =
      proxyTrap(
        handler,
        "getOwnPropertyDescriptor",
        Array(target, key)
      ) match {
        case Some(JSValue.Undefined) =>
          ownKeyDescriptor(target, key) match {
            case Some((_, attrs))
                if !attrs.configurable || !targetExtensible(target) =>
              ctx.throwTypeError("proxy: inconsistent getOwnPropertyDescriptor")
            case _ => JSValue.Undefined
          }
        case Some(descObj @ JSValue.Object(_)) =>
          val pd = parsePropertyDescriptor(descObj)
          ownKeyDescriptor(target, key) match {
            case None if !targetExtensible(target) =>
              ctx.throwTypeError("proxy: inconsistent getOwnPropertyDescriptor")
            case Some((_, attrs)) =>
              if !attrs.configurable then
                validateProxyDefineProperty(target, key, pd)
              if pd.configurable.contains(false) && attrs.configurable then
                ctx.throwTypeError(
                  "proxy: inconsistent getOwnPropertyDescriptor"
                )
            case _ => ()
          }
          descObj
        case Some(_) =>
          ctx.throwTypeError("proxy: inconsistent getOwnPropertyDescriptor")
        case None =>
          buildPropertyDescriptorObject(key.toString, ownKeyDescriptor(target, key))
      }

    def ordinaryGetPropertyValue(
        source: JSValue,
        key: JSValue,
        receiver: JSValue
    )(using JSContext): JSValue =
      source match {
        case JSValue.JSArrayVal(arr) =>
          key match {
            case JSValue.Symbol(_) => JSValue.Undefined
            case _ =>
              val keyStr = key.toString
              if keyStr.forall(_.isDigit) then arr.get(keyStr.toInt)
              else arr.getProperty(keyStr).getOrElse(JSValue.Undefined)
          }
        case JSValue.JSStr(s) =>
          key match {
            case JSValue.Symbol(_) => JSValue.Undefined
            case _ =>
              val keyStr = key.toString
              if keyStr.forall(_.isDigit) then {
                val idx = keyStr.toInt
                if idx >= 0 && idx < s.length then
                  JSValue.fromString(s.charAt(idx).toString)
                else JSValue.Undefined
              }
              else JSValue.Undefined
          }
        case _ =>
          objOf(source) match {
            case Some(obj) =>
              key match {
                case JSValue.Symbol(sym) =>
                  obj.getSymbolPropertyDescriptorWithOwner(sym) match {
                    case Some((_, _, attrs)) if attrs.getter.isDefined =>
                      invokeGetter(attrs.getter.get, receiver)
                    case _ => obj.getSymbol(sym)
                  }
                case _ =>
                  val keyStr = key.toString
                  obj.getPropertyDescriptorWithOwner(keyStr) match {
                    case Some((_, _, attrs)) if attrs.getter.isDefined =>
                      invokeGetter(attrs.getter.get, receiver)
                    case _ => obj.get(keyStr)
                  }
              }
            case None => JSValue.Undefined
          }
      }

    def getPropertyValue(
        source: JSValue,
        key: JSValue,
        receiver: JSValue
    )(using JSContext): JSValue =
      isProxyValue(source) match {
        case Some((proxyTarget, handler)) =>
          proxyTrap(handler, "get", Array(proxyTarget, key, receiver)) match {
            case Some(value) => value
            case None        =>
              ordinaryGetPropertyValue(proxyTarget, key, receiver)
          }
        case None => ordinaryGetPropertyValue(source, key, receiver)
      }

    def preventExtensionsOnTarget(target: JSValue)(using JSContext): Boolean =
      objOf(target) match {
        case Some(o) =>
          o.preventExtensions()
          true
        case None => false
      }

    def proxyPreventExtensions(
        proxyTarget: JSValue,
        handler: JSObject
    )(using JSContext): Boolean =
      proxyTrap(handler, "preventExtensions", Array(proxyTarget)) match {
        case Some(result) =>
          val ok = result.toBoolean
          if ok && targetExtensible(proxyTarget) then
            ctx.throwTypeError("proxy: inconsistent preventExtensions")
          ok
        case None => preventExtensionsOnTarget(proxyTarget)
      }

    def proxyIsExtensible(
        proxyTarget: JSValue,
        handler: JSObject
    )(using JSContext): Boolean =
      proxyTrap(handler, "isExtensible", Array(proxyTarget)) match {
        case Some(result) =>
          val trapResult = result.toBoolean
          if trapResult != targetExtensible(proxyTarget) then
            ctx.throwTypeError("proxy: inconsistent isExtensible")
          trapResult
        case None => targetExtensible(proxyTarget)
      }

    def definePropertyOnObjectKey(
        obj: JSObject,
        key: JSValue,
        pd: BuiltinHelpers.ParsedDescriptor
    )(using JSContext): Boolean =
      key match {
        case JSValue.Symbol(sym) =>
          if pd.isAccessor then
            obj.defineSymbolAccessorPropertyDetailed(
              sym,
              pd.getter,
              pd.setter,
              pd.hasGetter,
              pd.hasSetter,
              pd.enumerable,
              pd.configurable
            )
          else
            obj.defineSymbolDataProperty(
              sym,
              pd.value,
              pd.enumerable,
              pd.writable,
              pd.configurable
            )
        case _ =>
          val keyStr = key.toString
          TypedArrayBuiltins.defineTypedArrayIndexProperty(obj, keyStr, pd) match {
            case Some(result) => result
            case None =>
              if pd.isAccessor then
                obj.defineAccessorPropertyDetailed(
                  keyStr,
                  pd.getter,
                  pd.setter,
                  pd.hasGetter,
                  pd.hasSetter,
                  pd.enumerable,
                  pd.configurable
                )
              else
                obj.defineDataProperty(
                  keyStr,
                  pd.value,
                  pd.enumerable,
                  pd.writable,
                  pd.configurable
                )
          }
      }

    def defineProxyProperty(
        proxyTarget: JSValue,
        handler: JSObject,
        key: JSValue,
        pd: BuiltinHelpers.ParsedDescriptor
    )(using JSContext): Boolean =
      proxyTrap(
        handler,
        "defineProperty",
        Array(proxyTarget, key, parsedDescriptorToObject(pd))
      ) match {
        case Some(result) =>
          val ok = result.toBoolean
          if ok then validateProxyDefineProperty(proxyTarget, key, pd)
          ok
        case None =>
          objOf(proxyTarget) match {
            case Some(o) => definePropertyOnObjectKey(o, key, pd)
            case None    => false
          }
      }

    def freezeOrSealProxy(
        proxyTarget: JSValue,
        handler: JSObject,
        freeze: Boolean
    )(using JSContext): Boolean = {
      if !proxyPreventExtensions(proxyTarget, handler) then false
      else {
        proxyOwnKeys(proxyTarget, handler).foreach { key =>
          proxyGetOwnPropertyDescriptor(proxyTarget, handler, key) match {
            case descObj @ JSValue.Object(_) =>
              val current = parsePropertyDescriptor(descObj)
              val pd =
                if freeze && !current.isAccessor then
                  BuiltinHelpers.ParsedDescriptor(
                    enumerable = None,
                    writable = Some(false),
                    configurable = Some(false),
                    getter = None,
                    setter = None,
                    value = None,
                    hasValue = false,
                    hasWritable = true,
                    hasGetter = false,
                    hasSetter = false
                  )
                else
                  BuiltinHelpers.ParsedDescriptor(
                    enumerable = None,
                    writable = None,
                    configurable = Some(false),
                    getter = None,
                    setter = None,
                    value = None,
                    hasValue = false,
                    hasWritable = false,
                    hasGetter = false,
                    hasSetter = false
                  )
              if !defineProxyProperty(proxyTarget, handler, key, pd) then
                ctx.throwTypeError("Cannot define property")
            case _ => ()
          }
        }
        true
      }
    }

    def isFrozenOrSealedProxy(
        proxyTarget: JSValue,
        handler: JSObject,
        frozen: Boolean
    )(using JSContext): Boolean = {
      var result = true
      proxyOwnKeys(proxyTarget, handler).foreach { key =>
        proxyGetOwnPropertyDescriptor(proxyTarget, handler, key) match {
          case descObj @ JSValue.Object(_) =>
            val pd = parsePropertyDescriptor(descObj)
            if !pd.configurable.contains(false) then result = false
            if frozen && !pd.isAccessor && !pd.writable.contains(false) then
              result = false
          case _ => ()
        }
      }
      result && !proxyIsExtensible(proxyTarget, handler)
    }

    def primitivePrototype(value: JSValue)(using JSContext): Option[JSObject] =
      value match {
        case JSValue.JSStr(_) =>
          ctx.global.get("String") match {
            case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
              Some(nc.prototype)
            case _ => Some(ctx.objectPrototype)
          }
        case JSValue.Int32(_) | JSValue.Float64(_) =>
          ctx.global.get("Number") match {
            case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
              Some(nc.prototype)
            case _ => Some(ctx.objectPrototype)
          }
        case JSValue.Bool(_) =>
          ctx.global.get("Boolean") match {
            case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
              Some(nc.prototype)
            case _ => Some(ctx.objectPrototype)
          }
        case JSValue.Symbol(_) => Some(ctx.symbolPrototype)
        case JSValue.BigInt(_) =>
          ctx.global.get("BigInt") match {
            case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
              Some(nc.prototype)
            case _ => Some(ctx.objectPrototype)
          }
        case JSValue.JSArrayVal(_) => Some(ctx.arrayPrototype)
        case _                     => None
      }

    def prototypeValueOf(target: JSValue, allowPrimitives: Boolean)(using
        JSContext
    ): JSValue =
      objOf(target) match {
        case Some(o) =>
          o.getPrototype match {
            case null  => JSValue.Null
            case proto => JSValue.Object(proto)
          }
        case None if allowPrimitives =>
          primitivePrototype(target) match {
            case Some(proto) => JSValue.Object(proto)
            case None        =>
              target match {
                case JSValue.Null | JSValue.Undefined =>
                  ctx.throwTypeError("Object.getPrototypeOf called on null or undefined")
                case _ => JSValue.Undefined
              }
          }
        case None => ctx.throwTypeError("Reflect.getPrototypeOf called on non-object")
      }

    def proxyGetPrototype(
        proxyTarget: JSValue,
        handler: JSObject
    )(using JSContext): JSValue =
      proxyTrap(handler, "getPrototypeOf", Array(proxyTarget)) match {
        case Some(result @ (JSValue.Object(_) | JSValue.Null)) =>
          if !targetExtensible(proxyTarget) then {
            val actual = prototypeValueOf(proxyTarget, allowPrimitives = false)
            if actual != result then
              ctx.throwTypeError("proxy: inconsistent prototype")
          }
          result
        case Some(_) =>
          ctx.throwTypeError("proxy: inconsistent prototype")
        case None =>
          prototypeValueOf(proxyTarget, allowPrimitives = false)
      }

    def normalizePrototypeValue(proto: JSValue)(using
        JSContext
    ): JSObject | Null =
      proto match {
        case JSValue.Null => null
        case _ =>
          objOf(proto).getOrElse(
            ctx.throwTypeError("Prototype must be an object or null")
          )
      }

    def setPrototypeOnTargetValue(target: JSValue, proto: JSValue)(using
        JSContext
    ): Boolean = {
      val protoObj = normalizePrototypeValue(proto)
      objOf(target) match {
        case Some(o) =>
          o.setPrototype(protoObj)
          true
        case None =>
          target match {
            case JSValue.Null | JSValue.Undefined =>
              ctx.throwTypeError("Object.setPrototypeOf called on non-object")
            case _ => true
          }
      }
    }

    def proxySetPrototype(
        proxyTarget: JSValue,
        handler: JSObject,
        proto: JSValue
    )(using JSContext): Boolean = {
      normalizePrototypeValue(proto)
      proxyTrap(handler, "setPrototypeOf", Array(proxyTarget, proto)) match {
        case Some(result) =>
          val ok = result.toBoolean
          if ok && !targetExtensible(proxyTarget) then {
            val actual = prototypeValueOf(proxyTarget, allowPrimitives = false)
            val normalizedProto = proto match {
              case JSValue.Null => JSValue.Null
              case _            => JSValue.Object(objOf(proto).get)
            }
            if actual != normalizedProto then
              ctx.throwTypeError("proxy: inconsistent prototype")
          }
          ok
        case None =>
          setPrototypeOnTargetValue(proxyTarget, proto)
      }
    }

    def ownPropertyExists(target: JSValue, key: JSValue)(using
        JSContext
    ): Boolean =
      isProxyValue(target) match {
        case Some((proxyTarget, handler)) =>
          proxyGetOwnPropertyDescriptor(proxyTarget, handler, key) match {
            case JSValue.Object(_)    => true
            case JSValue.Undefined    => false
            case _                    => false
          }
        case None =>
          key match {
            case JSValue.Symbol(sym) =>
              objOf(target).exists(_.getOwnSymbolProperty(sym).isDefined)
            case _ =>
              val keyStr = key.toString
              target match {
                case JSValue.JSArrayVal(arr) =>
                  if keyStr == "length" then true
                  else if isArrayIndexKey(keyStr) then {
                    val idx = keyStr.toInt
                    idx >= 0 && idx < arr.getLength
                  }
                  else arr.getOwnProperty(keyStr).isDefined
                case JSValue.JSStr(s) =>
                  if keyStr == "length" then true
                  else if isArrayIndexKey(keyStr) then {
                    val idx = keyStr.toInt
                    idx >= 0 && idx < s.length
                  }
                  else false
                case _ =>
                  objOf(target).exists(_.getOwnPropertyDescriptor(keyStr).isDefined)
              }
          }
      }

    def definePropertyOnTarget(
        target: JSValue,
        propKey: String,
        descriptor: JSValue
    )(using JSContext): JSValue = {
      val pd = parsePropertyDescriptor(descriptor)
      target match {
        case proxy @ JSValue.Object(_) if isProxyValue(proxy).isDefined =>
          val (proxyTarget, handler) = isProxyValue(proxy).get
          if pd.isAccessor && pd.hasValueField then
            ctx.throwTypeError(
              "Invalid property descriptor. Cannot have both accessors and a value"
            )
          proxyTrap(
            handler,
            "defineProperty",
            Array(
              proxyTarget,
              JSValue.fromString(propKey),
              parsedDescriptorToObject(pd)
            )
          ) match {
            case Some(result) =>
              if !result.toBoolean then ctx.throwTypeError("Cannot define property")
              validateProxyDefineProperty(
                proxyTarget,
                JSValue.fromString(propKey),
                pd
              )
              target
            case None =>
              definePropertyOnTarget(proxyTarget, propKey, descriptor)
          }
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
            if pd.isAccessor && pd.hasValueField then
              ctx.throwTypeError(
                "Invalid property descriptor. Cannot have both accessors and a value"
              )
            val ok =
              TypedArrayBuiltins.defineTypedArrayIndexProperty(obj, propKey, pd) match {
                case Some(result) => result
                case None =>
                  if pd.isAccessor then
                    obj.defineAccessorPropertyDetailed(
                      propKey,
                      pd.getter,
                      pd.setter,
                      pd.hasGetter,
                      pd.hasSetter,
                      pd.enumerable,
                      pd.configurable
                    )
                  else
                    obj.defineDataProperty(
                      propKey,
                      pd.value,
                      pd.enumerable,
                      pd.writable,
                      pd.configurable
                    )
              }
            if !ok then ctx.throwTypeError("Cannot define property")
            target
          }
          objOf(target)
            .map(applyDefine)
            .getOrElse(ctx.throwTypeError("Object.defineProperty called on non-object"))
      }
    }

    def definePropertyOnTargetKey(
        target: JSValue,
        key: JSValue,
        descriptor: JSValue
    )(using JSContext): JSValue =
      key match {
        case JSValue.Symbol(sym) =>
          val pd = parsePropertyDescriptor(descriptor)
          isProxyValue(target) match {
            case Some((proxyTarget, handler)) =>
              if !defineProxyProperty(proxyTarget, handler, key, pd) then
                ctx.throwTypeError("Cannot define property")
              target
            case None =>
              objOf(target) match {
                case Some(obj) =>
                  if pd.isAccessor && pd.hasValueField then
                    ctx.throwTypeError("Invalid property descriptor")
                  val ok = definePropertyOnObjectKey(obj, JSValue.Symbol(sym), pd)
                  if !ok then ctx.throwTypeError("Cannot define property")
                  target
                case None =>
                  ctx.throwTypeError("Object.defineProperty called on non-object")
              }
          }
        case _ =>
          definePropertyOnTarget(target, key.toString, descriptor)
      }

    def definePropertiesOnTarget(
        target: JSValue,
        descriptors: JSValue
    )(using JSContext): JSValue = {
      val isTargetObject =
        objOf(target).isDefined || target.isInstanceOf[JSValue.JSArrayVal] ||
          isProxyValue(target).isDefined
      if !isTargetObject then
        ctx.throwTypeError("Object.defineProperties called on non-object")

      val props = toObjectForAssign(descriptors, ctx)
      val keys = isProxyValue(props) match {
        case Some((proxyTarget, handler)) =>
          proxyOwnKeys(proxyTarget, handler).filter { key =>
            proxyGetOwnPropertyDescriptor(proxyTarget, handler, key) match {
              case descObj @ JSValue.Object(_) =>
                parsePropertyDescriptor(descObj).enumerable.contains(true)
              case _ => false
            }
          }
        case None =>
          props match {
            case JSValue.Object(obj) =>
              val stringKeys = obj.getAllProperties.keys
                .filterNot(_.startsWith("__proxy_"))
                .filter(k =>
                  obj
                    .getOwnPropertyDescriptor(k)
                    .exists(_._2.enumerable)
                )
                .map(JSValue.fromString)
                .toVector
              val symbolKeys =
                obj.getOwnSymbolPropertyIds().map(JSValue.Symbol.apply).toVector
              stringKeys ++ symbolKeys
            case JSValue.JSArrayVal(arr) =>
              arr.getOwnIndexKeys
                .map(i => JSValue.fromString(i.toString))
                .toVector
            case _ => Vector.empty
          }
      }

      keys.foreach { key =>
        val descriptor = getPropertyValue(props, key, props)
        definePropertyOnTargetKey(target, key, descriptor)
      }
      target
    }

    def getIteratorSymbolId(using JSContext): Int =
      ctx.global.get("Symbol") match {
        case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
          nc.funcObj.get("iterator")(using ctx) match {
            case JSValue.Symbol(id) => id
            case _ => ctx.throwTypeError("Symbol.iterator not available")
          }
        case _ => ctx.throwTypeError("Symbol not available")
      }

    def getMethodValue(obj: JSValue, name: String)(using JSContext): JSValue =
      getPropertyValue(obj, JSValue.fromString(name), obj) match {
        case JSValue.Undefined =>
          ctx.throwTypeError(s"$name is not a function")
        case method => method
      }

    def iteratorClose(iterator: JSValue)(using JSContext): Unit =
      try {
        val returnMethod =
          getPropertyValue(iterator, JSValue.fromString("return"), iterator)
        if returnMethod != JSValue.Undefined then
          callFunctionWithThis(returnMethod, iterator, Array.empty)
      }
      catch {
        case _: Exception => ()
      }

    def requireEntryObject(entry: JSValue)(using JSContext): Unit =
      entry match {
        case JSValue.Object(_) | JSValue.JSArrayVal(_) | _: JSValue.Function |
            JSValue.Native(_) =>
          ()
        case _ => ctx.throwTypeError("Iterator value is not an entry object")
      }

    def defineFromEntry(
        result: JSObject,
        entry: JSValue
    )(using JSContext): Unit = {
      requireEntryObject(entry)
      val key = getPropertyValue(entry, JSValue.fromString("0"), entry)
      val value = getPropertyValue(entry, JSValue.fromString("1"), entry)
      val pd = BuiltinHelpers.ParsedDescriptor(
        enumerable = Some(true),
        writable = Some(true),
        configurable = Some(true),
        getter = None,
        setter = None,
        value = Some(value),
        hasValue = true,
        hasWritable = true,
        hasGetter = false,
        hasSetter = false
      )
      if !definePropertyOnObjectKey(result, key, pd) then
        ctx.throwTypeError("Cannot define property")
    }

    def objectFromEntriesIterable(iterable: JSValue)(using
        JSContext
    ): JSValue = {
      val result =
        JSObject(prototype = ctx.objectPrototype, extensible = true)

      iterable match {
        case JSValue.JSArrayVal(arr) =>
          var i = 0
          while i < arr.getLength do {
            defineFromEntry(result, arr.get(i))
            i += 1
          }
          JSValue.Object(result)
        case _ =>
          val iteratorMethod =
            getPropertyValue(iterable, JSValue.Symbol(getIteratorSymbolId), iterable)
          if iteratorMethod == JSValue.Undefined then
            ctx.throwTypeError("Object.fromEntries expects an iterable")

          val iterator =
            callFunctionWithThis(iteratorMethod, iterable, Array.empty)
          val nextMethod = getMethodValue(iterator, "next")
          var loopCount = 0L
          try {
            while loopCount < 9007199254740991L do {
              val nextResult =
                callFunctionWithThis(nextMethod, iterator, Array.empty)
              requireEntryObject(nextResult)
              val done =
                getPropertyValue(nextResult, JSValue.fromString("done"), nextResult)
                  .toBoolean
              if done then return JSValue.Object(result)

              val entry =
                getPropertyValue(nextResult, JSValue.fromString("value"), nextResult)
              defineFromEntry(result, entry)
              loopCount += 1
            }
            iteratorClose(iterator)
            ctx.throwError("RangeError", "Maximum iteration count exceeded")
          }
          catch {
            case e: Exception =>
              iteratorClose(iterator)
              throw e
          }
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
          given JSContext = ctx
          isProxyValue(target) match {
            case Some((proxyTarget, handler)) =>
              if !proxySetPrototype(proxyTarget, handler, proto) then
                ctx.throwTypeError("proxy: bad prototype")
              target
            case None =>
              setPrototypeOnTargetValue(target, proto)
              target
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
          val propertyKey = toPropertyKey(rawKey)
          isProxyValue(target) match {
            case Some((proxyTarget, handler)) =>
              val pd = parsePropertyDescriptor(descriptor)
              if pd.isAccessor && pd.hasValueField then
                ctx.throwTypeError("Invalid property descriptor")
              proxyTrap(
                handler,
                "defineProperty",
                Array(proxyTarget, propertyKey, parsedDescriptorToObject(pd))
              ) match {
                case Some(result) =>
                  if !result.toBoolean then
                    ctx.throwTypeError("Cannot define property")
                  validateProxyDefineProperty(proxyTarget, propertyKey, pd)
                  target
                case None =>
                  propertyKey match {
                    case JSValue.Symbol(sym) =>
                      objOf(proxyTarget) match {
                        case Some(obj) =>
                          val ok =
                            if pd.isAccessor then
                              obj.defineSymbolAccessorPropertyDetailed(
                                sym,
                                pd.getter,
                                pd.setter,
                                pd.hasGetter,
                                pd.hasSetter,
                                pd.enumerable,
                                pd.configurable
                              )
                            else
                              obj.defineSymbolDataProperty(
                                sym,
                                pd.value,
                                pd.enumerable,
                                pd.writable,
                                pd.configurable
                              )
                          if !ok then ctx.throwTypeError("Cannot define property")
                          target
                        case None =>
                          ctx.throwTypeError(
                            "Object.defineProperty called on non-object"
                          )
                      }
                    case _ =>
                      definePropertyOnTarget(
                        proxyTarget,
                        propertyKey.toString,
                        descriptor
                      )
                  }
              }
            case None =>
              propertyKey match {
                case JSValue.Symbol(sym) =>
                  val pd = parsePropertyDescriptor(descriptor)
                  objOf(target) match {
                    case Some(obj) =>
                      if pd.isAccessor && pd.hasValueField then
                        ctx.throwTypeError("Invalid property descriptor")
                      if pd.isAccessor then {
                        val ok = obj.defineSymbolAccessorPropertyDetailed(
                          sym,
                          pd.getter,
                          pd.setter,
                          pd.hasGetter,
                          pd.hasSetter,
                          pd.enumerable,
                          pd.configurable
                        )
                        if !ok then ctx.throwTypeError("Cannot define property")
                      }
                      else {
                        val ok = obj.defineSymbolDataProperty(
                          sym,
                          pd.value,
                          pd.enumerable,
                          pd.writable,
                          pd.configurable
                        )
                        if !ok then ctx.throwTypeError("Cannot define property")
                      }
                      target
                    case None =>
                      ctx.throwTypeError("Object.defineProperty called on non-object")
                  }
                case _ =>
                  definePropertyOnTarget(target, propertyKey.toString, descriptor)
              }
          }
        },
      length = 3
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
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            val result = JSArray.empty()
            proxyOwnKeys(proxyTarget, handler).foreach {
              case JSValue.JSStr(k) => result.push(JSValue.fromString(k))
              case _                => ()
            }
            JSValue.JSArrayVal(result)
          case None => objOf(target) match {
          case Some(o) =>
            val result = JSArray.empty()
            TypedArrayBuiltins
              .typedArrayIndexKeys(o)
              .getOrElse(Seq.empty)
              .foreach(k => result.push(JSValue.fromString(k)))
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
        }
    )

    val objectGetOwnPropertySymbols = NativeFunction(
      name = "getOwnPropertySymbols",
      impl = (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        val result = JSArray.empty()
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyOwnKeys(proxyTarget, handler).foreach {
              case sym: JSValue.Symbol => result.push(sym)
              case _                   => ()
            }
          case None =>
            objOf(target) match {
              case Some(o) =>
                o.getAllOwnSymbolPropertyIds().foreach(id =>
                  result.push(JSValue.Symbol(id))
                )
              case None => ()
            }
        }
        JSValue.JSArrayVal(result)
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
          isProxyValue(target) match {
            case Some((proxyTarget, handler)) =>
              proxyGetOwnPropertyDescriptor(proxyTarget, handler, rawKey)
            case None =>
              rawKey match {
                case JSValue.Symbol(sym) =>
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
                        TypedArrayBuiltins
                          .typedArrayIndexDescriptor(o, propKey)
                          .orElse(o.getOwnPropertyDescriptor(propKey))
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
        def setDescriptorProperty(key: JSValue, descValue: JSValue): Unit =
          descValue match {
            case JSValue.Object(descObj) =>
              key match {
                case JSValue.Symbol(sym) =>
                  result.setSymbol(sym, JSValue.Object(descObj))
                case _ =>
                  result.set(key.toString, JSValue.Object(descObj))
              }
            case _ => ()
          }
        def pushStringDescriptor(
            key: String,
            desc: Option[(JSValue, JSObject.PropertyAttributes)]
        ): Unit =
          setDescriptorProperty(
            JSValue.fromString(key),
            buildPropertyDescriptorObject(key, desc)
          )
        def pushSymbolDescriptor(
            sym: Int,
            desc: Option[(JSValue, JSObject.PropertyAttributes)]
        ): Unit =
          setDescriptorProperty(
            JSValue.Symbol(sym),
            buildPropertyDescriptorObject(s"Symbol($sym)", desc)
          )
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyOwnKeys(proxyTarget, handler).foreach { key =>
              setDescriptorProperty(
                key,
                proxyGetOwnPropertyDescriptor(proxyTarget, handler, key)
              )
            }
          case None =>
            objOf(target) match {
              case Some(o) =>
                o.getAllProperties.keys.foreach(k =>
                  pushStringDescriptor(k, o.getOwnPropertyDescriptor(k))
                )
                o.getAllOwnSymbolPropertyIds().foreach(sym =>
                  pushSymbolDescriptor(sym, o.getOwnSymbolPropertyDescriptor(sym))
                )
              case None =>
                target match {
                  case JSValue.JSArrayVal(arr) =>
                    for i <- arr.getOwnIndexKeys do
                      pushStringDescriptor(
                        i.toString,
                        Some(
                          arr.get(i) -> JSObject.PropertyAttributes(enumerable =
                            true
                          )
                        )
                      )
                    pushStringDescriptor(
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
                      pushStringDescriptor(
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
        }
        JSValue.Object(result)
    )

    val objectKeys = NativeFunction(
      name = "keys",
      impl = (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            val result = JSArray.empty()
            proxyOwnKeys(proxyTarget, handler).foreach {
              case JSValue.JSStr(key) =>
                proxyGetOwnPropertyDescriptor(
                  proxyTarget,
                  handler,
                  JSValue.fromString(key)
                ) match {
                  case JSValue.Object(descObj) =>
                    descObj.get("enumerable") match {
                      case JSValue.Bool(true) =>
                        result.push(JSValue.fromString(key))
                      case _ => ()
                    }
                  case _ => ()
                }
              case _ => ()
            }
            JSValue.JSArrayVal(result)
          case None => objOf(target) match {
            case Some(o) =>
              val result = JSArray.empty()
              TypedArrayBuiltins
                .typedArrayIndexKeys(o)
                .getOrElse(Seq.empty)
                .foreach(k => result.push(JSValue.fromString(k)))
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
        }
    )

    val objectGetPrototypeOf = NativeFunction(
      name = "getPrototypeOf",
      impl = (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyGetPrototype(proxyTarget, handler)
          case None =>
            prototypeValueOf(target, allowPrimitives = true)
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

          def getProxySourceValue(
              source: JSValue,
              proxyTarget: JSValue,
              handler: JSObject,
              key: JSValue
          ): JSValue =
            proxyTrap(handler, "get", Array(proxyTarget, key, source)) match {
              case Some(value) => value
              case None =>
                objOf(proxyTarget) match {
                  case Some(obj) =>
                    key match {
                      case JSValue.Symbol(symId) =>
                        obj.getSymbolPropertyDescriptorWithOwner(symId) match {
                          case Some((_, _, attrs)) if attrs.getter.isDefined =>
                            invokeGetter(attrs.getter.get, source)
                          case _ => obj.getSymbol(symId)
                        }
                      case _ =>
                        val keyStr = key.toString
                        obj.getPropertyDescriptorWithOwner(keyStr) match {
                          case Some((_, _, attrs)) if attrs.getter.isDefined =>
                            invokeGetter(attrs.getter.get, source)
                          case _ => obj.get(keyStr)
                        }
                    }
                  case None => JSValue.Undefined
                }
            }

          for i <- (offset + 1) until args.length do
            args(i) match {
              case src @ JSValue.Object(obj) =>
                isProxyValue(src) match {
                  case Some((proxyTarget, handler)) =>
                    proxyOwnKeys(proxyTarget, handler).foreach { key =>
                      proxyGetOwnPropertyDescriptor(
                        proxyTarget,
                        handler,
                        key
                      ) match {
                        case descObj @ JSValue.Object(_) =>
                          val pd = parsePropertyDescriptor(descObj)
                          if pd.enumerable.contains(true) then {
                            val value =
                              getProxySourceValue(src, proxyTarget, handler, key)
                            key match {
                              case JSValue.Symbol(symId) =>
                                setTargetSymbolProp(symId, value)
                              case _ => setTargetProp(key.toString, value)
                            }
                          }
                        case _ => ()
                      }
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
        val descriptors =
          if args.length > offset + 1 then args(offset + 1) else JSValue.Undefined
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
        val result = JSValue.Object(obj)
        if descriptors != JSValue.Undefined then {
          given JSContext = ctx
          definePropertiesOnTarget(result, descriptors)
        }
        result,
      length = 2
    )

    val objectDefineProperties = NativeFunction(
      name = "defineProperties",
      impl = (args, ctx) =>
        if args.length < 3 then
          ctx.throwTypeError("Object.defineProperties requires target and descriptors")
        else {
          val offset = if args.length >= 3 then 1 else 0
          val target = args(offset)
          val descriptors = args(offset + 1)
          given JSContext = ctx
          definePropertiesOnTarget(target, descriptors)
        },
      length = 2
    )

    val objectHasOwn = NativeFunction(
      name = "hasOwn",
      impl = (args, ctx) =>
        if args.length < 1 then
          ctx.throwTypeError("Object.hasOwn called on null or undefined")
        else {
          val offset = if args.length >= 3 then 1 else 0
          val target = args(offset)
          val key =
            if args.length > offset + 1 then args(offset + 1)
            else JSValue.fromString("undefined")
          given JSContext = ctx
          target match {
            case JSValue.Null | JSValue.Undefined =>
              ctx.throwTypeError("Object.hasOwn called on null or undefined")
            case _ =>
              JSValue.fromBoolean(ownPropertyExists(target, key))
          }
        }
    )

    val objectFromEntries = NativeFunction(
      name = "fromEntries",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then
          ctx.throwTypeError("Object.fromEntries expects an iterable")
        else {
          given JSContext = ctx
          objectFromEntriesIterable(args(offset))
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
              JSValue.fromBoolean(ownPropertyExists(target, rawKey))
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
        if target == JSValue.Null || target == JSValue.Undefined then
          ctx.throwTypeError("Object.values called on null or undefined")
        val result = JSArray.empty()
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyOwnKeys(proxyTarget, handler).foreach {
              case key @ JSValue.JSStr(_) =>
                proxyGetOwnPropertyDescriptor(proxyTarget, handler, key) match {
                  case descObj @ JSValue.Object(_) =>
                    val pd = parsePropertyDescriptor(descObj)
                    if pd.enumerable.contains(true) then
                      result.push(getPropertyValue(target, key, target))
                  case _ => ()
                }
              case _ => ()
            }
          case None =>
            objOf(target) match {
              case Some(o) =>
                o.getOwnPropertyKeys()
                  .filter(k => o.isEncodedSymbolKey(k).isEmpty)
                  .foreach(k =>
                    result.push(
                      getPropertyValue(target, JSValue.fromString(k), target)
                    )
                  )
              case None =>
                target match {
                  case JSValue.JSArrayVal(arr) =>
                    for i <- 0 until arr.getLength do result.push(arr.get(i))
                  case JSValue.JSStr(s) =>
                    for i <- 0 until s.length do
                      result.push(JSValue.fromString(s.charAt(i).toString))
                  case _ => ()
                }
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
        if target == JSValue.Null || target == JSValue.Undefined then
          ctx.throwTypeError("Object.entries called on null or undefined")
        val result = JSArray.empty()
        def pushEntry(key: String, value: JSValue): Unit = {
          val pair = JSArray.empty(); pair.push(JSValue.fromString(key));
          pair.push(value)
          result.push(JSValue.JSArrayVal(pair))
        }
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyOwnKeys(proxyTarget, handler).foreach {
              case key @ JSValue.JSStr(keyStr) =>
                proxyGetOwnPropertyDescriptor(proxyTarget, handler, key) match {
                  case descObj @ JSValue.Object(_) =>
                    val pd = parsePropertyDescriptor(descObj)
                    if pd.enumerable.contains(true) then
                      pushEntry(keyStr, getPropertyValue(target, key, target))
                  case _ => ()
                }
              case _ => ()
            }
          case None =>
            objOf(target) match {
              case Some(o) =>
                o.getOwnPropertyKeys()
                  .filter(k => o.isEncodedSymbolKey(k).isEmpty)
                  .foreach(k =>
                    pushEntry(
                      k,
                      getPropertyValue(target, JSValue.fromString(k), target)
                    )
                  )
              case None =>
                target match {
                  case JSValue.JSArrayVal(arr) =>
                    for i <- 0 until arr.getLength do
                      pushEntry(i.toString, arr.get(i))
                  case JSValue.JSStr(s) =>
                    for i <- 0 until s.length do
                      pushEntry(
                        i.toString,
                        JSValue.fromString(s.charAt(i).toString)
                      )
                  case _ => ()
                }
            }
        }
        JSValue.JSArrayVal(result)
    )

    val objectPrototypeToString = NativeFunction(
      name = "toString",
      length = 0,
      impl = (args, ctx) =>
        val receiver = args.headOption.getOrElse(JSValue.Undefined)
        val tag = receiver match {
          case JSValue.Undefined     => "Undefined"
          case JSValue.Null          => "Null"
          case JSValue.JSArrayVal(_) => "Array"
          case _: JSValue.Function   => "Function"
          case JSValue.Native(_)     => "Function"
          case JSValue.JSStr(_)      => "String"
          case JSValue.Bool(_)       => "Boolean"
          case _: JSValue.Number     => "Number"
          case JSValue.BigInt(_)     => "BigInt"
          case JSValue.Symbol(_)     => "Symbol"
          case JSValue.Object(obj) if obj.getOwnProperty("__promise")(using ctx).isDefined => "Promise"
          case _ => "Object"
        }
        JSValue.fromString(s"[object $tag]")
    )

    // Object.freeze/seal/isFrozen/isSealed/preventExtensions/isExtensible — extract common pattern
    val objectFreeze = NativeFunction(
      "freeze",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            if !freezeOrSealProxy(proxyTarget, handler, freeze = true) then
              ctx.throwTypeError("proxy preventExtensions handler returned false")
          case None =>
            objOf(target).foreach(_.freeze())
        }
        target
    )
    val objectSeal = NativeFunction(
      "seal",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            if !freezeOrSealProxy(proxyTarget, handler, freeze = false) then
              ctx.throwTypeError("proxy preventExtensions handler returned false")
          case None =>
            objOf(target).foreach(_.seal())
        }
        target
    )
    val objectPreventExtensions = NativeFunction(
      "preventExtensions",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            if !proxyPreventExtensions(proxyTarget, handler) then
              ctx.throwTypeError("proxy preventExtensions handler returned false")
          case None =>
            objOf(target).foreach(_.preventExtensions())
        }
        target
    )
    val objectIsFrozen = NativeFunction(
      "isFrozen",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            JSValue.Bool(isFrozenOrSealedProxy(proxyTarget, handler, frozen = true))
          case None =>
            JSValue.Bool(objOf(target).map(_.checkFrozen()).getOrElse(true))
        }
    )
    val objectIsSealed = NativeFunction(
      "isSealed",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            JSValue.Bool(isFrozenOrSealedProxy(proxyTarget, handler, frozen = false))
          case None =>
            JSValue.Bool(objOf(target).map(_.checkSealed()).getOrElse(true))
        }
    )
    val objectIsExtensible = NativeFunction(
      "isExtensible",
      (args, ctx) =>
        val target = args
          .lift(if args.length >= 2 then 1 else 0)
          .getOrElse(JSValue.Undefined)
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            JSValue.Bool(proxyIsExtensible(proxyTarget, handler))
          case None =>
            JSValue.Bool(objOf(target).map(_.isExtensible).getOrElse(false))
        }
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
      reg("getOwnPropertySymbols", objectGetOwnPropertySymbols)
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
          objOf(target) match {
            case Some(obj) =>
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
            case None => JSValue.Bool(false)
          }
        }
    )
    ctx.objectPrototype.defineProperty(
      "propertyIsEnumerable",
      JSValue.Native(objectPrototypePropertyIsEnumerable),
      enumerable = false
    )

    def legacyLookupAccessor(getter: Boolean): NativeFunction =
      NativeFunction(
        name = if getter then "__lookupGetter__" else "__lookupSetter__",
        impl = (args, ctx) =>
          given JSContext = ctx
          val target = args.headOption.getOrElse(JSValue.Undefined)
          if target == JSValue.Null || target == JSValue.Undefined then
            ctx.throwTypeError("Cannot convert null or undefined to object")
          val key = args.lift(1).getOrElse(JSValue.Undefined).toString
          objOf(target) match {
            case Some(obj) =>
              obj.getPropertyDescriptor(key) match {
                case Some((_, attrs)) =>
                  if getter then attrs.getter.getOrElse(JSValue.Undefined)
                  else attrs.setter.getOrElse(JSValue.Undefined)
                case None => JSValue.Undefined
              }
            case None => JSValue.Undefined
          }
      )

    ctx.objectPrototype.defineProperty(
      "__lookupGetter__",
      JSValue.Native(legacyLookupAccessor(getter = true)),
      enumerable = false
    )
    ctx.objectPrototype.defineProperty(
      "__lookupSetter__",
      JSValue.Native(legacyLookupAccessor(getter = false)),
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
