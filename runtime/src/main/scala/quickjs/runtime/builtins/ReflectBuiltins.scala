package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers.{
  extractJSObject,
  functionToBytecode,
  parsePropertyDescriptor,
  buildPropertyDescriptorObject,
  callFunctionWithThis
}

/** Reflect built-in: Reflect.get, set, has, deleteProperty, ownKeys, etc. */
object ReflectBuiltins {
  import quickjs.objmodel.{JSObject, JSArray}

  def initialize(ctx: JSContext): Unit = {
    given JSContext = ctx

    val reflectObj = JSObject(prototype = ctx.objectPrototype, extensible = true)

    // Helper to check if value is an object (including functions)
    def isObject(value: JSValue): Boolean =
      extractJSObject(value).isDefined || value.isInstanceOf[JSValue.JSArrayVal]

    // Helper to extract JSObject from various value types
    def objOf(value: JSValue): Option[JSObject] = extractJSObject(value)

    def isArrayIndexKey(key: String): Boolean =
      BuiltinHelpers.isArrayIndexKey(key)

    def arrayIndexFromKey(key: String): Option[Long] =
      BuiltinHelpers.arrayIndexFromKey(key)

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

    def proxyTrap(
        handler: JSObject,
        trapName: String,
        args: Array[JSValue]
    )(using JSContext): Option[JSValue] =
      handler.get(trapName) match {
        case JSValue.Undefined => None
        case trap =>
          Some(callFunctionWithThis(trap, JSValue.Object(handler), args))
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
      target match {
        case JSValue.JSArrayVal(arr) =>
          key match {
            case JSValue.Symbol(_) => None
            case _                 =>
              val keyStr = key.toString
              if isArrayIndexKey(keyStr) then
                arr.getOwnIndexDescriptor(arrayIndexFromKey(keyStr).get)
              else if keyStr == "length" then
                Some(
                  arr.getLengthValue -> JSObject.PropertyAttributes(
                    enumerable = false,
                    writable = arr.isLengthWritable,
                    configurable = false
                  )
                )
              else arr.getOwnPropertyDescriptor(keyStr)
          }
        case _ =>
          key match {
            case JSValue.Symbol(sym) =>
              objOf(target).flatMap(_.getOwnSymbolPropertyDescriptor(sym))
            case _ => ownStringDescriptor(target, key.toString)
          }
      }

    def keyIdentity(key: JSValue): String =
      key match {
        case JSValue.Symbol(sym) => s"@@symbol:$sym"
        case _                   => s"string:${key.toString}"
      }

    def targetExtensible(target: JSValue): Boolean =
      objOf(target).exists(_.isExtensible)

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

    def prototypeValueOf(target: JSValue)(using JSContext): JSValue =
      objOf(target) match {
        case Some(o) =>
          o.getPrototype match {
            case null  => JSValue.Null
            case proto => JSValue.Object(proto)
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
            val actual = prototypeValueOf(proxyTarget)
            if actual != result then
              ctx.throwTypeError("proxy: inconsistent prototype")
          }
          result
        case Some(_) =>
          ctx.throwTypeError("proxy: inconsistent prototype")
        case None =>
          prototypeValueOf(proxyTarget)
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
        case None => false
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
            val actual = prototypeValueOf(proxyTarget)
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

    def definePropertyOnObject(
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
          val propertyKey = key.toString
          TypedArrayBuiltins.defineTypedArrayIndexProperty(obj, propertyKey, pd) match {
            case Some(result) => result
            case None =>
              if pd.isAccessor then
                obj.defineAccessorPropertyDetailed(
                  propertyKey,
                  pd.getter,
                  pd.setter,
                  pd.hasGetter,
                  pd.hasSetter,
                  pd.enumerable,
                  pd.configurable
                )
              else
                obj.defineDataProperty(
                  propertyKey,
                  pd.value,
                  pd.enumerable,
                  pd.writable,
                  pd.configurable
                )
          }
      }

    def definePropertyOnArray(
        arr: JSArray,
        key: JSValue,
        pd: BuiltinHelpers.ParsedDescriptor
    )(using JSContext): Boolean =
      key match {
        case JSValue.Symbol(_) => false
        case _                 =>
          val keyStr = key.toString
          if !isArrayIndexKey(keyStr) then false
          else {
            val idx = arrayIndexFromKey(keyStr).get
            val existingDesc = arr.getIndexAttributes(idx).map { attrs =>
              (arr.getRaw(idx), attrs)
            }
            val enumerable = pd.enumerable.getOrElse(
              existingDesc.map(_._2.enumerable).getOrElse(false)
            )
            val writable = pd.writable.getOrElse(
              existingDesc.map(_._2.writable).getOrElse(false)
            )
            val configurable = pd.configurable.getOrElse(
              existingDesc.map(_._2.configurable).getOrElse(false)
            )
            if pd.isAccessor then
              val getter = pd.getter.orElse(existingDesc.flatMap(_._2.getter))
              val setter = pd.setter.orElse(existingDesc.flatMap(_._2.setter))
              arr.defineIndexAccessor(idx, getter, setter, enumerable, configurable)
            else
              val value = pd.value.getOrElse(arr.getRaw(idx))
              arr.defineIndexProperty(idx, value, enumerable, writable, configurable)
          }
      }

    def descriptorObjectForKey(
        target: JSValue,
        key: JSValue
    )(using JSContext): JSValue =
      objOf(target) match {
        case None =>
          target match {
            case JSValue.JSArrayVal(arr) =>
              key match {
                case JSValue.Symbol(_) => JSValue.Undefined
                case _                 =>
                  val keyStr = key.toString
                  if isArrayIndexKey(keyStr) then
                    buildPropertyDescriptorObject(
                      keyStr,
                      arr.getOwnIndexDescriptor(arrayIndexFromKey(keyStr).get)
                    )
                  else if keyStr == "length" then
                    buildPropertyDescriptorObject(
                      keyStr,
                      Some(
                        arr.getLengthValue -> JSObject.PropertyAttributes(
                          enumerable = false,
                          writable = arr.isLengthWritable,
                          configurable = false
                        )
                      )
                    )
                  else
                    buildPropertyDescriptorObject(
                      keyStr,
                      arr.getOwnPropertyDescriptor(keyStr)
                    )
              }
            case _ => JSValue.Undefined
          }
        case Some(o) =>
          key match {
            case JSValue.Symbol(sym) =>
              buildPropertyDescriptorObject(
                s"Symbol($sym)",
                o.getOwnSymbolPropertyDescriptor(sym)
              )
            case _ =>
              val keyStr = key.toString
              buildPropertyDescriptorObject(
                keyStr,
                TypedArrayBuiltins
                  .typedArrayIndexDescriptor(o, keyStr)
                  .orElse(o.getOwnPropertyDescriptor(keyStr))
              )
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

    def callGetter(getter: JSValue, receiver: JSValue)(using
        JSContext
    ): JSValue =
      getter match {
        case func: JSValue.Function =>
          Interpreter().call(
            functionToBytecode(func),
            receiver,
            Array.empty,
            func.closure
          )
        case JSValue.Native(nf: quickjs.value.NativeFunction) =>
          nf.call(Array(receiver))
        case _ => JSValue.Undefined
      }

    def callSetter(setter: JSValue, receiver: JSValue, value: JSValue)(using
        JSContext
    ): Unit =
      setter match {
        case func: JSValue.Function =>
          Interpreter().call(
            functionToBytecode(func),
            receiver,
            Array(value),
            func.closure
          )
        case JSValue.Native(nf: quickjs.value.NativeFunction) =>
          nf.call(Array(receiver, value))
        case _ => ()
      }

    def ordinaryGet(target: JSValue, key: JSValue, receiver: JSValue)(using
        JSContext
    ): JSValue =
      objOf(target) match {
        case Some(o) =>
          key match {
            case JSValue.Symbol(sym) =>
              o.getSymbolPropertyDescriptorWithOwner(sym) match {
                case Some((_, _, attrs)) if attrs.getter.isDefined =>
                  callGetter(attrs.getter.get, receiver)
                case _ => o.getSymbol(sym)
              }
            case _ =>
              val propertyKey = key.toString
              o.getPropertyDescriptorWithOwner(propertyKey) match {
                case Some((_, _, attrs)) if attrs.getter.isDefined =>
                  callGetter(attrs.getter.get, receiver)
                case _ => o.get(propertyKey)
              }
          }
        case None => JSValue.Undefined
      }

    def ordinarySet(
        target: JSValue,
        key: JSValue,
        value: JSValue,
        receiver: JSValue
    )(using JSContext): Boolean =
      objOf(target) match {
        case Some(o) =>
          key match {
            case JSValue.Symbol(sym) =>
              o.getSymbolPropertyDescriptorWithOwner(sym) match {
                case Some((_, _, attrs)) if attrs.setter.isDefined =>
                  callSetter(attrs.setter.get, receiver, value)
                  true
                case _ => o.setSymbol(sym, value)
              }
            case _ =>
              val propertyKey = key.toString
              o.getPropertyDescriptorWithOwner(propertyKey) match {
                case Some((_, _, attrs)) if attrs.setter.isDefined =>
                  callSetter(attrs.setter.get, receiver, value)
                  true
                case _ => o.set(propertyKey, value)
              }
          }
        case None => false
      }

    def ordinaryHas(target: JSValue, key: JSValue)(using JSContext): Boolean =
      key match {
        case JSValue.Symbol(sym) =>
          objOf(target).exists(_.hasSymbolProperty(sym))
        case _ =>
          val propertyKey = key.toString
          objOf(target) match {
            case Some(o) => o.hasProperty(propertyKey)
            case None =>
              target match {
                case JSValue.JSArrayVal(arr) =>
                  if propertyKey == "length" then true
                  else if propertyKey.forall(_.isDigit) then {
                    val idx = propertyKey.toInt
                    idx >= 0 && idx < arr.getLength
                  }
                  else arr.getProperty(propertyKey).isDefined
                case _ => false
              }
          }
      }

    def ordinaryDelete(target: JSValue, key: JSValue)(using
        JSContext
    ): Boolean =
      objOf(target) match {
        case Some(o) =>
          key match {
            case JSValue.Symbol(sym) => o.deleteSymbolProperty(sym)
            case _                   => o.deleteProperty(key.toString)
          }
        case None => false
      }

    // Reflect.get(target, propertyKey[, receiver])
    val reflectGet = NativeFunction(
      name = "get",
      length = 2,
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.get requires at least 2 arguments")
        val (_, rest) = BuiltinHelpers.nativeArgs(args)
        val target = rest(0)
        val propertyKey = rest(1)
        if !isObject(target) then
          ctx.throwTypeError("Reflect.get called on non-object")
        given JSContext = ctx
        val receiver = if rest.length > 2 then rest(2) else target
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyTrap(
              handler,
              "get",
              Array(proxyTarget, propertyKey, receiver)
            ) match {
              case Some(result) =>
                ownKeyDescriptor(proxyTarget, propertyKey) match {
                  case Some((targetValue, attrs))
                      if !attrs.configurable && !attrs.writable &&
                        !(attrs.isAccessor || attrs.getter.isDefined || attrs.setter.isDefined) &&
                        result != targetValue =>
                    ctx.throwTypeError("proxy: inconsistent get")
                  case Some((_, attrs))
                      if !attrs.configurable &&
                        (attrs.isAccessor || attrs.getter.isDefined || attrs.setter.isDefined) &&
                        attrs.getter.isEmpty &&
                        result != JSValue.Undefined =>
                    ctx.throwTypeError("proxy: inconsistent get")
                  case _ => ()
                }
                result
              case None => ordinaryGet(proxyTarget, propertyKey, receiver)
            }
          case None => ordinaryGet(target, propertyKey, receiver)
        }
    )

    // Reflect.set(target, propertyKey, value[, receiver])
    val reflectSet = NativeFunction(
      name = "set",
      length = 3,
      impl = (args, ctx) =>
        if args.length < 3 then
          ctx.throwTypeError("Reflect.set requires at least 3 arguments")
        val (_, rest) = BuiltinHelpers.nativeArgs(args)
        val target = rest(0)
        val propertyKey = rest(1)
        val value = rest(2)
        if !isObject(target) then
          ctx.throwTypeError("Reflect.set called on non-object")
        given JSContext = ctx
        val receiver = if rest.length > 3 then rest(3) else target
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyTrap(
              handler,
              "set",
              Array(proxyTarget, propertyKey, value, receiver)
            ) match {
              case Some(result) =>
                val success = result.toBoolean
                if success then
                  ownKeyDescriptor(proxyTarget, propertyKey) match {
                    case Some((targetValue, attrs))
                        if !attrs.configurable && !attrs.writable &&
                          !(attrs.isAccessor || attrs.getter.isDefined || attrs.setter.isDefined) &&
                          value != targetValue =>
                      ctx.throwTypeError("proxy: inconsistent set")
                    case Some((_, attrs))
                        if !attrs.configurable &&
                          (attrs.isAccessor || attrs.getter.isDefined || attrs.setter.isDefined) &&
                          attrs.setter.isEmpty =>
                      ctx.throwTypeError("proxy: inconsistent set")
                    case _ => ()
                  }
                JSValue.Bool(success)
              case None =>
                JSValue.Bool(
                  ordinarySet(proxyTarget, propertyKey, value, receiver)
                )
            }
          case None =>
            JSValue.Bool(ordinarySet(target, propertyKey, value, receiver))
        }
    )

    // Reflect.has(target, propertyKey)
    val reflectHas = NativeFunction(
      name = "has",
      length = 2,
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.has requires 2 arguments")
        val (_, rest) = BuiltinHelpers.nativeArgs(args)
        val target = rest(0)
        val propertyKey = rest(1)
        if !isObject(target) then
          ctx.throwTypeError("Reflect.has called on non-object")
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyTrap(handler, "has", Array(proxyTarget, propertyKey)) match {
              case Some(result) =>
                val present = result.toBoolean
                if !present then
                  ownKeyDescriptor(proxyTarget, propertyKey) match {
                    case Some((_, attrs)) if !attrs.configurable =>
                      ctx.throwTypeError("proxy: inconsistent has")
                    case Some(_) if !targetExtensible(proxyTarget) =>
                      ctx.throwTypeError("proxy: inconsistent has")
                    case _ => ()
                  }
                JSValue.Bool(present)
              case None => JSValue.Bool(ordinaryHas(proxyTarget, propertyKey))
            }
          case None => JSValue.Bool(ordinaryHas(target, propertyKey))
        }
    )

    // Reflect.deleteProperty(target, propertyKey)
    val reflectDeleteProperty = NativeFunction(
      name = "deleteProperty",
      length = 2,
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.deleteProperty requires 2 arguments")
        val (_, rest) = BuiltinHelpers.nativeArgs(args)
        val target = rest(0)
        val propertyKey = rest(1)
        if !isObject(target) then
          ctx.throwTypeError("Reflect.deleteProperty called on non-object")
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyTrap(
              handler,
              "deleteProperty",
              Array(proxyTarget, propertyKey)
            ) match {
              case Some(result) =>
                val deleted = result.toBoolean
                if deleted then
                  ownKeyDescriptor(proxyTarget, propertyKey) match {
                    case Some((_, attrs)) if !attrs.configurable =>
                      ctx.throwTypeError("proxy: inconsistent deleteProperty")
                    case _ => ()
                  }
                JSValue.Bool(deleted)
              case None => JSValue.Bool(ordinaryDelete(proxyTarget, propertyKey))
            }
          case None => JSValue.Bool(ordinaryDelete(target, propertyKey))
        }
    )

    // Reflect.ownKeys(target)
    val reflectOwnKeys = NativeFunction(
      name = "ownKeys",
      length = 1,
      impl = (args, ctx) =>
        if args.length < 1 then
          ctx.throwTypeError("Reflect.ownKeys requires 1 argument")
        val (_, rest) = BuiltinHelpers.nativeArgs(args); val target = rest.head
        if !isObject(target) then
          ctx.throwTypeError("Reflect.ownKeys called on non-object")
        given JSContext = ctx
        val result = JSArray.empty()
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyTrap(handler, "ownKeys", Array(proxyTarget)) match {
              case Some(JSValue.JSArrayVal(keysArr)) =>
                val keys = (0 until keysArr.getLength).map { i =>
                  keysArr.get(i) match {
                    case s @ JSValue.JSStr(_)  => s
                    case s @ JSValue.Symbol(_) => s
                    case _ =>
                      ctx.throwTypeError(
                        "proxy: properties must be strings or symbols"
                      )
                  }
                }.toVector
                validateProxyOwnKeys(proxyTarget, keys)
                keys.foreach(result.push)
              case Some(JSValue.Object(obj)) =>
                val length = obj.get("length").toNumber.toInt
                val keys = (0 until length).map { i =>
                  obj.get(i.toString) match {
                    case s @ JSValue.JSStr(_)  => s
                    case s @ JSValue.Symbol(_) => s
                    case _ =>
                      ctx.throwTypeError(
                        "proxy: properties must be strings or symbols"
                      )
                  }
                }.toVector
                validateProxyOwnKeys(proxyTarget, keys)
                keys.foreach(result.push)
              case Some(_) =>
                ctx.throwTypeError("proxy: ownKeys trap result is not an object")
              case None =>
                collectOwnKeys(proxyTarget).foreach(result.push)
            }
          case None =>
            objOf(target) match {
              case Some(o) =>
                TypedArrayBuiltins
                  .typedArrayIndexKeys(o)
                  .getOrElse(Seq.empty)
                  .foreach(k => result.push(JSValue.fromString(k)))
                o.getAllProperties.keys.foreach(k =>
                  result.push(JSValue.fromString(k))
                )
                o.getAllOwnSymbolPropertyIds().foreach(sym =>
                  result.push(JSValue.Symbol(sym))
                )
              case None =>
                target match {
                  case JSValue.JSArrayVal(arr) =>
                    for i <- 0 until arr.getLength do
                      result.push(JSValue.fromString(i.toString))
                    result.push(JSValue.fromString("length"))
                  case _ => ()
                }
            }
        }
        JSValue.JSArrayVal(result)
    )

    // Reflect.getPrototypeOf(target)
    val reflectGetPrototypeOf = NativeFunction(
      name = "getPrototypeOf",
      length = 1,
      impl = (args, ctx) =>
        if args.length < 1 then
          ctx.throwTypeError("Reflect.getPrototypeOf requires 1 argument")
        val (_, rest) = BuiltinHelpers.nativeArgs(args); val target = rest.head
        if !isObject(target) then
          ctx.throwTypeError("Reflect.getPrototypeOf called on non-object")
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyGetPrototype(proxyTarget, handler)
          case None =>
            prototypeValueOf(target)
        }
    )

    // Reflect.setPrototypeOf(target, proto)
    val reflectSetPrototypeOf = NativeFunction(
      name = "setPrototypeOf",
      length = 2,
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.setPrototypeOf requires 2 arguments")
        val (_, rest) = BuiltinHelpers.nativeArgs(args)
        val target = rest(0); val proto = rest(1)
        if !isObject(target) then
          ctx.throwTypeError("Reflect.setPrototypeOf called on non-object")
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            JSValue.Bool(proxySetPrototype(proxyTarget, handler, proto))
          case None =>
            JSValue.Bool(setPrototypeOnTargetValue(target, proto))
        }
    )

    // Reflect.defineProperty(target, propertyKey, attributes)
    val reflectDefineProperty = NativeFunction(
      name = "defineProperty",
      length = 3,
      impl = (args, ctx) =>
        if args.length < 3 then
          ctx.throwTypeError("Reflect.defineProperty requires 3 arguments")
        val (_, rest) = BuiltinHelpers.nativeArgs(args)
        val target = rest(0); val propertyKey = rest(1)
        val attributes = rest(2)
        if !isObject(target) then
          ctx.throwTypeError("Reflect.defineProperty called on non-object")
        given JSContext = ctx
        val pd = parsePropertyDescriptor(attributes)
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyTrap(
              handler,
              "defineProperty",
              Array(
                proxyTarget,
                propertyKey,
                parsedDescriptorToObject(pd)
              )
            ) match {
              case Some(result) =>
                if !result.toBoolean then JSValue.Bool(false)
                else {
                  validateProxyDefineProperty(proxyTarget, propertyKey, pd)
                  JSValue.Bool(true)
                }
              case None =>
                objOf(proxyTarget) match {
                  case Some(o) =>
                    val ok = definePropertyOnObject(o, propertyKey, pd)
                    JSValue.Bool(ok)
                  case None =>
                    proxyTarget match {
                      case JSValue.JSArrayVal(arr) =>
                        JSValue.Bool(definePropertyOnArray(arr, propertyKey, pd))
                      case _ => JSValue.Bool(false)
                    }
                }
            }
          case None =>
            objOf(target) match {
              case Some(o) =>
                val ok = definePropertyOnObject(o, propertyKey, pd)
                JSValue.Bool(ok)
              case None =>
                target match {
                  case JSValue.JSArrayVal(arr) =>
                    JSValue.Bool(definePropertyOnArray(arr, propertyKey, pd))
                  case _ => JSValue.Bool(false)
                }
            }
        }
    )

    // Reflect.getOwnPropertyDescriptor(target, propertyKey)
    val reflectGetOwnPropertyDescriptor = NativeFunction(
      name = "getOwnPropertyDescriptor",
      length = 2,
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError(
            "Reflect.getOwnPropertyDescriptor requires 2 arguments"
          )
        val (_, rest) = BuiltinHelpers.nativeArgs(args)
        val target = rest(0); val propertyKey = rest(1)
        if !isObject(target) then
          ctx.throwTypeError(
            "Reflect.getOwnPropertyDescriptor called on non-object"
          )
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            proxyTrap(
              handler,
              "getOwnPropertyDescriptor",
              Array(proxyTarget, propertyKey)
            ) match {
              case Some(JSValue.Undefined) =>
                ownKeyDescriptor(proxyTarget, propertyKey) match {
                  case Some((_, attrs))
                      if !attrs.configurable || !targetExtensible(proxyTarget) =>
                    ctx.throwTypeError(
                      "proxy: inconsistent getOwnPropertyDescriptor"
                    )
                  case _ => JSValue.Undefined
                }
              case Some(descObj @ JSValue.Object(_)) =>
                val pd = parsePropertyDescriptor(descObj)
                val targetDesc = ownKeyDescriptor(proxyTarget, propertyKey)
                targetDesc match {
                  case None if !targetExtensible(proxyTarget) =>
                    ctx.throwTypeError(
                      "proxy: inconsistent getOwnPropertyDescriptor"
                    )
                  case Some((_, attrs)) =>
                    if !attrs.configurable then
                      validateProxyDefineProperty(proxyTarget, propertyKey, pd)
                    if pd.configurable.contains(false) && attrs.configurable then
                      ctx.throwTypeError(
                        "proxy: inconsistent getOwnPropertyDescriptor"
                      )
                  case _ => ()
                }
                descObj
              case Some(_) =>
                ctx.throwTypeError(
                  "proxy: inconsistent getOwnPropertyDescriptor"
                )
              case None =>
                descriptorObjectForKey(proxyTarget, propertyKey)
            }
          case None =>
            descriptorObjectForKey(target, propertyKey)
        }
    )

    // Reflect.isExtensible(target)
    val reflectIsExtensible = NativeFunction(
      name = "isExtensible",
      length = 1,
      impl = (args, ctx) =>
        if args.length < 1 then
          ctx.throwTypeError("Reflect.isExtensible requires 1 argument")
        val (_, rest) = BuiltinHelpers.nativeArgs(args); val target = rest.head
        if !isObject(target) then
          ctx.throwTypeError("Reflect.isExtensible called on non-object")
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            JSValue.Bool(proxyIsExtensible(proxyTarget, handler))
          case None =>
            objOf(target) match {
              case Some(o) => JSValue.Bool(o.isExtensible)
              case None    => JSValue.Bool(false)
            }
        }
    )

    // Reflect.preventExtensions(target)
    val reflectPreventExtensions = NativeFunction(
      name = "preventExtensions",
      length = 1,
      impl = (args, ctx) =>
        if args.length < 1 then
          ctx.throwTypeError("Reflect.preventExtensions requires 1 argument")
        val (_, rest) = BuiltinHelpers.nativeArgs(args); val target = rest.head
        if !isObject(target) then
          ctx.throwTypeError("Reflect.preventExtensions called on non-object")
        given JSContext = ctx
        isProxyValue(target) match {
          case Some((proxyTarget, handler)) =>
            JSValue.Bool(proxyPreventExtensions(proxyTarget, handler))
          case None =>
            JSValue.Bool(preventExtensionsOnTarget(target))
        }
    )

    // Reflect.apply(target, thisArgument, argumentsList)
    val reflectApply = NativeFunction(
      name = "apply",
      length = 3,
      impl = (args, ctx) =>
        if args.length < 3 then
          ctx.throwTypeError("Reflect.apply requires 3 arguments")
        val (_, rest) = BuiltinHelpers.nativeArgs(args)
        val target = rest(0); val thisArg = rest(1); val argumentsList = rest(2)
        def extractArgs(list: JSValue)(using JSContext): Array[JSValue] =
          list match {
            case JSValue.JSArrayVal(arr) =>
              (0 until arr.getLength).map(arr.get).toArray
            case JSValue.Object(obj) =>
              val len = obj.get("length").toNumber.toInt
              (0 until len).map(i => obj.get(i.toString)).toArray
            case _ =>
              ctx.throwTypeError(
                "Reflect.apply: argumentsList must be an object"
              )
          }
        given JSContext = ctx
        val funcArgs = extractArgs(argumentsList)
        def toArgArray(values: Array[JSValue]): JSValue = {
          val arr = JSArray.empty()
          values.foreach(arr.push)
          JSValue.JSArrayVal(arr)
        }
        def isCallable(value: JSValue): Boolean =
          value match {
            case _: JSValue.Function => true
            case JSValue.Native(_: quickjs.value.NativeFunction) => true
            case JSValue.Native(_: quickjs.value.NativeConstructor) => true
            case proxy @ JSValue.Object(_) =>
              isProxyValue(proxy).exists { case (proxyTarget, _) =>
                isCallable(proxyTarget)
              }
            case _ => false
          }
        def callAny(
            callable: JSValue,
            receiver: JSValue,
            callArgs: Array[JSValue]
        ): JSValue =
          callable match {
            case proxy @ JSValue.Object(_) if isProxyValue(proxy).isDefined =>
              val (proxyTarget, handler) = isProxyValue(proxy).get
              if !isCallable(proxyTarget) then
                ctx.throwTypeError("proxy target is not callable")
              proxyTrap(
                handler,
                "apply",
                Array(proxyTarget, receiver, toArgArray(callArgs))
              ) match {
                case Some(result) => result
                case None         => callAny(proxyTarget, receiver, callArgs)
              }
            case func: JSValue.Function =>
              Interpreter()
                .call(functionToBytecode(func), receiver, callArgs, func.closure)
            case JSValue.Native(nf: quickjs.value.NativeFunction) =>
              val argsWithThis = new Array[JSValue](callArgs.length + 1)
              argsWithThis(0) = receiver
              Array.copy(callArgs, 0, argsWithThis, 1, callArgs.length)
              nf.call(argsWithThis)
            case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
              nc.call(callArgs)
            case _ => ctx.throwTypeError("Reflect.apply called on non-callable")
          }
        callAny(target, thisArg, funcArgs)
    )

    // Reflect.construct(target, argumentsList[, newTarget])
    val reflectConstruct = NativeFunction(
      name = "construct",
      length = 2,
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.construct requires at least 2 arguments")
        val (_, rest) = BuiltinHelpers.nativeArgs(args)
        val target = rest(0); val argumentsList = rest(1)
        val funcArgs: Array[JSValue] = argumentsList match {
          case JSValue.JSArrayVal(arr) =>
            (0 until arr.getLength).map(arr.get).toArray
          case _ => Array.empty
        }
        given JSContext = ctx
        def toArgArray(values: Array[JSValue]): JSValue = {
          val arr = JSArray.empty()
          values.foreach(arr.push)
          JSValue.JSArrayVal(arr)
        }
        def isObjectLike(value: JSValue): Boolean =
          value match {
            case JSValue.Object(_) | _: JSValue.Function | JSValue.JSArrayVal(_) |
                JSValue.Native(_) | _: JSValue.Promise | _: JSValue.Generator =>
              true
            case _ => false
          }
        def isConstructor(value: JSValue): Boolean =
          value match {
            case func: JSValue.Function => func.isConstructor
            case JSValue.Native(_: quickjs.value.NativeConstructor) => true
            case proxy @ JSValue.Object(_) =>
              isProxyValue(proxy).exists { case (proxyTarget, _) =>
                isConstructor(proxyTarget)
              }
            case _ => false
          }
        def constructAny(
            ctor: JSValue,
            ctorArgs: Array[JSValue],
            newTarget: JSValue
        ): JSValue =
          ctor match {
            case proxy @ JSValue.Object(_) if isProxyValue(proxy).isDefined =>
              val (proxyTarget, handler) = isProxyValue(proxy).get
              if !isConstructor(proxyTarget) then
                ctx.throwTypeError("proxy target is not a constructor")
              val result = proxyTrap(
                handler,
                "construct",
                Array(proxyTarget, toArgArray(ctorArgs), newTarget)
              ) match {
                case Some(trapResult) => trapResult
                case None             => constructAny(proxyTarget, ctorArgs, newTarget)
              }
              if !isObjectLike(result) then
                ctx.throwTypeError("proxy construct trap must return an object")
              result
            case func: JSValue.Function =>
              if !func.isConstructor then
                ctx.throwTypeError(s"${func.name} is not a constructor")
              if !isConstructor(newTarget) then
                ctx.throwTypeError(
                  "Reflect.construct: newTarget is not a constructor"
                )
              val prototypeSource =
                isProxyValue(newTarget).map(_._1).getOrElse(newTarget)
              val protoObj = objOf(prototypeSource).getOrElse(func.funcObj)
              val funcPrototype = protoObj.get("prototype") match {
                case JSValue.Object(proto) => proto;
                case _                     => ctx.objectPrototype
              }
              val newObj = JSObject(prototype = funcPrototype, extensible = true)
              val retValue = Interpreter().call(
                functionToBytecode(func),
                JSValue.Object(newObj),
                ctorArgs,
                func.closure,
                newTarget
              )
              if isObjectLike(retValue) then retValue else JSValue.Object(newObj)
            case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
              if !isConstructor(newTarget) then
                ctx.throwTypeError(
                  "Reflect.construct: newTarget is not a constructor"
                )
              val result = nc.construct(ctorArgs)
              val prototypeValue = isProxyValue(newTarget) match {
                case Some((proxyTarget, handler)) =>
                  proxyTrap(
                    handler,
                    "get",
                    Array(
                      proxyTarget,
                      JSValue.fromString("prototype"),
                      newTarget
                    )
                  ).getOrElse(
                    ordinaryGet(
                      proxyTarget,
                      JSValue.fromString("prototype"),
                      newTarget
                    )
                  )
                case None =>
                  ordinaryGet(
                    newTarget,
                    JSValue.fromString("prototype"),
                    newTarget
                  )
              }
              prototypeValue match {
                case JSValue.Object(proto) =>
                  result match {
                    case JSValue.Object(obj) => obj.setPrototype(proto)
                    case fn: JSValue.Function => fn.funcObj.setPrototype(proto)
                    case _ => ()
                  }
                case _ => ()
              }
              result
            case _ =>
              ctx.throwTypeError("Reflect.construct called on non-constructor")
          }
        val newTarget = if rest.length > 2 then rest(2) else target
        constructAny(target, funcArgs, newTarget)
    )

    // Register all methods on Reflect object
    reflectObj.defineProperty(
      "get",
      JSValue.Native(reflectGet),
      enumerable = false
    )
    reflectObj.defineProperty(
      "set",
      JSValue.Native(reflectSet),
      enumerable = false
    )
    reflectObj.defineProperty(
      "has",
      JSValue.Native(reflectHas),
      enumerable = false
    )
    reflectObj.defineProperty(
      "deleteProperty",
      JSValue.Native(reflectDeleteProperty),
      enumerable = false
    )
    reflectObj.defineProperty(
      "ownKeys",
      JSValue.Native(reflectOwnKeys),
      enumerable = false
    )
    reflectObj.defineProperty(
      "getPrototypeOf",
      JSValue.Native(reflectGetPrototypeOf),
      enumerable = false
    )
    reflectObj.defineProperty(
      "setPrototypeOf",
      JSValue.Native(reflectSetPrototypeOf),
      enumerable = false
    )
    reflectObj.defineProperty(
      "defineProperty",
      JSValue.Native(reflectDefineProperty),
      enumerable = false
    )
    reflectObj.defineProperty(
      "getOwnPropertyDescriptor",
      JSValue.Native(reflectGetOwnPropertyDescriptor),
      enumerable = false
    )
    reflectObj.defineProperty(
      "isExtensible",
      JSValue.Native(reflectIsExtensible),
      enumerable = false
    )
    reflectObj.defineProperty(
      "preventExtensions",
      JSValue.Native(reflectPreventExtensions),
      enumerable = false
    )
    reflectObj.defineProperty(
      "apply",
      JSValue.Native(reflectApply),
      enumerable = false
    )
    reflectObj.defineProperty(
      "construct",
      JSValue.Native(reflectConstruct),
      enumerable = false
    )

    // Symbol.toStringTag = "Reflect"
    val symToStringTag = ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.get("toStringTag")(using ctx)
      case _ => JSValue.Undefined
    }
    symToStringTag match {
      case sym: JSValue.Symbol =>
        reflectObj.initSymbolProperty(
          sym.value,
          JSValue.fromString("Reflect"),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }

    ctx.global.defineProperty(
      "Reflect",
      JSValue.Object(reflectObj),
      enumerable = false
    )
  }
}
