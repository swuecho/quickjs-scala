package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext

/** Object static methods (Object.keys, Object.defineProperty, freeze, seal, etc.). */
object ObjectBuiltins:
  import quickjs.objmodel.{JSObject, JSArray}

  def initialize(ctx: JSContext): Unit =
    def isArrayIndexKey(key: String): Boolean =
      key.nonEmpty && key.forall(_.isDigit) && (key.length == 1 || key.charAt(0) != '0')

    def hasOwnKey(target: JSValue, key: String)(using JSContext): Boolean =
      target match
        case JSValue.Object(obj) =>
          obj.getOwnProperty(key).isDefined
        case func: JSValue.Function =>
          func.funcObj.getOwnProperty(key).isDefined
        case JSValue.JSArrayVal(arr) =>
          if key == "length" then true
          else if isArrayIndexKey(key) then
            val idx = key.toInt
            idx >= 0 && idx < arr.getLength
          else
            arr.getOwnProperty(key).isDefined
        case _ => false

    def definePropertyOnTarget(
      target: JSValue,
      propKey: String,
      descriptor: JSValue
    )(using JSContext): JSValue =
      def applyDefine(obj: quickjs.objmodel.JSObject): JSValue =
        val existingDesc = obj.getOwnPropertyDescriptor(propKey)
        val (enumerableOpt, writableOpt, configurableOpt, getterOpt, setterOpt, valueOpt, hasWritableProp) =
          descriptor match
            case JSValue.Object(descObj) =>
              val enumerableOpt =
                descObj.getOwnProperty("enumerable") match
                  case Some(JSValue.Bool(b)) => Some(b)
                  case Some(_) => Some(false)
                  case None => None
              val writableOpt =
                descObj.getOwnProperty("writable") match
                  case Some(JSValue.Bool(b)) => Some(b)
                  case Some(_) => Some(false)
                  case None => None
              val configurableOpt =
                descObj.getOwnProperty("configurable") match
                  case Some(JSValue.Bool(b)) => Some(b)
                  case Some(_) => Some(false)
                  case None => None
              val getterOpt =
                descObj.getOwnProperty("get") match
                  case Some(JSValue.Undefined) | None => None
                  case Some(v) => Some(v)
              val setterOpt =
                descObj.getOwnProperty("set") match
                  case Some(JSValue.Undefined) | None => None
                  case Some(v) => Some(v)
              val valueOpt = descObj.getOwnProperty("value")
              val hasWritableProp = descObj.getOwnProperty("writable").isDefined
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
        val value = valueOpt.getOrElse(obj.get(propKey))
        val ok =
          if hasAccessor then
            // For accessor properties, we need to merge with existing accessor if the property
            // is not configurable. This allows adding a setter to an existing getter (or vice versa)
            // without requiring configurable: true
            val getter = getterOpt.orElse(existingDesc.flatMap(_._2.getter))
            val setter = setterOpt.orElse(existingDesc.flatMap(_._2.setter))
            obj.defineAccessorProperty(propKey, getter, setter, enumerable, configurable)
          else
            obj.defineProperty(propKey, value, enumerable, writable, configurable)
        if !ok then
          ctx.throwTypeError("Cannot define property")
        target

      target match
        case JSValue.Object(obj) => applyDefine(obj)
        case func: JSValue.Function => applyDefine(func.funcObj)
        case _ => target

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
          given JSContext = ctx
          definePropertyOnTarget(target, propKey, descriptor)
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

    val objectGetOwnPropertyDescriptors = NativeFunction(
      name = "getOwnPropertyDescriptors",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.Object(quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true))
        else
          val offset = if args.length >= 2 then 1 else 0
          val result = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
          def pushDescriptor(key: String, desc: Option[(JSValue, quickjs.objmodel.JSObject.PropertyAttributes)]): Unit =
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
                result.set(key, JSValue.Object(descObj))(using ctx)
              case None => ()

          args(offset) match
            case JSValue.Object(obj) =>
              obj.getAllProperties.keys.foreach { key =>
                pushDescriptor(key, obj.getOwnPropertyDescriptor(key)(using ctx))
              }
            case func: JSValue.Function =>
              func.funcObj.getAllProperties.keys.foreach { key =>
                pushDescriptor(key, func.funcObj.getOwnPropertyDescriptor(key)(using ctx))
              }
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do
                pushDescriptor(i.toString, Some(arr.get(i) -> quickjs.objmodel.JSObject.PropertyAttributes(enumerable = true)))
                i += 1
              pushDescriptor("length", Some(JSValue.fromInt(arr.getLength) -> quickjs.objmodel.JSObject.PropertyAttributes(enumerable = false, writable = true, configurable = false)))
              arr.getOwnPropertyKeys.foreach { key =>
                pushDescriptor(key, Some(arr.getProperty(key).getOrElse(JSValue.Undefined) -> quickjs.objmodel.JSObject.PropertyAttributes(enumerable = true)))
              }
            case _ => ()
          JSValue.Object(result)
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

    val objectDefineProperties = NativeFunction(
      name = "defineProperties",
      impl = (args, ctx) =>
        if args.length < 3 then
          JSValue.Undefined
        else
          val offset = if args.length >= 4 then 1 else 0
          val target = args(offset)
          val descriptors = args(offset + 1)
          given JSContext = ctx
          descriptors match
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
          target
    )

    val objectHasOwn = NativeFunction(
      name = "hasOwn",
      impl = (args, ctx) =>
        if args.length < 2 then
          JSValue.Bool(false)
        else
          val offset = if args.length >= 3 then 1 else 0
          val target = args(offset)
          val key = args(offset + 1).toString
          given JSContext = ctx
          JSValue.fromBoolean(hasOwnKey(target, key))
    )

    val objectFromEntries = NativeFunction(
      name = "fromEntries",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then
          JSValue.Object(quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true))
        else
          val result = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
          args(offset) match
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do
                arr.get(i) match
                  case JSValue.JSArrayVal(pair) =>
                    if pair.getLength >= 2 then
                      val key = pair.get(0).toString
                      val value = pair.get(1)
                      result.set(key, value)(using ctx)
                  case _ => ()
                i += 1
            case _ =>
              ctx.throwTypeError("Object.fromEntries expects an array")
          JSValue.Object(result)
    )

    val objectPrototypeHasOwnProperty = NativeFunction(
      name = "hasOwnProperty",
      impl = (args, ctx) =>
        if args.isEmpty then
          ctx.throwTypeError("Object.prototype.hasOwnProperty called on null or undefined")
        else
          val key = if args.length > 1 then args(1).toString else "undefined"
          val target = args(0)
          target match
            case JSValue.Null | JSValue.Undefined =>
              ctx.throwTypeError("Object.prototype.hasOwnProperty called on null or undefined")
            case _ =>
              given JSContext = ctx
              JSValue.fromBoolean(hasOwnKey(target, key))
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

    // Object.freeze(obj) - makes object immutable
    val objectFreeze = NativeFunction(
      name = "freeze",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.Undefined
        else
          val offset = if args.length >= 2 then 1 else 0
          val target = args(offset)
          given JSContext = ctx
          target match
            case JSValue.Object(obj) =>
              obj.freeze()
              target
            case func: JSValue.Function =>
              func.funcObj.freeze()
              target
            case _ =>
              // For non-objects, just return the value (ES5.1 throws, ES6+ returns value)
              target
    )

    // Object.seal(obj) - prevents adding/removing properties
    val objectSeal = NativeFunction(
      name = "seal",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.Undefined
        else
          val offset = if args.length >= 2 then 1 else 0
          val target = args(offset)
          given JSContext = ctx
          target match
            case JSValue.Object(obj) =>
              obj.seal()
              target
            case func: JSValue.Function =>
              func.funcObj.seal()
              target
            case _ =>
              target
    )

    // Object.isFrozen(obj) - checks if object is frozen
    val objectIsFrozen = NativeFunction(
      name = "isFrozen",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.Bool(true) // Non-object values are considered frozen
        else
          val offset = if args.length >= 2 then 1 else 0
          val target = args(offset)
          given JSContext = ctx
          target match
            case JSValue.Object(obj) =>
              // Check if truly frozen: not extensible and all properties non-writable, non-configurable
              JSValue.Bool(obj.checkFrozen())
            case func: JSValue.Function =>
              JSValue.Bool(func.funcObj.checkFrozen())
            case _ =>
              // Non-objects are considered frozen
              JSValue.Bool(true)
    )

    // Object.isSealed(obj) - checks if object is sealed
    val objectIsSealed = NativeFunction(
      name = "isSealed",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.Bool(true) // Non-object values are considered sealed
        else
          val offset = if args.length >= 2 then 1 else 0
          val target = args(offset)
          given JSContext = ctx
          target match
            case JSValue.Object(obj) =>
              // Check if truly sealed: not extensible and all properties non-configurable
              JSValue.Bool(obj.checkSealed())
            case func: JSValue.Function =>
              JSValue.Bool(func.funcObj.checkSealed())
            case _ =>
              // Non-objects are considered sealed
              JSValue.Bool(true)
    )

    // Object.preventExtensions(obj) - prevents adding new properties
    val objectPreventExtensions = NativeFunction(
      name = "preventExtensions",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.Undefined
        else
          val offset = if args.length >= 2 then 1 else 0
          val target = args(offset)
          target match
            case JSValue.Object(obj) =>
              obj.preventExtensions()
              target
            case func: JSValue.Function =>
              func.funcObj.preventExtensions()
              target
            case _ =>
              target
    )

    // Object.isExtensible(obj) - checks if object is extensible
    val objectIsExtensible = NativeFunction(
      name = "isExtensible",
      impl = (args, ctx) =>
        if args.length < 1 then
          JSValue.Bool(false) // Non-object values are not extensible
        else
          val offset = if args.length >= 2 then 1 else 0
          val target = args(offset)
          target match
            case JSValue.Object(obj) =>
              JSValue.Bool(obj.isExtensible)
            case func: JSValue.Function =>
              JSValue.Bool(func.funcObj.isExtensible)
            case _ =>
              // Non-objects are not extensible
              JSValue.Bool(false)
    )

    given JSContext = ctx
    val objectConstructorOpt =
      ctx.global.get("Object") match
        case JSValue.Native(cons: quickjs.value.NativeConstructor) => Some(cons)
        case _ => None

    objectConstructorOpt.foreach { cons =>
      cons.funcObj.set("setPrototypeOf", JSValue.Native(setPrototypeOf))
      cons.funcObj.set("defineProperty", JSValue.Native(defineProperty))
      cons.funcObj.set("defineProperties", JSValue.Native(objectDefineProperties))
      cons.funcObj.set("is", JSValue.Native(objectIs))
      cons.funcObj.set("getPrototypeOf", JSValue.Native(objectGetPrototypeOf))
      cons.funcObj.set("getOwnPropertyDescriptor", JSValue.Native(objectGetOwnPropertyDescriptor))
      cons.funcObj.set("getOwnPropertyDescriptors", JSValue.Native(objectGetOwnPropertyDescriptors))
      cons.funcObj.set("getOwnPropertyNames", JSValue.Native(objectGetOwnPropertyNames))
      cons.funcObj.set("keys", JSValue.Native(objectKeys))
      cons.funcObj.set("assign", JSValue.Native(objectAssign))
      cons.funcObj.set("create", JSValue.Native(objectCreate))
      cons.funcObj.set("values", JSValue.Native(objectValues))
      cons.funcObj.set("entries", JSValue.Native(objectEntries))
      cons.funcObj.set("hasOwn", JSValue.Native(objectHasOwn))
      cons.funcObj.set("fromEntries", JSValue.Native(objectFromEntries))
      // ES5 freeze/seal methods
      cons.funcObj.set("freeze", JSValue.Native(objectFreeze))
      cons.funcObj.set("seal", JSValue.Native(objectSeal))
      cons.funcObj.set("isFrozen", JSValue.Native(objectIsFrozen))
      cons.funcObj.set("isSealed", JSValue.Native(objectIsSealed))
      cons.funcObj.set("preventExtensions", JSValue.Native(objectPreventExtensions))
      cons.funcObj.set("isExtensible", JSValue.Native(objectIsExtensible))
    }
    ctx.objectPrototype.defineProperty("toString", JSValue.Native(objectPrototypeToString), enumerable = false)
    ctx.objectPrototype.defineProperty("hasOwnProperty", JSValue.Native(objectPrototypeHasOwnProperty), enumerable = false)

