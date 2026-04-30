package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.runtime.JSContext

/** Reflect built-in: Reflect.get, set, has, deleteProperty, ownKeys, etc. */
object ReflectBuiltins:
  import quickjs.objmodel.{JSObject, JSArray}

  def initialize(ctx: JSContext): Unit =
    given JSContext = ctx

    val reflectObj = JSObject(prototype = null, extensible = true)

    // Helper to extract JSObject from various value types
    def extractObject(value: JSValue): Option[JSObject] =
      value match
        case JSValue.Object(obj) => Some(obj)
        case func: JSValue.Function => Some(func.funcObj)
        case _ => None

    // Helper to check if value is an object (including functions)
    def isObject(value: JSValue): Boolean =
      value match
        case JSValue.Object(_) | _: JSValue.Function => true
        case _ => false

    // Reflect.get(target, propertyKey[, receiver])
    val reflectGet = NativeFunction(
      name = "get",
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.get requires at least 2 arguments")
        val offset = if args.length >= 3 then 1 else 0
        val target = args(offset)
        val propertyKey = args(offset + 1).toString

        if !isObject(target) then
          ctx.throwTypeError("Reflect.get called on non-object")

        given JSContext = ctx
        target match
          case JSValue.Object(obj) =>
            // Check for getter
            obj.getPropertyDescriptorWithOwner(propertyKey) match
              case Some((owner, value, attrs)) if attrs.getter.isDefined =>
                val receiver = if args.length > offset + 2 then args(offset + 2) else target
                attrs.getter.get match
                  case func: JSValue.Function =>
                    val interpreter = new Interpreter()
                    val bcFunc = new BytecodeFunction(
                      name = func.name,
                      bytecode = func.bytecode,
                      constants = func.constants,
                      stackSize = func.stackSize,
                      freeVars = Array.empty,
                      paramNames = func.paramNames,
                      localVarNames = func.localVarNames,
                      argumentsIndex = func.argumentsIndex,
                      isConstructor = false,
                      spanMap = func.spanMap
                    )
                    interpreter.call(bcFunc, receiver, Array.empty, func.closure)
                  case JSValue.Native(native) =>
                    native match
                      case nf: quickjs.value.NativeFunction =>
                        nf.call(Array(receiver))
                      case _ => JSValue.Undefined
                  case _ => JSValue.Undefined
              case _ =>
                obj.get(propertyKey)
          case func: JSValue.Function =>
            func.funcObj.get(propertyKey)
          case _ =>
            JSValue.Undefined
    )

    // Reflect.set(target, propertyKey, value[, receiver])
    val reflectSet = NativeFunction(
      name = "set",
      impl = (args, ctx) =>
        if args.length < 3 then
          ctx.throwTypeError("Reflect.set requires at least 3 arguments")
        val offset = if args.length >= 4 then 1 else 0
        val target = args(offset)
        val propertyKey = args(offset + 1).toString
        val value = args(offset + 2)

        if !isObject(target) then
          ctx.throwTypeError("Reflect.set called on non-object")

        given JSContext = ctx
        target match
          case JSValue.Object(obj) =>
            // Check for setter
            obj.getPropertyDescriptorWithOwner(propertyKey) match
              case Some((owner, _, attrs)) if attrs.setter.isDefined =>
                val receiver = if args.length > offset + 3 then args(offset + 3) else target
                attrs.setter.get match
                  case func: JSValue.Function =>
                    val interpreter = new Interpreter()
                    val bcFunc = new BytecodeFunction(
                      name = func.name,
                      bytecode = func.bytecode,
                      constants = func.constants,
                      stackSize = func.stackSize,
                      freeVars = Array.empty,
                      paramNames = func.paramNames,
                      localVarNames = func.localVarNames,
                      argumentsIndex = func.argumentsIndex,
                      isConstructor = false,
                      spanMap = func.spanMap
                    )
                    interpreter.call(bcFunc, receiver, Array(value), func.closure)
                    JSValue.Bool(true)
                  case JSValue.Native(native) =>
                    native match
                      case nf: quickjs.value.NativeFunction =>
                        nf.call(Array(receiver, value))
                        JSValue.Bool(true)
                      case _ => JSValue.Bool(false)
                  case _ => JSValue.Bool(false)
              case _ =>
                JSValue.Bool(obj.set(propertyKey, value))
          case func: JSValue.Function =>
            JSValue.Bool(func.funcObj.set(propertyKey, value))
          case _ =>
            JSValue.Bool(false)
    )

    // Reflect.has(target, propertyKey)
    val reflectHas = NativeFunction(
      name = "has",
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.has requires 2 arguments")
        val offset = if args.length >= 3 then 1 else 0
        val target = args(offset)
        val propertyKey = args(offset + 1).toString

        if !isObject(target) then
          ctx.throwTypeError("Reflect.has called on non-object")

        given JSContext = ctx
        target match
          case JSValue.Object(obj) =>
            JSValue.Bool(obj.hasProperty(propertyKey))
          case func: JSValue.Function =>
            JSValue.Bool(func.funcObj.hasProperty(propertyKey))
          case JSValue.JSArrayVal(arr) =>
            if propertyKey == "length" then JSValue.Bool(true)
            else if propertyKey.forall(_.isDigit) then
              val idx = propertyKey.toInt
              JSValue.Bool(idx >= 0 && idx < arr.getLength)
            else JSValue.Bool(arr.getProperty(propertyKey).isDefined)
          case _ =>
            JSValue.Bool(false)
    )

    // Reflect.deleteProperty(target, propertyKey)
    val reflectDeleteProperty = NativeFunction(
      name = "deleteProperty",
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.deleteProperty requires 2 arguments")
        val offset = if args.length >= 3 then 1 else 0
        val target = args(offset)
        val propertyKey = args(offset + 1).toString

        if !isObject(target) then
          ctx.throwTypeError("Reflect.deleteProperty called on non-object")

        given JSContext = ctx
        target match
          case JSValue.Object(obj) =>
            JSValue.Bool(obj.deleteProperty(propertyKey))
          case func: JSValue.Function =>
            JSValue.Bool(func.funcObj.deleteProperty(propertyKey))
          case _ =>
            JSValue.Bool(false)
    )

    // Reflect.ownKeys(target)
    val reflectOwnKeys = NativeFunction(
      name = "ownKeys",
      impl = (args, ctx) =>
        if args.length < 1 then
          ctx.throwTypeError("Reflect.ownKeys requires 1 argument")
        val offset = if args.length >= 2 then 1 else 0
        val target = args(offset)

        if !isObject(target) then
          ctx.throwTypeError("Reflect.ownKeys called on non-object")

        given JSContext = ctx
        val result = JSArray.empty()
        target match
          case JSValue.Object(obj) =>
            obj.getAllProperties.keys.foreach { key =>
              result.push(JSValue.fromString(key))
            }
          case func: JSValue.Function =>
            func.funcObj.getAllProperties.keys.foreach { key =>
              result.push(JSValue.fromString(key))
            }
          case JSValue.JSArrayVal(arr) =>
            var i = 0
            while i < arr.getLength do
              result.push(JSValue.fromString(i.toString))
              i += 1
            result.push(JSValue.fromString("length"))
          case _ => ()
        JSValue.JSArrayVal(result)
    )

    // Reflect.getPrototypeOf(target)
    val reflectGetPrototypeOf = NativeFunction(
      name = "getPrototypeOf",
      impl = (args, ctx) =>
        if args.length < 1 then
          ctx.throwTypeError("Reflect.getPrototypeOf requires 1 argument")
        val offset = if args.length >= 2 then 1 else 0
        val target = args(offset)

        if !isObject(target) then
          ctx.throwTypeError("Reflect.getPrototypeOf called on non-object")

        given JSContext = ctx
        target match
          case JSValue.Object(obj) =>
            obj.getPrototype match
              case null => JSValue.Null
              case proto => JSValue.Object(proto)
          case func: JSValue.Function =>
            func.funcObj.getPrototype match
              case null => JSValue.Null
              case proto => JSValue.Object(proto)
          case _ =>
            JSValue.Null
    )

    // Reflect.setPrototypeOf(target, proto)
    val reflectSetPrototypeOf = NativeFunction(
      name = "setPrototypeOf",
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.setPrototypeOf requires 2 arguments")
        val offset = if args.length >= 3 then 1 else 0
        val target = args(offset)
        val proto = args(offset + 1)

        if !isObject(target) then
          ctx.throwTypeError("Reflect.setPrototypeOf called on non-object")

        val protoObj: JSObject | Null = proto match
          case JSValue.Object(obj) => obj
          case JSValue.Null => null
          case func: JSValue.Function => func.funcObj
          case _ =>
            ctx.throwTypeError("Prototype must be an object or null")

        given JSContext = ctx
        target match
          case JSValue.Object(obj) =>
            obj.setPrototype(protoObj)
            JSValue.Bool(true)
          case func: JSValue.Function =>
            func.funcObj.setPrototype(protoObj)
            JSValue.Bool(true)
          case _ =>
            JSValue.Bool(false)
    )

    // Reflect.defineProperty(target, propertyKey, attributes)
    val reflectDefineProperty = NativeFunction(
      name = "defineProperty",
      impl = (args, ctx) =>
        if args.length < 3 then
          ctx.throwTypeError("Reflect.defineProperty requires 3 arguments")
        val offset = if args.length >= 4 then 1 else 0
        val target = args(offset)
        val propertyKey = args(offset + 1).toString
        val attributes = args(offset + 2)

        if !isObject(target) then
          ctx.throwTypeError("Reflect.defineProperty called on non-object")

        given JSContext = ctx

        // Parse the descriptor
        val (enumerableOpt, writableOpt, configurableOpt, getterOpt, setterOpt, valueOpt) =
          attributes match
            case JSValue.Object(descObj) =>
              val enumerableOpt = descObj.getOwnProperty("enumerable") match
                case Some(JSValue.Bool(b)) => Some(b)
                case Some(_) => Some(false)
                case None => None
              val writableOpt = descObj.getOwnProperty("writable") match
                case Some(JSValue.Bool(b)) => Some(b)
                case Some(_) => Some(false)
                case None => None
              val configurableOpt = descObj.getOwnProperty("configurable") match
                case Some(JSValue.Bool(b)) => Some(b)
                case Some(_) => Some(false)
                case None => None
              val getterOpt = descObj.getOwnProperty("get") match
                case Some(JSValue.Undefined) | None => None
                case Some(v) => Some(v)
              val setterOpt = descObj.getOwnProperty("set") match
                case Some(JSValue.Undefined) | None => None
                case Some(v) => Some(v)
              val valueOpt = descObj.getOwnProperty("value")
              (enumerableOpt, writableOpt, configurableOpt, getterOpt, setterOpt, valueOpt)
            case _ =>
              (None, None, None, None, None, None)

        val hasAccessor = getterOpt.isDefined || setterOpt.isDefined

        extractObject(target) match
          case Some(obj) =>
            val existingDesc = obj.getOwnPropertyDescriptor(propertyKey)
            val enumerable = enumerableOpt.getOrElse(existingDesc.map(_._2.enumerable).getOrElse(false))
            val writable = writableOpt.getOrElse(existingDesc.map(_._2.writable).getOrElse(false))
            val configurable = configurableOpt.getOrElse(existingDesc.map(_._2.configurable).getOrElse(false))
            val value = valueOpt.getOrElse(obj.get(propertyKey))

            val ok = if hasAccessor then
              val getter = getterOpt.orElse(existingDesc.flatMap(_._2.getter))
              val setter = setterOpt.orElse(existingDesc.flatMap(_._2.setter))
              obj.defineAccessorProperty(propertyKey, getter, setter, enumerable, configurable)
            else
              obj.defineProperty(propertyKey, value, enumerable, writable, configurable)

            JSValue.Bool(ok)
          case None =>
            JSValue.Bool(false)
    )

    // Reflect.getOwnPropertyDescriptor(target, propertyKey)
    val reflectGetOwnPropertyDescriptor = NativeFunction(
      name = "getOwnPropertyDescriptor",
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.getOwnPropertyDescriptor requires 2 arguments")
        val offset = if args.length >= 3 then 1 else 0
        val target = args(offset)
        val propertyKey = args(offset + 1).toString

        if !isObject(target) then
          ctx.throwTypeError("Reflect.getOwnPropertyDescriptor called on non-object")

        given JSContext = ctx

        def buildDescriptor(desc: Option[(JSValue, JSObject.PropertyAttributes)]): JSValue =
          desc match
            case Some((value, attrs)) =>
              val descObj = JSObject(prototype = ctx.objectPrototype, extensible = true)
              if attrs.getter.isDefined || attrs.setter.isDefined then
                attrs.getter.foreach(v => descObj.set("get", v))
                attrs.setter.foreach(v => descObj.set("set", v))
              else
                descObj.set("value", value)
                descObj.set("writable", JSValue.fromBoolean(attrs.writable))
              descObj.set("enumerable", JSValue.fromBoolean(attrs.enumerable))
              descObj.set("configurable", JSValue.fromBoolean(attrs.configurable))
              JSValue.Object(descObj)
            case None =>
              JSValue.Undefined

        target match
          case JSValue.Object(obj) =>
            buildDescriptor(obj.getOwnPropertyDescriptor(propertyKey))
          case func: JSValue.Function =>
            buildDescriptor(func.funcObj.getOwnPropertyDescriptor(propertyKey))
          case _ =>
            JSValue.Undefined
    )

    // Reflect.isExtensible(target)
    val reflectIsExtensible = NativeFunction(
      name = "isExtensible",
      impl = (args, ctx) =>
        if args.length < 1 then
          ctx.throwTypeError("Reflect.isExtensible requires 1 argument")
        val offset = if args.length >= 2 then 1 else 0
        val target = args(offset)

        if !isObject(target) then
          ctx.throwTypeError("Reflect.isExtensible called on non-object")

        target match
          case JSValue.Object(obj) =>
            JSValue.Bool(obj.isExtensible)
          case func: JSValue.Function =>
            JSValue.Bool(func.funcObj.isExtensible)
          case _ =>
            JSValue.Bool(false)
    )

    // Reflect.preventExtensions(target)
    val reflectPreventExtensions = NativeFunction(
      name = "preventExtensions",
      impl = (args, ctx) =>
        if args.length < 1 then
          ctx.throwTypeError("Reflect.preventExtensions requires 1 argument")
        val offset = if args.length >= 2 then 1 else 0
        val target = args(offset)

        if !isObject(target) then
          ctx.throwTypeError("Reflect.preventExtensions called on non-object")

        // Note: JSObject doesn't have a preventExtensions method yet
        // For now, we return true as a placeholder
        JSValue.Bool(true)
    )

    // Reflect.apply(target, thisArgument, argumentsList)
    val reflectApply = NativeFunction(
      name = "apply",
      impl = (args, ctx) =>
        if args.length < 3 then
          ctx.throwTypeError("Reflect.apply requires 3 arguments")
        val offset = if args.length >= 4 then 1 else 0
        val target = args(offset)
        val thisArg = args(offset + 1)
        val argumentsList = args(offset + 2)

        // Extract arguments from array
        val funcArgs: Array[JSValue] = argumentsList match
          case JSValue.JSArrayVal(arr) =>
            val result = new Array[JSValue](arr.getLength)
            var i = 0
            while i < arr.getLength do
              result(i) = arr.get(i)
              i += 1
            result
          case _ =>
            Array.empty

        given JSContext = ctx

        target match
          case func: JSValue.Function =>
            val interpreter = new Interpreter()
            val bcFunc = new BytecodeFunction(
              name = func.name,
              bytecode = func.bytecode,
              constants = func.constants,
              stackSize = func.stackSize,
              freeVars = Array.empty,
              paramNames = func.paramNames,
              localVarNames = func.localVarNames,
              argumentsIndex = func.argumentsIndex,
              isConstructor = func.isConstructor,
              spanMap = func.spanMap
            )
            interpreter.call(bcFunc, thisArg, funcArgs, func.closure)
          case JSValue.Native(nativeFuncWrapper) =>
            nativeFuncWrapper match
              case native: quickjs.value.NativeFunction =>
                val argsWithThis = new Array[JSValue](funcArgs.length + 1)
                argsWithThis(0) = thisArg
                Array.copy(funcArgs, 0, argsWithThis, 1, funcArgs.length)
                native.call(argsWithThis)
              case constructor: quickjs.value.NativeConstructor =>
                constructor.call(funcArgs)
              case _ =>
                ctx.throwTypeError("Reflect.apply called on non-callable")
          case _ =>
            ctx.throwTypeError("Reflect.apply called on non-callable")
    )

    // Reflect.construct(target, argumentsList[, newTarget])
    val reflectConstruct = NativeFunction(
      name = "construct",
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.construct requires at least 2 arguments")
        val offset = if args.length >= 3 then 1 else 0
        val target = args(offset)
        val argumentsList = args(offset + 1)
        val newTargetArg = if args.length > offset + 2 then Some(args(offset + 2)) else None

        // Extract arguments from array
        val funcArgs: Array[JSValue] = argumentsList match
          case JSValue.JSArrayVal(arr) =>
            val result = new Array[JSValue](arr.getLength)
            var i = 0
            while i < arr.getLength do
              result(i) = arr.get(i)
              i += 1
            result
          case _ =>
            Array.empty

        given JSContext = ctx

        target match
          case func: JSValue.Function =>
            if !func.isConstructor then
              ctx.throwTypeError(s"${func.name} is not a constructor")

            val newTarget = newTargetArg.getOrElse(target)
            val prototypeSource = newTarget match
              case ntFunc: JSValue.Function => ntFunc.funcObj
              case _ => func.funcObj

            val funcPrototype = prototypeSource.get("prototype") match
              case JSValue.Object(proto) => proto
              case _ => ctx.objectPrototype

            val newObj = JSObject(prototype = funcPrototype, extensible = true)

            val interpreter = new Interpreter()
            val bcFunc = new BytecodeFunction(
              name = func.name,
              bytecode = func.bytecode,
              constants = func.constants,
              stackSize = func.stackSize,
              freeVars = Array.empty,
              paramNames = func.paramNames,
              localVarNames = func.localVarNames,
              argumentsIndex = func.argumentsIndex,
              isConstructor = func.isConstructor,
              spanMap = func.spanMap
            )

            val retValue = interpreter.call(
              bcFunc,
              JSValue.Object(newObj),
              funcArgs,
              func.closure,
              target  // new.target
            )

            retValue match
              case JSValue.Object(_) => retValue
              case _ => JSValue.Object(newObj)

          case JSValue.Native(nativeFuncWrapper) =>
            nativeFuncWrapper match
              case constructor: quickjs.value.NativeConstructor =>
                constructor.construct(funcArgs)
              case _ =>
                ctx.throwTypeError("Reflect.construct called on non-constructor")
          case _ =>
            ctx.throwTypeError("Reflect.construct called on non-constructor")
    )

    // Register all methods on Reflect object
    reflectObj.set("get", JSValue.Native(reflectGet))
    reflectObj.set("set", JSValue.Native(reflectSet))
    reflectObj.set("has", JSValue.Native(reflectHas))
    reflectObj.set("deleteProperty", JSValue.Native(reflectDeleteProperty))
    reflectObj.set("ownKeys", JSValue.Native(reflectOwnKeys))
    reflectObj.set("getPrototypeOf", JSValue.Native(reflectGetPrototypeOf))
    reflectObj.set("setPrototypeOf", JSValue.Native(reflectSetPrototypeOf))
    reflectObj.set("defineProperty", JSValue.Native(reflectDefineProperty))
    reflectObj.set("getOwnPropertyDescriptor", JSValue.Native(reflectGetOwnPropertyDescriptor))
    reflectObj.set("isExtensible", JSValue.Native(reflectIsExtensible))
    reflectObj.set("preventExtensions", JSValue.Native(reflectPreventExtensions))
    reflectObj.set("apply", JSValue.Native(reflectApply))
    reflectObj.set("construct", JSValue.Native(reflectConstruct))

    ctx.global.set("Reflect", JSValue.Object(reflectObj))
