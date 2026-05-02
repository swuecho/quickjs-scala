package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers.{
  extractJSObject,
  functionToBytecode,
  parsePropertyDescriptor,
  buildPropertyDescriptorObject
}

/** Reflect built-in: Reflect.get, set, has, deleteProperty, ownKeys, etc. */
object ReflectBuiltins {
  import quickjs.objmodel.{JSObject, JSArray}

  def initialize(ctx: JSContext): Unit = {
    given JSContext = ctx

    val reflectObj = JSObject(prototype = null, extensible = true)

    // Helper to check if value is an object (including functions)
    def isObject(value: JSValue): Boolean = extractJSObject(value).isDefined

    // Helper to extract JSObject from various value types
    def objOf(value: JSValue): Option[JSObject] = extractJSObject(value)

    // Reflect.get(target, propertyKey[, receiver])
    val reflectGet = NativeFunction(
      name = "get",
      length = 2,
      impl = (args, ctx) =>
        if args.length < 2 then
          ctx.throwTypeError("Reflect.get requires at least 2 arguments")
        val (_, rest) = BuiltinHelpers.nativeArgs(args)
        val target = rest(0); val propertyKey = rest(1).toString
        if !isObject(target) then
          ctx.throwTypeError("Reflect.get called on non-object")
        given JSContext = ctx
        objOf(target) match {
          case Some(o) =>
            o.getPropertyDescriptorWithOwner(propertyKey) match {
              case Some((_, _, attrs)) if attrs.getter.isDefined =>
                val receiver = if rest.length > 2 then rest(2) else target
                attrs.getter.get match {
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
              case _ => o.get(propertyKey)
            }
          case None => JSValue.Undefined
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
        val target = rest(0); val propertyKey = rest(1).toString;
        val value = rest(2)
        if !isObject(target) then
          ctx.throwTypeError("Reflect.set called on non-object")
        given JSContext = ctx
        objOf(target) match {
          case Some(o) =>
            o.getPropertyDescriptorWithOwner(propertyKey) match {
              case Some((_, _, attrs)) if attrs.setter.isDefined =>
                val receiver = if rest.length > 3 then rest(3) else target
                attrs.setter.get match {
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
                JSValue.Bool(true)
              case _ => JSValue.Bool(o.set(propertyKey, value))
            }
          case None => JSValue.Bool(false)
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
        val target = rest(0); val propertyKey = rest(1).toString
        if !isObject(target) then
          ctx.throwTypeError("Reflect.has called on non-object")
        given JSContext = ctx
        objOf(target) match {
          case Some(o) => JSValue.Bool(o.hasProperty(propertyKey))
          case None    =>
            target match {
              case JSValue.JSArrayVal(arr) =>
                if propertyKey == "length" then JSValue.Bool(true)
                else if propertyKey.forall(_.isDigit) then {
                  val idx = propertyKey.toInt;
                  JSValue.Bool(idx >= 0 && idx < arr.getLength)
                }
                else JSValue.Bool(arr.getProperty(propertyKey).isDefined)
              case _ => JSValue.Bool(false)
            }
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
        val target = rest(0); val propertyKey = rest(1).toString
        if !isObject(target) then
          ctx.throwTypeError("Reflect.deleteProperty called on non-object")
        given JSContext = ctx
        objOf(target) match {
          case Some(o) => JSValue.Bool(o.deleteProperty(propertyKey))
          case None    => JSValue.Bool(false)
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
        objOf(target) match {
          case Some(o) =>
            o.getAllProperties.keys.foreach(k =>
              result.push(JSValue.fromString(k))
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
        objOf(target) match {
          case Some(o) =>
            o.getPrototype match {
              case null => JSValue.Null; case proto => JSValue.Object(proto)
            }
          case None => JSValue.Null
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
        val protoObj: JSObject | Null = proto match {
          case JSValue.Object(obj) => obj;
          case JSValue.Null        => null
          case _ => ctx.throwTypeError("Prototype must be an object or null")
        }
        given JSContext = ctx
        objOf(target) match {
          case Some(o) => o.setPrototype(protoObj); JSValue.Bool(true)
          case None    => JSValue.Bool(false)
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
        val target = rest(0); val propertyKey = rest(1).toString;
        val attributes = rest(2)
        if !isObject(target) then
          ctx.throwTypeError("Reflect.defineProperty called on non-object")
        given JSContext = ctx
        val pd = parsePropertyDescriptor(attributes)
        objOf(target) match {
          case Some(o) =>
            val existingDesc = o.getOwnPropertyDescriptor(propertyKey)
            val enumerable = pd.enumerable.getOrElse(
              existingDesc.map(_._2.enumerable).getOrElse(false)
            )
            val writable = pd.writable.getOrElse(
              existingDesc.map(_._2.writable).getOrElse(false)
            )
            val configurable = pd.configurable.getOrElse(
              existingDesc.map(_._2.configurable).getOrElse(false)
            )
            val value = pd.value.getOrElse(o.get(propertyKey))
            val ok = if pd.isAccessor then {
              val getter = pd.getter.orElse(existingDesc.flatMap(_._2.getter))
              val setter = pd.setter.orElse(existingDesc.flatMap(_._2.setter))
              o.defineAccessorProperty(
                propertyKey,
                getter,
                setter,
                enumerable,
                configurable
              )
            }
            else
              o.defineProperty(
                propertyKey,
                value,
                enumerable,
                writable,
                configurable
              )
            JSValue.Bool(ok)
          case None => JSValue.Bool(false)
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
        val target = rest(0); val propertyKey = rest(1).toString
        if !isObject(target) then
          ctx.throwTypeError(
            "Reflect.getOwnPropertyDescriptor called on non-object"
          )
        given JSContext = ctx
        objOf(target) match {
          case Some(o) =>
            buildPropertyDescriptorObject(
              propertyKey,
              o.getOwnPropertyDescriptor(propertyKey)
            )
          case None => JSValue.Undefined
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
        objOf(target) match {
          case Some(o) => JSValue.Bool(o.isExtensible)
          case None    => JSValue.Bool(false)
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
        JSValue.Bool(true)
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
        target match {
          case func: JSValue.Function =>
            Interpreter()
              .call(functionToBytecode(func), thisArg, funcArgs, func.closure)
          case JSValue.Native(nf: quickjs.value.NativeFunction) =>
            val argsWithThis = new Array[JSValue](funcArgs.length + 1);
            argsWithThis(0) = thisArg
            Array.copy(funcArgs, 0, argsWithThis, 1, funcArgs.length);
            nf.call(argsWithThis)
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            nc.call(funcArgs)
          case _ => ctx.throwTypeError("Reflect.apply called on non-callable")
        }
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
        target match {
          case func: JSValue.Function =>
            if !func.isConstructor then
              ctx.throwTypeError(s"${func.name} is not a constructor")
            val newTarget = if rest.length > 2 then rest(2) else target
            // Validate newTarget is constructable if it differs from target
            if rest.length > 2 then
              rest(2) match {
                case nt: JSValue.Function =>
                  if !nt.isConstructor then
                    ctx.throwTypeError(s"${nt.name} is not a constructor")
                case JSValue.Native(_: quickjs.value.NativeConstructor) => // OK
                case _                                                  =>
                  ctx.throwTypeError(
                    "Reflect.construct: newTarget is not a constructor"
                  )
              }
            val prototypeSource = objOf(newTarget).getOrElse(func.funcObj)
            val funcPrototype = prototypeSource.get("prototype") match {
              case JSValue.Object(proto) => proto;
              case _                     => ctx.objectPrototype
            }
            val newObj = JSObject(prototype = funcPrototype, extensible = true)
            val retValue = Interpreter().call(
              functionToBytecode(func),
              JSValue.Object(newObj),
              funcArgs,
              func.closure,
              target
            )
            retValue match {
              case JSValue.Object(_) => retValue;
              case _                 => JSValue.Object(newObj)
            }
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            nc.construct(funcArgs)
          case _ =>
            ctx.throwTypeError("Reflect.construct called on non-constructor")
        }
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
