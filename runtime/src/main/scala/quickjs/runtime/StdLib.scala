package quickjs.runtime

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.module.ModuleLoader
import quickjs.module.FileModuleLoader
import quickjs.runtime.builtins.BuiltinHelpers
import scala.collection.mutable
import java.math.{BigDecimal, BigInteger, MathContext, RoundingMode}
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale
import scala.util.Random
import scala.util.Sorting

/** Standard library initialization.
  *
  * Initializes built-in methods like Function.prototype.call, etc.
  * This is in a separate module to avoid circular dependencies between core and runtime.
  */
object StdLib:
  // Track for-of iteration indices for JSArrayVal (which doesn't have properties)
  private val forOfIndices = mutable.Map[Int, Int]()  // identityHashCode -> currentIndex

  private def initConstructor(
    constructor: quickjs.value.NativeConstructor,
    length: Int
  )(using ctx: JSContext): Unit =
    BuiltinHelpers.initConstructor(constructor, length)

  private final case class RegExpData(
    pattern: String, flags: String, global: Boolean, ignoreCase: Boolean,
    multiline: Boolean, dotAll: Boolean, unicode: Boolean, sticky: Boolean,
    regex: java.util.regex.Pattern
  )

  private def parseRegExpFlags(flags: String)(using ctx: JSContext) = BuiltinHelpers.parseRegExpFlags(flags)
  private def getRegExpData(value: JSValue)(using ctx: JSContext) = BuiltinHelpers.getRegExpData(value)

  private def callFunctionWithThis(
    funcValue: JSValue,
    thisValue: JSValue,
    args: Array[JSValue]
  )(using ctx: JSContext): JSValue =
    BuiltinHelpers.callFunctionWithThis(funcValue, thisValue, args)

  private def initializeForInHelpers(ctx: JSContext): Unit =
    quickjs.runtime.builtins.InternalHelpers.initializeForInHelpers(ctx)

  private def initializeModuleHelpers(ctx: JSContext, loader: Option[ModuleLoader]): Unit =
    quickjs.runtime.builtins.InternalHelpers.initializeModuleHelpers(ctx, loader)

  private def initializeArrayHelpers(ctx: JSContext): Unit =
    quickjs.runtime.builtins.InternalHelpers.initializeArrayHelpers(ctx)

  private def initializeObjectStatics(ctx: JSContext): Unit =
    quickjs.runtime.builtins.ObjectBuiltins.initialize(ctx)

  private def initializeMath(ctx: JSContext): Unit =
    import quickjs.objmodel.JSObject

    val mathObj = JSObject(prototype = null, extensible = true)
    given JSContext = ctx

    val absFunc = NativeFunction("abs", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.abs(args(1).toNumber))
    )
    mathObj.set("abs", JSValue.Native(absFunc))

    val floorFunc = NativeFunction("floor", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.floor(args(1).toNumber))
    )
    mathObj.set("floor", JSValue.Native(floorFunc))

    val ceilFunc = NativeFunction("ceil", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.ceil(args(1).toNumber))
    )
    mathObj.set("ceil", JSValue.Native(ceilFunc))

    val roundFunc = NativeFunction("round", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.round(args(1).toNumber))
    )
    mathObj.set("round", JSValue.Native(roundFunc))

    val maxFunc = NativeFunction("max", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        val values = args.drop(1).map(_.toNumber)
        JSValue.fromDouble(values.max)
    )
    mathObj.set("max", JSValue.Native(maxFunc))

    val minFunc = NativeFunction("min", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        val values = args.drop(1).map(_.toNumber)
        JSValue.fromDouble(values.min)
    )
    mathObj.set("min", JSValue.Native(minFunc))

    val powFunc = NativeFunction("pow", (args, _) =>
      if args.length < 3 then JSValue.fromInt(1)
      else JSValue.fromDouble(math.pow(args(1).toNumber, args(2).toNumber))
    )
    mathObj.set("pow", JSValue.Native(powFunc))

    val sqrtFunc = NativeFunction("sqrt", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.sqrt(args(1).toNumber))
    )
    mathObj.set("sqrt", JSValue.Native(sqrtFunc))

    val randomFunc = NativeFunction("random", (_, _) =>
      JSValue.fromDouble(Random.nextDouble())
    )
    mathObj.set("random", JSValue.Native(randomFunc))

    val sinFunc = NativeFunction("sin", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.sin(args(1).toNumber))
    )
    mathObj.set("sin", JSValue.Native(sinFunc))

    val cosFunc = NativeFunction("cos", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.cos(args(1).toNumber))
    )
    mathObj.set("cos", JSValue.Native(cosFunc))

    val tanFunc = NativeFunction("tan", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.tan(args(1).toNumber))
    )
    mathObj.set("tan", JSValue.Native(tanFunc))

    val asinFunc = NativeFunction("asin", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.asin(args(1).toNumber))
    )
    mathObj.set("asin", JSValue.Native(asinFunc))

    val acosFunc = NativeFunction("acos", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.acos(args(1).toNumber))
    )
    mathObj.set("acos", JSValue.Native(acosFunc))

    val atanFunc = NativeFunction("atan", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.atan(args(1).toNumber))
    )
    mathObj.set("atan", JSValue.Native(atanFunc))

    val atan2Func = NativeFunction("atan2", (args, _) =>
      if args.length < 3 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.atan2(args(1).toNumber, args(2).toNumber))
    )
    mathObj.set("atan2", JSValue.Native(atan2Func))

    val imulFunc = NativeFunction("imul", (args, _) =>
      if args.length < 3 then JSValue.fromInt(0)
      else
        val a = args(1).toNumber.toInt
        val b = args(2).toNumber.toInt
        JSValue.fromInt(a * b)
    )
    mathObj.set("imul", JSValue.Native(imulFunc))

    val froundFunc = NativeFunction("fround", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(args(1).toNumber.toFloat.toDouble)
    )
    mathObj.set("fround", JSValue.Native(froundFunc))

    val hypotFunc = NativeFunction("hypot", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        var result = 0.0
        var i = 1
        while i < args.length do
          result = math.hypot(result, args(i).toNumber)
          i += 1
        JSValue.fromDouble(result)
    )
    mathObj.set("hypot", JSValue.Native(hypotFunc))

    val expFunc = NativeFunction("exp", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.exp(args(1).toNumber))
    )
    mathObj.set("exp", JSValue.Native(expFunc))

    val logFunc = NativeFunction("log", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.log(args(1).toNumber))
    )
    mathObj.set("log", JSValue.Native(logFunc))

    val log10Func = NativeFunction("log10", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.log10(args(1).toNumber))
    )
    mathObj.set("log10", JSValue.Native(log10Func))

    val log2Func = NativeFunction("log2", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else JSValue.fromDouble(math.log(args(1).toNumber) / math.log(2.0))
    )
    mathObj.set("log2", JSValue.Native(log2Func))

    val truncFunc = NativeFunction("trunc", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        val value = args(1).toNumber
        val truncated = if value < 0 then math.ceil(value) else math.floor(value)
        JSValue.fromDouble(truncated)
    )
    mathObj.set("trunc", JSValue.Native(truncFunc))

    val signFunc = NativeFunction("sign", (args, _) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        val value = args(1).toNumber
        if value.isNaN then
          JSValue.Float64(Double.NaN)
        else if value == 0.0 then
          JSValue.Float64(value)
        else if value > 0 then
          JSValue.fromInt(1)
        else
          JSValue.fromInt(-1)
    )
    mathObj.set("sign", JSValue.Native(signFunc))

    val sumPreciseFunc = NativeFunction("sumPrecise", (args, ctx) =>
      if args.length <= 1 then JSValue.fromInt(0)
      else
        args(1) match
          case JSValue.JSArrayVal(arr) =>
            var sum = BigDecimal.ZERO
            var i = 0
            while i < arr.getLength do
              val value = arr.get(i).toNumber
              sum = sum.add(BigDecimal(value, MathContext.DECIMAL128))
              i += 1
            JSValue.fromDouble(sum.doubleValue())
          case _ =>
            ctx.throwTypeError("Math.sumPrecise expects an array")
    )
    mathObj.set("sumPrecise", JSValue.Native(sumPreciseFunc))

    mathObj.set("PI", JSValue.fromDouble(math.Pi))
    mathObj.set("E", JSValue.fromDouble(math.E))
    mathObj.set("SQRT2", JSValue.fromDouble(math.sqrt(2)))
    mathObj.set("SQRT1_2", JSValue.fromDouble(1.0 / math.sqrt(2)))
    mathObj.set("LN2", JSValue.fromDouble(math.log(2)))
    mathObj.set("LN10", JSValue.fromDouble(math.log(10)))
    mathObj.set("LOG2E", JSValue.fromDouble(1.0 / math.log(2)))
    mathObj.set("LOG10E", JSValue.fromDouble(1.0 / math.log(10)))

    ctx.global.set("Math", JSValue.Object(mathObj))

  private def initializeNumberString(ctx: JSContext): Unit =
    quickjs.runtime.builtins.NumberStringBuiltins.initialize(ctx)

  // Global symbol registry for Symbol.for() and Symbol.keyFor()
  private val globalSymbolRegistry = mutable.Map.empty[String, JSValue.Symbol]
  private var symbolCounter = 0

  // Well-known symbols storage
  private val wellKnownSymbols = mutable.Map.empty[String, JSValue.Symbol]

  private def getOrCreateWellKnownSymbol(name: String): JSValue.Symbol =
    wellKnownSymbols.getOrElseUpdate(name, {
      symbolCounter += 1
      JSValue.Symbol(symbolCounter)
    })

  private def initializeSymbol(ctx: JSContext): Unit =
    given JSContext = ctx

    // Create Symbol prototype
    val symbolPrototype = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)

    // Symbol constructor - when called without new, returns a new unique symbol
    val symbolConstructor = quickjs.value.NativeConstructor(
      name = "Symbol",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        // Symbol(description) returns a new unique symbol
        val description = args.headOption.getOrElse(JSValue.Undefined)
        symbolCounter += 1
        val sym = JSValue.Symbol(symbolCounter)
        // Store description on symbol object if we need Symbol.prototype.description
        sym
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Symbol is not a constructor"),
      prototype = symbolPrototype
    )
    initConstructor(symbolConstructor, length = 0)
    ctx.global.set("Symbol", JSValue.Native(symbolConstructor))
    symbolPrototype.defineProperty("constructor", JSValue.Native(symbolConstructor), enumerable = false)

    // Symbol.prototype.toString()
    val symbolToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Symbol(id)) => JSValue.fromString(s"Symbol($id)")
          case _ => ctx.throwTypeError("Symbol.prototype.toString called on non-Symbol")
    )
    symbolPrototype.defineProperty("toString", JSValue.Native(symbolToString),
      enumerable = false, writable = true, configurable = true
    )

    // Symbol.prototype.valueOf()
    val symbolValueOf = NativeFunction(
      name = "valueOf",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(sym: JSValue.Symbol) => sym
          case _ => ctx.throwTypeError("Symbol.prototype.valueOf called on non-Symbol")
    )
    symbolPrototype.defineProperty("valueOf", JSValue.Native(symbolValueOf),
      enumerable = false, writable = true, configurable = true
    )

    // Symbol.for(key) - returns a symbol from the global registry
    val symbolFor = NativeFunction(
      name = "for",
      impl = (args, ctx) =>
        given JSContext = ctx
        // args(0) is 'this', args(1) is the actual argument
        val key = if args.length > 1 then args(1).toString else ""
        globalSymbolRegistry.getOrElseUpdate(key, {
          symbolCounter += 1
          JSValue.Symbol(symbolCounter)
        })
    )
    symbolConstructor.funcObj.set("for", JSValue.Native(symbolFor))

    // Symbol.keyFor(sym) - returns the key for a symbol in the global registry
    val symbolKeyFor = NativeFunction(
      name = "keyFor",
      impl = (args, ctx) =>
        given JSContext = ctx
        // args(0) is 'this', args(1) is the actual argument
        val symArg = if args.length > 1 then args(1) else JSValue.Undefined
        symArg match
          case JSValue.Symbol(id) =>
            // Find the key for this symbol
            globalSymbolRegistry.find { case (_, sym) => sym.value == id } match
              case Some((key, _)) => JSValue.fromString(key)
              case None => JSValue.Undefined
          case _ =>
            ctx.throwTypeError("Symbol.keyFor requires a symbol argument")
    )
    symbolConstructor.funcObj.set("keyFor", JSValue.Native(symbolKeyFor))

    // Well-known symbols
    // Symbol.iterator - for...of loops, spread operator
    val symIterator = getOrCreateWellKnownSymbol("iterator")
    symbolConstructor.funcObj.set("iterator", symIterator)

    // Symbol.asyncIterator - for await...of loops
    val symAsyncIterator = getOrCreateWellKnownSymbol("asyncIterator")
    symbolConstructor.funcObj.set("asyncIterator", symAsyncIterator)

    // Symbol.toStringTag - used by Object.prototype.toString
    val symToStringTag = getOrCreateWellKnownSymbol("toStringTag")
    symbolConstructor.funcObj.set("toStringTag", symToStringTag)

    // Symbol.hasInstance - used by instanceof
    val symHasInstance = getOrCreateWellKnownSymbol("hasInstance")
    symbolConstructor.funcObj.set("hasInstance", symHasInstance)

    // Symbol.species - used for creating derived objects
    val symSpecies = getOrCreateWellKnownSymbol("species")
    symbolConstructor.funcObj.set("species", symSpecies)

  private def initializeRegExp(ctx: JSContext): Unit =
    val regexpPrototype = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
    given JSContext = ctx

    def buildRegExp(patternValue: JSValue, flagsValue: JSValue): JSValue =
      val (pattern, flags) =
        getRegExpData(patternValue) match
          case Some((_, data)) =>
            if flagsValue == JSValue.Undefined then
              return patternValue
            else
              (data.pattern, flagsValue.toString)
          case None =>
            (patternValue.toString, if flagsValue == JSValue.Undefined then "" else flagsValue.toString)
      val (_, global, ignoreCase, multiline, dotAll, unicode, sticky) = parseRegExpFlags(flags) // validate flags
      val obj = quickjs.objmodel.JSObject(prototype = regexpPrototype, extensible = true)
      obj.defineProperty("__regexpPattern", JSValue.fromString(pattern), enumerable = false)(using ctx)
      obj.defineProperty("__regexpFlags", JSValue.fromString(flags), enumerable = false)(using ctx)
      obj.defineProperty("source", JSValue.fromString(pattern), enumerable = false)(using ctx)
      obj.defineProperty("flags", JSValue.fromString(flags), enumerable = false)(using ctx)
      obj.defineProperty("global", JSValue.fromBoolean(global), enumerable = false)(using ctx)
      obj.defineProperty("ignoreCase", JSValue.fromBoolean(ignoreCase), enumerable = false)(using ctx)
      obj.defineProperty("multiline", JSValue.fromBoolean(multiline), enumerable = false)(using ctx)
      obj.defineProperty("dotAll", JSValue.fromBoolean(dotAll), enumerable = false)(using ctx)
      obj.defineProperty("unicode", JSValue.fromBoolean(unicode), enumerable = false)(using ctx)
      obj.defineProperty("sticky", JSValue.fromBoolean(sticky), enumerable = false)(using ctx)
      obj.defineProperty("lastIndex", JSValue.fromInt(0), enumerable = false, writable = true, configurable = false)(using ctx)
      JSValue.Object(obj)

    val regexpConstructor = quickjs.value.NativeConstructor(
      name = "RegExp",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        val pattern = if args.nonEmpty then args(0) else JSValue.fromString("")
        val flags = if args.length > 1 then args(1) else JSValue.Undefined
        buildRegExp(pattern, flags),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val pattern = if args.nonEmpty then args(0) else JSValue.fromString("")
        val flags = if args.length > 1 then args(1) else JSValue.Undefined
        buildRegExp(pattern, flags),
      prototype = regexpPrototype
    )
    initConstructor(regexpConstructor, length = 2)

    val regexpExec = NativeFunction(
      name = "exec",
      impl = (args, ctx) =>
        given JSContext = ctx
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        val input = if args.length > 1 then args(1).toString else ""
        getRegExpData(thisValue) match
          case Some((obj, data)) =>
            val start =
              if data.global then math.max(0, obj.get("lastIndex")(using ctx).toNumber.toInt)
              else 0
            val matcher = data.regex.matcher(input)
            if matcher.find(start) then
              if data.global then
                obj.set("lastIndex", JSValue.fromInt(matcher.end()))(using ctx)
              val arr = quickjs.objmodel.JSArray.empty()
              var i = 0
              while i <= matcher.groupCount() do
                arr.push(JSValue.fromString(matcher.group(i)))
                i += 1
              arr.setProperty("index", JSValue.fromInt(matcher.start()))
              arr.setProperty("input", JSValue.fromString(input))
              arr.setProperty("groups", JSValue.Undefined)
              JSValue.JSArrayVal(arr)
            else
              if data.global then obj.set("lastIndex", JSValue.fromInt(0))(using ctx)
              JSValue.Null
          case None =>
            ctx.throwTypeError("RegExp.prototype.exec called on non-RegExp")
    )

    val regexpTest = NativeFunction(
      name = "test",
      impl = (args, ctx) =>
        given JSContext = ctx
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        val input = if args.length > 1 then args(1).toString else ""
        getRegExpData(thisValue) match
          case Some((obj, data)) =>
            val start =
              if data.global then math.max(0, obj.get("lastIndex")(using ctx).toNumber.toInt)
              else 0
            val matcher = data.regex.matcher(input)
            val matched = matcher.find(start)
            if matched && data.global then
              obj.set("lastIndex", JSValue.fromInt(matcher.end()))(using ctx)
            else if !matched && data.global then
              obj.set("lastIndex", JSValue.fromInt(0))(using ctx)
            JSValue.fromBoolean(matched)
          case None =>
            ctx.throwTypeError("RegExp.prototype.test called on non-RegExp")
    )

    val regexpToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val thisValue = if args.nonEmpty then args(0) else JSValue.Undefined
        getRegExpData(thisValue) match
          case Some((_, data)) =>
            JSValue.fromString(s"/${data.pattern}/${data.flags}")
          case None =>
            ctx.throwTypeError("RegExp.prototype.toString called on non-RegExp")
    )

    regexpPrototype.defineProperty("exec", JSValue.Native(regexpExec), enumerable = false)
    regexpPrototype.defineProperty("test", JSValue.Native(regexpTest), enumerable = false)
    regexpPrototype.defineProperty("toString", JSValue.Native(regexpToString), enumerable = false)
    regexpPrototype.set("constructor", JSValue.Native(regexpConstructor))
    ctx.global.set("RegExp", JSValue.Native(regexpConstructor))

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

  private def initializeReflect(ctx: JSContext): Unit =
    import quickjs.objmodel.{JSObject, JSArray}
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
                    val interpreter = new quickjs.interpreter.Interpreter()
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
                    val interpreter = new quickjs.interpreter.Interpreter()
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
            val interpreter = new quickjs.interpreter.Interpreter()
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

            // Get the prototype from newTarget or target
            val newTarget = newTargetArg.getOrElse(target)
            val prototypeSource = newTarget match
              case ntFunc: JSValue.Function => ntFunc.funcObj
              case _ => func.funcObj

            val funcPrototype = prototypeSource.get("prototype") match
              case JSValue.Object(proto) => proto
              case _ => ctx.objectPrototype

            // Create new object
            val newObj = JSObject(prototype = funcPrototype, extensible = true)

            val interpreter = new quickjs.interpreter.Interpreter()
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

            // If function returns an object, use that; otherwise return the new object
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

    val dateGetFullYear = NativeFunction(
      name = "getFullYear",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getFullYear")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else
          val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(value.toLong), ZoneId.systemDefault())
          JSValue.fromInt(zdt.getYear)
    )

    val dateGetMonth = NativeFunction(
      name = "getMonth",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getMonth")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else
          val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(value.toLong), ZoneId.systemDefault())
          JSValue.fromInt(zdt.getMonthValue - 1) // JavaScript months are 0-indexed
    )

    val dateGetDate = NativeFunction(
      name = "getDate",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getDate")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else
          val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(value.toLong), ZoneId.systemDefault())
          JSValue.fromInt(zdt.getDayOfMonth)
    )

    val dateGetHours = NativeFunction(
      name = "getHours",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getHours")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else
          val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(value.toLong), ZoneId.systemDefault())
          JSValue.fromInt(zdt.getHour)
    )

    val dateGetMinutes = NativeFunction(
      name = "getMinutes",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getMinutes")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else
          val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(value.toLong), ZoneId.systemDefault())
          JSValue.fromInt(zdt.getMinute)
    )

    val dateGetSeconds = NativeFunction(
      name = "getSeconds",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getSeconds")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else
          val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(value.toLong), ZoneId.systemDefault())
          JSValue.fromInt(zdt.getSecond)
    )

    val dateGetDay = NativeFunction(
      name = "getDay",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getDay")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else
          val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(value.toLong), ZoneId.systemDefault())
          // JavaScript: Sunday = 0, Monday = 1, ..., Saturday = 6
          // Java: Monday = 1, ..., Sunday = 7
          val javaDay = zdt.getDayOfWeek.getValue
          JSValue.fromInt(if javaDay == 7 then 0 else javaDay)
    )

    val dateGetMilliseconds = NativeFunction(
      name = "getMilliseconds",
      impl = (args, ctx) =>
        given JSContext = ctx
        val (_, value) = requireDateObject(args, "getMilliseconds")
        if value.isNaN then JSValue.fromDouble(Double.NaN)
        else
          val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(value.toLong), ZoneId.systemDefault())
          JSValue.fromInt(zdt.getNano / 1000000)
    )

    datePrototype.defineProperty("toISOString", JSValue.Native(dateToISOString), enumerable = false)
    datePrototype.defineProperty("toString", JSValue.Native(dateToString), enumerable = false)
    datePrototype.defineProperty("getTime", JSValue.Native(dateGetTime), enumerable = false)
    datePrototype.defineProperty("valueOf", JSValue.Native(dateValueOf), enumerable = false)
    datePrototype.defineProperty("setUTCHours", JSValue.Native(dateSetUTCHours), enumerable = false)
    datePrototype.defineProperty("getFullYear", JSValue.Native(dateGetFullYear), enumerable = false)
    datePrototype.defineProperty("getMonth", JSValue.Native(dateGetMonth), enumerable = false)
    datePrototype.defineProperty("getDate", JSValue.Native(dateGetDate), enumerable = false)
    datePrototype.defineProperty("getHours", JSValue.Native(dateGetHours), enumerable = false)
    datePrototype.defineProperty("getMinutes", JSValue.Native(dateGetMinutes), enumerable = false)
    datePrototype.defineProperty("getSeconds", JSValue.Native(dateGetSeconds), enumerable = false)
    datePrototype.defineProperty("getDay", JSValue.Native(dateGetDay), enumerable = false)
    datePrototype.defineProperty("getMilliseconds", JSValue.Native(dateGetMilliseconds), enumerable = false)

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
          callFunctionValue(callback, JSValue.Undefined, Array.empty)
        }
        JSValue.Undefined
    )
    ctx.global.set("queueMicrotask", JSValue.Native(queueMicrotaskFunc))

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

    // Function.prototype.apply(thisArg, argsArray)
    val functionPrototypeApply = NativeFunction(
      name = "apply",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Function.prototype.apply called on non-function")
        else
          val func = args(0)  // The function to call
          val thisArg = if args.length > 1 then args(1) else JSValue.Undefined
          // Get arguments from array
          val actualArgs: Array[JSValue] = if args.length > 2 then
            args(2) match
              case JSValue.JSArrayVal(arr) =>
                val len = arr.length
                val result = new Array[JSValue](len)
                for i <- 0 until len do
                  result(i) = arr.get(i)
                result
              case JSValue.Null | JSValue.Undefined => Array.empty[JSValue]
              case other =>
                // Try to treat as array-like
                other match
                  case JSValue.Object(obj) =>
                    given JSContext = ctx
                    obj.get("length") match
                      case JSValue.Int32(len) =>
                        val result = new Array[JSValue](len)
                        for i <- 0 until len do
                          result(i) = obj.get(i.toString)
                        result
                      case _ => throw new RuntimeException("CreateListFromArrayLike called on non-object")
                  case _ => throw new RuntimeException("CreateListFromArrayLike called on non-object")
          else Array.empty[JSValue]

          func match
            case f: JSValue.Function =>
              given JSContext = ctx
              val interpreter = Interpreter()
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
              nativeFuncWrapper match
                case native: NativeFunction =>
                  val argsWithThis = new Array[JSValue](actualArgs.length + 1)
                  argsWithThis(0) = thisArg
                  Array.copy(actualArgs, 0, argsWithThis, 1, actualArgs.length)
                  given JSContext = ctx
                  native.call(argsWithThis)
                case constructor: quickjs.value.NativeConstructor =>
                  given JSContext = ctx
                  constructor.call(actualArgs)
                case _ =>
                  throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
            case _ =>
              throw new RuntimeException(s"Function.prototype.apply called on non-function: $func")
    )
    ctx.functionPrototype.set("apply", JSValue.Native(functionPrototypeApply))

    // Function.prototype.bind(thisArg, arg1, arg2, ...)
    val functionPrototypeBind = NativeFunction(
      name = "bind",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Function.prototype.bind called on non-function")
        else
          val func = args(0)  // The function to bind
          val boundThis = if args.length > 1 then args(1) else JSValue.Undefined
          val boundArgs = if args.length > 2 then args.slice(2, args.length) else Array.empty[JSValue]

          // Create a bound function
          val boundFunction = NativeFunction(
            name = "bound",
            impl = (callArgs, callCtx) =>
              // callArgs(0) is the thisArg passed to the bound function (ignored)
              val actualCallArgs = if callArgs.length > 1 then callArgs.slice(1, callArgs.length) else Array.empty[JSValue]
              // Combine bound args with call args
              val combinedArgs = boundArgs ++ actualCallArgs

              func match
                case f: JSValue.Function =>
                  given JSContext = callCtx
                  val interpreter = Interpreter()
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
                  interpreter.call(bcFunc, boundThis, combinedArgs, f.closure)
                case JSValue.Native(nativeFuncWrapper) =>
                  nativeFuncWrapper match
                    case native: NativeFunction =>
                      val argsWithThis = new Array[JSValue](combinedArgs.length + 1)
                      argsWithThis(0) = boundThis
                      Array.copy(combinedArgs, 0, argsWithThis, 1, combinedArgs.length)
                      given JSContext = callCtx
                      native.call(argsWithThis)
                    case constructor: quickjs.value.NativeConstructor =>
                      given JSContext = callCtx
                      constructor.call(combinedArgs)
                    case _ =>
                      throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
                case _ =>
                  throw new RuntimeException(s"Bound function called on non-function: $func")
          )
          JSValue.Native(boundFunction)
    )
    ctx.functionPrototype.set("bind", JSValue.Native(functionPrototypeBind))

    val functionPrototypeToString = NativeFunction(
      name = "toString",
      impl = (_, _) => JSValue.fromString("[object Function]")
    )
    ctx.functionPrototype.set("toString", JSValue.Native(functionPrototypeToString))

  private def initializeArrayConstructor(ctx: JSContext): Unit =
    def buildArray(values: Seq[JSValue]): JSValue =
      val arr = quickjs.objmodel.JSArray.empty()
      values.foreach(arr.push)
      JSValue.JSArrayVal(arr)

    def buildArrayFromArgs(args: Array[JSValue], offset: Int): JSValue =
      if args.length == offset then
        buildArray(Seq.empty)
      else if args.length == offset + 1 then
        args(offset) match
          case JSValue.Int32(i) =>
            if i < 0 then ctx.throwRangeError("Invalid array length")
            JSValue.JSArrayVal(quickjs.objmodel.JSArray(i))
          case JSValue.Float64(d) =>
            if d.isNaN || d.isInfinite || d < 0 || d != math.floor(d) then
              ctx.throwRangeError("Invalid array length")
            else if d > Int.MaxValue then
              ctx.throwRangeError("Invalid array length")
            else
              JSValue.JSArrayVal(quickjs.objmodel.JSArray(d.toInt))
          case _ =>
            buildArray(Seq(args(offset)))
      else
        buildArray(args.drop(offset).toSeq)

    val arrayConstructor = quickjs.value.NativeConstructor(
      name = "Array",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        val offset = if args.length >= 2 then 1 else 0
        buildArrayFromArgs(args, offset),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val offset = if args.length >= 2 then 1 else 0
        buildArrayFromArgs(args, offset),
      prototype = ctx.arrayPrototype
    )
    given JSContext = ctx
    initConstructor(arrayConstructor, length = 1)
    ctx.global.set("Array", JSValue.Native(arrayConstructor))
    ctx.arrayPrototype.defineProperty("constructor", JSValue.Native(arrayConstructor), enumerable = false)(using ctx)

    val arrayIsArray = NativeFunction(
      name = "isArray",
      impl = (args, _) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then
          JSValue.Bool(false)
        else
          JSValue.fromBoolean(args(offset).isInstanceOf[JSValue.JSArrayVal])
    )

    val arrayOf = NativeFunction(
      name = "of",
      impl = (args, _) =>
        val offset = if args.length >= 2 then 1 else 0
        val arr = quickjs.objmodel.JSArray.empty()
        var i = offset
        while i < args.length do
          arr.push(args(i))
          i += 1
        JSValue.JSArrayVal(arr)
    )

    val arrayFrom = NativeFunction(
      name = "from",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then
          JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else
          val source = args(offset)
          val mapFn = if args.length > offset + 1 then Some(args(offset + 1)) else None
          val thisArg = if args.length > offset + 2 then args(offset + 2) else JSValue.Undefined
          val result = quickjs.objmodel.JSArray.empty()
          given JSContext = ctx

          def pushValue(value: JSValue, index: Int): Unit =
            val mapped =
              mapFn match
                case Some(func) =>
                  callFunctionWithThis(func, thisArg, Array(value, JSValue.fromInt(index), source))
                case None => value
            result.push(mapped)

          source match
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do
                pushValue(arr.get(i), i)
                i += 1
            case JSValue.JSStr(str) =>
              var i = 0
              while i < str.length do
                pushValue(JSValue.fromString(str.charAt(i).toString), i)
                i += 1
            case JSValue.Object(obj) =>
              val len = obj.get("length").toNumber.toInt
              var i = 0
              while i < len do
                pushValue(obj.get(i.toString), i)
                i += 1
            case func: JSValue.Function =>
              val len = func.funcObj.get("length").toNumber.toInt
              var i = 0
              while i < len do
                pushValue(func.funcObj.get(i.toString), i)
                i += 1
            case _ => ()

          JSValue.JSArrayVal(result)
    )

    arrayConstructor.funcObj.set("isArray", JSValue.Native(arrayIsArray))
    arrayConstructor.funcObj.set("of", JSValue.Native(arrayOf))
    arrayConstructor.funcObj.set("from", JSValue.Native(arrayFrom))

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

    val arrayPrototypeEvery = NativeFunction(
      name = "every",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.every requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              var index = 0
              var passed = true
              while index < arr.getLength && passed do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                passed = callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx).toBoolean
                index += 1
              JSValue.fromBoolean(passed)
            case _ =>
              throw new RuntimeException(s"Array.prototype.every called on non-array: $arrValue")
    )

    val arrayPrototypeSome = NativeFunction(
      name = "some",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.some requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              var index = 0
              var found = false
              while index < arr.getLength && !found do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                found = callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx).toBoolean
                index += 1
              JSValue.fromBoolean(found)
            case _ =>
              throw new RuntimeException(s"Array.prototype.some called on non-array: $arrValue")
    )

    val arrayPrototypeFind = NativeFunction(
      name = "find",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.find requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              var index = 0
              var found: JSValue = JSValue.Undefined
              var done = false
              while index < arr.getLength && !done do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                if callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx).toBoolean then
                  found = elem
                  done = true
                index += 1
              found
            case _ =>
              throw new RuntimeException(s"Array.prototype.find called on non-array: $arrValue")
    )

    val arrayPrototypeFindIndex = NativeFunction(
      name = "findIndex",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.findIndex requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              var index = 0
              var found = -1
              while index < arr.getLength && found < 0 do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                if callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx).toBoolean then
                  found = index
                index += 1
              JSValue.fromInt(found)
            case _ =>
              throw new RuntimeException(s"Array.prototype.findIndex called on non-array: $arrValue")
    )

    val arrayPrototypeReverse = NativeFunction(
      name = "reverse",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.reverse called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              var i = 0
              while i < len / 2 do
                val left = arr.get(i)
                val right = arr.get(len - 1 - i)
                arr.set(i, right)
                arr.set(len - 1 - i, left)
                i += 1
              arrVal
            case _ =>
              throw new RuntimeException(s"Array.prototype.reverse called on non-array: $arrValue")
    )

    val arrayPrototypeFill = NativeFunction(
      name = "fill",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.fill called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              val value = if args.length > 1 then args(1) else JSValue.Undefined
              val startRaw = if args.length > 2 then args(2).toNumber.toInt else 0
              val endRaw = if args.length > 3 then args(3).toNumber.toInt else len
              val start = if startRaw < 0 then math.max(len + startRaw, 0) else math.min(startRaw, len)
              val end = if endRaw < 0 then math.max(len + endRaw, 0) else math.min(endRaw, len)
              var i = start
              while i < end do
                arr.set(i, value)
                i += 1
              arrVal
            case _ =>
              throw new RuntimeException(s"Array.prototype.fill called on non-array: $arrValue")
    )

    val arrayPrototypeAt = NativeFunction(
      name = "at",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.at called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              val indexRaw = if args.length > 1 then args(1).toNumber.toInt else 0
              val index = if indexRaw < 0 then len + indexRaw else indexRaw
              if index < 0 || index >= len then JSValue.Undefined else arr.get(index)
            case _ =>
              throw new RuntimeException(s"Array.prototype.at called on non-array: $arrValue")
    )

    val arrayPrototypeCopyWithin = NativeFunction(
      name = "copyWithin",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.copyWithin called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              val targetRaw = if args.length > 1 then args(1).toNumber.toInt else 0
              val startRaw = if args.length > 2 then args(2).toNumber.toInt else 0
              val endRaw = if args.length > 3 then args(3).toNumber.toInt else len
              val target = if targetRaw < 0 then math.max(len + targetRaw, 0) else math.min(targetRaw, len)
              val start = if startRaw < 0 then math.max(len + startRaw, 0) else math.min(startRaw, len)
              val end = if endRaw < 0 then math.max(len + endRaw, 0) else math.min(endRaw, len)
              val count = math.min(end - start, len - target)
              if count > 0 then
                val direction =
                  if start < target && target < start + count then -1 else 1
                var i = if direction > 0 then 0 else count - 1
                while i >= 0 && i < count do
                  val value = arr.get(start + i)
                  arr.set(target + i, value)
                  i += direction
              arrVal
            case _ =>
              throw new RuntimeException(s"Array.prototype.copyWithin called on non-array: $arrValue")
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

    val arrayPrototypeShift = NativeFunction(
      name = "shift",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.shift called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              if len == 0 then
                JSValue.Undefined
              else
                val first = arr.get(0)
                var i = 1
                while i < len do
                  arr.set(i - 1, arr.get(i))
                  i += 1
                arr.setLength(len - 1)
                first
            case _ =>
              throw new RuntimeException(s"Array.prototype.shift called on non-array: $arrValue")
    )

    val arrayPrototypeUnshift = NativeFunction(
      name = "unshift",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.unshift called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val elementsToAdd =
                if args.length > 1 then args.slice(1, args.length)
                else Array.empty[JSValue]
              val len = arr.getLength
              val addCount = elementsToAdd.length
              var i = len - 1
              while i >= 0 do
                arr.set(i + addCount, arr.get(i))
                i -= 1
              var j = 0
              while j < addCount do
                arr.set(j, elementsToAdd(j))
                j += 1
              JSValue.fromInt(arr.getLength)
            case _ =>
              throw new RuntimeException(s"Array.prototype.unshift called on non-array: $arrValue")
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

    val arrayPrototypeReduceRight = NativeFunction(
      name = "reduceRight",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.reduceRight requires a callback function")
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
                else arr.get(len - 1)
              var index = if hasInitial then len - 1 else len - 2
              while index >= 0 do
                val elem = arr.get(index)
                val callbackArgs = Array(acc, elem, JSValue.fromInt(index), arrVal)
                acc = callFunctionWithThis(callback, JSValue.Undefined, callbackArgs)(using ctx)
                index -= 1
              acc
            case _ =>
              throw new RuntimeException(s"Array.prototype.reduceRight called on non-array: $arrValue")
    )

    val arrayPrototypeSort = NativeFunction(
      name = "sort",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.sort called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val len = arr.getLength
              val compareFn = if args.length > 1 then Some(args(1)) else None
              val values = new Array[JSValue](len)
              var i = 0
              while i < len do
                values(i) = arr.get(i)
                i += 1
              def compareValues(a: JSValue, b: JSValue): Int =
                compareFn match
                  case Some(func) =>
                    val result = callFunctionWithThis(func, JSValue.Undefined, Array(a, b))(using ctx)
                    val num = result.toNumber
                    if num.isNaN then 0
                    else if num < 0 then -1
                    else if num > 0 then 1
                    else 0
                  case None =>
                    a.toString.compareTo(b.toString)
              Sorting.stableSort(values, (a, b) => compareValues(a, b) < 0)
              i = 0
              while i < len do
                arr.set(i, values(i))
                i += 1
              arrVal
            case _ =>
              throw new RuntimeException(s"Array.prototype.sort called on non-array: $arrValue")
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

    val arrayPrototypeJoin = NativeFunction(
      name = "join",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.join called on non-array")
        else
          val arrValue = args(0)
          val separator =
            if args.length > 1 && args(1) != JSValue.Undefined then args(1).toString else ","
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val sb = new StringBuilder()
              var i = 0
              while i < arr.getLength do
                if i > 0 then sb.append(separator)
                val elem = arr.get(i)
                elem match
                  case JSValue.Undefined | JSValue.Null => ()
                  case _ => sb.append(elem.toString)
                i += 1
              JSValue.fromString(sb.toString)
            case _ =>
              throw new RuntimeException(s"Array.prototype.join called on non-array: $arrValue")
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
    ctx.arrayPrototype.set("every", JSValue.Native(arrayPrototypeEvery))
    ctx.arrayPrototype.set("some", JSValue.Native(arrayPrototypeSome))
    ctx.arrayPrototype.set("find", JSValue.Native(arrayPrototypeFind))
    ctx.arrayPrototype.set("findIndex", JSValue.Native(arrayPrototypeFindIndex))
    ctx.arrayPrototype.set("reverse", JSValue.Native(arrayPrototypeReverse))
    ctx.arrayPrototype.set("fill", JSValue.Native(arrayPrototypeFill))
    ctx.arrayPrototype.set("at", JSValue.Native(arrayPrototypeAt))
    ctx.arrayPrototype.set("copyWithin", JSValue.Native(arrayPrototypeCopyWithin))
    ctx.arrayPrototype.set("splice", JSValue.Native(arrayPrototypeSplice))
    ctx.arrayPrototype.set("shift", JSValue.Native(arrayPrototypeShift))
    ctx.arrayPrototype.set("unshift", JSValue.Native(arrayPrototypeUnshift))
    ctx.arrayPrototype.set("toString", JSValue.Native(arrayPrototypeToString))
    ctx.arrayPrototype.set("reduceRight", JSValue.Native(arrayPrototypeReduceRight))
    ctx.arrayPrototype.set("sort", JSValue.Native(arrayPrototypeSort))
    ctx.arrayPrototype.set("join", JSValue.Native(arrayPrototypeJoin))
    ctx.arrayPrototype.set("concat", JSValue.Native(arrayPrototypeConcat))
    ctx.arrayPrototype.set("slice", JSValue.Native(arrayPrototypeSlice))

    // Array.prototype.flat(depth)
    val arrayPrototypeFlat = NativeFunction(
      name = "flat",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.flat called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              // Default depth is 1
              val depth = if args.length > 1 then
                args(1) match
                  case JSValue.Int32(d) => d
                  case JSValue.Float64(d) => d.toInt
                  case JSValue.Undefined => 1
                  case _ => 1
              else 1

              def flattenArray(source: quickjs.objmodel.JSArray, currentDepth: Int): quickjs.objmodel.JSArray =
                val result = quickjs.objmodel.JSArray.empty()
                val len = source.getLength
                for i <- 0 until len do
                  source.get(i) match
                    case inner: JSValue.JSArrayVal if currentDepth > 0 =>
                      val flattened = flattenArray(inner.value, currentDepth - 1)
                      val flatLen = flattened.getLength
                      for j <- 0 until flatLen do
                        result.push(flattened.get(j))
                    case v => result.push(v)
                result

              JSValue.JSArrayVal(flattenArray(arr, depth))
            case _ =>
              ctx.throwTypeError("Array.prototype.flat called on non-array")
    )
    ctx.arrayPrototype.set("flat", JSValue.Native(arrayPrototypeFlat))

    // Array.prototype.flatMap(callback, thisArg)
    val arrayPrototypeFlatMap = NativeFunction(
      name = "flatMap",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.flatMap called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val callback = if args.length > 1 then args(1) else JSValue.Undefined
              val thisArg = if args.length > 2 then args(2) else JSValue.Undefined

              val result = quickjs.objmodel.JSArray.empty()
              val interpreter = Interpreter()
              val len = arr.getLength

              for i <- 0 until len do
                val elem = arr.get(i)
                val callArgs = Array[JSValue](elem, JSValue.fromInt(i), arrValue)
                val mapped = callback match
                  case f: JSValue.Function =>
                    val bcFunc = new BytecodeFunction(
                      name = f.name, bytecode = f.bytecode, constants = f.constants,
                      stackSize = f.stackSize, freeVars = Array.empty, paramNames = f.paramNames,
                      localVarNames = f.localVarNames, argumentsIndex = f.argumentsIndex,
                      isConstructor = f.isConstructor
                    )
                    interpreter.call(bcFunc, thisArg, callArgs, f.closure)
                  case JSValue.Native(nf: NativeFunction) =>
                    val argsWithThis = new Array[JSValue](callArgs.length + 1)
                    argsWithThis(0) = thisArg
                    Array.copy(callArgs, 0, argsWithThis, 1, callArgs.length)
                    nf.call(argsWithThis)
                  case _ =>
                    ctx.throwTypeError("flatMap callback is not a function")

                // Flatten one level
                mapped match
                  case inner: JSValue.JSArrayVal =>
                    val innerLen = inner.value.getLength
                    for j <- 0 until innerLen do
                      result.push(inner.value.get(j))
                  case v => result.push(v)

              JSValue.JSArrayVal(result)
            case _ =>
              ctx.throwTypeError("Array.prototype.flatMap called on non-array")
    )
    ctx.arrayPrototype.set("flatMap", JSValue.Native(arrayPrototypeFlatMap))

  // ============================================================
  // Map Implementation
  // ============================================================

  /** Internal storage class for Map - uses AnyRef wrapper for proper key comparison */
  private final class JSMapStorage:
    // We use a LinkedHashMap to maintain insertion order
    // Keys are wrapped in MapKey to handle SameValueZero comparison
    private val storage = mutable.LinkedHashMap.empty[MapKey, JSValue]

    def get(key: JSValue): Option[JSValue] = storage.get(MapKey(key))
    def set(key: JSValue, value: JSValue): Unit = storage.update(MapKey(key), value)
    def has(key: JSValue): Boolean = storage.contains(MapKey(key))
    def delete(key: JSValue): Boolean =
      val k = MapKey(key)
      if storage.contains(k) then
        storage.remove(k)
        true
      else false
    def clear(): Unit = storage.clear()
    def size: Int = storage.size
    def entries: Iterator[(JSValue, JSValue)] = storage.iterator.map { case (k, v) => (k.value, v) }
    def keys: Iterator[JSValue] = storage.keysIterator.map(_.value)
    def values: Iterator[JSValue] = storage.valuesIterator

  /** Wrapper for Map keys that implements SameValueZero comparison */
  private final case class MapKey(value: JSValue):
    override def hashCode(): Int = value match
      case JSValue.Float64(d) if d.isNaN => 0 // All NaN values hash the same
      case JSValue.Float64(0.0) => 0 // +0 and -0 hash the same
      case JSValue.Int32(0) => 0
      case JSValue.Object(obj) => System.identityHashCode(obj)
      case JSValue.JSArrayVal(arr) => System.identityHashCode(arr)
      case f: JSValue.Function => System.identityHashCode(f)
      case JSValue.Native(n) => System.identityHashCode(n)
      case _ => value.hashCode()

    override def equals(other: Any): Boolean = other match
      case MapKey(otherValue) => sameValueZero(value, otherValue)
      case _ => false

    private def sameValueZero(a: JSValue, b: JSValue): Boolean = (a, b) match
      case (JSValue.Float64(x), JSValue.Float64(y)) if x.isNaN && y.isNaN => true
      case (JSValue.Float64(x), JSValue.Float64(y)) => x == y // handles +0 == -0
      case (JSValue.Int32(x), JSValue.Int32(y)) => x == y
      case (JSValue.Int32(x), JSValue.Float64(y)) => x.toDouble == y
      case (JSValue.Float64(x), JSValue.Int32(y)) => x == y.toDouble
      case (JSValue.Object(x), JSValue.Object(y)) => x eq y
      case (JSValue.JSArrayVal(x), JSValue.JSArrayVal(y)) => x eq y
      case (x: JSValue.Function, y: JSValue.Function) => x eq y
      case (JSValue.Native(x), JSValue.Native(y)) => x eq y
      case _ => a == b

  private def getMapStorage(obj: quickjs.objmodel.JSObject)(using ctx: JSContext): Option[JSMapStorage] =
    obj.getOwnProperty("__mapStorage") match
      case Some(JSValue.Native(storage: JSMapStorage)) => Some(storage)
      case _ => None

  private def initializeMap(ctx: JSContext): Unit =
    given JSContext = ctx

    val mapConstructor = quickjs.value.NativeConstructor(
      name = "Map",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Constructor Map requires 'new'"),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val obj = quickjs.objmodel.JSObject(prototype = ctx.mapPrototype, extensible = true)
        val storage = new JSMapStorage()
        obj.defineProperty("__mapStorage", JSValue.Native(storage), enumerable = false, writable = false, configurable = false)

        // If iterable is provided, add entries
        if args.nonEmpty && args(0) != JSValue.Null && args(0) != JSValue.Undefined then
          args(0) match
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do
                arr.get(i) match
                  case JSValue.JSArrayVal(entry) if entry.getLength >= 2 =>
                    storage.set(entry.get(0), entry.get(1))
                  case JSValue.Object(entryObj) =>
                    val key = entryObj.get("0")
                    val value = entryObj.get("1")
                    storage.set(key, value)
                  case _ =>
                    ctx.throwTypeError("Iterator value is not an entry object")
                i += 1
            case JSValue.Object(iterObj) =>
              // Try to iterate if it's array-like
              val len = iterObj.get("length").toNumber.toInt
              var i = 0
              while i < len do
                iterObj.get(i.toString) match
                  case JSValue.JSArrayVal(entry) if entry.getLength >= 2 =>
                    storage.set(entry.get(0), entry.get(1))
                  case JSValue.Object(entryObj) =>
                    val key = entryObj.get("0")
                    val value = entryObj.get("1")
                    storage.set(key, value)
                  case _ =>
                    ctx.throwTypeError("Iterator value is not an entry object")
                i += 1
            case _ => ()

        JSValue.Object(obj),
      prototype = ctx.mapPrototype
    )
    initConstructor(mapConstructor, length = 0)
    ctx.global.set("Map", JSValue.Native(mapConstructor))
    ctx.mapPrototype.defineProperty("constructor", JSValue.Native(mapConstructor), enumerable = false)

    // Map.prototype.get(key)
    val mapGet = NativeFunction(
      name = "get",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match
              case Some(storage) =>
                val key = if args.length > 1 then args(1) else JSValue.Undefined
                storage.get(key).getOrElse(JSValue.Undefined)
              case None => ctx.throwTypeError("get method called on non-Map object")
          case _ => ctx.throwTypeError("get method called on non-Map object")
    )

    // Map.prototype.set(key, value)
    val mapSet = NativeFunction(
      name = "set",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match
              case Some(storage) =>
                val key = if args.length > 1 then args(1) else JSValue.Undefined
                val value = if args.length > 2 then args(2) else JSValue.Undefined
                storage.set(key, value)
                JSValue.Object(obj) // Return the Map for chaining
              case None => ctx.throwTypeError("set method called on non-Map object")
          case _ => ctx.throwTypeError("set method called on non-Map object")
    )

    // Map.prototype.has(key)
    val mapHas = NativeFunction(
      name = "has",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match
              case Some(storage) =>
                val key = if args.length > 1 then args(1) else JSValue.Undefined
                JSValue.Bool(storage.has(key))
              case None => ctx.throwTypeError("has method called on non-Map object")
          case _ => ctx.throwTypeError("has method called on non-Map object")
    )

    // Map.prototype.delete(key)
    val mapDelete = NativeFunction(
      name = "delete",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match
              case Some(storage) =>
                val key = if args.length > 1 then args(1) else JSValue.Undefined
                JSValue.Bool(storage.delete(key))
              case None => ctx.throwTypeError("delete method called on non-Map object")
          case _ => ctx.throwTypeError("delete method called on non-Map object")
    )

    // Map.prototype.clear()
    val mapClear = NativeFunction(
      name = "clear",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match
              case Some(storage) =>
                storage.clear()
                JSValue.Undefined
              case None => ctx.throwTypeError("clear method called on non-Map object")
          case _ => ctx.throwTypeError("clear method called on non-Map object")
    )

    // Map.prototype.size (getter)
    val mapSizeGetter = NativeFunction(
      name = "get size",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match
              case Some(storage) => JSValue.fromInt(storage.size)
              case None => ctx.throwTypeError("size getter called on non-Map object")
          case _ => ctx.throwTypeError("size getter called on non-Map object")
    )

    // Map.prototype.forEach(callback, thisArg)
    val mapForEach = NativeFunction(
      name = "forEach",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match
              case Some(storage) =>
                val callback = if args.length > 1 then args(1) else JSValue.Undefined
                val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
                storage.entries.foreach { case (key, value) =>
                  callFunctionWithThis(callback, thisArg, Array(value, key, JSValue.Object(obj)))
                }
                JSValue.Undefined
              case None => ctx.throwTypeError("forEach method called on non-Map object")
          case _ => ctx.throwTypeError("forEach method called on non-Map object")
    )

    // Map.prototype.keys()
    val mapKeys = NativeFunction(
      name = "keys",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match
              case Some(storage) =>
                val arr = quickjs.objmodel.JSArray.empty()
                storage.keys.foreach(k => arr.push(k))
                JSValue.JSArrayVal(arr)
              case None => ctx.throwTypeError("keys method called on non-Map object")
          case _ => ctx.throwTypeError("keys method called on non-Map object")
    )

    // Map.prototype.values()
    val mapValues = NativeFunction(
      name = "values",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match
              case Some(storage) =>
                val arr = quickjs.objmodel.JSArray.empty()
                storage.values.foreach(v => arr.push(v))
                JSValue.JSArrayVal(arr)
              case None => ctx.throwTypeError("values method called on non-Map object")
          case _ => ctx.throwTypeError("values method called on non-Map object")
    )

    // Map.prototype.entries()
    val mapEntries = NativeFunction(
      name = "entries",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match
              case Some(storage) =>
                val arr = quickjs.objmodel.JSArray.empty()
                storage.entries.foreach { case (k, v) =>
                  val entry = quickjs.objmodel.JSArray.empty()
                  entry.push(k)
                  entry.push(v)
                  arr.push(JSValue.JSArrayVal(entry))
                }
                JSValue.JSArrayVal(arr)
              case None => ctx.throwTypeError("entries method called on non-Map object")
          case _ => ctx.throwTypeError("entries method called on non-Map object")
    )

    ctx.mapPrototype.set("get", JSValue.Native(mapGet))
    ctx.mapPrototype.set("set", JSValue.Native(mapSet))
    ctx.mapPrototype.set("has", JSValue.Native(mapHas))
    ctx.mapPrototype.set("delete", JSValue.Native(mapDelete))
    ctx.mapPrototype.set("clear", JSValue.Native(mapClear))
    ctx.mapPrototype.set("forEach", JSValue.Native(mapForEach))
    ctx.mapPrototype.set("keys", JSValue.Native(mapKeys))
    ctx.mapPrototype.set("values", JSValue.Native(mapValues))
    ctx.mapPrototype.set("entries", JSValue.Native(mapEntries))
    // size is a getter property
    ctx.mapPrototype.defineAccessorProperty(
      "size",
      getter = Some(JSValue.Native(mapSizeGetter)),
      setter = None,
      enumerable = false,
      configurable = true
    )

  // ============================================================
  // WeakMap Implementation
  // ============================================================

  /** Internal storage class for WeakMap - uses WeakHashMap with object identity */
  private final class JSWeakMapStorage:
    // Use WeakHashMap with identity-based wrapper for keys
    // Only allows objects as keys (enforced by WeakObjectKey)
    private val storage = java.util.WeakHashMap[WeakObjectKey, JSValue]()

    def get(key: JSValue): Option[JSValue] = key match
      case JSValue.Object(obj) =>
        Option(storage.get(WeakObjectKey(obj)))
      case JSValue.JSArrayVal(arr) =>
        Option(storage.get(WeakObjectKey(arr)))
      case f: JSValue.Function =>
        Option(storage.get(WeakObjectKey(f)))
      case JSValue.Native(n) =>
        Option(storage.get(WeakObjectKey(n)))
      case _ => None  // Non-object keys not allowed

    def set(key: JSValue, value: JSValue): Boolean =
      key match
        case JSValue.Object(obj) =>
          storage.put(WeakObjectKey(obj), value)
          true
        case JSValue.JSArrayVal(arr) =>
          storage.put(WeakObjectKey(arr), value)
          true
        case f: JSValue.Function =>
          storage.put(WeakObjectKey(f), value)
          true
        case JSValue.Native(n) =>
          storage.put(WeakObjectKey(n), value)
          true
        case _ => false  // Non-object keys not allowed

    def has(key: JSValue): Boolean =
      key match
        case JSValue.Object(obj) => storage.containsKey(WeakObjectKey(obj))
        case JSValue.JSArrayVal(arr) => storage.containsKey(WeakObjectKey(arr))
        case f: JSValue.Function => storage.containsKey(WeakObjectKey(f))
        case JSValue.Native(n) => storage.containsKey(WeakObjectKey(n))
        case _ => false

    def delete(key: JSValue): Boolean =
      key match
        case JSValue.Object(obj) =>
          storage.remove(WeakObjectKey(obj)) != null
        case JSValue.JSArrayVal(arr) =>
          storage.remove(WeakObjectKey(arr)) != null
        case f: JSValue.Function =>
          storage.remove(WeakObjectKey(f)) != null
        case JSValue.Native(n) =>
          storage.remove(WeakObjectKey(n)) != null
        case _ => false

  /** Wrapper for weak references that uses object identity */
  private final class WeakObjectKey(val obj: AnyRef):
    override def hashCode(): Int = System.identityHashCode(obj)
    override def equals(other: Any): Boolean = other match
      case that: WeakObjectKey => this.obj eq that.obj
      case _ => false

  private def getWeakMapStorage(obj: quickjs.objmodel.JSObject)(using ctx: JSContext): Option[JSWeakMapStorage] =
    obj.getOwnProperty("__weakMapStorage") match
      case Some(JSValue.Native(storage: JSWeakMapStorage)) => Some(storage)
      case _ => None

  private def initializeWeakMap(ctx: JSContext): Unit =
    given JSContext = ctx

    val weakMapConstructor = quickjs.value.NativeConstructor(
      name = "WeakMap",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Constructor WeakMap requires 'new'"),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val obj = quickjs.objmodel.JSObject(prototype = ctx.weakMapPrototype, extensible = true)
        val storage = new JSWeakMapStorage()
        obj.defineProperty("__weakMapStorage", JSValue.Native(storage), enumerable = false, writable = false, configurable = false)

        // If iterable is provided, add entries
        if args.nonEmpty && args(0) != JSValue.Null && args(0) != JSValue.Undefined then
          args(0) match
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do
                arr.get(i) match
                  case JSValue.JSArrayVal(entry) if entry.getLength >= 2 =>
                    storage.set(entry.get(0), entry.get(1))
                  case JSValue.Object(entryObj) =>
                    val key = entryObj.get("0")
                    val value = entryObj.get("1")
                    storage.set(key, value)
                  case _ => () // Skip invalid entries
                i += 1
            case JSValue.Object(iterObj) =>
              // Try to iterate if it's array-like
              val len = iterObj.get("length").toNumber.toInt
              var i = 0
              while i < len do
                iterObj.get(i.toString) match
                  case JSValue.JSArrayVal(entry) if entry.getLength >= 2 =>
                    storage.set(entry.get(0), entry.get(1))
                  case JSValue.Object(entryObj) =>
                    val key = entryObj.get("0")
                    val value = entryObj.get("1")
                    storage.set(key, value)
                  case _ => () // Skip invalid entries
                i += 1
            case _ => ()

        JSValue.Object(obj),
      prototype = ctx.weakMapPrototype
    )
    initConstructor(weakMapConstructor, length = 0)
    ctx.global.set("WeakMap", JSValue.Native(weakMapConstructor))
    ctx.weakMapPrototype.defineProperty("constructor", JSValue.Native(weakMapConstructor), enumerable = false)

    // WeakMap.prototype.get(key)
    val weakMapGet = NativeFunction(
      name = "get",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getWeakMapStorage(obj) match
              case Some(storage) =>
                val key = args.lift(1).getOrElse(JSValue.Undefined)
                storage.get(key).getOrElse(JSValue.Undefined)
              case None => ctx.throwTypeError("get called on incompatible WeakMap")
          case _ => ctx.throwTypeError("get called on incompatible object")
    )
    ctx.weakMapPrototype.defineProperty("get", JSValue.Native(weakMapGet),
      enumerable = false, writable = true, configurable = true
    )

    // WeakMap.prototype.set(key, value)
    val weakMapSet = NativeFunction(
      name = "set",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getWeakMapStorage(obj) match
              case Some(storage) =>
                val key = args.lift(1).getOrElse(JSValue.Undefined)
                val value = args.lift(2).getOrElse(JSValue.Undefined)
                if storage.set(key, value) then
                  args.head  // Return this WeakMap
                else
                  ctx.throwTypeError("Invalid value used as weak map key")
              case None => ctx.throwTypeError("set called on incompatible WeakMap")
          case _ => ctx.throwTypeError("set called on incompatible object")
    )
    ctx.weakMapPrototype.defineProperty("set", JSValue.Native(weakMapSet),
      enumerable = false, writable = true, configurable = true
    )

    // WeakMap.prototype.has(key)
    val weakMapHas = NativeFunction(
      name = "has",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getWeakMapStorage(obj) match
              case Some(storage) =>
                val key = args.lift(1).getOrElse(JSValue.Undefined)
                JSValue.Bool(storage.has(key))
              case None => ctx.throwTypeError("has called on incompatible WeakMap")
          case _ => ctx.throwTypeError("has called on incompatible object")
    )
    ctx.weakMapPrototype.defineProperty("has", JSValue.Native(weakMapHas),
      enumerable = false, writable = true, configurable = true
    )

    // WeakMap.prototype.delete(key)
    val weakMapDelete = NativeFunction(
      name = "delete",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getWeakMapStorage(obj) match
              case Some(storage) =>
                val key = args.lift(1).getOrElse(JSValue.Undefined)
                JSValue.Bool(storage.delete(key))
              case None => ctx.throwTypeError("delete called on incompatible WeakMap")
          case _ => ctx.throwTypeError("delete called on incompatible object")
    )
    ctx.weakMapPrototype.defineProperty("delete", JSValue.Native(weakMapDelete),
      enumerable = false, writable = true, configurable = true
    )

  // ============================================================
  // Set Implementation
  // ============================================================

  /** Internal storage class for Set */
  private final class JSSetStorage:
    private val storage = mutable.LinkedHashSet.empty[MapKey]

    def add(value: JSValue): Unit = storage.add(MapKey(value))
    def has(value: JSValue): Boolean = storage.contains(MapKey(value))
    def delete(value: JSValue): Boolean = storage.remove(MapKey(value))
    def clear(): Unit = storage.clear()
    def size: Int = storage.size
    def values: Iterator[JSValue] = storage.iterator.map(_.value)

  private def getSetStorage(obj: quickjs.objmodel.JSObject)(using ctx: JSContext): Option[JSSetStorage] =
    obj.getOwnProperty("__setStorage") match
      case Some(JSValue.Native(storage: JSSetStorage)) => Some(storage)
      case _ => None

  private def initializeSet(ctx: JSContext): Unit =
    given JSContext = ctx

    val setConstructor = quickjs.value.NativeConstructor(
      name = "Set",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Constructor Set requires 'new'"),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val obj = quickjs.objmodel.JSObject(prototype = ctx.setPrototype, extensible = true)
        val storage = new JSSetStorage()
        obj.defineProperty("__setStorage", JSValue.Native(storage), enumerable = false, writable = false, configurable = false)

        // If iterable is provided, add values
        if args.nonEmpty && args(0) != JSValue.Null && args(0) != JSValue.Undefined then
          args(0) match
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do
                storage.add(arr.get(i))
                i += 1
            case JSValue.JSStr(str) =>
              var i = 0
              while i < str.length do
                storage.add(JSValue.fromString(str.charAt(i).toString))
                i += 1
            case JSValue.Object(iterObj) =>
              // Try to iterate if it's array-like
              val len = iterObj.get("length").toNumber.toInt
              var i = 0
              while i < len do
                storage.add(iterObj.get(i.toString))
                i += 1
            case _ => ()

        JSValue.Object(obj),
      prototype = ctx.setPrototype
    )
    initConstructor(setConstructor, length = 0)
    ctx.global.set("Set", JSValue.Native(setConstructor))
    ctx.setPrototype.defineProperty("constructor", JSValue.Native(setConstructor), enumerable = false)

    // Set.prototype.add(value)
    val setAdd = NativeFunction(
      name = "add",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match
              case Some(storage) =>
                val value = if args.length > 1 then args(1) else JSValue.Undefined
                storage.add(value)
                JSValue.Object(obj) // Return the Set for chaining
              case None => ctx.throwTypeError("add method called on non-Set object")
          case _ => ctx.throwTypeError("add method called on non-Set object")
    )

    // Set.prototype.has(value)
    val setHas = NativeFunction(
      name = "has",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match
              case Some(storage) =>
                val value = if args.length > 1 then args(1) else JSValue.Undefined
                JSValue.Bool(storage.has(value))
              case None => ctx.throwTypeError("has method called on non-Set object")
          case _ => ctx.throwTypeError("has method called on non-Set object")
    )

    // Set.prototype.delete(value)
    val setDelete = NativeFunction(
      name = "delete",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match
              case Some(storage) =>
                val value = if args.length > 1 then args(1) else JSValue.Undefined
                JSValue.Bool(storage.delete(value))
              case None => ctx.throwTypeError("delete method called on non-Set object")
          case _ => ctx.throwTypeError("delete method called on non-Set object")
    )

    // Set.prototype.clear()
    val setClear = NativeFunction(
      name = "clear",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match
              case Some(storage) =>
                storage.clear()
                JSValue.Undefined
              case None => ctx.throwTypeError("clear method called on non-Set object")
          case _ => ctx.throwTypeError("clear method called on non-Set object")
    )

    // Set.prototype.size (getter)
    val setSizeGetter = NativeFunction(
      name = "get size",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match
              case Some(storage) => JSValue.fromInt(storage.size)
              case None => ctx.throwTypeError("size getter called on non-Set object")
          case _ => ctx.throwTypeError("size getter called on non-Set object")
    )

    // Set.prototype.forEach(callback, thisArg)
    val setForEach = NativeFunction(
      name = "forEach",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match
              case Some(storage) =>
                val callback = if args.length > 1 then args(1) else JSValue.Undefined
                val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
                storage.values.foreach { value =>
                  // Set forEach passes (value, value, set) to maintain consistency with Map
                  callFunctionWithThis(callback, thisArg, Array(value, value, JSValue.Object(obj)))
                }
                JSValue.Undefined
              case None => ctx.throwTypeError("forEach method called on non-Set object")
          case _ => ctx.throwTypeError("forEach method called on non-Set object")
    )

    // Set.prototype.values() - also aliased as keys()
    val setValues = NativeFunction(
      name = "values",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match
              case Some(storage) =>
                val arr = quickjs.objmodel.JSArray.empty()
                storage.values.foreach(v => arr.push(v))
                JSValue.JSArrayVal(arr)
              case None => ctx.throwTypeError("values method called on non-Set object")
          case _ => ctx.throwTypeError("values method called on non-Set object")
    )

    // Set.prototype.entries()
    val setEntries = NativeFunction(
      name = "entries",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match
              case Some(storage) =>
                val arr = quickjs.objmodel.JSArray.empty()
                storage.values.foreach { v =>
                  val entry = quickjs.objmodel.JSArray.empty()
                  entry.push(v)
                  entry.push(v) // Set entries are [value, value]
                  arr.push(JSValue.JSArrayVal(entry))
                }
                JSValue.JSArrayVal(arr)
              case None => ctx.throwTypeError("entries method called on non-Set object")
          case _ => ctx.throwTypeError("entries method called on non-Set object")
    )

    ctx.setPrototype.set("add", JSValue.Native(setAdd))
    ctx.setPrototype.set("has", JSValue.Native(setHas))
    ctx.setPrototype.set("delete", JSValue.Native(setDelete))
    ctx.setPrototype.set("clear", JSValue.Native(setClear))
    ctx.setPrototype.set("forEach", JSValue.Native(setForEach))
    ctx.setPrototype.set("values", JSValue.Native(setValues))
    ctx.setPrototype.set("keys", JSValue.Native(setValues)) // keys() is an alias for values()
    ctx.setPrototype.set("entries", JSValue.Native(setEntries))
    // size is a getter property
    ctx.setPrototype.defineAccessorProperty(
      "size",
      getter = Some(JSValue.Native(setSizeGetter)),
      setter = None,
      enumerable = false,
      configurable = true
    )

  // ============================================================
  // WeakSet Implementation
  // ============================================================

  /** Internal storage class for WeakSet - uses WeakHashMap */
  private final class JSWeakSetStorage:
    // Use a Set backed by WeakHashMap
    private val storage = java.util.WeakHashMap[WeakObjectKey, java.lang.Boolean]()

    def add(value: JSValue): Boolean =
      value match
        case JSValue.Object(obj) =>
          storage.put(WeakObjectKey(obj), java.lang.Boolean.TRUE)
          true
        case JSValue.JSArrayVal(arr) =>
          storage.put(WeakObjectKey(arr), java.lang.Boolean.TRUE)
          true
        case f: JSValue.Function =>
          storage.put(WeakObjectKey(f), java.lang.Boolean.TRUE)
          true
        case JSValue.Native(n) =>
          storage.put(WeakObjectKey(n), java.lang.Boolean.TRUE)
          true
        case _ => false  // Non-object values not allowed

    def has(value: JSValue): Boolean =
      value match
        case JSValue.Object(obj) => storage.containsKey(WeakObjectKey(obj))
        case JSValue.JSArrayVal(arr) => storage.containsKey(WeakObjectKey(arr))
        case f: JSValue.Function => storage.containsKey(WeakObjectKey(f))
        case JSValue.Native(n) => storage.containsKey(WeakObjectKey(n))
        case _ => false

    def delete(value: JSValue): Boolean =
      value match
        case JSValue.Object(obj) =>
          storage.remove(WeakObjectKey(obj)) != null
        case JSValue.JSArrayVal(arr) =>
          storage.remove(WeakObjectKey(arr)) != null
        case f: JSValue.Function =>
          storage.remove(WeakObjectKey(f)) != null
        case JSValue.Native(n) =>
          storage.remove(WeakObjectKey(n)) != null
        case _ => false

  private def getWeakSetStorage(obj: quickjs.objmodel.JSObject)(using ctx: JSContext): Option[JSWeakSetStorage] =
    obj.getOwnProperty("__weakSetStorage") match
      case Some(JSValue.Native(storage: JSWeakSetStorage)) => Some(storage)
      case _ => None

  private def initializeWeakSet(ctx: JSContext): Unit =
    given JSContext = ctx

    val weakSetConstructor = quickjs.value.NativeConstructor(
      name = "WeakSet",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Constructor WeakSet requires 'new'"),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val obj = quickjs.objmodel.JSObject(prototype = ctx.weakSetPrototype, extensible = true)
        val storage = new JSWeakSetStorage()
        obj.defineProperty("__weakSetStorage", JSValue.Native(storage), enumerable = false, writable = false, configurable = false)

        // If iterable is provided, add values
        if args.nonEmpty && args(0) != JSValue.Null && args(0) != JSValue.Undefined then
          args(0) match
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do
                storage.add(arr.get(i))
                i += 1
            case JSValue.Object(iterObj) =>
              // Try to iterate if it's array-like
              val len = iterObj.get("length").toNumber.toInt
              var i = 0
              while i < len do
                storage.add(iterObj.get(i.toString))
                i += 1
            case _ => ()

        JSValue.Object(obj),
      prototype = ctx.weakSetPrototype
    )
    initConstructor(weakSetConstructor, length = 0)
    ctx.global.set("WeakSet", JSValue.Native(weakSetConstructor))
    ctx.weakSetPrototype.defineProperty("constructor", JSValue.Native(weakSetConstructor), enumerable = false)

    // WeakSet.prototype.add(value)
    val weakSetAdd = NativeFunction(
      name = "add",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getWeakSetStorage(obj) match
              case Some(storage) =>
                val value = args.lift(1).getOrElse(JSValue.Undefined)
                if storage.add(value) then
                  args.head  // Return this WeakSet
                else
                  ctx.throwTypeError("Invalid value used in weak set")
              case None => ctx.throwTypeError("add called on incompatible WeakSet")
          case _ => ctx.throwTypeError("add called on incompatible object")
    )
    ctx.weakSetPrototype.defineProperty("add", JSValue.Native(weakSetAdd),
      enumerable = false, writable = true, configurable = true
    )

    // WeakSet.prototype.has(value)
    val weakSetHas = NativeFunction(
      name = "has",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getWeakSetStorage(obj) match
              case Some(storage) =>
                val value = args.lift(1).getOrElse(JSValue.Undefined)
                JSValue.Bool(storage.has(value))
              case None => ctx.throwTypeError("has called on incompatible WeakSet")
          case _ => ctx.throwTypeError("has called on incompatible object")
    )
    ctx.weakSetPrototype.defineProperty("has", JSValue.Native(weakSetHas),
      enumerable = false, writable = true, configurable = true
    )

    // WeakSet.prototype.delete(value)
    val weakSetDelete = NativeFunction(
      name = "delete",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getWeakSetStorage(obj) match
              case Some(storage) =>
                val value = args.lift(1).getOrElse(JSValue.Undefined)
                JSValue.Bool(storage.delete(value))
              case None => ctx.throwTypeError("delete called on incompatible WeakSet")
          case _ => ctx.throwTypeError("delete called on incompatible object")
    )
    ctx.weakSetPrototype.defineProperty("delete", JSValue.Native(weakSetDelete),
      enumerable = false, writable = true, configurable = true
    )

  /** Helper to get Promise from an object */
  private def getPromise(obj: quickjs.objmodel.JSObject)(using ctx: JSContext): Option[JSValue.Promise] =
    obj.getOwnProperty("__promise") match
      case Some(p: JSValue.Promise) => Some(p)
      case _ => None

  /** Call a function value (either native or bytecode) with given this and arguments */
  private def callFunctionValue(func: JSValue, thisArg: JSValue, args: Array[JSValue])(using ctx: JSContext): JSValue =
    BuiltinHelpers.callFunctionValue(func, thisArg, args)

  /** Resolve a promise with a value */
  private def promiseResolve(promise: JSValue.Promise, value: JSValue)(using ctx: JSContext): Unit =
    if promise.state != JSValue.PromiseState.Pending then return  // Already settled

    promise.state = JSValue.PromiseState.Fulfilled
    promise.result = value

    // Schedule all fulfillment reactions as microtasks
    val reactions = promise.fulfillReactions.toList
    promise.fulfillReactions.clear()
    promise.rejectReactions.clear()

    reactions.foreach { reaction =>
      ctx.queueMicrotask { () =>
        val result = reaction.onFulfilled match
          case JSValue.Native(native: quickjs.value.NativeFunction) =>
            native.call(Array(JSValue.Undefined, value))
          case JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            // Call the function - need to use interpreter
            value  // For now, just pass through
          case _ =>
            value  // No handler or not a function - pass through

        // Resolve the chained promise
        promiseResolve(reaction.promise, result)
      }
    }

  /** Create a resolved promise from a value - public helper for async/await */
  def promiseResolve(value: JSValue)(using ctx: JSContext): JSValue =
    // If already a promise, return it
    value match
      case JSValue.Object(obj) =>
        obj.getOwnProperty("__promise") match
          case Some(_: JSValue.Promise) =>
            return value
          case _ => ()
      case _ => ()

    // Create new fulfilled promise
    val promise = JSValue.Promise()
    promise.state = JSValue.PromiseState.Fulfilled
    promise.result = value

    val promiseObj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
    promiseObj.defineProperty("__promise", promise, enumerable = false)
    JSValue.Object(promiseObj)

  /** Reject a promise with a reason */
  private def promiseReject(promise: JSValue.Promise, reason: JSValue)(using ctx: JSContext): Unit =
    if promise.state != JSValue.PromiseState.Pending then return  // Already settled

    promise.state = JSValue.PromiseState.Rejected
    promise.result = reason

    // Schedule all rejection reactions as microtasks
    val reactions = promise.rejectReactions.toList
    promise.fulfillReactions.clear()
    promise.rejectReactions.clear()

    reactions.foreach { reaction =>
      ctx.queueMicrotask { () =>
        val result = reaction.onRejected match
          case JSValue.Native(native: quickjs.value.NativeFunction) =>
            native.call(Array(JSValue.Undefined, reason))
          case JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            // Call the function - need to use interpreter
            reason  // For now, just pass through
          case _ =>
            reason  // No handler or not a function - pass through

        // Resolve the chained promise
        promiseResolve(reaction.promise, result)
      }
    }

  private def initializePromise(ctx: JSContext): Unit =
    given JSContext = ctx

    val promiseConstructor = quickjs.value.NativeConstructor(
      name = "Promise",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Constructor Promise requires 'new'"),
      constructImpl = (args, ctx) =>
        given JSContext = ctx

        // Create the Promise object
        val promise = JSValue.Promise()
        val obj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
        obj.defineProperty("__promise", promise, enumerable = false, writable = false, configurable = false)

        // Create resolve and reject functions
        val resolveFunc = NativeFunction(
          name = "resolve",
          impl = (resolveArgs, _) =>
            val value = resolveArgs.lift(1).getOrElse(JSValue.Undefined)
            promiseResolve(promise, value)
            JSValue.Undefined
        )

        val rejectFunc = NativeFunction(
          name = "reject",
          impl = (rejectArgs, _) =>
            val reason = rejectArgs.lift(1).getOrElse(JSValue.Undefined)
            promiseReject(promise, reason)
            JSValue.Undefined
        )

        // Call the executor function
        if args.nonEmpty then
          args(0) match
            case JSValue.Native(native: quickjs.value.NativeFunction) =>
              native.call(Array(JSValue.Undefined, JSValue.Native(resolveFunc), JSValue.Native(rejectFunc)))
            case JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
              // Call the function using interpreter - for now, just skip
              ()
            case _ =>
              ctx.throwTypeError("Promise resolver is not a function")
          end match

        JSValue.Object(obj),
      prototype = ctx.promisePrototype
    )
    initConstructor(promiseConstructor, length = 1)
    ctx.global.set("Promise", JSValue.Native(promiseConstructor))
    ctx.promisePrototype.defineProperty("constructor", JSValue.Native(promiseConstructor), enumerable = false)

    // Promise.prototype.then(onFulfilled, onRejected)
    val promiseThen = NativeFunction(
      name = "then",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getPromise(obj) match
              case Some(promise) =>
                val onFulfilled = args.lift(1).getOrElse(JSValue.Undefined)
                val onRejected = args.lift(2).getOrElse(JSValue.Undefined)

                // Create a new promise for chaining
                val chainedPromise = JSValue.Promise()
                val reaction = JSValue.PromiseReaction(onFulfilled, onRejected, chainedPromise)

                promise.state match
                  case JSValue.PromiseState.Pending =>
                    // Add reactions to be called later
                    promise.fulfillReactions += reaction
                    promise.rejectReactions += reaction
                  case JSValue.PromiseState.Fulfilled =>
                    // Already fulfilled - call handler via microtask
                    ctx.queueMicrotask { () =>
                      val result = callFunctionValue(onFulfilled, JSValue.Undefined, Array(promise.result))
                      promiseResolve(chainedPromise, result)
                    }
                  case JSValue.PromiseState.Rejected =>
                    // Already rejected - call handler via microtask
                    ctx.queueMicrotask { () =>
                      val result = callFunctionValue(onRejected, JSValue.Undefined, Array(promise.result))
                      promiseResolve(chainedPromise, result)
                    }

                // Return the chained promise wrapped in an object
                val chainedObj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
                chainedObj.defineProperty("__promise", chainedPromise, enumerable = false, writable = false, configurable = false)
                JSValue.Object(chainedObj)

              case None => ctx.throwTypeError("then method called on non-Promise object")
          case _ => ctx.throwTypeError("then method called on non-Promise object")
    )

    // Promise.prototype.catch(onRejected)
    val promiseCatch = NativeFunction(
      name = "catch",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getPromise(obj) match
              case Some(promise) =>
                // catch(onRejected) is equivalent to then(undefined, onRejected)
                val onRejected = args.lift(1).getOrElse(JSValue.Undefined)

                // Inline the then logic with undefined as onFulfilled
                val chainedPromise = JSValue.Promise()
                val reaction = JSValue.PromiseReaction(JSValue.Undefined, onRejected, chainedPromise)

                promise.state match
                  case JSValue.PromiseState.Pending =>
                    promise.fulfillReactions += reaction
                    promise.rejectReactions += reaction
                  case JSValue.PromiseState.Fulfilled =>
                    // Pass through the fulfilled value via microtask
                    ctx.queueMicrotask { () =>
                      promiseResolve(chainedPromise, promise.result)
                    }
                  case JSValue.PromiseState.Rejected =>
                    // Call handler via microtask
                    ctx.queueMicrotask { () =>
                      val result = callFunctionValue(onRejected, JSValue.Undefined, Array(promise.result))
                      promiseResolve(chainedPromise, result)
                    }

                val chainedObj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
                chainedObj.defineProperty("__promise", chainedPromise, enumerable = false, writable = false, configurable = false)
                JSValue.Object(chainedObj)

              case None => ctx.throwTypeError("catch method called on non-Promise object")
          case _ => ctx.throwTypeError("catch method called on non-Promise object")
    )

    // Promise.prototype.finally(onFinally)
    val promiseFinally = NativeFunction(
      name = "finally",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Object(obj)) =>
            getPromise(obj) match
              case Some(promise) =>
                val onFinally = args.lift(1).getOrElse(JSValue.Undefined)
                // Create a new promise for chaining
                val chainedPromise = JSValue.Promise()

                val handler = onFinally match
                  case JSValue.Native(_: quickjs.value.NativeFunction) => onFinally
                  case JSValue.Function(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => onFinally
                  case _ => JSValue.Undefined

                promise.state match
                  case JSValue.PromiseState.Pending =>
                    // Add reactions that call finally then pass through
                    val reaction = JSValue.PromiseReaction(handler, handler, chainedPromise)
                    promise.fulfillReactions += reaction
                    promise.rejectReactions += reaction
                  case _ =>
                    // Already settled - call handler via microtask
                    ctx.queueMicrotask { () =>
                      handler match
                        case JSValue.Native(native: quickjs.value.NativeFunction) =>
                          native.call(Array(JSValue.Undefined))
                        case _ => ()
                      // Resolve with original result
                      promiseResolve(chainedPromise, promise.result)
                    }

                val chainedObj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
                chainedObj.defineProperty("__promise", chainedPromise, enumerable = false, writable = false, configurable = false)
                JSValue.Object(chainedObj)

              case None => ctx.throwTypeError("finally method called on non-Promise object")
          case _ => ctx.throwTypeError("finally method called on non-Promise object")
    )

    // Promise.resolve(value) - static method
    val promiseResolveStatic = NativeFunction(
      name = "resolve",
      impl = (args, ctx) =>
        given JSContext = ctx
        val value = args.lift(1).getOrElse(JSValue.Undefined)

        // If value is already a promise, return it
        value match
          case JSValue.Object(obj) =>
            getPromise(obj) match
              case Some(_) => return value
              case None => () // fall through to create new promise
          case _ => () // Not an object, create new promise

        // Create a new fulfilled promise
        val promise = JSValue.Promise(state = JSValue.PromiseState.Fulfilled, result = value)
        val obj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
        obj.defineProperty("__promise", promise, enumerable = false, writable = false, configurable = false)
        JSValue.Object(obj)
    )

    // Promise.reject(reason) - static method
    val promiseRejectStatic = NativeFunction(
      name = "reject",
      impl = (args, ctx) =>
        given JSContext = ctx
        val reason = args.lift(1).getOrElse(JSValue.Undefined)

        // Create a new rejected promise
        val promise = JSValue.Promise(state = JSValue.PromiseState.Rejected, result = reason)
        val obj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
        obj.defineProperty("__promise", promise, enumerable = false, writable = false, configurable = false)
        JSValue.Object(obj)
    )

    // Promise.all(iterable) - static method
    val promiseAllStatic = NativeFunction(
      name = "all",
      impl = (args, ctx) =>
        given JSContext = ctx
        val iterable = args.lift(1).getOrElse(JSValue.Undefined)

        // Create result promise
        val resultPromise = JSValue.Promise()
        val resultObj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
        resultObj.defineProperty("__promise", resultPromise, enumerable = false, writable = false, configurable = false)

        // Convert iterable to array of promises
        val promises = iterable match
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj) =>
            // Try to get array-like object
            obj.get("length")(using ctx) match
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
          case _ => IndexedSeq.empty

        if promises.isEmpty then
          // Empty iterable - resolve with empty array immediately
          resultPromise.state = JSValue.PromiseState.Fulfilled
          resultPromise.result = JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else
          // Track resolution state
          val results = new Array[JSValue](promises.length)
          var remainingCount = promises.length
          var rejected = false

          promises.zipWithIndex.foreach { case (promiseValue, index) =>
            // Check if value is already a promise
            val valuePromise = promiseValue match
              case JSValue.Object(obj) =>
                getPromise(obj) match
                  case Some(p) => p
                  case None =>
                    // Not a promise - treat as fulfilled
                    val p = JSValue.Promise(state = JSValue.PromiseState.Fulfilled, result = promiseValue)
                    p
              case _ =>
                // Primitive - treat as fulfilled
                val p = JSValue.Promise(state = JSValue.PromiseState.Fulfilled, result = promiseValue)
                p

            // Check promise state
            valuePromise.state match
              case JSValue.PromiseState.Fulfilled =>
                results(index) = valuePromise.result
                remainingCount -= 1
              case JSValue.PromiseState.Rejected if !rejected =>
                rejected = true
                resultPromise.state = JSValue.PromiseState.Rejected
                resultPromise.result = valuePromise.result
              case JSValue.PromiseState.Pending =>
                // For pending promises, we'd need to add reactions
                // For now, this simplified version doesn't handle pending promises
                results(index) = valuePromise.result
                remainingCount -= 1
              case _ => ()
          }

          // If all resolved and not rejected
          if !rejected && remainingCount == 0 then
            val resultArray = quickjs.objmodel.JSArray.empty()
            results.foreach(resultArray.push)
            resultPromise.state = JSValue.PromiseState.Fulfilled
            resultPromise.result = JSValue.JSArrayVal(resultArray)

        JSValue.Object(resultObj)
    )

    // Promise.race(iterable) - static method
    val promiseRaceStatic = NativeFunction(
      name = "race",
      impl = (args, ctx) =>
        given JSContext = ctx
        val iterable = args.lift(1).getOrElse(JSValue.Undefined)

        // Create result promise
        val resultPromise = JSValue.Promise()
        val resultObj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
        resultObj.defineProperty("__promise", resultPromise, enumerable = false, writable = false, configurable = false)

        // Convert iterable to array of promises
        val promises = iterable match
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj) =>
            obj.get("length")(using ctx) match
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
          case _ => IndexedSeq.empty

        // Race - first to settle wins
        var settled = false
        promises.foreach { promiseValue =>
          if !settled then
            promiseValue match
              case JSValue.Object(obj) =>
                getPromise(obj) match
                  case Some(p) =>
                    p.state match
                      case JSValue.PromiseState.Fulfilled =>
                        settled = true
                        resultPromise.state = JSValue.PromiseState.Fulfilled
                        resultPromise.result = p.result
                      case JSValue.PromiseState.Rejected =>
                        settled = true
                        resultPromise.state = JSValue.PromiseState.Rejected
                        resultPromise.result = p.result
                      case JSValue.PromiseState.Pending =>
                        // Pending - for now, use as fulfilled with undefined
                        ()
                  case None =>
                    // Non-promise value - treat as fulfilled
                    if !settled then
                      settled = true
                      resultPromise.state = JSValue.PromiseState.Fulfilled
                      resultPromise.result = promiseValue
              case _ =>
                // Primitive - treat as fulfilled immediately
                if !settled then
                  settled = true
                  resultPromise.state = JSValue.PromiseState.Fulfilled
                  resultPromise.result = promiseValue
        }

        // If iterable was empty, promise stays pending forever (as per spec)
        JSValue.Object(resultObj)
    )

    // Promise.allSettled(iterable) - static method
    val promiseAllSettledStatic = NativeFunction(
      name = "allSettled",
      impl = (args, ctx) =>
        given JSContext = ctx
        val iterable = args.lift(1).getOrElse(JSValue.Undefined)

        // Create result promise
        val resultPromise = JSValue.Promise()
        val resultObj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
        resultObj.defineProperty("__promise", resultPromise, enumerable = false, writable = false, configurable = false)

        // Convert iterable to array of promises
        val promises = iterable match
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj) =>
            obj.get("length")(using ctx) match
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
          case _ => IndexedSeq.empty

        if promises.isEmpty then
          // Empty iterable - resolve with empty array immediately
          resultPromise.state = JSValue.PromiseState.Fulfilled
          resultPromise.result = JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else
          // Build result objects
          val results = quickjs.objmodel.JSArray.empty()
          promises.foreach { promiseValue =>
            val resultObj = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)

            promiseValue match
              case JSValue.Object(obj) =>
                getPromise(obj) match
                  case Some(p) =>
                    p.state match
                      case JSValue.PromiseState.Fulfilled =>
                        resultObj.set("status", JSValue.fromString("fulfilled"))
                        resultObj.set("value", p.result)
                      case JSValue.PromiseState.Rejected =>
                        resultObj.set("status", JSValue.fromString("rejected"))
                        resultObj.set("reason", p.result)
                      case JSValue.PromiseState.Pending =>
                        // Treat as fulfilled for now
                        resultObj.set("status", JSValue.fromString("fulfilled"))
                        resultObj.set("value", JSValue.Undefined)
                  case None =>
                    // Non-promise - treat as fulfilled
                    resultObj.set("status", JSValue.fromString("fulfilled"))
                    resultObj.set("value", promiseValue)
              case _ =>
                // Primitive - treat as fulfilled
                resultObj.set("status", JSValue.fromString("fulfilled"))
                resultObj.set("value", promiseValue)

            results.push(JSValue.Object(resultObj))
          }

          resultPromise.state = JSValue.PromiseState.Fulfilled
          resultPromise.result = JSValue.JSArrayVal(results)

        JSValue.Object(resultObj)
    )

    // Promise.any(iterable) - static method
    val promiseAnyStatic = NativeFunction(
      name = "any",
      impl = (args, ctx) =>
        given JSContext = ctx
        val iterable = args.lift(1).getOrElse(JSValue.Undefined)

        // Create result promise
        val resultPromise = JSValue.Promise()
        val resultObj = quickjs.objmodel.JSObject(prototype = ctx.promisePrototype, extensible = true)
        resultObj.defineProperty("__promise", resultPromise, enumerable = false, writable = false, configurable = false)

        // Convert iterable to array of promises
        val promises = iterable match
          case JSValue.JSArrayVal(arr) => arr.getElements
          case JSValue.Object(obj) =>
            obj.get("length")(using ctx) match
              case JSValue.Int32(len) =>
                (0 until len).map(i => obj.get(i.toString)(using ctx))
              case _ => IndexedSeq.empty
          case _ => IndexedSeq.empty

        if promises.isEmpty then
          // Empty iterable - reject with AggregateError
          val errorObj = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
          errorObj.set("name", JSValue.fromString("AggregateError"))
          errorObj.set("message", JSValue.fromString("All promises were rejected"))
          errorObj.set("errors", JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty()))
          resultPromise.state = JSValue.PromiseState.Rejected
          resultPromise.result = JSValue.Object(errorObj)
        else
          // Any - first to fulfill wins
          var fulfilled = false
          val errors = quickjs.objmodel.JSArray.empty()

          promises.foreach { promiseValue =>
            if !fulfilled then
              promiseValue match
                case JSValue.Object(obj) =>
                  getPromise(obj) match
                    case Some(p) =>
                      p.state match
                        case JSValue.PromiseState.Fulfilled =>
                          fulfilled = true
                          resultPromise.state = JSValue.PromiseState.Fulfilled
                          resultPromise.result = p.result
                        case JSValue.PromiseState.Rejected =>
                          errors.push(p.result)
                        case JSValue.PromiseState.Pending =>
                          // Pending - skip for now
                          ()
                    case None =>
                      // Non-promise value - treat as fulfilled
                      if !fulfilled then
                        fulfilled = true
                        resultPromise.state = JSValue.PromiseState.Fulfilled
                        resultPromise.result = promiseValue
                case _ =>
                  // Primitive - treat as fulfilled immediately
                  if !fulfilled then
                    fulfilled = true
                    resultPromise.state = JSValue.PromiseState.Fulfilled
                    resultPromise.result = promiseValue
          }

          // If none fulfilled, reject with AggregateError
          if !fulfilled then
            val errorObj = quickjs.objmodel.JSObject(prototype = ctx.objectPrototype, extensible = true)
            errorObj.set("name", JSValue.fromString("AggregateError"))
            errorObj.set("message", JSValue.fromString("All promises were rejected"))
            errorObj.set("errors", JSValue.JSArrayVal(errors))
            resultPromise.state = JSValue.PromiseState.Rejected
            resultPromise.result = JSValue.Object(errorObj)

        JSValue.Object(resultObj)
    )

    ctx.promisePrototype.set("then", JSValue.Native(promiseThen))
    ctx.promisePrototype.set("catch", JSValue.Native(promiseCatch))
    ctx.promisePrototype.set("finally", JSValue.Native(promiseFinally))

    // Static methods on Promise constructor
    promiseConstructor.funcObj.set("resolve", JSValue.Native(promiseResolveStatic))
    promiseConstructor.funcObj.set("reject", JSValue.Native(promiseRejectStatic))
    promiseConstructor.funcObj.set("all", JSValue.Native(promiseAllStatic))
    promiseConstructor.funcObj.set("race", JSValue.Native(promiseRaceStatic))
    promiseConstructor.funcObj.set("allSettled", JSValue.Native(promiseAllSettledStatic))
    promiseConstructor.funcObj.set("any", JSValue.Native(promiseAnyStatic))
    ctx.global.set("Promise", JSValue.Native(promiseConstructor))

  private def initializeBigInt(ctx: JSContext): Unit =
    given JSContext = ctx

    val bigIntConstructor = quickjs.value.NativeConstructor(
      name = "BigInt",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        if args.isEmpty then JSValue.BigInt(java.math.BigInteger.ZERO)
        else args(0) match
          case JSValue.BigInt(b) => JSValue.BigInt(b)
          case JSValue.JSStr(s) =>
            try
              val trimmed = s.trim()
              if trimmed.isEmpty then
                ctx.throwSyntaxError("Cannot convert  to a BigInt")
              val (str, radix) =
                if trimmed.startsWith("0x") || trimmed.startsWith("0X") then (trimmed.substring(2), 16)
                else if trimmed.startsWith("0o") || trimmed.startsWith("0O") then (trimmed.substring(2), 8)
                else if trimmed.startsWith("0b") || trimmed.startsWith("0B") then (trimmed.substring(2), 2)
                else (trimmed, 10)
              JSValue.BigInt(new java.math.BigInteger(str, radix))
            catch
              case e: quickjs.runtime.JSException => throw e
              case _: NumberFormatException =>
                ctx.throwSyntaxError(s"Cannot convert $s to a BigInt")
          case JSValue.Int32(i) => JSValue.BigInt(java.math.BigInteger.valueOf(i.toLong))
          case JSValue.Float64(d) =>
            if d.isNaN || d.isInfinite then
              ctx.throwRangeError("The number cannot be converted to a BigInt because it is not an integer")
            JSValue.BigInt(java.math.BigInteger.valueOf(d.toLong))
          case JSValue.Bool(b) => JSValue.BigInt(if b then java.math.BigInteger.ONE else java.math.BigInteger.ZERO)
          case _ => ctx.throwTypeError(s"Cannot convert ${args(0)} to a BigInt"),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("BigInt is not a constructor. Use BigInt() without 'new'."),
      prototype = ctx.objectPrototype
    )

    // BigInt.asIntN(bits, bigint)
    val bigIntAsIntN = NativeFunction(
      name = "asIntN",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 3 then
          ctx.throwTypeError("BigInt.asIntN requires 2 arguments")
        val bits = args(1).toNumber.toInt
        val bigint = args(2) match
          case JSValue.BigInt(b) => b
          case _ => ctx.throwTypeError("BigInt.asIntN expects a BigInt")
        // Mask to N bits with sign extension
        val mask = java.math.BigInteger.ONE.shiftLeft(bits).subtract(java.math.BigInteger.ONE)
        val masked = bigint.and(mask)
        val signBit = java.math.BigInteger.ONE.shiftLeft(bits - 1)
        val result = if masked.testBit(bits - 1) then masked.subtract(mask).subtract(java.math.BigInteger.ONE) else masked
        JSValue.BigInt(result)
    )

    // BigInt.asUintN(bits, bigint)
    val bigIntAsUintN = NativeFunction(
      name = "asUintN",
      impl = (args, ctx) =>
        given JSContext = ctx
        if args.length < 3 then
          ctx.throwTypeError("BigInt.asUintN requires 2 arguments")
        val bits = args(1).toNumber.toInt
        val bigint = args(2) match
          case JSValue.BigInt(b) => b
          case _ => ctx.throwTypeError("BigInt.asUintN expects a BigInt")
        val mask = java.math.BigInteger.ONE.shiftLeft(bits).subtract(java.math.BigInteger.ONE)
        JSValue.BigInt(bigint.and(mask))
    )

    initConstructor(bigIntConstructor, length = 1)
    bigIntConstructor.funcObj.set("asIntN", JSValue.Native(bigIntAsIntN))
    bigIntConstructor.funcObj.set("asUintN", JSValue.Native(bigIntAsUintN))
    ctx.global.set("BigInt", JSValue.Native(bigIntConstructor))

    // Add toString and valueOf to BigInt prototype (through Object prototype works for now)
    // BigInt.prototype.toString
    val bigIntToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.BigInt(b)) =>
            val radix = if args.length > 1 then args(1).toNumber.toInt else 10
            if radix < 2 || radix > 36 then
              ctx.throwRangeError("toString() radix argument must be between 2 and 36")
            JSValue.fromString(b.toString(radix))
          case _ =>
            ctx.throwTypeError("BigInt.prototype.toString called on non-BigInt")
    )

    // BigInt.prototype.valueOf
    val bigIntValueOf = NativeFunction(
      name = "valueOf",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(b: JSValue.BigInt) => b
          case _ => ctx.throwTypeError("BigInt.prototype.valueOf called on non-BigInt")
    )

    // Add prototype methods to the BigInt "class" - these are accessed via property lookup
    ctx.global.set("__BigInt_toString", JSValue.Native(bigIntToString))
    ctx.global.set("__BigInt_valueOf", JSValue.Native(bigIntValueOf))

  /** Initialize all standard library methods */
  def initialize(ctx: JSContext): Unit =
    initialize(ctx, None)

  /** Initialize all standard library methods with optional module loader */
  def initialize(ctx: JSContext, moduleLoader: Option[ModuleLoader]): Unit =
    initializeFunctionPrototype(ctx)
    initializeArrayConstructor(ctx)
    initializeArrayPrototype(ctx)
    initializeForInHelpers(ctx)
    initializeModuleHelpers(ctx, moduleLoader)
    initializeArrayHelpers(ctx)
    initializeObjectStatics(ctx)
    initializeMath(ctx)
    initializeNumberString(ctx)
    initializeSymbol(ctx)
    initializeRegExp(ctx)
    initializeDate(ctx)
    initializeProxy(ctx)
    initializeReflect(ctx)
    initializeTestHelpers(ctx)
    initializeError(ctx)
    initializeMap(ctx)
    initializeSet(ctx)
    initializeWeakMap(ctx)
    initializeWeakSet(ctx)
    initializePromise(ctx)
    initializeBigInt(ctx)
