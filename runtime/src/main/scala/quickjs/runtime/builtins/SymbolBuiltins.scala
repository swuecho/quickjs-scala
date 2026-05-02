package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import scala.collection.mutable

/** Symbol built-in: Symbol constructor, Symbol.for, Symbol.keyFor, well-known symbols. */
object SymbolBuiltins:
  import quickjs.objmodel.JSObject

  // Global symbol registry for Symbol.for() and Symbol.keyFor()
  private val globalSymbolRegistry = mutable.Map.empty[String, JSValue.Symbol]
  private var symbolCounter = 0

  // Symbol descriptions (keyed by symbol id)
  private val symbolDescriptions = mutable.Map.empty[Int, String]

  /** Get the description for a symbol, or the default if none. */
  def getSymbolDescription(sym: JSValue.Symbol): String =
    symbolDescriptions.getOrElse(sym.value, {
      val kf = globalSymbolRegistry.find(_._2.value == sym.value).map(_._1)
      kf.getOrElse(s"${sym.value}")
    })

  // Well-known symbols storage
  private val wellKnownSymbols = mutable.Map.empty[String, JSValue.Symbol]

  private def getOrCreateWellKnownSymbol(name: String): JSValue.Symbol =
    wellKnownSymbols.getOrElseUpdate(name, {
      symbolCounter += 1
      val sym = JSValue.Symbol(symbolCounter)
      symbolDescriptions(symbolCounter) = s"Symbol.$name"
      sym
    })

  def initialize(ctx: JSContext): Unit =
    given JSContext = ctx

    // Create Symbol prototype
    val symbolPrototype = JSObject(prototype = ctx.objectPrototype, extensible = true)
    ctx.symbolPrototype = symbolPrototype

    // Symbol constructor - when called without new, returns a new unique symbol
    val symbolConstructor = quickjs.value.NativeConstructor(
      name = "Symbol",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        // Symbol(description) returns a new unique symbol
        // Use proper ToString which throws TypeError for Symbol args
        symbolCounter += 1
        val desc = if args.length > 0 then BuiltinHelpers.toJSString(args(0)) else ""
        val sym = JSValue.Symbol(symbolCounter)
        if desc.nonEmpty then symbolDescriptions(symbolCounter) = desc
        sym
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Symbol is not a constructor"),
      prototype = symbolPrototype
    )
    BuiltinHelpers.initConstructor(symbolConstructor, length = 0)
    ctx.global.defineProperty("Symbol", JSValue.Native(symbolConstructor), enumerable = false)
    symbolPrototype.defineProperty("constructor", JSValue.Native(symbolConstructor), enumerable = false)

    // Symbol.prototype.toString()
    val symbolToString = NativeFunction(
      name = "toString",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(JSValue.Symbol(id)) =>
            val desc = symbolDescriptions.get(id)
            JSValue.fromString(desc.map(d => s"Symbol($d)").getOrElse("Symbol()"))
          case Some(JSValue.Object(obj)) =>
            obj.getOwnProperty("__primitive") match
              case Some(JSValue.Symbol(id)) =>
                val desc = symbolDescriptions.get(id)
                JSValue.fromString(desc.map(d => s"Symbol($d)").getOrElse("Symbol()"))
              case _ => ctx.throwTypeError("Symbol.prototype.toString called on non-Symbol")
          case _ => ctx.throwTypeError("Symbol.prototype.toString called on non-Symbol")
    )
    symbolPrototype.defineProperty("toString", JSValue.Native(symbolToString),
      enumerable = false, writable = true, configurable = true
    )

    // Symbol.prototype.valueOf()
    val symbolValueOf = NativeFunction(
      name = "valueOf",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match
          case Some(sym: JSValue.Symbol) => sym
          case Some(JSValue.Object(obj)) =>
            obj.getOwnProperty("__primitive") match
              case Some(sym: JSValue.Symbol) => sym
              case _ => ctx.throwTypeError("Symbol.prototype.valueOf called on non-Symbol")
          case _ => ctx.throwTypeError("Symbol.prototype.valueOf called on non-Symbol")
    )
    symbolPrototype.defineProperty("valueOf", JSValue.Native(symbolValueOf),
      enumerable = false, writable = true, configurable = true
    )

    // Symbol.prototype.description - getter that returns the symbol's description
    val symbolDescriptionGetter = NativeFunction(
      name = "get description",
      impl = (args, ctx) =>
        given JSContext = ctx
        args(0) match
          case JSValue.Object(obj) =>
            obj.getOwnProperty("__primitive") match
              case Some(sym: JSValue.Symbol) =>
                val desc = symbolDescriptions.get(sym.value)
                desc.map(s => JSValue.fromString(s)).getOrElse(JSValue.Undefined)
              case _ => ctx.throwTypeError("Symbol.prototype.description called on non-Symbol")
          case JSValue.Symbol(id) =>
            val desc = symbolDescriptions.get(id)
            desc.map(s => JSValue.fromString(s)).getOrElse(JSValue.Undefined)
          case _ => ctx.throwTypeError("Symbol.prototype.description called on non-Symbol")
    )
    symbolPrototype.defineAccessorProperty("description", getter = Some(JSValue.Native(symbolDescriptionGetter)), setter = None, enumerable = false, configurable = true)

    // Symbol.for(key) - returns a symbol from the global registry
    val symbolFor = NativeFunction(
      name = "for",
      impl = (args, ctx) =>
        given JSContext = ctx
        val key = if args.length > 1 then BuiltinHelpers.toJSString(args(1)) else ""
        globalSymbolRegistry.getOrElseUpdate(key, {
          symbolCounter += 1
          val sym = JSValue.Symbol(symbolCounter)
          symbolDescriptions(symbolCounter) = key
          sym
        })
    )
    symbolConstructor.funcObj.defineProperty("for", JSValue.Native(symbolFor), enumerable = false)

    // Symbol.keyFor(sym) - returns the key for a symbol in the global registry
    val symbolKeyFor = NativeFunction(
      name = "keyFor",
      impl = (args, ctx) =>
        given JSContext = ctx
        val symArg = if args.length > 1 then args(1) else JSValue.Undefined
        symArg match
          case JSValue.Symbol(id) =>
            globalSymbolRegistry.find { case (_, sym) => sym.value == id } match
              case Some((key, _)) => JSValue.fromString(key)
              case None => JSValue.Undefined
          case _ =>
            ctx.throwTypeError("Symbol.keyFor requires a symbol argument")
    )
    symbolConstructor.funcObj.defineProperty("keyFor", JSValue.Native(symbolKeyFor), enumerable = false)

    // Well-known symbols (non-writable, non-enumerable, non-configurable)
    val symIterator = getOrCreateWellKnownSymbol("iterator")
    symbolConstructor.funcObj.defineProperty("iterator", symIterator, enumerable = false, writable = false, configurable = false)

    val symAsyncIterator = getOrCreateWellKnownSymbol("asyncIterator")
    symbolConstructor.funcObj.defineProperty("asyncIterator", symAsyncIterator, enumerable = false, writable = false, configurable = false)

    val symToStringTag = getOrCreateWellKnownSymbol("toStringTag")
    symbolConstructor.funcObj.defineProperty("toStringTag", symToStringTag, enumerable = false, writable = false, configurable = false)

    val symHasInstance = getOrCreateWellKnownSymbol("hasInstance")
    symbolConstructor.funcObj.defineProperty("hasInstance", symHasInstance, enumerable = false, writable = false, configurable = false)

    val symSpecies = getOrCreateWellKnownSymbol("species")
    symbolConstructor.funcObj.defineProperty("species", symSpecies, enumerable = false, writable = false, configurable = false)

    val symIsConcatSpreadable = getOrCreateWellKnownSymbol("isConcatSpreadable")
    symbolConstructor.funcObj.defineProperty("isConcatSpreadable", symIsConcatSpreadable, enumerable = false, writable = false, configurable = false)

    val symMatch = getOrCreateWellKnownSymbol("match")
    symbolConstructor.funcObj.defineProperty("match", symMatch, enumerable = false, writable = false, configurable = false)

    val symMatchAll = getOrCreateWellKnownSymbol("matchAll")
    symbolConstructor.funcObj.defineProperty("matchAll", symMatchAll, enumerable = false, writable = false, configurable = false)

    val symReplace = getOrCreateWellKnownSymbol("replace")
    symbolConstructor.funcObj.defineProperty("replace", symReplace, enumerable = false, writable = false, configurable = false)

    val symSearch = getOrCreateWellKnownSymbol("search")
    symbolConstructor.funcObj.defineProperty("search", symSearch, enumerable = false, writable = false, configurable = false)

    val symSplit = getOrCreateWellKnownSymbol("split")
    symbolConstructor.funcObj.defineProperty("split", symSplit, enumerable = false, writable = false, configurable = false)

    val symToPrimitive = getOrCreateWellKnownSymbol("toPrimitive")
    symbolConstructor.funcObj.defineProperty("toPrimitive", symToPrimitive, enumerable = false, writable = false, configurable = false)

    val symUnscopables = getOrCreateWellKnownSymbol("unscopables")
    symbolConstructor.funcObj.defineProperty("unscopables", symUnscopables, enumerable = false, writable = false, configurable = false)

    // Symbol.prototype[Symbol.toPrimitive] — returns the symbol primitive
    val symbolToPrimitiveMethod = NativeFunction(
      name = "[Symbol.toPrimitive]",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        args(0) match
          case s: JSValue.Symbol => s
          case JSValue.Object(obj) =>
            obj.getOwnProperty("__primitive") match
              case Some(s: JSValue.Symbol) => s
              case _ => ctx.throwTypeError("Symbol.toPrimitive called on non-Symbol")
          case _ => ctx.throwTypeError("Symbol.toPrimitive called on non-Symbol")
    )
    symbolPrototype.defineSymbolProperty(symToPrimitive.value, JSValue.Native(symbolToPrimitiveMethod), enumerable = false)

    // Symbol.prototype[Symbol.toStringTag] = "Symbol"
    symbolPrototype.initSymbolProperty(symToStringTag.value, JSValue.fromString("Symbol"), enumerable = false, writable = false, configurable = true)
