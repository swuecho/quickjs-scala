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

  // Well-known symbols storage
  private val wellKnownSymbols = mutable.Map.empty[String, JSValue.Symbol]

  private def getOrCreateWellKnownSymbol(name: String): JSValue.Symbol =
    wellKnownSymbols.getOrElseUpdate(name, {
      symbolCounter += 1
      JSValue.Symbol(symbolCounter)
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
        symbolCounter += 1
        val sym = JSValue.Symbol(symbolCounter)
        sym
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Symbol is not a constructor"),
      prototype = symbolPrototype
    )
    BuiltinHelpers.initConstructor(symbolConstructor, length = 0)
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
        val symArg = if args.length > 1 then args(1) else JSValue.Undefined
        symArg match
          case JSValue.Symbol(id) =>
            globalSymbolRegistry.find { case (_, sym) => sym.value == id } match
              case Some((key, _)) => JSValue.fromString(key)
              case None => JSValue.Undefined
          case _ =>
            ctx.throwTypeError("Symbol.keyFor requires a symbol argument")
    )
    symbolConstructor.funcObj.set("keyFor", JSValue.Native(symbolKeyFor))

    // Well-known symbols
    val symIterator = getOrCreateWellKnownSymbol("iterator")
    symbolConstructor.funcObj.set("iterator", symIterator)

    val symAsyncIterator = getOrCreateWellKnownSymbol("asyncIterator")
    symbolConstructor.funcObj.set("asyncIterator", symAsyncIterator)

    val symToStringTag = getOrCreateWellKnownSymbol("toStringTag")
    symbolConstructor.funcObj.set("toStringTag", symToStringTag)

    val symHasInstance = getOrCreateWellKnownSymbol("hasInstance")
    symbolConstructor.funcObj.set("hasInstance", symHasInstance)

    val symSpecies = getOrCreateWellKnownSymbol("species")
    symbolConstructor.funcObj.set("species", symSpecies)
