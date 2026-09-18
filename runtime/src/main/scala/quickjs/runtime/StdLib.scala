package quickjs.runtime

import quickjs.module.ModuleLoader
import quickjs.runtime.builtins.{
  BuiltinHelpers,
  FunctionBuiltins,
  ArrayBuiltins,
  InternalHelpers,
  IteratorBuiltins,
  ObjectBuiltins,
  NumberStringBuiltins,
  MathBuiltins,
  SymbolBuiltins,
  RegExpBuiltins,
  ProxyBuiltins,
  ReflectBuiltins,
  DateBuiltins,
  ErrorBuiltins,
  MapSetBuiltins,
  PromiseBuiltins,
  BigIntBuiltins,
  WeakRefBuiltins,
  TypedArrayBuiltins,
  IntlBuiltins,
  IteratorHelpers
}
import quickjs.value.JSValue

/** Standard library initialization facade.
  *
  * Delegates to individual built-in initializer files in
  * quickjs.runtime.builtins.*. This is kept as a single entry point to avoid
  * circular dependencies and maintain backward compatibility with existing
  * callers.
  */
object StdLib {

  /** Initialize all standard library methods */
  def initialize(ctx: JSContext): Unit =
    initialize(ctx, None)

  /** Initialize all standard library methods with optional module loader */
  def initialize(ctx: JSContext, moduleLoader: Option[ModuleLoader]): Unit = {
    // Shared iterator intrinsics must exist before any family creates its
    // iterators.
    IteratorBuiltins.initializePrototypes(ctx)
    FunctionBuiltins.initialize(ctx)
    ArrayBuiltins.initializeArrayConstructor(ctx)
    ArrayBuiltins.initializeArrayPrototype(ctx)
    InternalHelpers.initializeForInHelpers(ctx)
    InternalHelpers.initializeSuperHelpers(ctx)
    IntlBuiltins.initialize(ctx)
    InternalHelpers.initializeModuleHelpers(ctx, moduleLoader)
    InternalHelpers.initializeArrayHelpers(ctx)
    InternalHelpers.initializeTestHelpers(ctx)
    ObjectBuiltins.initialize(ctx)
    SymbolBuiltins.initialize(ctx)
    FunctionBuiltins.initializeSymbolMethods(ctx)
    IteratorBuiltins.initializeIteratorSymbol(ctx)
    IteratorHelpers.initialize(ctx)
    ArrayBuiltins.initializeArrayUnscopables(ctx)
    MathBuiltins.initialize(ctx)
    NumberStringBuiltins.initialize(ctx)
    RegExpBuiltins.initialize(ctx)
    DateBuiltins.initialize(ctx)
    ProxyBuiltins.initialize(ctx)
    ReflectBuiltins.initialize(ctx)
    ErrorBuiltins.initialize(ctx)
    MapSetBuiltins.initialize(ctx)
    PromiseBuiltins.initialize(ctx)
    BigIntBuiltins.initialize(ctx)
    WeakRefBuiltins.initialize(ctx)
    TypedArrayBuiltins.initialize(ctx)
    SymbolBuiltins.initializeSpeciesConstructors(ctx)
    normalizeBuiltinDescriptors(ctx)
  }

  /** Built-in properties are created during several independent initializer
    * passes. Normalize their common ECMAScript attribute: built-in methods and
    * constructor/prototype links are non-enumerable.
    */
  private def normalizeBuiltinDescriptors(ctx: JSContext): Unit = {
    given JSContext = ctx

    def markCallable(value: JSValue): Unit = value match {
      case JSValue.Native(function: quickjs.value.NativeFunction)
          // %Function.prototype% is itself a native function object; linking
          // it to itself would create a prototype cycle.
          if !(function.funcObj eq ctx.functionPrototype) =>
        function.funcObj.setPrototype(ctx.functionPrototype)
      case _ => ()
    }

    def makeOwnPropertiesNonEnumerable(obj: quickjs.objmodel.JSObject): Unit =
      obj.getAllOwnPropertyKeys().foreach { key =>
        obj.getOwnPropertyDescriptor(key).foreach { case (value, attrs) =>
          markCallable(value)
          attrs.getter.foreach(markCallable)
          attrs.setter.foreach(markCallable)
          if attrs.enumerable then
            if attrs.isAccessor || attrs.getter.isDefined || attrs.setter.isDefined
            then
              obj.defineAccessorPropertyDetailed(
                key,
                attrs.getter,
                attrs.setter,
                hasGetter = attrs.getter.isDefined,
                hasSetter = attrs.setter.isDefined,
                enumerable = Some(false),
                configurable = Some(attrs.configurable)
              )
            else
              obj.defineDataProperty(
                key,
                Some(value),
                Some(false),
                Some(attrs.writable),
                Some(attrs.configurable)
              )
        }
      }

    makeOwnPropertiesNonEnumerable(ctx.global)
    ctx.global.getAllOwnPropertyKeys().foreach { key =>
      ctx.global.getOwnProperty(key).foreach {
        case JSValue.Native(constructor: quickjs.value.NativeConstructor) =>
          makeOwnPropertiesNonEnumerable(constructor.funcObj)
          makeOwnPropertiesNonEnumerable(constructor.prototype)
        case JSValue.Native(function: quickjs.value.NativeFunction) =>
          makeOwnPropertiesNonEnumerable(function.funcObj)
        case JSValue.Object(obj) => makeOwnPropertiesNonEnumerable(obj)
        case _                   => ()
      }
    }

    ctx.global.get("Number") match {
      case JSValue.Native(number: quickjs.value.NativeConstructor) =>
        Seq(
          "MAX_VALUE",
          "MIN_VALUE",
          "NaN",
          "NEGATIVE_INFINITY",
          "POSITIVE_INFINITY",
          "EPSILON",
          "MAX_SAFE_INTEGER",
          "MIN_SAFE_INTEGER"
        ).foreach { key =>
          number.funcObj.getOwnProperty(key).foreach(value =>
            number.funcObj.defineProperty(
              key,
              value,
              enumerable = false,
              writable = false,
              configurable = false
            )
          )
        }
      case _ => ()
    }
  }
}
