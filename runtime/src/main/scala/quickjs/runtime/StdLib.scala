package quickjs.runtime

import quickjs.module.ModuleLoader
import quickjs.runtime.builtins.{
  BuiltinHelpers,
  FunctionBuiltins,
  ArrayBuiltins,
  InternalHelpers,
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
  BigIntBuiltins
}

/** Standard library initialization facade.
  *
  * Delegates to individual built-in initializer files in
  * quickjs.runtime.builtins.*. This is kept as a single entry point to avoid
  * circular dependencies and maintain backward compatibility with existing
  * callers.
  */
object StdLib:

  /** Initialize all standard library methods */
  def initialize(ctx: JSContext): Unit =
    initialize(ctx, None)

  /** Initialize all standard library methods with optional module loader */
  def initialize(ctx: JSContext, moduleLoader: Option[ModuleLoader]): Unit =
    FunctionBuiltins.initialize(ctx)
    ArrayBuiltins.initializeArrayConstructor(ctx)
    ArrayBuiltins.initializeArrayPrototype(ctx)
    InternalHelpers.initializeForInHelpers(ctx)
    InternalHelpers.initializeModuleHelpers(ctx, moduleLoader)
    InternalHelpers.initializeArrayHelpers(ctx)
    InternalHelpers.initializeTestHelpers(ctx)
    ObjectBuiltins.initialize(ctx)
    SymbolBuiltins.initialize(ctx)
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
