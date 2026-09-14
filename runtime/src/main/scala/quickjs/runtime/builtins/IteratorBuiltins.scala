package quickjs.runtime.builtins

import quickjs.objmodel.JSObject
import quickjs.runtime.JSContext
import quickjs.value.{JSValue, NativeFunction}

/** Shared iterator intrinsics.
  *
  * `%IteratorPrototype%` is created with the realm intrinsics; the per-family
  * prototypes (`%ArrayIteratorPrototype%`, `%StringIteratorPrototype%`) are
  * built here before the standard library runs so every iterator family shares
  * the same grandparent. `[Symbol.iterator]` is installed on
  * `%IteratorPrototype%` once the Symbol builtin exists.
  *
  * Map/Set iterators use `%IteratorPrototype%` directly and define their own
  * `next`.
  */
object IteratorBuiltins {

  /** Create the iterator prototypes. Runs before the Symbol builtin, so the
    * well-known `@@iterator` is added later by [[initializeIteratorSymbol]].
    */
  def initializePrototypes(ctx: JSContext): Unit = {
    given JSContext = ctx

    val arrayProto = JSObject(prototype = ctx.iteratorPrototype)
    arrayProto.defineProperty(
      "next",
      JSValue.Native(arrayIteratorNext()),
      enumerable = false,
      writable = true,
      configurable = true
    )
    ctx.arrayIteratorPrototype = arrayProto

    val stringProto = JSObject(prototype = ctx.iteratorPrototype)
    stringProto.defineProperty(
      "next",
      JSValue.Native(stringIteratorNext()),
      enumerable = false,
      writable = true,
      configurable = true
    )
    ctx.stringIteratorPrototype = stringProto
  }

  /** Install `%IteratorPrototype%[@@iterator]` (after Symbol is available). */
  def initializeIteratorSymbol(ctx: JSContext): Unit = {
    given JSContext = ctx
    wellKnownSymbol("iterator") match {
      case JSValue.Symbol(id) =>
        val self = NativeFunction(
          name = "[Symbol.iterator]",
          length = 0,
          impl = (args, _) => args.headOption.getOrElse(JSValue.Undefined)
        )
        ctx.iteratorPrototype.initSymbolProperty(
          id,
          JSValue.Native(self),
          enumerable = false,
          writable = true,
          configurable = true
        )
      case _ => ()
    }
    defineToStringTag(ctx.arrayIteratorPrototype, "Array Iterator")
    defineToStringTag(ctx.stringIteratorPrototype, "String Iterator")
  }

  /** Create an Array Iterator over `target` (array, array-like, typed array). */
  def createArrayIterator(target: JSValue, kind: String)(using
      ctx: JSContext
  ): JSValue = {
    val iterator = JSObject(prototype = ctx.arrayIteratorPrototype)
    iterator.initProperty(
      "__arrayIteratorTarget",
      target,
      enumerable = false,
      writable = true,
      configurable = false
    )
    iterator.initProperty(
      "__arrayIteratorIndex",
      JSValue.Int32(0),
      enumerable = false,
      writable = true,
      configurable = false
    )
    iterator.initProperty(
      "__arrayIteratorKind",
      JSValue.JSStr(kind),
      enumerable = false,
      writable = false,
      configurable = false
    )
    JSValue.Object(iterator)
  }

  /** Create a String Iterator over a string. */
  def createStringIterator(str: String)(using ctx: JSContext): JSValue = {
    val iterator = JSObject(prototype = ctx.stringIteratorPrototype)
    iterator.initProperty(
      "__stringIteratorString",
      JSValue.fromString(str),
      enumerable = false,
      writable = false,
      configurable = false
    )
    iterator.initProperty(
      "__stringIteratorIndex",
      JSValue.Int32(0),
      enumerable = false,
      writable = true,
      configurable = false
    )
    JSValue.Object(iterator)
  }

  // =========================================================================
  // Internals
  // =========================================================================

  private def wellKnownSymbol(name: String)(using ctx: JSContext): JSValue =
    ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.get(name)
      case _ => JSValue.Undefined
    }

  private def defineToStringTag(proto: JSObject, tag: String)(using
      ctx: JSContext
  ): Unit =
    wellKnownSymbol("toStringTag") match {
      case JSValue.Symbol(id) =>
        proto.initSymbolProperty(
          id,
          JSValue.fromString(tag),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }

  private def iteratorResult(value: JSValue, done: Boolean)(using
      ctx: JSContext
  ): JSValue = {
    val obj = JSObject(prototype = ctx.objectPrototype)
    obj.defineProperty(
      "value",
      value,
      enumerable = true,
      writable = true,
      configurable = true
    )
    obj.defineProperty(
      "done",
      JSValue.Bool(done),
      enumerable = true,
      writable = true,
      configurable = true
    )
    JSValue.Object(obj)
  }

  private def arrayIteratorNext(): NativeFunction =
    NativeFunction(
      name = "next",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(thisObj))
              if thisObj.getOwnProperty("__arrayIteratorTarget").isDefined =>
            val target = thisObj.get("__arrayIteratorTarget")
            val index = thisObj.get("__arrayIteratorIndex") match {
              case JSValue.Int32(i)   => i.toLong
              case JSValue.Float64(d) => d.toLong
              case _                  => 0L
            }
            if index >= ArrayBuiltins.arrayLikeLengthLong(target) then
              iteratorResult(JSValue.Undefined, done = true)
            else {
              thisObj.set(
                "__arrayIteratorIndex",
                JSValue.fromDouble((index + 1).toDouble)
              )
              val value = thisObj.get("__arrayIteratorKind") match {
                case JSValue.JSStr("key") => JSValue.fromDouble(index.toDouble)
                case JSValue.JSStr("entry") =>
                  val pair = quickjs.objmodel.JSArray.empty()
                  pair.push(JSValue.fromDouble(index.toDouble))
                  pair.push(ArrayBuiltins.arrayLikeGetLong(target, index))
                  JSValue.JSArrayVal(pair)
                case _ => ArrayBuiltins.arrayLikeGetLong(target, index)
              }
              iteratorResult(value, done = false)
            }
          case _ =>
            ctx.throwTypeError(
              "Array Iterator.prototype.next called on incompatible receiver"
            )
        }
    )

  private def stringIteratorNext(): NativeFunction =
    NativeFunction(
      name = "next",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(thisObj))
              if thisObj.getOwnProperty("__stringIteratorString").isDefined =>
            val str = thisObj.get("__stringIteratorString") match {
              case JSValue.JSStr(s) => s
              case _                => ""
            }
            val index = thisObj.get("__stringIteratorIndex") match {
              case JSValue.Int32(i)   => i
              case JSValue.Float64(d) => d.toInt
              case _                  => 0
            }
            if index >= str.length then
              iteratorResult(JSValue.Undefined, done = true)
            else {
              val cp = str.codePointAt(index)
              val width = Character.charCount(cp)
              thisObj.set("__stringIteratorIndex", JSValue.fromInt(index + width))
              iteratorResult(
                JSValue.fromString(new String(Character.toChars(cp))),
                done = false
              )
            }
          case _ =>
            ctx.throwTypeError(
              "String Iterator.prototype.next called on incompatible receiver"
            )
        }
    )
}
