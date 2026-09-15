package quickjs.node

import quickjs.value.JSValue
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.{JSArray, JSObject}

/** Thrown by `process.exit()` (and by a fatal uncaught script error when Node
  * compatibility mode is active). It is deliberately not a `RuntimeException`
  * so the interpreter's JS `try`/`catch` dispatch does not intercept it: user
  * code cannot catch an exit.
  */
final class NodeExit(val code: Int) extends Exception(s"process.exit($code)")

/** Small shared helpers for the Node compatibility layer. */
object NodeHelpers {

  /** True for values the fs/path APIs accept as a path argument. */
  def isPathLike(value: JSValue): Boolean = value match {
    case JSValue.JSStr(_) => true
    case JSValue.Object(obj) =>
      obj.getOwnPropertyRaw("__taView").isDefined ||
        obj.getOwnPropertyRaw("__urlRecord").isDefined ||
        obj.getOwnPropertyRaw("__abStorage").isDefined
    case _ => false
  }

  /** Drop the receiver that method dispatch prepends to path-based fs APIs.
    * The first real argument is always a string/Buffer/URL (or an fd number),
    * so any other object/function is `this`. This also handles packages that
    * copy the fs methods onto their own object (path-scurry, graceful-fs).
    */
  def stripPathReceiver(args: Array[JSValue]): Array[JSValue] =
    if args.nonEmpty then
      args(0) match {
        case JSValue.Object(obj) if !isPathLike(JSValue.Object(obj)) => args.drop(1)
        case JSValue.JSArrayVal(_)                   => args.drop(1)
        case _: JSValue.Function                     => args.drop(1)
        case JSValue.Native(_)                       => args.drop(1)
        case _                                       => args
      }
    else args

  /** Native method calls receive the receiver as `args(0)` (see
    * `Console.actualArgs`). Drop it when it is exactly the module/namespace
    * object a method was attached to, so that both `path.join(...)` and
    * `const { join } = path; join(...)` work.
    */
  def stripReceiver(
      args: Array[JSValue],
      receiver: JSObject
  ): Array[JSValue] =
    if args.nonEmpty then
      args(0) match {
        case JSValue.Object(obj) if obj eq receiver => args.drop(1)
        case _                                       => args
      }
    else args

  def stripReceiver(
      args: Array[JSValue],
      receiver: quickjs.value.NativeFunction
  ): Array[JSValue] =
    if args.nonEmpty then
      args(0) match {
        case JSValue.Native(nf: quickjs.value.NativeFunction)
            if nf.asInstanceOf[AnyRef] eq receiver.asInstanceOf[AnyRef] =>
          args.drop(1)
        case _ => args
      }
    else args

  def toStr(value: JSValue)(using ctx: JSContext): String =
    if value == JSValue.Undefined then "undefined"
    else BuiltinHelpers.toJSString(value)

  def toNumber(value: JSValue)(using ctx: JSContext): Double =
    BuiltinHelpers.toNumber(value)

  def toPath(value: JSValue)(using ctx: JSContext): String = value match {
    case JSValue.JSStr(s) => s
    case JSValue.Object(_) if isFileUrl(value) =>
      NodeUrl.fileURLToPath(value)
    case other => toStr(other)
  }

  def isFileUrl(value: JSValue): Boolean =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__urlRecord").isDefined
      case _ => false
    }

  /** Create an Error object with a `code` property (Node's style) and throw. */
  def throwCoded(
      name: String,
      message: String,
      code: String
  )(using ctx: JSContext): Nothing = {
    val error = ctx.createError(name, message)
    error match {
      case JSValue.Object(obj) =>
        obj.defineProperty(
          "code",
          JSValue.fromString(code),
          enumerable = false,
          writable = true,
          configurable = true
        )
      case _ => ()
    }
    throw new quickjs.runtime.JSException(error)
  }

  /** Empty JSArray. */
  def emptyArray(): JSArray = JSArray.empty()

  /** Build a plain object from key/value pairs (insertion ordered). */
  def objectOf(pairs: (String, JSValue)*)(using ctx: JSContext): JSObject = {
    val obj = JSObject(prototype = ctx.objectPrototype)
    pairs.foreach { case (key, value) => obj.set(key, value) }
    obj
  }

  /** Copy enumerable own string-keyed properties of `source` onto `target`. */
  def copyEnumerable(source: JSObject, target: JSObject)(using
      ctx: JSContext
  ): Unit =
    source.getAllOwnPropertyKeys().foreach { key =>
      source.getOwnPropertyDescriptor(key).foreach { case (value, attrs) =>
        if attrs.enumerable && !attrs.isAccessor && !key.startsWith("__") then
          target.set(key, value)
      }
    }

  /** SameValueZero, adequate for listener/bookkeeping comparisons. */
  def sameValue(a: JSValue, b: JSValue): Boolean = (a, b) match {
    case (JSValue.Int32(x), JSValue.Int32(y))     => x == y
    case (JSValue.Float64(x), JSValue.Float64(y)) => x == y
    case (JSValue.Int32(x), JSValue.Float64(y))   => x.toDouble == y
    case (JSValue.Float64(x), JSValue.Int32(y))   => x == y.toDouble
    case (JSValue.Bool(x), JSValue.Bool(y))       => x == y
    case (JSValue.JSStr(x), JSValue.JSStr(y))     => x == y
    case (JSValue.Symbol(x), JSValue.Symbol(y))   => x == y
    case (JSValue.BigInt(x), JSValue.BigInt(y))   => x == y
    case (JSValue.Undefined, JSValue.Undefined)   => true
    case (JSValue.Null, JSValue.Null)             => true
    case (JSValue.Object(x), JSValue.Object(y))   => x eq y
    case (JSValue.JSArrayVal(x), JSValue.JSArrayVal(y)) => x eq y
    case (fx: JSValue.Function, fy: JSValue.Function)   =>
      fx.asInstanceOf[AnyRef] eq fy.asInstanceOf[AnyRef]
    case (JSValue.Native(x), JSValue.Native(y)) =>
      x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]
    case _ => false
  }
}
