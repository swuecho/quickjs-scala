package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.{JSArray, JSObject}

import scala.collection.mutable

/** Node's `util` module (`format`, `inspect`, `promisify`, `inherits`, ...). */
object NodeUtil {

  // =========================================================================
  // Inspection
  // =========================================================================

  def inspect(
      value: JSValue,
      depth: Int = 2,
      seen: mutable.Map[AnyRef, Int] = mutable.Map.empty
  )(using ctx: JSContext): String = {
    def ind(level: Int): String = "  " * level

    def marker(ref: AnyRef): Option[String] =
      seen.get(ref).map { id =>
        s"[Circular *$id]"
      }

    def register(ref: AnyRef): Int = {
      val id = seen.size + 1
      seen(ref) = id
      id
    }

    def go(v: JSValue, level: Int): String = v match {
      case JSValue.Undefined => "undefined"
      case JSValue.Null      => "null"
      case JSValue.Bool(b)   => b.toString
      case JSValue.Int32(n)  => n.toString
      case JSValue.Float64(d) =>
        if d.isNaN then "NaN"
        else if d == Double.PositiveInfinity then "Infinity"
        else if d == Double.NegativeInfinity then "-Infinity"
        else if d == 0.0 && 1.0 / d < 0 then "-0"
        else if d == math.floor(d) && !d.isInfinite && math.abs(d) < 1e15 then d.toLong.toString
        else d.toString
      case JSValue.JSStr(s)   => quote(s)
      case JSValue.BigInt(b)  => s"${b}n"
      case JSValue.Symbol(id) => s"Symbol($id)"
      case f: JSValue.Function =>
        if f.name.isEmpty || f.name == "<arrow>" then "[Function (anonymous)]"
        else s"[Function: ${f.name}]"
      case JSValue.Native(nc: NativeConstructor) => s"[Function: ${nc.name}]"
      case JSValue.Native(nf: NativeFunction)    => s"[Function: ${nf.name}]"
      case JSValue.Native(other)                 => s"[Native: $other]"
      case _: JSValue.Promise        => "Promise { <state> }"
      case _: JSValue.Generator      => "Object [Generator] {}"
      case _: JSValue.AsyncFunction  => "[AsyncFunction]"
      case JSValue.JSArrayVal(arr) =>
        formatArray(arr, level)
      case value @ JSValue.Object(obj) =>
        if isBuffer(obj) then formatBuffer(value)
        else if obj.getOwnPropertyRaw("__dateValue").isDefined then
          formatDate(obj.getOwnPropertyRaw("__dateValue").get)
        else if obj.getOwnPropertyRaw("__regexpPattern").isDefined then
          formatRegExp(obj)
        else if obj.getOwnPropertyRaw("__mapStorage").isDefined then
          "Map { ... }"
        else if obj.getOwnPropertyRaw("__setStorage").isDefined then
          "Set { ... }"
        else if obj.getOwnPropertyRaw("__taView").isDefined then
          formatTypedArray(value, obj)
        else formatObject(value, obj, level)
      case other => String.valueOf(other)
    }

    def formatArray(arr: JSArray, level: Int): String = {
      val ref = arr.asInstanceOf[AnyRef]
      marker(ref) match {
        case Some(m) => m
        case None =>
          register(ref)
          val length = arr.getLength
          if length == 0 then "[]"
          else if level > depth then "[Array]"
          else {
            val items = (0 until length).map { i =>
              val item = arr.get(i)
              val text =
                item match {
                  case JSValue.Object(o) =>
                    inspect(item, depth, seen)(using ctx)
                  case _ => go(item, level + 1)
                }
              text
            }
            val shown =
              if items.length > 6 then
                items.take(6) :+ s"... ${items.length - 6} more items"
              else items
            s"[ ${shown.mkString(", ")} ]"
          }
      }
    }

    def formatObject(value: JSValue, obj: JSObject, level: Int): String = {
      val ref = obj.asInstanceOf[AnyRef]
      marker(ref) match {
        case Some(m) => m
        case None =>
          register(ref)
          val keys = obj
            .getAllOwnPropertyKeys()
            .filterNot(_.startsWith("__"))
          val entries = keys.filter { key =>
            obj.getOwnPropertyDescriptor(key).exists(_._2.enumerable)
          }
          if entries.isEmpty then {
            obj.getOwnPropertyRaw("__classTag__") match {
              case Some(JSValue.JSStr(tag)) => s"[$tag]"
              case _ => "{}"
            }
          } else if level > depth then "{Object}"
          else {
            val shown = entries.take(10)
            val parts = shown.map { key =>
              val prop = obj.getOwnProperty(key)(using ctx).getOrElse(JSValue.Undefined)
              s"$key: ${go(prop, level + 1)}"
            }
            val extra =
              if entries.length > 10 then Seq(s"... +${entries.length - 10} more")
              else Seq.empty
            s"{ ${(parts ++ extra).mkString(", ")} }"
          }
      }
    }

    def isBuffer(obj: JSObject): Boolean =
      obj.getOwnPropertyRaw("__isBuffer").contains(JSValue.Bool(true))

    def formatBuffer(value: JSValue): String =
      NodeBuffer.bytesOfValue(value) match {
        case Some(bytes) =>
          val shown = bytes.take(50).map(b => f"${b & 0xff}%02x").mkString(" ")
          val suffix = if bytes.length > 50 then " ..." else ""
          s"<Buffer $shown$suffix>"
        case None => "<Buffer>"
      }

    def formatDate(millis: JSValue): String = {
      val d = millis.toNumber
      if d.isNaN then "Invalid Date"
      else {
        val instant = java.time.Instant.ofEpochMilli(d.toLong)
        val text = instant.toString
        s"${text}Z"
      }
    }

    def formatRegExp(obj: JSObject): String = {
      val source = obj.getOwnPropertyRaw("__regexpPattern").map(_.toString).getOrElse("")
      val flags = obj.getOwnPropertyRaw("__regexpFlags").map(_.toString).getOrElse("")
      s"/$source/$flags"
    }

    def formatTypedArray(value: JSValue, obj: JSObject): String = {
      val bytes = NodeBuffer.bytesOfValue(value).getOrElse(Array.emptyByteArray)
      s"Uint8Array(${bytes.length}) [ ${bytes.take(20).map(_ & 0xff).mkString(", ")} ]"
    }

    def quote(s: String): String = {
      val sb = new StringBuilder("'")
      s.foreach {
        case '\''         => sb.append("\\'")
        case '\\'         => sb.append("\\\\")
        case '\n'         => sb.append("\\n")
        case '\r'         => sb.append("\\r")
        case '\t'         => sb.append("\\t")
        case c if c < ' ' => sb.append(f"\\u${c.toInt}%04x")
        case c            => sb.append(c)
      }
      sb.append("'")
      sb.toString
    }

    go(value, 0)
  }

  // =========================================================================
  // format
  // =========================================================================

  def format(args: Array[JSValue])(using ctx: JSContext): String = {
    if args.isEmpty then ""
    else
      args(0) match {
        case JSValue.JSStr(template) => formatTemplate(template, args.drop(1))
        case other =>
          args.map(v => inspect(v)).mkString(" ")
      }
  }

  private def formatTemplate(
      template: String,
      args: Array[JSValue]
  )(using ctx: JSContext): String = {
    val sb = new StringBuilder
    var argIndex = 0
    var i = 0
    while i < template.length do {
      val c = template.charAt(i)
      if c == '%' && i + 1 < template.length then {
        val spec = template.charAt(i + 1)
        spec match {
          case '%' => sb.append('%'); i += 2
          case 's' =>
            if argIndex < args.length then {
              sb.append(
                args(argIndex) match {
                  case JSValue.JSStr(s) => s
                  case other            => inspect(other)
                }
              )
              argIndex += 1
            }
            i += 2
          case 'd' | 'i' =>
            if argIndex < args.length then {
              sb.append(BuiltinHelpers.numberToJSString(args(argIndex).toNumber))
              argIndex += 1
            }
            i += 2
          case 'f' =>
            if argIndex < args.length then {
              val d = args(argIndex).toNumber
              sb.append(
                if d.isNaN then "NaN"
                else if d == math.floor(d) && !d.isInfinite then d.toLong.toString
                else d.toString
              )
              argIndex += 1
            }
            i += 2
          case 'j' =>
            if argIndex < args.length then {
              ctx.global.get("JSON") match {
                case JSValue.Object(json) =>
                  json.get("stringify")(using ctx) match {
                    case fn if BuiltinHelpers.isCallable(fn) =>
                      BuiltinHelpers
                        .callFunctionWithThis(fn, JSValue.Object(json), Array(args(argIndex))) match {
                        case JSValue.JSStr(s) => sb.append(s)
                        case _                => sb.append("[Circular]")
                      }
                    case _ => sb.append(inspect(args(argIndex)))
                  }
                case _ => sb.append(inspect(args(argIndex)))
              }
              argIndex += 1
            }
            i += 2
          case 'o' | 'O' =>
            if argIndex < args.length then {
              sb.append(inspect(args(argIndex)))
              argIndex += 1
            }
            i += 2
          case 'c' =>
            argIndex += 1
            i += 2
          case _ =>
            sb.append(c)
            i += 1
        }
      } else {
        sb.append(c)
        i += 1
      }
    }
    if argIndex < args.length then {
      sb.append(" ")
      sb.append(args.drop(argIndex).map(v => inspect(v)).mkString(" "))
    }
    sb.toString
  }

  // =========================================================================
  // Promises
  // =========================================================================

  def promisify(fn: JSValue)(using ctx: JSContext): JSValue = {
    JSValue.Native(NativeFunction(
      name = if BuiltinHelpers.isCallable(fn) then "promisified" else "promisified",
      length = 0,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val promiseCtor = callCtx.global.get("Promise") match {
          case JSValue.Native(nc: NativeConstructor) => nc
          case _ =>
            NodeHelpers.throwCoded("TypeError", "Promise is not available", "ERR_INTERNAL")
        }
        val executor = NativeFunction(
          name = "executor",
          length = 2,
          impl = (execArgs, execCtx) => {
            given JSContext = execCtx
            val resolve = execArgs.lift(1).getOrElse(JSValue.Undefined)
            val reject = execArgs.lift(2).getOrElse(JSValue.Undefined)
            val callback = NativeFunction(
              name = "callback",
              length = 2,
              impl = (cbArgs, cbCtx) => {
                given JSContext = cbCtx
                val n = cbArgs.length
                val err = if n >= 2 then cbArgs(n - 2) else JSValue.Undefined
                val value = if n >= 1 then cbArgs(n - 1) else JSValue.Undefined
                if err != JSValue.Undefined && err != JSValue.Null then
                  BuiltinHelpers.callFunctionWithThis(reject, JSValue.Undefined, Array(err))
                else
                  BuiltinHelpers.callFunctionWithThis(resolve, JSValue.Undefined, Array(value))
                JSValue.Undefined
              }
            )
            try
              BuiltinHelpers.callFunctionWithThis(
                fn,
                JSValue.Undefined,
                args ++ Array(JSValue.Native(callback))
              )
            catch {
              case e: quickjs.runtime.JSException =>
                BuiltinHelpers.callFunctionWithThis(reject, JSValue.Undefined, Array(e.getValue))
              case e: Throwable =>
                BuiltinHelpers.callFunctionWithThis(
                  reject,
                  JSValue.Undefined,
                  Array(execCtx.createError("Error", String.valueOf(e.getMessage)))
                )
            }
            JSValue.Undefined
          }
        )
        promiseCtor.construct(Array(JSValue.Native(executor)))
      }
    ))
  }

  def callbackify(fn: JSValue)(using ctx: JSContext): JSValue =
    JSValue.Native(NativeFunction(
      name = "callbackified",
      length = 0,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val callback = args.lastOption.getOrElse(JSValue.Undefined)
        val realArgs = if args.isEmpty then args else args.dropRight(1)
        val onFulfilled = NativeFunction(
          name = "onFulfilled",
          length = 1,
          impl = (fulfilledArgs, fCtx) => {
            given JSContext = fCtx
            BuiltinHelpers.callFunctionWithThis(
              callback,
              JSValue.Undefined,
              Array(JSValue.Null, fulfilledArgs.lastOption.getOrElse(JSValue.Undefined))
            )
          }
        )
        val onRejected = NativeFunction(
          name = "onRejected",
          length = 1,
          impl = (rejectedArgs, rCtx) => {
            given JSContext = rCtx
            BuiltinHelpers.callFunctionWithThis(
              callback,
              JSValue.Undefined,
              Array(rejectedArgs.lastOption.getOrElse(JSValue.Undefined))
            )
          }
        )
        val result =
          BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, realArgs)
        val thenFn = BuiltinHelpers.getPropertyWithGetter(result, "then")
        if BuiltinHelpers.isCallable(thenFn) then
          BuiltinHelpers.callFunctionWithThis(
            thenFn,
            result,
            Array(JSValue.Native(onFulfilled), JSValue.Native(onRejected))
          )
        JSValue.Undefined
      }
    ))

  // =========================================================================
  // Module factory
  // =========================================================================

  def create()(using ctx: JSContext): JSValue = {
    val util = JSObject(prototype = ctx.objectPrototype)

    def strip(args: Array[JSValue]): Array[JSValue] =
      NodeHelpers.stripReceiver(args, util)

    val formatFn = NativeFunction(
      name = "format",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        JSValue.fromString(format(strip(args)))
      }
    )
    util.set("format", JSValue.Native(formatFn))
    util.set(
      "formatWithOptions",
      JSValue.Native(
        NativeFunction(
          name = "formatWithOptions",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args).drop(1)
            JSValue.fromString(format(rest))
          }
        )
      )
    )
    util.set(
      "inspect",
      JSValue.Native(
        NativeFunction(
          name = "inspect",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            JSValue.fromString(
              inspect(strip(args).headOption.getOrElse(JSValue.Undefined))
            )
          }
        )
      )
    )
    util.set(
      "promisify",
      JSValue.Native(
        NativeFunction(
          name = "promisify",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            promisify(strip(args).headOption.getOrElse(JSValue.Undefined))
          }
        )
      )
    )
    util.set(
      "callbackify",
      JSValue.Native(
        NativeFunction(
          name = "callbackify",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            callbackify(strip(args).headOption.getOrElse(JSValue.Undefined))
          }
        )
      )
    )
    util.set(
      "inherits",
      JSValue.Native(
        NativeFunction(
          name = "inherits",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            val ctor = rest.headOption.getOrElse(JSValue.Undefined)
            val superCtor = rest.lift(1).getOrElse(JSValue.Undefined)
            (funcObjOf(ctor), prototypeOf(superCtor)) match {
              case (Some(ctorObj), Some(superProto)) =>
                prototypeValueOf(ctor) match {
                  case Some(ctorProto) => ctorProto.setPrototype(superProto)
                  case None            => ()
                }
                ctorObj.set("super_", superCtor)
              case _ => ()
            }
            JSValue.Undefined
          }
        )
      )
    )
    util.set(
      "deprecate",
      JSValue.Native(
        NativeFunction(
          name = "deprecate",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            val fn = rest.headOption.getOrElse(JSValue.Undefined)
            val message = rest.lift(1).map(NodeHelpers.toStr(_)).getOrElse("")
            var warned = false
            JSValue.Native(NativeFunction(
              name = "deprecated",
              length = 0,
              impl = (callArgs, innerCtx) => {
                given JSContext = innerCtx
                if !warned then {
                  warned = true
                  System.err.println(s"DeprecationWarning: $message")
                }
                BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, callArgs)
              }
            ))
          }
        )
      )
    )
    util.set(
      "isDeepStrictEqual",
      JSValue.Native(
        NativeFunction(
          name = "isDeepStrictEqual",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            JSValue.Bool(
              NodeAssert.deepEqualValues(
                rest.headOption.getOrElse(JSValue.Undefined),
                rest.lift(1).getOrElse(JSValue.Undefined),
                strict = true
              )
            )
          }
        )
      )
    )

    val types = JSObject(prototype = null)
    def typeCheck(name: String)(pred: JSValue => Boolean): Unit =
      types.set(
        name,
        JSValue.Native(NativeFunction(name, (args, _) => JSValue.Bool(args.headOption.exists(pred)), length = 1))
      )
    typeCheck("isPromise")(v =>
      v match {
        case JSValue.Object(obj) => obj.getOwnPropertyRaw("__promise").isDefined
        case _: JSValue.Promise  => true
        case _                   => false
      }
    )
    typeCheck("isDate")(v =>
      v match {
        case JSValue.Object(obj) => obj.getOwnPropertyRaw("__dateValue").isDefined
        case _                   => false
      }
    )
    typeCheck("isRegExp")(v =>
      v match {
        case JSValue.Object(obj) => obj.getOwnPropertyRaw("__regexpPattern").isDefined
        case _                   => false
      }
    )
    typeCheck("isMap")(v =>
      v match {
        case JSValue.Object(obj) => obj.getOwnPropertyRaw("__mapStorage").isDefined
        case _                   => false
      }
    )
    typeCheck("isSet")(v =>
      v match {
        case JSValue.Object(obj) => obj.getOwnPropertyRaw("__setStorage").isDefined
        case _                   => false
      }
    )
    typeCheck("isTypedArray")(v =>
      v match {
        case JSValue.Object(obj) => obj.getOwnPropertyRaw("__taView").isDefined
        case _                   => false
      }
    )
    typeCheck("isNativeError")(v =>
      v match {
        case JSValue.Object(obj) =>
          BuiltinHelpers.getPropertyWithGetter(v, "name") match {
            case JSValue.JSStr(n) =>
              Set("Error", "TypeError", "RangeError", "SyntaxError", "ReferenceError", "EvalError", "URIError", "AggregateError").contains(n)
            case _ => false
          }
        case _ => false
      }
    )
    util.set("types", JSValue.Object(types))

    util.set("TextEncoder", ctx.global.get("TextEncoder"))
    util.set("TextDecoder", ctx.global.get("TextDecoder"))

    JSValue.Object(util)
  }

  private def funcObjOf(value: JSValue): Option[JSObject] =
    value match {
      case f: JSValue.Function                => Some(f.funcObj)
      case JSValue.Native(nf: NativeFunction) => Some(nf.funcObj)
      case JSValue.Native(nc: NativeConstructor) => Some(nc.funcObj)
      case _                                  => None
    }

  private def prototypeValueOf(value: JSValue): Option[JSObject] =
    funcObjOf(value).flatMap(_.getOwnPropertyRaw("prototype")).collect {
      case JSValue.Object(obj) => obj
    }

  private def prototypeOf(value: JSValue): Option[JSObject] =
    value match {
      case JSValue.Native(nc: NativeConstructor) => Some(nc.prototype)
      case _                                     => prototypeValueOf(value)
    }
}
