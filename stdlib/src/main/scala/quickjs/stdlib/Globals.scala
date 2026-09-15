package quickjs.stdlib

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.JSObject
import java.nio.charset.StandardCharsets
import java.util.Base64

/** Common host globals that scripts expect: `atob`/`btoa`,
  * `TextEncoder`/`TextDecoder` and `structuredClone`.
  */
object Globals {

  def initialize()(using ctx: JSContext): Unit = {
    installLegacyEscapes()
    installBase64()
    installTextCodecs()
    installStructuredClone()
    URLBuiltins.initialize()
  }

  private def jsError(name: String, message: String)(using ctx: JSContext): Nothing = {
    val error = ctx.createError("Error", message)
    error match {
      case JSValue.Object(obj) =>
        obj.defineProperty(
          "name",
          JSValue.fromString(name),
          enumerable = false,
          writable = true,
          configurable = true
        )(using ctx)
      case _ => ()
    }
    throw new quickjs.runtime.JSException(error)
  }

  /** Annex B `escape`/`unescape` globals (V8 provides them everywhere). */
  private def installLegacyEscapes()(using ctx: JSContext): Unit = {
    val escapeFn = NativeFunction(
      name = "escape",
      length = 1,
      impl = (args, c) => {
        given JSContext = c
        val text = BuiltinHelpers.toJSString(
          args.headOption.getOrElse(JSValue.Undefined)
        )
        val sb = new StringBuilder
        text.foreach { ch =>
          if (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') ||
              (ch >= '0' && ch <= '9') || "@*_+-./".indexOf(ch) >= 0
          then sb.append(ch)
          else if ch < 256 then sb.append(f"%%${ch.toInt}%02X")
          else sb.append(f"%%u${ch.toInt}%04X")
        }
        JSValue.fromString(sb.toString)
      }
    )
    val unescapeFn = NativeFunction(
      name = "unescape",
      length = 1,
      impl = (args, c) => {
        given JSContext = c
        val text = BuiltinHelpers.toJSString(
          args.headOption.getOrElse(JSValue.Undefined)
        )
        val sb = new StringBuilder
        var i = 0
        while i < text.length do {
          val ch = text.charAt(i)
          if ch == '%' && i + 5 < text.length &&
              (text.charAt(i + 1) == 'u' || text.charAt(i + 1) == 'U')
          then {
            try {
              sb.append(Integer.parseInt(text.substring(i + 2, i + 6), 16).toChar)
              i += 6
            } catch {
              case _: NumberFormatException =>
                sb.append(ch)
                i += 1
            }
          } else if ch == '%' && i + 2 < text.length then {
            try {
              sb.append(Integer.parseInt(text.substring(i + 1, i + 3), 16).toChar)
              i += 3
            } catch {
              case _: NumberFormatException =>
                sb.append(ch)
                i += 1
            }
          } else {
            sb.append(ch)
            i += 1
          }
        }
        JSValue.fromString(sb.toString)
      }
    )
    ctx.global.set("escape", JSValue.Native(escapeFn))
    ctx.global.set("unescape", JSValue.Native(unescapeFn))
  }

  private def installBase64()(using ctx: JSContext): Unit = {
    def toBinaryString(value: JSValue)(using ctx: JSContext): String = {
      val text = BuiltinHelpers.toJSString(value)
      var i = 0
      while i < text.length do {
        if text.charAt(i) > 0xff then
          jsError("InvalidCharacterError", "The string to be encoded contains characters outside of the Latin1 range.")
        i += 1
      }
      text
    }

    val atobFn = NativeFunction(
      name = "atob",
      length = 1,
      impl = (args, ctx) => {
        given JSContext = ctx
        val input = BuiltinHelpers.toJSString(
          args.headOption.getOrElse(JSValue.Undefined)
        )
        val cleaned = input.filterNot(c => c == '\n' || c == '\r' || c == '\t' || c == ' ')
        try {
          val bytes = Base64.getDecoder.decode(cleaned)
          val sb = new StringBuilder(bytes.length)
          bytes.foreach(b => sb.append((b & 0xff).toChar))
          JSValue.fromString(sb.toString)
        } catch {
          case _: IllegalArgumentException =>
            jsError("InvalidCharacterError", "The string to be decoded is not correctly encoded.")
        }
      }
    )
    val btoaFn = NativeFunction(
      name = "btoa",
      length = 1,
      impl = (args, ctx) => {
        given JSContext = ctx
        val text = toBinaryString(args.headOption.getOrElse(JSValue.Undefined))
        val bytes = new Array[Byte](text.length)
        var i = 0
        while i < text.length do {
          bytes(i) = text.charAt(i).toByte
          i += 1
        }
        JSValue.fromString(Base64.getEncoder.encodeToString(bytes))
      }
    )
    ctx.global.set("atob", JSValue.Native(atobFn))
    ctx.global.set("btoa", JSValue.Native(btoaFn))
  }

  private def uint8ArrayOf(bytes: Array[Byte])(using ctx: JSContext): JSValue = {
    val arr = quickjs.objmodel.JSArray.empty()
    bytes.foreach(b => arr.push(JSValue.fromInt(b & 0xff)))
    ctx.global.get("Uint8Array") match {
      case JSValue.Native(ctor: NativeConstructor) =>
        ctor.construct(Array(JSValue.JSArrayVal(arr)))
      case _ =>
        JSValue.JSArrayVal(arr)
    }
  }

  /** Read bytes out of a Uint8Array/ArrayBuffer/array-like value. */
  private def bytesOf(value: JSValue)(using ctx: JSContext): Array[Byte] =
    value match {
      case JSValue.Undefined => Array.empty
      case JSValue.JSArrayVal(arr) =>
        (0 until arr.getLength).map(i => arr.get(i).toNumber.toInt.toByte).toArray
      case JSValue.Object(obj) =>
        // ArrayBuffer: wrap it in a Uint8Array view first.
        obj.getOwnProperty("__abStorage") match {
          case Some(_) =>
            ctx.global.get("Uint8Array") match {
              case JSValue.Native(ctor: NativeConstructor) =>
                bytesOf(ctor.construct(Array(value)))
              case _ => Array.empty
            }
          case None =>
            val interpreter = quickjs.interpreter.Interpreter()
            val length = interpreter.getPropertyValue(
              obj,
              value,
              "length",
              Nil,
              quickjs.tracing.TraceRecorder.Noop
            )
            length match {
              case JSValue.Int32(len) =>
                (0 until len).map { i =>
                  interpreter
                    .getPropertyValue(
                      obj,
                      value,
                      i.toString,
                      Nil,
                      quickjs.tracing.TraceRecorder.Noop
                    )
                    .toNumber
                    .toInt
                    .toByte
                }.toArray
              case _ => Array.empty
            }
        }
      case _ => Array.empty
    }

  private def installTextCodecs()(using ctx: JSContext): Unit = {
    // TextEncoder
    val encoderProto = JSObject(prototype = ctx.objectPrototype)
    val encoderCtor = NativeConstructor(
      name = "TextEncoder",
      callImpl = (_, ctx) => jsError("TypeError", "Constructor TextEncoder requires 'new'")(using ctx),
      constructImpl = (_, _) => JSValue.Object(JSObject(prototype = encoderProto)),
      prototype = encoderProto
    )
    BuiltinHelpers.initConstructor(encoderCtor, length = 0)
    encoderProto.defineProperty(
      "encoding",
      JSValue.fromString("utf-8"),
      enumerable = true,
      writable = false,
      configurable = true
    )
    val encodeFn = NativeFunction(
      name = "encode",
      length = 0,
      impl = (args, ctx) => {
        given JSContext = ctx
        val text = BuiltinHelpers.toJSString(
          args.lift(1).getOrElse(JSValue.Undefined)
        )
        uint8ArrayOf(text.getBytes(StandardCharsets.UTF_8))
      }
    )
    encoderProto.defineProperty("encode", JSValue.Native(encodeFn), enumerable = true)
    val encodeIntoFn = NativeFunction(
      name = "encodeInto",
      length = 2,
      impl = (args, ctx) => {
        given JSContext = ctx
        val text = BuiltinHelpers.toJSString(
          args.lift(1).getOrElse(JSValue.Undefined)
        )
        val dest = args.lift(2).getOrElse(JSValue.Undefined)
        val bytes = text.getBytes(StandardCharsets.UTF_8)
        var written = 0
        var read = 0
        val reflect = ctx.global.get("Reflect")
        val reflectSet = BuiltinHelpers.getPropertyWithGetter(reflect, "set")
        // Write whole code points only; Uint8Array length bounds the output.
        var i = 0
        while i < bytes.length && written < 4096 do {
          if dest.isInstanceOf[JSValue.Object] then {
            BuiltinHelpers.callFunctionWithThis(
              reflectSet,
              reflect,
              Array(dest, JSValue.fromInt(written), JSValue.fromInt(bytes(i) & 0xff))
            )
            ()
            written += 1
          }
          i += 1
        }
        read = text.length
        val obj = JSObject(prototype = ctx.objectPrototype)
        obj.set("read", JSValue.fromInt(read))
        obj.set("written", JSValue.fromInt(written))
        JSValue.Object(obj)
      }
    )
    encoderProto.defineProperty("encodeInto", JSValue.Native(encodeIntoFn), enumerable = true)

    // TextDecoder
    val decoderProto = JSObject(prototype = ctx.objectPrototype)
    val decoderCtor = NativeConstructor(
      name = "TextDecoder",
      callImpl = (_, ctx) => jsError("TypeError", "Constructor TextDecoder requires 'new'")(using ctx),
      constructImpl = (args, ctx) => {
        given JSContext = ctx
        val label =
          if args.nonEmpty && args(0) != JSValue.Undefined then
            BuiltinHelpers.toJSString(args(0)).toLowerCase
          else "utf-8"
        if label != "utf-8" && label != "utf8" && label != "unicode-1-1-utf-8" then
          jsError("RangeError", s"The encoding label provided ('$label') is invalid.")
        JSValue.Object(JSObject(prototype = decoderProto))
      },
      prototype = decoderProto
    )
    BuiltinHelpers.initConstructor(decoderCtor, length = 0)
    decoderProto.defineProperty(
      "encoding",
      JSValue.fromString("utf-8"),
      enumerable = true,
      writable = false,
      configurable = true
    )
    val decodeFn = NativeFunction(
      name = "decode",
      length = 0,
      impl = (args, ctx) => {
        given JSContext = ctx
        val input = args.lift(1).getOrElse(JSValue.Undefined)
        if input == JSValue.Undefined then JSValue.fromString("")
        else JSValue.fromString(new String(bytesOf(input), StandardCharsets.UTF_8))
      }
    )
    decoderProto.defineProperty("decode", JSValue.Native(decodeFn), enumerable = true)

    ctx.global.set("TextEncoder", JSValue.Native(encoderCtor))
    ctx.global.set("TextDecoder", JSValue.Native(decoderCtor))
  }

  private def installStructuredClone()(using ctx: JSContext): Unit = {
    val cloneFn = NativeFunction(
      name = "structuredClone",
      length = 1,
      impl = (args, ctx) => {
        given JSContext = ctx
        val seen =
          new java.util.IdentityHashMap[JSValue, JSValue]()

        def dataCloneError(): Nothing =
          jsError("DataCloneError", "The object could not be cloned.")

        def objOf(value: JSValue): JSObject = value match {
          case JSValue.Object(obj) => obj
          case _ => dataCloneError()
        }

        def ownProp(obj: JSObject, key: String): Option[JSValue] =
          obj.getOwnProperty(key)

        /** Copy an ArrayBuffer, TypedArray or DataView. */
        def cloneView(obj: JSObject, value: JSValue): JSValue = {
          val isTypedArray = ownProp(obj, "__taType").isDefined
          val isDataView = ownProp(obj, "__dvStorage").isDefined
          val isArrayBuffer =
            ownProp(obj, "__abStorage").isDefined && !isTypedArray && !isDataView
          if isArrayBuffer then cloneArrayBuffer(obj, value)
          else cloneTypedArrayOrDataView(obj, value, isTypedArray)
        }

        def cloneArrayBuffer(obj: JSObject, value: JSValue): JSValue = {
          val abCtor = ctx.global.get("ArrayBuffer") match {
            case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc
            case _ => dataCloneError()
          }
          val byteLength = BuiltinHelpers
            .toNumber(BuiltinHelpers.getPropertyWithGetter(value, "byteLength"))
            .toInt
          val copy = abCtor.construct(Array(JSValue.fromInt(byteLength)))
          seen.put(value, copy)
          val u8Ctor = ctx.global.get("Uint8Array") match {
            case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc
            case _ => dataCloneError()
          }
          val src = u8Ctor.construct(Array(value))
          val dst = u8Ctor.construct(Array(copy))
          BuiltinHelpers.callFunctionWithThis(
            u8Ctor.prototype.get("set")(using ctx),
            dst,
            Array(src)
          )
          copy
        }

        def cloneTypedArrayOrDataView(
            obj: JSObject,
            value: JSValue,
            isTypedArray: Boolean
        ): JSValue = {
          val buffer = BuiltinHelpers.getPropertyWithGetter(value, "buffer")
          val byteOffset = BuiltinHelpers
            .toNumber(BuiltinHelpers.getPropertyWithGetter(value, "byteOffset"))
            .toInt
          val byteLength = BuiltinHelpers
            .toNumber(BuiltinHelpers.getPropertyWithGetter(value, "byteLength"))
            .toInt
          // The clone gets a fresh buffer containing only the viewed bytes.
          val abCtor = ctx.global.get("ArrayBuffer") match {
            case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc
            case _ => dataCloneError()
          }
          val clonedBuffer = abCtor.construct(Array(JSValue.fromInt(byteLength)))
          val u8Ctor = ctx.global.get("Uint8Array") match {
            case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc
            case _ => dataCloneError()
          }
          val src = u8Ctor.construct(
            Array(buffer, JSValue.fromInt(byteOffset), JSValue.fromInt(byteLength))
          )
          val dst = u8Ctor.construct(Array(clonedBuffer))
          BuiltinHelpers.callFunctionWithThis(
            u8Ctor.prototype.get("set")(using ctx),
            dst,
            Array(src)
          )
          if !isTypedArray then {
            val dvCtor = ctx.global.get("DataView") match {
              case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc
              case _ => dataCloneError()
            }
            val copy = dvCtor.construct(Array(clonedBuffer))
            seen.put(value, copy)
            copy
          } else {
            // The typed array constructor is selected from Symbol.toStringTag.
            val tagValue = BuiltinHelpers.getSymbolPropertyWithGetter(
              value,
              BuiltinHelpers.wellKnownSymbolId("toStringTag")
            )
            val ctorName = tagValue match {
              case JSValue.JSStr(s) => s
              case _ => dataCloneError()
            }
            val ctor = ctx.global.get(ctorName) match {
              case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc
              case _ => dataCloneError()
            }
            val copy = ctor.construct(Array(clonedBuffer))
            seen.put(value, copy)
            copy
          }
        }

        def cloneMapSet(value: JSValue, obj: JSObject, isMap: Boolean): JSValue = {
          val ctorName = if isMap then "Map" else "Set"
          val ctor = ctx.global.get(ctorName) match {
            case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc
            case _ => dataCloneError()
          }
          val copy = ctor.construct(Array.empty)
          seen.put(value, copy)
          val adder = BuiltinHelpers.getPropertyWithGetter(
            copy,
            if isMap then "set" else "add"
          )
          val record = BuiltinHelpers.getIteratorRecord(value)
          try {
            var step = BuiltinHelpers.iteratorStepValue(record)
            while step.isDefined do {
              val item = step.get
              if isMap then {
                val k = BuiltinHelpers.getPropertyWithGetter(item, "0")
                val v = BuiltinHelpers.getPropertyWithGetter(item, "1")
                BuiltinHelpers.callFunctionWithThis(adder, copy, Array(clone(k), clone(v)))
              } else {
                BuiltinHelpers.callFunctionWithThis(adder, copy, Array(clone(item)))
              }
              step = BuiltinHelpers.iteratorStepValue(record)
            }
          } catch {
            case e: Throwable =>
              BuiltinHelpers.iteratorCloseRecord(record)
              throw e
          }
          copy
        }

        def clone(value: JSValue): JSValue = value match {
          case JSValue.Symbol(_) => dataCloneError()
          case _: JSValue.Function => dataCloneError()
          case JSValue.Native(_) => dataCloneError()
          case JSValue.JSArrayVal(_) =>
            val existing = seen.get(value)
            if existing != null then existing
            else
              value match {
                case JSValue.JSArrayVal(arr) =>
                  val copy = quickjs.objmodel.JSArray.empty()
                  seen.put(value, JSValue.JSArrayVal(copy))
                  var i = 0
                  while i < arr.getLength do {
                    copy.push(clone(arr.get(i)))
                    i += 1
                  }
                  JSValue.JSArrayVal(copy)
                case _ => dataCloneError()
              }
          case JSValue.Object(obj) =>
            val existing = seen.get(value)
            if existing != null then existing
            else if ownProp(obj, "__proxy_target").isDefined then
              dataCloneError()
            else if ownProp(obj, "__weakMapStorage").isDefined ||
              ownProp(obj, "__weakSetStorage").isDefined
            then dataCloneError()
            else if ownProp(obj, "__mapStorage").isDefined then
              cloneMapSet(value, obj, isMap = true)
            else if ownProp(obj, "__setStorage").isDefined then
              cloneMapSet(value, obj, isMap = false)
            else if ownProp(obj, "__taType").isDefined ||
              ownProp(obj, "__dvStorage").isDefined ||
              ownProp(obj, "__abStorage").isDefined
            then cloneView(obj, value)
            else if ownProp(obj, "__dateValue").isDefined then {
              val millis = ownProp(obj, "__dateValue").get
              val dateCtor = ctx.global.get("Date") match {
                case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc
                case _ => dataCloneError()
              }
              val copy = dateCtor.construct(Array(millis))
              seen.put(value, copy)
              copy
            } else if ownProp(obj, "__regexpPattern").isDefined then {
              val ctor = ctx.global.get("RegExp")
              val source = ownProp(obj, "__regexpPattern").get
              val flags = ownProp(obj, "__regexpFlags").getOrElse(JSValue.fromString(""))
              val result = BuiltinHelpers.callFunctionWithThis(
                ctor,
                JSValue.Undefined,
                Array(source, flags)
              )
              seen.put(value, result)
              result match {
                case JSValue.Object(re) =>
                  // Preserve lastIndex.
                  ownProp(obj, "lastIndex").foreach(v => re.set("lastIndex", v))
                case _ => ()
              }
              result
            } else {
              val copy = JSObject(prototype = obj.getPrototype)
              obj.getPrimitiveValue.foreach(copy.setPrimitiveValue)
              seen.put(value, JSValue.Object(copy))
              obj.getAllProperties.foreach { case (key, v) =>
                if !key.startsWith("__") then copy.set(key, clone(v))
              }
              JSValue.Object(copy)
            }
          case other => other
        }

        clone(args.headOption.getOrElse(JSValue.Undefined))
      }
    )
    ctx.global.set("structuredClone", JSValue.Native(cloneFn))
  }
}
