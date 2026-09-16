package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.{BuiltinHelpers, TypedArrayBuiltins}
import quickjs.runtime.builtins.TypedArrayBuiltins.{ArrayBufferStorage, TypedArrayType, TypedArrayView}
import quickjs.objmodel.{JSArray, JSObject}

import java.nio.charset.StandardCharsets
import java.util.Base64

/** Node's `Buffer` (a Uint8Array subclass with Node's binary helpers).
  *
  * Buffers are represented as typed array objects with a Buffer-specific
  * prototype chained to `Uint8Array.prototype`, so integer indexing, `.length`
  * and the Uint8Array methods keep working.
  */
object NodeBuffer {

  private val poolSize = 8192

  private var prototypeRef: JSObject | Null = null

  /** Expose the Buffer prototype to sibling modules (fs, modules, ...). */
  private[node] def prototype: JSObject = prototypeRef.asInstanceOf[JSObject]

  private[node] def isBufferValue(value: JSValue): Boolean =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__isBuffer").contains(JSValue.Bool(true))
      case _ => false
    }

  /** Copy the bytes out of a Buffer/Uint8Array/ArrayBuffer value. */
  private[node] def bytesOfValue(value: JSValue): Option[Array[Byte]] =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__taView") match {
          case Some(JSValue.Native(view: TypedArrayView)) =>
            if view.buffer.detached then Some(Array.emptyByteArray)
            else {
              val out = new Array[Byte](view.byteLength)
              System.arraycopy(view.buffer.data, view.byteOffset, out, 0, out.length)
              Some(out)
            }
          case _ =>
            obj.getOwnPropertyRaw("__abStorage") match {
              case Some(JSValue.Native(storage: ArrayBufferStorage)) =>
                if storage.detached then Some(Array.emptyByteArray)
                else Some(storage.data.clone())
              case _ => None
            }
        }
      case JSValue.JSArrayVal(arr) =>
        val out = new Array[Byte](arr.getLength)
        var i = 0
        while i < arr.getLength do {
          out(i) = (arr.get(i).toNumber.toInt & 0xff).toByte
          i += 1
        }
        Some(out)
      case _ => None
    }

  /** Create a real Buffer instance from raw bytes. */
  private[node] def makeBuffer(bytes: Array[Byte])(using ctx: JSContext): JSValue = {
    val proto = prototype
    val storage = ArrayBufferStorage(bytes.length)
    System.arraycopy(bytes, 0, storage.data, 0, bytes.length)
    val view =
      new TypedArrayView(storage, 0, bytes.length, TypedArrayType.Uint8, bytes.length)
    val obj = JSObject(prototype = proto)
    obj.initProperty("__taView", JSValue.Native(view), enumerable = false, writable = false, configurable = false)
    obj.initProperty("__taType", JSValue.Native(TypedArrayType.Uint8), enumerable = false, writable = false, configurable = false)
    obj.initProperty("__isBuffer", JSValue.Bool(true), enumerable = false, writable = false, configurable = false)
    JSValue.Object(obj)
  }

  def create()(using ctx: JSContext): JSValue = {
    val uint8Proto = ctx.getTypedArrayPrototype("Uint8Array")
    val bufferPrototype = JSObject(prototype = uint8Proto)
    prototypeRef = bufferPrototype

    // Created below; referenced by the static helpers to strip the receiver
    // that method dispatch passes as args(0) (Buffer.from => this === Buffer).
    var ctorRef: NativeConstructor = null

    def stripStatic(args: Array[JSValue]): Array[JSValue] =
      if args.nonEmpty then
        args(0) match {
          case JSValue.Native(nc: NativeConstructor)
              if ctorRef != null &&
                nc.asInstanceOf[AnyRef].eq(ctorRef.asInstanceOf[AnyRef]) =>
            args.drop(1)
          case _ => args
        }
      else args

    // ---- byte view helpers -------------------------------------------------

    def viewOf(value: JSValue): Option[TypedArrayView] =
      value match {
        case JSValue.Object(obj) =>
          obj.getOwnPropertyRaw("__taView") match {
            case Some(JSValue.Native(v: TypedArrayView)) => Some(v)
            case _                                       => None
          }
        case _ => None
      }

    def bytesOf(value: JSValue): Option[Array[Byte]] =
      viewOf(value).map { view =>
        if view.buffer.detached then Array.emptyByteArray
        else {
          val out = new Array[Byte](view.byteLength)
          System.arraycopy(view.buffer.data, view.byteOffset, out, 0, view.byteLength)
          out
        }
      }

    def newView(
        storage: ArrayBufferStorage,
        byteOffset: Int,
        length: Int
    ): JSValue =
      fromView(
        new TypedArrayView(
          storage,
          byteOffset,
          length,
          TypedArrayType.Uint8,
          length
        )
      )

    def fromView(view: TypedArrayView): JSValue = {
      val obj = JSObject(prototype = bufferPrototype)
      obj.initProperty("__taView", JSValue.Native(view), enumerable = false, writable = false, configurable = false)
      obj.initProperty("__taType", JSValue.Native(TypedArrayType.Uint8), enumerable = false, writable = false, configurable = false)
      obj.initProperty("__isBuffer", JSValue.Bool(true), enumerable = false, writable = false, configurable = false)
      JSValue.Object(obj)
    }

    def fromBytes(bytes: Array[Byte]): JSValue = {
      val storage = ArrayBufferStorage(bytes.length)
      System.arraycopy(bytes, 0, storage.data, 0, bytes.length)
      newView(storage, 0, bytes.length)
    }

    def isBuffer(value: JSValue): Boolean =
      value match {
        case JSValue.Object(obj) =>
          obj.getOwnPropertyRaw("__isBuffer").contains(JSValue.Bool(true))
        case _ => false
      }

    // ---- encodings ---------------------------------------------------------

    def normalizeEncoding(label: String): String =
      label.toLowerCase.replace("_", "").replace("-", "") match {
        case "utf8"                                   => "utf8"
        case "utf16le" | "ucs2" | "ucs2le" | "utf16"  => "utf16le"
        case "latin1" | "binary"                      => "latin1"
        case "ascii"                                  => "ascii"
        case "base64"                                 => "base64"
        case "base64url"                              => "base64url"
        case "hex"                                    => "hex"
        case other                                    => other
      }

    def bytesFromString(text: String, encoding: String): Array[Byte] =
      normalizeEncoding(encoding) match {
        case "utf8" => text.getBytes(StandardCharsets.UTF_8)
        case "latin1" =>
          val out = new Array[Byte](text.length)
          var i = 0
          while i < text.length do {
            out(i) = (text.charAt(i) & 0xff).toByte
            i += 1
          }
          out
        case "ascii" =>
          val out = new Array[Byte](text.length)
          var i = 0
          while i < text.length do {
            out(i) = (text.charAt(i) & 0x7f).toByte
            i += 1
          }
          out
        case "utf16le" =>
          val out = new Array[Byte](text.length * 2)
          var i = 0
          while i < text.length do {
            val c = text.charAt(i)
            out(i * 2) = (c & 0xff).toByte
            out(i * 2 + 1) = ((c >> 8) & 0xff).toByte
            i += 1
          }
          out
        case "hex" =>
          val cleaned = text.trim
          if cleaned.length % 2 != 0 then
            NodeHelpers.throwCoded(
              "Error",
              "The string must be an even number of hexadecimal characters",
              "ERR_INVALID_ARG_VALUE"
            )
          val out = new Array[Byte](cleaned.length / 2)
          var i = 0
          while i < out.length do {
            val hi = Character.digit(cleaned.charAt(i * 2), 16)
            val lo = Character.digit(cleaned.charAt(i * 2 + 1), 16)
            if hi < 0 || lo < 0 then
              NodeHelpers.throwCoded(
                "Error",
                "Invalid hexadecimal string",
                "ERR_INVALID_ARG_VALUE"
              )
            out(i) = ((hi << 4) | lo).toByte
            i += 1
          }
          out
        case "base64" | "base64url" =>
          val normalized = text
            .replace('-', '+')
            .replace('_', '/')
            .filterNot(c => c == '\n' || c == '\r' || c == ' ' || c == '\t')
          val padded = normalized + ("=" * ((4 - normalized.length % 4) % 4))
          try Base64.getDecoder.decode(padded)
          catch
            case _: IllegalArgumentException =>
              NodeHelpers.throwCoded(
                "Error",
                "Invalid base64 string",
                "ERR_INVALID_ARG_VALUE"
              )
        case other =>
          NodeHelpers.throwCoded(
            "TypeError",
            s"Unknown encoding: $other",
            "ERR_UNKNOWN_ENCODING"
          )
      }

    def stringFromBytes(bytes: Array[Byte], encoding: String): String =
      normalizeEncoding(encoding) match {
        case "utf8" => new String(bytes, StandardCharsets.UTF_8)
        case "latin1" =>
          val sb = new StringBuilder(bytes.length)
          bytes.foreach(b => sb.append((b & 0xff).toChar))
          sb.toString
        case "ascii" =>
          val sb = new StringBuilder(bytes.length)
          bytes.foreach(b => sb.append((b & 0x7f).toChar))
          sb.toString
        case "utf16le" =>
          val sb = new StringBuilder(bytes.length / 2)
          var i = 0
          while i + 1 < bytes.length do {
            sb.append(((bytes(i) & 0xff) | ((bytes(i + 1) & 0xff) << 8)).toChar)
            i += 2
          }
          sb.toString
        case "hex" =>
          val sb = new StringBuilder(bytes.length * 2)
          bytes.foreach(b => sb.append(f"${b & 0xff}%02x"))
          sb.toString
        case "base64"    => Base64.getEncoder.encodeToString(bytes)
        case "base64url" => Base64.getUrlEncoder.withoutPadding.encodeToString(bytes)
        case other =>
          NodeHelpers.throwCoded(
            "TypeError",
            s"Unknown encoding: $other",
            "ERR_UNKNOWN_ENCODING"
          )
      }

    /** Byte range from Node-style (offset, end) arguments. Negative offsets
      * count from the end; the end defaults to `total`.
      */
    def rangeOf(
        total: Int,
        startValue: JSValue,
        endValue: JSValue
    )(using ctx: JSContext): (Int, Int) = {
      def toIndex(value: JSValue, default: Int): Int =
        if value == JSValue.Undefined then default
        else {
          val d = NodeHelpers.toNumber(value)
          if d.isNaN then 0
          else if d < 0 then math.max(total + d.toInt, 0)
          else math.min(d.toInt, total)
        }
      val start = toIndex(startValue, 0)
      val end = toIndex(endValue, total)
      (start, math.max(start, end))
    }

    // ---- Buffer instance methods ------------------------------------------

    def installMethod(
        target: JSObject,
        name: String,
        length: Int
    )(impl: (Array[JSValue], JSContext) => JSValue): Unit = {
      val fn = NativeFunction(name = name, length = length, impl = impl)
      target.defineProperty(
        name,
        JSValue.Native(fn),
        enumerable = false,
        writable = true,
        configurable = true
      )
    }

    def asView(thisValue: JSValue, args: Array[JSValue])(using
        ctx: JSContext
    ): (TypedArrayView, Array[JSValue]) =
      viewOf(thisValue) match {
        case Some(view) => (view, args)
        case None =>
          NodeHelpers.throwCoded(
            "TypeError",
            "this is not a Buffer",
            "ERR_INVALID_THIS"
          )
      }

    def compareViews(a: TypedArrayView, b: TypedArrayView): Int = {
      val len = math.min(a.length, b.length)
      var i = 0
      while i < len do {
        val x = a.buffer.data(a.byteOffset + i) & 0xff
        val y = b.buffer.data(b.byteOffset + i) & 0xff
        if x != y then return x - y
        i += 1
      }
      a.length - b.length
    }

    // instance methods receive [this, ...args] so drop args(0) explicitly and
    // keep the receiver for identity-returning methods.
    def methodArgs(args: Array[JSValue]): (JSValue, Array[JSValue]) =
      if args.isEmpty then (JSValue.Undefined, Array.empty)
      else (args(0), args.drop(1))

    installMethod(bufferPrototype, "toString", 3) { (args, callCtx) =>
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      val encoding =
        rest.headOption match {
          case Some(JSValue.JSStr(s))          => s
          case Some(JSValue.Undefined) | None  => "utf8"
          case Some(other)                     => NodeHelpers.toStr(other)
        }
      val (start, end) = rangeOf(
        view.length,
        rest.lift(1).getOrElse(JSValue.Undefined),
        rest.lift(2).getOrElse(JSValue.Undefined)
      )
      val out = new Array[Byte](end - start)
      if !view.buffer.detached && end > start then
        System.arraycopy(view.buffer.data, view.byteOffset + start, out, 0, out.length)
      JSValue.fromString(stringFromBytes(out, encoding))
    }

    installMethod(bufferPrototype, "toJSON", 0) { (args, callCtx) =>
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      val data = JSArray.empty()
      var i = 0
      while i < view.length do {
        data.push(JSValue.fromInt(view.get(i).toNumber.toInt & 0xff))
        i += 1
      }
      JSValue.Object(
        NodeHelpers.objectOf(
          "type" -> JSValue.fromString("Buffer"),
          "data" -> JSValue.JSArrayVal(data)
        )
      )
    }

    installMethod(bufferPrototype, "equals", 1) { (args, callCtx) =>
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      val other = rest.headOption.flatMap(viewOf)
      JSValue.Bool(other.exists { o =>
        o.length == view.length && {
          var i = 0
          var equal = true
          while i < view.length && equal do {
            equal =
              (view.buffer.data(view.byteOffset + i) & 0xff) ==
                (o.buffer.data(o.byteOffset + i) & 0xff)
            i += 1
          }
          equal
        }
      })
    }

    installMethod(bufferPrototype, "compare", 1) { (args, callCtx) =>
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      rest.headOption.flatMap(viewOf) match {
        case Some(other) => JSValue.fromInt(compareViews(view, other))
        case None =>
          NodeHelpers.throwCoded(
            "TypeError",
            "The \"target\" argument must be an instance of Buffer",
            "ERR_INVALID_ARG_TYPE"
          )
      }
    }

    installMethod(bufferPrototype, "copy", 4) { (args, callCtx) =>
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      val targetView = rest.headOption.flatMap(viewOf).getOrElse(
        NodeHelpers.throwCoded(
          "TypeError",
          "The \"target\" argument must be an instance of Buffer",
          "ERR_INVALID_ARG_TYPE"
        )
      )
      val targetStart =
        if rest.lift(1).forall(_ == JSValue.Undefined) then 0
        else NodeHelpers.toNumber(rest(1)).toInt
      val sourceStart =
        if rest.lift(2).forall(_ == JSValue.Undefined) then 0
        else NodeHelpers.toNumber(rest(2)).toInt
      val sourceEnd =
        if rest.lift(3).forall(_ == JSValue.Undefined) then view.length
        else NodeHelpers.toNumber(rest(3)).toInt
      val toCopy = math.max(
        0,
        math.min(sourceEnd - sourceStart, targetView.length - targetStart)
      )
      if toCopy > 0 && !view.buffer.detached && !targetView.buffer.detached then
        System.arraycopy(
          view.buffer.data,
          view.byteOffset + sourceStart,
          targetView.buffer.data,
          targetView.byteOffset + targetStart,
          toCopy
        )
      JSValue.fromInt(toCopy)
    }

    installMethod(bufferPrototype, "slice", 2) { (args, callCtx) =>
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      val (start, end) = rangeOf(
        view.length,
        rest.headOption.getOrElse(JSValue.Undefined),
        rest.lift(1).getOrElse(JSValue.Undefined)
      )
      newView(view.buffer, view.byteOffset + start, end - start)
    }

    installMethod(bufferPrototype, "subarray", 2) { (args, callCtx) =>
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      val (start, end) = rangeOf(
        view.length,
        rest.headOption.getOrElse(JSValue.Undefined),
        rest.lift(1).getOrElse(JSValue.Undefined)
      )
      newView(view.buffer, view.byteOffset + start, end - start)
    }

    installMethod(bufferPrototype, "write", 4) { (args, callCtx) =>
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      val text = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val offset =
        if rest.lift(1).forall(_ == JSValue.Undefined) then 0
        else NodeHelpers.toNumber(rest(1)).toInt
      val maxLength =
        if rest.lift(2).forall(_ == JSValue.Undefined) then view.length - offset
        else NodeHelpers.toNumber(rest(2)).toInt
      val encoding =
        if rest.lift(3).forall(_ == JSValue.Undefined) then "utf8"
        else NodeHelpers.toStr(rest(3))
      val all = bytesFromString(text, encoding)
      val bytes =
        if normalizeEncoding(encoding) == "utf8" && all.length > maxLength then {
          // Do not split a multi-byte UTF-8 sequence at the boundary.
          var end = math.max(0, maxLength)
          while end > 0 && (all(end) & 0xc0) == 0x80 do end -= 1
          all.take(end)
        } else all.take(math.max(0, maxLength))
      val writable = math.max(0, math.min(bytes.length, view.length - offset))
      if writable > 0 && !view.buffer.detached then
        System.arraycopy(bytes, 0, view.buffer.data, view.byteOffset + offset, writable)
      JSValue.fromInt(writable)
    }

    installMethod(bufferPrototype, "fill", 4) { (args, callCtx) =>
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      val value = rest.headOption.getOrElse(JSValue.fromInt(0))
      val (start, end) = rangeOf(
        view.length,
        rest.lift(1).getOrElse(JSValue.Undefined),
        rest.lift(2).getOrElse(JSValue.Undefined)
      )
      val fillBytes: Array[Byte] = value match {
        case JSValue.JSStr(s) =>
          val encoding =
            if rest.lift(3).forall(_ == JSValue.Undefined) then "utf8"
            else NodeHelpers.toStr(rest(3))
          bytesFromString(s, encoding)
        case other =>
          Array(((NodeHelpers.toNumber(other).toInt) & 0xff).toByte)
      }
      if fillBytes.nonEmpty && !view.buffer.detached then {
        var i = start
        while i < end do {
          view.buffer.data(view.byteOffset + i) = fillBytes((i - start) % fillBytes.length)
          i += 1
        }
      }
      thisValue
    }

    def indexOfImpl(
        fromEnd: Boolean
    )(args: Array[JSValue], callCtx: JSContext): JSValue = {
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      val needleValue = rest.headOption.getOrElse(JSValue.Undefined)
      val needle: Array[Byte] =
        if isBuffer(needleValue) then bytesOf(needleValue).getOrElse(Array.emptyByteArray)
        else if needleValue.isInstanceOf[JSValue.JSStr] then {
          val encoding =
            if rest.lift(2).forall(_ == JSValue.Undefined) then "utf8"
            else NodeHelpers.toStr(rest(2))
          bytesFromString(NodeHelpers.toStr(needleValue), encoding)
        } else Array(((NodeHelpers.toNumber(needleValue).toInt) & 0xff).toByte)
      val offsetArg = rest.lift(1).getOrElse(JSValue.Undefined)
      val offset =
        if offsetArg == JSValue.Undefined then (if fromEnd then view.length else 0)
        else {
          val n = NodeHelpers.toNumber(offsetArg).toInt
          if n < 0 then math.max(view.length + n, 0) else math.min(n, view.length)
        }
      val data = view.buffer.data
      val base = view.byteOffset
      def matchesAt(i: Int): Boolean = {
        var j = 0
        while j < needle.length do {
          if i + j >= view.length ||
              (data(base + i + j) & 0xff) != (needle(j) & 0xff)
          then return false
          j += 1
        }
        true
      }
      if needle.isEmpty then JSValue.fromInt(math.min(offset, view.length))
      else if fromEnd then {
        var i = math.min(offset, view.length - needle.length)
        while i >= 0 do {
          if matchesAt(i) then return JSValue.fromInt(i)
          i -= 1
        }
        JSValue.fromInt(-1)
      } else {
        var i = offset
        while i <= view.length - needle.length do {
          if matchesAt(i) then return JSValue.fromInt(i)
          i += 1
        }
        JSValue.fromInt(-1)
      }
    }

    installMethod(bufferPrototype, "indexOf", 3)((a, c) => indexOfImpl(false)(a, c))
    installMethod(bufferPrototype, "lastIndexOf", 3)((a, c) => indexOfImpl(true)(a, c))
    installMethod(bufferPrototype, "includes", 2) { (args, callCtx) =>
      given JSContext = callCtx
      JSValue.Bool(indexOfImpl(false)(args, callCtx) != JSValue.fromInt(-1))
    }

    // Numeric readers/writers.
    def readNumber(size: Int, signed: Boolean, littleEndian: Boolean)(
        args: Array[JSValue],
        callCtx: JSContext
    ): JSValue = {
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      val offset =
        if rest.headOption.forall(_ == JSValue.Undefined) then 0
        else NodeHelpers.toNumber(rest(0)).toInt
      if offset < 0 || offset + size > view.length then
        NodeHelpers.throwCoded(
          "RangeError",
          "Attempt to access memory outside buffer bounds",
          "ERR_OUT_OF_RANGE"
        )
      val data = view.buffer.data
      val base = view.byteOffset + offset
      var value: Long = 0
      var i = 0
      while i < size do {
        val shift = if littleEndian then i else size - 1 - i
        value |= (data(base + i).toLong & 0xffL) << (shift * 8)
        i += 1
      }
      if signed then {
        val bits = size * 8
        val signBit = 1L << (bits - 1)
        val signedValue = if (value & signBit) != 0 then value - (1L << bits) else value
        JSValue.fromDouble(signedValue.toDouble)
      } else JSValue.fromDouble(value.toDouble)
    }

    def writeNumber(size: Int, littleEndian: Boolean)(
        args: Array[JSValue],
        callCtx: JSContext
    ): JSValue = {
      given JSContext = callCtx
      val (thisValue, rest) = methodArgs(args)
      val (view, _) = asView(thisValue, rest)
      val value = NodeHelpers.toNumber(rest.headOption.getOrElse(JSValue.fromInt(0))).toLong
      val offset =
        if rest.lift(1).forall(_ == JSValue.Undefined) then 0
        else NodeHelpers.toNumber(rest(1)).toInt
      if offset < 0 || offset + size > view.length then
        NodeHelpers.throwCoded(
          "RangeError",
          "Attempt to access memory outside buffer bounds",
          "ERR_OUT_OF_RANGE"
        )
      val data = view.buffer.data
      val base = view.byteOffset + offset
      var i = 0
      while i < size do {
        val shift = if littleEndian then i else size - 1 - i
        data(base + i) = ((value >> (shift * 8)) & 0xff).toByte
        i += 1
      }
      JSValue.fromInt(offset + size)
    }

    installMethod(bufferPrototype, "readUInt8", 1)((a, c) => readNumber(1, false, false)(a, c))
    installMethod(bufferPrototype, "readInt8", 1)((a, c) => readNumber(1, true, false)(a, c))
    installMethod(bufferPrototype, "readUInt16LE", 1)((a, c) => readNumber(2, false, true)(a, c))
    installMethod(bufferPrototype, "readUInt16BE", 1)((a, c) => readNumber(2, false, false)(a, c))
    installMethod(bufferPrototype, "readInt16LE", 1)((a, c) => readNumber(2, true, true)(a, c))
    installMethod(bufferPrototype, "readInt16BE", 1)((a, c) => readNumber(2, true, false)(a, c))
    installMethod(bufferPrototype, "readUInt32LE", 1)((a, c) => readNumber(4, false, true)(a, c))
    installMethod(bufferPrototype, "readUInt32BE", 1)((a, c) => readNumber(4, false, false)(a, c))
    installMethod(bufferPrototype, "readInt32LE", 1)((a, c) => readNumber(4, true, true)(a, c))
    installMethod(bufferPrototype, "readInt32BE", 1)((a, c) => readNumber(4, true, false)(a, c))
    installMethod(bufferPrototype, "writeUInt8", 2)((a, c) => writeNumber(1, false)(a, c))
    installMethod(bufferPrototype, "writeInt8", 2)((a, c) => writeNumber(1, false)(a, c))
    installMethod(bufferPrototype, "writeUInt16LE", 2)((a, c) => writeNumber(2, true)(a, c))
    installMethod(bufferPrototype, "writeUInt16BE", 2)((a, c) => writeNumber(2, false)(a, c))
    installMethod(bufferPrototype, "writeInt16LE", 2)((a, c) => writeNumber(2, true)(a, c))
    installMethod(bufferPrototype, "writeInt16BE", 2)((a, c) => writeNumber(2, false)(a, c))
    installMethod(bufferPrototype, "writeUInt32LE", 2)((a, c) => writeNumber(4, true)(a, c))
    installMethod(bufferPrototype, "writeUInt32BE", 2)((a, c) => writeNumber(4, false)(a, c))
    installMethod(bufferPrototype, "writeInt32LE", 2)((a, c) => writeNumber(4, true)(a, c))
    installMethod(bufferPrototype, "writeInt32BE", 2)((a, c) => writeNumber(4, false)(a, c))

    // ---- static helpers ----------------------------------------------------

    def toBytes(value: JSValue, encoding: String)(using ctx: JSContext): Array[Byte] =
      value match {
        case JSValue.JSStr(s) => bytesFromString(s, encoding)
        case other =>
          if isBuffer(other) then bytesOf(other).getOrElse(Array.emptyByteArray)
          else
            other match {
              case JSValue.JSArrayVal(arr) =>
                val out = new Array[Byte](arr.getLength)
                var i = 0
                while i < arr.getLength do {
                  out(i) = (NodeHelpers.toNumber(arr.get(i)).toInt & 0xff).toByte
                  i += 1
                }
                out
              case JSValue.Object(obj)
                  if obj.getOwnPropertyRaw("__abStorage").isDefined =>
                obj.getOwnPropertyRaw("__abStorage").get match {
                  case JSValue.Native(storage: ArrayBufferStorage) =>
                    if storage.detached then Array.emptyByteArray
                    else storage.data.clone()
                  case _ => Array.emptyByteArray
                }
              case JSValue.Object(obj)
                  if obj.getOwnPropertyRaw("type").contains(JSValue.fromString("Buffer")) =>
                // Legacy { type: 'Buffer', data: [...] }
                builtinHelpersGet(obj, "data") match {
                  case Some(JSValue.JSArrayVal(arr)) =>
                    val out = new Array[Byte](arr.getLength)
                    var i = 0
                    while i < arr.getLength do {
                      out(i) = (NodeHelpers.toNumber(arr.get(i)).toInt & 0xff).toByte
                      i += 1
                    }
                    out
                  case _ => Array.emptyByteArray
                }
              case _ =>
                NodeHelpers.throwCoded(
                  "TypeError",
                  "The first argument must be of type string or an instance of Buffer, ArrayBuffer, or Array",
                  "ERR_INVALID_ARG_TYPE"
                )
            }
      }

    def builtinHelpersGet(
        obj: JSObject,
        key: String
    )(using ctx: JSContext): Option[JSValue] =
      obj.getOwnProperty(key)

    val fromFn = NativeFunction(
      name = "from",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = stripStatic(args)
        val value = rest.headOption.getOrElse(JSValue.Undefined)
        val encoding =
          if rest.lift(1).forall(_ == JSValue.Undefined) then "utf8"
          else NodeHelpers.toStr(rest(1))
        value match {
          case JSValue.Object(obj)
              if obj.getOwnPropertyRaw("__taView").isDefined &&
                !isBuffer(value) =>
            fromBytes(bytesOf(value).getOrElse(Array.emptyByteArray))
          case JSValue.Object(obj)
              if obj.getOwnPropertyRaw("__abStorage").isDefined =>
            obj.getOwnPropertyRaw("__abStorage").get match {
              case JSValue.Native(storage: ArrayBufferStorage) =>
                if storage.detached then
                  NodeHelpers.throwCoded(
                    "TypeError",
                    "ArrayBuffer is detached",
                    "ERR_INVALID_STATE"
                  )
                newView(storage, 0, storage.byteLength)
              case _ => fromBytes(Array.emptyByteArray)
            }
          case _ => fromBytes(toBytes(value, encoding))
        }
      }
    )

    val allocFn = NativeFunction(
      name = "alloc",
      length = 3,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = stripStatic(args)
        val size =
          if rest.headOption.forall(_ == JSValue.Undefined) then 0
          else NodeHelpers.toNumber(rest(0)).toInt
        if size < 0 then
          NodeHelpers.throwCoded(
            "RangeError",
            "The value of \"size\" is out of range",
            "ERR_OUT_OF_RANGE"
          )
        val result = new Array[Byte](size)
        rest.lift(1).filter(_ != JSValue.Undefined) match {
          case Some(JSValue.JSStr(s)) =>
            val encoding =
              if rest.lift(2).forall(_ == JSValue.Undefined) then "utf8"
              else NodeHelpers.toStr(rest(2))
            val pattern = bytesFromString(s, encoding)
            var i = 0
            while i < size && pattern.nonEmpty do {
              result(i) = pattern(i % pattern.length)
              i += 1
            }
          case Some(other) =>
            java.util.Arrays.fill(result, (NodeHelpers.toNumber(other).toInt & 0xff).toByte)
          case None => ()
        }
        fromBytes(result)
      }
    )

    val allocUnsafeFn = NativeFunction(
      name = "allocUnsafe",
      length = 1,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = stripStatic(args)
        val size =
          if rest.headOption.forall(_ == JSValue.Undefined) then 0
          else NodeHelpers.toNumber(rest(0)).toInt
        fromBytes(new Array[Byte](math.max(0, size)))
      }
    )

    val concatFn = NativeFunction(
      name = "concat",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = stripStatic(args)
        val chunks: Seq[Array[Byte]] = rest.headOption.getOrElse(JSValue.Undefined) match {
          case JSValue.JSArrayVal(arr) =>
            (0 until arr.getLength).toSeq.map(i =>
              bytesOf(arr.get(i)).getOrElse(Array.emptyByteArray)
            )
          case _ =>
            NodeHelpers.throwCoded(
              "TypeError",
              "The \"list\" argument must be an instance of Array",
              "ERR_INVALID_ARG_TYPE"
            )
        }
        val total = rest.lift(1).filter(_ != JSValue.Undefined) match {
          case Some(value) => NodeHelpers.toNumber(value).toInt
          case None        => chunks.map(_.length).sum
        }
        val out = new Array[Byte](math.max(0, total))
        var offset = 0
        chunks.foreach { chunk =>
          val n = math.min(chunk.length, out.length - offset)
          if n > 0 then {
            System.arraycopy(chunk, 0, out, offset, n)
            offset += n
          }
        }
        fromBytes(out)
      }
    )

    val byteLengthFn = NativeFunction(
      name = "byteLength",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = stripStatic(args)
        val value = rest.headOption.getOrElse(JSValue.Undefined)
        val encoding =
          if rest.lift(1).forall(_ == JSValue.Undefined) then "utf8"
          else NodeHelpers.toStr(rest(1))
        JSValue.fromInt(toBytes(value, encoding).length)
      }
    )

    val isBufferFn = NativeFunction(
      name = "isBuffer",
      length = 1,
      impl = (args, callCtx) => {
        val rest = stripStatic(args)
        JSValue.Bool(rest.headOption.exists(isBuffer))
      }
    )

    val compareFn = NativeFunction(
      name = "compare",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = stripStatic(args)
        (rest.headOption.flatMap(viewOf), rest.lift(1).flatMap(viewOf)) match {
          case (Some(x), Some(y)) => JSValue.fromInt(compareViews(x, y))
          case _ =>
            NodeHelpers.throwCoded(
              "TypeError",
              "The arguments must be Buffers",
              "ERR_INVALID_ARG_TYPE"
            )
        }
      }
    )

    def constructFromArgs(args: Array[JSValue])(using ctx: JSContext): JSValue =
      args match {
        case Array(JSValue.Int32(size), _*) =>
          fromBytes(new Array[Byte](math.max(0, size)))
        case Array(JSValue.Float64(size), _*) =>
          fromBytes(new Array[Byte](math.max(0, size.toInt)))
        case Array(JSValue.JSStr(s), _*) =>
          fromBytes(bytesFromString(s, "utf8"))
        case _ => fromBytes(Array.emptyByteArray)
      }

    val bufferCtor = NativeConstructor(
      name = "Buffer",
      callImpl = (args, callCtx) => {
        given JSContext = callCtx
        constructFromArgs(args)
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        constructFromArgs(args)
      },
      prototype = bufferPrototype
    )
    ctorRef = bufferCtor
    BuiltinHelpers.initConstructor(bufferCtor, length = 1)

    def static(name: String, fn: NativeFunction, writable: Boolean = true): Unit =
      bufferCtor.funcObj.defineProperty(
        name,
        JSValue.Native(fn),
        enumerable = false,
        writable = writable,
        configurable = true
      )

    static("from", fromFn)
    static("alloc", allocFn)
    static("allocUnsafe", allocUnsafeFn)
    static("allocUnsafeSlow", allocUnsafeFn)
    static(
      "isEncoding",
      NativeFunction(
        name = "isEncoding",
        length = 1,
        impl = (args, callCtx) => {
          given JSContext = callCtx
          val rest = stripStatic(args)
          val label =
            rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
          val normalized = NodeEncodings.normalize(label)
          JSValue.Bool(
            Set(
              "utf8",
              "utf16le",
              "latin1",
              "ascii",
              "base64",
              "base64url",
              "hex"
            ).contains(normalized)
          )
        }
      )
    )
    static("concat", concatFn)
    static("byteLength", byteLengthFn)
    static("isBuffer", isBufferFn)
    static("compare", compareFn)
    bufferCtor.funcObj.defineProperty(
      "poolSize",
      JSValue.fromInt(poolSize),
      enumerable = false,
      writable = true,
      configurable = true
    )
    bufferCtor.funcObj.defineProperty(
      "BYTES_PER_ELEMENT",
      JSValue.fromInt(1),
      enumerable = false,
      writable = false,
      configurable = false
    )

    JSValue.Native(bufferCtor)
  }
}
