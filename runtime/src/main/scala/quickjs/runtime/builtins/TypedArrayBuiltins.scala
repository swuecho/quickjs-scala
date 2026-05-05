package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject
import scala.collection.mutable

object TypedArrayBuiltins {

  // ============================================================
  // Internal Data Structures
  // ============================================================

  final class ArrayBufferStorage(
      var data: Array[Byte],
      var byteLength: Int
  ) {
    var detached: Boolean = false
    val viewList: mutable.ArrayBuffer[TypedArrayView] = mutable.ArrayBuffer.empty
    var wrapperObject: JSValue | Null = null  // cached wrapper for identity
    def detach(): Unit = { detached = true; data = null; byteLength = 0; wrapperObject = null }
  }
  // Max buffer size: 2^31 - 1 bytes (JVM array limit)
  private val MaxBufferSize: Int = Int.MaxValue

  object ArrayBufferStorage {
    def apply(size: Int): ArrayBufferStorage = {
      if (size < 0 || size > MaxBufferSize)
        throw new RuntimeException(s"Invalid ArrayBuffer size: $size")
      new ArrayBufferStorage(new Array[Byte](size), size)
    }
  }

  final class TypedArrayView(
      val buffer: ArrayBufferStorage,
      val byteOffset: Int,
      val byteLength: Int,
      val elementType: TypedArrayType,
      val length: Int
  ) {
    buffer.viewList += this
    def get(index: Int): JSValue = {
      if (index < 0 || index >= length) throw new RuntimeException("TypedArray index out of bounds")
      if (buffer.detached) throw new RuntimeException("ArrayBuffer is detached")
      elementType.read(buffer.data, byteOffset + index * elementType.bytesPerElement)
    }
    def set(index: Int, value: JSValue): Unit = {
      if (index < 0 || index >= length) throw new RuntimeException("TypedArray index out of bounds")
      if (buffer.detached) throw new RuntimeException("ArrayBuffer is detached")
      val pos = byteOffset + index * elementType.bytesPerElement
      elementType.write(buffer.data, pos, elementType.convert(value))
    }
  }

  sealed abstract class TypedArrayType(
      val name: String, val bytesPerElement: Int, val className: String
  ) {
    def read(data: Array[Byte], pos: Int): JSValue
    def write(data: Array[Byte], pos: Int, value: JSValue): Unit
    def convert(value: JSValue): JSValue
    protected def toInt32(v: JSValue): Int = { val d = v.toNumber; if (d.isNaN || d.isInfinite) 0 else d.toInt }
    protected def toUint32(v: JSValue): Int = { val d = v.toNumber; if (d.isNaN || d.isInfinite) 0 else d.toInt & 0xffffffff }
    protected def toInt64(v: JSValue): Long = v match {
      case JSValue.BigInt(b) => b.longValue()
      case _ => val d = v.toNumber; if (d.isNaN || d.isInfinite) 0L else d.toLong
    }
  }

  object TypedArrayType {
    case object Int8 extends TypedArrayType("Int8", 1, "Int8Array") {
      def read(d: Array[Byte], p: Int) = JSValue.fromInt(d(p))
      def write(d: Array[Byte], p: Int, v: JSValue) = d(p) = toInt32(v).toByte
      override def convert(v: JSValue) = JSValue.fromInt(toInt32(v).toByte.toInt)
    }
    case object Uint8 extends TypedArrayType("Uint8", 1, "Uint8Array") {
      def read(d: Array[Byte], p: Int) = JSValue.fromInt(d(p) & 0xFF)
      def write(d: Array[Byte], p: Int, v: JSValue) = d(p) = (toInt32(v) & 0xFF).toByte
      override def convert(v: JSValue) = JSValue.fromInt(toInt32(v) & 0xFF)
    }
    case object Uint8Clamped extends TypedArrayType("Uint8Clamped", 1, "Uint8ClampedArray") {
      def read(d: Array[Byte], p: Int) = JSValue.fromInt(d(p) & 0xFF)
      def write(d: Array[Byte], p: Int, v: JSValue) = {
        val x = v.toNumber; val c = if (x.isNaN) 0 else if (x < 0) 0 else if (x > 255) 255 else Math.round(x).toInt
        d(p) = c.toByte
      }
      override def convert(v: JSValue) = {
        val x = v.toNumber; val c = if (x.isNaN) 0 else if (x < 0) 0 else if (x > 255) 255 else Math.round(x).toInt
        JSValue.fromInt(c)
      }
    }
    case object Int16 extends TypedArrayType("Int16", 2, "Int16Array") {
      def read(d: Array[Byte], p: Int) = JSValue.fromInt(((d(p + 1) & 0xFF) << 8 | (d(p) & 0xFF)).toShort.toInt)
      def write(d: Array[Byte], p: Int, v: JSValue) = { val x = toInt32(v).toShort; d(p) = (x & 0xFF).toByte; d(p + 1) = ((x >> 8) & 0xFF).toByte }
      override def convert(v: JSValue) = JSValue.fromInt(toInt32(v).toShort.toInt)
    }
    case object Uint16 extends TypedArrayType("Uint16", 2, "Uint16Array") {
      def read(d: Array[Byte], p: Int) = JSValue.fromInt((d(p + 1) & 0xFF) << 8 | (d(p) & 0xFF))
      def write(d: Array[Byte], p: Int, v: JSValue) = { val x = toInt32(v) & 0xFFFF; d(p) = (x & 0xFF).toByte; d(p + 1) = ((x >> 8) & 0xFF).toByte }
      override def convert(v: JSValue) = JSValue.fromInt(toInt32(v) & 0xFFFF)
    }
    case object Int32 extends TypedArrayType("Int32", 4, "Int32Array") {
      def read(d: Array[Byte], p: Int) = JSValue.fromInt(
        (d(p) & 0xFF) | ((d(p + 1) & 0xFF) << 8) | ((d(p + 2) & 0xFF) << 16) | ((d(p + 3) & 0xFF) << 24))
      def write(d: Array[Byte], p: Int, v: JSValue) = {
        val x = toInt32(v); d(p) = (x & 0xFF).toByte; d(p + 1) = ((x >> 8) & 0xFF).toByte
        d(p + 2) = ((x >> 16) & 0xFF).toByte; d(p + 3) = ((x >> 24) & 0xFF).toByte
      }
      override def convert(v: JSValue) = JSValue.fromInt(toInt32(v))
    }
    case object Uint32 extends TypedArrayType("Uint32", 4, "Uint32Array") {
      def read(d: Array[Byte], p: Int) = {
        val v = (d(p) & 0xFFL) | ((d(p + 1) & 0xFFL) << 8) | ((d(p + 2) & 0xFFL) << 16) | ((d(p + 3) & 0xFFL) << 24)
        JSValue.fromDouble(v.toDouble)
      }
      def write(d: Array[Byte], p: Int, v: JSValue) = {
        val x = toUint32(v); d(p) = (x & 0xFF).toByte; d(p + 1) = ((x >> 8) & 0xFF).toByte
        d(p + 2) = ((x >> 16) & 0xFF).toByte; d(p + 3) = ((x >> 24) & 0xFF).toByte
      }
      override def convert(v: JSValue) = JSValue.fromDouble(toUint32(v).toLong.toDouble)
    }
    case object BigInt64 extends TypedArrayType("BigInt64", 8, "BigInt64Array") {
      def read(d: Array[Byte], p: Int) = {
        var v = 0L; for (i <- 0 until 8) v |= (d(p + i) & 0xFFL) << (i * 8)
        JSValue.BigInt(java.math.BigInteger.valueOf(v))
      }
      def write(d: Array[Byte], p: Int, v: JSValue) = {
        val x = toInt64(v); for (i <- 0 until 8) d(p + i) = ((x >> (i * 8)) & 0xFF).toByte
      }
      override def convert(v: JSValue) = v match {
        case JSValue.BigInt(b) => JSValue.BigInt(b)
        case _ => JSValue.BigInt(java.math.BigInteger.valueOf(toInt64(v)))
      }
    }
    case object BigUint64 extends TypedArrayType("BigUint64", 8, "BigUint64Array") {
      def read(d: Array[Byte], p: Int) = {
        var v = 0L; for (i <- 0 until 8) v |= (d(p + i) & 0xFFL) << (i * 8)
        JSValue.BigInt(if (v >= 0) java.math.BigInteger.valueOf(v) else java.math.BigInteger.valueOf(v & Long.MaxValue).setBit(63))
      }
      def write(d: Array[Byte], p: Int, v: JSValue) = {
        val x = toInt64(v); for (i <- 0 until 8) d(p + i) = ((x >> (i * 8)) & 0xFF).toByte
      }
      override def convert(v: JSValue) = v match {
        case JSValue.BigInt(b) => JSValue.BigInt(b)
        case _ => val x = toInt64(v)
          JSValue.BigInt(if (x >= 0) java.math.BigInteger.valueOf(x) else java.math.BigInteger.valueOf(x & Long.MaxValue).setBit(63))
      }
    }
    case object Float32 extends TypedArrayType("Float32", 4, "Float32Array") {
      def read(d: Array[Byte], p: Int) = {
        val bits = (d(p) & 0xFF) | ((d(p + 1) & 0xFF) << 8) | ((d(p + 2) & 0xFF) << 16) | ((d(p + 3) & 0xFF) << 24)
        JSValue.fromDouble(java.lang.Float.intBitsToFloat(bits).toDouble)
      }
      def write(d: Array[Byte], p: Int, v: JSValue) = {
        val bits = java.lang.Float.floatToRawIntBits(v.toNumber.toFloat)
        d(p) = (bits & 0xFF).toByte; d(p + 1) = ((bits >> 8) & 0xFF).toByte; d(p + 2) = ((bits >> 16) & 0xFF).toByte; d(p + 3) = ((bits >> 24) & 0xFF).toByte
      }
      override def convert(v: JSValue) = JSValue.fromDouble(v.toNumber.toFloat.toDouble)
    }
    case object Float64 extends TypedArrayType("Float64", 8, "Float64Array") {
      def read(d: Array[Byte], p: Int) = {
        var bits = 0L; for (i <- 0 until 8) bits |= (d(p + i) & 0xFFL) << (i * 8)
        JSValue.fromDouble(java.lang.Double.longBitsToDouble(bits))
      }
      def write(d: Array[Byte], p: Int, v: JSValue) = {
        val bits = java.lang.Double.doubleToRawLongBits(v.toNumber)
        for (i <- 0 until 8) d(p + i) = ((bits >> (i * 8)) & 0xFF).toByte
      }
      override def convert(v: JSValue) = JSValue.fromDouble(v.toNumber)
    }
    case object Float16 extends TypedArrayType("Float16", 2, "Float16Array") {
      def read(d: Array[Byte], p: Int) = {
        val bits = ((d(p + 1) & 0xFF) << 8) | (d(p) & 0xFF)
        JSValue.fromDouble(float16ToFloat32(bits).toDouble)
      }
      def write(d: Array[Byte], p: Int, v: JSValue) = {
        val bits = float32ToFloat16(v.toNumber.toFloat)
        d(p) = (bits & 0xFF).toByte; d(p + 1) = ((bits >> 8) & 0xFF).toByte
      }
      override def convert(v: JSValue) = JSValue.fromDouble(float16ToFloat32(float32ToFloat16(v.toNumber.toFloat)).toDouble)
    }
    private def float16ToFloat32(bits: Int): Float = {
      val sign = (bits >> 15) & 1; val exp = (bits >> 10) & 0x1F; val mantissa = bits & 0x3FF
      if (exp == 0) { if (mantissa == 0) { if (sign == 1) -0.0f else 0.0f } else { val f = (mantissa.toFloat / 1024.0f) * math.pow(2.0, -14.0).toFloat; if (sign == 1) -f else f } }
      else if (exp == 31) { if (mantissa == 0) { if (sign == 1) Float.NegativeInfinity else Float.PositiveInfinity } else Float.NaN }
      else { java.lang.Float.intBitsToFloat((sign << 31) | ((exp + 112) << 23) | (mantissa << 13)) }
    }
    private def float32ToFloat16(f: Float): Int = {
      val bits = java.lang.Float.floatToRawIntBits(f); val sign = (bits >>> 16) & 0x8000; val exp = (bits >> 23) & 0xFF; val mantissa = bits & 0x7FFFFF
      if (exp == 0) sign else if (exp == 255) { if (mantissa == 0) sign | 0x7C00 else sign | 0x7C00 | (mantissa >> 13) }
      else { val newExp = exp - 127 + 15; if (newExp <= 0) sign else if (newExp >= 31) sign | 0x7C00 else sign | (newExp << 10) | (mantissa >> 13) }
    }
  }

  val AllTypes: Array[TypedArrayType] = Array(
    TypedArrayType.Int8, TypedArrayType.Uint8, TypedArrayType.Uint8Clamped,
    TypedArrayType.Int16, TypedArrayType.Uint16,
    TypedArrayType.Int32, TypedArrayType.Uint32,
    TypedArrayType.BigInt64, TypedArrayType.BigUint64,
    TypedArrayType.Float32, TypedArrayType.Float64, TypedArrayType.Float16
  )

  // ============================================================
  // Access helpers
  // ============================================================

  /** Find the TypedArrayType for a given NativeConstructor by name match. */
  private def findTypeForCtor(ctor: quickjs.value.NativeConstructor): Option[TypedArrayType] =
    AllTypes.find(_.className == ctor.name)

  /** ES ToNumber for objects: calls valueOf then toString, then converts to number.
    * For primitives, delegates to JSValue.toNumber.
    */
  private def toNumberProper(value: JSValue)(using ctx: JSContext): Double =
    value match {
      case JSValue.Object(obj) =>
        // Try valueOf first
        val valueOf = obj.getOwnProperty("valueOf")(using ctx)
        if (valueOf.isDefined) {
          val result = valueOf.get match {
            case JSValue.Native(nf: quickjs.value.NativeFunction) =>
              nf.call(Array(JSValue.Object(obj)))
            case f: JSValue.Function =>
              quickjs.interpreter.Interpreter().call(
                quickjs.runtime.builtins.BuiltinHelpers.functionToBytecode(f),
                JSValue.Object(obj), Array.empty, f.closure)
            case _ => JSValue.Undefined
          }
          result match {
            case _: JSValue.Object => // still object, try toString
            case prim => return prim.toNumber
          }
        }
        // Try toString
        val toStringFn = obj.getOwnProperty("toString")(using ctx)
        if (toStringFn.isDefined) {
          val result = toStringFn.get match {
            case JSValue.Native(nf: quickjs.value.NativeFunction) =>
              nf.call(Array(JSValue.Object(obj)))
            case f: JSValue.Function =>
              quickjs.interpreter.Interpreter().call(
                quickjs.runtime.builtins.BuiltinHelpers.functionToBytecode(f),
                JSValue.Object(obj), Array.empty, f.closure)
            case _ => JSValue.Undefined
          }
          result match {
            case _: JSValue.Object => Double.NaN
            case prim => prim.toNumber
          }
        } else Double.NaN
      case other => other.toNumber
    }

  private def getArrayBufferStorage(obj: JSObject): Option[ArrayBufferStorage] =
    obj.getOwnPropertyRaw("__abStorage").collect { case JSValue.Native(s: ArrayBufferStorage) => s }
  private def getTypedArrayView(obj: JSObject): Option[TypedArrayView] =
    obj.getOwnPropertyRaw("__taView").collect { case JSValue.Native(v: TypedArrayView) => v }
  private def getDataViewView(obj: JSObject): Option[TypedArrayView] =
    obj.getOwnPropertyRaw("__dvStorage").collect { case JSValue.Native(v: TypedArrayView) => v }
  private def getElementType(obj: JSObject): Option[TypedArrayType] =
    obj.getOwnPropertyRaw("__taType").collect { case JSValue.Native(t: TypedArrayType) => t }
  private def getView(obj: JSObject): Option[(TypedArrayView, TypedArrayType)] =
    for { view <- getTypedArrayView(obj); typ <- getElementType(obj) } yield (view, typ)
  private def isTypedArray(obj: JSObject): Boolean = getElementType(obj).isDefined
  private def isDataView(obj: JSObject): Boolean =
    obj.getOwnPropertyRaw("__dvStorage").exists { case JSValue.Native(_: TypedArrayView) => true; case _ => false }
  def isTypedArrayValue(value: JSValue): Boolean = value match {
    case JSValue.Object(obj) => getElementType(obj).isDefined; case _ => false
  }

  private def typedArrayGet(obj: JSObject, index: Int): JSValue =
    getView(obj) match {
      case Some((view, _)) => if (index < 0 || index >= view.length) JSValue.Undefined else view.get(index)
      case None => JSValue.Undefined
    }
  private def typedArraySet(obj: JSObject, index: Int, value: JSValue): Boolean =
    getView(obj) match {
      case Some((view, _)) => if (index < 0 || index >= view.length) false else { view.set(index, value); true }
      case None => false
    }

  private def toNative(fn: NativeFunction): JSValue = JSValue.Native(fn)
  private def toNativeFn(name: String, len: Int = 1)(f: Array[JSValue] => JSContext ?=> JSValue): JSValue =
    JSValue.Native(NativeFunction(name = name, length = len, impl = (args, ctx) => { given JSContext = ctx; f(args)(using ctx) }))
  private def toNativeGetter(name: String)(f: JSValue => JSContext ?=> JSValue): JSValue =
    JSValue.Native(NativeFunction(name = name, length = 0, impl = (args, ctx) => {
      given JSContext = ctx
      val thisVal = if (args.length > 0) args(0) else JSValue.Undefined
      f(thisVal)(using ctx)
    }))

  private def getWellKnownSymbol(name: String)(using ctx: JSContext): JSValue =
    ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc.funcObj.get(name)(using ctx)
      case _ => JSValue.Undefined
    }

  private def findOrCreateBufferObject(storage: ArrayBufferStorage)(using ctx: JSContext): JSValue = {
    if (storage.wrapperObject != null) return storage.wrapperObject.asInstanceOf[JSValue]
    val obj = JSObject(prototype = ctx.arrayBufferPrototype)
    obj.initProperty("__abStorage", JSValue.Native(storage), enumerable = false, writable = false, configurable = false)
    val result = JSValue.Object(obj)
    storage.wrapperObject = result
    result
  }

  // ============================================================
  // Initialization
  // ============================================================

  def initialize(ctx: JSContext): Unit = {
    given JSContext = ctx
    initializeArrayBuffer(ctx)
    initializeTypedArrayBaseObject(ctx)
    initializeDataView(ctx)
    for (typ <- AllTypes) initializeTypedArray(ctx, typ)
  }

  // ---- %TypedArray% intrinsic (shared base for all typed array constructors) ----
  private def initializeTypedArrayBaseObject(ctx: JSContext): Unit = {
    given JSContext = ctx

    // Create the shared TypedArray.prototype first (needed for constructor's prototype)
    val typedArraySharedProto = JSObject(prototype = ctx.objectPrototype)

    // Create %TypedArray% as a NativeConstructor (so IsConstructor returns true).
    // Calling or constructing %TypedArray% directly throws TypeError.
    val typedArrayBaseCtor = quickjs.value.NativeConstructor(
      name = "TypedArray",
      callImpl = (_, ctx) => { given JSContext = ctx; ctx.throwTypeError("TypedArray is not a constructor") },
      constructImpl = (_, ctx) => { given JSContext = ctx; ctx.throwTypeError("TypedArray is not a constructor") },
      prototype = typedArraySharedProto,
      length = 0
    )
    // Set the [[Prototype]] of %TypedArray% to Function.prototype (it is a function object)
    typedArrayBaseCtor.funcObj.setPrototype(ctx.functionPrototype)

    // %TypedArray%.prototype is set by NativeConstructor auto-init (funcObj.initProperty("prototype", ...))

    // Store the funcObj as the base object that individual constructors inherit from
    ctx.typedArrayBaseObject = typedArrayBaseCtor.funcObj

    val typedArrayBase = typedArrayBaseCtor.funcObj

    // ---- Shared prototype accessors (buffer, byteLength, byteOffset, length) ----
    def getThisView(thisVal: JSValue): (TypedArrayView, TypedArrayType) =
      thisVal match {
        case JSValue.Object(obj) => getView(obj) match {
          case Some(vt) => vt
          case None => ctx.throwTypeError("Expected TypedArray")
        }
        case _ => ctx.throwTypeError("Expected TypedArray")
      }

    typedArraySharedProto.initAccessorProperty("buffer",
      getter = Some(toNativeGetter("get buffer") { thisVal =>
        val (view, _) = getThisView(thisVal)
        findOrCreateBufferObject(view.buffer)
      }), setter = None, enumerable = false, configurable = true)

    typedArraySharedProto.initAccessorProperty("byteLength",
      getter = Some(toNativeGetter("get byteLength") { thisVal =>
        val (view, _) = getThisView(thisVal)
        JSValue.fromInt(view.byteLength)
      }), setter = None, enumerable = false, configurable = true)

    typedArraySharedProto.initAccessorProperty("byteOffset",
      getter = Some(toNativeGetter("get byteOffset") { thisVal =>
        val (view, _) = getThisView(thisVal)
        JSValue.fromInt(view.byteOffset)
      }), setter = None, enumerable = false, configurable = true)

    typedArraySharedProto.initAccessorProperty("length",
      getter = Some(toNativeGetter("get length") { thisVal =>
        val (view, _) = getThisView(thisVal)
        JSValue.fromInt(view.length)
      }), setter = None, enumerable = false, configurable = true)

    // Symbol.toStringTag — returns undefined on the base prototype
    // Individual typed array prototypes override this with type-specific names
    getWellKnownSymbol("toStringTag") match {
      case JSValue.Symbol(tagId) =>
        typedArraySharedProto.initSymbolAccessorProperty(
          tagId,
          getter = Some(toNativeGetter("get [Symbol.toStringTag]") { _ => JSValue.Undefined }),
          setter = None,
          enumerable = false, configurable = true
        )
      case _ => ()
    }

    // ---- Shared prototype methods (set, subarray, slice) ----
    typedArraySharedProto.initProperty("set", toNativeFn("set", 2) { args =>
      val (view, _) = getThisView(args(0))
      if (args.length < 2) ctx.throwTypeError("TypedArray.prototype.set requires an argument")
      val source = args(1)
      val targetOffset = if (args.length > 2) args(2).toNumber.toInt else 0
      source match {
        case JSValue.Object(srcObj) => getView(srcObj) match {
          case Some((srcView, _)) =>
            val count = math.min(srcView.length, view.length - targetOffset)
            for (i <- 0 until count) view.set(targetOffset + i, srcView.get(i))
          case None =>
            val srcLen = srcObj.get("length")(using ctx).toNumber.toInt
            val count = math.min(srcLen, view.length - targetOffset)
            for (i <- 0 until count) view.set(targetOffset + i, srcObj.get(i.toString)(using ctx))
        }
        case JSValue.JSArrayVal(srcArr) =>
          val count = math.min(srcArr.getLength, view.length - targetOffset)
          for (i <- 0 until count) view.set(targetOffset + i, srcArr.get(i))
        case _ => ctx.throwTypeError("Invalid source for TypedArray.set")
      }
      JSValue.Undefined
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("subarray", toNativeFn("subarray", 2) { args =>
      val (view, _) = getThisView(args(0))
      val begin = if (args.length > 1) { val b = args(1).toNumber.toInt; if (b < 0) math.max(0, view.length + b) else b } else 0
      val end = if (args.length > 2) { val e = args(2).toNumber.toInt; if (e < 0) math.max(0, view.length + e) else e } else view.length
      val newBegin = math.max(0, math.min(begin, view.length))
      val newLen = math.max(0, math.min(end, view.length) - newBegin)
      // Determine element type from the view
      val elemType = getElementType(args(0) match {
        case JSValue.Object(o) => o
        case _ => ctx.throwTypeError("Expected TypedArray")
      }).getOrElse(ctx.throwTypeError("Expected TypedArray"))
      createTypedArrayView(elemType, view.buffer, view.byteOffset + newBegin * elemType.bytesPerElement, newLen)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("slice", toNativeFn("slice", 2) { args =>
      val (view, _) = getThisView(args(0))
      val begin = if (args.length > 1) { val b = args(1).toNumber.toInt; if (b < 0) math.max(0, view.length + b) else b } else 0
      val end = if (args.length > 2) { val e = args(2).toNumber.toInt; if (e < 0) math.max(0, view.length + e) else e } else view.length
      val newBegin = math.max(0, math.min(begin, view.length))
      val newLen = math.max(0, math.min(end, view.length) - newBegin)
      val elemType = getElementType(args(0) match {
        case JSValue.Object(o) => o
        case _ => ctx.throwTypeError("Expected TypedArray")
      }).getOrElse(ctx.throwTypeError("Expected TypedArray"))
      val byteLen = newLen * elemType.bytesPerElement
      val newStorage = ArrayBufferStorage(byteLen)
      System.arraycopy(view.buffer.data, view.byteOffset + newBegin * elemType.bytesPerElement, newStorage.data, 0, byteLen)
      createTypedArrayFromStorage(elemType, newStorage, 0, byteLen)
    }, enumerable = false, writable = true, configurable = true)

    // Symbol.species — returns `this` (the constructor)
    getWellKnownSymbol("species") match {
      case JSValue.Symbol(speciesId) =>
        typedArrayBase.initSymbolAccessorProperty(
          speciesId,
          getter = Some(toNativeGetter("get [Symbol.species]") { thisVal => thisVal }),
          setter = None,
          enumerable = false, configurable = true
        )
      case _ => ()
    }

    // %TypedArray%.from(source [, mapFn [, thisArg]])
    // args layout: args(0)=this, args(1)=source, args(2)=mapFn, args(3)=thisArg
    typedArrayBase.initProperty("from", toNativeFn("from", 1) { args =>
      val C = if (args.length >= 1) args(0) else JSValue.Undefined
      val source = if (args.length >= 2) args(1) else JSValue.Undefined
      val mapFn = if (args.length >= 3) args(2) else JSValue.Undefined
      val thisArg = if (args.length >= 4) args(3) else JSValue.Undefined

      // 1. If IsConstructor(C) is false, throw TypeError
      val ctor: quickjs.value.NativeConstructor = C match {
        case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc
        case JSValue.Object(obj) =>
          obj.getOwnPropertyRaw("__nativeCtor") match {
            case Some(JSValue.Native(nc: quickjs.value.NativeConstructor)) => nc
            case _ => ctx.throwTypeError("TypedArray.from requires a constructor")
          }
        case _ => ctx.throwTypeError("TypedArray.from requires a constructor")
      }

      // Collect items from the source BEFORE allocating (so getter errors propagate first)
      import scala.collection.mutable.ArrayBuffer
      val items: ArrayBuffer[JSValue] = ArrayBuffer.empty

      // Helper to get a property value calling getters (wraps Interpreter)
      def getPropWithGetter(obj: JSValue, key: String): JSValue =
        obj match {
          case JSValue.Object(o) =>
            o.getOwnPropertyDescriptor(key)(using ctx) match {
              case Some((_, attrs)) if attrs.getter.isDefined =>
                attrs.getter.get match {
                  case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                    nf.call(Array(obj))
                  case f: JSValue.Function =>
                    quickjs.interpreter.Interpreter().call(
                      quickjs.runtime.builtins.BuiltinHelpers.functionToBytecode(f),
                      obj, Array.empty, f.closure)
                  case _ => JSValue.Undefined
                }
              case Some((value, _)) => value
              case None => o.get(key)(using ctx)
            }
          case _ =>
            // Non-object values don't have getters, return undefined for properties
            JSValue.Undefined
        }

      // Helper to get a symbol property with getter
      def getSymbolPropWithGetter(obj: JSValue, symbolId: Int): JSValue =
        obj match {
          case JSValue.Object(o) =>
            o.getOwnSymbolPropertyDescriptor(symbolId)(using ctx) match {
              case Some((_, attrs)) if attrs.getter.isDefined =>
                attrs.getter.get match {
                  case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                    nf.call(Array(obj))
                  case f: JSValue.Function =>
                    quickjs.interpreter.Interpreter().call(
                      quickjs.runtime.builtins.BuiltinHelpers.functionToBytecode(f),
                      obj, Array.empty, f.closure)
                  case _ => JSValue.Undefined
                }
              case Some((value, _)) => value
              case None => o.getSymbol(symbolId)(using ctx)
            }
          case _ => JSValue.Undefined
        }

      // First, try iterator protocol via Symbol.iterator
      getWellKnownSymbol("iterator") match {
        case JSValue.Symbol(iterSymId) =>
          val iteratorMethod = getSymbolPropWithGetter(source, iterSymId)
          if (!iteratorMethod.isUndefined && !iteratorMethod.isNull && iteratorMethod != JSValue.Undefined) {
            // Source is iterable: use iterator
            val iterator = iteratorMethod match {
              case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                nf.call(Array(iteratorMethod, source))
              case f: JSValue.Function =>
                quickjs.interpreter.Interpreter().call(
                  quickjs.runtime.builtins.BuiltinHelpers.functionToBytecode(f),
                  source, Array.empty, f.closure)
              case _ => JSValue.Undefined
            }
            if (!iterator.isUndefined && !iterator.isNull) {
              var done = false
              while (!done) {
                val nextResultVal = getPropWithGetter(iterator, "next") match {
                  case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                    nf.call(Array(iterator, iterator))
                  case f: JSValue.Function =>
                    quickjs.interpreter.Interpreter().call(
                      quickjs.runtime.builtins.BuiltinHelpers.functionToBytecode(f),
                      iterator, Array.empty, f.closure)
                  case _ => JSValue.Undefined
                }
                nextResultVal match {
                  case JSValue.Object(nextObj) =>
                    val doneVal = getPropWithGetter(JSValue.Object(nextObj), "done")
                    done = doneVal.toBoolean
                    if (!done) {
                      items += getPropWithGetter(JSValue.Object(nextObj), "value")
                    }
                  case _ => done = true
                }
              }
            }
          } else {
            // No iterator, fall back to array-like
            source match {
              case JSValue.JSArrayVal(arr) =>
                for (i <- 0 until arr.getLength) items += arr.get(i)
              case JSValue.Object(obj) =>
                val lenVal = getPropWithGetter(JSValue.Object(obj), "length")
                val len = if (lenVal.isUndefined) 0 else toNumberProper(lenVal).toInt
                for (i <- 0 until len) {
                  items += getPropWithGetter(JSValue.Object(obj), i.toString)
                }
              case _ => ()
            }
          }
        case _ =>
          // No Symbol.iterator available, use array-like path
          source match {
            case JSValue.JSArrayVal(arr) =>
              for (i <- 0 until arr.getLength) items += arr.get(i)
            case JSValue.Object(obj) =>
              val lenVal = getPropWithGetter(JSValue.Object(obj), "length")
              val len = if (lenVal.isUndefined) 0 else toNumberProper(lenVal).toInt
              for (i <- 0 until len) {
                items += getPropWithGetter(JSValue.Object(obj), i.toString)
              }
            case _ => ()
          }
      }

      // Apply mapFn if provided
      val mappedItems = if (mapFn.isUndefined) items.toSeq else {
        items.toSeq.zipWithIndex.map { case (item, idx) =>
          mapFn match {
            case JSValue.Native(nf: quickjs.value.NativeFunction) =>
              nf.call(Array(mapFn, item, JSValue.fromInt(idx), thisArg))
            case f: JSValue.Function =>
              quickjs.interpreter.Interpreter().call(
                quickjs.runtime.builtins.BuiltinHelpers.functionToBytecode(f),
                thisArg, Array(item, JSValue.fromInt(idx)), f.closure)
            case _ => item
          }
        }
      }

      // Now allocate and fill the typed array
      findTypeForCtor(ctor) match {
        case Some(typ) =>
          val storage = ArrayBufferStorage(mappedItems.size * typ.bytesPerElement)
          val view = new TypedArrayView(storage, 0, mappedItems.size * typ.bytesPerElement, typ, mappedItems.size)
          for (i <- mappedItems.indices) view.set(i, mappedItems(i))
          createTypedArrayFromView(typ, view)
        case None =>
          ctx.throwTypeError("TypedArray.from: this is not a typed array constructor")
      }
    }, enumerable = false, writable = true, configurable = true)

    // %TypedArray%.of(...items)
    // args layout: args(0)=this, args(1..N)=items
    typedArrayBase.initProperty("of", toNativeFn("of", 0) { args =>
      val C = if (args.length >= 1) args(0) else JSValue.Undefined
      val items = args.drop(1) // items = args(1..)

      // 1. If IsConstructor(C) is false, throw TypeError
      val ctor: quickjs.value.NativeConstructor = C match {
        case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc
        case JSValue.Object(obj) =>
          obj.getOwnPropertyRaw("__nativeCtor") match {
            case Some(JSValue.Native(nc: quickjs.value.NativeConstructor)) => nc
            case _ => ctx.throwTypeError("TypedArray.of requires a constructor")
          }
        case _ => ctx.throwTypeError("TypedArray.of requires a constructor")
      }

      // Find the element type and create the typed array
      val typOpt = findTypeForCtor(ctor)
      typOpt match {
        case Some(typ) =>
          val storage = ArrayBufferStorage(items.length * typ.bytesPerElement)
          val view = new TypedArrayView(storage, 0, items.length * typ.bytesPerElement, typ, items.length)
          for (i <- items.indices) view.set(i, items(i))
          createTypedArrayFromView(typ, view)
        case None =>
          ctx.throwTypeError("TypedArray.of: this is not a typed array constructor")
      }
    }, enumerable = false, writable = true, configurable = true)

    ctx.typedArrayBaseObject = typedArrayBase
  }

  // ---- ArrayBuffer ----
  private def initializeArrayBuffer(ctx: JSContext): Unit = {
    given JSContext = ctx
    val abProto = JSObject(prototype = ctx.objectPrototype)

    val abCtor = quickjs.value.NativeConstructor(
      name = "ArrayBuffer",
      callImpl = (_, ctx) => { given JSContext = ctx; ctx.throwTypeError("Constructor ArrayBuffer requires 'new'") },
      constructImpl = (args, ctx) => {
        given JSContext = ctx
        val length = if (args.nonEmpty) {
          val d = args(0).toNumber
          if (d < 0 || d.isNaN || d.isInfinite || d > Int.MaxValue.toDouble)
            ctx.throwRangeError("Invalid array buffer length")
          d.toInt
        } else 0
        if (length < 0) ctx.throwRangeError("Invalid array buffer length")
        val storage = ArrayBufferStorage(length)
        val obj = JSObject(prototype = abProto)
        obj.initProperty("__abStorage", JSValue.Native(storage), enumerable = false, writable = false, configurable = false)
        JSValue.Object(obj)
      },
      prototype = abProto
    )

    // byteLength getter
    abProto.initAccessorProperty("byteLength",
      getter = Some(toNativeGetter("get byteLength") { thisVal => thisVal match {
        case JSValue.Object(obj) => getArrayBufferStorage(obj) match {
          case Some(s) => JSValue.fromInt(s.byteLength)
          case None => ctx.throwTypeError("Expected ArrayBuffer")
        }
        case _ => ctx.throwTypeError("Expected ArrayBuffer")
      }}), setter = None, enumerable = false, configurable = true)

    // slice method
    abProto.initProperty("slice", toNativeFn("slice", 2) { args => args(0) match {
      case JSValue.Object(obj) => getArrayBufferStorage(obj) match {
        case Some(storage) =>
          if (storage.detached) ctx.throwTypeError("ArrayBuffer is detached")
          val len = storage.byteLength
          val begin = if (args.length > 1) math.max(0, args(1).toNumber.toInt) else 0
          val end = if (args.length > 2) math.min(len, args(2).toNumber.toInt) else len
          val newLen = math.max(0, end - begin)
          val newStorage = ArrayBufferStorage(newLen)
          System.arraycopy(storage.data, begin, newStorage.data, 0, newLen)
          val newObj = JSObject(prototype = ctx.arrayBufferPrototype)
          newObj.initProperty("__abStorage", JSValue.Native(newStorage), enumerable = false, writable = false, configurable = false)
          JSValue.Object(newObj)
        case None => ctx.throwTypeError("Expected ArrayBuffer")
      }
      case _ => ctx.throwTypeError("Expected ArrayBuffer")
    }}, enumerable = false, writable = true, configurable = true)

    // ArrayBuffer.isView static
    abCtor.funcObj.initProperty("isView", toNativeFn("isView", 1) { args =>
      (if (args.length > 1) args(1) else JSValue.Undefined) match {
        case JSValue.Object(obj) => JSValue.Bool(isTypedArray(obj) || isDataView(obj))
        case _ => JSValue.Bool(false)
      }
    }, enumerable = false, writable = true, configurable = true)

    ctx.global.defineProperty("ArrayBuffer", JSValue.Native(abCtor), enumerable = false, writable = true, configurable = true)
    ctx.arrayBufferPrototype = abProto
  }

  // ---- TypedArray ----
  private def initializeTypedArray(ctx: JSContext, typ: TypedArrayType): Unit = {
    given JSContext = ctx

    // Get the shared TypedArray.prototype from %TypedArray%
    val typedArraySharedProto = ctx.typedArrayBaseObject.get("prototype")(using ctx) match {
      case JSValue.Object(obj) => obj
      case _ => ctx.objectPrototype
    }

    val taProto = JSObject(prototype = typedArraySharedProto)

    val taCtor = quickjs.value.NativeConstructor(
      name = typ.className,
      callImpl = (_, ctx) => { given JSContext = ctx; ctx.throwTypeError(s"Constructor ${typ.className} requires 'new'") },
      constructImpl = (args, ctx) => { given JSContext = ctx; constructTypedArray(typ, args) },
      prototype = taProto
    )

    // Set the [[Prototype]] of this constructor to %TypedArray%
    // so that Object.getPrototypeOf(Int8Array) returns %TypedArray%
    taCtor.funcObj.setPrototype(ctx.typedArrayBaseObject)

    // toStringTag symbol — type-specific name (e.g. "Int8Array", "Float64Array")
    // Overrides the base accessor on TypedArray.prototype
    getWellKnownSymbol("toStringTag") match {
      case JSValue.Symbol(id) =>
        val typeName = typ.className
        taProto.initSymbolAccessorProperty(
          id,
          getter = Some(toNativeGetter("get [Symbol.toStringTag]") { _ => JSValue.fromString(typeName) }),
          setter = None,
          enumerable = false, configurable = true
        )
      case _ => ()
    }

    // BYTES_PER_ELEMENT — per-constructor static property
    taCtor.funcObj.initProperty("BYTES_PER_ELEMENT", JSValue.fromInt(typ.bytesPerElement), enumerable = false, writable = false, configurable = true)

    ctx.global.defineProperty(typ.className, JSValue.Native(taCtor), enumerable = false, writable = true, configurable = true)
    ctx.registerTypedArrayPrototype(typ.className, taProto)
  }

  // ---- TypedArray construction helpers ----
  private def constructTypedArray(typ: TypedArrayType, args: Array[JSValue])(using ctx: JSContext): JSValue = {
    if (args.isEmpty) ctx.throwTypeError(s"${typ.className} requires an argument")
    args(0) match {
      case JSValue.Int32(len) =>
        createTypedArrayFromStorage(typ, ArrayBufferStorage(math.max(0, len) * typ.bytesPerElement), 0, math.max(0, len) * typ.bytesPerElement)
      case JSValue.Float64(d) =>
        val len = if (d.isNaN || d.isInfinite) 0 else d.toInt
        createTypedArrayFromStorage(typ, ArrayBufferStorage(math.max(0, len) * typ.bytesPerElement), 0, math.max(0, len) * typ.bytesPerElement)
      case JSValue.Object(obj) if getArrayBufferStorage(obj).isDefined => constructFromBuffer(typ, obj, args)
      case JSValue.Object(obj) if getView(obj).isDefined => getView(obj) match {
        case Some((srcView, _)) =>
          val byteLen = srcView.length * typ.bytesPerElement
          val storage = ArrayBufferStorage(byteLen)
          val view = new TypedArrayView(storage, 0, byteLen, typ, srcView.length)
          for (i <- 0 until srcView.length) view.set(i, srcView.get(i))
          createTypedArrayFromStorage(typ, storage, 0, byteLen)
        case None => ctx.throwTypeError("Invalid arguments")
      }
      case JSValue.Object(obj) =>
        val length = obj.get("length")(using ctx).toNumber.toInt
        val byteLen = length * typ.bytesPerElement
        val storage = ArrayBufferStorage(byteLen)
        val view = new TypedArrayView(storage, 0, byteLen, typ, length)
        for (i <- 0 until length) view.set(i, obj.get(i.toString)(using ctx))
        createTypedArrayFromStorage(typ, storage, 0, byteLen)
      case JSValue.JSArrayVal(arr) =>
        val length = arr.getLength
        val byteLen = length * typ.bytesPerElement
        val storage = ArrayBufferStorage(byteLen)
        val view = new TypedArrayView(storage, 0, byteLen, typ, length)
        for (i <- 0 until length) view.set(i, arr.get(i))
        createTypedArrayFromStorage(typ, storage, 0, byteLen)
      case _ => ctx.throwTypeError(s"Invalid argument for ${typ.className}")
    }
  }

  private def constructFromBuffer(typ: TypedArrayType, bufferObj: JSObject, args: Array[JSValue])(using ctx: JSContext): JSValue = {
    getArrayBufferStorage(bufferObj) match {
      case Some(storage) =>
        if (storage.detached) ctx.throwTypeError("ArrayBuffer is detached")
        val byteOffset = if (args.length > 1) args(1).toNumber.toInt else 0
        if (byteOffset < 0 || byteOffset % typ.bytesPerElement != 0)
          ctx.throwRangeError(s"Invalid byte offset for ${typ.className}")
        val remaining = storage.byteLength - byteOffset
        if (args.length > 2 && args(2) != JSValue.Undefined) {
          val length = args(2).toNumber.toInt
          if (length < 0 || byteOffset + length * typ.bytesPerElement > storage.byteLength)
            ctx.throwRangeError(s"Invalid length for ${typ.className}")
          createTypedArrayView(typ, storage, byteOffset, length)
        } else createTypedArrayView(typ, storage, byteOffset, remaining / typ.bytesPerElement)
      case None => ctx.throwTypeError("Expected ArrayBuffer")
    }
  }

  private def createTypedArrayView(typ: TypedArrayType, buffer: ArrayBufferStorage, byteOffset: Int, length: Int)(using ctx: JSContext): JSValue = {
    val byteLen = length * typ.bytesPerElement
    val view = new TypedArrayView(buffer, byteOffset, byteLen, typ, length)
    createTypedArrayFromView(typ, view)
  }

  private def createTypedArrayFromView(typ: TypedArrayType, view: TypedArrayView)(using ctx: JSContext): JSValue = {
    val proto = ctx.getTypedArrayPrototype(typ.className)
    val obj = JSObject(prototype = proto)
    obj.initProperty("__taView", JSValue.Native(view), enumerable = false, writable = false, configurable = false)
    obj.initProperty("__taType", JSValue.Native(typ), enumerable = false, writable = false, configurable = false)
    JSValue.Object(obj)
  }

  private def createTypedArrayFromStorage(typ: TypedArrayType, buffer: ArrayBufferStorage, byteOffset: Int, byteLength: Int)(using ctx: JSContext): JSValue = {
    val length = byteLength / typ.bytesPerElement
    val view = new TypedArrayView(buffer, byteOffset, byteLength, typ, length)
    createTypedArrayFromView(typ, view)
  }

  // ---- DataView ----
  private def initializeDataView(ctx: JSContext): Unit = {
    given JSContext = ctx
    val dvProto = JSObject(prototype = ctx.objectPrototype)

    val dvCtor = quickjs.value.NativeConstructor(
      name = "DataView",
      callImpl = (_, ctx) => { given JSContext = ctx; ctx.throwTypeError("Constructor DataView requires 'new'") },
      constructImpl = (args, ctx) => {
        given JSContext = ctx
        if (args.isEmpty) ctx.throwTypeError("DataView requires an ArrayBuffer argument")
        args(0) match {
          case JSValue.Object(bufferObj) => getArrayBufferStorage(bufferObj) match {
            case Some(storage) =>
              if (storage.detached) ctx.throwTypeError("ArrayBuffer is detached")
              val byteOffset = if (args.length > 1) args(1).toNumber.toInt else 0
              if (byteOffset < 0 || byteOffset > storage.byteLength) ctx.throwRangeError("Invalid byteOffset for DataView")
              val remaining = storage.byteLength - byteOffset
              val byteLength = if (args.length > 2 && args(2) != JSValue.Undefined) args(2).toNumber.toInt else remaining
              if (byteLength < 0 || byteOffset + byteLength > storage.byteLength) ctx.throwRangeError("Invalid byteLength for DataView")
              val view = new TypedArrayView(storage, byteOffset, byteLength, TypedArrayType.Uint8, byteLength)
              val obj = JSObject(prototype = dvProto)
              obj.initProperty("__dvStorage", JSValue.Native(view), enumerable = false, writable = false, configurable = false)
              JSValue.Object(obj)
            case None => ctx.throwTypeError("Expected ArrayBuffer")
          }
          case _ => ctx.throwTypeError("Expected ArrayBuffer")
        }
      },
      prototype = dvProto
    )

    dvProto.initAccessorProperty("buffer",
      getter = Some(toNativeGetter("get buffer") { thisVal => thisVal match {
        case JSValue.Object(obj) => getDataViewView(obj) match {
          case Some(view) => findOrCreateBufferObject(view.buffer)
          case None => ctx.throwTypeError("Expected DataView")
        }
        case _ => ctx.throwTypeError("Expected DataView")
      }}), setter = None, enumerable = false, configurable = true)

    dvProto.initAccessorProperty("byteLength",
      getter = Some(toNativeGetter("get byteLength") { thisVal => thisVal match {
        case JSValue.Object(obj) => getDataViewView(obj) match {
          case Some(view) => JSValue.fromInt(view.byteLength)
          case None => ctx.throwTypeError("Expected DataView")
        }
        case _ => ctx.throwTypeError("Expected DataView")
      }}), setter = None, enumerable = false, configurable = true)

    dvProto.initAccessorProperty("byteOffset",
      getter = Some(toNativeGetter("get byteOffset") { thisVal => thisVal match {
        case JSValue.Object(obj) => getDataViewView(obj) match {
          case Some(view) => JSValue.fromInt(view.byteOffset)
          case None => ctx.throwTypeError("Expected DataView")
        }
        case _ => ctx.throwTypeError("Expected DataView")
      }}), setter = None, enumerable = false, configurable = true)

    // DataView get methods
    val getMethods = List(
      ("getInt8", 1), ("getUint8", 1), ("getInt16", 2), ("getUint16", 2),
      ("getInt32", 4), ("getUint32", 4), ("getFloat32", 4), ("getFloat64", 8),
      ("getBigInt64", 8), ("getBigUint64", 8)
    )
    for ((name, byteSize) <- getMethods) {
      val fn = toNativeFn(name, 2) { args => args(0) match {
        case JSValue.Object(obj) => getDataViewView(obj) match {
          case Some(view) =>
            if (view.buffer.detached) ctx.throwTypeError("ArrayBuffer is detached")
            if (args.length < 2) ctx.throwTypeError(s"DataView.$name requires a byteOffset argument")
            val offset = args(1).toNumber.toInt
            if (offset < 0 || offset + byteSize > view.byteLength)
              ctx.throwRangeError("Offset is outside the bounds of the DataView")
            val pos = view.byteOffset + offset
            name match {
              case "getInt8" => JSValue.fromInt(view.buffer.data(pos).toInt)
              case "getUint8" => JSValue.fromInt(view.buffer.data(pos) & 0xFF)
              case "getInt16" => JSValue.fromInt(readI16BE(view.buffer.data, pos).toInt)
              case "getUint16" => JSValue.fromInt(readU16BE(view.buffer.data, pos))
              case "getInt32" => JSValue.fromInt(readI32BE(view.buffer.data, pos))
              case "getUint32" => JSValue.fromDouble(readU32BE(view.buffer.data, pos).toDouble)
              case "getFloat32" => JSValue.fromDouble(java.lang.Float.intBitsToFloat(readI32BE(view.buffer.data, pos)).toDouble)
              case "getFloat64" => JSValue.fromDouble(java.lang.Double.longBitsToDouble(readI64BE(view.buffer.data, pos)))
              case "getBigInt64" => JSValue.BigInt(java.math.BigInteger.valueOf(readI64BE(view.buffer.data, pos)))
              case "getBigUint64" =>
                val vv = readI64BE(view.buffer.data, pos)
                JSValue.BigInt(if (vv >= 0) java.math.BigInteger.valueOf(vv) else java.math.BigInteger.valueOf(vv & Long.MaxValue).setBit(63))
              case _ => JSValue.Undefined
            }
          case None => ctx.throwTypeError("Expected DataView")
        }
        case _ => ctx.throwTypeError("Expected DataView")
      }}
      dvProto.initProperty(name, fn, enumerable = false, writable = true, configurable = true)
    }

    // DataView set methods
    val setMethods = List(
      ("setInt8", 1), ("setUint8", 1), ("setInt16", 2), ("setUint16", 2),
      ("setInt32", 4), ("setUint32", 4), ("setFloat32", 4), ("setFloat64", 8),
      ("setBigInt64", 8), ("setBigUint64", 8)
    )
    for ((name, byteSize) <- setMethods) {
      val fn = toNativeFn(name, 3) { args => args(0) match {
        case JSValue.Object(obj) => getDataViewView(obj) match {
          case Some(view) =>
            if (view.buffer.detached) ctx.throwTypeError("ArrayBuffer is detached")
            if (args.length < 2) ctx.throwTypeError(s"DataView.$name requires a byteOffset argument")
            val offset = args(1).toNumber.toInt
            if (offset < 0 || offset + byteSize > view.byteLength)
              ctx.throwRangeError("Offset is outside the bounds of the DataView")
            if (args.length < 3) ctx.throwTypeError(s"DataView.$name requires a value argument")
            val value = args(2)
            val pos = view.byteOffset + offset
            name match {
              case "setInt8" => view.buffer.data(pos) = value.toNumber.toInt.toByte
              case "setUint8" => view.buffer.data(pos) = (value.toNumber.toInt & 0xFF).toByte
              case "setInt16" => writeI16BE(view.buffer.data, pos, value.toNumber.toInt.toShort)
              case "setUint16" => writeU16BE(view.buffer.data, pos, value.toNumber.toInt & 0xFFFF)
              case "setInt32" => writeI32BE(view.buffer.data, pos, value.toNumber.toInt)
              case "setUint32" => writeU32BE(view.buffer.data, pos, value.toNumber.toLong.toInt)
              case "setFloat32" => writeI32BE(view.buffer.data, pos, java.lang.Float.floatToRawIntBits(value.toNumber.toFloat))
              case "setFloat64" => writeI64BE(view.buffer.data, pos, java.lang.Double.doubleToRawLongBits(value.toNumber))
              case "setBigInt64" => writeI64BE(view.buffer.data, pos, value match { case JSValue.BigInt(b) => b.longValue(); case _ => value.toNumber.toLong })
              case "setBigUint64" => writeI64BE(view.buffer.data, pos, value match { case JSValue.BigInt(b) => b.longValue(); case _ => value.toNumber.toLong })
              case _ => ()
            }
            JSValue.Undefined
          case None => ctx.throwTypeError("Expected DataView")
        }
        case _ => ctx.throwTypeError("Expected DataView")
      }}
      dvProto.initProperty(name, fn, enumerable = false, writable = true, configurable = true)
    }

    // toStringTag
    getWellKnownSymbol("toStringTag") match {
      case JSValue.Symbol(id) =>
        dvProto.initSymbolAccessorProperty(
          id,
          getter = Some(toNativeGetter("get [Symbol.toStringTag]") { _ => JSValue.fromString("DataView") }),
          setter = None,
          enumerable = false, configurable = true
        )
      case _ => ()
    }

    ctx.global.defineProperty("DataView", JSValue.Native(dvCtor), enumerable = false, writable = true, configurable = true)
  }

  // ---- Big-endian read/write helpers ----
  private def readI16BE(d: Array[Byte], p: Int): Short = ((d(p) & 0xFF) << 8 | (d(p + 1) & 0xFF)).toShort
  private def readU16BE(d: Array[Byte], p: Int): Int = (d(p) & 0xFF) << 8 | (d(p + 1) & 0xFF)
  private def readI32BE(d: Array[Byte], p: Int): Int = (d(p) & 0xFF) << 24 | (d(p + 1) & 0xFF) << 16 | (d(p + 2) & 0xFF) << 8 | (d(p + 3) & 0xFF)
  private def readU32BE(d: Array[Byte], p: Int): Long = (d(p) & 0xFFL) << 24 | (d(p + 1) & 0xFFL) << 16 | (d(p + 2) & 0xFFL) << 8 | (d(p + 3) & 0xFFL)
  private def readI64BE(d: Array[Byte], p: Int): Long = (d(p) & 0xFFL) << 56 | (d(p + 1) & 0xFFL) << 48 | (d(p + 2) & 0xFFL) << 40 | (d(p + 3) & 0xFFL) << 32 | (d(p + 4) & 0xFFL) << 24 | (d(p + 5) & 0xFFL) << 16 | (d(p + 6) & 0xFFL) << 8 | (d(p + 7) & 0xFFL)
  private def writeI16BE(d: Array[Byte], p: Int, v: Short): Unit = { d(p) = ((v >> 8) & 0xFF).toByte; d(p + 1) = (v & 0xFF).toByte }
  private def writeU16BE(d: Array[Byte], p: Int, v: Int): Unit = { d(p) = ((v >> 8) & 0xFF).toByte; d(p + 1) = (v & 0xFF).toByte }
  private def writeI32BE(d: Array[Byte], p: Int, v: Int): Unit = { d(p) = ((v >> 24) & 0xFF).toByte; d(p + 1) = ((v >> 16) & 0xFF).toByte; d(p + 2) = ((v >> 8) & 0xFF).toByte; d(p + 3) = (v & 0xFF).toByte }
  private def writeU32BE(d: Array[Byte], p: Int, v: Int): Unit = { d(p) = ((v >>> 24) & 0xFF).toByte; d(p + 1) = ((v >>> 16) & 0xFF).toByte; d(p + 2) = ((v >>> 8) & 0xFF).toByte; d(p + 3) = (v & 0xFF).toByte }
  private def writeI64BE(d: Array[Byte], p: Int, v: Long): Unit = { d(p) = ((v >> 56) & 0xFF).toByte; d(p + 1) = ((v >> 48) & 0xFF).toByte; d(p + 2) = ((v >> 40) & 0xFF).toByte; d(p + 3) = ((v >> 32) & 0xFF).toByte; d(p + 4) = ((v >> 24) & 0xFF).toByte; d(p + 5) = ((v >> 16) & 0xFF).toByte; d(p + 6) = ((v >> 8) & 0xFF).toByte; d(p + 7) = (v & 0xFF).toByte }
}
