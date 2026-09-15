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
    def detach(): Unit = { detached = true; data = null; byteLength = 0 }
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
    protected def toInt32(v: JSValue): Int = {
      val d = v.toNumber
      if d.isNaN || d.isInfinite then 0
      else (d % 4294967296.0).toLong.toInt
    }
    protected def toUint32(v: JSValue): Int = {
      val d = v.toNumber
      if d.isNaN || d.isInfinite then 0
      else (d % 4294967296.0).toLong.toInt
    }
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
      private def clamp(v: JSValue): Int = {
        val x = v.toNumber
        if x.isNaN || x <= 0 then 0
        else if x >= 255 then 255
        else {
          val f = math.floor(x)
          val fraction = x - f
          if fraction > 0.5 then f.toInt + 1
          else if fraction < 0.5 then f.toInt
          else {
            val base = f.toInt
            if base % 2 == 0 then base else base + 1
          }
        }
      }
      def write(d: Array[Byte], p: Int, v: JSValue) = {
        d(p) = clamp(v).toByte
      }
      override def convert(v: JSValue) = JSValue.fromInt(clamp(v))
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
        JSValue.fromDouble(BuiltinHelpers.halfBitsToDouble(bits))
      }
      def write(d: Array[Byte], p: Int, v: JSValue) = {
        val bits = BuiltinHelpers.doubleToHalfBits(v.toNumber)
        d(p) = (bits & 0xFF).toByte; d(p + 1) = ((bits >> 8) & 0xFF).toByte
      }
      override def convert(v: JSValue) =
        JSValue.fromDouble(
          BuiltinHelpers.halfBitsToDouble(BuiltinHelpers.doubleToHalfBits(v.toNumber))
        )
    }
    private def float16ToFloat32(bits: Int): Float = {
      val sign = (bits >> 15) & 1; val exp = (bits >> 10) & 0x1F; val mantissa = bits & 0x3FF
      if (exp == 0) { if (mantissa == 0) { if (sign == 1) -0.0f else 0.0f } else { val f = (mantissa.toFloat / 1024.0f) * math.pow(2.0, -14.0).toFloat; if (sign == 1) -f else f } }
      else if (exp == 31) { if (mantissa == 0) { if (sign == 1) Float.NegativeInfinity else Float.PositiveInfinity } else Float.NaN }
      else { java.lang.Float.intBitsToFloat((sign << 31) | ((exp + 112) << 23) | (mantissa << 13)) }
    }
    private def float32ToFloat16(f: Float): Int =
      BuiltinHelpers.doubleToHalfBits(f.toDouble)
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
    BuiltinHelpers.toNumber(value)

  private def getArrayBufferStorage(obj: JSObject): Option[ArrayBufferStorage] =
    obj.getOwnPropertyRaw("__abStorage").collect { case JSValue.Native(s: ArrayBufferStorage) => s }

  def detachArrayBuffer(value: JSValue)(using ctx: JSContext): Unit = value match {
    case JSValue.Object(obj) =>
      getArrayBufferStorage(obj) match {
        case Some(storage) => storage.detach()
        case None          => ctx.throwTypeError("Expected ArrayBuffer")
      }
    case _ => ctx.throwTypeError("Expected ArrayBuffer")
  }
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

  private def canonicalNumericIndexString(key: String): Option[Double] =
    if key == "-0" then Some(-0.0d)
    else if key == "NaN" then Some(Double.NaN)
    else if key == "Infinity" then Some(Double.PositiveInfinity)
    else if key == "-Infinity" then Some(Double.NegativeInfinity)
    else {
      val decimalPattern = "-?(0|[1-9][0-9]*)(\\.[0-9]+)?".r
      key match {
        case decimalPattern(_, _) =>
          try Some(key.toDouble)
          catch case _: NumberFormatException => None
        case _ => None
      }
    }

  private def isNegativeZero(value: Double): Boolean =
    value == 0.0d &&
      java.lang.Double.doubleToRawLongBits(value) ==
        java.lang.Double.doubleToRawLongBits(-0.0d)

  def typedArrayIndexDescriptor(
      obj: JSObject,
      key: String
  )(using JSContext): Option[(JSValue, JSObject.PropertyAttributes)] =
    getView(obj).flatMap { case (view, _) =>
      canonicalNumericIndexString(key) match {
        case Some(index)
            if !index.isNaN && !index.isInfinity && !isNegativeZero(index) &&
              index >= 0.0d && index == math.floor(index) &&
              index <= Int.MaxValue.toDouble && index.toInt < view.length =>
          Some(
            (
              view.get(index.toInt),
              JSObject.PropertyAttributes(
                enumerable = true,
                writable = true,
                configurable = true
              )
            )
          )
        case _ => None
      }
    }

  def typedArrayIndexValue(obj: JSObject, key: String)(using JSContext): Option[JSValue] =
    typedArrayIndexDescriptor(obj, key).map(_._1)

  def typedArrayIndexKeys(obj: JSObject): Option[Seq[String]] =
    getView(obj).map { case (view, _) =>
      if view.buffer.detached then Seq.empty
      else (0 until view.length).map(_.toString)
    }

  def setTypedArrayIndexValue(
      obj: JSObject,
      key: String,
      value: JSValue
  )(using JSContext): Option[Boolean] =
    getView(obj).flatMap { case (view, _) =>
      canonicalNumericIndexString(key).map { index =>
        val validIndex =
          !index.isNaN && !index.isInfinity && !isNegativeZero(index) &&
            index >= 0.0d && index == math.floor(index) &&
            index <= Int.MaxValue.toDouble && index.toInt < view.length
        if validIndex then {
          view.set(index.toInt, value)
          true
        } else false
      }
    }

  def defineTypedArrayIndexProperty(
      obj: JSObject,
      key: String,
      pd: BuiltinHelpers.ParsedDescriptor
  )(using JSContext): Option[Boolean] =
    getView(obj).flatMap { case (view, _) =>
      canonicalNumericIndexString(key).map { index =>
        val validIndex =
          !index.isNaN && !index.isInfinity && !isNegativeZero(index) &&
            index >= 0.0d && index == math.floor(index) &&
            index <= Int.MaxValue.toDouble && index.toInt < view.length
        val validDescriptor =
          !pd.isAccessor &&
            !pd.writable.contains(false) &&
            !pd.enumerable.contains(false) &&
            !pd.configurable.contains(false)

        if !validIndex || !validDescriptor then false
        else {
          pd.value.foreach(value => view.set(index.toInt, value))
          true
        }
      }
    }

  private def typedArrayGet(obj: JSObject, index: Int): JSValue =
    getView(obj) match {
      case Some((view, _)) => if (index < 0 || index >= view.length) JSValue.Undefined else view.get(index)
      case None => JSValue.Undefined
    }

  def indexedElement(obj: JSObject, index: Long): Option[JSValue] =
    getView(obj).flatMap { case (view, _) =>
      Option.when(index >= 0 && index < view.length)(view.get(index.toInt))
    }
  private def typedArraySet(obj: JSObject, index: Int, value: JSValue): Boolean =
    getView(obj) match {
      case Some((view, _)) => if (index < 0 || index >= view.length) false else { view.set(index, value); true }
      case None => false
    }

  private def isConstructorValue(value: JSValue): Boolean =
    value match {
      case func: JSValue.Function => func.isConstructor
      case JSValue.Native(_: quickjs.value.NativeConstructor) => true
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__nativeCtor").exists {
          case JSValue.Native(_: quickjs.value.NativeConstructor) => true
          case _ => false
        }
      case _ => false
    }

  private def constructValue(
      constructorValue: JSValue,
      args: Array[JSValue]
  )(using ctx: JSContext): JSValue =
    constructorValue match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.construct(args)
      case func: JSValue.Function if func.isConstructor =>
        val prototype =
          func.funcObj.get("prototype")(using ctx) match {
            case JSValue.Object(proto) => proto
            case _ => ctx.objectPrototype
          }
        val newObj = JSObject(prototype = prototype, extensible = true)
        val ret = quickjs.interpreter.Interpreter().call(
          BuiltinHelpers.functionToBytecode(func),
          JSValue.Object(newObj),
          args,
          func.closure,
          constructorValue
        )
        ret match {
          case _: JSValue.Object | _: JSValue.Function | _: JSValue.JSArrayVal |
              _: JSValue.Generator | _: JSValue.Promise | _: JSValue.Native |
              _: JSValue.AsyncFunction =>
            ret
          case _ => JSValue.Object(newObj)
        }
      case JSValue.Object(obj) =>
        obj.getOwnPropertyRaw("__nativeCtor") match {
          case Some(JSValue.Native(nc: quickjs.value.NativeConstructor)) =>
            nc.construct(args)
          case _ => ctx.throwTypeError("TypedArray constructor expected")
        }
      case _ =>
        ctx.throwTypeError("TypedArray constructor expected")
    }

  private def createTypedArrayWithConstructor(
      constructorValue: JSValue,
      length: Int
  )(using ctx: JSContext): JSValue =
    createTypedArrayWithConstructorArgs(
      constructorValue,
      Array[JSValue](JSValue.fromInt(length)),
      length
    )

  private def createTypedArrayWithConstructorArgs(
      constructorValue: JSValue,
      args: Array[JSValue],
      requiredLength: Int
  )(using ctx: JSContext): JSValue =
    if !isConstructorValue(constructorValue) then
      ctx.throwTypeError("TypedArray constructor expected")
    val result = constructValue(constructorValue, args)
    result match {
      case JSValue.Object(obj) =>
        getView(obj) match {
          case Some((view, _)) =>
            if view.length < requiredLength then
              ctx.throwTypeError("TypedArray length is too small")
            result
          case None =>
            ctx.throwTypeError("TypedArray constructor result is not a typed array")
        }
      case _ =>
        ctx.throwTypeError("TypedArray constructor result is not a typed array")
    }

  private def getObjectLike(value: JSValue): Option[JSObject] =
    BuiltinHelpers.extractJSObject(value).orElse {
      value match {
        case JSValue.Native(nf: quickjs.value.NativeFunction) => Some(nf.funcObj)
        case _ => None
      }
    }

  private def getPropertyWithGetter(
      target: JSValue,
      key: String,
      receiver: JSValue
  )(using ctx: JSContext): JSValue =
    getObjectLike(target) match {
      case Some(obj) =>
        obj.getPropertyDescriptorWithOwner(key)(using ctx) match {
          case Some((_, _, attrs)) if attrs.getter.isDefined =>
            BuiltinHelpers.callFunctionWithThis(attrs.getter.get, receiver, Array.empty)
          case Some((_, value, _)) => value
          case None => JSValue.Undefined
        }
      case None => JSValue.Undefined
    }

  private def getSymbolPropertyWithGetter(
      target: JSValue,
      symbolId: Int,
      receiver: JSValue
  )(using ctx: JSContext): JSValue =
    getObjectLike(target) match {
      case Some(obj) =>
        obj.getSymbolPropertyDescriptorWithOwner(symbolId)(using ctx) match {
          case Some((_, _, attrs)) if attrs.getter.isDefined =>
            BuiltinHelpers.callFunctionWithThis(attrs.getter.get, receiver, Array.empty)
          case Some((_, value, _)) => value
          case None => JSValue.Undefined
        }
      case None => JSValue.Undefined
    }

  private def createTypedArrayBySpecies(
      source: JSValue,
      defaultType: TypedArrayType,
      length: Int
  )(using ctx: JSContext): JSValue =
    val defaultStorage = () => {
      val byteLen = length * defaultType.bytesPerElement
      val storage = ArrayBufferStorage(byteLen)
      val view = new TypedArrayView(storage, 0, byteLen, defaultType, length)
      createTypedArrayFromView(defaultType, view)
    }

    val constructor = getPropertyWithGetter(source, "constructor", source)
    if constructor == JSValue.Undefined then defaultStorage()
    else if constructor == JSValue.Null || getObjectLike(constructor).isEmpty then
      ctx.throwTypeError("TypedArray constructor property must be an object")
    else {
      val species =
        getWellKnownSymbol("species") match {
          case JSValue.Symbol(speciesId) =>
            getSymbolPropertyWithGetter(constructor, speciesId, constructor)
          case _ => JSValue.Undefined
        }
      if species == JSValue.Undefined || species == JSValue.Null then
        defaultStorage()
      else
        createTypedArrayWithConstructor(species, length)
    }

  private def createTypedArrayBySpeciesWithArgs(
      source: JSValue,
      constructorArgs: Array[JSValue],
      requiredLength: Int,
      defaultResult: => JSValue
  )(using ctx: JSContext): JSValue =
    val constructor = getPropertyWithGetter(source, "constructor", source)
    if constructor == JSValue.Undefined then defaultResult
    else if constructor == JSValue.Null || getObjectLike(constructor).isEmpty then
      ctx.throwTypeError("TypedArray constructor property must be an object")
    else {
      val species =
        getWellKnownSymbol("species") match {
          case JSValue.Symbol(speciesId) =>
            getSymbolPropertyWithGetter(constructor, speciesId, constructor)
          case _ => JSValue.Undefined
        }
      if species == JSValue.Undefined || species == JSValue.Null then
        defaultResult
      else
        createTypedArrayWithConstructorArgs(species, constructorArgs, requiredLength)
    }

  private def toNative(fn: NativeFunction): JSValue = JSValue.Native(fn)
  private def toNativeFn(name: String, len: Int = 1)(f: Array[JSValue] => JSContext ?=> JSValue): JSValue =
    JSValue.Native(NativeFunction(name = name, length = len, impl = (args, ctx) => {
      given JSContext = ctx
      // Method calls pass `this` as args(0). A plain zero-argument call passes
      // nothing, so supply `undefined` as the receiver.
      val effectiveArgs: Array[JSValue] =
        if args.nonEmpty then args else Array(JSValue.Undefined)
      f(effectiveArgs)(using ctx)
    }))
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
    // Reassert DataView's data-valued tag after the shared typed-array
    // prototypes have installed their accessor-valued tags.
    getWellKnownSymbol("toStringTag") match {
      case JSValue.Symbol(id) =>
        ctx.global.get("DataView") match {
          case JSValue.Native(ctor: quickjs.value.NativeConstructor) =>
            ctor.prototype.initSymbolProperty(
              id,
              JSValue.fromString("DataView"),
              enumerable = false,
              writable = false,
              configurable = true
            )
          case _ => ()
        }
      case _ => ()
    }
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
        if view.buffer.detached then JSValue.fromInt(0)
        else JSValue.fromInt(view.byteLength)
      }), setter = None, enumerable = false, configurable = true)

    typedArraySharedProto.initAccessorProperty("byteOffset",
      getter = Some(toNativeGetter("get byteOffset") { thisVal =>
        val (view, _) = getThisView(thisVal)
        if view.buffer.detached then JSValue.fromInt(0)
        else JSValue.fromInt(view.byteOffset)
      }), setter = None, enumerable = false, configurable = true)

    typedArraySharedProto.initAccessorProperty("length",
      getter = Some(toNativeGetter("get length") { thisVal =>
        val (view, _) = getThisView(thisVal)
        if view.buffer.detached then JSValue.fromInt(0)
        else JSValue.fromInt(view.length)
      }), setter = None, enumerable = false, configurable = true)


    val typedArrayValues = toNativeFn("values", 0) { args =>
      getThisView(args(0))
      IteratorBuiltins.createArrayIterator(args(0), "value")
    }
    val typedArrayKeys = toNativeFn("keys", 0) { args =>
      getThisView(args(0))
      IteratorBuiltins.createArrayIterator(args(0), "key")
    }
    val typedArrayEntries = toNativeFn("entries", 0) { args =>
      getThisView(args(0))
      IteratorBuiltins.createArrayIterator(args(0), "entry")
    }
    typedArraySharedProto.initProperty("values", typedArrayValues, enumerable = false, writable = true, configurable = true)
    typedArraySharedProto.initProperty("keys", typedArrayKeys, enumerable = false, writable = true, configurable = true)
    typedArraySharedProto.initProperty("entries", typedArrayEntries, enumerable = false, writable = true, configurable = true)
    getWellKnownSymbol("iterator") match {
      case JSValue.Symbol(sym) =>
        typedArraySharedProto.initSymbolProperty(sym, typedArrayValues, enumerable = false, writable = true, configurable = true)
      case _ => ()
    }

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

    def toIntegerOrInfinity(value: JSValue): Double =
      val n = toNumberProper(value)
      if n.isNaN || n == 0.0 then 0.0
      else if n.isInfinity then n
      else math.signum(n) * math.floor(math.abs(n))

    def relativeIndex(value: JSValue, len: Int, defaultValue: Int): Int =
      if value == JSValue.Undefined then defaultValue
      else
        val n = toIntegerOrInfinity(value)
        if n == Double.NegativeInfinity then 0
        else if n < 0 then math.max(len + n.toInt, 0)
        else math.min(n.toInt, len)

    def sameValueZero(a: JSValue, b: JSValue): Boolean = (a, b) match {
      case (JSValue.Float64(x), JSValue.Float64(y)) if x.isNaN && y.isNaN => true
      case (JSValue.Float64(x), JSValue.Int32(y)) if x.isNaN             => false
      case (JSValue.Int32(x), JSValue.Float64(y)) if y.isNaN             => false
      case (JSValue.Float64(x), JSValue.Int32(y))                        => x == y.toDouble
      case (JSValue.Int32(x), JSValue.Float64(y))                        => x.toDouble == y
      case (JSValue.BigInt(x), JSValue.BigInt(y))                        => x == y
      case _                                                            => a == b
    }

    def strictEquals(a: JSValue, b: JSValue): Boolean = (a, b) match {
      case (JSValue.Float64(x), JSValue.Float64(y)) if x.isNaN || y.isNaN => false
      case (JSValue.Float64(x), JSValue.Int32(y))                        => x == y.toDouble
      case (JSValue.Int32(x), JSValue.Float64(y))                        => x.toDouble == y
      case (JSValue.BigInt(x), JSValue.BigInt(y))                        => x == y
      case _                                                            => a == b
    }

    def isCallable(value: JSValue): Boolean =
      value match {
        case _: JSValue.Function | JSValue.Native(_: quickjs.value.NativeFunction) |
            JSValue.Native(_: quickjs.value.NativeConstructor) =>
          true
        case _ => false
      }

    def typedArrayJoin(view: TypedArrayView, separator: String): JSValue =
      val builder = new StringBuilder
      for i <- 0 until view.length do
        if i > 0 then builder.append(separator)
        val element = view.get(i)
        if element != JSValue.Undefined && element != JSValue.Null then
          builder.append(BuiltinHelpers.toJSString(element))
      JSValue.fromString(builder.toString)

    def copyTypedArray(view: TypedArrayView, typ: TypedArrayType): JSValue =
      val byteLen = view.length * typ.bytesPerElement
      val storage = ArrayBufferStorage(byteLen)
      val copied = new TypedArrayView(storage, 0, byteLen, typ, view.length)
      for i <- 0 until view.length do copied.set(i, view.get(i))
      createTypedArrayFromView(typ, copied)

    def defaultSortCompare(a: JSValue, b: JSValue): Int = (a, b) match {
      case (JSValue.BigInt(x), JSValue.BigInt(y)) => x.compareTo(y)
      case _ =>
        val x = a.toNumber
        val y = b.toNumber
        if x.isNaN then if y.isNaN then 0 else 1
        else if y.isNaN then -1
        else if x < y then -1
        else if x > y then 1
        else if x != 0.0 then 0
        else
          val xNegativeZero = java.lang.Double.doubleToRawLongBits(x) == java.lang.Double.doubleToRawLongBits(-0.0d)
          val yNegativeZero = java.lang.Double.doubleToRawLongBits(y) == java.lang.Double.doubleToRawLongBits(-0.0d)
          if xNegativeZero == yNegativeZero then 0
          else if xNegativeZero then -1
          else 1
    }

    def sortTypedArray(view: TypedArrayView, compareFn: JSValue): Unit =
      if compareFn != JSValue.Undefined && !isCallable(compareFn) then
        ctx.throwTypeError("comparison function must be callable")
      if view.length > 1 then
        val indexedValues = (0 until view.length).map(i => (view.get(i), i)).toArray
        def cmp(left: (JSValue, Int), right: (JSValue, Int)): Int =
          val raw =
            if compareFn == JSValue.Undefined then defaultSortCompare(left._1, right._1)
            else
              val result = BuiltinHelpers.callFunctionWithThis(
                compareFn,
                JSValue.Undefined,
                Array(left._1, right._1)
              ).toNumber
              if result.isNaN then 0 else result.sign.toInt
          if raw == 0 then left._2.compareTo(right._2) else raw
        val sorted = indexedValues.sortWith((a, b) => cmp(a, b) < 0)
        for i <- sorted.indices do view.set(i, sorted(i)._1)

    def requireCallback(args: Array[JSValue], method: String): JSValue =
      val callback = if args.length > 1 then args(1) else JSValue.Undefined
      if !isCallable(callback) then
        ctx.throwTypeError(s"TypedArray.prototype.$method callback must be callable")
      callback

    def callTypedArrayCallback(callback: JSValue, thisArg: JSValue, receiver: JSValue, view: TypedArrayView, index: Int): JSValue =
      BuiltinHelpers.callFunctionWithThis(
        callback,
        thisArg,
        Array(view.get(index), JSValue.fromInt(index), receiver)
      )

    // ---- Shared prototype methods ----
    typedArraySharedProto.initProperty("at", toNativeFn("at", 1) { args =>
      val (view, _) = getThisView(args(0))
      val rawIndex = if args.length > 1 then toIntegerOrInfinity(args(1)) else 0.0
      val index =
        if rawIndex < 0 then view.length + rawIndex.toInt
        else rawIndex.toInt
      if index < 0 || index >= view.length then JSValue.Undefined else view.get(index)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("set", toNativeFn("set", 1) { args =>
      val (view, _) = getThisView(args(0))
      if (args.length < 2) ctx.throwTypeError("TypedArray.prototype.set requires an argument")
      val source = args(1)
      val rawOffset =
        if args.length > 2 then toIntegerOrInfinity(args(2)) else 0.0
      if rawOffset < 0 || rawOffset == Double.PositiveInfinity then
        ctx.throwRangeError("TypedArray.set offset is out of range")
      val targetOffset = rawOffset.toInt
      def ensureFits(length: Int): Unit =
        if targetOffset > view.length || length > view.length - targetOffset then
          ctx.throwRangeError("TypedArray.set source is too large")
      source match {
        case JSValue.Object(srcObj) => getView(srcObj) match {
          case Some((srcView, _)) =>
            ensureFits(srcView.length)
            // Snapshot first because source and target may overlap.
            val values = Array.tabulate(srcView.length)(srcView.get)
            for i <- values.indices do view.set(targetOffset + i, values(i))
          case None =>
            val rawLength = toIntegerOrInfinity(srcObj.get("length")(using ctx))
            val srcLen =
              if rawLength <= 0 then 0
              else math.min(rawLength, Int.MaxValue.toDouble).toInt
            ensureFits(srcLen)
            for i <- 0 until srcLen do
              view.set(targetOffset + i, srcObj.get(i.toString)(using ctx))
        }
        case JSValue.JSArrayVal(srcArr) =>
          ensureFits(srcArr.getLength)
          for i <- 0 until srcArr.getLength do view.set(targetOffset + i, srcArr.get(i))
        case JSValue.Null | JSValue.Undefined =>
          BuiltinHelpers.toObject(source) // throws the required TypeError
        case primitive =>
          BuiltinHelpers.toObject(primitive) match {
            case JSValue.Object(srcObj) =>
              val rawLength = toIntegerOrInfinity(srcObj.get("length")(using ctx))
              val srcLen = if rawLength <= 0 then 0 else rawLength.toInt
              ensureFits(srcLen)
              for i <- 0 until srcLen do
                view.set(targetOffset + i, srcObj.get(i.toString)(using ctx))
            case _ => ()
          }
      }
      JSValue.Undefined
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("copyWithin", toNativeFn("copyWithin", 2) { args =>
      val (view, _) = getThisView(args(0))
      val target = relativeIndex(if args.length > 1 then args(1) else JSValue.Undefined, view.length, 0)
      val start = relativeIndex(if args.length > 2 then args(2) else JSValue.Undefined, view.length, 0)
      val end = relativeIndex(if args.length > 3 then args(3) else JSValue.Undefined, view.length, view.length)
      val count = math.min(end - start, view.length - target)
      if count > 0 then
        val values = (0 until count).map(i => view.get(start + i)).toArray
        for i <- values.indices do view.set(target + i, values(i))
      args(0)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("fill", toNativeFn("fill", 1) { args =>
      val (view, _) = getThisView(args(0))
      val value = if args.length > 1 then args(1) else JSValue.Undefined
      val start = relativeIndex(if args.length > 2 then args(2) else JSValue.Undefined, view.length, 0)
      val end = relativeIndex(if args.length > 3 then args(3) else JSValue.Undefined, view.length, view.length)
      var i = start
      while i < end do
        BuiltinHelpers.checkInterrupted(i)
        view.set(i, value)
        i += 1
      args(0)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("reverse", toNativeFn("reverse", 0) { args =>
      val (view, _) = getThisView(args(0))
      var lower = 0
      var upper = view.length - 1
      while lower < upper do
        BuiltinHelpers.checkInterrupted(lower)
        val lowerValue = view.get(lower)
        val upperValue = view.get(upper)
        view.set(lower, upperValue)
        view.set(upper, lowerValue)
        lower += 1
        upper -= 1
      args(0)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("toReversed", toNativeFn("toReversed", 0) { args =>
      val (view, typ) = getThisView(args(0))
      val copy = copyTypedArray(view, typ)
      copy match {
        case JSValue.Object(copyObj) =>
          val copyView = getTypedArrayView(copyObj).getOrElse(ctx.throwTypeError("Expected TypedArray"))
          var lower = 0
          var upper = copyView.length - 1
          while lower < upper do
            BuiltinHelpers.checkInterrupted(lower)
            val lowerValue = copyView.get(lower)
            val upperValue = copyView.get(upper)
            copyView.set(lower, upperValue)
            copyView.set(upper, lowerValue)
            lower += 1
            upper -= 1
        case _ => ()
      }
      copy
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("forEach", toNativeFn("forEach", 1) { args =>
      val (view, _) = getThisView(args(0))
      val callback = requireCallback(args, "forEach")
      val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
      for i <- 0 until view.length do
        callTypedArrayCallback(callback, thisArg, args(0), view, i)
      JSValue.Undefined
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("every", toNativeFn("every", 1) { args =>
      val (view, _) = getThisView(args(0))
      val callback = requireCallback(args, "every")
      val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
      var i = 0
      var result = true
      while i < view.length && result do
        BuiltinHelpers.checkInterrupted(i)
        result = callTypedArrayCallback(callback, thisArg, args(0), view, i).toBoolean
        i += 1
      JSValue.Bool(result)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("some", toNativeFn("some", 1) { args =>
      val (view, _) = getThisView(args(0))
      val callback = requireCallback(args, "some")
      val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
      var i = 0
      var result = false
      while i < view.length && !result do
        BuiltinHelpers.checkInterrupted(i)
        result = callTypedArrayCallback(callback, thisArg, args(0), view, i).toBoolean
        i += 1
      JSValue.Bool(result)
    }, enumerable = false, writable = true, configurable = true)

    def typedArrayFind(args: Array[JSValue], method: String, reverse: Boolean, returnIndex: Boolean): JSValue =
      val (view, _) = getThisView(args(0))
      val callback = requireCallback(args, method)
      val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
      var i = if reverse then view.length - 1 else 0
      val end = if reverse then -1 else view.length
      val step = if reverse then -1 else 1
      while i != end do
        BuiltinHelpers.checkInterrupted(i)
        val value = view.get(i)
        if BuiltinHelpers.callFunctionWithThis(callback, thisArg, Array(value, JSValue.fromInt(i), args(0))).toBoolean then
          return if returnIndex then JSValue.Int32(i) else value
        i += step
      if returnIndex then JSValue.Int32(-1) else JSValue.Undefined

    typedArraySharedProto.initProperty("find", toNativeFn("find", 1) { args =>
      typedArrayFind(args, "find", reverse = false, returnIndex = false)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("findIndex", toNativeFn("findIndex", 1) { args =>
      typedArrayFind(args, "findIndex", reverse = false, returnIndex = true)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("findLast", toNativeFn("findLast", 1) { args =>
      typedArrayFind(args, "findLast", reverse = true, returnIndex = false)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("findLastIndex", toNativeFn("findLastIndex", 1) { args =>
      typedArrayFind(args, "findLastIndex", reverse = true, returnIndex = true)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("map", toNativeFn("map", 1) { args =>
      val (view, typ) = getThisView(args(0))
      val callback = requireCallback(args, "map")
      val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
      val result = createTypedArrayBySpecies(args(0), typ, view.length)
      val mapped = result match {
        case JSValue.Object(obj) => getTypedArrayView(obj).getOrElse(ctx.throwTypeError("Expected TypedArray"))
        case _ => ctx.throwTypeError("Expected TypedArray")
      }
      for i <- 0 until view.length do
        mapped.set(i, callTypedArrayCallback(callback, thisArg, args(0), view, i))
      result
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("filter", toNativeFn("filter", 1) { args =>
      val (view, typ) = getThisView(args(0))
      val callback = requireCallback(args, "filter")
      val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
      val kept = scala.collection.mutable.ArrayBuffer.empty[JSValue]
      for i <- 0 until view.length do
        val value = view.get(i)
        if BuiltinHelpers.callFunctionWithThis(callback, thisArg, Array(value, JSValue.fromInt(i), args(0))).toBoolean then
          kept += value
      val result = createTypedArrayBySpecies(args(0), typ, kept.length)
      val filtered = result match {
        case JSValue.Object(obj) => getTypedArrayView(obj).getOrElse(ctx.throwTypeError("Expected TypedArray"))
        case _ => ctx.throwTypeError("Expected TypedArray")
      }
      for i <- kept.indices do filtered.set(i, kept(i))
      result
    }, enumerable = false, writable = true, configurable = true)

    def typedArrayReduce(args: Array[JSValue], method: String, reverse: Boolean): JSValue =
      val (view, _) = getThisView(args(0))
      val callback = requireCallback(args, method)
      val hasInitial = args.length > 2
      if view.length == 0 && !hasInitial then
        ctx.throwTypeError("Reduce of empty typed array with no initial value")
      var accumulator =
        if hasInitial then args(2)
        else if reverse then view.get(view.length - 1)
        else view.get(0)
      var i =
        if reverse then (if hasInitial then view.length - 1 else view.length - 2)
        else (if hasInitial then 0 else 1)
      while if reverse then i >= 0 else i < view.length do
        BuiltinHelpers.checkInterrupted(i)
        accumulator = BuiltinHelpers.callFunctionWithThis(
          callback,
          JSValue.Undefined,
          Array(accumulator, view.get(i), JSValue.fromInt(i), args(0))
        )
        if reverse then i -= 1 else i += 1
      accumulator

    typedArraySharedProto.initProperty("reduce", toNativeFn("reduce", 1) { args =>
      typedArrayReduce(args, "reduce", reverse = false)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("reduceRight", toNativeFn("reduceRight", 1) { args =>
      typedArrayReduce(args, "reduceRight", reverse = true)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("sort", toNativeFn("sort", 1) { args =>
      val (view, _) = getThisView(args(0))
      val compareFn = if args.length > 1 then args(1) else JSValue.Undefined
      sortTypedArray(view, compareFn)
      args(0)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("toSorted", toNativeFn("toSorted", 1) { args =>
      val (view, typ) = getThisView(args(0))
      val compareFn = if args.length > 1 then args(1) else JSValue.Undefined
      val copy = copyTypedArray(view, typ)
      copy match {
        case JSValue.Object(copyObj) =>
          val copyView = getTypedArrayView(copyObj).getOrElse(ctx.throwTypeError("Expected TypedArray"))
          sortTypedArray(copyView, compareFn)
        case _ => ()
      }
      copy
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("with", toNativeFn("with", 2) { args =>
      val (view, typ) = getThisView(args(0))
      val rawIndex = if args.length > 1 then toIntegerOrInfinity(args(1)) else 0.0
      val index =
        if rawIndex < 0 then view.length + rawIndex.toInt
        else rawIndex.toInt
      val value = if args.length > 2 then args(2) else JSValue.Undefined
      val convertedValue = typ.convert(value)
      if index < 0 || index >= view.length then
        ctx.throwRangeError("invalid array index")
      val copy = copyTypedArray(view, typ)
      copy match {
        case JSValue.Object(copyObj) =>
          val copyView = getTypedArrayView(copyObj).getOrElse(ctx.throwTypeError("Expected TypedArray"))
          copyView.set(index, convertedValue)
        case _ => ()
      }
      copy
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("subarray", toNativeFn("subarray", 2) { args =>
      val (view, typ) = getThisView(args(0))
      val begin = if (args.length > 1) { val b = args(1).toNumber.toInt; if (b < 0) math.max(0, view.length + b) else b } else 0
      val end = if (args.length > 2) { val e = args(2).toNumber.toInt; if (e < 0) math.max(0, view.length + e) else e } else view.length
      val newBegin = math.max(0, math.min(begin, view.length))
      val newLen = math.max(0, math.min(end, view.length) - newBegin)
      val byteOffset = view.byteOffset + newBegin * typ.bytesPerElement
      val bufferObject = findOrCreateBufferObject(view.buffer)
      createTypedArrayBySpeciesWithArgs(
        args(0),
        Array[JSValue](bufferObject, JSValue.fromInt(byteOffset), JSValue.fromInt(newLen)),
        newLen,
        createTypedArrayView(typ, view.buffer, byteOffset, newLen)
      )
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
      val result = createTypedArrayBySpecies(args(0), elemType, newLen)
      result match {
        case JSValue.Object(obj) =>
          val targetView = getTypedArrayView(obj).getOrElse(ctx.throwTypeError("Expected TypedArray"))
          if getElementType(obj).contains(elemType) then
            System.arraycopy(view.buffer.data, view.byteOffset + newBegin * elemType.bytesPerElement, targetView.buffer.data, targetView.byteOffset, byteLen)
          else
            for i <- 0 until newLen do targetView.set(i, view.get(newBegin + i))
        case _ => ctx.throwTypeError("Expected TypedArray")
      }
      result
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("includes", toNativeFn("includes", 1) { args =>
      val (view, _) = getThisView(args(0))
      val search = if args.length > 1 then args(1) else JSValue.Undefined
      val start = relativeIndex(if args.length > 2 then args(2) else JSValue.Undefined, view.length, 0)
      var found = false
      var i = start
      while i < view.length && !found do
        BuiltinHelpers.checkInterrupted(i)
        found = sameValueZero(view.get(i), search)
        i += 1
      JSValue.Bool(found)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("indexOf", toNativeFn("indexOf", 1) { args =>
      val (view, _) = getThisView(args(0))
      val search = if args.length > 1 then args(1) else JSValue.Undefined
      val start = relativeIndex(if args.length > 2 then args(2) else JSValue.Undefined, view.length, 0)
      var result = -1
      var i = start
      while i < view.length && result < 0 do
        BuiltinHelpers.checkInterrupted(i)
        if strictEquals(view.get(i), search) then result = i
        i += 1
      JSValue.Int32(result)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("lastIndexOf", toNativeFn("lastIndexOf", 1) { args =>
      val (view, _) = getThisView(args(0))
      val search = if args.length > 1 then args(1) else JSValue.Undefined
      val fromIndex =
        if args.length > 2 && args(2) != JSValue.Undefined then
          val n = toIntegerOrInfinity(args(2))
          if n == Double.NegativeInfinity then -1
          else if n < 0 then view.length + n.toInt
          else math.min(n.toInt, view.length - 1)
        else view.length - 1
      var result = -1
      var i = fromIndex
      while i >= 0 && result < 0 do
        BuiltinHelpers.checkInterrupted(i)
        if strictEquals(view.get(i), search) then result = i
        i -= 1
      JSValue.Int32(result)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("join", toNativeFn("join", 1) { args =>
      val (view, _) = getThisView(args(0))
      val separator =
        if args.length > 1 && args(1) != JSValue.Undefined then BuiltinHelpers.toJSString(args(1))
        else ","
      typedArrayJoin(view, separator)
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("toString", toNativeFn("toString", 0) { args =>
      val (view, _) = getThisView(args(0))
      typedArrayJoin(view, ",")
    }, enumerable = false, writable = true, configurable = true)

    typedArraySharedProto.initProperty("toLocaleString", toNativeFn("toLocaleString", 0) { args =>
      val (view, _) = getThisView(args(0))
      val builder = new StringBuilder
      // ES %TypedArray%.prototype.toLocaleString invokes each element's own
      // `toLocaleString` method, which is observable (the method may be
      // overridden on Number.prototype/BigInt.prototype).
      var i = 0
      while i < view.length do {
        if i > 0 then builder.append(",")
        val element = view.get(i)
        if element != JSValue.Undefined && element != JSValue.Null then {
          val boxed = BuiltinHelpers.toObject(element)
          val method =
            BuiltinHelpers.getPropertyWithGetter(boxed, "toLocaleString")
          if !BuiltinHelpers.isCallable(method) then
            ctx.throwTypeError("toLocaleString is not callable")
          val text = BuiltinHelpers.callFunctionWithThis(
            method,
            element,
            Array.empty
          )
          builder.append(BuiltinHelpers.toJSString(text))
        }
        i += 1
      }
      JSValue.fromString(builder.toString)
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

      if (source.isUndefined || source.isNull)
        ctx.throwTypeError("TypedArray.from requires an array-like or iterable object")

      def isCallable(value: JSValue): Boolean =
        value match {
          case _: JSValue.Function | JSValue.Native(_: quickjs.value.NativeFunction) |
              JSValue.Native(_: quickjs.value.NativeConstructor) =>
            true
          case _ => false
        }

      if (!mapFn.isUndefined && !isCallable(mapFn))
        ctx.throwTypeError("TypedArray.from mapper must be callable")

      if !isConstructorValue(C) then
        ctx.throwTypeError("TypedArray.from requires a constructor")

      // Collect items from the source BEFORE allocating (so getter errors propagate first)
      import scala.collection.mutable.ArrayBuffer
      val items: ArrayBuffer[JSValue] = ArrayBuffer.empty

      def callWithThis(funcValue: JSValue, thisValue: JSValue, callArgs: Array[JSValue]): JSValue =
        quickjs.runtime.builtins.BuiltinHelpers.callFunctionWithThis(
          funcValue,
          thisValue,
          callArgs
        )(using ctx)

      // Helper to get a property value calling getters (wraps Interpreter)
      def getPropWithGetter(value: JSValue, key: String): JSValue =
        value match {
          case JSValue.JSArrayVal(arr) =>
            if (key == "length") JSValue.fromInt(arr.getLength)
            else if (key.forall(_.isDigit) && key.nonEmpty) arr.get(key.toInt)
            else ctx.arrayPrototype.get(key)(using ctx)
          case JSValue.JSStr(str) =>
            if (key == "length") JSValue.fromInt(str.length)
            else if (key.forall(_.isDigit) && key.nonEmpty) {
              val index = key.toInt
              if (index >= 0 && index < str.length) JSValue.fromString(str.charAt(index).toString)
              else JSValue.Undefined
            } else JSValue.Undefined
          case JSValue.Object(o) =>
            val numericKey = key.forall(_.isDigit) && key.nonEmpty
            if numericKey then
              getView(o) match {
                case Some((view, _)) =>
                  val index = key.toInt
                  if (index >= 0 && index < view.length) view.get(index)
                  else JSValue.Undefined
                case None =>
                  o.getOwnPropertyDescriptor(key)(using ctx) match {
                    case Some((_, attrs)) if attrs.getter.isDefined =>
                      callWithThis(attrs.getter.get, value, Array.empty)
                    case Some((value, _)) => value
                    case None => o.get(key)(using ctx)
                  }
              }
            else
              o.getOwnPropertyDescriptor(key)(using ctx) match {
                case Some((_, attrs)) if attrs.getter.isDefined =>
                  callWithThis(attrs.getter.get, value, Array.empty)
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
          case JSValue.JSArrayVal(_) =>
            ctx.arrayPrototype.getSymbol(symbolId)(using ctx)
          case JSValue.JSStr(_) =>
            ctx.global.get("String") match {
              case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                nc.prototype.getSymbol(symbolId)(using ctx)
              case _ => JSValue.Undefined
            }
          case JSValue.Object(o) =>
            o.getOwnSymbolPropertyDescriptor(symbolId)(using ctx) match {
              case Some((_, attrs)) if attrs.getter.isDefined =>
                callWithThis(attrs.getter.get, obj, Array.empty)
              case Some((value, _)) => value
              case None => o.getSymbol(symbolId)(using ctx)
            }
          case _ => JSValue.Undefined
        }

      def collectArrayLike(value: JSValue): Unit = {
        val len = getPropWithGetter(value, "length") match {
          case JSValue.Undefined => 0
          case lenVal            => toNumberProper(lenVal).toInt
        }
        for (i <- 0 until math.max(0, len))
          items += getPropWithGetter(value, i.toString)
      }

      // First, try iterator protocol via Symbol.iterator
      getWellKnownSymbol("iterator") match {
        case JSValue.Symbol(iterSymId) =>
          val iteratorMethod = getSymbolPropWithGetter(source, iterSymId)
          if (!iteratorMethod.isUndefined && !iteratorMethod.isNull && iteratorMethod != JSValue.Undefined) {
            if (!isCallable(iteratorMethod))
              ctx.throwTypeError("value is not iterable")
            // Source is iterable: use iterator
            val iterator = callWithThis(iteratorMethod, source, Array.empty)
            if (iterator.isUndefined || iterator.isNull)
              ctx.throwTypeError("iterator method did not return an object")
            val nextMethod = getPropWithGetter(iterator, "next")
            if (!isCallable(nextMethod))
              ctx.throwTypeError("iterator next is not callable")
            var done = false
            while (!done) {
              val nextResultVal = callWithThis(nextMethod, iterator, Array.empty)
              nextResultVal match {
                case JSValue.Object(nextObj) =>
                  val doneVal = getPropWithGetter(JSValue.Object(nextObj), "done")
                  done = doneVal.toBoolean
                  if (!done) {
                    items += getPropWithGetter(JSValue.Object(nextObj), "value")
                  }
                case _ =>
                  ctx.throwTypeError("iterator result is not an object")
                }
              }
          } else {
            // No iterator, fall back to array-like
            collectArrayLike(source)
          }
        case _ =>
          // No Symbol.iterator available, use array-like path
          collectArrayLike(source)
      }

      val result = createTypedArrayWithConstructor(C, items.size)
      result match {
        case JSValue.Object(obj) =>
          val (view, resultType) = getView(obj).get
          for (i <- items.indices) {
            val mappedValue =
              if mapFn.isUndefined then items(i)
              else callWithThis(mapFn, thisArg, Array(items(i), JSValue.fromInt(i)))
            if !view.buffer.detached then {
              val value = resultType match {
                case TypedArrayType.BigInt64 | TypedArrayType.BigUint64 =>
                  BuiltinHelpers.toPrimitiveNumber(mappedValue)
                case _ => JSValue.fromDouble(toNumberProper(mappedValue))
              }
              view.set(i, value)
            }
          }
          result
        case _ =>
          ctx.throwTypeError("TypedArray.from: this is not a typed array constructor")
      }
    }, enumerable = false, writable = true, configurable = true)

    // %TypedArray%.of(...items)
    // args layout: args(0)=this, args(1..N)=items
    typedArrayBase.initProperty("of", toNativeFn("of", 0) { args =>
      val C = if (args.length >= 1) args(0) else JSValue.Undefined
      val items = args.drop(1) // items = args(1..)

      if !isConstructorValue(C) then
        ctx.throwTypeError("TypedArray.of requires a constructor")

      val result = createTypedArrayWithConstructor(C, items.length)
      result match {
        case JSValue.Object(obj) =>
          val (view, _) = getView(obj).get
          for (i <- items.indices) view.set(i, items(i))
          result
        case _ =>
          ctx.throwTypeError("TypedArray.of: this is not a typed array constructor")
      }
    }, enumerable = false, writable = true, configurable = true)

    ctx.typedArrayBaseObject = typedArrayBase
  }

  // ---- ArrayBuffer ----
  private def initializeArrayBuffer(ctx: JSContext): Unit = {
    given JSContext = ctx
    val abProto = JSObject(prototype = ctx.objectPrototype)

    def constructArrayBuffer(
        args: Array[JSValue],
        newTarget: Option[JSValue]
    )(using constructionCtx: JSContext): JSValue = {
      val length =
        if args.nonEmpty then BuiltinHelpers.toIndex(args(0)) else 0L
      val prototype = newTarget.flatMap { target =>
        BuiltinHelpers.getPropertyWithGetter(target, "prototype") match {
          case JSValue.Object(proto) => Some(proto)
          case _                     => None
        }
      }.getOrElse(abProto)
      // GetPrototypeFromConstructor is observable before backing storage is
      // allocated, including when allocation will subsequently fail.
      if length > Int.MaxValue.toLong then
        constructionCtx.throwRangeError("Invalid array buffer length")
      val storage = ArrayBufferStorage(length.toInt)
      val obj = JSObject(prototype = prototype)
      obj.initProperty("__abStorage", JSValue.Native(storage), enumerable = false, writable = false, configurable = false)
      val result = JSValue.Object(obj)
      storage.wrapperObject = result
      result
    }

    val abCtor = quickjs.value.NativeConstructor(
      name = "ArrayBuffer",
      callImpl = (_, ctx) => { given JSContext = ctx; ctx.throwTypeError("Constructor ArrayBuffer requires 'new'") },
      constructImpl = (args, constructCtx) =>
        constructArrayBuffer(args, None)(using constructCtx),
      prototype = abProto,
      constructWithNewTarget = Some((args, newTarget, constructCtx) =>
        constructArrayBuffer(args, Some(newTarget))(using constructCtx)
      ),
      superInitImpl = Some((thisValue, args, initCtx) => {
        given JSContext = initCtx
        thisValue match {
          case JSValue.Object(receiver) =>
            val length: Long = args.headOption match {
              case None => 0L
              case Some(v) =>
                val d = v.toNumber
                if d.isNaN || d <= 0 then 0L
                else if d.isInfinite || d > Int.MaxValue.toDouble then
                  initCtx.throwRangeError("Invalid array buffer length")
                else math.floor(d).toLong
            }
            val storage = ArrayBufferStorage(length.toInt)
            receiver.initProperty(
              "__abStorage",
              JSValue.Native(storage),
              enumerable = false,
              writable = false,
              configurable = false
            )
            val result = JSValue.Object(receiver)
            storage.wrapperObject = result
            result
          case _ =>
            initCtx.throwTypeError("Constructor ArrayBuffer requires 'new'")
        }
      })
    )
    BuiltinHelpers.initConstructor(abCtor, length = 1)

    getWellKnownSymbol("toStringTag") match {
      case JSValue.Symbol(id) =>
        abProto.initSymbolProperty(
          id,
          JSValue.fromString("ArrayBuffer"),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }

    def toIndex(value: JSValue): Int =
      val d = value.toNumber
      if d.isNaN || d <= 0 then 0
      else if d.isInfinite || d > Int.MaxValue.toDouble then
        ctx.throwRangeError("Invalid array buffer length")
      else math.floor(d).toInt

    def createArrayBufferObject(storage: ArrayBufferStorage): JSValue =
      val obj = JSObject(prototype = ctx.arrayBufferPrototype)
      obj.initProperty("__abStorage", JSValue.Native(storage), enumerable = false, writable = false, configurable = false)
      val result = JSValue.Object(obj)
      storage.wrapperObject = result
      result

    // byteLength getter
    abProto.initAccessorProperty("byteLength",
      getter = Some(toNativeGetter("get byteLength") { thisVal => thisVal match {
        case JSValue.Object(obj) => getArrayBufferStorage(obj) match {
          case Some(s) => JSValue.fromInt(s.byteLength)
          case None => ctx.throwTypeError("Expected ArrayBuffer")
        }
        case _ => ctx.throwTypeError("Expected ArrayBuffer")
      }}), setter = None, enumerable = false, configurable = true)

    abProto.initAccessorProperty("detached",
      getter = Some(toNativeGetter("get detached") { thisVal => thisVal match {
        case JSValue.Object(obj) => getArrayBufferStorage(obj) match {
          case Some(s) => JSValue.Bool(s.detached)
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
          createArrayBufferObject(newStorage)
        case None => ctx.throwTypeError("Expected ArrayBuffer")
      }
      case _ => ctx.throwTypeError("Expected ArrayBuffer")
    }}, enumerable = false, writable = true, configurable = true)

    def transferArrayBuffer(args: Array[JSValue]): JSValue =
      args(0) match {
        case JSValue.Object(obj) => getArrayBufferStorage(obj) match {
          case Some(storage) =>
            if storage.detached then ctx.throwTypeError("ArrayBuffer is detached")
            val oldLen = storage.byteLength
            val newLen =
              if args.length > 1 && args(1) != JSValue.Undefined then toIndex(args(1))
              else oldLen
            val newStorage = ArrayBufferStorage(newLen)
            if oldLen > 0 && newLen > 0 then
              System.arraycopy(storage.data, 0, newStorage.data, 0, math.min(oldLen, newLen))
            storage.detach()
            createArrayBufferObject(newStorage)
          case None => ctx.throwTypeError("Expected ArrayBuffer")
        }
        case _ => ctx.throwTypeError("Expected ArrayBuffer")
      }

    abProto.initProperty("transfer", toNativeFn("transfer", 1)(transferArrayBuffer), enumerable = false, writable = true, configurable = true)
    abProto.initProperty("transferToFixedLength", toNativeFn("transferToFixedLength", 1)(transferArrayBuffer), enumerable = false, writable = true, configurable = true)

    // ArrayBuffer.isView static
    abCtor.funcObj.initProperty("isView", toNativeFn("isView", 1) { args =>
      (if (args.length > 1) args(1) else args.headOption.getOrElse(JSValue.Undefined)) match {
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
      callImpl = (_, ctx) => {
        given JSContext = ctx
        ctx.throwTypeError(s"Constructor ${typ.className} requires 'new'")
      },
      constructImpl = (args, ctx) => { given JSContext = ctx; constructTypedArray(typ, args) },
      prototype = taProto,
      superInitImpl = Some((thisValue, args, initCtx) => {
        given JSContext = initCtx
        thisValue match {
          case JSValue.Object(receiver) =>
            constructTypedArray(typ, args) match {
              case JSValue.Object(created) =>
                created.getOwnPropertyRaw("__taView").foreach(v =>
                  receiver.initProperty("__taView", v, enumerable = false, writable = false, configurable = false)
                )
                created.getOwnPropertyRaw("__taType").foreach(v =>
                  receiver.initProperty("__taType", v, enumerable = false, writable = false, configurable = false)
                )
                JSValue.Object(receiver)
              case _ =>
                initCtx.throwTypeError(s"Invalid ${typ.className} construction")
            }
          case _ =>
            initCtx.throwTypeError(s"Constructor ${typ.className} requires 'new'")
        }
      })
    )
    BuiltinHelpers.initConstructor(taCtor, length = 3)

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

    // BYTES_PER_ELEMENT — per-constructor static property (not configurable)
    taCtor.funcObj.initProperty("BYTES_PER_ELEMENT", JSValue.fromInt(typ.bytesPerElement), enumerable = false, writable = false, configurable = false)
    taProto.initProperty("BYTES_PER_ELEMENT", JSValue.fromInt(typ.bytesPerElement), enumerable = false, writable = false, configurable = false)

    ctx.global.defineProperty(typ.className, JSValue.Native(taCtor), enumerable = false, writable = true, configurable = true)
    ctx.registerTypedArrayPrototype(typ.className, taProto)
  }

  // ---- TypedArray construction helpers ----
  private def constructTypedArray(typ: TypedArrayType, args: Array[JSValue])(using ctx: JSContext): JSValue = {
    if args.isEmpty then
      return createTypedArrayFromStorage(typ, ArrayBufferStorage(0), 0, 0)
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
        val iteratorMethod = getWellKnownSymbol("iterator") match {
          case JSValue.Symbol(id) =>
            obj.getSymbolPropertyDescriptorWithOwner(id) match {
              case Some((_, _, attrs)) if attrs.getter.isDefined =>
                BuiltinHelpers.callFunctionWithThis(
                  attrs.getter.get,
                  JSValue.Object(obj),
                  Array.empty
                )
              case Some((_, method, _)) => method
              case None                 => JSValue.Undefined
            }
          case _ => JSValue.Undefined
        }
        val values = scala.collection.mutable.ArrayBuffer.empty[JSValue]
        if iteratorMethod != JSValue.Undefined && iteratorMethod != JSValue.Null then {
          if !BuiltinHelpers.isCallable(iteratorMethod) then
            ctx.throwTypeError("iterator method is not callable")
          val iterator = BuiltinHelpers.callFunctionWithThis(
            iteratorMethod,
            JSValue.Object(obj),
            Array.empty
          )
          var done = false
          while !done do {
            val next = BuiltinHelpers.getPropertyWithGetter(iterator, "next")
            if !BuiltinHelpers.isCallable(next) then
              ctx.throwTypeError("iterator next is not callable")
            val result = BuiltinHelpers.callFunctionWithThis(next, iterator, Array.empty)
            val doneValue = BuiltinHelpers.getPropertyWithGetter(result, "done")
            done = doneValue.toBoolean
            if !done then
              values += BuiltinHelpers.getPropertyWithGetter(result, "value")
          }
        } else {
          val length = math.max(0, BuiltinHelpers.toNumber(
            BuiltinHelpers.getPropertyWithGetter(JSValue.Object(obj), "length")
          ).toInt)
          for i <- 0 until length do
            values += BuiltinHelpers.getPropertyWithGetter(JSValue.Object(obj), i.toString)
        }
        val byteLen = values.length * typ.bytesPerElement
        val storage = ArrayBufferStorage(byteLen)
        val view = new TypedArrayView(storage, 0, byteLen, typ, values.length)
        for i <- values.indices do view.set(i, values(i))
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

    def initializeViewObject(
        receiver: JSObject,
        args: Array[JSValue],
        newTarget: Option[JSValue] = None
    ): JSValue = {
      if args.isEmpty then ctx.throwTypeError("DataView requires an ArrayBuffer argument")
      args(0) match {
        case JSValue.Object(bufferObj) => getArrayBufferStorage(bufferObj) match {
          case Some(storage) =>
            val byteOffset =
              if args.length > 1 then BuiltinHelpers.toIndex(args(1)).toInt else 0
            if storage.detached then ctx.throwTypeError("ArrayBuffer is detached")
            if byteOffset > storage.byteLength then
              ctx.throwRangeError("Invalid byteOffset for DataView")
            val remaining = storage.byteLength - byteOffset
            val byteLength =
              if args.length > 2 && args(2) != JSValue.Undefined then
                BuiltinHelpers.toIndex(args(2)).toInt
              else remaining
            if byteOffset + byteLength > storage.byteLength then
              ctx.throwRangeError("Invalid byteLength for DataView")
            val prototype = newTarget.flatMap { target =>
              BuiltinHelpers.getPropertyWithGetter(target, "prototype") match {
                case JSValue.Object(proto) => Some(proto)
                case _                     => None
              }
            }.getOrElse(dvProto)
            // The prototype lookup can execute arbitrary code, including
            // detaching the buffer, so validate again afterwards.
            if storage.detached then ctx.throwTypeError("ArrayBuffer is detached")
            receiver.setPrototype(prototype)
            val view = new TypedArrayView(
              storage,
              byteOffset,
              byteLength,
              TypedArrayType.Uint8,
              byteLength
            )
            receiver.initProperty(
              "__dvStorage",
              JSValue.Native(view),
              enumerable = false,
              writable = false,
              configurable = false
            )
            JSValue.Object(receiver)
          case None => ctx.throwTypeError("Expected ArrayBuffer")
        }
        case _ => ctx.throwTypeError("Expected ArrayBuffer")
      }
    }

    val dvCtor = quickjs.value.NativeConstructor(
      name = "DataView",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        callCtx.throwTypeError("Constructor DataView requires 'new'")
      },
      constructImpl = (args, constructCtx) => {
        given JSContext = constructCtx
        initializeViewObject(JSObject(prototype = dvProto), args)
      },
      prototype = dvProto,
      constructWithNewTarget = Some((args, newTarget, constructCtx) => {
        given JSContext = constructCtx
        initializeViewObject(
          JSObject(prototype = dvProto),
          args,
          Some(newTarget)
        )
      }),
      superInitImpl = Some((thisValue, args, initCtx) => {
        given JSContext = initCtx
        thisValue match {
          case JSValue.Object(receiver) =>
            initializeViewObject(receiver, args)
          case _ =>
            initCtx.throwTypeError("Constructor DataView requires 'new'")
        }
      })
    )
    BuiltinHelpers.initConstructor(dvCtor, length = 1)

    getWellKnownSymbol("toStringTag") match {
      case JSValue.Symbol(id) =>
        dvProto.initSymbolProperty(
          id,
          JSValue.fromString("DataView"),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }

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
          case Some(view) =>
            if view.buffer.detached then ctx.throwTypeError("ArrayBuffer is detached")
            JSValue.fromInt(view.byteLength)
          case None => ctx.throwTypeError("Expected DataView")
        }
        case _ => ctx.throwTypeError("Expected DataView")
      }}), setter = None, enumerable = false, configurable = true)

    dvProto.initAccessorProperty("byteOffset",
      getter = Some(toNativeGetter("get byteOffset") { thisVal => thisVal match {
        case JSValue.Object(obj) => getDataViewView(obj) match {
          case Some(view) =>
            if view.buffer.detached then ctx.throwTypeError("ArrayBuffer is detached")
            JSValue.fromInt(view.byteOffset)
          case None => ctx.throwTypeError("Expected DataView")
        }
        case _ => ctx.throwTypeError("Expected DataView")
      }}), setter = None, enumerable = false, configurable = true)

    // DataView get methods
    val getMethods = List(
      ("getInt8", 1), ("getUint8", 1), ("getInt16", 2), ("getUint16", 2),
      ("getInt32", 4), ("getUint32", 4), ("getFloat32", 4), ("getFloat64", 8),
      ("getBigInt64", 8), ("getBigUint64", 8), ("getFloat16", 2)
    )
    for ((name, byteSize) <- getMethods) {
      val fn = toNativeFn(name, 1) { args => args(0) match {
        case JSValue.Object(obj) => getDataViewView(obj) match {
          case Some(view) =>
            val offset = BuiltinHelpers.toIndex(
              if args.length > 1 then args(1) else JSValue.Undefined
            ).toInt
            if (view.buffer.detached) ctx.throwTypeError("ArrayBuffer is detached")
            if (offset + byteSize > view.byteLength)
              ctx.throwRangeError("Offset is outside the bounds of the DataView")
            val pos = view.byteOffset + offset
            val littleEndian = args.length > 2 && args(2).toBoolean
            name match {
              case "getInt8" => JSValue.fromInt(view.buffer.data(pos).toInt)
              case "getUint8" => JSValue.fromInt(view.buffer.data(pos) & 0xFF)
              case "getInt16" => JSValue.fromInt((if littleEndian then readU16LE(view.buffer.data, pos) else readU16BE(view.buffer.data, pos)).toShort.toInt)
              case "getUint16" => JSValue.fromInt(if littleEndian then readU16LE(view.buffer.data, pos) else readU16BE(view.buffer.data, pos))
              case "getInt32" => JSValue.fromInt(if littleEndian then readI32LE(view.buffer.data, pos) else readI32BE(view.buffer.data, pos))
              case "getUint32" => JSValue.fromDouble((if littleEndian then readU32LE(view.buffer.data, pos) else readU32BE(view.buffer.data, pos)).toDouble)
              case "getFloat16" =>
                val bits = if littleEndian then readU16LE(view.buffer.data, pos) else readU16BE(view.buffer.data, pos)
                JSValue.fromDouble(TypedArrayType.Float16.read(Array((bits & 0xff).toByte, ((bits >> 8) & 0xff).toByte), 0).toNumber)
              case "getFloat32" => JSValue.fromDouble(java.lang.Float.intBitsToFloat(if littleEndian then readI32LE(view.buffer.data, pos) else readI32BE(view.buffer.data, pos)).toDouble)
              case "getFloat64" => JSValue.fromDouble(java.lang.Double.longBitsToDouble(if littleEndian then readI64LE(view.buffer.data, pos) else readI64BE(view.buffer.data, pos)))
              case "getBigInt64" => JSValue.BigInt(java.math.BigInteger.valueOf(if littleEndian then readI64LE(view.buffer.data, pos) else readI64BE(view.buffer.data, pos)))
              case "getBigUint64" =>
                val vv = if littleEndian then readI64LE(view.buffer.data, pos) else readI64BE(view.buffer.data, pos)
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
      ("setBigInt64", 8), ("setBigUint64", 8), ("setFloat16", 2)
    )
    for ((name, byteSize) <- setMethods) {
      val fn = toNativeFn(name, 2) { args => args(0) match {
        case JSValue.Object(obj) => getDataViewView(obj) match {
          case Some(view) =>
            val offset = BuiltinHelpers.toIndex(
              if args.length > 1 then args(1) else JSValue.Undefined
            ).toInt
            if (view.buffer.detached) ctx.throwTypeError("ArrayBuffer is detached")
            if (offset + byteSize > view.byteLength)
              ctx.throwRangeError("Offset is outside the bounds of the DataView")
            if (args.length < 3) ctx.throwTypeError(s"DataView.$name requires a value argument")
            val value = args(2)
            val pos = view.byteOffset + offset
            val littleEndian = args.length > 3 && args(3).toBoolean
            name match {
              case "setInt8" => view.buffer.data(pos) = value.toNumber.toInt.toByte
              case "setUint8" => view.buffer.data(pos) = (value.toNumber.toInt & 0xFF).toByte
              case "setInt16" => if littleEndian then writeI16LE(view.buffer.data, pos, value.toNumber.toInt.toShort) else writeI16BE(view.buffer.data, pos, value.toNumber.toInt.toShort)
              case "setUint16" => if littleEndian then writeU16LE(view.buffer.data, pos, value.toNumber.toInt & 0xFFFF) else writeU16BE(view.buffer.data, pos, value.toNumber.toInt & 0xFFFF)
              case "setInt32" => if littleEndian then writeI32LE(view.buffer.data, pos, value.toNumber.toInt) else writeI32BE(view.buffer.data, pos, value.toNumber.toInt)
              case "setUint32" => if littleEndian then writeU32LE(view.buffer.data, pos, value.toNumber.toLong.toInt) else writeU32BE(view.buffer.data, pos, value.toNumber.toLong.toInt)
              case "setFloat16" =>
                val tmp = new Array[Byte](2)
                TypedArrayType.Float16.write(tmp, 0, value)
                val bits = (tmp(0) & 0xff) | ((tmp(1) & 0xff) << 8)
                if littleEndian then writeU16LE(view.buffer.data, pos, bits) else writeU16BE(view.buffer.data, pos, bits)
              case "setFloat32" =>
                val bits = java.lang.Float.floatToRawIntBits(value.toNumber.toFloat)
                if littleEndian then writeI32LE(view.buffer.data, pos, bits) else writeI32BE(view.buffer.data, pos, bits)
              case "setFloat64" =>
                val bits = java.lang.Double.doubleToRawLongBits(value.toNumber)
                if littleEndian then writeI64LE(view.buffer.data, pos, bits) else writeI64BE(view.buffer.data, pos, bits)
              case "setBigInt64" =>
                val bits = value match { case JSValue.BigInt(b) => b.longValue(); case _ => value.toNumber.toLong }
                if littleEndian then writeI64LE(view.buffer.data, pos, bits) else writeI64BE(view.buffer.data, pos, bits)
              case "setBigUint64" =>
                val bits = value match { case JSValue.BigInt(b) => b.longValue(); case _ => value.toNumber.toLong }
                if littleEndian then writeI64LE(view.buffer.data, pos, bits) else writeI64BE(view.buffer.data, pos, bits)
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
  private def readU16LE(d: Array[Byte], p: Int): Int = (d(p) & 0xFF) | ((d(p + 1) & 0xFF) << 8)
  private def readI32BE(d: Array[Byte], p: Int): Int = (d(p) & 0xFF) << 24 | (d(p + 1) & 0xFF) << 16 | (d(p + 2) & 0xFF) << 8 | (d(p + 3) & 0xFF)
  private def readI32LE(d: Array[Byte], p: Int): Int = (d(p) & 0xFF) | ((d(p + 1) & 0xFF) << 8) | ((d(p + 2) & 0xFF) << 16) | ((d(p + 3) & 0xFF) << 24)
  private def readU32BE(d: Array[Byte], p: Int): Long = (d(p) & 0xFFL) << 24 | (d(p + 1) & 0xFFL) << 16 | (d(p + 2) & 0xFFL) << 8 | (d(p + 3) & 0xFFL)
  private def readU32LE(d: Array[Byte], p: Int): Long = (d(p) & 0xFFL) | ((d(p + 1) & 0xFFL) << 8) | ((d(p + 2) & 0xFFL) << 16) | ((d(p + 3) & 0xFFL) << 24)
  private def readI64BE(d: Array[Byte], p: Int): Long = (d(p) & 0xFFL) << 56 | (d(p + 1) & 0xFFL) << 48 | (d(p + 2) & 0xFFL) << 40 | (d(p + 3) & 0xFFL) << 32 | (d(p + 4) & 0xFFL) << 24 | (d(p + 5) & 0xFFL) << 16 | (d(p + 6) & 0xFFL) << 8 | (d(p + 7) & 0xFFL)
  private def readI64LE(d: Array[Byte], p: Int): Long = (d(p) & 0xFFL) | ((d(p + 1) & 0xFFL) << 8) | ((d(p + 2) & 0xFFL) << 16) | ((d(p + 3) & 0xFFL) << 24) | ((d(p + 4) & 0xFFL) << 32) | ((d(p + 5) & 0xFFL) << 40) | ((d(p + 6) & 0xFFL) << 48) | ((d(p + 7) & 0xFFL) << 56)
  private def writeI16BE(d: Array[Byte], p: Int, v: Short): Unit = { d(p) = ((v >> 8) & 0xFF).toByte; d(p + 1) = (v & 0xFF).toByte }
  private def writeI16LE(d: Array[Byte], p: Int, v: Short): Unit = { d(p) = (v & 0xFF).toByte; d(p + 1) = ((v >> 8) & 0xFF).toByte }
  private def writeU16BE(d: Array[Byte], p: Int, v: Int): Unit = { d(p) = ((v >> 8) & 0xFF).toByte; d(p + 1) = (v & 0xFF).toByte }
  private def writeU16LE(d: Array[Byte], p: Int, v: Int): Unit = { d(p) = (v & 0xFF).toByte; d(p + 1) = ((v >> 8) & 0xFF).toByte }
  private def writeI32BE(d: Array[Byte], p: Int, v: Int): Unit = { d(p) = ((v >> 24) & 0xFF).toByte; d(p + 1) = ((v >> 16) & 0xFF).toByte; d(p + 2) = ((v >> 8) & 0xFF).toByte; d(p + 3) = (v & 0xFF).toByte }
  private def writeI32LE(d: Array[Byte], p: Int, v: Int): Unit = { d(p) = (v & 0xFF).toByte; d(p + 1) = ((v >> 8) & 0xFF).toByte; d(p + 2) = ((v >> 16) & 0xFF).toByte; d(p + 3) = ((v >> 24) & 0xFF).toByte }
  private def writeU32BE(d: Array[Byte], p: Int, v: Int): Unit = { d(p) = ((v >>> 24) & 0xFF).toByte; d(p + 1) = ((v >>> 16) & 0xFF).toByte; d(p + 2) = ((v >>> 8) & 0xFF).toByte; d(p + 3) = (v & 0xFF).toByte }
  private def writeU32LE(d: Array[Byte], p: Int, v: Int): Unit = { d(p) = (v & 0xFF).toByte; d(p + 1) = ((v >>> 8) & 0xFF).toByte; d(p + 2) = ((v >>> 16) & 0xFF).toByte; d(p + 3) = ((v >>> 24) & 0xFF).toByte }
  private def writeI64BE(d: Array[Byte], p: Int, v: Long): Unit = { d(p) = ((v >> 56) & 0xFF).toByte; d(p + 1) = ((v >> 48) & 0xFF).toByte; d(p + 2) = ((v >> 40) & 0xFF).toByte; d(p + 3) = ((v >> 32) & 0xFF).toByte; d(p + 4) = ((v >> 24) & 0xFF).toByte; d(p + 5) = ((v >> 16) & 0xFF).toByte; d(p + 6) = ((v >> 8) & 0xFF).toByte; d(p + 7) = (v & 0xFF).toByte }
  private def writeI64LE(d: Array[Byte], p: Int, v: Long): Unit = { d(p) = (v & 0xFF).toByte; d(p + 1) = ((v >> 8) & 0xFF).toByte; d(p + 2) = ((v >> 16) & 0xFF).toByte; d(p + 3) = ((v >> 24) & 0xFF).toByte; d(p + 4) = ((v >> 32) & 0xFF).toByte; d(p + 5) = ((v >> 40) & 0xFF).toByte; d(p + 6) = ((v >> 48) & 0xFF).toByte; d(p + 7) = ((v >> 56) & 0xFF).toByte }
}
