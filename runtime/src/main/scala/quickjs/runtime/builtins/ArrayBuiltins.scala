package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers.{
  initConstructor,
  callFunctionValue,
  callFunctionWithThis,
  nativeArgs,
  functionToBytecode,
  extractJSObject,
  getPropertyWithGetter,
  toNumber,
  toIntegerOrInfinity,
  isCallable
}
import scala.util.Sorting

/** Array built-in constructor and prototype methods. */
object ArrayBuiltins {
  import quickjs.objmodel.{JSObject, JSArray}

  // --- Helpers ---

  /** Extract the underlying JSArray from args(0) (the `this` value). Throws
    * RuntimeException on mismatch (existing behavior, should be TypeError).
    */
  private def thisArray(args: Array[JSValue], method: String): JSArray = {
    if args.isEmpty then
      throw new RuntimeException(s"Array.prototype.$method called on non-array")
    args(0) match {
      case JSValue.JSArrayVal(arrVal) => arrVal
      case _                          =>
        throw new RuntimeException(
          s"Array.prototype.$method called on non-array"
        )
    }
  }

  /** ECMAScript Array.prototype methods are intentionally generic.  Keep the
    * receiver as a JSValue so callbacks observe the original value, while the
    * helpers below implement LengthOfArrayLike, HasProperty, and Get.
    */
  private def arrayLikeReceiver(
      args: Array[JSValue],
      method: String
  )(using ctx: JSContext): JSValue =
    if args.isEmpty || args(0) == JSValue.Undefined || args(0) == JSValue.Null then
      ctx.throwTypeError(s"Array.prototype.$method called on null or undefined")
    else args(0)

  private def arrayLikeLength(value: JSValue)(using ctx: JSContext): Int =
    value match {
      case JSValue.JSArrayVal(arr) => arr.getLength
      case JSValue.JSStr(s)        => s.length
      case _                       =>
        val number = extractJSObject(value)
          .map(_ => toNumber(getPropertyWithGetter(value, "length")))
          .getOrElse(0.0)
        if number.isNaN || number <= 0 then 0
        else if number >= Int.MaxValue then Int.MaxValue
        else math.floor(number).toInt
    }

  private def arrayLikeHas(value: JSValue, index: Int)(using ctx: JSContext): Boolean =
    value match {
      case JSValue.JSArrayVal(arr) =>
        arr.hasIndex(index) || ctx.arrayPrototype.hasProperty(index.toString)
      case JSValue.JSStr(s) => index >= 0 && index < s.length
      case _ => extractJSObject(value).exists(_.hasProperty(index.toString))
    }

  private def arrayLikeGet(value: JSValue, index: Int)(using ctx: JSContext): JSValue =
    value match {
      case JSValue.JSArrayVal(arr) if arr.hasIndex(index) =>
        arr.getIndexAttributes(index).flatMap(_.getter) match {
          case Some(getter) => callFunctionWithThis(getter, value, Array.empty)
          case None         => arr.get(index)
        }
      case JSValue.JSArrayVal(_) =>
        ctx.arrayPrototype.getPropertyDescriptorWithOwner(index.toString) match {
          case Some((_, _, attrs)) if attrs.getter.isDefined =>
            callFunctionWithThis(attrs.getter.get, value, Array.empty)
          case Some((_, stored, _)) => stored
          case None                 => JSValue.Undefined
        }
      case JSValue.JSStr(s) if index >= 0 && index < s.length =>
        JSValue.JSStr(s.substring(index, index + 1))
      case _ =>
        extractJSObject(value) match {
          case Some(obj) =>
            obj.getPropertyDescriptorWithOwner(index.toString) match {
              case Some((_, _, attrs)) if attrs.getter.isDefined =>
                callFunctionWithThis(attrs.getter.get, value, Array.empty)
              case Some((_, stored, _)) => stored
              case None                 => JSValue.Undefined
            }
          case None => JSValue.Undefined
        }
    }

  private def requireCallback(value: JSValue)(using ctx: JSContext): JSValue =
    if !isCallable(value) then ctx.throwTypeError("Callback is not a function")
    else value

  /** Native builtin loops must cooperate with the runner/embedding host's
    * thread interruption just like the bytecode dispatch loop does.
    */
  private def checkInterrupted(iteration: Int): Unit =
    if (iteration & 1023) == 0 && Thread.currentThread().isInterrupted then
      throw new InterruptedException("JavaScript execution interrupted")

  /** Iterate an array calling a callback(element, index, array) -> JSValue.
    * Returns a new array with callback results (like map).
    */
  private def iterateMap(
      arr: JSArray,
      callback: JSValue,
      thisArg: JSValue,
      ctx: JSContext
  ): JSArray = {
    val result = JSArray.empty()
    var i = 0
    given JSContext = ctx
    while i < arr.getLength do {
      result.push(
        callFunctionWithThis(
          callback,
          thisArg,
          Array(arr.get(i), JSValue.fromInt(i), JSValue.JSArrayVal(arr))
        )
      )
      i += 1
    }
    result
  }

  /** Iterate testing each element with callback(element, index, array). Returns
    * index of first true, or -1.
    */
  private def iterateFind(
      arr: JSArray,
      callback: JSValue,
      thisArg: JSValue,
      ctx: JSContext
  ): Int = {
    var i = 0
    given JSContext = ctx
    while i < arr.getLength do {
      if callFunctionWithThis(
          callback,
          thisArg,
          Array(arr.get(i), JSValue.fromInt(i), JSValue.JSArrayVal(arr))
        ).toBoolean
      then return i
      i += 1
    }
    -1
  }

  /** Execute callback(element, index, array) for each element (returning
    * nothing).
    */
  private def iterateForEach(
      arr: JSArray,
      callback: JSValue,
      thisArg: JSValue,
      ctx: JSContext
  ): Unit = {
    var i = 0
    given JSContext = ctx
    while i < arr.getLength do {
      callFunctionWithThis(
        callback,
        thisArg,
        Array(arr.get(i), JSValue.fromInt(i), JSValue.JSArrayVal(arr))
      )
      i += 1
    }
  }

  /** Clamp an index to [0, len], with negative values counting from end. */
  private def clampIndex(raw: Int, len: Int): Int =
    if raw < 0 then math.max(len + raw, 0) else math.min(raw, len)

  private def relativeIndex(value: JSValue, len: Int)(using ctx: JSContext): Int = {
    val raw = toIntegerOrInfinity(value)
    if raw == Double.NegativeInfinity then 0
    else if raw < 0 then math.max(len.toLong + raw.toLong, 0L).toInt
    else if raw == Double.PositiveInfinity then len
    else math.min(raw.toLong, len.toLong).toInt
  }

  private def fromIndex(value: JSValue, len: Int)(using ctx: JSContext): Int = {
    val raw = toIntegerOrInfinity(value)
    if raw == Double.PositiveInfinity then len
    else if raw == Double.NegativeInfinity then 0
    else if raw >= 0 then math.min(raw.toLong, len.toLong).toInt
    else math.max(len.toLong + raw.toLong, 0L).toInt
  }

  private def getWellKnownSymbol(name: String)(using ctx: JSContext): JSValue =
    ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.get(name)(using ctx)
      case _ => JSValue.Undefined
    }

  def initializeArrayConstructor(ctx: JSContext): Unit = {
    def buildArray(values: Seq[JSValue]): JSValue = {
      val arr = quickjs.objmodel.JSArray.empty()
      values.foreach(arr.push)
      JSValue.JSArrayVal(arr)
    }

    def buildArrayFromArgs(args: Array[JSValue], offset: Int): JSValue =
      if args.length == offset then buildArray(Seq.empty)
      else if args.length == offset + 1 then
        args(offset) match {
          case JSValue.Int32(i) =>
            if i < 0 then ctx.throwRangeError("Invalid array length")
            JSValue.JSArrayVal(quickjs.objmodel.JSArray(i))
          case JSValue.Float64(d) =>
            if d.isNaN || d.isInfinite || d < 0 || d != math.floor(d) then
              ctx.throwRangeError("Invalid array length")
            else if d > Int.MaxValue then
              ctx.throwRangeError("Invalid array length")
            else JSValue.JSArrayVal(quickjs.objmodel.JSArray(d.toInt))
          case _ =>
            buildArray(Seq(args(offset)))
        }
      else buildArray(args.drop(offset).toSeq)

    val arrayConstructor = quickjs.value.NativeConstructor(
      name = "Array",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        val (thisArg, realArgs) = BuiltinHelpers.nativeArgs(args)
        buildArrayFromArgs(realArgs, 0)
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        buildArrayFromArgs(args, 0)
      ,
      prototype = ctx.arrayPrototype
    )
    given JSContext = ctx
    initConstructor(arrayConstructor, length = 1)
    ctx.global.set("Array", JSValue.Native(arrayConstructor))
    ctx.arrayPrototype.defineProperty(
      "constructor",
      JSValue.Native(arrayConstructor),
      enumerable = false
    )(using ctx)

    val arrayIsArray = NativeFunction(
      name = "isArray",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then JSValue.Bool(false)
        else {
          given JSContext = ctx
          def check(v: JSValue): Boolean = v match {
            case JSValue.JSArrayVal(_) => true
            case JSValue.Object(obj)   =>
              // Check if proxy and unwrap
              obj.getOwnProperty("__proxy_target") match {
                case Some(JSValue.Null) =>
                  ctx.throwTypeError(
                    "Cannot perform 'isArray' on a revoked proxy"
                  )
                case Some(target) => check(target)
                case None         => obj.isArray
              }
            case _ => false
          }
          JSValue.Bool(check(args(offset)))
        }
    )

    val arrayOf = NativeFunction(
      name = "of",
      impl = (args, _) =>
        val offset = if args.length >= 2 then 1 else 0
        val arr = quickjs.objmodel.JSArray.empty()
        var i = offset
        while i < args.length do {
          arr.push(args(i))
          i += 1
        }
        JSValue.JSArrayVal(arr)
    )

    val arrayFrom = NativeFunction(
      name = "from",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then
          ctx.throwTypeError("Array.from requires an array-like or iterable object")
        else {
          val thisValue =
            if offset == 1 then args(0) else JSValue.Undefined
          val source = args(offset)
          source match {
            case JSValue.Null | JSValue.Undefined =>
              ctx.throwTypeError("Array.from requires an array-like or iterable object")
            case _ => ()
          }
          val mapFn =
            if args.length > offset + 1 then Some(args(offset + 1)) else None
          val thisArg =
            if args.length > offset + 2 then args(offset + 2)
            else JSValue.Undefined
          given JSContext = ctx

          def isConstructor(value: JSValue): Boolean =
            value match {
              case func: JSValue.Function => func.isConstructor
              case JSValue.Native(_: quickjs.value.NativeConstructor) => true
              case _ => false
            }

          def constructFromThis(lengthArg: Option[Int]): JSValue =
            if isConstructor(thisValue) then
              val ctorArgs =
                lengthArg
                  .map(len => Array[JSValue](JSValue.fromInt(len)))
                  .getOrElse(Array.empty[JSValue])
              thisValue match {
                case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                  nc.construct(ctorArgs)
                case func: JSValue.Function =>
                  val prototype =
                    func.funcObj.get("prototype")(using ctx) match {
                      case JSValue.Object(proto) => proto
                      case _                     => ctx.objectPrototype
                    }
                  val newObj = JSObject(prototype = prototype, extensible = true)
                  val ret = Interpreter().call(
                    functionToBytecode(func),
                    JSValue.Object(newObj),
                    ctorArgs,
                    func.closure
                  )
                  ret match {
                    case _: JSValue.Object | _: JSValue.Function |
                        _: JSValue.JSArrayVal | _: JSValue.Generator |
                        _: JSValue.Promise | _: JSValue.Native |
                        _: JSValue.AsyncFunction =>
                      ret
                    case _ => JSValue.Object(newObj)
                  }
                case _ => JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
              }
            else
              lengthArg match {
                case Some(len) => JSValue.JSArrayVal(quickjs.objmodel.JSArray(len))
                case None      => JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
              }

          def defineResultIndex(target: JSValue, index: Int, value: JSValue): Unit =
            target match {
              case JSValue.JSArrayVal(arr) =>
                arr.defineIndexProperty(
                  index,
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )
              case JSValue.Object(obj) =>
                obj.defineProperty(
                  index.toString,
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )(using ctx)
              case func: JSValue.Function =>
                func.funcObj.defineProperty(
                  index.toString,
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )(using ctx)
              case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                nc.funcObj.defineProperty(
                  index.toString,
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )(using ctx)
              case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                nf.funcObj.defineProperty(
                  index.toString,
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )(using ctx)
              case _ =>
                ctx.throwTypeError("Array.from constructor result is not an object")
            }

          def setResultLength(target: JSValue, length: Int): Unit =
            target match {
              case JSValue.JSArrayVal(arr) => arr.setLength(length)
              case JSValue.Object(obj) =>
                obj.defineProperty(
                  "length",
                  JSValue.fromInt(length),
                  enumerable = false,
                  writable = true,
                  configurable = false
                )(using ctx)
              case func: JSValue.Function =>
                func.funcObj.defineProperty(
                  "length",
                  JSValue.fromInt(length),
                  enumerable = false,
                  writable = true,
                  configurable = false
                )(using ctx)
              case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                nc.funcObj.defineProperty(
                  "length",
                  JSValue.fromInt(length),
                  enumerable = false,
                  writable = true,
                  configurable = false
                )(using ctx)
              case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                nf.funcObj.defineProperty(
                  "length",
                  JSValue.fromInt(length),
                  enumerable = false,
                  writable = true,
                  configurable = false
                )(using ctx)
              case _ => ()
            }

          def getProperty(value: JSValue, key: JSValue): JSValue =
            value match {
              case JSValue.JSArrayVal(arr) =>
                key match {
                  case JSValue.Symbol(sym) =>
                    ctx.arrayPrototype.getSymbol(sym)(using ctx)
                  case _ =>
                    val keyStr = key.toString
                    if keyStr.forall(_.isDigit) then arr.get(keyStr.toInt)
                    else arr.getOwnProperty(keyStr).getOrElse(
                      ctx.arrayPrototype.get(keyStr)(using ctx)
                    )
                }
              case JSValue.JSStr(str) =>
                key match {
                  case JSValue.Symbol(sym) =>
                    ctx.global.get("String") match {
                      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                        nc.prototype.getSymbol(sym)(using ctx)
                      case _ => JSValue.Undefined
                    }
                  case _ =>
                    val keyStr = key.toString
                    if keyStr == "length" then JSValue.Int32(str.length)
                    else if keyStr.forall(_.isDigit) then {
                      val idx = keyStr.toInt
                      if idx >= 0 && idx < str.length then
                        JSValue.fromString(str.charAt(idx).toString)
                      else JSValue.Undefined
                    }
                    else JSValue.Undefined
                }
              case JSValue.Object(obj) =>
                key match {
                  case JSValue.Symbol(sym) => obj.getSymbol(sym)(using ctx)
                  case _                   => obj.get(key.toString)(using ctx)
                }
              case func: JSValue.Function =>
                key match {
                  case JSValue.Symbol(sym) => func.funcObj.getSymbol(sym)(using ctx)
                  case _                   => func.funcObj.get(key.toString)(using ctx)
                }
              case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                key match {
                  case JSValue.Symbol(sym) => nf.funcObj.getSymbol(sym)(using ctx)
                  case _                   => nf.funcObj.get(key.toString)(using ctx)
                }
              case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                key match {
                  case JSValue.Symbol(sym) => nc.funcObj.getSymbol(sym)(using ctx)
                  case _                   => nc.funcObj.get(key.toString)(using ctx)
                }
              case _ => JSValue.Undefined
            }

          def isCallable(value: JSValue): Boolean =
            value match {
              case _: JSValue.Function | JSValue.Native(_: NativeFunction) |
                  JSValue.Native(_: quickjs.value.NativeConstructor) =>
                true
              case _ => false
            }

          def iteratorClose(iterator: JSValue): Unit =
            try {
              val returnMethod = getProperty(iterator, JSValue.fromString("return"))
              if returnMethod != JSValue.Undefined then
                callFunctionWithThis(returnMethod, iterator, Array.empty)
            }
            catch {
              case _: Exception => ()
            }

          def pushValue(target: JSValue, value: JSValue, index: Int): Unit = {
            val mapped =
              mapFn match {
                case Some(func) if func != JSValue.Undefined =>
                  if !isCallable(func) then
                    ctx.throwTypeError("Array.from mapper must be callable")
                  callFunctionWithThis(
                    func,
                    thisArg,
                    Array(value, JSValue.fromInt(index))
                  )
                case None => value
                case _    => value
              }
            defineResultIndex(target, index, mapped)
          }

          val iteratorMethod = getWellKnownSymbol("iterator") match {
            case JSValue.Symbol(sym) => getProperty(source, JSValue.Symbol(sym))
            case _                   => JSValue.Undefined
          }

          if iteratorMethod != JSValue.Undefined && iteratorMethod != JSValue.Null
          then {
            if !isCallable(iteratorMethod) then
              ctx.throwTypeError("value is not iterable")
            val result = constructFromThis(None)
            val iterator =
              callFunctionWithThis(iteratorMethod, source, Array.empty)
            val nextMethod = getProperty(iterator, JSValue.fromString("next"))
            if !isCallable(nextMethod) then
              ctx.throwTypeError("iterator next is not callable")
            var index = 0
            try {
              var done = false
              while !done do {
                val nextResult =
                  callFunctionWithThis(nextMethod, iterator, Array.empty)
                nextResult match {
                  case JSValue.Object(_) =>
                    if getProperty(nextResult, JSValue.fromString("done")).toBoolean
                    then done = true
                    else {
                      val value =
                        getProperty(nextResult, JSValue.fromString("value"))
                      pushValue(result, value, index)
                      index += 1
                    }
                  case _ =>
                    iteratorClose(iterator)
                    ctx.throwTypeError("iterator result is not an object")
                }
              }
            }
            catch {
              case e: Exception =>
                iteratorClose(iterator)
                throw e
            }
            setResultLength(result, index)
            result
          }
          else {
            val length = source match {
              case JSValue.JSArrayVal(arr) => arr.getLength
              case JSValue.JSStr(str)      => str.length
              case JSValue.Object(obj)     => obj.get("length").toNumber.toInt
              case func: JSValue.Function  => func.funcObj.get("length").toNumber.toInt
              case _                       => 0
            }
            val result = constructFromThis(Some(length))
            source match {
              case JSValue.JSArrayVal(arr) =>
                var i = 0
                while i < arr.getLength do {
                  pushValue(result, arr.get(i), i)
                  i += 1
                }
              case JSValue.JSStr(str) =>
                var i = 0
                while i < str.length do {
                  pushValue(result, JSValue.fromString(str.charAt(i).toString), i)
                  i += 1
                }
              case JSValue.Object(obj) =>
                val len = obj.get("length").toNumber.toInt
                var i = 0
                while i < len do {
                  pushValue(result, obj.get(i.toString), i)
                  i += 1
                }
              case func: JSValue.Function =>
                val len = func.funcObj.get("length").toNumber.toInt
                var i = 0
                while i < len do {
                  pushValue(result, func.funcObj.get(i.toString), i)
                  i += 1
                }
              case _ => ()
            }
            setResultLength(result, length)
            result
          }
        }
    )

    arrayConstructor.funcObj.defineProperty(
      "isArray",
      JSValue.Native(arrayIsArray),
      enumerable = false,
      writable = true,
      configurable = true
    )(using ctx)
    arrayConstructor.funcObj.defineProperty(
      "of",
      JSValue.Native(arrayOf),
      enumerable = false,
      writable = true,
      configurable = true
    )(using ctx)
    arrayConstructor.funcObj.defineProperty(
      "from",
      JSValue.Native(arrayFrom),
      enumerable = false,
      writable = true,
      configurable = true
    )(using ctx)
  }

  /** Initialize Array.prototype methods */
  def initializeArrayPrototype(ctx: JSContext): Unit = {
    def strictEquals(a: JSValue, b: JSValue): Boolean = (a, b) match {
      case (JSValue.Int32(x), JSValue.Int32(y))     => x == y
      case (JSValue.Float64(x), JSValue.Float64(y)) =>
        !x.isNaN && !y.isNaN && x == y
      case (JSValue.Int32(x), JSValue.Float64(y)) =>
        !y.isNaN && x.toDouble == y
      case (JSValue.Float64(x), JSValue.Int32(y)) =>
        !x.isNaN && x == y.toDouble
      case _ => a == b
    }

    def sameValueZero(a: JSValue, b: JSValue): Boolean = (a, b) match {
      case (JSValue.Float64(x), JSValue.Float64(y)) if x.isNaN && y.isNaN =>
        true
      case _ => strictEquals(a, b)
    }
    // Array.prototype.push(element1, ..., elementN)
    // Appends elements to the end of an array and returns the new length
    val arrayPrototypePush = NativeFunction(
      name = "push",
      impl = (args, ctx) =>
        val arr = thisArray(args, "push")
        given JSContext = ctx
        for i <- 1 until args.length do arr.push(args(i))
        JSValue.fromInt(arr.getLength)
    )

    // Array.prototype.map(callback)
    // Creates a new array with the results of calling a provided function on every element
    val arrayPrototypeMap = NativeFunction(
      name = "map",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "map")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        val result = JSArray(len)
        var i = 0
        while i < len do {
          checkInterrupted(i)
          if arrayLikeHas(receiver, i) then
            result.set(i, callFunctionWithThis(callback, thisArg,
              Array(arrayLikeGet(receiver, i), JSValue.fromInt(i), receiver)))
          i += 1
        }
        JSValue.JSArrayVal(result)
    )

    val arrayPrototypeFilter = NativeFunction(
      name = "filter",
      impl = (args, ctx) =>
        val receiver = arrayLikeReceiver(args, "filter")(using ctx)
        given JSContext = ctx
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        val resultArr = JSArray.empty()
        var i = 0
        while i < len do {
          checkInterrupted(i)
          if arrayLikeHas(receiver, i) then {
            val elem = arrayLikeGet(receiver, i)
            if callFunctionWithThis(
              callback,
              thisArg,
              Array(elem, JSValue.fromInt(i), receiver)
            ).toBoolean
            then resultArr.push(elem)
          }
          i += 1
        }
        JSValue.JSArrayVal(resultArr)
    )

    val arrayPrototypeForEach = NativeFunction(
      name = "forEach",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "forEach")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var i = 0
        while i < len do {
          checkInterrupted(i)
          if arrayLikeHas(receiver, i) then
            callFunctionWithThis(callback, thisArg,
              Array(arrayLikeGet(receiver, i), JSValue.fromInt(i), receiver))
          i += 1
        }
        JSValue.Undefined
    )

    val arrayPrototypeReduce = NativeFunction(
      name = "reduce",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "reduce")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val hasInitial = args.length > 2
        var index = 0
        var acc = if hasInitial then args(2) else JSValue.Undefined
        if !hasInitial then {
          while index < len && !arrayLikeHas(receiver, index) do {
            checkInterrupted(index)
            index += 1
          }
          if index >= len then ctx.throwTypeError("Reduce of empty array with no initial value")
          acc = arrayLikeGet(receiver, index)
          index += 1
        }
        while index < len do {
          checkInterrupted(index)
          if arrayLikeHas(receiver, index) then
            acc = callFunctionWithThis(callback, JSValue.Undefined,
              Array(acc, arrayLikeGet(receiver, index), JSValue.fromInt(index), receiver)
            )
          index += 1
        }
        acc
    )

    val arrayPrototypeIncludes = NativeFunction(
      name = "includes",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "includes")
        val search = if args.length > 1 then args(1) else JSValue.Undefined
        val len = arrayLikeLength(receiver)
        if len == 0 then JSValue.Bool(false)
        else {
          var k = if args.length > 2 then fromIndex(args(2), len) else 0
          var found = false
          while k < len && !found do {
            checkInterrupted(k)
            // includes uses Get rather than HasProperty, so holes compare as undefined.
            if sameValueZero(arrayLikeGet(receiver, k), search) then found = true
            k += 1
          }
          JSValue.fromBoolean(found)
        }
    )

    val arrayPrototypeIndexOf = NativeFunction(
      name = "indexOf",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "indexOf")
        val search = if args.length > 1 then args(1) else JSValue.Undefined
        val len = arrayLikeLength(receiver)
        var k = if args.length > 2 then fromIndex(args(2), len) else 0
        var idx = -1
        while k < len && idx < 0 do {
          checkInterrupted(k)
          if arrayLikeHas(receiver, k) && strictEquals(arrayLikeGet(receiver, k), search) then idx = k
          k += 1
        }
        JSValue.fromInt(idx)
    )

    val arrayPrototypeLastIndexOf = NativeFunction(
      name = "lastIndexOf",
      length = 1,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "lastIndexOf")
        val search = if args.length > 1 then args(1) else JSValue.Undefined
        val len = arrayLikeLength(receiver)
        var k =
          if len == 0 then -1
          else if args.length <= 2 || args(2) == JSValue.Undefined then len - 1
          else {
            val n = args(2).toNumber
            if n.isNaN then 0
            else if n >= 0 then math.min(n.toInt, len - 1)
            else len + n.toInt
          }
        var idx = -1
        while k >= 0 && idx < 0 do {
          checkInterrupted(k)
          if arrayLikeHas(receiver, k) &&
              strictEquals(arrayLikeGet(receiver, k), search)
          then idx = k
          k -= 1
        }
        JSValue.fromInt(idx)
    )

    val arrayPrototypeEvery = NativeFunction(
      name = "every",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "every")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var i = 0
        var result = true
        while i < len && result do {
          checkInterrupted(i)
          if arrayLikeHas(receiver, i) then
            result = callFunctionWithThis(callback, thisArg,
              Array(arrayLikeGet(receiver, i), JSValue.fromInt(i), receiver)).toBoolean
          i += 1
        }
        JSValue.fromBoolean(result)
    )

    val arrayPrototypeSome = NativeFunction(
      name = "some",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "some")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var i = 0
        var result = false
        while i < len && !result do {
          checkInterrupted(i)
          if arrayLikeHas(receiver, i) then
            result = callFunctionWithThis(callback, thisArg,
              Array(arrayLikeGet(receiver, i), JSValue.fromInt(i), receiver)).toBoolean
          i += 1
        }
        JSValue.fromBoolean(result)
    )

    val arrayPrototypeFind = NativeFunction(
      name = "find",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "find")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var i = 0
        var result: JSValue = JSValue.Undefined
        var found = false
        while i < len && !found do {
          checkInterrupted(i)
          val value = arrayLikeGet(receiver, i)
          if callFunctionWithThis(callback, thisArg,
              Array(value, JSValue.fromInt(i), receiver)).toBoolean
          then { result = value; found = true }
          i += 1
        }
        result
    )

    val arrayPrototypeFindIndex = NativeFunction(
      name = "findIndex",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "findIndex")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        var i = 0
        var result = -1
        while i < len && result < 0 do {
          checkInterrupted(i)
          if callFunctionWithThis(callback, thisArg,
              Array(arrayLikeGet(receiver, i), JSValue.fromInt(i), receiver)).toBoolean
          then result = i
          i += 1
        }
        JSValue.fromInt(result)
    )

    val arrayPrototypeReverse = NativeFunction(
      name = "reverse",
      impl = (args, ctx) =>
        val arr = thisArray(args, "reverse")
        val len = arr.getLength
        var i = 0
        while i < len / 2 do {
          val tmp = arr.get(i)
          arr.set(i, arr.get(len - 1 - i))
          arr.set(len - 1 - i, tmp)
          i += 1
        }
        args(0)
    )

    val arrayPrototypeFill = NativeFunction(
      name = "fill",
      impl = (args, ctx) =>
        val arr = thisArray(args, "fill")
        val len = arr.getLength
        val value = if args.length > 1 then args(1) else JSValue.Undefined
        val start =
          clampIndex(if args.length > 2 then args(2).toNumber.toInt else 0, len)
        val end = clampIndex(
          if args.length > 3 then args(3).toNumber.toInt else len,
          len
        )
        var i = start
        while i < end do { arr.set(i, value); i += 1 }
        args(0)
    )

    val arrayPrototypeAt = NativeFunction(
      name = "at",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "at")
        val len = arrayLikeLength(receiver)
        val raw = if args.length > 1 then toIntegerOrInfinity(args(1)) else 0.0
        if raw.isInfinite then JSValue.Undefined
        else {
          val idx = raw.toInt
          val i = if idx < 0 then len + idx else idx
          if i < 0 || i >= len then JSValue.Undefined else arrayLikeGet(receiver, i)
        }
    )

    val arrayPrototypeCopyWithin = NativeFunction(
      name = "copyWithin",
      impl = (args, ctx) =>
        val arr = thisArray(args, "copyWithin")
        val len = arr.getLength
        val target =
          clampIndex(if args.length > 1 then args(1).toNumber.toInt else 0, len)
        val start =
          clampIndex(if args.length > 2 then args(2).toNumber.toInt else 0, len)
        val end = clampIndex(
          if args.length > 3 then args(3).toNumber.toInt else len,
          len
        )
        val count = math.min(end - start, len - target)
        if count > 0 then {
          val dir = if start < target && target < start + count then -1 else 1
          var i = if dir > 0 then 0 else count - 1
          while i >= 0 && i < count do {
            arr.set(target + i, arr.get(start + i)); i += dir
          }
        }
        args(0)
    )

    val arrayPrototypeSplice = NativeFunction(
      name = "splice",
      impl = (args, ctx) =>
        val arr = thisArray(args, "splice")
        val len = arr.getLength
        val actualStart =
          clampIndex(if args.length > 1 then args(1).toNumber.toInt else 0, len)
        val deleteCount = math.max(
          0,
          math.min(
            if args.length > 2 then args(2).toNumber.toInt
            else len - actualStart,
            len - actualStart
          )
        )
        val items =
          if args.length > 3 then args.slice(3, args.length).toSeq
          else Seq.empty
        JSValue.JSArrayVal(arr.splice(actualStart, deleteCount, items))
    )

    val arrayPrototypeShift = NativeFunction(
      name = "shift",
      impl = (args, ctx) =>
        val arr = thisArray(args, "shift")
        val len = arr.getLength
        if len == 0 then JSValue.Undefined
        else {
          val first = arr.get(0)
          var i = 1; while i < len do { arr.set(i - 1, arr.get(i)); i += 1 }
          arr.setLength(len - 1)
          first
        }
    )

    val arrayPrototypeUnshift = NativeFunction(
      name = "unshift",
      impl = (args, ctx) =>
        val arr = thisArray(args, "unshift")
        val elements =
          if args.length > 1 then args.slice(1, args.length)
          else Array.empty[JSValue]
        val len = arr.getLength; val n = elements.length
        var i = len - 1; while i >= 0 do { arr.set(i + n, arr.get(i)); i -= 1 }
        var j = 0; while j < n do { arr.set(j, elements(j)); j += 1 }
        JSValue.fromInt(arr.getLength)
    )

    // Array.prototype.pop()
    // Removes the last element from an array and returns that element
    val arrayPrototypePop = NativeFunction(
      name = "pop",
      impl = (args, _) =>
        val arr = thisArray(args, "pop")
        arr.pop()
    )

    val arrayPrototypeReduceRight = NativeFunction(
      name = "reduceRight",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "reduceRight")
        val len = arrayLikeLength(receiver)
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val hasInitial = args.length > 2
        var index = len - 1
        var acc = if hasInitial then args(2) else JSValue.Undefined
        if !hasInitial then {
          while index >= 0 && !arrayLikeHas(receiver, index) do {
            checkInterrupted(index)
            index -= 1
          }
          if index < 0 then ctx.throwTypeError("Reduce of empty array with no initial value")
          acc = arrayLikeGet(receiver, index)
          index -= 1
        }
        while index >= 0 do {
          checkInterrupted(index)
          if arrayLikeHas(receiver, index) then
            acc = callFunctionWithThis(callback, JSValue.Undefined,
              Array(acc, arrayLikeGet(receiver, index), JSValue.fromInt(index), receiver)
            )
          index -= 1
        }
        acc
    )

    val arrayPrototypeSort = NativeFunction(
      name = "sort",
      impl = (args, ctx) =>
        val arr = thisArray(args, "sort")
        given JSContext = ctx
        val len = arr.getLength;
        val compareFn = if args.length > 1 then Some(args(1)) else None
        val values = (0 until len).map(arr.get).toArray
        def cmp(a: JSValue, b: JSValue): Int = compareFn match {
          case Some(func) =>
            val num = callFunctionWithThis(
              func,
              JSValue.Undefined,
              Array(a, b)
            ).toNumber
            if num.isNaN then 0 else num.sign.toInt
          case None => a.toString.compareTo(b.toString)
        }
        Sorting.stableSort(values, (a, b) => cmp(a, b) < 0)
        for i <- 0 until len do arr.set(i, values(i))
        args(0)
    )

    // Array.prototype.toString()
    // Joins elements with commas
    val arrayPrototypeToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        args(0) match {
          case JSValue.JSArrayVal(arrVal) =>
            JSValue.fromString(
              (0 until arrVal.getLength)
                .map(i => arrVal.get(i).toString)
                .mkString(",")
            )
          case _ => JSValue.fromString("")
        }
    )

    val arrayPrototypeJoin = NativeFunction(
      name = "join",
      impl = (args, ctx) =>
        val arr = thisArray(args, "join")
        val sep =
          if args.length > 1 && args(1) != JSValue.Undefined then
            args(1).toString
          else ","
        JSValue.fromString(
          (0 until arr.getLength)
            .map(i =>
              arr.get(i) match {
                case JSValue.Undefined | JSValue.Null => ""
                case v                                => v.toString
              }
            )
            .mkString(sep)
        )
    )

    val arrayPrototypeToLocaleString = NativeFunction(
      name = "toLocaleString",
      impl = (args, ctx) =>
        given JSContext = ctx
        val arr = thisArray(args, "toLocaleString")
        val values = (0 until arr.getLength).map { index =>
          arr.get(index) match {
            case JSValue.Null | JSValue.Undefined => ""
            case value =>
              val method = BuiltinHelpers.getPropertyWithGetter(
                value,
                "toLocaleString"
              )
              if !BuiltinHelpers.isCallable(method) then
                ctx.throwTypeError("toLocaleString is not callable")
              BuiltinHelpers
                .callFunctionWithThis(method, value, Array.empty)
                .toString
          }
        }
        JSValue.fromString(values.mkString(","))
    )

    // Array.prototype.concat(value1, value2, ..., valueN)
    // Returns a new array comprised of this array joined with other array(s) and/or value(s)
    val arrayPrototypeConcat = NativeFunction(
      name = "concat",
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "concat")
        val resultArr = JSArray.empty()
        val spreadSymbol = getWellKnownSymbol("isConcatSpreadable") match {
          case JSValue.Symbol(id) => Some(id)
          case _                  => None
        }
        def spreadOverride(value: JSValue): Option[Boolean] =
          spreadSymbol.flatMap { id => value match {
            case JSValue.JSArrayVal(arr) => arr.getOwnSymbol(id).map(_.toBoolean)
            case _ => extractJSObject(value).flatMap { obj =>
              obj.getSymbolPropertyDescriptorWithOwner(id) match {
                case Some((_, _, attrs)) if attrs.getter.isDefined =>
                  Some(callFunctionWithThis(attrs.getter.get, value, Array.empty).toBoolean)
                case Some((_, stored, _)) => Some(stored.toBoolean)
                case None                 => None
              }
            }
          }}
        def isSpreadable(value: JSValue): Boolean =
          spreadOverride(value).getOrElse(value.isInstanceOf[JSValue.JSArrayVal])

        var target = 0
        def append(value: JSValue): Unit =
          if isSpreadable(value) then {
            val len = arrayLikeLength(value)
            var sourceIndex = 0
            while sourceIndex < len do {
              checkInterrupted(sourceIndex)
              if arrayLikeHas(value, sourceIndex) then
                resultArr.set(target, arrayLikeGet(value, sourceIndex))
              sourceIndex += 1
              target += 1
            }
          } else {
            resultArr.set(target, value)
            target += 1
          }

        append(receiver)
        var j = 1
        while j < args.length do { append(args(j)); j += 1 }
        resultArr.setLength(target)
        JSValue.JSArrayVal(resultArr)
    )

    // Array.prototype.slice(begin, end)
    // Returns a shallow copy of a portion of an array
    val arrayPrototypeSlice = NativeFunction(
      name = "slice",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        val receiver = arrayLikeReceiver(args, "slice")
        val len = arrayLikeLength(receiver)
        val start =
          if args.length > 1 then relativeIndex(args(1), len) else 0
        val stop = if args.length > 2 && args(2) != JSValue.Undefined then
          relativeIndex(args(2), len)
        else len
        val count = math.max(stop - start, 0)
        val resultArr = JSArray(count)
        var i = start
        var target = 0
        while i < stop do {
          checkInterrupted(target)
          if arrayLikeHas(receiver, i) then resultArr.set(target, arrayLikeGet(receiver, i))
          i += 1
          target += 1
        }
        JSValue.JSArrayVal(resultArr)
    )

    // Add methods to Array.prototype
    given JSContext = ctx
    def defineArrayMethod(name: String, function: NativeFunction): Unit =
      ctx.arrayPrototype.defineProperty(name, JSValue.Native(function),
        enumerable = false, writable = true, configurable = true)

    defineArrayMethod("push", arrayPrototypePush)
    defineArrayMethod("pop", arrayPrototypePop)
    defineArrayMethod("map", arrayPrototypeMap)
    defineArrayMethod("filter", arrayPrototypeFilter)
    defineArrayMethod("forEach", arrayPrototypeForEach)
    defineArrayMethod("reduce", arrayPrototypeReduce)
    defineArrayMethod("includes", arrayPrototypeIncludes)
    defineArrayMethod("indexOf", arrayPrototypeIndexOf)
    defineArrayMethod("lastIndexOf", arrayPrototypeLastIndexOf)
    defineArrayMethod("every", arrayPrototypeEvery)
    defineArrayMethod("some", arrayPrototypeSome)
    defineArrayMethod("find", arrayPrototypeFind)
    defineArrayMethod("findIndex", arrayPrototypeFindIndex)
    defineArrayMethod("reverse", arrayPrototypeReverse)
    defineArrayMethod("fill", arrayPrototypeFill)
    defineArrayMethod("at", arrayPrototypeAt)
    defineArrayMethod("copyWithin", arrayPrototypeCopyWithin)
    defineArrayMethod("splice", arrayPrototypeSplice)
    defineArrayMethod("shift", arrayPrototypeShift)
    defineArrayMethod("unshift", arrayPrototypeUnshift)
    defineArrayMethod("toString", arrayPrototypeToString)
    defineArrayMethod("toLocaleString", arrayPrototypeToLocaleString)
    defineArrayMethod("reduceRight", arrayPrototypeReduceRight)
    defineArrayMethod("sort", arrayPrototypeSort)
    defineArrayMethod("join", arrayPrototypeJoin)
    defineArrayMethod("concat", arrayPrototypeConcat)
    defineArrayMethod("slice", arrayPrototypeSlice)

    def arrayIteratorResult(value: JSValue, done: Boolean)(using
        JSContext
    ): JSValue = {
      val obj = JSObject(prototype = ctx.objectPrototype, extensible = true)
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

    def createArrayIterator(arr: JSArray, kind: String)(using
        JSContext
    ): JSValue = {
      val iterator = JSObject(prototype = ctx.objectPrototype, extensible = true)
      iterator.defineProperty(
        "__arrayIteratorTarget",
        JSValue.JSArrayVal(arr),
        enumerable = false,
        writable = true,
        configurable = false
      )
      iterator.defineProperty(
        "__arrayIteratorIndex",
        JSValue.Int32(0),
        enumerable = false,
        writable = true,
        configurable = false
      )
      iterator.defineProperty(
        "__arrayIteratorKind",
        JSValue.JSStr(kind),
        enumerable = false,
        writable = true,
        configurable = false
      )
      val next = NativeFunction(
        name = "next",
        length = 0,
        impl = (args, ctx) =>
          given JSContext = ctx
          val thisObj = args.headOption match {
            case Some(JSValue.Object(o)) => o
            case _ =>
              ctx.throwTypeError(
                "Array Iterator.prototype.next called on incompatible receiver"
              )
          }
          val target = thisObj.get("__arrayIteratorTarget") match {
            case JSValue.JSArrayVal(a) => a
            case _ =>
              ctx.throwTypeError(
                "Array Iterator.prototype.next called on incompatible receiver"
              )
          }
          val index = thisObj.get("__arrayIteratorIndex") match {
            case JSValue.Int32(i)   => i
            case JSValue.Float64(d) => d.toInt
            case _                  => 0
          }
          if index >= target.getLength then
            arrayIteratorResult(JSValue.Undefined, done = true)
          else {
            thisObj.defineProperty(
              "__arrayIteratorIndex",
              JSValue.Int32(index + 1),
              enumerable = false,
              writable = true,
              configurable = false
            )
            val value = thisObj.get("__arrayIteratorKind") match {
              case JSValue.JSStr("key") => JSValue.Int32(index)
              case JSValue.JSStr("entry") =>
                val pair = JSArray.empty()
                pair.push(JSValue.Int32(index))
                pair.push(target.get(index))
                JSValue.JSArrayVal(pair)
              case _ => target.get(index)
            }
            arrayIteratorResult(value, done = false)
          }
      )
      iterator.defineProperty(
        "next",
        JSValue.Native(next),
        enumerable = false,
        writable = true,
        configurable = true
      )
      getWellKnownSymbol("iterator") match {
        case JSValue.Symbol(sym) =>
          val selfIterator = NativeFunction(
            name = "[Symbol.iterator]",
            length = 0,
            impl = (args, _) => args.headOption.getOrElse(JSValue.Undefined)
          )
          iterator.initSymbolProperty(
            sym,
            JSValue.Native(selfIterator),
            enumerable = false,
            writable = true,
            configurable = true
          )
        case _ => ()
      }
      JSValue.Object(iterator)
    }

    val arrayPrototypeValues = NativeFunction(
      name = "values",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        createArrayIterator(thisArray(args, "values"), "value")
    )
    val arrayPrototypeKeys = NativeFunction(
      name = "keys",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        createArrayIterator(thisArray(args, "keys"), "key")
    )
    val arrayPrototypeEntries = NativeFunction(
      name = "entries",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        createArrayIterator(thisArray(args, "entries"), "entry")
    )
    ctx.arrayPrototype.defineProperty(
      "values",
      JSValue.Native(arrayPrototypeValues),
      enumerable = false,
      writable = true,
      configurable = true
    )
    ctx.arrayPrototype.defineProperty(
      "keys",
      JSValue.Native(arrayPrototypeKeys),
      enumerable = false,
      writable = true,
      configurable = true
    )
    ctx.arrayPrototype.defineProperty(
      "entries",
      JSValue.Native(arrayPrototypeEntries),
      enumerable = false,
      writable = true,
      configurable = true
    )
    getWellKnownSymbol("iterator") match {
      case JSValue.Symbol(sym) =>
        ctx.arrayPrototype.initSymbolProperty(
          sym,
          JSValue.Native(arrayPrototypeValues),
          enumerable = false,
          writable = true,
          configurable = true
        )
      case _ => ()
    }

    // Array.prototype.flat(depth)
    val arrayPrototypeFlat = NativeFunction(
      name = "flat",
      impl = (args, ctx) =>
        val arr = thisArray(args, "flat")
        val depth = if args.length > 1 then args(1).toNumber.toInt else 1
        def flatten(source: JSArray, d: Int): JSArray = {
          val result = JSArray.empty()
          for i <- 0 until source.getLength do
            source.get(i) match {
              case JSValue.JSArrayVal(inner) if d > 0 =>
                val f = flatten(inner, d - 1)
                for j <- 0 until f.getLength do result.push(f.get(j))
              case v => result.push(v)
            }
          result
        }
        JSValue.JSArrayVal(flatten(arr, depth))
    )
    defineArrayMethod("flat", arrayPrototypeFlat)

    // Array.prototype.flatMap(callback, thisArg)
    val arrayPrototypeFlatMap = NativeFunction(
      name = "flatMap",
      impl = (args, ctx) =>
        val arr = thisArray(args, "flatMap")
        given JSContext = ctx
        val callback = requireCallback(if args.length > 1 then args(1) else JSValue.Undefined)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        val result = JSArray.empty()
        for i <- 0 until arr.getLength do {
          val mapped = callback match {
            case f: JSValue.Function =>
              Interpreter().call(
                functionToBytecode(f),
                thisArg,
                Array(arr.get(i), JSValue.fromInt(i), JSValue.JSArrayVal(arr)),
                f.closure
              )
            case JSValue.Native(nf: NativeFunction) =>
              nf.call(
                Array(
                  thisArg,
                  arr.get(i),
                  JSValue.fromInt(i),
                  JSValue.JSArrayVal(arr)
                )
              )
            case _ => ctx.throwTypeError("flatMap callback is not a function")
          }
          mapped match {
            case JSValue.JSArrayVal(inner) =>
              for j <- 0 until inner.getLength do result.push(inner.get(j))
            case v => result.push(v)
          }
        }
        JSValue.JSArrayVal(result)
    )
    defineArrayMethod("flatMap", arrayPrototypeFlatMap)
  }
}
