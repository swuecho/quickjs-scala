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
  functionToBytecode
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
          JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else {
          val source = args(offset)
          val mapFn =
            if args.length > offset + 1 then Some(args(offset + 1)) else None
          val thisArg =
            if args.length > offset + 2 then args(offset + 2)
            else JSValue.Undefined
          val result = quickjs.objmodel.JSArray.empty()
          given JSContext = ctx

          def pushValue(value: JSValue, index: Int): Unit = {
            val mapped =
              mapFn match {
                case Some(func) =>
                  callFunctionWithThis(
                    func,
                    thisArg,
                    Array(value, JSValue.fromInt(index), source)
                  )
                case None => value
              }
            result.push(mapped)
          }

          source match {
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do {
                pushValue(arr.get(i), i)
                i += 1
              }
            case JSValue.JSStr(str) =>
              var i = 0
              while i < str.length do {
                pushValue(JSValue.fromString(str.charAt(i).toString), i)
                i += 1
              }
            case JSValue.Object(obj) =>
              val len = obj.get("length").toNumber.toInt
              var i = 0
              while i < len do {
                pushValue(obj.get(i.toString), i)
                i += 1
              }
            case func: JSValue.Function =>
              val len = func.funcObj.get("length").toNumber.toInt
              var i = 0
              while i < len do {
                pushValue(func.funcObj.get(i.toString), i)
                i += 1
              }
            case _ => ()
          }

          JSValue.JSArrayVal(result)
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
        val arr = thisArray(args, "map")
        val callback = args(1)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        JSValue.JSArrayVal(iterateMap(arr, callback, thisArg, ctx))
    )

    val arrayPrototypeFilter = NativeFunction(
      name = "filter",
      impl = (args, ctx) =>
        val arr = thisArray(args, "filter")
        val callback = args(1)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        given JSContext = ctx
        val resultArr = JSArray.empty()
        var i = 0
        while i < arr.getLength do {
          val elem = arr.get(i)
          if callFunctionWithThis(
              callback,
              thisArg,
              Array(elem, JSValue.fromInt(i), JSValue.JSArrayVal(arr))
            ).toBoolean
          then resultArr.push(elem)
          i += 1
        }
        JSValue.JSArrayVal(resultArr)
    )

    val arrayPrototypeForEach = NativeFunction(
      name = "forEach",
      impl = (args, ctx) =>
        val arr = thisArray(args, "forEach")
        iterateForEach(
          arr,
          args(1),
          if args.length > 2 then args(2) else JSValue.Undefined,
          ctx
        )
        JSValue.Undefined
    )

    val arrayPrototypeReduce = NativeFunction(
      name = "reduce",
      impl = (args, ctx) =>
        val arr = thisArray(args, "reduce")
        val callback = args(1)
        given JSContext = ctx
        val len = arr.getLength
        val hasInitial = args.length > 2
        if len == 0 && !hasInitial then
          throw new RuntimeException(
            "TypeError: Reduce of empty array with no initial value"
          )
        var acc = if hasInitial then args(2) else arr.get(0)
        var index = if hasInitial then 0 else 1
        while index < len do {
          acc = callFunctionWithThis(
            callback,
            JSValue.Undefined,
            Array(
              acc,
              arr.get(index),
              JSValue.fromInt(index),
              JSValue.JSArrayVal(arr)
            )
          )
          index += 1
        }
        acc
    )

    val arrayPrototypeIncludes = NativeFunction(
      name = "includes",
      impl = (args, ctx) =>
        val arr = thisArray(args, "includes")
        val search = if args.length > 1 then args(1) else JSValue.Undefined
        val fromIndex = if args.length > 2 then args(2).toNumber.toInt else 0
        val len = arr.getLength
        var k =
          if fromIndex < 0 then math.max(len + fromIndex, 0) else fromIndex
        var found = false
        while k < len && !found do {
          if sameValueZero(arr.get(k), search) then found = true
          k += 1
        }
        JSValue.fromBoolean(found)
    )

    val arrayPrototypeIndexOf = NativeFunction(
      name = "indexOf",
      impl = (args, ctx) =>
        val arr = thisArray(args, "indexOf")
        val search = if args.length > 1 then args(1) else JSValue.Undefined
        val fromIndex = if args.length > 2 then args(2).toNumber.toInt else 0
        val len = arr.getLength
        var k =
          if fromIndex < 0 then math.max(len + fromIndex, 0) else fromIndex
        var idx = -1
        while k < len && idx < 0 do {
          if strictEquals(arr.get(k), search) then idx = k
          k += 1
        }
        JSValue.fromInt(idx)
    )

    val arrayPrototypeEvery = NativeFunction(
      name = "every",
      impl = (args, ctx) =>
        val arr = thisArray(args, "every")
        val callback = args(1)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        given JSContext = ctx
        var i = 0
        while i < arr.getLength && callFunctionWithThis(
            callback,
            thisArg,
            Array(arr.get(i), JSValue.fromInt(i), JSValue.JSArrayVal(arr))
          ).toBoolean
        do i += 1
        JSValue.fromBoolean(i >= arr.getLength)
    )

    val arrayPrototypeSome = NativeFunction(
      name = "some",
      impl = (args, ctx) =>
        val arr = thisArray(args, "some")
        val callback = args(1)
        val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
        JSValue.fromBoolean(iterateFind(arr, callback, thisArg, ctx) >= 0)
    )

    val arrayPrototypeFind = NativeFunction(
      name = "find",
      impl = (args, ctx) =>
        val arr = thisArray(args, "find")
        val idx = iterateFind(
          arr,
          args(1),
          if args.length > 2 then args(2) else JSValue.Undefined,
          ctx
        )
        if idx >= 0 then arr.get(idx) else JSValue.Undefined
    )

    val arrayPrototypeFindIndex = NativeFunction(
      name = "findIndex",
      impl = (args, ctx) =>
        val arr = thisArray(args, "findIndex")
        JSValue.fromInt(
          iterateFind(
            arr,
            args(1),
            if args.length > 2 then args(2) else JSValue.Undefined,
            ctx
          )
        )
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
        val arr = thisArray(args, "at")
        val len = arr.getLength
        val idx = if args.length > 1 then args(1).toNumber.toInt else 0
        val i = if idx < 0 then len + idx else idx
        if i < 0 || i >= len then JSValue.Undefined else arr.get(i)
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
        val arr = thisArray(args, "reduceRight")
        val callback = args(1)
        given JSContext = ctx
        val len = arr.getLength; val hasInitial = args.length > 2
        if len == 0 && !hasInitial then
          throw new RuntimeException(
            "TypeError: Reduce of empty array with no initial value"
          )
        var acc = if hasInitial then args(2) else arr.get(len - 1)
        var index = if hasInitial then len - 1 else len - 2
        while index >= 0 do {
          acc = callFunctionWithThis(
            callback,
            JSValue.Undefined,
            Array(
              acc,
              arr.get(index),
              JSValue.fromInt(index),
              JSValue.JSArrayVal(arr)
            )
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

    // Array.prototype.concat(value1, value2, ..., valueN)
    // Returns a new array comprised of this array joined with other array(s) and/or value(s)
    val arrayPrototypeConcat = NativeFunction(
      name = "concat",
      impl = (args, ctx) =>
        val arr = thisArray(args, "concat")
        val resultArr = JSArray.empty()
        for i <- 0 until arr.getLength do resultArr.push(arr.get(i))
        for j <- 1 until args.length do
          args(j) match {
            case JSValue.JSArrayVal(other) =>
              for k <- 0 until other.getLength do resultArr.push(other.get(k))
            case elem => resultArr.push(elem)
          }
        JSValue.JSArrayVal(resultArr)
    )

    // Array.prototype.slice(begin, end)
    // Returns a shallow copy of a portion of an array
    val arrayPrototypeSlice = NativeFunction(
      name = "slice",
      impl = (args, ctx) =>
        val arr = thisArray(args, "slice")
        val len = arr.getLength
        val start =
          clampIndex(if args.length > 1 then args(1).toNumber.toInt else 0, len)
        val stop = clampIndex(
          if args.length > 2 then args(2).toNumber.toInt else len,
          len
        )
        val resultArr = JSArray.empty()
        var i = start; while i < stop do { resultArr.push(arr.get(i)); i += 1 }
        JSValue.JSArrayVal(resultArr)
    )

    // Add methods to Array.prototype
    given JSContext = ctx
    ctx.arrayPrototype.set("push", JSValue.Native(arrayPrototypePush))
    ctx.arrayPrototype.set("pop", JSValue.Native(arrayPrototypePop))
    ctx.arrayPrototype.set("map", JSValue.Native(arrayPrototypeMap))
    ctx.arrayPrototype.set("filter", JSValue.Native(arrayPrototypeFilter))
    ctx.arrayPrototype.set("forEach", JSValue.Native(arrayPrototypeForEach))
    ctx.arrayPrototype.set("reduce", JSValue.Native(arrayPrototypeReduce))
    ctx.arrayPrototype.set("includes", JSValue.Native(arrayPrototypeIncludes))
    ctx.arrayPrototype.set("indexOf", JSValue.Native(arrayPrototypeIndexOf))
    ctx.arrayPrototype.set("every", JSValue.Native(arrayPrototypeEvery))
    ctx.arrayPrototype.set("some", JSValue.Native(arrayPrototypeSome))
    ctx.arrayPrototype.set("find", JSValue.Native(arrayPrototypeFind))
    ctx.arrayPrototype.set("findIndex", JSValue.Native(arrayPrototypeFindIndex))
    ctx.arrayPrototype.set("reverse", JSValue.Native(arrayPrototypeReverse))
    ctx.arrayPrototype.set("fill", JSValue.Native(arrayPrototypeFill))
    ctx.arrayPrototype.set("at", JSValue.Native(arrayPrototypeAt))
    ctx.arrayPrototype.set(
      "copyWithin",
      JSValue.Native(arrayPrototypeCopyWithin)
    )
    ctx.arrayPrototype.set("splice", JSValue.Native(arrayPrototypeSplice))
    ctx.arrayPrototype.set("shift", JSValue.Native(arrayPrototypeShift))
    ctx.arrayPrototype.set("unshift", JSValue.Native(arrayPrototypeUnshift))
    ctx.arrayPrototype.set("toString", JSValue.Native(arrayPrototypeToString))
    ctx.arrayPrototype.set(
      "reduceRight",
      JSValue.Native(arrayPrototypeReduceRight)
    )
    ctx.arrayPrototype.set("sort", JSValue.Native(arrayPrototypeSort))
    ctx.arrayPrototype.set("join", JSValue.Native(arrayPrototypeJoin))
    ctx.arrayPrototype.set("concat", JSValue.Native(arrayPrototypeConcat))
    ctx.arrayPrototype.set("slice", JSValue.Native(arrayPrototypeSlice))

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
    ctx.arrayPrototype.set("flat", JSValue.Native(arrayPrototypeFlat))

    // Array.prototype.flatMap(callback, thisArg)
    val arrayPrototypeFlatMap = NativeFunction(
      name = "flatMap",
      impl = (args, ctx) =>
        val arr = thisArray(args, "flatMap")
        given JSContext = ctx
        val callback = if args.length > 1 then args(1) else JSValue.Undefined
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
    ctx.arrayPrototype.set("flatMap", JSValue.Native(arrayPrototypeFlatMap))
  }
}
