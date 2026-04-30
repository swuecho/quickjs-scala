package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers.{initConstructor, callFunctionValue, callFunctionWithThis}
import scala.util.Sorting

/** Function and Array built-in methods. */
object FunctionArrayBuiltins:
  import quickjs.objmodel.{JSObject, JSArray}

  def initializeFunctionPrototype(ctx: JSContext): Unit =
    val functionPrototypeCall = NativeFunction(
      name = "call",
      impl = (args, ctx) =>
        // When called as a method, args(0) is the function (this value)
        // args(1) is the thisArg, args(2...) are the actual arguments
        if args.isEmpty then
          throw new RuntimeException("Function.prototype.call called on non-function")
        else
          val func = args(0)  // The function to call
          val thisArg = if args.length > 1 then args(1) else JSValue.Undefined
          val actualArgs = if args.length > 2 then args.slice(2, args.length) else Array.empty[JSValue]

          func match
            case f: JSValue.Function =>
              // Call the bytecode function with the custom this binding
              given JSContext = ctx
              val interpreter = Interpreter()
              // Create a temporary BytecodeFunction wrapper
              val bcFunc = new BytecodeFunction(
                name = f.name,
                bytecode = f.bytecode,
                constants = f.constants,
                stackSize = f.stackSize,
                freeVars = Array.empty,
                paramNames = f.paramNames,
                localVarNames = f.localVarNames,
                argumentsIndex = f.argumentsIndex,
                isConstructor = f.isConstructor
              )
              interpreter.call(bcFunc, thisArg, actualArgs, f.closure)
            case JSValue.Native(nativeFuncWrapper) =>
              // Call the native function with the custom this binding
              nativeFuncWrapper match
                case native: NativeFunction =>
                  // Prepend thisArg to arguments for native functions that expect it
                  val argsWithThis = new Array[JSValue](actualArgs.length + 1)
                  argsWithThis(0) = thisArg
                  Array.copy(actualArgs, 0, argsWithThis, 1, actualArgs.length)
                  given JSContext = ctx
                  native.call(argsWithThis)
                case constructor: quickjs.value.NativeConstructor =>
                  // Native constructor called with .call()
                  given JSContext = ctx
                  constructor.call(actualArgs)
                case _ =>
                  throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
            case _ =>
              throw new RuntimeException(s"Function.prototype.call called on non-function: $func")
    )

    // Add methods to Function.prototype
    given JSContext = ctx
    ctx.functionPrototype.set("call", JSValue.Native(functionPrototypeCall))

    // Function.prototype.apply(thisArg, argsArray)
    val functionPrototypeApply = NativeFunction(
      name = "apply",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Function.prototype.apply called on non-function")
        else
          val func = args(0)  // The function to call
          val thisArg = if args.length > 1 then args(1) else JSValue.Undefined
          // Get arguments from array
          val actualArgs: Array[JSValue] = if args.length > 2 then
            args(2) match
              case JSValue.JSArrayVal(arr) =>
                val len = arr.length
                val result = new Array[JSValue](len)
                for i <- 0 until len do
                  result(i) = arr.get(i)
                result
              case JSValue.Null | JSValue.Undefined => Array.empty[JSValue]
              case other =>
                // Try to treat as array-like
                other match
                  case JSValue.Object(obj) =>
                    given JSContext = ctx
                    obj.get("length") match
                      case JSValue.Int32(len) =>
                        val result = new Array[JSValue](len)
                        for i <- 0 until len do
                          result(i) = obj.get(i.toString)
                        result
                      case _ => throw new RuntimeException("CreateListFromArrayLike called on non-object")
                  case _ => throw new RuntimeException("CreateListFromArrayLike called on non-object")
          else Array.empty[JSValue]

          func match
            case f: JSValue.Function =>
              given JSContext = ctx
              val interpreter = Interpreter()
              val bcFunc = new BytecodeFunction(
                name = f.name,
                bytecode = f.bytecode,
                constants = f.constants,
                stackSize = f.stackSize,
                freeVars = Array.empty,
                paramNames = f.paramNames,
                localVarNames = f.localVarNames,
                argumentsIndex = f.argumentsIndex,
                isConstructor = f.isConstructor
              )
              interpreter.call(bcFunc, thisArg, actualArgs, f.closure)
            case JSValue.Native(nativeFuncWrapper) =>
              nativeFuncWrapper match
                case native: NativeFunction =>
                  val argsWithThis = new Array[JSValue](actualArgs.length + 1)
                  argsWithThis(0) = thisArg
                  Array.copy(actualArgs, 0, argsWithThis, 1, actualArgs.length)
                  given JSContext = ctx
                  native.call(argsWithThis)
                case constructor: quickjs.value.NativeConstructor =>
                  given JSContext = ctx
                  constructor.call(actualArgs)
                case _ =>
                  throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
            case _ =>
              throw new RuntimeException(s"Function.prototype.apply called on non-function: $func")
    )
    ctx.functionPrototype.set("apply", JSValue.Native(functionPrototypeApply))

    // Function.prototype.bind(thisArg, arg1, arg2, ...)
    val functionPrototypeBind = NativeFunction(
      name = "bind",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Function.prototype.bind called on non-function")
        else
          val func = args(0)  // The function to bind
          val boundThis = if args.length > 1 then args(1) else JSValue.Undefined
          val boundArgs = if args.length > 2 then args.slice(2, args.length) else Array.empty[JSValue]

          // Create a bound function
          val boundFunction = NativeFunction(
            name = "bound",
            impl = (callArgs, callCtx) =>
              // callArgs(0) is the thisArg passed to the bound function (ignored)
              val actualCallArgs = if callArgs.length > 1 then callArgs.slice(1, callArgs.length) else Array.empty[JSValue]
              // Combine bound args with call args
              val combinedArgs = boundArgs ++ actualCallArgs

              func match
                case f: JSValue.Function =>
                  given JSContext = callCtx
                  val interpreter = Interpreter()
                  val bcFunc = new BytecodeFunction(
                    name = f.name,
                    bytecode = f.bytecode,
                    constants = f.constants,
                    stackSize = f.stackSize,
                    freeVars = Array.empty,
                    paramNames = f.paramNames,
                    localVarNames = f.localVarNames,
                    argumentsIndex = f.argumentsIndex,
                    isConstructor = f.isConstructor
                  )
                  interpreter.call(bcFunc, boundThis, combinedArgs, f.closure)
                case JSValue.Native(nativeFuncWrapper) =>
                  nativeFuncWrapper match
                    case native: NativeFunction =>
                      val argsWithThis = new Array[JSValue](combinedArgs.length + 1)
                      argsWithThis(0) = boundThis
                      Array.copy(combinedArgs, 0, argsWithThis, 1, combinedArgs.length)
                      given JSContext = callCtx
                      native.call(argsWithThis)
                    case constructor: quickjs.value.NativeConstructor =>
                      given JSContext = callCtx
                      constructor.call(combinedArgs)
                    case _ =>
                      throw new RuntimeException(s"Invalid native function: $nativeFuncWrapper")
                case _ =>
                  throw new RuntimeException(s"Bound function called on non-function: $func")
          )
          JSValue.Native(boundFunction)
    )
    ctx.functionPrototype.set("bind", JSValue.Native(functionPrototypeBind))

    val functionPrototypeToString = NativeFunction(
      name = "toString",
      impl = (_, _) => JSValue.fromString("[object Function]")
    )
    ctx.functionPrototype.set("toString", JSValue.Native(functionPrototypeToString))


  def initializeArrayConstructor(ctx: JSContext): Unit =
    def buildArray(values: Seq[JSValue]): JSValue =
      val arr = quickjs.objmodel.JSArray.empty()
      values.foreach(arr.push)
      JSValue.JSArrayVal(arr)

    def buildArrayFromArgs(args: Array[JSValue], offset: Int): JSValue =
      if args.length == offset then
        buildArray(Seq.empty)
      else if args.length == offset + 1 then
        args(offset) match
          case JSValue.Int32(i) =>
            if i < 0 then ctx.throwRangeError("Invalid array length")
            JSValue.JSArrayVal(quickjs.objmodel.JSArray(i))
          case JSValue.Float64(d) =>
            if d.isNaN || d.isInfinite || d < 0 || d != math.floor(d) then
              ctx.throwRangeError("Invalid array length")
            else if d > Int.MaxValue then
              ctx.throwRangeError("Invalid array length")
            else
              JSValue.JSArrayVal(quickjs.objmodel.JSArray(d.toInt))
          case _ =>
            buildArray(Seq(args(offset)))
      else
        buildArray(args.drop(offset).toSeq)

    val arrayConstructor = quickjs.value.NativeConstructor(
      name = "Array",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        val offset = if args.length >= 2 then 1 else 0
        buildArrayFromArgs(args, offset),
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val offset = if args.length >= 2 then 1 else 0
        buildArrayFromArgs(args, offset),
      prototype = ctx.arrayPrototype
    )
    given JSContext = ctx
    initConstructor(arrayConstructor, length = 1)
    ctx.global.set("Array", JSValue.Native(arrayConstructor))
    ctx.arrayPrototype.defineProperty("constructor", JSValue.Native(arrayConstructor), enumerable = false)(using ctx)

    val arrayIsArray = NativeFunction(
      name = "isArray",
      impl = (args, _) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then
          JSValue.Bool(false)
        else
          JSValue.fromBoolean(args(offset).isInstanceOf[JSValue.JSArrayVal])
    )

    val arrayOf = NativeFunction(
      name = "of",
      impl = (args, _) =>
        val offset = if args.length >= 2 then 1 else 0
        val arr = quickjs.objmodel.JSArray.empty()
        var i = offset
        while i < args.length do
          arr.push(args(i))
          i += 1
        JSValue.JSArrayVal(arr)
    )

    val arrayFrom = NativeFunction(
      name = "from",
      impl = (args, ctx) =>
        val offset = if args.length >= 2 then 1 else 0
        if args.length <= offset then
          JSValue.JSArrayVal(quickjs.objmodel.JSArray.empty())
        else
          val source = args(offset)
          val mapFn = if args.length > offset + 1 then Some(args(offset + 1)) else None
          val thisArg = if args.length > offset + 2 then args(offset + 2) else JSValue.Undefined
          val result = quickjs.objmodel.JSArray.empty()
          given JSContext = ctx

          def pushValue(value: JSValue, index: Int): Unit =
            val mapped =
              mapFn match
                case Some(func) =>
                  callFunctionWithThis(func, thisArg, Array(value, JSValue.fromInt(index), source))
                case None => value
            result.push(mapped)

          source match
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do
                pushValue(arr.get(i), i)
                i += 1
            case JSValue.JSStr(str) =>
              var i = 0
              while i < str.length do
                pushValue(JSValue.fromString(str.charAt(i).toString), i)
                i += 1
            case JSValue.Object(obj) =>
              val len = obj.get("length").toNumber.toInt
              var i = 0
              while i < len do
                pushValue(obj.get(i.toString), i)
                i += 1
            case func: JSValue.Function =>
              val len = func.funcObj.get("length").toNumber.toInt
              var i = 0
              while i < len do
                pushValue(func.funcObj.get(i.toString), i)
                i += 1
            case _ => ()

          JSValue.JSArrayVal(result)
    )

    arrayConstructor.funcObj.set("isArray", JSValue.Native(arrayIsArray))
    arrayConstructor.funcObj.set("of", JSValue.Native(arrayOf))
    arrayConstructor.funcObj.set("from", JSValue.Native(arrayFrom))

  /** Initialize Array.prototype methods */
  def initializeArrayPrototype(ctx: JSContext): Unit =
    def strictEquals(a: JSValue, b: JSValue): Boolean = (a, b) match
      case (JSValue.Int32(x), JSValue.Int32(y)) => x == y
      case (JSValue.Float64(x), JSValue.Float64(y)) =>
        !x.isNaN && !y.isNaN && x == y
      case (JSValue.Int32(x), JSValue.Float64(y)) =>
        !y.isNaN && x.toDouble == y
      case (JSValue.Float64(x), JSValue.Int32(y)) =>
        !x.isNaN && x == y.toDouble
      case _ => a == b

    def sameValueZero(a: JSValue, b: JSValue): Boolean = (a, b) match
      case (JSValue.Float64(x), JSValue.Float64(y)) if x.isNaN && y.isNaN => true
      case _ => strictEquals(a, b)
    // Array.prototype.push(element1, ..., elementN)
    // Appends elements to the end of an array and returns the new length
    val arrayPrototypePush = NativeFunction(
      name = "push",
      impl = (args, ctx) =>
        // args(0) is the array (this value)
        // args(1...) are the elements to push
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.push called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              // Add each element to the array using the push method
              for i <- 1 until args.length do
                arr.push(args(i))
              JSValue.fromInt(arr.getLength)
            case _ =>
              throw new RuntimeException(s"Array.prototype.push called on non-array: $arrValue")
    )

    // Array.prototype.map(callback)
    // Creates a new array with the results of calling a provided function on every element
    val arrayPrototypeMap = NativeFunction(
      name = "map",
      impl = (args, ctx) =>
        // args(0) is the array (this value)
        // args(1) is the callback function
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.map requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val resultArr = quickjs.objmodel.JSArray.empty()
              var index = 0

              // Call callback for each element
              while index < arr.getLength do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                val result = callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx)
                resultArr.push(result)
                index += 1

              JSValue.JSArrayVal(resultArr)
            case _ =>
              throw new RuntimeException(s"Array.prototype.map called on non-array: $arrValue")
    )

    val arrayPrototypeFilter = NativeFunction(
      name = "filter",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.filter requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val resultArr = quickjs.objmodel.JSArray.empty()
              var index = 0
              while index < arr.getLength do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                val keep = callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx).toBoolean
                if keep then resultArr.push(elem)
                index += 1
              JSValue.JSArrayVal(resultArr)
            case _ =>
              throw new RuntimeException(s"Array.prototype.filter called on non-array: $arrValue")
    )

    val arrayPrototypeForEach = NativeFunction(
      name = "forEach",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.forEach requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              var index = 0
              while index < arr.getLength do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx)
                index += 1
              JSValue.Undefined
            case _ =>
              throw new RuntimeException(s"Array.prototype.forEach called on non-array: $arrValue")
    )

    val arrayPrototypeReduce = NativeFunction(
      name = "reduce",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.reduce requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val len = arr.getLength
              val hasInitial = args.length > 2
              if len == 0 && !hasInitial then
                throw new RuntimeException("TypeError: Reduce of empty array with no initial value")
              var acc =
                if hasInitial then args(2)
                else arr.get(0)
              var index = if hasInitial then 0 else 1
              while index < len do
                val elem = arr.get(index)
                val callbackArgs = Array(acc, elem, JSValue.fromInt(index), arrVal)
                acc = callFunctionWithThis(callback, JSValue.Undefined, callbackArgs)(using ctx)
                index += 1
              acc
            case _ =>
              throw new RuntimeException(s"Array.prototype.reduce called on non-array: $arrValue")
    )

    val arrayPrototypeIncludes = NativeFunction(
      name = "includes",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.includes called on non-array")
        else
          val arrValue = args(0)
          val search = if args.length > 1 then args(1) else JSValue.Undefined
          val fromIndex =
            if args.length > 2 then args(2).toNumber.toInt
            else 0
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              var k = if fromIndex < 0 then math.max(len + fromIndex, 0) else fromIndex
              var found = false
              while k < len && !found do
                if sameValueZero(arr.get(k), search) then
                  found = true
                k += 1
              JSValue.fromBoolean(found)
            case _ =>
              throw new RuntimeException(s"Array.prototype.includes called on non-array: $arrValue")
    )

    val arrayPrototypeIndexOf = NativeFunction(
      name = "indexOf",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.indexOf called on non-array")
        else
          val arrValue = args(0)
          val search = if args.length > 1 then args(1) else JSValue.Undefined
          val fromIndex =
            if args.length > 2 then args(2).toNumber.toInt
            else 0
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              var k = if fromIndex < 0 then math.max(len + fromIndex, 0) else fromIndex
              var idx = -1
              while k < len && idx < 0 do
                if strictEquals(arr.get(k), search) then
                  idx = k
                k += 1
              JSValue.fromInt(idx)
            case _ =>
              throw new RuntimeException(s"Array.prototype.indexOf called on non-array: $arrValue")
    )

    val arrayPrototypeEvery = NativeFunction(
      name = "every",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.every requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              var index = 0
              var passed = true
              while index < arr.getLength && passed do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                passed = callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx).toBoolean
                index += 1
              JSValue.fromBoolean(passed)
            case _ =>
              throw new RuntimeException(s"Array.prototype.every called on non-array: $arrValue")
    )

    val arrayPrototypeSome = NativeFunction(
      name = "some",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.some requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              var index = 0
              var found = false
              while index < arr.getLength && !found do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                found = callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx).toBoolean
                index += 1
              JSValue.fromBoolean(found)
            case _ =>
              throw new RuntimeException(s"Array.prototype.some called on non-array: $arrValue")
    )

    val arrayPrototypeFind = NativeFunction(
      name = "find",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.find requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              var index = 0
              var found: JSValue = JSValue.Undefined
              var done = false
              while index < arr.getLength && !done do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                if callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx).toBoolean then
                  found = elem
                  done = true
                index += 1
              found
            case _ =>
              throw new RuntimeException(s"Array.prototype.find called on non-array: $arrValue")
    )

    val arrayPrototypeFindIndex = NativeFunction(
      name = "findIndex",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.findIndex requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          val thisArg = if args.length > 2 then args(2) else JSValue.Undefined
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              var index = 0
              var found = -1
              while index < arr.getLength && found < 0 do
                val elem = arr.get(index)
                val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                if callFunctionWithThis(callback, thisArg, callbackArgs)(using ctx).toBoolean then
                  found = index
                index += 1
              JSValue.fromInt(found)
            case _ =>
              throw new RuntimeException(s"Array.prototype.findIndex called on non-array: $arrValue")
    )

    val arrayPrototypeReverse = NativeFunction(
      name = "reverse",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.reverse called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              var i = 0
              while i < len / 2 do
                val left = arr.get(i)
                val right = arr.get(len - 1 - i)
                arr.set(i, right)
                arr.set(len - 1 - i, left)
                i += 1
              arrVal
            case _ =>
              throw new RuntimeException(s"Array.prototype.reverse called on non-array: $arrValue")
    )

    val arrayPrototypeFill = NativeFunction(
      name = "fill",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.fill called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              val value = if args.length > 1 then args(1) else JSValue.Undefined
              val startRaw = if args.length > 2 then args(2).toNumber.toInt else 0
              val endRaw = if args.length > 3 then args(3).toNumber.toInt else len
              val start = if startRaw < 0 then math.max(len + startRaw, 0) else math.min(startRaw, len)
              val end = if endRaw < 0 then math.max(len + endRaw, 0) else math.min(endRaw, len)
              var i = start
              while i < end do
                arr.set(i, value)
                i += 1
              arrVal
            case _ =>
              throw new RuntimeException(s"Array.prototype.fill called on non-array: $arrValue")
    )

    val arrayPrototypeAt = NativeFunction(
      name = "at",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.at called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              val indexRaw = if args.length > 1 then args(1).toNumber.toInt else 0
              val index = if indexRaw < 0 then len + indexRaw else indexRaw
              if index < 0 || index >= len then JSValue.Undefined else arr.get(index)
            case _ =>
              throw new RuntimeException(s"Array.prototype.at called on non-array: $arrValue")
    )

    val arrayPrototypeCopyWithin = NativeFunction(
      name = "copyWithin",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.copyWithin called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              val targetRaw = if args.length > 1 then args(1).toNumber.toInt else 0
              val startRaw = if args.length > 2 then args(2).toNumber.toInt else 0
              val endRaw = if args.length > 3 then args(3).toNumber.toInt else len
              val target = if targetRaw < 0 then math.max(len + targetRaw, 0) else math.min(targetRaw, len)
              val start = if startRaw < 0 then math.max(len + startRaw, 0) else math.min(startRaw, len)
              val end = if endRaw < 0 then math.max(len + endRaw, 0) else math.min(endRaw, len)
              val count = math.min(end - start, len - target)
              if count > 0 then
                val direction =
                  if start < target && target < start + count then -1 else 1
                var i = if direction > 0 then 0 else count - 1
                while i >= 0 && i < count do
                  val value = arr.get(start + i)
                  arr.set(target + i, value)
                  i += direction
              arrVal
            case _ =>
              throw new RuntimeException(s"Array.prototype.copyWithin called on non-array: $arrValue")
    )

    val arrayPrototypeSplice = NativeFunction(
      name = "splice",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.splice called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              val startRaw = if args.length > 1 then args(1).toNumber.toInt else 0
              val actualStart =
                if startRaw < 0 then math.max(len + startRaw, 0)
                else math.min(startRaw, len)
              val deleteCountRaw =
                if args.length > 2 then args(2).toNumber.toInt
                else len - actualStart
              val actualDelete = math.max(0, math.min(deleteCountRaw, len - actualStart))
              val items =
                if args.length > 3 then args.slice(3, args.length).toSeq
                else Seq.empty
              val removed = arr.splice(actualStart, actualDelete, items)
              JSValue.JSArrayVal(removed)
            case _ =>
              throw new RuntimeException(s"Array.prototype.splice called on non-array: $arrValue")
    )

    val arrayPrototypeShift = NativeFunction(
      name = "shift",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.shift called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val len = arr.getLength
              if len == 0 then
                JSValue.Undefined
              else
                val first = arr.get(0)
                var i = 1
                while i < len do
                  arr.set(i - 1, arr.get(i))
                  i += 1
                arr.setLength(len - 1)
                first
            case _ =>
              throw new RuntimeException(s"Array.prototype.shift called on non-array: $arrValue")
    )

    val arrayPrototypeUnshift = NativeFunction(
      name = "unshift",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.unshift called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val elementsToAdd =
                if args.length > 1 then args.slice(1, args.length)
                else Array.empty[JSValue]
              val len = arr.getLength
              val addCount = elementsToAdd.length
              var i = len - 1
              while i >= 0 do
                arr.set(i + addCount, arr.get(i))
                i -= 1
              var j = 0
              while j < addCount do
                arr.set(j, elementsToAdd(j))
                j += 1
              JSValue.fromInt(arr.getLength)
            case _ =>
              throw new RuntimeException(s"Array.prototype.unshift called on non-array: $arrValue")
    )

    // Array.prototype.pop()
    // Removes the last element from an array and returns that element
    val arrayPrototypePop = NativeFunction(
      name = "pop",
      impl = (args, ctx) =>
        // args(0) is the array (this value)
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.pop called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              arr.pop()
            case _ =>
              throw new RuntimeException(s"Array.prototype.pop called on non-array: $arrValue")
    )

    val arrayPrototypeReduceRight = NativeFunction(
      name = "reduceRight",
      impl = (args, ctx) =>
        if args.length < 2 then
          throw new RuntimeException("Array.prototype.reduceRight requires a callback function")
        else
          val arrValue = args(0)
          val callback = args(1)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val len = arr.getLength
              val hasInitial = args.length > 2
              if len == 0 && !hasInitial then
                throw new RuntimeException("TypeError: Reduce of empty array with no initial value")
              var acc =
                if hasInitial then args(2)
                else arr.get(len - 1)
              var index = if hasInitial then len - 1 else len - 2
              while index >= 0 do
                val elem = arr.get(index)
                val callbackArgs = Array(acc, elem, JSValue.fromInt(index), arrVal)
                acc = callFunctionWithThis(callback, JSValue.Undefined, callbackArgs)(using ctx)
                index -= 1
              acc
            case _ =>
              throw new RuntimeException(s"Array.prototype.reduceRight called on non-array: $arrValue")
    )

    val arrayPrototypeSort = NativeFunction(
      name = "sort",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.sort called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val len = arr.getLength
              val compareFn = if args.length > 1 then Some(args(1)) else None
              val values = new Array[JSValue](len)
              var i = 0
              while i < len do
                values(i) = arr.get(i)
                i += 1
              def compareValues(a: JSValue, b: JSValue): Int =
                compareFn match
                  case Some(func) =>
                    val result = callFunctionWithThis(func, JSValue.Undefined, Array(a, b))(using ctx)
                    val num = result.toNumber
                    if num.isNaN then 0
                    else if num < 0 then -1
                    else if num > 0 then 1
                    else 0
                  case None =>
                    a.toString.compareTo(b.toString)
              Sorting.stableSort(values, (a, b) => compareValues(a, b) < 0)
              i = 0
              while i < len do
                arr.set(i, values(i))
                i += 1
              arrVal
            case _ =>
              throw new RuntimeException(s"Array.prototype.sort called on non-array: $arrValue")
    )

    // Array.prototype.toString()
    // Joins elements with commas
    val arrayPrototypeToString = NativeFunction(
      name = "toString",
      impl = (args, ctx) =>
        if args.isEmpty then
          JSValue.fromString("")
        else
          args(0) match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val sb = new StringBuilder()
              var i = 0
              while i < arr.getLength do
                if i > 0 then sb.append(",")
                sb.append(arr.get(i).toString)
                i += 1
              JSValue.fromString(sb.toString)
            case _ =>
              JSValue.fromString("")
    )

    val arrayPrototypeJoin = NativeFunction(
      name = "join",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.join called on non-array")
        else
          val arrValue = args(0)
          val separator =
            if args.length > 1 && args(1) != JSValue.Undefined then args(1).toString else ","
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val sb = new StringBuilder()
              var i = 0
              while i < arr.getLength do
                if i > 0 then sb.append(separator)
                val elem = arr.get(i)
                elem match
                  case JSValue.Undefined | JSValue.Null => ()
                  case _ => sb.append(elem.toString)
                i += 1
              JSValue.fromString(sb.toString)
            case _ =>
              throw new RuntimeException(s"Array.prototype.join called on non-array: $arrValue")
    )

    // Array.prototype.concat(value1, value2, ..., valueN)
    // Returns a new array comprised of this array joined with other array(s) and/or value(s)
    val arrayPrototypeConcat = NativeFunction(
      name = "concat",
      impl = (args, ctx) =>
        // args(0) is the array (this value)
        // args(1...) are values/arrays to concatenate
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.concat called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val resultArr = quickjs.objmodel.JSArray.empty()

              // Copy all elements from this array
              var i = 0
              while i < arr.getLength do
                resultArr.push(arr.get(i))
                i += 1

              // Concatenate additional arguments
              for j <- 1 until args.length do
                args(j) match
                  case otherArr: JSValue.JSArrayVal =>
                    // Concatenate array elements
                    var k = 0
                    while k < otherArr.value.getLength do
                      resultArr.push(otherArr.value.get(k))
                      k += 1
                  case elem =>
                    // Concatenate single element
                    resultArr.push(elem)

              JSValue.JSArrayVal(resultArr)
            case _ =>
              throw new RuntimeException(s"Array.prototype.concat called on non-array: $arrValue")
    )

    // Array.prototype.slice(begin, end)
    // Returns a shallow copy of a portion of an array
    val arrayPrototypeSlice = NativeFunction(
      name = "slice",
      impl = (args, ctx) =>
        // args(0) is the array (this value)
        // args(1) is begin (optional)
        // args(2) is end (optional)
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.slice called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val length = arr.getLength

              // Parse begin parameter
              val begin = if args.length > 1 then
                args(1) match
                  case JSValue.Int32(i) => i
                  case JSValue.Float64(d) => d.toInt
                  case _ => 0
              else
                0

              // Handle negative begin
              val start = if begin < 0 then
                val normalized = length + begin
                if normalized < 0 then 0 else normalized
              else
                if begin > length then length else begin

              // Parse end parameter
              val end = if args.length > 2 then
                args(2) match
                  case JSValue.Int32(i) => i
                  case JSValue.Float64(d) => d.toInt
                  case _ => length
              else
                length

              // Handle negative end
              val stop = if end < 0 then
                val normalized = length + end
                if normalized < 0 then 0 else normalized
              else
                if end > length then length else end

              // Create result array with sliced elements
              val resultArr = quickjs.objmodel.JSArray.empty()
              var i = start
              while i < stop do
                resultArr.push(arr.get(i))
                i += 1

              JSValue.JSArrayVal(resultArr)
            case _ =>
              throw new RuntimeException(s"Array.prototype.slice called on non-array: $arrValue")
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
    ctx.arrayPrototype.set("copyWithin", JSValue.Native(arrayPrototypeCopyWithin))
    ctx.arrayPrototype.set("splice", JSValue.Native(arrayPrototypeSplice))
    ctx.arrayPrototype.set("shift", JSValue.Native(arrayPrototypeShift))
    ctx.arrayPrototype.set("unshift", JSValue.Native(arrayPrototypeUnshift))
    ctx.arrayPrototype.set("toString", JSValue.Native(arrayPrototypeToString))
    ctx.arrayPrototype.set("reduceRight", JSValue.Native(arrayPrototypeReduceRight))
    ctx.arrayPrototype.set("sort", JSValue.Native(arrayPrototypeSort))
    ctx.arrayPrototype.set("join", JSValue.Native(arrayPrototypeJoin))
    ctx.arrayPrototype.set("concat", JSValue.Native(arrayPrototypeConcat))
    ctx.arrayPrototype.set("slice", JSValue.Native(arrayPrototypeSlice))

    // Array.prototype.flat(depth)
    val arrayPrototypeFlat = NativeFunction(
      name = "flat",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.flat called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              // Default depth is 1
              val depth = if args.length > 1 then
                args(1) match
                  case JSValue.Int32(d) => d
                  case JSValue.Float64(d) => d.toInt
                  case JSValue.Undefined => 1
                  case _ => 1
              else 1

              def flattenArray(source: quickjs.objmodel.JSArray, currentDepth: Int): quickjs.objmodel.JSArray =
                val result = quickjs.objmodel.JSArray.empty()
                val len = source.getLength
                for i <- 0 until len do
                  source.get(i) match
                    case inner: JSValue.JSArrayVal if currentDepth > 0 =>
                      val flattened = flattenArray(inner.value, currentDepth - 1)
                      val flatLen = flattened.getLength
                      for j <- 0 until flatLen do
                        result.push(flattened.get(j))
                    case v => result.push(v)
                result

              JSValue.JSArrayVal(flattenArray(arr, depth))
            case _ =>
              ctx.throwTypeError("Array.prototype.flat called on non-array")
    )
    ctx.arrayPrototype.set("flat", JSValue.Native(arrayPrototypeFlat))

    // Array.prototype.flatMap(callback, thisArg)
    val arrayPrototypeFlatMap = NativeFunction(
      name = "flatMap",
      impl = (args, ctx) =>
        if args.isEmpty then
          throw new RuntimeException("Array.prototype.flatMap called on non-array")
        else
          val arrValue = args(0)
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val callback = if args.length > 1 then args(1) else JSValue.Undefined
              val thisArg = if args.length > 2 then args(2) else JSValue.Undefined

              val result = quickjs.objmodel.JSArray.empty()
              val interpreter = Interpreter()
              val len = arr.getLength

              for i <- 0 until len do
                val elem = arr.get(i)
                val callArgs = Array[JSValue](elem, JSValue.fromInt(i), arrValue)
                val mapped = callback match
                  case f: JSValue.Function =>
                    val bcFunc = new BytecodeFunction(
                      name = f.name, bytecode = f.bytecode, constants = f.constants,
                      stackSize = f.stackSize, freeVars = Array.empty, paramNames = f.paramNames,
                      localVarNames = f.localVarNames, argumentsIndex = f.argumentsIndex,
                      isConstructor = f.isConstructor
                    )
                    interpreter.call(bcFunc, thisArg, callArgs, f.closure)
                  case JSValue.Native(nf: NativeFunction) =>
                    val argsWithThis = new Array[JSValue](callArgs.length + 1)
                    argsWithThis(0) = thisArg
                    Array.copy(callArgs, 0, argsWithThis, 1, callArgs.length)
                    nf.call(argsWithThis)
                  case _ =>
                    ctx.throwTypeError("flatMap callback is not a function")

                // Flatten one level
                mapped match
                  case inner: JSValue.JSArrayVal =>
                    val innerLen = inner.value.getLength
                    for j <- 0 until innerLen do
                      result.push(inner.value.get(j))
                  case v => result.push(v)

              JSValue.JSArrayVal(result)
            case _ =>
              ctx.throwTypeError("Array.prototype.flatMap called on non-array")
    )
    ctx.arrayPrototype.set("flatMap", JSValue.Native(arrayPrototypeFlatMap))

  // ============================================================
  // Map Implementation
  // ============================================================

  /** Internal storage class for Map - uses AnyRef wrapper for proper key comparison */
