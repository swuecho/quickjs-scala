package quickjs.runtime

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.interpreter.Interpreter
import quickjs.bytecode.BytecodeFunction

/** Standard library initialization.
  *
  * Initializes built-in methods like Function.prototype.call, etc.
  * This is in a separate module to avoid circular dependencies between core and runtime.
  */
object StdLib:
  /** Initialize Function.prototype methods */
  def initializeFunctionPrototype(ctx: JSContext): Unit =
    // Function.prototype.call(thisArg, arg1, arg2, ...)
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
                localVarNames = f.localVarNames
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

  /** Initialize Array.prototype methods */
  def initializeArrayPrototype(ctx: JSContext): Unit =
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
          arrValue match
            case arrVal: JSValue.JSArrayVal =>
              given JSContext = ctx
              val arr = arrVal.value
              val resultArr = quickjs.objmodel.JSArray.empty()
              var index = 0

              // Call callback for each element
              while index < arr.getLength do
                val elem = arr.get(index)
                callback match
                  case func: JSValue.Function =>
                    // Call the callback with (element, index, array)
                    val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                    val interpreter = Interpreter()
                    val bcFunc = new BytecodeFunction(
                      name = func.name,
                      bytecode = func.bytecode,
                      constants = func.constants,
                      stackSize = func.stackSize,
                      freeVars = Array.empty,
                      paramNames = func.paramNames,
                      localVarNames = func.localVarNames
                    )
                    val result = interpreter.call(bcFunc, JSValue.Undefined, callbackArgs, func.closure)
                    resultArr.push(result)
                  case JSValue.Native(nativeFuncWrapper) =>
                    // Call native callback
                    val callbackArgs = Array(elem, JSValue.fromInt(index), arrVal)
                    nativeFuncWrapper match
                      case native: NativeFunction =>
                        val result = native.call(callbackArgs)
                        resultArr.push(result)
                      case _ =>
                        throw new RuntimeException(s"Invalid callback function: $nativeFuncWrapper")
                  case _ =>
                    throw new RuntimeException(s"Array.prototype.map callback is not a function: $callback")
                index += 1

              JSValue.JSArrayVal(resultArr)
            case _ =>
              throw new RuntimeException(s"Array.prototype.map called on non-array: $arrValue")
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
    ctx.arrayPrototype.set("concat", JSValue.Native(arrayPrototypeConcat))
    ctx.arrayPrototype.set("slice", JSValue.Native(arrayPrototypeSlice))

  /** Initialize all standard library methods */
  def initialize(ctx: JSContext): Unit =
    initializeFunctionPrototype(ctx)
    initializeArrayPrototype(ctx)
