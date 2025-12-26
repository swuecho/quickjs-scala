package quickjs.stdlib

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject
import quickjs.objmodel.JSArray
import scala.collection.mutable.ArrayBuffer

/** JavaScript Array object with methods.
  *
  * Provides arr.push(elem1, elem2, ...), arr.pop(), etc.
  * Uses proper 'this' binding for method calls.
  */
object ArrayStatics:
  /** Initialize Array methods */
  def initialize()(using ctx: JSContext): Unit =
    val arrayObj = JSObject(prototype = ctx.arrayPrototype, extensible = true)

    // arr.push(elem1, elem2, ...) - adds elements to end of array
    // 'this' is passed as args(0), actual arguments start at args(1)
    val pushFunc = NativeFunction("push", (args, context) =>
      if args.isEmpty then
        JSValue.fromInt(0)
      else
        // args(0) is 'this' (the array)
        args(0) match
          case arrVal: JSValue.JSArrayVal =>
            val arr = arrVal.value
            val elementsToAdd = args.drop(1)
            for elem <- elementsToAdd do
              arr.push(elem)
            JSValue.fromInt(arr.length)
          case _ =>
            JSValue.fromInt(0)
    )
    arrayObj.set("push", JSValue.Native(pushFunc))

    // arr.pop() - removes and returns last element
    // 'this' is passed as args(0)
    val popFunc = NativeFunction("pop", (args, context) =>
      if args.isEmpty then
        JSValue.Undefined
      else
        // args(0) is 'this' (the array)
        args(0) match
          case arrVal: JSValue.JSArrayVal =>
            arrVal.value.pop()
          case _ =>
            JSValue.Undefined
    )
    arrayObj.set("pop", JSValue.Native(popFunc))

    // arr.map(callback) - transform each element
    val mapFunc = NativeFunction("map", (args, context) =>
      if args.length < 2 then
        JSValue.Undefined
      else
        args(0) match
          case arrVal: JSValue.JSArrayVal =>
            val arr = arrVal.value
            val callback = args(1)

            // Create new array with transformed elements
            val newArr = JSArray.empty()
            for i <- 0 until arr.getLength do
              val elem = arr.get(i)
              // Call callback(element, index, array)
              callback match
                case func: JSValue.Function =>
                  val bcFunc = new quickjs.interpreter.BytecodeFunction(
                    name = func.name,
                    bytecode = func.bytecode,
                    constants = func.constants,
                    stackSize = func.stackSize,
                    freeVars = Array.empty,
                    paramNames = func.paramNames
                  )
                  val interp = quickjs.interpreter.Interpreter()
                  val callbackArgs = Array(elem, JSValue.fromInt(i), arrVal)
                  val result = interp.call(bcFunc, arrVal, callbackArgs, func.closure)
                  newArr.push(result)
                case _ =>
                  newArr.push(elem)

            JSValue.JSArrayVal(newArr)
          case _ =>
            JSValue.Undefined
    )
    arrayObj.set("map", JSValue.Native(mapFunc))

    // arr.filter(callback) - keep elements matching predicate
    val filterFunc = NativeFunction("filter", (args, context) =>
      if args.length < 2 then
        JSValue.Undefined
      else
        args(0) match
          case arrVal: JSValue.JSArrayVal =>
            val arr = arrVal.value
            val callback = args(1)

            // Create new array with filtered elements
            val newArr = JSArray.empty()
            for i <- 0 until arr.getLength do
              val elem = arr.get(i)
              // Call callback(element, index, array)
              callback match
                case func: JSValue.Function =>
                  val bcFunc = new quickjs.interpreter.BytecodeFunction(
                    name = func.name,
                    bytecode = func.bytecode,
                    constants = func.constants,
                    stackSize = func.stackSize,
                    freeVars = Array.empty,
                    paramNames = func.paramNames
                  )
                  val interp = quickjs.interpreter.Interpreter()
                  val callbackArgs = Array(elem, JSValue.fromInt(i), arrVal)
                  val result = interp.call(bcFunc, arrVal, callbackArgs, func.closure)
                  // If callback returns truthy value, keep element
                  if result.toBoolean then
                    newArr.push(elem)
                case _ =>
                  ()

            JSValue.JSArrayVal(newArr)
          case _ =>
            JSValue.Undefined
    )
    arrayObj.set("filter", JSValue.Native(filterFunc))

    // arr.reduce(callback, initialValue) - reduce to single value
    val reduceFunc = NativeFunction("reduce", (args, context) =>
      if args.length < 2 then
        JSValue.Undefined
      else
        args(0) match
          case arrVal: JSValue.JSArrayVal =>
            val arr = arrVal.value
            val callback = args(1)
            val hasInitial = args.length >= 3

            var accumulator = if hasInitial then args(2) else arr.get(0)
            val startIndex = if hasInitial then 0 else 1

            for i <- startIndex until arr.getLength do
              val elem = arr.get(i)
              callback match
                case func: JSValue.Function =>
                  val bcFunc = new quickjs.interpreter.BytecodeFunction(
                    name = func.name,
                    bytecode = func.bytecode,
                    constants = func.constants,
                    stackSize = func.stackSize,
                    freeVars = Array.empty,
                    paramNames = func.paramNames
                  )
                  val interp = quickjs.interpreter.Interpreter()
                  val callbackArgs = Array(accumulator, elem, JSValue.fromInt(i), arrVal)
                  accumulator = interp.call(bcFunc, arrVal, callbackArgs, func.closure)
                case _ =>
                  ()

            accumulator
          case _ =>
            JSValue.Undefined
    )
    arrayObj.set("reduce", JSValue.Native(reduceFunc))

    // arr.slice(start, end) - extract portion of array
    val sliceFunc = NativeFunction("slice", (args, context) =>
      if args.isEmpty then
        JSValue.JSArrayVal(JSArray.empty())
      else
        args(0) match
          case arrVal: JSValue.JSArrayVal =>
            val arr = arrVal.value
            val len = arr.getLength

            val start = if args.length >= 2 then
              normalizeIndex(args(1).toNumber.toInt, len)
            else
              0

            val end = if args.length >= 3 then
              normalizeIndex(args(2).toNumber.toInt, len)
            else
              len

            // Create new array with sliced elements
            val newArr = JSArray.empty()
            for i <- start until Math.min(end, len) if i >= 0 do
              newArr.push(arr.get(i))

            JSValue.JSArrayVal(newArr)
          case _ =>
            JSValue.Undefined
    )
    arrayObj.set("slice", JSValue.Native(sliceFunc))

    // arr.forEach(callback) - call function for each element
    val forEachFunc = NativeFunction("forEach", (args, context) =>
      if args.length < 2 then
        JSValue.Undefined
      else
        args(0) match
          case arrVal: JSValue.JSArrayVal =>
            val arr = arrVal.value
            val callback = args(1)

            for i <- 0 until arr.getLength do
              val elem = arr.get(i)
              // Call callback(element, index, array)
              callback match
                case func: JSValue.Function =>
                  val bcFunc = new quickjs.interpreter.BytecodeFunction(
                    name = func.name,
                    bytecode = func.bytecode,
                    constants = func.constants,
                    stackSize = func.stackSize,
                    freeVars = Array.empty,
                    paramNames = func.paramNames
                  )
                  val interp = quickjs.interpreter.Interpreter()
                  val callbackArgs = Array(elem, JSValue.fromInt(i), arrVal)
                  interp.call(bcFunc, arrVal, callbackArgs, func.closure)
                case _ =>
                  ()

            JSValue.Undefined
          case _ =>
            JSValue.Undefined
    )
    arrayObj.set("forEach", JSValue.Native(forEachFunc))

    // arr.shift() - removes and returns first element
    val shiftFunc = NativeFunction("shift", (args, context) =>
      if args.isEmpty then
        JSValue.Undefined
      else
        args(0) match
          case arrVal: JSValue.JSArrayVal =>
            val arr = arrVal.value
            if arr.getLength > 0 then
              val first = arr.get(0)
              // Shift all elements down
              for i <- 0 until (arr.getLength - 1) do
                arr.set(i, arr.get(i + 1))
              // Remove last element
              arr.length = arr.getLength - 1
              first
            else
              JSValue.Undefined
          case _ =>
            JSValue.Undefined
    )
    arrayObj.set("shift", JSValue.Native(shiftFunc))

    // arr.unshift(elem1, elem2, ...) - adds elements to beginning of array
    val unshiftFunc = NativeFunction("unshift", (args, context) =>
      if args.isEmpty then
        JSValue.fromInt(0)
      else
        args(0) match
          case arrVal: JSValue.JSArrayVal =>
            val arr = arrVal.value
            val elementsToAdd = args.drop(1)
            val oldLen = arr.getLength

            // Make room for new elements at the beginning
            for i <- (oldLen - 1) to 0 by -1 do
              arr.set(i + elementsToAdd.length, arr.get(i))

            // Add new elements at the beginning
            for (elem, i) <- elementsToAdd.zipWithIndex do
              arr.set(i, elem)

            JSValue.fromInt(arr.length)
          case _ =>
            JSValue.fromInt(0)
    )
    arrayObj.set("unshift", JSValue.Native(unshiftFunc))

    // arr.concat(other1, other2, ...) - concatenate arrays
    val concatFunc = NativeFunction("concat", (args, context) =>
      if args.isEmpty then
        JSValue.JSArrayVal(JSArray.empty())
      else
        args(0) match
          case arrVal: JSValue.JSArrayVal =>
            val arr = arrVal.value
            val newArr = JSArray.empty()

            // Add elements from this array
            for i <- 0 until arr.getLength do
              newArr.push(arr.get(i))

            // Add elements from other arrays
            for elem <- args.drop(1) do
              elem match
                case otherArr: JSValue.JSArrayVal =>
                  for i <- 0 until otherArr.value.getLength do
                    newArr.push(otherArr.value.get(i))
                case other =>
                  newArr.push(other)

            JSValue.JSArrayVal(newArr)
          case _ =>
            JSValue.Undefined
    )
    arrayObj.set("concat", JSValue.Native(concatFunc))

    ctx.global.set("Array", JSValue.Object(arrayObj))

  /** Helper to normalize negative indices */
  private def normalizeIndex(index: Int, length: Int): Int =
    if index < 0 then
      Math.max(0, length + index)
    else
      Math.min(index, length)
