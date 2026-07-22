package quickjs.stdlib

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject
import quickjs.objmodel.JSArray
import quickjs.bytecode.BytecodeFunction
import quickjs.interpreter.Interpreter

/** JavaScript Array object with methods.
  *
  * Provides arr.push(elem1, elem2, ...), arr.pop(), etc. Uses proper 'this'
  * binding for method calls.
  */
object ArrayStatics {
  import NativeFunctionBuilder.*

  /** Initialize Array methods */
  def initialize()(using ctx: JSContext): Unit = {
    val arrayObj = JSObject(prototype = ctx.arrayPrototype, extensible = true)

    // Simple array methods using the builder
    arrayObj.set("pop", JSValue.Native(arrayMethod("pop")(_.value.pop())))
    arrayObj.set("shift", JSValue.Native(arrayMethod("shift")(shiftImpl)))

    // Array methods with additional arguments
    arrayObj.set("push", JSValue.Native(arrayMethodWithArgs("push")(pushImpl)))
    arrayObj.set(
      "unshift",
      JSValue.Native(arrayMethodWithArgs("unshift")(unshiftImpl))
    )
    arrayObj.set(
      "slice",
      JSValue.Native(arrayMethodWithArgs("slice")(sliceImpl))
    )
    arrayObj.set(
      "concat",
      JSValue.Native(arrayMethodWithArgs("concat")(concatImpl))
    )

    // Complex array methods with callbacks (map, filter, forEach, reduce)
    arrayObj.set("map", JSValue.Native(arrayCallbackMethod("map")(mapImpl)))
    arrayObj.set(
      "filter",
      JSValue.Native(arrayCallbackMethod("filter")(filterImpl))
    )
    arrayObj.set(
      "forEach",
      JSValue.Native(arrayCallbackMethod("forEach")(forEachImpl))
    )
    arrayObj.set(
      "reduce",
      JSValue.Native(arrayReduceMethod("reduce")(reduceImpl))
    )

    ctx.global.set("Array", JSValue.Object(arrayObj))
  }

  // Simple implementations
  private def shiftImpl(arrVal: JSValue.JSArrayVal): JSValue = {
    val arr = arrVal.value
    if arr.getLength > 0 then {
      val first = arr.get(0)
      // Shift all elements down
      for i <- 0 until (arr.getLength - 1) do arr.set(i, arr.get(i + 1))
      // Remove last element
      arr.setLength(arr.getLength - 1)
      first
    } else JSValue.Undefined
  }

  // Array method with arguments implementations
  private def pushImpl(
      arrVal: JSValue.JSArrayVal,
      args: Array[JSValue]
  ): JSValue = {
    val arr = arrVal.value
    val elementsToAdd = args.drop(1)
    for elem <- elementsToAdd do arr.push(elem)
    arr.getLengthValue
  }

  private def unshiftImpl(
      arrVal: JSValue.JSArrayVal,
      args: Array[JSValue]
  ): JSValue = {
    val arr = arrVal.value
    val elementsToAdd = args.drop(1)
    val oldLen = arr.getLength

    // Make room for new elements at the beginning
    for i <- (oldLen - 1) to 0 by -1 do
      arr.set(i + elementsToAdd.length, arr.get(i))

    // Add new elements at the beginning
    for (elem, i) <- elementsToAdd.zipWithIndex do arr.set(i, elem)

    arr.getLengthValue
  }

  private def sliceImpl(
      arrVal: JSValue.JSArrayVal,
      args: Array[JSValue]
  ): JSValue = {
    val arr = arrVal.value
    val len = arr.getLength

    val start =
      if args.length >= 2 then normalizeIndex(args(1).toNumber.toInt, len)
      else 0

    val end =
      if args.length >= 3 then normalizeIndex(args(2).toNumber.toInt, len)
      else len

    // Create new array with sliced elements
    val newArr = JSArray.empty()
    for i <- start until Math.min(end, len) if i >= 0 do newArr.push(arr.get(i))

    JSValue.JSArrayVal(newArr)
  }

  private def concatImpl(
      arrVal: JSValue.JSArrayVal,
      args: Array[JSValue]
  ): JSValue = {
    val arr = arrVal.value
    val newArr = JSArray.empty()

    // Add elements from this array
    for i <- 0 until arr.getLength do newArr.push(arr.get(i))

    // Add elements from other arrays
    for elem <- args.drop(1) do
      elem match {
        case otherArr: JSValue.JSArrayVal =>
          for i <- 0 until otherArr.value.getLength do
            newArr.push(otherArr.value.get(i))
        case other =>
          newArr.push(other)
      }

    JSValue.JSArrayVal(newArr)
  }

  // Callback method implementations (map, filter, forEach)
  private def mapImpl(arrVal: JSValue.JSArrayVal, callback: JSValue.Function)(
      using ctx: JSContext
  ): JSValue = {
    val arr = arrVal.value
    val newArr = JSArray.empty()
    for i <- 0 until arr.getLength do {
      val elem = arr.get(i)
      val result = callCallback(callback, elem, JSValue.fromInt(i), arrVal)
      newArr.push(result)
    }
    JSValue.JSArrayVal(newArr)
  }

  private def filterImpl(
      arrVal: JSValue.JSArrayVal,
      callback: JSValue.Function
  )(using ctx: JSContext): JSValue = {
    val arr = arrVal.value
    val newArr = JSArray.empty()
    for i <- 0 until arr.getLength do {
      val elem = arr.get(i)
      val result = callCallback(callback, elem, JSValue.fromInt(i), arrVal)
      if result.toBoolean then newArr.push(elem)
    }
    JSValue.JSArrayVal(newArr)
  }

  private def forEachImpl(
      arrVal: JSValue.JSArrayVal,
      callback: JSValue.Function
  )(using ctx: JSContext): JSValue = {
    val arr = arrVal.value
    for i <- 0 until arr.getLength do {
      val elem = arr.get(i)
      callCallback(callback, elem, JSValue.fromInt(i), arrVal)
    }
    JSValue.Undefined
  }

  private def reduceImpl(
      arrVal: JSValue.JSArrayVal,
      args: (JSValue.Function, Boolean, Option[JSValue])
  )(using ctx: JSContext): JSValue = {
    val arr = arrVal.value
    val (callback, hasInitial, initialOpt) = args

    var accumulator =
      if hasInitial then initialOpt.getOrElse(JSValue.Undefined) else arr.get(0)
    val startIndex = if hasInitial then 0 else 1

    for i <- startIndex until arr.getLength do {
      val elem = arr.get(i)
      accumulator =
        callCallback(callback, accumulator, elem, JSValue.fromInt(i), arrVal)
    }

    accumulator
  }

  // Helper to call a callback function
  private def callCallback(callback: JSValue.Function, args: JSValue*)(using
      ctx: JSContext
  ): JSValue = {
    val bcFunc = new BytecodeFunction(
      name = callback.name,
      bytecode = callback.bytecode,
      constants = callback.constants,
      stackSize = callback.stackSize,
      freeVars = Array.empty,
      paramNames = callback.paramNames
    )
    val interp = Interpreter()
    interp.call(bcFunc, args.head, args.toArray, callback.closure)
  }

  // Helper for array callback methods
  private def arrayCallbackMethod(name: String)(
      impl: (JSValue.JSArrayVal, JSValue.Function) => JSValue
  )(using ctx: JSContext): NativeFunction =
    NativeFunction(
      name,
      (args, context) =>
        if args.length < 2 then JSValue.Undefined
        else
          args(0) match {
            case arrVal: JSValue.JSArrayVal =>
              args(1) match {
                case func: JSValue.Function => impl(arrVal, func)
                case _                      => JSValue.Undefined
              }
            case _ => JSValue.Undefined
          }
    )

  // Helper for reduce method (special case with optional initial value)
  private def arrayReduceMethod(name: String)(
      impl: (
          JSValue.JSArrayVal,
          (JSValue.Function, Boolean, Option[JSValue])
      ) => JSValue
  )(using ctx: JSContext): NativeFunction =
    NativeFunction(
      name,
      (args, context) =>
        if args.length < 2 then JSValue.Undefined
        else
          args(0) match {
            case arrVal: JSValue.JSArrayVal =>
              args(1) match {
                case func: JSValue.Function =>
                  val hasInitial = args.length >= 3
                  val initialOpt = if hasInitial then Some(args(2)) else None
                  impl(arrVal, (func, hasInitial, initialOpt))
                case _ => JSValue.Undefined
              }
            case _ => JSValue.Undefined
          }
    )

  /** Helper to normalize negative indices */
  private def normalizeIndex(index: Int, length: Int): Int =
    if index < 0 then Math.max(0, length + index)
    else Math.min(index, length)
}
