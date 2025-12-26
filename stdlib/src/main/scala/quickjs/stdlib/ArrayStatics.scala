package quickjs.stdlib

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject

/** JavaScript Array object with static methods.
  *
  * Provides Array.push(arr, elements), Array.pop(arr), etc.
  * Note: These are workarounds until proper 'this' binding is implemented.
  */
object ArrayStatics:
  /** Initialize Array static methods */
  def initialize()(using ctx: JSContext): Unit =
    val arrayObj = JSObject(prototype = ctx.arrayPrototype, extensible = true)

    // Array.push(arr, elem1, elem2, ...) - adds elements to end of array
    val pushFunc = NativeFunction("push", (args, context) =>
      if args.isEmpty then
        JSValue.fromInt(0)
      else
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

    // Array.pop(arr) - removes and returns last element
    val popFunc = NativeFunction("pop", (args, context) =>
      if args.isEmpty then
        JSValue.Undefined
      else
        args(0) match
          case arrVal: JSValue.JSArrayVal =>
            arrVal.value.pop()
          case _ =>
            JSValue.Undefined
    )
    arrayObj.set("pop", JSValue.Native(popFunc))

    ctx.global.set("Array", JSValue.Object(arrayObj))
