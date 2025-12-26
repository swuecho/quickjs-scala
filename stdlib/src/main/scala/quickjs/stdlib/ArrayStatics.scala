package quickjs.stdlib

import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject

/** JavaScript Array object with methods.
  *
  * Provides arr.push(elem1, elem2, ...), arr.pop(), etc.
  * Now uses proper 'this' binding for method calls.
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

    ctx.global.set("Array", JSValue.Object(arrayObj))
