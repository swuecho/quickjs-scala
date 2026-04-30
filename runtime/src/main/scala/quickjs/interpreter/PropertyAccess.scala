package quickjs.interpreter

import quickjs.value.JSValue
import quickjs.runtime.JSContext
import quickjs.tracing.TraceRecorder
import quickjs.bytecode.BytecodeFunction

/** Property access helpers (get/set/call) with Proxy support.
  * Mixed into Interpreter, also used by BytecodeLoop via the interpreter reference.
  */
private[interpreter] trait PropertyAccess:
  self: Interpreter =>

  def callAccessor(
    funcValue: JSValue, thisValue: JSValue, args: Array[JSValue],
    withStack: List[quickjs.objmodel.JSObject], trace: TraceRecorder
  )(using ctx: JSContext): JSValue =
    funcValue match
      case func: JSValue.Function =>
        val bcFunc = new BytecodeFunction(
          name = func.name, bytecode = func.bytecode, constants = func.constants,
          stackSize = func.stackSize, freeVars = Array.empty, paramNames = func.paramNames,
          localVarNames = func.localVarNames, argumentsIndex = func.argumentsIndex,
          isConstructor = func.isConstructor, isGenerator = func.isGenerator,
          spanMap = func.spanMap, isStrict = func.isStrict)
        self.call(bcFunc, thisValue, args, func.closure, withObjects = withStack, trace = trace)
      case JSValue.Native(nativeFuncWrapper) =>
        nativeFuncWrapper match
          case native: quickjs.value.NativeFunction =>
            self.withNativeFrame(native.name) {
              val argsWithThis = new Array[JSValue](args.length + 1)
              argsWithThis(0) = thisValue
              Array.copy(args, 0, argsWithThis, 1, args.length)
              native.call(argsWithThis)
            }
          case _ => JSValue.Undefined
      case _ => JSValue.Undefined

  def getPropertyValue(
    obj: quickjs.objmodel.JSObject, receiver: JSValue, key: String,
    withStack: List[quickjs.objmodel.JSObject], trace: TraceRecorder
  )(using ctx: JSContext): JSValue =
    val proxyTarget = obj.getOwnProperty("__proxy_target")(using ctx)
    val proxyHandler = obj.getOwnProperty("__proxy_handler")(using ctx)
    (proxyTarget, proxyHandler) match
      case (Some(target), Some(JSValue.Object(handler))) =>
        handler.getOwnProperty("get")(using ctx) match
          case Some(getTrap) =>
            callAccessor(getTrap, JSValue.Object(handler), Array(target, JSValue.fromString(key), receiver), withStack, trace)
          case None =>
            target match
              case JSValue.Object(targetObj) => getPropertyValue(targetObj, receiver, key, withStack, trace)
              case _ => JSValue.Undefined
      case _ =>
        obj.getOwnPropertyDescriptor(key)(using ctx) match
          case Some((value, attrs)) =>
            attrs.getter match
              case Some(getter) => callAccessor(getter, receiver, Array.empty, withStack, trace)
              case None => value
          case None =>
            obj.getPrototype match
              case null => JSValue.Undefined
              case proto => getPropertyValue(proto, receiver, key, withStack, trace)

  def setPropertyValue(
    obj: quickjs.objmodel.JSObject, receiver: JSValue, key: String, value: JSValue,
    withStack: List[quickjs.objmodel.JSObject], trace: TraceRecorder
  )(using ctx: JSContext): Unit =
    val proxyTarget = obj.getOwnProperty("__proxy_target")(using ctx)
    val proxyHandler = obj.getOwnProperty("__proxy_handler")(using ctx)
    (proxyTarget, proxyHandler) match
      case (Some(target), Some(JSValue.Object(handler))) =>
        handler.getOwnProperty("set")(using ctx) match
          case Some(setTrap) =>
            callAccessor(setTrap, JSValue.Object(handler), Array(target, JSValue.fromString(key), value, receiver), withStack, trace)
            ()
          case None =>
            target match
              case JSValue.Object(targetObj) => setPropertyValue(targetObj, receiver, key, value, withStack, trace)
              case _ => ()
      case _ =>
        obj.getPropertyDescriptorWithOwner(key)(using ctx) match
          case Some((_, _, attrs)) if attrs.getter.isDefined || attrs.setter.isDefined =>
            attrs.setter.foreach(setter => callAccessor(setter, receiver, Array(value), withStack, trace))
          case Some((owner, _, attrs)) =>
            if attrs.writable then
              if owner eq obj then obj.set(key, value)(using ctx)
              else obj.defineProperty(key, value, enumerable = true, writable = true, configurable = true)(using ctx)
          case None =>
            obj.set(key, value)(using ctx)
