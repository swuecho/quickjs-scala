package quickjs.interpreter

import quickjs.value.JSValue
import quickjs.runtime.JSContext
import quickjs.tracing.TraceRecorder
import quickjs.bytecode.BytecodeFunction

/** Property access helpers (get/set/call) with Proxy support. Mixed into
  * Interpreter, also used by BytecodeLoop via the interpreter reference.
  */
private[interpreter] trait PropertyAccess {
  self: Interpreter =>

  def callAccessor(
      funcValue: JSValue,
      thisValue: JSValue,
      args: Array[JSValue],
      withStack: List[quickjs.objmodel.JSObject],
      trace: TraceRecorder
  )(using ctx: JSContext): JSValue =
    funcValue match {
      case func: JSValue.Function =>
        val bcFunc = new BytecodeFunction(
          name = func.name,
          bytecode = func.bytecode,
          constants = func.constants,
          stackSize = func.stackSize,
          freeVars = Array.empty,
          paramNames = func.paramNames,
          localVarNames = func.localVarNames,
          argumentsIndex = func.argumentsIndex,
          isConstructor = func.isConstructor,
          isGenerator = func.isGenerator,
          spanMap = func.spanMap,
          isStrict = func.isStrict
        )
        self.call(
          bcFunc,
          thisValue,
          args,
          func.closure,
          withObjects = withStack,
          trace = trace
        )
      case JSValue.Native(nativeFuncWrapper) =>
        nativeFuncWrapper match {
          case native: quickjs.value.NativeFunction =>
            self.withNativeFrame(native.name) {
              val argsWithThis = new Array[JSValue](args.length + 1)
              argsWithThis(0) = thisValue
              Array.copy(args, 0, argsWithThis, 1, args.length)
              native.call(argsWithThis)
            }
          case _ => JSValue.Undefined
        }
      case _ => JSValue.Undefined
    }

  def getPropertyValue(
      obj: quickjs.objmodel.JSObject,
      receiver: JSValue,
      key: String,
      withStack: List[quickjs.objmodel.JSObject],
      trace: TraceRecorder
  )(using ctx: JSContext): JSValue = {
    val proxyTarget = obj.getOwnProperty("__proxy_target")(using ctx)
    val proxyHandler = obj.getOwnProperty("__proxy_handler")(using ctx)
    val proxyRevoked = obj.getOwnProperty("__proxy_revoked")(using ctx) match {
      case Some(JSValue.Bool(true)) => true
      case _                        => false
    }
    if (proxyTarget.isDefined || proxyHandler.isDefined) &&
        (proxyRevoked || proxyTarget.contains(JSValue.Null) || proxyHandler
          .contains(JSValue.Null))
    then ctx.throwTypeError("Cannot perform operation on a revoked proxy")
    (proxyTarget, proxyHandler) match {
      case (Some(target), Some(JSValue.Object(handler))) =>
        handler.getOwnProperty("get")(using ctx) match {
          case Some(getTrap) =>
            callAccessor(
              getTrap,
              JSValue.Object(handler),
              Array(target, JSValue.fromString(key), receiver),
              withStack,
              trace
            )
          case None =>
            target match {
              case JSValue.Object(targetObj) =>
                getPropertyValue(targetObj, receiver, key, withStack, trace)
              case _ => JSValue.Undefined
            }
        }
      case _ =>
        obj.getOwnPropertyDescriptor(key)(using ctx) match {
          case Some((value, attrs)) =>
            attrs.getter match {
              case Some(getter) =>
                callAccessor(getter, receiver, Array.empty, withStack, trace)
              case None => value
            }
          case None =>
            obj.getPrototype match {
              case null  => JSValue.Undefined
              case proto =>
                getPropertyValue(proto, receiver, key, withStack, trace)
            }
        }
    }
  }

  def setPropertyValue(
      obj: quickjs.objmodel.JSObject,
      receiver: JSValue,
      key: String,
      value: JSValue,
      withStack: List[quickjs.objmodel.JSObject],
      trace: TraceRecorder,
      isStrict: Boolean = false
  )(using ctx: JSContext): Unit = {
    val proxyTarget = obj.getOwnProperty("__proxy_target")(using ctx)
    val proxyHandler = obj.getOwnProperty("__proxy_handler")(using ctx)
    val proxyRevoked = obj.getOwnProperty("__proxy_revoked")(using ctx) match {
      case Some(JSValue.Bool(true)) => true
      case _                        => false
    }
    if (proxyTarget.isDefined || proxyHandler.isDefined) &&
        (proxyRevoked || proxyTarget.contains(JSValue.Null) || proxyHandler
          .contains(JSValue.Null))
    then ctx.throwTypeError("Cannot perform operation on a revoked proxy")
    (proxyTarget, proxyHandler) match {
      case (Some(target), Some(JSValue.Object(handler))) =>
        handler.getOwnProperty("set")(using ctx) match {
          case Some(setTrap) =>
            callAccessor(
              setTrap,
              JSValue.Object(handler),
              Array(target, JSValue.fromString(key), value, receiver),
              withStack,
              trace
            )
            ()
          case None =>
            target match {
              case JSValue.Object(targetObj) =>
                setPropertyValue(
                  targetObj,
                  receiver,
                  key,
                  value,
                  withStack,
                  trace,
                  isStrict
                )
              case _ => ()
            }
        }
      case _ =>
        obj.getPropertyDescriptorWithOwner(key)(using ctx) match {
          case Some((_, _, attrs))
              if attrs.getter.isDefined || attrs.setter.isDefined =>
            attrs.setter.foreach(setter =>
              callAccessor(setter, receiver, Array(value), withStack, trace)
            )
          case Some((owner, _, attrs)) =>
            if attrs.writable then
              if owner eq obj then {
                if !obj.set(key, value)(using ctx) && isStrict then
                  ctx.throwTypeError(
                    "Cannot set property '" + key + "' on non-extensible object"
                  )
              } else
                obj.defineProperty(
                  key,
                  value,
                  enumerable = true,
                  writable = true,
                  configurable = true
                )(using ctx)
            else if isStrict then
              ctx.throwTypeError(
                "Cannot set property '" + key + "' - not writable"
              )
          case None =>
            if !obj.set(key, value)(using ctx) && isStrict then
              ctx.throwTypeError(
                "Cannot add property '" + key + "', object is not extensible"
              )
        }
    }
  }

  // =========================================================================
  // Symbol-keyed property access
  // =========================================================================

  /** Get property value by symbol id (walks prototype chain). */
  def getPropertyValueBySymbol(
      obj: quickjs.objmodel.JSObject,
      receiver: JSValue,
      symbolId: Int,
      withStack: List[quickjs.objmodel.JSObject],
      trace: TraceRecorder
  )(using ctx: JSContext): JSValue =
    obj.getOwnSymbolPropertyDescriptor(symbolId)(using ctx) match {
      case Some((value, attrs)) =>
        attrs.getter match {
          case Some(getter) =>
            callAccessor(getter, receiver, Array.empty, withStack, trace)
          case None => value
        }
      case None =>
        obj.getPrototype match {
          case null  => JSValue.Undefined
          case proto =>
            getPropertyValueBySymbol(
              proto,
              receiver,
              symbolId,
              withStack,
              trace
            )
        }
    }

  /** Set property value by symbol id. */
  def setPropertyValueBySymbol(
      obj: quickjs.objmodel.JSObject,
      receiver: JSValue,
      symbolId: Int,
      value: JSValue,
      withStack: List[quickjs.objmodel.JSObject],
      trace: TraceRecorder,
      isStrict: Boolean = false
  )(using ctx: JSContext): Unit =
    obj.getSymbolPropertyDescriptorWithOwner(symbolId)(using ctx) match {
      case Some((_, _, attrs))
          if attrs.getter.isDefined || attrs.setter.isDefined =>
        attrs.setter.foreach(setter =>
          callAccessor(setter, receiver, Array(value), withStack, trace)
        )
      case Some((owner, _, attrs)) =>
        if attrs.writable then
          if owner eq obj then {
            if !obj.setSymbol(symbolId, value)(using ctx) && isStrict then
              ctx.throwTypeError(
                "Cannot set symbol property on non-extensible object"
              )
          } else
            obj.defineSymbolProperty(
              symbolId,
              value,
              enumerable = true,
              writable = true,
              configurable = true
            )(using ctx)
        else if isStrict then
          ctx.throwTypeError("Cannot set symbol property - not writable")
      case None =>
        if !obj.setSymbol(symbolId, value)(using ctx) && isStrict then
          ctx.throwTypeError(
            "Cannot add symbol property, object is not extensible"
          )
    }
}
