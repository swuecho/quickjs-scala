package quickjs.interpreter

import quickjs.bytecode.*
import quickjs.value.JSValue
import quickjs.runtime.JSContext
import quickjs.tracing.{
  InstructionTrace,
  SourceLocation,
  TraceLocal,
  TraceRecorder,
  TraceValue
}
import scala.util.control.Breaks.*
import scala.annotation.switch
import scala.collection.mutable

/** Executes bytecode for a single function call within a given Frame. Contains
  * the main dispatch loop extracted from Interpreter.call().
  */
private[interpreter] final class BytecodeLoop(
    interpreter: Interpreter,
    val frame: Frame,
    val function: BytecodeFunction,
    val trace: TraceRecorder,
    val newTarget: JSValue
)(using val ctx: JSContext) {
  import BytecodeLoop.*
  import Interpreter.{readInt32, readDouble, readString}

  /** Compute the bytecode size of a string operand: 4-byte length prefix + UTF-8 bytes. */
  private inline def stringOpSize(s: String): Int =
    4 + s.getBytes(java.nio.charset.StandardCharsets.UTF_8).length

  // Convenience accessors for frame state
  private def stack = frame.stack
  private def stackTop_=(v: Int) = frame.stackTop = v
  private def stackTop = frame.stackTop
  private def pc_=(v: Int) = frame.pc = v
  private def pc = frame.pc
  private def bytecode = frame.bytecode
  private def locals = frame.locals
  private def localsCount_=(v: Int) = frame.localsCount = v
  private def localsCount = frame.localsCount
  private def thisValue = frame.thisValue
  private def closure = frame.closure
  private def withStack = frame.withStack
  private def tryStack = frame.tryStack
  private def lastException_=(v: JSValue) = frame.lastException = v
  private def lastException = frame.lastException
  private def pendingException_=(v: Option[JSValue]) = frame.pendingException =
    v
  private def pendingException = frame.pendingException
  private def result_=(v: JSValue) = frame.result = v
  private def result = frame.result
  private def lastResolvedName_=(v: String) = frame.lastResolvedName = v
  private def lastResolvedName = frame.lastResolvedName
  private def lastResolvedKind_=(v: String) = frame.lastResolvedKind = v
  private def lastResolvedKind = frame.lastResolvedKind
  private def iterations_=(v: Long) = frame.iterations = v
  private def iterations = frame.iterations

  private def proxyParts(value: JSValue): Option[(JSValue, quickjs.objmodel.JSObject)] =
    value match {
      case JSValue.Object(obj) =>
        val targetOpt = obj.getOwnProperty("__proxy_target")(using ctx)
        val handlerOpt = obj.getOwnProperty("__proxy_handler")(using ctx)
        val revoked = obj.getOwnProperty("__proxy_revoked")(using ctx) match {
          case Some(JSValue.Bool(true)) => true
          case _                        => false
        }
        if (targetOpt.isDefined || handlerOpt.isDefined) &&
            (revoked || targetOpt.contains(JSValue.Null) || handlerOpt.contains(
              JSValue.Null
            ))
        then ctx.throwTypeError("Cannot perform operation on a revoked proxy")
        (targetOpt, handlerOpt) match {
          case (Some(target), Some(JSValue.Object(handler))) => Some((target, handler))
          case _                                            => None
        }
      case _ => None
    }

  private def isCallableValue(value: JSValue): Boolean =
    value match {
      case _: JSValue.Function => true
      case JSValue.Native(_: quickjs.value.NativeFunction) => true
      case JSValue.Native(_: quickjs.value.NativeConstructor) => true
      case proxy @ JSValue.Object(_) =>
        proxyParts(proxy).exists { case (target, _) => isCallableValue(target) }
      case _ => false
    }

  private def isConstructorValue(value: JSValue): Boolean =
    value match {
      case func: JSValue.Function => func.isConstructor
      case JSValue.Native(_: quickjs.value.NativeConstructor) => true
      case proxy @ JSValue.Object(_) =>
        proxyParts(proxy).exists { case (target, _) => isConstructorValue(target) }
      case _ => false
    }

  private def getHandlerMethod(handler: quickjs.objmodel.JSObject, name: String): JSValue =
    handler.get(name)(using ctx)

  private def argsArray(args: Array[JSValue]): JSValue = {
    val arr = quickjs.objmodel.JSArray.empty()
    args.foreach(arr.push)
    JSValue.JSArrayVal(arr)
  }

  private def callValue(
      funcValue: JSValue,
      thisArg: JSValue,
      args: Array[JSValue]
  ): JSValue =
    funcValue match {
      case proxy @ JSValue.Object(_) if proxyParts(proxy).isDefined =>
        callProxy(proxy, thisArg, args)
      case func: JSValue.Function =>
        if func.isClassConstructor then
          ctx.throwTypeError(
            s"Class constructor ${func.name} cannot be invoked without 'new'"
          )
        val bcFunc = new BytecodeFunction(
          name = func.name,
          bytecode = func.bytecode,
          constants = func.constants,
          stackSize = func.stackSize,
          freeVars = Array.empty,
          freeVarSlots = func.freeVarSlots,
          paramNames = func.paramNames,
          localVarNames = func.localVarNames,
          argumentsIndex = func.argumentsIndex,
          isConstructor = func.isConstructor,
          isClassConstructor = func.isClassConstructor,
          isGenerator = func.isGenerator,
          isAsync = func.isAsync,
          length = func.paramNames.length,
          spanMap = func.spanMap,
          isStrict = func.isStrict,
          parameterScopeEndPc = func.parameterScopeEndPc
        )
        interpreter.call(
          bcFunc,
          thisArg,
          args,
          func.closure,
          trace = trace,
          calleeValue = func
        )
      case JSValue.Native(nativeFuncWrapper) =>
        nativeFuncWrapper match {
          case native: quickjs.value.NativeFunction =>
            val argsWithThis = new Array[JSValue](args.length + 1)
            argsWithThis(0) = thisArg
            Array.copy(args, 0, argsWithThis, 1, args.length)
            interpreter.withNativeFrame(native.name)(native.call(argsWithThis))
          case constructor: quickjs.value.NativeConstructor =>
            // A native constructor called as a method ignores `this` (only an
            // explicit `super()` initializes an existing receiver, via
            // `__funcSpread`/`superInitImpl`).
            interpreter.withNativeFrame(constructor.name)(
              constructor.call(args)
            )
          case _ =>
            ctx.throwTypeError(s"Invalid native function: $nativeFuncWrapper")
        }
      case JSValue.Undefined =>
        ctx.throwTypeError(
          s"TypeError: Cannot call non-function value: undefined (lastLookup=$lastResolvedName, kind=$lastResolvedKind, this=$thisArg)"
        )
      case other =>
        ctx.throwTypeError(s"Cannot call non-function value: $other")
    }

  private def callProxy(
      proxy: JSValue.Object,
      thisArg: JSValue,
      args: Array[JSValue]
  ): JSValue = {
    val (target, handler) = proxyParts(proxy).get
    if !isCallableValue(target) then
      throw new RuntimeException("TypeError: proxy target is not callable")
    getHandlerMethod(handler, "apply") match {
      case JSValue.Undefined => callValue(target, thisArg, args)
      case method =>
        if !isCallableValue(method) then
          throw new RuntimeException("TypeError: proxy apply trap is not callable")
        callValue(method, JSValue.Object(handler), Array(target, thisArg, argsArray(args)))
    }
  }

  private def constructValue(
      constructorValue: JSValue,
      args: Array[JSValue],
      newTargetValue: JSValue
  ): JSValue =
    constructorValue match {
      case proxy @ JSValue.Object(_) if proxyParts(proxy).isDefined =>
        constructProxy(proxy, args, newTargetValue)
      case JSValue.Native(constructorWrapper) =>
        constructorWrapper match {
          case constructor: quickjs.value.NativeConstructor =>
            interpreter.withNativeFrame(constructor.name) {
              constructor.construct(args, newTargetValue)
            }
          case _ =>
            throw new RuntimeException(
              s"TypeError: Cannot use 'new' with non-constructor: $constructorValue"
            )
        }
      case func: JSValue.Function =>
        if !func.isConstructor then {
          val errObj = ctx.global.get("TypeError") match {
            case JSValue.Native(nc) =>
              nc match {
                case ctor: quickjs.value.NativeConstructor =>
                  ctor.call(
                    Array(
                      JSValue.fromString(s"${func.name} is not a constructor")
                    )
                  )(using ctx)
                case _ =>
                  JSValue.fromString(s"${func.name} is not a constructor")
              }
            case _ => JSValue.fromString(s"${func.name} is not a constructor")
          }
          throw new quickjs.runtime.JSException(errObj)
        }
        val prototypeSource =
          proxyParts(newTargetValue).map(_._1).getOrElse(newTargetValue)
        val rawPrototype: JSValue = prototypeSource match {
          case f: JSValue.Function =>
            f.funcObj.get("prototype")(using ctx)
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            nc.funcObj.get("prototype")(using ctx)
          case _ =>
            func.funcObj.get("prototype")(using ctx)
        }
        val funcPrototype = prototypeObjectOf(rawPrototype)
        import quickjs.objmodel.JSObject
        val newObj = JSObject(prototype = funcPrototype, extensible = true)
        // `Foo.prototype = new Array(...)` gives instances a non-JSObject
        // [[Prototype]]; keep the array value for lookups (indices, length and
        // the array prototype chain).
        rawPrototype match {
          case arr: JSValue.JSArrayVal => newObj.setPrototypeValue(arr)
          case _                       => ()
        }
        // A class whose superclass chain leads to a callable native that
        // creates exotic instances (currently Array) must build that exotic
        // receiver so `super()` can initialize it in place.
        val derivedClass =
          func.funcObj.getOwnProperty("__derivedClass").isDefined
        val extendsArray = {
          var p: JSObject | Null = funcPrototype
          var found = false
          while p != null && !found do {
            if p eq ctx.arrayPrototype then found = true
            else p = p.getPrototype
          }
          found
        }
        val receiver: JSValue =
          if derivedClass && extendsArray then {
            val arr = quickjs.objmodel.JSArray.empty()
            arr.setPrototypeOverride(JSValue.Object(funcPrototype))
            JSValue.JSArrayVal(arr)
          } else JSValue.Object(newObj)
        if derivedClass then
          receiver match {
            case JSValue.Object(obj) =>
              obj.defineProperty(
                "__thisUninitialized",
                JSValue.Bool(true),
                enumerable = false,
                writable = true,
                configurable = true
              )
            case JSValue.JSArrayVal(arr) =>
              arr.setProperty("__thisUninitialized", JSValue.Bool(true))
            case _ => ()
          }
        val bcFunc = new BytecodeFunction(
          name = func.name,
          bytecode = func.bytecode,
          constants = func.constants,
          stackSize = func.stackSize,
          freeVars = Array.empty,
          freeVarSlots = func.freeVarSlots,
          paramNames = func.paramNames,
          localVarNames = func.localVarNames,
          argumentsIndex = func.argumentsIndex,
          isConstructor = func.isConstructor,
          isClassConstructor = func.isClassConstructor,
          isGenerator = func.isGenerator,
          isAsync = func.isAsync,
          spanMap = func.spanMap,
          isStrict = func.isStrict,
          parameterScopeEndPc = func.parameterScopeEndPc
        )
        val retValue = interpreter.call(
          bcFunc,
          receiver,
          args,
          func.closure,
          newTargetValue,
          withStack.toList,
          trace = trace,
          calleeValue = func
        )
        retValue match {
          case _: JSValue.Object | _: JSValue.Function | _: JSValue.JSArrayVal |
              _: JSValue.Generator | _: JSValue.Promise | _: JSValue.Native |
              _: JSValue.AsyncFunction =>
            retValue
          case JSValue.Undefined => receiver
          case _ if derivedClass =>
            // A derived constructor may only return an Object or undefined.
            ctx.throwTypeError(
              "Derived constructors may only return object or undefined"
            )
          case _ => receiver
        }
      case _ =>
        throw new RuntimeException(
          s"TypeError: Cannot use 'new' with non-constructor: $constructorValue"
        )
    }

  private def constructProxy(
      proxy: JSValue.Object,
      args: Array[JSValue],
      newTargetValue: JSValue
  ): JSValue = {
    val (target, handler) = proxyParts(proxy).get
    if !isConstructorValue(target) then
      throw new RuntimeException("TypeError: proxy target is not a constructor")
    val result = getHandlerMethod(handler, "construct") match {
      case JSValue.Undefined => constructValue(target, args, newTargetValue)
      case method =>
        if !isCallableValue(method) then
          throw new RuntimeException("TypeError: proxy construct trap is not callable")
        callValue(method, JSValue.Object(handler), Array(target, argsArray(args), newTargetValue))
    }
    result match {
      case _: JSValue.Object | _: JSValue.Function | _: JSValue.JSArrayVal |
          _: JSValue.Generator | _: JSValue.Promise | _: JSValue.Native |
          _: JSValue.AsyncFunction =>
        result
      case _ => throw new RuntimeException("TypeError: proxy construct trap must return an object")
    }
  }

  def localNameFor(index: Int): Option[String] =
    if index < function.paramNames.length then Some(function.paramNames(index))
    else {
      val localIndex = index - function.paramNames.length
      if localIndex >= 0 && localIndex < function.localVarNames.length then
        Some(function.localVarNames(localIndex))
      else None
    }

  def attachErrorLocation(obj: quickjs.objmodel.JSObject): Unit =
    function.lineColForPc(pc).foreach { case (line, col) =>
      val adjCol = Math.max(1, col)
      val hasLine = obj.getOwnPropertyRaw("lineNumber").nonEmpty
      val hasCol = obj.getOwnPropertyRaw("columnNumber").nonEmpty
      // A lazily captured stack already carries accurate source positions.
      val hasLocatedStack = ctx.capturedFrames(obj).isDefined || (obj
        .getOwnPropertyRaw("stack") match {
        case Some(JSValue.JSStr(stackTrace)) =>
          """:\d+:\d+""".r.findFirstIn(stackTrace).nonEmpty
        case _ => false
      })
      if !hasLocatedStack && !hasLine then
        obj.defineProperty(
          "lineNumber",
          JSValue.fromInt(line),
          enumerable = false
        )(using ctx)
      if !hasLocatedStack && !hasCol then
        obj.defineProperty(
          "columnNumber",
          JSValue.fromInt(adjCol),
          enumerable = false
        )(using ctx)
      if (!hasLine || !hasCol) && !hasLocatedStack then
        obj.getOwnPropertyRaw("stack") match {
          case Some(JSValue.JSStr(stackTrace))
              if !stackTrace.contains(s":$line:$adjCol") =>
            obj.set(
              "stack",
              JSValue.fromString(
                s"    at ${ctx.sourceName}:$line:$adjCol\n$stackTrace"
              )
            )(using ctx)
          case _ => ()
        }
    }

  /** Resume a suspended async frame with a fulfillment value. */
  private[interpreter] def resumeWithValue(value: JSValue): JSValue = {
    stack(stackTop) = value
    stackTop += 1
    run()
  }

  /** Resume a suspended async frame by throwing into it. User `catch` blocks
    * are honored through the normal try-handler dispatch.
    */
  private[interpreter] def resumeWithThrow(value: JSValue): JSValue = {
    if handleException(value) then run()
    else throw new quickjs.runtime.JSException(value)
  }

  private def handleException(value: JSValue): Boolean =
    if tryStack.nonEmpty then {
      val handler = tryStack.remove(tryStack.length - 1)
      stackTop = handler.stackTop
      // Abandon any `with` scopes opened inside the protected range.
      while withStack.length > handler.withStackDepth do
        withStack.remove(withStack.length - 1)
      lastException = value
      if handler.catchPc >= 0 then {
        pendingException = None
        pc = handler.catchPc
        true
      } else if handler.finallyPc >= 0 then {
        pendingException = Some(value)
        pc = handler.finallyPc
        true
      } else false
    } else false

  // =========================================================================
  // Extracted opcode handlers (called from run()'s dispatch)
  // =========================================================================

  /** Resolve a GetConst opcode: load constant and create closure if needed. */
  private def resolveGetConst(): Unit = {
    val index = readInt32(bytecode, pc + 1)
    val constValue = function.constants(index)
    val value = constValue match {
      case bcFunc: BytecodeFunction =>
        val newClosure = mutable.Map.empty[String, JSValue.VarRef]
        if bcFunc.captureParentClosure then newClosure ++= closure
        val inParameterScope =
          function.parameterScopeEndPc > 0 && pc < function.parameterScopeEndPc
        for varName <- bcFunc.freeVars do
          if varName == "$this" then
            // Capture 'this' from the enclosing scope (for arrow functions)
            newClosure(varName) = new JSValue.VarRef(thisValue)
          else if varName == "$newTarget" then
            // Capture 'new.target' from the enclosing scope (for arrow functions)
            newClosure(varName) = new JSValue.VarRef(newTarget)
          else {
            val paramIndex = function.paramNames.indexOf(varName)
            val slotFromMap =
              bcFunc.freeVarSlots
                .get(varName)
                .filter(i => i >= 0 && i < locals.length && i < localsCount)
            val fromClosure = closure.get(varName)
            if paramIndex >= 0 && paramIndex < localsCount && paramIndex < locals.length
            then newClosure(varName) = locals(paramIndex)
            else if slotFromMap.isDefined &&
                !(inParameterScope &&
                  fromClosure.exists(ref => ref.isEvalVar || ref.isFunctionName))
            then newClosure(varName) = locals(slotFromMap.get)
            else {
              val localVarIndex = function.localVarNames.indexOf(varName)
              if inParameterScope &&
                  fromClosure.exists(ref => ref.isEvalVar || ref.isFunctionName)
              then
                newClosure(varName) = fromClosure.get
              else if localVarIndex >= 0 &&
                  (!inParameterScope || varName == "arguments")
              then
                newClosure(varName) = locals(localVarIndex)
              else {
                if fromClosure.isDefined then
                  newClosure(varName) = fromClosure.get
                else
                  newClosure(varName) =
                    new JSValue.VarRef(JSValue.GlobalRef(varName))
              }
            }
          }
        // A function created lexically inside `with` captures the object
        // environment records as part of its scope chain.
        if withStack.nonEmpty then
          withStack.zipWithIndex.foreach { case (obj, i) =>
            newClosure(interpreter.withCaptureKey(i)) =
              new JSValue.VarRef(JSValue.Object(obj))
          }
        val funcObj = quickjs.objmodel.JSObject(
          prototype =
            if bcFunc.isAsync && bcFunc.isGenerator then
              ctx.asyncGeneratorFunctionPrototype
            else if bcFunc.isGenerator then ctx.generatorFunctionPrototype
            else if bcFunc.isAsync then ctx.asyncFunctionPrototype
            else ctx.functionPrototype,
          extensible = true
        )
        val funcValue = JSValue.Function(
          name = bcFunc.name,
          bytecode = bcFunc.bytecode,
          constants = bcFunc.constants,
          stackSize = bcFunc.stackSize,
          closure = newClosure,
          freeVarSlots = bcFunc.freeVarSlots,
          paramNames = bcFunc.paramNames,
          localVarNames = bcFunc.localVarNames,
          parentLocalVarNames = function.localVarNames,
          argumentsIndex = bcFunc.argumentsIndex,
          isConstructor = bcFunc.isConstructor,
          isClassConstructor = bcFunc.isClassConstructor,
          isGenerator = bcFunc.isGenerator,
          isAsync = bcFunc.isAsync,
          funcObj = funcObj,
          spanMap = bcFunc.spanMap,
          isStrict = bcFunc.isStrict,
          parameterScopeEndPc = bcFunc.parameterScopeEndPc
        )
        bcFunc.functionExpressionName.foreach { selfName =>
          val selfRef = new JSValue.VarRef(funcValue)
          selfRef.setFunctionName()
          if bcFunc.isStrict then selfRef.setConst()
          funcValue.closure(selfName) = selfRef
        }
        // Only ordinary (constructable) functions and generator functions have
        // an own `prototype`; arrows, methods, async and async generator
        // functions do not.
        val hasPrototype = bcFunc.isConstructor || bcFunc.isGenerator
        if hasPrototype then {
          val protoObj = quickjs.objmodel.JSObject(
            prototype =
              if bcFunc.isGenerator && bcFunc.isAsync then
                ctx.asyncGeneratorPrototype
              else if bcFunc.isGenerator then ctx.generatorPrototype
              else ctx.objectPrototype,
            extensible = true
          )
          protoObj.defineProperty("constructor", funcValue, enumerable = false)(
            using ctx
          )
          funcObj.defineProperty(
            "prototype",
            JSValue.Object(protoObj),
            enumerable = false,
            writable = true,
            configurable = false
          )(using ctx)
        }
        funcObj.defineProperty(
          "length",
          JSValue.fromInt(bcFunc.length),
          enumerable = false,
          writable = false,
          configurable = true
        )(using ctx)
        funcObj.defineProperty(
          "name",
          JSValue.fromString(bcFunc.name),
          enumerable = false,
          writable = false,
          configurable = true
        )(using ctx)
        funcValue
      case jsValue: JSValue => jsValue
      case _                => JSValue.Undefined
    }
    stack(stackTop) = value
    stackTop += 1
    pc += 5
  }

  /** Resolve a GetProp opcode: property access on any value type. */
  private def resolveGetProp(propName: String): Unit = {
    val objValue = stack(stackTop - 1)
    stackTop -= 1
    if objValue == JSValue.Null || objValue == JSValue.Undefined then
      ctx.throwTypeError(
        s"Cannot read properties of ${if objValue == JSValue.Null then "null" else "undefined"} (reading '$propName')"
      )
    lastResolvedName = propName
    lastResolvedKind = "prop"
    val result = objValue match {
      case JSValue.Object(obj) =>
        interpreter.getPropertyValue(
          obj,
          objValue,
          propName,
          withStack.toList,
          trace
        )
      case arrVal: JSValue.JSArrayVal =>
        interpreter.resolveArrayProperty(arrVal.value, propName)
      case strVal: JSValue.JSStr =>
        if propName == "length" then JSValue.fromInt(strVal.value.length)
        else if propName == "toString" then
          Interpreter.primitiveToStringNative("toString", strVal)
        else
          ctx.global.get("String") match {
            case JSValue.Native(c: quickjs.value.NativeConstructor) =>
              val proto = c.prototype;
              if proto != null then
                interpreter.getPropertyValue(
                  proto,
                  objValue,
                  propName,
                  withStack.toList,
                  trace
                )
              else JSValue.Undefined
            case JSValue.Object(o) => o.get(propName)
            case _                 => JSValue.Undefined
          }
      case _: JSValue.Int32 | _: JSValue.Float64 =>
        ctx.global.get("Number") match {
          case JSValue.Native(c: quickjs.value.NativeConstructor) =>
            val proto = c.prototype;
            if proto != null then
              interpreter.getPropertyValue(
                proto,
                objValue,
                propName,
                withStack.toList,
                trace
              )
            else JSValue.Undefined
          case JSValue.Object(o) => o.get(propName)
          case _                 => JSValue.Undefined
        }
      case JSValue.BigInt(_) =>
        // Auto-box through BigInt.prototype
        ctx.global.get("BigInt") match {
          case JSValue.Native(c: quickjs.value.NativeConstructor) =>
            interpreter.getPropertyValue(
              c.prototype,
              objValue,
              propName,
              withStack.toList,
              trace
            )
          case JSValue.Object(o) => o.get(propName)
          case _                 => JSValue.Undefined
        }
      case JSValue.Bool(_) =>
        // Boolean primitives use ordinary property lookup through
        // Boolean.prototype (the transient wrapper is not observable here).
        ctx.global.get("Boolean") match {
          case JSValue.Native(c: quickjs.value.NativeConstructor) =>
            val proto = c.prototype
            if proto != null then
              interpreter.getPropertyValue(
                proto,
                objValue,
                propName,
                withStack.toList,
                trace
              )
            else JSValue.Undefined
          case JSValue.Object(o) => o.get(propName)(using ctx)
          case _                 => JSValue.Undefined
        }
      case JSValue.Symbol(_) =>
        // Auto-box through Symbol.prototype
        if propName == "toString" then
          ctx.symbolPrototype.get("toString")(using ctx)
        else
          // Check for accessor properties on the prototype
          ctx.symbolPrototype.getOwnPropertyDescriptor(propName) match {
            case Some((_, attrs)) if attrs.getter.isDefined =>
              attrs.getter.get match {
                case func: JSValue.Function =>
                  interpreter.call(
                    new BytecodeFunction(
                      name = func.name,
                      bytecode = func.bytecode,
                      constants = func.constants,
                      stackSize = func.stackSize,
                      freeVars = Array.empty,
                      freeVarSlots = func.freeVarSlots,
                      paramNames = func.paramNames,
                      localVarNames = func.localVarNames,
                      argumentsIndex = func.argumentsIndex,
                      isConstructor = func.isConstructor,
                      isClassConstructor = func.isClassConstructor,
                      isGenerator = func.isGenerator,
                      spanMap = func.spanMap,
                      isStrict = func.isStrict,
                      parameterScopeEndPc = func.parameterScopeEndPc
                    ),
                    objValue,
                    Array.empty,
                    func.closure
                  )
                case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                  nf.call(Array(objValue))
                case _ => JSValue.Undefined
              }
            case _ => ctx.symbolPrototype.get(propName)(using ctx)
          }
      case funcVal: JSValue.Function =>
        val r = interpreter.getPropertyValue(
          funcVal.funcObj,
          funcVal,
          propName,
          withStack.toList,
          trace
        )
        if r == JSValue.Undefined && funcVal.funcObj.getPrototype == null then
          ctx.functionPrototype.get(propName)(using ctx)
        else if r == JSValue.Undefined then
          ctx.global.get("Function") match {
            case JSValue.Object(o) => o.get(propName)
            case _                 => JSValue.Undefined
          }
        else r
      case JSValue.Native(nativeFuncWrapper) =>
        nativeFuncWrapper match {
          case c: quickjs.value.NativeConstructor =>
            val r = interpreter.getPropertyValue(
              c.funcObj,
              objValue,
              propName,
              withStack.toList,
              trace
            )
            if r == JSValue.Undefined && c.funcObj.getPrototype == null then
              ctx.functionPrototype.get(propName)(using ctx)
            else r
          case nf: quickjs.value.NativeFunction =>
            val r = interpreter.getPropertyValue(
              nf.funcObj,
              objValue,
              propName,
              withStack.toList,
              trace
            )
            if r == JSValue.Undefined && nf.funcObj.getPrototype == null then
              ctx.functionPrototype.get(propName)(using ctx)
            else r
          case _ =>
            val r = ctx.functionPrototype.get(propName)(using ctx)
            if r == JSValue.Undefined then
              ctx.global.get("Function") match {
                case JSValue.Object(o) => o.get(propName)
                case _                 => JSValue.Undefined
              }
            else r
        }
      case _ => JSValue.Undefined
    }
    stack(stackTop) = result
    stackTop += 1
    pc += 1 + stringOpSize(propName)
  }

  /** Execute a Call opcode. */
  private def doCall(): Unit = {
    val argc = readInt32(bytecode, pc + 1)
    val funcValue = stack(stackTop - argc - 1)
    val args = new Array[JSValue](argc)
    for i <- 0 until argc do args(i) = stack(stackTop - argc + i)
    val previousStackTop = stackTop
    stackTop -= (argc + 1)
    java.util.Arrays.fill(
      stack.asInstanceOf[Array[Object]],
      stackTop,
      previousStackTop,
      JSValue.Undefined
    )
    funcValue match {
      case func: JSValue.Function =>
        if func.isClassConstructor then
          ctx.throwTypeError(
            s"Class constructor ${func.name} cannot be invoked without 'new'"
          )
        val bcFunc = new BytecodeFunction(
          name = func.name,
          bytecode = func.bytecode,
          constants = func.constants,
          stackSize = func.stackSize,
          freeVars = Array.empty,
          freeVarSlots = func.freeVarSlots,
          paramNames = func.paramNames,
          localVarNames = func.localVarNames,
          argumentsIndex = func.argumentsIndex,
          isConstructor = func.isConstructor,
          isClassConstructor = func.isClassConstructor,
          isGenerator = func.isGenerator,
          isAsync = func.isAsync,
          length = func.paramNames.length,
          spanMap = func.spanMap,
          isStrict = func.isStrict,
          parameterScopeEndPc = func.parameterScopeEndPc
        )
        // For arrow functions, use captured '$this' from closure
        val capturedThis =
          func.closure.get("$this").map(_.get).getOrElse(JSValue.Undefined)
        val effectiveThis =
          if func.name == "<arrow>" then capturedThis else JSValue.Undefined
        val ret = interpreter.call(
          bcFunc,
          effectiveThis,
          args,
          func.closure,
          trace = trace,
          calleeValue = func
        )
        stack(stackTop) = ret; stackTop += 1
      case JSValue.Native(nativeFuncWrapper) =>
        nativeFuncWrapper match {
          case native: quickjs.value.NativeFunction
              if native.name == "__directEval" ||
                native.name == "__directEvalField" ||
                native.name == "__directEvalPrivate" =>
            val evalResult = interpreter.runDirectEval(
              native = native,
              args = args,
              thisValue = thisValue,
              newTarget = newTarget,
              function = function,
              pc = pc,
              locals = locals,
              closure = closure,
              withStack = withStack,
              trace = trace
            )
            stack(stackTop) = evalResult; stackTop += 1
          case native: quickjs.value.NativeFunction =>
            val ret =
              interpreter.withNativeFrame(native.name)(native.call(args))
            stack(stackTop) = ret; stackTop += 1
          case constructor: quickjs.value.NativeConstructor =>
            val ret = interpreter.withNativeFrame(constructor.name) {
              constructor.call(args)
            }
            stack(stackTop) = ret; stackTop += 1
          case _ =>
            ctx.throwTypeError(s"Invalid native function: $nativeFuncWrapper")
        }
      case proxy @ JSValue.Object(_) if proxyParts(proxy).isDefined =>
        val ret = callProxy(proxy, JSValue.Undefined, args)
        stack(stackTop) = ret; stackTop += 1
      case JSValue.Undefined =>
        ctx.throwTypeError(
          s"TypeError: Cannot call non-function value: undefined (lastLookup=$lastResolvedName, kind=$lastResolvedKind, this=$thisValue)"
        )
      case other =>
        ctx.throwTypeError(s"Cannot call non-function value: $other")
    }
    pc += 5
  }

  /** Execute a New opcode. */
  private def doNew(): Unit = {
    val argc = readInt32(bytecode, pc + 1)
    val constructorValue = stack(stackTop - argc - 1)
    val args = new Array[JSValue](argc)
    for i <- 0 until argc do args(i) = stack(stackTop - argc + i)
    val previousStackTop = stackTop
    stackTop -= (argc + 1)
    java.util.Arrays.fill(
      stack.asInstanceOf[Array[Object]],
      stackTop,
      previousStackTop,
      JSValue.Undefined
    )
    val result = constructValue(constructorValue, args, constructorValue)
    stack(stackTop) = result; stackTop += 1
    pc += 5
  }

  /**
   * `#field in obj` (ES2022 ergonomic brand checks): true when the object has
   * the private data field, method or accessor declared by this class. The
   * class-unique encoded name makes fields of other classes miss.
   */
  private def doPrivateIn(encodedName: String): Unit = {
    val (displayName, fieldName) = resolvePrivateFieldName(encodedName)
    val objValue = stack(stackTop - 1)
    stackTop -= 1
    def hasField(targetObj: quickjs.objmodel.JSObject): Boolean = {
      def mapHas(key: String): Boolean =
        targetObj.getOwnProperty(key) match {
          case Some(JSValue.Object(map)) => map.getOwnProperty(fieldName).isDefined
          case _                         => false
        }
      mapHas("__private__") || mapHas("__privateMethods__") ||
      mapHas("__privateGetters__") || mapHas("__privateSetters__")
    }
    val result = objValue match {
      case JSValue.Object(obj) => hasField(obj)
      case f: JSValue.Function => hasField(f.funcObj)
      case _ =>
        ctx.throwTypeError(
          s"Cannot use 'in' on a non-object with private field #$displayName"
        )
    }
    stack(stackTop) = JSValue.Bool(result)
    stackTop += 1
    pc += 1 + stringOpSize(encodedName)
  }

  /** Resolve a GetPrivateField opcode. */
  private def resolvePrivateFieldName(encodedName: String): (String, String) = {
    val separator = encodedName.indexOf('\u001f')
    if separator < 0 then (encodedName, encodedName)
    else {
      val displayName = encodedName.substring(0, separator)
      val bindingName = encodedName.substring(separator + 1)
      val localIndex = function.localVarNames.indexOf(bindingName)
      val bindingValue =
        if localIndex >= 0 && localIndex < locals.length then locals(localIndex).get
        else closure.get(bindingName).map(_.get).getOrElse(JSValue.Undefined)
      bindingValue match {
        case JSValue.JSStr(value) => (displayName, value)
        case _                    => (displayName, displayName)
      }
    }
  }

  private def resolveGetPrivateField(encodedName: String): Unit = {
    val (displayName, fieldName) = resolvePrivateFieldName(encodedName)
    val objValue = stack(stackTop - 1)
    stackTop -= 1
    val targetObj = objValue match {
      case JSValue.Object(obj) => obj;
      case f: JSValue.Function => f.funcObj
      case _                   =>
        ctx.throwTypeError(
          s"Cannot read private field #$displayName from non-object"
        )
    }
    def getPrivateDataField(privMapObj: quickjs.objmodel.JSObject): JSValue =
      privMapObj.getOwnProperty(fieldName) match {
        case Some(value) => value
        case None =>
          ctx.throwTypeError(
            s"Cannot read private field #$displayName from an object whose class did not declare it"
          )
      }
    def getPrivateMethodOrData(): JSValue =
      targetObj.getOwnProperty("__privateMethods__") match {
        case Some(JSValue.Object(methodsMap)) =>
          methodsMap.getOwnProperty(fieldName) match {
            case Some(method) => method
            case None =>
              targetObj.getOwnProperty("__private__") match {
                case Some(JSValue.Object(privMapObj)) =>
                  getPrivateDataField(privMapObj)
                case _ =>
                  ctx.throwTypeError(
                    s"Cannot read private field #$displayName from an object whose class did not declare it"
                  )
              }
          }
        case _ =>
          targetObj.getOwnProperty("__private__") match {
            case Some(JSValue.Object(privMapObj)) =>
              getPrivateDataField(privMapObj)
            case _ =>
              ctx.throwTypeError(
                s"Cannot read private field #$displayName from an object whose class did not declare it"
              )
          }
      }
    val result = targetObj.getOwnProperty("__privateGetters__") match {
      case Some(JSValue.Object(gettersMap)) =>
        gettersMap.get(fieldName)(using ctx) match {
          case fn: JSValue.Function =>
            val bcFunc = new BytecodeFunction(
              name = fn.name,
              bytecode = fn.bytecode,
              constants = fn.constants,
              stackSize = fn.stackSize,
              freeVars = Array.empty,
              freeVarSlots = fn.freeVarSlots,
              paramNames = fn.paramNames,
              localVarNames = fn.localVarNames,
              argumentsIndex = fn.argumentsIndex,
              isConstructor = fn.isConstructor,
              isClassConstructor = fn.isClassConstructor,
              isGenerator = fn.isGenerator,
              isAsync = fn.isAsync,
              length = fn.paramNames.length,
              spanMap = fn.spanMap,
              isStrict = fn.isStrict,
              parameterScopeEndPc = fn.parameterScopeEndPc
            )
            interpreter.call(
              bcFunc,
              objValue,
              Array.empty,
              fn.closure,
              trace = trace
            )
          case _ =>
            getPrivateMethodOrData()
        }
      case _ => getPrivateMethodOrData()
    }
    stack(stackTop) = result; stackTop += 1
    pc += 1 + stringOpSize(encodedName)
  }

  /** Execute a SetPrivateField opcode. */
  private def doSetPrivateField(encodedName: String): Unit = {
    val (displayName, fieldName) = resolvePrivateFieldName(encodedName)
    val value = stack(stackTop - 1)
    val objValue = stack(stackTop - 2)
    stackTop -= 2
    val targetObj = objValue match {
      case JSValue.Object(obj) => obj;
      case f: JSValue.Function => f.funcObj
      case _                   =>
        ctx.throwTypeError(
          s"Cannot write private field #$displayName to non-object"
        )
    }
    def setExistingPrivateField(): Unit =
      val isPrivateMethod =
        targetObj.getOwnProperty("__privateMethods__") match {
          case Some(JSValue.Object(methodsMap)) =>
            methodsMap.getOwnProperty(fieldName).isDefined
          case _ => false
        }
      if isPrivateMethod then
        ctx.throwTypeError(s"Cannot assign to private method #$displayName")
      targetObj.getOwnProperty("__private__") match {
        case Some(JSValue.Object(privMapObj))
            if privMapObj.getOwnProperty(fieldName).isDefined =>
          privMapObj.set(fieldName, value)(using ctx)
        case _ =>
          ctx.throwTypeError(
            s"Cannot write private field #$displayName to an object whose class did not declare it"
          )
      }
    targetObj.getOwnProperty("__privateSetters__") match {
      case Some(JSValue.Object(settersMap)) =>
        settersMap.get(fieldName)(using ctx) match {
          case fn: JSValue.Function =>
            val bcFunc = new BytecodeFunction(
              name = fn.name,
              bytecode = fn.bytecode,
              constants = fn.constants,
              stackSize = fn.stackSize,
              freeVars = Array.empty,
              freeVarSlots = fn.freeVarSlots,
              paramNames = fn.paramNames,
              localVarNames = fn.localVarNames,
              argumentsIndex = fn.argumentsIndex,
              isConstructor = fn.isConstructor,
              isClassConstructor = fn.isClassConstructor,
              isGenerator = fn.isGenerator,
              isAsync = fn.isAsync,
              length = fn.paramNames.length,
              spanMap = fn.spanMap,
              isStrict = fn.isStrict,
              parameterScopeEndPc = fn.parameterScopeEndPc
            )
            interpreter.call(
              bcFunc,
              objValue,
              Array(value),
              fn.closure,
              trace = trace
            )
          case _ =>
            setExistingPrivateField()
        }
      case _ =>
        setExistingPrivateField()
    }
    stack(stackTop) = value; stackTop += 1
    pc += 1 + stringOpSize(encodedName)
  }

  private def getOrCreatePrivateMap(
      obj: quickjs.objmodel.JSObject
  ): quickjs.objmodel.JSObject =
    obj.getOwnProperty("__private__") match {
      case Some(JSValue.Object(pm)) => pm
      case _                        =>
        val pm = quickjs.objmodel.JSObject(prototype = null, extensible = true)
        obj.defineProperty(
          "__private__",
          JSValue.Object(pm),
          enumerable = false,
          writable = false,
          configurable = false
        )
        pm
    }

  /** Execute a CallMethod opcode. */
  private def doCallMethod(): Unit = {
    val argc = readInt32(bytecode, pc + 1)
    val thisVal = stack(stackTop - argc - 2)
    val funcValue = stack(stackTop - argc - 1)
    val args = new Array[JSValue](argc)
    for i <- 0 until argc do args(i) = stack(stackTop - argc + i)
    val previousStackTop = stackTop
    stackTop -= (argc + 2)
    java.util.Arrays.fill(
      stack.asInstanceOf[Array[Object]],
      stackTop,
      previousStackTop,
      JSValue.Undefined
    )
    funcValue match {
      case func: JSValue.Function =>
        val bcFunc = new BytecodeFunction(
          name = func.name,
          bytecode = func.bytecode,
          constants = func.constants,
          stackSize = func.stackSize,
          freeVars = Array.empty,
          freeVarSlots = func.freeVarSlots,
          paramNames = func.paramNames,
          localVarNames = func.localVarNames,
          argumentsIndex = func.argumentsIndex,
          isConstructor = func.isConstructor,
          isClassConstructor = func.isClassConstructor,
          isGenerator = func.isGenerator,
          isAsync = func.isAsync,
          spanMap = func.spanMap,
          isStrict = func.isStrict,
          parameterScopeEndPc = func.parameterScopeEndPc
        )
        val ret = interpreter.call(
          bcFunc,
          thisVal,
          args,
          func.closure,
          trace = trace,
          calleeValue = func
        )
        stack(stackTop) = ret; stackTop += 1
      case JSValue.Native(nativeFuncWrapper) =>
        nativeFuncWrapper match {
          case native: quickjs.value.NativeFunction =>
            val argsWithThis = new Array[JSValue](argc + 1);
            argsWithThis(0) = thisVal
            Array.copy(args, 0, argsWithThis, 1, argc)
            val ret = interpreter.withNativeFrame(native.name) {
              native.call(argsWithThis)
            }
            stack(stackTop) = ret; stackTop += 1
          case constructor: quickjs.value.NativeConstructor =>
            val ret = interpreter.withNativeFrame(constructor.name) {
              constructor.call(args)
            }
            stack(stackTop) = ret; stackTop += 1
          case _ =>
            ctx.throwTypeError(s"Invalid native function: $nativeFuncWrapper")
        }
      case proxy @ JSValue.Object(_) if proxyParts(proxy).isDefined =>
        val ret = callProxy(proxy, thisVal, args)
        stack(stackTop) = ret; stackTop += 1
      case JSValue.Undefined =>
        ctx.throwTypeError(
          s"TypeError: Cannot call non-function value: undefined (lastLookup=$lastResolvedName, kind=$lastResolvedKind, this=$thisVal)"
        )
      case other =>
        ctx.throwTypeError(s"Cannot call non-function value: $other")
    }
    pc += 5
  }

  /** Execute GetElem opcode. */
  /** Check if an object is a typed array (has __taView internal property). */
  private def isTypedArrayObj(obj: quickjs.objmodel.JSObject): Boolean =
    obj.getOwnPropertyRaw("__taView").isDefined

  /** Get a typed array element by index. Returns Undefined if OOB. */
  private def typedArrayGet(obj: quickjs.objmodel.JSObject, index: Int): JSValue =
    obj.getOwnPropertyRaw("__taView") match {
      case Some(JSValue.Native(view: quickjs.runtime.builtins.TypedArrayBuiltins.TypedArrayView)) =>
        if (index < 0 || index >= view.length) JSValue.Undefined
        else try view.get(index) catch { case _: Exception => JSValue.Undefined }
      case _ => JSValue.Undefined
    }

  /** Set a typed array element by index. */
  private def typedArraySet(obj: quickjs.objmodel.JSObject, index: Int, value: JSValue): Unit =
    obj.getOwnPropertyRaw("__taView") match {
      case Some(JSValue.Native(view: quickjs.runtime.builtins.TypedArrayBuiltins.TypedArrayView)) =>
        if (index < 0 || index >= view.length)
          throw new RuntimeException("TypedArray index out of bounds")
        else view.set(index, value)
      case _ => ()
    }

  private def getArrayIndex(
      array: quickjs.objmodel.JSArray,
      index: Long
  ): JSValue =
    array.getOwnIndexDescriptor(index) match {
      case Some((_, attrs)) if attrs.getter.isDefined =>
        quickjs.runtime.builtins.BuiltinHelpers.callFunctionWithThis(
          attrs.getter.get,
          JSValue.JSArrayVal(array),
          Array.empty
        )
      case Some(_) => array.get(index)
      case None =>
        quickjs.runtime.builtins.BuiltinHelpers.getPropertyWithGetter(
          JSValue.JSArrayVal(array),
          index.toString
        )
    }

  private def setArrayIndex(
      array: quickjs.objmodel.JSArray,
      index: Long,
      value: JSValue
  ): Unit =
    array.getOwnIndexDescriptor(index) match {
      case Some((_, attrs)) if attrs.isAccessor =>
        attrs.setter match {
          case Some(setter) =>
            quickjs.runtime.builtins.BuiltinHelpers.callFunctionWithThis(
              setter,
              JSValue.JSArrayVal(array),
              Array(value)
            )
          case None if function.isStrict =>
            ctx.throwTypeError(s"Cannot set property '$index'")
          case None => ()
        }
      case Some((_, attrs)) if !attrs.writable =>
        if function.isStrict then
          ctx.throwTypeError(s"Cannot set property '$index' - not writable")
      case Some(_) => array.set(index, value)
      case None =>
        val inherited = array.getPrototypeOverride match {
          case Some(JSValue.Object(proto)) =>
            proto.getPropertyDescriptorWithOwner(index.toString)
          case Some(JSValue.Null) => None
          case _ => ctx.arrayPrototype.getPropertyDescriptorWithOwner(index.toString)
        }
        inherited match {
          case Some((_, _, attrs)) if attrs.setter.isDefined =>
            quickjs.runtime.builtins.BuiltinHelpers.callFunctionWithThis(
              attrs.setter.get,
              JSValue.JSArrayVal(array),
              Array(value)
            )
          case Some((_, _, attrs))
              if attrs.isAccessor || attrs.getter.isDefined || !attrs.writable =>
            if function.isStrict then ctx.throwTypeError(s"Cannot set property '$index'")
          case _ if function.isStrict && !array.isExtensible =>
            ctx.throwTypeError(
              "Cannot add property '" + index + "', object is not extensible"
            )
          case _ => array.set(index, value)
        }
    }

  private def arrayIndexFromNumber(number: Double): Option[Long] =
    Option.when(
      !number.isNaN && !number.isInfinite && number >= 0 &&
        number <= 4294967294.0 && number == math.floor(number)
    )(number.toLong)

  private def setArrayLength(
      array: quickjs.objmodel.JSArray,
      value: JSValue
  ): Unit = {
    val number = quickjs.runtime.builtins.BuiltinHelpers.toNumber(value)
    if number.isNaN || number.isInfinite || number < 0 ||
        number > 4294967295.0 || number != math.floor(number)
    then ctx.throwRangeError("Invalid array length")
    else if !array.setLength(number.toLong) && function.isStrict then
      ctx.throwTypeError("Cannot assign to array length")
  }

  private def setArrayProperty(
      array: quickjs.objmodel.JSArray,
      key: String,
      value: JSValue
  ): Unit =
    array.getOwnPropertyDescriptor(key) match {
      case Some((_, attrs)) if attrs.isAccessor =>
        attrs.setter match {
          case Some(setter) =>
            quickjs.runtime.builtins.BuiltinHelpers.callFunctionWithThis(
              setter,
              JSValue.JSArrayVal(array),
              Array(value)
            )
          case None if function.isStrict =>
            ctx.throwTypeError(s"Cannot set property '$key'")
          case None => ()
        }
      case Some((_, attrs)) if !attrs.writable =>
        if function.isStrict then
          ctx.throwTypeError(s"Cannot set property '$key' - not writable")
      case Some(_) =>
        // Own writable data property: update it without consulting an
        // inherited accessor (OrdinarySet step 2.b).
        array.setProperty(key, value)
      case None =>
        ctx.arrayPrototype.getPropertyDescriptorWithOwner(key) match {
          case Some((_, _, attrs)) if attrs.isAccessor =>
            attrs.setter match {
              case Some(setter) =>
                quickjs.runtime.builtins.BuiltinHelpers.callFunctionWithThis(
                  setter,
                  JSValue.JSArrayVal(array),
                  Array(value)
                )
              case None if function.isStrict =>
                ctx.throwTypeError(s"Cannot set property '$key'")
              case None => ()
            }
          case Some((_, _, attrs)) if !attrs.writable =>
            if function.isStrict then
              ctx.throwTypeError(s"Cannot set property '$key' - not writable")
          case _ => array.setProperty(key, value)
        }
    }

  private def doGetElem(): Unit = {
    val rawKey = stack(stackTop - 1)
    val objValue = stack(stackTop - 2)
    // RequireObjectCoercible(base) precedes ToPropertyKey(property) (ES2024
    // 13.3.3): the null check must fire before a key object's toString runs.
    if objValue == JSValue.Null || objValue == JSValue.Undefined then
      val key = rawKey match {
        case JSValue.JSStr(s) => s
        case other            => other.toString
      }
      ctx.throwTypeError(
        s"Cannot read properties of ${if objValue == JSValue.Null then "null" else "undefined"} (reading '$key')"
      )
    val indexValue = quickjs.runtime.builtins.BuiltinHelpers.toElementKey(rawKey)
    stackTop -= 2
    val result = (objValue, indexValue) match {
      case (JSValue.JSArrayVal(arr), JSValue.Int32(i)) if i >= 0 =>
        getArrayIndex(arr, i.toLong)
      case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) =>
        arrayIndexFromNumber(d) match {
          case Some(index) => getArrayIndex(arr, index)
          case None =>
            interpreter.resolveArrayProperty(
              arr,
              quickjs.runtime.builtins.BuiltinHelpers.numberToJSString(d)
            )
        }
      case (JSValue.JSArrayVal(arr), JSValue.JSStr(propName)) =>
        interpreter.arrayIndexFromKey(propName) match {
          case Some(index) => getArrayIndex(arr, index)
          case None        => interpreter.resolveArrayProperty(arr, propName)
        }
      // TypedArray element access by integer index
      case (JSValue.Object(obj), JSValue.Int32(i)) if isTypedArrayObj(obj) =>
        typedArrayGet(obj, i)
      case (JSValue.Object(obj), JSValue.Float64(d)) if isTypedArrayObj(obj) =>
        typedArrayGet(obj, d.toInt)
      case (JSValue.Object(obj), JSValue.JSStr(propName)) =>
        interpreter.getPropertyValue(
          obj,
          objValue,
          propName,
          withStack.toList,
          trace
        )
      case (funcVal: JSValue.Function, JSValue.JSStr(propName)) =>
        interpreter.getPropertyValue(
          funcVal.funcObj,
          funcVal,
          propName,
          withStack.toList,
          trace
        )
      case (JSValue.Object(obj), JSValue.Int32(i)) =>
        interpreter.getPropertyValue(
          obj,
          objValue,
          i.toString,
          withStack.toList,
          trace
        )
      case (JSValue.Object(obj), JSValue.Float64(d)) =>
        interpreter.getPropertyValue(
          obj,
          objValue,
          quickjs.runtime.builtins.BuiltinHelpers.numberToJSString(d),
          withStack.toList,
          trace
        )
      case (funcVal: JSValue.Function, JSValue.Int32(i)) =>
        interpreter.getPropertyValue(
          funcVal.funcObj,
          funcVal,
          i.toString,
          withStack.toList,
          trace
        )
      case (funcVal: JSValue.Function, JSValue.Float64(d)) =>
        interpreter.getPropertyValue(
          funcVal.funcObj,
          funcVal,
          quickjs.runtime.builtins.BuiltinHelpers.numberToJSString(d),
          withStack.toList,
          trace
        )
      case (
            JSValue.Native(nf: quickjs.value.NativeFunction),
            JSValue.JSStr(propName)
          ) =>
        interpreter.getPropertyValue(
          nf.funcObj,
          objValue,
          propName,
          withStack.toList,
          trace
        )
      case (
            JSValue.Native(nc: quickjs.value.NativeConstructor),
            JSValue.JSStr(propName)
          ) =>
        interpreter.getPropertyValue(
          nc.funcObj,
          objValue,
          propName,
          withStack.toList,
          trace
        )
      case (
            JSValue.Native(nf: quickjs.value.NativeFunction),
            JSValue.Int32(i)
          ) =>
        interpreter.getPropertyValue(
          nf.funcObj,
          objValue,
          i.toString,
          withStack.toList,
          trace
        )
      case (
            JSValue.Native(nc: quickjs.value.NativeConstructor),
            JSValue.Int32(i)
          ) =>
        interpreter.getPropertyValue(
          nc.funcObj,
          objValue,
          i.toString,
          withStack.toList,
          trace
        )
      case (
            JSValue.Native(nf: quickjs.value.NativeFunction),
            JSValue.Float64(d)
          ) =>
        interpreter.getPropertyValue(
          nf.funcObj,
          objValue,
          d.toInt.toString,
          withStack.toList,
          trace
        )
      case (
            JSValue.Native(nc: quickjs.value.NativeConstructor),
            JSValue.Float64(d)
          ) =>
        interpreter.getPropertyValue(
          nc.funcObj,
          objValue,
          d.toInt.toString,
          withStack.toList,
          trace
        )
      case (JSValue.Symbol(_), JSValue.JSStr(propName)) =>
        interpreter.getPropertyValue(
          ctx.symbolPrototype,
          objValue,
          propName,
          withStack.toList,
          trace
        )
      case (JSValue.Symbol(_), JSValue.Int32(i)) =>
        interpreter.getPropertyValue(
          ctx.symbolPrototype,
          objValue,
          i.toString,
          withStack.toList,
          trace
        )
      case (JSValue.Symbol(_), JSValue.Float64(d)) =>
        interpreter.getPropertyValue(
          ctx.symbolPrototype,
          objValue,
          d.toInt.toString,
          withStack.toList,
          trace
        )
      case (JSValue.Symbol(_), JSValue.JSStr(propName)) =>
        // Symbol auto-boxing for bracket access: symbol[prop]
        interpreter.getPropertyValue(
          ctx.symbolPrototype,
          objValue,
          propName,
          withStack.toList,
          trace
        )
      case (JSValue.Symbol(_), JSValue.Int32(i)) =>
        interpreter.getPropertyValue(
          ctx.symbolPrototype,
          objValue,
          i.toString,
          withStack.toList,
          trace
        )
      case (JSValue.Symbol(_), JSValue.Float64(d)) =>
        interpreter.getPropertyValue(
          ctx.symbolPrototype,
          objValue,
          d.toInt.toString,
          withStack.toList,
          trace
        )
      case (obj, JSValue.Symbol(sym)) =>
        // Symbol as key: use symbol property lookup
        obj match {
          case JSValue.JSArrayVal(arr) =>
            arr.getOwnSymbol(sym).getOrElse(
              interpreter.getPropertyValueBySymbol(
                ctx.arrayPrototype,
                objValue,
                sym,
                withStack.toList,
                trace
              )
            )
          case JSValue.Object(o) =>
            interpreter.getPropertyValueBySymbol(
              o,
              objValue,
              sym,
              withStack.toList,
              trace
            )
          case fv: JSValue.Function =>
            interpreter.getPropertyValueBySymbol(
              fv.funcObj,
              objValue,
              sym,
              withStack.toList,
              trace
            )
          case JSValue.Native(nf: quickjs.value.NativeFunction) =>
            interpreter.getPropertyValueBySymbol(
              nf.funcObj,
              objValue,
              sym,
              withStack.toList,
              trace
            )
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            interpreter.getPropertyValueBySymbol(
              nc.funcObj,
              objValue,
              sym,
              withStack.toList,
              trace
            )
          case JSValue.Symbol(_) =>
            interpreter.getPropertyValueBySymbol(
              ctx.symbolPrototype,
              objValue,
              sym,
              withStack.toList,
              trace
            )
          case JSValue.JSStr(_) =>
            symbolLookupOnPrimitive(objValue, "String", sym)
          case JSValue.Int32(_) | JSValue.Float64(_) =>
            symbolLookupOnPrimitive(objValue, "Number", sym)
          case JSValue.Bool(_) =>
            symbolLookupOnPrimitive(objValue, "Boolean", sym)
          case JSValue.BigInt(_) =>
            symbolLookupOnPrimitive(objValue, "BigInt", sym)
          case _ => JSValue.Undefined
        }
      case (JSValue.JSStr(str), JSValue.Int32(i)) =>
        if i >= 0 && i < str.length then JSValue.JSStr(str.charAt(i).toString)
        else JSValue.Undefined
      case (JSValue.JSStr(str), JSValue.Float64(d)) =>
        val i = d.toInt;
        if i >= 0 && i < str.length then JSValue.JSStr(str.charAt(i).toString)
        else JSValue.Undefined
      case (
            receiver @ (_: JSValue.JSStr | _: JSValue.Int32 |
                _: JSValue.Float64 | JSValue.Bool(_) | JSValue.BigInt(_)),
            JSValue.JSStr(propName)
          ) =>
        // Computed string-key access on primitives auto-boxes through the
        // corresponding prototype (`"abc"[methodName]`).
        primitiveStringKey(receiver, propName)
      case _ => JSValue.Undefined
    }
    stack(stackTop) = result; stackTop += 1
    pc += 1
  }

  /** Property lookup on a primitive receiver with a string key (auto-boxing). */
  private def primitiveStringKey(receiver: JSValue, propName: String): JSValue =
    receiver match {
      case strVal: JSValue.JSStr =>
        if propName == "length" then JSValue.fromInt(strVal.value.length)
        else if propName == "toString" then
          Interpreter.primitiveToStringNative("toString", strVal)
        else primitivePrototypeLookup(strVal, "String", propName)
      case _: JSValue.Int32 | _: JSValue.Float64 =>
        primitivePrototypeLookup(receiver, "Number", propName)
      case JSValue.Bool(_) =>
        primitivePrototypeLookup(receiver, "Boolean", propName)
      case JSValue.BigInt(_) =>
        primitivePrototypeLookup(receiver, "BigInt", propName)
      case _ => JSValue.Undefined
    }

  /** Look up `propName` on the prototype of a primitive's wrapper object. */
  private def primitivePrototypeLookup(
      receiver: JSValue,
      constructorName: String,
      propName: String
  ): JSValue =
    ctx.global.get(constructorName) match {
      case JSValue.Native(c: quickjs.value.NativeConstructor)
          if c.prototype != null =>
        interpreter.getPropertyValue(
          c.prototype,
          receiver,
          propName,
          withStack.toList,
          trace
        )
      case _ => JSValue.Undefined
    }

  /** Symbol-keyed property lookup on a primitive receiver (auto-boxing):
    * resolve through the constructor's prototype so `""[Symbol.iterator]`
    * finds `String.prototype[Symbol.iterator]`.
    */
  private def symbolLookupOnPrimitive(
      receiver: JSValue,
      constructorName: String,
      symbolId: Int
  ): JSValue =
    ctx.global.get(constructorName) match {
      case JSValue.Native(c: quickjs.value.NativeConstructor)
          if c.prototype != null =>
        interpreter.getPropertyValueBySymbol(
          c.prototype,
          receiver,
          symbolId,
          withStack.toList,
          trace
        )
      case _ => JSValue.Undefined
    }

  /** True when `value` is a derived-class receiver whose constructor has not
    * yet called `super()`.
    */
  private def isUninitializedDerivedThis(value: JSValue): Boolean =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnProperty("__thisUninitialized") match {
          case Some(JSValue.Bool(true)) => true
          case _                        => false
        }
      case JSValue.JSArrayVal(arr) =>
        arr.getOwnProperty("__thisUninitialized") match {
          case Some(JSValue.Bool(true)) => true
          case _                        => false
        }
      case _ => false
    }

  private def checkDerivedThisInitialized(value: JSValue): Unit =
    if isUninitializedDerivedThis(value) then
      throw new quickjs.runtime.JSException(
        ctx.createError(
          "ReferenceError",
          "Must call super constructor in derived class before accessing 'this' or returning from derived constructor"
        )
      )

  /** An object environment record `HasBinding`, honoring `Symbol.unscopables`. */
  private def withObjectHasBinding(
      obj: quickjs.objmodel.JSObject,
      varName: String
  ): Boolean =
    if !obj.hasProperty(varName)(using ctx) then false
    else {
      val unscopables = ctx.unscopablesSymbolId
      if unscopables < 0 then true
      else
        obj.getSymbol(unscopables)(using ctx) match {
          case JSValue.Object(u) => !u.get(varName)(using ctx).toBoolean
          case _                 => true
        }
    }

  /** Resolve a PutGlobal opcode. */
  private def resolvePutGlobal(varName: String): Unit = {
    val value = stack(stackTop - 1)
    stackTop -= 1
    def assignVarRef(varRef: JSValue.VarRef): Unit =
      if varRef.isFunctionName then {
        if varRef.isConst then
          ctx.throwTypeError("Assignment to constant variable.")
      } else if varRef.isConst && varRef.get != JSValue.Uninitialized then
        ctx.throwTypeError("Assignment to constant variable.")
      else varRef.set(value)

    val withTarget =
      withStack.reverseIterator.find(obj => withObjectHasBinding(obj, varName))
    def globalPropertyExists(name: String): Boolean =
      ctx.global.hasProperty(name)(using ctx)
    def setGlobalProperty(name: String, newValue: JSValue): Unit =
      interpreter.setPropertyValue(
        ctx.global,
        JSValue.Object(ctx.global),
        name,
        newValue,
        withStack.toList,
        trace,
        function.isStrict
      )
      ctx.deletedGlobalProperties -= name
    withTarget match {
      case Some(obj) => obj.set(varName, value)(using ctx)
      case None      =>
        val paramIndex = function.paramNames.indexOf(varName)
        val localVarIndex = function.localVarNames.indexOf(varName)
        if withStack.nonEmpty && paramIndex >= 0 && paramIndex < locals.length then {
          val varRef = locals(paramIndex)
          assignVarRef(varRef)
        } else if withStack.nonEmpty && localVarIndex >= 0 && localVarIndex < locals.length then {
          val varRef = locals(localVarIndex)
          assignVarRef(varRef)
        } else closure.get(varName) match {
          case Some(varRef) =>
            varRef.get match {
              case JSValue.GlobalRef(refName) =>
                val existsInGlobal =
                  ctx.globalScope.has(refName) || globalPropertyExists(refName)
                if function.isStrict && !existsInGlobal then
                  throw new RuntimeException(
                    s"ReferenceError: $refName is not defined"
                  )
                if ctx.globalScope.has(refName) || !globalPropertyExists(refName)
                then {
                  ctx.globalScope.setVariable(refName, value)
                  if ctx.global.getOwnProperty(refName).isDefined then
                    ctx.global.set(refName, value)(using ctx)
                  ctx.deletedGlobalProperties -= refName
                }
                else setGlobalProperty(refName, value)
              case _ =>
                assignVarRef(varRef)
            }
          case None =>
            val existsInGlobal =
              ctx.globalScope.has(varName) || globalPropertyExists(varName)
            if function.isStrict && !existsInGlobal then
              throw new RuntimeException(
                s"ReferenceError: $varName is not defined"
              )
            if ctx.globalScope.has(varName) || !globalPropertyExists(varName)
            then {
              ctx.globalScope.setVariable(varName, value)
              if ctx.global.getOwnProperty(varName).isDefined then
                ctx.global.set(varName, value)(using ctx)
              ctx.deletedGlobalProperties -= varName
            }
            else setGlobalProperty(varName, value)
        }
    }
    pc += 1 + stringOpSize(varName)
  }

  /** Resolve a GetGlobal opcode. */
  /** Resolve a get for a global/closure/with binding, also returning the
    * object environment record base when the binding came from `with`.
    */
  private def resolveGetGlobalValue(
      varName: String,
      throwIfUnresolved: Boolean
  ): (JSValue, quickjs.objmodel.JSObject | Null) = {
    lastResolvedName = varName; lastResolvedKind = "global"
    def getGlobalProperty(name: String): Option[JSValue] =
      if ctx.global.hasProperty(name)(using ctx) then
        Some(
          interpreter.getPropertyValue(
            ctx.global,
            JSValue.Object(ctx.global),
            name,
            withStack.toList,
            trace
          )
        )
      else None
    val withResult =
      withStack.reverseIterator.find(obj => withObjectHasBinding(obj, varName))
    def checkedBinding(value: JSValue): JSValue =
      if value == JSValue.Uninitialized then
        throw new RuntimeException(
          s"ReferenceError: Cannot access '$varName' before initialization"
        )
      value
    if varName == "$newTarget" then (newTarget, null)
    else
      withResult match {
      case Some(obj) =>
        (
          interpreter.getPropertyValue(
            obj,
            JSValue.Object(obj),
            varName,
            withStack.toList,
            trace
          ),
          obj
        )
      case None      =>
        val paramIndex = function.paramNames.indexOf(varName)
        val localVarIndex = function.localVarNames.indexOf(varName)
        if paramIndex >= 0 && paramIndex < locals.length then
          (checkedBinding(locals(paramIndex).get), null)
        else if localVarIndex >= 0 && localVarIndex < locals.length then
          (checkedBinding(locals(localVarIndex).get), null)
        else closure.get(varName) match {
          case Some(varRef) =>
            varRef.get match {
              case JSValue.GlobalRef(refName) =>
                val value = ctx.globalScope
                  .getVariable(refName)
                  .orElse(getGlobalProperty(refName))
                  .getOrElse {
                    ctx.globalScope.getFunction(refName).getOrElse {
                      if throwIfUnresolved then
                        throw new RuntimeException(
                          s"ReferenceError: $refName is not defined"
                        )
                      else JSValue.Undefined
                    }
                  }
                (value, null)
              case value => (checkedBinding(value), null)
            }
          case None =>
            val value = ctx.globalScope
              .getVariable(varName)
              .orElse(getGlobalProperty(varName))
              .getOrElse {
                ctx.globalScope.getFunction(varName).getOrElse {
                  if throwIfUnresolved then
                    throw new RuntimeException(
                      s"ReferenceError: $varName is not defined"
                    )
                  else JSValue.Undefined
                }
              }
            (value, null)
        }
      }
  }

  private def resolveGetGlobal(
      varName: String,
      throwIfUnresolved: Boolean = true
  ): Unit = {
    val (result, _) = resolveGetGlobalValue(varName, throwIfUnresolved)
    stack(stackTop) = result; stackTop += 1
    pc += 1 + stringOpSize(varName)
  }

  /** `WithGetGlobal`: push the value followed by its object-environment base
    * (or undefined when the binding did not come from `with`).
    */
  private def resolveGetGlobalWithBase(varName: String): Unit = {
    val (result, base) = resolveGetGlobalValue(varName, true)
    stack(stackTop) = result; stackTop += 1
    stack(stackTop) =
      if base == null then JSValue.Undefined else JSValue.Object(base)
    stackTop += 1
    pc += 1 + stringOpSize(varName)
  }

  /** `WithPutGlobal`: when a base object was captured by the matching get,
    * perform PutValue against that reference even if the binding was deleted.
    */
  private def resolvePutGlobalWithBase(varName: String): Unit = {
    val base = stack(stackTop - 1)
    val value = stack(stackTop - 2)
    stackTop -= 2
    base match {
      case JSValue.Object(obj) =>
        interpreter.setPropertyValue(
          obj,
          JSValue.Object(obj),
          varName,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
        pc += 1 + stringOpSize(varName)
      case _ =>
        stack(stackTop) = value; stackTop += 1
        resolvePutGlobal(varName)
    }
  }

  /** A constructor's `prototype` may itself be a function or array (Node's
    * `Router.prototype = function () {}`), in which case construction uses
    * that value's object as the receiver prototype.
    */
  private def prototypeObjectOf(
      value: JSValue
  ): quickjs.objmodel.JSObject = {
    value match {
      case JSValue.Object(proto) => proto
      case f: JSValue.Function   => f.funcObj
      case JSValue.Native(nf: quickjs.value.NativeFunction) => nf.funcObj
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj
      case _ => ctx.objectPrototype
    }
  }

  /** Execute Instanceof opcode. */
  private def doInstanceof(): Unit = {
    val constructor = stack(stackTop - 1); val obj = stack(stackTop - 2);
    stackTop -= 2
    stack(stackTop) =
      JSValue.Bool(
        quickjs.runtime.builtins.BuiltinHelpers.instanceofOperator(obj, constructor)
      )
    stackTop += 1; pc += 1
  }

  /** Execute SetProp opcode. */
  private def doSetProp(propName: String): Unit = {
    val value = stack(stackTop - 1); val objValue = stack(stackTop - 2);
    stackTop -= 2
    objValue match {
      case JSValue.Object(obj) =>
        interpreter.setPropertyValue(
          obj,
          objValue,
          propName,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case JSValue.JSArrayVal(arr) =>
        if propName == "length" then setArrayLength(arr, value)
        else
          interpreter.arrayIndexFromKey(propName) match {
            case Some(index) => setArrayIndex(arr, index, value)
            case None        => setArrayProperty(arr, propName, value)
          }
      case funcVal: JSValue.Function =>
        interpreter.setPropertyValue(
          funcVal.funcObj,
          funcVal,
          propName,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case JSValue.Native(nw) =>
        nw match {
          case c: quickjs.value.NativeConstructor =>
            interpreter.setPropertyValue(
              c.funcObj,
              objValue,
              propName,
              value,
              withStack.toList,
              trace,
              function.isStrict
            )
          case nf: quickjs.value.NativeFunction =>
            interpreter.setPropertyValue(
              nf.funcObj,
              objValue,
              propName,
              value,
              withStack.toList,
              trace,
              function.isStrict
            )
          case _ =>
            throw new RuntimeException(
              s"Cannot set property on native function: $objValue"
            )
        }
      case _ =>
        if function.isStrict then
          ctx.throwTypeError(s"Cannot create property '$propName' on primitive")
    }
    stack(stackTop) = value; stackTop += 1; pc += 1 + stringOpSize(propName)
  }

  /** Execute SetElem opcode. */
  private def doSetElem(): Unit = {
    val value = stack(stackTop - 1);
    val rawKey = stack(stackTop - 2);
    val objValue = stack(stackTop - 3)
    // As in doGetElem, the object check comes before ToPropertyKey.
    if objValue == JSValue.Null || objValue == JSValue.Undefined then
      val key = rawKey match {
        case JSValue.JSStr(s) => s
        case other            => other.toString
      }
      ctx.throwTypeError(
        s"Cannot set properties of ${if objValue == JSValue.Null then "null" else "undefined"} (setting '$key')"
      )
    val indexValue = quickjs.runtime.builtins.BuiltinHelpers.toElementKey(rawKey)
    stackTop -= 3
    (objValue, indexValue) match {
      case (JSValue.JSArrayVal(arr), JSValue.Int32(i)) =>
        if i >= 0 then setArrayIndex(arr, i.toLong, value)
        else setArrayProperty(arr, i.toString, value)
      case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) =>
        arrayIndexFromNumber(d) match {
          case Some(index) => setArrayIndex(arr, index, value)
          case None =>
            setArrayProperty(
              arr,
              quickjs.runtime.builtins.BuiltinHelpers.numberToJSString(d),
              value
            )
        }
      case (JSValue.JSArrayVal(arr), JSValue.JSStr(propertyName)) =>
        if propertyName == "length" then setArrayLength(arr, value)
        else
          interpreter.arrayIndexFromKey(propertyName) match {
            case Some(index) => setArrayIndex(arr, index, value)
            case None        => setArrayProperty(arr, propertyName, value)
          }
      // TypedArray element assignment by integer index
      case (JSValue.Object(obj), JSValue.Int32(i)) if isTypedArrayObj(obj) =>
        typedArraySet(obj, i, value)
      case (JSValue.Object(obj), JSValue.Float64(d)) if isTypedArrayObj(obj) =>
        typedArraySet(obj, d.toInt, value)
      case (JSValue.Object(obj), JSValue.JSStr(pn)) =>
        interpreter.setPropertyValue(
          obj,
          objValue,
          pn,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (fv: JSValue.Function, JSValue.JSStr(pn)) =>
        interpreter.setPropertyValue(
          fv.funcObj,
          fv,
          pn,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (JSValue.Object(obj), JSValue.Int32(i)) =>
        interpreter.setPropertyValue(
          obj,
          objValue,
          i.toString,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (JSValue.Object(obj), JSValue.Float64(d)) =>
        interpreter.setPropertyValue(
          obj,
          objValue,
          quickjs.runtime.builtins.BuiltinHelpers.numberToJSString(d),
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (fv: JSValue.Function, JSValue.Int32(i)) =>
        interpreter.setPropertyValue(
          fv.funcObj,
          fv,
          i.toString,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (fv: JSValue.Function, JSValue.Float64(d)) =>
        interpreter.setPropertyValue(
          fv.funcObj,
          fv,
          quickjs.runtime.builtins.BuiltinHelpers.numberToJSString(d),
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (obj, JSValue.Symbol(sym)) =>
        obj match {
          case JSValue.JSArrayVal(arr) =>
            arr.setSymbol(sym, value)
          case JSValue.Object(o) =>
            interpreter.setPropertyValueBySymbol(
              o,
              objValue,
              sym,
              value,
              withStack.toList,
              trace,
              function.isStrict
            )
          case fv: JSValue.Function =>
            interpreter.setPropertyValueBySymbol(
              fv.funcObj,
              objValue,
              sym,
              value,
              withStack.toList,
              trace,
              function.isStrict
            )
          case JSValue.Native(nf: quickjs.value.NativeFunction) =>
            interpreter.setPropertyValueBySymbol(
              nf.funcObj,
              objValue,
              sym,
              value,
              withStack.toList,
              trace,
              function.isStrict
            )
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            interpreter.setPropertyValueBySymbol(
              nc.funcObj,
              objValue,
              sym,
              value,
              withStack.toList,
              trace,
              function.isStrict
            )
          case _ => ()
        }
      case (
            JSValue.Native(nf: quickjs.value.NativeFunction),
            JSValue.JSStr(pn)
          ) =>
        interpreter.setPropertyValue(
          nf.funcObj,
          objValue,
          pn,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (
            JSValue.Native(nc: quickjs.value.NativeConstructor),
            JSValue.JSStr(pn)
          ) =>
        interpreter.setPropertyValue(
          nc.funcObj,
          objValue,
          pn,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (
            JSValue.Native(nf: quickjs.value.NativeFunction),
            JSValue.Int32(i)
          ) =>
        interpreter.setPropertyValue(
          nf.funcObj,
          objValue,
          i.toString,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (
            JSValue.Native(nc: quickjs.value.NativeConstructor),
            JSValue.Int32(i)
          ) =>
        interpreter.setPropertyValue(
          nc.funcObj,
          objValue,
          i.toString,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (
            JSValue.Native(nf: quickjs.value.NativeFunction),
            JSValue.Float64(d)
          ) =>
        interpreter.setPropertyValue(
          nf.funcObj,
          objValue,
          d.toInt.toString,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (
            JSValue.Native(nc: quickjs.value.NativeConstructor),
            JSValue.Float64(d)
          ) =>
        interpreter.setPropertyValue(
          nc.funcObj,
          objValue,
          d.toInt.toString,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
      case (JSValue.Undefined | JSValue.Null | JSValue.Bool(_) |
            JSValue.Int32(_) | JSValue.Float64(_) | JSValue.BigInt(_) |
            JSValue.JSStr(_) | JSValue.Symbol(_), _) =>
        if function.isStrict then
          ctx.throwTypeError("Cannot create property on primitive")
      case _ => ()
    }
    stack(stackTop) = value; stackTop += 1; pc += 1
  }

  private def throwDeleteNullishTypeError(): Nothing = {
    val errObj = ctx.global.get("TypeError") match {
      case JSValue.Native(nc) =>
        nc match {
          case ctor: quickjs.value.NativeConstructor =>
            ctor.call(
              Array(
                JSValue.fromString(
                  "Cannot delete property of null or undefined"
                )
              )
            )(using ctx)
          case _ =>
            JSValue.fromString("Cannot delete property of null or undefined")
        }
      case _ =>
        JSValue.fromString("Cannot delete property of null or undefined")
    }
    throw new quickjs.runtime.JSException(errObj)
  }

  /** Execute Delete opcode. */
  private def doDelete(): Unit = {
    val propName = stack(stackTop - 1); val obj = stack(stackTop - 2);
    stackTop -= 2
    val prop = propName match {
      case JSValue.JSStr(s) => s
      case _ =>
        quickjs.runtime.builtins.BuiltinHelpers
          .toPropertyKey(propName) match {
          case JSValue.JSStr(s) => s
          case v                => v.toString
        }
    }
    val r = obj match {
      case JSValue.Object(o) =>
        val deleted = o.deleteProperty(prop)(using ctx)
        if deleted && (o eq ctx.global) then ctx.deletedGlobalProperties += prop
        JSValue.Bool(deleted)
      case fn: JSValue.Function =>
        JSValue.Bool(fn.funcObj.deleteProperty(prop)(using ctx))
      case JSValue.Native(nf: quickjs.value.NativeFunction) =>
        JSValue.Bool(nf.funcObj.deleteProperty(prop)(using ctx))
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        JSValue.Bool(nc.funcObj.deleteProperty(prop)(using ctx))
      case JSValue.JSArrayVal(arr) =>
        if interpreter.isArrayIndexKey(prop) then
          JSValue.Bool(arr.deleteIndex(interpreter.arrayIndexFromKey(prop).get))
        else if prop == "length" then JSValue.Bool(false)
        else JSValue.Bool(arr.deleteProperty(prop))
      case JSValue.Null | JSValue.Undefined =>
        throwDeleteNullishTypeError()
      case _ => JSValue.Bool(true)
    }
    if function.isStrict && r == JSValue.Bool(false) then
      ctx.throwTypeError(s"Cannot delete property '$prop'")
    stack(stackTop) = r; stackTop += 1; pc += 1
  }

  /** Execute InitElem opcode. */
  private def doInitElem(): Unit = {
    val value = stack(stackTop - 1); val indexValue = stack(stackTop - 2);
    val objValue = stack(stackTop - 3); stackTop -= 3
    (objValue, indexValue) match {
      case (JSValue.JSArrayVal(arr), JSValue.Int32(i))   => arr.set(i, value)
      case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) =>
        arr.set(d.toInt, value)
      case _ => ()
    }
    stack(stackTop) = objValue; stackTop += 1; pc += 1
  }

  /** Execute DefinePrivateField opcode. */
  private def doDefinePrivateField(encodedName: String): Unit = {
    val (displayName, fieldName) = resolvePrivateFieldName(encodedName)
    val value = stack(stackTop - 1); val objValue = stack(stackTop - 2);
    stackTop -= 2
    val targetObj = objValue match {
      case JSValue.Object(obj) => obj
      case f: JSValue.Function => f.funcObj
      case _ =>
        ctx.throwTypeError(
          s"Cannot define private field #$displayName on non-object"
        )
    }
    val privMapObj = getOrCreatePrivateMap(targetObj)
    privMapObj.defineProperty(
      fieldName,
      value,
      enumerable = false,
      writable = true,
      configurable = true
    )
    stack(stackTop) = objValue; stackTop += 1; pc += 1 + stringOpSize(encodedName)
  }

  /** Execute Await opcode. */
  private def doAwait(): Unit = {
    val awaitedValue = stack(stackTop - 1); stackTop -= 1
    val r = awaitedValue match {
      case JSValue.Object(obj) =>
        obj.getOwnProperty("__promise") match {
          case Some(promise: JSValue.Promise) =>
            promise.state match {
              case JSValue.PromiseState.Fulfilled => promise.result
              case JSValue.PromiseState.Rejected  =>
                ctx.throwException(promise.result)
              case JSValue.PromiseState.Pending =>
                // The awaited promise is still pending: suspend this frame and
                // let the async driver resume it from a promise reaction.
                pc += 1
                throw AwaitPending(awaitedValue)
            }
          case _ => awaitedValue
        }
      case _ => awaitedValue
    }
    stack(stackTop) = r; stackTop += 1; pc += 1
  }

  /** Await inside an async function: always suspend, even for settled
    * promises, so continuations run as microtasks like V8.
    */
  private def doAwaitAsync(): Unit = {
    val awaitedValue = stack(stackTop - 1)
    stackTop -= 1
    pc += 1
    throw AwaitPending(awaitedValue)
  }

  // =========================================================================
  // Main dispatch loop
  // =========================================================================


  /** Dispatch to the opcode group containing `opcode`. */
  private def runOpcodeGroup(group: Int, opcode: Opcode): Boolean =
    group match {
      case 0 => runGroup0(opcode)
      case 1 => runGroup1(opcode)
      case 2 => runGroup2(opcode)
      case 3 => runGroup3(opcode)
      case 4 => runGroup4(opcode)
      case 5 => runGroup5(opcode)
      case 6 => runGroup6(opcode)
      case 7 => runGroup7(opcode)
      case 8 => runGroup8(opcode)
      case 9 => runGroup9(opcode)
      case 10 => runGroup10(opcode)
      case 11 => runGroup11(opcode)
      case _ => throw new RuntimeException(s"Unimplemented opcode: $opcode")
    }

  /** Opcode dispatch group 0. Returns true when the function should return. */
  private def runGroup0(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.Invalid =>
              throw new RuntimeException("Invalid opcode")

            case Opcode.Nop =>
              pc += 1

            case Opcode.PushWith =>
              val value = stack(stackTop - 1)
              stackTop -= 1
              // ES ToObject: primitives are boxed, and function values use
              // their function object as the environment record.
              quickjs.runtime.builtins.BuiltinHelpers.toObject(value) match {
                case JSValue.Object(obj) =>
                  withStack += obj
                case f: JSValue.Function =>
                  withStack += f.funcObj
                case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                  withStack += nf.funcObj
                case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                  withStack += nc.funcObj
                case _ =>
                  throw new RuntimeException(
                    "TypeError: cannot create a with environment for this value"
                  )
              }
              pc += 1

            case Opcode.PopWith =>
              if withStack.nonEmpty then withStack.remove(withStack.length - 1)
              pc += 1

            case Opcode.TryStart =>
              val catchPc = readInt32(bytecode, pc + 1)
              val finallyPc = readInt32(bytecode, pc + 5)
              tryStack += TryHandler(catchPc, finallyPc, stackTop, withStack.length)
              pc += 9

            case Opcode.TryEnd =>
              if tryStack.nonEmpty then tryStack.remove(tryStack.length - 1)
              pc += 1

            case Opcode.Throw =>
              val value = stack(stackTop - 1)
              stackTop -= 1
              value match {
                case JSValue.Object(obj) =>
                  if ctx.isErrorObject(obj) then {
                    ctx.attachStack(obj)
                    attachErrorLocation(obj)
                  }
                case _ =>
                  ()
              }
              throw new quickjs.runtime.JSException(value)

            case Opcode.GetException =>
              stack(stackTop) = lastException
              stackTop += 1
              pc += 1

            case Opcode.RethrowIfPending =>
              pendingException match {
                case Some(value) =>
                  pendingException = None
                  if value == Interpreter.breakSignal then throw BreakException
                  else if value == Interpreter.continueSignal then
                    throw ContinueException
                  else throw new quickjs.runtime.JSException(value)
                case None =>
                  pc += 1
              }

            case Opcode.Await => doAwait()
            case Opcode.AwaitAsync => doAwaitAsync()

            // =========================================================================
            // Stack Manipulation - Push Constants
            // =========================================================================
            case Opcode.PushI32 =>
              val value = readInt32(bytecode, pc + 1)
              stack(stackTop) = JSValue.fromInt(value)
              stackTop += 1
              pc += 5

            case Opcode.PushFloat64 =>
              val value = readDouble(bytecode, pc + 1)
              stack(stackTop) = JSValue.fromDouble(value)
              stackTop += 1
              pc += 9

            case Opcode.PushUndefined =>
              stack(stackTop) = JSValue.Undefined
              stackTop += 1
              pc += 1

            case Opcode.PushNull =>
              stack(stackTop) = JSValue.Null
              stackTop += 1
              pc += 1

            case Opcode.PushTrue =>
              stack(stackTop) = JSValue.Bool(true)
              stackTop += 1
              pc += 1

            case Opcode.PushFalse =>
              stack(stackTop) = JSValue.Bool(false)
              stackTop += 1
              pc += 1

            case Opcode.Drop =>
              stackTop -= 1
              pc += 1

            case Opcode.Dup =>
              stack(stackTop) = stack(stackTop - 1)
              stackTop += 1
              pc += 1

    }
    false
  }

  /** Opcode dispatch group 1. Returns true when the function should return. */
  private def runGroup1(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.Dup2 =>
              stack(stackTop) = stack(stackTop - 2)
              stack(stackTop + 1) = stack(stackTop - 1)
              stackTop += 2
              pc += 1

            case Opcode.Nip =>
              stack(stackTop - 2) = stack(stackTop - 1)
              stackTop -= 1
              pc += 1

            // =========================================================================
            // Variable Access (Locals and Arguments)
            // =========================================================================
            case Opcode.GetLoc =>
              val index = readInt32(bytecode, pc + 1)
              if index < 0 || index >= locals.length then
                throw new RuntimeException(
                  s"GetLoc: Index $index out of bounds for locals array (length ${locals.length})"
                )
              val value = locals(index).get
              if value == JSValue.Uninitialized then
                throw new RuntimeException(
                  "ReferenceError: Cannot access binding before initialization"
                )
              stack(stackTop) = value
              stackTop += 1
              pc += 5

            case Opcode.GetThis =>
              checkDerivedThisInitialized(thisValue)
              stack(stackTop) = thisValue
              stackTop += 1
              pc += 1

            case Opcode.GetThisUnchecked =>
              stack(stackTop) = thisValue
              stackTop += 1
              pc += 1

            case Opcode.MarkThisInitialized =>
              thisValue match {
                case JSValue.Object(obj) =>
                  obj.deleteProperty("__thisUninitialized")
                case JSValue.JSArrayVal(arr) =>
                  arr.deleteProperty("__thisUninitialized")
                case _ => ()
              }
              pc += 1

            case Opcode.PutLoc =>
              val index = readInt32(bytecode, pc + 1)
              if index < 0 || index >= locals.length then
                throw new RuntimeException(
                  s"PutLoc: Index $index out of bounds for locals array (length ${locals.length})"
                )
              stackTop -= 1
              val target = locals(index)
              if target.isConst && target.get != JSValue.Uninitialized then
                throw new RuntimeException(
                  "TypeError: Assignment to constant variable."
                )
              target.set(stack(stackTop))
              if index >= localsCount then localsCount = index + 1
              pc += 5

            case Opcode.SetLocUninitialized =>
              val index = readInt32(bytecode, pc + 1)
              if index < 0 || index >= locals.length then
                throw new RuntimeException(
                  s"SetLocUninitialized: Index $index out of bounds for locals array (length ${locals.length})"
                )
              locals(index).set(JSValue.Uninitialized)
              if index >= localsCount then localsCount = index + 1
              pc += 5

            case Opcode.SetLocConst =>
              val index = readInt32(bytecode, pc + 1)
              if index < 0 || index >= locals.length then
                throw new RuntimeException(
                  s"SetLocConst: Index $index out of bounds for locals array (length ${locals.length})"
                )
              locals(index).setConst()
              pc += 5

            case Opcode.CloneLocRef =>
              val index = readInt32(bytecode, pc + 1)
              if index < 0 || index >= locals.length then
                throw new RuntimeException(
                  s"CloneLocRef: Index $index out of bounds for locals array (length ${locals.length})"
                )
              // Fresh binding for the next loop iteration: closures created in
              // the previous iteration keep the old VarRef.
              val previous = locals(index)
              val copy = new JSValue.VarRef(previous.get)
              if previous.isConst then copy.setConst()
              if previous.isFunctionName then copy.setFunctionName()
              if previous.isEvalVar then copy.setEvalVar()
              locals(index) = copy
              pc += 5

            case Opcode.GetLocCheck =>
              val index = readInt32(bytecode, pc + 1)
              if index < 0 || index >= locals.length then
                throw new RuntimeException(
                  s"GetLocCheck: Index $index out of bounds for locals array (length ${locals.length})"
                )
              val value = locals(index).get
              if value == JSValue.Uninitialized then
                throw new RuntimeException(
                  s"ReferenceError: Cannot access lexical variable before initialization"
                )
              stack(stackTop) = value
              stackTop += 1
              pc += 5

            case Opcode.GetArg =>
              val index = readInt32(bytecode, pc + 1)
              if index < 0 then
                throw new RuntimeException(
                  s"GetArg: Negative argument index $index"
                )
              stack(stackTop) =
                if index < frame.args.length then frame.args(index)
                else JSValue.Undefined
              stackTop += 1
              pc += 5

            case Opcode.GetRestArgs =>
              val index = readInt32(bytecode, pc + 1)
              val rest = quickjs.objmodel.JSArray.empty()
              var i = math.max(index, 0)
              while i < frame.args.length do {
                rest.push(frame.args(i))
                i += 1
              }
              stack(stackTop) = JSValue.JSArrayVal(rest)
              stackTop += 1
              pc += 5

    }
    false
  }

  /** Opcode dispatch group 2. Returns true when the function should return. */
  private def runGroup2(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.PutArg =>
              val index = readInt32(bytecode, pc + 1)
              if index < 0 || index >= locals.length then
                throw new RuntimeException(
                  s"PutArg: Index $index out of bounds for locals array (length ${locals.length})"
                )
              stackTop -= 1
              locals(index).set(stack(stackTop))
              if index >= localsCount then localsCount = index + 1
              pc += 5

            // =========================================================================
            // Unary Operations
            // =========================================================================
            case Opcode.Neg =>
              val a = stack(stackTop - 1)
              stackTop -= 1
              val r = a match {
                case JSValue.BigInt(b) => JSValue.BigInt(b.negate())
                case _ =>
                  JSValue.fromDouble(
                    -quickjs.runtime.builtins.BuiltinHelpers.toNumber(a)
                  )
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Pos =>
              val a = stack(stackTop - 1)
              stack(stackTop - 1) = JSValue.fromDouble(
                quickjs.runtime.builtins.BuiltinHelpers.toNumber(a)(using ctx)
              )
              pc += 1

            case Opcode.Not =>
              val a = stack(stackTop - 1)
              stackTop -= 1
              val r = JSValue.Bool(!a.toBoolean)
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.LNot =>
              val a = stack(stackTop - 1)
              stackTop -= 1
              val r = a match {
                case JSValue.BigInt(b) => JSValue.BigInt(b.not())
                case _ =>
                  JSValue.Int32(
                    ~quickjs.runtime.builtins.BuiltinHelpers.toNumber(a).toInt
                  )
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.PreInc =>
              val a = stack(stackTop - 1)
              stackTop -= 1
              val r = a match {
                case JSValue.BigInt(b) =>
                  JSValue.BigInt(b.add(java.math.BigInteger.ONE))
                case _ =>
                  JSValue.fromDouble(
                    quickjs.runtime.builtins.BuiltinHelpers.toNumber(a) + 1
                  )
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.PostInc =>
              val a = stack(stackTop - 1)
              stackTop -= 1
              val (oldVal, newVal) = a match {
                case JSValue.BigInt(b) =>
                  (a, JSValue.BigInt(b.add(java.math.BigInteger.ONE)))
                case _ =>
                  val oldNum =
                    quickjs.runtime.builtins.BuiltinHelpers.toNumber(a)
                  (JSValue.fromDouble(oldNum), JSValue.fromDouble(oldNum + 1))
              }
              stack(stackTop) = oldVal
              stack(stackTop + 1) = newVal
              stackTop += 2
              pc += 1

            case Opcode.PreDec =>
              val a = stack(stackTop - 1)
              stackTop -= 1
              val r = a match {
                case JSValue.BigInt(b) =>
                  JSValue.BigInt(b.subtract(java.math.BigInteger.ONE))
                case _ =>
                  JSValue.fromDouble(
                    quickjs.runtime.builtins.BuiltinHelpers.toNumber(a) - 1
                  )
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

    }
    false
  }

  /** Opcode dispatch group 3. Returns true when the function should return. */
  private def runGroup3(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.PostDec =>
              val a = stack(stackTop - 1)
              stackTop -= 1
              val (oldVal, newVal) = a match {
                case JSValue.BigInt(b) =>
                  (a, JSValue.BigInt(b.subtract(java.math.BigInteger.ONE)))
                case _ =>
                  val oldNum =
                    quickjs.runtime.builtins.BuiltinHelpers.toNumber(a)
                  (JSValue.fromDouble(oldNum), JSValue.fromDouble(oldNum - 1))
              }
              stack(stackTop) = oldVal
              stack(stackTop + 1) = newVal
              stackTop += 2
              pc += 1

            case Opcode.Typeof =>
              val a = stack(stackTop - 1)
              stackTop -= 1
              val typeName = a match {
                case JSValue.Undefined                         => "undefined"
                case JSValue.Null                              => "object"
                case _: JSValue.Bool                           => "boolean"
                case _: JSValue.Int32 | _: JSValue.Float64     => "number"
                case _: JSValue.BigInt                         => "bigint"
                case _: JSValue.JSStr                          => "string"
                case _: JSValue.Symbol                         => "symbol"
                case _: JSValue.Function                       => "function"
                case JSValue.Object(_) | _: JSValue.JSArrayVal => "object"
                case JSValue.Native(_)                         => "function"
                case _                                         => "object"
              }
              val r = JSValue.fromString(typeName)
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Delete =>
              val propName = stack(stackTop - 1)
              val obj = stack(stackTop - 2)
              stackTop -= 2
              val r = propName match {
                case JSValue.Symbol(sym) =>
                  // Delete symbol-keyed property
                  obj match {
                    case JSValue.Object(o) =>
                      JSValue.Bool(o.deleteSymbolProperty(sym)(using ctx))
                    case fn: JSValue.Function =>
                      JSValue.Bool(
                        fn.funcObj.deleteSymbolProperty(sym)(using ctx)
                      )
                    case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                      JSValue.Bool(
                        nf.funcObj.deleteSymbolProperty(sym)(using ctx)
                      )
                    case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                      JSValue.Bool(
                        nc.funcObj.deleteSymbolProperty(sym)(using ctx)
                      )
                    case JSValue.Null | JSValue.Undefined =>
                      throwDeleteNullishTypeError()
                    case _ => JSValue.Bool(true)
                  }
                case _ =>
                  val prop = propName match {
                    case JSValue.JSStr(s) => s
                    case _                => propName.toNumber.toInt.toString
                  }
                  obj match {
                    case JSValue.Object(o) =>
                      val deleted = o.deleteProperty(prop)(using ctx)
                      if deleted && (o eq ctx.global) then
                        ctx.deletedGlobalProperties += prop
                      JSValue.Bool(deleted)
                    case fn: JSValue.Function =>
                      JSValue.Bool(fn.funcObj.deleteProperty(prop)(using ctx))
                    case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                      JSValue.Bool(nf.funcObj.deleteProperty(prop)(using ctx))
                    case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                      JSValue.Bool(nc.funcObj.deleteProperty(prop)(using ctx))
                    case JSValue.JSArrayVal(arr) =>
                      if interpreter.isArrayIndexKey(prop) then
                        JSValue.Bool(arr.deleteIndex(interpreter.arrayIndexFromKey(prop).get))
                      else if prop == "length" then JSValue.Bool(false)
                      else JSValue.Bool(arr.deleteProperty(prop))
                    case JSValue.Null | JSValue.Undefined =>
                      throwDeleteNullishTypeError()
                    case _ =>
                      JSValue.Bool(true)
                  }
              }
              if function.isStrict && r == JSValue.Bool(false) then
                val displayKey = propName match {
                  case JSValue.Symbol(sym) => s"Symbol($sym)"
                  case JSValue.JSStr(s)    => s
                  case _                   => propName.toString
                }
                ctx.throwTypeError(s"Cannot delete property '$displayKey'")
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.DeleteName =>
              val name = readString(bytecode, pc + 1)
              resolveDeleteName(name)

            case Opcode.PrivateIn =>
              val name = readString(bytecode, pc + 1)
              doPrivateIn(name)

            // =========================================================================
            // Binary Arithmetic Operations
            // =========================================================================
    }
    false
  }

  /** `delete ident` inside `with`: the object environment record shadows
    * outer bindings, so resolution happens at runtime. Returns false when the
    * name is bound as a variable (var/param/closure) and true for unresolvable
    * references, per ES DeleteOperator semantics.
    */
  private def resolveDeleteName(varName: String): Unit = {
    val result =
      withStack.reverseIterator.find(obj => withObjectHasBinding(obj, varName)) match {
        case Some(obj) =>
          val deleted = obj.deleteProperty(varName)(using ctx)
          if !deleted && function.isStrict then
            ctx.throwTypeError(s"Cannot delete property '$varName'")
          JSValue.Bool(deleted)
        case None =>
          val paramIndex = function.paramNames.indexOf(varName)
          val localVarIndex = function.localVarNames.indexOf(varName)
          if paramIndex >= 0 && paramIndex < locals.length then JSValue.Bool(false)
          else if localVarIndex >= 0 && localVarIndex < locals.length then
            JSValue.Bool(false)
          else if closure
              .get(varName)
              .exists(ref => !ref.get.isInstanceOf[JSValue.GlobalRef])
          then JSValue.Bool(false)
          else if ctx.global.hasProperty(varName)(using ctx) then {
            val deleted = ctx.global.deleteProperty(varName)(using ctx)
            if !deleted && function.isStrict then
              ctx.throwTypeError(s"Cannot delete property '$varName'")
            if deleted then ctx.deletedGlobalProperties += varName
            JSValue.Bool(deleted)
          } else if ctx.globalScope.has(varName) then JSValue.Bool(false)
          else JSValue.Bool(true)
      }
    stack(stackTop) = result
    stackTop += 1
    pc += 1 + stringOpSize(varName)
  }

  /** Opcode dispatch group 4. Returns true when the function should return. */
  private def runGroup4(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.Add =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val leftPrimitive =
                quickjs.runtime.builtins.BuiltinHelpers.toPrimitive(a, "default")
              val rightPrimitive =
                quickjs.runtime.builtins.BuiltinHelpers.toPrimitive(b, "default")
              val r = (leftPrimitive, rightPrimitive) match {
                case (JSValue.JSStr(_), _) | (_, JSValue.JSStr(_)) =>
                  JSValue.fromString(
                    quickjs.runtime.builtins.BuiltinHelpers
                      .toJSString(leftPrimitive) +
                      quickjs.runtime.builtins.BuiltinHelpers
                        .toJSString(rightPrimitive)
                  )
                case (_: JSValue.BigInt, _) | (_, _: JSValue.BigInt) =>
                  JSValue.add(leftPrimitive, rightPrimitive)
                case _ =>
                  JSValue.fromDouble(
                    leftPrimitive.toNumber + rightPrimitive.toNumber
                  )
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Sub =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = (a, b) match {
                case (_: JSValue.BigInt, _) | (_, _: JSValue.BigInt) =>
                  JSValue.subtract(a, b)
                case _ =>
                  JSValue.fromDouble(
                    quickjs.runtime.builtins.BuiltinHelpers.toNumber(a) -
                      quickjs.runtime.builtins.BuiltinHelpers.toNumber(b)
                  )
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Mul =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = (a, b) match {
                case (_: JSValue.BigInt, _) | (_, _: JSValue.BigInt) =>
                  JSValue.multiply(a, b)
                case _ =>
                  JSValue.fromDouble(
                    quickjs.runtime.builtins.BuiltinHelpers.toNumber(a) *
                      quickjs.runtime.builtins.BuiltinHelpers.toNumber(b)
                  )
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Div =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = (a, b) match {
                case (_: JSValue.BigInt, _) | (_, _: JSValue.BigInt) =>
                  JSValue.divide(a, b)
                case _ =>
                  JSValue.fromDouble(
                    quickjs.runtime.builtins.BuiltinHelpers.toNumber(a) /
                      quickjs.runtime.builtins.BuiltinHelpers.toNumber(b)
                  )
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Mod =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = (a, b) match {
                case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
                  if y.equals(java.math.BigInteger.ZERO) then
                    throw new RuntimeException("RangeError: Division by zero")
                  JSValue.BigInt(x.remainder(y))
                case (JSValue.BigInt(_), _) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case (_, JSValue.BigInt(_)) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case _ =>
                  val na =
                    quickjs.runtime.builtins.BuiltinHelpers.toNumber(a)
                  val nb =
                    quickjs.runtime.builtins.BuiltinHelpers.toNumber(b)
                  // Java's double `%` is fmod: zero keeps the dividend's sign
                  // (`-1 % -1` is -0) and `x % Infinity` is x, matching
                  // Number::remainder. The hand-rolled version produced +0.
                  JSValue.fromDouble(na % nb)
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

    }
    false
  }

  /** Opcode dispatch group 5. Returns true when the function should return. */
  private def runGroup5(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.Pow =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = (a, b) match {
                case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
                  if y.signum() < 0 then
                    throw new RuntimeException(
                      "RangeError: BigInt negative exponent"
                    )
                  JSValue.BigInt(x.pow(y.intValue()))
                case (JSValue.BigInt(_), _) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case (_, JSValue.BigInt(_)) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case _ =>
                  val na =
                    quickjs.runtime.builtins.BuiltinHelpers.toNumber(a)
                  val nb =
                    quickjs.runtime.builtins.BuiltinHelpers.toNumber(b)
                  JSValue.fromDouble(math.pow(na, nb))
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Comma =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              stack(stackTop) = b
              stackTop += 1
              pc += 1

            // =========================================================================
            // Comparison Operations
            // =========================================================================
            case Opcode.Lt =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.Bool(Interpreter.compare(a, b) < 0)
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Lte =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.Bool(Interpreter.compare(a, b) <= 0)
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Gt =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.Bool(Interpreter.compare(a, b) > 0)
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Gte =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.Bool(Interpreter.compare(a, b) >= 0)
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Eq =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.Bool(Interpreter.looseEqual(a, b))
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Neq =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.Bool(!Interpreter.looseEqual(a, b))
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.StrictEq =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.Bool(Interpreter.strictEqual(a, b))
              stack(stackTop) = r
              stackTop += 1
              pc += 1

    }
    false
  }

  /** Opcode dispatch group 6. Returns true when the function should return. */
  private def runGroup6(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.StrictNeq =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.Bool(!Interpreter.strictEqual(a, b))
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            // =========================================================================
            // Bitwise Operations
            // =========================================================================
            case Opcode.And =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = (a, b) match {
                case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
                  JSValue.BigInt(x.and(y))
                case (JSValue.BigInt(_), _) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case (_, JSValue.BigInt(_)) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case _ =>
                  JSValue.Int32(Interpreter.toInt32(a) & Interpreter.toInt32(b))
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Or =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = (a, b) match {
                case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
                  JSValue.BigInt(x.or(y))
                case (JSValue.BigInt(_), _) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case (_, JSValue.BigInt(_)) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case _ =>
                  JSValue.Int32(Interpreter.toInt32(a) | Interpreter.toInt32(b))
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Xor =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = (a, b) match {
                case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
                  JSValue.BigInt(x.xor(y))
                case (JSValue.BigInt(_), _) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case (_, JSValue.BigInt(_)) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case _ =>
                  JSValue.Int32(Interpreter.toInt32(a) ^ Interpreter.toInt32(b))
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Shl =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = (a, b) match {
                case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
                  JSValue.BigInt(x.shiftLeft(y.intValue()))
                case (JSValue.BigInt(_), _) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case (_, JSValue.BigInt(_)) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case _ =>
                  JSValue.Int32(
                    Interpreter.toInt32(a) << (Interpreter.toInt32(b) & 0x1f)
                  )
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

    }
    false
  }

  /** Opcode dispatch group 7. Returns true when the function should return. */
  private def runGroup7(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.Sar =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = (a, b) match {
                case (JSValue.BigInt(x), JSValue.BigInt(y)) =>
                  JSValue.BigInt(x.shiftRight(y.intValue()))
                case (JSValue.BigInt(_), _) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case (_, JSValue.BigInt(_)) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case _ =>
                  JSValue.Int32(
                    Interpreter.toInt32(a) >> (Interpreter.toInt32(b) & 0x1f)
                  )
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Shr =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = (a, b) match {
                case (JSValue.BigInt(_), _) =>
                  throw new RuntimeException(
                    "TypeError: BigInts have no unsigned right shift; use >> instead"
                  )
                case (_, JSValue.BigInt(_)) =>
                  throw new RuntimeException(
                    "TypeError: Cannot mix BigInt and other types"
                  )
                case _ =>
                  val shiftCount = Interpreter.toInt32(b) & 0x1f
                  val unsignedResult = Interpreter.toInt32(a) >>> shiftCount
                  val asUnsigned = unsignedResult.toLong & 0xffffffffL
                  JSValue.fromDouble(asUnsigned.toDouble)
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.LogicalAnd =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = if a.toBoolean then b else a
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.LogicalOr =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = if a.toBoolean then a else b
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            // =========================================================================
            // Instanceof and In Operators
            // =========================================================================
            case Opcode.Instanceof =>
              doInstanceof()

            case Opcode.In =>
              val propName = stack(stackTop - 2)
              val objVal = stack(stackTop - 1)
              stackTop -= 2
              stack(stackTop) =
                quickjs.runtime.builtins.BuiltinHelpers.inOperator(
                  propName,
                  objVal
                )
              stackTop += 1
              pc += 1

            // =========================================================================
            // Control Flow (Jumps, Returns, Break, Continue)
            // =========================================================================
            case Opcode.IfFalse =>
              val offset = readInt32(bytecode, pc + 1)
              val value = stack(stackTop - 1)
              stackTop -= 1
              if !value.toBoolean then pc += offset + 1
              else pc += 5

            case Opcode.IfTrue =>
              val offset = readInt32(bytecode, pc + 1)
              val value = stack(stackTop - 1)
              stackTop -= 1
              if value.toBoolean then pc += offset + 1
              else pc += 5

            case Opcode.Goto =>
              val offset = readInt32(bytecode, pc + 1)
              pc += offset + 1

            case Opcode.Break =>
              throw BreakException

            case Opcode.Continue =>
              throw ContinueException

    }
    false
  }

  /** Opcode dispatch group 8. Returns true when the function should return. */
  private def runGroup8(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.Return =>
              // Statement completions such as try/finally may have no value.
              // Treat an empty operand stack as JavaScript undefined.
              result = if stackTop > 0 then stack(stackTop - 1) else JSValue.Undefined
              if isUninitializedDerivedThis(thisValue) then
                result match {
                  case JSValue.Undefined =>
                    // No super() and no object return: ReferenceError.
                    checkDerivedThisInitialized(thisValue)
                  case _ => ()
                }
              return true

            case Opcode.ReturnUndef =>
              checkDerivedThisInitialized(thisValue)
              result = JSValue.Undefined
              return true

            // =========================================================================
            // Function Calls (Call and CallMethod)
            // =========================================================================
            case Opcode.Call => doCall()

            case Opcode.CallMethod => doCallMethod()

            // =========================================================================
            // Object Creation (New, NewObject, NewArray)
            // =========================================================================
            case Opcode.New => doNew()

            case Opcode.NewObject =>
              import quickjs.objmodel.JSObject
              val obj =
                JSObject(prototype = ctx.objectPrototype, extensible = true)
              stack(stackTop) = JSValue.Object(obj)
              stackTop += 1
              pc += 1

            case Opcode.NewArray =>
              val size = readInt32(bytecode, pc + 1)
              import quickjs.objmodel.JSArray
              val arr = JSArray(size)
              stack(stackTop) = JSValue.JSArrayVal(arr)
              stackTop += 1
              pc += 5

            // =========================================================================
            // Property Access (GetElem, SetElem, InitElem, GetProp, SetProp)
            // =========================================================================
            case Opcode.GetElem => doGetElem()

            case Opcode.SetElem => doSetElem()
    }
    false
  }

  /** Opcode dispatch group 9. Returns true when the function should return. */
  private def runGroup9(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.InitElem =>
              val value = stack(stackTop - 1)
              val indexValue = stack(stackTop - 2)
              val objValue = stack(stackTop - 3)
              stackTop -= 3

              (objValue, indexValue) match {
                case (JSValue.JSArrayVal(arr), JSValue.Int32(i)) =>
                  arr.set(i, value)
                case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) =>
                  arr.set(d.toInt, value)
                case (obj, JSValue.Symbol(sym)) =>
                  // Initialize symbol-keyed property
                  obj match {
                    case JSValue.Object(o) =>
                      o.initSymbolProperty(
                        sym,
                        value,
                        enumerable = true,
                        writable = true,
                        configurable = true
                      )
                    case fv: JSValue.Function =>
                      fv.funcObj.initSymbolProperty(
                        sym,
                        value,
                        enumerable = true,
                        writable = true,
                        configurable = true
                      )
                    case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                      nf.funcObj.initSymbolProperty(
                        sym,
                        value,
                        enumerable = true,
                        writable = true,
                        configurable = true
                      )
                    case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                      nc.funcObj.initSymbolProperty(
                        sym,
                        value,
                        enumerable = true,
                        writable = true,
                        configurable = true
                      )
                    case _ => ()
                  }
                case (obj, JSValue.JSStr(propName)) =>
                  // Initialize string-keyed property on any object type
                  obj match {
                    case JSValue.Object(o) =>
                      o.initProperty(
                        propName,
                        value,
                        enumerable = true,
                        writable = true,
                        configurable = true
                      )
                    case fv: JSValue.Function =>
                      fv.funcObj.initProperty(
                        propName,
                        value,
                        enumerable = true,
                        writable = true,
                        configurable = true
                      )
                    case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                      nf.funcObj.initProperty(
                        propName,
                        value,
                        enumerable = true,
                        writable = true,
                        configurable = true
                      )
                    case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                      nc.funcObj.initProperty(
                        propName,
                        value,
                        enumerable = true,
                        writable = true,
                        configurable = true
                      )
                    case _ => ()
                  }
                case _ =>
                  ()
              }

              stack(stackTop) = objValue
              stackTop += 1
              pc += 1

            case Opcode.GetProp =>
              resolveGetProp(readString(bytecode, pc + 1))

    }
    false
  }

  /** Opcode dispatch group 10. Returns true when the function should return. */
  private def runGroup10(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.SetProp =>
              val propName = readString(bytecode, pc + 1)
              val value = stack(stackTop - 1)
              val objValue = stack(stackTop - 2)
              stackTop -= 2

              objValue match {
                case JSValue.Object(obj) =>
                  interpreter.setPropertyValue(
                    obj,
                    objValue,
                    propName,
                    value,
                    withStack.toList,
                    trace,
                    function.isStrict
                  )
                case JSValue.JSArrayVal(arr) =>
                  if propName == "length" then
                    setArrayLength(arr, value)
                  else
                    interpreter.arrayIndexFromKey(propName) match {
                      case Some(index) => setArrayIndex(arr, index, value)
                      case None        => setArrayProperty(arr, propName, value)
                    }
                case funcVal: JSValue.Function =>
                  interpreter.setPropertyValue(
                    funcVal.funcObj,
                    funcVal,
                    propName,
                    value,
                    withStack.toList,
                    trace,
                    function.isStrict
                  )
                case JSValue.Native(nativeWrapper) =>
                  nativeWrapper match {
                    case constructor: quickjs.value.NativeConstructor =>
                      interpreter.setPropertyValue(
                        constructor.funcObj,
                        objValue,
                        propName,
                        value,
                        withStack.toList,
                        trace,
                        function.isStrict
                      )
                    case nativeFunc: quickjs.value.NativeFunction =>
                      interpreter.setPropertyValue(
                        nativeFunc.funcObj,
                        objValue,
                        propName,
                        value,
                        withStack.toList,
                        trace,
                        function.isStrict
                      )
                    case _ =>
                      throw new RuntimeException(
                        s"Cannot set property on native function: $objValue"
                      )
                  }
                case _ =>
                  if function.isStrict then
                    ctx.throwTypeError(
                      s"Cannot create property '$propName' on primitive"
                    )
              }

              stack(stackTop) = value
              stackTop += 1
              pc += 1 + stringOpSize(propName)

            // Private field access
            case Opcode.GetPrivateField =>
              resolveGetPrivateField(readString(bytecode, pc + 1))

            case Opcode.SetPrivateField =>
              doSetPrivateField(readString(bytecode, pc + 1))

            case Opcode.DefinePrivateField =>
              doDefinePrivateField(readString(bytecode, pc + 1))
            case Opcode.Swap =>
              val a = stack(stackTop - 1)
              val b = stack(stackTop - 2)
              stack(stackTop - 1) = b
              stack(stackTop - 2) = a
              pc += 1

            case Opcode.Rotate =>
              val a = stack(stackTop - 1)
              val b = stack(stackTop - 2)
              val c = stack(stackTop - 3)
              stack(stackTop - 1) = c
              stack(stackTop - 2) = a
              stack(stackTop - 3) = b
              pc += 1

            // =========================================================================
            // Global Scope and Variable Declarations (DefVar, DefFun, PutGlobal, GetGlobal)
            // =========================================================================
    }
    false
  }

  /** Opcode dispatch group 11. Returns true when the function should return. */
  private def runGroup11(opcode: Opcode): Boolean = {
    opcode match {
            case Opcode.DefVar =>
              val varName = readString(bytecode, pc + 1)
              val value = stack(stackTop - 1)
              stackTop -= 1
              val withTarget =
                withStack.reverseIterator.find(obj =>
                  withObjectHasBinding(obj, varName)
                )
              withTarget match {
                case Some(obj) =>
                  // `var foo = value` inside `with`: the declaration is hoisted
                  // but the initializer is a PutValue that resolves through the
                  // object environment record.
                  if !ctx.globalScope.has(varName) then
                    ctx.globalScope.setVariable(varName, JSValue.Undefined)
                  obj.set(varName, value)(using ctx)
                  ctx.deletedGlobalProperties -= varName
                case None =>
                  if function.globalVarConfigurable then
                    ctx.global.defineProperty(
                      varName,
                      value,
                      enumerable = true,
                      writable = true,
                      configurable = true
                    )(using ctx)
                    ctx.deletedGlobalProperties -= varName
                  else {
                    ctx.globalScope.setVariable(varName, value)
                    ctx.global.getOwnPropertyDescriptor(varName) match {
                      case Some(_) => ctx.global.set(varName, value)(using ctx)
                      case None =>
                        ctx.global.defineProperty(
                          varName,
                      value,
                      enumerable = true,
                      writable = true,
                      configurable = false
                    )(using ctx)
                }
                ctx.deletedGlobalProperties -= varName
                  }
              }
              pc += 1 + stringOpSize(varName)

            case Opcode.DefFun =>
              val funName = readString(bytecode, pc + 1)
              val funcValue = stack(stackTop - 1)
              stackTop -= 1
              ctx.globalScope.setVariable(funName, funcValue)
              ctx.global.getOwnPropertyDescriptor(funName) match {
                case Some(_) => ctx.global.set(funName, funcValue)(using ctx)
                case None =>
                  ctx.global.defineProperty(
                    funName,
                    funcValue,
                    enumerable = true,
                    writable = true,
                    configurable = false
                  )(using ctx)
              }
              pc += 1 + stringOpSize(funName)

            case Opcode.PutGlobal =>
              resolvePutGlobal(readString(bytecode, pc + 1))

            case Opcode.GetGlobal =>
              resolveGetGlobal(readString(bytecode, pc + 1))

            case Opcode.GetGlobalOrUndefined =>
              resolveGetGlobal(
                readString(bytecode, pc + 1),
                throwIfUnresolved = false
              )

            case Opcode.GetGlobalWithBase =>
              resolveGetGlobalWithBase(readString(bytecode, pc + 1))

            case Opcode.PutGlobalWithBase =>
              resolvePutGlobalWithBase(readString(bytecode, pc + 1))

            // =========================================================================
            // Scope Management (EnterScope, LeaveScope) and Constants (GetConst)
            // =========================================================================
            case Opcode.EnterScope =>
              val scopeIndex = readInt32(bytecode, pc + 1)
              pc += 1 + 4

            case Opcode.LeaveScope =>
              val scopeIndex = readInt32(bytecode, pc + 1)
              pc += 1 + 4

            case Opcode.GetConst => resolveGetConst()

            case _ =>
              throw new RuntimeException(s"Unimplemented opcode: $opcode")
    }
    false
  }

  def run(): JSValue = {
    // Optional runaway-loop safety net. Zero means unlimited (the default):
    // the test262 runner cancels by wall-clock timeout and thread interrupt,
    // and ordinary embedders should not see large but finite programs aborted.
    val maxIterations = ctx.maxInstructionCount

    // Force the control-flow singletons to initialize at a shallow stack.
    // They are matched in this method's `catch`; loading them while
    // unwinding a StackOverflowError used to leave the class initializer in a
    // failed state, turning deep recursion into NoClassDefFoundError.
    val _ = (BreakException, ContinueException, Interpreter.breakSignal)

    ctx.enterJsFrame()
    try {
      breakable {
        while pc < bytecode.length do {
          iterations += 1
          if maxIterations > 0 && iterations > maxIterations then
            throw new RuntimeException(
              s"Infinite loop detected: executed $maxIterations instructions without terminating"
            )
          // Test runners and embedding hosts can cancel runaway execution by
          // interrupting the interpreter thread. Sampling the flag keeps the
          // check off the per-instruction hot path.
          if (iterations & 1023L) == 0L && Thread.currentThread().isInterrupted then
            throw new InterruptedException("JavaScript execution interrupted")
          try {
          ctx.updateTopFramePc(pc)
          val opcodeCode = bytecode(pc).toInt & 0xff
          val opcode = Opcode.lookup(opcodeCode) match {
            case null => Opcode.Invalid
            case op   => op
          }

          // Debug tracing
          if DebugTracer.global.isEnabled then
            DebugTracer.global.traceInstruction(
              pc = pc,
              opcode = opcode,
              stack = stack,
              stackTop = stackTop,
              locals = locals,
              localsCount = localsCount
            )
          if trace.isEnabled then {
            val stackSnapshot =
              (0 until stackTop).map(i => TraceValue.from(stack(i))).toVector
            val localsSnapshot =
              (0 until localsCount).map { i =>
                TraceLocal(i, localNameFor(i), TraceValue.from(locals(i).get))
              }.toVector
            val location =
              function.lineColForPc(pc).map { case (line, column) =>
                SourceLocation(line, column)
              }
            trace.recordInstruction(
              InstructionTrace(
                pc = pc,
                opcode = opcode,
                stack = stackSnapshot,
                locals = localsSnapshot,
                location = location
              )
            )
          }

          val group = BytecodeLoop.opcodeGroups(opcodeCode)
          if runOpcodeGroup(group, opcode) then break()
        } catch {
          case _: StackOverflowError =>
            // The host stack is exhausted (deep JS recursion or a recursive
            // native helper). QuickJS C checks its stack limit before every
            // call and raises RangeError; convert the host overflow into the
            // same JavaScript error, which `try`/`catch` can observe.
            val err = ctx.createError(
              quickjs.runtime.ErrorType.RangeError,
              "Maximum call stack size exceeded"
            )
            if !handleException(err) then
              throw new quickjs.runtime.JSException(err)
          case BreakException =>
            if tryStack.nonEmpty then {
              val handler = tryStack.remove(tryStack.length - 1)
              if handler.finallyPc >= 0 then {
                stackTop = handler.stackTop
                pendingException = Some(Interpreter.breakSignal)
                pc = handler.finallyPc
              } else break()
            } else break()
          case ContinueException =>
            if tryStack.nonEmpty then {
              val handler = tryStack.remove(tryStack.length - 1)
              if handler.finallyPc >= 0 then {
                stackTop = handler.stackTop
                pendingException = Some(Interpreter.continueSignal)
                pc = handler.finallyPc
              } else ()
            } else ()
          case pending: AwaitPending =>
            throw new AsyncSuspension(
              frame,
              function,
              trace,
              newTarget,
              pending.promise,
              interpreter
            )
          case jsEx: quickjs.runtime.JSException =>
            jsEx.getValue match {
              case JSValue.Object(obj) =>
                if ctx.isErrorObject(obj) then {
                  ctx.attachStack(obj)
                  attachErrorLocation(obj)
                }
              case _ =>
                ()
            }
            if !handleException(jsEx.getValue) then throw jsEx
          case ex: RuntimeException =>
            val err = interpreter.runtimeExceptionToError(ex)
            err match {
              case JSValue.Object(obj) =>
                if ctx.isErrorObject(obj) then attachErrorLocation(obj)
              case _ => ()
            }
            if !handleException(err) then
              throw new quickjs.runtime.JSException(err)
        }
      }
    }
    } finally ctx.exitJsFrame()
    result
  } // end run
} // end BytecodeLoop

object BytecodeLoop {
  /** Maps opcode byte values to their dispatch group. */
  private[interpreter] val opcodeGroups: Array[Int] = {
    val arr = new Array[Int](256)
    java.util.Arrays.fill(arr, -1)
    Opcode.values.foreach { op =>
      arr(op.code) = op match {
        case Opcode.Add => 4
        case Opcode.And => 6
        case Opcode.Await => 0
        case Opcode.AwaitAsync => 0
        case Opcode.Break => 7
        case Opcode.Call => 8
        case Opcode.CallMethod => 8
        case Opcode.Comma => 5
        case Opcode.Continue => 7
        case Opcode.DefFun => 11
        case Opcode.DefVar => 11
        case Opcode.DefinePrivateField => 10
        case Opcode.Delete => 3
        case Opcode.DeleteName => 3
        case Opcode.PrivateIn => 3
        case Opcode.Div => 4
        case Opcode.Drop => 0
        case Opcode.Dup => 0
        case Opcode.Dup2 => 1
        case Opcode.EnterScope => 11
        case Opcode.Eq => 5
        case Opcode.GetArg => 1
        case Opcode.GetConst => 11
        case Opcode.GetElem => 8
        case Opcode.GetException => 0
        case Opcode.GetGlobal => 11
        case Opcode.GetGlobalOrUndefined => 11
        case Opcode.GetGlobalWithBase => 11
        case Opcode.GetThisUnchecked => 1
        case Opcode.MarkThisInitialized => 1
        case Opcode.GetLoc => 1
        case Opcode.GetLocCheck => 1
        case Opcode.GetPrivateField => 10
        case Opcode.GetProp => 9
        case Opcode.GetRestArgs => 1
        case Opcode.GetThis => 1
        case Opcode.Goto => 7
        case Opcode.Gt => 5
        case Opcode.Gte => 5
        case Opcode.IfFalse => 7
        case Opcode.IfTrue => 7
        case Opcode.In => 7
        case Opcode.InitElem => 9
        case Opcode.Instanceof => 7
        case Opcode.Invalid => 0
        case Opcode.LNot => 2
        case Opcode.LeaveScope => 11
        case Opcode.LogicalAnd => 7
        case Opcode.LogicalOr => 7
        case Opcode.Lt => 5
        case Opcode.Lte => 5
        case Opcode.Mod => 4
        case Opcode.Mul => 4
        case Opcode.Neg => 2
        case Opcode.Neq => 5
        case Opcode.New => 8
        case Opcode.NewArray => 8
        case Opcode.NewObject => 8
        case Opcode.Nip => 1
        case Opcode.Nop => 0
        case Opcode.Not => 2
        case Opcode.Or => 6
        case Opcode.PopWith => 0
        case Opcode.Pos => 2
        case Opcode.PostDec => 3
        case Opcode.PostInc => 2
        case Opcode.Pow => 5
        case Opcode.PreDec => 2
        case Opcode.PreInc => 2
        case Opcode.PushFalse => 0
        case Opcode.PushFloat64 => 0
        case Opcode.PushI32 => 0
        case Opcode.PushNull => 0
        case Opcode.PushTrue => 0
        case Opcode.PushUndefined => 0
        case Opcode.PushWith => 0
        case Opcode.PutArg => 2
        case Opcode.PutGlobal => 11
        case Opcode.PutGlobalWithBase => 11
        case Opcode.PutLoc => 1
        case Opcode.RethrowIfPending => 0
        case Opcode.Return => 8
        case Opcode.ReturnUndef => 8
        case Opcode.Rotate => 10
        case Opcode.Sar => 7
        case Opcode.SetElem => 8
        case Opcode.SetLocConst => 1
        case Opcode.CloneLocRef => 1
        case Opcode.SetLocUninitialized => 1
        case Opcode.SetPrivateField => 10
        case Opcode.SetProp => 10
        case Opcode.Shl => 6
        case Opcode.Shr => 7
        case Opcode.StrictEq => 5
        case Opcode.StrictNeq => 6
        case Opcode.Sub => 4
        case Opcode.Swap => 10
        case Opcode.Throw => 0
        case Opcode.TryEnd => 0
        case Opcode.TryStart => 0
        case Opcode.Typeof => 3
        case Opcode.Xor => 6
        case _ => -1
      }
    }
    arr
  }
}

/** Signals that `await` suspended an async frame on a pending promise. */
private[interpreter] final case class AwaitPending(promise: JSValue)
    extends scala.util.control.ControlThrowable

/** A suspended async frame plus everything needed to resume it. */
private[interpreter] final class AsyncSuspension(
    val frame: Frame,
    val function: BytecodeFunction,
    val trace: TraceRecorder,
    val newTarget: JSValue,
    val awaited: JSValue,
    val interpreter: Interpreter
) extends Exception
