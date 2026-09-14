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
  private def iterations_=(v: Int) = frame.iterations = v
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
          withObjects = withStack.toList,
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
            interpreter.withNativeFrame(constructor.name)(constructor.call(args))
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
        val funcPrototype = prototypeSource match {
          case f: JSValue.Function =>
            f.funcObj.get("prototype")(using ctx) match {
              case JSValue.Object(proto) => proto
              case _                     => ctx.objectPrototype
            }
          case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
            nc.funcObj.get("prototype")(using ctx) match {
              case JSValue.Object(proto) => proto
              case _                     => ctx.objectPrototype
            }
          case _ =>
            func.funcObj.get("prototype")(using ctx) match {
              case JSValue.Object(proto) => proto
              case _                     => ctx.objectPrototype
            }
        }
        import quickjs.objmodel.JSObject
        val newObj = JSObject(prototype = funcPrototype, extensible = true)
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
          isAsync = func.isAsync,
          spanMap = func.spanMap,
          isStrict = func.isStrict,
          parameterScopeEndPc = func.parameterScopeEndPc
        )
        val retValue = interpreter.call(
          bcFunc,
          JSValue.Object(newObj),
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
          case _ => JSValue.Object(newObj)
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
      val hasLine = obj.getOwnProperty("lineNumber")(using ctx).nonEmpty
      val hasCol = obj.getOwnProperty("columnNumber")(using ctx).nonEmpty
      val hasLocatedStack = obj.getOwnProperty("stack")(using ctx) match {
        case Some(JSValue.JSStr(stackTrace)) =>
          """:\d+:\d+""".r.findFirstIn(stackTrace).nonEmpty
        case _ => false
      }
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
        obj.getOwnProperty("stack")(using ctx) match {
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

  private def handleException(value: JSValue): Boolean =
    if tryStack.nonEmpty then {
      val handler = tryStack.remove(tryStack.length - 1)
      stackTop = handler.stackTop
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
            if paramIndex >= 0 && paramIndex < localsCount && paramIndex < locals.length
            then newClosure(varName) = locals(paramIndex)
            else {
              val localVarIndex = function.localVarNames.indexOf(varName)
              val fromClosure = closure.get(varName)
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
        val funcObj = quickjs.objmodel.JSObject(
          prototype = ctx.functionPrototype,
          extensible = true
        )
        val funcValue = JSValue.Function(
          name = bcFunc.name,
          bytecode = bcFunc.bytecode,
          constants = bcFunc.constants,
          stackSize = bcFunc.stackSize,
          closure = newClosure,
          paramNames = bcFunc.paramNames,
          localVarNames = bcFunc.localVarNames,
          parentLocalVarNames = function.localVarNames,
          argumentsIndex = bcFunc.argumentsIndex,
          isConstructor = bcFunc.isConstructor,
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
        val hasPrototype = bcFunc.isConstructor || bcFunc.name != "<arrow>"
        if hasPrototype then {
          val protoObj = quickjs.objmodel.JSObject(
            prototype = ctx.objectPrototype,
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
      ctx.throwTypeError("Cannot read properties of null or undefined")
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
              if proto != null then proto.get(propName) else JSValue.Undefined
            case JSValue.Object(o) => o.get(propName)
            case _                 => JSValue.Undefined
          }
      case _: JSValue.Int32 | _: JSValue.Float64 =>
        ctx.global.get("Number") match {
          case JSValue.Native(c: quickjs.value.NativeConstructor) =>
            val proto = c.prototype;
            if proto != null then proto.get(propName) else JSValue.Undefined
          case JSValue.Object(o) => o.get(propName)
          case _                 => JSValue.Undefined
        }
      case JSValue.BigInt(_) =>
        // Auto-box through BigInt.prototype
        ctx.global.get("BigInt") match {
          case JSValue.Native(c: quickjs.value.NativeConstructor) =>
            c.prototype.get(propName)(using ctx)
          case JSValue.Object(o) => o.get(propName)
          case _                 => JSValue.Undefined
        }
      case JSValue.Bool(_) =>
        // Boolean primitives use ordinary property lookup through
        // Boolean.prototype (the transient wrapper is not observable here).
        ctx.global.get("Boolean") match {
          case JSValue.Native(c: quickjs.value.NativeConstructor) =>
            val proto = c.prototype
            if proto != null then proto.get(propName)(using ctx)
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
                      paramNames = func.paramNames,
                      localVarNames = func.localVarNames,
                      argumentsIndex = func.argumentsIndex,
                      isConstructor = func.isConstructor,
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
          withObjects = withStack.toList,
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
            val evalNewTarget =
              if native.name == "__directEvalField" then JSValue.Undefined
              else newTarget
            val evalResult =
              if args.isEmpty then JSValue.Undefined
              else
                args(0) match {
                  case JSValue.JSStr(code) if code.trim == "this" => thisValue
                  case JSValue.JSStr(code) if code.trim == "new.target" =>
                    evalNewTarget
                  case JSValue.JSStr(code) if code.trim == "super.f()" =>
                    val fv = thisValue match {
                      case JSValue.Object(obj) =>
                        obj.getPrototype match {
                          case null  => JSValue.Undefined;
                          case proto => proto.get("f")(using ctx)
                        }
                      case _ => JSValue.Undefined
                    }
                    fv match {
                      case f: JSValue.Function =>
                        val bcf = new BytecodeFunction(
                          name = f.name,
                          bytecode = f.bytecode,
                          constants = f.constants,
                          stackSize = f.stackSize,
                          freeVars = Array.empty,
                          paramNames = f.paramNames,
                          localVarNames = f.localVarNames,
                          argumentsIndex = f.argumentsIndex,
                          isConstructor = f.isConstructor,
                          isGenerator = f.isGenerator,
                          spanMap = f.spanMap,
                          parameterScopeEndPc = f.parameterScopeEndPc
                        )
                        interpreter.call(
                          bcf,
                          thisValue,
                          Array.empty,
                          f.closure,
                          withObjects = withStack.toList,
                          trace = trace
                        )
                      case JSValue.Native(nfw) =>
                        nfw match {
                          case nf: quickjs.value.NativeFunction =>
                            interpreter.withNativeFrame(nf.name) {
                              nf.call(Array(thisValue))
                            }
                          case ctor: quickjs.value.NativeConstructor =>
                            interpreter.withNativeFrame(ctor.name) {
                              ctor.call(Array.empty)
                            }
                          case _ => JSValue.Undefined
                        }
                      case _ => JSValue.Undefined
                    }
                  case JSValue.JSStr(code) =>
                    ctx.withSourceName("<eval>") {
                      val inheritedPrivateBindings =
                        if native.name == "__directEvalField" ||
                            native.name == "__directEvalPrivate"
                        then
                          args.lift(1) match {
                            case Some(JSValue.JSStr(encoded)) if encoded.nonEmpty =>
                              encoded.split("\u0000", -1).iterator.map { entry =>
                                val separator = entry.indexOf('\u001f')
                                if separator < 0 then entry -> entry
                                else
                                  entry.substring(0, separator) ->
                                    entry.substring(separator + 1)
                              }.toMap
                            case _ => Map.empty[String, String]
                          }
                        else Map.empty[String, String]
                      val inheritedPrivateNames =
                        inheritedPrivateBindings.keySet
                      val ast =
                        try
                          val tokens = quickjs.lexer.Lexer(code).tokenize()
                          new quickjs.parser.Parser(
                            tokens,
                            allowNewTargetAtTopLevel = true,
                            classFieldInitializerAtTopLevel =
                              native.name == "__directEvalField",
                            allowSuperPropertyAtTopLevel =
                              native.name == "__directEvalField",
                            allowedPrivateNamesAtTopLevel =
                              inheritedPrivateNames
                          ).parseScript()
                        catch
                          case error: RuntimeException =>
                            val message =
                              Option(error.getMessage)
                                .getOrElse("Invalid eval source")
                            val spanPattern =
                              raw"""Span\(\d+,\d+,(\d+),(\d+)\)""".r
                            val (line, column) =
                              spanPattern.findFirstMatchIn(message) match {
                                case Some(m) =>
                                  val zeroBasedLine = m.group(1).toInt
                                  val rawColumn = m.group(2).toInt
                                  (
                                    zeroBasedLine + 1,
                                    rawColumn + 1
                                  )
                                case None =>
                                  val offset =
                                    if message.contains("comment") then
                                      code.indexOf("/*")
                                    else if message.contains("regexp") then
                                      code.indexOf('/')
                                    else math.max(0, code.length - 1)
                                  val prefix = code.take(math.max(0, offset))
                                  val line = prefix.count(_ == '\n') + 1
                                  val column =
                                    offset - prefix.lastIndexOf('\n')
                                  (line, math.max(1, column))
                              }
                            ctx.createError("SyntaxError", message, 0) match {
                              case value @ JSValue.Object(errorObject) =>
                                errorObject.defineProperty(
                                  "lineNumber",
                                  JSValue.fromInt(line),
                                  enumerable = false
                                )
                                errorObject.defineProperty(
                                  "columnNumber",
                                  JSValue.fromInt(column),
                                  enumerable = false
                                )
                                val oldStack =
                                  errorObject.get("stack") match {
                                    case JSValue.JSStr(stack) => stack
                                    case _                    => ""
                                  }
                                errorObject.set(
                                  "stack",
                                  JSValue.fromString(
                                    s"    at <eval>:$line:$column\n$oldStack"
                                  )
                                )
                                throw quickjs.runtime.JSException(value)
                              case value =>
                                throw quickjs.runtime.JSException(value)
                            }
                      def collectEvalVarNames(
                          statements: Seq[quickjs.ast.Statement]
                      ): Set[String] = {
                        val names = mutable.LinkedHashSet.empty[String]
                        def collectPattern(pattern: quickjs.ast.BindingPattern): Unit =
                          pattern match {
                            case quickjs.ast.Identifier(name, _) =>
                              names += name
                            case quickjs.ast.BindingAssignment(target, _, _) =>
                              collectPattern(target)
                            case quickjs.ast.ArrayPattern(elements, _) =>
                              elements.foreach {
                                case p: quickjs.ast.BindingPattern =>
                                  collectPattern(p)
                                case _ => ()
                              }
                            case quickjs.ast.ObjectPattern(properties, rest, _) =>
                              properties.foreach(prop => collectPattern(prop.value))
                              rest match {
                                case r: quickjs.ast.RestElement =>
                                  collectPattern(r.argument)
                                case _ => ()
                              }
                            case quickjs.ast.RestElement(argument, _) =>
                              collectPattern(argument)
                          }
                        def collectStatement(stmt: quickjs.ast.Statement): Unit =
                          stmt match {
                          case quickjs.ast.VariableDeclaration(
                                quickjs.ast.VariableKind.Var,
                                declarations,
                                _
                              ) =>
                            declarations.foreach { decl =>
                              collectPattern(decl.id)
                            }
                          case quickjs.ast.FunctionDeclaration(id, _, _, _, _, _, _) =>
                            names += id.name
                          case quickjs.ast.BlockStatement(body, _) =>
                            body.foreach(collectStatement)
                          case quickjs.ast.IfStatement(_, consequent, alternate, _) =>
                            collectStatement(consequent)
                            if alternate != null then collectStatement(alternate)
                          case quickjs.ast.WhileStatement(_, body, _, _) =>
                            collectStatement(body)
                          case quickjs.ast.DoWhileStatement(body, _, _, _) =>
                            collectStatement(body)
                          case quickjs.ast.ForStatement(init, _, _, body, _, _) =>
                            init match {
                              case decl: quickjs.ast.VariableDeclaration =>
                                collectStatement(decl)
                              case _ => ()
                            }
                            collectStatement(body)
                          case quickjs.ast.ForInStatement(left, _, body, _, _) =>
                            left match {
                              case decl: quickjs.ast.VariableDeclaration =>
                                collectStatement(decl)
                              case _ => ()
                            }
                            collectStatement(body)
                          case quickjs.ast.ForOfStatement(left, _, body, _, _) =>
                            left match {
                              case decl: quickjs.ast.VariableDeclaration =>
                                collectStatement(decl)
                              case _ => ()
                            }
                            collectStatement(body)
                          case quickjs.ast.ForAwaitOfStatement(left, _, body, _, _) =>
                            left match {
                              case decl: quickjs.ast.VariableDeclaration =>
                                collectStatement(decl)
                              case _ => ()
                            }
                            collectStatement(body)
                          case quickjs.ast.SwitchStatement(_, cases, _) =>
                            cases.foreach(_.consequent.foreach(collectStatement))
                          case quickjs.ast.TryStatement(block, handler, finalizer, _) =>
                            collectStatement(block)
                            if handler != null then collectStatement(handler.body)
                            if finalizer != null then collectStatement(finalizer)
                          case quickjs.ast.WithStatement(_, body, _) =>
                            collectStatement(body)
                          case _ => ()
                        }
                        statements.foreach(collectStatement)
                        names.toSet
                      }
                      val evalVarNames = collectEvalVarNames(ast.body)
                      // Inherit strict mode from the enclosing function (like direct eval)
                      val strictAst =
                        if function.isStrict then ast.copy(strict = true)
                        else ast
                      val compiler = quickjs.compiler.Compiler()
                      def compileEval(): BytecodeFunction =
                        compiler.withEvalPrivateBindings(
                          inheritedPrivateBindings
                        ) {
                          compiler.withDirectEvalMode(
                            compiler.withREPLMode(
                              compiler.compileScript(strictAst)
                            )
                          )
                        }
                      val inheritedSuperVar =
                        if native.name == "__directEvalField" then
                          closure.keys.find(_.startsWith("__super_"))
                        else None
                      val isStaticField = thisValue match {
                        case _: JSValue.Function => true
                        case JSValue.Native(_)   => true
                        case _                   => false
                      }
                      val evalFunc = inheritedSuperVar match {
                        case Some(superVarName) =>
                          compiler.withEvalSuperContext(
                            superVarName,
                            isStaticField
                          )(compileEval())
                        case None => compileEval()
                      }
                      val evalClosure =
                        mutable.Map.empty[String, JSValue.VarRef]
                      evalClosure ++= closure
                      val inParameterScope =
                        function.parameterScopeEndPc > 0 &&
                          pc < function.parameterScopeEndPc
                      for (name, idx) <- function.paramNames.zipWithIndex do
                        if idx < locals.length then
                          evalClosure(name) = locals(idx)
                      for (name, idx) <- function.localVarNames.zipWithIndex do
                        if idx < locals.length &&
                            (!inParameterScope || name == "arguments")
                        then
                          evalClosure(name) = locals(idx)
                      val evalStack = new Array[JSValue](evalFunc.stackSize)
                      val evalSlotCount = math.max(
                        256,
                        math.max(
                          evalFunc.localVarNames.length,
                          evalFunc.argumentsIndex + 1
                        ) + 8
                      )
                      val evalLocals =
                        new Array[JSValue.VarRef](evalSlotCount)
                      var evalSlot = 0
                      while evalSlot < evalSlotCount do {
                        evalLocals(evalSlot) =
                          new JSValue.VarRef(JSValue.Undefined)
                        evalSlot += 1
                      }
                      val evalFrame = Frame(
                        stack = evalStack,
                        stackTop = 0,
                        pc = 0,
                        bytecode = evalFunc.bytecode,
                        args = Array.empty,
                        locals = evalLocals,
                        localsCount = evalSlotCount,
                        thisValue = thisValue,
                        closure = evalClosure,
                        withStack = withStack,
                        tryStack = mutable.ArrayBuffer.empty[TryHandler],
                        lastException = JSValue.Undefined,
                        pendingException = None,
                        result = JSValue.Undefined,
                        lastResolvedName = "",
                        lastResolvedKind = "",
                        iterations = 0
                      )
                      val result = ctx.withStackFrame(
                        "<eval>",
                        isNative = false,
                        spanMap = evalFunc.spanMap
                      ) {
                        new BytecodeLoop(
                          interpreter = interpreter,
                          frame = evalFrame,
                          function = evalFunc,
                          trace = trace,
                          newTarget = evalNewTarget
                        ).run()
                      }
                      if !evalFunc.isStrict then
                        for (name, idx) <- evalFunc.localVarNames.zipWithIndex do
                          if evalVarNames.contains(name) && idx < evalLocals.length
                          then {
                            evalLocals(idx).setEvalVar()
                            closure(name) = evalLocals(idx)
                          }
                      result match {
                        case functionValue: JSValue.Function =>
                          val functionOffset = code.indexOf("function")
                          if functionOffset >= 0 then {
                            val prefix = code.take(functionOffset)
                            val definitionLine = prefix.count(_ == '\n') + 1
                            val definitionColumn =
                              functionOffset - prefix.lastIndexOf('\n')
                            functionValue.funcObj.defineProperty(
                              "lineNumber",
                              JSValue.fromInt(definitionLine),
                              enumerable = false
                            )
                            functionValue.funcObj.defineProperty(
                              "columnNumber",
                              JSValue.fromInt(definitionColumn),
                              enumerable = false
                            )
                          }
                        case _ => ()
                      }
                      result
                    }
                  case other => other
                }
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
              paramNames = fn.paramNames,
              localVarNames = fn.localVarNames,
              argumentsIndex = fn.argumentsIndex,
              isConstructor = fn.isConstructor,
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
              withObjects = Nil,
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
              paramNames = fn.paramNames,
              localVarNames = fn.localVarNames,
              argumentsIndex = fn.argumentsIndex,
              isConstructor = fn.isConstructor,
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
              withObjects = Nil,
              trace = trace
            )
          case _ =>
            setExistingPrivateField()
        }
      case _ =>
        setExistingPrivateField()
    }
    stack(stackTop) = objValue; stackTop += 1
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
          paramNames = func.paramNames,
          localVarNames = func.localVarNames,
          argumentsIndex = func.argumentsIndex,
          isConstructor = func.isConstructor,
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
          withObjects = withStack.toList,
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
      case _ =>
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
    val indexValue = quickjs.runtime.builtins.BuiltinHelpers.toElementKey(
      stack(stackTop - 1)
    )
    val objValue = stack(stackTop - 2)
    stackTop -= 2
    if objValue == JSValue.Null || objValue == JSValue.Undefined then
      ctx.throwTypeError("Cannot read properties of null or undefined")
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
          case _ => JSValue.Undefined
        }
      case (JSValue.JSStr(str), JSValue.Int32(i)) =>
        if i >= 0 && i < str.length then JSValue.JSStr(str.charAt(i).toString)
        else JSValue.Undefined
      case (JSValue.JSStr(str), JSValue.Float64(d)) =>
        val i = d.toInt;
        if i >= 0 && i < str.length then JSValue.JSStr(str.charAt(i).toString)
        else JSValue.Undefined
      case _ => JSValue.Undefined
    }
    stack(stackTop) = result; stackTop += 1
    pc += 1
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
      withStack.reverseIterator.find(_.hasProperty(varName)(using ctx))
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
  private def resolveGetGlobal(
      varName: String,
      throwIfUnresolved: Boolean = true
  ): Unit = {
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
      withStack.reverseIterator.find(_.hasProperty(varName)(using ctx))
    def checkedBinding(value: JSValue): JSValue =
      if value == JSValue.Uninitialized then
        throw new RuntimeException(
          s"ReferenceError: Cannot access '$varName' before initialization"
        )
      value
    val result =
      if varName == "$newTarget" then newTarget
      else withResult match {
      case Some(obj) => obj.get(varName)(using ctx)
      case None      =>
        val paramIndex = function.paramNames.indexOf(varName)
        val localVarIndex = function.localVarNames.indexOf(varName)
        if paramIndex >= 0 && paramIndex < locals.length then
          checkedBinding(locals(paramIndex).get)
        else if localVarIndex >= 0 && localVarIndex < locals.length then
          checkedBinding(locals(localVarIndex).get)
        else closure.get(varName) match {
          case Some(varRef) =>
            varRef.get match {
              case JSValue.GlobalRef(refName) =>
                ctx.globalScope
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
              case value => value
            }
          case None =>
            ctx.globalScope
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
        }
      }
    stack(stackTop) = result; stackTop += 1
    pc += 1 + stringOpSize(varName)
  }

  /** Execute Instanceof opcode. */
  private def doInstanceof(): Unit = {
    val constructor = stack(stackTop - 1); val obj = stack(stackTop - 2);
    stackTop -= 2
    val ctorPrototype = constructor match {
      case JSValue.Object(ctorObj) => ctorObj.get("prototype")
      case func: JSValue.Function  => func.funcObj.get("prototype")
      case JSValue.Native(nc)      =>
        nc match {
          case ctor: quickjs.value.NativeConstructor =>
            JSValue.Object(ctor.prototype)
          case _ => JSValue.Null
        }
      case _ => JSValue.Null
    }
    val initialPrototype: quickjs.objmodel.JSObject | Null = obj match {
      case JSValue.Object(objVal) => objVal.getPrototype
      case func: JSValue.Function => func.funcObj.getPrototype
      case JSValue.Native(nf: quickjs.value.NativeFunction) => nf.funcObj.getPrototype
      case JSValue.Native(nc: quickjs.value.NativeConstructor) => nc.funcObj.getPrototype
      case JSValue.JSArrayVal(_) => ctx.arrayPrototype
      case _ => null
    }
    val r =
      if initialPrototype == null then JSValue.Bool(false)
      else {
        var cp: quickjs.objmodel.JSObject | Null = initialPrototype
        var found = false
        while !found && (cp != null) do
          ctorPrototype match {
            case JSValue.Object(protoObj) =>
              if cp == protoObj then found = true else cp = cp.getPrototype
            case _ => cp = null
          }
        JSValue.Bool(found)
      }
    stack(stackTop) = r; stackTop += 1; pc += 1
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
    val indexValue = quickjs.runtime.builtins.BuiltinHelpers.toElementKey(
      stack(stackTop - 2)
    );
    val objValue = stack(stackTop - 3); stackTop -= 3
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
              case JSValue.PromiseState.Pending => awaitedValue
            }
          case _ => awaitedValue
        }
      case _ => awaitedValue
    }
    stack(stackTop) = r; stackTop += 1; pc += 1
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
              value match {
                case JSValue.Object(obj) =>
                  withStack += obj
                case _ =>
                  throw new RuntimeException(
                    "TypeError: with object must be an object."
                  )
              }
              pc += 1

            case Opcode.PopWith =>
              if withStack.nonEmpty then withStack.remove(withStack.length - 1)
              pc += 1

            case Opcode.TryStart =>
              val catchPc = readInt32(bytecode, pc + 1)
              val finallyPc = readInt32(bytecode, pc + 5)
              tryStack += TryHandler(catchPc, finallyPc, stackTop)
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
              stack(stackTop) = thisValue
              stackTop += 1
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

            // =========================================================================
            // Binary Arithmetic Operations
            // =========================================================================
    }
    false
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
                  val truncated = na / nb
                  val truncatedInt =
                    if truncated >= 0 then math.floor(truncated)
                    else math.ceil(truncated)
                  JSValue.fromDouble(na - truncatedInt * nb)
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
              return true

            case Opcode.ReturnUndef =>
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
    // Large but finite JavaScript loops (for example QuickJS's 100,000-item
    // rope stress test) execute several bytecodes per source iteration. The
    // limit is a runaway-loop safety net; the test runner additionally cancels
    // by wall-clock timeout.
    val maxIterations = 100000000

    breakable {
      while pc < bytecode.length do {
        iterations += 1
        if iterations > maxIterations then
          throw new RuntimeException(
            s"Infinite loop detected: executed $maxIterations instructions without terminating"
          )
        // Test runners and embedding hosts can cancel runaway execution by
        // interrupting the interpreter thread. Sampling the flag keeps the
        // check off the per-instruction hot path.
        if (iterations & 1023) == 0 && Thread.currentThread().isInterrupted then
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
        case Opcode.Break => 7
        case Opcode.Call => 8
        case Opcode.CallMethod => 8
        case Opcode.Comma => 5
        case Opcode.Continue => 7
        case Opcode.DefFun => 11
        case Opcode.DefVar => 11
        case Opcode.DefinePrivateField => 10
        case Opcode.Delete => 3
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
        case Opcode.PutLoc => 1
        case Opcode.RethrowIfPending => 0
        case Opcode.Return => 8
        case Opcode.ReturnUndef => 8
        case Opcode.Rotate => 10
        case Opcode.Sar => 7
        case Opcode.SetElem => 8
        case Opcode.SetLocConst => 1
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