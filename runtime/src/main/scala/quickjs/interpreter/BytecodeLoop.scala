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
      val adjCol = Math.max(1, col - 1)
      val hasLine = obj.getOwnProperty("lineNumber")(using ctx).nonEmpty
      val hasCol = obj.getOwnProperty("columnNumber")(using ctx).nonEmpty
      if !hasLine then
        obj.defineProperty(
          "lineNumber",
          JSValue.fromInt(line),
          enumerable = false
        )(using ctx)
      if !hasCol then
        obj.defineProperty(
          "columnNumber",
          JSValue.fromInt(adjCol),
          enumerable = false
        )(using ctx)
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
              if localVarIndex >= 0 then
                newClosure(varName) = locals(localVarIndex)
              else {
                val fromClosure = closure.get(varName)
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
          isStrict = bcFunc.isStrict
        )
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
            enumerable = false
          )(using ctx)
        }
        funcObj.defineProperty(
          "length",
          JSValue.fromInt(bcFunc.length),
          enumerable = false
        )(using ctx)
        funcObj.defineProperty(
          "name",
          JSValue.fromString(bcFunc.name),
          enumerable = false
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
        if propName == "length" then JSValue.fromInt(arrVal.value.length)
        else if propName == "toString" then
          Interpreter.arrayToStringNative(arrVal)
        else
          arrVal.value.getProperty(propName) match {
            case Some(value) => value
            case None        =>
              val r = ctx.arrayPrototype.get(propName)(using ctx)
              if r == JSValue.Undefined then
                ctx.global.get("Array") match {
                  case JSValue.Object(o) => o.get(propName)
                  case _                 => JSValue.Undefined
                }
              else r
          }
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
        if propName == "toString" then
          Interpreter.primitiveToStringNative("toString", objValue)
        else
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
        if propName == "toString" then
          Interpreter.primitiveToStringNative("toString", objValue)
        else JSValue.Undefined
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
                      isStrict = func.isStrict
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
    stackTop -= (argc + 1)
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
          isStrict = func.isStrict
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
          trace = trace
        )
        stack(stackTop) = ret; stackTop += 1
      case JSValue.Native(nativeFuncWrapper) =>
        nativeFuncWrapper match {
          case native: quickjs.value.NativeFunction if native.name == "eval" =>
            val evalResult =
              if args.isEmpty then JSValue.Undefined
              else
                args(0) match {
                  case JSValue.JSStr(code) if code.trim == "this" => thisValue
                  case JSValue.JSStr(code) if code.trim == "new.target" =>
                    newTarget
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
                          spanMap = f.spanMap
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
                      val tokens = quickjs.lexer.Lexer(code).tokenize()
                      val ast = quickjs.parser.Parser(tokens).parseScript()
                      // Inherit strict mode from the enclosing function (like direct eval)
                      val strictAst =
                        if function.isStrict then ast.copy(strict = true)
                        else ast
                      val compiler = quickjs.compiler.Compiler()
                      val evalFunc =
                        compiler.withREPLMode(compiler.compileScript(strictAst))
                      val evalClosure =
                        mutable.Map.empty[String, JSValue.VarRef]
                      evalClosure ++= closure
                      for (name, idx) <- function.paramNames.zipWithIndex do
                        if idx < locals.length then
                          evalClosure(name) = locals(idx)
                      for (name, idx) <- function.localVarNames.zipWithIndex do
                        if idx < locals.length then
                          evalClosure(name) = locals(idx)
                      interpreter.call(
                        evalFunc,
                        thisValue,
                        Array.empty,
                        evalClosure,
                        newTarget,
                        withStack.toList,
                        trace = trace
                      )
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
            throw new RuntimeException(
              s"TypeError: Invalid native function: $nativeFuncWrapper"
            )
        }
      case JSValue.Undefined =>
        throw new RuntimeException(
          s"TypeError: Cannot call non-function value: undefined (lastLookup=$lastResolvedName, kind=$lastResolvedKind, this=$thisValue)"
        )
      case other =>
        throw new RuntimeException(
          s"TypeError: Cannot call non-function value: $other"
        )
    }
    pc += 5
  }

  /** Execute a New opcode. */
  private def doNew(): Unit = {
    val argc = readInt32(bytecode, pc + 1)
    val constructorValue = stack(stackTop - argc - 1)
    val args = new Array[JSValue](argc)
    for i <- 0 until argc do args(i) = stack(stackTop - argc + i)
    stackTop -= (argc + 1)
    val result = constructorValue match {
      case JSValue.Native(constructorWrapper) =>
        constructorWrapper match {
          case constructor: quickjs.value.NativeConstructor =>
            interpreter.withNativeFrame(constructor.name) {
              constructor.construct(args)
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
        val funcPrototype = func.funcObj.get("prototype")(using ctx) match {
          case JSValue.Object(proto) => proto;
          case _                     => ctx.objectPrototype
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
          spanMap = func.spanMap,
          isStrict = func.isStrict
        )
        val retValue = interpreter.call(
          bcFunc,
          JSValue.Object(newObj),
          args,
          func.closure,
          constructorValue,
          withStack.toList,
          trace = trace
        )
        // If constructor returns an object (including Function, Array, etc.), use it; otherwise use new instance
        retValue match {
          case _: JSValue.Object | _: JSValue.Function | _: JSValue.JSArrayVal |
              _: JSValue.Generator | _: JSValue.Promise | _: JSValue.Native |
              _: JSValue.AsyncFunction =>
            retValue
          case JSValue.Null | JSValue.Undefined | _: JSValue.Bool |
              _: JSValue.Int32 | _: JSValue.Float64 | _: JSValue.JSStr |
              _: JSValue.BigInt | _: JSValue.Symbol =>
            JSValue.Object(newObj)
          case _ => JSValue.Object(newObj)
        }
      case _ =>
        throw new RuntimeException(
          s"TypeError: Cannot use 'new' with non-constructor: $constructorValue"
        )
    }
    stack(stackTop) = result; stackTop += 1
    pc += 5
  }

  /** Resolve a GetPrivateField opcode. */
  private def resolveGetPrivateField(fieldName: String): Unit = {
    val objValue = stack(stackTop - 1)
    stackTop -= 1
    val targetObj = objValue match {
      case JSValue.Object(obj) => obj;
      case f: JSValue.Function => f.funcObj
      case _                   =>
        ctx.throwTypeError(
          s"Cannot read private field #$fieldName from non-object"
        )
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
              isStrict = fn.isStrict
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
            targetObj.getOwnProperty("__private__") match {
              case Some(JSValue.Object(privMapObj)) =>
                privMapObj.get(fieldName)(using ctx)
              case _ =>
                ctx.throwTypeError(
                  s"Cannot read private field #$fieldName from an object whose class did not declare it"
                )
            }
        }
      case _ =>
        targetObj.getOwnProperty("__private__") match {
          case Some(JSValue.Object(privMapObj)) =>
            privMapObj.get(fieldName)(using ctx)
          case _ =>
            ctx.throwTypeError(
              s"Cannot read private field #$fieldName from an object whose class did not declare it"
            )
        }
    }
    stack(stackTop) = result; stackTop += 1
    pc += 1 + stringOpSize(fieldName)
  }

  /** Execute a SetPrivateField opcode. */
  private def doSetPrivateField(fieldName: String): Unit = {
    val value = stack(stackTop - 1)
    val objValue = stack(stackTop - 2)
    stackTop -= 2
    val targetObj = objValue match {
      case JSValue.Object(obj) => obj;
      case f: JSValue.Function => f.funcObj
      case _                   =>
        ctx.throwTypeError(
          s"Cannot write private field #$fieldName to non-object"
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
              isStrict = fn.isStrict
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
            val privMapObj = getOrCreatePrivateMap(targetObj)
            privMapObj.set(fieldName, value)(using ctx)
        }
      case _ =>
        val privMapObj = getOrCreatePrivateMap(targetObj)
        privMapObj.set(fieldName, value)(using ctx)
    }
    stack(stackTop) = objValue; stackTop += 1
    pc += 1 + stringOpSize(fieldName)
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
    stackTop -= (argc + 2)
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
        val ret = interpreter.call(
          bcFunc,
          thisVal,
          args,
          func.closure,
          withObjects = withStack.toList,
          trace = trace
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
            throw new RuntimeException(
              s"TypeError: Invalid native function: $nativeFuncWrapper"
            )
        }
      case JSValue.Undefined =>
        throw new RuntimeException(
          s"TypeError: Cannot call non-function value: undefined (lastLookup=$lastResolvedName, kind=$lastResolvedKind, this=$thisVal)"
        )
      case other =>
        throw new RuntimeException(
          s"TypeError: Cannot call non-function value: $other"
        )
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

  private def doGetElem(): Unit = {
    val indexValue = stack(stackTop - 1)
    val objValue = stack(stackTop - 2)
    stackTop -= 2
    val result = (objValue, indexValue) match {
      case (JSValue.JSArrayVal(arr), JSValue.Int32(i))   => arr.get(i)
      case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) => arr.get(d.toInt)
      case (JSValue.JSArrayVal(arr), JSValue.JSStr(propName)) =>
        if interpreter.isArrayIndexKey(propName) then arr.get(propName.toInt)
        else interpreter.resolveArrayProperty(arr, propName)
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
          d.toInt.toString,
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
          d.toInt.toString,
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
    val withTarget =
      withStack.reverseIterator.find(_.hasProperty(varName)(using ctx))
    withTarget match {
      case Some(obj) => obj.set(varName, value)(using ctx)
      case None      =>
        closure.get(varName) match {
          case Some(varRef) =>
            varRef.get match {
              case JSValue.GlobalRef(refName) =>
                ctx.globalScope.setVariable(refName, value)
              case _ =>
                if varRef.isConst && varRef.get != JSValue.Uninitialized then
                  throw new RuntimeException(
                    "TypeError: Assignment to constant variable."
                  )
                varRef.set(value)
            }
          case None =>
            val existsInGlobal =
              ctx.globalScope.has(varName) || ctx.global.get(varName)(using
                ctx
              ) != JSValue.Undefined
            if function.isStrict && !existsInGlobal then
              throw new RuntimeException(
                s"ReferenceError: $varName is not defined"
              )
            ctx.globalScope.setVariable(varName, value)
        }
    }
    pc += 1 + stringOpSize(varName)
  }

  /** Resolve a GetGlobal opcode. */
  private def resolveGetGlobal(varName: String): Unit = {
    lastResolvedName = varName; lastResolvedKind = "global"
    val withResult =
      withStack.reverseIterator.find(_.hasProperty(varName)(using ctx))
    val result = withResult match {
      case Some(obj) => obj.get(varName)(using ctx)
      case None      =>
        closure.get(varName) match {
          case Some(varRef) =>
            varRef.get match {
              case JSValue.GlobalRef(refName) =>
                ctx.globalScope
                  .getVariable(refName)
                  .orElse {
                    val gv = ctx.global.get(refName);
                    if gv != JSValue.Undefined then Some(gv) else None
                  }
                  .getOrElse {
                    ctx.globalScope
                      .getFunction(refName)
                      .getOrElse(JSValue.Undefined)
                  }
              case value => value
            }
          case None =>
            ctx.globalScope
              .getVariable(varName)
              .orElse {
                val gv = ctx.global.get(varName);
                if gv != JSValue.Undefined then Some(gv) else None
              }
              .getOrElse {
                ctx.globalScope
                  .getFunction(varName)
                  .getOrElse(JSValue.Undefined)
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
    val r = obj match {
      case JSValue.Object(objVal) =>
        var cp: quickjs.objmodel.JSObject | Null = objVal.getPrototype;
        var found = false
        while !found && (cp != null) do
          ctorPrototype match {
            case JSValue.Object(protoObj) =>
              if cp == protoObj then found = true else cp = cp.getPrototype
            case _ => cp = null
          }
        JSValue.Bool(found)
      case _ => JSValue.Bool(false)
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
        if propName == "length" then arr.setLength(value.toNumber.toInt)
        else if interpreter.isArrayIndexKey(propName) then
          if function.isStrict && !arr.isExtensible && !arr.hasIndex(
              propName.toInt
            )
          then
            ctx.throwTypeError(
              "Cannot add property '" + propName + "', object is not extensible"
            )
          else arr.set(propName.toInt, value)
        else arr.setProperty(propName, value)
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
        throw new RuntimeException(
          s"Cannot set property on non-object: $objValue"
        )
    }
    stack(stackTop) = objValue; stackTop += 1; pc += 1 + stringOpSize(propName)
  }

  /** Execute SetElem opcode. */
  private def doSetElem(): Unit = {
    val value = stack(stackTop - 1); val indexValue = stack(stackTop - 2);
    val objValue = stack(stackTop - 3); stackTop -= 3
    (objValue, indexValue) match {
      case (JSValue.JSArrayVal(arr), JSValue.Int32(i)) =>
        if function.isStrict && !arr.isExtensible && !arr.hasIndex(i) then
          ctx.throwTypeError(
            "Cannot add property '" + i + "', object is not extensible"
          )
        else arr.set(i, value)
      case (JSValue.JSArrayVal(arr), JSValue.Float64(d)) =>
        val i = d.toInt
        if function.isStrict && !arr.isExtensible && !arr.hasIndex(i) then
          ctx.throwTypeError(
            "Cannot add property '" + i + "', object is not extensible"
          )
        else arr.set(i, value)
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
          d.toInt.toString,
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
          d.toInt.toString,
          value,
          withStack.toList,
          trace,
          function.isStrict
        )
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
      case _ => ()
    }
    stack(stackTop) = value; stackTop += 1; pc += 1
  }

  /** Execute Delete opcode. */
  private def doDelete(): Unit = {
    val propName = stack(stackTop - 1); val obj = stack(stackTop - 2);
    stackTop -= 2
    val prop = propName match {
      case JSValue.JSStr(s) => s; case _ => propName.toNumber.toInt.toString
    }
    val r = obj match {
      case JSValue.Object(o) => JSValue.Bool(o.deleteProperty(prop)(using ctx))
      case JSValue.Native(nf: quickjs.value.NativeFunction) =>
        JSValue.Bool(nf.funcObj.deleteProperty(prop)(using ctx))
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        JSValue.Bool(nc.funcObj.deleteProperty(prop)(using ctx))
      case JSValue.Null | JSValue.Undefined =>
        if function.isStrict then
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
                  JSValue.fromString(
                    "Cannot delete property of null or undefined"
                  )
              }
            case _ =>
              JSValue.fromString("Cannot delete property of null or undefined")
          }
          throw new quickjs.runtime.JSException(errObj)
        else JSValue.Bool(true)
      case _ => JSValue.Bool(true)
    }
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
  private def doDefinePrivateField(fieldName: String): Unit = {
    val value = stack(stackTop - 1); val objValue = stack(stackTop - 2);
    stackTop -= 2
    objValue match {
      case JSValue.Object(obj) =>
        val privMapObj = getOrCreatePrivateMap(obj)
        privMapObj.defineProperty(
          fieldName,
          value,
          enumerable = false,
          writable = true,
          configurable = true
        )
      case _ =>
        ctx.throwTypeError(
          s"Cannot define private field #$fieldName on non-object"
        )
    }
    stack(stackTop) = objValue; stackTop += 1; pc += 1 + stringOpSize(fieldName)
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

  def run(): JSValue = {
    val maxIterations = 100000

    breakable {
      while pc < bytecode.length do {
        iterations += 1
        if iterations > maxIterations then
          throw new RuntimeException(
            s"Infinite loop detected: executed $maxIterations instructions without terminating"
          )
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

          opcode match {
            // =========================================================================
            // Control Flow & Exception Handling
            // =========================================================================
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

            // =========================================================================
            // Variable Access (Locals and Arguments)
            // =========================================================================
            case Opcode.GetLoc =>
              val index = readInt32(bytecode, pc + 1)
              if index < 0 || index >= locals.length then
                throw new RuntimeException(
                  s"GetLoc: Index $index out of bounds for locals array (length ${locals.length})"
                )
              stack(stackTop) = locals(index).get
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
              stack(stackTop) = locals(index).get
              stackTop += 1
              pc += 5

            case Opcode.PutArg =>
              val index = readInt32(bytecode, pc + 1)
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
                case _                 => JSValue.fromDouble(-a.toNumber)
              }
              stack(stackTop) = r
              stackTop += 1
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
                case _                 => JSValue.Int32(~a.toNumber.toInt)
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
                case _ => JSValue.fromDouble(a.toNumber + 1)
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
                  val oldNum = a.toNumber
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
                case _ => JSValue.fromDouble(a.toNumber - 1)
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.PostDec =>
              val a = stack(stackTop - 1)
              stackTop -= 1
              val (oldVal, newVal) = a match {
                case JSValue.BigInt(b) =>
                  (a, JSValue.BigInt(b.subtract(java.math.BigInteger.ONE)))
                case _ =>
                  val oldNum = a.toNumber
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
                    case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                      JSValue.Bool(
                        nf.funcObj.deleteSymbolProperty(sym)(using ctx)
                      )
                    case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                      JSValue.Bool(
                        nc.funcObj.deleteSymbolProperty(sym)(using ctx)
                      )
                    case JSValue.Null | JSValue.Undefined =>
                      if function.isStrict then {
                        val typeErrorValue = ctx.global.get("TypeError")
                        val errObj = typeErrorValue match {
                          case JSValue.Native(nativeCtor) =>
                            nativeCtor match {
                              case ctor: quickjs.value.NativeConstructor =>
                                ctor.call(
                                  Array(
                                    JSValue.fromString(
                                      "Cannot delete property of null or undefined"
                                    )
                                  )
                                )(using ctx)
                              case _ =>
                                JSValue.fromString(
                                  "Cannot delete property of null or undefined"
                                )
                            }
                          case _ =>
                            JSValue.fromString(
                              "Cannot delete property of null or undefined"
                            )
                        }
                        throw new quickjs.runtime.JSException(errObj)
                      } else JSValue.Bool(true)
                    case _ => JSValue.Bool(true)
                  }
                case _ =>
                  val prop = propName match {
                    case JSValue.JSStr(s) => s
                    case _                => propName.toNumber.toInt.toString
                  }
                  obj match {
                    case JSValue.Object(o) =>
                      JSValue.Bool(o.deleteProperty(prop)(using ctx))
                    case JSValue.Native(nf: quickjs.value.NativeFunction) =>
                      JSValue.Bool(nf.funcObj.deleteProperty(prop)(using ctx))
                    case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
                      JSValue.Bool(nc.funcObj.deleteProperty(prop)(using ctx))
                    case JSValue.Null | JSValue.Undefined =>
                      if function.isStrict then {
                        val typeErrorValue = ctx.global.get("TypeError")
                        val errObj = typeErrorValue match {
                          case JSValue.Native(nativeCtor) =>
                            nativeCtor match {
                              case ctor: quickjs.value.NativeConstructor =>
                                ctor.call(
                                  Array(
                                    JSValue.fromString(
                                      "Cannot delete property of null or undefined"
                                    )
                                  )
                                )(using ctx)
                              case _ =>
                                JSValue.fromString(
                                  "Cannot delete property of null or undefined"
                                )
                            }
                          case _ =>
                            JSValue.fromString(
                              "Cannot delete property of null or undefined"
                            )
                        }
                        throw new quickjs.runtime.JSException(errObj)
                      } else JSValue.Bool(true)
                    case _ =>
                      JSValue.Bool(true)
                  }
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            // =========================================================================
            // Binary Arithmetic Operations
            // =========================================================================
            case Opcode.Add =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.add(a, b)
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Sub =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.subtract(a, b)
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Mul =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.multiply(a, b)
              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.Div =>
              val b = stack(stackTop - 1)
              val a = stack(stackTop - 2)
              stackTop -= 2
              val r = JSValue.divide(a, b)
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
                  val na = a.toNumber
                  val nb = b.toNumber
                  val truncated = na / nb
                  val truncatedInt =
                    if truncated >= 0 then math.floor(truncated)
                    else math.ceil(truncated)
                  JSValue.fromDouble(na - truncatedInt * nb)
              }
              stack(stackTop) = r
              stackTop += 1
              pc += 1

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
                  val na = a.toNumber
                  val nb = b.toNumber
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
              val constructor = stack(stackTop - 1)
              val obj = stack(stackTop - 2)
              stackTop -= 2

              val ctorPrototype = constructor match {
                case JSValue.Object(ctorObj)    => ctorObj.get("prototype")
                case func: JSValue.Function     => func.funcObj.get("prototype")
                case JSValue.Native(nativeCtor) =>
                  nativeCtor match {
                    case ctor: quickjs.value.NativeConstructor =>
                      JSValue.Object(ctor.prototype)
                    case _ => JSValue.Null
                  }
                case _ => JSValue.Null
              }

              val r = obj match {
                case JSValue.Object(objVal) =>
                  var currentProto: quickjs.objmodel.JSObject | Null =
                    objVal.getPrototype
                  var found = false
                  while !found && (currentProto != null) do
                    ctorPrototype match {
                      case JSValue.Object(protoObj) =>
                        if currentProto == protoObj then found = true
                        else currentProto = currentProto.getPrototype
                      case _ =>
                        currentProto = null
                    }
                  JSValue.Bool(found)
                case _ => JSValue.Bool(false)
              }

              stack(stackTop) = r
              stackTop += 1
              pc += 1

            case Opcode.In =>
              val propName = stack(stackTop - 2)
              val objVal = stack(stackTop - 1)
              stackTop -= 2
              val prop = propName match {
                case JSValue.JSStr(s) => s
                case _                => propName.toNumber.toInt.toString
              }
              val r = objVal match {
                case JSValue.Object(o) =>
                  JSValue.Bool(o.hasProperty(prop))
                case _ =>
                  JSValue.Bool(false)
              }
              stack(stackTop) = r
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

            case Opcode.Return =>
              result = stack(stackTop - 1)
              break

            case Opcode.ReturnUndef =>
              result = JSValue.Undefined
              break

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
                    arr.setLength(value.toNumber.toInt)
                  else if interpreter.isArrayIndexKey(propName) then
                    if function.isStrict && !arr.isExtensible && !arr.hasIndex(
                        propName.toInt
                      )
                    then
                      ctx.throwTypeError(
                        "Cannot add property '" + propName + "', object is not extensible"
                      )
                    else arr.set(propName.toInt, value)
                  else arr.setProperty(propName, value)
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
                  throw new RuntimeException(
                    s"Cannot set property on non-object: $objValue"
                  )
              }

              stack(stackTop) = objValue
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
            case Opcode.DefVar =>
              val varName = readString(bytecode, pc + 1)
              val value = stack(stackTop - 1)
              stackTop -= 1
              ctx.globalScope.setVariable(varName, value)
              pc += 1 + stringOpSize(varName)

            case Opcode.DefFun =>
              val funName = readString(bytecode, pc + 1)
              val funcValue = stack(stackTop - 1)
              stackTop -= 1
              ctx.globalScope.setVariable(funName, funcValue)
              pc += 1 + stringOpSize(funName)

            case Opcode.PutGlobal =>
              resolvePutGlobal(readString(bytecode, pc + 1))

            case Opcode.GetGlobal =>
              resolveGetGlobal(readString(bytecode, pc + 1))

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
  // No extra state needed
}
