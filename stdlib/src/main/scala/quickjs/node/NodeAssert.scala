package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.{JSArray, JSObject}

/** Node's `assert` module. */
object NodeAssert {

  /** Object.is semantics. */
  def sameValueStrict(a: JSValue, b: JSValue): Boolean = (a, b) match {
    case (JSValue.Int32(x), JSValue.Float64(y)) =>
      y == x.toDouble && !(x == 0 && 1.0 / y < 0)
    case (JSValue.Float64(x), JSValue.Int32(y)) =>
      x == y.toDouble && !(y == 0 && 1.0 / x < 0)
    case (JSValue.Float64(x), JSValue.Float64(y)) =>
      (x.isNaN && y.isNaN) || (x == y && (x != 0 || 1.0 / x == 1.0 / y))
    case _ => NodeHelpers.sameValue(a, b)
  }

  /** Structural equality. `strict` selects SameValue for primitives and
    * requires matching prototypes.
    */
  def deepEqualValues(a: JSValue, b: JSValue, strict: Boolean)(using
      ctx: JSContext
  ): Boolean = {
    if strict then {
      if sameValueStrict(a, b) then return true
    } else if a == b || NodeHelpers.sameValue(a, b) then return true

    def numberEqual(x: JSValue, y: JSValue): Boolean = {
      val nx = x.toNumber
      val ny = y.toNumber
      if nx.isNaN || ny.isNaN then strict && nx.isNaN && ny.isNaN
      else if strict then sameValueStrict(x, y)
      else nx == ny
    }

    (a, b) match {
      case (x @ (_: JSValue.Int32 | _: JSValue.Float64), y @ (_: JSValue.Int32 | _: JSValue.Float64)) =>
        numberEqual(x, y)
      case (JSValue.JSArrayVal(x), JSValue.JSArrayVal(y)) =>
        x.getLength == y.getLength && {
          var i = 0
          var equal = true
          while i < x.getLength && equal do {
            equal = deepEqualValues(x.get(i), y.get(i), strict)
            i += 1
          }
          equal
        }
      case (JSValue.Object(x), JSValue.Object(y)) =>
        if strict && (x.getPrototype ne y.getPrototype) then false
        else if x.getOwnPropertyRaw("__dateValue").isDefined &&
          y.getOwnPropertyRaw("__dateValue").isDefined
        then
          x.getOwnPropertyRaw("__dateValue").get.toNumber ==
            y.getOwnPropertyRaw("__dateValue").get.toNumber
        else if x.getOwnPropertyRaw("__regexpPattern").isDefined &&
          y.getOwnPropertyRaw("__regexpPattern").isDefined
        then
          x.getOwnPropertyRaw("__regexpPattern") == y.getOwnPropertyRaw("__regexpPattern") &&
            x.getOwnPropertyRaw("__regexpFlags") == y.getOwnPropertyRaw("__regexpFlags")
        else if x.getOwnPropertyRaw("__taView").isDefined &&
          y.getOwnPropertyRaw("__taView").isDefined
        then
          NodeBuffer.bytesOfValue(a) == NodeBuffer.bytesOfValue(b)
        else {
          def keys(obj: JSObject): Seq[String] =
            obj
              .getAllOwnPropertyKeys()
              .filterNot(_.startsWith("__"))
              .filter(k => obj.getOwnPropertyDescriptor(k).exists(_._2.enumerable))
              .sorted
          val xKeys = keys(x)
          val yKeys = keys(y)
          xKeys == yKeys && xKeys.forall { key =>
            deepEqualValues(
              x.getOwnPropertyRaw(key).getOrElse(JSValue.Undefined),
              y.getOwnPropertyRaw(key).getOrElse(JSValue.Undefined),
              strict
            )
          }
        }
      case _ => false
    }
  }

  private def errorName(value: JSValue)(using ctx: JSContext): String =
    BuiltinHelpers.getPropertyWithGetter(value, "name") match {
      case JSValue.JSStr(s) => s
      case _                => "Error"
    }

  private def errorMessage(value: JSValue)(using ctx: JSContext): String =
    BuiltinHelpers.getPropertyWithGetter(value, "message") match {
      case JSValue.JSStr(s) => s
      case JSValue.Undefined => ""
      case other             => NodeHelpers.toStr(other)
    }

  def create()(using ctx: JSContext): JSValue = {
    var assertFnRef: NativeFunction = null

    def strip(args: Array[JSValue]): Array[JSValue] =
      if args.nonEmpty then
        args(0) match {
          case JSValue.Native(nf: NativeFunction)
              if assertFnRef != null &&
                nf.asInstanceOf[AnyRef].eq(assertFnRef.asInstanceOf[AnyRef]) =>
            args.drop(1)
          case _ => args
        }
      else args

    def fail(
        message: String,
        actual: JSValue = JSValue.Undefined,
        expected: JSValue = JSValue.Undefined,
        operator: String = ""
    )(using ctx: JSContext): Nothing = {
      val error = ctx.createError("Error", message)
      error match {
        case JSValue.Object(obj) =>
          obj.defineProperty("name", JSValue.fromString("AssertionError"), enumerable = false, writable = true, configurable = true)
          obj.defineProperty("code", JSValue.fromString("ERR_ASSERTION"), enumerable = false, writable = true, configurable = true)
          obj.defineProperty("actual", actual, enumerable = false, writable = true, configurable = true)
          obj.defineProperty("expected", expected, enumerable = false, writable = true, configurable = true)
          obj.defineProperty("operator", JSValue.fromString(operator), enumerable = false, writable = true, configurable = true)
        case _ => ()
      }
      throw new quickjs.runtime.JSException(error)
    }

    def assertOn(condition: Boolean, message: JSValue)(using ctx: JSContext): Unit =
      if !condition then {
        val text =
          message match {
            case JSValue.Undefined => "The expression evaluated to a falsy value"
            case JSValue.JSStr(s)  => s
            case other             => NodeHelpers.toStr(other)
          }
        fail(s"[$text]", operator = "==")
      }

    assertFnRef = NativeFunction(
      name = "assert",
      length = 2,
      impl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = strip(args)
        val value = rest.headOption.getOrElse(JSValue.Undefined)
        assertOn(value.toBoolean, rest.lift(1).getOrElse(JSValue.Undefined))
        JSValue.Undefined
      }
    )
    val assertFn = assertFnRef

    def method(name: String, arity: Int)(
        impl: (Array[JSValue], JSContext) => JSValue
    ): Unit =
      assertFn.funcObj.defineProperty(
        name,
        JSValue.Native(
          NativeFunction(
            name = name,
            length = arity,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              impl(strip(args), callCtx)
            }
          )
        ),
        enumerable = true,
        writable = true,
        configurable = true
      )

    def assertionConstructor(): NativeConstructor = {
      val errorProto = ctx.global.get("Error") match {
        case JSValue.Native(nc: NativeConstructor) => nc.prototype
        case _                                     => ctx.objectPrototype
      }
      NativeConstructor(
        name = "AssertionError",
        callImpl = (args, callCtx) => {
          given JSContext = callCtx
          makeAssertionError(args)
        },
        constructImpl = (args, callCtx) => {
          given JSContext = callCtx
          makeAssertionError(args)
        },
        prototype = errorProto
      )
    }

    def makeAssertionError(args: Array[JSValue])(using ctx: JSContext): JSValue = {
      val options = args.headOption.getOrElse(JSValue.Undefined)
      val message =
        options match {
          case JSValue.JSStr(s) => s
          case _ =>
            BuiltinHelpers.getPropertyWithGetter(options, "message") match {
              case JSValue.JSStr(s) => s
              case _                => "AssertionError"
            }
        }
      val error = ctx.createError("Error", message)
      error match {
        case JSValue.Object(obj) =>
          obj.defineProperty("name", JSValue.fromString("AssertionError"), enumerable = false, writable = true, configurable = true)
          obj.defineProperty("code", JSValue.fromString("ERR_ASSERTION"), enumerable = false, writable = true, configurable = true)
        case _ => ()
      }
      error
    }

    val assertionErrorCtor = assertionConstructor()
    BuiltinHelpers.initConstructor(assertionErrorCtor, length = 1)
    assertFn.funcObj.defineProperty(
      "AssertionError",
      JSValue.Native(assertionErrorCtor),
      enumerable = true,
      writable = true,
      configurable = true
    )

    method("ok", 2)((args, _) => {
      given JSContext = ctx
      val value = args.headOption.getOrElse(JSValue.Undefined)
      assertOn(value.toBoolean, args.lift(1).getOrElse(JSValue.Undefined))
      JSValue.Undefined
    })

    method("equal", 3)((args, _) => {
      given JSContext = ctx
      val actual = args.headOption.getOrElse(JSValue.Undefined)
      val expected = args.lift(1).getOrElse(JSValue.Undefined)
      if !(actual.toNumber == expected.toNumber || actual == expected) then
        fail(
          s"Expected values to be loosely equal: ${NodeUtil.inspect(actual)} != ${NodeUtil.inspect(expected)}",
          actual,
          expected,
          "=="
        )
      JSValue.Undefined
    })

    method("notEqual", 3)((args, _) => {
      given JSContext = ctx
      val actual = args.headOption.getOrElse(JSValue.Undefined)
      val expected = args.lift(1).getOrElse(JSValue.Undefined)
      if actual == expected then
        fail(
          s"Expected values to be strictly not equal: ${NodeUtil.inspect(actual)} != ${NodeUtil.inspect(expected)}",
          actual,
          expected,
          "!="
        )
      JSValue.Undefined
    })

    method("strictEqual", 3)((args, _) => {
      given JSContext = ctx
      val actual = args.headOption.getOrElse(JSValue.Undefined)
      val expected = args.lift(1).getOrElse(JSValue.Undefined)
      if !sameValueStrict(actual, expected) then
        fail(
          s"Expected values to be strictly equal: ${NodeUtil.inspect(actual)} !== ${NodeUtil.inspect(expected)}",
          actual,
          expected,
          "strictEqual"
        )
      JSValue.Undefined
    })

    method("notStrictEqual", 3)((args, _) => {
      given JSContext = ctx
      val actual = args.headOption.getOrElse(JSValue.Undefined)
      val expected = args.lift(1).getOrElse(JSValue.Undefined)
      if sameValueStrict(actual, expected) then
        fail(
          s"Expected values to be strictly not equal: ${NodeUtil.inspect(actual)} !== ${NodeUtil.inspect(expected)}",
          actual,
          expected,
          "notStrictEqual"
        )
      JSValue.Undefined
    })

    method("deepEqual", 3)((args, _) => {
      given JSContext = ctx
      val actual = args.headOption.getOrElse(JSValue.Undefined)
      val expected = args.lift(1).getOrElse(JSValue.Undefined)
      if !deepEqualValues(actual, expected, strict = false) then
        fail(
          s"Expected values to be loosely deep-equal: ${NodeUtil.inspect(actual)} != ${NodeUtil.inspect(expected)}",
          actual,
          expected,
          "deepEqual"
        )
      JSValue.Undefined
    })

    method("notDeepEqual", 3)((args, _) => {
      given JSContext = ctx
      val actual = args.headOption.getOrElse(JSValue.Undefined)
      val expected = args.lift(1).getOrElse(JSValue.Undefined)
      if deepEqualValues(actual, expected, strict = false) then
        fail("Expected values not to be loosely deep-equal", actual, expected, "notDeepEqual")
      JSValue.Undefined
    })

    method("deepStrictEqual", 3)((args, _) => {
      given JSContext = ctx
      val actual = args.headOption.getOrElse(JSValue.Undefined)
      val expected = args.lift(1).getOrElse(JSValue.Undefined)
      if !deepEqualValues(actual, expected, strict = true) then
        fail(
          s"Expected values to be strictly deep-equal: ${NodeUtil.inspect(actual)} !== ${NodeUtil.inspect(expected)}",
          actual,
          expected,
          "deepStrictEqual"
        )
      JSValue.Undefined
    })

    method("notDeepStrictEqual", 3)((args, _) => {
      given JSContext = ctx
      val actual = args.headOption.getOrElse(JSValue.Undefined)
      val expected = args.lift(1).getOrElse(JSValue.Undefined)
      if deepEqualValues(actual, expected, strict = true) then
        fail("Expected values not to be strictly deep-equal", actual, expected, "notDeepStrictEqual")
      JSValue.Undefined
    })

    def matchesExpected(
        error: JSValue,
        expected: JSValue
    )(using ctx: JSContext): Boolean =
      expected match {
        case JSValue.Undefined            => true
        case JSValue.Object(obj)
            if obj.getOwnPropertyRaw("__regexpPattern").isDefined =>
          val source = obj.getOwnPropertyRaw("__regexpPattern").get
          val flags = obj.getOwnPropertyRaw("__regexpFlags").map(NodeHelpers.toStr(_)).getOrElse("")
          val message = errorMessage(error)
          try
            val pattern = java.util.regex.Pattern.compile(NodeHelpers.toStr(source), {
              var f = 0
              if flags.contains("i") then f |= java.util.regex.Pattern.CASE_INSENSITIVE
              if flags.contains("m") then f |= java.util.regex.Pattern.MULTILINE
              if flags.contains("s") then f |= java.util.regex.Pattern.DOTALL
              f
            })
            pattern.matcher(message).find()
          catch case _: Throwable => false
        case JSValue.JSStr(expectedName) =>
          errorName(error) == expectedName
        case fn if BuiltinHelpers.isCallable(fn) =>
          val expectedName = {
            funcName(fn)
          }
          val proto = fn match {
            case JSValue.Native(nc: NativeConstructor) => Some(nc.prototype)
            case _ =>
              funcObjOfLocal(fn).flatMap(_.getOwnPropertyRaw("prototype")).collect {
                case JSValue.Object(p) => p
              }
          }
          error match {
            case JSValue.Object(obj) =>
              proto.exists(p => obj.hasPrototype(p)) || errorName(error) == expectedName
            case _ => false
          }
        case JSValue.Object(_) =>
          val expectedName =
            BuiltinHelpers.getPropertyWithGetter(expected, "name") match {
              case JSValue.JSStr(s) => s
              case _                => ""
            }
          expectedName.isEmpty || errorName(error) == expectedName
        case _ => false
      }

    def funcName(value: JSValue): String = value match {
      case f: JSValue.Function                   => f.name
      case JSValue.Native(nf: NativeFunction)    => nf.name
      case JSValue.Native(nc: NativeConstructor) => nc.name
      case _                                     => ""
    }

    def funcObjOfLocal(value: JSValue): Option[JSObject] = value match {
      case f: JSValue.Function                   => Some(f.funcObj)
      case JSValue.Native(nf: NativeFunction)    => Some(nf.funcObj)
      case JSValue.Native(nc: NativeConstructor) => Some(nc.funcObj)
      case _                                     => None
    }

    def callAndCatch(fn: JSValue)(using ctx: JSContext): Either[JSValue, Unit] =
      try {
        BuiltinHelpers.callFunctionWithThis(fn, JSValue.Undefined, Array.empty)
        Right(())
      } catch {
        case e: quickjs.runtime.JSException => Left(e.getValue)
        case e: Throwable =>
          Left(ctx.createError("Error", String.valueOf(e.getMessage)))
      }

    method("throws", 3)((args, _) => {
      given JSContext = ctx
      val fn = args.headOption.getOrElse(JSValue.Undefined)
      val expected = args.lift(1).getOrElse(JSValue.Undefined)
      callAndCatch(fn) match {
        case Left(error) =>
          if !matchesExpected(error, expected) then
            fail(s"Missing expected exception: ${NodeUtil.inspect(error)}", error, expected, "throws")
        case Right(()) =>
          fail("Missing expected exception (no error thrown)", operator = "throws")
      }
      JSValue.Undefined
    })

    method("doesNotThrow", 3)((args, _) => {
      given JSContext = ctx
      val fn = args.headOption.getOrElse(JSValue.Undefined)
      callAndCatch(fn) match {
        case Left(error) =>
          throw new quickjs.runtime.JSException(error)
        case Right(()) => ()
      }
      JSValue.Undefined
    })

    method("rejects", 3)((args, callCtx) => {
      given JSContext = callCtx
      val promise = args.headOption.getOrElse(JSValue.Undefined)
      val expected = args.lift(1).getOrElse(JSValue.Undefined)
      val onRejected = NativeFunction(
        name = "onRejected",
        length = 1,
        impl = (rejectedArgs, rCtx) => {
          given JSContext = rCtx
          val error = rejectedArgs.lastOption.getOrElse(JSValue.Undefined)
          if !matchesExpected(error, expected) then
            fail(s"Missing expected rejection: ${NodeUtil.inspect(error)}", error, expected, "rejects")
          JSValue.Undefined
        }
      )
      val onFulfilled = NativeFunction(
        name = "onFulfilled",
        length = 1,
        impl = (_, fCtx) => {
          given JSContext = fCtx
          fail("Missing expected rejection", operator = "rejects")
        }
      )
      val thenFn = BuiltinHelpers.getPropertyWithGetter(promise, "then")
      BuiltinHelpers.callFunctionWithThis(
        thenFn,
        promise,
        Array(JSValue.Native(onFulfilled), JSValue.Native(onRejected))
      )
    })

    method("doesNotReject", 3)((args, callCtx) => {
      given JSContext = callCtx
      val promise = args.headOption.getOrElse(JSValue.Undefined)
      val onRejected = NativeFunction(
        name = "onRejected",
        length = 1,
        impl = (rejectedArgs, rCtx) => {
          given JSContext = rCtx
          throw new quickjs.runtime.JSException(
            rejectedArgs.lastOption.getOrElse(JSValue.Undefined)
          )
        }
      )
      val onFulfilled = NativeFunction(
        name = "onFulfilled",
        length = 1,
        impl = (_, _) => JSValue.Undefined
      )
      val thenFn = BuiltinHelpers.getPropertyWithGetter(promise, "then")
      BuiltinHelpers.callFunctionWithThis(
        thenFn,
        promise,
        Array(JSValue.Native(onFulfilled), JSValue.Native(onRejected))
      )
    })

    method("fail", 1)((args, _) => {
      given JSContext = ctx
      val message = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("Failed")
      fail(message, operator = "fail")
    })

    method("ifError", 1)((args, _) => {
      given JSContext = ctx
      args.headOption match {
        case Some(JSValue.Undefined) | Some(JSValue.Null) | None => ()
        case Some(error) => throw new quickjs.runtime.JSException(error)
      }
      JSValue.Undefined
    })

    method("match", 3)((args, _) => {
      given JSContext = ctx
      val text = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val regexp = args.lift(1).getOrElse(JSValue.Undefined)
      val matched =
        regexp match {
          case JSValue.Object(obj)
              if obj.getOwnPropertyRaw("__regexpPattern").isDefined =>
            val source = NodeHelpers.toStr(obj.getOwnPropertyRaw("__regexpPattern").get)
            java.util.regex.Pattern.compile(source).matcher(text).find()
          case _ => false
        }
      if !matched then fail(s"The input did not match the regular expression", operator = "match")
      JSValue.Undefined
    })

    // `assert.strict` is the same function.
    assertFn.funcObj.defineProperty(
      "strict",
      JSValue.Native(assertFn),
      enumerable = true,
      writable = true,
      configurable = true
    )

    JSValue.Native(assertFn)
  }
}
