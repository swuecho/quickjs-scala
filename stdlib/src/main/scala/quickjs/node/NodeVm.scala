package quickjs.node

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import quickjs.objmodel.JSObject

/** Node's `vm` module (pragmatic implementation).
  *
  * Scripts run in the current realm's global scope; contextified objects are
  * accepted but share the global object (creating isolated realms is left to
  * `$262.createRealm` in the test host).
  */
object NodeVm {

  private def optionOf(options: JSValue, key: String)(using
      ctx: JSContext
  ): JSValue =
    options match {
      case JSValue.Object(_) => BuiltinHelpers.getPropertyWithGetter(options, key)
      case _                 => JSValue.Undefined
    }

  def create()(using ctx: JSContext): JSValue = {
    val vm = JSObject(prototype = ctx.objectPrototype)

    def strip(args: Array[JSValue]): Array[JSValue] =
      args.headOption match {
        case Some(JSValue.Object(obj)) if obj eq vm => args.drop(1)
        case _                                       => args
      }

    def runCode(
        code: String,
        filename: String
    ): JSValue = {
      val tokens = Lexer(code).tokenize()
      val ast = Parser(tokens, code).parseScript()
      val compiler = Compiler()
      val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
      val interpreter = Interpreter()
      val previous = ctx.sourceName
      if filename.nonEmpty then ctx.setSourceName(filename)
      try interpreter.call(bytecode, JSValue.Undefined, Array.empty)
      finally ctx.setSourceName(previous)
    }

    def filenameOf(options: JSValue): String =
      optionOf(options, "filename") match {
        case JSValue.JSStr(s) => s
        case _                => ""
      }

    // ---- Script ----------------------------------------------------------

    val scriptProto = JSObject(prototype = ctx.objectPrototype)
    val scriptCtor = NativeConstructor(
      name = "Script",
      callImpl = (_, callCtx) => {
        given JSContext = callCtx
        callCtx.throwTypeError(
          "Class constructor Script cannot be invoked without 'new'"
        )
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val rest = args.headOption match {
          case Some(JSValue.Object(obj)) if obj eq scriptProto => args.drop(1)
          case _                                               => args
        }
        val code = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
        val options = rest.lift(1).getOrElse(JSValue.Undefined)
        val script = JSObject(prototype = scriptProto)
        val filename = filenameOf(options)
        script.set("filename", JSValue.fromString(filename))
        script.set("cachedData", JSValue.Null)
        def runWithArgs(runArgs: Array[JSValue]): JSValue = {
          val opts = runArgs.headOption.getOrElse(JSValue.Undefined)
          val effective =
            if filename.nonEmpty then filename else filenameOf(opts)
          runCode(code, effective)
        }
        script.set(
          "runInThisContext",
          JSValue.Native(
            NativeFunction(
              name = "runInThisContext",
              length = 1,
              impl = (runArgs, runCtx) => {
                given JSContext = runCtx
                val rest =
                  runArgs.headOption match {
                    case Some(JSValue.Object(obj)) if obj eq script =>
                      runArgs.drop(1)
                    case _ => runArgs
                  }
                runWithArgs(rest)
              }
            )
          )
        )
        script.set(
          "runInContext",
          JSValue.Native(
            NativeFunction(
              name = "runInContext",
              length = 2,
              impl = (runArgs, runCtx) => {
                given JSContext = runCtx
                // Contexts share the global object in this implementation.
                runWithArgs(runArgs.drop(1))
              }
            )
          )
        )
        script.set(
          "runInNewContext",
          JSValue.Native(
            NativeFunction(
              name = "runInNewContext",
              length = 2,
              impl = (runArgs, runCtx) => {
                given JSContext = runCtx
                runWithArgs(runArgs.drop(1))
              }
            )
          )
        )
        script.set(
          "createCachedData",
          JSValue.Native(
            NativeFunction(
              name = "createCachedData",
              length = 0,
              impl = (_, _) => NodeBuffer.makeBuffer(Array.emptyByteArray)
            )
          )
        )
        JSValue.Object(script)
      },
      prototype = scriptProto
    )
    BuiltinHelpers.initConstructor(scriptCtor, length = 2)
    scriptProto.defineProperty(
      "constructor",
      JSValue.Native(scriptCtor),
      enumerable = false
    )(using ctx)
    vm.set("Script", JSValue.Native(scriptCtor))

    // ---- Top-level helpers ------------------------------------------------

    vm.set(
      "runInThisContext",
      JSValue.Native(
        NativeFunction(
          name = "runInThisContext",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            val code = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            runCode(code, filenameOf(rest.lift(1).getOrElse(JSValue.Undefined)))
          }
        )
      )
    )
    vm.set(
      "runInContext",
      JSValue.Native(
        NativeFunction(
          name = "runInContext",
          length = 3,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            val code = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            runCode(code, filenameOf(rest.lift(2).getOrElse(JSValue.Undefined)))
          }
        )
      )
    )
    vm.set(
      "runInNewContext",
      JSValue.Native(
        NativeFunction(
          name = "runInNewContext",
          length = 3,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            val code = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            runCode(code, filenameOf(rest.lift(2).getOrElse(JSValue.Undefined)))
          }
        )
      )
    )
    vm.set(
      "createContext",
      JSValue.Native(
        NativeFunction(
          name = "createContext",
          length = 2,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            rest.headOption match {
              case Some(value @ JSValue.Object(obj)) =>
                obj.set(
                  "__vmContext",
                  JSValue.Bool(true)
                )(using callCtx)
                value
              case Some(value) => value
              case None =>
                val obj = JSObject(prototype = callCtx.objectPrototype)
                obj.set("__vmContext", JSValue.Bool(true))(using callCtx)
                JSValue.Object(obj)
            }
          }
        )
      )
    )
    vm.set(
      "isContext",
      JSValue.Native(
        NativeFunction(
          name = "isContext",
          length = 1,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            JSValue.Bool(rest.headOption match {
              case Some(JSValue.Object(obj)) =>
                obj.getOwnPropertyRaw("__vmContext").isDefined
              case _ => false
            })
          }
        )
      )
    )
    vm.set(
      "compileFunction",
      JSValue.Native(
        NativeFunction(
          name = "compileFunction",
          length = 3,
          impl = (args, callCtx) => {
            given JSContext = callCtx
            val rest = strip(args)
            val code = rest.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
            val params = rest.lift(1) match {
              case Some(JSValue.JSArrayVal(arr)) =>
                (0 until arr.getLength).toArray.map(i =>
                  JSValue.fromString(NodeHelpers.toStr(arr.get(i)))
                )
              case _ => Array.empty[JSValue]
            }
            val functionCtor = callCtx.global.get("Function")
            BuiltinHelpers.callFunctionWithThis(
              functionCtor,
              JSValue.Undefined,
              params :+ JSValue.fromString(code)
            )
          }
        )
      )
    )
    val constants = JSObject(prototype = null)
    constants.set("DONT_CONTEXTIFY", JSValue.fromString("DONT_CONTEXTIFY"))
    vm.set("constants", JSValue.Object(constants))

    JSValue.Object(vm)
  }
}
