package quickjs.node

import quickjs.value.{JSValue, NativeConstructor, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.objmodel.JSObject

/** Node's `tty` module: `isatty` plus minimal `ReadStream`/`WriteStream`. */
object NodeTty {

  def create()(using ctx: JSContext): JSValue = {
    val tty = JSObject(prototype = ctx.objectPrototype)

    tty.set(
      "isatty",
      JSValue.Native(
        NativeFunction(
          name = "isatty",
          length = 1,
          impl = (args, _) => {
            val fd =
              args.headOption match {
                case Some(JSValue.Int32(n))   => n
                case Some(JSValue.Float64(d)) => d.toInt
                case _                        => -1
              }
            JSValue.Bool((fd == 0 || fd == 1 || fd == 2) && System.console() != null)
          }
        )
      )
    )

    val readStreamProto = JSObject(prototype = ctx.objectPrototype)
    val writeStreamProto = JSObject(prototype = ctx.objectPrototype)
    readStreamProto.set("isTTY", JSValue.Bool(true))
    writeStreamProto.set("isTTY", JSValue.Bool(true))

    def emitterObject(proto: JSObject, fd: Int): JSValue = {
      val obj = JSObject(prototype = proto)
      obj.set("fd", JSValue.fromInt(fd))
      obj.set("isTTY", JSValue.Bool(true))
      obj.set("readable", JSValue.Bool(true))
      obj.set("writable", JSValue.Bool(true))
      obj.set("columns", JSValue.fromInt(80))
      obj.set("rows", JSValue.fromInt(24))
      JSValue.Object(obj)
    }

    val readCtor = NativeConstructor(
      name = "ReadStream",
      callImpl = (args, callCtx) => {
        given JSContext = callCtx
        val fd = args.headOption.map(v => NodeHelpers.toNumber(v).toInt).getOrElse(0)
        emitterObject(readStreamProto, fd)
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val fd = args.headOption.map(v => NodeHelpers.toNumber(v).toInt).getOrElse(0)
        emitterObject(readStreamProto, fd)
      },
      prototype = readStreamProto
    )
    val writeCtor = NativeConstructor(
      name = "WriteStream",
      callImpl = (args, callCtx) => {
        given JSContext = callCtx
        val fd = args.headOption.map(v => NodeHelpers.toNumber(v).toInt).getOrElse(1)
        emitterObject(writeStreamProto, fd)
      },
      constructImpl = (args, callCtx) => {
        given JSContext = callCtx
        val fd = args.headOption.map(v => NodeHelpers.toNumber(v).toInt).getOrElse(1)
        emitterObject(writeStreamProto, fd)
      },
      prototype = writeStreamProto
    )
    BuiltinHelpersInit(readCtor, 1)
    BuiltinHelpersInit(writeCtor, 1)

    tty.set("ReadStream", JSValue.Native(readCtor))
    tty.set("WriteStream", JSValue.Native(writeCtor))

    JSValue.Object(tty)
  }

  private def BuiltinHelpersInit(
      ctor: NativeConstructor,
      length: Int
  )(using ctx: JSContext): Unit =
    quickjs.runtime.builtins.BuiltinHelpers.initConstructor(ctor, length)
}
