package quickjs.node

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.objmodel.{JSArray, JSObject}

import java.nio.charset.StandardCharsets

/** Node's legacy `querystring` module. */
object NodeQuerystring {

  def create()(using ctx: JSContext): JSValue = {
    val querystring = JSObject(prototype = ctx.objectPrototype)

    def strip(args: Array[JSValue]): Array[JSValue] =
      NodeHelpers.stripReceiver(args, querystring)

    def method(name: String, arity: Int)(
        impl: Array[JSValue] => JSValue
    ): Unit =
      querystring.set(
        name,
        JSValue.Native(
          NativeFunction(
            name = name,
            length = arity,
            impl = (args, callCtx) => {
              given JSContext = callCtx
              impl(strip(args))
            }
          )
        )
      )

    method("escape", 1)(args =>
      JSValue.fromString(encode(NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))))
    )
    method("unescape", 1)(args =>
      JSValue.fromString(decode(NodeHelpers.toStr(args.headOption.getOrElse(JSValue.Undefined))))
    )

    method("stringify", 4)(args => {
      given JSContext = ctx
      val value = args.headOption.getOrElse(JSValue.Undefined)
      val separator =
        args.lift(1).filter(_ != JSValue.Undefined).map(NodeHelpers.toStr(_)).getOrElse("&")
      val equals =
        args.lift(2).filter(_ != JSValue.Undefined).map(NodeHelpers.toStr(_)).getOrElse("=")
      val pairs = scala.collection.mutable.ArrayBuffer.empty[String]
      def append(key: String, value: JSValue): Unit = {
        val text =
          value match {
            case JSValue.JSStr(s) => s
            case JSValue.Int32(n) => n.toString
            case JSValue.Float64(d) => NodeHelpers.toStr(JSValue.fromDouble(d))
            case JSValue.Bool(b)  => b.toString
            case JSValue.Null     => ""
            case JSValue.Undefined | _: JSValue.Function | _: JSValue.Native => ""
            case other => NodeHelpers.toStr(other)
          }
        pairs += s"${encode(key)}$equals${encode(text)}"
      }
      value match {
        case JSValue.Object(obj) =>
          obj.getAllOwnPropertyKeys().foreach { key =>
            obj.getOwnProperty(key).foreach { v =>
              append(key, v)
            }
          }
        case JSValue.JSArrayVal(arr) =>
          var i = 0
          while i < arr.getLength do {
            append(i.toString, arr.get(i))
            i += 1
          }
        case _ => ()
      }
      JSValue.fromString(pairs.mkString(separator))
    })

    method("parse", 4)(args => {
      given JSContext = ctx
      val text = args.headOption.map(NodeHelpers.toStr(_)).getOrElse("")
      val separator =
        args.lift(1).filter(_ != JSValue.Undefined).map(NodeHelpers.toStr(_)).getOrElse("&")
      val equals =
        args.lift(2).filter(_ != JSValue.Undefined).map(NodeHelpers.toStr(_)).getOrElse("=")
      val result = JSObject(prototype = null)
      if text.nonEmpty then {
        // A key appearing multiple times becomes an array.
        val seen = scala.collection.mutable.HashSet.empty[String]
        text.split(java.util.regex.Pattern.quote(separator), -1).foreach { pair =>
          val index = pair.indexOf(equals)
          val (rawKey, rawValue) =
            if index < 0 then (pair, "") else (pair.substring(0, index), pair.substring(index + equals.length))
          val key = decode(rawKey)
          val decodedValue = JSValue.fromString(decode(rawValue))
          result.getOwnProperty(key) match {
            case Some(JSValue.JSArrayVal(existing)) =>
              existing.push(decodedValue)
            case Some(existing) =>
              val arr = JSArray.empty()
              arr.push(existing)
              arr.push(decodedValue)
              result.set(key, JSValue.JSArrayVal(arr))
            case None =>
              result.set(key, decodedValue)
          }
        }
      }
      JSValue.Object(result)
    })

    JSValue.Object(querystring)
  }

  private def encode(text: String): String = {
    val sb = new StringBuilder
    text.getBytes(StandardCharsets.UTF_8).foreach { byte =>
      val c = (byte & 0xff).toChar
      val keep =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
          (c >= '0' && c <= '9') || c == '-' || c == '.' || c == '_' || c == '~'
      if keep then sb.append(c) else sb.append(f"%%${byte & 0xff}%02X")
    }
    sb.toString
  }

  private def decode(text: String): String = {
    if !text.contains('%') && !text.contains('+') then text
    else {
      val bytes = new java.io.ByteArrayOutputStream()
      var i = 0
      while i < text.length do {
        val c = text.charAt(i)
        if c == '%' && i + 2 < text.length then {
          try {
            bytes.write(Integer.parseInt(text.substring(i + 1, i + 3), 16))
            i += 3
          } catch {
            case _: NumberFormatException =>
              bytes.write(c.toInt)
              i += 1
          }
        } else if c == '+' then {
          bytes.write(' ')
          i += 1
        } else {
          bytes.write(c.toString.getBytes(StandardCharsets.UTF_8))
          i += 1
        }
      }
      new String(bytes.toByteArray, StandardCharsets.UTF_8)
    }
  }
}
