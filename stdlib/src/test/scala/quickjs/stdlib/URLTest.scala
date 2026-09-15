package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.FunSuite
import java.nio.charset.StandardCharsets

/** Tests for the WHATWG `URL` / `URLSearchParams` host globals.
  *
  * `url-conformance.js` is a generated fixture: it contains a corpus of URL
  * parse results (href, origin, protocol, username, password, host, hostname,
  * port, pathname, search, hash or "must fail") produced from Node 20's
  * WHATWG URL implementation, and evaluates the same inputs in this engine.
  */
class URLTest extends FunSuite:

  private def eval(source: String)(using ctx: JSContext): JSValue =
    val tokens = Lexer(source).tokenize()
    val ast = Parser(tokens).parseScript()
    val bytecode = Compiler().compileScript(ast)
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty)

  private def withContext(body: JSContext ?=> Unit): Unit = {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)
    JSON.initialize()
    Globals.initialize()
    body
  }

  private def stringResult(source: String)(using ctx: JSContext): String =
    eval(source) match {
      case JSValue.JSStr(s) => s
      case other            => fail(s"expected a string, got $other")
    }

  private def runFixture(resource: String)(using ctx: JSContext): String = {
    val stream = getClass.getResourceAsStream(resource)
    assert(stream != null, s"$resource fixture is missing")
    val source = new String(stream.readAllBytes(), StandardCharsets.UTF_8)
    stringResult(source)
  }

  test("WHATWG URL parse conformance corpus") {
    withContext {
      assertEquals(runFixture("/url-conformance.js"), "OK")
    }
  }

  test("WHATWG URL setter conformance corpus") {
    withContext {
      assertEquals(runFixture("/url-setter-conformance.js"), "OK")
    }
  }

  test("basic URL properties") {
    withContext {
      val result = stringResult("""
        |var u = new URL("http://user:pass@example.com:8080/a/b?x=1#frag");
        |[
        |  u.href, u.origin, u.protocol, u.username, u.password,
        |  u.host, u.hostname, u.port, u.pathname, u.search, u.hash,
        |  String(u), u.toJSON()
        |].join("|");
        |""".stripMargin)
      assertEquals(
        result,
        "http://user:pass@example.com:8080/a/b?x=1#frag|http://example.com:8080|http:|" +
          "user|pass|example.com:8080|example.com|8080|/a/b|?x=1|#frag|" +
          "http://user:pass@example.com:8080/a/b?x=1#frag|" +
          "http://user:pass@example.com:8080/a/b?x=1#frag"
      )
    }
  }

  test("relative resolution and default ports") {
    withContext {
      val result = stringResult("""
        |[
        |  new URL("../x", "http://a/b/c/d;p?q").href,
        |  new URL("//other/p", "http://a/b").href,
        |  new URL("?q", "http://a/b#f").href,
        |  new URL("#f", "http://a/b?q").href,
        |  new URL("http://a:80/").href,
        |  new URL("https://a:443/").href,
        |  new URL("mailto:a@b.c").href,
        |  new URL("mailto:a@b.c").protocol,
        |  new URL("mailto:a@b.c").pathname
        |].join("|");
        |""".stripMargin)
      assertEquals(
        result,
        "http://a/b/x|http://other/p|http://a/b?q|http://a/b?q#f|http://a/|https://a/|" +
          "mailto:a@b.c|mailto:|a@b.c"
      )
    }
  }

  test("origin is null for opaque schemes and follows blob URLs") {
    withContext {
      val result = stringResult("""
        |[
        |  new URL("file:///tmp/x").origin,
        |  new URL("mailto:a@b.c").origin,
        |  new URL("blob:https://example.com/uuid").origin,
        |  new URL("data:text/plain,x").origin
        |].join("|");
        |""".stripMargin)
      assertEquals(result, "null|null|https://example.com|null")
    }
  }

  test("URL canParse and parse") {
    withContext {
      val result = stringResult("""
        |var threw = 0;
        |try { URL.canParse(); } catch (e) { threw += e instanceof TypeError ? 1 : 0; }
        |try { URL.parse(); } catch (e) { threw += e instanceof TypeError ? 1 : 0; }
        |[
        |  URL.canParse("http://x"), URL.canParse("not a url"),
        |  URL.canParse("/p", "http://base/"), URL.canParse("/p"),
        |  URL.canParse(undefined), String(URL.parse(undefined)),
        |  String(URL.parse("http://x/")), String(URL.parse("not a url")),
        |  String(URL.parse("/p", "http://base/")), typeof URL.parse("not a url"),
        |  threw
        |].join("|");
        |""".stripMargin)
      assertEquals(
        result,
        "true|false|true|false|false|null|http://x/|null|http://base/p|object|2"
      )
    }
  }

  test("invalid URLs throw TypeError") {
    withContext {
      val result = stringResult("""
        |var out = [];
        |for (var i = 0; i < 5; i++) {
        |  try {
        |    if (i === 0) new URL();
        |    if (i === 1) new URL("not a url");
        |    if (i === 2) new URL("http://a.1/");
        |    if (i === 3) new URL("http://exa mple.com/");
        |    if (i === 4) new URL("x", "not a base");
        |    out.push("no");
        |  } catch (e) {
        |    out.push(e instanceof TypeError ? "TypeError" : e.name);
        |  }
        |}
        |out.join(",");
        |""".stripMargin)
      assertEquals(result, "TypeError,TypeError,TypeError,TypeError,TypeError")
    }
  }

  test("URL setters") {
    withContext {
      val result = stringResult("""
        |var u = new URL("http://a/b/c?x#y");
        |var out = [];
        |u.pathname = "p";                       out.push(u.href);
        |u.pathname = "/q/";                     out.push(u.href);
        |u.host = "h:81";                        out.push(u.href);
        |u.hostname = "z";                       out.push(u.href);
        |u.port = "82";                          out.push(u.href);
        |u.protocol = "https:";                  out.push(u.href);
        |u.username = "us er";                   out.push(u.username);
        |u.password = "p@ss";                    out.push(u.password);
        |u.search = "q=1";                       out.push(u.href);
        |u.search = "?q=2";                      out.push(u.href);
        |u.search = "";                          out.push(u.href);
        |u.search = "?a b";                      out.push(u.href);
        |u.hash = "f f";                         out.push(u.href);
        |u.hash = "#g";                          out.push(u.href);
        |u.hash = "";                            out.push(u.href);
        |out.join("\n");
        |""".stripMargin)
      assertEquals(
        result,
        """http://a/p?x#y
          |http://a/q/?x#y
          |http://h:81/q/?x#y
          |http://z:81/q/?x#y
          |http://z:82/q/?x#y
          |https://z:82/q/?x#y
          |us%20er
          |p%40ss
          |https://us%20er:p%40ss@z:82/q/?q=1#y
          |https://us%20er:p%40ss@z:82/q/?q=2#y
          |https://us%20er:p%40ss@z:82/q/#y
          |https://us%20er:p%40ss@z:82/q/?a%20b#y
          |https://us%20er:p%40ss@z:82/q/?a%20b#f%20f
          |https://us%20er:p%40ss@z:82/q/?a%20b#g
          |https://us%20er:p%40ss@z:82/q/?a%20b""".stripMargin
      )
    }
  }

  test("opaque path URLs ignore pathname/host setters") {
    withContext {
      val result = stringResult("""
        |var m = new URL("mailto:a@b.c");
        |m.pathname = "z";
        |m.host = "example.com";
        |m.username = "u";
        |m.search = "subject=hi";
        |m.hash = "body";
        |m.href;
        |""".stripMargin)
      assertEquals(result, "mailto:a@b.c?subject=hi#body")
    }
  }

  test("searchParams is cached and live with the URL") {
    withContext {
      val result = stringResult("""
        |var u = new URL("http://x/?a=1");
        |var sp = u.searchParams;
        |var same = sp === u.searchParams;
        |u.search = "?b=2";
        |var afterSearch = sp.get("a") + "," + sp.get("b");
        |sp.append("c", "3");
        |var hrefAfterAppend = u.href;
        |u.href = "http://y/?d=4";
        |var afterHref = sp.get("d") + "," + sp.get("b");
        |[same, afterSearch, hrefAfterAppend, afterHref, sp === u.searchParams].join("|");
        |""".stripMargin)
      assertEquals(
        result,
        "true|null,2|http://x/?b=2&c=3|4,null|true"
      )
    }
  }

  test("URLSearchParams parsing and serialization") {
    withContext {
      val result = stringResult("""
        |var out = [];
        |var s = new URLSearchParams("?a=1&b=hello+world&c=%C3%A9");
        |out.push(s.size, s.get("b"), s.get("c"), s.has("a", "1"), s.has("a", "2"));
        |s.delete("a", "1"); out.push(s.toString());
        |out.push(new URLSearchParams().toString());
        |out.push(new URLSearchParams("a").toString());
        |out.push(new URLSearchParams("a=").toString());
        |out.push(new URLSearchParams("a&&b&=x&c").toString());
        |var sorted = new URLSearchParams("b=2&a=3&B=1&a=1"); sorted.sort(); out.push(sorted.toString());
        |out.push(new URLSearchParams([["x", 1], ["y", 2]]).toString());
        |out.push(new URLSearchParams({p: 1, q: "x y"}).toString());
        |out.push(new URLSearchParams(new URLSearchParams("z=9")).toString());
        |out.push(new URLSearchParams(new Map([["m", "1"]])).toString());
        |out.join("|");
        |""".stripMargin)
      assertEquals(
        result,
        "3|hello world|é|true|false|b=hello+world&c=%C3%A9||a=|a=|" +
          "a=&b=&=x&c=|B=1&a=3&a=1&b=2|x=1&y=2|p=1&q=x+y|z=9|m=1"
      )
    }
  }

  test("URLSearchParams form encoding round-trip") {
    withContext {
      val result = stringResult("""
        |var s = new URLSearchParams();
        |var chars = ["abc", "*", "-", ".", "_", "~", "!", "'", "(", ")", " ", "+", "%", "&", "=", "/", "?"];
        |for (var i = 0; i < chars.length; i++) { s.set("k" + i, chars[i]); }
        |var round = new URLSearchParams(s.toString());
        |chars.every(function (c, i) { return round.get("k" + i) === c; }) + "|" + s.toString();
        |""".stripMargin)
      assertEquals(
        result,
        "true|k0=abc&k1=*&k2=-&k3=.&k4=_&k5=%7E&k6=%21&k7=%27&k8=%28&k9=%29&" +
          "k10=+&k11=%2B&k12=%25&k13=%26&k14=%3D&k15=%2F&k16=%3F"
      )
    }
  }

  test("URLSearchParams iterators and forEach") {
    withContext {
      val result = stringResult("""
        |var s = new URLSearchParams("a=1&b=2&a=3");
        |var out = [];
        |out.push(Array.from(s.keys()).join(","));
        |out.push(Array.from(s.values()).join(","));
        |out.push(Array.from(s.entries()).map(function (p) { return p.join(":"); }).join(","));
        |out.push(Array.from(s).map(function (p) { return p.join("="); }).join(","));
        |var seen = [];
        |s.forEach(function (value, name, self) {
        |  seen.push(name + "=" + value + (self === s ? "" : "!"));
        |});
        |out.push(seen.join(","));
        |out.push(Object.prototype.toString.call(s.entries()));
        |out.push(Object.prototype.toString.call(s));
        |out.push(Object.prototype.toString.call(new URL("http://x")));
        |out.join("|");
        |""".stripMargin)
      assertEquals(
        result,
        "a,b,a|1,2,3|a:1,b:2,a:3|a=1,b=2,a=3|a=1,b=2,a=3|" +
          "[object URLSearchParams Iterator]|[object URLSearchParams]|[object URL]"
      )
    }
  }

  test("URLSearchParams missing arguments throw TypeError") {
    withContext {
      val result = stringResult("""
        |var s = new URLSearchParams("a=1");
        |var out = [];
        |function check(f) { try { f(); out.push("no"); } catch (e) { out.push(e instanceof TypeError ? "TypeError" : e.name); } }
        |check(function () { s.get(); });
        |check(function () { s.has(); });
        |check(function () { s.append("x"); });
        |check(function () { s.delete(); });
        |check(function () { s.set("x"); });
        |check(function () { URLSearchParams.prototype.toString.call({}); });
        |check(function () { new URL("http://x").href; ({}).__proto__; URLSearchParams.prototype.forEach.call(5, function () {}); });
        |out.join(",");
        |""".stripMargin)
      assertEquals(result, "TypeError,TypeError,TypeError,TypeError,TypeError,TypeError,TypeError")
    }
  }

  test("URL subclassing initializes the receiver") {
    withContext {
      val result = stringResult("""
        |class MyURL extends URL {}
        |class MyParams extends URLSearchParams {}
        |var u = new MyURL("http://a/b?x=1");
        |var p = new MyParams("a=1");
        |(u instanceof MyURL) + "|" + (u instanceof URL) + "|" + u.href + "|" +
        |  (p instanceof URLSearchParams) + "|" + p.get("a") + "|" + p.size;
        |""".stripMargin)
      assertEquals(
        result,
        "true|true|http://a/b?x=1|true|1|1"
      )
    }
  }

  test("URLSearchParams object lookup uses own enumerable keys") {
    withContext {
      val result = stringResult("""
        |var proto = { inherited: 1 };
        |var record = Object.create(proto);
        |record.b = 2;
        |record.a = 1;
        |new URLSearchParams(record).toString();
        |""".stripMargin)
      assertEquals(result, "b=2&a=1")
    }
  }
