package quickjs.node

import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.{JSValue, NativeFunction}
import quickjs.stdlib.{Console, Globals, JSON}
import munit.FunSuite

import java.nio.file.{Files, Path}

/** End-to-end tests for the Node compatibility layer: `require`, Node module
  * resolution, `process`, `Buffer`, `path`, `fs`, `util`, `assert`, `events`
  * and `url`.
  */
class NodeCompatTest extends FunSuite {

  private def withProject(
      files: Map[String, String],
      entry: String = "main.js",
      argv: Vector[String] = Vector("node", "main.js")
  )(body: JSContext ?=> (NodeRuntime, Path) => Unit): Unit = {
    val dir = Files.createTempDirectory("quickjs-node-test")
    try {
      files.foreach { case (relative, content) =>
        val path = dir.resolve(relative)
        Files.createDirectories(path.getParent)
        Files.writeString(path, content)
      }
      given rt: JSRuntime = JSRuntime()
      val ctx: JSContext = JSContext(rt)
      given JSContext = ctx
      StdLib.initialize(ctx)
      JSON.initialize()
      Console.initialize()
      Globals.initialize()
      val node = NodeRuntime.install(
        NodeOptions(argv = argv, cwd = dir)
      )
      rt.setModuleLoader(node.loader)
      body(node, dir)
      ctx.runMicrotasks()
    } finally {
      // Best-effort cleanup.
      try {
        val stream = Files.walk(dir)
        try
          stream
            .sorted(java.util.Comparator.reverseOrder())
            .forEach(p => Files.deleteIfExists(p))
        finally stream.close()
      } catch case _: Throwable => ()
    }
  }

  /** Capture `console.*` calls at the JS level. This avoids racing other
    * suites that redirect `System.out` while tests run in parallel.
    */
  private def installConsoleCapture(lines: scala.collection.mutable.ArrayBuffer[String])(using
      ctx: JSContext
  ): Unit =
    ctx.global.get("console") match {
      case JSValue.Object(console) =>
        Seq("log", "info", "debug", "error", "warn").foreach { name =>
          console.defineProperty(
            name,
            JSValue.Native(
              NativeFunction(
                name = name,
                length = 0,
                impl = (args, callCtx) => {
                  given JSContext = callCtx
                  val values =
                    if args.nonEmpty && (args(0) match {
                        case JSValue.Object(obj) => obj eq console
                        case _                   => false
                      })
                    then args.drop(1)
                    else args
                  lines += values
                    .map(value =>
                      value match {
                        case JSValue.JSStr(s) => s
                        case other            => quickjs.util.PrettyPrinter.consoleFormat(other)
                      }
                    )
                    .mkString(" ")
                  JSValue.Undefined
                }
              )
            ),
            enumerable = true,
            writable = true,
            configurable = true
          )
        }
      case _ => ()
    }

  private def runAndCapture(node: NodeRuntime, path: Path)(using JSContext): String = {
    val lines = scala.collection.mutable.ArrayBuffer.empty[String]
    installConsoleCapture(lines)
    node.loader.runMainFile(path)
    node.loop.run(summon[JSContext], propagateErrors = true)
    lines.mkString("\n")
  }

  test("require resolves relative files, JSON and node_modules packages") {
    withProject(
      Map(
        "main.js" ->
          """const local = require('./lib/util.js');
            |const data = require('./data.json');
            |const pkg = require('mypkg');
            |console.log(local.value, data.a, pkg.name);
            |""".stripMargin,
        "lib/util.js" -> "module.exports = { value: 'local' };",
        "data.json" -> """{ "a": 42 }""",
        "node_modules/mypkg/package.json" ->
          """{ "name": "mypkg", "main": "lib/index.js" }""",
        "node_modules/mypkg/lib/index.js" ->
          "module.exports = { name: 'mypkg' };"
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "local 42 mypkg")
    }
  }

  test("package exports and require cycles") {
    withProject(
      Map(
        "main.js" ->
          """const a = require('./a.js');
            |const sub = require('pkg/sub');
            |console.log(a.name, a.fromB, sub.kind);
            |""".stripMargin,
        "a.js" -> "exports.name = 'a'; const b = require('./b.js'); exports.fromB = b.name;",
        "b.js" -> "exports.name = 'b'; const a = require('./a.js'); exports.fromA = a.name;",
        "node_modules/pkg/package.json" ->
          """{ "name": "pkg", "exports": { ".": "./index.js", "./sub": "./sub/index.js" } }""",
        "node_modules/pkg/index.js" -> "module.exports = { kind: 'main' };",
        "node_modules/pkg/sub/index.js" -> "module.exports = { kind: 'sub' };"
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "a b sub")
    }
  }

  test("__dirname/__filename and top-level this in CommonJS") {
    withProject(
      Map(
        "main.js" ->
          """const path = require('path');
            |console.log(path.basename(__filename), path.basename(__dirname) === path.basename(process.cwd()));
            |console.log(this === module.exports);
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "main.js true\ntrue")
    }
  }

  test("fs sync round-trip, stats and error codes") {
    withProject(
      Map(
        "main.js" ->
          """const fs = require('fs');
            |fs.mkdirSync('sub/deep', { recursive: true });
            |fs.writeFileSync('sub/deep/f.txt', 'hello');
            |fs.appendFileSync('sub/deep/f.txt', ' world');
            |console.log(fs.readFileSync('sub/deep/f.txt', 'utf8'));
            |console.log(Buffer.isBuffer(fs.readFileSync('sub/deep/f.txt')));
            |console.log(fs.statSync('sub/deep/f.txt').isFile(), fs.statSync('sub').isDirectory());
            |console.log(fs.readdirSync('sub/deep').includes('f.txt'));
            |const dirents = fs.readdirSync('sub/deep', { withFileTypes: true });
            |console.log(dirents[0].name, dirents[0].isFile());
            |try { fs.readFileSync('nope.txt'); } catch (e) { console.log(e.code); }
            |fs.rmSync('sub', { recursive: true, force: true });
            |console.log(fs.existsSync('sub'));
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(
        out.trim,
        "hello world\ntrue\ntrue true\ntrue\nf.txt true\nENOENT\nfalse"
      )
    }
  }

  test("Buffer encodings and static helpers") {
    withProject(
      Map(
        "main.js" ->
          """const { Buffer } = require('buffer');
            |console.log(Buffer.from('héllo').toString('hex'));
            |console.log(Buffer.from('aGk=', 'base64').toString());
            |console.log(Buffer.from([1, 2, 3, 4]).slice(1, 3).toString('hex'));
            |console.log(Buffer.concat([Buffer.from('ab'), Buffer.from('cd')]).toString());
            |console.log(Buffer.byteLength('héllo'), Buffer.alloc(3, 1).toString('hex'));
            |const b = Buffer.from('abcdef');
            |console.log(b.indexOf('cd'), b.includes('ef'), b.equals(Buffer.from('abcdef')));
            |console.log(b.readUInt16LE(0).toString(16), Buffer.isBuffer(b));
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(
        out.trim,
        "68c3a96c6c6f\nhi\n0203\nabcd\n6 010101\n2 true true\n6261 true"
      )
    }
  }

  test("path functions match Node POSIX semantics") {
    withProject(
      Map(
        "main.js" ->
          """const path = require('path');
            |console.log(path.join('/a/b', '../c'));
            |console.log(path.relative('/a/b', '/a/c'));
            |console.log(path.basename('/x/y/z.txt', '.txt'));
            |console.log(path.dirname('/x/y/z.txt'), path.extname('a.tar.gz'));
            |console.log(JSON.stringify(path.parse('/x/y/z.txt')));
            |console.log(path.resolve('/a', 'b', '..', 'c'));
            |console.log(path.normalize('a/../../b'), path.isAbsolute('/a'), path.isAbsolute('a'));
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(
        out.trim,
        """/a/c
          |../c
          |z
          |/x/y .gz
          |{"root":"/","dir":"/x/y","base":"z.txt","ext":".txt","name":"z"}
          |/a/c
          |../b true false""".stripMargin
      )
    }
  }

  test("process, util, assert, events and url") {
    withProject(
      Map(
        "main.js" ->
          """const util = require('util');
            |const assert = require('assert');
            |const { EventEmitter } = require('events');
            |const url = require('url');
            |console.log(util.format('%s=%d', 'x', 42));
            |console.log(util.inspect({ a: [1, 2], b: 'x' }));
            |assert.strictEqual(1, 1);
            |assert.deepStrictEqual({ a: [1], s: 'x' }, { a: [1], s: 'x' });
            |assert.throws(() => { throw new TypeError('boom'); }, TypeError);
            |const strict = require('assert/strict');
            |strict.strictEqual(2, 2);
            |const ee = new EventEmitter();
            |let got = null;
            |ee.on('x', v => got = v);
            |ee.emit('x', 5);
            |console.log('events', got, ee.listenerCount('x'));
            |class Sub extends EventEmitter { constructor() { super(); this.v = 9; } }
            |const sub = new Sub();
            |let subGot = null;
            |sub.on('y', v => subGot = v);
            |sub.emit('y', sub.v);
            |console.log('subclass', sub instanceof EventEmitter, subGot);
            |console.log(url.fileURLToPath(url.pathToFileURL('/a b/c')), url.pathToFileURL('/x').href);
            |console.log(process.platform.length > 0, process.cwd().length > 0, Array.isArray(process.argv));
            |process.on('exit', code => console.log('exit', code));
            |""".stripMargin
      ),
      argv = Vector("node", "main.js", "extra")
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(
        out.trim,
        """x=42
          |{ a: [ 1, 2 ], b: 'x' }
          |events 5 1
          |subclass true 9
          |/a b/c file:///x
          |true true true""".stripMargin
      )
    }
  }

  test("ESM imports builtins and CommonJS modules") {
    withProject(
      Map(
        "main.mjs" ->
          """import fs from 'node:fs';
            |import path from 'node:path';
            |import { strictEqual } from 'node:assert';
            |import cjs from './dep.js';
            |console.log(typeof fs.readFileSync, path.sep, cjs.hello);
            |strictEqual(cjs.hello, 'cjs');
            |const mod = await import('./dep.js');
            |console.log('dynamic', mod.default.num);
            |""".stripMargin,
        "dep.js" -> "module.exports = { hello: 'cjs', num: 7 };"
      ),
      entry = "main.mjs",
      argv = Vector("node", "main.mjs")
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.mjs"))
      assertEquals(out.trim, "function / cjs\ndynamic 7")
    }
  }

  test("module type from package.json selects ESM for .js files") {
    withProject(
      Map(
        "package.json" -> """{ "type": "module" }""",
        "main.js" ->
          """import path from 'node:path';
            |export const value = 1;
            |console.log('esm', path.sep);
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "esm /")
    }
  }

  test("process.exit terminates with the requested code") {
    withProject(
      Map(
        "main.js" ->
          """process.on('exit', code => console.log('exiting', code));
            |process.exit(3);
            |console.log('never');
            |""".stripMargin
      )
    ) { (node, dir) =>
      val lines = scala.collection.mutable.ArrayBuffer.empty[String]
      installConsoleCapture(lines)
      try node.loader.runMainFile(dir.resolve("main.js"))
      catch case exit: NodeExit => assertEquals(exit.code, 3)
      assertEquals(lines.mkString("\n").trim, "exiting 3")
    }
  }

  test("fs.promises exposes promise-returning file APIs") {
    withProject(
      Map(
        "main.js" ->
          """const fs = require('fs');
            |fs.promises.writeFile('pw.txt', 'async').then(() => fs.promises.readFile('pw.txt', 'utf8'))
            |  .then(text => console.log('promises', text, typeof fs.promises.readFile));
            |""".stripMargin
      )
    ) { (node, dir) =>
      val lines = scala.collection.mutable.ArrayBuffer.empty[String]
      installConsoleCapture(lines)
      node.loader.runMainFile(dir.resolve("main.js"))
      node.loop.run(summon[JSContext], propagateErrors = true)
      assertEquals(lines.mkString("\n").trim, "promises async function")
    }
  }


  /** Start a small local HTTP server for networking tests. */
  private def withHttpServer(body: Int => Unit): Unit = {
    val server = com.sun.net.httpserver.HttpServer.create(
      new java.net.InetSocketAddress("127.0.0.1", 0),
      0
    )
    def respond(
        exchange: com.sun.net.httpserver.HttpExchange,
        status: Int,
        contentType: String,
        text: String,
        extraHeader: Option[String] = None
    ): Unit = {
      val bytes = text.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      exchange.getResponseHeaders.set("content-type", contentType)
      extraHeader.foreach { header =>
        val idx = header.indexOf(':')
        exchange.getResponseHeaders.set(
          header.substring(0, idx).trim,
          header.substring(idx + 1).trim
        )
      }
      exchange.sendResponseHeaders(status, bytes.length.toLong)
      try exchange.getResponseBody.write(bytes)
      finally exchange.close()
    }
    server.createContext(
      "/json",
      exchange =>
        respond(
          exchange,
          200,
          "application/json",
          """{"msg":"hello","n":42}""",
          Some("X-Test: yes")
        )
    )
    server.createContext(
      "/echo",
      exchange => {
        val bytes = exchange.getRequestBody.readAllBytes()
        respond(
          exchange,
          201,
          "text/plain",
          "got:" + new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
        )
      }
    )
    server.createContext(
      "/slow",
      exchange => {
        Thread.sleep(2000)
        respond(exchange, 200, "text/plain", "slow")
      }
    )
    server.setExecutor(java.util.concurrent.Executors.newCachedThreadPool())
    server.start()
    try body(server.getAddress.getPort)
    finally server.stop(0)
  }

  test("fetch resolves responses, json, headers and error statuses") {
    withHttpServer { port =>
      withProject(
        Map(
          "main.js" ->
            s"""|(async () => {
                |  const r = await fetch('http://127.0.0.1:$port/json');
                |  console.log('status', r.status, r.ok, r.headers.get('content-type'), r.headers.get('x-test'));
                |  console.log('json', (await r.json()).msg);
                |  const post = await fetch('http://127.0.0.1:$port/echo', { method: 'POST', body: 'abc' });
                |  console.log('post', post.status, await post.text());
                |  const missing = await fetch('http://127.0.0.1:$port/nope');
                |  console.log('404', missing.status, missing.ok);
                |})().catch(e => console.log('fail', e.name, e.message));
                |""".stripMargin
        )
      ) { (node, dir) =>
        val out = runAndCapture(node, dir.resolve("main.js"))
        assertEquals(
          out.trim,
          "status 200 true application/json yes\njson hello\npost 201 got:abc\n404 404 false"
        )
      }
    }
  }

  test("fetch abort rejects with AbortError") {
    withHttpServer { port =>
      withProject(
        Map(
          "main.js" ->
            s"""|(async () => {
                |  const ac = new AbortController();
                |  setTimeout(() => ac.abort(), 30);
                |  try {
                |    await fetch('http://127.0.0.1:$port/slow', { signal: ac.signal });
                |    console.log('no abort');
                |  } catch (e) {
                |    console.log('abort', e.name);
                |  }
                |})();
                |""".stripMargin
        )
      ) { (node, dir) =>
        val out = runAndCapture(node, dir.resolve("main.js"))
        assertEquals(out.trim, "abort AbortError")
      }
    }
  }

  test("http.get streams response data and end events") {
    withHttpServer { port =>
      withProject(
        Map(
          "main.js" ->
            s"""|const http = require('http');
                |http.get('http://127.0.0.1:$port/json', res => {
                |  let body = '';
                |  res.setEncoding('utf8');
                |  res.on('data', chunk => body += chunk);
                |  res.on('end', () => console.log('http', res.statusCode, res.headers['x-test'], JSON.parse(body).n));
                |}).on('error', e => console.log('err', e.message));
                |""".stripMargin
        )
      ) { (node, dir) =>
        val out = runAndCapture(node, dir.resolve("main.js"))
        assertEquals(out.trim, "http 200 yes 42")
      }
    }
  }


  test("zlib round-trips and perf_hooks exposes performance") {
    withProject(
      Map(
        "main.js" ->
          """const zlib = require('zlib');
            |const { performance } = require('perf_hooks');
            |const gz = zlib.gzipSync('hello world');
            |console.log('gzip', zlib.gunzipSync(gz).toString());
            |console.log('deflate', zlib.inflateSync(zlib.deflateSync('abc')).toString());
            |console.log('perf', typeof performance.now(), typeof globalThis.performance.now());
            |zlib.gzip('async', (err, buf) => console.log('cb', err === null, zlib.gunzipSync(buf).toString()));
            |zlib.promises.gzip('prom').then(buf => console.log('promise', zlib.gunzipSync(buf).toString()));
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      // The callback and promise zlib variants both complete on the host pool,
      // so their relative order is not deterministic.
      assertEquals(
        out.trim.split("\n").sortBy(identity).mkString("\n"),
        "gzip hello world\ndeflate abc\nperf number number\ncb true async\npromise prom"
          .split("\n")
          .sortBy(identity)
          .mkString("\n")
      )
    }
  }


  test("package imports (#subpath) resolve via package.json") {
    withProject(
      Map(
        "package.json" ->
          """{ "name": "app", "imports": { "#colors": "./lib/colors.cjs" } }""",
        "lib/colors.cjs" -> "module.exports = { red: 1 };",
        "main.js" ->
          """const colors = require('#colors');
            |console.log('imports', colors.red);
            |""".stripMargin
      ),
      argv = Vector("node", "main.js")
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "imports 1")
    }
  }


  test("streams pipe data and string_decoder decodes UTF-8") {
    withProject(
      Map(
        "main.js" ->
          """const { Readable, Writable, PassThrough } = require('stream');
            |const { StringDecoder } = require('string_decoder');
            |const decoder = new StringDecoder('utf8');
            |const source = new Readable();
            |const sink = new Writable();
            |let received = '';
            |sink._write = (chunk, enc, cb) => { received += decoder.write(chunk); cb(); };
            |source.push(Buffer.from('hé'));
            |source.push(Buffer.from('llo'));
            |source.push(null);
            |source.pipe(sink);
            |console.log('pipe', received, decoder.end().length);
            |const pt = new PassThrough();
            |let pass = '';
            |pt.on('data', chunk => pass += chunk.toString());
            |pt.write(Buffer.from('abc'));
            |pt.end();
            |setTimeout(() => console.log('pass', pass), 10);
            |const tty = require('tty');
            |console.log('tty', typeof tty.isatty(1), require('diagnostics_channel').channel('x').hasSubscribers);
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(
        out.trim,
        "pipe héllo 0\ntty boolean false\npass abc"
      )
    }
  }


  test("fs methods copied onto another object still work") {
    withProject(
      Map(
        "main.js" ->
          """const fs = require('fs');
            |const wrapper = { readdirSync: fs.readdirSync, readFileSync: fs.readFileSync };
            |fs.writeFileSync('f.txt', 'data');
            |const entries = wrapper.readdirSync('.', { withFileTypes: true });
            |console.log('reexport', Array.isArray(entries), entries.some(e => e.name === 'f.txt'));
            |console.log('read', wrapper.readFileSync('f.txt', 'utf8'));
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "reexport true true\nread data")
    }
  }

  test("crypto, child_process and querystring") {
    withProject(
      Map(
        "main.js" ->
          """const crypto = require('crypto');
            |const qs = require('querystring');
            |console.log(crypto.createHash('sha256').update('abc').digest('hex'));
            |console.log(crypto.createHash('md5').update('abc').digest('base64'));
            |console.log(crypto.createHmac('sha256', 'key').update('msg').digest('hex'));
            |console.log(crypto.randomBytes(8).length, crypto.randomUUID().length);
            |console.log(crypto.timingSafeEqual(Buffer.from('ab'), Buffer.from('ab')));
            |console.log(globalThis.crypto.getRandomValues(new Uint8Array(4)).length);
            |console.log(qs.stringify({ a: 1, b: 'x y' }), JSON.stringify(qs.parse('a=1&b=2&a=3')));
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(
        out.trim,
        """ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
          |kAFQmDzST7DWlj99KOF/cg==
          |2d93cbc1be167bcb1637a4a23cbff01a7878f0c50ee833954ea5221bb1b8c628
          |8 36
          |true
          |4
          |a=1&b=x%20y {"a":["1","3"],"b":"2"}""".stripMargin
      )
    }
  }

  test("child_process.execSync runs a shell command") {
    assume(!NodeOs.isWindows, "shell test is POSIX-only")
    withProject(
      Map(
        "main.js" ->
          """const { execSync, spawnSync } = require('child_process');
            |console.log(execSync('echo compat').toString().trim());
            |try { execSync('exit 3'); } catch (e) { console.log('status', e.status); }
            |const r = spawnSync('echo', ['spawned']);
            |console.log(r.status, r.stdout.toString().trim());
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "compat\nstatus 3\n0 spawned")
    }
  }

  test("differential: output matches the node binary when available") {
    val nodeBinary = Seq("node")
      .find { name =>
        try {
          val process = new ProcessBuilder(name, "--version")
            .redirectErrorStream(true)
            .start()
          process.waitFor()
          process.exitValue() == 0
        } catch case _: Throwable => false
      }
    assume(nodeBinary.isDefined, "node binary is not installed")

    val files = Map(
      "main.js" ->
        """const path = require('path');
          |const fs = require('fs');
          |const util = require('util');
          |const assert = require('assert');
          |const { EventEmitter } = require('events');
          |const { Buffer } = require('buffer');
          |fs.mkdirSync('d', { recursive: true });
          |fs.writeFileSync('d/f.txt', 'x');
          |assert.deepStrictEqual({ a: [1] }, { a: [1] });
          |const ee = new EventEmitter();
          |let v = null;
          |ee.on('e', x => v = x);
          |ee.emit('e', 3);
          |console.log(path.join('a', 'b', '..', 'c'), fs.readFileSync('d/f.txt', 'utf8'), v);
          |console.log(util.format('%s:%d', 'n', 5));
          |console.log(Buffer.from('abc').toString('base64'));
          |console.log(JSON.stringify(path.parse('x/y.txt')));
          |console.log(process.argv.slice(2).join(','));
          |""".stripMargin
    )

    withProject(files, argv = Vector("node", "main.js", "one", "two")) {
      (node, dir) =>
        val nodeOut = {
          val process = new ProcessBuilder(nodeBinary.get, "main.js", "one", "two")
            .directory(dir.toFile)
            .redirectErrorStream(true)
            .start()
          val text = new String(process.getInputStream.readAllBytes())
          process.waitFor()
          text
        }
        val ourOut = runAndCapture(node, dir.resolve("main.js"))
        assertEquals(ourOut.trim, nodeOut.trim)
    }
  }
}
