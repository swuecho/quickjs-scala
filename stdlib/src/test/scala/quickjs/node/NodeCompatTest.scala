package quickjs.node

import quickjs.runtime.{JSContext, JSException, JSRuntime, StdLib}
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

  test("top-level await in an ESM entry waits for timers and fs") {
    withProject(
      Map(
        "main.mjs" ->
          """import { readFile } from 'node:fs/promises';
            |const tick = await new Promise(resolve => setTimeout(() => resolve(1), 15));
            |const text = await readFile('./data.txt', 'utf8');
            |console.log('tla', tick + 1, text.trim());
            |""".stripMargin,
        "data.txt" -> "hello\n"
      ),
      entry = "main.mjs",
      argv = Vector("node", "main.mjs")
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.mjs"))
      assertEquals(out.trim, "tla 2 hello")
    }
  }

  test("imported ESM modules with top-level await finish before their bindings are read") {
    withProject(
      Map(
        "dep.mjs" ->
          """export const value = await new Promise(resolve => setTimeout(() => resolve(40), 10));
            |""".stripMargin,
        "main.mjs" ->
          """import { value } from './dep.mjs';
            |const extra = await new Promise(resolve => setTimeout(() => resolve(2), 5));
            |console.log('sum', value + extra);
            |""".stripMargin
      ),
      entry = "main.mjs",
      argv = Vector("node", "main.mjs")
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.mjs"))
      assertEquals(out.trim, "sum 42")
    }
  }

  test("top-level for await iterates sync and async iterables") {
    withProject(
      Map(
        "main.mjs" ->
          """const out = [];
            |for await (const v of [1, 2, 3]) out.push(v);
            |const asyncIterable = {
            |  [Symbol.asyncIterator]() {
            |    let i = 10;
            |    return {
            |      next: () => Promise.resolve(i < 12
            |        ? { value: ++i, done: false }
            |        : { value: undefined, done: true })
            |    };
            |  }
            |};
            |for await (const v of asyncIterable) out.push(v);
            |for await (const v of (async function* () { yield 20; yield 21; })()) out.push(v);
            |console.log('fa', out.join(','));
            |""".stripMargin
      ),
      entry = "main.mjs",
      argv = Vector("node", "main.mjs")
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.mjs"))
      assertEquals(out.trim, "fa 1,2,3,11,12,20,21")
    }
  }

  test("top-level await rejection fails module evaluation") {
    withProject(
      Map(
        "main.mjs" ->
          """await Promise.reject(new Error('tla boom'));
            |console.log('never');
            |""".stripMargin
      ),
      entry = "main.mjs",
      argv = Vector("node", "main.mjs")
    ) { (node, dir) =>
      val error = intercept[JSException] {
        node.loader.runMainFile(dir.resolve("main.mjs"))
      }
      error.getValue match {
        case JSValue.Object(obj) =>
          assertEquals(obj.get("message"), JSValue.fromString("tla boom"))
        case other =>
          fail(s"expected an Error object, got $other")
      }
    }
  }

  test("constants module mirrors fs.constants and exposes hasOwnProperty") {
    withProject(
      Map(
        "main.js" ->
          """const fs = require('fs');
            |const constants = require('constants');
            |console.log(
            |  constants.O_CREAT === fs.constants.O_CREAT,
            |  typeof constants.hasOwnProperty,
            |  constants.hasOwnProperty('O_WRONLY'),
            |  constants.O_RDWR === 2
            |);
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "true function true true")
    }
  }

  test("os.constants, stream statics, util helpers and v8") {
    withProject(
      Map(
        "main.js" ->
          """const os = require('os');
            |const stream = require('stream');
            |const util = require('util');
            |const v8 = require('v8');
            |console.log(
            |  os.constants.signals.SIGTERM === 15,
            |  os.constants.errno.ENOENT === 2,
            |  stream.getDefaultHighWaterMark(false) === 65536,
            |  stream.getDefaultHighWaterMark(true) === 16,
            |  typeof util.debuglog('x').enabled === 'boolean',
            |  util.stripVTControlCharacters('\u001b[31mred\u001b[39m') === 'red',
            |  typeof util.callbackify === 'function',
            |  v8.deserialize(v8.serialize({ a: 1 })).a === 1
            |);
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "true true true true true true true true")
    }
  }

  test("spawnSync exposes the output array and buffer encoding") {
    withProject(
      Map(
        "main.js" ->
          """const {spawnSync} = require('child_process');
            |const r = spawnSync('echo', ['hi'], { encoding: 'buffer' });
            |console.log(
            |  r.status === 0,
            |  Array.isArray(r.output),
            |  r.output.length === 3,
            |  Buffer.isBuffer(r.stdout),
            |  r.stdout.toString().trim() === 'hi'
            |);
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "true true true true true")
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
  test("module.createRequire returns a working require for the caller") {
    withProject(
      Map(
        "main.js" ->
          """const { createRequire } = require('node:module');
            |const req = createRequire(__filename);
            |console.log(req('./dep.js').value);
            |if (typeof createRequire !== 'function') throw new Error('createRequire');
            |""".stripMargin,
        "dep.js" -> "module.exports = { value: 'dep-value' };"
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim.split('\n').head, "dep-value")
    }
  }

  test("require of an ESM module honors the 'module.exports' interop export") {
    withProject(
      Map(
        "main.js" ->
          """const ui = require('./dep.mjs');
            |console.log(typeof ui, ui('opts'));
            |""".stripMargin,
        "dep.mjs" ->
          """function ui(opts) { return 'ui:' + opts; }
            |export default ui;
            |export { ui as 'module.exports' };
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "function ui:opts")
    }
  }

  test("ESM default function declarations create a module-local binding") {
    withProject(
      Map(
        "main.mjs" ->
          """import ui, { mod } from './dep.mjs';
            |console.log(ui('a'), mod('b'));
            |""".stripMargin,
        "dep.mjs" ->
          """export default function ui(opts) { return 'ui:' + opts; }
            |export function mod(opts) { return ui(opts) + ':mod'; }
            |export { ui as 'module.exports' };
            |""".stripMargin
      ),
      entry = "main.mjs",
      argv = Vector("node", "main.mjs")
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.mjs"))
      assertEquals(out.trim, "ui:a ui:b:mod")
    }
  }

  test("ESM exported bindings are live after evaluation (TypeScript enums)") {
    withProject(
      Map(
        "main.mjs" ->
          """import { Kind } from './dep.mjs';
            |console.log(Kind.BOOLEAN, Kind.NUMBER);
            |""".stripMargin,
        "dep.mjs" ->
          """export var Kind;
            |(function (Kind) {
            |  Kind["BOOLEAN"] = "boolean";
            |  Kind["NUMBER"] = "number";
            |})(Kind || (Kind = {}));
            |""".stripMargin
      ),
      entry = "main.mjs",
      argv = Vector("node", "main.mjs")
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.mjs"))
      assertEquals(out.trim, "boolean number")
    }
  }

  test("util.format.apply(format, args) matches a direct call") {
    withProject(
      Map(
        "main.js" ->
          """const util = require('util');
            |console.log(util.format.apply(util.format, ['a %s', 'b']));
            |console.log(util.format.call(util.format, '%d-%d', 1, 2));
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "a b\n1-2")
    }
  }

  test("ESM namespace imports of builtins call methods with the module receiver") {
    withProject(
      Map(
        "main.mjs" ->
          """import * as sp from 'node:path';
            |import * as fsns from 'node:fs';
            |import * as osns from 'node:os';
            |console.log(sp.normalize('/a//b/../c'), sp.join('x', 'y'));
            |const file = sp.join(osns.tmpdir(), 'ns-receiver-' + process.pid + '.txt');
            |fsns.writeFileSync(file, 'hi');
            |console.log(fsns.readFileSync(file, 'utf8'), typeof osns.platform());
            |fsns.unlinkSync(file);
            |""".stripMargin
      ),
      entry = "main.mjs",
      argv = Vector("node", "main.mjs")
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.mjs"))
      assertEquals(out.trim, "/a/c x/y\nhi string")
    }
  }


  test("ESM named imports read CJS accessor exports") {
    withProject(
      Map(
        "main.mjs" ->
          """import { map, filter } from './ops.cjs';
            |console.log(map('x'), filter('y'));
            |""".stripMargin,
        "ops.cjs" ->
          """Object.defineProperty(exports, 'map', { enumerable: true, get: () => (v) => 'map:' + v });
            |Object.defineProperty(exports, 'filter', { enumerable: true, get: () => (v) => 'filter:' + v });
            |""".stripMargin
      ),
      entry = "main.mjs",
      argv = Vector("node", "main.mjs")
    ) { (node, dir) =>
      assertEquals(
        runAndCapture(node, dir.resolve("main.mjs")).trim,
        "map:x filter:y"
      )
    }
  }

  test("require of a CJS re-export facade returns the CJS value") {
    withProject(
      Map(
        "main.js" ->
          """const F = require('./index.mjs');
            |console.log(typeof F, F.name, F.x, new F().tag);
            |const Only = require('./only.mjs');
            |console.log(typeof Only, typeof Only.default);
            |""".stripMargin,
        "index.mjs" ->
          """export { default } from './dep.cjs';
            |export * from './dep.cjs';
            |""".stripMargin,
        "dep.cjs" ->
          """function F() { this.tag = 'made'; }
            |F.x = 7;
            |module.exports = F;
            |module.exports.F = F;
            |""".stripMargin,
        "only.mjs" -> "export default function F() {}\n"
      )
    ) { (node, dir) =>
      assertEquals(
        runAndCapture(node, dir.resolve("main.js")).trim,
        "function F 7 made\nobject function"
      )
    }
  }

  test("timer globals copied onto another object still fire") {
    withProject(
      Map(
        "main.js" ->
          """const holder = { si: setImmediate, st: setTimeout };
            |holder.si(() => console.log('immediate'));
            |holder.st(() => console.log('timeout'), 1);
            |""".stripMargin
      )
    ) { (node, dir) =>
      assertEquals(
        runAndCapture(node, dir.resolve("main.js")).trim,
        "immediate\ntimeout"
      )
    }
  }

  test("super() without arguments initializes a native superclass receiver") {
    withProject(
      Map(
        "main.js" ->
          """const { EventEmitter } = require('events');
            |class E extends EventEmitter { constructor() { super(); this.tag = 'e'; } }
            |const e = new E();
            |console.log(e instanceof E, e instanceof EventEmitter, e.tag);
            |let got = null;
            |e.on('x', v => got = v);
            |e.emit('x', 5);
            |console.log('got', got);
            |""".stripMargin
      )
    ) { (node, dir) =>
      assertEquals(
        runAndCapture(node, dir.resolve("main.js")).trim,
        "true true e\ngot 5"
      )
    }
  }

  test("async child_process spawn/exec/execFile stream output") {
    withProject(
      Map(
        "main.js" ->
          """const { spawn, exec, execFile } = require('child_process');
            |const child = spawn('echo', ['hello']);
            |let out = '';
            |child.stdout.on('data', d => (out += d));
            |child.on('close', code => {
            |  console.log('spawn', out.trim(), code);
            |  exec('echo exec-ok', (err, stdout) => {
            |    console.log('exec', err === null, stdout.trim());
            |    execFile('echo', ['file-ok'], (err2, stdout2) => {
            |      console.log('execFile', err2 === null, stdout2.trim());
            |      const bad = spawn('definitely-not-a-real-binary-xyz');
            |      bad.on('error', e => console.log('err', e.code));
            |    });
            |  });
            |});
            |""".stripMargin
      )
    ) { (node, dir) =>
      assertEquals(
        runAndCapture(node, dir.resolve("main.js")).trim,
        "spawn hello 0\nexec true exec-ok\nexecFile true file-ok\nerr ENOENT"
      )
    }
  }

  test("async child_process supports stdin and kill") {
    withProject(
      Map(
        "main.js" ->
          """const { spawn } = require('child_process');
            |const upper = spawn('tr', ['a-z', 'A-Z']);
            |let out = '';
            |upper.stdout.on('data', d => (out += d));
            |upper.on('close', code => {
            |  console.log('stdin', out.trim(), code);
            |  const sleeper = spawn('sleep', ['5']);
            |  sleeper.on('close', (c, signal) => {
            |    console.log('killed', sleeper.killed, c !== 0);
            |  });
            |  setTimeout(() => sleeper.kill('SIGKILL'), 30);
            |});
            |upper.stdin.end('abc\n');
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assertEquals(out.trim, "stdin ABC 0\nkilled true true")
    }
  }


  test("readline line events, question and async iteration") {
    withProject(
      Map(
        "main.js" ->
          """const readline = require('readline');
            |function fakeInput() {
            |  return { on() {}, addListener() {}, removeListener() {}, resume() {}, pause() {} };
            |}
            |function fakeOutput(sink) {
            |  return { write(s) { sink.push(s); return true; } };
            |}
            |const out = [];
            |const rl = readline.createInterface({ input: fakeInput(), output: fakeOutput(out), terminal: false });
            |const lines = [];
            |rl.on('line', l => lines.push(l));
            |rl.write('alpha\nbeta\n');
            |rl.question('name? ', answer => {
            |  console.log('lines', lines.join(','), 'answer', answer, 'prompt', JSON.stringify(out.join('')));
            |  rl.close();
            |});
            |rl.write('bob\n');
            |""".stripMargin
      )
    ) { (node, dir) =>
      assertEquals(
        runAndCapture(node, dir.resolve("main.js")).trim,
        "lines alpha,beta answer bob prompt \"name? \""
      )
    }
  }

  test("readline/promises question and vm helpers") {
    withProject(
      Map(
        "main.js" ->
          """const rlp = require('readline/promises');
            |const vm = require('vm');
            |const input = { on() {}, addListener() {}, removeListener() {}, resume() {}, pause() {} };
            |const rl = rlp.createInterface({ input, terminal: false });
            |rl.question('q? ').then(answer => {
            |  console.log('promise', answer);
            |});
            |rl.write('pong\n');
            |console.log('vm', vm.runInThisContext('1 + 2'), new vm.Script('var z = 4; z * 2').runInThisContext());
            |console.log('vm-fn', vm.compileFunction('return 5')());
            |console.log('vm-ctx', vm.isContext(vm.createContext({})));
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assert(out.contains("promise pong"), out)
      assert(out.contains("vm 3 8"), out)
      assert(out.contains("vm-fn 5"), out)
      assert(out.contains("vm-ctx true"), out)
    }
  }

  test("repl evaluates expressions and exits") {
    withProject(
      Map(
        "main.js" ->
          """const repl = require('repl');
            |const output = [];
            |const input = { on() {}, addListener() {}, removeListener() {}, resume() {}, pause() {} };
            |const server = repl.start({
            |  input,
            |  output: { write(s) { output.push(s); return true; } },
            |  terminal: false,
            |  prompt: ''
            |});
            |server.__rl.write('1 + 2\n');
            |server.__rl.write('var x = 7\n');
            |server.__rl.write('x * 2\n');
            |server.on('exit', () => console.log('repl-output', JSON.stringify(output.join(''))));
            |server.__rl.write('.exit\n');
            |""".stripMargin
      )
    ) { (node, dir) =>
      assertEquals(
        runAndCapture(node, dir.resolve("main.js")).trim,
        "repl-output \"3\\n14\\n\""
      )
    }
  }


  test("http.createServer serves requests and parses bodies") {
    withProject(
      Map(
        "main.js" ->
          """const http = require('http');
            |const server = http.createServer((req, res) => {
            |  if (req.url === '/post') {
            |    let body = '';
            |    req.on('data', d => (body += d));
            |    req.on('end', () => {
            |      res.statusCode = 201;
            |      res.setHeader('X-Echo', 'yes');
            |      res.end('echo:' + body);
            |    });
            |    return;
            |  }
            |  res.setHeader('Content-Type', 'application/json');
            |  res.end(JSON.stringify({ method: req.method, url: req.url, ua: req.headers['user-agent'] }));
            |});
            |server.listen(0, '127.0.0.1', () => {
            |  const port = server.address().port;
            |  http.get({ hostname: '127.0.0.1', port, path: '/hi?x=1', headers: { 'User-Agent': 'qjs' } }, res => {
            |    let data = '';
            |    res.on('data', c => (data += c));
            |    res.on('end', () => {
            |      console.log('GET', res.statusCode, data);
            |      const req = http.request({ hostname: '127.0.0.1', port, path: '/post', method: 'POST', headers: { 'Content-Length': 5 } }, res2 => {
            |        let d2 = '';
            |        res2.on('data', c => (d2 += c));
            |        res2.on('end', () => {
            |          console.log('POST', res2.statusCode, res2.headers['x-echo'], d2);
            |          server.close(() => console.log('CLOSED'));
            |        });
            |      });
            |      req.write('hello');
            |      req.end();
            |    });
            |  });
            |});
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assert(
        out.contains("""GET 200 {"method":"GET","url":"/hi?x=1","ua":"qjs"}"""),
        out
      )
      assert(out.contains("POST 201 yes echo:hello"), out)
      assert(out.contains("CLOSED"), out)
    }
  }

  test("EventEmitter methods can be mixed into a function") {
    withProject(
      Map(
        "main.js" ->
          """const EventEmitter = require('events');
            |function app() {}
            |for (const key of Object.getOwnPropertyNames(EventEmitter.prototype)) {
            |  if (key !== 'constructor') {
            |    Object.defineProperty(app, key, Object.getOwnPropertyDescriptor(EventEmitter.prototype, key));
            |  }
            |}
            |let got = null;
            |app.on('x', v => (got = v));
            |app.emit('x', 7);
            |console.log('mixin', got, app.listenerCount('x'));
            |""".stripMargin
      )
    ) { (node, dir) =>
      assertEquals(
        runAndCapture(node, dir.resolve("main.js")).trim,
        "mixin 7 1"
      )
    }
  }

  test("fs.watch reports changes and closes") {
    withProject(
      Map(
        "main.js" ->
          """const fs = require('fs');
            |const path = require('path');
            |const file = path.join(process.cwd(), 'watched.txt');
            |fs.writeFileSync(file, 'a');
            |const watcher = fs.watch(file, (eventType, filename) => {
            |  console.log('EVENT', eventType, filename);
            |  watcher.close();
            |});
            |setTimeout(() => fs.writeFileSync(file, 'b'), 50);
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assert(out.contains("EVENT change watched.txt"), out)
    }
  }

  test("fs.watchFile polls for changes and unwatchFile stops") {
    withProject(
      Map(
        "main.js" ->
          """const fs = require('fs');
            |const path = require('path');
            |const file = path.join(process.cwd(), 'polled.txt');
            |fs.writeFileSync(file, 'a');
            |let fired = 0;
            |fs.watchFile(file, { interval: 30 }, (curr, prev) => {
            |  fired++;
            |  console.log('POLL', curr.size, prev.size);
            |  fs.unwatchFile(file);
            |});
            |setTimeout(() => fs.writeFileSync(file, 'changed'), 100);
            |setTimeout(() => console.log('DONE', fired), 400);
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assert(out.contains("POLL 7 1"), out)
      assert(out.contains("DONE 1"), out)
    }
  }

  test("Readable subclasses drive _read in object mode") {
    withProject(
      Map(
        "main.js" ->
          """const { Readable } = require('stream');
            |class R extends Readable {
            |  constructor() { super({ objectMode: true }); this.i = 0; }
            |  _read() {
            |    if (this.i < 3) this.push({ n: this.i++ });
            |    else this.push(null);
            |  }
            |}
            |const r = new R();
            |const seen = [];
            |r.on('data', d => seen.push(d.n));
            |r.on('end', () => console.log('STREAM', seen.join(',')));
            |""".stripMargin
      )
    ) { (node, dir) =>
      assertEquals(
        runAndCapture(node, dir.resolve("main.js")).trim,
        "STREAM 0,1,2"
      )
    }
  }

  test("fs/promises methods strip arbitrary receivers") {
    withProject(
      Map(
        "main.js" ->
          """const fsp = require('fs/promises');
            |fsp.stat.call({}, __filename).then(s => console.log('STAT-CALL', s.isFile()));
            |""".stripMargin
      )
    ) { (node, dir) =>
      assert(
        runAndCapture(node, dir.resolve("main.js")).contains("STAT-CALL true")
      )
    }
  }

  test("mkdtemp accepts arbitrary prefixes") {
    withProject(
      Map(
        "main.js" ->
          """const fs = require('fs');
            |const path = require('path');
            |const base = path.join(process.cwd(), 'mkdtemp-');
            |const dir = fs.mkdtempSync(base);
            |console.log('SYNC', dir.startsWith(base), fs.statSync(dir).isDirectory());
            |fs.promises.mkdtemp(base).then(p =>
            |  console.log('PROMISE', p.startsWith(base))
            |);
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js"))
      assert(out.contains("SYNC true true"), out)
      assert(out.contains("PROMISE true"), out)
    }
  }

  test("crypto ciphers, KDF vectors and signatures") {
    withProject(
      Map(
        "main.js" ->
          """const crypto = require('crypto');
            |const key = crypto.randomBytes(32);
            |const iv = crypto.randomBytes(12);
            |const cipher = crypto.createCipheriv('aes-256-gcm', key, iv);
            |cipher.setAAD(Buffer.from('hdr'));
            |const enc = Buffer.concat([cipher.update('secret'), cipher.final()]);
            |const tag = cipher.getAuthTag();
            |const decipher = crypto.createDecipheriv('aes-256-gcm', key, iv);
            |decipher.setAAD(Buffer.from('hdr'));
            |decipher.setAuthTag(tag);
            |console.log('GCM', Buffer.concat([decipher.update(enc), decipher.final()]).toString());
            |console.log('PBKDF2', crypto.pbkdf2Sync('password', 'salt', 4096, 32, 'sha256').toString('hex').slice(0, 16));
            |console.log('SCRYPT', crypto.scryptSync('', '', 32, { N: 16, r: 1, p: 1 }).toString('hex').slice(0, 16));
            |console.log('HKDF', crypto.hkdfSync('sha256', Buffer.alloc(22, 0x0b), Buffer.from('000102030405060708090a0b0c', 'hex'), Buffer.from('f0f1f2f3f4f5f6f7f8f9', 'hex'), 16).toString('hex').slice(0, 16));
            |const kp = crypto.generateKeyPairSync('rsa', { modulusLength: 2048 });
            |const sig = crypto.sign('sha256', Buffer.from('data'), kp.privateKey);
            |console.log('RSA', crypto.verify('sha256', Buffer.from('data'), kp.publicKey, sig));
            |const ed = crypto.generateKeyPairSync('ed25519');
            |const pem = ed.privateKey.export({ format: 'pem' });
            |const edSig = crypto.sign(null, Buffer.from('x'), pem);
            |console.log('ED', crypto.verify(null, Buffer.from('x'), ed.publicKey.export({ format: 'pem' }), edSig));
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js")).trim
      val lines = out.split("\n").toList
      assertEquals(lines.head, "GCM secret")
      assertEquals(lines(1), "PBKDF2 c5e478d59288c841")
      assertEquals(lines(2), "SCRYPT 77d6576238657b20")
      assertEquals(lines(3), "HKDF 3cb25f25faacd57a")
      assertEquals(lines(4), "RSA true")
      assertEquals(lines(5), "ED true")
    }
  }

  test("crypto secret keys and Buffer statics used by safe-buffer") {
    withProject(
      Map(
        "main.js" ->
          """const crypto = require('crypto');
            |const secret = crypto.createSecretKey(Buffer.from('secret'));
            |console.log('SECRET', secret.type, secret.symmetricKeySize);
            |console.log('HMAC', crypto.createHmac('sha256', secret).update('data').digest('hex').slice(0, 16));
            |console.log('ISA', typeof Buffer.isEncoding === 'function', Buffer.isEncoding('utf8'), Buffer.isEncoding('nope'));
            |console.log('SLOW', Buffer.allocUnsafeSlow(4).length, Buffer.isBuffer(Buffer.allocUnsafeSlow(4)));
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js")).trim
      val lines = out.split("\n").toList
      assertEquals(lines.head, "SECRET secret 6")
      assertEquals(lines(1), "HMAC 1b2c16b75bd2a870")
      assertEquals(lines(2), "ISA true true false")
      assertEquals(lines(3), "SLOW 4 true")
    }
  }

  test("require.extensions registers custom loaders and resolves them") {
    withProject(
      Map(
        "main.js" ->
          """const fs = require('fs');
            |const path = require('path');
            |fs.writeFileSync(path.join(__dirname, 'data.txt'), 'hello');
            |require.extensions['.txt'] = function (module, filename) {
            |  module.exports = 'TXT:' + fs.readFileSync(filename, 'utf8');
            |};
            |console.log('EXT', require('./data.txt'));
            |console.log('EXT2', require('./data'));
            |const mod = require('module');
            |console.log('HAS', typeof require.extensions['.js'], typeof mod.extensions['.js'], typeof mod.Module._preloadModules);
            |""".stripMargin
      )
    ) { (node, dir) =>
      val out = runAndCapture(node, dir.resolve("main.js")).trim
      val lines = out.split("\n").toList
      assertEquals(lines.head, "EXT TXT:hello")
      assertEquals(lines(1), "EXT2 TXT:hello")
      assertEquals(lines(2), "HAS function function function")
    }
  }
}
