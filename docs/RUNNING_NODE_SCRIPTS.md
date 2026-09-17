# Running Node.js Scripts with QuickJS-Scala

QuickJS-Scala ships a standalone runner (`quickjs-runner.jar`) that executes
JavaScript files without Node.js installed. With the `--node` flag it runs
Node-style scripts: `require`, CommonJS/ESM resolution through `node_modules`,
`process`, `fs`, `path`, streams, HTTP servers/clients, crypto, and the rest of
the compatibility layer described below.

This guide covers:

1. Building the runner
2. Running plain scripts vs. Node scripts
3. Command-line reference
4. What `--node` provides (globals, built-ins, resolution)
5. Worked examples
6. Embedding `NodeRuntime` in a Scala program
7. Differences from real Node and troubleshooting

---

## 1. Build the runner

```bash
# from the repository root
sbt runner/assembly
```

This produces a self-contained JAR:

```
runner/target/scala-3.7.4/quickjs-runner.jar
```

The Scala version in the path depends on the project build; `runner/target/scala-*/quickjs-runner.jar`
always matches. Java 21+ is required.

For a convenient command (optional):

```bash
alias quickjs='java -jar /path/to/quickjs-scala/runner/target/scala-3.7.4/quickjs-runner.jar'

# e.g. use the built jar:
quickjs --node script.js
```

During development you can run the same entry point through sbt:

```bash
sbt "runner/runMain quickjs.stdlib.Runner --node script.js"
```

---

## 2. Quick start

```bash
# Plain JS script (browser-like globals: console, timers, URL, atob, ...)
java -jar quickjs-runner.jar script.js

# Node.js compatibility mode: require/process/fs/http/...
java -jar quickjs-runner.jar --node script.js

# ES module (import, top-level await, import.meta.url)
java -jar quickjs-runner.jar --node app.mjs

# Inline evaluation
java -jar quickjs-runner.jar --node -e "console.log(process.version)"

# Pass script arguments (exposed via process.argv)
java -jar quickjs-runner.jar --node tool.js --verbose input.txt
```

A minimal Node script:

```js
// hello.js
const os = require('os');
const path = require('path');

console.log('hello from', process.platform);
console.log('argv:', process.argv.slice(2));
console.log(path.join(os.tmpdir(), 'demo.txt'));
```

```bash
java -jar quickjs-runner.jar --node hello.js one two
# hello from linux
# argv: [ 'one', 'two' ]
# /tmp/demo.txt
```

Without `--node`, the same file still runs but `require` and `process` are not
defined — plain mode is only for self-contained scripts or ES modules.

---

## 3. Modes of execution

| Invocation | Mode | Description |
|---|---|---|
| `runner script.js` | plain script | Browser-like globals; no `require`/`process`. |
| `runner script.mjs` | ES module | `import`/`export`, top-level `await`, `import.meta.url`. |
| `runner -m script.js` | ES module (forced) | Treat a `.js` file as a module. |
| `runner --node script.js` | Node CommonJS | `require`, `module.exports`, `__filename`, `__dirname`. |
| `runner --node script.mjs` | Node ESM | Node-style imported ESM with the Node built-ins. |
| `runner --node -e "<code>"` | Node CJS eval | Runs inline code as a CommonJS module (`<eval>`). |
| `runner -e "<code>"` | plain eval | Inline script evaluation. |

### Plain script mode

`Globals.initialize` + `Timers.initialize` are installed: `console`, `setTimeout`/`setInterval`/`clearTimeout`/`clearInterval`,
`URL`/`URLSearchParams`, `TextEncoder`/`TextDecoder`, `atob`/`btoa`, `structuredClone`,
`escape`/`unescape`, plus a `scriptArgs` global. The event loop is drained after the
top-level code finishes.

### Node mode (`--node`)

`NodeRuntime.install` replaces the plain environment with the Node surface:

* CommonJS by default; `.mjs`, or a nearest `package.json` with `"type": "module"`,
  selects ESM.
* `require()` resolution: relative paths, extension and `/index` lookup,
  `node_modules` walk, `package.json` `main`/`exports`/`type`, JSON modules,
  circular dependencies, `require.extensions`, and `module._compile` (used by
  loaders such as ts-node).
* `import`/dynamic `import()`/`require()` interop (CJS namespaces, `import fs from 'node:fs'`,
  `export { x as 'module.exports' }`).
* The host event loop (`HostEventLoop`) runs timers, `setImmediate`, async fs,
  zlib, child processes and HTTP completions. Top-level `await` suspends the
  module evaluation and lets the loop make progress.

---

## 4. Command-line reference

```
Usage: quickjs-runner [options] <script.js> [args...]

Options:
  -m, --module         Execute the file as an ES module
      --node           Run with Node.js compatibility (require, process, fs, ...)
  -e, --eval <code>    Evaluate inline JavaScript code
  -v, --version        Show version number
  -h, --help           Show this help message

Examples:
  quickjs-runner script.js
  quickjs-runner module.mjs
  quickjs-runner -m script.js
  quickjs-runner --node script.js
  quickjs-runner --eval "console.log('hello')"
```

Notes:

* Everything after the script path is passed to the script (`process.argv`,
  `scriptArgs`). `--` also ends option parsing.
* `--node` must appear before the script path.
* `process.argv` is `["node", "<absolute script path>", ...args]`. In
  `--node -e` mode it is `["node", ...args]`.
* Exit status is the script's `process.exit(code)` value; uncaught errors print
  a formatted stack trace and exit with `1`.

---

## 5. What `--node` provides

### Globals

| Global | Notes |
|---|---|
| `process` | Real EventEmitter: `argv`, `env`, `cwd()`, `chdir()`, `exit()`, `exitCode`, `nextTick()`, `hrtime`, `stdout`/`stderr`, `stdin` (a real Readable), `platform`, `arch`, `version`, `versions`, `on('exit')`, ... |
| `Buffer` | `Uint8Array` subclass with Node encodings and statics (`from`, `alloc`, `concat`, `isBuffer`, `allocUnsafeSlow`, `isEncoding`, ...) |
| `global` | Alias of `globalThis` |
| `setTimeout`/`setInterval`/`setImmediate` (+`clear*`) | Loop-backed, due-time ordered, microtasks drained between callbacks |
| `console` | Node-style `util.format` formatting; `log`/`info`/`debug` to stdout, `error`/`warn` to stderr, `dir`, `assert`, `trace` |
| `fetch`, `Headers`, `Request`, `Response`, `AbortController`, `AbortSignal` | Built on `java.net.http.HttpClient` (buffered bodies) |
| `crypto` | WebCrypto-like global (also `require('crypto')`) |
| `TextEncoder`/`TextDecoder`, `atob`/`btoa`, `structuredClone`, `URL`/`URLSearchParams`, `Intl` | Shared with plain mode |
| `escape`/`unescape` | Legacy globals |

### Built-in modules

All support both `require('fs')` and `require('node:fs')`, and the corresponding
`import` forms:

```
assert, assert/strict        fs, fs/promises           path, path/posix, path/win32
buffer                       http, https, http2        perf_hooks
child_process                module                    process
console                      net, tls                  querystring
constants                    os                        readline, readline/promises
crypto                       repl                      stream, stream/promises
diagnostics_channel          string_decoder            timers, timers/promises
tty                          url                       util, util/types
v8                           vm                        zlib
```

Highlights:

* **`fs`** — sync API (`readFileSync`, `writeFileSync`, `readdirSync`, `statSync`, `mkdirSync`, `rmSync`, `mkdtempSync`, `watch`, `watchFile`, `createReadStream`/`createWriteStream`, ...), callback API and `fs.promises`. Async work runs on a thread pool and completes back on the event loop.
* **`stream`** — `Readable`/`Writable`/`Duplex`/`Transform`/`PassThrough`, `pipe`, `pipeline`, `finished`, object mode and `_read` subclasses (enough for `readdirp`/`chokidar`).
* **`http`/`https`** — client `get`/`request` and a real `createServer` with chunked bodies, `writeHead`, headers and default status codes (Express works).
* **`crypto`** — hashes, HMAC, random bytes, ciphers (AES CBC/CTR/GCM/..., chacha20-poly1305), PBKDF2/scrypt/HKDF, RSA/EC/Ed25519 key generation, sign/verify, `KeyObject` and PEM/DER export.
* **`child_process`** — `spawn` (streams + `exit`/`close` events), `exec`, `execFile`, and the sync variants.
* **`module`** — `createRequire`, `builtinModules`, `isBuiltin`, `_compile`, `_cache`, `_extensions`, `_resolveFilename`, `wrap`.

### Module resolution

```js
require('./lib/util');        // relative, adds .js/.json/index.js as needed
require('../package');        // package.json main / exports
require('lodash');            // node_modules walk
require('node:fs');           // builtin with node: prefix
require('./data.json');       // JSON module
```

Install dependencies with a real Node/npm first; QuickJS-Scala resolves and
executes whatever is in `node_modules`. It does not implement npm itself.

---

## 6. Worked examples

### 6.1 CommonJS, `fs`, `path`

```js
// files.js
const fs = require('node:fs');
const path = require('node:path');

const file = path.join(__dirname, 'out.txt');
fs.writeFileSync(file, 'hello from quickjs-scala');
console.log(fs.readFileSync(file, 'utf8'));
console.log(path.basename(__filename), typeof __dirname);
```

```bash
java -jar quickjs-runner.jar --node files.js
# hello from quickjs-scala
# files.js string
```

### 6.2 `async`/`await`, promises and timers

```js
// async.js
const fs = require('node:fs/promises');
const path = require('node:path');
const { setTimeout: sleep } = require('node:timers/promises');

(async () => {
  await fs.writeFile(path.join(__dirname, 'a.txt'), 'async body');
  const text = await fs.readFile('a.txt', 'utf8');
  await sleep(10);
  console.log('read:', text);
})();
```

### 6.3 HTTP server and client

`express`-style code and plain `http.createServer` both work:

```js
// server.js
const http = require('node:http');

const server = http.createServer((req, res) => {
  res.writeHead(200, { 'Content-Type': 'text/plain' });
  res.end('hello from quickjs-scala');
});

server.listen(0, '127.0.0.1', () => {
  const port = server.address().port;
  http.get(`http://127.0.0.1:${port}/`, (res) => {
    let body = '';
    res.on('data', (chunk) => (body += chunk));
    res.on('end', () => {
      console.log('status:', res.statusCode, 'body:', body);
      server.close();
    });
  });
});
```

`fetch` uses the same loop for `await` (in a module or async function):

```js
const res = await fetch('https://example.com');
console.log(res.status, (await res.text()).length);
```

### 6.4 Streams

`fs` read/write streams and `.pipe()` work end to end:

```js
// streams.js
const fs = require('node:fs');

fs.writeFileSync('src.txt', 'stream body');
const out = fs.createWriteStream('dst.txt');
out.on('finish', () => {
  console.log('dst =', fs.readFileSync('dst.txt', 'utf8'));
});
fs.createReadStream('src.txt').pipe(out);
```

Flowing reads (`on('data')`/`on('end')`) and piping through `PassThrough`
to `process.stdout` also work:

```js
const { PassThrough } = require('node:stream');
fs.createReadStream('src.txt').pipe(new PassThrough()).pipe(process.stdout);
```

See "Differences from real Node" for stream APIs that are not covered yet.

### 6.5 `child_process`

```js
const { execSync, exec } = require('node:child_process');

console.log(execSync('echo from-child').toString().trim());

exec('echo async-child', (err, stdout) => {
  if (err) throw err;
  console.log(stdout.trim());
});
```

### 6.6 `crypto`

```js
const crypto = require('node:crypto');
console.log(crypto.createHash('sha256').update('abc').digest('hex'));
// ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
```

### 6.7 ES modules and packages

```
my-app/
  package.json        { "type": "module" }
  index.js
  lib/greet.js
```

```js
// index.js
import { readFileSync } from 'node:fs';
import { greet } from './lib/greet.js';

console.log(greet(readFileSync('./name.txt', 'utf8').trim()));
console.log(import.meta.url);
```

```bash
java -jar quickjs-runner.jar --node index.js
```

`.mjs` files are always ESM. `import.meta.url` is a `file://` URL; relative
specifiers resolve against the importing file.

### 6.8 ts-node / custom extensions

```bash
java -jar quickjs-runner.jar --node -e "require('ts-node').register({ transpileOnly: true }); console.log(require('./hello.ts'))"
```

(Requires `ts-node` and `typescript` installed in `node_modules`.)
`require.extensions` is honored for user-registered extensions, and
extension-less resolution consults the registered keys. (`transpileOnly` avoids
the very slow full type-check pass.)

---

## 7. Embedding `NodeRuntime` in Scala

The runner is a thin wrapper around `quickjs.node.NodeRuntime`; you can use the
same API in tests or an application:

```scala
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.stdlib.{Console, Globals, JSON}
import quickjs.node.{NodeExit, NodeOptions, NodeRuntime}
import java.nio.file.Paths

val rt = JSRuntime()
given JSContext = JSContext(rt)
StdLib.initialize(summon[JSContext])
JSON.initialize()
Console.initialize()
Globals.initialize()

val node = NodeRuntime.install(
  NodeOptions(
    argv = Vector("node", "script.js", "--flag"),
    cwd = Paths.get(".").toAbsolutePath.normalize
  )
)
rt.setModuleLoader(node.loader)

try
  node.loader.runMainFile(Paths.get("script.js"))
  node.loop.run(summon[JSContext], propagateErrors = true)
catch
  case exit: NodeExit => println(s"process.exit(${exit.code})")

summon[JSContext].runMicrotasks()
```

`NodeRuntime.install` returns the runtime instance; `node.loader` is the
`NodeModuleLoader` (implements `require` and ESM loading), `node.loop` is the
`HostEventLoop`, and `node.state.exitCode` holds the requested exit code.
`runSource(source, filename)` executes an inline CommonJS string (what
`--node -e` uses).

---

## 8. Exit codes and the event loop

* After the top-level script finishes, the runner drains microtasks and then
  keeps the JVM alive until the event loop has no pending timers, I/O or child
  processes.
* `process.exit(n)` throws `NodeExit`, which the runner converts to a JVM exit
  status of `n` (after running `process.on('exit')` listeners). `process.exitCode = n`
  is applied when the script finishes naturally.
* An uncaught exception prints a formatted stack trace with the script name and
  exits `1`; syntax errors are reported with line/column context.

```js
// exit.js
console.log('before exit');
process.exit(3);
```

```bash
java -jar quickjs-runner.jar --node exit.js; echo "exit=$?"
# before exit
# exit=3
```

---

## 9. Differences from real Node

QuickJS-Scala is not a Node binary replacement; it is a JavaScript engine with a
compatibility layer. The engine itself is ES2024+ (see `docs/CONFORMANCE.md`),
and the host layer implements the APIs common scripts and packages need.

Known gaps/limitations:

* **No `npm`**: install dependencies with real Node first. Pre-published bundles
  and `node_modules` trees execute normally.
* **Not implemented**: `worker_threads`, `dgram`, `dns`, `stream/web`
  (`ReadableStream`/`WebSocket`), `ws`/socket.io-style upgrades, and
  `http.createServer` keep-alive/pipelining (connections close after the
  response).
* **`fetch` buffers the whole body** (no streaming `response.body`).
* **Streams are partial**: `fs` streams, `.pipe()`, `PassThrough` and the
  `pipe`/`pipeline` plumbing work, but `Readable.from(...)` and the
  constructor-option forms (`new Readable({ read })`, `new Transform({ transform })`)
  are not implemented, and custom `Writable` subclasses may not receive data.
  Prefer `fs.createReadStream`/`createWriteStream` with `.pipe()`.
* **Unhandled promise rejections are silently ignored** (real Node aborts by
  default). Always `.catch()` or wrap async work in `try`/`catch`; otherwise a
  failed `await` can exit with status `0` and no output.
* **`path.win32` aliases POSIX `path`**; on Windows-style inputs behavior may
  differ.
* **`crypto`** lacks JWK export and PKCS#1 PEM parsing (PKCS#8/SPKI PEM works).
* **`Intl`** is a pragmatic JDK/ICU-backed subset rather than full ICU data.
* **ts-node full type-check mode** works but compiling `lib.d.ts` through the
  interpreter is slow; use `transpileOnly: true` for speed.
* `process.version` reports a Node compatibility version (`v22.0.0` by
  default), not the engine version; `process.versions` lists `node`/`v8`/`uv`/`openssl`
  placeholders. Run `runner --version` for the QuickJS-Scala runner version.

Packages exercised end to end: chalk, dayjs, zod, lodash, ajv, shelljs, execa,
moment, underscore, fs-extra, yargs, axios, express, chokidar, mocha, concurrently,
jsonwebtoken, ts-node (transpile-only). Regular engine conformance is tracked in
`docs/CONFORMANCE.md`.

---

## 10. Troubleshooting

| Symptom | Likely cause / fix |
|---|---|
| `ReferenceError: require is not defined` | Missing `--node`. |
| `Cannot find module '...'` | Dependency not installed in the expected `node_modules`; install with real Node, or check the path casing. |
| Script exits immediately before async work finishes | Await the work (keep a promise/timer pending) — the loop exits when nothing is scheduled. `setTimeout(..., 0)` holds it open. |
| `process.exit()` inside top-level `await` | Supported (`NodeExit` bypasses the module loader wrapper); make sure `--node` is set. |
| Stack trace points at `<eval>` / runner internals | Use `--node` and a file path rather than `-e` for better source names; errors in CommonJS wrappers report the file. |
| `Failed to parse CommonJS module <path>` | The file uses syntax or a feature the parser does not know yet (e.g. a proposal not implemented). Try running it under real Node to isolate syntax vs. host API. |
| Slow start / high memory on big scripts | JVM startup is a few hundred ms; for repeated runs keep a REPL or embed `NodeRuntime`. Use `JAVA_OPTS`/`-Xmx` for large workloads. |

Useful diagnostics:

```bash
# Which version and flags does the runner expose?
java -jar quickjs-runner.jar --help
java -jar quickjs-runner.jar --version

# Verify a feature in isolation
java -jar quickjs-runner.jar --node -e "console.log(typeof require('node:fs').readFileSync)"
```

---

## See also

* `docs/README.md` — documentation index
* `AGENTS.md` — current Node compatibility status and engine architecture
* `stdlib/src/main/scala/quickjs/node/` — implementation of the Node modules
* `stdlib/src/test/scala/quickjs/node/NodeCompatTest.scala` — end-to-end tests
  covering resolution, cycles, `fs`, `Buffer`, `path`, ESM interop, HTTP,
  streams, crypto, child processes and more
