# QuickJS-Scala - Claude Code Reference

## Project Overview

QuickJS-Scala is a JavaScript engine written in Scala 3 for the JVM, inspired by the QuickJS C implementation. The goal is to create a production-grade JavaScript engine with full ES2024+ support. 

**Before implement a feature, check the original c version first, should follow similar approach**
**When fixing a bug but not sure about the approach, check the original quickjs c version for ideas.**
**When the problem is tricky, create test step by step to help investigate, when done. keep the test**

**Current Status**: Phase 3 - Substantial language support with most ES2024 features. 1,244 tests passing, 0 failures. 15 test262 smoke test suites. Full test262 sweep (aggregated from per-directory chunks, Sep 16 2026, after the Intl/regex/ESM/WeakMap round): 34,887/49,765 passing (89.0% of executed tests; 10,558 skipped by feature config; 403 failures, 3,869 errors, 48 timeouts). That is +357 passing over the previous 34,530/88.7% (errors 3,972 -> 3,869, timeouts 50 -> 48); the skip drop follows enabling `top-level-await` (+260 tests executed). 5 QuickJS C test files all passing. The Runner can execute ordinary scripts (including `.mjs` modules) — see "Scripting support" below.

**Scripting support (Sep 2026)** — running ordinary JS scripts with the `runner` project (`sbt runner/assembly` → `quickjs-runner.jar`):
- **console**: every `console.log` used to print the console object itself (the receiver was formatted). Receiver is now skipped, top-level strings print raw, and `error`/`warn` go to stderr; added `info`, `debug`, `dir`, `assert`, `trace`.
- **ES modules in the Runner**: `.mjs` files (or `--module`) are parsed/compiled as modules with `FileModuleLoader` rooted at the script's directory. `import`, dynamic `import()`, top-level `await`, and `import.meta.url` (now populated as a `file://` URL) work. `Runner` also sets `scriptArgs`.
- **Named capture groups**: `exec`/`match` results expose a real `groups` object (null prototype, per spec) and `indices.groups` with the `d` flag; `String.replace`/`replaceAll` support `$<name>` (including GetSubstitution rules for unmatched and unknown names). ES group names no longer need to start with a Latin letter — `__proto__`, `_`, Unicode identifiers and escaped surrogate pairs are renamed to JVM-safe names. Backreferences `\k<name>` follow. 21/25 `named-groups` tests.
- **RegExp compilation is cached on the JS object** (`__regexpData`) instead of rebuilding `java.util.regex.Pattern` on every `exec`; the ES grammar is validated for `new RegExp(...)` too (previously only regex literals were validated), which fixed invalid-group-name tests and speeds up regex-heavy scripts.
- **Array own-property assignment**: assigning to an array key that has an own writable data property but an inherited prototype accessor used to call the prototype setter; it now updates the own property (OrdinarySet 2.b).
- **Host globals** (`stdlib/Globals.scala`, installed by the Runner): `atob`/`btoa`, `TextEncoder`/`TextDecoder` (UTF-8, incl. `encodeInto`), `structuredClone` (cycles, plain objects/arrays, Date, RegExp with `lastIndex`, Map, Set, ArrayBuffer, TypedArrays with fresh view-sized buffers, DataView, boxed primitives; throws a `DataCloneError` for functions, symbols, proxies and WeakMap/WeakSet).
- **`URL` / `URLSearchParams`** (`stdlib/URLBuiltins.scala`): a WHATWG URL parser (special schemes and default ports, percent-encoding sets, UTS #46 IDNA via ICU, IPv4/IPv6, relative resolution, file URLs with Windows drive letters, opaque paths), all URL accessors and setters, `URL.canParse`/`URL.parse`, and a live `url.searchParams` (the cached object is re-parsed when `search`/`href` change, and mutations update the URL). `URLSearchParams` implements the form-urlencoded parser/serializer, `size`, `has`/`delete` with optional value, stable `sort`, iterators and `forEach`. `URLTest` runs Node-generated fixtures (379 parse cases, 62 setter cases) plus behavior tests; `scripts/generate-url-fixtures.js` regenerates the fixtures and `scripts/fuzz-url.js` differentially fuzzes the parser against `whatwg-url` through the assembled runner (21,000+ random cases pass).
- **Timers** (`stdlib/Timers.scala`, installed by the Runner): `setTimeout`/`setInterval`/`clearTimeout`/`clearInterval` with due-time ordering, microtask draining between callbacks, string bodies, and an event loop drained after the script.
- `ScriptingSupportTest` covers console routing, base64, text codecs, structuredClone, timers and a real ESM import chain.
- A pragmatic `Intl` is now available (`Segmenter`, `NumberFormat`, `DateTimeFormat`, `Collator`, `PluralRules`, `ListFormat`, `RelativeTimeFormat`, `DisplayNames`); `toLocaleString` still does not route through it. Still missing for Node-style scripts: locale-correct `toLocaleString` and full structured-clone coverage.

**Node.js compatibility mode (Sep 2026)** — `runner --node script.js` runs ordinary Node scripts (CommonJS by default, ESM for `.mjs`/`"type": "module"`):
- New `quickjs.node` package (`stdlib/src/main/scala/quickjs/node/`): `NodeRuntime` installs the globals/modules; `NodeModuleLoader` implements `require`, Node resolution (relative paths, extension/index lookup, `node_modules` walk, `package.json` `main`/`exports`/`type`, JSON modules, circular deps) and compiles CommonJS sources with Node's module wrapper (`exports, require, module, __filename, __dirname`; top-level `this === module.exports`, top-level `return` allowed).
- Built-ins: `process` (argv/env/cwd/chdir/exit/exitCode/nextTick/hrtime/stdout/stderr/`on('exit')`), `Buffer` (Uint8Array subclass with Node encodings and static helpers), `path`, `fs` (sync API, callback APIs and `fs.promises`), `timers`/`timers/promises`, `os`, `util` (`format`/`inspect`/`promisify`/`inherits`/`types`), `assert`, `events` (EventEmitter), `url` (+`fileURLToPath`/`pathToFileURL`), `crypto` (hashes, HMAC, random values, global WebCrypto `crypto`), `child_process` (`execSync`/`execFileSync`/`spawnSync`), `querystring`, `buffer`, `zlib` (sync/callback/promises), `perf_hooks`; globals `fetch`, `Headers`, `Request`, `Response`, `AbortController`/`AbortSignal`.
- ESM interop: `import fs from 'node:fs'`, named imports and `import * as ns` work for built-ins and CommonJS files (namespace wrapping with `default` + enumerable named exports); `require()` of ESM returns the namespace object; dynamic `import()` of `.mjs`/CJS/JSON works.
- `NodeCompatTest` (26 tests) covers resolution, cycles, `fs`, Buffer, path, process/assert/events/util/url, ESM interop, package `type`, `process.exit`, crypto/child_process/querystring, top-level await, and includes a differential test against the real `node` binary when it is on `PATH` (skipped otherwise).
- `http`/`https` provide client `get`/`request` with `response`/`data`/`end` events; `fetch` uses `java.net.http.HttpClient` with redirects, AbortSignal cancellation and buffered bodies.
- Known differences/limits: `readline`, `worker_threads`, `vm`, `dgram` and `http.createServer` are not implemented; `path.win32` aliases POSIX; `fetch` buffers the whole body (no streaming `response.body`); `Intl` is a pragmatic JDK/ICU- backed subset (no `%Intl%`-style locale data beyond the JVM's).
- Async host layer: `HostEventLoop` owns timers, `setImmediate`, microtasks and a thread-safe completion queue; blocking operations run on a daemon pool and post completions back to the loop thread (`retain`/`release` keep the loop alive for in-flight HTTP/fs work). `fs` callback APIs, `fs.promises`, `zlib` async APIs and HTTP requests run for real asynchronously; `require('timers')`/`require('timers/promises')` are loop-backed.

**Promise resolution / thenable adoption (Sep 2026)** — `PromiseBuiltins.promiseResolve` fulfilled promises with whatever `then` callbacks returned, so `.then(() => Promise.resolve(2))` rejected with the inner promise and generic thenables were not assimilated. It now implements the ES ResolvePromise procedure: adopting a promise registers reactions on it (or recurses through already-fulfilled wrappers), generic thenables get a queued job that calls `then(resolve, reject)` with a once-guard, self-resolution rejects with TypeError, and plain values still settle synchronously (`Promise.resolve(1)` stays fulfilled). Fixed `fs.promises` chaining (`readFile().then(...)`) and all promise-chain code; regression tests in `ConformanceRegressionTest`.

**Async/await suspension (Sep 2026)** — the async VM was a stub: `resumeAsyncFunction` was a placeholder and `await` unwrapped only already-settled promises, so `await <pending>` silently produced the promise object and never resumed. The main interpreter now suspends:
- A new `AwaitAsync` opcode (async functions and module bodies, code 94) always suspends; the top-level `Await` opcode keeps the historical synchronous unwrap for non-async legacy contexts.
- `BytecodeLoop` throws `AwaitPending`, `run` converts it to `AsyncSuspension` carrying the live `Frame`/function/trace, and `Interpreter.call` adopts the awaited value with `PromiseBuiltins.promiseResolve(...).then(...)`. On settlement `Interpreter.resumeAsync` re-enters a `BytecodeLoop` over the same frame: `resumeWithValue` pushes the result at the saved pc, `resumeWithThrow` routes the rejection through the normal try-handler dispatch so `try`/`catch`/`finally` and `with` scopes work across suspensions. Returns settle the async promise through the ES ResolvePromise procedure.
- `findFreeVariablesForClosure`/`findFreeVariables` now traverse `AwaitExpression`, `YieldExpression` and `TemplateLiteral`, so a global captured only inside `await`/`yield`/template is captured (previously `await p` for a top-level `const p` threw `ReferenceError`).
- Microtask ordering now matches V8: `await` of settled promises still yields a microtask, and `await fs.promises.*`/`await fetch`-style code resumes in order. Regression tests in `ConformanceRegressionTest`.

**Top-level await (Sep 2026)** — module bodies are compiled async and driven to settlement, so `.mjs` entry points and imported modules can `await` pending promises (timers, `fs.promises`, `fetch`, nested TLA imports):
- `Compiler.compileModule` sets `currentFunctionIsAsync = true` while compiling and returns the body via `BytecodeFunction.withAsync(true)`; top-level `await` therefore emits `AwaitAsync` and suspends instead of unwrapping.
- `JSRuntime.setHostAwaitDriver` lets embedders pump host work: `NodeRuntime` installs `HostEventLoop.runUntil(ctx, until)` and the standalone Runner installs `Timers.runUntil(state, until)` so timer/async-I/O completions make progress while a module waits.
- `quickjs.module.ModuleEvaluation.settleAndCheck` drives the module evaluation promise (`ctx.runMicrotasks()` + the host driver) and rethrows rejections. `FileModuleLoader.loadModule`, the in-memory loader path, `Runner.execute`, and the test262 runner all use it, so imported modules finish evaluating before their namespace is exposed. If no host work remains and the promise is still pending, evaluation returns with partial exports (the process then exits like Node).
- `NodeExit` is now a `ControlThrowable`, so `process.exit()` inside a TLA wait is not wrapped as a module-load error.
- `for await` now creates its iterator with `__createAsyncIterator`: it prefers `Symbol.asyncIterator`, falls back to a sync iterator (arrays, strings, sync iterables) whose results the compiler still awaits, and accepts direct `next`-bearing iterators (async generators). Previously `for await` only worked over objects that already exposed `next`, so `for await (const x of [1, 2, 3])` silently iterated zero times in every context.
- `top-level-await` is enabled in `test262.conf`: `language/module-code/top-level-await` passes 262/287 (91.3%) — the remaining failures are parser early-error propagation and a few test262-runner fixture artifacts. Regression tests in `NodeCompatTest`, `ScriptingSupportTest`, and `ESModuleTest`.

**npm package compatibility (Sep 2026)** — probing popular packages (prettier, commander, lodash, zod, ajv, glob, rimraf, chalk, shelljs, minimist, semver, uuid, ...) showed the blockers were engine/parser bugs, not missing host APIs. Fixed, with regression tests in `ConformanceRegressionTest`/`NodeCompatTest`:
- **`return` before `}`** without a semicolon or newline (`{ return }`, ubiquitous in minified code) failed to parse; `parseReturnStatement` now treats `}` as a terminator.
- **Ternary branches used the comma-including parser**, so `{ f: () => a ? b : c, g: 2 }` swallowed the comma and broke object literals (prettier's bundle). Both branches now use the comma-free AssignmentExpression grammar.
- **Arrow block bodies did not hoist function declarations** (`it.f = ei; function ei() {}` inside `x => { ... }` threw ReferenceError; glob's minified bundle). They now get the same declaration pre-pass as `compileFunctionBody`.
- **`Scope` used nesting depth as block identity**, so sibling blocks collided: a `const x` in one branch made a `let x` in a later sibling look const (commander) or leaked across blocks (zod). `Scope` now assigns unique block ids, tracks the active block chain for visibility, keeps `var`/temps function-scoped, and resolves `const`/`lexical`/`lookup` to the innermost visible binding.
- **RegExp ES→Java translation**: literal `[` inside character classes is escaped (`[^[\]]`), literal braces become `\x7B`/`\x7D` (Java rejects `\{`), and `\f \n \r \t \v` are recognized as CharacterEscapes under the `u` flag. These unblocked lodash, shelljs and ajv regexes.
- **`class X extends EventEmitter { super() }`**: `EventEmitter` now has `superInitImpl`/`.call(obj)` support initializing the existing receiver.
- **`#subpath` package imports** resolve through the nearest `package.json` `imports` map (chalk); legacy `escape`/`unescape` globals are installed.
- CommonJS parse failures now name the offending file (`Failed to parse CommonJS module <path>: ...`).
- **Closure capture of block-scoped bindings**: the function-body pre-pass declared every collected name at block 0, so a later `const`/`let` in a block got a second slot and `localVarNames.indexOf(name)` pointed at the empty pre-pass slot. `Scope.declare` now reclassifies that slot for the real lexical declaration, and `getAllLocalVarNames` names the most recent declaration so closures in later sibling blocks capture the right slot. This fixed zod's `checks is not defined`.
-  **Slot-aware closure capture** (`BytecodeFunction.freeVarSlots`): free variables that resolve to the immediate parent's locals are captured by slot index, not by name. This fixes named class expressions sharing an inner name (`const A = class u {}; const B = class u {}`), sibling block shadowing, and paves over the name-collision weakness of `localVarNames`. Named class expressions now get a fresh compile-time block scope.
- **Derived-class initialization**: public/private field initializers of a derived class now run **after** the user's `super()` call (they were prepended to the constructor body, so `class D extends B { sep = '/' }` threw "Must call super…"); static private fields and private assignment expressions now yield the assigned value (path-scurry's `fullpath()`).
- **Free variables in parameter defaults** are now captured (`function f({fs = mt} = {})` inside an IIFE used to throw `mt is not defined`).
- **Optional calls** (`obj.x?.()`, `obj?.x?.()`, `this.#s?.x?.()`) now short-circuit on nullish receivers/callees without evaluating arguments.
- **`String.prototype.normalize`** (NFC/NFD/NFKC/NFKD, lone surrogates preserved) and regexp `\0` translation (`\x00`) added.
- **fs path APIs strip the receiver by argument shape** (first arg is a string/Buffer/URL/fd), so packages that copy `fs` methods onto their own object (path-scurry, graceful-fs) work; `globSync` and `rimrafSync` now run end-to-end.
- Remaining package blockers: zod (`def.fn` is undefined in `$ZodCustom.check` — an object/field propagation issue), chalk and ajv runtime paths ("Cannot access binding before initialization", a TDZ interaction with same-named sibling slots that name-based capture still gets wrong in indirect-capture chains), shelljs ("Cannot destructure null or undefined").
- **Host modules added**: `stream` (EventEmitter-based `Readable`/`Writable`/`Duplex`/`Transform`/`PassThrough`, `pipe`/`pipeline`/`finished`/`stream.promises.pipeline`), `fs.createReadStream`/`createWriteStream`, `tty` (`isatty`, `ReadStream`/`WriteStream`), `string_decoder` (`StringDecoder`), `diagnostics_channel` (subscription-free channels and tracing channels).
- Still blocked: packages needing `stream` (minipass, shelljs), the `tty` module (chalk), and two runtime bugs surfaced on load: lodash (`ToPrimitive`: "Cannot convert object to primitive value") and zod (`checks is not defined`, a top-level binding/hoisting issue).

**Parser compatibility + primitive receivers (Sep 2026)** — probing top npm packages after the TLA work found three small parser gaps and several engine bugs in primitive property lookup:
- **Contextual keywords as function names**: the statement dispatcher only treated `function <IdentifierToken>` as a declaration, so `function from(time) {}` (moment) mis-parsed as a function expression. `function from/as/of/get/set/async` and `function *of` now parse as declarations (reserved words are still rejected by `validateBindingIdentifier`), and named function expressions accept them too.
- **`async(...)` call vs async arrow**: `asynckit` (axios dependency) calls a function named `async`, e.g. `async(callback)`. The expression parser committed to an async arrow on seeing `async (`. It now scans to the matching `)` and only takes the arrow path when `=>` follows with no line terminator; otherwise `async` is an identifier and the call parses. Regression tests in `ParserTest` and `ConformanceRegressionTest`.
- **Arbitrary module namespace names** (ES2022): `export { x as 'module.exports' }` and `import { 'name' as x }` are parsed (`ModuleExportName = IdentifierName | StringLiteral`) and the compiler uses the string names for `__moduleExport`/imports (yargs). The `arbitrary-module-namespace-names` test262 feature remains skipped (8/16 — mostly namespace-object gaps); yargs itself now runs (see round 3 below).
- **Symbol-keyed access on primitives auto-boxes**: `''[Symbol.iterator]()` returned `undefined` because `doGetElem`'s symbol case had no primitive receivers; it now resolves through `String`/`Number`/`Boolean`/`BigInt` prototypes with the primitive as receiver. This unblocked `get-intrinsic`/`object.getprototypeof`-style packages (axios dependency).
- **Primitive `GetProp` invokes accessors**: the `String`/`Number`/`BigInt`/`Boolean` paths in `resolveGetProp` used `JSObject.get` (raw values), so `''.__proto__` skipped the accessor. They now use `getPropertyValue`, which invokes getters with the primitive receiver.
- **`__proto__` accessor**: the getter/setter only accepted `JSObject`, so `[].__proto__ === Array.prototype` (dunder-proto/get) threw. They now use the shared `prototypeValueOf`/`setPrototypeOnTargetValue` helpers, covering arrays, functions, natives and primitives.
- **Extracted `Reflect.apply`/`Reflect.construct`**: the impls stripped the first argument unconditionally, so `const apply = Reflect.apply; apply(fn, null, args)` lost `fn` (call-bind-apply-helpers). They now strip the receiver only when it is the Reflect object.
- **`console.log` of a Promise/Generator** threw a `MatchError` in `PrettyPrinter.format`; it now prints `Promise { <pending>|value|<rejected> ... }` and `[Generator]`.
- Package probes: **moment** now works end to end; **asynckit** loads; **axios** gets past parsing and the `__proto__`/string-iterator/Reflect bugs but still fails in `get-intrinsic` (`head of empty array`); **yargs** parses but needs `Intl`.

**Scope and per-iteration bindings (Sep 2026)** — the two long-standing scope bugs behind chalk, dayjs and zod are fixed:
- **Function declarations are local bindings, not globals.** `findDeclaredVariables` did not collect `FunctionDeclaration` names and the `DefFun` opcode unconditionally wrote to `ctx.global`, so every `function f() {}` inside a function body leaked to `globalThis` and an inner `function M` often resolved to an outer `var M` (dayjs failed with `Cannot create property 'parse' on primitive`). `findDeclaredVariables` now includes function declarations, and `compileStatement(FunctionDeclaration)` stores into the local slot (`declared` + `putLoc`) for function/module/direct-eval bodies; only script-top-level declarations still use `DefFun`. This also fixes recursion/self-shadowing (`function M` inside a function named `M`) and matches Node's CommonJS scoping (top-level functions are module-scoped, not global).
- **Per-iteration `let`/`const` loop bindings.** A new `CloneLocRef` opcode (code 95) replaces a local slot's `VarRef` with a fresh one (preserving const/function-name/eval flags). The compiler emits it at the `for` continue point (before the update) and before each `for-in`/`for-of`/`for-await-of` binding assignment, but only when the loop body references one of the bound names or contains direct eval, so plain loops avoid the allocation. Closures created in one iteration keep the binding they captured while the next iteration gets a fresh one: `for (const name of ['a','b','c']) fns.push(() => name)` now yields `a,b,c` and `for (let i = 0; i < 3; i++)` yields `0,1,2` (also for destructuring, nested loops, `continue`/`break`, and eval inside nested functions). `GeneratorSupport` handles the opcode too.
- **`Array(n)` without `new`** returned an empty array because `callImpl` stripped the first argument as a receiver; it now uses the real arguments, so `Array(2).length === 2` (dayjs's zero-padding depended on this).
- Package probes after the fixes: **chalk**, **dayjs**, **zod**, **fs-extra**, **lodash**, **ajv**, **shelljs**, **execa** (v5 and v9), moment, underscore and asynckit now load and run. (The remaining blockers at this point — axios's `net` and yargs' `Intl` — were both removed in round 3.)

**Node ecosystem round 8 (Sep 2026)** — `http.createServer` (express now runs end to end):
- **HTTP server** (`NodeHttp`): `http.createServer`/`https.createServer` and `http.Server` build on the net/tls server (`net.createServer` + a `connection` listener). Each connection parses HTTP/1.1 request lines and headers, exposes `IncomingMessage` (`method`, `url`, `headers` lowercased, `rawHeaders`, `socket`, and `data`/`end` events) and `ServerResponse` (`statusCode`, `statusMessage`, `setHeader`/`getHeader`/`getHeaders`/`getHeaderNames`/`hasHeader`/`removeHeader`, `writeHead`, `write`, `end`, `flushHeaders`, `headersSent`/`writableEnded`/`finished`). Bodies support `Content-Length` and chunked transfer-encoding; responses use `Content-Length` on a single `end`, chunked framing after `write`, `Date`/`Connection: close`, and default `STATUS_CODES`. `http.IncomingMessage`, `http.ServerResponse`, `http.OutgoingMessage` and `http.Server` constructors exist (express needs `http.IncomingMessage.prototype`). `net.Server` gained `emit`/`removeListener`/`off`/`removeAllListeners`/`listenerCount`.
- **Engine bugs fixed en route**:
  - **Function-valued prototypes**: `Foo.prototype = function () {}` (router) was not used as the receiver prototype by `new` and not recognised by `instanceof`, so `Router`'s `if (!(this instanceof Router)) return new Router()` recursed until `StackOverflowError`. `constructValue`/`prototypeObjectOf` and `doInstanceof` now unwrap function/native prototypes.
  - **Destructuring defaults in variable declarations** (`const { d = D } = o` inside a function) were missing from `findFreeVariables`/`findFreeVariablesForClosure`, so `path-to-regexp`'s `DEFAULT_DELIMITER` resolved to a global (`ReferenceError`). Pattern defaults/computed keys now contribute free variables; `containsDirectEval` traverses patterns too.
  - **EventEmitter mixing**: methods copied onto a plain object or function (express's `mixin(app, EventEmitter.prototype)`) now lazily initialise the listener store, and `storeOf`/`initializeEmitter` handle function receivers.
- **Verification**: `express@5.2.1` loads, `app.get(...)` + `app.listen(...)` serve requests (`EXPRESS OK 200 'hello'`), including route matching via `path-to-regexp`. concurrently, chokidar, mocha, yargs, axios, execa unaffected. Four new regression tests (`NodeCompatTest` http server + emitter mixing, `ConformanceRegressionTest` function-valued prototypes + destructuring defaults).

**Node ecosystem round 7 (Sep 2026)** — `readline`, `repl`, `vm` and a real `process.stdin` (ts-node loads):
- **`process.stdin`** is now a real Readable over `System.in`: reading starts lazily on `on('data')`/`addListener`/`resume`/`read` and the event loop is retained until EOF, so scripts that wait on piped stdin stay alive. `setRawMode`/`ref`/`unref` included. Host stream prototypes must exist before process streams are built, so `NodeStream.create()` now runs before `NodeProcess.create` in `NodeRuntime`.
- **`readline`** (`NodeReadline`): `createInterface` with `line`/`close`/`pause`/`resume`/`history`/`error` events; `question` (callback), `prompt`/`setPrompt`/`getPrompt`, `pause`/`resume`/`close`/`write`, a dynamic `history` getter, `terminal`, `completer`, and `[Symbol.asyncIterator]` (with a pending-line queue so lines that arrive between `next()` calls are not lost). Also `cursorTo`/`moveCursor`/`clearLine`/`clearScreenDown`/`emitKeypressEvents`.
- **`readline/promises`**: promise-returning `question` and the same async iterator.
- **`repl`**: `start`/`REPLServer`, evaluating each line with the global `eval` (called as a native so the source is its first argument), Node-style inspection of results, `.help`/`.break`/`.clear`/`.exit`, `defineCommand`, `context`/`useGlobal`, an `exit` event, and a hidden `__rl` hook for tests.
- **`vm`**: `runInThisContext`/`runInContext`/`runInNewContext`, `new vm.Script(...)` with the same run methods plus `createCachedData`, `createContext`/`isContext`, `compileFunction` and `constants`. Contexts share the realm global (enough for ts-node's REPL).
- **`require('console')`** returns the global console object (ts-node requires it).
- **Verification**: piped-stdin scripts cover line events, `question`, `for await`, promises and TTY-less REPL input; `repl.start` prints `6` for `2 * 3` and exits; **ts-node now loads** (`typeof require('ts-node').register === 'function'`); concurrently/chokidar/mocha/yargs/axios unaffected. Three new `NodeCompatTest` tests (readline, readline/promises + vm, repl); full suite now 1,242 tests.

**Node ecosystem round 6 (Sep 2026)** — async `child_process` (concurrently now runs):
- **`spawn`/`exec`/`execFile`** (`NodeChildProcess.create(state, loop, streams)`): `spawn` starts a JVM process and returns a `ChildProcess` emitter with `pid`/`spawnfile`/`spawnargs`/`killed`/`exitCode`/`signalCode`, `stdin` (writes go to the process pipe), `stdout`/`stderr` and `kill(signal)` (`SIGKILL` uses `destroyForcibly`). Reader threads push chunks into real `NodeStream` Readable objects (new public `newReadable`/`newWritable`/`pushToStream`/`endStream`/`emitStream` API), `spawn`/`error`/`exit`/`close` fire in Node's order, and `HostEventLoop.execute` keeps the loop alive until the child exits and both readers finish. `exec`/`execFile` buffer `stdout`/`stderr` (default `utf8`, `encoding: 'buffer'` supported), honor `cwd`/`env`/`timeout`, and call back with `code`/`status`/`signal`/`killed`/`cmd`/`stdout`/`stderr`; `spawn('nope')` produces the `ENOENT` error event.
- **Verification**: `concurrently(['echo one','echo two'])` streams both lines and resolves `CONCURRENTLY OK 2`; `execa('echo', ['async-exec'])` resolves asynchronously; chokidar still reaches `ready`; mocha still runs suites; yargs/axios/lodash/ajv/shelljs unaffected. Two new `NodeCompatTest` tests cover streamed output, `exec`/`execFile`, `ENOENT`, stdin piping and `kill`. `net.Server.close()` also no longer races the accept loop into a spurious `EADDRINUSE listen failed` error.

**Node ecosystem round 5 (Sep 2026)** — mocha now runs tests end to end:
- **CJS accessor exports**: `cjsNamespace` copied exports from `getOwnPropertyDescriptor`, so TypeScript-compiled packages (`Object.defineProperty(exports, "map", { enumerable: true, get })`, ubiquitous in `rxjs/operators`) produced namespaces whose named exports were all `undefined`. The namespace builder now reads through `[[Get]]` (`BuiltinHelpers.getPropertyWithGetter`), so `import { map } from 'rxjs/operators'` works. This was concurrently's real blocker.
- **`require(esm)` CJS re-export facades**: cargo-culted ESM wrappers like `export { default } from './index.cjs'; export * from './index.cjs'` (mocha's `index.js`) now return the CJS `module.exports` when at least one named export mirrors a property of `default` (`NodeModuleLoader.cjsReexportDefault`), matching Node; default-only ESM modules still return the namespace object. `require('mocha')` is the Mocha class again and mocha runs suites (`2 passing`).
- **Method calls of native constructors ignore `this`**: `obj.String = String; obj.String('x')` used to call `NativeConstructor.superInitImpl` and box the receiver (`{0:'x', ...}`). `BytecodeLoop`'s two `callValue`/`CallMethod` sites now use `constructor.call(args)`; only an explicit `super()` initializes an existing receiver, so non-spread `super(...)` now routes through `__funcSpread` (the old `CallMethod` path relied on `callWithThis`).
- **Copied timer globals**: `Runner.immediately = global.setImmediate` scheduled the receiver class as the callback. `setTimeout`/`setInterval`/`setImmediate`/`clear*` now ignore a leading receiver that cannot be a callback/id (class constructors excluded).
- **`process` is a real EventEmitter** (general listener map, working `emit`, `once` wrappers, `addListener`/`off`/`removeListener`/`removeAllListeners`/`listenerCount`), and `process.stdout`/`stderr` expose the full emitter surface (`addListener`/`removeListener`/`emit`/…), which rxjs's `fromEvent` requires (`Invalid event target` fixed).
- **`console.*` now uses `util.format` semantics** (`%s`/`%d`/`%i`/`%f`/`%j`/`%o`/`%O`/`%c`, Node-style object inspection) and ignores receivers copied onto other objects (`Base.consoleLog = console.log`), so mocha's reporter output is correct (`✔ passes`, `2 passing (21ms)`).
- **Next blockers**: concurrently now needs async `child_process.spawn`; express fails reading `.prototype` in its dependency tree; ts-node needs `readline`/`repl`.
- Regression tests: `ConformanceRegressionTest` (native constructor as method), `ScriptingSupportTest` (console formatting/copied receiver), `NodeCompatTest` (CJS accessor exports, re-export facade, copied timers, `super()` with a native superclass).

**Node ecosystem round 4 (Sep 2026)** — the first Tier-1 blocker round for real packages (chokidar/mocha/express):
- **ESM namespace receivers** (`NodeHelpers.registerReceiverAlias`): `import * as fs from 'node:fs'; fs.readFileSync(...)` passes the ESM **namespace object** as the native receiver, but builtin methods strip only the canonical module object, so `path.normalize('/a//b')` in chokidar threw `Cannot convert object to primitive value`. `builtinNamespace`/`cjsNamespace` now register each namespace as an alias of its module object in an identity map consulted by `stripReceiver`. chokidar now loads and `chokidar.watch(...)` reaches `ready`.
- **RegExp character-class ranges**: the validator rejected any single-character escape after a `-` in `u` mode, so the ubiquitous `[a-zA-Z0-9_\u{AD}\u{C0}-\u{D6}...]` (ansi/glob-style classes) failed to parse. Only *character-class escapes* (`\d`, `\p{...}`, …) are invalid range endpoints; the check now requires `escaped == true`. `\u{...}` escapes inside character classes are also translated to Java's `\x{...}` form (the in-class escape branch previously copied them verbatim and `Pattern` rejected `\u{...}`).
- **V8 stack introspection** (`Error`): `Error.stackTraceLimit` (default 10), `Error.prepareStackTrace`, `Error.captureStackTrace(target[, ctorOpt])` and CallSite objects with `getFileName`/`getLineNumber`/`getColumnNumber`/`getFunctionName`/`getThis`/`getTypeName`/`getMethodName`/`isEval`/`isNative`/`toString` (depd, source-map-support, get-caller-file; express now gets past `depd`). Errors install a lazy own `stack` accessor backed by captured frames (`JSContext.CapturedFrame`, `captureFrames`/`formatFrames`/`installLazyStack`); the runtime registers the accessor factory and the `prepareStackTrace` evaluator via `setStackGetterFactory`/`setStackFormatter` (core cannot build native functions itself). `Interpreter.withNativeFrame` refreshes the captured frames while unwinding, and JSON/eval parse errors install `<json>`/`<eval>` frames through the same path.
- **Line terminators no longer break expressions**: `('p'\n + 'x')` (Babel/TS output) and `f\n(x)` are valid continuations per the grammar; the additive loop and call `(` previously applied ASI heuristics. Arrow functions are the exception (`() => {}\n() => {}` stays two statements, since an arrow is only an AssignmentExpression).
- **Block comments advance line/column tracking** (`Lexer.advance` now updates line/column for `\n`/`\r`, so `/* multi-line */` no longer desyncs line numbers in diagnostics).
- **Package probes**: chokidar works end to end; mocha now parses through `diff` (next blocker: `Cannot use 'new' with non-constructor`); express gets past `depd` (next: `Cannot read properties of undefined (reading 'prototype')`); concurrently's next blocker is a bare `map` resolved as `global`; ts-node needs `repl`. Regression tests in `ParserTest`, `LexerTest`, `ConformanceRegressionTest` and `NodeCompatTest`.

**Node ecosystem round 3 (Sep 2026)** — yargs and axios now run end to end:
- **`Intl`** (`runtime/.../builtins/IntlBuiltins.scala`): `Intl.Segmenter` (grapheme/word/sentence via `java.text.BreakIterator`, real `{segment, index, input}` iterables), `NumberFormat` (decimal/percent/currency, fraction/integer digit options), `DateTimeFormat` (date/time styles and explicit field options, locale field order via ICU `DateTimePatternGenerator`), `Collator`, `PluralRules` (ICU), `ListFormat`/`RelativeTimeFormat`/`DisplayNames` (ICU), plus `getCanonicalLocales`/`supportedLocalesOf`/`supportedValuesOf`. Registered in `StdLib.initialize` as the global `Intl`. This unblocked `cliui` → `string-width` → yargs.
- **Unicode property escapes**: Java's `Pattern` spells binary properties `IsXxx`, has no `Default_Ignorable_Code_Point`/`Any`/`ID_*`/`Math`, and rejects ES long general-category names. `BuiltinHelpers.translateUnicodePropertyEscapes` now rewrites every `\p{...}`/`\P{...}`: bare names/`gc=` values map through a general-category alias table, `Script=`/`scx=` become `\p{Is<Script>}`, binary properties get the `Is` prefix, `Default_Ignorable_Code_Point` becomes an explicit code-point class, `ID_Start`/`ID_Continue`/`Math` are approximated, and the `v`-flag “properties of strings” emoji sequences (`RGI_Emoji`, `Basic_Emoji`, keycap/flag/tag/ZWJ/modifier variants) expand to non-capturing groups built from `\p{IsEmoji}`/`IsEmoji_Modifier`/regional-indicator classes. The `{`-escaping pass also learned not to mangle `\x{...}` escapes.
- **`export default function f(){}` / `export default class C{}` create a local binding** (the parser parses them as `FunctionExpression`/`ClassExpression`), so a later `export { f as ... }` resolved to a global and threw `ReferenceError` (`cliui/index.mjs`). The compiler now declares the name in module scope and re-exports it.
- **Module exports are live**: `export var X; (function (X) { X.A = 1 })(X || (X = {}))` (TypeScript enums, `yargs-parser-types.js`) exported `undefined` because `__moduleExport` captured the declaration-time value. `compileScript` now re-exports every top-level `export`ed binding (variable declarations and `export { a as b }` specifiers) after the module body finishes evaluating.
- **`node:module` builtin**: `createRequire(pathOrFileURL)`/`createRequireFromPath`, `builtinModules`, `isBuiltin` (`NodeModuleLoader.createModuleBuiltin`). `require()` of an ESM module now returns the module's `'module.exports'` export when present (Node's CJS interop marker), so `cliui`'s `export { ui as 'module.exports' }` yields the function itself.
- **`Function.prototype.bind` of non-constructable functions** returned a `NativeFunction`, whose method-call path prepends the receiver as the first argument, so a bound class method received the receiver object as its first real parameter (`y18n.setLocale` set `locale` to the y18n instance). Bound functions are now always `NativeConstructor`s: `callImpl` receives plain arguments, `constructImpl` still forwards to `constructBound` for constructable targets, and throws `TypeError` for non-constructable ones (so `new (boundClassMethod)` still throws).
- **`fn.apply(fn, args)` / `fn.call(fn, ...)`** on an extracted standalone native function (`shim.format.apply(shim.format, [...])`) prepended the function again, so `util.format` printed the function itself. Self-application now passes the arguments through unchanged.
- **Class computed element keys are evaluated exactly once**: `computedKeyTemp` compared the `ComputedPropertyName` node held in `computedClassKeys` against the *inner* expression passed by `emitPropertyKey`, never matched, and re-evaluated the key (side effects ran twice, each creating fresh `WeakMap`s). Both sides are now unwrapped before the identity comparison.
- **WeakMap/WeakSet storage**: `java.util.WeakHashMap[WeakObjectKey, _]` where `WeakObjectKey` strongly references the key object was *prematurely evicting entries* — the JVM could collect the wrapper (which the map holds only weakly) together with its entry even though the key object was still alive (`WeakMap.has` went false after GC, breaking yargs' private-field state). Both storages now use an `IdentityHashMap` keyed by the stable underlying object (`JSObject`/`JSArray`/`funcObj`), so entries persist for as long as the WeakMap.
- **Package probes**: yargs 18 (`yargs(['--verbose','-n','alice']).option(...).parse()`, `.getHelp()`, `.command(...).parse()`), axios (local HTTP server round-trip), zod, lodash, ajv, shelljs, execa, moment/dayjs, fs-extra, chalk and the rest of the battery run. Regression tests added in `ConformanceRegressionTest` (Intl, property escapes, computed class keys, bound class methods, apply/call self, WeakMap + `System.gc()`) and `NodeCompatTest` (`module.createRequire`, `require(esm)` `module.exports`, ESM default-function binding, live TS-enum exports, `util.format.apply`).

**Node ecosystem round 2 (Sep 2026)** — ajv, shelljs and execa were fixed by another batch of engine and host-API fixes:
- **`super` property accessors** bind `this` to the superclass prototype instead of the receiver, so a getter read (`super.names`, ajv's `If.get names`) got the wrong object. `compileExpression` now compiles `MemberExpression(SuperExpression(...))` through a new `__getSuperProp(proto, key, receiver)` helper that invokes accessors with the current `this` (method calls already passed the receiver). Static-method `super.m()` keeps the old path.
- **`export async function`** was rejected by the parser (`Unsupported export declaration`, unicorn-magic); and **`export * as ns from '...'`** (ES2020 namespace re-export) was unsupported. `ExportAllDeclaration` carries an optional namespace name and the compiler exports the namespace object.
- **ES2025 Set methods**: `union`, `intersection`, `difference`, `symmetricDifference`, `isSubsetOf`, `isSupersetOf`, `isDisjointFrom` (execa used `Set.prototype.union`).
- **Async generator function intrinsics**: `%AsyncGeneratorFunction.prototype%` and `%AsyncGeneratorPrototype%` now exist and are the `[[Prototype]]` of async generator functions, so `Object.getPrototypeOf(Object.getPrototypeOf(async function*(){}).prototype)` works (`@sec-ant/readable-stream`).
- **Logical assignment to private fields** (`this.#x ??= v`) now compiles (was `Unsupported logical assignment property key`).
- **Host APIs added**: `os.constants` (signals/errno/priority/dlopen, Linux values), `util.debuglog`/`stripVTControlCharacters`/`callbackify`/`aborted`, `stream.getDefaultHighWaterMark`/`setDefaultHighWaterMark`/`isErrored`/`isDisturbed`/`duplexPair`, `stream/promises` registration, a minimal `node:v8` (`serialize`/`deserialize` JSON round-trip, `isUtf8`, heap stubs).
- **`child_process.spawnSync`** now returns Node's `output: [null, stdout, stderr]` array and supports `encoding: 'buffer'` (returns Buffers); `process.execPath` prefers a real `node` binary on `PATH` so shelljs's `exec` helper works. Errors from reads on nullish values now include the property name (`Cannot read properties of undefined (reading 'x')`).
- **Computed string-key access on primitives** (Sep 2026): `"abc"["toUpperCase"]` / `(5)["toFixed"]` returned `undefined` because `doGetElem` had no primitive-receiver string-key case, so closures over a parameter used as a computed key (lodash's `createCaseFirst`: `chr[methodName]()`) failed with `lastLookup=methodName, kind=global` and `camelCase` broke. `doGetElem` now auto-boxes strings/numbers/booleans/bigints through their prototypes for string keys.
- **Extracted Reflect statics** (Sep 2026): `Reflect.getPrototypeOf`/`ownKeys`/`isExtensible`/`get`/`set`/`has`/`deleteProperty`/`defineProperty`/`getOwnPropertyDescriptor`/`setPrototypeOf`/`preventExtensions` stripped `args(0)` unconditionally, so an extracted `const gpo = Reflect.getPrototypeOf; gpo(x)` threw `NoSuchElementException: head of empty array` (`get-intrinsic`'s `get-proto`). `ReflectBuiltins` now normalizes with a `reflectArgs` helper (strip only when the first argument is the Reflect object) and checks arity after normalization. Axios advanced past `get-intrinsic` and is now blocked by the missing `net` module.
- **`constants` legacy module** (Sep 2026): `require('constants')` was missing, breaking `graceful-fs`/`fs-extra`. `NodeRuntime` now registers it as an ordinary object with the same values as `fs.constants` (plus the common `O_*`/`S_I*` flags) but with `Object.prototype`, so `constants.hasOwnProperty(...)` behaves like Node. `fs.constants` itself stays a null-prototype object, matching Node.
- **`== null` no longer coerces objects** (Sep 2026): `looseEqual` ran the object-to-primitive conversion for `obj == null`/`obj == undefined`, which is a spec violation and made lodash's `baseGetTag`/`isFunction` invoke the mixed-in `toString` wrapper on `LazyWrapper.prototype` during load (`Cannot call non-function value: undefined (lastLookup=value)`). `null`/`undefined` comparisons now short-circuit before ToPrimitive. Within an hour of this fix lodash loaded and 13/13 core operations (`chunk`, `map`, `template`, `cloneDeep`, `merge`, `sortBy`, `range`, chain wrappers, …) passed; only `template` `<%- %>` escaping still returns `[object String]` (`<%= %>` interpolation and the rest of the template engine work).
- **Extracted native methods** (Sep 2026): `Math.ceil`/`floor`/`min`/`max`/`round` read their arguments at `args(1)`, so `const f = Math.ceil; f(3)` produced `NaN` (lodash captures `nativeMin`/`nativeMax`/`nativeCeil`/`nativeFloor` at load). `MathBuiltins.registerFunc` now normalizes the receiver-first shape by supplying the Math object when the call did not come through `Math.f(...)`, so both shapes work. `Function(['a','b'], body)` also stringifies its arguments with JS ToString (arrays become the comma-separated parameter list) instead of Scala `toString`.
- test262: `language/block-scope` 128/145, `for-of` 658/736 executed, `for-in` 77/114, `for` 2317/2463 (small gains; remaining failures are other binding/eval edges). Regression tests in `ConformanceRegressionTest`.

**Const loop bindings (Sep 2026)** — `for (const x in obj)` and `for (const [a, b] of pairs)` used to throw `TypeError: Assignment to constant variable` on the second iteration: the compiler only reset simple `const` identifiers to uninitialized at the top of each iteration. `Compiler` now resets every name collected from the binding pattern in both the for-in and for-of (sync and for-await) heads. Per-iteration closure environments are now created by the `CloneLocRef` opcode described above.

**Runner classpath (Sep 2026)** — `sbt runner/run`/`assembly` lacked `icu4j` (the parser's Unicode provider) and failed with `NoClassDefFoundError: com/ibm/icu/lang/UCharacter`; `build.sbt` now declares it for the runner project.

**Feature-config audit (Sep 2026)** — enabled 7 previously skipped features (+300 passing tests):
- `Math.sumPrecise` — rewritten on the iterator protocol: accepts any iterable, throws TypeError and closes the iterator on non-Number elements, handles NaN/±Infinity, accumulates exactly and preserves the `-0` rules. 10/10.
- `promise-with-resolvers` — `Promise.withResolvers` now builds its result via `NewPromiseCapability(this)`, so `this` (including subclasses) is used and non-constructors throw TypeError. 6/6.
- `symbols-as-weakmap-keys` — `WeakSet` gained a symbol backing store like `WeakMap` (WeakMap 78/79, WeakSet 66/67; remaining errors are unrelated).
- `for-in-order` — own-key enumeration order passes (9 tests).
- `stable-typedarray-sort` — TypedArray `sort` stability passes.
- `well-formed-json-stringify` — `JSON.stringify` now escapes unpaired surrogates as `\uXXXX` in `stdlib/JSON.scala` while keeping valid surrogate pairs.
- `top-level-await` — module bodies are compiled async and driven to settlement (see the Top-level await section); `language/module-code/top-level-await` passes 262/287 (91.3%).
- Evaluated but left skipped (pass rates too low to claim support): `regexp-unicode-property-escapes` (7%), `regexp-v-flag` (19%), `regexp-modifiers` (0%), `regexp-lookbehind` (47%), `regexp-match-indices` (50%), `regexp-duplicate-named-groups` (~50%), `cross-realm` (29%), `u180e` (the 3 dotall tests need code-unit `.` semantics), `proxy-missing-checks`, `export-star-as-namespace-from-module`, `well-formed`-adjacent JSON options, and all unimplemented proposals (Temporal, Atomics, Iterator helpers, Set methods, resizable ArrayBuffers, import attributes, …). (`Intl` exists as a JDK/ICU-backed subset but the `intl-*` feature flags remain off.)

**Class subclassing (Sep 2026)** — 34,275 → 34,429 passed, 4,183 → 4,034 errors; `class/subclass` 62 → 165 of 184:
- **Derived-constructor `this` semantics**: class constructors carrying heritage are marked with a non-enumerable `__derivedClass` property at definition; `constructValue` places a `__thisUninitialized` marker on the receiver. `GetThis` throws ReferenceError before `super()`, `GetThisUnchecked` (new opcode) lets `super()`/the default constructor forward the receiver, and `MarkThisInitialized` clears the marker just before the superclass body runs. Derived constructors that return a non-undefined primitive now throw TypeError from `constructValue` (uncatchable), and missing-super returns throw ReferenceError.
- **`super()` for every native builtin**: `NativeConstructor.superInitImpl` added for `Array` (builds/initializes a real exotic Array receiver with prototype override), `Map`/`Set`/`WeakMap`/`WeakSet`, `Boolean`/`Number`/`String`, `Error` + all NativeErrors/AggregateError, `Date`, `RegExp`, `ArrayBuffer`, all TypedArrays and `DataView`; `Symbol` rejects construction. `__funcSpread` validates that the superclass is a constructor and clears the marker before invoking it.
- **Class definition validation**: `class extends <non-constructor>` (generators, async functions, arrows, primitives, proxies of non-constructors) throws TypeError before any `"prototype"` lookup; `class X extends null` sets `X.prototype.[[Prototype]] = null` and keeps `X.[[Prototype]] = Function.prototype`. Class constructors can no longer be called without `new` (TypeError).
- **Function property rules**: only constructable and generator functions get an own `prototype`; async functions, async generators, arrows and methods no longer do. `async function*`/generator own-property tests now pass.
- **Compiler slot alignment**: `Scope.getAllLocalVarNames` dropped slots for shadowed names (e.g. two `catch (e)` bindings), so the runtime `localVarNames.indexOf(name)` no longer matched compiler slot indices. It now returns a slot-indexed array with unique placeholders for shadowed slots; this fixed a latent corruption where `class D extends Base { super() }` resolved `super` to `D` after an earlier caught exception.
- **`instanceof` / `isPrototypeOf`** honor a `JSArray`'s prototype override, so `new (class extends Array {})() instanceof Subclass` works.

**Earlier (Sep 2026, second sweep)** — +583 test262 tests (33,692 → 34,275 passed; errors 4,771 → 4,183):
- **`with` + PutValue reference semantics**: new `GetGlobalWithBase`/`PutGlobalWithBase` opcodes preserve the object environment record base across compound assignments and `++`/`--` inside `with`, even when the getter deletes the binding (`scope.x` deleted during `x ^= 3` now writes back to `scope`). `DefVar` inside `with` routes the initializer through the object record, and `Symbol.unscopables` is honored by `HasBinding`.
- **Compiler control-flow context leak**: nested function/arrow bodies inherited the enclosing function's `loopStack`, `finallyStack` and `iteratorCloseStack`, so a `return` inside a closure emitted the outer for-of iterator's `__iteratorClose` and could corrupt the call. `compileFunctionBody`/`compileArrowFunctionBody` now save/reset/restore them.
- **ToPrimitive in builtin conversions**: `Number`, `String`, `Math.*`, `parseInt`/`parseFloat`, `isNaN`/`isFinite`, Date setters and `DataView`/`TypedArray` coercion now use `BuiltinHelpers.toNumber`/`toJSString` (call `valueOf`/`toString`, propagate abrupt completions) instead of the object-blind `JSValue.toNumber`; the latter now parses `0b`/`0o` string literals and all ECMAScript whitespace; `Number(BigInt)` converts.
- **Math**: no-argument functions return NaN, `round(±Infinity)`/`round(-0.5)`/`clz32`/`imul`/`min`/`max`/`hypot`/`sumPrecise` fixed, and `Math.f16round` (ES2025, round-to-nearest-even) added; `Float16Array` uses the same half-float conversion.
- **Date**: multi-argument `new Date(y, m, ...)`, spec `MakeDay`/`MakeTime`/`MakeDate` double arithmetic for `Date.UTC`, ordered setter coercion with throw propagation, `Date.prototype[Symbol.toPrimitive]`.
- **Promise**: `all`/`allSettled`/`any`/`race` use `NewPromiseCapability(this)` + iterator records; `then` uses `SpeciesConstructor` + a result capability; `catch` is generic; `finally` builds `thenFinally`/`catchFinally` thunks; `NewPromiseCapability` executor/resolve/reject functions are anonymous with `%Function.prototype%`; `Promise.prototype[Symbol.toStringTag]`; native `super()` support via `NativeConstructor.superInitImpl`/`callWithThis`.
- **RegExp/exec**: `lastIndex` uses ToLength semantics for global and sticky, unicode mid-surrogate-pair `lastIndex` rounds back to the code point start, matches never start inside a surrogate pair, and `String.prototype.match` advances empty matches with `AdvanceStringIndex`.
- **Proxy**: proxies mirror the target's `[[Prototype]]`, property get/set forward to function/native/array targets, and `Function.prototype.call`/`apply` invoke callable proxies through the `apply` trap.
- **TypedArray**: `toLocaleString` invokes each element's method, `BYTES_PER_ELEMENT` is non-configurable, ToInt32/ToUint32 wrap instead of saturating.
- **`initProperty` order for native constructors**: `length` before `name` (built-in function property order); bound function `length` uses the JS-visible length (was 0) and is passed to the `NativeConstructor`.
- Regression coverage: `ConformanceRegressionTest` (21 tests).

**Interpreter performance (Sep 2026)**: `BytecodeLoop.run` was a single ~100KB method that exceeded HotSpot's method-size limit and was interpreted even after warmup (~1M instructions/sec). The opcode case bodies are now split across 12 `runGroupN` methods selected by a precomputed `BytecodeLoop.opcodeGroups` table, so C2 compiles the hot paths. Tight-loop throughput is ~15M instructions/sec (15x faster); test262 timeouts dropped from 107 to 48 and the previously-timing-out `RegExp/CharacterClassEscapes` tests now pass. Guarded by `InterpreterPerfRegressionTest`. `maxIterations` (runaway-loop guard) was raised to 100M.

**Known sweep memory pressure**: resolved. Heavy tests (arrays with millions of indices, Date/DST stress) used to fill the heap: `JSArray.writeElement` materialized dense storage up to `MaxDenseIndex` (1M entries) for a single large-index write, and dense presence was tracked with a boxed-Long `HashSet`. Now far indices use the sparse map (`MaxDenseGap = 4096`), `new Array(n)` pre-allocates at most `MaxPreallocated = 65536` slots, and dense presence uses a compact `BitSet`; `readElement` checks the sparse map first when it is non-empty. The `language` chunk now completes with no GC warnings. `scripts/test262-chunks.sh` runs `built-ins`/`language`/`staging`/`harness` in separate JVMs and prints an aggregate (the single-JVM full sweep can still GC-thrash, so prefer the script).

**Latest Fixes (Sep 2026)** — +817 test262 tests from a full sweep:
- **String literal early errors**: `StringToken` gained a `legacyEscape` flag; strict mode rejects legacy octal / non-octal decimal escapes (`\1`, `\8`, `\0` + digit). Malformed `\x`/`\u` escapes and raw LF/CR are rejected in all modes; `\u2028`/`\u2029` are valid raw (JSON-superset) and valid after a backslash (line continuation); Annex B octal truncation (`\400` = `\40` + `0`) implemented.
- **Numeric literal early errors**: `NumberToken` gained a `legacy` flag; strict mode rejects legacy octal (`010`) and non-octal decimal (`08`) literals (sloppy values now decode octal, `010 === 8`). Numeric separators may not be trailing or appear in legacy forms; a numeric literal followed immediately by an identifier start/digit (`3in`, `1.toString`) is a SyntaxError.
- **Directive prologue scanning**: `"use strict"` is now found anywhere in the leading run of string-literal statements (`function f() { "\1"; "use strict"; }` is strict).
- **`$262.createRealm`**: the test host now builds fresh realms (new runtime/context with its own intrinsics, `global`, `evalScript`, recursive `createRealm`), fixing cross-realm tests that do not declare the `cross-realm` feature (~50 tests).
- **Test262 timeout** default lowered from 10s to 5s (`-Dquickjs.test262.timeoutSeconds=N`): stuck tests no longer hold workers and accumulate memory.
- **Interpreter JIT**: split the monolithic `BytecodeLoop.run` dispatch into 12 JIT-compilable `runGroupN` methods via a precomputed `opcodeGroups` table (~15x throughput, test262 timeouts 107 → 48).
- **Computed element keys use ToPropertyKey**: `doGetElem`/`doSetElem`/`doDelete` normalized keys via the new `BuiltinHelpers.toElementKey` (numbers/strings/symbols keep fast paths; objects, functions, booleans, bigints, undefined/null convert). Previously `o[fn] = v` was silently a no-op: `cpn-*` computed-property tests went from 72 failures to 54 (remaining: accessor computed keys, `__classElementKey_N is not defined`, object-to-primitive conversion). Related gap: `String(fn)` still yields `[object Function]` instead of the function source text (`Function.prototype.toString`).
- **Shared iterator intrinsics**: `%IteratorPrototype%` is created with the realm intrinsics; `%ArrayIteratorPrototype%` and `%StringIteratorPrototype%` share it, Map/Set iterator prototypes inherit from it, and typed-array iterators use `%ArrayIteratorPrototype%` (via `IteratorBuiltins`, which also implements `String.prototype[Symbol.iterator]`). ArrayIteratorPrototype 16/27 -> 23/27, Map/Set 10/11, String Symbol.iterator 6/6.
- **`RegExp.prototype[Symbol.matchAll]` + `%RegExpStringIteratorPrototype%`**: proper lazy iterator that performs `RegExpExec` (observable custom `exec`), copies `lastIndex` to a fresh matcher, advances empty matches with `AdvanceStringIndex`, and `String.prototype.matchAll` delegates to it (with the global-RegExp TypeError). RegExpStringIteratorPrototype 0/17 -> 17/17, RegExp Symbol.matchAll 0 -> 17/26, String matchAll 9 -> 20/26. Remaining: `SpeciesConstructor`/`Construct` support and IsRegExp ordering.
- **Map/Set iterator objects**: `Map.prototype.entries/keys/values` and `Set.prototype.entries/values/keys` now return real iterator objects (`next`, `@@iterator` returns self, `@@toStringTag` = "Map/Set Iterator", receiver brand checks) instead of arrays; `clear()` during iteration is observed. Map non-passing 10 → 5, `MapIteratorPrototype`/`SetIteratorPrototype` now 10/11 and 8/11.
- **`Promise.withResolvers`** (ES2024) implemented.
- **Date brand checks**: `Date.prototype` methods now throw TypeError on non-Date receivers (29 tests).
- **String coercions**: `startsWith`/`endsWith`/`includes`/`codePointAt` use ToString/ToNumber helpers so Symbols throw instead of stringifying; String/prototype non-passing 179 → 164.
- **Array memory + cancellation**: sparse writes beyond the dense end no longer materialize up to 1M slots; dense presence uses a `BitSet`; `JSArray.apply(n)` caps pre-allocation. Native loops honor thread interruption via `BuiltinHelpers.checkInterrupted` (TypedArray set/reduce/every/some/fill/join/find/indexOf, Array join/toLocaleString), and `GeneratorSupport`/`runMicrotasks` rethrow interrupts instead of swallowing them. `scripts/test262-chunks.sh` aggregates per-directory sweeps.
- **Object-pattern shorthand defaults** (`{ a = 1 }`, CoverInitializedName): now parsed and accepted in destructuring contexts (for-of/for-in/for-await heads, assignments), rejected as a plain object literal. The compiler already supported `AssignmentExpression` property values. (~56 tests)
- **Escaped keywords are IdentifierNames, not keywords**: `IdentifierToken` gained an `escaped` flag, so `\u0067et`/`\u0061sync` no longer act as get/set/async/static. Context rules now reject escaped `await`/`yield` in async/generator/module code and reserved words as labels. Object literals now require a comma between properties. (~70 tests)
- **Module mode in the parser**: `new Parser(tokens, moduleMode = true)` treats module code as strict and reserves `await` (used by `Test262Runner` and `ModuleLoader`).
- **Interpreter perf**: `JSContext.updateTopFramePc` mutated a case class per instruction (allocation); `StackFrame.pc` is now a `var`. Thread-interrupt cancellation is sampled every 1024 instructions instead of every instruction.
- **Known perf limit**: resolved — `BytecodeLoop.run` was too large for HotSpot's default `DontCompileHugeMethods` limit; it is now split into `runGroupN` methods (see "Interpreter performance" above).
- **Parser/lexer performance**: `Lexer.tokenize()` returned a list-backed `Seq`, making `Parser.tokens(pos)` O(n) and parsing quadratic. Now returns an `IndexedSeq`; parsing `deepEqual.js` went 340ms → 15ms and the full test262 sweep 24min → 6min. Guarded by `ParserPerfRegressionTest`.
- **Fixed-size local frames**: `Interpreter`/generator/eval frames were hardcoded to 256 local slots, crashing functions with more locals (`PutLoc: Index out of bounds`). Frames now size from `localVarNames` with the historical floor kept (30 tests).
- **Do-while `continue` target**: the compiler jumped to the loop start instead of the test, so `do { try { continue } finally {} } while (...)` looped forever (9 try-statement timeouts).
- **Direct-eval `++i`**: `compileIncrementDecrement` used the "top-level var = global" heuristic in direct-eval mode while declarations used locals, so `eval("for (var i = 0; i < 2; ++i) ...")` never advanced `i`.
- **Generator VM opcodes**: implemented `EnterScope`/`LeaveScope`, `PushWith`/`PopWith` (with `withStack` preserved across yields) and `In` (shared `BuiltinHelpers.inOperator`).
- **For-head early errors**: initializers in for-in/of declarations and invalid assignment-pattern heads (`[...x, y]`) are rejected; strict `"use strict"` directives are applied while parsing function/method/arrow bodies.
- Lexer: `<<=`, `>>=`, `>>>=` were lexed as shift + `=` because the assignment branch checked the wrong character.
- Parser: nested ternary in the alternate branch (`a ? b : c ? d : e`) was rejected; now parsed per grammar (fixed test262's `deepEqual.js` harness).
- Parser: spread followed by trailing comma in array literals (`[...a,]`) is valid ES2017+; the trailing comma now only triggers the "rest element must be last" early error when the literal is used as an assignment pattern.
- Lexer: numeric separators crashed with `NumberFormatException` when building the value (`1.0e-1_0`); underscores are stripped before conversion.
- Built-ins: corrected `length` property values for Date/RegExp/JSON/Object/Number/Boolean/Error/Promise/String/parseInt/Function.prototype.apply (~70 tests).
- **Regexp literal early errors** (`RegExpSyntax.scala`): flag validity/duplicates, line terminators, named-group syntax/duplicates/dangling `\k`, invalid braced quantifiers, quantified assertions, Unicode-mode identity/`\c`/decimal/`\u{...}` escapes and class ranges (~90 tests).
- **Parser early errors**: ReservedWord shorthand properties, duplicate object-literal `__proto__`, getter/setter arity, strict-reserved identifier refs, `with` in strict mode, reserved class names, `return`/`break`/`continue` outside their contexts, and declarations in single-statement bodies (~100 tests).
- Tests: `QuickJSJavaScriptTest` has a 120s timeout (test_builtin.js runs close to munit's 30s default under load).

**Known architectural gap**: ordinary objects cannot have a `JSArray` as their `[[Prototype]]` (`JSObject.prototype` is typed `JSObject | Null`; `JSArray` is a separate class). This breaks `foo.prototype = new Array(...); new foo()` and `Object.create(array)`. Repro kept in `stdlib/src/test/scala/quickjs/stdlib/ScratchParseTest.scala` (ignored). Fix requires a prototype-value abstraction or unifying arrays with objects.

**Recent Progress (May 2026)**:
- Implemented TypedArrays (12 types: Int8, Uint8, Uint8Clamped, Int16, Uint16, Int32, Uint32, Float32, Float64, BigInt64, BigUint64, Float16) + ArrayBuffer + DataView
- Added `%TypedArray%` intrinsic object (shared base for all typed array constructors)
- Added TypedArray static methods `from`, `of`, and `Symbol.species`
- Added ArrayBuffer/DataView/TypedArray test262 smoke tests (3 new suites, 200 tests)
- Fixed parser to accept contextual keywords (`from`, `as`, `get`, `set`, `static`, `of`, `yield`, `await`, `let`) as identifiers
- Fixed ArrayBuffer constructor OOM on large size inputs
- Implemented real `eval` function with special inline handling for `eval("this")`, `eval("new.target")`, and `eval("super.f()")`
- Added `__proto__` getter/setter on `Object.prototype` and `__proto__:` support in object literals
- Fixed `Function.prototype.bind` — name, length, constructability, and bound `new`
- Fixed `new Array(...)` multi-argument construction
- Added global `isNaN` and `isFinite` functions
- Added context tracking (`currentThis`, `currentClosure`) to JSContext for eval
- Made `delete` on null/undefined respect strict mode (return true in non-strict, throw TypeError in strict)
- Re-enabled 14 previously-excluded test_language.js test functions
- Implemented `AggregateError`, `EvalError`, and `URIError`, including `cause` options and `AggregateError.errors` iterable conversion
- Updated `Promise.any` to reject with a real `AggregateError` instance
- Improved array index property descriptor compatibility and `Reflect.defineProperty` support for arrays
- Implemented `import.meta` for modules with a cached null-prototype meta object
- Implemented dynamic `import()` with Promise resolution/rejection over existing module loaders
- Improved TypedArray indexed property descriptors, indexed `defineProperty`, and own-key enumeration
- Aligned `TypedArray.from`/`of` with QuickJS generic constructor creation and result validation
- Added `TypedArray` species construction for `map`, `filter`, and `slice`
- Added `TypedArray.prototype.subarray` species construction with shared buffer/offset/length arguments

**QuickJS C Test Status (5 files)**:
- `test_loop.js` — ✅ ALL PASS
- `test_bigint.js` — ✅ ALL PASS
- `test_builtin.js` — ✅ ALL PASS (excluded: TypedArrays, WeakRef, FinalizationRegistry, generators, rope, line/col, eval scope, enum order, Math.sumPrecise, Date, RegExp, JSON, Map, Symbol, WeakMap, Number, String, Array, Function edge cases)
- `test_closure.js` — ✅ ALL PASS (excluded: test_with, test_eval_closure, test_eval_const — require direct eval scope)
- `test_language.js` — ✅ ALL PASS (14/26 test functions pass, 12 excluded: argument_scope, function_expr_name, parse_arrow_function, global_var_opt, parse_semicolon, labels, labels2, destructuring, function_length, object_literal, unicode_ident — various edge cases; test_delete excluded due to QuickJS-specific non-strict delete behavior)

## Architecture Overview

### Core Design Decisions

1. **Stack-based bytecode interpreter** (not JVM bytecode generation)
2. **JVM GC integration** (not custom mark-and-sweep)
3. **Tagged union type system** for JavaScript values
4. **Hand-written recursive descent parser** (not parser combinators as originally planned)

### Module Structure

```
quickjs-scala/
├── build.sbt
├── core/                        # Core type system
│   └── src/main/scala/quickjs/
│       ├── value/               # JSValue tagged union (Int32, Float64, BigInt, JSStr, Symbol, Object, etc.)
│       ├── runtime/             # JSRuntime, JSContext
│       ├── atom/                # Atom table (string interning)
│       └── objmodel/            # JSObject, JSArray, properties
├── parser/                      # ES2024+ parser
│   └── src/main/scala/quickjs/
│       ├── ast/                 # AST nodes
│       ├── lexer/               # Lexer with BigInt literal support
│       └── parser/              # Hand-written recursive descent parser
├── compiler/                    # Bytecode compiler
│   └── src/main/scala/quickjs/
│       ├── bytecode/            # Opcode definitions (92 opcodes)
│       └── compiler/            # Compiler with closure capture analysis
├── runtime/                     # Interpreter & standard library
│   └── src/main/scala/quickjs/
│       ├── interpreter/         # Stack-based bytecode interpreter
│       ├── runtime/             # StdLib (Promise, Map, Set, WeakMap, WeakSet, Symbol, RegExp, Date, Proxy, Reflect, BigInt, Error)
│       └── repl/                # REPL with completion
└── stdlib/                      # Standard library & tests
    └── src/test/
        ├── scala/               # Scala test suites
        └── resources/           # QuickJS C test files
```

## Key Files and Their Purpose

### Core Type System

**`/core/src/main/scala/quickjs/value/JSValue.scala`**
- Tagged union representation: `sealed trait JSValue` with case classes
- Types: `Undefined`, `Null`, `Bool`, `Int32`, `Float64`, `JSStr`, `Symbol`, `BigInt` (wraps `java.math.BigInteger`), `Object`, `JSArrayVal`, `Function`, `Generator`, `Promise`, `Native`
- Smart constructors: `fromInt`, `fromDouble`, `fromBoolean`, `fromString`
- Arithmetic operations: `add`, `subtract`, `multiply`, `divide` — all have BigInt cases
- Type conversions: `toBoolean`, `toNumber`, `toString`
- VarRef for closure variable indirection (pointer sharing)
- `GlobalRef` for lazy global scope lookup from closures

**`/core/src/main/scala/quickjs/objmodel/JSObject.scala`**
- Property storage in `mutable.LinkedHashMap`
- Prototype chain support
- Property descriptors with attributes (enumerable, writable, configurable)
- Getter/setter support
- Extensibility, sealing, freezing flags

**`/core/src/main/scala/quickjs/runtime/JSContext.scala`**
- Execution context, global object, intrinsics
- Error construction with stack trace attachment
- Microtask queue for Promise resolution
- Prototype chain: objectPrototype → functionPrototype → ... → null

### Parser

**`/parser/src/main/scala/quickjs/ast/AST.scala`**
- AST nodes for ES2024+ grammar
- Supports: literals, identifiers, private identifiers, binary/unary expressions, all statements, control flow, functions, arrow functions, classes, template literals, optional chaining, nullish coalescing, destructuring, spread/rest, modules

**`/parser/src/main/scala/quickjs/parser/Parser.scala`**
- Hand-written recursive descent parser
- Full JavaScript expression parsing with operator precedence
- Labeled statement support
- BigInt literal parsing (`0n`, `0xFn`, `0o7n`, `0b1n`)

### Compiler

**`/compiler/src/main/scala/quickjs/bytecode/Opcode.scala`**
- 92 opcodes: stack manipulation, arithmetic, comparison, bitwise, logical, control flow, objects, arrays, exceptions, closures, iterators, generators, async

**`/compiler/src/main/scala/quickjs/compiler/Compiler.scala`**
- AST → bytecode compilation
- Closure capture analysis (free variable detection)
- Scope management for let/const/var
- Label resolution for break/continue
- TDZ enforcement via GetLocCheck/SetLocUninitialized

### Interpreter

**`/runtime/src/main/scala/quickjs/interpreter/Interpreter.scala`**
- Stack-based bytecode interpreter with @switch dispatch
- Closure creation via VarRef sharing
- BigInt arithmetic, comparison, and bitwise operations
- try/catch/finally support
- Generator support (basic)

### Standard Library

**`/runtime/src/main/scala/quickjs/runtime/StdLib.scala`** (55 lines — initialization facade)

**`/runtime/src/main/scala/quickjs/runtime/builtins/`** (~7,600 lines across 15 files)
- Initializes all built-in objects:
  - `Object` (create, assign, keys, values, entries, defineProperty, getOwnPropertyDescriptor, freeze, seal, is, hasOwn, etc.)
  - `Array` (push, pop, shift, unshift, slice, concat, map, filter, forEach, reduce, splice, indexOf, includes, flat, flatMap, find, sort, etc.)
  - `Function` (call, apply, bind)
  - `String` (charAt, indexOf, slice, split, replace, match, startsWith, endsWith, padStart, padEnd, trim, etc.)
  - `Number` (isFinite, isInteger, isNaN, parseInt, parseFloat, toFixed, toExponential, etc.)
  - `Boolean` (toString, valueOf)
  - `Math` (abs, floor, ceil, round, max, min, pow, sqrt, random, sin, cos, etc.)
  - `Date` (constructor, parse, UTC, now, get/set methods)
  - `RegExp` (exec, test, toString, flags, sticky/dotAll/unicode support)
  - `Symbol` (constructor, for, keyFor, well-known symbols)
  - `Map` (get, set, has, delete, clear, size, forEach, entries, keys, values)
  - `Set` (add, has, delete, clear, size, forEach, entries, keys, values)
  - `WeakMap` (get, set, has, delete)
  - `WeakSet` (add, has, delete)
  - `Promise` (then, catch, finally, resolve, reject, all, race, allSettled, any)
  - `Proxy` (get, set, has, deleteProperty, ownKeys, getOwnPropertyDescriptor, defineProperty)
  - `Reflect` (get, set, has, deleteProperty, ownKeys, getPrototypeOf, setPrototypeOf, defineProperty, getOwnPropertyDescriptor)
  - `BigInt` (constructor with string/number/bool conversion, asIntN, asUintN)
  - `WeakMap` (get, set, has, delete)
  - `WeakSet` (add, has, delete)
  - `Error`, `TypeError`, `ReferenceError`, `SyntaxError`, `RangeError`, `EvalError`, `URIError`, `AggregateError` (with stack traces)
  - `console` (log with pretty printing)
  - `JSON` (parse, stringify with reviver/replacer/space)
  - File-based module loading (`import`/`export`) via ModuleLoader

## How the Pipeline Works

### Example: Evaluating `1 + 2 = 3`

```scala
// 1. Create AST
val ast = Script(
  body = Seq(
    ExpressionStatement(
      BinaryExpression(
        operator = BinaryOperator.Add,
        left = Literal(JSValue.fromInt(1), Span(0, 1, 0, 0)),
        right = Literal(JSValue.fromInt(2), Span(4, 5, 0, 4)),
        span = Span(0, 5, 0, 0)
      ),
      span = Span(0, 5, 0, 0)
    )
  ),
  span = Span(0, 5, 0, 0)
)

// 2. Compile to bytecode
val compiler = Compiler()
val bytecode = compiler.compileScript(ast)

// Generated bytecode:
// PushI32(1)
// PushI32(2)
// Add
// ReturnUndef

// 3. Execute bytecode
given ctx: JSContext = JSContext(JSRuntime())
val interpreter = Interpreter()
val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

// Result: JSValue.Undefined (because ExpressionStatement drops it)
```

## Build and Test Commands

```bash
# Compile all modules
sbt compile

# Run all tests (698 tests, 0 failures; test262 smoke tests auto-skip if not cloned)
sbt test

# Clone test262 for conformance testing (if you don't already have it)
# If test262/ already exists as a symlink or clone, skip this.
git clone --depth 1 https://github.com/tc39/test262.git test262

# Run specific test
sbt "testOnly quickjs.stdlib.QuickJSJavaScriptTest"
```

## Test Status

**Current Test Count**: 1,244 tests, 0 failures, 0 errors

### Test Distribution
- **stdlib**: 943 tests — language features, built-in objects, JSON, arrays, TypedArrays, Node compatibility (`NodeCompatTest`), etc.
- **runtime**: 204 tests — interpreter correctness, closures, try/catch, classes, etc.
- **compiler**: 13 tests
- **parser**: 84 tests (lexer + parser + strict mode)
- **core**: 16 tests
- **REPL**: 23 tests
- **Various debug/trace tests**: ~98 tests
- **test262 smoke tests**: 15 suites (~871 tests) — see below
- **QuickJS C test files**: 5 files run via `QuickJSJavaScriptTest` — all pass

### test262 Conformance (15 suites, ~871 tests)
| Suite | Tests | Passed | Errors | Skipped | Pass Rate |
|-------|-------|--------|--------|---------|-----------|
| `Array/isArray` | 29 | 29 | 0 | 0 | 100% |
| `Object/assign` | 38 | 27 | 11 | 0 | 71.1% |
| `Math` | 50 | 50 | 0 | 0 | 100% |
| `language/literals` | 50 | 42 | 0 | 8 | 100% |
| `Symbol` | 94 | 64 | 13 | 17 | 83.1% |
| `BigInt` | 50 | 31 | 19 | 0 | 62.0% |
| `Map` | 50 | 27 | 8 | 15 | 77.1% |
| `Set` | 50 | 48 | 1 | 1 | 98.0% |
| `WeakMap` | 30 | 20 | 6 | 4 | 76.9% |
| `WeakSet` | 30 | 22 | 3 | 5 | 88.0% |
| `Promise` | 50 | 23 | 27 | 0 | 46% |
| `Reflect` | 50 | 39 | 11 | 0 | 78% |
| `TypedArray` | 100 | 51 | 21 | 28 | 70.8% |
| `ArrayBuffer` | 50 | 26 | 12 | 12 | 68.4% |
| `DataView` | 50 | 15 | 14 | 21 | 51.7% |

Main engine gaps exposed: `ToNumber` doesn't call `valueOf`/`toString` on objects for all paths (partial fix), iterator closing/error paths in `from` are partial, and resizable/immutable ArrayBuffer variants remain.

### QuickJS C Test File Status
| File | Status | Remaining Issue |
|---|---|---|
| `test_loop.js` | ✅ All pass | — |
| `test_bigint.js` | ✅ All pass | — |
| `test_closure.js` | ✅ All pass | — |
| `test_language.js` | ✅ All pass | — |
| `test_builtin.js` | ✅ All pass | — |

## Current Priorities

**For running real Node packages (highest ROI first):**
1. **`fs.watch`/`watchFile`** — chokidar/nodemon watch callbacks.
2. **Crypto ciphers/KDFs/keypair** (`createCipheriv`, `pbkdf2Sync`, `scryptSync`, `generateKeyPairSync`).
3. **`stream/web` `ReadableStream` + fetch streaming**, `dns`, `worker_threads`.
4. **`require.extensions` / loader hooks** — ts-node loads but needs its `.ts` extension handler to transpile required files.
5. **`ws`/socket.io** — WebSocket upgrade handling on top of the new HTTP server.
6. Smaller gaps surfaced by probes: `fs.mkdtempSync` rejects arbitrary prefixes; HTTP keep-alive/pipelining is not implemented (responses close the connection).

**For test262 conformance (larger clusters, lower per-test value for Node scripts):**
8. **Parser early errors** — ~560 tests expect a `SyntaxError` during parse that is not thrown (mostly `language/expressions`, `language/statements`, `language/module-code`, `language/eval-code`), plus ~220 tests expecting a `TypeError`/`ReferenceError` at runtime.
9. **Missing-throw semantics** — negative tests where the engine does not throw dominate `built-ins/Function`, `built-ins/Proxy`, `staging/sm` and `built-ins/RegExp`.
10. **`super` in expressions** and module harness gaps (`assert is not defined` in `language/module-code`, `language/import` namespace edges).
11. **TypedArray test262** — resizable/immutable ArrayBuffer variants, subclass species edge cases, iterator-closing error paths.
12. **Performance optimization** — no inline caching, peephole optimization.

## Quick Reference

### Creating and executing JavaScript

```scala
import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.runtime.StdLib
import quickjs.value.JSValue

given rt: JSRuntime = JSRuntime()
given ctx: JSContext = JSContext(rt)
StdLib.initialize(ctx)

def eval(source: String): JSValue =
  val lexer = Lexer(source)
  val tokens = lexer.tokenize()
  val parser = Parser(tokens)
  val ast = parser.parseScript()
  val compiler = Compiler()
  val bytecode = compiler.compileScript(ast)
  val interpreter = Interpreter()
  interpreter.call(bytecode, JSValue.Undefined, Array.empty)
```

### Key Architecture Notes

- **VarRef indirection**: Closure variables use `VarRef` wrappers for shared mutation — both parent and child functions see the same `VarRef` object
- **GetGlobal opcode**: Resolves variables by checking `with` stack → closure map → global scope, in that order
- **Closure creation**: `GetConst` opcode creates `JSValue.Function` from `BytecodeFunction`, sharing `VarRef` objects between parent and child
- **BigInt**: Uses `java.math.BigInteger`, all arithmetic/comparison/bitwise ops have BigInt cases, throws `TypeError` on mix with Number
- **`getAllLocalVarNames`**: Returns ALL variables (params + locals + arguments) sorted by declaration index. This index directly maps to the interpreter's `locals` array position.
