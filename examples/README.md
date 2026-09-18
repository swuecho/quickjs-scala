# QuickJS-Scala Examples

Runnable JavaScript examples that demonstrate the engine and its host layer.
They are also useful as smoke tests for a fresh build.

## Build the runner

```bash
# from the repository root
sbt runner/assembly
JAR=runner/target/scala-3.7.4/quickjs-runner.jar   # scala-3.x path
```

Java 21+ is required. Every command below is relative to the repository root.

## The examples

| File | Mode | What it demonstrates |
| --- | --- | --- |
| [`feature_demo.js`](feature_demo.js) | plain | ES5-to-ES2020 language basics: variables, operators, control flow, objects, arrays, classes, errors, Map/Set, proxies |
| [`language_tour.js`](language_tour.js) | plain | Modern language features: private fields/brand checks, static blocks, generators, iterators, symbols, proxies, BigInt, `Object.groupBy`, Set methods, `Array.prototype.toSorted`, `structuredClone`, `WeakRef`, named-group regexps, `Intl` |
| [`async_patterns.js`](async_patterns.js) | plain / `--node` | Promises (`all`/`allSettled`/`any`/`race`/`try`/`withResolvers`), async/await, thenable adoption, microtask vs timer ordering, async generators and `for await`, `AbortController` (in `--node`) |
| [`web_globals.js`](web_globals.js) | plain | Browser-style globals: `URL`/`URLSearchParams`, `TextEncoder`/`TextDecoder` (incl. `encodeInto`), `atob`/`btoa`, `structuredClone`, `Intl`, timers |
| [`esm-demo/`](esm-demo/) | ES module | ESM: named/default/namespace imports, re-export barrels, JSON modules with import attributes, dynamic `import()`, top-level `await`, `import.meta.url` |
| [`node_cli.js`](node_cli.js) | `--node` | A Node CLI: `process.argv`, `fs.promises` + sync `fs`, `path`, `Buffer`, `os`, JSON output |
| [`http_server.js`](http_server.js) | `--node` | `http.createServer` with routing and JSON bodies, called back via `http.get` and `fetch` |
| [`streams_pipeline.js`](streams_pipeline.js) | `--node` | `fs.createReadStream`/`createWriteStream`, `PassThrough`, `StringDecoder`, `.pipe()`, `finished()` |
| [`crypto_toolkit.js`](crypto_toolkit.js) | `--node` | Hashes, HMAC, PBKDF2/HKDF, random values, AES-256-GCM authenticated encryption, UUIDs |
| [`child_process.js`](child_process.js) | `--node` | `execSync`/`execFileSync`/`exec`/`spawnSync`/`spawn`, streaming stdout/stderr, stdin piping, `kill()` |
| [`os_module.js`](os_module.js) | `--node` | A minimal `os` + class + private field example |

## Running everything

```bash
# one example at a time
java -jar $JAR examples/language_tour.js
java -jar $JAR examples/async_patterns.js
java -jar $JAR --node examples/node_cli.js README.md
java -jar $JAR --node examples/http_server.js
java -jar $JAR examples/esm-demo/main.mjs --table

# or all of them (uses the assembled jar, builds it if missing)
examples/run-all.sh
```

`run-all.sh` runs each example with `timeout` and stops at the first failure,
printing a summary. Pass `--keep-going` to run everything anyway.

## Which mode do I need?

* **Plain mode** (`runner script.js`) installs browser-like globals only:
  `console`, timers, `URL`, `TextEncoder`/`TextDecoder`, `atob`/`btoa`,
  `structuredClone`, `Intl` and `scriptArgs`. Use it for the language and
  async examples.
* **ES module mode** (`.mjs`, or `-m`) adds `import`/`export`, dynamic
  `import()`, top-level `await` and `import.meta.url`.
* **Node mode** (`--node`) adds `require`, `process`, `Buffer`, the `node:`
  built-in modules and the host event loop. Use it for the filesystem,
  HTTP, stream, crypto and child-process examples.

See [`../docs/RUNNING_NODE_SCRIPTS.md`](../docs/RUNNING_NODE_SCRIPTS.md) for the
full Node compatibility guide.
