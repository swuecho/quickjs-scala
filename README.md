# QuickJS-Scala

[![Scala CI](https://github.com/swuecho/quickjs-scala/actions/workflows/scala.yml/badge.svg)](https://github.com/swuecho/quickjs-scala/actions/workflows/scala.yml)

A JavaScript engine for the JVM, written in Scala 3 and inspired by the
[QuickJS](https://bellard.org/quickjs/) C implementation. The goal is a
production-grade engine with full ES2024+ support: parser, bytecode compiler,
stack-based interpreter, standard library, REPL, and a Node.js compatibility
layer.

> QuickJS-Scala is an independent project, not affiliated with or endorsed by
> the QuickJS authors.

## Highlights

- **ES2024+ language support** — modules, top-level `await`, generators/async
  generators, classes with private fields and static blocks, destructuring,
  optional chaining, `with`, direct `eval`, proxies, typed arrays, `Intl`, and
  more.
- **Stack-based bytecode interpreter** mirroring QuickJS's design (typed
  opcodes, closure capture by slot, exception handlers, generator/async
  suspend-resume).
- **JVM garbage collection** instead of a hand-written GC.
- **Standalone runner** — a single self-contained JAR executes `.js` and `.mjs`
  files with no sbt or Node.js installation required.
- **Node.js compatibility mode** (`--node`) — CommonJS + ESM resolution,
  `require`, `process`, `fs`, `path`, `http`, `crypto`, `stream`, timers, and
  more.
- **REPL** with history, tab completion, and bytecode tracing.
- **Conformance-driven development** — the [test262](https://github.com/tc39/test262)
  suite and the upstream QuickJS C test files run unmodified.

## Status

| Metric | Result |
| --- | --- |
| Project test suite (`sbt test`) | 1,340 tests, 0 failures |
| test262 full sweep | 37,766 passing (93.7% of executed tests) |
| Upstream QuickJS C test files | 5/5 passing |

The engine targets Java 21+. The `Intl` implementation is a pragmatic
JDK/ICU-backed subset rather than a complete ECMA-402 implementation. See
[docs/CONFORMANCE.md](docs/CONFORMANCE.md) for the exact conformance baseline
and [docs/RUNNING_NODE_SCRIPTS.md](docs/RUNNING_NODE_SCRIPTS.md) for Node
compatibility details and known gaps.

## Quick start

Build the standalone runner:

```bash
sbt runner/assembly
```

This produces `runner/target/scala-*/quickjs-runner.jar`. Then:

```bash
# Plain JS script (browser-like globals: console, timers, URL, atob, ...)
java -jar runner/target/scala-*/quickjs-runner.jar script.js

# Node.js compatibility mode: require/process/fs/http/...
java -jar runner/target/scala-*/quickjs-runner.jar --node script.js

# ES module (import, top-level await, import.meta.url)
java -jar runner/target/scala-*/quickjs-runner.jar --node app.mjs

# Inline evaluation
java -jar runner/target/scala-*/quickjs-runner.jar -e "console.log(1 + 2)"

# REPL
sbt "stdlib/run"
```

Example `script.js` (run it with `--node`):

```js
const os = require('os');

class Greeter {
  #name;
  constructor(name) { this.#name = name; }
  greet() { return `Hello, ${this.#name}!`; }
}

console.log(new Greeter(os.platform()).greet());
console.log([1, 2, 3].map(x => x * 2));
```

More runnable scripts live in [`examples/`](examples/README.md): a language
tour, async/await patterns, ES modules with top-level await, a Node CLI, an
HTTP server, streams, crypto and child processes. Run them all with
`examples/run-all.sh`.

## Building from source

```bash
# Compile everything
sbt compile

# Build the runner JAR
sbt runner/assembly

# Clean build
sbt clean
```

### Running the tests

```bash
# Full suite (~20 s once compiled; suites run in parallel)
sbt test

# Previously failing tests plus suites affected by source changes
sbt testQuick

# A single suite
sbt "stdlib/testOnly quickjs.stdlib.QuickJSLanguageTest"
```

### test262 conformance

```bash
# One-time: clone the conformance suite next to the project
git clone --depth 1 https://github.com/tc39/test262.git test262

# Chunked full sweep in separate JVMs (~3 min)
scripts/test262-chunks.sh

# A quick slice
sbt "stdlib/runMain quickjs.stdlib.Test262Runner test262.conf 500 language/statements/try"

# Only the previous failures (seconds)
scripts/test262-rerun.sh
```

The smoke-test suites in `sbt test` skip automatically when `test262/` is not
present.

## Project structure

```
quickjs-scala/
├── core/       # JSValue tagged union, JSObject/JSArray, runtime context
├── parser/     # Lexer, RegExp syntax validation, AST, recursive-descent parser
├── compiler/   # AST -> bytecode compiler, opcodes, stack analysis
├── runtime/    # Bytecode interpreter, generators/async, built-ins, REPL
├── stdlib/     # Host globals, runner, test262 runner, Node compatibility layer
├── examples/   # Runnable example scripts (see examples/README.md)
└── web/        # Scala.js execution-trace frontend (Laminar + Vite)
```

### Design decisions

1. **Stack-based bytecode interpreter**, not JVM bytecode generation — easier to
   align semantics with QuickJS and to trace execution.
2. **JVM GC integration** — no custom mark-and-sweep; uses the mature JVM
   collectors.
3. **Tagged union type system** (`sealed trait JSValue`) for JavaScript values,
   with fast paths for `Int32`/`Float64` arithmetic.

## Documentation

- [docs/README.md](docs/README.md) — documentation index
- [examples/README.md](examples/README.md) — runnable example scripts (`run-all.sh`)
- [docs/CONFORMANCE.md](docs/CONFORMANCE.md) — conformance policy and baseline
- [docs/RUNNING_NODE_SCRIPTS.md](docs/RUNNING_NODE_SCRIPTS.md) — Node mode guide
- [docs/QUICKJS_COMPARISON.md](docs/QUICKJS_COMPARISON.md) — comparison with QuickJS C
- [docs/REPL.md](docs/REPL.md) — REPL usage and tracing
- [AGENTS.md](AGENTS.md) — detailed development log and architecture notes

## Contributing

Contributions are welcome. Before opening a pull request:

1. Run `sbt test` (1,340 tests, all green).
2. Add a regression test for every bug fix.
3. For conformance work, run `scripts/test262-rerun.sh` and confirm no
   regressions.

## License

[MIT](LICENSE) © 2025-2026 Hao Wu. See
[THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md) for QuickJS attribution and
test-file licensing.
