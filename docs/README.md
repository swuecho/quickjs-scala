# QuickJS-Scala Documentation

This directory contains documentation for QuickJS-Scala, a JavaScript engine
written in Scala 3 for the JVM (ES2024+).

## Start Here

| Document | Description |
|----------|-------------|
| [`../AGENTS.md`](../AGENTS.md) | **Current status, test counts, sweep results and priorities** |
| [`REPL.md`](REPL.md) | Interactive JavaScript shell: commands, tracing, limitations |
| [`RUNNING_NODE_SCRIPTS.md`](RUNNING_NODE_SCRIPTS.md) | Running JS/Node scripts with the runner (`--node`), built-ins, embedding |
| [`CONFORMANCE.md`](CONFORMANCE.md) | Conformance testing methodology (QuickJS C tests, test262) |
| [`../examples/README.md`](../examples/README.md) | 12 runnable example scripts and `run-all.sh` |

## Documentation Index

### Guides

| Document | Description | Last Updated |
|----------|-------------|--------------|
| [`REPL.md`](REPL.md) | REPL design, commands, result display, scoping, tracing | 2026-09-19 |
| [`RUNNING_NODE_SCRIPTS.md`](RUNNING_NODE_SCRIPTS.md) | Script runner, Node.js compatibility mode, host APIs | 2026-09-17 |
| [`CONFORMANCE.md`](CONFORMANCE.md) | test262 / QuickJS C conformance runners and rules | 2026-09-11 |
| [`NATIVE_FUNCTION_DELEGATION.md`](NATIVE_FUNCTION_DELEGATION.md) | How the VM calls Scala native functions (built-ins, `super()`) | 2026-05-02 |

### Implementation Notes

| Document | Description | Date |
|----------|-------------|------|
| [`LET_CONST_SCOPE_IMPLEMENTATION.md`](LET_CONST_SCOPE_IMPLEMENTATION.md) | `let`/`const` block scoping and TDZ | 2025-12 |
| [`LABELED_STATEMENTS.md`](LABELED_STATEMENTS.md) | Labeled `break`/`continue` | 2025-12 |
| [`CLOSURE_IMPLEMENTATION.md`](CLOSURE_IMPLEMENTATION.md) | Closure/VarRef capture analysis | 2025-12 |
| [`CLOSURE_ALIGNMENT.md`](CLOSURE_ALIGNMENT.md) | Closure design vs. QuickJS C | 2025-12 |
| [`DEBUGGER_SUPPORT.md`](DEBUGGER_SUPPORT.md) | Debug tooling. ⚠️ **Partially historical** — `debugger;` statements are currently no-ops; the working feature is the REPL instruction tracer ([`REPL.md`](REPL.md)) | 2025-12 |
| [`scala3-features-tutorial.md`](scala3-features-tutorial.md) | Scala 3 features used by the engine (tutorial) | 2026 |

### Comparisons & Analysis

| Document | Description | Last Updated |
|----------|-------------|--------------|
| [`QUICKJS_COMPARISON.md`](QUICKJS_COMPARISON.md) | Feature parity with QuickJS C | 2026-09-18 |
| [`PARSER_COMPARISON.md`](PARSER_COMPARISON.md) | Parser implementation comparison with C | 2025-12-25 |
| [`QUICKJS_TEST_INTEGRATION.md`](QUICKJS_TEST_INTEGRATION.md) | Porting the QuickJS C test files | 2025-12 |

### Historical Snapshots

These capture earlier milestones and are kept for the record. They do **not**
reflect the current test counts — see [`../AGENTS.md`](../AGENTS.md).

| Document | Description | Snapshot |
|----------|-------------|----------|
| [`PROGRESS.md`](PROGRESS.md) | Project status, roadmap and test breakdown | 2026-07-10 |
| [`RECENT_WORK.md`](RECENT_WORK.md) | Session notes for the let/const scoping work | 2025-12-27 |
| [`SCALA_REWRITE_PLAN.md`](SCALA_REWRITE_PLAN.md) | Original C → Scala rewrite plan | 2025-12 |
| [`EDUCATIONAL_PLATFORM.md`](EDUCATIONAL_PLATFORM.md) | Planned visualization web platform | 2025-12 |
| [`ideas.md`](ideas.md) | Ideas and future enhancements | 2025-12-26 |
| [`FIX_SUMMARY.md`](FIX_SUMMARY.md) | Early priority-fix report | 2025-12 |
| [`COMPREHENSIVE_TEST_REPORT.md`](COMPREHENSIVE_TEST_REPORT.md) | Early comprehensive test report | 2025-12 |
| [`PARSER_PRECEDENCE_FIX.md`](PARSER_PRECEDENCE_FIX.md) | Parser precedence-chain bug fix | 2025-12 |

## Current Status (September 2026)

- **Unit/integration tests**: 1,341 tests, 0 failures, 0 errors
- **Full test262 sweep**: 37,766 / 49,502 passing (93.7% of executed tests;
  9,202 skipped by feature config; 165 failures, 2,334 errors, 35 timeouts) in
  ~3 minutes wall clock
- **QuickJS C regression files**: all 5 pass (`test_loop`, `test_bigint`,
  `test_closure`, `test_language`, `test_builtin`)
- **Runner**: ordinary scripts, ES modules and Node.js compatibility mode
  (`--node`); see [`RUNNING_NODE_SCRIPTS.md`](RUNNING_NODE_SCRIPTS.md)
- **REPL**: full standard library at the prompt; see [`REPL.md`](REPL.md)

### Module Overview

```
core/       # Tagged-union values, object model, JSContext/JSRuntime, atoms
parser/     # Lexer, AST, hand-written recursive-descent parser (ES2024+)
compiler/   # Bytecode opcodes, stack analysis, AST -> bytecode compiler
runtime/    # Bytecode interpreter, generators/async, built-ins, REPL, modules
stdlib/     # Runner, JSON/console/host globals, Node.js compatibility layer
web/        # Scala.js frontend for bytecode trace visualization
```

## Development Workflow

```bash
# Compile everything
sbt compile

# All tests (~20-30 s, suites run in parallel)
sbt test

# Incremental: previously failing tests + suites affected by source changes
sbt testQuick

# A single suite / test
sbt "runtime/testOnly quickjs.repl.*"
sbt "stdlib/testOnly quickjs.stdlib.ConformanceRegressionTest -- -z try"

# Full test262 sweep (~3 min, separate JVMs per chunk)
scripts/test262-chunks.sh

# Re-run only the previous test262 failures (seconds)
scripts/test262-rerun.sh
```

Build the standalone runner (see
[`RUNNING_NODE_SCRIPTS.md`](RUNNING_NODE_SCRIPTS.md)):

```bash
sbt runner/assembly
java -jar runner/target/scala-*/quickjs-runner.jar script.js
```

## Contributing

When making changes:

1. Update the relevant documentation (and this index for new docs).
2. Add or update tests.
3. Keep `AGENTS.md` current — it is the canonical status document; only refresh
   the historical documents above when explicitly asked.
