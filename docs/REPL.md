# QuickJS-Scala REPL

**Version**: 0.2.0
**Last Updated**: 2026-09-19
**Status**: Phase 3 — interactive shell for the full engine

---

## Overview

The QuickJS-Scala REPL (Read-Eval-Print Loop) is an interactive JavaScript
shell. It is a thin layer over the same pipeline as the script runner — lexer →
parser → bytecode compiler → interpreter — so whatever the engine supports
(classes, destructuring, generators, async functions, the standard library, …)
is available at the prompt.

### Features

- **Expression results** — the value of the last expression is printed
- **`_` special variable** — the previous non-`undefined` result
- **Multi-line input** — automatically detected from unbalanced brackets
- **Persistent history** — stored in `~/.quickjs-scala-history`
- **Tab completion** — commands, keywords, common globals and their properties
- **Commands** — `.help`, `.load`, `.reset`, `.debug`, `.trace show`, `.vars`, …
- **Instruction tracing** — inspect bytecode execution with `.debug`
- **Structured errors** — source excerpt and JS stack trace
- **Standard library** — `console`, `JSON`, `Math`, `RegExp`, `Date`, `Map`,
  `Set`, `TypedArray`, `Intl`, `URL`, `atob`/`btoa`, `TextEncoder`/
  `TextDecoder`, `structuredClone`, … (through the `stdlib` entry point)

---

## Quick Start

### Running the REPL

```bash
# Recommended: full standard library + host globals
sbt "stdlib/run"

# Engine-only shell: runtime built-ins (Math, Array, String, ...) but no
# console/JSON/URL. Useful when embedding the REPL.
sbt "runtime/runMain quickjs.repl.REPL"
```

> `sbt "runtime/run"` does **not** work — the runtime module defines more than
> one main class, so sbt cannot pick one. Use `runMain` or the `stdlib` entry
> point.

### Basic session

```
$ sbt "stdlib/run"
QuickJS-Scala REPL v0.2.0
Type .help for help, .quit to exit

js> 1 + 2
3
js> var x = 42
js> x
42
js> _ + 8
50
js> [1, 2, 3].map(n => n * 2)
[2, 4, 6]
js> "hello".toUpperCase()
"HELLO"
js> Math.max(1, 5)
5
js> console.log("a", [1, 2, 3])
a [ 1, 2, 3 ]
js> .quit
```

Statements that produce `undefined` (`var x = 42`, loops, `if`, …) print
nothing; only the value of the last expression of an input is shown.

---

## Result Display

The REPL prints the last expression value with `PrettyPrinter.shortFormat`:

| Value | Display |
|-------|---------|
| `undefined` | nothing is printed |
| `null`, `true`, `42`, `3.14` | `null`, `true`, `42`, `3.14` |
| `"hi"` | `"hi"` (escaped and quoted) |
| `[1, "two", null]` | `[1, "two", null]` |
| `{a: 1, b: 2}` | `{a: 1, b: 2}` (up to 5 own properties, otherwise `{...N props...}`) |
| `function foo() {}` | `[Function: foo]` |
| `10n` | `10n` |
| `Symbol("s")` | `Symbol(id)` |
| `Promise.resolve(41)` | `Promise { 41 }` / `Promise { <pending> }` |
| generators | `[Generator]` |

Host objects that store their state in internal slots rather than JS-visible
properties (e.g. `Date`, `Map`, `Set`, native functions) are shown with their
internal representation (`{__dateValue: 0}`, …) rather than a browser-style
one. See **Limitations**.

---

## Persistent Bindings and Scoping

Each input is parsed and compiled as its own script. `var` and function
declarations write to the global object and therefore survive across inputs;
lexical declarations (`let`, `const`, `class`) are scoped to the evaluation
that created them:

```
js> var a = 1
js> a
1
js> function add(x, y) { return x + y }
js> add(2, 3)
5

js> let b = 10
js> b
ReferenceError: b is not defined
```

To use `let`/`const`/`class`, keep the declaration and its uses in the same
input (single line or a multi-line block):

```
js> let p = 1, q = 2; p + q
3
```

Top-level `return` is accepted as a REPL convenience:

```
js> return 5
5
```

---

## Multi-Line Input

The REPL switches to the continuation prompt when an input is incomplete:

```
js> function factorial(n) {
 ...   if (n <= 1) return 1
 ...   return n * factorial(n - 1)
 ... }
js> factorial(5)
120
```

Detection rules (`REPL.needsMoreLines` / `REPL.isComplete`):

- Input continues while it has more `{`, `(`, `[` than their closing
  counterparts, or ends with an opening bracket.
- Once bracket counts balance, the accumulated text is evaluated.
- Pressing Enter on an empty line while the bracket counts are balanced
  evaluates the buffer as-is.

The check is textual: brackets inside strings, comments or regular expressions
are counted too, and a line that ends with a binary operator is not treated as
incomplete. When in doubt, type the whole statement on one line or use
`.load`.

---

## Commands

Commands start with `.` in the first column. Unknown commands print
`Unknown command: … Type .help for available commands`.

| Command | Aliases | Description |
|---------|---------|-------------|
| `.help` | `.h` | Show the command list and a feature summary |
| `.quit` | `.exit`, `.q` | Leave the REPL (also `Ctrl-D`) |
| `.load <file>` | | Read a file and evaluate it in REPL mode (last expression printed) |
| `.reset` | `.clear` | Delete configurable global properties and re-install the host environment |
| `.debug` | `.trace` | Enable instruction tracing (prompt becomes `debug js>`), trace printed after each evaluation |
| `.nodebug` | `.notrace` | Disable instruction tracing |
| `.trace show` | | Print the accumulated trace |
| `.vars` | `.v` | Print a summary of common global variables |
| `.vars global` | `.vg` | Same as `.vars` (locals are unavailable outside a running execution) |
| `.bt` | `.backtrace`, `.stack` | Reserved for stack traces; currently prints “Stack trace tracking coming soon.” |

`Ctrl-C` abandons the current (possibly multi-line) input and shows a fresh
prompt without clearing the session state.

### `.load`

```
js> .load examples/language_tour.js
Loading: examples/language_tour.js
... (the tour prints its sections) ...
Language tour complete.
```

`.load` uses the raw file content and the REPL parser, so it does not support
ES module syntax (`import`/`export`); run modules with the
[script runner](RUNNING_NODE_SCRIPTS.md) instead.

### `.reset`

`.reset` deletes every *configurable* own property of the global object,
re-installs the standard library and host globals via the entry point’s
initializer, and clears `_`. Non-configurable bindings — `var`/function
declarations and some built-ins — cannot be deleted and survive, so restart
the REPL for a completely fresh context.

### Tracing

```
js> .debug
Debug mode enabled.
  Instructions will be traced as they execute.
  Use .nodebug to disable.

debug js> 1 + 1
2

Execution trace:
[0] PushI32
[5] PushI32 | stack: [1]
[10] Add | stack: [1, 1]
[11] Return | stack: [2]

debug js> .nodebug
Debug mode disabled.
```

Traces show the program counter, opcode and (up to three) stack and local
values per instruction; see `DebugTracer` in
`runtime/src/main/scala/quickjs/interpreter/DebugMode.scala`.

### Tab completion

`Tab` completes, from a static table (`REPLCompleter`):

- REPL commands (`.help`, `.load`, `.trace show`, …)
- JavaScript keywords and a few globals (`Array`, `Object`, `String`, `Math`,
  `Function`, `console`, …)
- Properties of `Math`, `Array`, `String`, `Number`, `console` and generic
  `toString`/`valueOf` members.

It does not introspect live variables or arbitrary objects.

---

## Error Reporting

Runtime and syntax errors are formatted by `ErrorHandler` with the offending
source line and the JavaScript stack trace:

```
js> notDefinedVariable
ReferenceError: notDefinedVariable is not defined
  // where: <repl>
  1| notDefinedVariable
  Stack trace:
    at <script> (<repl>:1:1)

js> throw new Error("boom")
Error: boom
  // where: <repl>
  1| throw new Error("boom")
```

`debugger;` statements are currently parsed as no-ops (no breakpoint support).

---

## Architecture

### Pipeline

```
Input line
    ↓
Lexer (tokenize)
    ↓
Parser (allowTopLevelReturn = true, source = line)
    ↓
Compiler.withREPLMode (last expression value is returned)
    ↓
Interpreter.call
    ↓
PrettyPrinter.shortFormat (display)
```

### Source layout

```
runtime/src/main/scala/quickjs/repl/
├── REPL.scala            # JLine loop, commands, evaluation, reset, history
└── REPLCompleter.scala   # static tab completion

runtime/src/main/scala/quickjs/interpreter/DebugMode.scala
                          # DebugTracer, VariableInspector, DebugCommand
runtime/src/main/scala/quickjs/diagnostic/ErrorHandler.scala
                          # error formatting with source excerpts

stdlib/src/main/scala/quickjs/stdlib/Main.scala
                          # recommended entry point (stdlib + console + host globals)
```

The `REPL` class takes an optional `reinitialize: () => Unit` callback used by
`.reset`; `quickjs.stdlib.Main` passes an initializer that installs `StdLib`,
`JSON`, `console` and the host globals.

---

## Testing

```bash
sbt "runtime/testOnly quickjs.repl.*"
```

5 suites, 40 tests:

| Suite | Tests | Covers |
|-------|-------|--------|
| `REPLTest` | 10 | pipeline evaluation, multi-line, functions, arrows |
| `REPLValueDisplayTest` | 5 | REPL-mode last-expression values |
| `REPLPersistenceTest` | 4 | `var`/function persistence across evaluations |
| `REPLImprovementsTest` | 9 | `PrettyPrinter` output and `console.log` |
| `REPLDebugTest` | 12 | `DebugCommand` parsing and `DebugTracer` |

---

## Limitations

1. **Lexical declarations do not persist** — `let`, `const` and `class` are
   scoped to the input that declares them. Use `var` for cross-input state.
2. **`.reset` cannot remove non-configurable bindings** — `var`/function
   declarations and some built-ins survive; restart for a clean session.
3. **No top-level `await`** — inputs are scripts, not async modules.
   `import`/`export` are syntax errors, and dynamic `import()` rejects with
   `no module loader configured` because the REPL entry points do not install a
   module loader. Use the script runner for modules.
4. **`.bt` is a stub** — stack traces are printed with errors, but there is no
   interactive backtrace command, stepping, or breakpoints.
5. **Bracket-counting completion** — multi-line detection is textual, not
   parser-driven (see above).
6. **Static completion** — live variables and arbitrary object properties are
   not completed.
7. **Value display** — host objects with internal slots (Date, Map, Set,
   native functions) print their internal representation.
8. **Host APIs are limited to the entry point** — the `stdlib` entry provides
   `console`, `JSON`, `URL`, `atob`/`btoa`, `TextEncoder`/`TextDecoder` and
   `structuredClone`, but no timers (`setTimeout`, …), no `require` and no Node
   built-ins. Use `--node` in the [script runner](RUNNING_NODE_SCRIPTS.md) for
   those.
9. **Single history file** — `~/.quickjs-scala-history` is shared by all
   sessions and stored in JLine's `timestamp:line` format.

---

## Comparison with Other JavaScript Engines

### vs Node.js REPL

| Feature | Node.js REPL | QuickJS-Scala REPL |
|---------|--------------|--------------------|
| Expression results | ✅ | ✅ |
| Multi-line input | ✅ | ✅ (bracket detection) |
| Tab completion | ✅ dynamic | ⚠️ static subset |
| Persistent history | ✅ | ✅ |
| `console.log` | ✅ | ✅ (`stdlib` entry) |
| Top-level `await` | ✅ | ❌ |
| Module imports | ✅ | ❌ (runner supports them) |
| `let`/`const` persistence | ✅ | ❌ |
| Node built-ins (`fs`, …) | ✅ | ❌ |

### vs QuickJS C (`qjs`)

| Feature | QuickJS C | QuickJS-Scala |
|---------|-----------|---------------|
| ES2024 support | Full | Most features (93.7% of executed test262 tests, Sep 2026) |
| Performance | Native | JVM (interpreted bytecode) |
| Memory usage | Minimal | Higher (JVM/GC overhead) |
| Type safety | C | Scala 3 (sealed types, exhaustive matching) |
| Debugging | GDB / remote debugger | Instruction tracing via REPL |
| Extensibility | C modules | Scala modules |

---

## Future Enhancements

- **Real stack traces** for `.bt`/`.backtrace`
- **Persistent lexical bindings** (`let`/`const`/`class`) across inputs
- **Top-level `await`** and module loading in the REPL
- **Dynamic completion** driven by live globals and object properties
- **Parser-driven multi-line detection** (continue editing until the input
  parses)
- **Syntax highlighting** and paren matching (colored errors/prompts exist
  today)
- **`.save <file>` / `.dump`** session and bytecode commands
- Timers and an event loop for `setTimeout`/`setInterval` at the prompt

---

## References

- **QuickJS C**: https://bellard.org/quickjs/
- **JLine**: https://github.com/jline/jline3
- **Scala 3**: https://docs.scala-lang.org/scala3/book/
- **Project docs**:
  - [`RUNNING_NODE_SCRIPTS.md`](RUNNING_NODE_SCRIPTS.md) — runner, Node mode, modules
  - [`CONFORMANCE.md`](CONFORMANCE.md) — conformance testing methodology
  - [`../AGENTS.md`](../AGENTS.md) — current status and sweep results
