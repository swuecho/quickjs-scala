# QuickJS-Scala - Claude Code Reference

## Project Overview

QuickJS-Scala is a JavaScript engine written in Scala 3 for the JVM, inspired by the QuickJS C implementation. The goal is to create a production-grade JavaScript engine with full ES2024+ support. 

**Before implement a feature, check the original c version first, should follow similar approach**
**When fixing a bug but not sure about the approach, check the original quickjs c version for ideas.**
**When the problem is tricky, create test step by step to help investigate, when done. keep the test**

**Current Status**: Phase 3 - Substantial language support with most ES2024 features. 1,341 tests passing, 0 failures. 15 test262 smoke test suites (all 100% of executed tests except one `Reflect` error). Full test262 sweep (aggregated from per-directory chunks, Sep 19 2026, after the examples/robustness round): 37,766/49,502 passing (93.7% of executed tests; 9,202 skipped by feature config; 165 failures, 2,334 errors, 35 timeouts) in ~3 minutes wall clock. The previous sweep was 37,753/93.7% with 2,499 non-passing; this round gained 13 passes and cut errors by 11 with zero regressions. 5 QuickJS C test files all passing. The Runner can execute ordinary scripts (including `.mjs` modules) — see "Scripting support" below. A new `examples/` suite (12 runnable scripts plus `run-all.sh`) exercises the engine and the Node layer end to end.

**REPL round (Sep 2026)** — the interactive shell was brought back in line with the engine and documented:
- `quickjs.stdlib.Main` (`sbt "stdlib/run"`, the documented entry point) now initializes the same environment as the script runner (`StdLib`, `JSON`, `console`, host globals) instead of only the legacy `ArrayStatics` + JSON, so `Math`, `String` methods, `console.log`, `JSON`, `URL`, `atob`/`btoa` and `structuredClone` all work at the prompt. `runtime/runMain quickjs.repl.REPL` installs the runtime built-ins too.
- JLine's history `!` event expansion is disabled: it silently ate backslashes, corrupting string escapes (`"a\nb"`) and regexp literals (`/\d/`) typed at the prompt.
- Persistent history is actually enabled by setting JLine's `history-file` variable to `~/.quickjs-scala-history` (the old docs claimed it, but the variable was never set, so history was in-memory only).
- `.trace show` is reachable (the command parser matched `.trace` first and re-enabled tracing instead), `.reset` re-installs the host environment through a new `reinitialize` hook, and `.vars` no longer prints its header twice.
- `docs/REPL.md` rewritten (expression results, commands, scoping, display, limitations); `docs/README.md`, `docs/DEBUGGER_SUPPORT.md` and `docs/PROGRESS.md` cross-references refreshed.
- Regression coverage: 1 new `REPLDebugTest` test (`.trace show`); full suite now 1,341 tests, 0 failures.

**Examples and robustness round (Sep 2026)** — `examples/` gained 10 runnable scripts (`language_tour.js`, `async_patterns.js`, `web_globals.js`, `esm-demo/`, `node_cli.js`, `http_server.js`, `streams_pipeline.js`, `crypto_toolkit.js`, `child_process.js`, plus `run-all.sh` and `README.md`). Writing them surfaced six engine/host bugs, all fixed with regression coverage:
- **Destructuring assignment to private fields** (`[this.#w, this.#h] = pair`) failed to compile with "Unsupported property key: PrivateIdentifier": the destructuring target path only emitted `setProp`. It now emits `setPrivateField` when the target is a private name (the `[obj, value]` stack shape is identical).
- **`queueMicrotask` never ran its callback for plain global calls**: the helper read `args(1)`, but global calls do not prepend a receiver, so the callback was `undefined` and silently dropped (method-style calls worked). It now accepts both shapes and throws `TypeError` for non-callable arguments.
- **`for..of` over an async-only iterable hung forever**: `%AsyncGeneratorPrototype%` inherited `%IteratorPrototype%`, whose sync `@@iterator` returns `this`, so a sync for-of loop called the async generator's `next()` and spun on the returned promise with `done === undefined`. Added a distinct `%AsyncIteratorPrototype%` (`[[Prototype]]` of `%AsyncGeneratorPrototype%`, own self-returning `@@asyncIterator`, no sync `@@iterator`); `for..of` over an async generator now throws `TypeError: value is not iterable`, matching test262's `AsyncIteratorPrototype` expectations.
- **`Intl.Segmenter` word segments now expose `isWordLike`**: the ICU4J `BreakIterator`'s `getRuleStatus()` classifies each boundary (letters/digits/ideographs true; whitespace/punctuation false), matching V8. Other granularities omit the property.
- **`TextEncoder.encodeInto` wrote nothing**: it used `Reflect.set(dest, index, byte)`, which does not update a typed array's backing buffer, ignored the destination length (it wrote to 4096) and reported `read = source.length` even when truncated. It now writes UTF-8 bytes directly into the view's backing buffer, stops before a code point that does not fit or at a lone surrogate, and requires a real `Uint8Array` destination (`TypeError` otherwise).
- **Host errors created through `Globals.jsError` were not real error types**: they were base `Error` objects with a renamed `name`, so `e instanceof TypeError` was false. Standard names now construct through the realm's error constructor; host-specific names (`InvalidCharacterError`) keep the rename fallback.
- `HttpServer`, `fetch`, streams, `child_process`, `crypto` and the ESM loader were exercised end to end by the new scripts. Regression coverage: 4 new `ConformanceRegressionTest` tests (private destructuring, `queueMicrotask`, async-iterator prototype/no-sync-iteration, `Segmenter.isWordLike`) and 1 `ScriptingSupportTest` test (`encodeInto`); full suite now 1,340 tests, 0 failures.

**Interpreter performance round (Sep 2026)** — call-heavy code is ~3-4x faster, `sbt test` dropped from ~26-30s to ~20s, and the test262 smoke suites no longer time out (Math went from a 30s munit timeout to 0.8s). The changes are all compile-time sizing or local fast paths; no architectural rewrite.
- **Exact operand-stack sizing** (`compiler/.../bytecode/StackAnalysis.scala`): every function used to request a 4096-slot `JSValue` stack (32KB per call). A worklist over the compiled instructions now computes the maximum depth reachable through normal and exception paths (`TryStart` registers its catch/finally blocks as extra edges, whose entry depth is the try-entry depth because the runtime restores `stackTop`), with a `+8` slack and a 4096 fallback for opcodes the analysis does not model or a stack-positive cycle. Guards: `maxDepth > 4096` or a step cap returns `-1` (fallback) so a compiler bug can never hang the build. Generator functions keep a 256-slot floor because the generator resume loop owns its own suspend/resume accounting.
- **Exact local-slot frames**: the fixed 256-slot locals array allocated 256 `VarRef`s per call; frames now size from the slot-indexed `getAllLocalVarNames` (`max(16, slots + 8)`). The eval frame and generator `vars` array use the same bound.
- **The `arguments` object is created only when observable**: `BytecodeFunction.referencesArguments` is set when the body (or a nested closure / parameter default / direct eval) can mention `arguments`; the interpreter skips building the mapped arguments object otherwise. `Function`/`BytecodeFunction` carry the flag through `doCall`/`constructValue`/generators.
- **Per-call lookups removed**: `$this`/`$newTarget` closure lookups are skipped when the closure map is empty, and `JSContext.globalObjectValue` caches the `JSValue.Object(global)` wrapper used for sloppy-mode `this`.
- **`for-in` stack leak fixed** (found by the new analysis): the enumerability guard pushed `keys[index]`, the helper call consumed it, and the binding recomputed it — leaving one operand on the stack per iteration. Loops silently degraded after ~4096 keys (previously masked by the 4096-slot frames). `for (var k in o)` over 100k keys now works; a regression test covers 20k keys.
- **Numeric fast paths** (mirroring QuickJS's tagged-int arithmetic): `Add`/`Sub`/`Mul` keep `Int32` when the result fits and widen to the exact `Float64` otherwise; `PreInc`/`PostInc`/`PreDec`/`PostDec` update `Int32`/`Float64` in place; `<`/`<=`/`>`/`>=` compare numeric operand pairs directly (`compareOperands`) instead of running the full abstract relational comparison. NaN and `-0` behavior is preserved.
- **`Array` spread reads iterator results through `[[Get]]`** (`IteratorComplete`/`IteratorValue`): `...iter` over an iterator whose `value`/`done` getters throw used to loop forever (raw `.get` bypassed the accessor, so a poisoned iterator never terminated and abandoned test262 workers OOMed the sweep). Same fix for `yield*` in `GeneratorSupport`, where a throwing getter is now routed through the generator's own `try`/`catch` handlers (+2 test262 tests). `yield*` still reads `value` eagerly, so `star-rhs-iter-nrml-res-done-no-value.js` (which expects lazy value access) remains failing.
- **Measured** (`scripts/bench-micro.js`): 500k plain calls 1931ms → 435ms, method calls 1954ms → 605ms, closure calls 1721ms → 426ms, `fib(20)` 78ms → 18ms, property access 615ms → 575ms. The interpreter dispatch and boxed `JSValue` arithmetic remain the next ceiling (a full unboxed representation would be an architectural change).
- Regression coverage: 3 new `ConformanceRegressionTest` tests (for-in over 20k keys, spread getter invocation, exact `stackSize`); `InterpreterPerfRegressionTest` now warms up twice and takes the best of up to three measured runs so parallel suites cannot measure a cold, interpreted loop. Full suite now 1,333 tests, 0 failures.

**Compile-time O(n^2) and staging round (Sep 2026)** — large functions compile ~50x faster, the full sweep runs in ~3 minutes, and abandoned timeout tests no longer leak CPU/heap:
- **Bytecode offsets are tracked while emitting** (`InstructionBuffer.byteLength`/`byteOffsetOf`): the compiler recomputed `instructions.foldLeft(0)(_ + _.size)` at 54 patch sites and `slice(...).map(_.size).sum` at 3 more, making compilation quadratic on large bodies (a 35 KB file of 1,833 `try {} catch(e) {}` statements took ~22s). `currentBytecodePos`/`bytecodePosAt` are now O(1) for compiled buffers (`addOne`/`update` maintain the running size; jump placeholders are patched with same-size instructions). 3,600 statements went 72s → 1.4s; `staging/sm/regress/regress-561031.js` went from a 5s timeout to ~1s and now passes.
- **Catch clauses no longer leak their compile-time block scope**: `compileStatement(TryStatement)` entered a `Scope.enterBlockScope()` and never called `leaveBlockScope()`, so every later lookup scanned every previous catch block (`activeBlocks.contains`, boxed Ints) and sibling bindings stayed visible. The runtime name-based local fallback in `resolveGetGlobalValue`/`resolveDeleteName` is now gated on `withStack.nonEmpty`, so `try {} catch (e) {} return e` correctly throws ReferenceError instead of returning the caught value. Errors dropped from 2,353 to 2,345 in the sweep.
- **Native loops honor thread interrupts**: `__destructureArray` and `appendSpreadSource` now call `BuiltinHelpers.checkInterrupted` every 1024 iterations. An infinite iterator being destructured used to keep allocating millions of elements after the 5s timeout abandoned the test (the OOM source in earlier sweeps); the worker now dies promptly.
- **Rest assignment targets are evaluated before iterator consumption** (`[...obj[key]] = source`): the rest element's target reference is hoisted into temps before `__destructureArray`, so an abrupt completion in the target cannot be preceded by unbounded iteration (`staging/sm/destructuring/array-iterator-close.js` no longer hangs/OOMs; it still fails an assertion because IteratorClose-on-abrupt is not yet emitted).
- **String operands are cached per frame** (`BytecodeLoop.stringAt`): `readString` decoded UTF-8 and allocated a String on every `GetProp`/`GetGlobal`; a small direct-mapped table keyed by byte offset removes that allocation (property-access benchmark 585ms → 554ms).
- **test262 worker oversubscription**: `scripts/test262-chunks.sh` defaults to 1.5x the core count (`TEST262_WORKERS` overrides; the runner cap is now 2x cores). A timed-out test holds its worker for the full timeout, so extra workers overlap the waits: staging 46s → 41s, built-ins 59s → 55s, language unchanged, full sweep ~187s → ~183s.
- Remaining staging time is dominated by inherently heavy tests that need deeper call-path work: the eight `Date/dst-offset-caching-*.js` fragments (~17s each, 4x the 5s cap), `TypedArray/sort_large_countingsort.js`, `Array/toSpliced-dense.js` and `RegExp/unicode-class-braced.js`.
- Regression coverage: 2 new tests (3,000-statement compile throughput, catch bindings do not leak). Full suite now 1,335 tests, 0 failures.

**Super property and home-object round (Sep 2026)** — +76 test262 passing (93.5% → 93.7%), zero regressions:
- **Object-literal methods have a `[[HomeObject]]`**: a per-literal cell (allocated only when some method/accessor references `super`, so spread-heavy code does not retain literals) is captured by the methods and filled once the object exists. `super.prop` reads the home object's `[[Prototype]]` at access time, so a later `Object.setPrototypeOf` is observed. Base-class methods (`class C {}` with no `extends`) use the same mechanism — instance methods home on `C.prototype`, static on `C` — which also makes `super.x` work in classes without heritage. `language/expressions/super` went 50 → 85 passing.
- **Super writes go through the receiver**: `super.prop = v`, compound assignment and `++`/`--` compile to `__setSuperProp(base, key, value, this, strict)`, which reuses `Reflect.set` (OrdinarySet with a distinct receiver): the lookup starts at the base but the property is created/updated on `this`, frozen/non-writable receivers throw in strict mode, and accessor setters run. `this` is read before the base and key (`GetThisBinding`), so an uninitialized derived `this` throws ReferenceError before a poisoned computed key is evaluated. `super.method(...)` uses `__getSuperProp` with `this` as receiver (accessor-aware).
- **`super()` construct semantics**: `__funcSpread` takes a construct flag; a non-constructor super throws TypeError (including native methods), the object returned by the parent constructor replaces the frame's `this` (new `SetThis` opcode), and the new `MarkSuperCalled` opcode implements the per-frame "super constructor may only be called once" ReferenceError.
- **`__objectSpread` implements CopyDataProperties**: getters are invoked with the source as receiver and copied as data properties, own enumerable symbols are copied, arrays contribute their indices and symbols, and string primitives contribute their code-unit indices. This fixed the `call-spread-obj-*` tests and several object-spread tests; previously getters spread as `undefined`, symbols were dropped and array/string sources copied nothing.
- Remaining `super` gaps (6 tests): direct `eval` inside a method cannot see the enclosing `super` (4), `super()` uses the captured superclass instead of the dynamic `GetSuperConstructor()` when the class prototype is changed later, and `new.target` is not forwarded to the parent constructor.
- Regression coverage: 3 new `ConformanceRegressionTest` tests (home-object reads/writes/increment/base-class/static, `super()` rebinding and double-call, spread getters/symbols/arrays/strings). Full suite now 1,330 tests, 0 failures.

**String protocol dispatch round (Sep 2026)** — +60 test262 passing (93.3% → 93.4%), zero regressions:
- **`String.prototype.match/search/replace/replaceAll/split/matchAll` follow the 2025 dispatch rules**: after `RequireObjectCoercible`, `GetMethod(argument, @@symbol)` is only consulted when the argument is an **Object**, so primitives are not boxed and getters installed on `String.prototype`/`Number.prototype` are not triggered. The native fast paths remain for non-dispatching arguments.
- **`split` passes the original receiver to `@@split`** and calls `ToString(separator)` before the `lim == 0` check and before the `separator is undefined` shortcut. Empty cases match V8 (`''.split('') === []`, `''.split(',') === ['']`, `'a,'.split(',') === ['a','']`), and the native regexp branch was removed so every regexp split goes through the new `RegExp.prototype[@@split]`.
- **`replaceAll` passes the receiver to `@@replace`** and performs `IsRegExp` + the non-global `flags` check before `ToString(this)`/`ToString(searchValue)`; `Symbol.match` is read exactly once (a real RegExp uses its inherited `RegExp.prototype[@@match]`).
- **`matchAll`** checks `IsRegExp` + global only for Object arguments and creates the fallback RegExp with the `g` flag.
- **`replaceAll` checks `IsRegExp` + the global flag before the `@@replace` lookup** and, when `@@replace` is absent/undefined, falls back to literal replacement of `ToString(searchValue)` (a RegExp with `Symbol.replace` deleted behaves like `str.replaceAll('/./g', …)`). Functional callbacks only receive the trailing `namedCaptures` argument when one exists, so string-pattern callbacks get `(matched, position, string)` with `arguments.length === 3`.
- **`__funcSpread` accepts native methods**: `super[Symbol.replace](...args)` inside a class method threw "Super constructor is not a constructor" because the spread helper only handled user functions and native constructors; it now calls a `NativeFunction` with the receiver prepended.
- Fixed 48 `built-ins/String/prototype` tests (the `cstm-*-primitive` protocol family, `searchValue-replacer-*`, split ordering/empty-string cases, the `getSubstitution` string fallback) plus 21 `staging/sm` tests. 53 `String/prototype` errors remain, mostly `match` (13, `index` on fallback matches) and scattered coercion cases.
- Regression coverage: 3 new `ConformanceRegressionTest` tests (primitive non-boxing, split receiver/order, replaceAll receiver; `super` spread with native methods; replaceAll string fallback and callback arity). Full suite now 1,327 tests, 0 failures.

**RegExp protocol round (Sep 2026)** — +170 test262 passing (92.9% → 93.3%), zero regressions:
- **Generic `RegExp.prototype[Symbol.match/search/replace/split/matchAll]`** (ES2024 22.2.6) replace the old delegation to the String methods, so a non-RegExp receiver's observable `exec`, `flags`, `lastIndex`, `constructor` and `Symbol.species` protocol is followed and all errors propagate. All five directories are now 100% of executed tests: replace 70/70, match 78/79, search 23/23, split 43/43, matchAll 26/26 (the one failure needs code-unit `.`/surrogate matching that Java's regex cannot express). This fixed 45+34+25+13+9 tests plus 6 `RegExp/prototype/flags`.
- **`GetSubstitution`** (the `$&`, `` $` ``, `$'`, `$n`/`$nn`, `$<name>` rules) operates on materialized captures with QuickJS's `$nn` bounds refinement; captures are ToString-ed eagerly and `groups` is ToObject-ed, so the `result-*-err` tests see their poisoned getters. A custom `exec` result with a `matched` longer than the input no longer overflows `substring`.
- **`flags` reads its component properties**: `get RegExp.prototype.flags` composes `hasIndices/global/ignoreCase/multiline/dotAll/unicode/unicodeSets/sticky` with `[[Get]]` and a new `unicodeSets` getter exists, so own property overrides and poisoned getters are observed (6 tests).
- **`Set(receiver, "lastIndex", v, true)`** (accessor setters, TypeError for missing setter or non-writable data property) is used by `exec`/`test` and all symbol methods, including the built-in fallback path.
- **Sticky matching anchors exactly**: `exec`/`test` require `matcher.start() == lastIndex` instead of Java's region/lookingAt (which would re-anchor `^`), fixing `/^a/y.test(' a')` and the `y-*` cases.
- **RegExp construction applies `IsRegExp`**: a non-[[RegExpMatcher]] object with a truthy `Symbol.match` contributes its `source`/`flags` properties, and other patterns use JS `ToString` (not the Scala `toString`), so `Symbol.match` getters, `toString` throwing and the observable read order are respected (3 matchAll tests, `RegExpCreate`).
- **`matchAll` uses SpeciesConstructor + Construct** and supports `get-constructor`/`get-species` errors, non-object constructors and non-constructor species.
- **Engine fix found en route**: `BuiltinHelpers.getPropertyWithGetter` returned `undefined` for an array's `length` (it is a virtual property with no own descriptor), which broke the capture count and `args.length` in the replace protocol; it now returns `getLength`. This also fixed 8 `Array/prototype/reduce`/`reduceRight` tests.
- Regression coverage: 4 new `ConformanceRegressionTest` tests (exec-protocol replace, observable flags/custom exec match, sticky anchoring, species-constructor split). Full suite now 1,324 tests, 0 failures.
- Known remaining `String.prototype.match/replace/search/split/matchAll` gaps: the String methods still use their native fast paths and do not dispatch through `IsRegExp` + the receiver's `@@match`/`@@replace`/... (73 errors), so `String.prototype.*` protocol tests remain a separate cluster.

**Module instantiation round (Sep 2026)** — +9 test262 passing (92.8% → 92.9%), zero regressions:
- **Exported declarations are instantiated with the module**: `collectVarNames` now traverses `ExportNamedDeclaration` and `ExportDefaultDeclaration(statement)`, so `export var v` creates its binding (undefined) before evaluation and an assignment before the declaration no longer resolves to a global. Exported function declarations — including a named `export default function f(){}` — are defined and exported during instantiation, so the module itself (or a self-importer) can call `f()` before the declaration statement is reached. Fixes `instn-local-bndng-export-fun/gen/var` and `verify-dfs`.
- **Negative module tests resolve fixtures correctly**: `runNegativeTest` now roots the `FileModuleLoader` at the test's directory and registers the entry module under its absolute path, exactly like `runRegularTest`. Every negative module test with a relative fixture import used to fail with `Module not found: <cwd>/language/...`; fixes the `top-level-await/module-import-rejection*` tests and `eval-rqstd-abrupt`.
- **CR-only frontmatter**: `parseFrontmatter` normalizes CR/CRLF before splitting into metadata lines, so `includes:` is read for tests whose source uses Carriage Return line terminators.
- Regression coverage: 2 new tests (`Test262Test` CR-only frontmatter, `FileModuleLoadingTest` exported function/var instantiation). Full suite now 1,320 tests, 0 failures.
- Known remaining module gaps: imported bindings are still snapshots taken when the `import` statement executes, so a self-importing module cannot call an imported function before that statement (`instn-named-bndng-fun`, `instn-named-bndng-dflt-fun-*`, `*-gen-*`) and assignments to imported `const` bindings before the statement do not throw (`instn-named-bndng-var`, `instn-iee-bndng-var`); imported `let`/`const` TDZ across cycles is not enforced either.

**Member reference and private-name lexing round (Sep 2026)** — +54 test262 passing (92.7% → 92.8%), zero regressions:
- **Compound assignment captures the member reference** (`Compiler`): `obj[prop] op= rhs` and `obj.prop op= rhs` now evaluate the base and key once into temp locals, run `RequireObjectCoercible(base)` before `ToPropertyKey(property)`, then do GetValue → RHS → operator → PutValue against that same base/key. Previously the member was compiled twice (once inside the desugared binary right-hand side and once as the assignment target), so base/key expressions, getters and `toString` ran twice, the RHS was evaluated before the reference, and a null base converted the key before throwing. This fixes all `S11.13.2_A7.*` cases plus evaluation-order tests in `logical-assignment`, prefix/postfix increment/decrement and `member-expression` (38 of the 49 `compound-assignment` failures).
- **`%` uses fmod semantics** (both `BytecodeLoop` and `GeneratorSupport`): the hand-rolled `n - trunc(n/d)*d` produced `+0` for `-1 % -1`, lost `-0 % 3`'s sign and returned NaN for `x % Infinity`. Java's double `%` is fmod — zero keeps the dividend's sign and `x % Infinity == x` — matching `Number::remainder`. Also fixed `language/expressions/modulus` and `mod-whitespace`.
- **RequireObjectCoercible precedes ToPropertyKey** in `doGetElem`/`doSetElem`, so `null[throwingKey]` throws TypeError without invoking the key's `toString`.
- **Private identifiers end an expression for regex disambiguation**: `this.#x /= 2` (and `%=`) is a divide-assign, not the start of a regexp. `isRegexpAllowed` returned true after a `PrivateIdentifierToken`; it now returns false like for ordinary identifiers.
- Known remaining: the 11 `compound-assignment/S11.13.2_A6.*_T1` tests require capturing the LHS reference cell before a direct `eval()` in the RHS introduces a shadowing binding (QuickJS gets this for free because closure references are slot-based).
- Regression coverage: 3 new `ConformanceRegressionTest` tests (single evaluation/order of compound member references, null-base TypeError ordering, fmod remainder, private divide-assign). Full suite now 1,318 tests, 0 failures.

**Lexical conformance round (Sep 2026)** — +75 test262 passing (92.5% → 92.7%), 3,014 → 2,939 non-passing, zero regressions:
- **`raw` tests execute the file verbatim**: `Test262Runner.runTest` now evaluates the original source for tests with `flags: [raw]` instead of only the text after the frontmatter. This fixed all 17 `language/comments/hashbang` tests (escaped `#`/`!`, preceding whitespace/comments/statements, multiple hashbangs, line terminators) and is how the test262 harness defines `raw`.
- **Parser-directed regex literals after `}`** (mirrors QuickJS's `js_parse_primary` rewind): `Lexer.scanRegExpLiteral(source, start)` scans a literal at an explicit position and `parsePrimaryExpression` reinterprets a `Div`/`DivAssign` token at a primary position as a regex, skipping the tokens the eager lexer produced inside it. The lexer's previous-token heuristic still guesses division after `}` (QuickJS's `is_regexp_allowed` does the same), but the parser recovers. The optional `source` parameter on `Parser` is threaded through every engine entry point (direct/indirect eval, `Function`, modules, REPL, `vm`, timers, CommonJS wrapper, Runner, Test262Runner). Fixes all 25 `language/white-space/after-regular-expression-literal-*`.
- **Line terminators**: `advance()` counts U+2028/U+2029 as line breaks; comment scanning stops at all four terminators; `<ZWNBSP>` (U+FEFF) is whitespace; the hashbang skip ends at LS/PS; `skipLineComment` checks the position rather than the NUL sentinel, so a real U+0000 inside a comment no longer ends it (`language/comments/S7.4_A5`).
- **Use Strict directives must be exact**: `'use str\<newline>ict'` and `"use\x20strict"` are ordinary string literals, not directives. `isPlainStringLiteral` checks the raw source slice for a backslash; both the token-level `isUseStrictDirectiveAhead` and the statement-level `extractStrictMode` use it. Fixes `language/directive-prologue/14.1-4-s.js` and `14.1-5-s.js`.
- **Frontmatter line endings**: `parseFrontmatter` normalizes CR/CRLF before splitting, so CR-only test files (e.g. `Function/prototype/toString/line-terminator-normalisation-CR.js`) get their `includes:` injected (previously the whole body was one comment, so the test passed vacuously).
- **Empty eval fast path**: direct and indirect eval sources with an empty statement list return `undefined` without compiling, which keeps the 65k-eval loop in `language/comments/S7.4_A5.js` inside the 5s per-test timeout.
- Regression coverage: 5 new `ConformanceRegressionTest` tests (regex after `}`, hashbang terminators and negative forms, ZWNBSP/LS/PS, NUL in comments, escaped `use strict`). Full suite now 1,315 tests, 0 failures.

**Import attributes, JSON modules, WeakRef and iterator helpers round (Sep 2026)** — +727 test262 passing, 9,948 → 9,202 skipped, 92.4% → 92.5%:
- **Flags enabled**: `import-attributes`, `json-modules`, `WeakRef`, `FinalizationRegistry`, `iterator-helpers` and `host-gc-required`; the `WeakRef`/`FinalizationRegistry` directory excludes were removed. The focused runs are 100% of executed tests: import attributes 89/90 (1 `cross-realm`), WeakRef 27/29 (2 skipped), FinalizationRegistry 46/47 (1 skipped), iterator helpers 393 executed/393 (the other 117 are `iterator-sequencing`/`joint-iteration` and `Symbol.dispose`, still skipped). `host-gc-required` staging tests are 6/15.
- **Import attributes** (`with { key: "value" }`, ES2025): `ImportAttribute` AST nodes plus `attributes` fields on `ImportDeclaration`/`ExportNamedDeclaration`/`ExportAllDeclaration`; the parser accepts IdentifierName or string keys and string values, allows a leading line terminator, rejects duplicate keys and non-string values. The compiler materializes the clause with a new `__makeImportAttributes` helper and passes it to `__moduleInstantiate(specifier, attributes, names…)`/`__moduleImport(specifier, attributes)`.
- **JSON modules**: `FileModuleLoader` treats a resolved module as JSON when the `with` clause says `type: "json"` or the file name ends in `.json` (matching quickjs C's `js_module_test_json`). JSON modules are parsed with `%JSON.parse%` at instantiation time (invalid JSON is a resolution SyntaxError), expose only a synthetic `default` export (named bindings are a link error), and the parsed value is cached so every import site — static, namespace, indirect or dynamic — sees the same object. `JSON.parse` now creates objects with `Object.prototype` (not a null prototype) and defines keys via CreateDataProperty so `__proto__` is an ordinary property.
- **Dynamic `import(spec, options)`**: the options expression is evaluated (and may suspend or throw) before the import job is created; `__dynamicImport` implements the ES option processing — `options.with` must be an object, its own enumerable string-valued entries are enumerated with `EnumerableOwnPropertyNames` semantics (proxies go through `ownKeys`/`getOwnPropertyDescriptor`), non-string values reject with TypeError, and failures after expression evaluation become rejections. The specifier is `ToString`-ed before options validation.
- **WeakRef / FinalizationRegistry**: `CanBeHeldWeakly` rejects `Symbol.for` registered symbols (non-registered symbols are accepted) in both constructors and unregister tokens. Both constructors now use `BuiltinHelpers.initConstructor` (so `Object.getPrototypeOf(WeakRef) === Function.prototype` and `length`/`name`/`prototype` descriptors are right) and implement `constructWithNewTarget` = `OrdinaryCreateFromConstructor`, invoking the `NewTarget.prototype` accessor with the proper receiver and falling back to the intrinsic prototype when it is not an object (including bound-function NewTargets).
- **`$262.gc`** is installed by the test262 host (best-effort `System.gc()`/`runFinalization()` cycles) for the `host-gc-required` tests.
- **Iterator helpers** (`runtime/builtins/IteratorHelpers.scala`): `%Iterator%` is a subclassable abstract constructor (`Iterator()`/`new Iterator()` throw, `class X extends Iterator` works via `superInitImpl`), with own `from` and a `prototype` whose `constructor` and `[Symbol.toStringTag]` are accessor properties whose setters ignore prototype properties (`SetterThatIgnoresPrototypeProperties`). `Iterator.from` handles strings (without boxing before the `@@iterator` lookup so the getter still sees a primitive), other primitives, iterables and bare iterators, returning the object unchanged when `%IteratorPrototype%` is already in its prototype chain and otherwise wrapping it in `%WrapForValidIteratorPrototype%` (`next`/`return` forward to the captured record).
- **Lazy helpers** (`map`/`filter`/`take`/`drop`/`flatMap`) are native state machines hung off `%IteratorHelperPrototype%`, not JS generators: argument validation failure closes the underlying iterator even though `next` was never read; `take`/`drop` convert the limit and close on `NaN`/negative/abrupt `ToNumber`; `return()` closes the underlying iterator even before the first `next` and ignores its argument; re-entrant `next()` throws TypeError (the running-helper guard); mapper/predicate errors run `IteratorClose` with a throw completion (close errors are swallowed and the original error is rethrown); `flatMap` uses `GetIteratorFlattenable` with primitives rejected, closes the inner iterator before the outer one, and never re-closes an already-exhausted inner iterator. The eager `reduce`/`toArray`/`forEach`/`some`/`every`/`find` share the same `IteratorStepValue`/`IteratorClose` primitives; `some`/`every`/`find` close the iterator when they short-circuit.
- **Engine fixes found while enabling the features**: the template-expression scanner now handles regex literals (tracking the previous significant token to distinguish `/` division) and nested `${…}` templates inside `readTemplateLiteralRaw`, so `temporalHelpers.js` parses; `Array.from`'s iterator path uses accessor-aware `[[Get]]` (`getPropertyWithGetter`), which removed a real infinite loop/hang when a `value` getter throws; `delete <non-reference>` evaluates the operand and returns true instead of emitting a `Delete` opcode for a missing object (it used to crash the interpreter with `Index -1 out of bounds`); `getSymbolPropertyWithGetter` invokes proxy `get` traps and passes primitive receivers to wrapper accessors; the generator VM gained `Mod`/`Pow`/`Neg`/`Not`/`LNot`/`And`/`Or`/`Xor`/`Shl`/`Sar`/`Shr` (BigInt-aware) so generator bodies using those operators no longer abort with "Unimplemented generator opcode".
- Regression coverage: 8 new `ConformanceRegressionTest` tests (import attributes parse/duplicate keys, WeakRef registered symbols + NewTarget prototype, iterator helper pipelines and close semantics, `Iterator.prototype` accessors, template expression scanning, `Array.from` throwing value getters, `delete` of a non-reference) and 1 new `FileModuleLoadingTest` (JSON module imports, named-binding rejection, invalid JSON resolution error). Full suite now 1,310 tests, 0 failures.

**Class static blocks, generator constructors and async conformance round (Sep 2026)** — +368 test262 passing, 10,205 → 9,948 skipped, 92.1% → 92.4%:
- **Flags enabled**: `caller`, `u180e`, `class-static-block`, and the `AsyncFunction`/`AsyncGeneratorFunction`/`AsyncGeneratorPrototype`/`AsyncFromSyncIteratorPrototype` directories plus `language/expressions/await` are no longer excluded. The async directories run 146/146 executed tests (3 `cross-realm` cases remain skipped), `class-static-block` is 63/63 and `caller`+`u180e` are 45/46 (the remaining `without-dotall.js` case needs code-unit `.` against a surrogate pair; Java's regex matches code points).
- **Legacy `caller`/`arguments`**: `%ThrowTypeError%` now mirrors QuickJS's `js_throw_type_error` — reading `caller`/`arguments` on a non-strict ordinary function that owns a `prototype` returns `undefined`, while `Function.prototype` itself, strict functions, generators, async functions, methods/arrows, bound functions and all writes throw. A single function serves as both getter and setter (the setter is recognized by its extra argument), preserving the `%ThrowTypeError%` identity invariant.
- **Class static blocks** (`StaticBlock` AST node): the parser treats `static { ... }` as a function-like boundary with its own `var`/lexical scope (an `arguments`/`return`/`await`/`yield` early error, `super.x` allowed but not `super()`, and duplicate-lexical validation). Nested ordinary functions and methods escape the restrictions; arrow parameters inherit them while arrow bodies do not. The compiler compiles each block with `compileFunctionBody("<static>", …, isStrict = true)` and immediately invokes it with the constructor as `this`, interleaved with static field initializers in source order. The body sees class private names and the static `super` base.
- **`%GeneratorFunction%`, `%AsyncFunction%`, `%AsyncGeneratorFunction%` constructors**: all three now exist (none is a global; they are reached through `.constructor`), create functions from source (`buildFunctionRaw` supports the async/generator combinations) and have `%Function%` as `[[Prototype]]`. `%AsyncFunction.prototype%` was added to `JSContext`; async function values now chain to it instead of `Function.prototype`. Constructor properties are non-writable, and `JSObject.getPrototypeValue` maps constructor `funcObj`s back to their wrappers via `__nativeCtor`.
- **Shared generator prototypes**: `Interpreter.installGeneratorPrototypes` installs `next`/`return`/`throw`/`@@iterator`/`@@asyncIterator`/`constructor` on `%GeneratorPrototype%` and `%AsyncGeneratorPrototype%`, and generator objects are created with the callee's `funcObj` so the function's `prototype` is read *after* parameter initialization (the `generator-created-after-decl-inst` cases). Iterator result objects now get `Object.prototype`, async generator methods return rejected promises for brand-check failures, and `%AsyncGeneratorPrototype%[@@toStringTag]` is registered.
- **Class code is strict**: class constructors, methods and static blocks are compiled with `isStrict = true` (previously the runtime flag was false even though the parser enforced strict syntax). This fixed the `restricted-properties` tests, strict `this` handling and `caller` behavior for classes.
- **Static `super.x` was broken in general**: `__getSuperProp` only accepted plain `JSObject` bases, so every static-method super property read returned `undefined`. It now accepts constructor/native/function bases too. The class `prototype.constructor` link is created with CreateDataProperty semantics, so extending `%GeneratorFunction%` works despite its non-writable `constructor`.
- **Parser depth semantics**: `await`/`yield` are expressions only in the innermost async/generator function — nested ordinary functions treat them as identifiers (`await-in-nested-function`); `await` in async formal parameters is an early error; function-declaration names are parsed in the enclosing scope so static-block binding restrictions apply; builder sources are parenthesized so `async function(...)` parses as an expression.
- **Known remaining failures from this round**: `language/module-code/eval-rqstd-order.js` (evaluation order for `export * as ns`), `built-ins/RegExp/dotall/without-dotall.js` (non-unicode `.` matching one half of a surrogate pair). The exact code-unit `.` rewrite was tried and reverted because long `.*` matches overflowed the regex engine's stack; the simple `[^\n\r\u2028\u2029]` class stays fast.
- Regression coverage: 3 new `ConformanceRegressionTest` tests (static-block ordering/scope/early errors, generator/async constructors plus prototype chains and `caller`, `await`/`yield` as identifiers in nested functions). Full suite now 1,301 tests, 0 failures.

**ES2025 built-ins round (Sep 2026)** — +361 test262 passing, 10,558 → 10,205 skipped, 92.0% → 92.1%:
- **Already-implemented features enabled in `test262.conf`** (353 more tests executed): `set-methods`, `upsert`, `array-grouping`, `promise-try`, `Error.isError`, `RegExp.escape`, `export-star-as-namespace-from-module`. The focused new-feature suite (Map/WeakMap upsert, Object/Map groupBy, Promise.try, Error.isError, RegExp.escape, Set methods) runs 327/327 executed tests at 100% (3 cross-realm cases remain feature-skipped).
- **Map/WeakMap upsert**: `getOrInsert` and `getOrInsertComputed` on both prototypes. Keys canonicalize `-0` to `+0` (`normalizeMapKey` is used by `JSMapStorage` and `JSSetStorage` too, so `map.keys()`/`set` iteration report canonical keys). `getOrInsertComputed` checks callability before lookup (matching QuickJS C) and passes the canonicalized key to the callback. `WeakMap`/`WeakSet` keys now honor `CanBeHeldWeakly`: `Symbol.for` registered symbols are rejected (new `SymbolBuiltins.isRegisteredSymbol`), non-registered symbols are still accepted.
- **`Object.groupBy` / `Map.groupBy`**: shared `MapSetBuiltins.groupBy` implements the ES GroupBy algorithm over the iterator protocol (callback receives `(value, index)` with `this === undefined`, iterator is closed on callback/iterator abrupt completions). `Object.groupBy` produces a null-prototype object and supports symbol keys; `Map.groupBy` produces a real `Map`. Callable check happens before the iterable is touched.
- **`Promise.try`**: `NewPromiseCapability(this)`, calls the callback with the trailing arguments, resolves with the result and rejects with a thrown exception. Synchronous `TypeError` (not a rejection) when `this` is not a constructor, matching QuickJS C.
- **`Error.isError`**: `JSObject` gained a `[[ErrorData]]` internal-slot marker (`markErrorData`/`hasErrorData`) set by `setupErrorObject`, so every constructor-created error (including runtime-thrown natives and `AggregateError`) reports `true`, while `Object.create(Error.prototype)`, `Error.prototype`, plain objects and primitives report `false`. The marker survives `Object.setPrototypeOf`.
- **`RegExp.escape`**: mirrors QuickJS C's `EncodeForRegExpEscape` over UTF-16 code units — leading ASCII digit/letter becomes `\xHH`; syntax characters and `/` are backslash-escaped; other punctuators and whitespace ≤ 0xFF become `\xHH`; whitespace/line terminators and surrogates use `\uXXXX`; non-string input throws TypeError.
- **Set methods rewritten to ES `GetSetRecord`**: all seven methods now validate the receiver's `[[SetData]]`, then read `size` → `ToNumber(size)` (NaN TypeError, negative RangeError) → `has` (callable) → `keys` (callable), and branch on `this.size` vs `other.size`. Iteration order follows the spec (result order follows the receiver when it is not larger, otherwise the argument's `keys()`), `has` is only called where the spec requires it, and the argument's iterator is closed on early returns. This fixed the 30 previously failing `set-like-*`/`converts-negative-zero` tests.
- **Engine fixes en route**: `new Function()` compiled an empty body as raw opcode 0 (`Invalid`) and threw `Invalid opcode` when called; it now compiles an empty body through the normal path like `new Function("")`. `BuiltinHelpers.getSymbolPropertyWithGetter` had no primitive-receiver path, so `getIterator("abc")` failed with "value is not iterable" (`Object.groupBy('abc', ...)`); primitives now resolve symbol-keyed properties through their wrapper prototypes. `BuiltinHelpers.IteratorRecord` now caches `[[NextMethod]]` like `GetIterator`, so builtins read `iterator.next` exactly once — this fixed the `set-like-class-order` order assertions and matches the spec everywhere records are used.
- Acknowledged new failure: `language/module-code/eval-rqstd-order.js` (module evaluation order for `export * as ns`) fails now that `export-star-as-namespace-from-module` is enabled (18/19 of that feature pass).
- Regression coverage: 8 new `ConformanceRegressionTest` tests (Map/WeakMap upsert canonical keys and weak-key rejection, groupBy grouping/null-prototype/iterator close, Promise.try, Error.isError marker semantics, RegExp.escape encoding table, `new Function()`, Set `GetSetRecord` ordering). Full suite now 1,298 tests, 0 failures.

**Operator and object-semantics round (Sep 2026)** — +274 test262 passing, 3,142 → 2,865 errors (91.3% → 92.0%):
- **Relational comparison applies ToPrimitive.** `Interpreter.compare` used raw `toNumber`, so every object compared as NaN: `new Date(0) < new Date(1000)` returned false, `[1] < 2` false, `({valueOf(){return 1}}) <= 1` false. It now converts both operands with `ToPrimitive(hint number)` (invoking `Symbol.toPrimitive`/`valueOf`/`toString`), compares strings by UTF-16 code unit, BigInts exactly (`BigDecimal` of the Double, not a rounded BigInt), and handles BigInt↔String through `BuiltinHelpers.stringToBigInt` (invalid strings = incomparable). Numeric comparison uses `<`/`>` instead of subtraction, so equal infinities are equal. Symbol operands throw TypeError. All four `less/greater-than(-or-equal)` directories now 100%.
- **`instanceof` protocol.** New `BuiltinHelpers.instanceofOperator`/`ordinaryHasInstance`: the RHS must be an object (TypeError otherwise), a callable `Symbol.hasInstance` takes precedence, an object that is neither callable nor has `@@hasInstance` throws TypeError, a non-object `C.prototype` throws, and bound functions delegate through a hidden `__boundTarget`. `%Function.prototype%[@@hasInstance]` is registered (length 1, name `[Symbol.hasInstance]`, non-writable/enumerable/configurable false) and `Function.prototype[Symbol.hasInstance].call()` returns false for non-callables. Proxies still bypass the `getPrototypeOf` trap (1 known failure).
- **`%Function.prototype%` is callable.** It used to be a plain JSObject (`typeof` "object", calling it threw). It is now a `NativeFunction` whose `funcObj` is the shared function prototype; `NativeFunction` records itself on its `funcObj` as `__nativeFunc`, and `JSObject.getPrototypeValue` canonicalizes such prototype objects back to the wrapper, so `typeof Function.prototype === "function"`, `Function.prototype()` is undefined and `Object.getPrototypeOf(fn) === Function.prototype` holds. `Function(...)` results now get their own `prototype` object (they were missing it), and concise methods/accessors are not constructors (no `prototype`, `new` throws), tracked by a compiler `currentFunctionIsMethod` flag. `StdLib.normalizeBuiltinDescriptors` no longer self-links `Function.prototype` (that created a prototype cycle).
- **`call`/`apply`/`bind`.** `Function.prototype.call`/`apply` throw TypeError for non-callable receivers, `CreateListFromArrayLike` accepts any object (functions included) and invokes getters, and `Interpreter.call` now boxes primitive `this` for sloppy functions (`Function("this.touched=true;return this").call(1)` boxes). `bind` validates the target at bind time, reads `name`/own `length` with [[Get]] (inherited/non-Number `length` = 0) and implements SetFunctionName/SetFunctionLength (string-only names, ToIntegerOrInfinity, Infinity and >Int32 values preserved). Annex B `caller`/`arguments` poison accessors were added to `Function.prototype` (`bound.caller` throws, `hasOwnProperty` false). `await`/`yield` as identifier fallbacks now go through the postfix tails, so `await(null)` parses as a call.
- **`#x in obj` (ES2022 private brand checks).** The parser accepts `PrivateIdentifier in ShiftExpression`, rejects undeclared private names and arrow-function right operands, and the new `PrivateIn` opcode (97) checks `__private__`/`__privateMethods__`/`__privateGetters__`/`__privateSetters__` by class-unique field name (primitive operands throw TypeError). Private-name bindings are captured by `findFreeVariablesForClosure`; `GeneratorSupport` handles the opcode too. `language/expressions/in` is now 100%.
- **`let` ASI in single-statement contexts.** `if (false) let\n{}` and friends parse `let` as the identifier expression via `parseStatement(singleStatementContext = true)` (loops, if/else, with, labeled bodies) and fall back when the next token is on a new line (except `let [`).
- **Smaller fixes:** object literal `__proto__: <non-object>` is ignored (`__objectSetProto`) and concise methods/accessors named `__proto__` define an own property (`__defineOwn`, `InternalHelpers`); `Object.keys(null)` and `Array.prototype.join.call(null)` throw TypeError; `%GeneratorFunction.prototype%` exists so `Object.getPrototypeOf(function*(){})` is a non-callable object; `decodeURI`/`decodeURIComponent` only accept ASCII hex digits (Unicode digits like U+0660 were accepted via `Character.digit`).
- Regression coverage: 13 new `ConformanceRegressionTest` tests (comparisons, `instanceof`, `Function.prototype` identity/callability, `__proto__`, nullish receivers, private brand checks, `let` ASI, bound name/length, sloppy `this` boxing, non-constructor methods, poison accessors). Full suite now 1,290 tests, 0 failures.
- One expected legacy regression: `staging/sm/regress/regress-586482-5.js` reads `arguments.callee.caller`; Annex B `caller` poison now throws instead of returning undefined (QuickJS C poisons it too).

**Module instantiation round (Sep 2026)** — `language/module-code` 406 → 468 passing (183 → 81 non-passing):
- **Import/export are ModuleItems, not Statements.** The parser now accepts them only at the top level of module code (`moduleItemAllowed`); `if (x) export ...`, nested blocks, function bodies, class methods and arrow bodies are SyntaxErrors. This alone fixed the ~120 `parse-err-decl-pos-*` tests.
- **Statement terminators.** Import/export forms that require `;` (named exports, `export default <expr>`, `import`/`export ... from`) enforce ASI via `requireStatementEnd`, while declaration forms (`export function/class`) correctly do not need one. `export default null, null` and `export {} null` are now rejected.
- **Module early errors.** Duplicate top-level lexical names (module functions are lexically scoped: `function f(){} function f(){}` and `var f; function f(){}` are errors), duplicate export names, exported locals that are not declared (`export { Number }`), `eval`/`arguments` as import/binding identifiers in strict code, duplicate and undefined labels (labels are now function-scoped), and `await` treated as an AwaitExpression only in async functions and at module top level (nested function bodies/parameters treat it as an identifier).
- **Static module linking.** `FileModuleLoader` builds a `ModuleRecord` per module (explicit exports, `export *`, import requests, imported-binding targets) and implements `ModuleDeclarationInstantiation` over the graph: named imports, namespace imports and indirect re-exports are resolved against dependency exports, ambiguous star resolutions are rejected, re-exported imported bindings follow their import target, and `export * as ns` resolves to the namespace identity. The compiler emits `__moduleInstantiate(source, …names)` before any body statement, so resolution errors fire before `$DONOTEVALUATE()`. Node's loader disables static linking because builtins/CommonJS interop synthesize exports at runtime.
- **Module bindings are instantiated.** Top-level `var` exists as `undefined` from the start; `let`/`const`/`class` start in the TDZ, which is now also enforced through closure reads (an `Uninitialized` captured `VarRef` throws ReferenceError); exported function declarations are registered before the body so cyclic importers can call them.
- **Loader fixes:** the generic loader parses dependencies with `moduleMode = true`, and `ESModuleTest`/`QuickJSModuleTest` now compile their module sources as modules (their `evalScript` helper was script-mode).
- The `language` chunk gained 203 passes and lost 185 failures (21,054 → 21,257 passing, 390 → 205 failing); `staging` gained 7.

**Robustness round (Sep 2026)** — host-crash and long-running-script fixes:
- **`try` leaked an operand-stack slot per execution.** Try/catch blocks were compiled with `preserveExpressionValue = true` unconditionally and `TryEnd` never restored the stack, so any `try` inside a loop grew `Frame.stack` by one slot per iteration and crashed after ~4096 iterations with a raw `IndexOutOfBoundsException`. `Compiler.compileStatement(TryStatement)` now preserves completion values only when the enclosing context consumes them; `eval("try { 1 } catch {}") === 1` still holds.
- **The runaway-loop guard aborted legitimate programs.** `BytecodeLoop.run` gave up after a fixed 100M instructions (generators after 10M), so a 20M-iteration loop failed with "Infinite loop detected". The budget now comes from `JSRuntime.setInstructionLimit` (default 0 = unlimited); the test262 runner keeps cancelling via wall-clock timeout and thread interrupt.
- **Deep recursion killed the host.** A JVM `StackOverflowError` (about 300 frames under the default 1 MB thread stack) unwound into `run`'s `catch`, which lazily initialized `BreakException$` at exhausted depth and produced `NoClassDefFoundError` instead of a JavaScript error. `JSContext` now counts interpreter frames and throws `RangeError: Maximum call stack size exceeded` (`setMaxCallDepth`, default 1000); `BytecodeLoop` catches `StackOverflowError` as a backstop and routes it through try handlers, `GeneratorSupport` converts it in generator bodies, and the control-flow singletons are initialized on entry.
- **Microtask draining was re-entrant.** `popStackFrame` calls `runMicrotasks` when the call stack empties, and each microtask that resumed an async frame popped frames again, recursing one JVM level per `await`; a `for` loop with 5,000 awaits overflowed the host stack. `runMicrotasks` is now guarded against re-entry.
- **`Object.create` rejected array/function/native prototypes** (only plain objects and functions); it now uses the same value-aware prototype assignment as `Object.setPrototypeOf`.
- Host failures that are not deliberate JavaScript errors are labeled `Internal engine error (<ExceptionClass>)` instead of leaking raw JVM text.
- Regression coverage: eight new `ConformanceRegressionTest` tests (try stack balance, try completion values, 12M-iteration loop, configured instruction budget, catchable `RangeError`, low call-depth limit, long await loop, `Object.create` prototypes). Fixing a missing `}` in that file also re-enabled 11 tests that had been silently nested and never executed (one had a wrong `returnTrue` vs `returnTrue()` assertion, now corrected), and the stale `ScratchParseTest` array-prototype FIXME is un-ignored and passes.

**Conformance round (Sep 2026) — bitwise ToPrimitive, direct eval, `with`, array-valued prototypes**:
- **Bitwise operators apply ToPrimitive**: `Interpreter.toInt32` used `JSValue.toNumber` (which maps every object to NaN), so `new Number(3) | 0` was 0 and `valueOf` was never called. It now uses `BuiltinHelpers.toNumber` and propagates abrupt completions; this fixed ~150 tests across `compound-assignment` (126 -> 49) and the `bitwise-*`/shift directories.
- **Direct eval (`runDirectEval`)**: the direct-eval case was extracted from `BytecodeLoop` into `Interpreter.runDirectEval`, which `GeneratorSupport` now also calls, so generator/async-generator parameter defaults get real direct-eval semantics. Added: `EvalDeclarationInstantiation` early errors (strict eval may not bind `arguments`/`eval`; non-strict eval may not var-declare a name already bound in the parameter environment, including the implicit `arguments` binding of non-simple parameter lists), `import`/`export` in eval is a SyntaxError, eval inherits strict mode (new `Parser(strictMode = ...)` that rejects reserved words without module `await` semantics), and `new.target` at script scope is a SyntaxError (arrows keep QuickJS behavior). `language/eval-code/direct`: 159 -> 18 non-passing (268/286 executed passing).
- **`with` semantics**: try handlers now record the `with` stack depth (`TryHandler.withStackDepth`, 4-tuples in generator state) so `throw`/`yield` unwinding abandons scopes opened inside the protected range; `break`/`continue` emit `popWith` for scopes they exit (parallel `loopWithDepths` stack); functions created inside a `with` capture its object environment records in their closure (`Interpreter.withCaptureKey`/`capturedWithObjects`) instead of every call inheriting the caller's `with` chain; `with` boxes primitives via ToObject; script `var` declarations are hoisted to script entry (`collectVarNames`) so unreachable declarations still create bindings and initializers assign through the `with` chain; a new `DeleteName` opcode (code 96, `delete ident`) resolves `delete` through the `with` chain/locals/global. `language/statements/with`: 108 -> 20 non-passing (161/181).
- **Array-valued prototypes**: ordinary objects can now have a JSArray as `[[Prototype]]` (`JSObject.getPrototypeValue`/`setPrototypeValue`), with lookups (`get`, `hasProperty`, descriptors, symbols, `getPropertyWithGetter`, array-like helpers), `Object.getPrototypeOf`/`setPrototypeOf`/`__proto__`, `instanceof` and `isPrototypeOf` all value-aware. `Foo.prototype = new Array(1,2,3); new Foo()` now inherits indexes, `length` and the array method chain (`built-ins/Array/prototype/every`: 1 -> 58 fixed).
- **test262 runner**: `*_FIXTURE.js` files are no longer executed as tests; module tests that import themselves now resolve to the entry module record (registered as loading before evaluation with the harness-injected source), fixing the `assert is not defined` cluster.

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
- Known differences/limits: `worker_threads`, `dgram`, `stream/web` (`ReadableStream`) and `async_hooks` are not implemented; `path.win32` aliases POSIX; `fetch` buffers the whole body (no streaming `response.body`); `Intl` is a pragmatic JDK/ICU- backed subset (no `%Intl%`-style locale data beyond the JVM's).
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

**Node ecosystem round 11 (Sep 2026)** — `require.extensions` and ts-node:
- **`require.extensions`** (shared with `module.extensions` / `module._extensions`): user handlers registered for an extension are used when `require` loads that file, and extension-less resolution consults the registered keys (so `require('./hello.ts')` and `require('./hello')` both work). `module._compile(source, filename)` runs CommonJS source inside the existing module object, which is what ts-node's loader calls. Default `.js`/`.json` handlers preserve the built-in behaviour.
- **`module` builtin** gained `Module` with `_extensions`/`_cache`/`_preloadModules`/`_resolveFilename`/`_findPath`/`wrap`/`createRequire` (ts-node and `source-map-support` poke at these), and `require()` now ignores a method receiver so `module.require('module')` works.
- **ts-node works**: `require('ts-node').register({transpileOnly: true})` then `require('./hello.ts')` returns compiled exports; without `transpileOnly` full type checking works too (including reporting type errors), though compiling `lib.d.ts` through the interpreter takes ~3.5 minutes.
- **Bugs fixed en route**: `debugger;` statements were rejected (`'debugger' is not a valid identifier`) — they are parsed as no-op statements now; and **parameter defaults in nested functions/arrows/declarations** did not propagate enclosing free variables (`findFreeVarsInFunctionForClosure` and the `FunctionDeclaration` cases ignored `freeVarsInParamDefaults`), which surfaced as `returnTrue is not defined` while loading typescript.js.
- Two new tests (`NodeCompatTest` `require.extensions` + extension-less resolution; `ConformanceRegressionTest` nested parameter defaults); full suite now 1,252 tests.

**Node ecosystem round 10 (Sep 2026)** — crypto ciphers, KDFs, key pairs and signatures (jsonwebtoken runs):
- **Ciphers** (`NodeCrypto`): `createCipheriv`/`createDecipheriv` for AES-128/192/256 in CBC/ECB/CTR/CFB/CFB8/OFB/GCM, `des-cbc`/`des-ede3-cbc`/`des-ede3` and `chacha20-poly1305`, with `update`/`final`, `setAAD`, `getAuthTag`/`setAuthTag` (GCM tags split from the final block), `setAutoPadding`, streaming encodings and key/IV length validation. Legacy `createCipher`/`createDecipher` use OpenSSL `EVP_BytesToKey` (MD5). `getCiphers`/`getHashes` list supported names.
- **KDFs** (`NodeCryptoExtras`, pure JVM): `pbkdf2Sync`/`pbkdf2` (hand-rolled HMAC so empty passwords work), `scryptSync`/`scrypt` (RFC 7914 Salsa20/8 + ROMix, `N`/`r`/`p`/`maxmem` validation) and `hkdfSync`/`hkdf`. Verified against RFC vectors: PBKDF2 SHA-256 `c5e478d5…`, scrypt `77d65762…`, HKDF `3cb25f25…`.
- **Keys and signatures**: `generateKeyPairSync`/`generateKeyPair` (RSA with `modulusLength`/`publicExponent`, EC with named curves, Ed25519), `createPrivateKey`/`createPublicKey` (PEM PKCS#8/SPKI parsing, RSA public derivation), `createSecretKey`, `sign`/`verify` (RSA/ECDSA/Ed25519 algorithm mapping), `createSign`/`createVerify`, a real `KeyObject` constructor and `export({format})` (PEM/DER/oct JWK). Async variants run on the host loop (`NodeCrypto.create(loop)`).
- **`Buffer` statics** used by `safe-buffer`'s fast path: `allocUnsafeSlow` and `isEncoding`; without them safe-buffer defines its own `SafeBuffer`, which lacks `isBuffer` and broke `jwa`/`jsonwebtoken`.
- **Verification**: `jsonwebtoken` signs/verifies HS256 and RS256 (and rejects a wrong secret); Express, concurrently, chokidar, mocha, yargs and axios unaffected. Two new `NodeCompatTest` tests (cipher/KDF/signature vectors, secret keys + Buffer statics); full suite now 1,251 tests.

**Node ecosystem round 9 (Sep 2026)** — `fs.watch`/`watchFile` and working `Readable._read` (chokidar detects changes):
- **`fs.watch`** returns an FSWatcher backed by a **shared** Java `WatchService` (one inotify instance per runtime, like libuv) with per-key dispatch: `change`/`rename` events, file watches via the parent directory with name filtering, optional `recursive` registration, `ENOENT` error events for missing paths, and idempotent `close()`. Per-watcher services exhausted the host inotify-instance limit when chokidar watched a large tree; the shared registry fixed that.
- **`fs.watchFile`** polls `stat` (default 5007 ms, `interval` option) and invokes `(curr, prev)` Stats objects on mtime/size/type changes; `fs.unwatchFile(path[, listener])` stops all or matching watchers. `fs.mkdtempSync`/`fs.promises.mkdtemp` now accept arbitrary prefixes (split into parent + name prefix; Java needs ≥3 chars).
- **`Readable` subclass support**: the `_read(batch)` hook is now invoked (with the high-water-mark batch size) in flowing and pull modes, and `objectMode`/`readableObjectMode`/`writableObjectMode` options are honored — object chunks are emitted as values instead of being byte-coerced. This fixed `readdirp` (and therefore chokidar) silently stalling; `fs/promises` wrappers also strip arbitrary receivers (`fsp.stat.call(obj, path)`).
- **Verification**: chokidar emits `add`/`change` for new and modified files and closes cleanly; concurrently, express, mocha, yargs and axios unaffected. Five new `NodeCompatTest` tests (fs.watch, fs.watchFile + unwatchFile, Readable object-mode `_read`, fs/promises receiver stripping, mkdtemp prefixes); full suite now 1,248 tests.

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

**Prototype values**: `JSObject` gained a value-aware `prototypeValue` slot (`getPrototypeValue`/`setPrototypeValue`) so arrays, functions and natives can be `[[Prototype]]` values. `foo.prototype = new Array(...); new foo()`, `Object.setPrototypeOf`, `__proto__` and `Object.create` all honor them (the old `ScratchParseTest` repro is enabled and passing). Remaining edge: an explicitly set function/native prototype is normalized to `JSValue.Object(funcObj)`, so `Object.getPrototypeOf(x) === fn` is false after `Object.setPrototypeOf(x, fn)`/`Object.create(fn)`; property lookups and `instanceof` are unaffected.

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

# Run all tests (1,279 tests, 0 failures; test262 smoke tests auto-skip if not cloned)
sbt test

# Clone test262 for conformance testing (if you don't already have it)
# If test262/ already exists as a symlink or clone, skip this.
git clone --depth 1 https://github.com/tc39/test262.git test262

# Run specific test
sbt "testOnly quickjs.stdlib.QuickJSJavaScriptTest"
```

### Fast iteration

- **Suites run in parallel** (capped at the processor count) and console capture
  is thread-local (`Console.withOutput`), so `sbt test` is ~26-30 s; the wall
  time is dominated by the upstream `test_builtin.js` run and the two
  interpreter-throughput regression tests.
- **Incremental tests**: `sbt testQuick` runs previously failing tests plus
  suites affected by real (content) source changes. Editing the interpreter or
  core touches most of the suite (~35 s); editing a leaf file skips unrelated
  suites. For a single suite, prefer `testOnly` below.
- **Focused suites**: `sbt "runtime/testOnly quickjs.interpreter.TryCatchTest"`,
  or filter test names with `sbt "stdlib/testOnly quickjs.stdlib.ConformanceRegressionTest -- -z try"`.
- **test262, only the previous failures** (seconds, not minutes):
  ```bash
  scripts/test262-rerun.sh                  # re-runs test262_errors.txt
  TEST262_USE_JAR=1 scripts/test262-rerun.sh   # via the assembled jar (~2 s)
                                             # rebuild with: sbt runner/assembly
  ```
- **test262, a directory or filter**: the runner takes `maxTests` and a filter
  (a path fragment or a file of test paths):
  ```bash
  sbt "stdlib/runMain quickjs.stdlib.Test262Runner test262.conf 200000 language/module-code"
  sbt "stdlib/runMain quickjs.stdlib.Test262Runner test262.conf 500 language/statements/try"
  ```
- **Full sweep**: `scripts/test262-chunks.sh` (~3 min; chunks in separate JVMs
  to keep memory flat). `TEST262_TIMEOUT_SECONDS`, `TEST262_HEAP`,
  `TEST262_WORKERS` and the runner's `-Dquickjs.test262.workers=N` tune it. The
  script defaults to 1.5x the core count: a timed-out test holds its worker for
  the full per-test timeout, and a little oversubscription keeps the other
  tests running (staging drops from ~46s to ~41s, built-ins ~59s to ~55s,
  language is neutral). Run chunks sequentially; parallel chunks share the same
  cores and only add GC pressure.

## Test Status

**Current Test Count**: 1,341 tests, 0 failures, 0 errors (measured after the REPL round)

### Test Distribution
- **stdlib**: 1,037 tests — language features, built-in objects, JSON, arrays, TypedArrays, Node compatibility (`NodeCompatTest`), test262 smoke suites, etc.
- **runtime**: 205 tests — interpreter correctness, closures, try/catch, classes, etc.
- **compiler**: 13 tests
- **parser**: 86 tests (lexer + parser + strict mode)
- **test262 smoke tests**: 15 suites (~771 tests, 692 executed) — see below
- **QuickJS C test files**: 5 files run via `QuickJSJavaScriptTest` — all pass

### test262 Conformance (15 suites, ~771 tests, measured after the compile-time/staging round)
| Suite | Tests | Passed | Errors | Skipped | Pass Rate |
|-------|-------|--------|--------|---------|-----------|
| `Array/isArray` | 29 | 29 | 0 | 0 | 100% |
| `Object/assign` | 38 | 38 | 0 | 0 | 100% |
| `Math` | 50 | 50 | 0 | 0 | 100% |
| `language/literals` | 50 | 42 | 0 | 8 | 100% |
| `Symbol` | 94 | 77 | 0 | 17 | 100% |
| `BigInt` | 50 | 50 | 0 | 0 | 100% |
| `Map` | 50 | 49 | 0 | 1 | 100% |
| `Set` | 50 | 49 | 0 | 1 | 100% |
| `WeakMap` | 30 | 29 | 0 | 1 | 100% |
| `WeakSet` | 30 | 29 | 0 | 1 | 100% |
| `Promise` | 50 | 50 | 0 | 0 | 100% |
| `Reflect` | 50 | 49 | 1 | 0 | 98.0% |
| `TypedArray` | 100 | 83 | 0 | 17 | 100% |
| `ArrayBuffer` | 50 | 38 | 0 | 12 | 100% |
| `DataView` | 50 | 29 | 0 | 21 | 100% |

Main remaining smoke-suite gap: one `Reflect` error. The skip counts are feature-config exclusions (resizable/immutable ArrayBuffer variants, TypedArrays edge features, Symbol `cross-realm`, etc.), not failures.

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
1. **`stream/web` `ReadableStream` + fetch streaming**, `dns`, `worker_threads`.
2. **`ws`/socket.io** — WebSocket upgrade handling on top of the new HTTP server.
3. Smaller gaps surfaced by probes: HTTP keep-alive/pipelining is not implemented (responses close the connection); `stream` backpressure/highWaterMark is approximate; `crypto` JWK export and PKCS#1 PEM parsing are unsupported; ts-node's full type-check mode is very slow (transpile-only is fast).

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
