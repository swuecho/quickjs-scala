# ECMAScript Conformance Baseline

QuickJS-Scala treats test failures as data, not passes. The conformance runners
must not rewrite upstream tests, suppress exceptions, or accept the wrong error
phase/type.

## QuickJS regression files

Four currently supported upstream files run unchanged as normal tests:

```bash
sbt "stdlib/testOnly quickjs.stdlib.QuickJSJavaScriptTest"
```

The complete `test_builtin.js` file is an explicit quarantine because it still
contains unsupported groups. It is never edited at runtime. Run it to expose
the first incompatibility with:

```bash
sbt -Dquickjs.conformance.fullBuiltin=true \
  "stdlib/testOnly quickjs.stdlib.QuickJSJavaScriptTest"
```

Remove the quarantine only after the complete file passes.

## test262

The Scala test suite runs small feature samples. For an actual compatibility
baseline, invoke the runner directly without a maximum:

```bash
sbt "stdlib/runMain quickjs.stdlib.Test262Runner test262.conf"
```

An optional second argument limits the number of enumerated tests, and an
optional third argument filters paths:

```bash
sbt "stdlib/runMain quickjs.stdlib.Test262Runner test262.conf 500 language/eval-code"
```

Every run writes:

- `test262_report.txt`: every PASS, FAIL, ERROR, SKIP, and TIMEOUT
- `test262_errors.txt`: failures, errors, and timeouts only

The command exits unsuccessfully when any executed test fails, errors, or times
out. Skips remain visible in the report and pass-rate calculations exclude them.
Tests without `onlyStrict` or `noStrict` flags execute in both modes. Module
tests execute through module bytecode when enabled by `test262.conf`.

Full runs use up to eight isolated workers and a one-second ceiling per strict
or non-strict variant so one runaway program cannot stall the corpus. Override
the worker count when reproducing timing-sensitive results:

```bash
sbt -Dquickjs.test262.workers=1 \
  "stdlib/runMain quickjs.stdlib.Test262Runner test262.conf"
```

`test262.conf` is the explicit capability inventory. Keep a feature marked
`skip` until its implementation is intended to be tested; do not exclude a
directory merely because it currently fails.

## Latest full baseline

The full configured corpus was run on 2026-07-15 after the class-element,
runtime-private-name, nested-class, direct-eval private-environment, `new`
precedence, and Annex B legacy accessor fixes:

```text
Total: 53690 | Passed: 26606 | Failed: 1224 | Errors: 11399 |
Skipped: 14416 | Timeouts: 45 | Pass rate: 67.7%
Time: 1355813ms
```

This supersedes the 61.8% baseline (24,261 passes, 1,808 failures, 13,156
errors, and 49 timeouts), adding 2,345 passes, reducing failures by 584,
reducing errors by 1,757, and reducing timeouts by four. It is 5.9 pass-rate
points higher. Relative to the earlier 53.5% baseline, this is 5,469
additional passes and a 14.2-point increase. Relative to the original 42.6%
baseline, this is 9,770 additional passes and a 25.1-point increase.
Focused runs overwrite the generated report files, so preserve a report before
starting a filtered run when historical per-test comparisons are needed.

After that baseline, parameter initialization was separated from the function's
local slots so defaults observe the required left-to-right temporal dead zone.
Async-generator resume methods now return Promises and expose
`Symbol.asyncIterator`; the complete `dflt-params-ref-` family passes 96/96.
Computed object binding keys and their for-of/for-await declaration paths were
also implemented: `obj-ptrn-prop-eval-err.js` passes 62/62. A broader async
generator expression run currently passes 479/623 (77.0%), with one skipped.
Native functions now participate in ordinary object introspection through their
function objects (the Math smoke sample is 50/50), and rest formal parameters
are supported for functions, arrows, and generators. Both the dedicated
`language/rest-parameters/` suite (11/11) and the cross-form `rest-params`
family (32/32) pass.

The first generic `Array.prototype` batch now covers `map`, `filter`,
`forEach`, `indexOf`, `every`, `some`, `reduce`, and `reduceRight`. A focused
run of all 2,811 `Array.prototype` tests first improved from 688 passes (25.3%)
to 1,488 passes (54.7%). These methods accept array-like receivers, snapshot
`length`, skip holes through `HasProperty`, observe inherited/accessor indexed
properties, and preserve holes in `map`. The remaining Array failures are now
concentrated in other methods (notably `concat`, mutators, and generic
`includes`/`slice` behavior) plus shared coercion and species semantics.

A second Array batch added generic `includes`, `find`, `findIndex`, `at`, and
`slice`, shared object-aware `ToNumber`/`ToIntegerOrInfinity`, correct builtin
property descriptors, Array branding, and the core generic behavior of
`concat`. After adding Array-instance symbol storage and cooperative cancellation
for native loops, the same 2,811-test cluster now passes 1,851 tests (68.1%)
with only 12 timeouts: 1,163 more passes than the original Array baseline.
Focused executed-test results are 25/27 for
`includes`, 19/19 for `find`, 11/11 for `at`, and 46/66 for `slice`. Array
instances now retain own symbol-keyed properties, enabling
`Symbol.isConcatSpreadable` overrides.

Unicode identifier classification now follows the Unicode `ID_Start` and
`ID_Continue` properties, including ECMAScript's `$`, `_`, ZWNJ/ZWJ, and
`Other_ID_Start` additions. Direct and escaped supplementary-plane identifiers
and private identifiers are supported. The focused `language/identifiers/`
run improved from 67 passes with 71 errors to 268/268. Binding identifiers now
reject always-reserved words in literal and escaped forms, and strict-mode
future-reserved words are handled separately from valid contextual names.
The runner now reprocesses metadata following a `negative:` block and uses a
configurable ten-second per-variant timeout, allowing the generated Unicode
stress tests to complete without weakening cancellation of genuinely stuck
tests.

Class bodies now enforce strict-mode parsing, unique constructors, private-name
uniqueness (while permitting a matching getter/setter pair), forbidden
`#constructor`, field `constructor`, and static `prototype` names, special
constructor restrictions, and same-line field separators. The focused 1,534
class-declaration element tests improved from 746 passes (48.7%) to 788 passes
(51.4%) after the structural checks. Generator and async grammar context is now
shared by function declarations, expressions, object/class methods, and async
arrows; the cross-form `await-as-binding-identifier` and
`yield-as-binding-identifier` families pass 50/50 and 48/48 respectively.

Field-initializer static semantics now track lexical boundaries: `arguments`
and direct `super()` are rejected through nested arrows but ordinary functions
stop the initializer-specific traversal. Both cross-form families pass 60/60.
`yield` is parsed at its AssignmentExpression grammar position, bringing the
cross-form `yield-as-identifier-reference` family to 48/48. Direct `super()` is
now restricted to derived constructors (42/42 focused static/special/no-
heritage cases). Lexical private-name environments support forward references
and nested classes while rejecting unresolved names: invalid-name syntax is
56/56 and class-heritage environment coverage is 12/12. After the first part
of this batch, the combined 2,962 declaration/expression class-element run was
1,709 passes (57.8%). After the direct-super and private-environment fixes it
reached 1,856 passes (62.7%), with only four parse-negative misses; those final
four unparenthesized-arrow heritage cases now pass their six-test focused
family as well.

Class element runtime semantics have since advanced substantially. Class
methods now use configurable, non-enumerable, writable descriptors and private
getter/setter initialization uses the correct native-call argument convention.
Bracketed class element names retain their computed syntax in the AST, are
evaluated once at class definition, and apply the string-hinted
`ToPropertyKey` operation (including `Symbol.toPrimitive`, Symbol keys, and
abrupt completions). Both the string and Symbol `computed-names.js` families
pass 34/34, and the focused computed-name coercion families pass 14/14.
Private generator methods now preserve their generator/async flags (20/22 sync
and 156/158 async at that checkpoint). `yield*` now obtains and retains an iterator, following the QuickJS C
delegation boundary, and its no-LineTerminator early error is enforced. The
723-test `yield-star` family reached 711 passes before the five newline cases
were fixed; those five now pass separately. Together these changes moved the
combined 2,962 declaration/expression class-element run from 1,856 passes
(62.7%), through 2,326 (78.6%), to 2,589 passes (87.5%), with 369 errors, four
skips, and no failures or timeouts. The complete repository suite is green at
711 passed, zero failed/errors, and one ignored test.

`new.target` now has a dedicated AST node and observes the current constructor
NewTarget, including lexical capture through arrows, direct-eval field rules,
Reflect construction, bound constructors, custom prototypes, and Proxy
construct paths. Its focused family passes all 90 executed tests, with 40
feature-skipped tests. Yield operands now stop at the surrounding comma, as in
QuickJS C's assignment-expression parser, bringing the complete cross-form
`yield-spread-obj.js` family to 24/24. Computed static and instance class keys
are pre-evaluated once in original source order; both intercalated field tests
pass, and function objects now participate correctly in `for...in` and
`propertyIsEnumerable`. The combined 2,962 class-element run consequently
improved again to 2,636 passes (89.1%), with 322 errors, four skips, and no
failures or timeouts. The complete repository suite is green at 713 passed,
zero failed/errors, and one ignored test.

Class-field direct eval now carries its lexical initializer context through
nested arrows. It rejects `arguments` and `super()` before executing the eval
source, permits direct `super.property` while rejecting it in indirect eval,
and inherits the enclosing class private-name environment. Private methods and
accessors are installed before instance fields, so they are visible to field
initializers. The focused `direct-eval-` family improved from 44/169 to 168/169;
the only remaining case is an unrelated SpiderMonkey staging test for a
shadowed global `eval`. The combined 2,962 class-element run now passes 2,768
tests (93.6%), with 190 errors, four skips, and no failures or timeouts. The
complete repository suite remains green at 714 passed, zero failed/errors, and
one ignored test.

Private instance methods and accessors now follow QuickJS C's class-scope
binding model: their closures are created once per class evaluation and reused
by every instance instead of being recreated by each constructor call. This
fixes private method identity and configures sync, async, generator, and async
generator names as `#name`. Static private getters/setters now use the private
accessor storage path instead of attempting to define a public property whose
key is a `PrivateIdentifier`. The focused private method comparison tests pass
3/3, the private async/generator name families pass 6/6, and the private
accessor-name family passes 40/40. The combined 2,962 class-element run now
passes 2,815 tests (95.2%), with 143 errors, four skips, and no failures or
timeouts.

Private names are now allocated dynamically for each class evaluation and
captured through class methods, constructors, and direct field eval. Private
field initialization has a distinct define path, while later reads and writes
perform a real brand check instead of silently creating or returning a missing
string-keyed slot. The repeated-evaluation static private getter and setter
families each pass all four executed variants (with one feature skip apiece),
and retained coverage rejects cross-brand instance method, static getter, and
static setter access. The combined 2,962 class-element run consequently reaches
2,834 passes (95.8%), with 124 errors, four skips, and no failures or timeouts.
The complete repository suite is green at 717 passed, zero failed/errors, and
one ignored test. The largest remaining private-element cluster is nested class
expression construction and shadowing, which is separate from private-brand
identity.

Nested class expressions now use the correct `new MemberExpression(args)`
precedence, so `new holder.C()` constructs `holder.C` rather than evaluating
`(new holder).C()`. Parenthesized constructors such as `new (factory())()` are
also accepted. Static-field `this` no longer leaks into nested class methods or
constructors, outer private environments are retained through nested static
field initializers, and private methods use a distinct non-writable runtime
slot. The complete `on-nested-class` family improved from 0/48 to 48/48, while
the repeated private-method brand family passes all five executed variants.
Ordinary direct eval in class methods now inherits private bindings without
acquiring field-initializer-only syntax restrictions; its focused visibility
family passes 12/12. The combined 2,962 class-element run now reaches 2,906
passes (98.2%), with 52 errors, four skips, and no failures or timeouts. Annex B
`__lookupGetter__` and `__lookupSetter__` support additionally brings the
focused accessor-visibility family to 6/6. The complete repository suite is
green at 719 passed, zero failed/errors, and one ignored test.

An intermediate full run before native-loop cancellation was added reached
22,812 passes (57.7%) and 13,562 errors, but its 776 timeouts were contaminated
by cancelled native loops continuing to consume worker CPU. It is not the new
authoritative baseline. The focused rerun reduced Array timeouts from 72 to 12;
the clean full rerun above supersedes it.

After the 67.7% full baseline, property-descriptor conversion was aligned with
QuickJS C's `js_obj_to_desc`: inherited descriptor fields and accessor side
effects are observed in specification order, array descriptor objects are
accepted, data/accessor conflicts are rejected after conversion, and
`Object.defineProperty` applies `ToPropertyKey`. Math, JSON, and Reflect now
inherit from `Object.prototype`, and the standard `Object.create`,
`Object.defineProperty`, and `Object.defineProperties` lengths are correct.
Focused results improved to 845/1,131 for `defineProperty` (74.9%), 429/632 for
`defineProperties` (68.0%), and 271/320 for `create` (84.7%), with no failures
or timeouts in those runs. Relative to the full-run error inventory, these
three families remove 285 errors; a new complete run is required before
promoting that gain into the authoritative global baseline. The complete
repository suite is green at 720 passed, zero failed/errors, and one ignored
test.
