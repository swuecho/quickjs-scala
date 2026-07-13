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

`test262.conf` is the explicit capability inventory. Keep a feature marked
`skip` until its implementation is intended to be tested; do not exclude a
directory merely because it currently fails.
