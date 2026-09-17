#!/usr/bin/env bash
#
# Re-run only the tests listed in a failure file (default: test262_errors.txt).
#
# The test262 runner accepts a file as its filter argument and runs exactly the
# listed relative test paths. Use this after a fix to re-check the previous
# failures in seconds instead of re-running a whole directory or sweep.
#
# Typical loop:
#   sbt "stdlib/runMain quickjs.stdlib.Test262Runner test262.conf 200000 language/module-code"
#   ... fix code ...
#   scripts/test262-rerun.sh                 # re-runs the 81 module failures
#
# Environment:
#   TEST262_USE_JAR=1     run the assembled runner jar instead of sbt (fast;
#                         rebuild with `sbt runner/assembly` after code changes)
#   TEST262_TIMEOUT_SECONDS / TEST262_MAX_TESTS are forwarded to the runner.
#
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

LIST="${1:-test262_errors.txt}"
if [ ! -s "$LIST" ]; then
  echo "No non-passing tests in '$LIST' (nothing to re-run)."
  exit 0
fi

TIMEOUT="${TEST262_TIMEOUT_SECONDS:-}"
MAX_TESTS="${TEST262_MAX_TESTS:-200000}"
JVM_FLAGS=()
if [ -n "$TIMEOUT" ]; then
  JVM_FLAGS+=("-Dquickjs.test262.timeoutSeconds=$TIMEOUT")
fi

JAR="runner/target/scala-3.7.4/quickjs-runner.jar"
if [ "${TEST262_USE_JAR:-0}" = "1" ] && [ -f "$JAR" ]; then
  # The jar runs the classes as of the last `sbt runner/assembly`; warn when
  # sources have changed since, so results are not mistaken for fresh ones.
  stale=$(find core/src runtime/src parser/src compiler/src stdlib/src \
    -name '*.scala' -newer "$JAR" -print -quit 2>/dev/null)
  if [ -n "$stale" ]; then
    echo "warning: $JAR is older than $stale; rebuild with 'sbt runner/assembly'" >&2
  fi
  echo "Re-running failures from $LIST via $JAR"
  exec java ${TEST262_JAVA_OPTS:-} "${JVM_FLAGS[@]}" -cp "$JAR" \
    quickjs.stdlib.Test262Runner test262.conf "$MAX_TESTS" "$LIST"
else
  SBT_FLAGS=""
  for flag in "${JVM_FLAGS[@]:-}"; do
    [ -n "$flag" ] && SBT_FLAGS="$SBT_FLAGS -J$flag"
  done
  echo "Re-running failures from $LIST via sbt"
  exec sbt -batch $SBT_FLAGS \
    "stdlib/runMain quickjs.stdlib.Test262Runner test262.conf $MAX_TESTS $LIST"
fi
