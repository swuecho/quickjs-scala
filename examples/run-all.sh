#!/usr/bin/env bash
# Run every QuickJS-Scala example against the assembled runner jar.
#
#   examples/run-all.sh                 # stop at the first failure
#   examples/run-all.sh --keep-going    # run all, then report failures
#
# Builds the jar with `sbt runner/assembly` when it is missing.
set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

JAR="$(ls runner/target/scala-*/quickjs-runner.jar 2>/dev/null | head -1 || true)"
if [[ -z "${JAR}" || ! -f "${JAR}" ]]; then
  echo "==> building the runner jar (sbt runner/assembly)"
  sbt -batch runner/assembly >/dev/null
  JAR="$(ls runner/target/scala-*/quickjs-runner.jar 2>/dev/null | head -1 || true)"
fi
if [[ -z "${JAR}" || ! -f "${JAR}" ]]; then
  echo "error: could not find runner/target/scala-*/quickjs-runner.jar" >&2
  exit 1
fi

KEEP_GOING=0
[[ "${1:-}" == "--keep-going" ]] && KEEP_GOING=1

TIMEOUT_BIN="$(command -v timeout || true)"
run_example() {
  local name="$1"; shift
  echo
  echo "==> ${name}"
  if [[ -n "${TIMEOUT_BIN}" ]]; then
    "${TIMEOUT_BIN}" 60 java -jar "${JAR}" "$@"
  else
    java -jar "${JAR}" "$@"
  fi
  local status=$?
  if [[ ${status} -ne 0 ]]; then
    echo "FAILED (${status}): ${name}" >&2
    FAILED+=("${name} (exit ${status})")
    if [[ ${KEEP_GOING} -eq 0 ]]; then
      echo
      echo "summary: ${#FAILED[@]} failure(s); stopped early" >&2
      exit 1
    fi
  fi
}

FAILED=()

run_example "feature_demo.js"            examples/feature_demo.js
run_example "language_tour.js"           examples/language_tour.js
run_example "async_patterns.js (plain)"  examples/async_patterns.js
run_example "async_patterns.js (node)"   --node examples/async_patterns.js
run_example "web_globals.js"             examples/web_globals.js
run_example "esm-demo/main.mjs"          examples/esm-demo/main.mjs --table
run_example "node_cli.js"                --node examples/node_cli.js README.md docs/README.md
run_example "os_module.js"               --node examples/os_module.js
run_example "http_server.js"             --node examples/http_server.js
run_example "streams_pipeline.js"        --node examples/streams_pipeline.js
run_example "crypto_toolkit.js"          --node examples/crypto_toolkit.js
run_example "child_process.js"           --node examples/child_process.js

echo
if [[ ${#FAILED[@]} -eq 0 ]]; then
  echo "all examples passed"
else
  echo "failed examples:" >&2
  printf '  - %s\n' "${FAILED[@]}" >&2
  exit 1
fi
