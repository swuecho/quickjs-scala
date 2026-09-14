#!/usr/bin/env bash
#
# Run the full test262 suite in per-directory chunks, each in a fresh JVM, and
# print an aggregate summary.
#
# Why chunks: with the JIT-compiled interpreter, heavy tests (multi-million
# element arrays, Date/DST stress) allocate much faster. A single long-lived
# JVM can fill its heap and GC-thrash even though every individual chunk
# completes comfortably. Restarting the JVM between chunks keeps memory flat.
#
# Environment variables:
#   TEST262_CHUNKS           space-separated filters (default: built-ins language staging harness)
#   TEST262_HEAP             JVM max heap per chunk (default: 4g)
#   TEST262_TIMEOUT_SECONDS  per-test timeout (default: 5)
#   TEST262_MAX_TESTS        max tests per chunk (default: 200000, i.e. no limit)
#   SBT                      sbt launcher (default: sbt)
#
# Usage:
#   scripts/test262-chunks.sh
#   TEST262_HEAP=6g scripts/test262-chunks.sh
#
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

CHUNKS="${TEST262_CHUNKS:-built-ins language staging harness}"
HEAP="${TEST262_HEAP:-4g}"
TIMEOUT_SECONDS="${TEST262_TIMEOUT_SECONDS:-5}"
MAX_TESTS="${TEST262_MAX_TESTS:-200000}"
SBT="${SBT:-sbt}"

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

for chunk in $CHUNKS; do
  echo "=== test262 chunk: $chunk ==="
  "$SBT" -J-Xmx"$HEAP" \
    -Dquickjs.test262.timeoutSeconds="$TIMEOUT_SECONDS" \
    -batch "stdlib/runMain quickjs.stdlib.Test262Runner test262.conf $MAX_TESTS $chunk" \
    || true # non-zero exit just means the chunk had failures; aggregated below
  cp test262_report.txt "$tmpdir/$chunk.txt" 2>/dev/null || true
done

python3 - "$tmpdir" "$CHUNKS" <<'PY'
import os, re, sys

tmpdir, chunks = sys.argv[1], sys.argv[2].split()
keys = ["Total", "Passed", "Failed", "Errors", "Skipped", "Timeouts"]
tot = {k: 0 for k in keys}
missing = []
for c in chunks:
    path = os.path.join(tmpdir, c + ".txt")
    if not os.path.exists(path):
        missing.append(c)
        continue
    line = open(path).readline()
    for k in keys:
        m = re.search(k + r": (\d+)", line)
        if m:
            tot[k] += int(m.group(1))

executed = tot["Total"] - tot["Skipped"]
rate = tot["Passed"] / executed * 100 if executed else 0.0
print("=== test262 aggregate ===")
print(
    f"Total: {tot['Total']} | Passed: {tot['Passed']} | Failed: {tot['Failed']} "
    f"| Errors: {tot['Errors']} | Skipped: {tot['Skipped']} "
    f"| Timeouts: {tot['Timeouts']} | Pass rate: {rate:.1f}%"
)
if missing:
    print(f"missing chunk reports: {', '.join(missing)}")
non_passing = tot["Failed"] + tot["Errors"] + tot["Timeouts"]
sys.exit(1 if non_passing or missing else 0)
PY
