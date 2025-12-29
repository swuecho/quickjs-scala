#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "Starting trace server watcher..."
cd "${ROOT_DIR}"
exec sbt "~runtime/runMain quickjs.tracing.TraceServer 8125"
