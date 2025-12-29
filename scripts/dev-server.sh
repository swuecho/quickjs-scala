#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

PORT="${TRACE_PORT:-8125}"
WAIT_SECONDS="${TRACE_WAIT_SECONDS:-30}"

is_port_free() {
  local port="$1"
  (echo >"/dev/tcp/127.0.0.1/${port}") >/dev/null 2>&1 && return 1 || return 0
}

SECONDS_WAITED=0
until is_port_free "${PORT}"; do
  if [ "${SECONDS_WAITED}" -ge "${WAIT_SECONDS}" ]; then
    echo "Port ${PORT} still in use after ${WAIT_SECONDS}s. Set TRACE_PORT or stop the existing server." >&2
    exit 1
  fi
  echo "Port ${PORT} in use, waiting... (${SECONDS_WAITED}/${WAIT_SECONDS}s)"
  sleep 1
  SECONDS_WAITED=$((SECONDS_WAITED + 1))
done

echo "Starting trace server watcher on port ${PORT}..."
cd "${ROOT_DIR}"
exec sbt "~runtime/runMain quickjs.tracing.TraceServer ${PORT}"
