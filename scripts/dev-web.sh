#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FRONTEND_DIR="${ROOT_DIR}/web-frontend"

if [[ ! -f "${FRONTEND_DIR}/package.json" ]]; then
  echo "Missing web-frontend/package.json. Aborting."
  exit 1
fi

if ! command -v npm >/dev/null 2>&1; then
  echo "npm is required to run Vite. Please install Node.js."
  exit 1
fi

if [[ ! -d "${FRONTEND_DIR}/node_modules" ]]; then
  echo "Installing frontend dependencies..."
  (cd "${FRONTEND_DIR}" && npm install)
fi

cleanup() {
  if [[ -n "${SBT_FRONTEND_PID:-}" ]]; then
    kill "${SBT_FRONTEND_PID}" >/dev/null 2>&1 || true
  fi
}

trap cleanup EXIT

echo "Starting Scala.js fastLinkJS watcher..."
(cd "${ROOT_DIR}" && sbt "~webFrontend/fastLinkJS") &
SBT_FRONTEND_PID=$!

echo "Starting Vite dev server..."
cd "${FRONTEND_DIR}"
npm run dev
