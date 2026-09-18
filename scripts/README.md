# Scripts

Development and conformance scripts. Run them from the repository root.

| Script | Purpose |
| --- | --- |
| `bench-micro.js` | Micro-benchmarks for interpreter hot paths |
| `demo.js` | Feature demonstration script |
| `fuzz-url.js` | Differentially fuzz the WHATWG URL parser against Node/`whatwg-url` |
| `generate-url-fixtures.js` | Regenerate the URL conformance fixtures (`url-corpus.json`) |
| `test.js` | Small sample script |
| `dev-server.sh` | Start the trace-server backend watcher for the web frontend |
| `dev-web.sh` | Start the Scala.js watcher and Vite dev server for the web frontend |
| `test262-chunks.sh` | Run the full test262 sweep in separate JVMs, then aggregate |
| `test262-rerun.sh` | Re-run only the tests listed in `test262_errors.txt` |

See [../docs/RUNNING_NODE_SCRIPTS.md](../docs/RUNNING_NODE_SCRIPTS.md) for the
runner and Node compatibility guide, [../docs/CONFORMANCE.md](../docs/CONFORMANCE.md)
for the test262 workflow, and [../examples/README.md](../examples/README.md) for
runnable example scripts.
