// ============================================================
// QuickJS-Scala: ES module demo (import / export / top-level await)
//
// Run:  java -jar quickjs-runner.jar examples/esm-demo/main.mjs
//       java -jar quickjs-runner.jar examples/esm-demo/main.mjs --table
//
// Demonstrates named/default/namespace imports, re-export barrels,
// JSON modules with import attributes, dynamic import(), and
// top-level await (the runner drives timers while the module waits).
// ============================================================

// Static imports: default binding, named bindings, a namespace object and a
// JSON module declared with an import attribute.
import sales from "./data/sales.json" with { type: "json" };
import describe, { mean, median, stddev, sum } from "./lib/stats.mjs";
import * as stats from "./lib/stats.mjs";
import { currency } from "./lib/format.mjs";

const out = console.log;
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

out("── ES modules ──────────────────────────────");
out("module url       :", import.meta.url);
out("json default     :", sales.regions.length, "regions,", sales.quarters.join("/"));

// Namespaces expose the module's named exports (plus `default`).
out("namespace keys   :", Object.keys(stats).sort().join(", "));
out("namespace.mean   :", typeof stats.mean);

// All regions flattened into one series.
const allSales = sales.regions.flatMap((region) => region.sales);
out("all sales        :", describe(allSales));
out("recomputed       :", `sum=${sum(allSales)} mean=${mean(allSales)} median=${median(allSales)} stddev=${stddev(allSales).toFixed(1)}`);

out("");
out("Quarterly growth (last two quarters):");
for (const region of sales.regions) {
  const latest = region.sales[region.sales.length - 1];
  const previous = region.sales[region.sales.length - 2];
  const growth = (latest - previous) / previous;
  out("  " + region.name.padEnd(14), currency(previous), "→", currency(latest), `(${(growth * 100).toFixed(1)}%)`);
}

// Top-level await: the module body suspends here; the runner keeps the event
// loop alive until the timer fires (the same mechanism used for fs/fetch).
await sleep(5);
out("");
out("top-level await  : resumed after a timer (import.meta.url is a file URL:", import.meta.url.startsWith("file://") + ")");

// Dynamic import: only load the table renderer when it is needed.
if (scriptArgs.includes("--table")) {
  const { render } = await import("./lib/report.mjs");
  const { table } = await import("./lib/index.mjs"); // barrel re-export
  const { header, rows } = render(sales);
  out("");
  out(table(header, rows));
} else {
  out("hint             : pass --table to dynamically import the table renderer");
}
