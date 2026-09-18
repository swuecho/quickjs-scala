#!/usr/bin/env node
// ============================================================
// QuickJS-Scala: a small Node-style CLI ("qwc" - quick word count)
//
// Run:
//   java -jar quickjs-runner.jar --node examples/node_cli.js README.md AGENTS.md
//   java -jar quickjs-runner.jar --node examples/node_cli.js --json README.md
//   java -jar quickjs-runner.jar --node examples/node_cli.js --top 5 README.md
//
// Exercises process.argv, fs (sync + promises), path, Buffer and
// process.stdout in Node compatibility mode.
// ============================================================

const fs = require("node:fs");
const fsp = require("node:fs/promises");
const path = require("node:path");

function parseArgs(argv) {
  const options = { json: false, top: 0, files: [] };
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === "--json") options.json = true;
    else if (arg === "--top") options.top = Math.max(0, Number(argv[++i]) || 0);
    else if (arg === "-h" || arg === "--help") options.help = true;
    else options.files.push(arg);
  }
  return options;
}

const STOP_WORDS = new Set([
  "the", "a", "an", "and", "or", "of", "to", "in", "is", "are", "for", "on",
  "with", "as", "by", "at", "it", "its", "this", "that", "be", "from", "not",
]);

function analyze(source, name) {
  const bytes = Buffer.byteLength(source, "utf8");
  const lines = source.length === 0 ? 0 : source.split(/\r\n|\r|\n/).length;
  const words = source.toLowerCase().match(/[\p{L}\p{N}_']+/gu) || [];
  const frequencies = new Map();
  for (const word of words) {
    if (STOP_WORDS.has(word)) continue;
    frequencies.set(word, (frequencies.get(word) || 0) + 1);
  }
  return { name, bytes, lines, words: words.length, frequencies };
}

function top(words, count) {
  return [...words.entries()]
    .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
    .slice(0, count);
}

function formatBytes(n) {
  if (n < 1024) return `${n} B`;
  if (n < 1024 * 1024) return `${(n / 1024).toFixed(1)} KiB`;
  return `${(n / (1024 * 1024)).toFixed(2)} MiB`;
}

async function main() {
  const options = parseArgs(process.argv.slice(2));
  if (options.help || options.files.length === 0) {
    console.log("usage: quickjs-runner --node examples/node_cli.js [--json] [--top N] <file...>");
    console.log(`cwd: ${process.cwd()}`);
    return;
  }

  const reports = [];
  for (const file of options.files) {
    const resolved = path.resolve(file);
    const relative = path.relative(process.cwd(), resolved);
    const display = relative && !relative.startsWith("..") ? relative : path.basename(resolved);
    try {
      const source = await fsp.readFile(resolved, "utf8");
      reports.push(analyze(source, display));
    } catch (err) {
      // Another fs flavour: the sync API. ENOENT is reported, not thrown.
      reports.push({ name: display, error: err.code || err.message });
    }
  }

  if (options.json) {
    process.stdout.write(
      JSON.stringify(
        reports.map(({ frequencies, ...report }) =>
          report.error
            ? report
            : { ...report, top: options.top > 0 ? top(frequencies, options.top) : undefined }
        ),
        null,
        2
      ) + "\n"
    );
    return;
  }

  const totals = { bytes: 0, lines: 0, words: 0 };
  console.log(`scanned ${reports.length} path(s) from ${process.cwd()}\n`);
  for (const report of reports) {
    if (report.error) {
      console.log(`  ${report.name}: ERROR ${report.error}`);
      continue;
    }
    totals.bytes += report.bytes;
    totals.lines += report.lines;
    totals.words += report.words;
    console.log(
      `  ${report.name.padEnd(24)} ${String(report.lines).padStart(6)} lines ` +
        `${String(report.words).padStart(7)} words ${formatBytes(report.bytes).padStart(9)}`
    );
    if (options.top > 0) {
      console.log(
        "      top: " +
          top(report.frequencies, options.top)
            .map(([word, count]) => `${word}(${count})`)
            .join(" ")
      );
    }
  }
  console.log(
    `\n  ${"TOTAL".padEnd(24)} ${String(totals.lines).padStart(6)} lines ` +
      `${String(totals.words).padStart(7)} words ${formatBytes(totals.bytes).padStart(9)}`
  );

  // Write a small summary next to the process's temp directory, then read it
  // back with the sync API to show both flavours.
  const outFile = path.join(require("node:os").tmpdir(), "qwc-summary.txt");
  fs.writeFileSync(outFile, `files=${reports.length} words=${totals.words}\n`);
  const summary = fs.readFileSync(outFile, "utf8").trim();
  console.log(`\n  summary written to ${outFile}: ${summary}`);
  fs.unlinkSync(outFile);
}

main().catch((err) => {
  console.error("qwc failed:", err);
  process.exitCode = 1;
});
