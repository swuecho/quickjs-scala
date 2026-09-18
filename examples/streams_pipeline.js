#!/usr/bin/env node
// ============================================================
// QuickJS-Scala: fs streams and piping
//
// Run:  java -jar quickjs-runner.jar --node examples/streams_pipeline.js
//
// Creates a scratch directory, then exercises fs.createReadStream,
// fs.createWriteStream, PassThrough, StringDecoder and .pipe(),
// cleaning everything up at the end.
// ============================================================

const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const { PassThrough, finished } = require("node:stream");
const { StringDecoder } = require("node:string_decoder");

const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "quickjs-streams-"));
const sourceFile = path.join(scratch, "source.txt");
const copyFile = path.join(scratch, "copy.txt");

// A small text source: three log-ish lines.
const lines = [
  "2026-09-18 INFO  engine started",
  "2026-09-18 WARN  slow path used",
  "2026-09-18 INFO  engine stopped",
];
fs.writeFileSync(sourceFile, lines.join("\n") + "\n");
console.log(`scratch: ${scratch}`);
console.log(`source : ${fs.statSync(sourceFile).size} bytes written\n`);

function collect(stream) {
  return new Promise((resolve, reject) => {
    const decoder = new StringDecoder("utf8");
    let text = "";
    stream.on("data", (chunk) => (text += decoder.write(chunk)));
    stream.on("end", () => resolve(text + decoder.end()));
    stream.on("error", reject);
  });
}

async function main() {
  // ----------------------------------------------------------
  console.log("── 1. reading with a StringDecoder");
  // ----------------------------------------------------------
  const text = await collect(fs.createReadStream(sourceFile));
  console.log(`  read ${text.length} chars, ${text.trim().split("\n").length} lines`);
  console.log(`  first line: ${JSON.stringify(text.split("\n")[0])}`);

  // ----------------------------------------------------------
  console.log("\n── 2. pipe(): read stream → write stream");
  // ----------------------------------------------------------
  await new Promise((resolve, reject) => {
    const dest = fs.createWriteStream(copyFile);
    dest.on("finish", resolve);
    dest.on("error", reject);
    fs.createReadStream(sourceFile).pipe(dest);
  });
  const copied = fs.readFileSync(copyFile, "utf8");
  console.log(`  copied ${Buffer.byteLength(copied)} bytes, identical:`, copied === text);

  // ----------------------------------------------------------
  console.log("\n── 3. setEncoding(): string chunks instead of Buffers");
  // ----------------------------------------------------------
  const stringChunks = [];
  await new Promise((resolve, reject) => {
    const readable = fs.createReadStream(sourceFile);
    readable.setEncoding("utf8");
    readable.on("data", (chunk) => {
      if (typeof chunk !== "string") throw new Error("expected a string chunk");
      stringChunks.push(chunk);
    });
    readable.on("end", resolve);
    readable.on("error", reject);
  });
  console.log(`  ${stringChunks.length} string chunk(s), total ${stringChunks.join("").length} chars`);

  // ----------------------------------------------------------
  console.log("\n── 4. Manual writes and backpressure-ish accounting");
  // ----------------------------------------------------------
  const manualFile = path.join(scratch, "manual.txt");
  await new Promise((resolve, reject) => {
    const out = fs.createWriteStream(manualFile);
    out.on("finish", resolve);
    out.on("error", reject);
    let written = 0;
    for (let i = 1; i <= 5; i++) {
      const record = `record-${i}\n`;
      written += out.write(Buffer.from(record, "utf8"));
    }
    out.end(`wrote ${written} bytes across 5 records\n`);
  });
  console.log(`  manual file lines: ${fs.readFileSync(manualFile, "utf8").trim().split("\n").length}`);

  // ----------------------------------------------------------
  console.log("\n── 5. finished(): wait for a stream to drain");
  // ----------------------------------------------------------
  const sink = new PassThrough();
  const chunks = [];
  sink.on("data", (chunk) => chunks.push(chunk.toString("utf8")));
  sink.write("hello ");
  sink.write("streams");
  sink.end("!");
  await new Promise((resolve, reject) => finished(sink, (err) => (err ? reject(err) : resolve())));
  console.log(`  received: ${JSON.stringify(chunks.join(""))}`);

  // Clean up the scratch directory tree.
  fs.rmSync(scratch, { recursive: true, force: true });
  console.log(`\ncleaned up ${scratch}:`, !fs.existsSync(scratch));
}

main().catch((err) => {
  console.error("streams demo failed:", err);
  process.exitCode = 1;
});
