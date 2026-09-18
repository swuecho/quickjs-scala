#!/usr/bin/env node
// ============================================================
// QuickJS-Scala: child processes
//
// Run:  java -jar quickjs-runner.jar --node examples/child_process.js
//
// execSync / exec / spawnSync / spawn with streaming stdout and
// stdin piping. Uses common POSIX utilities (`echo`, `cat`, `sh`).
// ============================================================

const { exec, execSync, execFileSync, spawn, spawnSync } = require("node:child_process");

const out = console.log;

function section(n, title) {
  out(`\n── ${n}. ${title} ${"─".repeat(Math.max(2, 52 - title.length - n.toString().length))}`);
}

async function main() {
  // ------------------------------------------------------------
  section(1, "execSync: blocking, buffered output");
  // ------------------------------------------------------------
  const syncOut = execSync("echo hello from execSync").toString().trim();
  out("execSync:", JSON.stringify(syncOut));
  const listed = execFileSync("printf", ["%s-%s", "exec", "File"]).toString();
  out("execFileSync:", JSON.stringify(listed));

  // ------------------------------------------------------------
  section(2, "exec: asynchronous callback with buffered output");
  // ------------------------------------------------------------
  const execResult = await new Promise((resolve, reject) => {
    exec("echo async-stdout && echo async-stderr >&2", (error, stdout, stderr) => {
      if (error) reject(error);
      else resolve({ stdout: stdout.trim(), stderr: stderr.trim() });
    });
  });
  out("exec stdout:", JSON.stringify(execResult.stdout));
  out("exec stderr:", JSON.stringify(execResult.stderr));

  // ------------------------------------------------------------
  section(3, "spawnSync: status, signal and buffered streams");
  // ------------------------------------------------------------
  const status = spawnSync("sh", ["-c", "echo one; echo two >&2; exit 3"]);
  out("status:", status.status, "signal:", status.signal);
  out("stdout:", JSON.stringify(status.stdout.toString().trim()));
  out("stderr:", JSON.stringify(status.stderr.toString().trim()));

  const missing = spawnSync("definitely-not-a-real-command");
  out("spawnSync failure reported:", missing.error !== undefined, "|", missing.error ? missing.error.message : "none", "status:", missing.status);

  // ------------------------------------------------------------
  section(4, "spawn: streaming stdout/stderr events");
  // ------------------------------------------------------------
  await new Promise((resolve, reject) => {
    const child = spawn("sh", [
      "-c",
      "for i in 1 2 3; do echo line-$i; done; echo oops >&2",
    ]);
    out("spawned pid:", typeof child.pid === "number");
    const stdoutLines = [];
    const stderrLines = [];
    child.stdout.on("data", (chunk) => stdoutLines.push(...chunk.toString("utf8").trim().split("\n")));
    child.stderr.on("data", (chunk) => stderrLines.push(chunk.toString("utf8").trim()));
    child.on("error", reject);
    child.on("exit", (code, signal) => out(`exit event: code=${code} signal=${signal}`));
    child.on("close", (code) => {
      out("stdout lines:", JSON.stringify(stdoutLines));
      out("stderr lines:", JSON.stringify(stderrLines));
      if (code !== 0) reject(new Error(`child failed: ${code}`));
      else resolve();
    });
  });

  // ------------------------------------------------------------
  section(5, "spawn: writing to child stdin");
  // ------------------------------------------------------------
  const upper = await new Promise((resolve, reject) => {
    const child = spawn("tr", ["a-z", "A-Z"]);
    let result = "";
    child.stdout.on("data", (chunk) => (result += chunk.toString("utf8")));
    child.on("error", reject);
    child.on("close", () => resolve(result.trim()));
    child.stdin.write("piped through tr\n");
    child.stdin.end("second line\n");
  });
  out("child output:", JSON.stringify(upper));

  // ------------------------------------------------------------
  section(6, "spawn + kill");
  // ------------------------------------------------------------
  await new Promise((resolve) => {
    const child = spawn("sh", ["-c", "sleep 30"]);
    child.on("exit", (code, signal) => {
      out(`killed child: killed=${child.killed} signal=${signal} code=${code}`);
      resolve();
    });
    setTimeout(() => child.kill("SIGTERM"), 10);
  });

  out("\nchild processes complete.");
}

main().catch((err) => {
  console.error("child_process demo failed:", err);
  process.exitCode = 1;
});
