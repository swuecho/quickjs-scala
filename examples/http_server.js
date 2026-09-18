#!/usr/bin/env node
// ============================================================
// QuickJS-Scala: HTTP server + client demo
//
// Run:  java -jar quickjs-runner.jar --node examples/http_server.js
//
// Starts a tiny JSON API on an ephemeral port with `http.createServer`,
// calls it back with `http.get` and `fetch`, then shuts the server down.
// ============================================================

const http = require("node:http");

function sendJson(res, status, payload) {
  const body = JSON.stringify(payload);
  res.writeHead(status, {
    "Content-Type": "application/json; charset=utf-8",
    "Content-Length": Buffer.byteLength(body),
  });
  res.end(body);
}

function readBody(req) {
  return new Promise((resolve, reject) => {
    let body = "";
    req.setEncoding("utf8");
    req.on("data", (chunk) => (body += chunk));
    req.on("end", () => resolve(body));
    req.on("error", reject);
  });
}

const startedAt = Date.now();
const server = http.createServer(async (req, res) => {
  const url = new URL(req.url, `http://${req.headers.host}`);
  console.log(`  ${req.method} ${url.pathname}${url.search}`);

  if (req.method === "GET" && url.pathname === "/") {
    res.writeHead(200, { "Content-Type": "text/plain" });
    res.end("QuickJS-Scala HTTP server\n");
    return;
  }

  if (req.method === "GET" && url.pathname === "/api/time") {
    sendJson(res, 200, {
      now: new Date().toISOString(),
      uptimeMs: Date.now() - startedAt,
      engine: "quickjs-scala",
    });
    return;
  }

  if (req.method === "GET" && url.pathname === "/api/greet") {
    const name = url.searchParams.get("name") || "world";
    sendJson(res, 200, { greeting: `Hello, ${name}!`, query: Object.fromEntries(url.searchParams) });
    return;
  }

  if (req.method === "POST" && url.pathname === "/api/echo") {
    const body = await readBody(req);
    let parsed = null;
    try {
      parsed = JSON.parse(body);
    } catch {
      parsed = body;
    }
    sendJson(res, 200, { received: parsed, contentType: req.headers["content-type"] });
    return;
  }

  sendJson(res, 404, { error: "not found", path: url.pathname });
});

function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => {
      let body = "";
      res.setEncoding("utf8");
      res.on("data", (chunk) => (body += chunk));
      res.on("end", () => resolve({ status: res.statusCode, headers: res.headers, body }));
    }).on("error", reject);
  });
}

server.listen(0, "127.0.0.1", async () => {
  const base = `http://127.0.0.1:${server.address().port}`;
  console.log(`server listening on ${base}\n`);

  // 1. http.get with a query string
  const greeting = await httpGet(`${base}/api/greet?name=QuickJS&lang=en`);
  console.log("http.get  →", greeting.status, greeting.body);
  console.log("           content-type:", greeting.headers["content-type"]);

  // 2. fetch POST with a JSON body
  const echo = await fetch(`${base}/api/echo`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ message: "hello from fetch", nested: { ok: true } }),
  });
  console.log("fetch     →", echo.status, await echo.json());

  // 3. fetch GET parsing JSON
  const time = await fetch(`${base}/api/time`);
  const payload = await time.json();
  console.log("fetch GET →", time.status, { engine: payload.engine, hasTime: /^\d{4}-/.test(payload.now) });

  // 4. 404 path
  const missing = await fetch(`${base}/does-not-exist`);
  console.log("missing   →", missing.status, await missing.json());

  server.close(() => console.log("\nserver closed"));
});
