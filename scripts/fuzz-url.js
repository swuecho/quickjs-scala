#!/usr/bin/env node
/*
 * Differential fuzzer: generates random URL inputs, computes the reference
 * WHATWG URL result, feeds them through quickjs-runner.jar (which should
 * implement the same parser) and reports any differences.
 *
 * The reference is the `whatwg-url` package when it can be resolved (set
 * NODE_PATH, e.g. `npm install whatwg-url@11` somewhere); otherwise Node's
 * built-in URL is used. Node's built-in implementation has a few quirks, so
 * prefer whatwg-url for spec-accurate comparisons.
 *
 * Usage: node scripts/fuzz-url.js [iterations] [seed]
 */
/* eslint-disable */
"use strict";

const fs = require("fs");
const path = require("path");
const { execFileSync } = require("child_process");

let RefURL = globalThis.URL;
try {
  RefURL = require("whatwg-url").URL;
  console.log("reference: whatwg-url package");
} catch (e) {
  console.log("reference: Node built-in URL");
}

const iterations = parseInt(process.argv[2] || "2000", 10);
let seed = parseInt(process.argv[3] || "123456789", 10) >>> 0;
function rnd() {
  seed = (seed * 1664525 + 1013904223) >>> 0;
  return seed / 4294967296;
}
function pick(arr) {
  return arr[Math.floor(rnd() * arr.length)];
}

const schemes = ["http", "https", "ftp", "ws", "wss", "file", "foo", "data", "mailto", "a.b", "blob"];
const hosts = [
  "example.com", "EXAMPLE.com", "sub.example.com", "127.0.0.1", "0x7f.1", "0177.1",
  "[::1]", "[2001:db8::1]", "[::ffff:1.2.3.4]", "caf\u00e9.fr", "\u4f8b\u3048.jp",
  "fa\u00df.de", "a..b", "a_b", "xn--fa-hia.de", "1.2.3.4.", "0", "999.999.999.999",
  "lOcAlHoSt", "a%b", "a%20b", "\u00e9", "a" + "\u3002" + "b", "[0:0:0:0:0:0:0:1]"
];
const paths = ["", "/", "/a/b", "/a/../b", "/./a", "/%2e%2e/x", "/a b", "/a%20b", "/\u00e9", "/%C3%A9", "//double", "/a\\b", "/c:/x", "/c|/x", "/..", "/."];
const queries = ["", "?", "?a=1", "?a=1&b=2", "?a b", "?a'b", "?%C3%A9", "?a=%zz", "?a#b"];
const fragments = ["", "#", "#f", "#a b", "#%C3%A9", "#a?b", "#a#b"];
const userinfo = ["", "u@", "u:p@", "u:p:q@", "us er:p@ss@", "@", ":p@"];
const ports = ["", ":80", ":443", ":8080", ":0", ":65535", ":65536", ":01", ":", "::80"];
const schemesPrefixes = ["", "http:", "https:", "//", "///", "http:/", "http:\\", "\\\\"];
const bases = [
  "http://a/b/c?d#e",
  "https://user:pw@h:81/x/y",
  "file:///C:/dir/file",
  "file://host/share",
  "foo://h/p",
  "mailto:a@b",
  "http://[::1]:8080/p"
];

function randomInput() {
  let s = "";
  s += pick(schemesPrefixes);
  s += pick(schemes);
  s += ":";
  const style = rnd();
  if (style < 0.5) {
    s += "//";
    s += pick(userinfo);
    s += pick(hosts);
    s += pick(ports);
  } else if (style < 0.7) {
    s += "//";
  }
  s += pick(paths);
  s += pick(queries);
  s += pick(fragments);
  if (rnd() < 0.2) s = s.replace(/a/g, "A");
  return s;
}

const cases = [];
for (let i = 0; i < iterations; i++) {
  const input = randomInput();
  const base = rnd() < 0.5 ? pick(bases) : null;
  let expected = null;
  try {
    const u = base ? new RefURL(input, base) : new RefURL(input);
    expected = {
      ok: true,
      href: u.href, origin: u.origin, protocol: u.protocol, username: u.username,
      password: u.password, host: u.host, hostname: u.hostname, port: u.port,
      pathname: u.pathname, search: u.search, hash: u.hash
    };
  } catch (e) {
    expected = { ok: false };
  }
  cases.push({ input, base, expected });
}

const jar = path.join(__dirname, "..", "runner", "target", "scala-3.7.4", "quickjs-runner.jar");
const lines = [];
// Run in batches: very large script arrays hit engine-internal limits.
for (let offset = 0; offset < cases.length; offset += 500) {
  const batch = cases.slice(offset, offset + 500);
  const js = `
(function () {
  var cases = ${JSON.stringify(batch)};
  var out = [];
  for (var i = 0; i < cases.length; i++) {
    var c = cases[i];
    try {
      var u = c.base === null ? new URL(c.input) : new URL(c.input, c.base);
      out.push(JSON.stringify({ ok: true, href: u.href, origin: u.origin, protocol: u.protocol, username: u.username, password: u.password, host: u.host, hostname: u.hostname, port: u.port, pathname: u.pathname, search: u.search, hash: u.hash }));
    } catch (e) {
      out.push(JSON.stringify({ ok: false }));
    }
  }
  console.log(out.join("\\n"));
})();
`;
  const tmp = path.join(require("os").tmpdir(), "url-fuzz.js");
  fs.writeFileSync(tmp, js);
  const output = execFileSync("java", ["-jar", jar, tmp], { maxBuffer: 1 << 30 }).toString();
  lines.push(...output.trimEnd().split("\n"));
}

let mismatches = 0;
for (let i = 0; i < cases.length; i++) {
  const actual = JSON.parse(lines[i]);
  const expected = cases[i].expected;
  const fields = ["ok", "href", "origin", "protocol", "username", "password", "host", "hostname", "port", "pathname", "search", "hash"];
  for (const f of fields) {
    if (actual[f] !== expected[f]) {
      mismatches++;
      if (mismatches <= 30) {
        console.log(`mismatch #${i} input=${JSON.stringify(cases[i].input)} base=${JSON.stringify(cases[i].base)} field=${f} got=${JSON.stringify(actual[f])} want=${JSON.stringify(expected[f])}`);
      }
      break;
    }
  }
}
console.log(`checked ${cases.length} cases, ${mismatches} mismatches`);
process.exit(mismatches === 0 ? 0 : 1);
