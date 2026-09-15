#!/usr/bin/env node
/*
 * Regenerates the URL conformance fixtures used by
 * stdlib/src/test/scala/quickjs/stdlib/URLTest.scala.
 *
 * The expected values come from Node's WHATWG URL implementation, so this
 * script must be run with a Node build that has a current implementation
 * (Node 18+). It writes:
 *
 *   stdlib/src/test/resources/url-conformance.js
 *       Parses every [input, base?] pair from url-corpus.json and compares
 *       href/origin/protocol/username/password/host/hostname/port/pathname/
 *       search/hash (or "must throw") against the values recorded here.
 *
 *   stdlib/src/test/resources/url-setter-conformance.js
 *       Applies sequences of URL property assignments and compares the href
 *       after every assignment.
 *
 * Usage: node scripts/generate-url-fixtures.js
 */
"use strict";

const fs = require("fs");
const path = require("path");

const root = path.join(__dirname, "..");
const resources = path.join(root, "stdlib", "src", "test", "resources");

const corpus = JSON.parse(
  fs.readFileSync(path.join(resources, "url-corpus.json"), "utf8")
);

const fields = [
  "href", "origin", "protocol", "username", "password", "host", "hostname",
  "port", "pathname", "search", "hash"
];

const parseCases = corpus.map(([input, base]) => {
  try {
    const u = base !== undefined ? new URL(input, base) : new URL(input);
    const entry = { i: input, b: base === undefined ? null : base, ok: true };
    for (const f of fields) entry[f] = u[f];
    return entry;
  } catch (e) {
    return { i: input, b: base === undefined ? null : base, ok: false };
  }
});

const parseProgram = `(function () {
  var failures = [];
  var cases = ${JSON.stringify(parseCases)};
  function push(msg) { failures.push(msg); }
  for (var k = 0; k < cases.length; k++) {
    var c = cases[k];
    var u = null;
    try {
      u = c.b === null ? new URL(c.i) : new URL(c.i, c.b);
    } catch (e) {
      if (c.ok) push("case " + k + " " + JSON.stringify(c.i) + " (base " + JSON.stringify(c.b) + ") expected ok but threw " + e);
      continue;
    }
    if (!c.ok) { push("case " + k + " " + JSON.stringify(c.i) + " expected failure but got " + u.href); continue; }
    var fields = ${JSON.stringify(fields)};
    for (var j = 0; j < fields.length; j++) {
      var f = fields[j];
      if (u[f] !== c[f]) push("case " + k + " " + JSON.stringify(c.i) + " (base " + JSON.stringify(c.b) + ") ." + f + ": got " + JSON.stringify(u[f]) + " want " + JSON.stringify(c[f]));
    }
  }
  return failures.length === 0 ? "OK" : failures.slice(0, 40).join("\\n");
})();
`;

fs.writeFileSync(path.join(resources, "url-conformance.js"), parseProgram);

// --- setter behaviour ------------------------------------------------------

const setterCases = [
  ["http://a/b/c?x#y", [["pathname", "p"], ["search", "q=1"], ["hash", "h"]]],
  ["http://a/b", [["host", "h:81"]]],
  ["http://a/b", [["host", "h:80"]]],
  ["http://a/b", [["hostname", ""]]],
  ["http://a/b", [["hostname", "EXAMPLE.com"]]],
  ["http://a/b", [["port", ""]]],
  ["http://a/b", [["port", "99999"]]],
  ["http://a/b", [["port", 80]]],
  ["http://a/b", [["port", "0x50"]]],
  ["http://a/b", [["port", " 80 "]]],
  ["http://a/b", [["protocol", "https"], ["port", "443"]]],
  ["http://a/b", [["protocol", "foo"]]],
  ["foo:bar", [["protocol", "http"]]],
  ["foo://h/b", [["protocol", "https"]]],
  ["http://u:p@a/b", [["username", "x y"], ["password", "p@ss"]]],
  ["http://a/b", [["username", ""], ["password", ""]]],
  ["mailto:a@b", [["pathname", "z"], ["host", "h"], ["port", "8080"], ["username", "u"], ["password", "p"]]],
  ["file:///a/b", [["host", "h"], ["pathname", "c"]]],
  ["file:///a/b", [["pathname", "/d/e"]]],
  ["http://a/b", [["pathname", "//double//slash/../x"]]],
  ["http://a/b", [["pathname", null]]],
  ["http://a/b", [["search", 123]]],
  ["http://a/b", [["hash", 456]]],
  ["http://a/b", [["search", "?a b&c=d"]]],
  ["http://a/b", [["hash", "#a b"]]],
  ["http://a/b", [["hash", "?"]]],
  ["http://a/b", [["search", "?"]]],
  ["http://a/b", [["search", "??"]]],
  ["http://a/b", [["search", "?a'b"]]],
  ["foo://a/b", [["search", "?a'b"]]],
  ["http://a/b", [["pathname", ""]]],
  ["http://a/b", [["pathname", "/"]]],
  ["http://a/b", [["href", "https://x/y?z"]]],
  ["http://a/b", [["href", "not a url"], ["hostname", "kept"]]],
  ["http://a/b", [["host", "[::1]:99"]]],
  ["http://a:99/b", [["host", "[2001:db8::1]"]]],
  ["http://a:99/b", [["host", "[2001:db8::1]:88"]]],
  ["http://a/b", [["host", "caf\u00e9.fr"]]],
  ["http://a/b", [["hostname", "\u4f8b\u3048.jp"]]],
  ["foo://h/p", [["hostname", "g"], ["port", "12"], ["pathname", "z"]]],
  ["foo:/opaque", [["pathname", "z"], ["host", "h"], ["port", "1"], ["username", "u"], ["password", "p"]]],
  ["http://a/b", [["protocol", "https"], ["host", "b:444"], ["search", "s=1"], ["hash", "f"]]],
  ["https://a:443/b", [["protocol", "http"]]],
  ["http://a/b", [["host", "a..b"]]],
  ["http://a/b", [["host", "."]]],
  ["http://a/b", [["host", "999.999.999.999"]]],
  ["http://a/b", [["host", "0x7f.1"]]],
  ["http://a/b", [["host", "0177.1"]]],
  ["http://a/b", [["pathname", "caf\u00e9"]]],
  ["http://a/b", [["pathname", "%zz"]]],
  ["http://a/b", [["search", "a=%zz"]]],
  ["http://a/b", [["hash", "%zz"]]],
  ["http://a:443/", [["protocol", "https"]]],
  ["https://a:80/", [["protocol", "http"]]],
  ["http://a:81/", [["protocol", "https"]]],
  ["http://a/b", [["hostname", "x:y"], ["hostname", "x:"]]],
  ["http://a/b", [["host", "x:99"], ["host", "x:y"]]],
  ["http://a/b", [["hostname", "[::1]"], ["port", "8080"]]],
  ["file:///a/b", [["hostname", "h"], ["pathname", "c|/d"]]],
  ["http://a/b", [["username", 12], ["password", null]]],
  ["foo://a/b", [["protocol", "bar"], ["hash", "#"]]],
  ["http://a/b", [["search", "?"], ["hash", "#"], ["pathname", ""]]]
];

function runSetters() {
  const results = [];
  for (const [start, ops] of setterCases) {
    const u = new URL(start);
    const row = [];
    for (const [prop, value] of ops) {
      let err = null;
      try {
        u[prop] = value;
      } catch (e) {
        err = e.constructor.name;
      }
      row.push({ prop, value: value === undefined ? null : value, href: u.href, err });
    }
    results.push({ start, ops: row });
  }
  return results;
}

const setterProgram = `(function () {
  var failures = [];
  var cases = ${JSON.stringify(runSetters())};
  for (var k = 0; k < cases.length; k++) {
    var c = cases[k];
    var u = new URL(c.start);
    for (var j = 0; j < c.ops.length; j++) {
      var op = c.ops[j];
      var err = null;
      try { u[op.prop] = op.value; } catch (e) { err = e.constructor.name; }
      if (err !== op.err) { failures.push("case " + k + " (" + c.start + ") set " + op.prop + "=" + JSON.stringify(op.value) + ": threw " + err + " want " + op.err); continue; }
      if (err === null && u.href !== op.href) failures.push("case " + k + " (" + c.start + ") set " + op.prop + "=" + JSON.stringify(op.value) + ": got " + JSON.stringify(u.href) + " want " + JSON.stringify(op.href));
    }
  }
  return failures.length === 0 ? "OK" : failures.slice(0, 40).join("\\n");
})();
`;

fs.writeFileSync(path.join(resources, "url-setter-conformance.js"), setterProgram);

console.log("parse cases:", parseCases.length, "setter cases:", setterCases.length);
