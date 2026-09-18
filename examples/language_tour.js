// ============================================================
// QuickJS-Scala: Modern JavaScript Language Tour
//
// Shows the ES2020+ features the engine implements beyond the
// basics in `feature_demo.js`: private class members, static
// blocks, generators, symbols, proxies, BigInt, modern array
// methods, RegExp named groups and Intl.
//
// Run:  java -jar quickjs-runner.jar examples/language_tour.js
// ============================================================

"use strict";

const out = console.log;

function section(n, title) {
  out(`\n── ${n}. ${title} ${"─".repeat(Math.max(2, 52 - title.length - n.toString().length))}`);
}

// ------------------------------------------------------------
section(1, "Scoping, optional chaining, nullish");
// ------------------------------------------------------------

let counter = 0;
const bump = () => ++counter;
[1, 2, 3].forEach(bump);
out("let/const + arrow:", counter);

const config = { server: { port: 8080 }, debug: false };
out("optional chain:", config.server?.port, config.missing?.port, config.missing?.());
out("nullish chains:", config.debug ?? true, config.missing ?? "fallback");

let score = 0;
score ||= 42;
let retries;
retries ??= 3;
const flags = {};
flags.on = flags.on ??= true;
out("logical assignment:", score, retries, flags.on);

// Limits are block-scoped per iteration, so closures capture each value.
const triples = [];
for (let i = 0; i < 3; i++) triples.push(() => i * 3);
out("per-iteration let:", triples.map((f) => f()).join(", "));

// ------------------------------------------------------------
section(2, "Destructuring and spread");
// ------------------------------------------------------------

const [first, , third, ...rest] = [1, 2, 3, 4, 5, 6];
out("array pattern:", first, third, rest);

const { name = "anon", address: { city = "?" } = {}, ...others } = {
  name: "Ada",
  address: { city: "London" },
  role: "engineer",
  year: 1843,
};
out("object pattern:", name, city, others);

function swap([a, b]) {
  [a, b] = [b, a];
  return [a, b];
}
out("swap:", swap(["left", "right"]));

const merged = { ...config, port: 9090, server: { ...config.server, host: "0.0.0.0" } };
out("spread merge:", JSON.stringify(merged));

let a = [3, 1], b = [2];
out("spread call:", Math.max(...a, ...b, 10));

// ------------------------------------------------------------
section(3, "Template literals and tagged templates");
// ------------------------------------------------------------

const item = { label: "widget", count: 3, price: 1.5 };
out(`  ${item.label}: ${item.count} × $${item.price} = $${item.count * item.price}`);

function html(strings, ...values) {
  return strings.reduce(
    (acc, s, i) => acc + s + (values[i] === undefined ? "" : String(values[i]).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]))),
    ""
  );
}
const userInput = '<script>alert("xss")</script>';
out("tagged template:", html`<p>${userInput}</p>`);

// ------------------------------------------------------------
section(4, "Classes: private state, static blocks, super");
// ------------------------------------------------------------

class Shape {
  static #count = 0;
  #name;
  constructor(name) {
    this.#name = name;
    Shape.#count++;
  }
  get name() { return this.#name; }
  set name(value) { this.#name = String(value); }
  get area() { return 0; }
  static get count() { return Shape.#count; }
  describe() { return `${this.name} (area ${this.area.toFixed(1)})`; }
  static create(...args) { return new this(...args); }
}

class Circle extends Shape {
  static #unit = "px";
  #radius;
  static {
    // Static block runs once, in source order, before any instance exists.
    out("  [static block] registering Circle with unit", Circle.#unit);
  }
  constructor(radius) { super("circle"); this.#radius = radius; }
  get radius() { return this.#radius; }
  get area() { return Math.PI * this.#radius ** 2; }
  static describeUnit() { return `unit=${Circle.#unit}`; }
}

class Rect extends Shape {
  #w; #h;
  constructor(w, h) { super("rect"); [this.#w, this.#h] = [w, h]; }
  get area() { return this.#w * this.#h; }
  hasArea() { return this.#w > 0 && this.#h > 0; }
  // `#field in obj` is a brand check: true only for genuine Rect instances.
  sameBrand(other) { return #w in other; }
}

const shapes = [new Circle(1), new Rect(3, 4)];
for (const s of shapes) out("  " + s.describe());

out("static getter:", Shape.count);
out("static method + new.target:", Circle.create(2) instanceof Circle, Shape.create(2) instanceof Circle, Circle.describeUnit());
out("private brand check (#x in obj):", shapes[1].sameBrand(shapes[1]), shapes[1].sameBrand(shapes[0]), shapes[1].hasArea());

class Temperature {
  constructor(celsius) { this.celsius = celsius; }
  static fromFahrenheit(f) { return new Temperature(((f - 32) * 5) / 9); }
  static [Symbol.hasInstance](value) { return typeof value === "number" || value instanceof Temperature; }
  toString() { return `${this.celsius.toFixed(1)}°C`; }
}
out("static factory:", Temperature.fromFahrenheit(212).toString());
out("custom instanceof:", 21 instanceof Temperature);

// ------------------------------------------------------------
section(5, "Iterators and generators");
// ------------------------------------------------------------

const range = {
  from: 1,
  to: 5,
  [Symbol.iterator]() {
    let current = this.from;
    const last = this.to;
    return { next: () => (current <= last ? { value: current++, done: false } : { done: true }) };
  },
};

out("custom iterable:", [...range]);
out("for..of destructuring:", [...range].map((n) => `<${n}>`).join(""));

function* fibonacci() {
  let [a, b] = [0, 1];
  while (true) {
    yield a;
    [a, b] = [b, a + b];
  }
}

const fib = fibonacci();
const firstTen = [];
while (firstTen.length < 10) firstTen.push(fib.next().value);
out("generator:", firstTen.join(", "));

function* delegating() {
  yield "start";
  yield* [1, 2, 3];
  yield "end";
}
out("yield*:", [...delegating()].join(" → "));

// Generators are two-way: .next(value) resumes inside the body.
function* accumulator() {
  let total = 0;
  while (true) {
    const received = yield total;
    if (received === undefined) return total;
    total += received;
  }
}
const acc = accumulator();
acc.next();
[10, 20, 30].forEach((n) => acc.next(n));
out("two-way generator:", acc.next().value);

// ------------------------------------------------------------
section(6, "Symbols and protocols");
// ------------------------------------------------------------

const ID = Symbol("id");
const point = { [ID]: 42, x: 1 };
out("symbol key:", point[ID], Symbol.keyFor(Symbol.for("shared")));
out("well-known:", typeof Symbol.asyncIterator, typeof Symbol.toPrimitive);

const price = {
  amount: 9.99,
  [Symbol.toPrimitive](hint) { return hint === "number" ? this.amount : `$${this.amount}`; },
};
out("Symbol.toPrimitive:", price * 2, `${price}`);

// ------------------------------------------------------------
section(7, "Collections and grouping");
// ------------------------------------------------------------

const inventory = [
  { item: "apple", kind: "fruit", qty: 4 },
  { item: "carrot", kind: "veg", qty: 7 },
  { item: "pear", kind: "fruit", qty: 2 },
  { item: "leek", kind: "veg", qty: 5 },
];
const byKind = Object.groupBy(inventory, (row) => row.kind);
out("Object.groupBy:", Object.keys(byKind).map((k) => `${k}×${byKind[k].length}`).join(", "));

const totals = Map.groupBy(inventory, (row) => row.kind);
for (const [kind, rows] of totals) {
  totals.set(kind, rows.reduce((sum, row) => sum + row.qty, 0));
}
out("Map.groupBy + reduce:", [...totals].map(([k, v]) => `${k}=${v}`).join(", "));

const evens = new Set([0, 2, 4, 6, 8]);
const primes = new Set([2, 3, 5, 7]);
out("Set.union:", [...evens.union(primes)].join(","));
out("Set.intersection:", [...evens.intersection(primes)].join(","));
out("Set.difference:", [...evens.difference(primes)].join(","));
out("Set.symmetricDifference:", [...primes.symmetricDifference(evens)].join(","));
out("Set.relation:", evens.isDisjointFrom(primes), new Set([2, 4]).isSubsetOf(evens));

const cache = new WeakMap();
const key = { id: "ephemeral" };
cache.set(key, { computed: true });
out("WeakMap:", cache.get(key).computed, cache.has({}));

// ------------------------------------------------------------
section(8, "Proxy and Reflect");
// ------------------------------------------------------------

const validator = {
  set(target, prop, value) {
    if (prop === "age" && (!Number.isInteger(value) || value < 0)) {
      throw new RangeError(`invalid age: ${value}`);
    }
    return Reflect.set(target, prop, value);
  },
  get(target, prop, receiver) {
    if (typeof prop === "string" && !(prop in target)) return `«missing:${prop}»`;
    return Reflect.get(target, prop, receiver);
  },
};

const profile = new Proxy({ name: "Grace", age: 0 }, validator);
profile.age = 85;
out("proxy get/set:", profile.name, profile.age, profile.unknown);
try {
  profile.age = -1;
} catch (err) {
  out("proxy invariant:", err.constructor.name, err.message);
}
out("Reflect.ownKeys:", Reflect.ownKeys(profile).join(", "));

// A memoize proxy: any method call is cached by name + arguments.
function memoize(fn) {
  const store = new Map();
  return new Proxy(fn, {
    apply(target, thisArg, args) {
      const cacheKey = JSON.stringify(args);
      if (!store.has(cacheKey)) store.set(cacheKey, Reflect.apply(target, thisArg, args));
      return store.get(cacheKey);
    },
  });
}
let calls = 0;
const slowDouble = memoize((n) => (calls++, n * 2));
out("memoized proxy:", slowDouble(21), slowDouble(21), `(computed ${calls}×)`);

// ------------------------------------------------------------
section(9, "BigInt and numeric formats");
// ------------------------------------------------------------

const huge = 2n ** 200n;
out("2n ** 200n has", huge.toString().length, "digits");
out("BigInt literal forms:", (0xff_ffn).toString(), (0b1010n).toString(), (0o777n).toString());
out("BigInt division truncates:", 7n / 2n, "remainder:", 7n % 2n);
out("BigInt.asUintN(8, 300n):", BigInt.asUintN(8, 300n));
out("Number separators:", 1_000_000, 0.1 + 0.2, Number.EPSILON);
out("big number formatting:", new Intl.NumberFormat("en-US").format(1234567.891));

// ------------------------------------------------------------
section(10, "Modern built-ins");
// ------------------------------------------------------------

const nums = [5, 3, 8, 1, 9, 2];
out("toSorted (copy):", nums.toSorted((x, y) => x - y), "original untouched:", nums.join(","));
out("toReversed:", nums.toReversed().join(","));
out("with(index):", nums.with(2, 99).join(","));
out("at(-1) / findLast:", nums.at(-1), nums.findLast((n) => n % 2 === 0));
out("flatMap:", [1, 2, 3].flatMap((n) => [n, n * n]).join(","));
out("Object.hasOwn:", Object.hasOwn(item, "label"), Object.hasOwn(item, "nope"));
out("Error.isError:", Error.isError(new TypeError("x")), Error.isError({}));
out("Promise.withResolvers:", typeof Promise.withResolvers);

const cloneTarget = { list: [1, 2, 3], nested: { when: new Date(0) }, map: new Map([["k", "v"]]) };
cloneTarget.self = cloneTarget;
const clone = structuredClone(cloneTarget);
out("structuredClone: cycle kept:", clone.self === clone, "date:", clone.nested.when.toISOString(), "map:", clone.map.get("k"));

let finalized = 0;
const registry = new FinalizationRegistry(() => finalized++);
const held = (() => {
  registry.register({ payload: "bye" }, "bye");
  return new WeakRef({ name: "weak target" });
})();
out("WeakRef/FinalizationRegistry:", held.deref().name, "collectable:", typeof held.deref());

// ------------------------------------------------------------
section(11, "Regular expressions");
// ------------------------------------------------------------

const logLine = "2026-09-18 12:34:56 [WARN] disk 91% full";
const parsed = /(?<date>\d{4}-\d{2}-\d{2})\s(?<time>\d{2}:\d{2}:\d{2})\s\[(?<level>\w+)]\s(?<msg>.*)/.exec(logLine);
out("named groups:", parsed.groups.date, parsed.groups.level, "-", parsed.groups.msg);

const { indices } = /(?<currency>\$)(?<amount>\d+)/d.exec("Total: $250");
out("match indices:", indices.groups.currency, indices.groups.amount);

out("lookbehind:", "price: $50, tax: $10".match(/(?<=\$)\d+/g).join(", "));
out("unicode property:", /\p{Script=Greek}+/u.test("αβγ"), /\p{Emoji}/u.test("🚀"));
out("RegExp.escape:", RegExp.escape("1 + 1 = 2"));
out("sticky flag:", /World/y.test("Hello World")); // anchored at lastIndex 0

// ------------------------------------------------------------
section(12, "Quick Intl use");
// ------------------------------------------------------------

const when = new Date(Date.UTC(2026, 8, 18, 12, 0, 0));
out("DateTimeFormat:", new Intl.DateTimeFormat("en-GB", { dateStyle: "full", timeZone: "UTC" }).format(when));
out("RelativeTimeFormat:", new Intl.RelativeTimeFormat("en", { numeric: "auto" }).format(-1, "day"));
out("ListFormat:", new Intl.ListFormat("en", { style: "long", type: "disjunction" }).format(["Ada", "Grace", "Alan"]));
out("PluralRules:", new Intl.PluralRules("en").select(1), new Intl.PluralRules("en").select(2));
out("Collator:", new Intl.Collator("en").compare("résumé", "resume") !== 0);
const segments = [...new Intl.Segmenter("en", { granularity: "word" }).segment("Hello, 世界!")].filter((s) => s.isWordLike).map((s) => s.segment);
out("Segmenter words:", segments.join("|"));

out("\nLanguage tour complete.");
