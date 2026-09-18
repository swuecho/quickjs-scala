// ============================================================
// QuickJS-Scala: browser-style host globals (plain mode)
//
// Run:  java -jar quickjs-runner.jar examples/web_globals.js
//
// These globals work without --node: URL/URLSearchParams,
// TextEncoder/TextDecoder, atob/btoa, structuredClone, Intl and
// the timer functions.
// ============================================================

const out = console.log;

function section(n, title) {
  out(`\n── ${n}. ${title} ${"─".repeat(Math.max(2, 52 - title.length - n.toString().length))}`);
}

// ------------------------------------------------------------
section(1, "URL parsing and mutation");
// ------------------------------------------------------------
const url = new URL("https://user:pw@example.com:8443/api/items?page=2&tag=a#results");
out("href      :", url.href);
out("protocol  :", url.protocol, "| host:", url.host, "| port:", url.port);
out("pathname  :", url.pathname, "| hash:", url.hash);
out("username  :", url.username, "| password:", url.password.length > 0);
url.searchParams.set("page", "3");
url.searchParams.append("tag", "b");
url.hash = "#page-3";
out("mutated   :", url.href);

const relative = new URL("../other?x=1", "https://example.com/a/b/c");
out("relative  :", relative.href);
out("canParse  :", URL.canParse("https://example.com"), URL.canParse("not a url"));
out("URL.parse :", URL.parse("https://example.com/x")?.hostname, URL.parse("nope"));

// ------------------------------------------------------------
section(2, "URLSearchParams");
// ------------------------------------------------------------
const params = new URLSearchParams("b=2&a=1&a=0&c=hello+world");
out("size      :", params.size);
out("get('a')  :", params.get("a"), "| getAll('a'):", params.getAll("a").join(","));
out("has('c')  :", params.has("c"), "| c:", params.get("c"));
params.sort();
out("sorted    :", params.toString());
out("entries   :", [...params].map(([k, v]) => `${k}=${v}`).join("&"));
params.delete("a");
out("after del :", params.toString());

// ------------------------------------------------------------
section(3, "Base64 and text codecs");
// ------------------------------------------------------------
const encoder = new TextEncoder();
const decoder = new TextDecoder();

// btoa/atob are byte-oriented (Latin-1), exactly as in browsers.
out("btoa      :", btoa("QuickJS!"));
out("atob      :", atob(btoa("QuickJS!")));
out("latin1    :", atob(btoa("café")));

// Non-Latin-1 text goes through TextEncoder first.
const emojiBytes = encoder.encode("QuickJS 🚀");
let binary = "";
for (const byte of emojiBytes) binary += String.fromCharCode(byte);
out("unicode   :", btoa(binary));
out("round trip:", decoder.decode(Uint8Array.from(atob(btoa(binary)), (c) => c.charCodeAt(0))));

const bytes = encoder.encode("héllo, 世界");
out("encoded   :", bytes.length, "bytes:", [...bytes].slice(0, 8).join(","), "…");
out("decoded   :", decoder.decode(bytes));
out("utf8      :", decoder.decode(new Uint8Array([0x68, 0x69]), { stream: false }));

const target = new Uint8Array(4);
const written = encoder.encodeInto("abcdef", target);
out("encodeInto:", `read=${written.read} written=${written.written}`, [...target].join(","));
const small = new Uint8Array(4);
const partial = encoder.encodeInto("ab🚀", small);
out("partial   :", `read=${partial.read} written=${partial.written}`, [...small].join(","), "→", JSON.stringify(decoder.decode(small.subarray(0, partial.written))));

// ------------------------------------------------------------
section(4, "structuredClone");
// ------------------------------------------------------------
const original = {
  when: new Date(Date.UTC(2026, 8, 18, 12, 0, 0)),
  pattern: /quick(js)?/gi,
  counters: new Map([["a", 1], ["b", 2]]),
  tags: new Set(["x", "y"]),
  nested: { list: [1, 2, 3] },
  bytes: new Uint8Array([1, 2, 3, 4]),
};
original.self = original; // cycles survive the clone
const copy = structuredClone(original);

copy.nested.list.push(4);
out("independent:", original.nested.list.length === 3, copy.nested.list.length === 4);
out("cycle kept :", copy.self === copy, "| original cycle:", original.self === original);
out("types      :", copy.when instanceof Date, copy.pattern instanceof RegExp, copy.pattern.flags, copy.counters.get("b"), copy.tags.has("y"), copy.bytes instanceof Uint8Array);
out("typed array:", [...copy.bytes].join(","));

// ------------------------------------------------------------
section(5, "Intl quick tour");
// ------------------------------------------------------------
const number = 1234567.891;
out("decimal    :", new Intl.NumberFormat("en-US").format(number));
out("currency   :", new Intl.NumberFormat("de-DE", { style: "currency", currency: "EUR" }).format(number));
out("percent    :", new Intl.NumberFormat("en-US", { style: "percent", maximumFractionDigits: 1 }).format(0.1234));
out(
  "dateTime   :",
  new Intl.DateTimeFormat("en-GB", { dateStyle: "medium", timeStyle: "short", timeZone: "UTC" }).format(original.when)
);
out("collator   :", new Intl.Collator("en").compare("apple", "banana") < 0);
out("listFormat :", new Intl.ListFormat("en", { type: "conjunction" }).format(["a", "b", "c"]));
out("relTime    :", new Intl.RelativeTimeFormat("en", { numeric: "auto" }).format(2, "days"));
out("plural     :", new Intl.PluralRules("en").select(1), "/", new Intl.PluralRules("en").select(5));

// ------------------------------------------------------------
section(6, "Timers");
// ------------------------------------------------------------
(async () => {
  await new Promise((resolve) => {
    const order = [];
    const interval = setInterval(() => {
      order.push("tick");
      if (order.length === 3) {
        clearInterval(interval);
        setTimeout(() => {
          order.push("after-clear");
          out("interval   :", order.join(" → "));
          resolve();
        }, 1);
      }
    }, 1);
  });

  out("\nWeb globals complete.");
})();
