// ============================================================
// QuickJS-Scala: Promises, async/await and async iteration
//
// Run:  java -jar quickjs-runner.jar examples/async_patterns.js
//
// Shows Promise combinators (all/allSettled/any/race/try),
// async/await, async generators, `for await`, microtask vs
// macrotask ordering, thenable adoption and AbortController.
// ============================================================

const out = console.log;
const sleep = (ms, value) => new Promise((resolve) => setTimeout(() => resolve(value), ms));

function section(n, title) {
  out(`\n── ${n}. ${title} ${"─".repeat(Math.max(2, 52 - title.length - n.toString().length))}`);
}

async function main() {
  // ----------------------------------------------------------
  section(1, "Promise states and chaining");
  // ----------------------------------------------------------

  const resolved = Promise.resolve("resolved-value");
  const rejected = Promise.reject(new Error("rejected-value"));
  rejected.catch(() => {}); // avoid an unhandled rejection

  out("state:", await resolved);
  out("rejection caught:", await rejected.catch((err) => err.message));
  out(
    "chain:",
    await resolved
      .then((v) => v.toUpperCase())
      .then((v) => `${v}!`)
      .finally(() => out("  [finally runs exactly once]"))
  );

  // ----------------------------------------------------------
  section(2, "Combinators");
  // ----------------------------------------------------------

  const delay = (ms, value) => sleep(ms, value);
  const fails = (ms, message) => sleep(ms).then(() => Promise.reject(new Error(message)));

  out("all:", await Promise.all([delay(5, "a"), delay(1, "b"), Promise.resolve("c")]));
  out(
    "allSettled:",
    (await Promise.allSettled([delay(2, 1), fails(1, "boom")])).map((r) =>
      r.status === "fulfilled" ? `ok(${r.value})` : `err(${r.reason.message})`
    )
  );
  out("any:", await Promise.any([fails(1, "slow loser"), delay(2, "winner")]));
  out("race:", await Promise.race([delay(1, "fast"), delay(30, "slow")]));

  // Promise.try runs sync code and turns a throw into a rejection.
  const risky = (shouldThrow) =>
    Promise.try(() => {
      if (shouldThrow) throw new Error("sync throw");
      return 42;
    });
  out("try (ok):", await risky(false), "try (throw):", await risky(true).catch((e) => e.message));

  // Promise.withResolvers exposes resolve/reject next to the promise.
  const { promise, resolve } = Promise.withResolvers();
  setTimeout(() => resolve("withResolvers"), 1);
  out("withResolvers:", await promise);

  // ----------------------------------------------------------
  section(3, "async/await control flow");
  // ----------------------------------------------------------

  async function fetchUser(id) {
    await sleep(2);
    if (id < 0) throw new RangeError(`no user ${id}`);
    return { id, name: `user-${id}` };
  }

  out("sequential:", (await fetchUser(1)).name, (await fetchUser(2)).name);
  out("concurrent:", (await Promise.all([fetchUser(3), fetchUser(4)])).map((u) => u.name));

  try {
    await fetchUser(-1);
  } catch (err) {
    out("try/catch across await:", err.constructor.name, "-", err.message);
  }

  // A rejected await inside a loop can be collected instead of aborting.
  const results = [];
  for (const id of [1, -1, 2]) {
    try {
      results.push((await fetchUser(id)).name);
    } catch {
      results.push("error");
    }
  }
  out("per-iteration handling:", results.join(", "));

  // async functions always return a Promise, even for sync throws.
  const neverThrows = (async () => {
    throw new TypeError("inside async");
  })();
  out("sync throw becomes rejection:", neverThrows instanceof Promise, await neverThrows.catch((e) => e.message));

  // ----------------------------------------------------------
  section(4, "Microtasks vs timers");
  // ----------------------------------------------------------

  const order = [];
  setTimeout(() => order.push("timeout"), 0);
  queueMicrotask(() => order.push("microtask"));
  Promise.resolve().then(() => order.push("promise.then"));
  const synchronousSuffix = order.concat("sync-end");
  await sleep(5);
  out("sync:", synchronousSuffix.join(" → "));
  out("after await:", order.join(" → "));

  // ----------------------------------------------------------
  section(5, "Thenables are adopted");
  // ----------------------------------------------------------

  const thenable = {
    then(onFulfilled) {
      setTimeout(() => onFulfilled("adopted from thenable"), 1);
    },
  };
  out("await thenable:", await thenable);
  out("Promise.resolve thenable:", await Promise.resolve({ then: (cb) => cb("resolved via then") }));
  const circular = Promise.resolve().then(() => circular);
  out("self-resolution rejects:", await circular.catch((e) => `${e.constructor.name}: self-resolution`));

  // ----------------------------------------------------------
  section(6, "Async generators and for await");
  // ----------------------------------------------------------

  // An async iterable: next() returns promises (here backed by a timer).
  // `for await` handles closing via return() when the loop exits early.
  function asyncRange(from, to, delayMs) {
    return {
      [Symbol.asyncIterator]() {
        let current = from;
        return {
          async next() {
            await sleep(delayMs);
            return current <= to ? { value: current++, done: false } : { value: undefined, done: true };
          },
          async return() {
            out("  [iterator closed early]");
            return { value: undefined, done: true };
          },
        };
      },
    };
  }

  const collected = [];
  for await (const value of asyncRange(1, 4, 1)) collected.push(value);
  out("async iterator:", collected.join(", "));

  for await (const value of asyncRange(1, 100, 1)) {
    if (value > 2) break;
  }

  // Async generators yield promises for each value; consume them with for await.
  async function* asyncLabels() {
    yield "first";
    yield "second";
    return "finished";
  }

  const labels = [];
  for await (const label of asyncLabels()) labels.push(label);
  out("async generator:", labels.join(", "));

  // for await works over sync iterables too (arrays, strings).
  const chars = [];
  for await (const ch of "abc") chars.push(ch);
  out("for await over a string:", chars.join("-"));

  function* syncRange(n) {
    for (let i = 1; i <= n; i++) yield i;
  }
  const squares = [];
  for await (const n of syncRange(4)) squares.push(n * n);
  out("for await over a sync generator:", squares.join(", "));

  // Async generators can be consumed with Promise.all over .next() ...
  const it = asyncLabels();
  const [first, second, third] = await Promise.all([it.next(), it.next(), it.next()]);
  out("manual .next():", first.value, second.value, third.done ? "(done)" : third.value);

  // ... or piped through an async transform over sync sources.
  async function* mapAsync(source, fn) {
    for (const value of source) yield fn(value);
  }
  function* naturals() {
    let n = 1;
    while (true) yield n++;
  }
  const doubled = [];
  for await (const value of mapAsync(naturals(), (n) => n * 2)) {
    doubled.push(value);
    if (value >= 10) break;
  }
  out("async pipeline:", doubled.join(", "));

  // ----------------------------------------------------------
  section(7, "Cancellation with AbortController");
  // ----------------------------------------------------------

  if (typeof AbortController === "undefined") {
    out("  (AbortController is installed in Node compatibility mode;");
    out("   rerun this script with --node to see this section)");
  } else {
    function slowOperation(signal) {
      return new Promise((resolve, reject) => {
        const timer = setTimeout(() => resolve("finished"), 50);
        signal.addEventListener("abort", () => {
          clearTimeout(timer);
          const err = new Error("The operation was aborted");
          err.name = "AbortError";
          reject(err);
        });
      });
    }

    const controller = new AbortController();
    setTimeout(() => controller.abort(), 1);
    try {
      await slowOperation(controller.signal);
    } catch (err) {
      out("aborted:", err.name, "-", err.message, "| signal.aborted:", controller.signal.aborted);
    }
  }

  out("\nAsync patterns complete.");
}

main().catch((err) => {
  console.error("example failed:", err);
  if (typeof process !== "undefined") process.exitCode = 1;
});
