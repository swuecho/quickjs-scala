// Microbenchmarks for the interpreter. Run with:
//   java -jar runner/target/scala-3.7.4/quickjs-runner.jar scripts/bench-micro.js
// Each workload runs several times; the best time is reported.

function bench(name, fn, iters) {
  iters = iters || 5;
  var best = Infinity;
  for (var i = 0; i < iters; i++) {
    var t0 = Date.now();
    fn();
    var dt = Date.now() - t0;
    if (dt < best) best = dt;
  }
  console.log(name + ": " + best + "ms");
}

// 1. integer arithmetic loop
bench("int-arith      ", function () {
  var s = 0;
  for (var i = 0; i < 1000000; i++) { s += i; }
  return s;
});

// 2. float arithmetic
bench("float-arith    ", function () {
  var s = 0.5;
  for (var i = 0; i < 1000000; i++) { s = s * 1.000001 + 0.25; }
  return s;
});

// 3. function calls
function add1(x) { return x + 1; }
bench("func-call      ", function () {
  var s = 0;
  for (var i = 0; i < 500000; i++) { s = add1(s); }
  return s;
});

// 4. property read/write on an object
bench("prop-access    ", function () {
  var o = { x: 1, y: 2 };
  var s = 0;
  for (var i = 0; i < 1000000; i++) { s += o.x; o.x = s; }
  return s;
});

// 5. array element read/write
bench("array-elem     ", function () {
  var a = [];
  var s = 0;
  for (var i = 0; i < 1000000; i++) { a[i] = i; s += a[i]; }
  return s;
});

// 6. array push
bench("array-push     ", function () {
  var a = [];
  for (var i = 0; i < 200000; i++) { a.push(i); }
  return a.length;
});

// 7. string concat
bench("string-concat  ", function () {
  var s = "";
  for (var i = 0; i < 20000; i++) { s += "x"; }
  return s.length;
});

// 8. method call
bench("method-call    ", function () {
  var o = { n: 0, inc: function () { return this.n++; } };
  for (var i = 0; i < 500000; i++) { o.inc(); }
  return o.n;
});

// 9. closure call
bench("closure-call   ", function () {
  var x = 0;
  var f = function () { return ++x; };
  for (var i = 0; i < 500000; i++) { f(); }
  return x;
});

// 10. try/catch in loop
bench("try-catch      ", function () {
  var s = 0;
  for (var i = 0; i < 200000; i++) {
    try { s += i; } catch (e) { s++; }
  }
  return s;
});

// 11. object allocation
bench("obj-alloc      ", function () {
  var s = 0;
  for (var i = 0; i < 200000; i++) {
    var o = { a: i, b: i + 1 };
    s += o.a;
  }
  return s;
});

// 12. map get/set
bench("map-ops        ", function () {
  var m = new Map();
  var s = 0;
  for (var i = 0; i < 100000; i++) { m.set(i, i); s += m.get(i); }
  return s;
});

// 13. fiber-ish: recursion
function fib(n) { return n < 2 ? n : fib(n - 1) + fib(n - 2); }
bench("fib(20)        ", function () { return fib(20); });

// 14. regexp
var re = /[a-z]+([0-9]+)/g;
bench("regexp         ", function () {
  var n = 0;
  for (var i = 0; i < 20000; i++) {
    re.lastIndex = 0;
    var m = re.exec("abc123def456");
    if (m) n += m[1].length;
  }
  return n;
});

// 15. JSON round trip
bench("json           ", function () {
  var o = { a: 1, b: [1, 2, 3], c: "hello", d: { e: true } };
  var s = "";
  for (var i = 0; i < 10000; i++) {
    s = JSON.stringify(o);
    JSON.parse(s);
  }
  return s.length;
});
