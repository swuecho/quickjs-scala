// Demo script for QuickJS-Scala implemented features

function section(title) {
  console.log("\n==", title, "==");
}

section("Variables and Expressions");
var a = 3;
let b = 4;
const c = 5;
console.log("a + b + c =", a + b + c);
console.log("typeof c =", typeof c);

section("Functions, Closures, and Arrow Functions");
function add(x, y) {
  return x + y;
}
var mul = function (x, y) {
  return x * y;
};
var makeCounter = function () {
  var count = 0;
  return function () {
    count = count + 1;
    return count;
  };
};
var counter = makeCounter();
console.log("add(2, 3) =", add(2, 3));
console.log("mul(2, 3) =", mul(2, 3));
console.log("counter() =", counter(), counter());
console.log("arrow (x => x * 2)(6) =", (x => x * 2)(6));

section("Control Flow");
var sum = 0;
for (var i = 0; i < 5; i = i + 1) {
  if (i === 3) continue;
  sum = sum + i;
}
console.log("sum (skipping 3) =", sum);

var w = 0;
while (w < 3) {
  w = w + 1;
}
console.log("while result =", w);

var d = 0;
do {
  d = d + 1;
} while (d < 2);
console.log("do/while result =", d);

section("Labeled Statements");
var labelCount = 0;
outer: for (var x = 0; x < 3; x = x + 1) {
  inner: for (var y = 0; y < 3; y = y + 1) {
    if (x === 1 && y === 1) {
      labelCount = labelCount + 1;
      break outer;
    }
  }
}
console.log("labelCount =", labelCount);

section("Objects and Properties");
var obj = {
  name: "QuickJS-Scala",
  version: "0.2.0"
};
console.log("obj.name =", obj.name);
console.log("'name' in obj =", "name" in obj);
console.log("obj.hasOwnProperty? (delete test) =", delete obj.version, obj.version);

section("Arrays and Methods");
var arr = [1, 2, 3, 4, 5];
console.log("arr =", arr);
console.log("map x2 =", arr.map(x => x * 2));
console.log("filter even =", arr.filter(x => x % 2 === 0));
console.log("reduce sum =", arr.reduce(function (acc, x) { return acc + x; }, 0));
console.log("slice(1, 4) =", arr.slice(1, 4));
console.log("forEach sum =", (function () { var total = 0; arr.forEach(function (x) { total = total + x; }); return total; })());
console.log("concat =", arr.concat([6, 7]));
console.log("includes 3 =", arr.includes(3));
console.log("indexOf 4 =", arr.indexOf(4));
console.log("splice (remove 2, add 9,10) =", arr.splice(1, 2, 9, 10), arr);
console.log("push =", arr.push(11), arr);
console.log("pop =", arr.pop(), arr);
arr.customProp = "custom";
console.log("array custom prop =", arr.customProp);

section("Strings");
var s = "QuickJS-Scala";
console.log("startsWith 'Quick' =", s.startsWith("Quick"));
console.log("endsWith 'Scala' =", s.endsWith("Scala"));
console.log("indexOf 'Scala' =", s.indexOf("Scala"));
console.log("split =", s.split("-"));
console.log("replace =", s.replace("Scala", "JS"));
console.log("slice =", s.slice(0, 7));
console.log("padStart =", "7".padStart(3, "0"));
console.log("padEnd =", "7".padEnd(3, "0"));
console.log("match 'JS' =", s.match("JS"));

section("JSON");
var jsonText = JSON.stringify({
  name: "QJS",
  nums: [1, 2, 3],
  nested: { ok: true }
});
console.log("JSON.stringify =", jsonText);
var parsed = JSON.parse(jsonText, function (key, value) {
  if (key === "name") return value + "-parsed";
  return value;
});
console.log("JSON.parse reviver =", parsed.name);

section("Date");
var epoch = new Date(0);
console.log("Date(0).toISOString =", epoch.toISOString());
var t = Date.parse("1970-01-01T00:00:00.000Z");
console.log("Date.parse =", t);
var utc = Date.UTC(2000, 0, 1, 0, 0, 0);
console.log("Date.UTC =", utc);
var now = new Date(utc);
now.setUTCHours(12);
console.log("setUTCHours =", now.toISOString());

section("Operators");
console.log("typeof null =", typeof null);
console.log("void 0 =", void 0);
console.log("delete arr[0] =", delete arr[0], arr[0]);
console.log("(arr instanceof Array) =", arr instanceof Array);

console.log("\nDemo completed.");
