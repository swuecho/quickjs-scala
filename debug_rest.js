// Exact code from test
var result = (function() {
  var [a, ...rest] = [1, 2, 3, 4, 5];
  return a + rest.length + rest[0] + rest[1];
})();
console.log("result:", result);
