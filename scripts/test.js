// Test file for QuickJS-Scala runner

console.log("Hello from QuickJS-Scala!");

var x = 10;
var y = 20;
console.log("x + y =", x + y);

// Test arrays
var arr = [1, 2, 3, 4, 5];
console.log("Array:", arr);
console.log("Doubled:", arr.map(x => x * 2));

// Test objects
var obj = { name: "QuickJS", version: "0.2.0" };
console.log("Object:", obj);

console.log("Test completed successfully!");
