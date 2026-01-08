// ============================================================
// QuickJS-Scala Feature Demo
// This file demonstrates all JavaScript features currently
// supported by the QuickJS-Scala implementation.
// ============================================================

console.log("=== QuickJS-Scala Feature Demo ===\n");

// ============================================================
// 1. VARIABLES (var, let, const)
// ============================================================
console.log("--- 1. Variables ---");

var globalVar = "I'm a var";
let blockScoped = "I'm a let";
const constant = "I'm a const";

console.log(globalVar);
console.log(blockScoped);
console.log(constant);

// Block scoping
{
    let innerLet = "inner";
    const innerConst = "inner const";
    console.log("Block scoped:", innerLet, innerConst);
}

// ============================================================
// 2. DATA TYPES
// ============================================================
console.log("\n--- 2. Data Types ---");

var num = 42;
var float = 3.14159;
var str = "Hello, World!";
var bool = true;
var nul = null;
var undef = undefined;
var arr = [1, 2, 3];
var obj = { name: "test", value: 123 };

console.log("Number:", num, typeof num);
console.log("Float:", float, typeof float);
console.log("String:", str, typeof str);
console.log("Boolean:", bool, typeof bool);
console.log("Null:", nul, typeof nul);
console.log("Undefined:", undef, typeof undef);
console.log("Array:", arr, typeof arr);
console.log("Object:", obj, typeof obj);

// ============================================================
// 3. OPERATORS
// ============================================================
console.log("\n--- 3. Operators ---");

// Arithmetic
console.log("10 + 3 =", 10 + 3);
console.log("10 - 3 =", 10 - 3);
console.log("10 * 3 =", 10 * 3);
console.log("10 / 3 =", 10 / 3);
console.log("10 % 3 =", 10 % 3);
console.log("2 ** 10 =", 2 ** 10);

// Comparison
console.log("5 == '5':", 5 == '5');
console.log("5 === '5':", 5 === '5');
console.log("5 != '5':", 5 != '5');
console.log("5 !== '5':", 5 !== '5');

// Logical
console.log("true && false:", true && false);
console.log("true || false:", true || false);
console.log("!true:", !true);

// Bitwise
console.log("5 & 3:", 5 & 3);
console.log("5 | 3:", 5 | 3);
console.log("5 ^ 3:", 5 ^ 3);
console.log("~5:", ~5);
console.log("5 << 1:", 5 << 1);
console.log("5 >> 1:", 5 >> 1);
console.log("-5 >>> 1:", -5 >>> 1);

// Nullish coalescing
var nullValue = null;
var definedValue = "defined";
console.log("null ?? 'default':", nullValue ?? 'default');
console.log("'defined' ?? 'default':", definedValue ?? 'default');

// Optional chaining
var nested = { a: { b: { c: 42 } } };
console.log("nested?.a?.b?.c:", nested?.a?.b?.c);
console.log("nested?.x?.y?.z:", nested?.x?.y?.z);

// typeof, instanceof, in, delete
console.log("typeof []:", typeof []);
console.log("[] instanceof Array:", [] instanceof Array);
console.log("'name' in obj:", 'name' in obj);

var toDelete = { a: 1, b: 2 };
delete toDelete.a;
console.log("After delete:", toDelete);

// ============================================================
// 4. CONTROL FLOW
// ============================================================
console.log("\n--- 4. Control Flow ---");

// if/else
var x = 10;
if (x > 5) {
    console.log("x > 5");
} else if (x === 5) {
    console.log("x === 5");
} else {
    console.log("x < 5");
}

// Ternary operator
var result = x > 5 ? "greater" : "not greater";
console.log("Ternary result:", result);

// switch
var day = 3;
switch (day) {
    case 1: console.log("Monday"); break;
    case 2: console.log("Tuesday"); break;
    case 3: console.log("Wednesday"); break;
    default: console.log("Other day");
}

// while loop
var i = 0;
var sum = 0;
while (i < 5) {
    sum += i;
    i++;
}
console.log("While loop sum (0-4):", sum);

// do-while loop
var j = 0;
do {
    j++;
} while (j < 3);
console.log("Do-while result:", j);

// for loop
var forSum = 0;
for (var k = 1; k <= 5; k++) {
    forSum += k;
}
console.log("For loop sum (1-5):", forSum);

// for-in loop
var person = { name: "Alice", age: 30, city: "NYC" };
console.log("For-in loop:");
for (var key in person) {
    console.log("  " + key + ": " + person[key]);
}

// for-of loop
var colors = ["red", "green", "blue"];
console.log("For-of loop:");
for (var color of colors) {
    console.log("  " + color);
}

// for-of with string
console.log("For-of with string 'Hi':");
for (var char of "Hi") {
    console.log("  " + char);
}

// Labeled statements
outer: for (var a = 0; a < 3; a++) {
    for (var b = 0; b < 3; b++) {
        if (a === 1 && b === 1) {
            console.log("Breaking outer at", a, b);
            break outer;
        }
    }
}

// ============================================================
// 5. FUNCTIONS
// ============================================================
console.log("\n--- 5. Functions ---");

// Function declaration
function greet(name) {
    return "Hello, " + name + "!";
}
console.log(greet("World"));

// Function expression
var multiply = function(a, b) {
    return a * b;
};
console.log("3 * 4 =", multiply(3, 4));

// Arrow functions
var add = (a, b) => a + b;
console.log("Arrow: 2 + 3 =", add(2, 3));

var square = x => x * x;
console.log("Arrow: 5^2 =", square(5));

var sayHello = () => "Hello!";
console.log("Arrow no params:", sayHello());

// Arrow with block body
var factorial = n => {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
};
console.log("Factorial(5):", factorial(5));

// Default parameters
function greetWithDefault(name, greeting) {
    greeting = greeting || "Hello";
    return greeting + ", " + name;
}
console.log(greetWithDefault("Alice"));
console.log(greetWithDefault("Bob", "Hi"));

// Rest parameters (in destructuring)
function sumAll() {
    var total = 0;
    for (var i = 0; i < arguments.length; i++) {
        total += arguments[i];
    }
    return total;
}
console.log("Sum all (1,2,3,4,5):", sumAll(1, 2, 3, 4, 5));

// Closures
function counter() {
    var count = 0;
    return function() {
        count++;
        return count;
    };
}
var myCounter = counter();
console.log("Counter:", myCounter(), myCounter(), myCounter());

// Higher-order functions
function applyTwice(fn, x) {
    return fn(fn(x));
}
console.log("Apply twice (x+1, 5):", applyTwice(x => x + 1, 5));

// Function.prototype methods
function introduce(greeting) {
    return greeting + ", I'm " + this.name;
}
var alice = { name: "Alice" };
console.log("call:", introduce.call(alice, "Hi"));
console.log("apply:", introduce.apply(alice, ["Hello"]));
var boundIntro = introduce.bind(alice);
console.log("bind:", boundIntro("Hey"));

// ============================================================
// 6. OBJECTS
// ============================================================
console.log("\n--- 6. Objects ---");

// Object literal
var book = {
    title: "JavaScript Guide",
    pages: 300,
    author: {
        name: "John Doe",
        email: "john@example.com"
    },
    getInfo: function() {
        return this.title + " by " + this.author.name;
    }
};
console.log("Book:", book.title, "-", book.pages, "pages");
console.log("Author:", book.author.name);
console.log("Info:", book.getInfo());

// Property shorthand
var name = "Test";
var value = 42;
var shorthand = { name, value };
console.log("Shorthand:", shorthand);

// Computed property names
var propName = "dynamic";
var computed = { [propName]: "value", ["key" + 2]: "value2" };
console.log("Computed:", computed);

// Object methods
console.log("Object.keys:", Object.keys(book));
console.log("Object.values:", Object.values(shorthand));
console.log("Object.entries:", Object.entries(shorthand));

var merged = Object.assign({}, { a: 1 }, { b: 2 });
console.log("Object.assign:", merged);

var created = Object.create({ inherited: true });
created.own = "property";
console.log("Object.create - own:", created.own);
console.log("Object.create - inherited:", created.inherited);

// Property descriptors
var desc = {};
Object.defineProperty(desc, 'readonly', {
    value: 42,
    writable: false,
    enumerable: true
});
console.log("Defined property:", desc.readonly);

// Getters and setters
var temperature = {
    _celsius: 0,
    get fahrenheit() {
        return this._celsius * 9/5 + 32;
    },
    set fahrenheit(f) {
        this._celsius = (f - 32) * 5/9;
    }
};
temperature.fahrenheit = 68;
console.log("Celsius:", temperature._celsius);
console.log("Fahrenheit:", temperature.fahrenheit);

// Object spread operator
var base = { x: 1, y: 2 };
var extended = { ...base, z: 3 };
console.log("Object spread:", extended);
var merged2 = { ...base, ...{ a: 10, b: 20 }, z: 100 };
console.log("Multiple spreads:", merged2);

// ============================================================
// 7. ARRAYS
// ============================================================
console.log("\n--- 7. Arrays ---");

var numbers = [1, 2, 3, 4, 5];
console.log("Array:", numbers);
console.log("Length:", numbers.length);
console.log("First:", numbers[0]);
console.log("Last:", numbers[numbers.length - 1]);

// Array methods
numbers.push(6);
console.log("After push(6):", numbers);

var popped = numbers.pop();
console.log("After pop():", numbers, "- popped:", popped);

var shifted = numbers.shift();
console.log("After shift():", numbers, "- shifted:", shifted);

numbers.unshift(0);
console.log("After unshift(0):", numbers);

console.log("slice(1, 3):", numbers.slice(1, 3));
console.log("concat([6,7]):", numbers.concat([6, 7]));
console.log("indexOf(3):", numbers.indexOf(3));
console.log("includes(3):", numbers.includes(3));
console.log("join('-'):", numbers.join('-'));

// Functional methods
var doubled = numbers.map(x => x * 2);
console.log("map(x*2):", doubled);

var evens = numbers.filter(x => x % 2 === 0);
console.log("filter(even):", evens);

var total = numbers.reduce((acc, x) => acc + x, 0);
console.log("reduce(sum):", total);

var hasEven = numbers.some(x => x % 2 === 0);
console.log("some(even):", hasEven);

var allPositive = numbers.every(x => x >= 0);
console.log("every(>=0):", allPositive);

var found = numbers.find(x => x > 2);
console.log("find(>2):", found);

var foundIndex = numbers.findIndex(x => x > 2);
console.log("findIndex(>2):", foundIndex);

// forEach
console.log("forEach:");
[10, 20, 30].forEach(function(val, idx) {
    console.log("  [" + idx + "] = " + val);
});

// sort and reverse
var unsorted = [3, 1, 4, 1, 5, 9, 2, 6];
console.log("Sorted:", unsorted.slice().sort((a, b) => a - b));
console.log("Reversed:", [1, 2, 3].reverse());

// flat and flatMap
var nested2 = [[1, 2], [3, 4], [5]];
console.log("flat:", nested2.flat());

var nums2 = [1, 2, 3];
console.log("flatMap:", nums2.flatMap(x => [x, x * 2]));

// fill and copyWithin
var filled = [1, 2, 3, 4, 5];
filled.fill(0, 1, 3);
console.log("fill(0,1,3):", filled);

// Array.from and Array.of
console.log("Array.from('abc'):", Array.from('abc'));
console.log("Array.of(1,2,3):", Array.of(1, 2, 3));
console.log("Array.isArray([]):", Array.isArray([]));

// Array spread operator
var arr1 = [1, 2];
var arr2 = [3, 4];
var combined = [...arr1, ...arr2, 5];
console.log("Array spread:", combined);
var cloned = [...arr1];
console.log("Array clone:", cloned);

// ============================================================
// 8. STRINGS
// ============================================================
console.log("\n--- 8. Strings ---");

var greeting = "Hello, World!";
console.log("String:", greeting);
console.log("Length:", greeting.length);
console.log("charAt(0):", greeting.charAt(0));
console.log("charCodeAt(0):", greeting.charCodeAt(0));
console.log("indexOf('o'):", greeting.indexOf('o'));
console.log("lastIndexOf('o'):", greeting.lastIndexOf('o'));
console.log("slice(0, 5):", greeting.slice(0, 5));
console.log("substring(7, 12):", greeting.substring(7, 12));
console.log("toUpperCase():", greeting.toUpperCase());
console.log("toLowerCase():", greeting.toLowerCase());
console.log("split(', '):", greeting.split(', '));
console.log("replace('World', 'JS'):", greeting.replace('World', 'JS'));
console.log("includes('World'):", greeting.includes('World'));
console.log("startsWith('Hello'):", greeting.startsWith('Hello'));
console.log("endsWith('!'):", greeting.endsWith('!'));
console.log("trim('  hi  '):", "  hi  ".trim());
console.log("padStart(10, '0'):", "42".padStart(5, '0'));
console.log("padEnd(10, '.'):", "hi".padEnd(5, '.'));
console.log("repeat(3):", "ab".repeat(3));

// ============================================================
// 9. TEMPLATE LITERALS
// ============================================================
console.log("\n--- 9. Template Literals ---");

var firstName = "John";
var lastName = "Doe";
var age2 = 30;

console.log(`Hello, ${firstName} ${lastName}!`);
console.log(`Age: ${age2}, Birth year: ${2024 - age2}`);
console.log(`Expression: ${2 + 2 * 3}`);
console.log(`Nested: ${`inner ${1 + 1}`}`);

// Multi-line
var multiLine = `Line 1
Line 2
Line 3`;
console.log("Multi-line:", multiLine);

// ============================================================
// 10. DESTRUCTURING
// ============================================================
console.log("\n--- 10. Destructuring ---");

// Array destructuring
var [first, second, third] = [1, 2, 3];
console.log("Array destructuring:", first, second, third);

var [head, ...tail] = [1, 2, 3, 4, 5];
console.log("Rest in array:", head, tail);

var [a2 = 10, b2 = 20] = [1];
console.log("Default values:", a2, b2);

// Object destructuring
var { title: bookTitle, pages: pageCount } = { title: "JS Book", pages: 200 };
console.log("Object destructuring:", bookTitle, pageCount);

var { x: x2 = 0, y: y2 = 0 } = { x: 5 };
console.log("Object with defaults:", x2, y2);

// Nested destructuring
var { author: { name: authorName } } = { author: { name: "Jane" } };
console.log("Nested destructuring:", authorName);

// Function parameter destructuring
function printPerson({ name: n, age: a }) {
    console.log("Person:", n, "age", a);
}
printPerson({ name: "Alice", age: 25 });

// ============================================================
// 11. CLASSES
// ============================================================
console.log("\n--- 11. Classes ---");

// Class declaration
class Animal {
    constructor(name) {
        this.name = name;
    }

    speak() {
        return this.name + " makes a sound";
    }

    static info() {
        return "Animals are living beings";
    }

    get upperName() {
        return this.name.toUpperCase();
    }

    set upperName(val) {
        this.name = val.toLowerCase();
    }
}

var animal = new Animal("Generic");
console.log("Animal:", animal.name);
console.log("Speak:", animal.speak());
console.log("Static:", Animal.info());
console.log("Getter:", animal.upperName);
animal.upperName = "NEW";
console.log("After setter:", animal.name);

// Inheritance
class Dog extends Animal {
    constructor(name, breed) {
        super(name);
        this.breed = breed;
    }

    speak() {
        return this.name + " barks";
    }

    fetch() {
        return this.name + " fetches the ball";
    }
}

var dog = new Dog("Buddy", "Labrador");
console.log("Dog:", dog.name, "-", dog.breed);
console.log("Dog speaks:", dog.speak());
console.log("Dog fetches:", dog.fetch());
console.log("Dog instanceof Dog:", dog instanceof Dog);
console.log("Dog instanceof Animal:", dog instanceof Animal);

// Class expression
var Cat = class {
    constructor(name) {
        this.name = name;
    }
    meow() {
        return this.name + " says meow";
    }
};
var cat = new Cat("Whiskers");
console.log("Cat:", cat.meow());

// Static fields
class Counter2 {
    static count = 0;

    constructor() {
        Counter2.count++;
    }

    static getCount() {
        return Counter2.count;
    }
}
new Counter2();
new Counter2();
new Counter2();
console.log("Counter static:", Counter2.getCount());

// ============================================================
// 12. ERROR HANDLING
// ============================================================
console.log("\n--- 12. Error Handling ---");

// try/catch/finally
try {
    throw new Error("Something went wrong");
} catch (e) {
    console.log("Caught error:", e.message);
} finally {
    console.log("Finally block executed");
}

// Different error types
try {
    throw new TypeError("Type mismatch");
} catch (e) {
    console.log("TypeError caught:", e.name, "-", e.message);
}

try {
    throw new ReferenceError("Variable not found");
} catch (e) {
    console.log("ReferenceError caught:", e.name, "-", e.message);
}

// Custom error handling
function divide(a, b) {
    if (b === 0) {
        throw new Error("Division by zero");
    }
    return a / b;
}

try {
    console.log("10 / 2 =", divide(10, 2));
    console.log("10 / 0 =", divide(10, 0));
} catch (e) {
    console.log("Division error:", e.message);
}

// ============================================================
// 13. MATH OBJECT
// ============================================================
console.log("\n--- 13. Math Object ---");

console.log("Math.PI:", Math.PI);
console.log("Math.E:", Math.E);
console.log("Math.abs(-5):", Math.abs(-5));
console.log("Math.floor(4.7):", Math.floor(4.7));
console.log("Math.ceil(4.2):", Math.ceil(4.2));
console.log("Math.round(4.5):", Math.round(4.5));
console.log("Math.max(1,5,3):", Math.max(1, 5, 3));
console.log("Math.min(1,5,3):", Math.min(1, 5, 3));
console.log("Math.pow(2, 8):", Math.pow(2, 8));
console.log("Math.sqrt(16):", Math.sqrt(16));
console.log("Math.random() (0-1):", Math.random());
console.log("Math.sign(-10):", Math.sign(-10));
console.log("Math.trunc(4.7):", Math.trunc(4.7));

// ============================================================
// 14. NUMBER OBJECT
// ============================================================
console.log("\n--- 14. Number Object ---");

console.log("Number.MAX_VALUE:", Number.MAX_VALUE);
console.log("Number.MIN_VALUE:", Number.MIN_VALUE);
console.log("Number.POSITIVE_INFINITY:", Number.POSITIVE_INFINITY);
console.log("Number.NEGATIVE_INFINITY:", Number.NEGATIVE_INFINITY);
console.log("Number.NaN:", Number.NaN);
console.log("Number.isNaN(NaN):", Number.isNaN(NaN));
console.log("Number.isFinite(100):", Number.isFinite(100));
console.log("Number.isInteger(5.0):", Number.isInteger(5.0));
console.log("Number.parseInt('42'):", Number.parseInt('42'));
console.log("Number.parseFloat('3.14'):", Number.parseFloat('3.14'));
console.log("(3.14159).toFixed(2):", (3.14159).toFixed(2));

// ============================================================
// 15. DATE OBJECT
// ============================================================
console.log("\n--- 15. Date Object ---");

var now = new Date();
console.log("Current date:", now.toString());
console.log("Year:", now.getFullYear());
console.log("Month (0-11):", now.getMonth());
console.log("Date:", now.getDate());
console.log("Hours:", now.getHours());
console.log("Minutes:", now.getMinutes());
console.log("Seconds:", now.getSeconds());
console.log("Day of week (0-6):", now.getDay());
console.log("Timestamp:", now.getTime());
console.log("Date.now():", Date.now());

var specific = new Date(2024, 0, 15, 10, 30, 0);
console.log("Specific date:", specific.toISOString());

// ============================================================
// 16. REGULAR EXPRESSIONS
// ============================================================
console.log("\n--- 16. Regular Expressions ---");

var regex = /hello/i;
console.log("Test 'Hello World':", regex.test("Hello World"));
console.log("Test 'Goodbye':", regex.test("Goodbye"));

var emailRegex = /\w+@\w+\.\w+/;
console.log("Email test:", emailRegex.test("test@example.com"));

var text = "The quick brown fox";
console.log("Match 'quick':", text.match(/quick/));
console.log("Search 'brown':", text.search(/brown/));
console.log("Replace:", text.replace(/fox/, "dog"));

var global = /o/g;
console.log("Match all 'o':", "hello world".match(global));

// RegExp constructor
var dynamic = new RegExp("world", "i");
console.log("Dynamic regex:", dynamic.test("Hello World"));

// ============================================================
// 17. JSON
// ============================================================
console.log("\n--- 17. JSON ---");

var obj2 = { name: "John", age: 30, hobbies: ["reading", "gaming"] };
var jsonStr = JSON.stringify(obj2);
console.log("Stringify:", jsonStr);

var parsed = JSON.parse(jsonStr);
console.log("Parse:", parsed.name, parsed.age);

// Pretty print
console.log("Pretty:");
console.log(JSON.stringify(obj2, null, 2));

// ============================================================
// 18. MAP
// ============================================================
console.log("\n--- 18. Map ---");

var map = new Map();
map.set('a', 1);
map.set('b', 2);
map.set('c', 3);
console.log("Map size:", map.size);
console.log("Get 'b':", map.get('b'));
console.log("Has 'a':", map.has('a'));
console.log("Has 'x':", map.has('x'));

// Object as key
var objKey = { id: 1 };
map.set(objKey, "object value");
console.log("Object key:", map.get(objKey));

// Iteration
console.log("Map keys:", map.keys());
console.log("Map values:", map.values());
console.log("Map entries:", map.entries());

// forEach
var mapSum = 0;
var numMap = new Map([['x', 10], ['y', 20], ['z', 30]]);
numMap.forEach(function(value, key) {
    mapSum += value;
});
console.log("Map forEach sum:", mapSum);

// Chaining
var chained = new Map().set('a', 1).set('b', 2).set('c', 3);
console.log("Chained map size:", chained.size);

// delete and clear
map.delete('a');
console.log("After delete 'a':", map.has('a'));

// ============================================================
// 19. SET
// ============================================================
console.log("\n--- 19. Set ---");

var set = new Set();
set.add(1);
set.add(2);
set.add(3);
set.add(2); // Duplicate - ignored
console.log("Set size:", set.size);
console.log("Has 2:", set.has(2));
console.log("Has 5:", set.has(5));

// From array (deduplication)
var uniqueSet = new Set([1, 2, 2, 3, 3, 3, 4]);
console.log("Unique set size:", uniqueSet.size);
console.log("Unique values:", uniqueSet.values());

// forEach
var setSum = 0;
uniqueSet.forEach(function(value) {
    setSum += value;
});
console.log("Set forEach sum:", setSum);

// Chaining
var chainedSet = new Set().add(1).add(2).add(3);
console.log("Chained set size:", chainedSet.size);

// delete
set.delete(2);
console.log("After delete 2:", set.has(2));

// Set from string
var charSet = new Set("hello");
console.log("Chars in 'hello':", charSet.size); // h, e, l, o = 4

// ============================================================
// 20. PROXY
// ============================================================
console.log("\n--- 20. Proxy ---");

var target = { message: "Hello", count: 0 };
var handler = {
    get: function(obj, prop) {
        console.log("  Getting property:", prop);
        return obj[prop];
    },
    set: function(obj, prop, value) {
        console.log("  Setting property:", prop, "to", value);
        obj[prop] = value;
        return true;
    }
};

var proxy = new Proxy(target, handler);
console.log("Proxy get:", proxy.message);
proxy.count = 42;
console.log("Target after proxy set:", target.count);

// ============================================================
// 21. ADVANCED PATTERNS
// ============================================================
console.log("\n--- 21. Advanced Patterns ---");

// IIFE (Immediately Invoked Function Expression)
var iife = (function() {
    var private = "I'm private";
    return {
        getPrivate: function() { return private; }
    };
})();
console.log("IIFE:", iife.getPrivate());

// Module pattern
var Calculator = (function() {
    var result = 0;
    return {
        add: function(x) { result += x; return this; },
        subtract: function(x) { result -= x; return this; },
        getResult: function() { return result; }
    };
})();
Calculator.add(10).add(5).subtract(3);
console.log("Calculator result:", Calculator.getResult());

// Memoization
function memoize(fn) {
    var cache = {};
    return function(n) {
        if (cache[n] !== undefined) {
            return cache[n];
        }
        var result = fn(n);
        cache[n] = result;
        return result;
    };
}

var fib = memoize(function(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
});
console.log("Fibonacci(20):", fib(20));

// Currying
function curry(fn) {
    return function(a) {
        return function(b) {
            return fn(a, b);
        };
    };
}

var curriedAdd = curry(function(a, b) { return a + b; });
console.log("Curried add:", curriedAdd(3)(4));

// ============================================================
// SUMMARY
// ============================================================
console.log("\n=== Demo Complete ===");
console.log("QuickJS-Scala supports:");
console.log("- Variables: var, let, const");
console.log("- All operators including ??, ?.");
console.log("- Control flow: if, switch, for, while, for-in, for-of");
console.log("- Functions: declarations, expressions, arrows, closures");
console.log("- Classes: inheritance, static, getters/setters");
console.log("- Destructuring: arrays and objects");
console.log("- Template literals");
console.log("- Built-ins: Object, Array, String, Number, Math, Date, RegExp, JSON");
console.log("- Collections: Map, Set");
console.log("- Error handling: try/catch/finally");
console.log("- Proxy");
console.log("- And much more!");
