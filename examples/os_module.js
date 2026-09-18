const os = require('os');

class Greeter {
  #name;
  constructor(name) { this.#name = name; }
  greet() { return `Hello, ${this.#name}!`; }
}

console.log(new Greeter(os.platform()).greet());
console.log([1, 2, 3].map(x => x * 2));
