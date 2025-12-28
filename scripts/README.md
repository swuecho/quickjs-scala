# QuickJS-Scala

A JavaScript engine written in Scala 3 for the JVM, inspired by the QuickJS C implementation. The goal is to create a production-grade JavaScript engine with full ES2024+ support.

## Features

- **Complete ES2024+ support** (in progress)
- **Stack-based bytecode interpreter** - Matches QuickJS architecture
- **JVM GC integration** - Leverages mature JVM garbage collectors
- **Tagged union type system** - Type-safe JavaScript value representation
- **REPL** - Interactive JavaScript shell with debugging support
- **Standalone Runner** - Run JavaScript files like Node.js

## Quick Start

### Running JavaScript Files

The standalone runner allows you to execute JavaScript files similar to Node.js:

```bash
# Run a JavaScript file
sbt "runner/runMain quickjs.stdlib.Runner script.js"

# Evaluate inline JavaScript
sbt "runner/runMain quickjs.stdlib.Runner --eval \"console.log('Hello, World!')\""

# Show help
sbt "runner/runMain quickjs.stdlib.Runner --help"

# Show version
sbt "runner/runMain quickjs.stdlib.Runner --version"
```

### REPL (Interactive Shell)

For interactive development and debugging:

```bash
sbt "stdlib/run"
```

REPL features:
- Multi-line input support
- Debug/trace mode with `.debug` command
- Variable inspection with `.vars` command
- Load files with `.load script.js`
- Command history
- Tab completion

### Example JavaScript File

Create a file `script.js`:

```javascript
// Hello World
console.log("Hello from QuickJS-Scala!");

// Variables
var x = 10;
var y = 20;
console.log("x + y =", x + y);

// Arrays
var arr = [1, 2, 3, 4, 5];
console.log("Array:", arr);
console.log("Doubled:", arr.map(x => x * 2));

// Objects
var obj = { name: "QuickJS", version: "0.2.0" };
console.log("Object:", obj);

// Control flow
for (var i = 0; i < 5; i++) {
  console.log("Count:", i);
}

// Functions
function add(a, b) {
  return a + b;
}
console.log("add(5, 3) =", add(5, 3));
```

Run it:

```bash
sbt "runner/runMain quickjs.stdlib.Runner script.js"
```

## Building

```bash
# Compile all modules
sbt compile

# Compile specific module
sbt "core/compile"
sbt "parser/compile"
sbt "compiler/compile"
sbt "runtime/compile"
sbt "stdlib/compile"
sbt "runner/compile"

# Clean build
sbt clean
```

## Testing

```bash
# Run all tests
sbt test

# Run specific test suite
sbt "stdlib/testOnly quickjs.stdlib.QuickJSLanguageTest"

# Run specific test
sbt "stdlib/testOnly quickjs.stdlib.QuickJSLanguageTest -- -z \"arithmetic\""
```

## Project Structure

```
quickjs-scala/
├── build.sbt                    # SBT multi-project build
├── core/                        # Core type system
│   └── src/main/scala/quickjs/
│       ├── value/               # JSValue tagged union
│       ├── runtime/             # JSRuntime, JSContext
│       ├── atom/                # Atom table (string interning)
│       └── objmodel/            # JSObject, properties
├── parser/                      # ES2024+ parser
│   └── src/main/scala/quickjs/
│       ├── ast/                 # AST nodes
│       ├── lexer/               # Lexer implementation
│       └── parser/              # Parser (recursive descent)
├── compiler/                    # Bytecode compiler
│   └── src/main/scala/quickjs/
│       ├── bytecode/            # Opcode definitions
│       └── compiler/            # Compiler orchestration
├── runtime/                     # Interpreter & REPL
│   └── src/main/scala/quickjs/
│       ├── interpreter/         # Bytecode interpreter
│       ├── repl/                # REPL implementation
│       └── runtime/             # Standard library bindings
├── stdlib/                      # Standard library & tests
│   └── src/main/scala/quickjs/
│       └── stdlib/              # Array, Math, String, JSON
└── runner/                      # Standalone runner
    └── src/main/scala/quickjs/
        └── stdlib/
            └── Runner.scala     # Main entry point for running files
```

## Supported JavaScript Features

### ✅ Fully Implemented

- **Literals**: Numbers, strings, booleans, null, undefined, objects, arrays
- **Variables**: `var`, `let`, `const`
- **Operators**: Arithmetic, comparison, logical, bitwise, typeof, instanceof, in, delete
- **Control Flow**: `if/else`, `while`, `for`, `do/while`, `switch`, `break`, `continue`
- **Functions**: Declarations, expressions, arrow functions, closures
- **Objects**: Literals, property access, method calls
- **Arrays**: Literals, element access, methods (map, filter, push, pop, etc.)
- **Labeled Statements**: `break label`, `continue label`
- **Standard Library**: Array methods, Math functions, String methods, JSON

### 🚧 In Progress

- Error handling (try/catch/finally, throw)
- Regular expressions
- Classes and prototypes
- Modules (ES6 import/export)
- Async/await and Promises
- Generators and iterators

## Architecture

### Design Decisions

1. **Stack-based bytecode interpreter** (not JVM bytecode)
   - Easier to port from QuickJS C
   - Better control over semantics
   - Trade-off: Slower than direct JVM bytecode

2. **JVM GC integration** (not custom mark-and-sweep)
   - Eliminates 2,000+ lines of GC code
   - Leverages mature JVM collectors (G1, ZGC)
   - Trade-off: Less control over GC pauses

3. **Tagged union type system** for JavaScript values
   - Type-safe with sealed traits
   - Smart constructors for optimization
   - Inline storage for primitives

## Documentation

See `/docs/README.md` for:
- Comprehensive project status
- Progress tracking
- Development workflow
- Technical comparisons

## Status

**Version**: 0.2.0

**Test Coverage**: 84% (171/203 passing)

**Phase**: Core language implementation complete, standard library in progress

## Contributing

This is an educational project demonstrating Scala 3's capabilities for systems programming. The architecture prioritizes:
- Type safety
- Code clarity
- Maintainability
- Correctness

over raw performance.

## License

This project is inspired by and based on the QuickJS C implementation by Fabrice Bellard.

## References

- Original QuickJS: https://bellard.org/quickjs/
- ES2024 Specification: https://tc39.es/ecma262/
