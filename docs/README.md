# QuickJS-Scala Documentation

This directory contains comprehensive documentation about the QuickJS-Scala project.

## Quick Start

**New to the project?** Start with:
1. [PROGRESS.md](PROGRESS.md) - Current status and test results
2. [RECENT_WORK.md](RECENT_WORK.md) - Latest development session

## Documentation Index

### Progress & Status

| Document | Description | Last Updated |
|----------|-------------|--------------|
| [PROGRESS.md](PROGRESS.md) | **Comprehensive project status, test results, roadmap** | 2025-12-26 |
| [RECENT_WORK.md](RECENT_WORK.md) | **Latest development session - number type optimization fixes** | 2025-12-26 |

### Planning & Design

| Document | Description | Last Updated |
|----------|-------------|--------------|
| [SCALA_REWRITE_PLAN.md](SCALA_REWRITE_PLAN.md) | Original rewrite plan from C to Scala | 2025-12-25 |
| [ideas.md](ideas.md) | Ideas and future enhancements | 2025-12-26 |

### Technical Comparisons

| Document | Description | Last Updated |
|----------|-------------|--------------|
| [QUICKJS_COMPARISON.md](QUICKJS_COMPARISON.md) | Comparison with QuickJS C implementation | 2025-12-25 |
| [PARSER_COMPARISON.md](PARSER_COMPARISON.md) | Parser implementation comparison | 2025-12-25 |
| [REPL.md](REPL.md) | REPL design and implementation | 2025-12-25 |

## Quick Reference

### Project Statistics
- **Total Lines**: ~8,000+
- **Test Coverage**: 84% (94/112 passing)
- **Core Language**: 100% (29/29 passing)
- **ES2024+ Features**: ~60% implemented

### Module Overview
```
core/       # Type system (JSValue), runtime model (JSContext, JSRuntime)
parser/     # Lexer, Parser, AST nodes
compiler/   # Bytecode compiler, opcode definitions
runtime/    # Stack-based bytecode interpreter
stdlib/     # Standard library (Array, Math, String, JSON)
```

### Key Files
| File | Purpose | Lines |
|------|---------|-------|
| `core/.../JSValue.scala` | Type system, smart constructors | ~200 |
| `runtime/.../Interpreter.scala` | Bytecode execution | ~970 |
| `compiler/.../Compiler.scala` | AST to bytecode | ~700 |
| `parser/.../Parser.scala` | AST generation | ~600 |
| `lexer/.../Lexer.scala` | Tokenization | ~200 |

## Test Results Summary

```
QuickJSLanguageTest    ✅ 29/29 (100%)  - Core language features
JSONTest               ⚠️  26/31 (84%)  - JSON parsing/stringifying
ComprehensiveTest      ⚠️  29/42 (69%)  - Advanced features
Other tests            ✅  All passing   - REPL, basics
```

## Development Workflow

### Running Tests
```bash
# All tests
sbt test

# Specific suite
sbt "stdlib/testOnly quickjs.stdlib.QuickJSLanguageTest"

# Specific test
sbt "stdlib/testOnly quickjs.stdlib.QuickJSLanguageTest -- -z \"arithmetic\""
```

### Building
```bash
# Compile all
sbt compile

# Specific module
sbt "core/compile"
sbt "runtime/compile"
sbt "compiler/compile"
```

## Current Status

**Phase 2**: Core Language Implementation
- ✅ **Complete**: Arithmetic, control flow, functions, objects, arrays
- ✅ **Complete**: typeof, instanceof, new operator, this binding
- ⚠️ **In Progress**: Standard library (Math, String, JSON)
- ❌ **Not Started**: Async/await, Promises, Generators, Modules

**Next Priority**: Fix remaining 18 test failures (see PROGRESS.md)

## Contributing

When making changes:
1. Update the relevant documentation
2. Add/update tests
3. Update RECENT_WORK.md with session notes
4. Update PROGRESS.md if milestones are reached

## References

- **QuickJS C**: `/home/hwu/dev/quickjs/quickjs.c` (60,000 lines)
- **QuickJS Opcodes**: `/home/hwu/dev/quickjs/quickjs-opcode.h`
- **Project Root**: `/home/hwu/dev/quickjs-scala/`

---

**Last Updated**: 2025-12-26
**Project Status**: Phase 2 Complete - 84% test coverage
