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
| [PROGRESS.md](PROGRESS.md) | **Comprehensive project status, test results, roadmap** | 2025-12-28 |
| [RECENT_WORK.md](RECENT_WORK.md) | **Latest development sessions** | 2025-12-28 |

### Planning & Design

| Document | Description | Last Updated |
|----------|-------------|--------------|
| [SCALA_REWRITE_PLAN.md](SCALA_REWRITE_PLAN.md) | Original rewrite plan from C to Scala | 2025-12-28 |
| [ideas.md](ideas.md) | Ideas and future enhancements | 2025-12-26 |

### Feature Documentation

| Document | Description | Last Updated |
|----------|-------------|--------------|
| [LABELED_STATEMENTS.md](LABELED_STATEMENTS.md) | Labeled statements implementation (break/continue with labels) | 2025-12-28 |
| [DEBUGGER_SUPPORT.md](DEBUGGER_SUPPORT.md) | Debugger support with breakpoints | 2025-12-28 |
| [LET_CONST_SCOPE_IMPLEMENTATION.md](LET_CONST_SCOPE_IMPLEMENTATION.md) | Let/const block scoping implementation | 2025-12-27 |
| [CLOSURE_IMPLEMENTATION.md](CLOSURE_IMPLEMENTATION.md) | Closure implementation details | 2025-12-25 |
| [CLOSURE_ALIGNMENT.md](CLOSURE_ALIGNMENT.md) | Closure alignment with QuickJS C | 2025-12-25 |

### Technical Comparisons

| Document | Description | Last Updated |
|----------|-------------|--------------|
| [QUICKJS_COMPARISON.md](QUICKJS_COMPARISON.md) | Comparison with QuickJS C implementation | 2025-12-25 |
| [PARSER_COMPARISON.md](PARSER_COMPARISON.md) | Parser implementation comparison | 2025-12-25 |
| [REPL.md](REPL.md) | REPL design and implementation | 2025-12-25 |

## Quick Reference

### Project Statistics
- **Total Lines**: ~25,000+
- **Test Coverage**: 99.5% (224/225 passing)
- **Core Language**: 100% (29/29 passing)
- **ES2024+ Features**: ~70% implemented

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
| `core/.../JSValue.scala` | Type system, smart constructors | 240 |
| `runtime/.../Interpreter.scala` | Bytecode execution | 1,791 |
| `compiler/.../Compiler.scala` | AST to bytecode | 2,836 |
| `parser/.../Parser.scala` | Hand-written recursive descent parser | 1,794 |
| `lexer/.../Lexer.scala` | Tokenization | ~200 |

## Test Results Summary

```
QuickJSLanguageTest    ✅ 29/29 (100%)  - Core language features
QuickJSLoopTest        ✅ All passing   - Loops and labeled statements
QuickJSClosureTest     ✅ All passing   - Closures and variable capture
FunctionExpressionTest ✅ All passing   - Function expressions and arrows
ComprehensiveTest      ✅ All passing   - Advanced features
JSONTest               ✅ Most passing  - JSON parsing/stringifying
Other test suites      ✅ All passing   - REPL, interpreter, operators
Overall:               ✅ 224/225 (99.5%)
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

**Phase 2 Complete**: Core Language Implementation
- ✅ **Complete**: Arithmetic, control flow, functions, objects, arrays
- ✅ **Complete**: typeof, instanceof, new operator, this binding
- ✅ **Complete**: Let/const block scoping
- ✅ **Complete**: Labeled statements (break/continue with labels)
- ✅ **Complete**: Debugger support with breakpoints
- ✅ **Complete**: REPL with debugging integration
- ✅ **Mostly Complete**: Standard library (Math, String, JSON)
- ⚠️ **In Progress**: Remaining edge cases

**Next Priority**: Standard library completion (JSON.stringify edge cases, String constructor)

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

**Last Updated**: 2025-12-28
**Project Status**: Phase 2 Complete - 99.5% test coverage
