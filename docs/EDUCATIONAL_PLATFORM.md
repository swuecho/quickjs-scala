# QuickJS-Scala Educational Platform

## Project Overview

Build an interactive web-based platform that visualizes how a JavaScript engine works under the hood. Users can write JavaScript code and see real-time visualizations of:

- **AST (Abstract Syntax Tree)** - how code is parsed
- **Bytecode** - compiled instructions
- **Stack** - runtime stack changes
- **Memory** - variables, closures, objects
- **Execution** - step-by-step instruction highlighting

## Vision

Create the most comprehensive educational tool for understanding JavaScript engine internals. Similar to how "Python Tutor" helps people understand Python execution, this platform will make JavaScript engine internals visible and interactive.

**Goal**: Help developers, students, and enthusiasts understand:
- How JavaScript code becomes executable bytecode
- How stack-based interpreters work
- How closures capture variables
- How objects and prototypes work
- How the QuickJS engine architecture compares to V8, SpiderMonkey

---

## Architecture

### Tech Stack

```
┌─────────────────────────────────────────────────────────┐
│                    Frontend (Scala.js)                   │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │  Code Editor  │  │  Visualizer  │  │   Controls   │  │
│  │   (Monaco)    │  │  (Canvas)    │  │  (Buttons)   │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
└─────────────────────────────────────────────────────────┘
                           ↕ WebSocket
┌─────────────────────────────────────────────────────────┐
│                 Backend (QuickJS-Scala)                 │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │    Parser    │  │   Compiler   │  │  Tracing     │  │
│  │  (AST gen)   │  │ (Bytecode)   │  │ Interpreter  │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
└─────────────────────────────────────────────────────────┘
```

### Components

#### 1. Frontend (Scala.js + Web Technologies)
- **Code Editor**: Monaco Editor (VS Code's editor)
- **Visualizations**: HTML5 Canvas or SVG
- **UI Framework**: Laminar or Tyrian (Scala.js reactive UI)
- **Communication**: WebSocket or HTTP streaming

#### 2. Backend (QuickJS-Scala + HTTP Server)
- **QuickJS Engine**: Existing parser, compiler, interpreter
- **Tracing Mode**: New interpreter execution mode that captures:
  - Each instruction executed
  - Stack state before/after each instruction
  - Memory reads/writes
  - Branch decisions
- **API Server**: HTTP endpoints for compilation and execution
- **WebSocket Server**: Real-time execution streaming

#### 3. Shared Code (Cross-compiled)
- AST definitions
- Bytecode opcodes
- Value types
- Serialization for frontend consumption

---

## Features

### Phase 1: MVP (Minimum Viable Platform)

#### 1.1 Code Editor + Execution
- Monaco Editor for JavaScript input
- "Run" button to execute code
- Console output display
- Basic error handling

#### 1.2 AST Visualization
- Tree view of parsed AST
- Expandable/collapsible nodes
- Hover for node types and properties
- Highlight source code → AST node mapping

#### 1.3 Bytecode Display
- List of compiled bytecode instructions
- Instruction highlighting (current position)
- Operand display (constants, variables)
- Line number mapping to source

### Phase 2: Runtime Visualization

#### 2.1 Stack Visualization
- Visual stack (push/pop animations)
- Stack frames for function calls
- Value type indicators (Int32, Float64, String, Object, etc.)
- Stack depth tracking

#### 2.2 Memory Inspector
- Variable table (name → value)
- Object property inspector
- Closure captured variables
- Prototype chain display

#### 2.3 Step-by-Step Execution
- "Step Into", "Step Over", "Step Out" controls
- Execution speed slider
- Breakpoints (click on bytecode lines)
- Playback history (rewind/fast-forward)

### Phase 3: Advanced Features

#### 3.1 Comparative Analysis
- Side-by-side comparison with V8 bytecode
- Performance metrics (instruction count, memory usage)
- Optimization opportunities visualization

#### 3.2 Interactive Tutorials
- Guided lessons ("How Closures Work")
- Challenges ("Implement a function that uses X")
- Quizzes and assessments

#### 3.3 Community Features
- Share code snippets via URL
- Example gallery (algorithms, patterns)
- Export visualizations as images/GIFs

---

## UI Mockup

```
┌────────────────────────────────────────────────────────────────────────────┐
│  QuickJS-Scala Engine Visualizer                              [Share] [?] │
├──────────────────────┬─────────────────────────────────────────────────────┤
│                      │                                                     │
│  ┌────────────────┐  │  ┌──────────────────────────────────────────────┐  │
│  │   JavaScript   │  │  │              AST Visualization              │  │
│  │     Editor     │  │  │                                              │  │
│  │                │  │  │  📦 Script                                  │  │
│  │  let x = 1 + 2;│  │  │    └─ 📄 ExpressionStatement                │  │
│  │                │  │  │          └─ ➕ BinaryExpression(+)          │  │
│  │  function      │  │  │                ├─ 📘 Literal(1)              │  │
│  │    add(a, b) { │  │  │                └─ 📘 Literal(2)              │  │
│  │      return    │  │  │                                              │  │
│  │        a + b;  │  │  └──────────────────────────────────────────────┘  │
│  │    }           │  │                                                     │
│  │                │  │  ┌──────────────────────────────────────────────┐  │
│  │  add(5, 3);    │  │  │           Bytecode Instructions              │  │
│  └────────────────┘  │  │                                              │  │
│                      │  │  0: PushI32(1)                                │  │
│  ┌────────────────┐  │  │  1: PushI32(2)                                │  │
│  │  [▶ Run]       │  │  │  2: Add ← CURRENT                            │  │
│  │  [⏭ Step Over] │  │  │  3: SetVar("x")                              │  │
│  │  [⏯ Pause]     │  │  │  4: PushI32(0)   // return undefined         │  │
│  │  [⏹ Reset]     │  │  │  5: Return                                    │  │
│  └────────────────┘  │  └──────────────────────────────────────────────┘  │
│                      │                                                     │
│  ┌────────────────┐  │  ┌──────────────────────────────────────────────┐  │
│  │   Console      │  │  │              Stack Visualization             │  │
│  │                │  │  │                                              │  │
│  │ > 8            │  │  │  ┌─────────────┐                             │  │
│  │                │  │  │  │ Int32: 2    │ ← top                       │  │
│  └────────────────┘  │  │  ├─────────────┤                             │  │
│                      │  │  │ Int32: 1    │                             │  │
│                      │  │  └─────────────┘                             │  │
│                      │  └──────────────────────────────────────────────┘  │
│                      │                                                     │
│                      │  ┌──────────────────────────────────────────────┐  │
│                      │  │           Memory Inspector                   │  │
│                      │  │                                              │  │
│                      │  │  Variables:                                  │  │
│                      │  │    x → Int32(3)                              │  │
│                      │  │    add → Closure { captured: [] }            │  │
│                      │  └──────────────────────────────────────────────┘  │
└──────────────────────┴─────────────────────────────────────────────────────┘
```

---

## Implementation Plan

### Step 1: Backend Tracing Infrastructure
- Add execution tracing mode to `Interpreter.scala`
- Capture stack/mem state after each instruction
- Create API data structures for serialization
- Add HTTP/WebSocket server (e.g., http4s, Play Framework)

### Step 2: Basic Frontend Setup
- Set up Scala.js project
- Choose and configure UI framework (Laminar recommended)
- Integrate Monaco Editor
- Create basic layout

### Step 3: AST Visualization
- Serialize AST to JSON
- Render tree view in frontend
- Add node expansion/collapse
- Link source code to AST nodes

### Step 4: Bytecode Display
- Serialize bytecode instructions
- Display instruction list
- Add current position highlighting
- Map bytecode to source lines

### Step 5: Stack & Memory Visualization
- Visual stack component (push/pop animations)
- Memory table for variables
- Object inspector
- Update on each execution step

### Step 6: Step Execution Controls
- WebSocket for real-time execution streaming
- Step Into/Over/Out implementation
- Playback history (store execution states)
- Speed control

### Step 7: Polish & Advanced Features
- Comparative analysis with other engines
- Tutorials and examples
- Sharing and export features
- Performance optimization

---

## File Structure

```
quickjs-scala/
├── engine/               # Existing quickjs-scala modules
│   ├── core/
│   ├── parser/
│   ├── compiler/
│   ├── runtime/
│   └── stdlib/
├── web-frontend/         # New: Scala.js frontend
│   ├── src/main/scala/
│   │   ├── ui/          # UI components
│   │   ├── viz/         # Visualizations
│   │   ├── api/         # Backend communication
│   │   └── shared/      # Shared types
│   ├── src/main/resources/
│   │   └── index.html
│   └── build.sbt
├── web-backend/          # New: HTTP/WebSocket server
│   ├── src/main/scala/
│   │   ├── server/      # HTTP server setup
│   │   ├── tracing/     # Execution tracer
│   │   └── api/         # REST endpoints
│   └── build.sbt
└── docs/
    └── EDUCATIONAL_PLATFORM.md
```

---

## Success Metrics

### Technical
- ✅ Can execute and visualize 100% of existing test cases
- ✅ < 100ms latency for step execution
- ✅ Support for 1000+ instruction executions without lag
- ✅ Mobile-responsive UI

### Educational
- ✅ Users can explain stack-based execution after using platform
- ✅ Users can identify bytecode operations from JS code
- ✅ Platform helps debug closure issues

### Adoption
- ✅ Featured on Scala/JavaScript community sites
- ✅ Used in university courses
- ✅ 1000+ unique users/month

---

## References

- **Python Tutor**: https://pythontutor.com/ (inspiration)
- **QuickJS C Implementation**: /home/hwu/dev/quickjs/quickjs.c
- **Bytecode Explorer**: V8/SpiderMonkey tools
- **Scala.js UI Frameworks**:
  - Laminar: https://laminar.dev/
  - Tyrian: https://tyrian.indigoengine.io/
- **Monaco Editor**: https://microsoft.github.io/monaco-editor/

---

## Next Steps

1. **Prototype**: Build a single-page demo with hardcoded visualization
2. **Validation**: Get feedback from potential users (students, developers)
3. **Architecture Decision**: Choose UI framework and backend stack
4. **MVP Sprint**: Implement Phase 1 features (4-6 weeks)
5. **Launch**: Public beta with example gallery
6. **Iterate**: Add features based on user feedback

---

## Why This Matters

This platform will:
- **Showcase Scala 3**'s capabilities for systems programming
- **Demonstrate functional programming** for compiler/interpreter design
- **Provide unique educational value** - no comparable tool for JavaScript
- **Build community** around the QuickJS-Scala project
- **Create portfolio-worthy work** that stands out

---

*Last Updated: December 2025*
