# QuickJS-Scala Web Frontend

Web-based Execution Trace Studio for QuickJS-Scala JavaScript engine. Built with Scala.js, Laminar, and Vite.

## Overview

This is a browser-based development tool that allows you to:
- Write JavaScript code in an interactive editor
- Execute it against the QuickJS-Scala engine
- Visualize bytecode instructions, stack states, and execution flow
- Debug step-by-step through the interpreter

**Tech Stack:**
- **Scala.js** - Scala compiled to JavaScript
- **Laminar** - Reactive UI library
- **Vite** - Fast dev server with auto-refresh
- **Scala.js DOM** - Browser API bindings

## Quick Start

### Prerequisites

```bash
# Install npm dependencies (first time only)
cd web-frontend
npm install
```

### Development Mode

**Terminal 1 - Start backend trace server:**
```bash
cd ..
./scripts/dev-server.sh
```
Starts trace server on `http://localhost:8125`

**Terminal 2 - Start frontend:**
```bash
./scripts/dev-web.sh
```
Starts Vite dev server on `http://localhost:5173`

**Browser:**
Open `http://localhost:5173/`

### Standalone Build

```bash
# Build Scala.js (optimized)
sbt "webFrontend/fullLinkJS"

# Build Vite bundle
cd web-frontend
npm run build

# Output: web-frontend/vite-dist/
```

## Architecture

### How Vite Connects to Scala.js

This project uses a **file-watching rebuild** pattern (not true HMR):

```
┌─────────────────────────────┐      ┌──────────────────────────────┐
│  sbt ~webFrontend/fastLinkJS│      │  Vite Dev Server            │
│  (Scala.js file watcher)     │      │  (vite --host)              │
├─────────────────────────────┤      ├──────────────────────────────┤
│  Watches Scala files         │      │  Serves static files        │
│  Recompiles on change        │ ───► │  Proxies /trace → backend   │
│  Outputs to dist/main.js     │      │  Auto-reloads browser       │
└─────────────────────────────┘      └──────────────────────────────┘
         │                                          │
         └────────────writes to─────────────────────┘
                    dist/main.js
```

### Development Workflow

1. **Edit Scala code** (`src/main/scala/quickjs/web/TraceApp.scala`)
2. **sbt watcher detects change** → recompiles Scala.js → overwrites `dist/main.js`
3. **Vite detects file change** → triggers browser refresh
4. **Browser reloads** → fetches new `main.js` → re-initializes app

**Note:** Scala.js doesn't support true Hot Module Replacement. The "HMR" here is fast rebuild (~1-2s) + browser auto-refresh.

## Project Structure

```
web-frontend/
├── src/main/scala/quickjs/web/
│   └── TraceApp.scala           # Main Laminar application
├── dist/
│   └── main.js                  # Scala.js compiler output (dev)
├── vite-dist/                   # Production build output
├── index.html                   # Entry point (loads dist/main.js)
├── styles.css                   # Application styles
├── vite.config.js               # Vite configuration
├── package.json                 # npm dependencies
└── README.md                    # This file
```

## Configuration

### build.sbt - Scala.js Output

```scala
lazy val webFrontend = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "dist"
  )
```

- `fastLinkJS` - Fast builds for development (outputs to `dist/`)
- `fullLinkJS` - Optimized builds for production

### vite.config.js - Dev Server

```javascript
export default defineConfig({
  server: {
    port: 5173,
    proxy: {
      "/trace": "http://localhost:8125",  // Backend trace server
    },
  },
  build: {
    outDir: "vite-dist",
  },
});
```

### index.html - Entry Point

```html
<script src="dist/main.js"></script>
```

## Application Features

### Source Editor
- Multi-line JavaScript editor
- REPL mode toggle (evaluate each line or full script)
- Syntax error display with stack traces

### Bytecode Panel
- Shows compiled bytecode instructions
- Displays instruction opcodes and operands
- Highlights current instruction during trace
- Shows raw bytecode bytes with address offsets

### Trace Events Panel
- Lists all execution events (instructions, calls, returns)
- Step-by-step navigation (Previous/Next buttons)
- Shows program counter and source location
- Color-coded by event type

### Runtime Stack Panel
- Visualizes JavaScript stack state
- Shows value types and representations
- Highlights top-of-stack
- Animated stack push/pop indicators

### Event Details Panel
- Raw JSON view of selected trace event
- Useful for debugging the tracer itself

## Development Scripts

See `../scripts/` for helper scripts:

- **dev-web.sh** - Starts Scala.js watcher + Vite dev server
- **dev-server.sh** - Starts backend trace server with file watching

## Troubleshooting

### Port Already in Use

```bash
# Kill existing process
lsof -ti:5173 | xargs kill -9
```

### npm Dependencies Missing

```bash
cd web-frontend
npm install
```

### Browser Shows Old Code

1. Hard refresh: `Ctrl+Shift+R` (Windows/Linux) or `Cmd+Shift+R` (Mac)
2. Check `dist/main.js` modification time
3. Verify sbt watcher output shows "FastLinkJS" compilation

### Can't Connect to Backend

1. Check backend is running: `curl http://localhost:8125`
2. Verify Vite proxy configuration in `vite.config.js`
3. Check browser console for CORS errors

### Scala.js Compilation Errors

1. Check Terminal 2 (sb watcher) for error details
2. Fix syntax/type errors in Scala source
3. Watcher will auto-retry on next save

## Key Technologies

- **Scala.js 1.x** - Scala to JavaScript compiler
- **Laminar 16.0** - Reactive UI library built on Scala.js
- **Vite 5.x** - Fast dev server with ES module support
- **scala-js-dom 2.8** - TypeScript-style DOM bindings

## Browser Compatibility

- Chrome/Edge 90+
- Firefox 88+
- Safari 14+

## See Also

- **Main Project**: `../README.md` - Overall QuickJS-Scala documentation
- **Dev Scripts**: `../scripts/README.md` - Development workflow details
- **Developer Guide**: `../CLAUDE.md` - Architecture reference
