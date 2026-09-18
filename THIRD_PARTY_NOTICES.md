# Third-Party Notices

QuickJS-Scala is an independent project. It is not affiliated with or endorsed
by the QuickJS project or its authors.

## QuickJS (https://bellard.org/quickjs/)

The engine's architecture and bytecode design are inspired by QuickJS, and the
following test files are copied verbatim from the QuickJS repository:

- `stdlib/src/test/resources/quickjs-tests/test_bigint.js`
- `stdlib/src/test/resources/quickjs-tests/test_builtin.js`
- `stdlib/src/test/resources/quickjs-tests/test_closure.js`
- `stdlib/src/test/resources/quickjs-tests/test_language.js`
- `stdlib/src/test/resources/quickjs-tests/test_loop.js`

QuickJS is distributed under the MIT License:

```
QuickJS Javascript Engine

Copyright (c) 2017-2021 Fabrice Bellard
Copyright (c) 2017-2021 Charlie Gordon

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
```

## test262 (https://github.com/tc39/test262)

The conformance suite is not vendored. It is cloned separately by developers
and by CI, and is distributed under its own (BSD-style) license.

## Runtime dependencies

The build and runtime dependencies (Scala 3, sbt, munit, JLine, ICU4J, Laminar,
scala-js-dom, Vite) are used under their respective licenses. Refer to their
repositories for details.
