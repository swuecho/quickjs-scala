# Parser Precedence Chain Fix - Critical Bug Resolution

## Problem

After implementing shift operators (`<<`, `>>`, `>>>`) and exponentiation (`**`), basic arithmetic broke:
- `1 + 2` returned `2` instead of `3`
- Parser created TWO separate statements instead of ONE binary expression
- AST showed: `Script(List(ExpressionStatement(Literal(1)), ExpressionStatement(UnaryExpression(Plus,Literal(2)))))`
- Expected: `Script(List(ExpressionStatement(BinaryExpression(Add,Literal(1),Literal(2)))))`

## Root Cause

**Circular dependency in parser precedence chain**:

When implementing shift operators, I initially made `parseShiftExpression` call `parseMultiplicativeExpression` directly, skipping `parseAdditiveExpression`. This broke the precedence chain.

Then, I tried to fix it by making `parseShiftExpression` call `parseAdditiveExpression`, but `parseAdditiveExpression` was already calling `parseShiftExpression`, creating **infinite recursion**!

## The Fix

**Correct JavaScript operator precedence** (highest to lowest):

1. Multiplicative (`*`, `/`, `%`) - HIGHEST precedence (after unary/exponentiation)
2. Additive (`+`, `-`) - Lower than multiplicative
3. Shift (`<<`, `>>`, `>>>`) - Lower than additive
4. Relational (`<`, `>`, `<=`, `>=`) - Lower than shift
5. Equality (`==`, `===`, `!=`, `!==`) - Lower than relational
6. Logical AND (`&&`) - Lower than equality
7. Logical OR (`||`) - Lower than AND
8. Assignment (`=`, `+=`, etc.) - LOWEST precedence

**Fixed precedence chain in Parser.scala**:

```scala
parseAssignmentExpression()
  → parseLogicalOrExpression()
    → parseLogicalAndExpression()
      → parseEqualityExpression()
        → parseRelationalExpression()         // <, >, <=, >=
          → parseShiftExpression()             // <<, >>, >>>
            → parseAdditiveExpression()        // +, -
              → parseMultiplicativeExpression() // *, /, %
                → parseExponentiationExpression() // ** (right-associative)
                  → parseUnaryExpression()
```

## Code Changes

**File**: `/home/hwu/dev/quickjs-scala/parser/src/main/scala/quickjs/parser/Parser.scala`

### Before (Broken - Circular Dependency)

```scala
private def parseAdditiveExpression(): Expression =
  var left = parseShiftExpression()  // WRONG! Creates circular dependency
  // ...

private def parseShiftExpression(): Expression =
  var left = parseAdditiveExpression()  // WRONG! Creates circular dependency
  // ...
```

### After (Fixed)

```scala
private def parseAdditiveExpression(): Expression =
  var left = parseMultiplicativeExpression()  // CORRECT!
  while isOperator(Operator.Add) || isOperator(Operator.Sub) do
    // ...
    val right = parseMultiplicativeExpression()  // CORRECT!
    left = BinaryExpression(op, left, right, span)
  left

private def parseShiftExpression(): Expression =
  var left = parseAdditiveExpression()  // CORRECT!
  while isOperator(Operator.LeftShift) || isOperator(Operator.RightShift) ||
        isOperator(Operator.UnsignedRightShift) do
    // ...
    val right = parseAdditiveExpression()  // CORRECT!
    left = BinaryExpression(op, left, right, span)
  left
```

## Test Results

### Before Fix
```
Source: 1 + 2
AST: Script(List(
  ExpressionStatement(Literal(1,...)),
  ExpressionStatement(UnaryExpression(Plus,Literal(2),...))
))
Result: 2
Status: FAILED
```

### After Fix
```
Source: 1 + 2
AST: Script(List(
  ExpressionStatement(BinaryExpression(Add,Literal(1),Literal(2),...))
))
Result: 3
Status: PASSED
```

## Impact

### QuickJS Language Test Suite

**Before**: 11/29 tests passing (38%)
**After**: 12/29 tests passing (41%)

**Newly Passing Tests**:
1. ✅ test_op1: addition and subtraction
2. ✅ test_op1: unary plus and minus
3. ✅ test_op1: multiplication and division
4. ✅ test_op1: left shift (1 << 0)
5. ✅ test_op1: signed right shift (-4 >> 1)
6. ✅ test_op1: unsigned right shift (-4 >>> 1)

### Verification

All arithmetic operations now work correctly:
- `1 + 2 = 3` ✅
- `1 - 2 = -1` ✅
- `2 * 3 = 6` ✅
- `4 / 2 = 2` ✅
- `1 << 2 = 4` ✅
- `-4 >> 1 = -2` ✅
- `-4 >>> 1 = 2147483646` ✅

## Lessons Learned

1. **Precedence chains must be acyclic**: Each level must call the NEXT higher precedence level, never the previous level
2. **JavaScript operator precedence is counter-intuitive**: Shift has LOWER precedence than additive, not higher!
3. **Test incrementally**: After adding new operators, test basic arithmetic first before running full test suite
4. **Debug output is essential**: Printing the AST helped identify the issue immediately

## References

- **MDN: Operator Precedence**: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
- **QuickJS C Source**: `/home/hwu/dev/quickjs/quickjs.c`
- **Test Suite**: `/home/hwu/dev/quickjs/tests/test_language.js`

## Next Steps

1. Fix hex string literal parsing (`"0x12345" | 0`)
2. Fix typeof operator (returns index instead of type string)
3. Fix instanceof operator (not implemented)
4. Fix in operator (not implemented)
5. Fix loose equality coercion edge cases

## Files Modified

1. `/home/hwu/dev/quickjs-scala/parser/src/main/scala/quickjs/parser/Parser.scala` (lines 428-460)
2. `/home/hwu/dev/quickjs-scala/runtime/src/test/scala/quickjs/debug/AdditionDebugTest.scala` (debug test)

## Summary

Fixed a critical parser bug that broke all arithmetic operations. The issue was a circular dependency in the precedence chain caused by incorrect understanding of JavaScript operator precedence. The fix involved correcting the call sequence to match JavaScript's actual precedence rules.

**Status**: ✅ RESOLVED - Basic arithmetic now works correctly
