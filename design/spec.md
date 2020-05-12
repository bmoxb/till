# TILL Specification

## Data Types

### Boolean

* `Bool` refers to the Boolean type.
* `true` and `false` are the only valid Boolean literals in TILL.

### Numbers

* `Num` refers to a number type (64-bit floating-point).
* Number literals are base-10 and may or may not have a decimal point.
* `10`, `0`, `123.5` are all valid number literals.
* `1A`, `12.`, `x` are not valid number literals.

### Characters

* `Char` refers to a single Extended ASCII character.
* `'x'`, `'5'`, `'&'`, `' '` are all valid character literals.
* `'は'` is an example of an invalid character literal (Unicode characters are not allowed).
* `''` indicates the null ASCII character.
* `'\n', '\t'`, `'\\'`, `'\''` are the supported escape sequences.

### Arrays

* `[X]` refers to an array contain any number of values of type `X` (where `X` is `Bool`, `Num`, or `Char`).
* `[10, 5.2, 2 + 1]` is an example of a valid number array literal.
* `[]` is a valid literal for any array type.
* `"abc"` is syntactic sugar for `['a', 'b', 'c']` - the former is referred to as a string literal and allows for the `\"` escape sequence in addition to those supported by individual characters.

## Expressions

### Arithmetic

* Basic addition, subtraction, multiplication, and division operations are supported on number types.
* Multiplication and division have higher precedence than addition and subtraction.
* Prefix negation is done using the `~` operator.

```
5 + 1.2
1 - ~2
1.2 * 2.5
6 / 2
```

### Comparisons

* Numbers and characters can be compared using the `<` (less-than) and `>` (greater-than) operators.
* When comparing characters, the ASCII value of the left and right operand is what is actually being compared (i.e. `'\n'` is less than `'A'` but greater than `''`).

### Equality

* Any two of the same type can be compared using the `==` (equals) and `!=` (not-equals) operators.
* Attempting to compare different types will result in an error.
* These have priority over comparisons (`x > y == y < x` is equivalent `(x > y) == (y < x)` for example).

### Unary

* `!` is the Boolean 'not' unary operator.
* `~` is the negation operator (equivalent to unary `-` in other languages).