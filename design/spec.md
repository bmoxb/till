# TILL Specification

## Data Types

### Boolean

* `Bool` refers to the Boolean type.
* `true` and `false` are the only valid Boolean literals in TILL.

### Numbers

* `Num` refers to a number type (64-bit floating-point).
* Number literals are base-10 and may or may not have a decimal point.
* `10`, `0`, `123.5` are all valid number literals.
* `1A`, `.5`, `12.` are all invalid.

### Characters

* `Char` refers to a UTF-8 character.
* `'x'`, `'5'`, `'は'` `'&'`, `' '` are all valid character literals.
* `''` indicates a null character.
* `'\n', '\t'`, `'\\'`, `'\''` are the supported escape sequences.

### Arrays

* `[X]` refers to an array contain any number of values of type `X` (where `X` is `Bool`, `Num`, or `Char`).
* `[10, 5.2, 2 + 1]` is an example of a valid array literal of `[Num]` type.
* `[]` is a valid literal for any array type.
* `"abc"` is syntactic sugar for `['a', 'b', 'c']` - the former is referred to as a string literal and allows for the `\"` escape sequence in addition to those supported by individual characters.

## Expressions

### Priority

* A sub-expression can be surrounded by brackets to ensure that it is executed before the outer expression - for example, `(1 + 2) * 3` would result in 9 with those brackets, but 7 were they not present.

### Function Calls

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

* Any two of the same type can be compared using the `==` (equals) operator.
* Attempting to compare different types will result in an error.
* These have priority over comparisons (`x > y == y < x` is equivalent `(x > y) == (y < x)` for example).

### Unary

* `!` is the Boolean 'not' unary operator.
* `~` is the negation operator (equivalent to unary `-` in other languages).

### Array Indexing

* An array value can be accessed using `some_array[index]` syntax.
* Array indices begin from 0.
* Attempting to access an array using an index that is out of bounds will result in a run-time error.

## Statements

### Expression Statement

* An expression statement is simply an expression followed by a newline.
* This may be done for two reasons: 
  * For the side-effects produced when an expression is evaluated (e.g. calling a function that outputs a string but returns nothing).
  * To return a value at the end of a function.

### Variables

* A variable called `name` of type `T` would be declared as so `T name` or like `T name = <expr>` where `<expr>` is a valid expression of type `T`.
* Variables can be reassigned like so `var = <expr>` where `var` is a previously-declared variable within the current scope, and `<expr>` is an expression matching the type of `var`.
* When a variable is declared but not immediately assigned to, it adopts a default value based on its type:
  * For `Char` types: `''` (null character)
  * For `Num` types: `0.0`
  * For `Bool` types: `false`
  * For any array type: `[]` (empty array)

### Function Definitions

* Functions are defined by specifying the function name, parameters and (optionally) its return type, followed by the function body. For example, to define a function `add_numbers` which takes two `Num` parameters and returns a `Num` value:

```
add_numbers(Num x, Num y) -> Num
	x + y
```

* The function body is known as a 'block' - a newline followed by a number of statements indented with a tab character or 4 spaces.
* A function does not have to have a return type. Should such a function end with an expression statement, the result of that expression is discarded.

### While