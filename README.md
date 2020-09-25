# Till

**This project is for learning purposes only.**

## Features

* Implemented from scratch (without the use of compiler frameworks, lexer or parser generators)
* Static type checking
* Proper scoping and variable shadowing
* Python-style blocks denoted by indentation
* Expressions with proper operator precedence
* Function overloading
* Recursive-descent parser
* Intel-syntax x86_64 assembly produced

## Known Issues

* Only indentation with literal `\t` tab characters is allowed - indentation with spaces is not supported.
* All values take up 8 bytes regardless of type (especially wasteful in the case of Boolean values).
* The produced output assembly code is entirely unoptimised.
* Defining a function expected to return a value that has a function body not guaranteed to return does not result in a compiler error or warning provided at least one `return` statement is found in the function body.
* The use of uninitialised variables is not prevented nor acknowledged by the compiler.

## Usage

* `./run.sh fib.til` - Run the Fibonacci sequence example program (must be done from within the `examples/` subdirectory).
* `cargo run` - Build the project and start interactive mode.
* `cargo run /dir/code.til` - Compile a till program and write the output assembly to `out.asm` in the current directory.
* `cargo run /dir/code.til /dir/code.asm` - Compile a till program and write the output assembly to the file at the path specified.
* `cargo test` - Run unit tests.
* `cargo doc --open` - Build and show the documentation (opens in the default browser).

## Language

* The till language is rather primitive and would not be useful in any real-world situations.

### Types

* `Bool` - Boolean type. Can be either `true` or `false`.
* `Num` - Number (64-bit floating-point). Literals are written in base-10 and may or may not include a decimal point.
  * `10`, `0`, `123.528` are all valid number literals.
  * `1A`, `.5`, `12.` are all invalid.
* `Char` - UTF-8 character.
  * `'x'`, `'5'`, `'は'` `'&'`, `' '` are all valid character literals.
  * `''` indicates a null character (equivalent to `'\0'` in C).
  * `'\n', '\t'`, `'\\'`, `'\''` are the only supported escape sequences.

### Expressions

* A function `func` may be called with a first argument `5.5` and a second argument  `'a'` like so: `func(5.5, 'a')`
  * Attempting to call a function with the wrong number of arguments or arguments of the wrong type will result in an error.
  * Functions that do not return a value cannot be called in an expression.
  * A function must be defined and in scope before it may be called.
* Addition (`+`), subtraction (`-`), multiplication (`*`), and division (`/`) are the available arithmetic operations.
  * Multiplication and division have higher precedence than addition and subtraction.
* Any expression or part of an expression enclosed in brackets `()` will be evaluated first.
* Numbers can be compared using the `<` (less than) and `>` (greater than) operators.
* Any two expressions or values of the same type can be compared using the `==` (equals) operator.
  * This operator has priority over the greater than and less than operators (`x > y == y < x` is equivalent to `(x > y) == (y < x)` for example).
* `!` is the Boolean 'not' unary operator.
* `~` is the negation operator (equivalent to unary `-` in other languages).

### Statements

* A variable with the identifier `name` of type `T` would be declared by either `T name` or `T name = <expr>` where `<expr>` is a valid expression of type `T`.
  * Variables can be reassigned by `var = <expr>` where `var` is a previously-declared variable accessible from the current scope, and `<expr>` is an expression matching the type of `var`.
  * A variable should really be given an initial value before use however the compiler does not prevent the use of uninitialised variables.
* Functions can be defined by specifying a function name, parameters and (optionally) a return type, followed by a function body. For example, to define a function `add_numbers` which takes two `Num` parameters and returns a `Num` value:

```
add_numbers(Num x, Num y) -> Num
	return x + y
```

* The `return` keyword is used to end a function and (optionally) return a value to the caller.

* A block of a code can be run based on some condition using an 'if' statement:

```
if x > 5
	x = x / 2
```

* A block of code can be repeated based on some condition using a 'while' statement:

```
while x > 5
	x = x / 2
```

* The value of an expression can be display to standard out using the syntax `display <expr>` where `<expr>` is a valid expression of any type.

### Scoping

* A function or variable declared in a given scope will be accessible from within that scope as well as any scopes nested within.
* Variables can be 'shadowed' (a variable declared in an inner scope with an identifier the same as that of a variable declared in an outer scope will be considered a separate variable until the end of the inner scope). The following code would result in `2` and then `1` being displayed to the console:

```
Num x = 1

if true
	Num x = 2
	display x

display x
```

* Functions on the other hand cannot be shadowed like variables. The following code would result in an error as a new function `func` taking a `Num` argument cannot be defined when such a function is already accessible from the current scope:

```
func(Num x) -> Char
	if x > 12.5
		return 'a'
	return 'b'

if true
	func(Num x) -> Char
		return 'a'
```

