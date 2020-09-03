# TILL (Tiny Interpreted Lightweight Language)

## Checklist

* [x] Lexer
  * [x] Read character-by-character input from a stream
  * [x] Allow for the specification of states and transitions between them
  * [x] Define all states and transitions required for interpreting TILL
  * [x] Return line and character position alongside token and lexeme
* [x] Parser
  * [x] Create an unambiguous grammar
  * [x] Implement the recursive-descent parser
    * [x] Parse all expression types
    * [x] Parse all statement types
* [x] Checker
  * [x] Perform type checking
    * [x] For expressions
    * [x] For statements
  * [x] Ensure scoping rules are met
* [ ] Once it has been determined valid by the checker, execute the program
* [ ] Provide appropriate debug logging at all stages
* [ ] Provide clear error messages to the user should their program be invalid
* [ ] Have both a REPL and the support for pre-written scripts
* [ ] Test extensively