# Till

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
  * [x] Produce a final immediate representation of the input program
* [ ] Code generator
  * [ ] Produce machine code using the final IR
  * [ ] Write the machine code to an output file
* [ ] Provide appropriate debug logging at all stages
* [ ] Provide clear error messages to the user should their program be invalid
* [ ] Test extensively