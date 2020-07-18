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
* [ ] Executor
  * [ ] Perform type checking
  * [ ] Ensure scoping rules are met
  * [ ] Construct a final immediate representation of the parsed program
  * [ ] Execute said representation
* [ ] Provide appropriate debug logging at all stages
* [ ] Test extensively