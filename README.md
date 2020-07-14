# TILL (Tiny Interpreted Lightweight Language)

## Checklist

* [x] Lexer
  * [x] Read character-by-character input from a stream
  * [x] Allow for the specification of states and transitions between them
  * [x] Define all states and transitions required for interpreting TILL
  * [x] Return line and character position alongside token and lexeme
* [ ] Parser
  * [x] Create an unambigious grammar
  * [x] Implement the recursive-descent parser
    * [x] Parse all expression types
    * [x] Parse all statement types
  * [ ] Test extensively
* [ ] Checker
  * [ ] Ensure functions and variables referenced are defined and within scope
  * [ ] Perform type checking
* [ ] Executor
