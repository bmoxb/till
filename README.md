# TILL (Tiny Interpreted Lightweight Language)

## Checklist

* [x] Lexer
  * [x] Read character-by-character input from a stream
  * [x] Allow for the specification of states and transitions between them
  * [x] Define all states and transitions required for interpreting TILL
  * [x] Return line and character position alongside token and lexeme
* [ ] Parser
  * [ ] Create an unambigious grammar
  * [ ] Implement the recursive-descent parser
* [ ] Checker
  * [ ] Ensure functions and variables referenced are defined and within scope
  * [ ] Perform type checking
* [ ] Executor
