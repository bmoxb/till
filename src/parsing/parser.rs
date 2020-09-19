//! Handle the parsing of a series of tokens into a collection of `Statement`
//! instances.

use crate::{ stream, lexing::lexer };
use std::iter;

/// Returns an iterator that yields abstract syntax representations for each
/// TILL statement parsed from the given token stream.
pub fn input<T: Iterator<Item=lexer::Token>>(tokens: T) -> StatementStream<T> {
    StatementStream { tokens: tokens.peekable() }
}

pub struct StatementStream<T: Iterator<Item=lexer::Token>> {
    tokens: iter::Peekable<T>
}

impl<T: Iterator<Item=lexer::Token>> Iterator for StatementStream<T> {
    type Item = super::Result<super::Statement>;

    /// Return the next AST statement parsed from the given token stream.
    /// Returns `None` in the case of the token stream having reached its end.
    fn next(&mut self) -> Option<Self::Item> {
        log::info!("Attempting to parse next statement from token stream...");

        if self.more_tokens_in_stream() {
            let stmt = self.statement(0, "top-level statement");
            
            match &stmt {
                Ok(valid_stmt) => log::info!("Parsed next statement from token stream:\n{:#?}", valid_stmt),
                Err(e) => log::info!("Failed to parse next statement from token stream due to error: {}", e)
            }

            let _ = self.consume_token_if_type(&lexer::TokenType::Newline(0), "top-level statement");

            Some(stmt)
        }
        else {
            log::info!("Token stream is already empty so returning None");
            None
        }
    }
}

impl<T: Iterator<Item=lexer::Token>> StatementStream<T> {
    fn more_tokens_in_stream(&mut self) -> bool {
        self.tokens.peek().is_some()
    }

    /// Will see what token is next without advancing the position in the token
    /// stream. Will error if the end of the token stream is reached.
    fn peek_token(&mut self, failure_msg: &'static str) -> super::Result<&lexer::Token> {
        // Could use Result::ok_or but want to log tokens as accessed by the parser.
        match self.tokens.peek() {
            Some(tok) => {
                log::trace!("Peeked token: {:?}", tok);
                Ok(tok)
            }
            None => Err(super::Failure::UnexpectedStreamEnd(failure_msg))
        }
    }

    /// Take the next token and advance the position in the token stream. Will
    /// error if the end of the token stream is reached.
    fn consume_token(&mut self, failure_msg: &'static str) -> super::Result<lexer::Token> {
        match self.tokens.next() {
            Some(tok) => {
                log::trace!("Consumed token: {:?}", tok);
                Ok(tok)
            }
            None => Err(super::Failure::UnexpectedStreamEnd(failure_msg))
        }
    }

    /// Will consume a token, returning said token if it is of the specified
    /// token type. If it is not of that type or if the end of the token stream
    /// is reached, an error will be returned.
    fn consume_token_of_expected_type(&mut self, required_type: &lexer::TokenType, failure_msg: &'static str) -> super::Result<lexer::Token> {
        let tok = self.consume_token(failure_msg)?;

        if tok.tok_type == *required_type {
            log::trace!("Token is of expected type: {:?}", required_type);
            Ok(tok)
        }
        else { Err(super::Failure::UnexpectedToken(tok, failure_msg)) }
    }

    /// Peek the next token and compare its type with that specified. Will error
    /// if the token stream end is reached.
    fn check_type_of_peeked_token(&mut self, required_type: &lexer::TokenType, failure_msg: &'static str) -> super::Result<bool> {
        Ok(self.peek_token(failure_msg)?.tok_type == *required_type)
    }

    /// Will consume the next token in the stream  if it is of the type specified.
    /// Otherwise, the stream position is not advanced and nothing is returned.
    /// Will error if the end of the token stream is reached.
    fn consume_token_if_type(&mut self, required_type: &lexer::TokenType, failure_msg: &'static str) -> super::Result<Option<lexer::Token>> {
        if self.check_type_of_peeked_token(required_type, failure_msg)? {
            log::trace!("Token consumed based on type: {:?}", required_type);
            Ok(Some(self.consume_token(failure_msg)?))
        }
        else { Ok(None) }
    }

    /// Parse a TILL statement.
    ///
    /// `<stmt> ::= <if> | <while> | <function> | <declaration> | <assignment> | <return> | <display>`
    fn statement(&mut self, current_indent: usize, stmt_type_name: &'static str) -> super::Result<super::Statement> {
        log::trace!("Parsing statement...");

        let coming_tok = self.peek_token("statement")?;
        match &coming_tok.tok_type {
            // If statement:
            lexer::TokenType::IfKeyword => self.if_stmt(current_indent),

            // While loop statement:
            lexer::TokenType::WhileKeyword => self.while_stmt(current_indent),

            // Function definition or variable assignment:
            lexer::TokenType::Identifier(x) => {
                let identifier = x.to_string();

                // Consume identifier token and take its stream position:
                let pos = self.consume_token("").unwrap().lexeme.pos;

                if self.check_type_of_peeked_token(&lexer::TokenType::BracketOpen, "statement")? {
                    self.define_function_stmt(current_indent, identifier, pos)
                }
                else if self.check_type_of_peeked_token(&lexer::TokenType::Equals, "statement")? {
                    self.assignment_stmt(identifier)
                }
                else { Err(super::Failure::UnexpectedToken(self.consume_token("statement")?, "statement")) }
            }

            // Variable declaration:
            lexer::TokenType::TypeIdentifier(_) => self.variable_declaration_stmt(),

            // Return:
            lexer::TokenType::ReturnKeyword => self.return_stmt(),

            // Display:
            lexer::TokenType::DisplayKeyword => self.display_stmt(),

            _ => Err(super::Failure::UnexpectedToken(self.consume_token("statement")?, stmt_type_name))
        }
    }

    /// Parse an if statement.
    ///
    /// `<if> ::= "if" <expr> <block>`
    fn if_stmt(&mut self, current_indent: usize) -> super::Result<super::Statement> {
        // Consume the if keyword token:
        self.consume_token_of_expected_type(&lexer::TokenType::IfKeyword, "if keyword")?;

        Ok(super::Statement::If {
            condition: self.expression()?,
            block: self.block(current_indent)?
        })
    }

    /// Parse a while loop statement.
    ///
    /// `<while> ::= "while" <expr> <block>`
    fn while_stmt(&mut self, current_indent: usize) -> super::Result<super::Statement> {
        self.consume_token_of_expected_type(&lexer::TokenType::WhileKeyword, "while keyword")?;

        Ok(super::Statement::While {
            condition: self.expression()?,
            block: self.block(current_indent)?
        })
    }

    /// Parse a function definition statement. The function name identifier is
    /// assumed to have already have been consumed.
    ///
    /// `<function> ::= identifier "(" (<param> ("," <param>)*)? ")" ("->" <type>)? <block>`
    fn define_function_stmt(&mut self, current_indent: usize, identifier: String, pos: stream::Position) -> super::Result<super::Statement> {
        self.consume_token_of_expected_type(&lexer::TokenType::BracketOpen, "open bracket ( token")?;

        let mut parameters = Vec::new();

        // Check firstly if there are any parameters for this function:
        if !self.check_type_of_peeked_token(&lexer::TokenType::BracketClose, "function definition")? {
            loop {
                parameters.push(self.parse_parameter()?);
                
                if self.consume_token_if_type(&lexer::TokenType::Comma,
                    "comma , token to seperate function parameters")?.is_none() { break }
            }
        }

        self.consume_token_of_expected_type(&lexer::TokenType::BracketClose, "close bracket ) token")?;

        let return_type = if self.consume_token_if_type(&lexer::TokenType::Arrow, "function definition")?.is_some() {
            Some(self.consume_type_identifier("function return type")?)
        }
        else { None };

        Ok(super::Statement::FunctionDefinition {
            pos, identifier, parameters, return_type,
            body: self.block(current_indent)?
        })
    }


    /// Parse a variable declaration statement that may optionally include an
    /// initial assignment value for that variable.
    ///
    /// `<declaration> ::= <type> identifier ("=" <expr>)?`
    fn variable_declaration_stmt(&mut self) -> super::Result<super::Statement> {
        let var_type = self.consume_type_identifier("variable type")?;
        let (identifier, _) = self.consume_identifier("variable identifier")?;

        // Variable declaration can optionally include a value for said variable:
        let value = if self.consume_token_if_type(&lexer::TokenType::Equals, "").unwrap_or(None).is_some() {
            Some(self.expression()?)
        }
        else { None };

        Ok(super::Statement::VariableDeclaration { var_type, identifier, value })
    }

    /// Parse a variable assignment statement. The identifier token is already
    /// assumed to have been consumed and the identifier string from said token
    /// passed to this method.
    ///
    /// `<assignment> ::= identifier "=" <expr>`
    fn assignment_stmt(&mut self, identifier: String) -> super::Result<super::Statement> {
        self.consume_token_of_expected_type(&lexer::TokenType::Equals, "equals = after identifier")?;

        Ok(super::Statement::VariableAssignment {
            identifier,
            assign_to: self.expression()?
        })
    }

    /// Parse a function return statement.
    ///
    /// `<return> ::= "return" <expr>?`
    fn return_stmt(&mut self) -> super::Result<super::Statement> {
        self.consume_token_of_expected_type(&lexer::TokenType::ReturnKeyword, "return keyword")?;

        Ok(super::Statement::Return(self.expression().ok()))
    }

    /// Display the resulting value of an expression.
    ///
    /// `<display> ::= "display" <expr>`
    fn display_stmt(&mut self) -> super::Result<super::Statement> {
        self.consume_token_of_expected_type(&lexer::TokenType::DisplayKeyword, "display keyword")?;

        Ok(super::Statement::Display(self.expression()?))
    }

    /// `<param> ::= <type> identifier`
    fn parse_parameter(&mut self) -> super::Result<super::Parameter> {
        let param_type = self.consume_type_identifier("function parameter type")?;
        let (identifier, pos) = self.consume_identifier("function parameter identifier")?;

        Ok(super::Parameter { param_type, pos, identifier  })
    }

    /// Parse a block (a collection of one or more sequential statements that
    /// start at an identation level one higher than the previous indentation
    /// level).
    ///
    /// `<block> ::= newlines indentincr <chunk> indentdecr`
    fn block(&mut self, indent_before_block: usize) -> super::Result<super::Block> {
        let block_indent = indent_before_block + 1;
        
        self.consume_token_of_expected_type(&lexer::TokenType::Newline(block_indent), "increase indent for start of block")?;
        log::trace!("Start of block with indent level: {}", block_indent);

        let stmts = self.block_stmts(block_indent)?;
        Ok(stmts)
    }

    /// Parse a chunk (a collection of one or more sequential statements at a
    /// given indentation level).
    ///
    /// `<chunk> ::= (<stmt> newlines)* <stmt>`
    fn block_stmts(&mut self, block_indent: usize) -> super::Result<Vec<super::Statement>> {
        let mut stmts = Vec::new();

        loop {
            log::trace!("Adding new statment to block");

            let stmt = self.statement(block_indent, "statement contained in block")?;
            stmts.push(stmt);

            match self.peek_token("") {
                Ok(lexer::Token { tok_type: lexer::TokenType::Newline(indent), lexeme }) => {
                    if *indent == block_indent {
                        log::trace!("Next statement in block at same indentation level of {}", indent);
                        let _ = self.consume_token(""); // Only consume token if block continues.
                    }
                    else if *indent < block_indent {
                        log::trace!("Block ending as indent decreased to {}", indent);
                        // Do not consume final newline token after block ends.
                        break;
                    }
                    else {
                        log::info!("Indent has unexpectedly increased to {} so returning Failure", indent);
                        return Err(super::Failure::UnexpectedIndent {
                            expected_indent: block_indent,
                            encountered_indent: *indent,
                            pos: lexeme.pos.clone()
                        });
                    }
                }

                Ok(_) | Err(super::Failure::UnexpectedStreamEnd(_)) => {
                    log::info!("Stream ended during block so assuming this is the end of said block");
                    break;
                }

                Err(x) => return Err(x) // Return any other type of error
            }
        }

        Ok(stmts)
    }

    /// In order to facilitate proper operator prescendence, the grammar for
    /// expressions has a lot of very similar patterns (see 'expr', 'comparison',
    /// 'multiplcation', etc. in grammar file). This method is present to reduce
    /// the amount of repeated code required.
    fn left_right_expr(&mut self, sub_expr_func: fn(&mut Self) -> super::Result<super::Expression>,
    seperators: &[(lexer::TokenType, fn(Box<super::Expression>, Box<super::Expression>) -> super::Expression)])
    -> super::Result<super::Expression> {
        let mut expr = sub_expr_func(self);
        
        for (seperating_tok_type, make_expr_func) in seperators {
            if self.consume_token_if_type(seperating_tok_type, "").unwrap_or(None).is_some() {
                let left = Box::new(expr?);
                let right = Box::new(sub_expr_func(self)?);
                
                expr = Ok(make_expr_func(left, right));
            }
        }

        expr
    }

    /// Parse a TILL expression. Will return Failure should the token stream be
    /// at its end or if an expected token is encountered.
    ///
    /// `<expr> ::= <comparison> ("==" <comparison>)*`
    fn expression(&mut self) -> super::Result<super::Expression> {
        log::trace!("Parsing expression...");

        self.left_right_expr(
            Self::comparison_expr,
            &[(lexer::TokenType::DoubleEquals, |l, r| super::Expression::Equal(l, r))]
        )
    }

    /// `<comparison> ::= <addition> (("<"|">") <addition>)*`
    fn comparison_expr(&mut self) -> super::Result<super::Expression> {
        self.left_right_expr(
            Self::addition_expr,
            &[
                (lexer::TokenType::GreaterThan,
                |l, r| super::Expression::GreaterThan(l, r)),
                (lexer::TokenType::LessThan,
                |l, r| super::Expression::LessThan(l, r))
            ]
        )
    }

    /// `<addition> ::= <multiplication> (("+"|"-") <multiplication>)*`
    fn addition_expr(&mut self) -> super::Result<super::Expression> {
        self.left_right_expr(
            Self::multiplication_expr,
            &[
                (lexer::TokenType::Plus,
                |l, r| super::Expression::Add(l, r)),
                (lexer::TokenType::Minus,
                |l, r| super::Expression::Subtract(l, r))
            ]
        )
    }

    /// `<multiplication> ::= <unary> (("*"|"/") <unary>)*`
    fn multiplication_expr(&mut self) -> super::Result<super::Expression> {
        self.left_right_expr(
            Self::unary_expr,
            &[
                (lexer::TokenType::Star,
                |l, r| super::Expression::Multiply(l, r)),
                (lexer::TokenType::Slash,
                |l, r| super::Expression::Divide(l, r))
            ]
        )
    }

    /// `<unary> ::= ("!"|"~") <unary> | <primary>`
    fn unary_expr(&mut self) -> super::Result<super::Expression> {
        if self.consume_token_if_type(&lexer::TokenType::Tilde, "unary expression")?.is_some() {
            Ok(super::Expression::UnaryMinus(Box::new(self.expression()?)))
        }
        else if self.consume_token_if_type(&lexer::TokenType::ExclaimationMark, "unary expression")?.is_some() {
            Ok(super::Expression::BooleanNot(Box::new(self.expression()?)))
        }
        else { self.primary_expr() }
    }

    /// Parse a primary expression (a literal, expression enclosed in brackets,
    /// or variable identifier).
    ///
    /// ```
    /// <primary> ::= number | string | character | "true" | "false"
    ///             | "[" <exprs>? "]" | "(" <expr> ")"
    ///             | identifier ("(" <exprs>? ")")?
    /// ```
    fn primary_expr(&mut self) -> super::Result<super::Expression> {
        let tok = self.consume_token("primary expression")?;

        log::trace!("Primary expression token: {}", tok);

        match tok.tok_type {
            // Handle expression enclosed in brackets:
            lexer::TokenType::BracketOpen => {
                let expr = self.expression()?;
                self.consume_token_of_expected_type(&lexer::TokenType::BracketClose, "closing bracket ) token")?;
                Ok(expr)
            }

            lexer::TokenType::Identifier(identifier) => {
                // If open bracket follows identifier, then this must be a function
                // call:
                if self.consume_token_if_type(&lexer::TokenType::BracketOpen, "primary expression").unwrap_or(None).is_some() {
                    let args = if self.check_type_of_peeked_token(&lexer::TokenType::BracketClose, "function call")? {
                        vec![] // Closing bracket immediately following an opening
                               // bracket indicates a function taking no arguments.
                    }
                    else { self.expressions()? };

                    self.consume_token_of_expected_type(&lexer::TokenType::BracketClose, "function call closing bracket ) token")?;

                    Ok(super::Expression::FunctionCall {
                        args, identifier,
                        pos: tok.lexeme.pos
                    })
                }
                else {
                    Ok(super::Expression::Variable { identifier, pos: tok.lexeme.pos })
                }
            }

            lexer::TokenType::NumberLiteral(value) => Ok(super::Expression::NumberLiteral { value, pos: tok.lexeme.pos }),
            lexer::TokenType::CharLiteral(value) => Ok(super::Expression::CharLiteral { value, pos: tok.lexeme.pos }),
            lexer::TokenType::TrueKeyword => Ok(super::Expression::BooleanLiteral { value: true, pos: tok.lexeme.pos }),
            lexer::TokenType::FalseKeyword => Ok(super::Expression::BooleanLiteral { value: false, pos: tok.lexeme.pos }),

            _ => Err(super::Failure::UnexpectedToken(tok, "primary expression"))
        }
    }

    /// `<exprs> ::= <expr> ("," <expr>)*`
    fn expressions(&mut self) -> super::Result<Vec<super::Expression>> {
        let mut exprs = vec![self.expression()?];

        // Consume the comma tokens seperating expressions (ignore result as end
        // of stream should not cause error here):
        while self.consume_token_if_type(&lexer::TokenType::Comma, "comma , token seperating expressions").unwrap_or(None).is_some() {
            exprs.push(self.expression()?);
        }

        Ok(exprs)
    }

    fn consume_identifier(&mut self, msg: &'static str) -> super::Result<(String, stream::Position)> {
        let tok = self.consume_token(msg)?;

        match tok.tok_type {
            lexer::TokenType::Identifier(ident) => Ok((ident, tok.lexeme.pos)),
            _ => Err(super::Failure::UnexpectedToken(tok, msg))
        }
    }

    fn consume_type_identifier(&mut self, msg: &'static str) -> super::Result<String> {
        let tok = self.consume_token(msg)?;

        match tok.tok_type {
            lexer::TokenType::TypeIdentifier(ident) => Ok(ident),
            _ => Err(super::Failure::UnexpectedToken(tok, msg))
        }
    }
}



#[cfg(test)]
#[allow(illegal_floating_point_literal_pattern)]
mod tests {
    use crate::{ assert_pattern, parsing, lexing::lexer, stream::Stream };

    fn quick_parse(inp: &str) -> super::StatementStream<impl Iterator<Item=lexer::Token>> {
        let final_inp = inp.trim().replace("    ", "\t");
        let tokens = lexer::input(Stream::from_str(&final_inp)).map(Result::unwrap);
        super::input(tokens)
    }

    #[test]
    fn literal_primary_exprs() {
        let mut prsr = quick_parse("10.5 true false '日'");
        
        assert_pattern!(prsr.primary_expr(), Ok(parsing::Expression::NumberLiteral { pos: _, value: 10.5 }));
        assert_pattern!(prsr.primary_expr(), Ok(parsing::Expression::BooleanLiteral { pos: _, value: true }));
        assert_pattern!(prsr.primary_expr(), Ok(parsing::Expression::BooleanLiteral { pos: _, value: false }));
        match prsr.primary_expr() {
            Ok(parsing::Expression::CharLiteral { pos: _, value: x }) => { assert_eq!(x, '日'); }
            _ => panic!()
        }
    }

    #[test]
    fn identifier_primary_exprs() {
        match quick_parse("my_func(2, true)").primary_expr() {
            Ok(parsing::Expression::FunctionCall {pos: _, identifier, args }) => {
                assert_eq!(identifier, "my_func".to_string());
                assert_eq!(args.len(), 2);

                assert_pattern!(args[0], parsing::Expression::NumberLiteral { pos: _, value: 2.0 });
                assert_pattern!(args[1], parsing::Expression::BooleanLiteral { pos: _, value: true });
            }
            _ => panic!()
        }

        match quick_parse("no_args()").primary_expr() {
            Ok(parsing::Expression::FunctionCall { pos: _, identifier, args }) => {
                assert_eq!(identifier, "no_args".to_string());
                assert!(args.is_empty());
            }
            _ => panic!()
        }

        match quick_parse("my_var").primary_expr() {
            Ok(parsing::Expression::Variable { pos: _, identifier }) => {
                assert_eq!(identifier, "my_var".to_string());
            }
            _ => panic!()
        }
    }

    #[test]
    fn test_unary_exprs() {
        match quick_parse("~10").unary_expr() {
            Ok(parsing::Expression::UnaryMinus(expr)) => {
                assert_pattern!(*expr, parsing::Expression::NumberLiteral { pos: _, value: 10.0 });
            }
            _ => panic!()
        }

        match quick_parse("!some_bool").unary_expr() {
            Ok(parsing::Expression::BooleanNot(expr)) => {
                assert_pattern!(*expr, parsing::Expression::Variable { pos: _, identifier: _ });
            }
            _ => panic!()
        }
    }

    #[test]
    fn expr_operator_precedence() {
        assert_pattern!(quick_parse("3 / 4 + 2").expression(), Ok(parsing::Expression::Add(_, _)));
        assert_pattern!(quick_parse("2 + 3 - 4").expression(), Ok(parsing::Expression::Subtract(_, _)));
        assert_pattern!(quick_parse("1 + 3 > 2").expression(), Ok(parsing::Expression::GreaterThan(_, _)));
        assert_pattern!(quick_parse(" 1 > 2 == 3 < 4").expression(), Ok(parsing::Expression::Equal(_, _)));
        assert_pattern!(quick_parse("3 * (4 + 2)").expression(), Ok(parsing::Expression::Multiply(_, _)));
    }

    #[test]
    fn variable_assignment_stmts() {
        let mut prsr = quick_parse("x = 10\nx =");
        
        match prsr.next().unwrap() {
            Ok(parsing::Statement::VariableAssignment { identifier, assign_to }) => {
                assert_eq!(identifier, "x".to_string());
                assert_pattern!(assign_to, parsing::Expression::NumberLiteral { pos: _, value: 10.0 });
            }
            _ => panic!()
        }

        assert_pattern!(prsr.next().unwrap(), Err(parsing::Failure::UnexpectedStreamEnd(_)));

        assert!(prsr.next().is_none());
    }

    #[test]
    fn variable_declaration_stmts() {
        match quick_parse("Char x").next().unwrap() {
            Ok(parsing::Statement::VariableDeclaration {
                value: None, var_type, identifier
            }) => {
                assert_eq!(identifier, "x".to_string());
                assert_eq!(var_type, "Char".to_string());
            }
            _ => panic!()
        }

        match quick_parse("Num x = 2.5\n\n").next().unwrap() {
            Ok(parsing::Statement::VariableDeclaration {
                value: Some(parsing::Expression::NumberLiteral { pos: _, value: 2.5}),
                var_type, identifier
            }) => {
                assert_eq!(identifier, "x".to_string());
                assert_eq!(var_type, "Num".to_string());
            }
            _ => panic!()
        }
    }

    #[test]
    fn if_stmts() {
        let mut prsr = quick_parse("
if x == 10
    Num y = 2
    y = y + x

    if z == 'a'
        if func(z)
            ty = y + 1

    x = 0");

        assert_pattern!(prsr.next().unwrap(), Ok(parsing::Statement::If {
            condition: parsing::Expression::Equal(_, _), block: _
        }));
    }

    #[test]
    fn while_stmts() {
        let mut prsr = quick_parse("while x < 10\n\tx = x + func(2)\n") ;

        assert_pattern!(prsr.next().unwrap(), Ok(parsing::Statement::While {
            condition: parsing::Expression::LessThan(_, _), block: _
        }))
    }

    #[test]
    fn function_parameters() {
        match quick_parse("Num my_param").parse_parameter() {
            Ok(parsing::Parameter { param_type, identifier, pos: _ }) => {
                assert_eq!(param_type, "Num".to_string());
                assert_eq!(identifier, "my_param".to_string());
            }
            _ => panic!()
        }
    }

    #[test]
    fn function_definition_stmts() {
        let mut prsr = quick_parse("
some_function(Num x, Num y) -> Num
    if x > y
        Num z = (x - y) * (y - x)
    
no_args()
    Char x = '\\n'");

        match prsr.next().unwrap() {
            Ok(parsing::Statement::FunctionDefinition {
                identifier, parameters, body: _, pos: _,
                return_type: Some(_)
            }) => {
                assert_eq!(identifier, "some_function".to_string());
                assert_eq!(parameters.len(), 2);
            }
            _ => panic!()
        }

        match prsr.next().unwrap() {
            Ok(parsing::Statement::FunctionDefinition {
                identifier, parameters, body: _, pos: _,
                return_type: None
            }) => {
                assert_eq!(identifier, "no_args".to_string());
                assert!(parameters.is_empty());
            }
            _ => panic!()
        }
    }

    #[test]
    fn return_stmts() {
        assert_eq!(
            quick_parse("return\n\n").next().unwrap(),
            Ok(parsing::Statement::Return(None))
        );

        match quick_parse("return 2.5").next().unwrap() {
            Ok(parsing::Statement::Return(Some(
                parsing::Expression::NumberLiteral { pos: _, value: 2.5 }
            ))) => {}
            _ => panic!()
        }
    }
}