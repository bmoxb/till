use crate::lexing::lexer;
use std::{ iter, fmt };

/// Returns an iterator that yields abstract syntax representations for each
/// TILL statement parsed from the given token stream.
pub fn input<T: Iterator<Item=lexer::Token>>(tokens: T) -> StatementStream<T> {
    StatementStream { tokens: tokens.peekable() }
}

/// Represents the two types of syntax errors: the encountering of an unexpected
/// token, and the encountering of the end of the token stream when it is not
/// expected.
#[derive(Debug)]
pub enum Failure {
    UnexpectedToken(lexer::Token, &'static str),
    UnexpectedStreamEnd(&'static str)
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Failure::UnexpectedToken(tok, expected) => write!(f, "Expected {} yet encountered unexpected {}", expected, tok),
            Failure::UnexpectedStreamEnd(expected) => write!(f, "Encountered the end of the token stream yet expected {}", expected)
        }
    }
}

pub struct StatementStream<T: Iterator<Item=lexer::Token>> {
    tokens: iter::Peekable<T>
}

impl<T: Iterator<Item=lexer::Token>> Iterator for StatementStream<T> {
    type Item = Result<super::Statement, Failure>;

    /// Return the next AST statement parsed from the given token stream.
    /// Returns None in the case of the token stream having reached its end.
    fn next(&mut self) -> Option<Self::Item> {
        log::info!("-- Next Parsing --");

        if self.more_tokens_in_stream() {
            let stmt = self.statement(0, "top-level statement");

            let _ = self.consume_token_if_type(&lexer::TokenType::Newline(0), "top-level statement");

            Some(stmt)
        }
        else { None }
    }
}

impl<T: Iterator<Item=lexer::Token>> StatementStream<T> {
    fn more_tokens_in_stream(&mut self) -> bool {
        self.tokens.peek().is_some()
    }

    /// Will see what token is next without advancing the position in the token
    /// stream. Will error if the end of the token stream is reached.
    fn peek_token(&mut self, failure_msg: &'static str) -> Result<&lexer::Token, Failure> {
        // Could use Result::ok_or but want to log tokens as accessed by the parser.
        match self.tokens.peek() {
            Some(tok) => {
                log::trace!("Peeked token: {:?}", tok);
                Ok(tok)
            }
            None => Err(Failure::UnexpectedStreamEnd(failure_msg))
        }
    }

    /// Take the next token and advance the position in the token stream. Will
    /// error if the end of the token stream is reached.
    fn consume_token(&mut self, failure_msg: &'static str) -> Result<lexer::Token, Failure> {
        match self.tokens.next() {
            Some(tok) => {
                log::trace!("Consumed token: {:?}", tok);
                Ok(tok)
            }
            None => Err(Failure::UnexpectedStreamEnd(failure_msg))
        }
    }

    /// Will consume a token, returning said token if it is of the specified
    /// token type. If it is not of that type or if the end of the token stream
    /// is reached, an error will be returned.
    fn consume_token_of_expected_type(&mut self, required_type: &lexer::TokenType, failure_msg: &'static str) -> Result<lexer::Token, Failure> {
        let tok = self.consume_token(failure_msg)?;

        if tok.tok_type == *required_type {
            log::trace!("Token is of expected type: {:?}", required_type);
            Ok(tok)
        }
        else { Err(Failure::UnexpectedToken(tok, failure_msg)) }
    }

    /// Peek the next token and compare its type with that specified. Will error
    /// if the token stream end is reached.
    fn check_type_of_peeked_token(&mut self, required_type: &lexer::TokenType, failure_msg: &'static str) -> Result<bool, Failure> {
        Ok(self.peek_token(failure_msg)?.tok_type == *required_type)
    }

    /// Will consume the next token in the stream  if it is of the type specified.
    /// Otherwise, the stream position is not advanced and nothing is returned.
    /// Will error if the end of the token stream is reached.
    fn consume_token_if_type(&mut self, required_type: &lexer::TokenType, failure_msg: &'static str) -> Result<Option<lexer::Token>, Failure> {
        if self.check_type_of_peeked_token(required_type, failure_msg)? {
            log::trace!("Token consumed based on type: {:?}", required_type);
            Ok(Some(self.consume_token(failure_msg)?))
        }
        else { Ok(None) }
    }

    /// Parse a TILL statement.
    ///
    /// `<stmt> ::= <if> | <function> | <declaration> | <assignment>`
    fn statement(&mut self, current_indent: usize, stmt_type_name: &'static str) -> Result<super::Statement, Failure> {
        log::debug!("Parsing statement...");

        let coming_tok = self.peek_token("statement")?;
        match &coming_tok.tok_type {
            // If statement:
            lexer::TokenType::IfKeyword => self.if_stmt(current_indent),

            // Function definition or variable assignment:
            lexer::TokenType::Identifier(x) => {
                let identifier = x.to_string();
                self.consume_token("statement")?;

                if self.check_type_of_peeked_token(&lexer::TokenType::BracketOpen, "statement")? {
                    self.define_function_stmt(identifier)
                }
                else if self.check_type_of_peeked_token(&lexer::TokenType::Equals, "statement")? {
                    self.assignment_stmt(identifier)
                }
                else { Err(Failure::UnexpectedToken(self.consume_token("statement")?, "statement")) }
            }

            // Variable declaration:
            /*lexer::TokenType::BracketSquareClose |
            lexer::TokenType::TypeIdentifier(_) => self.variable_declaration_stmt(),*/

            _ => Err(Failure::UnexpectedToken(self.consume_token("statement")?, stmt_type_name))
        }
    }

    /// Parse an if statement that may optionally include an else clause.
    ///
    /// `<if> ::= "if" <expr> <block> ("else" <block>)?`
    fn if_stmt(&mut self, current_indent: usize) -> Result<super::Statement, Failure> {
        // Consume the if keyword token:
        self.consume_token_of_expected_type(&lexer::TokenType::IfKeyword, "if keyword")?;

        Ok(super::Statement::If {
            condition: self.expression()?,
            if_block: self.block(current_indent)?,
            else_block: {
                // Note that the `Result` of the following method call is not
                // handled as else blocks are optional and therefore checking
                // for one shouldn't cause error when at token stream end.
                if self.consume_token_if_type(&lexer::TokenType::ElseKeyword, "if statement").unwrap_or(None).is_some() {
                    Some(self.block(current_indent)?)
                }
                else { None }
            }
        })
    }

    /// Parse a function definition statement. The function name identifier is
    /// assumed to have already have been consumed.
    ///
    /// `<function> ::= identifier "(" (<param> ("," <param>)*)? ")" ("->" <type>)? <block>`
    fn define_function_stmt(&mut self, _identifier: String) -> Result<super::Statement, Failure> {
        unimplemented!() // TODO
    }


    /// Parse a variable declaration statement that may optionally include an
    /// initial assignment value for that variable.
    ///
    /// `<declaration> ::= <type> identifier ("=" <expr>)?`
    /* // TODO! fn variable_declaration_stmt(&mut self) -> Result<super::Statement, Failure> {
        let var_type = self.parse_type()?;
        
        let identifier = match self.consume_token_of_expected_type(&lexer::TokenType::Identifier, "variable identifier")?.tok_type {
            lexer::TokenType::Identifier(x) => x
        };

        // variable declaration can optionally include a value for said variable:
        let value = if self.consume_token_if_type(&lexer::TokenType::Equals, "varriable declaration").unwrap_or(None).is_some() {
            Some(self.expression()?)
        }
        else { None };

        Ok(super::Statement::VariableDeclaration { var_type, identifier, value })
    }*/

    /// Parse a variable assignment statement. The identifier token is already
    /// assumed to have been consumed and the identifier string from said token
    /// passed to this method.
    ///
    /// `<assignment> ::= identifier "=" <expr>`
    fn assignment_stmt(&mut self, identifier: String) -> Result<super::Statement, Failure> {
        self.consume_token_of_expected_type(&lexer::TokenType::Equals, "equals = after identifier")?;

        Ok(super::Statement::VariableAssignment {
            identifier,
            assign_to: self.expression()?
        })
    }

    fn parse_type(&mut self) -> Result<super::Type, Failure> {
        let tok = self.consume_token("type")?;
        
        match tok.tok_type {
            // Array type:
            lexer::TokenType::BracketSquareOpen => {
                let contained_type = Box::new(self.parse_type()?);
                self.consume_token_of_expected_type(&lexer::TokenType::BracketSquareClose, "closing square bracket ] for array type")?;
                Ok(super::Type::Array(contained_type))
            }

            // Type identifier:
            lexer::TokenType::TypeIdentifier(identifier) => Ok(super::Type::Identifier {
                pos: tok.lexeme.pos,
                identifier
            }),

            _ => Err(Failure::UnexpectedToken(tok, "type"))
        }
    }

    /// Parse a block (a collection of one or more sequential statements that
    /// start at an identation level one higher than the previous indentation
    /// level).
    ///
    /// `<block> ::= newlines indentincr <chunk> indentdecr`
    fn block(&mut self, indent_before_block: usize) -> Result<super::Block, Failure> {
        let block_indent = indent_before_block + 1;
        
        self.consume_token_of_expected_type(&lexer::TokenType::Newline(block_indent), "increase indent for start of block")?; // TODO: unexpected indent error?
        log::debug!("Start of block with indent level: {}", block_indent);

        let stmts = self.block_stmts(block_indent)?;
        Ok(super::Block(stmts))
    }

    /// Parse a chunk (a collection of one or more sequential statements at a
    /// given indentation level).
    ///
    /// `<chunk> ::= (<stmt> newlines)*`
    fn block_stmts(&mut self, block_indent: usize) -> Result<Vec<super::Statement>, Failure> {
        let mut stmts = Vec::new();

        loop {
            log::trace!("Adding new statment to block");

            let stmt = self.statement(block_indent, "statement contained in block")?;
            stmts.push(stmt);

            if self.consume_token_if_type(&lexer::TokenType::Newline(block_indent), "block indent")?.is_some() {
                log::trace!("Next statement in block at same indentation level: {}", block_indent);
            }
            else {
                self.consume_token_of_expected_type(&lexer::TokenType::Newline(block_indent - 1), "block indent")?;
                log::debug!("Indent decrease so ending block");
                break;
            }
        }

        Ok(stmts)
    }

    /// In order to facilitate proper operator prescendence, the grammar for
    /// expressions has a lot of very similar patterns (see 'expr', 'comparison',
    /// 'multiplcation', etc. in grammar file). This method is present to reduce
    /// the amount of repeated code required.
    fn left_right_expr(&mut self, sub_expr_func: fn(&mut Self) -> Result<super::Expression, Failure>,
    seperators: &[(lexer::TokenType, fn(Box<super::Expression>, Box<super::Expression>) -> super::Expression)])
    -> Result<super::Expression, Failure> {
        let mut expr = sub_expr_func(self);
        
        for (seperating_tok_type, make_expr_func) in seperators {
            if self.consume_token_if_type(seperating_tok_type, "expression").unwrap_or(None).is_some() {
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
    /// `<expr> ::= <comparison> (("!="|"==") <comparison>)*`
    fn expression(&mut self) -> Result<super::Expression, Failure> {
        log::trace!("Parsing expression...");

        self.left_right_expr(
            Self::comparison_expr,
            &[(lexer::TokenType::DoubleEquals, |l, r| super::Expression::Equal(l, r))]
        )
    }

    /// `<comparison> ::= <addition> (("<"|">") <addition>)*`
    fn comparison_expr(&mut self) -> Result<super::Expression, Failure> {
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
    fn addition_expr(&mut self) -> Result<super::Expression, Failure> {
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
    fn multiplication_expr(&mut self) -> Result<super::Expression, Failure> {
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
    fn unary_expr(&mut self) -> Result<super::Expression, Failure> {
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
    fn primary_expr(&mut self) -> Result<super::Expression, Failure> {
        let tok = self.consume_token("primary expression")?;

        log::trace!("Primary expression token: {}", tok);

        match tok.tok_type {
            // Handle expression enclosed in brackets:
            lexer::TokenType::BracketOpen => {
                let expr = self.expression()?;
                self.consume_token_of_expected_type(&lexer::TokenType::BracketClose, "closing bracket ) token")?;
                Ok(expr)
            }

            // Array literals:
            lexer::TokenType::BracketSquareOpen => {
                let exprs = if self.check_type_of_peeked_token(&lexer::TokenType::BracketSquareClose, "array literal")? {
                    vec![] // Obviously an empty array literal `[]` should a closing
                           // bracket token immediately follow the opening one.
                }
                else { self.expressions()? };
                
                self.consume_token_of_expected_type(&lexer::TokenType::BracketSquareClose, "array literal closing square bracket ] token")?;
                
                Ok(super::Expression::Array(exprs))
            }

            lexer::TokenType::Identifier(identifier) => {
                // If open bracket follows identifier, then this must be a function
                // call:
                if self.consume_token_if_type(&lexer::TokenType::BracketOpen, "primary expression").unwrap_or(None).is_some() {
                    let args = if self.check_type_of_peeked_token(&lexer::TokenType::BracketClose, "function call")? {
                        vec![] // As with array literal above, an immediately
                               // following closing brackets means a function
                               // without any arguments.
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
            lexer::TokenType::StringLiteral(value) => Ok(super::Expression::StringLiteral { value, pos: tok.lexeme.pos }),
            lexer::TokenType::TrueKeyword => Ok(super::Expression::BooleanLiteral { value: true, pos: tok.lexeme.pos }),
            lexer::TokenType::FalseKeyword => Ok(super::Expression::BooleanLiteral { value: false, pos: tok.lexeme.pos }),

            _ => Err(Failure::UnexpectedToken(tok, "primary expression"))
        }
    }

    /// `<exprs> ::= <expr> ("," <expr>)*`
    fn expressions(&mut self) -> Result<Vec<super::Expression>, Failure> {
        let mut exprs = vec![self.expression()?];

        // Ignore result as end of stream should not cause error here:
        while self.consume_token_if_type(&lexer::TokenType::Comma, "comma , token seperating expressions").unwrap_or(None).is_some() {
            log::trace!("Comma seperating expressions consumed");
            exprs.push(self.expression()?);
        }

        Ok(exprs)
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing;
    use crate::lexing::lexer;
    use crate::stream::Stream;

    macro_rules! assert_pattern {
        ($x:expr, $y:pat) => {
            match $x {
                $y => {}
                _ => panic!("Expression is not of correct type: {:?}", $x)
            }
        };
    }

    fn quick_parse(inp: &str) -> super::StatementStream<impl Iterator<Item=lexer::Token>> {
        let tokens = lexer::input(Stream::from_str(inp)).map(Result::unwrap);
        super::input(tokens)
    }

    #[test]
    fn test_assignment_statements() {
        assert_pattern!(
            quick_parse("a = 10 + 5").next().unwrap(),
            Ok(parsing::Statement::VariableAssignment {
                identifier: _,
                assign_to: parsing::Expression::Add(_, _)
            })
        );
    }

    #[test]
    fn test_if_statements() {
        let mut prsr = quick_parse("\
            if true\n\t\
                a = 5\n\
            if 10 == 2 + 5\n\t\
                x = \"this\"\n\
            else\n\t\
                x = \"that\"\n\
        ");

        assert_pattern!(
            prsr.if_stmt(0),
            Ok(parsing::Statement::If {
                condition: parsing::Expression::BooleanLiteral { pos: _, value: true },
                if_block: parsing::Block(_),
                else_block: None
            })
        );

        assert_pattern!(
            prsr.next().unwrap(),
            Ok(parsing::Statement::If {
                condition: parsing::Expression::Equal(_, _),
                if_block: parsing::Block(_),
                else_block: Some(parsing::Block(_))
            })
        );
    }

    #[test]
    fn test_parsing_types() {
        assert_pattern!(quick_parse("Int").parse_type(),
            Ok(parsing::Type::Identifier { pos: _, identifier: _ }));
        assert_pattern!(quick_parse("[Char]").parse_type(), Ok(parsing::Type::Array(_)));
        assert_pattern!(quick_parse("[Int").parse_type(), Err(super::Failure::UnexpectedStreamEnd(_)));
    }

    #[test]
    #[allow(illegal_floating_point_literal_pattern)]
    fn test_simple_primary_expressions() {
        let mut prsr = quick_parse("10.5 \"string\" my_identifier true =");

        assert_pattern!(prsr.primary_expr(),
            Ok(parsing::Expression::NumberLiteral { pos: _, value: 10.5 }));
        assert_pattern!(prsr.expression(),
            Ok(parsing::Expression::StringLiteral { pos: _, value: _ }));
        assert_pattern!(prsr.primary_expr(),
            Ok(parsing::Expression::Variable { pos: _, identifier: _ }));
        assert_pattern!(prsr.expression(),
            Ok(parsing::Expression::BooleanLiteral { pos: _, value: true }));
        assert_pattern!(prsr.primary_expr(), Err(super::Failure::UnexpectedToken(_, _)));
        assert_pattern!(prsr.expression(), Err(super::Failure::UnexpectedStreamEnd(_)));
    }

    #[test]
    fn test_array_literal_expressions() {
        assert_pattern!(quick_parse("[10 - 2, 2.5 * 6]").primary_expr(), Ok(parsing::Expression::Array(_)));
        assert_pattern!(quick_parse("[]").expression(), Ok(parsing::Expression::Array(_)));
        assert_pattern!(quick_parse("[1, 2, 3").primary_expr(), Err(super::Failure::UnexpectedStreamEnd(_)));
    }

    #[test]
    fn test_function_call_expressions() {
        assert_pattern!(quick_parse("func(10 + 2)").expression(),
            Ok(parsing::Expression::FunctionCall { pos: _, identifier: _, args: _ }));
        assert_pattern!(quick_parse("no_args_function()").primary_expr(),
            Ok(parsing::Expression::FunctionCall { pos: _, identifier: _, args: _ }));
        assert_pattern!(quick_parse("func(1, 5").expression(), Err(super::Failure::UnexpectedStreamEnd(_)));
    }

    #[test]
    fn test_variable_dereference_expressions() {
        assert_pattern!(quick_parse("my_variable").primary_expr(),
            Ok(parsing::Expression::Variable { pos: _, identifier: _ }));
    }

    #[test]
    #[allow(illegal_floating_point_literal_pattern)]
    fn test_unary_expressions() {
        let mut prsr = quick_parse("10 ~10 !true");

        assert_pattern!(prsr.unary_expr(),
            Ok(parsing::Expression::NumberLiteral { pos: _, value: 10.0 }));
        assert_pattern!(prsr.expression(), Ok(parsing::Expression::UnaryMinus(_)));
        assert_pattern!(prsr.unary_expr(), Ok(parsing::Expression::BooleanNot(_)));
    }

    #[test]
    fn test_expression_prescendece() {
        assert_pattern!(quick_parse("10 + 2 * 5").expression(), Ok(parsing::Expression::Add(_, _)));
        assert_pattern!(quick_parse("2 * 5 + 10").expression(), Ok(parsing::Expression::Add(_, _)));
        assert_pattern!(quick_parse("(10 + 2) * 5").expression(), Ok(parsing::Expression::Multiply(_, _)));
        assert_pattern!(quick_parse("10 > 2 / 5").expression(), Ok(parsing::Expression::GreaterThan(_, _)));
        assert_pattern!(quick_parse("true == 10 > 2 / 5").expression(), Ok(parsing::Expression::Equal(_, _)));
    }

    #[test]
    fn test_sequences_of_expressions() {
        assert_eq!(quick_parse("10 + 5, 2").expressions().unwrap().len(), 2);
        assert_eq!(quick_parse("10 + 5").expressions().unwrap().len(), 1);
        assert_pattern!(quick_parse("").expressions(), Err(super::Failure::UnexpectedStreamEnd(_)));
        assert_pattern!(quick_parse(",").expressions(), Err(super::Failure::UnexpectedToken(_, _)));
    }
}