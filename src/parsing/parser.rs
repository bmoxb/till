use crate::lexing::lexer;
use crate::stream;
use std::{ mem, iter, fmt };

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
    UnexpectedStreamEnd(&'static str),
    UnexpectedIndent { pos: stream::Position, unexpected_indent: usize, expected_indent: usize }
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Failure::UnexpectedToken(tok, expected) => write!(f, "Expected {} yet encountered unexpected {}", expected, tok),
            Failure::UnexpectedStreamEnd(expected) => write!(f, "Encountered the end of the token stream yet expected {}", expected),
            Failure::UnexpectedIndent { pos, unexpected_indent, expected_indent } => write!(f, "Expected an indent level of {} tabs yet encoutered {} tabs at {}", expected_indent, unexpected_indent, pos)
        }
    }
}

pub struct StatementStream<T: Iterator<Item=lexer::Token>> {
    tokens: iter::Peekable<T>
}

impl<T: Iterator<Item=lexer::Token>> Iterator for StatementStream<T> {
    type Item = Result<super::Statement, Failure>;

    /// Return the next AST statement parsed from the given token stream.
    fn next(&mut self) -> Option<Self::Item> {
        log::info!("-- Next Parsing --");

        if self.tokens.peek().is_some() { // Not at stream end?
            Some(self.statement(0, "top-level statement"))
        }
        else { None }
    }
}

impl<T: Iterator<Item=lexer::Token>> StatementStream<T> {
    /// Parse a TILL statement.
    fn statement(&mut self, current_indent: usize, stmt_type_name: &'static str) -> Result<super::Statement, Failure> {
        log::debug!("Parsing statement...");

        if let Some(tok) = self.tokens.next() {
            match tok.tok_type {
                lexer::TokenType::IfKeyword => self.if_stmt(current_indent),
                lexer::TokenType::Identifier(ident_name) => self.assignment_stmt(ident_name),

                _ => Err(Failure::UnexpectedToken(tok, stmt_type_name))
            }
        }
        else { Err(Failure::UnexpectedStreamEnd(stmt_type_name)) }
    }

    fn if_stmt(&mut self, current_indent: usize) -> Result<super::Statement, Failure> {
        // Keyword 'if' token already assumed to have been consumed.

        let condition = self.expression()?;
        let if_block = self.block(current_indent)?;
        let mut else_block = None;

        if let Some(tok) = self.tokens.next() {
            if tok.tok_type == lexer::TokenType::ElseKeyword {
                else_block = Some(self.block(current_indent)?)
            }
        }

        Ok(super::Statement::If { condition, if_block, else_block })
    }

    fn assignment_stmt(&mut self, ident_name: String) -> Result<super::Statement, Failure> {
        if let Some(tok) = self.tokens.next() {
            match tok.tok_type {
                lexer::TokenType::Equals => {
                    Ok(super::Statement::VariableAssignment {
                        identifier: ident_name,
                        assign_to: self.expression()?
                    })
                }
                _ => Err(Failure::UnexpectedToken(tok, "equals = after identifier to indicate assignment"))
            }
        }
        else { Err(Failure::UnexpectedStreamEnd("equals = after identifier to indicate assignment")) }
    }

    /// Parse a collection of one or more statements that start a new level of
    /// indentation.
    fn block(&mut self, indent_before_block: usize) -> Result<super::Block, Failure> {
        if let Some(begin_tok) = self.tokens.next() {
            match begin_tok.tok_type {
                // New block begins with a newline token:
                lexer::TokenType::Newline(block_indent) => {
                    // Indent level must increase by 1 to indicate the start of
                    // a new block:
                    if block_indent == indent_before_block + 1 {
                        log::debug!("Start of block with indent level: {}", block_indent);

                        let stmts = self.block_stmts(block_indent)?;
                        Ok(super::Block(stmts))
                    }
                    else {
                        Err(Failure::UnexpectedIndent {
                            pos: begin_tok.lexeme.pos,
                            expected_indent: indent_before_block + 1,
                            unexpected_indent: block_indent
                        })
                    }
                }

                _ => Err(Failure::UnexpectedToken(begin_tok, "increase indent for start of block"))
            }
        }
        else { Err(Failure::UnexpectedStreamEnd("increase indent for start of block")) }
    }

    fn block_stmts(&mut self, block_indent: usize) -> Result<Vec<super::Statement>, Failure> {
        let mut stmts = Vec::new();

        loop {
            log::trace!("Adding new statment to block");

            let stmt = self.statement(block_indent, "statement contained in block")?;
            stmts.push(stmt);

            // Expecting a newline token next:
            if let Some(seperating_tok) = self.tokens.next() {
                match seperating_tok.tok_type {
                    lexer::TokenType::Newline(newline_indent) => {
                        if newline_indent == block_indent {
                            log::trace!("Next statement in block at same indentation level: {}", block_indent)
                        }
                        else if newline_indent == block_indent - 1 {
                            log::debug!("Indent decrease so ending block");
                            break;
                        }
                        else {
                            return Err(Failure::UnexpectedIndent {
                                pos: seperating_tok.lexeme.pos,
                                expected_indent: block_indent,
                                unexpected_indent: newline_indent
                            })
                        }
                    }
                    
                    _ => return Err(Failure::UnexpectedToken(seperating_tok, "newline and indent between statements in block"))
                }
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
            if let Some(tok) = self.tokens.peek() {
                if tok.tok_type == *seperating_tok_type {
                    self.tokens.next();
                    expr = Ok(make_expr_func(Box::new(expr?), Box::new(sub_expr_func(self)?)));
                }
            }
        }

        expr
    }

    fn expression(&mut self) -> Result<super::Expression, Failure> {
        self.left_right_expr(
            Self::comparison_expr,
            &[(lexer::TokenType::DoubleEquals, |l, r| super::Expression::Equal(l, r))]
        )
    }

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

    fn unary_expr(&mut self) -> Result<super::Expression, Failure> {
        if let Some(coming_tok) = self.tokens.peek() {
            match coming_tok.tok_type {
                lexer::TokenType::Tilde => {
                    self.tokens.next();
                    Ok(super::Expression::UnaryMinus(Box::new(self.expression()?)))
                }
                lexer::TokenType::ExclaimationMark => {
                    self.tokens.next();
                    Ok(super::Expression::BooleanNot(Box::new(self.expression()?)))
                }
                _ => self.primary_expr()
            }
        }
        else { Err(Failure::UnexpectedStreamEnd("unary expression")) }
    }

    fn primary_expr(&mut self) -> Result<super::Expression, Failure> {
        // TODO: Array literals...

        if let Some(tok) = self.tokens.next() {
            match tok.tok_type {
                lexer::TokenType::BracketOpen => {
                    let expr = self.expression()?;

                    if let Some(next_tok) = self.tokens.next() {
                        if next_tok.tok_type == lexer::TokenType::BracketClose { Ok(expr) }
                        else { Err(Failure::UnexpectedToken(next_tok, "closing bracket ) token")) }
                    }
                    else { Err(Failure::UnexpectedStreamEnd("closing bracket ) token")) }
                }

                lexer::TokenType::NumberLiteral(_) => Ok(super::Expression::NumberLiteral(tok)),
                lexer::TokenType::StringLiteral(_) => Ok(super::Expression::StringLiteral(tok)),
                lexer::TokenType::Identifier(_) => Ok(super::Expression::Variable(tok)),
                lexer::TokenType::TrueKeyword |
                lexer::TokenType::FalseKeyword => Ok(super::Expression::BooleanLiteral(tok)),
                _ => Err(Failure::UnexpectedToken(tok, "primary expression"))
            }
        }
        else { Err(Failure::UnexpectedStreamEnd("primary expression")) }
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing;
    use crate::lexing::lexer;
    use crate::stream::Stream;

    macro_rules! assert_expr_type {
        ($x:expr, $y:pat) => {
            match Result::unwrap($x) {
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
    #[allow(illegal_floating_point_literal_pattern)]
    fn test_simple_primary_expressions() {
        let mut prsr = quick_parse("10.5 \"string\" my_identifier true");

        assert_expr_type!(prsr.primary_expr(),
            parsing::Expression::NumberLiteral(lexer::Token {
                tok_type: lexer::TokenType::NumberLiteral(10.5),
                lexeme: _
            })
        );
        assert_expr_type!(prsr.expression(), parsing::Expression::StringLiteral(_));
        assert_expr_type!(prsr.primary_expr(), parsing::Expression::Variable(_));
        assert_expr_type!(prsr.expression(),
            parsing::Expression::BooleanLiteral(lexer::Token {
                tok_type: lexer::TokenType::TrueKeyword,
                lexeme: _
            })
        );
    }

    #[test]
    fn test_equivalence_expression() {
        let mut prsr = quick_parse("10 == 2");
        
        assert_expr_type!(prsr.expression(), parsing::Expression::Equal(_, _));
    }

    #[test]
    fn test_expression_prescendece() {
        assert_expr_type!(quick_parse("10 + 2 * 5").expression(), parsing::Expression::Add(_, _));
        assert_expr_type!(quick_parse("2 * 5 + 10").expression(), parsing::Expression::Add(_, _));
        assert_expr_type!(quick_parse("10 > 2 / 5").expression(), parsing::Expression::GreaterThan(_, _));
        assert_expr_type!(quick_parse("true == 10 > 2 / 5").expression(), parsing::Expression::Equal(_, _));
    }
}