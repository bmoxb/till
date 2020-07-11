use crate::lexing::lexer;
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
    UnexpectedStreamEnd(&'static str)
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Failure::UnexpectedToken(tok, expected) => write!(f, "Expected {} yet encountered token {:?}", expected, tok),
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
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(tok) = self.tokens.next() {
            Some(match tok.tok_type {
                lexer::TokenType::IfKeyword => self.if_statement(),

                _ => Err(Failure::UnexpectedToken(tok, "top-level statement"))
            })
        }
        else { None }
    }
}

impl<T: Iterator<Item=lexer::Token>> StatementStream<T> {
    fn if_statement(&mut self) -> Result<super::Statement, Failure> {
        // Keyword 'if' token already assumed to have been consumed.

        let condition = self.expression()?;
        let if_block = self.block()?;
        let mut else_block = None;

        if let Some(tok) = self.tokens.next() {
            if tok.tok_type == lexer::TokenType::ElseKeyword {
                else_block = Some(self.block()?)
            }
        }

        Ok(super::Statement::If { condition, if_block, else_block })
    }

    fn block(&mut self) -> Result<Vec<super::Statement>, Failure> {
        unimplemented!() // TODO!
    }

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

    #[test]
    fn test_simple_primary_expressions() {
        let tokens = lexer::input(Stream::from_str("10.5 \"string\" my_identifier true")).map(Result::unwrap);
        let mut prsr = super::input(tokens);

        assert_expr_type!(prsr.primary_expr(), parsing::Expression::NumberLiteral(_));
        assert_expr_type!(prsr.expression(), parsing::Expression::StringLiteral(_));
        assert_expr_type!(prsr.primary_expr(), parsing::Expression::Variable(_));
        assert_expr_type!(prsr.expression(), parsing::Expression::BooleanLiteral(_));
    }

    #[test]
    fn test_equivalence_expression() {
        let tokens = lexer::input(Stream::from_str("10 == 2")).map(Result::unwrap);
        let mut prsr = super::input(tokens);
        
        assert_expr_type!(prsr.expression(), parsing::Expression::Equal(_, _));
    }
}