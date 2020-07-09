use crate::lexing::lexer;
use std::iter::Peekable;
use std::mem;

/// Returns an iterator that yields abstract syntax representations for each
/// TILL statement parsed from the given token stream.
pub fn input<T: Iterator<Item=lexer::Token>>(tokens: T) -> StatementStream<T> {
    StatementStream { tokens: tokens.peekable() }
}

pub struct StatementStream<T: Iterator<Item=lexer::Token>> {
    tokens: Peekable<T>
}

impl<T: Iterator<Item=lexer::Token>> Iterator for StatementStream<T> {
    type Item = super::Statement;

    /// Return the next AST statement parsed from the given token stream.
    fn next(&mut self) -> Option<Self::Item> {
        /*
         * IF STATEMENT:
         * "if" <expr> <block> ("else" <block>)?
         */
        if self.match_token_type(&lexer::TokenType::IfKeyword).is_some() {
            log::trace!("Matching potential if statement...");

            if let Some(condition) = self.expression() {
                if let Some(if_block) = self.block() {
                    // Is there an else block here too?
                    let else_block = if self.match_token_type(&lexer::TokenType::ElseKeyword).is_some() {
                        log::trace!("Optional else block appears to be present also (assuming block follows encountered 'else' keyword)...");
                        self.block()
                    }
                    else { None };

                    log::debug!("Matched if statement!");

                    return Some(super::Statement::If {
                        condition, if_block, else_block
                    })
                }
                else { log::trace!("If statement match failed as a block was expected"); }

            }
            else { log::trace!("If statement match failed as condition expression was expected"); }
        }
        
        /*
         * FUNCTION DEFINITION:
         * identifier <parameters> ('->' <type>)? <block>
         */
        // TODO: ...

        None
    }
}

impl<T: Iterator<Item=lexer::Token>> StatementStream<T> {
    // TODO: use matches instead of this function?
    fn match_token_type(&mut self, match_type: &lexer::TokenType) -> Option<lexer::Token> {
        if let Some(current_token) = self.tokens.peek() {
            // Match enum variant, irrespective of contained value:
            if mem::discriminant(match_type) == mem::discriminant(&current_token.tok_type) {
                return self.tokens.next();
            }
        }
        None
    }

    /*fn match_token_types(&mut self, any_of: &[lexer::TokenType]) -> Option<lexer::Token> {
        if let Some(current_token) = self.tokens.peek() {
            for match_token in any_of {
                // Match enum variant, irrespective of contained value:
                if mem::discriminant(match_token) == mem::discriminant(&current_token.tok_type) {
                    return self.tokens.next();
                }
            }
        }
        None
    }*/

    fn block(&mut self) -> Option<Vec<super::Statement>> {
        None
    }

    fn left_right_expr<F: Fn(&mut Self) -> Option<super::Expression>>(&mut self, subexpr_func: F,
        separator_to_expr_func: &[(lexer::TokenType, fn(Box<super::Expression>, Box<super::Expression>) -> super::Expression)]) -> Option<super::Expression> {
        // Match the initial left-hand expression:
        if let Some(mut left) = subexpr_func(self) {
            for (separator_tok_type, construction_func) in separator_to_expr_func {
                // Match separator token (e.g. double equals, greater than, etc.):
                if self.match_token_type(separator_tok_type).is_some() {
                    // Match the subsequent right-hand expression:
                    if let Some(right) = subexpr_func(self) {
                        // Form a new left-hand expression that is the product
                        // of the previous two expressions:
                        left = construction_func(Box::new(left), Box::new(right));
                    }
                }
            }

            Some(left)
        }
        else { None }
    }

    fn expression(&mut self) -> Option<super::Expression> {
        self.left_right_expr(
            Self::comparison_expr,
            &[(
                lexer::TokenType::DoubleEquals,
                |l, r| super::Expression::Equal(l, r)
            )]
        )
    }

    fn comparison_expr(&mut self) -> Option<super::Expression> {
        self.left_right_expr(
            Self::addition_expr,
            &[
                (
                    lexer::TokenType::GreaterThan,
                    |l, r| super::Expression::GreaterThan(l, r)
                ),
                (
                    lexer::TokenType::LessThan,
                    |l, r| super::Expression::LessThan(l, r)
                )
            ]
        )
    }

    fn addition_expr(&mut self) -> Option<super::Expression> {
        self.left_right_expr(
            Self::multiplication_expr,
            &[
                (
                    lexer::TokenType::Plus,
                    |l, r| super::Expression::Add(l, r)
                 ),
                 (
                     lexer::TokenType::Minus,
                     |l, r| super::Expression::Subtract(l, r)
                 )
            ]
        )
    }

    fn multiplication_expr(&mut self) -> Option<super::Expression> {
        self.left_right_expr(
            Self::unary_expr,
            &[
                (
                    lexer::TokenType::Star,
                    |l, r| super::Expression::Multiply(l, r)
                ),
                (
                    lexer::TokenType::Slash,
                    |l, r| super::Expression::Divide(l, r)
                )
            ]
        )
    }

    fn unary_expr(&mut self) -> Option<super::Expression> {
        if self.match_token_type(&lexer::TokenType::Tilde).is_some() {
            Some(super::Expression::UnaryMinus(Box::new(self.expression()?)))
        }
        else if self.match_token_type(&lexer::TokenType::ExclaimationMark).is_some() {
            Some(super::Expression::BooleanNot(Box::new(self.expression()?)))
        }
        else { self.expression() }
    }

    fn primary_expr(&mut self) -> Option<super::Expression> {
        if let Some(tok) = self.tokens.next() {
            // TODO: Array literals...
            // TODO: Bracketed expressions...
            match &tok.tok_type {
                lexer::TokenType::NumberLiteral(_) => Some(super::Expression::NumberLiteral(tok)),
                lexer::TokenType::StringLiteral(_) => Some(super::Expression::StringLiteral(tok)),
                lexer::TokenType::Identifier(_) => Some(super::Expression::Variable(tok)),
                lexer::TokenType::TrueKeyword | lexer::TokenType::FalseKeyword => Some(super::Expression::BooleanLiteral(tok)),
                _ => None
            }
        }
        else {
            log::debug!("Expected primary expression yet have reached end of token stream!");
            None
        }
    }
}

#[cfg(test)]
mod tests {
    //use crate::parsing;
    use crate::lexing;
    use crate::lexing::lexer;
    use crate::stream;

    #[test]
    fn test_token_type_matching() {
        let tokens = vec![
            lexing::GenericToken {
                tok_type: lexer::TokenType::Identifier("abc".to_string()),
                lexeme: lexing::Lexeme {
                    text: "abc".to_string(),
                    pos: stream::Position::new()
                }
            },
            lexing::GenericToken {
                tok_type: lexer::TokenType::StringLiteral("wow\n".to_string()),
                lexeme: lexing::Lexeme {
                    text: "\"wow\\n\"".to_string(),
                    pos: stream::Position::new()
                }
            }
        ].into_iter();
        
        let mut prsr = super::input(tokens);

        assert!(prsr.match_token_type(&lexer::TokenType::Identifier("".to_string())).is_some());
        assert!(prsr.match_token_type(&lexer::TokenType::Minus).is_none());
        assert!(prsr.match_token_type(&lexer::TokenType::StringLiteral("".to_string())).is_some());
    }

    #[test]
    fn test_parsing_if_statement() {
        let tokens = lexer::input(stream::Stream::from_str("if 10\n\t")).map(Result::unwrap);
        let mut prsr = super::input(tokens);

        // TODO: test!
        /*assert_eq!(
            prsr.next(),
            Some(parsing::Statement::If {
                condition: parsing::Expression,
                if_block: vec![],
                else_block: None
            })
        );*/
    }
}