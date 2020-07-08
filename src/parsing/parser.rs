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
        if self.match_token_types(&[lexer::TokenType::IfKeyword]) {
            log::trace!("Matching potential if statement...");

            if let Some(condition) = self.expression() {
                if let Some(if_block) = self.block() {
                    // Is there an else block here too?
                    let else_block = if self.match_token_types(&[lexer::TokenType::ElseKeyword]) {
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
    fn match_token_types(&mut self, any_of: &[lexer::TokenType]) -> bool { // TODO: Return the matched token!
        if let Some(current_token) = self.tokens.peek() {
            for match_token in any_of {
                // Match enum variant, irrespective of contained value:
                if mem::discriminant(match_token) == mem::discriminant(&current_token.tok_type) {
                    self.tokens.next();
                    return true;
                }
            }
        }
        false
    }

    fn block(&mut self) -> Option<Vec<super::Statement>> {
        None
    }

    fn left_right_expr<F, G>(&mut self, subexpr_func: F, separating_tokens: &[lexer::TokenType], construct_func: G) -> Option<super::Expression>
    where F: Fn(&mut Self) -> Option<super::Expression>, G: Fn(Box<super::Expression>, Box<super::Expression>) -> super::Expression {
        if let Some(mut left) = subexpr_func(self) {
            while self.match_token_types(separating_tokens) {
                if let Some(right) = subexpr_func(self) {
                    left = construct_func(Box::new(left), Box::new(right));
                }
                else { break }
            }

            Some(left)
        }
        else { None }
    }

    fn expression(&mut self) -> Option<super::Expression> {
        self.left_right_expr(
            Self::comparison_expr,
            &[lexer::TokenType::DoubleEquals /* match '!=' too? */],
            |l, r| super::Expression::Equal(l, r)
        )
    }

    fn comparison_expr(&mut self) -> Option<super::Expression> {
        self.left_right_expr(
            Self::addition_expr,
            &[lexer::TokenType::GreaterThan, lexer::TokenType::LessThan],
            |l, r| super::Expression::GreaterThan(l, r) // TODO: or less than...
        )
    }

    fn addition_expr(&mut self) -> Option<super::Expression> {
        self.left_right_expr(
            Self::multiplication_expr,
            &[lexer::TokenType::Plus, lexer::TokenType::Minus],
            |l, r| super::Expression::Add(l, r) // TODO: or subtract...
        )
    }

    fn multiplication_expr(&mut self) -> Option<super::Expression> {
        self.left_right_expr(
            Self::unary_expr,
            &[lexer::TokenType::Star, lexer::TokenType::Slash],
            |l, r| super::Expression::Multiply(l, r) // TODO: or divide...
        )
    }

    fn unary_expr(&mut self) -> Option<super::Expression> {
        None // TODO: ...
    }

    fn primary_expr(&mut self) -> Option<super::Expression> {
        None // TODO: ...
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

        assert!(prsr.match_token_types(&[lexer::TokenType::Identifier("".to_string())]));
        assert_eq!(prsr.match_token_types(&[lexer::TokenType::Minus, lexer::TokenType::NumberLiteral(0.0)]), false);
        assert!(prsr.match_token_types(&[lexer::TokenType::Minus, lexer::TokenType::StringLiteral("".to_string())]));
    }

    #[test]
    fn test_parsing_if_statement() {
        pretty_env_logger::init_timed(); // TODO: Temporary...

        let tokens = vec![
            lexing::GenericToken {
                tok_type: lexer::TokenType::IfKeyword,
                lexeme: lexing::Lexeme {
                    text: "if".to_string(),
                    pos: stream::Position::new()
                }
            },
            lexing::GenericToken {
                tok_type: lexer::TokenType::NumberLiteral(10.0),
                lexeme: lexing::Lexeme {
                    text: "10".to_string(),
                    pos: stream::Position::new()
                }
            },
            lexing::GenericToken {
                tok_type: lexer::TokenType::Newline(1),
                lexeme: lexing::Lexeme {
                    text: "\n\t".to_string(),
                    pos: stream::Position::new()
                }
            }
        ].into_iter();

        let mut prsr = super::input(tokens);

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