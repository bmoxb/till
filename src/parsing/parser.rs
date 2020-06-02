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
    fn next(&mut self) -> Option<Self::Item> { None }
}

impl<T: Iterator<Item=lexer::Token>> StatementStream<T> {
    fn match_token_types(&mut self, any_of: &[lexer::TokenType]) -> bool {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexing;
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
        
        let mut lxr = input(tokens);

        assert!(lxr.match_token_types(&[lexer::TokenType::Identifier("".to_string())]));
        assert_eq!(lxr.match_token_types(&[lexer::TokenType::Minus, lexer::TokenType::NumberLiteral(0.0)]), false);
        assert!(lxr.match_token_types(&[lexer::TokenType::Minus, lexer::TokenType::StringLiteral("".to_string())]));
    }
}