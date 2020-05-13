mod lexer;

use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords:
    IfKeyword, // if
    ElseKeyword, // else
    // Whitespace:
    Newlines,
    IndentIncr,
    IndentDecr,
    // Brackets:
    BracketOpen, // (
    BracketClose, // )
    BracketSquareOpen, // [
    BracketSquareClose, // ]
    // Identifiers:
    Identifier(String),
    TypeIdentifier(String),
    // Literals:
    NumberLiteral(f64),
    StringLiteral(String),
    // Other:
    Arrow, // ->
    Comma, // ,
    Equals, // =
    Plus, // +
    Minus, // -
    Slash, // /
    Star, // *
    Caret // ^
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum StateKey {
    Initial,
    Integer, PotentialReal, Real
}

fn match_digit(c: &char) -> bool { c.is_digit(10) }

fn parse_number_literal(s: &str) -> Token { Token::NumberLiteral(s.parse().unwrap()) }

pub fn new_lexer() -> lexer::Lexer<'static, StateKey, Token> {
    let mut states = HashMap::new();

    states.insert(
        StateKey::Initial,
        lexer::State {
            parse: lexer::Parse::Invalid, // cannot exit initial state
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_digit),
                    to: lexer::Dest::To(StateKey::Integer)
                }
            ]
        }
    );

    states.insert(
        StateKey::Integer,
        lexer::State {
            parse: lexer::Parse::ByFunction(&parse_number_literal),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('.'),
                    to: lexer::Dest::To(StateKey::PotentialReal)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_digit),
                    to: lexer::Dest::ToSelf
                }
            ]
        }
    );

    states.insert(
        StateKey::PotentialReal,
        lexer::State {
            parse: lexer::Parse::Invalid, // Digit(s), decimal point, without further digit(s) in invalid
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_digit),
                    to: lexer::Dest::To(StateKey::Real)
                }
            ]
        }
    );

    states.insert(
        StateKey::Real,
        lexer::State {
            parse: lexer::Parse::ByFunction(&parse_number_literal),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_digit),
                    to: lexer::Dest::ToSelf
                }
            ]
        }
    );
    
    lexer::Lexer::new(states, StateKey::Initial)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number_literal() {
        let mut lxr = new_lexer();
        
        lxr.input_str("12.3");
        assert_eq!(lxr.next(),
            Some(lexer::LexResult::Success(Token::NumberLiteral(12.3), "12.3".to_string()))
        );

        assert_eq!(lxr.next(), None); // end of stream

        lxr.input_str("12.");
        assert_eq!(lxr.next(),
            Some(lexer::LexResult::Failure("12.".to_string()))
        );

        lxr.input_str("10abc");
        assert_eq!(lxr.next(),
            Some(lexer::LexResult::Success(Token::NumberLiteral(10.0), "10".to_string()))
        );
    }
}