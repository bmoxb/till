mod lexer;

use std::collections::HashMap;
use lazy_static::lazy_static;

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

#[derive(PartialEq, Eq, Hash)]
pub enum StateKey {
    Initial,
    Integer, PotentialReal, Real
}

pub fn new_lexer() -> lexer::Lexer<'static, StateKey, Token> {
    lexer::Lexer::new(&STATES, StateKey::Initial)
}

lazy_static! {
    static ref STATES: lexer::States<StateKey, Token> = {
        let mut map = HashMap::new();

        map.insert(
            StateKey::Initial,
            lexer::State {
                parse_fn: None, // cannot exit initial state
                transitions: vec![
                    lexer::Transition {
                        match_fn: |c| c.is_digit(10),
                        to: lexer::Dest::To(StateKey::Integer)
                    }
                ]
            }
        );

        map.insert(
            StateKey::Integer,
            lexer::State {
                parse_fn: Some(|c| Token::NumberLiteral(c.parse().unwrap())),
                transitions: vec![
                    lexer::Transition {
                        match_fn: |c| c == &'.',
                        to: lexer::Dest::To(StateKey::PotentialReal)
                    },
                    lexer::Transition {
                        match_fn: |c| c.is_digit(10),
                        to: lexer::Dest::ToSelf
                    }
                ]
            }
        );

        map.insert(
            StateKey::PotentialReal,
            lexer::State {
                parse_fn: None, // Digit(s), decimal point, without further digit(s) in invalid
                transitions: vec![
                    lexer::Transition {
                        match_fn: |c| c.is_digit(10),
                        to: lexer::Dest::To(StateKey::Real)
                    }
                ]
            }
        );

        map.insert(
            StateKey::Real,
            lexer::State {
                parse_fn: Some(|c| Token::NumberLiteral(c.parse().unwrap())),
                transitions: vec![
                    lexer::Transition {
                        match_fn: |c| c.is_digit(10),
                        to: lexer::Dest::ToSelf
                    }
                ]
            }
        );

        map
    };
}
