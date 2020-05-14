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
    Integer, PotentialReal, Real,
    IdentifierOrKeyword
}

pub fn new_lexer() -> lexer::Lexer<'static, StateKey, Token> {
    let mut states = HashMap::new();

    /* INITIAL STATE */

    states.insert(
        StateKey::Initial,
        lexer::State {
            parse: lexer::Parse::Invalid, // cannot exit initial state
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_digit),
                    to: lexer::Dest::To(StateKey::Integer)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&|c| c.is_ascii_lowercase() || *c == '_'),
                    to: lexer::Dest::To(StateKey::IdentifierOrKeyword)
                }
            ]
        }
    );

    /* NUMBER LITERALS */

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

    /* KEYWORDS & IDENTIFIERS */

    states.insert(
        StateKey::IdentifierOrKeyword,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                match lexeme {
                    "if" => Token::IfKeyword,
                    "else" => Token::ElseKeyword,
                    x => Token::Identifier(x.to_string())
                }
            }),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&|c| c.is_ascii_alphanumeric()),
                    to: lexer::Dest::ToSelf
                }
            ]
        }
    );

    let ignore = vec![' ']; // spaces can be ignored when in initial state
    
    lexer::Lexer::new(states, StateKey::Initial, ignore)
}

fn match_digit(c: &char) -> bool { c.is_digit(10) }
fn parse_number_literal(s: &str) -> Token { Token::NumberLiteral(s.parse().unwrap()) }


#[cfg(test)]
mod tests {
    use super::*;

    fn assert_lexing_success(lxr: &mut lexer::Lexer<StateKey, Token>, tok: Token, lexeme: &str) {
        assert_eq!(
            lxr.next(),
            Some(lexer::LexResult::Success(tok, lexeme.to_string()))
        )
    }

    fn assert_lexing_failure(lxr: &mut lexer::Lexer<StateKey, Token>, lexeme: &str) {
        assert_eq!(
            lxr.next(),
            Some(lexer::LexResult::Failure(lexeme.to_string()))
        )
    }

    #[test]
    fn test_ignored_characters() {
        let mut lxr = new_lexer();
        lxr.set_stream_by_str("   5 6.2  ");

        assert_lexing_success(&mut lxr, Token::NumberLiteral(5.0), "5");
        assert_lexing_success(&mut lxr, Token::NumberLiteral(6.2), "6.2");
        assert_eq!(lxr.next(), None); // last 2 spaces are ignored so effectively end of stream
    }

    #[test]
    fn test_number_literals() {
        let mut lxr = new_lexer();
        
        lxr.set_stream_by_str("12.3nexttoken");
        assert_lexing_success(&mut lxr, Token::NumberLiteral(12.3), "12.3");

        lxr.set_stream_by_str("12.");
        assert_lexing_failure(&mut lxr, "12.");

        assert_eq!(lxr.next(), None);
    }

    #[test]
    fn test_identifiers() {
        let mut lxr = new_lexer();

        lxr.set_stream_by_str("someTHIng _with5and6");

        let first = "someTHIng";
        assert_lexing_success(&mut lxr, Token::Identifier(first.to_string()), first);

        let second = "_with5and6";
        assert_lexing_success(&mut lxr, Token::Identifier(second.to_string()), second);
    }

    #[test]
    fn test_keywords() {
        let mut lxr = new_lexer();
        lxr.set_stream_by_str("if else");

        assert_lexing_success(&mut lxr, Token::IfKeyword, "if");
        assert_lexing_success(&mut lxr, Token::ElseKeyword, "else");
    }
}