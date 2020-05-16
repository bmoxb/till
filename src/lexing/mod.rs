pub mod lexer;

use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords:
    IfKeyword, // if
    ElseKeyword, // else
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
    Newline(usize), // value is indentation level of the new line
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
pub enum Key {
    Initial,
    Integer, PotentialReal, Real,
    IdentifierOrKeyword,
    Newline
}

pub fn new_lexer() -> lexer::Lexer<'static, Key, Token> {
    let mut states = HashMap::new();

    /* INITIAL STATE */

    states.insert(
        Key::Initial,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_digit),
                    to: lexer::Dest::To(Key::Integer)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&|c| c.is_ascii_lowercase() || *c == '_'),
                    to: lexer::Dest::To(Key::IdentifierOrKeyword)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\n'),
                    to: lexer::Dest::To(Key::Newline)
                }
            ]
        }
    );

    /* NUMBER LITERALS */

    states.insert(
        Key::Integer,
        lexer::State {
            parse: lexer::Parse::ByFunction(&parse_number_literal),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('.'),
                    to: lexer::Dest::To(Key::PotentialReal)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_digit),
                    to: lexer::Dest::ToSelf
                }
            ]
        }
    );

    states.insert(
        Key::PotentialReal,
        lexer::State {
            parse: lexer::Parse::Invalid, // Digit(s), decimal point, without further digit(s) is invalid.
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_digit),
                    to: lexer::Dest::To(Key::Real)
                }
            ]
        }
    );

    states.insert(
        Key::Real,
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
        Key::IdentifierOrKeyword,
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
                    match_by: lexer::Match::ByFunction(&|c| c.is_ascii_alphanumeric() || *c == '_'),
                    to: lexer::Dest::ToSelf
                }
            ]
        }
    );
    
    /* NEWLINES & INDENTATION */

    states.insert(
        Key::Newline,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                Token::Newline(lexeme.matches("\t").count())
            }),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChars(vec!['\n', '\t']),
                    to: lexer::Dest::ToSelf
                }
            ]
        }
    );

    let ignore = vec![' ']; // Spaces can be ignored when in the initial state.

    lexer::Lexer::new(states, Key::Initial, ignore)
}

fn match_digit(c: &char) -> bool { c.is_digit(10) }
fn parse_number_literal(s: &str) -> Token { Token::NumberLiteral(s.parse().unwrap()) }


#[cfg(test)]
mod tests {
    use super::*;
    use crate::stream::Stream;

    fn assert_success(lxr: &mut lexer::Lexer<Key, Token>, expected_tok: Token) {
        if let Some(lexer::LexResult::Success(_, _, tok)) = lxr.next() {
            assert_eq!(tok, expected_tok);
        }
        else { panic!("Expected LexResult::Success variant!"); }
    }

    fn assert_failure(lxr: &mut lexer::Lexer<Key, Token>, expect_lexeme: &str) {
        if let Some(lexer::LexResult::Failure(lexeme, _)) = lxr.next() {
            assert_eq!(lexeme, expect_lexeme.to_string());
        }
        else { panic!("Expected LexResult::Failure variant!"); }
    }

    #[test]
    fn test_ignored_characters() {
        let mut lxr = new_lexer();
        lxr.stream = Some(Stream::from_str("   5 6.2  "));

        assert_success(&mut lxr, Token::NumberLiteral(5.0));
        assert_success(&mut lxr, Token::NumberLiteral(6.2));
        assert_eq!(lxr.next(), None); // last 2 spaces are ignored so effectively end of stream
    }

    #[test]
    fn test_number_literals() {
        let mut lxr = new_lexer();
        
        lxr.stream = Some(Stream::from_str("12.3nexttoken"));
        assert_success(&mut lxr, Token::NumberLiteral(12.3));

        lxr.stream = Some(Stream::from_str("12."));
        assert_failure(&mut lxr, "12.");

        assert_eq!(lxr.next(), None);
    }

    #[test]
    fn test_identifiers() {
        let mut lxr = new_lexer();

        lxr.stream = Some(Stream::from_str("someTHIng _with5and6"));
        assert_success(&mut lxr, Token::Identifier("someTHIng".to_string()));
        assert_success(&mut lxr, Token::Identifier("_with5and6".to_string()));
    }

    #[test]
    fn test_keywords() {
        let mut lxr = new_lexer();
        lxr.stream = Some(Stream::from_str("if else"));

        assert_success(&mut lxr, Token::IfKeyword);
        assert_success(&mut lxr, Token::ElseKeyword);
    }

    #[test]
    fn test_indentation() {
        let mut lxr = new_lexer();
        lxr.stream = Some(Stream::from_str("zero\n\tfirst\n\t\tsecond\nzero_again"));

        assert_success(&mut lxr, Token::Identifier("zero".to_string()));
        assert_success(&mut lxr, Token::Newline(1));
        assert_success(&mut lxr, Token::Identifier("first".to_string()));
        assert_success(&mut lxr, Token::Newline(2));
        assert_success(&mut lxr, Token::Identifier("second".to_string()));
        assert_success(&mut lxr, Token::Newline(0));
        assert_success(&mut lxr, Token::Identifier("zero_again".to_string()));
    }
}