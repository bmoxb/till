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
    CharLiteral(char),
    // Other:
    Newline(usize), // Value is indentation level of the new line.
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
    IdentifierOrKeyword, TypeIdentifier,
    Newline,
    PotentialString, StringEscapeSequence, StringLiteral,
    BeginChar, CharEnd, CharEscapeSequence, CharLiteral
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
                    match_by: lexer::Match::ByFunction(&|c| c.is_ascii_uppercase()),
                    to: lexer::Dest::To(Key::TypeIdentifier)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\n'),
                    to: lexer::Dest::To(Key::Newline)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('"'),
                    to: lexer::Dest::To(Key::PotentialString)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\''),
                    to: lexer::Dest::To(Key::BeginChar)
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

    /* KEYWORDS, IDENTIFIERS & TYPE IDENTIFIERS */

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
                    match_by: lexer::Match::ByFunction(&match_alphanumeric_or_underscore),
                    to: lexer::Dest::ToSelf
                }
            ]
        }
    );

    states.insert(
        Key::TypeIdentifier,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                Token::TypeIdentifier(lexeme.to_string())
            }),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_alphanumeric_or_underscore),
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
                let line = lexeme.split("\n").last().unwrap(); // Ignore any empty lines, only consider final populated line.
                Token::Newline(line.matches("\t").count())
            }),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChars(vec!['\n', '\t']),
                    to: lexer::Dest::ToSelf
                }
            ]
        }
    );

    /* STRING LITERALS */

    states.insert(
        Key::PotentialString,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\\'),
                    to: lexer::Dest::To(Key::StringEscapeSequence)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('"'),
                    to: lexer::Dest::To(Key::StringLiteral)
                },
                lexer::Transition {
                    match_by: lexer::Match::Any, // I.e. not \ or " character
                    to: lexer::Dest::ToSelf
                }
            ]
        }
    );

    states.insert(
        Key::StringEscapeSequence,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChars(vec!['n', 't', '\\', '"']),
                    to: lexer::Dest::To(Key::PotentialString)
                }
            ]
        }
    );

    states.insert(
        Key::StringLiteral,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                let mut literal = String::new();

                if lexeme != "\"\"" {
                    let mut iter = lexeme[1..lexeme.len()-1].chars();
                    
                    while let Some(chr) = iter.next() {
                        literal.push(
                            if chr == '\\' { char_to_escape_sequence(iter.next().unwrap()) }
                            else { chr }
                        );
                    }
                }

                Token::StringLiteral(literal)
            }),
            transitions: vec![]
        }
    );

    /* CHARACTER LITERALS */

    states.insert(
        Key::BeginChar,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\''),
                    to: lexer::Dest::To(Key::CharLiteral)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\\'),
                    to: lexer::Dest::To(Key::CharEscapeSequence)
                },
                lexer::Transition {
                    match_by: lexer::Match::Any,
                    to: lexer::Dest::To(Key::CharEnd)
                }
            ]
        }
    );

    states.insert(
        Key::CharEnd,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\''),
                    to: lexer::Dest::To(Key::CharLiteral)
                }
            ]
        }
    );

    states.insert(
        Key::CharEscapeSequence,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChars(vec!['n', 't', '\\', '\'']),
                    to: lexer::Dest::To(Key::CharEnd)
                }
            ]
        }
    );

    states.insert(
        Key::CharLiteral,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                Token::CharLiteral(
                    if lexeme == "''" { '\0' }
                    else {
                        let mut chars = lexeme.chars();
                        let chr = chars.nth(1).unwrap();

                        if chr == '\\' { char_to_escape_sequence(chars.next().unwrap()) }
                        else { chr }
                    }
                )
            }),
            transitions: vec![]
        }
    );

    let ignore = vec![' ']; // Spaces can be ignored when in the initial state.

    lexer::Lexer::new(states, Key::Initial, ignore)
}

fn match_digit(c: &char) -> bool { c.is_digit(10) }
fn match_alphanumeric_or_underscore(c: &char) -> bool { c.is_ascii_alphanumeric() || *c == '_' }
fn parse_number_literal(s: &str) -> Token { Token::NumberLiteral(s.parse().unwrap()) }

fn char_to_escape_sequence(chr: char) -> char {
    match chr {
        'n' => '\n',
        't' => '\t',
        x => x
    }
}


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
        lxr.stream = Some(Stream::from_str("someTHIng _with5and6   Type Nice1_"));

        assert_success(&mut lxr, Token::Identifier("someTHIng".to_string()));
        assert_success(&mut lxr, Token::Identifier("_with5and6".to_string()));
        
        assert_success(&mut lxr, Token::TypeIdentifier("Type".to_string()));
        assert_success(&mut lxr, Token::TypeIdentifier("Nice1_".to_string()));
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

        lxr.stream = Some(Stream::from_str("\n\t\t\n\t"));

        assert_success(&mut lxr, Token::Newline(1));
        assert_eq!(lxr.next(), None);
    }

    #[test]
    fn test_string_literals() {
        let mut lxr = new_lexer();
        lxr.stream = Some(Stream::from_str("\"\" \"hello\\tworld\" \"世界\" \"\\n\\t\\\"\\\\\""));

        assert_success(&mut lxr, Token::StringLiteral("".to_string()));
        assert_success(&mut lxr, Token::StringLiteral("hello\tworld".to_string()));
        assert_success(&mut lxr, Token::StringLiteral("世界".to_string()));
        assert_success(&mut lxr, Token::StringLiteral("\n\t\"\\".to_string()));
    }

    #[test]
    fn test_char_literals() {
        let mut lxr = new_lexer();
        lxr.stream = Some(Stream::from_str("'' 'a' 'わ' '\\'' '\\n'"));

        assert_success(&mut lxr, Token::CharLiteral('\0'));
        assert_success(&mut lxr, Token::CharLiteral('a'));
        assert_success(&mut lxr, Token::CharLiteral('わ'));
        assert_success(&mut lxr, Token::CharLiteral('\''));
        assert_success(&mut lxr, Token::CharLiteral('\n'));
    }
}