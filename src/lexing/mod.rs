pub mod lexer;

use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Newline(usize), // Value is indentation level of the new line.

    Identifier(String),
    TypeIdentifier(String),

    NumberLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),

    IfKeyword, // if
    ElseKeyword, // else
    WhileKeyword, // while
    TrueKeyword, // true
    FalseKeyword, // false

    BracketOpen, // (
    BracketClose, // )
    BracketSquareOpen, // [
    BracketSquareClose, // ]

    DoubleEquals, // ==
    Arrow, // ->

    GreaterThan, // >
    LessThan, // <
    Comma, // ,
    Equals, // =
    Plus, // +
    Minus, // -
    Slash, // /
    Star, // *
    Caret, // ^
    ExclaimationMark, // !
    Tilde // ~
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Key {
    Initial,
    Integer, PotentialReal, Real,
    IdentifierOrKeyword, TypeIdentifier,
    Newline,
    PotentialString, StringEscapeSequence, StringLiteral,
    BeginChar, CharEnd, CharEscapeSequence, CharLiteral,
    Minus,
    Equals,
    Other
}

/// Static `lexer::Lexer` type that is specific to the TILL language (i.e. set
/// up to return TILL tokens and use the appropriate state keys).
/// Use the `new_lexer` function *once* to create an instance of this type that
/// has the appropriate states required to lex TILL code.
pub type TillLexer = lexer::Lexer<'static, Key, Token>;

pub fn new_lexer() -> TillLexer {
    log::info!("Setting up lexer states and transitions...");

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
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('-'),
                    to: lexer::Dest::To(Key::Minus)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('='),
                    to: lexer::Dest::To(Key::Equals)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChars(vec!['(', ')', '[', ']', '>', '<', ',', '+', '/', '*', '^', '!', '~']),
                    to: lexer::Dest::To(Key::Other)
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
                    "while" => Token::WhileKeyword,
                    "true" => Token::TrueKeyword,
                    "false" => Token::FalseKeyword,
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

    /* MINUS */

    states.insert(
        Key::Minus,
        lexer::State {
            parse: lexer::Parse::To(Token::Minus),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('>'), // Lexeme will be: ->
                    to: lexer::Dest::To(Key::Other)
                }
            ]
        }
    );

    /* EQUALS */

    states.insert(
        Key::Equals,
        lexer::State {
            parse: lexer::Parse::To(Token::Equals),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('='), // Lexeme will be: ==
                    to: lexer::Dest::To(Key::Other)
                }
            ]
        }
    );


    /* OTHER TOKENS */

    states.insert(
        Key::Other,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                match lexeme {
                    "->" => Token::Arrow,
                    "==" => Token::DoubleEquals,

                    "(" => Token::BracketOpen,
                    ")" => Token::BracketClose,
                    "[" => Token::BracketSquareOpen,
                    "]" => Token::BracketSquareClose,
                    ">" => Token::GreaterThan,
                    "<" => Token::LessThan,
                    "," => Token::Comma,
                    "+" => Token::Plus,
                    "/" => Token::Slash,
                    "*" => Token::Star,
                    "^" => Token::Caret,
                    "!" => Token::ExclaimationMark,
                    "~" => Token::Tilde,
                    _ => panic!()
                }
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

    impl lexer::LexIterator<'_, Key, Token> {
        fn assert_next(&mut self, expected_tok: Token) -> &mut Self {
            if let Some(lexer::LexResult::Success(_, _, tok)) = self.next() {
                assert_eq!(tok, expected_tok);
            }
            else { panic!("Expected LexResult::Success variant!"); }
            self
        }
    
        fn assert_failure_next(&mut self, expect_lexeme: &str) -> &mut Self {
            if let Some(lexer::LexResult::Failure(lexeme, _)) = self.next() {
                assert_eq!(lexeme, expect_lexeme.to_string());
            }
            else { panic!("Expected LexResult::Failure variant!"); }
            self
        }
    
        fn assert_end_of_stream(&mut self) {
            assert_eq!(self.next(), None);
        }
    }

    #[test]
    fn test_ignored_characters() {
        new_lexer().input(Stream::from_str("  5 6.2   "))
        .assert_next(Token::NumberLiteral(5.0))
        .assert_next(Token::NumberLiteral(6.2))
        .assert_end_of_stream();
    }

    #[test]
    fn test_number_literals() {
        new_lexer().input(Stream::from_str("12.3 12."))
        .assert_next(Token::NumberLiteral(12.3))
        .assert_failure_next("12.");
    }

    #[test]
    fn test_identifiers() {
        new_lexer().input(Stream::from_str("someTHIng _with5and6   Type Nice1_"))
        .assert_next(Token::Identifier("someTHIng".to_string()))
        .assert_next(Token::Identifier("_with5and6".to_string()))
        .assert_next(Token::TypeIdentifier("Type".to_string()))
        .assert_next(Token::TypeIdentifier("Nice1_".to_string()));
    }

    #[test]
    fn test_keywords() {
        new_lexer().input(Stream::from_str("if else  while  true false"))
        .assert_next(Token::IfKeyword)
        .assert_next(Token::ElseKeyword)
        .assert_next(Token::WhileKeyword)
        .assert_next(Token::TrueKeyword)
        .assert_next(Token::FalseKeyword);
    }

    #[test]
    fn test_indentation() {
        new_lexer().input(Stream::from_str("0\n\t1\n\t\t2\n0   \n\t\t\n\t"))
        .assert_next(Token::NumberLiteral(0.0))
        .assert_next(Token::Newline(1))
        .assert_next(Token::NumberLiteral(1.0))
        .assert_next(Token::Newline(2))
        .assert_next(Token::NumberLiteral(2.0))
        .assert_next(Token::Newline(0))
        .assert_next(Token::NumberLiteral(0.0))

        .assert_next(Token::Newline(1))
        .assert_end_of_stream();
    }

    #[test]
    fn test_string_literals() {
        new_lexer().input(Stream::from_str("\"\" \"hello\\tworld\" \"世界\" \"\\n\\t\\\"\\\\\""))
        .assert_next(Token::StringLiteral("".to_string()))
        .assert_next(Token::StringLiteral("hello\tworld".to_string()))
        .assert_next(Token::StringLiteral("世界".to_string()))
        .assert_next(Token::StringLiteral("\n\t\"\\".to_string()));
    }

    #[test]
    fn test_char_literals() {
        new_lexer().input(Stream::from_str("'' 'a' 'わ' '\\'' '\\n'"))
        .assert_next(Token::CharLiteral('\0'))
        .assert_next(Token::CharLiteral('a'))
        .assert_next(Token::CharLiteral('わ'))
        .assert_next(Token::CharLiteral('\''))
        .assert_next(Token::CharLiteral('\n'));
    }

    #[test]
    fn test_minus_and_arrow() {
        new_lexer().input(Stream::from_str("- ->"))
        .assert_next(Token::Minus)
        .assert_next(Token::Arrow);
    }

    #[test]
    fn test_equals_and_double_equals() {
        new_lexer().input(Stream::from_str("= =="))
        .assert_next(Token::Equals)
        .assert_next(Token::DoubleEquals);
    }

    #[test]
    fn test_other_tokens() {
        new_lexer().input(Stream::from_str("() [] > < , + / * ^ ! ~"))
        .assert_next(Token::BracketOpen).assert_next(Token::BracketClose)
        .assert_next(Token::BracketSquareOpen).assert_next(Token::BracketSquareClose)
        .assert_next(Token::GreaterThan)
        .assert_next(Token::LessThan)
        .assert_next(Token::Comma)
        .assert_next(Token::Plus)
        .assert_next(Token::Slash)
        .assert_next(Token::Star)
        .assert_next(Token::Caret)
        .assert_next(Token::ExclaimationMark)
        .assert_next(Token::Tilde);
    }
}