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
    BeginChar, CharEnd, CharEscapeSequence, CharLiteral,
    Minus, Arrow,
    Other
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
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('-'),
                    to: lexer::Dest::To(Key::Minus)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChars(vec!['(', ')', '[', ']', ',', '=', '+', '/', '*', '^']),
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

    /* MINUS & ARROW */

    states.insert(
        Key::Minus,
        lexer::State {
            parse: lexer::Parse::To(Token::Minus),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('>'),
                    to: lexer::Dest::To(Key::Arrow)
                }
            ]
        }
    );

    states.insert(
        Key::Arrow,
        lexer::State {
            parse: lexer::Parse::To(Token::Arrow),
            transitions: vec![]
        }
    );

    /* OTHER TOKENS */

    states.insert(
        Key::Other,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                match lexeme {
                    "(" => Token::BracketOpen,
                    ")" => Token::BracketClose,
                    "[" => Token::BracketSquareOpen,
                    "]" => Token::BracketSquareClose,
                    "," => Token::Comma,
                    "=" => Token::Equals,
                    "+" => Token::Plus,
                    "/" => Token::Slash,
                    "*" => Token::Star,
                    "^" => Token::Caret,
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

    struct TestLexer<'a> {
        lxr: lexer::Lexer<'a, Key, Token>
    }

    impl TestLexer<'_> {
        fn input(s: &str) -> TestLexer {
            let mut lxr = new_lexer();
            lxr.stream = Some(Stream::from_str(s));

            TestLexer { lxr }
        }

        fn expect_next(&mut self, expected_tok: Token) -> &mut Self {
            if let Some(lexer::LexResult::Success(_, _, tok)) = self.lxr.next() {
                assert_eq!(tok, expected_tok);
            }
            else { panic!("Expected LexResult::Success variant!"); }
            self
        }

        fn expect_failure_next(&mut self, expect_lexeme: &str) -> &mut Self {
            if let Some(lexer::LexResult::Failure(lexeme, _)) = self.lxr.next() {
                assert_eq!(lexeme, expect_lexeme.to_string());
            }
            else { panic!("Expected LexResult::Failure variant!"); }
            self
        }

        fn expect_end_of_stream(&mut self) {
            assert_eq!(self.lxr.next(), None);
        }
    }

    #[test]
    fn test_ignored_characters() {
        TestLexer::input("  5 6.2   ")
        .expect_next(Token::NumberLiteral(5.0))
        .expect_next(Token::NumberLiteral(6.2))
        .expect_end_of_stream();
    }

    #[test]
    fn test_number_literals() {
        TestLexer::input("12.3 12.")
        .expect_next(Token::NumberLiteral(12.3))
        .expect_failure_next("12.");
    }

    #[test]
    fn test_identifiers() {
        TestLexer::input("someTHIng _with5and6   Type Nice1_")
        .expect_next(Token::Identifier("someTHIng".to_string()))
        .expect_next(Token::Identifier("_with5and6".to_string()))
        .expect_next(Token::TypeIdentifier("Type".to_string()))
        .expect_next(Token::TypeIdentifier("Nice1_".to_string()));
    }

    #[test]
    fn test_keywords() {
        TestLexer::input("if else")
        .expect_next(Token::IfKeyword)
        .expect_next(Token::ElseKeyword);
    }

    #[test]
    fn test_indentation() {
        TestLexer::input("0\n\t1\n\t\t2\n0   \n\t\t\n\t")
        .expect_next(Token::NumberLiteral(0.0))
        .expect_next(Token::Newline(1))
        .expect_next(Token::NumberLiteral(1.0))
        .expect_next(Token::Newline(2))
        .expect_next(Token::NumberLiteral(2.0))
        .expect_next(Token::Newline(0))
        .expect_next(Token::NumberLiteral(0.0))

        .expect_next(Token::Newline(1))
        .expect_end_of_stream();
    }

    #[test]
    fn test_string_literals() {
        TestLexer::input("\"\" \"hello\\tworld\" \"世界\" \"\\n\\t\\\"\\\\\"")
        .expect_next(Token::StringLiteral("".to_string()))
        .expect_next(Token::StringLiteral("hello\tworld".to_string()))
        .expect_next(Token::StringLiteral("世界".to_string()))
        .expect_next(Token::StringLiteral("\n\t\"\\".to_string()));
    }

    #[test]
    fn test_char_literals() {
        TestLexer::input("'' 'a' 'わ' '\\'' '\\n'")
        .expect_next(Token::CharLiteral('\0'))
        .expect_next(Token::CharLiteral('a'))
        .expect_next(Token::CharLiteral('わ'))
        .expect_next(Token::CharLiteral('\''))
        .expect_next(Token::CharLiteral('\n'));
    }

    #[test]
    fn test_minus_and_arrow() {
        TestLexer::input("- ->")
        .expect_next(Token::Minus)
        .expect_next(Token::Arrow);
    }

    #[test]
    fn test_other_tokens() {
        TestLexer::input("() []  ,  = + / * ^")
        .expect_next(Token::BracketOpen).expect_next(Token::BracketClose)
        .expect_next(Token::BracketSquareOpen).expect_next(Token::BracketSquareClose)
        .expect_next(Token::Comma)
        .expect_next(Token::Equals)
        .expect_next(Token::Plus)
        .expect_next(Token::Slash)
        .expect_next(Token::Star)
        .expect_next(Token::Caret);
    }
}