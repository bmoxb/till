pub mod lexer;

use std::collections::HashMap;

/// All lexing tokens yielded by the TILL lexer.
#[derive(Debug, PartialEq, Clone)]
pub enum TillToken {
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

/// The range of state keys used by the TILL lexer.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TillKey {
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

/// Type alias for the generic lexer but with the TILL-specific Key and Token
/// type arguments.
pub type TillLexer<'a> = lexer::Lexer<'a, TillKey, TillToken>;

pub type TillLexTokenIterator<'a> = lexer::LexTokenIterator<'a, TillKey, TillToken>;

/// Returns a static `lexer::Lexer` type that is specific to the TILL language
/// (i.e. set up to return TILL tokens and use the appropriate state keys).
/// Use this function *once* to create an instance of this type that has the
/// appropriate states required to lex TILL code.
pub fn new_till_lexer<'a>() -> lexer::Lexer<'a, TillKey, TillToken> {
    log::info!("Setting up lexer states and transitions...");

    let mut states = HashMap::new();

    /* INITIAL STATE */

    states.insert(
        TillKey::Initial,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_digit),
                    to: lexer::Dest::To(TillKey::Integer)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&|c| c.is_ascii_lowercase() || *c == '_'),
                    to: lexer::Dest::To(TillKey::IdentifierOrKeyword)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&|c| c.is_ascii_uppercase()),
                    to: lexer::Dest::To(TillKey::TypeIdentifier)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\n'),
                    to: lexer::Dest::To(TillKey::Newline)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('"'),
                    to: lexer::Dest::To(TillKey::PotentialString)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\''),
                    to: lexer::Dest::To(TillKey::BeginChar)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('-'),
                    to: lexer::Dest::To(TillKey::Minus)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('='),
                    to: lexer::Dest::To(TillKey::Equals)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChars(vec!['(', ')', '[', ']', '>', '<', ',', '+', '/', '*', '^', '!', '~']),
                    to: lexer::Dest::To(TillKey::Other)
                }
            ]
        }
    );

    /* NUMBER LITERALS */

    states.insert(
        TillKey::Integer,
        lexer::State {
            parse: lexer::Parse::ByFunction(&parse_number_literal),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('.'),
                    to: lexer::Dest::To(TillKey::PotentialReal)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_digit),
                    to: lexer::Dest::ToSelf
                }
            ]
        }
    );

    states.insert(
        TillKey::PotentialReal,
        lexer::State {
            parse: lexer::Parse::Invalid, // Digit(s), decimal point, without further digit(s) is invalid.
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByFunction(&match_digit),
                    to: lexer::Dest::To(TillKey::Real)
                }
            ]
        }
    );

    states.insert(
        TillKey::Real,
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
        TillKey::IdentifierOrKeyword,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                match lexeme {
                    "if" => TillToken::IfKeyword,
                    "else" => TillToken::ElseKeyword,
                    "while" => TillToken::WhileKeyword,
                    "true" => TillToken::TrueKeyword,
                    "false" => TillToken::FalseKeyword,
                    x => TillToken::Identifier(x.to_string())
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
        TillKey::TypeIdentifier,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                TillToken::TypeIdentifier(lexeme.to_string())
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
        TillKey::Newline,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                let line = lexeme.split("\n").last().unwrap(); // Ignore any empty lines, only consider final populated line.
                TillToken::Newline(line.matches("\t").count())
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
        TillKey::PotentialString,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\\'),
                    to: lexer::Dest::To(TillKey::StringEscapeSequence)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('"'),
                    to: lexer::Dest::To(TillKey::StringLiteral)
                },
                lexer::Transition {
                    match_by: lexer::Match::Any, // I.e. not \ or " character
                    to: lexer::Dest::ToSelf
                }
            ]
        }
    );

    states.insert(
        TillKey::StringEscapeSequence,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChars(vec!['n', 't', '\\', '"']),
                    to: lexer::Dest::To(TillKey::PotentialString)
                }
            ]
        }
    );

    states.insert(
        TillKey::StringLiteral,
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

                TillToken::StringLiteral(literal)
            }),
            transitions: vec![]
        }
    );

    /* CHARACTER LITERALS */

    states.insert(
        TillKey::BeginChar,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\''),
                    to: lexer::Dest::To(TillKey::CharLiteral)
                },
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\\'),
                    to: lexer::Dest::To(TillKey::CharEscapeSequence)
                },
                lexer::Transition {
                    match_by: lexer::Match::Any,
                    to: lexer::Dest::To(TillKey::CharEnd)
                }
            ]
        }
    );

    states.insert(
        TillKey::CharEnd,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('\''),
                    to: lexer::Dest::To(TillKey::CharLiteral)
                }
            ]
        }
    );

    states.insert(
        TillKey::CharEscapeSequence,
        lexer::State {
            parse: lexer::Parse::Invalid,
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChars(vec!['n', 't', '\\', '\'']),
                    to: lexer::Dest::To(TillKey::CharEnd)
                }
            ]
        }
    );

    states.insert(
        TillKey::CharLiteral,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                TillToken::CharLiteral(
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
        TillKey::Minus,
        lexer::State {
            parse: lexer::Parse::To(TillToken::Minus),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('>'), // Lexeme will be: ->
                    to: lexer::Dest::To(TillKey::Other)
                }
            ]
        }
    );

    /* EQUALS */

    states.insert(
        TillKey::Equals,
        lexer::State {
            parse: lexer::Parse::To(TillToken::Equals),
            transitions: vec![
                lexer::Transition {
                    match_by: lexer::Match::ByChar('='), // Lexeme will be: ==
                    to: lexer::Dest::To(TillKey::Other)
                }
            ]
        }
    );


    /* OTHER TOKENS */

    states.insert(
        TillKey::Other,
        lexer::State {
            parse: lexer::Parse::ByFunction(&|lexeme| {
                match lexeme {
                    "->" => TillToken::Arrow,
                    "==" => TillToken::DoubleEquals,

                    "(" => TillToken::BracketOpen,
                    ")" => TillToken::BracketClose,
                    "[" => TillToken::BracketSquareOpen,
                    "]" => TillToken::BracketSquareClose,
                    ">" => TillToken::GreaterThan,
                    "<" => TillToken::LessThan,
                    "," => TillToken::Comma,
                    "+" => TillToken::Plus,
                    "/" => TillToken::Slash,
                    "*" => TillToken::Star,
                    "^" => TillToken::Caret,
                    "!" => TillToken::ExclaimationMark,
                    "~" => TillToken::Tilde,
                    _ => panic!()
                }
            }),
            transitions: vec![]
        }
    );

    let ignore = vec![' ']; // Spaces can be ignored when in the initial state.

    lexer::Lexer::new(states, TillKey::Initial, ignore)
}

fn match_digit(c: &char) -> bool { c.is_digit(10) }
fn match_alphanumeric_or_underscore(c: &char) -> bool { c.is_ascii_alphanumeric() || *c == '_' }
fn parse_number_literal(s: &str) -> TillToken { TillToken::NumberLiteral(s.parse().unwrap()) }

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

    // Define some helper methods to make tests easier to write. These methods on
    // TillLexTokenIterator will only exist in test builds of the program.
    impl TillLexTokenIterator<'_> {
        fn assert_next(&mut self, expected_tok: TillToken) -> &mut Self {
            if let Some(Ok(lexer::LexToken(tok, ..))) = self.next() {
                assert_eq!(tok, expected_tok);
            }
            else { panic!("Expected Ok(LexToken(..))"); }
            self
        }

        fn assert_unexpected_char_next(&mut self, expected_chr: char) -> &mut Self {
            let next = self.next().unwrap();
            
            if let Err(lexer::LexFailure::UnexpectedChar(chr, ..)) = next {
                assert_eq!(chr, expected_chr);
            }
            else { panic!("Expected Err(LexFailure::UnexpectedChar(..)) - not {:?}", next); }
            
            self
        }

        fn assert_unexpected_eof_next(&mut self) -> &mut Self {
            let next = self.next().unwrap();

            if let Err(lexer::LexFailure::UnexpectedEof(..)) = next {}
            else { panic!("Expected Err(LexFailure::UnexpectedEof(..)) - not {:?}", next); }
    
            self
        }
    
        fn assert_end_of_stream(&mut self) {
            assert_eq!(self.next(), None);
        }
    }

    #[test]
    fn test_ignored_characters() {
        new_till_lexer().input(Stream::from_str("  5 6.2   "))
        .assert_next(TillToken::NumberLiteral(5.0))
        .assert_next(TillToken::NumberLiteral(6.2))
        .assert_end_of_stream();
    }

    #[test]
    fn test_number_literals() {
        new_till_lexer().input(Stream::from_str("12.3 12."))
        .assert_next(TillToken::NumberLiteral(12.3))
        .assert_unexpected_eof_next();
    }

    #[test]
    fn test_identifiers() {
        new_till_lexer().input(Stream::from_str("someTHIng _with5and6   Type Nice1_"))
        .assert_next(TillToken::Identifier("someTHIng".to_string()))
        .assert_next(TillToken::Identifier("_with5and6".to_string()))
        .assert_next(TillToken::TypeIdentifier("Type".to_string()))
        .assert_next(TillToken::TypeIdentifier("Nice1_".to_string()));
    }

    #[test]
    fn test_keywords() {
        new_till_lexer().input(Stream::from_str("if else  while  true false"))
        .assert_next(TillToken::IfKeyword)
        .assert_next(TillToken::ElseKeyword)
        .assert_next(TillToken::WhileKeyword)
        .assert_next(TillToken::TrueKeyword)
        .assert_next(TillToken::FalseKeyword);
    }

    #[test]
    fn test_indentation() {
        new_till_lexer().input(Stream::from_str("0\n\t1\n\t\t2\n0   \n\t\t\n\t"))
        .assert_next(TillToken::NumberLiteral(0.0))
        .assert_next(TillToken::Newline(1))
        .assert_next(TillToken::NumberLiteral(1.0))
        .assert_next(TillToken::Newline(2))
        .assert_next(TillToken::NumberLiteral(2.0))
        .assert_next(TillToken::Newline(0))
        .assert_next(TillToken::NumberLiteral(0.0))

        .assert_next(TillToken::Newline(1))
        .assert_end_of_stream();
    }

    #[test]
    fn test_string_literals() {
        new_till_lexer().input(Stream::from_str("\"\" \"hello\\tworld\" \"世界\" \"\\n\\t\\\"\\\\\" \"not terminated..."))
        .assert_next(TillToken::StringLiteral("".to_string()))
        .assert_next(TillToken::StringLiteral("hello\tworld".to_string()))
        .assert_next(TillToken::StringLiteral("世界".to_string()))
        .assert_next(TillToken::StringLiteral("\n\t\"\\".to_string()))
        .assert_unexpected_eof_next();
    }

    #[test]
    fn test_char_literals() {
        new_till_lexer().input(Stream::from_str("'' 'a' 'わ' '\\'' '\\n'"))
        .assert_next(TillToken::CharLiteral('\0'))
        .assert_next(TillToken::CharLiteral('a'))
        .assert_next(TillToken::CharLiteral('わ'))
        .assert_next(TillToken::CharLiteral('\''))
        .assert_next(TillToken::CharLiteral('\n'));
    }

    #[test]
    fn test_minus_and_arrow() {
        new_till_lexer().input(Stream::from_str("- ->"))
        .assert_next(TillToken::Minus)
        .assert_next(TillToken::Arrow);
    }

    #[test]
    fn test_equals_and_double_equals() {
        new_till_lexer().input(Stream::from_str("= =="))
        .assert_next(TillToken::Equals)
        .assert_next(TillToken::DoubleEquals);
    }

    #[test]
    fn test_other_tokens() {
        new_till_lexer().input(Stream::from_str("() [] > < , + / * ^ ! ~"))
        .assert_next(TillToken::BracketOpen).assert_next(TillToken::BracketClose)
        .assert_next(TillToken::BracketSquareOpen).assert_next(TillToken::BracketSquareClose)
        .assert_next(TillToken::GreaterThan)
        .assert_next(TillToken::LessThan)
        .assert_next(TillToken::Comma)
        .assert_next(TillToken::Plus)
        .assert_next(TillToken::Slash)
        .assert_next(TillToken::Star)
        .assert_next(TillToken::Caret)
        .assert_next(TillToken::ExclaimationMark)
        .assert_next(TillToken::Tilde);
    }
    
    #[test]
    fn test_lexing_errors() {
        new_till_lexer().input(Stream::from_str("10.a 10."))
        .assert_unexpected_char_next('a')
        .assert_next(TillToken::Identifier("a".to_string()))
        .assert_unexpected_eof_next();
    }
}