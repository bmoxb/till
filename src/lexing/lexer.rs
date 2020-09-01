use crate::stream;
use std::{ fmt, collections::HashMap };

pub type Token = super::GenericToken<TokenType>;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self.tok_type {
            TokenType::Newline(x) if x > 0 => "indentation level",
            TokenType::Newline(0) => "newline",
            TokenType::Identifier(_) => "identifier",
            TokenType::TypeIdentifier(_) => "type identifier",
            TokenType::NumberLiteral(_) |
            TokenType::StringLiteral(_) |
            TokenType::CharLiteral(_) => "literal",
            TokenType::IfKeyword |
            TokenType::WhileKeyword |
            TokenType::TrueKeyword |
            TokenType::FalseKeyword => "keyword",
            _ => "token"
        };

        write!(f, "{} {}", name, self.lexeme)
    }
}

pub type TokenStream<'a> = super::GenericTokenStream<'a, TokenType, StateKey>;

/// All lexing tokens yielded by the TILL lexer.
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Newline(usize), // Value is indentation level of the new line.

    Identifier(String),
    TypeIdentifier(String),

    NumberLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),

    IfKeyword, // if
    WhileKeyword, // while
    TrueKeyword, // true
    FalseKeyword, // false
    ReturnKeyword, // return

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
pub enum StateKey {
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

pub fn input<'a>(strm: stream::Stream) -> TokenStream<'a> {
    super::GenericTokenStream {
        strm,
        settings: &TILL_SETTINGS
    }
}

lazy_static::lazy_static! {
    static ref TILL_SETTINGS: super::LexerSettings<'static, TokenType, StateKey> = {
        let mut states = HashMap::new();

        /* INITIAL STATE */

        states.insert(
            StateKey::Initial,
            super::State {
                parse: super::Parse::Invalid,
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByFunction(&match_digit),
                        to: super::Dest::To(StateKey::Integer)
                    },
                    super::Transition {
                        match_by: super::Match::ByFunction(&|c| c.is_ascii_lowercase() || *c == '_'),
                        to: super::Dest::To(StateKey::IdentifierOrKeyword)
                    },
                    super::Transition {
                        match_by: super::Match::ByFunction(&|c| c.is_ascii_uppercase()),
                        to: super::Dest::To(StateKey::TypeIdentifier)
                    },
                    super::Transition {
                        match_by: super::Match::ByChar('\n'),
                        to: super::Dest::To(StateKey::Newline)
                    },
                    super::Transition {
                        match_by: super::Match::ByChar('"'),
                        to: super::Dest::To(StateKey::PotentialString)
                    },
                    super::Transition {
                        match_by: super::Match::ByChar('\''),
                        to: super::Dest::To(StateKey::BeginChar)
                    },
                    super::Transition {
                        match_by: super::Match::ByChar('-'),
                        to: super::Dest::To(StateKey::Minus)
                    },
                    super::Transition {
                        match_by: super::Match::ByChar('='),
                        to: super::Dest::To(StateKey::Equals)
                    },
                    super::Transition {
                        match_by: super::Match::ByChars(vec!['(', ')', '[', ']', '>', '<', ',', '+', '/', '*', '^', '!', '~']),
                        to: super::Dest::To(StateKey::Other)
                    }
                ]
            }
        );

        /* NUMBER LITERALS */

        states.insert(
            StateKey::Integer,
            super::State {
                parse: super::Parse::ByFunction(&parse_number_literal),
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByChar('.'),
                        to: super::Dest::To(StateKey::PotentialReal)
                    },
                    super::Transition {
                        match_by: super::Match::ByFunction(&match_digit),
                        to: super::Dest::ToSelf
                    }
                ]
            }
        );

        states.insert(
            StateKey::PotentialReal,
            super::State {
                parse: super::Parse::Invalid, // Digit(s), decimal point, without further digit(s) is invalid.
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByFunction(&match_digit),
                        to: super::Dest::To(StateKey::Real)
                    }
                ]
            }
        );

        states.insert(
            StateKey::Real,
            super::State {
                parse: super::Parse::ByFunction(&parse_number_literal),
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByFunction(&match_digit),
                        to: super::Dest::ToSelf
                    }
                ]
            }
        );

        /* KEYWORDS, IDENTIFIERS & TYPE IDENTIFIERS */

        states.insert(
            StateKey::IdentifierOrKeyword,
            super::State {
                parse: super::Parse::ByFunction(&|lexeme| {
                    match lexeme {
                        "if" => TokenType::IfKeyword,
                        "while" => TokenType::WhileKeyword,
                        "true" => TokenType::TrueKeyword,
                        "false" => TokenType::FalseKeyword,
                        "return" => TokenType::ReturnKeyword,
                        x => TokenType::Identifier(x.to_string())
                    }
                }),
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByFunction(&match_alphanumeric_or_underscore),
                        to: super::Dest::ToSelf
                    }
                ]
            }
        );

        states.insert(
            StateKey::TypeIdentifier,
            super::State {
                parse: super::Parse::ByFunction(&|lexeme| {
                    TokenType::TypeIdentifier(lexeme.to_string())
                }),
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByFunction(&match_alphanumeric_or_underscore),
                        to: super::Dest::ToSelf
                    }
                ]
            }
        );
        
        /* NEWLINES & INDENTATION */

        states.insert(
            StateKey::Newline,
            super::State {
                parse: super::Parse::ByFunction(&|lexeme| {
                    let line = lexeme.split("\n").last().unwrap(); // Ignore any empty lines, only consider final populated line.
                    TokenType::Newline(line.matches("\t").count())
                }),
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByChars(vec!['\n', '\t']),
                        to: super::Dest::ToSelf
                    }
                ]
            }
        );

        /* STRING LITERALS */

        states.insert(
            StateKey::PotentialString,
            super::State {
                parse: super::Parse::Invalid,
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByChar('\\'),
                        to: super::Dest::To(StateKey::StringEscapeSequence)
                    },
                    super::Transition {
                        match_by: super::Match::ByChar('"'),
                        to: super::Dest::To(StateKey::StringLiteral)
                    },
                    super::Transition {
                        match_by: super::Match::Any, // I.e. not \ or " character
                        to: super::Dest::ToSelf
                    }
                ]
            }
        );

        states.insert(
            StateKey::StringEscapeSequence,
            super::State {
                parse: super::Parse::Invalid,
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByChars(vec!['n', 't', '\\', '"']),
                        to: super::Dest::To(StateKey::PotentialString)
                    }
                ]
            }
        );

        states.insert(
            StateKey::StringLiteral,
            super::State {
                parse: super::Parse::ByFunction(&|lexeme| {
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

                    TokenType::StringLiteral(literal)
                }),
                transitions: vec![]
            }
        );

        /* CHARACTER LITERALS */

        states.insert(
            StateKey::BeginChar,
            super::State {
                parse: super::Parse::Invalid,
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByChar('\''),
                        to: super::Dest::To(StateKey::CharLiteral)
                    },
                    super::Transition {
                        match_by: super::Match::ByChar('\\'),
                        to: super::Dest::To(StateKey::CharEscapeSequence)
                    },
                    super::Transition {
                        match_by: super::Match::Any,
                        to: super::Dest::To(StateKey::CharEnd)
                    }
                ]
            }
        );

        states.insert(
            StateKey::CharEnd,
            super::State {
                parse: super::Parse::Invalid,
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByChar('\''),
                        to: super::Dest::To(StateKey::CharLiteral)
                    }
                ]
            }
        );

        states.insert(
            StateKey::CharEscapeSequence,
            super::State {
                parse: super::Parse::Invalid,
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByChars(vec!['n', 't', '\\', '\'']),
                        to: super::Dest::To(StateKey::CharEnd)
                    }
                ]
            }
        );

        states.insert(
            StateKey::CharLiteral,
            super::State {
                parse: super::Parse::ByFunction(&|lexeme| {
                    TokenType::CharLiteral(
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
            StateKey::Minus,
            super::State {
                parse: super::Parse::To(TokenType::Minus),
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByChar('>'), // Lexeme will be: ->
                        to: super::Dest::To(StateKey::Other)
                    }
                ]
            }
        );

        /* EQUALS */

        states.insert(
            StateKey::Equals,
            super::State {
                parse: super::Parse::To(TokenType::Equals),
                transitions: vec![
                    super::Transition {
                        match_by: super::Match::ByChar('='), // Lexeme will be: ==
                        to: super::Dest::To(StateKey::Other)
                    }
                ]
            }
        );


        /* OTHER TOKENS */

        states.insert(
            StateKey::Other,
            super::State {
                parse: super::Parse::ByFunction(&|lexeme| {
                    match lexeme {
                        "->" => TokenType::Arrow,
                        "==" => TokenType::DoubleEquals,

                        "(" => TokenType::BracketOpen,
                        ")" => TokenType::BracketClose,
                        "[" => TokenType::BracketSquareOpen,
                        "]" => TokenType::BracketSquareClose,
                        ">" => TokenType::GreaterThan,
                        "<" => TokenType::LessThan,
                        "," => TokenType::Comma,
                        "+" => TokenType::Plus,
                        "/" => TokenType::Slash,
                        "*" => TokenType::Star,
                        "^" => TokenType::Caret,
                        "!" => TokenType::ExclaimationMark,
                        "~" => TokenType::Tilde,
                        _ => panic!()
                    }
                }),
                transitions: vec![]
            }
        );

        let initial_state_key = StateKey::Initial;

        let match_ignored = super::Match::ByChar(' ');

        super::LexerSettings { states, initial_state_key, match_ignored }
    };
}

fn match_digit(c: &char) -> bool { c.is_digit(10) }

fn match_alphanumeric_or_underscore(c: &char) -> bool { c.is_ascii_alphanumeric() || *c == '_' }

fn parse_number_literal(s: &str) -> TokenType { TokenType::NumberLiteral(s.parse().unwrap()) }

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
    use super::super::{ GenericToken, Failure };
    use crate::stream::Stream;

    // Define some helper methods to make tests easier to write. These methods on
    // TokenStream will only exist in test builds of the program.
    impl TokenStream<'_> {
        fn assert_next(&mut self, expected_tok_type: TokenType) -> &mut Self {
            if let Some(Ok(GenericToken { tok_type, .. })) = self.next() {
                assert_eq!(tok_type, expected_tok_type);
            }
            else { panic!("Expected Ok(LexToken(..))"); }
            self
        }

        fn assert_unexpected_char_next(&mut self, expected_chr: char) -> &mut Self {
            if let Some(Err(Failure::UnexpectedChar(chr, ..))) = self.next() {
                assert_eq!(chr, expected_chr);
            }
            else { panic!("Expected Err(LexFailure::UnexpectedChar(..))"); }
            
            self
        }

        fn assert_unexpected_eof_next(&mut self) -> &mut Self {
            if let Some(Err(Failure::UnexpectedEof(..))) = self.next() {}
            else { panic!("Expected Err(LexFailure::UnexpectedEof(..))"); }
    
            self
        }
    
        fn assert_end_of_stream(&mut self) {
            assert_eq!(self.next(), None);
        }
    }

    #[test]
    fn ignored_characters() {
        input(Stream::from_str("  5 6.2   "))
        .assert_next(TokenType::NumberLiteral(5.0))
        .assert_next(TokenType::NumberLiteral(6.2))
        .assert_end_of_stream();
    }

    #[test]
    fn number_literals() {
        input(Stream::from_str("12.3 12."))
        .assert_next(TokenType::NumberLiteral(12.3))
        .assert_unexpected_eof_next();
    }

    #[test]
    fn identifiers() {
        input(Stream::from_str("someTHIng _with5and6   Type Nice1_"))
        .assert_next(TokenType::Identifier("someTHIng".to_string()))
        .assert_next(TokenType::Identifier("_with5and6".to_string()))
        .assert_next(TokenType::TypeIdentifier("Type".to_string()))
        .assert_next(TokenType::TypeIdentifier("Nice1_".to_string()));
    }

    #[test]
    fn keywords() {
        input(Stream::from_str("if  while  true false  return"))
        .assert_next(TokenType::IfKeyword)
        .assert_next(TokenType::WhileKeyword)
        .assert_next(TokenType::TrueKeyword)
        .assert_next(TokenType::FalseKeyword)
        .assert_next(TokenType::ReturnKeyword);
    }

    #[test]
    fn indentation() {
        input(Stream::from_str("0\n\t1\n\t\t2\n0   \n\t\t\n\t"))
        .assert_next(TokenType::NumberLiteral(0.0))
        .assert_next(TokenType::Newline(1))
        .assert_next(TokenType::NumberLiteral(1.0))
        .assert_next(TokenType::Newline(2))
        .assert_next(TokenType::NumberLiteral(2.0))
        .assert_next(TokenType::Newline(0))
        .assert_next(TokenType::NumberLiteral(0.0))

        .assert_next(TokenType::Newline(1))
        .assert_end_of_stream();
    }

    #[test]
    fn string_literals() {
        input(Stream::from_str("\"\" \"hello\\tworld\" \"世界\" \"\\n\\t\\\"\\\\\" \"not terminated..."))
        .assert_next(TokenType::StringLiteral("".to_string()))
        .assert_next(TokenType::StringLiteral("hello\tworld".to_string()))
        .assert_next(TokenType::StringLiteral("世界".to_string()))
        .assert_next(TokenType::StringLiteral("\n\t\"\\".to_string()))
        .assert_unexpected_eof_next();
    }

    #[test]
    fn char_literals() {
        input(Stream::from_str("'' 'a' 'わ' '\\'' '\\n'"))
        .assert_next(TokenType::CharLiteral('\0'))
        .assert_next(TokenType::CharLiteral('a'))
        .assert_next(TokenType::CharLiteral('わ'))
        .assert_next(TokenType::CharLiteral('\''))
        .assert_next(TokenType::CharLiteral('\n'));
    }

    #[test]
    fn minus_and_arrow() {
        input(Stream::from_str("- ->"))
        .assert_next(TokenType::Minus)
        .assert_next(TokenType::Arrow);
    }

    #[test]
    fn equals_and_double_equals() {
        input(Stream::from_str("= =="))
        .assert_next(TokenType::Equals)
        .assert_next(TokenType::DoubleEquals);
    }

    #[test]
    fn other_tokens() {
        input(Stream::from_str("() [] > < , + / * ^ ! ~"))
        .assert_next(TokenType::BracketOpen).assert_next(TokenType::BracketClose)
        .assert_next(TokenType::BracketSquareOpen).assert_next(TokenType::BracketSquareClose)
        .assert_next(TokenType::GreaterThan)
        .assert_next(TokenType::LessThan)
        .assert_next(TokenType::Comma)
        .assert_next(TokenType::Plus)
        .assert_next(TokenType::Slash)
        .assert_next(TokenType::Star)
        .assert_next(TokenType::Caret)
        .assert_next(TokenType::ExclaimationMark)
        .assert_next(TokenType::Tilde);
    }
    
    #[test]
    fn lexing_errors() {
        input(Stream::from_str("10.a 10."))
        .assert_unexpected_char_next('a')
        .assert_next(TokenType::Identifier("a".to_string()))
        .assert_unexpected_eof_next();
    }
}