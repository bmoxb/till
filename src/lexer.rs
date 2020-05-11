use std::{ io, fs };
use regex::Regex;
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
    NumberLiteral(f32),
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

enum Match<'a> {
    ByChar(char),
    ByRegex(&'a Regex)
}

enum Transition<'a> {
    ToSelf(Match<'a>),
    ToElsewhere(Match<'a>, State<'a>)
}

struct State<'a> {
    transitions: Vec<Transition<'a>>,
    parse: Option<fn(&str) -> Token>
}

fn number_literal_parse(lexeme: &str) -> Token {
    Token::NumberLiteral(lexeme.parse().unwrap())
}

fn string_literal_parse(lexeme: &str) -> Token {
    Token::StringLiteral(lexeme.to_owned()) // TODO: Handle escape sequences, remove opening & closing quotes
}

lazy_static! {
    static ref DIGIT_REGEX: Regex = Regex::new(r"[0-9]").unwrap();

    static ref STATES: State<'static> = State { // INITIAL STATE
        parse: None,
        transitions: vec![
            Transition::ToElsewhere(
                Match::ByRegex(&DIGIT_REGEX),
                State { // INTEGER STATE
                    parse: Some(number_literal_parse),
                    transitions: vec![
                        Transition::ToSelf(Match::ByRegex(&DIGIT_REGEX)),
                        Transition::ToElsewhere(
                            Match::ByChar('.'),
                            State { // POTENTIAL FLOAT STATE
                                parse: None, // do not accept digit(s), decimal point, but no following digit(s)
                                transitions: vec![
                                    Transition::ToElsewhere(
                                        Match::ByRegex(&DIGIT_REGEX),
                                        State { // FLOAT STATE
                                            parse: Some(number_literal_parse),
                                            transitions: vec![Transition::ToSelf(Match::ByRegex(&DIGIT_REGEX))]
                                        }
                                    )
                                ]
                            }
                        )
                    ]
                }
            )
        ]
    };
}

pub struct Lexer {
    reader: Box<dyn io::Read>
}

impl Lexer {
    pub fn from_stream(reader: Box<dyn io::Read>) -> Lexer {
        Lexer { reader }
    }

    pub fn from_file(f: fs::File) -> Lexer {
        let buf_reader = io::BufReader::new(f);
        Lexer::from_stream(Box::new(buf_reader))
    }

    fn next_char(&mut self) -> Option<char> {
        let mut buf = [0];
        self.reader.read(&mut buf).ok()?;
        
        let c = buf[0] as char;
        if c != '\u{0}' { Some(c) } else { None }
    }
}

/*impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {}
}*/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_streaming() {
        let mut lex = Lexer::from_stream(Box::new("xy".as_bytes()));
        assert_eq!(lex.next_char(), Some('x'));
        assert_eq!(lex.next_char(), Some('y'));
        assert_eq!(lex.next_char(), None);
    }
}