use crate::stream::Stream;
use crate::lexing::lexer::Lexer;

pub struct Parser<'a, Key, Token> {
    lxr: Lexer<'a, Key, Token>
}

impl<'a, Key, Token> Parser<'a, Key, Token> {
    pub fn new(lxr: Lexer<'a, Key, Token>) -> Parser<'a, Key, Token> {
        Parser { lxr }
    }

    pub fn input(&'a self, strm: Stream) -> ParseIterator<'a, Key, Token> {
        ParseIterator {
            prsr: self,
            strm
        }
    }
}

pub struct ParseIterator<'a, Key, Token> {
    prsr: &'a Parser<'a, Key, Token>,
    strm: Stream
}

// TODO: impl Iterator for ParseIterator