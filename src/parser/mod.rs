mod ast;

use super::lexing::lexer;

pub struct Parser<'a, Key, Token> {
    lex_iterator: lexer::LexIterator<'a, Key, Token>
}

impl<Key, Token> Parser<'_, Key, Token> {
    pub fn new(lex_iterator: lexer::LexIterator<'_, Key, Token>) -> Parser<'_, Key, Token> {
        Parser { lex_iterator }
    }
}

impl<Key, Token> Iterator for Parser<'_, Key, Token> {
    type Item = ast::Stat;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}