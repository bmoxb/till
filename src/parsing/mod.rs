pub mod ast;

use crate::lexing;

/// Returns an iterator that yields abstract syntax representations for each
/// TILL statement parsed from the given token stream.
pub fn input<'a>(lex_iterator: lexing::TillLexIterator<'a>) -> ParseIterator<'a> {
    ParseIterator { lex_iterator }
}

pub struct ParseIterator<'a> {
    lex_iterator: lexing::TillLexIterator<'a>
}

impl Iterator for ParseIterator<'_> {
    type Item = ast::Stat;

    /// Return the next AST statement parsed from the given token stream.
    fn next(&mut self) -> Option<ast::Stat> {
        None
    }
}