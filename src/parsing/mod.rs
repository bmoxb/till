pub mod ast;

/// Returns an iterator that yields abstract syntax representations for each
/// TILL statement parsed from the given token stream.
pub fn input(/*token_iter: ...*/) -> ParseIterator {
    ParseIterator { /*token_iter*/ }
}

pub struct ParseIterator {
    //token_iter: ...
}

impl ParseIterator {
    //fn match(&mut self, tokens: &[lexing::TillToken]) -> bool {}
}

impl Iterator for ParseIterator {
    type Item = ast::Stat;

    /// Return the next AST statement parsed from the given token stream.
    fn next(&mut self) -> Option<ast::Stat> { None }
}