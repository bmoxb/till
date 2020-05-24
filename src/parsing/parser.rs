use crate::lexing::lexer;

/// Returns an iterator that yields abstract syntax representations for each
/// TILL statement parsed from the given token stream.
pub fn input(tokens: Vec<lexer::Token>) -> StatementStream {
    StatementStream { tokens }
}

pub struct StatementStream {
    tokens: Vec<lexer::Token>
}

impl Iterator for StatementStream {
    type Item = super::Statement;

    /// Return the next AST statement parsed from the given token stream.
    fn next(&mut self) -> Option<Self::Item> { None }
}