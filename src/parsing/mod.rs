mod parser;

use crate::lexing;

/// Static `parser::Parser` type that is specific to the TILL language (i.e. set
/// up to use the TILL lexer).
/// Use the `new_parser` function *once* to create an instance of this type (will
/// use `lexing::new_lexer` function to automatically create an appropriate lexer
/// to be used by the new parser).
pub type TillParser = parser::Parser<'static, lexing::Key, lexing::Token>;

pub fn new_parser() -> TillParser {
    let lxr = lexing::new_lexer();
    log::info!("Using newly-made lexer instance to create parser instance...");
    parser::Parser::new(lxr)
}

mod ast {
    pub enum Stat {
        // ...
    }
    
    pub enum Expr {
        // ...
    }
}