use std::{ io, fs };

pub enum Token {
    // Keywords:
    IfKeyword,
    ElseKeyword,
    // Whitespace:
    Newlines,
    IndentIncr,
    IndentDecr,
    // Brackets:
    BracketOpen,
    BracketClose,
    BracketSquareOpen,
    BracketSquareClose,
    // Identifiers:
    Identifier,
    TypeIdentifier,
    // Literals:
    NumberLiteral,
    StringLiteral,
    // Other:
    Arrow,
    Comma,
    Equals
}

pub struct Lexer {
    reader: Box<dyn io::Read>
}

impl Lexer {
    fn from_stream(reader: Box<dyn io::Read>) -> Lexer {
        Lexer { reader }
    }

    fn from_file(f: fs::File) -> Lexer {
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