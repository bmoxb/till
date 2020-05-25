use std::{ fs, fmt };
use char_stream::CharStream;

#[derive(Clone, Debug, PartialEq)]
pub struct Position {
    pub position: u64,
    pub line_number: u64,
    pub line_position: u64
}

impl Position {
    pub fn new() -> Position {
        Position { position: 0, line_number: 1, line_position: 0 }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "character {} of line {}", self.line_position, self.line_number)
    }
}

pub struct Stream {
    char_stream: CharStream,
    pos: Position
}

impl Stream {
    pub fn from_str(s: &str) -> Stream {
        Stream {
            char_stream: CharStream::from(s),
            pos: Position::new()
        }
    }

    pub fn from_file(f: fs::File) -> Stream {
        Stream {
            char_stream: CharStream::from_file(f),
            pos: Position::new()
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.char_stream.peek()
    }

    pub fn advance(&mut self) -> &Position {
        if let Some(chr) = self.char_stream.next() {
            self.pos.position += 1;

            if chr == '\n' {
                self.pos.line_number += 1;
                self.pos.line_position = 0;
            }
            else { self.pos.line_position += 1; }
        }

        &self.pos
    }

    pub fn get_pos(&self) -> &Position { &self.pos }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_position_tracking() {
        let mut s = super::Stream::from_str("a\nb");
        
        let mut pos = s.advance();
        assert_eq!(pos.position, 1);
        assert_eq!(pos.line_number, 1);
        assert_eq!(pos.line_position, 1);

        pos = s.advance();
        assert_eq!(pos.position, 2);
        assert_eq!(pos.line_number, 2);
        assert_eq!(pos.line_position, 0);

        pos = s.advance();
        assert_eq!(pos.position, 3);
        assert_eq!(pos.line_number, 2);
        assert_eq!(pos.line_position, 1);
    }
}