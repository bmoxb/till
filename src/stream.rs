use std::fs;
use char_stream::CharStream;

pub struct Stream {
    char_stream: CharStream,
    position: u64,
    line_number: u64,
    line_position: u64
}

impl Stream {
    pub fn from_str(s: &str) -> Stream {
        Stream {
            char_stream: CharStream::from(s),
            position: 0, line_number: 1, line_position: 0
        }
    }

    pub fn from_file(f: fs::File) -> Stream {
        Stream {
            char_stream: CharStream::from_file(f),
            position: 0, line_number: 1, line_position: 0
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.char_stream.peek()
    }

    pub fn advance(&mut self) {
        if let Some(chr) = self.char_stream.next() {
            self.position += 1;

            if chr == '\n' {
                self.line_number += 1;
                self.line_position = 0;
            }
            else { self.line_position += 1; }
        }
    }

    pub fn get_position(&self) -> u64 { self.position }
    pub fn get_line_number(&self) -> u64 { self.line_number }
    pub fn get_line_position(&self) -> u64 { self.line_position }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_position_tracking() {
        let mut s = super::Stream::from_str("a\nb");
        
        s.advance();
        assert_eq!(s.position, 1);
        assert_eq!(s.line_number, 1);
        assert_eq!(s.line_position, 1);

        s.advance();
        assert_eq!(s.position, 2);
        assert_eq!(s.line_number, 2);
        assert_eq!(s.line_position, 0);

        s.advance();
        assert_eq!(s.position, 3);
        assert_eq!(s.line_number, 2);
        assert_eq!(s.line_position, 1);
    }
}