use std::{ io, fs, collections::HashMap };

pub enum Dest<Key> {
    ToSelf, To(Key)
}

pub struct Transition<Key> {
    pub match_fn: fn(&char) -> bool,
    pub to: Dest<Key>
}

pub struct State<Key, Token> {
    pub parse_fn: Option<fn(&str) -> Token>,
    pub transitions: Vec<Transition<Key>>
}

pub type States<Key, Token> = HashMap<Key, State<Key, Token>>;

pub struct Lexer<'a, Key, Token> {
    reader: Option<Box<dyn io::Read>>,
    states: &'a States<Key, Token>,
    current_state_key: Key
}

impl<Key, Token> Lexer<'_, Key, Token> {
    pub fn new(states: &States<Key, Token>, initial_state_key: Key) -> Lexer<Key, Token> {
        Lexer {
            reader: None,
            states,
            current_state_key: initial_state_key
        }
    }

    pub fn set_reader(&mut self, reader: Box<dyn io::Read>) {
        self.reader = Some(reader);
    }

    pub fn set_reader_by_file(&mut self, f: fs::File) {
        let buf_reader = io::BufReader::new(f);
        self.reader = Some(Box::new(buf_reader));
    }

    fn next_char(&mut self) -> Option<char> {
        match &mut self.reader {
            Some(reader) => {
                let mut buf = [0];
                reader.read(&mut buf).ok()?;
                
                let c = buf[0] as char;
                if c != '\u{0}' { Some(c) } else { None }
            },
            None => None
        }
    }
}

/*impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {}
}*/

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(PartialEq, Eq, Hash)]
    enum Key { First, Second }
    enum Token { This, That }

    #[test]
    fn test_streaming() {
        let states: States<Key, Token> = HashMap::new();
        let mut lex = Lexer::new(&states, Key::First);
        lex.set_reader(Box::new("xy".as_bytes()));
        
        assert_eq!(lex.next_char(), Some('x'));
        assert_eq!(lex.next_char(), Some('y'));
        assert_eq!(lex.next_char(), None);
    }
}