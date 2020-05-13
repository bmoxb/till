use std::{ io, fs, collections::HashMap, hash::Hash, fmt::Debug };

pub enum Dest<Key> {
    ToSelf, To(Key)
}

pub struct Transition<'a, Key> {
    pub match_fn: &'a dyn Fn(&char) -> bool,
    pub to: Dest<Key>
}

pub struct State<'a, Key, Token> {
    pub parse_fn: Option<&'a dyn Fn(&str) -> Token>,
    pub transitions: Vec<Transition<'a, Key>>
}

pub type States<'a, Key, Token> = HashMap<Key, State<'a, Key, Token>>;

pub struct Lexer<'a, Key: Copy, Token> {
    reader: Option<Box<dyn io::Read>>,
    states: States<'a, Key, Token>,
    initial_state_key: Key
}

impl<Key: Copy, Token> Lexer<'_, Key, Token> {
    pub fn new(states: States<Key, Token>, initial_state_key: Key) -> Lexer<Key, Token> {
        Lexer {
            reader: None,
            states,
            initial_state_key
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

impl<Key: Copy + Eq + Hash + Debug, Token: Debug> Iterator for Lexer<'_, Key, Token> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let mut current_key = self.initial_state_key;
        let mut lexeme = String::new();

        while let Some(c) = self.next_char() {
            lexeme.push(c);
            println!("Current lexeme: {}", lexeme);

            if let Some(state) = self.states.get(&current_key) {
                println!("Current state: {:?}", current_key);

                for transition in &state.transitions {
                    if (transition.match_fn)(&c) {
                        println!("Found appropriate transition for character: {}", c);

                        match transition.to {
                            Dest::To(new_key) => {
                                println!("Transitioning state from {:?} to {:?}...", current_key, new_key);
                                current_key = new_key;
                            }

                            Dest::ToSelf => { println!("Remaining in current state {:?}...", current_key); }
                        }
                        break;
                    }
                }
            }
            else { panic!("Transitioned to undefined state key: {:?}", current_key); }
        }

        if let Some(parse_fn) = self.states.get(&current_key).unwrap().parse_fn {
            let tok = parse_fn(&lexeme);
            println!("Lexeme parsed to token: {:?}", tok);
            Some(tok)
        }
        else {
            println!("Finished in state which cannot be exited from!");
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
    enum Key { Initial }
    enum Token {}

    #[test]
    fn test_streaming() {
        let mut lex = Lexer::<Key, Token>::new(HashMap::new(), Key::Initial);
        lex.set_reader(Box::new("xy".as_bytes()));
        
        assert_eq!(lex.next_char(), Some('x'));
        assert_eq!(lex.next_char(), Some('y'));
        assert_eq!(lex.next_char(), None);
    }
}