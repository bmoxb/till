use std::{ io, fs, collections::HashMap, hash::Hash, fmt::Debug };

/// Describes a lexing state. Can include any number of transitions to other
/// states. When the lexer finds no appropriate transitions from this state,
/// the specified parsing function (should there be one) is called in order to
/// convert the lexeme to a token.
/// Should the lexer find itself on a state with no parsing function that it is
/// unable to transition off from, it is evident that the input stream is invalid
/// for the given lexer states and a lexical error has occured.
pub struct State<'a, Key, Token> {
    pub parse_fn: Option<&'a dyn Fn(&str) -> Token>,
    pub transitions: Vec<Transition<'a, Key>>
}

/// Describes a transition from one state to another (or itself). The lexer
/// decides whether this transition can be followed by calling that transition's
/// matching function with the character most recently read from the stream.
/// Should the matching funtion return turn true, the lexer will transition
/// states as specified.
pub struct Transition<'a, Key> {
    pub match_fn: &'a dyn Fn(&char) -> bool,
    pub to: Dest<Key>
}

/// Indicates how the lexer should transition state - either to remain on the
/// current state or to transition to a state with a given key.
pub enum Dest<Key> {
    ToSelf, To(Key)
}

/// Type allias for a hash map of state keys to states.
pub type States<'a, Key, Token> = HashMap<Key, State<'a, Key, Token>>;

/// Indicates the result of attempting to find the next token - either success
/// (includes the token and the valid lexeme) or failure (just the invalid lexeme
/// and obviously no token).
#[derive(Debug, PartialEq)]
pub enum LexResult<Token> {
    Success(Token, String),
    Failure(String)
}

pub struct Lexer<'a, Key: Copy, Token> {
    reader: Option<Box<dyn io::Read>>,
    states: States<'a, Key, Token>,
    initial_state_key: Key
}

impl<Key: Copy, Token> Lexer<'_, Key, Token> {
    /// Create a new lexer with it's own unique set of states.
    pub fn new(states: States<Key, Token>, initial_state_key: Key) -> Lexer<Key, Token> {
        Lexer {
            reader: None,
            states,
            initial_state_key
        }
    }

    /// Set the input stream that the lexer will read from.
    pub fn input_reader(&mut self, reader: Box<dyn io::Read>) {
        self.reader = Some(reader);
    }

    /// Create a buffered reader using a given file and set it is as the lexer
    /// input stream.
    pub fn input_file(&mut self, f: fs::File) {
        let buf_reader = io::BufReader::new(f);
        self.input_reader(Box::new(buf_reader));
    }

    /// Convert a given static &str to bytes to be used as the input stream.
    /// This is to be used primarily in tests.
    pub fn input_str(&mut self, s: &'static str) {
        self.input_reader(Box::new(s.as_bytes()));
    }

    /// Read the next character from the current input stream. Will return None
    /// should no input stream by set or if the end of that stream has been
    /// reached or cannot be read from.
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
    type Item = LexResult<Token>;

    /// Return the next token and lexeme in the current input stream.
    fn next(&mut self) -> Option<Self::Item> {
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
                        break; // If a match is found for this transition, there's
                               // reason to check remaining transitions also.
                    }
                }
            }
            else { panic!("Transitioned to undefined state key: {:?}", current_key); }
        }

        if let Some(parse_fn) = self.states.get(&current_key).unwrap().parse_fn {
            let tok = parse_fn(&lexeme);
            println!("Lexeme parsed to token: {:?}", tok);
            Some(LexResult::Success(tok, lexeme))
        }
        else {
            if !lexeme.is_empty() { 
                println!("Finished in state which cannot be exited from!");
                Some(LexResult::Failure(lexeme))
            }
            else {
                println!("Reached end of current input stream.");
                None
            }
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
        lex.input_str("xy");
        
        assert_eq!(lex.next_char(), Some('x'));
        assert_eq!(lex.next_char(), Some('y'));
        assert_eq!(lex.next_char(), None);
    }
}