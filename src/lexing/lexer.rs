use std::{ io, fs, collections::HashMap, hash::Hash, fmt::Debug };

/// Describes a lexing state. Can include any number of transitions to other
/// states. When the lexer finds no appropriate transitions from this state,
/// the specified parsing function (should there be one) is called in order to
/// convert the lexeme to a token.
/// Should the lexer find itself on a state with no parsing function that it is
/// unable to transition off from, it is evident that the input stream is invalid
/// for the given lexer states and a lexical error has occured.
pub struct State<'a, Key, Token> {
    pub parse: Parse<'a, Token>,
    pub transitions: Vec<Transition<'a, Key>>
}

pub enum Parse<'a, Token> {
    To(Token), // For tokens that require no data from the lexeme (e.g. `IfKeyword`).
    ByFunction(&'a dyn Fn(&str) -> Token), // For tokens with data extracted from lexeme (e.g. `NumberLiteral`, `Identifier`).
    Invalid // For transitional states that do not produce a token (e.g. `PotentialReal`).
}

/// Describes a transition from one state to another (or itself). The lexer
/// decides whether this transition can be followed by calling that transition's
/// matching function with the character most recently read from the stream.
/// Should the matching funtion return turn true, the lexer will transition
/// states as specified.
pub struct Transition<'a, Key> {
    pub match_by: Match<'a>,
    pub to: Dest<Key>
}

pub enum Match<'a> {
    ByChar(char),
    ByFunction(&'a dyn Fn(&char) -> bool)
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

impl<Key, Token> Lexer<'_, Key, Token>
where Key: Copy + Eq + Hash + Debug {
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

    /// Indicate character(s) which, given no valid transition is found, can be
    /// skipped by the lexer.
    pub fn ignore_match(&mut self) {}

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

    fn get_state(&self, key: Key) -> &State<Key, Token> {
        self.states.get(&key).expect(&format!("Lexer has no state defined for key: {:?}", key))
    }
}

impl<Key, Token> Iterator for Lexer<'_, Key, Token>
where Key: Copy + Eq + Hash + Debug,
      Token: Clone + Debug {
    type Item = LexResult<Token>;

    /// Return the next token and lexeme in the current input stream.
    /// Returns `None` should the end of the current input stream have been
    /// reached.
    fn next(&mut self) -> Option<Self::Item> {
        let mut current_key = self.initial_state_key;
        let mut lexeme = String::new(); // each char read from stream is appended to this string
        
        while let Some(chr) = self.next_char() {
            let state = self.get_state(current_key);
            println!("Current state: {:?}", current_key);

            let transition_attempt = attempt_state_transition(current_key, &state.transitions, chr);

            if let Some(new_key) = transition_attempt {
                println!("State transition made - continuing...");
                current_key = new_key; // change state
            }
            else {
                println!("State transition could not be made - attempting to parse lexeme...");
                return Some(attempt_token_parse(lexeme, state));
            }

            lexeme.push(chr);
            println!("Current lexeme: {}", lexeme);
        }

        if !lexeme.is_empty() {
            println!("Reached end of input stream - attempting to parse lexeme...");
            Some(attempt_token_parse(lexeme, self.get_state(current_key)))
        }
        else { None }
    }
}

/// Attempt to transition state given a vector of transitions and the current
/// input character. Will return `Some` holding the next state key should an
/// appropriate transition be found (whether to the current state or elsewhere).
/// `None` is returned when no appropriate transitions could be found.
fn attempt_state_transition<Key>(current_key: Key, transitions : &Vec<Transition<Key>>, chr: char) -> Option<Key>
where Key: Copy + Debug {
    for transition in transitions {
        let should_transition = match transition.match_by {
            Match::ByChar(expected) => { chr == expected }
            Match::ByFunction(func) => { func(&chr) }
        };

        if should_transition {
            println!("Found appropriate transition for character: {}", chr);

            return match &transition.to {
                Dest::To(new_key) => {
                    println!("Transitioning state from {:?} to {:?}...", current_key, new_key);
                    Some(*new_key) // To new state...
                }

                Dest::ToSelf => {
                    println!("Remaining in current state {:?}...", current_key);
                    Some(current_key) // To same state...
                }
            }
        }
    }

    println!("No transitions found for character: {}", chr);
    None
}

/// Attempt to convert a lexeme into a token, assuming a given lexeme and final
/// lexer state (no more possible transitions could be made or reached end of
/// input stream).
fn attempt_token_parse<Key, Token>(lexeme: String, final_state: &State<Key, Token>) -> LexResult<Token>
where Token: Clone + Debug{
    let potential_tok = match &final_state.parse {
        Parse::To(t) => { Some(t.clone()) }
        Parse::ByFunction(func) => { Some(func(&lexeme)) }
        Parse::Invalid => { None }
    };

    match potential_tok {
        Some(tok) => {
            println!("Lexeme parsed to token: {:?}\n", tok);
            LexResult::Success(tok, lexeme)
        }
        None => {
            println!("Could not parse to token from lexeme: {}\n", lexeme);
            LexResult::Failure(lexeme)
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