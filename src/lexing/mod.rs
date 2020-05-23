pub mod lexer;

use crate::stream;
use std::{ fmt, hash::Hash, collections::HashMap };

/// Represents a token holding a token type and a lexeme.
#[derive(Debug, PartialEq)]
pub struct GenericToken<TokenType> {
    tok_type: TokenType,
    lexeme: Lexeme
}

/// Holds the raw lexeme string, as well as the position in the input stream the
/// lexeme is from.
#[derive(Debug, PartialEq)]
pub struct Lexeme {
    text: String,
    pos: stream::Position
}

impl fmt::Display for Lexeme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} at {:?}", self.text, self.pos)
    }
}

/// Represents the two type of lexical analysis errors: the encountering of an
/// unexpected character, and the reaching of the end of an input stream when it
/// is not expected.
#[derive(Debug, PartialEq)]
pub enum Failure {
    UnexpectedChar(char, Lexeme),
    UnexpectedEof(Lexeme)
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Failure::UnexpectedChar(unexpected_char, lexeme) => write!(f, "Encountered unexpected character {:?} while analysing lexeme {}", unexpected_char, lexeme),
            Failure::UnexpectedEof(lexeme) => write!(f, "Encountered unexpected end of stream while analysing {}", lexeme)
        }
    }
}

/// Iterator that yields tokens.
pub struct GenericTokenStream<'a, TokenType, StateKey> {
    strm: stream::Stream,
    settings: &'a LexerSettings<'a, TokenType, StateKey>
}

impl<'a, TokenType, StateKey> Iterator for GenericTokenStream<'a, TokenType, StateKey>
where StateKey: Eq + Copy + Hash + fmt::Debug, TokenType: Clone + fmt::Debug {
    type Item = Result<GenericToken<TokenType>, Failure>;

    /// Attempt to yield the next token.
    fn next(&mut self) -> Option<Self::Item> {
        log::info!("-- Next Token --");

        let mut current_key = self.settings.initial_state_key;
        let mut text = String::new();

        let mut unexpected_char: Option<char> = None;

        while let Some(chr) = self.strm.peek() {
            log::trace!("Peeking character: {:?}", chr);
            
            let state = self.settings.get_state(current_key);

            if let Some(new_key) = attempt_state_transition(current_key, &state.transitions, chr) {
                text.push(chr);
                self.strm.advance();
                log::trace!("Character added to lexeme string: {:?}", text);

                current_key = new_key;
                log::trace!("State transitioned made - continuing...");
            }
            else {
                log::trace!("No appropriate transitions from state {:?} found!", current_key);

                if self.settings.match_ignored.is_a_match(chr) && current_key == self.settings.initial_state_key {
                    log::trace!("As currently in the initial state, character can be ignored - continuing...");
                    self.strm.advance(); // Advance the stream but don't add ignored character to lexeme string.
                }
                else {
                    log::trace!("Character cannot be ignored - breaking...");
                    unexpected_char = Some(chr);
                    break;
                }
            }
        }

        if !text.is_empty() {
            log::trace!("Attempting to parse lexeme...");
            Some(attempt_parse_lexeme_to_token(
                Lexeme { text, pos: self.strm.get_pos().clone() },
                unexpected_char, self.settings.get_state(current_key)
            ))
        }
        else { None } // Nothing added to lexeme - assume stream had already reached end.
    }
}

/// Attempt to transition state given a vector of transitions and the current
/// input character. Will return `Some` holding the next state key should an
/// appropriate transition be found (whether to the current state or elsewhere).
/// `None` is returned when no appropriate transitions could be found.
fn attempt_state_transition<StateKey>(current_key: StateKey, transitions : &Vec<Transition<StateKey>>, chr: char) -> Option<StateKey>
where StateKey: Copy + fmt::Debug {
    for transition in transitions {
        if transition.match_by.is_a_match(chr) {
            return match &transition.to {
                Dest::To(new_key) => {
                    log::trace!("Transitioning state from {:?} to {:?}...", current_key, new_key);
                    Some(*new_key) // To new state...
                }

                Dest::ToSelf => {
                    log::trace!("Remaining in current state {:?}...", current_key);
                    Some(current_key) // To same state...
                }
            }
        }
    }
    None // No appropriate transition found (to self or otherwise) so return nothing.
}

fn attempt_parse_lexeme_to_token<TokenType, StateKey>(lexeme: Lexeme, next_chr: Option<char>, final_state: &State<TokenType, StateKey>) -> Result<GenericToken<TokenType>, Failure>
where TokenType: fmt::Debug + Clone {
    match final_state.parse.lexeme_string_to_token_type::<StateKey>(&lexeme.text) {
        Some(tok_type) => {
            log::debug!("Lexeme {} parsed to token type: {:?}", lexeme, tok_type);
            //Ok(LexToken(tok, lexeme, pos))
            Ok(GenericToken { tok_type, lexeme })
        }
        None => {
            log::debug!("Ccould not parse to token from lexeme {}",  lexeme);
            Err(match next_chr {
                Some(chr) => {
                    log::trace!("Failure to parse to token a result of unexpected character: {:?}", chr);
                    Failure::UnexpectedChar(chr, lexeme)
                }
                None => {
                    log::trace!("Failure to parse to a token a result of reaching the stream end unexpectedly");
                    Failure::UnexpectedEof(lexeme)
                }
            })
        }
    }
}

struct LexerSettings<'a, TokenType, StateKey> {
    /// The collection of states that make up the lexer's automaton:
    states: HashMap<StateKey, State<'a, TokenType, StateKey>>,
    /// The key which indicates the starting/initial state:
    initial_state_key: StateKey,
    /// Indicates any characters that can be ignored when in the initial state:
    match_ignored: Match<'a>
}

impl<TokenType, StateKey> LexerSettings<'_, TokenType, StateKey>
where StateKey: Eq + Hash + fmt::Debug {
    /// Method to fetch a state using a given state key - panics if state does
    /// not exist.
    fn get_state<'a>(&'a self, key: StateKey) -> &'a State<'a, TokenType, StateKey> {
        self.states.get(&key).expect(&format!("Lexer transitioned into an undefined state: {:?}", key))
    }
}

/// Describes a lexing state. Can include any number of transitions to other
/// states. When the lexer finds no appropriate transitions from this state,
/// the specified parsing function or target token of the `parse` member is
/// used to yield a token type. A lexical error is produced if `parse` is of the
/// `Parse::Invalid` variant when a transition away from this state cannot be
/// made).
struct State<'a, TokenType, StateKey> {
    parse: Parse<'a, TokenType>,
    transitions: Vec<Transition<'a, StateKey>>
}

/// When the lexer finds itself in a state that it cannot transition from, it
/// relies on the the value of State::parse in order to either yield a token or
/// realise that a lexical error should be produced.
enum Parse<'a, TokenType> {
    /// For producing token type that require no data from the lexeme (e.g. `BracketOpen`):
    To(TokenType),
    /// For token types with information extracted from lexeme (e.g. `NumberLiteral`, `Identifier`):
    ByFunction(&'a (dyn Fn(&str) -> TokenType + Send + Sync)),
    // For transitional states that do not produce a token at all (e.g. in `PotentialReal` state):
    Invalid
}

impl<TokenType> Parse<'_, TokenType>
where TokenType: Clone {
    fn lexeme_string_to_token_type<'a, StateKey>(&self, lexeme_text: &str) -> Option<TokenType> {
        match self {
            Parse::To(tok) => Some(tok.clone()),
            Parse::ByFunction(func) => Some(func(lexeme_text)),
            Parse::Invalid => None
        }
    }
}

/// Describes a transition from one state to another (or to itself). The lexer
/// decides whether this transition should be followed by calling that
/// transition's matching function with the character most recently read from
/// the stream. Should the matching funtion return turn true, the lexer will
/// transition states as specified.
struct Transition<'a, StateKey> {
    match_by: Match<'a>,
    to: Dest<StateKey>
}

/// Criteria by which it is decided whether the lexer should transition state
/// given the most recent character read from stream.
enum Match<'a> {
    /// Match by a single character:
    ByChar(char),
    /// Match by a number of possible characters:
    ByChars(Vec<char>),
    /// Provide read charater to function which will return true if transition should be made:
    ByFunction(&'a (dyn Fn(&char) -> bool + Send + Sync)),
    /// Will always match and thus transition:
    Any
}

impl Match<'_> {
    fn is_a_match(&self, chr: char) -> bool {
        match self {
            Match::ByChar(expected) => chr == *expected,
            Match::ByChars(possible) => possible.contains(&chr),
            Match::ByFunction(func) => func(&chr),
            Match::Any => true
        }
    }
}

/// Indicates how the lexer should transition state - either to remain on the
/// current state or to transition to a state with a given key.
enum Dest<StateKey> {
    /// For remaining on the same state:
    ToSelf,
    // For transitioning to other states:
    To(StateKey)
}