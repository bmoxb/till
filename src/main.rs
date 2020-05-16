mod stream;
mod lexing;

use std::{ env, fs, path::Path };

fn main() {
    match env::args().nth(1) {
        Some(path) => execute_file(Path::new(&path)),
        None => repl()
    }
}

fn execute_file(path: &Path) {
    if let Ok(file) = fs::File::open(path) {
        println!("Successfully opened file: {}", path.display());

        let mut lxr = lexing::new_lexer();
        lxr.stream = Some(stream::Stream::from_file(file));

        let tokens: Vec<lexing::Token> = lxr.filter_map(|result| {
            match result {
                lexing::lexer::LexResult::Success(_, pos, tok) => {
                    println!("From {:?} obtained token: {:?}", pos, tok);
                    Some(tok)
                }
                lexing::lexer::LexResult::Failure(lexeme, pos) => {
                    println!("At {:?} failed to obtain token from lexeme: {}", pos, lexeme);
                    None
                }
            }
        }).collect();

        println!("Tokens: {:?}", tokens);
    }
}

fn repl() {}