mod stream;
mod lexing;

use std::{ env, fs, path::Path };

fn main() {
    // Only enable logging if debug build:
    #[cfg(debug_assertions)]
    pretty_env_logger::init_timed();

    match env::args().nth(1) {
        Some(path) => execute_file(Path::new(&path)),
        None => repl()
    }
}

fn execute_file(path: &Path) {
    if let Ok(file) = fs::File::open(path) {
        log::info!("Successfully opened file: {}", path.canonicalize().unwrap().display());

        let mut lxr = lexing::new_lexer();
        lxr.stream = Some(stream::Stream::from_file(file));

        let tokens: Vec<lexing::Token> = lxr.filter_map(|result| {
            match result {
                lexing::lexer::LexResult::Success(_, _, tok) => Some(tok),
                lexing::lexer::LexResult::Failure(_, _) => None
            }
        }).collect();

        log::info!("Tokens: {:?}", tokens);
    }
}

fn repl() {
    log::info!("Beginning REPL...");
}