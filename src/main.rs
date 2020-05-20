mod stream;
mod lexing;

use std::{ env, fs, io, path::Path };

fn main() {
    // Only enable logging if debug build:
    #[cfg(debug_assertions)]
    pretty_env_logger::init_timed();

    match env::args().nth(1) {
        Some(path) => execute_file(Path::new(&path)),
        None => repl()
    }
}

fn execute_file(relative_path: &Path) {
    let path = match relative_path.canonicalize() {
        Ok(full_path) => full_path,
        Err(_) => relative_path.to_path_buf()
    };

    match fs::File::open(&path) {
        Ok(file) => {
            log::info!("Successfully opened file: {}", path.display());

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
        Err(e) => {
            match e.kind() {
                io::ErrorKind::NotFound => println!("File not found at: {}", path.display()),
                io::ErrorKind::PermissionDenied => println!("Lack required permissions to read file at: {}", path.display()),
                kind => println!("Error of kind {:?} occured when attempting to open file at: {}", kind, path.display())
            }
        }
    }
}

fn repl() {
    log::info!("Beginning REPL...");
}