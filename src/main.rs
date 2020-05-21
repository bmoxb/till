mod stream;
mod lexing;
mod parsing;

use std::{ env, fs, io, io::BufRead, io::Write, path::Path };

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn main() {
    // Only enable logging if debug build:
    #[cfg(debug_assertions)]
    pretty_env_logger::init_timed();

    let lxr = lexing::new_till_lexer();

    match env::args().nth(1) {
        Some(path) => execute_by_file(Path::new(&path), &lxr),
        None => execute_by_repl(&lxr)
    }
}

fn execute_by_file(relative_path: &Path, lxr: &lexing::TillLexer) {
    log::info!("Executing TILL file...");

    let path = match relative_path.canonicalize() {
        Ok(full_path) => full_path,
        Err(_) => relative_path.to_path_buf()
    };

    match fs::File::open(&path) {
        Ok(file) => {
            log::info!("Successfully opened file: {}", path.display());

            execute(stream::Stream::from_file(file), &lxr);
        }
        Err(e) => {
            match e.kind() {
                io::ErrorKind::NotFound => println!("File not found at: {}", path.display()),
                io::ErrorKind::PermissionDenied => println!("Lack required permissions to read file at: {}", path.display()),
                kind => {
                    println!("Error occured when attempting to open file at: {}", path.display());
                    log::error!("File open error kind: {:?}", kind);
                }
            }
        }
    }
}

fn execute_by_repl(lxr: &lexing::TillLexer) {
    log::info!("Beginning REPL...");

    println!("Tiny Interpreted Lightweight Language (TILL) {}", VERSION);

    loop {
        let mut input = String::new();

        while !input.ends_with("\n\n") {
            let mut buffer = String::new();

            print!("> ");
            io::stdout().flush().unwrap();

            match io::stdin().lock().read_line(&mut buffer) {
                Ok(_) => input.push_str(&buffer),
                Err(e) => {
                    log::warn!("Read from standard input error: {}", e);
                    break;
                }
            }
        }

        execute(stream::Stream::from_str(&input), lxr);
    }
}

fn execute(strm: stream::Stream, lxr: &lexing::TillLexer) {
    log::info!("Lexing...");
    let tokens = lxr.input(strm);

    log::info!("Parsing...");
    let syntax_tree = parsing::input(tokens);

    log::info!("Type checking... TODO!");

    log::info!("Executing... TODO!");
}