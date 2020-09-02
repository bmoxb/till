mod stream;
mod lexing;
mod parsing;
mod checking;

use std::{ env, fs, io, io::BufRead, io::Write, path::Path };

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn main() {
    // Only enable logging if debug build:
    #[cfg(debug_assertions)]
    pretty_env_logger::init_timed();

    match env::args().nth(1) {
        Some(path) => execute_by_file(Path::new(&path)),
        None => execute_by_repl()
    }
}

fn execute_by_file(relative_path: &Path) {
    log::info!("Executing TILL file...");

    let path = match relative_path.canonicalize() {
        Ok(full_path) => full_path,
        Err(_) => relative_path.to_path_buf()
    };

    match fs::File::open(&path) {
        Ok(file) => {
            log::info!("Successfully opened file: {}", path.display());

            execute(stream::Stream::from_file(file));
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

fn execute_by_repl() {
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

        execute(stream::Stream::from_str(&input));
    }
}

fn execute(strm: stream::Stream) {
    let tokens = lexing::lexer::input(strm).filter_map(|x| filter_map_func(x, "LEXICAL"));

    let unchecked_syntax_tree = parsing::parser::input(tokens).filter_map(|x| filter_map_func(x, "SYNTAX"));

    checking::checker::input(unchecked_syntax_tree).filter_map(|x| filter_map_func(x, "SEMANTIC")).last();
}

fn filter_map_func<T, E: std::fmt::Display>(value: Result<T, E>, error_type: &str) -> Option<T> {
    if let Err(e) = &value { println!("{} ERROR: {}", error_type, e); }
    value.ok()
}