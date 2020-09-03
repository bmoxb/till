mod stream;
mod lexing;
mod parsing;
mod checking;

use std::{ env, fs, io, fmt };
use std::path::{ Path, PathBuf };

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn main() {
    // Only enable logging if debug build:
    #[cfg(debug_assertions)]
    pretty_env_logger::init_timed();

    let mut args = env::args();

    match args.len() {
        3 => compile(&args.nth(1).unwrap(), &args.nth(2).unwrap()),
        2 => compile(&args.nth(1).unwrap(), "a.out"),
        _ => println!("Please use the following syntax: till <input file path> [output file path]")
    }
}

fn compile(relative_in: &str, relative_out: &str) {
    println!("-- Till Compiler {} --", VERSION);

    let in_path = to_full_path(relative_in);
    let _out_path = to_full_path(relative_out);

    match fs::File::open(&in_path) {
        Ok(file) => {
            println!("Input file path: {}", in_path.display());

            let strm = stream::Stream::from_file(file);

            let tokens = lexing::lexer::input(strm).filter_map(|x| filter_map_func(x, "lexical"));
            let syntax_tree = parsing::parser::input(tokens).filter_map(|x| filter_map_func(x, "syntax"));
            let final_ir = checking::checker::input(syntax_tree).filter_map(|x| filter_map_func(x, "semantic"));
            
            println!("{:#?}", final_ir.collect::<Vec<parsing::Statement>>()); // temp
        }
        Err(e) => {
            match e.kind() {
                io::ErrorKind::NotFound => println!("File not found at: {}", in_path.display()),
                io::ErrorKind::PermissionDenied => println!("Lack required permissions to read file at: {}", in_path.display()),
                kind => {
                    println!("Error occured when attempting to open file at: {}", in_path.display());
                    log::error!("File open error kind: {:?}", kind);
                }
            }
        }
    }
}

fn filter_map_func<T, E: fmt::Display>(value: Result<T, E>, compilation_stage: &str) -> Option<T> {
        if let Err(e) = &value {
            println!("{} ERROR: {}", compilation_stage.to_ascii_uppercase(), e);
            std::process::exit(0);
        }
        value.ok()
    }

fn to_full_path(relative: &str) -> PathBuf {
    let relative_path = Path::new(relative);
    
    match relative_path.canonicalize() {
        Ok(full_path) => full_path,
        Err(_) => relative_path.to_path_buf()
    }
}