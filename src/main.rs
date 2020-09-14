//! Compiler implemented from scratch in Rust for a toy language featuring static
//! type checking.
//! 
//! [See on GitHub](https://github.com/WiredSound/till)

mod stream;
mod lexing;
mod parsing;
mod checking;
mod codegen;

use std::io::prelude::*;
use std::{ env, fs, io, fmt };
use std::path::{ Path, PathBuf };

fn main() {
    // Only enable logging if debug build:
    #[cfg(debug_assertions)]
    pretty_env_logger::init_timed();

    let args: Vec<String> = env::args().collect();

    println!("{:?}", args);

    match args.len() {
        3 => compile(&args[1], &args[2]),
        2 => compile(&args[1], "a.out"),
        _ => println!("Please use the following syntax: till <input file path> [output file path]")
    }
}

/// Read till code from the file at the specified input path, compile that code,
/// and then write the resulting machine code to the file at the specified output
/// path.
fn compile(relative_in: &str, relative_out: &str) {
    println!("-- Till Compiler {} --", env!("CARGO_PKG_VERSION"));

    let in_path = to_full_path(relative_in);
    let out_path = to_full_path(relative_out);

    match fs::File::open(&in_path) {
        Ok(file) => {
            println!("Opening input file: {}", in_path.display());

            let strm = stream::Stream::from_file(file);

            let tokens = lexing::lexer::input(strm).filter_map(|x| display_any_failures(x, "lexical"));
            let syntax_tree = parsing::parser::input(tokens).filter_map(|x| display_any_failures(x, "syntax"));
            let final_ir = display_any_failures(checking::checker::input(syntax_tree), "semantic").unwrap();
            let asm = codegen::genelf64::input(final_ir);

            match fs::File::create(&out_path) {
                Ok(mut out_file) => {
                    match out_file.write_all(asm.as_bytes()) {
                        Ok(_) => println!("Wrote to output file: {}", out_path.display()),
                        Err(e) => display_file_error(e, &out_path)
                    }
                }
                
                Err(e) => display_file_error(e, &out_path)
            }
        }
        Err(e) => display_file_error(e, &in_path)
    }
}

fn display_any_failures<T, E: fmt::Display>(value: Result<T, E>, compilation_stage: &str) -> Option<T> {
    if let Err(e) = &value {
        println!("{} ERROR: {}", compilation_stage.to_ascii_uppercase(), e);
        std::process::exit(0);
    }
    value.ok()
}

fn display_file_error(e: std::io::Error, path: &Path) {
    match e.kind() {
        io::ErrorKind::NotFound => println!("File not found at: {}", path.display()),
        io::ErrorKind::PermissionDenied => println!("Lack required permissions to access file at: {}", path.display()),
        kind => {
            println!("Error occured when attempting to access file at: {}", path.display());
            log::error!("File error kind: {:?}", kind);
        }
    }
}

/// Take a relative path in `&str` form and convert it into an absolute path
/// contained within a `PathBuf`.
fn to_full_path(relative: &str) -> PathBuf {
    let relative_path = Path::new(relative);
    
    match relative_path.canonicalize() {
        Ok(full_path) => full_path,
        Err(_) => relative_path.to_path_buf()
    }
}