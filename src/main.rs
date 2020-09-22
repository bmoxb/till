//! Compiler implemented from scratch in Rust for a toy language featuring static
//! type checking.
//! 
//! [See on GitHub](https://github.com/WiredSound/till)

#![macro_use]

/// Debugging macro for checking whether an expression matches a given pattern.
#[macro_export]
#[cfg(debug_assertions)]
macro_rules! assert_pattern {
    ($x:expr, $y:pat) => {
        match $x { $y => {}, _ => panic!("{:?}", $x) }
    };
}

mod stream;
mod lexing;
mod parsing;
mod checking;
mod codegen;

use stream::Stream;
use std::{
    io::prelude::*,
    env, fs, io, fmt,
    path::{ Path, PathBuf }
};

fn main() {
    println!("-- Till Compiler {} --\n", env!("CARGO_PKG_VERSION"));

    // Only enable logging if debug build:
    #[cfg(debug_assertions)]
    pretty_env_logger::init_timed();

    let args: Vec<String> = env::args().collect();

    match args.len() {
        3 => read_compile_write(&args[1], &args[2]),
        2 => read_compile_write(&args[1], "out.asm"),
        _ => interactive()
    }
}

/// Read till code from the file at the specified input path, compile that code,
/// and then write the resulting machine code to the file at the specified output
/// path.
fn read_compile_write(relative_in: &str, relative_out: &str) {
    let in_path = to_full_path(relative_in);
    let out_path = to_full_path(relative_out);

    match fs::File::open(&in_path) {
        Ok(file) => {
            println!("Opening input file: {}", in_path.display());

            let asm = compile(Stream::from_file(file));

            match fs::File::create(&out_path) {
                Ok(mut out_file) => {
                    match out_file.write_all(asm.as_bytes()) {
                        Ok(_) => println!("Writing to output file: {}", out_path.display()),
                        Err(e) => display_file_error(e, out_path.display())
                    }
                }
                
                Err(e) => display_file_error(e, out_path.display())
            }
        }
        Err(e) => display_file_error(e, in_path.display())
    }
}

/// Read input from stdin until EOF encountered and then compile that input as
/// till code.
fn interactive() {
    println!("Please type your code and then press Ctrl-D to compile...");

    let mut buf = String::new();

    match io::stdin().lock().read_to_string(&mut buf) {
        Ok(_) => {
            let asm = compile(Stream::from_str(&buf));
            println!("\n{}", asm);
        }
        Err(e) => display_file_error(e, "<stdin>")
    }
}

/// Perform lexical, syntactic, and semantic analysis on the till code from a
/// given input stream and then generate elf64 Intel-syntax assembly code.
fn compile(strm: Stream) -> String {
    let tokens = lexing::lexer::input(strm).filter_map(|x| display_any_failures(x, "lexical"));
    let syntax_tree = parsing::parser::input(tokens).filter_map(|x| display_any_failures(x, "syntax"));
    let final_ir = display_any_failures(checking::checker::input(syntax_tree), "semantic").unwrap();
    codegen::genelf64::input(final_ir)
}

/// Helper function that displays any errors and exits should one be encountered.
fn display_any_failures<T, E: fmt::Display>(value: Result<T, E>, compilation_stage: &str) -> Option<T> {
    if let Err(e) = &value {
        println!("{} ERROR: {}", compilation_stage.to_ascii_uppercase(), e);
        std::process::exit(0);
    }
    value.ok()
}

/// Display a given file input/output error.
fn display_file_error<T: fmt::Display>(e: std::io::Error, path: T) {
    match e.kind() {
        io::ErrorKind::NotFound => println!("File not found at: {}", path),
        io::ErrorKind::PermissionDenied => println!("Lack required permissions to access file at: {}", path),
        kind => {
            println!("Error occured when attempting to access file at: {}", path);
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