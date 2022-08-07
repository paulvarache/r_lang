use std::{
    env,
    fs::File,
    io::{self, stdout, BufRead, BufReader, Write},
    process,
};

mod ast;
mod interpreter;
mod environment;
mod lox_error;
mod parser;
mod scanner;

use parser::Parser;
use stringreader::StringReader;

use crate::{ast::*, interpreter::Interpreter, scanner::Scanner};

pub struct Lox {
    interpreter: Interpreter,
}

impl Default for Lox {
    fn default() -> Self {
        Self::new()
    }
}

impl Lox {
    pub fn new() -> Self {
        Self {
            interpreter: Interpreter::new(),
        }
    }
    pub fn run_promt(&self) {
        let stdin = io::stdin();
        print!("> ");
        stdout().flush().expect("Could not flush stdout");

        for line in stdin.lock().lines() {
            match line {
                Ok(line) if !line.is_empty() => {
                    self.run(Box::new(StringReader::new(&line)));
                    print!("> ");
                    stdout().flush().expect("Could not flush stdout");
                }
                _ => break,
            }
        }
    }

    pub fn run_file(&self, path: &str) -> io::Result<()> {
        let f = File::open(path)?;
        let reader = BufReader::new(f);
        if self.run(Box::new(reader)) {
            std::process::exit(65);
        }
        Ok(())
    }

    pub fn print_usage(&self) {
        print!("Usage: lox <src>");
        process::exit(64);
    }
    fn run<'a>(&self, reader: Box<dyn io::Read + 'a>) -> bool {
        let mut had_error = false;
        let mut parser = Parser::new(Box::new(Scanner::new(reader)));
        let printer = AstPrinter {};

        loop {
            match parser.declaration() {
                Ok(stmt) => match stmt {
                    Some(stmt) => {
                        println!(
                            "Parsed statement {}",
                            printer.print(&stmt).unwrap()
                        );
                        // Don't evaluate the next statement it it failed before
                        if had_error {
                            continue;
                        }

                        match self.interpreter.execute(&stmt) {
                            Ok(_) => {}
                            Err(_) => {
                                had_error = true;
                                break;
                            }
                        }
                    },
                    None => break
                },
                Err(_) => had_error = true,
            };
        }

        had_error
    }
}

pub fn main() {
    let lox = Lox::new();
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => lox.run_promt(),
        2 => lox.run_file(&args[1]).expect("Could not read file"),
        _ => lox.print_usage(),
    }
}
