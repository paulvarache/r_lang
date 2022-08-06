use std::{
    env,
    fs::File,
    io::{self, stdout, BufRead, BufReader, Write},
    process,
};

mod ast;
mod interpreter;
mod lox_error;
mod parser;
mod scanner;

use parser::parser::Parser;
use stringreader::StringReader;

use crate::{ast::*, interpreter::interpreter::Interpreter, scanner::scanner::Scanner};

pub struct Lox {}

impl Lox {
    pub fn new() -> Self {
        Self {}
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
        let mut parser = Parser::new(Scanner::new(reader));
        let printer = AstPrinter {};
        let interpreter = Interpreter::new();

        loop {
            match parser.declaration() {
                Ok(stmt) => match stmt {
                    Some(stmt) => {
                        println!(
                            "Interpreting statement {}",
                            printer.print(&stmt).unwrap()
                        );

                        match interpreter.evaluate(&stmt) {
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
