use std::{
    env,
    fmt::{self},
    fs::File,
    io::{self, stdout, BufRead, BufReader, Write},
    process,
};

mod ast;
mod parser;
mod scanner;
mod interpreter;

use parser::parser::Parser;
use stringreader::StringReader;

use crate::{ast::*, scanner::scanner::Scanner, interpreter::interpreter::Interpreter};

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
                    match self.run(Box::new(StringReader::new(&line))) {
                        Ok(_) => {}
                        Err(error) => println!("{}", error),
                    }
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
        match self.run(Box::new(reader)) {
            Ok(_) => {}
            Err(error) => println!("{}", error),
        }
        Ok(())
    }

    pub fn print_usage(&self) {
        print!("Usage: lox <src>");
        process::exit(64);
    }
    fn run<'a>(&self, reader: Box<dyn io::Read + 'a>) -> Result<(), LoxError> {
        let mut parser = Parser::new(Scanner::new(reader));
        let printer = AstPrinter {};

        let expr = parser.expression()?;
        println!("Interpreting expression {}", printer.print(&expr).unwrap());

        let interpreter = Interpreter {};

        println!("{}", interpreter.evaluate(&expr)?);

        // loop {
        //     let token = scanner.next()?;
        //     match token {
        //         Some(token) => println!("{}", token),
        //         None => break,
        //     }
        // }

        Ok(())
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

#[derive(Debug)]
pub struct LoxError {
    line: usize,
    message: String,
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[line {}] {}", self.line, self.message)
    }
}

impl LoxError {
    pub fn new(line: usize, message: String) -> Self {
        Self { line, message }
    }
}
