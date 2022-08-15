use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::io;
use std::io::stdout;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;
use std::process;
use std::rc::Rc;

mod ast;
mod bytecode;
mod callable;
mod class;
mod environment;
mod error;
mod function;
mod interpreter;
mod native;
mod parser;
mod resolver;
mod scanner;

use bytecode::chunk::Chunk;
use bytecode::emitter::Emitter;
use bytecode::sourcemap::Sourcemap;
use bytecode::vm::VM;
use bytecode::Compiler;
use colored::Colorize;
use error::LoxError;
use parser::Parser;
use resolver::Resolver;
use stringreader::StringReader;

use crate::interpreter::Interpreter;
use crate::scanner::Scanner;

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
        #[cfg(feature = "use_bytecode")]
        {
            let scanner = Scanner::new(reader);

            let emitter = Emitter::new();
            let mut compiler = Compiler::new(Box::new(scanner), Box::new(emitter));

            let chunk = compiler.compile().expect("compilation error");

            let mut vm = VM::new();

            match vm.run(chunk.as_ref()) {
                Ok(_) => {}
                Err(e) => eprintln!("ERROR"),
            }

            return false;
        }
        #[cfg(not(feature = "use_bytecode"))]
        {
            let mut had_error = false;
            let scanner = Scanner::new(reader);
            let mut parser = Parser::new(Box::new(scanner));
            let resolver = Resolver::new(&self.interpreter);
            // let printer = AstPrinter {};
            let mut errors = Vec::new();

            loop {
                match parser.declaration() {
                    Ok(stmt) => match stmt {
                        Some(stmt) => {
                            // println!(
                            //     "Parsed statement {}",
                            //     printer.print(&stmt).unwrap()
                            // );

                            match resolver.resolve(&stmt) {
                                Ok(_) => {
                                    // Don't evaluate the next statement it it failed before
                                    if had_error {
                                        continue;
                                    }

                                    match self.interpreter.execute(&stmt) {
                                        Ok(_) => {}
                                        Err(err) => {
                                            errors.push(err);
                                            had_error = true;
                                            break;
                                        }
                                    }
                                }
                                Err(err) => {
                                    errors.push(err);
                                    had_error = true;
                                }
                            }
                        }
                        None => break,
                    },
                    Err(err) => {
                        errors.push(err);
                        had_error = true
                    }
                };
            }

            errors.sort_by_key(|a| a.span());

            for err in errors {
                println!("{}: {}", "error".red(), format!("{err}").bright_white());
                match err {
                    LoxError::Scanner(err) => {
                        println!("{}", parser.scanner.format_error_loc(err.span));
                    }
                    LoxError::Parser(err) => {
                        let token = err.next_token.unwrap_or(err.token);
                        println!("{}", parser.scanner.format_error_loc(token.span));
                    }
                    LoxError::Interpreter(err) => {
                        println!("{}", parser.scanner.format_error_loc(err.span));
                    }
                    _ => {}
                }
                println!();
            }

            had_error
        }
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
