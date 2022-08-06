use std::fmt;

use crate::{scanner::token::Token, ast::Expr};

#[derive(Debug)]
pub enum LoxError {
    Scanner { line: usize, message: String },
    Parser { token: Token, message: String },
    Interpreter { expr: Expr, message: String },
}

impl LoxError {
    pub fn report(&self) {
        eprintln!("{}", self)
    }
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxError::Scanner { line, message } => {
                writeln!(f, "scanner error: {}", message)?;
                write!(f, "{} |", line)
            }
            LoxError::Parser { token, message } => {
                writeln!(f, "parser error: {}", message)?;
                write!(f, "{} |", token.line)
            }
            LoxError::Interpreter { expr:_, message } => {
                writeln!(f, "interpreter error: {}", message)?;
                write!(f, "{} |", false) // TODO: display interpreter error
            }
        }
    }
}
