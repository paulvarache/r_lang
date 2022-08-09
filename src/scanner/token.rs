use std::fmt;

use crate::lox_error::Demistify;

use super::token_type::TokenType;
use super::value::Value;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: (usize, usize),
    pub end: (usize, usize),
}

impl Span {
    pub fn new(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Self { start: (start_line, start_col), end: (end_line, end_col) }
    }

    pub(crate) fn new_from_range(start: Span, end: Span) -> Span {
        Self { start: start.start.clone(), end: end.end.clone() }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub span: Span,
    pub literal: Option<Value>,
}

impl Token {
    pub fn new(ttype: TokenType, lexeme: String, span: Span, literal: Option<Value>) -> Self {
        Self {
            ttype,
            lexeme,
            span,
            literal,
        }
    }
    pub fn as_string(&self) -> String {
        self.lexeme.clone()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} {} {}",
            self.ttype,
            self.lexeme,
            match &self.literal {
                Some(Value::String(s)) => s.clone(),
                Some(Value::Number(n)) => n.to_string(),
                _ => "".to_string(),
            }
        )
    }
}

impl Demistify for Token {
    fn demistify(&self) -> String {
        match self.ttype {
            TokenType::Number => format!("'{}'", self.lexeme),
            TokenType::Eof => "end-of-file".to_string(),
            _ => format!("{}", self)
        }
    }
}