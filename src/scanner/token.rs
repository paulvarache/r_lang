use std::fmt;

use super::{token_type::TokenType, value::Value};

#[derive(Debug, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub literal: Option<Value>,
}

impl Token {
    pub fn new(ttype: TokenType, lexeme: String, line: usize, literal: Option<Value>) -> Self {
        Self { ttype, lexeme, line, literal }
    }
    pub fn as_string(&self) -> String {
        self.lexeme.clone()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {} {}", self.ttype, self.lexeme, match &self.literal {
            Some(Value::String(s)) => s.clone(),
            Some(Value::Number(n)) => n.to_string(),
            _ => "".to_string()
        })
    }
}