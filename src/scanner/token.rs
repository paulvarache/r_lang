use std::fmt;

use crate::error::Demistify;

use super::span::Span;
use super::token_type::TokenType;
use super::value::Value;

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