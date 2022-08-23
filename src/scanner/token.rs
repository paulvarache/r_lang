use std::fmt;

use crate::error::Demistify;

use super::span::Span;
use super::token_type::TokenType;

#[derive(Default, Debug, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub span: Span,
}

impl Token {
    pub fn new(ttype: TokenType, lexeme: String, span: Span) -> Self {
        Self {
            ttype,
            lexeme,
            span,
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
            "{:?} {}",
            self.ttype,
            self.lexeme,
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