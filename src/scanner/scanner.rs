use std::io::{self, BufRead, BufReader, Read};

use crate::{scanner::value::Value, LoxError};

use super::{token::Token, token_type::TokenType};

pub struct Scanner<'a> {
    cursor: usize,
    buffer: Vec<u8>,
    reached_end: bool,
    line: usize,
    reader: BufReader<Box<dyn io::Read + 'a>>,
}

fn is_alpha(c: u8) -> bool {
    c >= b'a' && c <= b'z' || c >= b'A' && c <= b'Z' || c == b'_'
}

fn is_digit(c: u8) -> bool {
    c >= b'0' && c <= b'9'
}

fn is_alpha_numeric(c: u8) -> bool {
    is_alpha(c) || is_digit(c)
}

impl<'a> Scanner<'a> {
    pub fn new(r: Box<dyn io::Read + 'a>) -> Self {
        Self {
            cursor: 0,
            line: 1,
            reached_end: false,
            buffer: Vec::new(),
            reader: BufReader::new(r),
        }
    }
    pub fn next(&mut self) -> Result<Option<Token>, LoxError> {
        if self.reached_end {
            return Ok(None);
        }
        if self.is_at_end() {
            self.reached_end = true;
            return Ok(Some(Token::new(
                TokenType::Eof,
                String::from(""),
                self.line,
                None,
            )));
        }
        let c = self.advance();
        match c {
            None => {
                self.reached_end = true;
                Ok(Some(Token::new(
                    TokenType::Eof,
                    String::from(""),
                    self.line,
                    None,
                )))
            }
            Some(c) => match c {
                b'(' => Ok(Some(self.get_token(TokenType::OpenParen))),
                b')' => Ok(Some(self.get_token(TokenType::CloseParen))),
                b'{' => Ok(Some(self.get_token(TokenType::OpenBrace))),
                b'}' => Ok(Some(self.get_token(TokenType::CloseBrace))),
                b',' => Ok(Some(self.get_token(TokenType::Comma))),
                b'.' => Ok(Some(self.get_token(TokenType::Dot))),
                b'-' => Ok(Some(self.get_token(TokenType::Minus))),
                b'+' => Ok(Some(self.get_token(TokenType::Plus))),
                b';' => Ok(Some(self.get_token(TokenType::Semicolon))),
                b'*' => Ok(Some(self.get_token(TokenType::Star))),
                b'!' => Ok(Some(
                    self.get_equal_token(TokenType::BangEqual, TokenType::Bang),
                )),
                b'=' => Ok(Some(
                    self.get_equal_token(TokenType::EqualEqual, TokenType::Equal),
                )),
                b'<' => Ok(Some(
                    self.get_equal_token(TokenType::LessEqual, TokenType::Less),
                )),
                b'>' => Ok(Some(
                    self.get_equal_token(TokenType::GreaterEqual, TokenType::Greater),
                )),
                b'/' => {
                    if self.is_match(b'/') {
                        self.skip_line_comment();
                        self.next()
                    } else if self.is_match(b'*') {
                        self.skip_comment();
                        self.next()
                    } else {
                        Ok(Some(self.get_token(TokenType::Slash)))
                    }
                }
                b' ' | b'\r' | b'\t' => {
                    self.skip();
                    self.next()
                }
                b'\n' => {
                    self.skip();
                    self.line += 1;
                    self.next()
                }
                b'"' => self.get_string(),
                b'0'..=b'9' => self.get_number(),
                _ if is_alpha(c) => self.get_identifier(),
                _ => Err(LoxError::new(self.line, "Unexpected character".to_string())),
            },
        }
    }
    // TODO: https://doc.rust-lang.org/std/io/trait.BufRead.html#method.has_data_left
    pub fn is_at_end(&mut self) -> bool {
        self.buffer.len() == 0 && self.reader.fill_buf().expect("Could not read").len() == 0
    }
    fn fill_buffer(&mut self, n: usize) -> io::Result<()> {
        let mut buf: [u8; 1] = [0];
        while self.cursor + n > self.buffer.len() {
            self.reader.read_exact(&mut buf)?;
            self.buffer.push(buf[0]);
        }
        Ok(())
    }
    fn advance(&mut self) -> Option<u8> {
        match self.peek() {
            Some(c) => {
                self.cursor += 1;
                Some(c)
            }
            None => None,
        }
    }
    fn skip(&mut self) {
        self.buffer = self.buffer[self.cursor..].to_vec();
        self.cursor = 0;
    }
    fn is_match(&mut self, c: u8) -> bool {
        match self.peek() {
            Some(f) if f == c => {
                self.advance();
                true
            }
            _ => false,
        }
    }
    fn peek(&mut self) -> Option<u8> {
        match self.fill_buffer(1) {
            Ok(_) => Some(self.buffer[self.cursor]),
            Err(_) => None,
        }
    }
    fn peek_next(&mut self) -> Option<u8> {
        match self.fill_buffer(2) {
            Ok(_) => Some(self.buffer[self.cursor + 1]),
            Err(_) => None,
        }
    }
    fn consume(&mut self) -> String {
        let leftover = self.buffer.split_off(self.cursor);
        let s = String::from_utf8(self.buffer.clone()).expect("Failed to parse utf-8");
        self.buffer = leftover;
        self.cursor = 0;

        s
    }
    fn get_equal_token(&mut self, equal_type: TokenType, not_equal_type: TokenType) -> Token {
        let tok = if self.is_match(b'=') {
            equal_type
        } else {
            not_equal_type
        };
        self.get_token(tok)
    }
    fn get_token(&mut self, ttype: TokenType) -> Token {
        Token::new(ttype, self.consume(), self.line, None)
    }
    fn get_string(&mut self) -> Result<Option<Token>, LoxError> {
        loop {
            match self.peek() {
                Some(c) if c != b'"' => {
                    if c == b'\n' {
                        self.line += 1;
                    }
                    self.advance();
                }
                None => {
                    return Err(LoxError::new(self.line, "Unterminated string".to_string()));
                }
                _ => {
                    self.advance(); // Eat the closing "

                    let s: String = String::from_utf8(self.buffer[1..self.cursor - 1].to_vec())
                        .expect("Failed to parse utf-8");
                    return Ok(Some(Token::new(
                        TokenType::String,
                        self.consume(),
                        self.line,
                        Some(Value::String(s)),
                    )));
                }
            }
        }
    }
    fn get_number(&mut self) -> Result<Option<Token>, LoxError> {
        let mut found_dot = false;
        loop {
            match self.peek() {
                Some(b'0'..=b'9') => { // numbers, good stuff
                    self.advance();
                },
                Some(b'.') if found_dot == false => { // dot, but only once
                    match self.peek_next() { // next one must be a number, no 0.
                        Some(b'0'..=b'9') => { // Still good
                            self.advance();
                            found_dot = true;
                        },
                        _ => break
                    }
                },
                _ => break
            }
        }
        let s = self.consume();
        match s.clone().parse::<f64>() {
            Err(_) => Err(LoxError::new(self.line, "Could not parse number".to_string())),
            Ok(literal) => Ok(Some(Token::new(TokenType::Number, s, self.line, Some(Value::Number(literal)))))
        }
    }
    fn get_identifier(&mut self) -> Result<Option<Token>, LoxError> {
        loop {
            match self.peek() {
                Some(c) if is_alpha_numeric(c) => { self.advance(); }
                _ => { break; }
            }
        }
        let lexeme = self.consume();
        let ttype = self.get_keyword_token_type(&lexeme).or(Some(TokenType::Identifier)).unwrap();
        Ok(Some(Token::new(ttype, lexeme, self.line, None)))
    }
    fn skip_comment(&mut self) {
        loop {
            match self.advance() {
                Some(b'*') if self.peek() == Some(b'/') => {
                    self.advance();
                    break;
                },
                _ => {}
            }
        }
    }
    fn skip_line_comment(&mut self) {
        loop {
            match self.peek() {
                Some(c) if c != b'\n' => {
                    self.advance();
                }
                _ => {
                    break;
                }
            }
        }
    }
    fn get_keyword_token_type(&self, lexeme: &str) -> Option<TokenType> {
        match lexeme {
            "and" => Some(TokenType::And),
            "class" => Some(TokenType::Class),
            "else" => Some(TokenType::Else),
            "false" => Some(TokenType::False),
            "for" => Some(TokenType::For),
            "fun" => Some(TokenType::Fun),
            "if" => Some(TokenType::If),
            "nil" => Some(TokenType::Nil),
            "or" => Some(TokenType::Or),
            "print" => Some(TokenType::Print),
            "return" => Some(TokenType::Return),
            "super" => Some(TokenType::Super),
            "this" => Some(TokenType::This),
            "true" => Some(TokenType::True),
            "var" => Some(TokenType::Var),
            "while" => Some(TokenType::While),
            _ => None,
        }
    }
}


#[cfg(test)]
mod tests {
    use stringreader::StringReader;

    use crate::scanner::value::Value;

    use super::Scanner;

    #[test]
    fn test_string_token() {
        let reader = StringReader::new("\"hello\"");
        let mut scanner = Scanner::new(Box::new(reader));

        let result = scanner.next();

        assert!(result.is_ok());
        let opt = result.ok().unwrap();
        assert!(opt.is_some());
        let opt_inner = opt.unwrap();
        assert!(opt_inner.literal.is_some());
        let literal = opt_inner.literal.unwrap();
        assert_eq!(literal, Value::String("hello".to_string()));
    }
    #[test]
    fn test_string_token_seq() {
        let reader = StringReader::new("\"hello\" \"hi\"");
        let mut scanner = Scanner::new(Box::new(reader));

        for i in ["hello", "hi"] {
            let result = scanner.next();
    
            assert!(result.is_ok());
            let opt = result.ok().unwrap();
            assert!(opt.is_some());
            let opt_inner = opt.unwrap();
            assert!(opt_inner.literal.is_some());
            let literal = opt_inner.literal.unwrap();
            assert_eq!(literal, Value::String(i.to_string()));
        }

    }
}