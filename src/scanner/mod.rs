pub mod token;
pub mod token_type;
pub mod value;

use std::io::{self, BufRead, BufReader, Read};

use crate::{
    lox_error::{Demistify, LoxError, LoxErrorCode, LoxResult, ScannerError, ScannerErrorCode},
    scanner::value::Value,
};

use self::{
    token::{Span, Token},
    token_type::TokenType,
};

impl Demistify for ScannerError {
    fn demistify(&self) -> String {
        match self.code {
            LoxErrorCode::Scanner(code) => match code {
                ScannerErrorCode::NumberParsingError => {
                    format!("could not parse number at '({}, {}) -> ({}, {})'", self.span.start.0, self.span.start.1, self.span.end.0, self.span.end.1)
                }
                ScannerErrorCode::UnterminatedString => {
                    format!("unterminated string at '({}, {}) -> ({}, {})'", self.span.start.0, self.span.start.1, self.span.end.0, self.span.end.1)
                }
                ScannerErrorCode::Unknown => "".to_string(),
            },
            _ => "".to_string(),
        }
    }
}

pub trait Scan {
    fn line(&self) -> usize;
    fn span(&self) -> Span;
    fn next(&mut self) -> LoxResult<Option<Token>>;
}

pub struct Scanner<'a> {
    last: Option<u8>,
    cursor: usize,
    buffer: Vec<u8>,
    reached_end: bool,
    line: usize,
    col: usize,
    position: (usize, usize),
    reader: BufReader<Box<dyn io::Read + 'a>>,
}

fn is_alpha(c: u8) -> bool {
    (b'a'..=b'z').contains(&c) || (b'A'..=b'Z').contains(&c) || c == b'_'
}

fn is_digit(c: u8) -> bool {
    (b'0'..=b'9').contains(&c)
}

fn is_alpha_numeric(c: u8) -> bool {
    is_alpha(c) || is_digit(c)
}

impl<'a> Scan for Scanner<'a> {
    fn line(&self) -> usize {
        self.line
    }
    fn span(&self) -> Span {
        Span::new(self.position.0, self.position.1, self.line, self.col)
    }
    fn next(&mut self) -> Result<Option<Token>, LoxError> {
        if self.reached_end {
            return Ok(None);
        }
        if self.is_at_end() {
            self.reached_end = true;
            return Ok(Some(self.get_token(TokenType::Eof)));
        }
        let c = self.advance();
        match c {
            None => {
                self.reached_end = true;
                Ok(Some(self.get_token(TokenType::Eof)))
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
                    self.col = 1;
                    self.line += 1;
                    self.next()
                }
                b'"' => self.get_string(),
                b'0'..=b'9' => self.get_number(),
                _ if is_alpha(c) => self.get_identifier(),
                _ => Err(self.error(ScannerErrorCode::Unknown)),
            },
        }
    }
}

impl<'a> Scanner<'a> {
    pub fn new(r: Box<dyn io::Read + 'a>) -> Self {
        Self {
            last: None,
            cursor: 0,
            line: 1,
            col: 1,
            position: (1, 1),
            reached_end: false,
            buffer: Vec::new(),
            reader: BufReader::new(r),
        }
    }
    pub fn error(&mut self, code: ScannerErrorCode) -> LoxError {
        let next_c = self.peek();
        match self.last {
            Some(last) => self.report_error(next_c, code),
            None => self.report_error(next_c, code),
        }
    }
    fn report_error(&mut self, next_c: Option<u8>, code: ScannerErrorCode) -> LoxError {
        let err = LoxError::Scanner(ScannerError {
            span: self.span(),
            next_c,
            code: LoxErrorCode::Scanner(code),
        });
        err.report();
        err
    }
    // TODO: https://doc.rust-lang.org/std/io/trait.BufRead.html#method.has_data_left
    pub fn is_at_end(&mut self) -> bool {
        self.buffer.is_empty() && self.reader.fill_buf().expect("Could not read").is_empty()
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
                self.col += 1;
                self.cursor += 1;
                Some(c)
            }
            None => None,
        }
    }
    fn skip(&mut self) {
        self.buffer = self.buffer[self.cursor..].to_vec();
        self.cursor = 0;
        self.position = (self.line, self.col);
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
        self.position = (self.line, self.col);

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
        Token::new(ttype, self.consume(), 0, self.span(), None)
    }
    fn get_token_literal(&mut self, ttype: TokenType, literal: Value) -> Token {
        let span = Span::new(self.position.0, self.position.1, self.line, self.col);
        Token::new(ttype, self.consume(), 0, self.span(), Some(literal))
    }
    fn get_string(&mut self) -> Result<Option<Token>, LoxError> {
        loop {
            match self.peek() {
                Some(c) if c != b'"' => {
                    if c == b'\n' {
                        self.col = 1;
                        self.line += 1;
                    }
                    self.advance();
                }
                None => {
                    return Err(self.error(ScannerErrorCode::UnterminatedString));
                }
                _ => {
                    self.advance(); // Eat the closing "

                    let s: String = String::from_utf8(self.buffer[1..self.cursor - 1].to_vec())
                        .expect("Failed to parse utf-8");
                    return Ok(Some(self.get_token_literal(TokenType::String, Value::String(s))))
                }
            }
        }
    }
    fn get_number(&mut self) -> Result<Option<Token>, LoxError> {
        let mut found_dot = false;
        loop {
            match self.peek() {
                Some(b'0'..=b'9') => {
                    // numbers, good stuff
                    self.advance();
                }
                Some(b'.') if !found_dot => {
                    // dot, but only once
                    match self.peek_next() {
                        // next one must be a number, no 0.
                        Some(b'0'..=b'9') => {
                            // Still good
                            self.advance();
                            found_dot = true;
                        }
                        _ => break,
                    }
                }
                _ => break,
            }
        }
        let s = self.consume();
        match s.clone().parse::<f64>() {
            Err(_) => Err(self.error(ScannerErrorCode::NumberParsingError)),
            Ok(n) => Ok(Some(self.get_token_literal(TokenType::Number, Value::Number(n)))),
        }
    }
    fn get_identifier(&mut self) -> Result<Option<Token>, LoxError> {
        loop {
            match self.peek() {
                Some(c) if is_alpha_numeric(c) => {
                    self.advance();
                }
                _ => {
                    break;
                }
            }
        }
        let lexeme = self.consume();
        let ttype = self
            .get_keyword_token_type(&lexeme)
            .unwrap_or(TokenType::Identifier);

        Ok(Some(self.get_token(ttype)))
    }
    fn skip_comment(&mut self) {
        loop {
            match self.advance() {
                Some(b'*') if self.peek() == Some(b'/') => {
                    self.advance();
                    break;
                }
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

    use crate::scanner::{value::Value, Scan};

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

    #[test]
    fn line() {
        let reader = StringReader::new("\n\n\n\n");
        let mut scanner = Scanner::new(Box::new(reader));

        assert_eq!(scanner.line, 1);
        assert!(scanner.next().is_ok());
        assert_eq!(scanner.line, 5);
    }
    #[test]
    fn col() {
        let reader = StringReader::new("var a\nvar\n");
        let mut scanner = Scanner::new(Box::new(reader));

        assert_eq!(scanner.line, 1);
        assert_eq!(scanner.col, 1);
        assert!(scanner.next().is_ok());
        assert_eq!(scanner.col, 4);
        assert!(scanner.next().is_ok());
        assert_eq!(scanner.col, 6);
        assert!(scanner.next().is_ok());
        assert_eq!(scanner.line, 2);
        assert_eq!(scanner.col, 4);
    }
}
