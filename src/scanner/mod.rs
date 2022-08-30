pub mod span;
pub mod token;
pub mod token_type;

use std::io;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;

use colored::Colorize;

use crate::error::Demistify;
use crate::error::LoxError;
use crate::error::LoxErrorCode;
use crate::error::LoxResult;
use crate::error::ScannerError;
use crate::error::ScannerErrorCode;

use self::span::Span;
use self::token::Token;
use self::token_type::TokenType;

impl Demistify for ScannerError {
    fn demistify(&self) -> String {
        match self.code {
            LoxErrorCode::Scanner(code) => match code {
                ScannerErrorCode::UnterminatedString => "unterminated string".to_string(),
                ScannerErrorCode::Unknown => "".to_string(),
            },
        }
    }
}

enum TemplateLiteralCtx {
    String,
    Value,
}

pub trait Scan {
    fn span(&self) -> Span;
    fn format_error_loc(&self, span: Span) -> String;
    fn format_backtrace_line(&self, span: Span) -> String;
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
    source: Vec<String>,
    template_literal_depth: usize,
    template_literal_ctx: TemplateLiteralCtx,
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
        loop {
            if let Some(c) = self.advance() {
                if let Some(token) = self.parse_token(c)? {
                    return Ok(Some(token));
                }
            } else {
                self.reached_end = true;
                return Ok(Some(self.get_token(TokenType::Eof)));
            }
        }
    }

    fn format_backtrace_line(&self, span: Span) -> String {
        let default = "".to_string();
        let mut res = Vec::new();
        let all_lines = span.start.0..(span.end.0 + 1);
        let max_pad = all_lines.clone().max().unwrap_or(1).to_string().len();
        for i in all_lines {
            let line = self.source.get(i - 1).unwrap_or(&default);

            let start_col = if span.start.0 == i { span.start.1 } else { 1 };
            let end_col = if span.end.0 == i {
                span.end.1
            } else {
                line.len() + 1
            };

            let line_start = line.chars().take(start_col - 1).collect::<String>();
            let line_highlight = line
                .chars()
                .skip(start_col - 1)
                .take(end_col - start_col)
                .collect::<String>();
            let line_end = line.chars().skip(end_col - 1).collect::<String>();

            let line_prefix = format!("{i:00$} |", max_pad).cyan();

            res.push(format!(
                " {line_prefix} {}{}{}",
                line_start.dimmed(),
                line_highlight,
                line_end.dimmed()
            ));
        }
        res.join("\n")
    }

    fn format_error_loc(&self, span: Span) -> String {
        let default = "".to_string();
        let mut res = Vec::new();
        let all_lines = span.start.0..(span.end.0 + 1);
        let max_pad = all_lines.clone().max().unwrap_or(1).to_string().len();
        res.push(format!(" {:01$} |", "", max_pad).cyan().to_string());
        for i in all_lines {
            let line = self.source.get(i - 1).unwrap_or(&default);

            let start_col = if span.start.0 == i { span.start.1 } else { 1 };
            let end_col = if span.end.0 == i {
                span.end.1
            } else {
                line.len() + 1
            };

            let nil = "";
            let underline = format!("{nil:^<0$}", end_col - start_col).red();
            let fill = format!(
                "{nil:0$}{underline}{nil:1$}",
                start_col - 1,
                line.len().saturating_sub(end_col - 1)
            );

            let line_prefix = format!("{i:00$} |", max_pad).cyan();
            let underline_prefix = format!("{:01$} |", "", max_pad).cyan();

            res.push(format!(" {line_prefix} {line}"));
            res.push(format!(" {underline_prefix} {fill}"));
        }
        res.join("\n")
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
            source: vec![],
            template_literal_depth: 0,
            template_literal_ctx: TemplateLiteralCtx::String,
        }
    }
    fn begin_template_literal(&mut self) {
        self.template_literal_depth += 1;
    }
    fn end_template_literal(&mut self) {
        self.template_literal_depth -= 1;
    }
    fn is_within_template_literal(&self) -> bool {
        self.template_literal_depth != 0
    }
    fn last_match(&self, expect: u8) -> bool {
        if let Some(last) = self.last {
            if last == expect {
                return true;
            }
        }
        return false;
    }
    fn parse_token(&mut self, c: u8) -> Result<Option<Token>, LoxError> {
        if self.is_within_template_literal()
            && matches!(self.template_literal_ctx, TemplateLiteralCtx::String)
        {
            match c {
                b'`' if !self.last_match(b'\\') => {
                    self.end_template_literal();
                    Ok(Some(self.get_token(TokenType::Backtick)))
                }
                b'{' if !self.last_match(b'\\') => {
                    self.template_literal_ctx = TemplateLiteralCtx::Value;
                    Ok(Some(self.get_token(TokenType::OpenBrace)))
                }
                _ => self.get_template_literal_string(),
            }
        } else {
            match c {
                b'(' => Ok(Some(self.get_token(TokenType::OpenParen))),
                b')' => Ok(Some(self.get_token(TokenType::CloseParen))),
                b'{' => Ok(Some(self.get_token(TokenType::OpenBrace))),
                b'}' => {
                    if self.is_within_template_literal() {
                        self.template_literal_ctx = TemplateLiteralCtx::String;
                    }
                    Ok(Some(self.get_token(TokenType::CloseBrace)))
                }
                b'[' => Ok(Some(self.get_token(TokenType::OpenSqr))),
                b']' => Ok(Some(self.get_token(TokenType::CloseSqr))),
                b',' => Ok(Some(self.get_token(TokenType::Comma))),
                b'.' => Ok(Some(self.get_token(TokenType::Dot))),
                b'-' => Ok(Some(self.get_token(TokenType::Minus))),
                b'+' => Ok(Some(self.get_token(TokenType::Plus))),
                b':' => Ok(Some(self.get_token(TokenType::Colon))),
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
                        Ok(None)
                    } else if self.is_match(b'*') {
                        self.skip_comment();
                        Ok(None)
                    } else {
                        Ok(Some(self.get_token(TokenType::Slash)))
                    }
                }
                b'\r' => {
                    self.col -= 1;
                    self.skip();
                    Ok(None)
                }
                b' ' | b'\t' => {
                    self.skip();
                    Ok(None)
                }
                b'\n' => {
                    self.col = 1;
                    self.line += 1;
                    self.skip();
                    Ok(None)
                }
                b'`' => {
                    self.begin_template_literal();
                    Ok(Some(self.get_token(TokenType::Backtick)))
                }
                b'"' => self.get_string(),
                b'0'..=b'9' => self.get_number(),
                _ if is_alpha(c) => self.get_identifier(),
                _ => Err(self.error(ScannerErrorCode::Unknown)),
            }
        }
    }
    pub fn error(&mut self, code: ScannerErrorCode) -> LoxError {
        let next_c = self.peek();
        match self.last {
            Some(_) => self.report_error(next_c, code),
            None => self.report_error(next_c, code),
        }
    }
    fn report_error(&mut self, next_c: Option<u8>, code: ScannerErrorCode) -> LoxError {
        LoxError::Scanner(ScannerError {
            span: self.span(),
            next_c,
            code: LoxErrorCode::Scanner(code),
        })
    }
    // TODO: https://doc.rust-lang.org/std/io/trait.BufRead.html#method.has_data_left
    pub fn is_at_end(&mut self) -> bool {
        self.buffer.is_empty() && self.reader.fill_buf().expect("Could not read").is_empty()
    }
    fn fill_buffer(&mut self, n: usize) -> io::Result<()> {
        let mut buf: [u8; 1] = [0];
        while self.cursor + n > self.buffer.len() {
            self.reader.read_exact(&mut buf)?;
            if buf[0] != b'\r' {
                self.buffer.push(buf[0]);
            }
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
        self.consume();
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
        let lines = s.clone();
        let sp: Vec<String> = lines.split('\n').map(|s| s.to_string()).collect();
        let mut last = self.source.pop().unwrap_or_else(|| "".to_string());
        let def: (&String, &[String]) = (&"".to_string(), &[]);
        let (first, l) = sp.split_first().unwrap_or(def);
        last += first;
        self.source.push(last);
        self.source.append(&mut l.to_vec());
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
        let span = self.span();
        Token::new(ttype, self.consume(), span)
    }
    fn get_token_literal(&mut self, ttype: TokenType, lexeme: String, span: Span) -> Token {
        Token::new(ttype, lexeme, span)
    }
    fn get_string(&mut self) -> Result<Option<Token>, LoxError> {
        let mut last = b' ';
        loop {
            match self.peek() {
                Some(c) if c != b'"' || last == b'\\' => {
                    if c == b'\n' {
                        self.col = 1;
                        self.line += 1;
                    }
                    last = c;
                    self.advance();
                }
                None => {
                    return Err(self.error(ScannerErrorCode::UnterminatedString));
                }
                _ => {
                    self.advance(); // Eat the closing "
                    let span = self.span();
                    let s: String = String::from_utf8(self.buffer[1..self.cursor - 1].to_vec())
                        .expect("Failed to parse utf-8");
                    return Ok(Some(self.get_token_literal(
                        TokenType::String,
                        s.clone(),
                        span,
                    )));
                }
            }
        }
    }
    fn get_template_literal_string(&mut self) -> Result<Option<Token>, LoxError> {
        loop {
            match self.peek() {
                Some(c) => match c {
                    b'\n' => {
                        self.col = 1;
                        self.line += 1;
                        self.advance();
                    }
                    b'`' if !self.last_match(b'\\') => {
                        break;
                    }
                    b'{' if !self.last_match(b'\\') => {
                        break;
                    }
                    _ => {
                        self.advance();
                    }
                },
                None => {
                    return Err(self.error(ScannerErrorCode::UnterminatedString));
                }
            }
        }
        let span = self.span();
        let s: String =
            String::from_utf8(self.buffer[0..self.cursor].to_vec()).expect("Failed to parse utf-8");
        return Ok(Some(self.get_token_literal(
            TokenType::String,
            s.clone(),
            span,
        )));
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
        let span = self.span();
        let s = self.consume();
        Ok(Some(self.get_token_literal(TokenType::Number, s, span)))
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
        let span = self.span();
        let lexeme = self.consume();
        let ttype = self
            .get_keyword_token_type(&lexeme)
            .unwrap_or(TokenType::Identifier);

        Ok(Some(Token::new(ttype, lexeme, span)))
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
            "use" => Some(TokenType::Use),
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
            "let" => Some(TokenType::Let),
            "while" => Some(TokenType::While),
            "assert" => Some(TokenType::Assert),
            "enum" => Some(TokenType::Enum),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use stringreader::StringReader;

    use crate::scanner::Scan;

    use super::Scanner;

    #[test]
    fn get_float() {
        let reader = StringReader::new("0.0");
        let mut scanner = Scanner::new(Box::new(reader));

        let result = scanner.next();

        assert!(result.is_ok());

        assert!(result.unwrap().is_some());
    }

    #[test]
    fn string_token() {
        let reader = StringReader::new("\"hello\"");
        let mut scanner = Scanner::new(Box::new(reader));

        let result = scanner.next();

        assert!(result.is_ok());
        let opt = result.ok().unwrap();
        assert!(opt.is_some());
        let opt_inner = opt.unwrap();
        assert_eq!(opt_inner.lexeme, "hello");
    }
    #[test]
    fn string_token_seq() {
        let reader = StringReader::new("\"hello\" \"hi\"");
        let mut scanner = Scanner::new(Box::new(reader));

        for i in ["hello", "hi"] {
            let result = scanner.next();

            assert!(result.is_ok());
            let opt = result.ok().unwrap();
            assert!(opt.is_some());
            let opt_inner = opt.unwrap();
            assert_eq!(opt_inner.lexeme, i);
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
