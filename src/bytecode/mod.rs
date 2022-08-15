use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::ParserError;
use crate::error::ParserErrorCode;
use crate::scanner::span::Span;
use crate::scanner::token::Token;
use crate::scanner::token_type::TokenType;
use crate::scanner::Scan;

use self::chunk::Chunk;
use self::emitter::Emit;
use self::opcode::OpCode;
use self::precedence::Precedence;

pub mod chunk;
mod debug;
mod opcode;
pub mod sourcemap;
pub mod value;
pub mod vm;
pub mod emitter;
pub mod precedence;

type FnPtr<'a> = fn(&mut Compiler<'a>) -> LoxResult<()>;

struct ParseRule<'a> {
    prefix: Option<FnPtr<'a>>,
    infix: Option<FnPtr<'a>>,
    precedence: Precedence,
}

pub struct Compiler<'a> {
    last: Option<Token>,
    next: Option<Token>,
    scanner: Box<dyn Scan + 'a>,
    emitter: Box<dyn Emit + 'a>,
}

impl<'a> Compiler<'a> {
    pub fn new(scanner: Box<dyn Scan + 'a>, emitter: Box<dyn Emit + 'a>) -> Self {
        Self {
            next: None,
            last: None,
            scanner,
            emitter,
        }
    }
    pub fn compile(&mut self) -> LoxResult<Box<Chunk>> {
        self.expression()?;
        Ok(self.emitter.get_chunk())
    }
    fn get_rule(&self, ttype: TokenType) -> ParseRule<'a> {
        match ttype {
            TokenType::OpenParen => ParseRule { prefix: Some(Compiler::grouping), infix: None, precedence: Precedence::None },
            TokenType::CloseParen => todo!(),
            TokenType::OpenBrace => todo!(),
            TokenType::CloseBrace => todo!(),
            TokenType::Comma => todo!(),
            TokenType::Dot => todo!(),
            TokenType::Minus => ParseRule { prefix: Some(Compiler::unary), infix: Some(Compiler::binary), precedence: Precedence::Term },
            TokenType::Plus => ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Term },
            TokenType::Semicolon => todo!(),
            TokenType::Slash => todo!(),
            TokenType::Star => todo!(),
            TokenType::Bang => todo!(),
            TokenType::BangEqual => todo!(),
            TokenType::Equal => todo!(),
            TokenType::EqualEqual => todo!(),
            TokenType::Greater => todo!(),
            TokenType::GreaterEqual => todo!(),
            TokenType::Less => todo!(),
            TokenType::LessEqual => todo!(),
            TokenType::Identifier => todo!(),
            TokenType::String => todo!(),
            TokenType::Number => ParseRule { prefix: Some(Compiler::number), infix: None, precedence: Precedence::None },
            TokenType::And => todo!(),
            TokenType::Class => todo!(),
            TokenType::Else => todo!(),
            TokenType::False => todo!(),
            TokenType::Fun => todo!(),
            TokenType::For => todo!(),
            TokenType::If => todo!(),
            TokenType::Nil => todo!(),
            TokenType::Or => todo!(),
            TokenType::Print => todo!(),
            TokenType::Return => todo!(),
            TokenType::Super => todo!(),
            TokenType::This => todo!(),
            TokenType::True => todo!(),
            TokenType::Var => todo!(),
            TokenType::While => todo!(),
            TokenType::Eof => todo!(),
        }
    }
    fn expression(&mut self) -> LoxResult<()> {
        self.parse_precedence(Precedence::Assignment)?;
        Ok(())
    }
    fn emit(&mut self, byte: u8, span: Span) {
        self.emitter.emit(byte, span)
    }
    fn emit_constant(&mut self, value: f64, span: Span) {
        self.emitter.emit_constant(value, span);
    }
    fn peek(&mut self) -> LoxResult<Option<Token>> {
        if matches!(self.next, None) {
            self.next = self.scanner.next()?;
        }
        Ok(self.next.clone())
    }
    fn advance(&mut self) -> LoxResult<Option<Token>> {
        match self.next.clone() {
            Some(token) => {
                self.last = Some(token.clone());
                self.next = None;
                Ok(Some(token))
            }
            _ => {
                self.last = self.scanner.next()?;
                Ok(self.last.clone())
            }
        }
    }
    fn skip(&mut self) -> Result<(), LoxError> {
        if !matches!(self.next, None) {
            self.last = self.next.clone();
            self.next = None;
        } else {
            self.last = self.scanner.next()?;
        }
        Ok(())
    }
    fn consume(&mut self, ttype: TokenType, code: ParserErrorCode) -> LoxResult<Token> {
        match self.peek()? {
            Some(token) if token.ttype == ttype => {
                self.skip()?;
                Ok(token)
            }
            _ => Err(self.error(code)), // error reporting will grab the correct tokens as we didn't consume anything here
        }
    }
    fn grouping(&mut self) -> LoxResult<()> {
        self.expression()?;
        self.consume(TokenType::CloseParen, ParserErrorCode::MissingClosingParenAfterGroup)?;
        Ok(())
    }
    fn unary(&mut self) -> LoxResult<()> {
        if let Some(token) = self.last.clone() {
            self.parse_precedence(Precedence::Unary)?;
            match token.ttype {
                TokenType::Minus => self.emit(OpCode::Negate.into(), token.span),
                _ => unreachable!()
            }
        }
        Ok(())
    }
    fn binary(&mut self) -> LoxResult<()> {
        if let Some(token) = self.last.clone() {
            let rule = self.get_rule(token.ttype.clone());
            if let Some(p) =  Precedence::from_index(rule.precedence.get_enum_index() + 1) {
                self.parse_precedence(p)?;
                match token.ttype.clone() {
                    TokenType::Plus => self.emit(OpCode::Add.into(), token.span),
                    _ => {}
                }
            }
        }

        Ok(())
    }
    fn number(&mut self) -> LoxResult<()> {
        if let Some(token) = self.last.clone() {
            let f = token.lexeme.parse::<f64>().unwrap(); // Invalid numbers is handled at scan time
            self.emit_constant(f, token.span);
        }
        Ok(())
    }
    fn parse_precedence(&mut self, precedence: Precedence) -> LoxResult<()> {
        if let Some(token) = self.advance()? {
            let rule = self.get_rule(token.ttype);
            if let Some(prefix) = rule.prefix {
                prefix(self)?;
            }
        }

        loop {
            if let Some(token) = self.peek()? {
                let rule = self.get_rule(token.ttype);
                if precedence <= rule.precedence {
                    self.advance()?;
                    if let Some(token) = self.last.clone() {
                        if let Some(infix) = self.get_rule(token.ttype).infix {
                            infix(self)?;
                        }
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        Ok(())
    }
    fn error(&mut self, code: ParserErrorCode) -> LoxError {
        let next = self.peek().unwrap_or(None);
        match &self.last {
            Some(token) => self.report_error(token, &next, code),
            None => self.report_error(
                &Token::new(TokenType::Eof, "".to_string(), self.scanner.span(), None),
                &None,
                code,
            ),
        }
    }
    fn report_error(
        &self,
        token: &Token,
        next_token: &Option<Token>,
        code: ParserErrorCode,
    ) -> LoxError {
        LoxError::Parser(ParserError {
            token: token.clone(),
            next_token: next_token.clone(),
            code,
        })
    }
}
