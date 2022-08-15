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
use self::value::Value;

pub mod chunk;
mod debug;
pub mod emitter;
mod opcode;
pub mod precedence;
pub mod sourcemap;
pub mod value;
pub mod vm;

type FnPtr<'a> = fn(&mut Compiler<'a>) -> LoxResult<()>;

#[derive(Default)]
struct ParseRule<'a> {
    prefix: Option<FnPtr<'a>>,
    infix: Option<FnPtr<'a>>,
    precedence: Precedence,
}

pub struct Compiler<'a> {
    last: Token,
    next: Option<Token>,
    pub scanner: Box<dyn Scan + 'a>,
    pub emitter: Box<dyn Emit + 'a>,
}

macro_rules! binary_rule {
    ($p:path) => {
        ParseRule {
            prefix: None,
            infix: Some(Compiler::binary),
            precedence: $p,
        }
    };
}

macro_rules! literal_rule {
    () => {
        ParseRule {
            prefix: Some(Compiler::literal),
            infix: None,
            precedence: Precedence::None,
        }
    };
}

macro_rules! comparison_rule {
    () => {
        ParseRule {
            prefix: None,
            infix: Some(Compiler::binary),
            precedence: Precedence::Comparison,
        }
    };
}

impl<'a> Compiler<'a> {
    pub fn new(scanner: Box<dyn Scan + 'a>, emitter: Box<dyn Emit + 'a>) -> Self {
        Self {
            next: None,
            last: Token::default(),
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
            TokenType::OpenParen => ParseRule {
                prefix: Some(Compiler::grouping),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::CloseParen => ParseRule::default(),
            TokenType::OpenBrace => todo!(),
            TokenType::CloseBrace => todo!(),
            TokenType::Comma => todo!(),
            TokenType::Dot => todo!(),
            TokenType::Minus => ParseRule {
                prefix: Some(Compiler::unary),
                infix: Some(Compiler::binary),
                precedence: Precedence::Term,
            },
            TokenType::Plus => binary_rule!(Precedence::Term),
            TokenType::Semicolon => todo!(),
            TokenType::Slash => binary_rule!(Precedence::Factor),
            TokenType::Star => binary_rule!(Precedence::Factor),
            TokenType::Bang => ParseRule {
                prefix: Some(Compiler::unary),
                infix: None,
                precedence: Precedence::Term,
            },
            TokenType::BangEqual => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Equality,
            },
            TokenType::Equal => todo!(),
            TokenType::EqualEqual => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Equality,
            },
            TokenType::Greater => comparison_rule!(),
            TokenType::GreaterEqual => comparison_rule!(),
            TokenType::Less => comparison_rule!(),
            TokenType::LessEqual => comparison_rule!(),
            TokenType::Identifier => todo!(),
            TokenType::String => ParseRule {
                prefix: Some(Compiler::string),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Number => ParseRule {
                prefix: Some(Compiler::number),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::And => todo!(),
            TokenType::Class => todo!(),
            TokenType::Else => todo!(),
            TokenType::False => literal_rule!(),
            TokenType::Fun => todo!(),
            TokenType::For => todo!(),
            TokenType::If => todo!(),
            TokenType::Nil => literal_rule!(),
            TokenType::Or => todo!(),
            TokenType::Print => todo!(),
            TokenType::Return => todo!(),
            TokenType::Super => todo!(),
            TokenType::This => todo!(),
            TokenType::True => literal_rule!(),
            TokenType::Var => todo!(),
            TokenType::While => todo!(),
            TokenType::Eof => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Undefined => panic!("undefined token for rule"),
        }
    }
    fn expression(&mut self) -> LoxResult<()> {
        self.parse_precedence(Precedence::Assignment, ParserErrorCode::UnterminatedBlock)?;
        Ok(())
    }
    fn emit(&mut self, byte: u8, span: Span) {
        self.emitter.emit(byte, span)
    }
    fn emit_bytes(&mut self, a: u8, b: u8, span: Span) {
        self.emit(a, span);
        self.emit(b, span);
    }
    fn emit_constant(&mut self, value: Value, span: Span) {
        self.emitter.emit_constant(value, span);
    }
    fn peek(&mut self) -> LoxResult<Token> {
        if let Some(token) = &self.next {
            Ok(token.clone())
        } else {
            let next_token = self.scanner.next()?.expect("trying to read past EOF");
            self.next = Some(next_token.clone());
            Ok(next_token)
        }
    }
    fn advance(&mut self) -> LoxResult<Token> {
        self.last = self.peek()?;
        self.next = None;
        Ok(self.last.clone())
    }
    fn skip(&mut self) -> Result<(), LoxError> {
        self.advance()?;
        Ok(())
    }
    fn consume(&mut self, ttype: TokenType, code: ParserErrorCode) -> LoxResult<Token> {
        let token = self.peek()?;
        if token.ttype == ttype {
            self.skip()?;
            Ok(token)
        } else {
            Err(self.error(code)) // error reporting will grab the correct tokens as we didn't consume anything here
        }
    }
    fn grouping(&mut self) -> LoxResult<()> {
        self.expression()?;
        self.consume(
            TokenType::CloseParen,
            ParserErrorCode::MissingClosingParenAfterGroup,
        )?;
        Ok(())
    }
    fn unary(&mut self) -> LoxResult<()> {
        let token = self.last.clone();
        self.parse_precedence(
            Precedence::Unary,
            ParserErrorCode::MissingUnaryRightHandSide,
        )?;
        match token.ttype {
            TokenType::Minus => self.emit(OpCode::Negate.into(), token.span),
            TokenType::Bang => self.emit(OpCode::Not.into(), token.span),
            _ => unreachable!(),
        }

        Ok(())
    }
    fn binary(&mut self) -> LoxResult<()> {
        let token = self.last.clone();
        let rule = self.get_rule(token.ttype.clone());
        if let Some(p) = Precedence::from_index(rule.precedence.get_enum_index() + 1) {
            self.parse_precedence(p, ParserErrorCode::MissingTermRightHandSide)?;
            match token.ttype.clone() {
                TokenType::Plus => self.emit(OpCode::Add.into(), token.span),
                TokenType::Minus => self.emit(OpCode::Subtract.into(), token.span),
                TokenType::Star => self.emit(OpCode::Multiply.into(), token.span),
                TokenType::Slash => self.emit(OpCode::Divide.into(), token.span),
                TokenType::EqualEqual => self.emit(OpCode::Equal.into(), token.span),
                TokenType::BangEqual => {
                    self.emit_bytes(OpCode::Equal.into(), OpCode::Not.into(), token.span)
                }
                TokenType::Greater => self.emit(OpCode::Greater.into(), token.span),
                TokenType::GreaterEqual => {
                    self.emit_bytes(OpCode::Less.into(), OpCode::Not.into(), token.span)
                }
                TokenType::Less => self.emit(OpCode::Less.into(), token.span),
                TokenType::LessEqual => {
                    self.emit_bytes(OpCode::Greater.into(), OpCode::Not.into(), token.span)
                }
                _ => panic!("invalid binary operator {:?}", token.ttype),
            }
        }

        Ok(())
    }
    fn number(&mut self) -> LoxResult<()> {
        let token = &self.last;
        let f = token.lexeme.parse::<f64>().unwrap(); // Invalid numbers is handled at scan time
        self.emit_constant(Value::Number(f), token.span);
        Ok(())
    }
    fn string(&mut self) -> LoxResult<()> {
        let token = &self.last;
        self.emit_constant(Value::String(token.lexeme.clone()), token.span);
        Ok(())
    }
    fn literal(&mut self) -> LoxResult<()> {
        let token = &self.last;
        match token.ttype {
            TokenType::Nil => self.emit(OpCode::Nil.into(), token.span),
            TokenType::True => self.emit(OpCode::True.into(), token.span),
            TokenType::False => self.emit(OpCode::False.into(), token.span),
            _ => {}
        }
        Ok(())
    }
    fn parse_precedence(&mut self, precedence: Precedence, code: ParserErrorCode) -> LoxResult<()> {
        let token = self.advance()?;
        let rule = self.get_rule(token.ttype);
        let prefix = rule.prefix.ok_or_else(|| self.error(code))?;
        prefix(self)?;

        loop {
            let token = self.peek()?;

            let rule = self.get_rule(token.ttype.clone());
            if precedence <= rule.precedence {
                self.skip()?;
                if let Some(infix) = self.get_rule(token.ttype).infix {
                    infix(self)?
                }
            } else {
                break;
            }
        }
        Ok(())
    }
    fn error(&mut self, code: ParserErrorCode) -> LoxError {
        let next = self.peek().unwrap(); // Happy to panic if our code errors before reading the first token
        self.report_error(&self.last, &next, code)
    }
    fn report_error(&self, token: &Token, next_token: &Token, code: ParserErrorCode) -> LoxError {
        LoxError::Parser(ParserError {
            token: token.clone(),
            next_token: next_token.clone(),
            code,
        })
    }
}
