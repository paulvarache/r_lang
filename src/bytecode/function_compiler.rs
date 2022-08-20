use super::chunk::Chunk;
use super::function::FunctionType;
use super::local::Local;
use super::opcode::OpCode;
use super::sourcemap::Sourcemap;
use crate::error::ParserErrorCode;
use crate::scanner::span::Span;
use crate::scanner::token::Token;

use super::value::Value;

type ParseResult<T> = Result<T, ParserErrorCode>;

pub struct FunctionCompiler {
    arity: u8,
    name: String,
    locals: Vec<Local>,
    scope_depth: usize,
    pub chunk: Chunk,
    pub sourcemap: Sourcemap,
    pub function_type: FunctionType,
}

impl FunctionCompiler {
    pub fn new(name: &str, function_type: FunctionType) -> Self {
        Self {
            arity: 0,
            locals: Vec::new(),
            scope_depth: 0,
            chunk: Chunk::new(),
            sourcemap: Sourcemap::new(),
            function_type,
            name: name.to_string(),
        }
    }
    pub fn identifer_constant(&mut self, name: &Token) -> u8 {
        let value = Value::String(name.lexeme.clone());
        self.make_constant(value)
    }
    pub fn make_constant(&mut self, value: Value) -> u8 {
        self.chunk.add_constant(value)
    }
    fn write(&mut self, byte: u8, span: Span) {
        let index = self.chunk.len();
        self.chunk.write(byte);
        self.sourcemap.describe_byte(index, span);
    }
    pub fn emit(&mut self, byte: u8, span: Span) {
        self.write(byte, span);
    }
    pub fn emit_bytes(&mut self, a: u8, b: u8, span: Span) {
        self.emit(a, span);
        self.emit(b, span);
    }
    pub fn emit_constant(&mut self, value: Value, span: Span) {
        let const_addr = self.chunk.add_constant(value);
        self.write(OpCode::Constant.into(), span);
        self.write(const_addr, span);
    }
    pub fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    pub fn end_scope(&mut self, span: Span) {
        let mut n: u8 = 0;

        // Count how many are in the current depth
        for i in (0..self.locals.len()).rev() {
            if let Some(depth) = self.locals[i].depth {
                if depth < self.scope_depth {
                    break;
                }
            } else {
                break;
            }
            n += 1;
        }

        // Don't emit the extra POPN if there is nothing to pop
        if n != 0 {
            // Pop that many
            self.emit(OpCode::Popn.into(), span);
            self.emit(n, span);
        }

        // Remove the locals
        self.locals
            .truncate(self.locals.len().saturating_sub(n as usize));
        self.scope_depth -= 1;
    }

    pub fn declare_variable(&mut self, name: &Token) -> ParseResult<()> {
        if self.scope_depth == 0 {
            return Ok(());
        }
        for local in self.locals.iter().rev() {
            if let Some(depth) = local.depth {
                if depth < self.scope_depth {
                    break;
                }
            }

            if local.name.lexeme == name.lexeme {
                return Err(ParserErrorCode::LocalAlreadyDefined);
            }
        }
        self.add_local(name)
    }
    pub fn define_variable(&mut self, const_addr: u8, span: Span) {
        if self.is_local_scope() {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpCode::DefineGlobal.into(), const_addr, span);
    }
    pub fn add_local(&mut self, name: &Token) -> ParseResult<()> {
        if self.locals.len() == u8::MAX.into() {
            return Err(ParserErrorCode::TooManyLocals);
        }
        self.locals.push(Local::new(name));
        Ok(())
    }

    pub fn resolve_local(&self, name: &Token) -> ParseResult<Option<u8>> {
        for i in (0..self.locals.len()).rev() {
            let local = &self.locals[i];
            if local.name.lexeme == name.lexeme {
                if local.depth.is_none() {
                    return Err(ParserErrorCode::ReadOwnLocalBeforeInitialized);
                }
                return Ok(Some(i as u8));
            }
        }
        Ok(None)
    }

    pub fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }
        if let Some(local) = self.locals.last_mut() {
            local.initialize(self.scope_depth);
        }
    }

    pub fn emit_jump(&mut self, opcode: OpCode, span: Span) -> usize {
        self.emit(opcode.into(), span);
        self.emit(0xFF, span);
        self.emit(0xFF, span);

        self.current_addr() - 2
    }

    pub fn patch_jump(&mut self, addr: usize) -> ParseResult<()> {
        let jump = (self.current_addr() - addr - 2) as u16;

        if jump > u16::MAX {
            return Err(ParserErrorCode::JumpTooLong);
        }

        self.chunk.write_at(addr, (jump >> 8) as u8 & 0xFF);
        self.chunk.write_at(addr + 1, jump as u8 & 0xFF);
        Ok(())
    }

    pub fn emit_loop(&mut self, addr: usize, span: Span) -> ParseResult<()> {
        self.emit(OpCode::Loop.into(), span);

        let offset = (self.current_addr() - addr + 2) as u16;
        if offset > u16::MAX {
            return Err(ParserErrorCode::JumpTooLong);
        }

        self.emit((offset >> 8) as u8 & 0xFF, span);
        self.emit(offset as u8 & 0xFF, span);
        Ok(())
    }

    pub fn is_global_scope(&self) -> bool {
        self.scope_depth == 0
    }
    pub fn is_local_scope(&self) -> bool {
        !self.is_global_scope()
    }
    pub fn current_addr(&self) -> usize {
        self.chunk.len()
    }
    pub fn increase_arity(&mut self) {
        self.arity += 1;
    }
    pub fn arity(&self) -> u8 {
        return self.arity;
    }
}