use super::chunk::Chunk;
use super::function::FunctionType;
use super::local::Local;
use super::opcode::OpCode;
use super::sourcemap::Sourcemap;
use crate::error::CompilerErrorCode;
use crate::scanner::span::Span;
use crate::scanner::token::Token;

use super::value::Value;

type ParseResult<T> = Result<T, CompilerErrorCode>;

pub struct FunctionCompiler {
    arity: u8,
    pub name: String,
    locals: Vec<Local>,
    scope_depth: usize,
    pub chunk: Chunk,
    pub sourcemap: Sourcemap,
    pub function_type: FunctionType,
    pub enclosing: Option<Box<FunctionCompiler>>,
    pub upvalues: Vec<(bool, u8)>,
    assert_default_msg_addr: Option<u8>,
}

impl FunctionCompiler {
    pub fn new(name: &str, function_type: FunctionType) -> Self {
        let init_local_name = match function_type {
            FunctionType::Function => "",
            _ => "this",
        };
        Self {
            arity: 0,
            locals: vec![Local {
                name: init_local_name.to_string(),
                depth: Some(0),
                is_captured: false,
            }],
            scope_depth: 0,
            chunk: Chunk::new(),
            sourcemap: Sourcemap::new(),
            function_type,
            name: name.to_string(),
            enclosing: None,
            upvalues: Vec::new(),
            assert_default_msg_addr: None,
        }
    }
    pub fn get_assert_default_msg_addr(&mut self) -> u8 {
        self.assert_default_msg_addr.unwrap_or_else(|| {
            let addr = self.make_constant(Value::String("failed assertion".to_string()));
            self.assert_default_msg_addr = Some(addr);
            addr
        })
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
    pub fn emit<T: Into<u8>>(&mut self, byte: T, span: Span) {
        self.write(byte.into(), span);
    }
    pub fn emit_bytes<T: Into<u8>, U: Into<u8>>(&mut self, a: T, b: U, span: Span) {
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
            if self.locals[i].is_captured {
                self.emit(OpCode::CloseUpvalue, span);
            } else {
                self.emit(OpCode::Pop, span);
            }
            n += 1;
        }

        // Remove the locals
        self.locals
            .truncate(self.locals.len().saturating_sub(n as usize));
        self.scope_depth = self.scope_depth.saturating_sub(1);
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

            if local.name == name.lexeme {
                return Err(CompilerErrorCode::LocalAlreadyDefined);
            }
        }
        self.add_local(name)
    }
    pub fn define_variable(&mut self, const_addr: u8, span: Span) {
        if self.is_local_scope() {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpCode::DefineGlobal, const_addr, span);
    }
    pub fn add_local(&mut self, name: &Token) -> ParseResult<()> {
        if self.locals.len() == u8::MAX.into() {
            return Err(CompilerErrorCode::TooManyLocals);
        }
        self.locals.push(Local::new(name.lexeme.clone()));
        Ok(())
    }

    pub fn resolve_local(&self, name: &Token) -> ParseResult<Option<u8>> {
        for i in (0..self.locals.len()).rev() {
            let local = &self.locals[i];
            if local.name == name.lexeme {
                if local.depth.is_none() {
                    return Err(CompilerErrorCode::ReadOwnLocalBeforeInitialized);
                }
                return Ok(Some(i as u8));
            }
        }
        Ok(None)
    }

    pub fn resolve_upvalue(&mut self, name: &Token) -> ParseResult<Option<u8>> {
        if let Some(enclosing) = &mut self.enclosing {
            if let Some(local) = enclosing.resolve_local(name)? {
                enclosing.locals[local as usize].is_captured = true;
                return self.add_upvalue(local, true);
            }
            if let Some(addr) = enclosing.resolve_upvalue(name)? {
                return self.add_upvalue(addr, false);
            }
        } else {
            return Ok(None);
        }
        Ok(None)
    }

    fn add_upvalue(&mut self, addr: u8, is_local: bool) -> ParseResult<Option<u8>> {
        for (i, upvalue) in self.upvalues.iter().enumerate() {
            if upvalue.1 == addr && upvalue.0 == is_local {
                return Ok(Some(i as u8));
            }
        }
        if self.upvalues.len() == 255 {
            return Err(CompilerErrorCode::TooManyLocals);
        }
        let upvalue = (is_local, addr);
        let upvalue_addr = self.upvalues.len();
        self.upvalues.push(upvalue);
        Ok(Some(upvalue_addr as u8))
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
        self.emit(opcode, span);
        self.emit(0xFF, span);
        self.emit(0xFF, span);

        self.current_addr() - 2
    }

    pub fn patch_jump(&mut self, addr: usize) -> ParseResult<()> {
        let jump = (self.current_addr() - addr - 2) as u16;

        if jump > u16::MAX {
            return Err(CompilerErrorCode::JumpTooLong);
        }

        self.chunk.write_at(addr, (jump >> 8) as u8 & 0xFF);
        self.chunk.write_at(addr + 1, jump as u8 & 0xFF);
        Ok(())
    }

    pub fn emit_loop(&mut self, addr: usize, span: Span) -> ParseResult<()> {
        self.emit(OpCode::Loop, span);

        let offset = (self.current_addr() - addr + 2) as u16;
        if offset > u16::MAX {
            return Err(CompilerErrorCode::JumpTooLong);
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
