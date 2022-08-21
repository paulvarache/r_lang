use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::RuntimeError;
use crate::error::RuntimeErrorCode;

use super::closure::Closure;
use super::value::Value;

pub struct CallFrame {
    pub closure: Closure,
    pub ip: usize,
    pub slots_offset: usize,
}

impl CallFrame {
    pub fn new(closure: Closure, slots_offset: usize) -> Self {
        Self {
            closure,
            ip: 0,
            slots_offset,
        }
    }
    pub fn advance(&mut self) -> usize {
        let res = self.ip;
        self.ip += 1;
        res
    }
    pub fn read_byte(&mut self) -> LoxResult<u8> {
        let index = self.advance();
        self.closure
            .function
            .chunk
            .get_at(index)
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfChunkBounds))
    }
    pub fn read_short(&mut self) -> LoxResult<u16> {
        let n1 = self.read_byte()?;
        let n2 = self.read_byte()?;

        Ok(u16::from(n1) << 8 | u16::from(n2))
    }
    pub fn read_constant(&mut self) -> LoxResult<Value> {
        let constant_addr = self.read_byte()?;
        self.closure
            .function
            .chunk
            .get_constant(constant_addr as usize)
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfConstantsBounds))
    }
    pub fn read_constant_long(&mut self) -> LoxResult<Value> {
        let addr = self.advance();
        let constant_addr = self.closure.function.chunk.get_constant_long_addr(addr)?;
        self.ip += 2;
        self.closure
            .function
            .chunk
            .get_constant(constant_addr as usize)
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfConstantsBounds))
    }
    pub fn get_byte(&self, addr: usize) -> Option<u8> {
        self.closure.function.chunk.get_at(addr)
    }
    fn error(&self, code: RuntimeErrorCode) -> LoxError {
        LoxError::Runtime(RuntimeError {
            func_id: self.closure.function.id(),
            code,
            addr: self.ip - 1,
        })
    }
}
