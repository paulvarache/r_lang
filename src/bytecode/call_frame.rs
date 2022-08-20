use std::rc::Rc;

use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::RuntimeError;
use crate::error::RuntimeErrorCode;

use super::function::Function;

pub struct CallFrame {
    pub function: Rc<Function>,
    pub ip: usize,
    pub slots_offset: usize,
}

impl CallFrame {
    pub fn new(function: &Rc<Function>, slots_offset: usize) -> Self {
        Self {
            function: Rc::clone(function),
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
        self.function
            .chunk
            .get_at(index)
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfChunkBounds))
    }
    pub fn read_short(&mut self) -> LoxResult<u16> {
        let n1 = self.read_byte()?;
        let n2 = self.read_byte()?;

        Ok(u16::from(n1) << 8 | u16::from(n2))
    }
    fn error(&self, code: RuntimeErrorCode) -> LoxError {
        LoxError::Runtime(RuntimeError {
            func_id: self.function.id(),
            code,
            addr: self.ip - 1,
        })
    }
}
