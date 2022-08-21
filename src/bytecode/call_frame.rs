use std::cell::RefCell;
use std::rc::Rc;

use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::RuntimeError;
use crate::error::RuntimeErrorCode;

use super::closure::Closure;
use super::value::Value;

pub struct CallFrame {
    pub closure: Rc<RefCell<Closure>>,
    pub ip: usize,
    pub slots_offset: usize,
}

impl CallFrame {
    pub fn new(closure: &Rc<RefCell<Closure>>, slots_offset: usize) -> Self {
        Self {
            closure: Rc::clone(closure),
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
            .borrow()
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
    pub fn set_upvalue(&mut self, addr: usize, value: &Value) {
        self.closure.borrow_mut().upvalues[addr] = value.clone();
    }
    pub fn get_upvalue(&self, addr: usize) -> Value {
        self.closure.borrow_mut().upvalues[addr].clone()
    }
    pub fn get_byte(&self, addr: usize) -> Option<u8> {
        self.closure.borrow().function.chunk.get_at(addr)
    }
    fn error(&self, code: RuntimeErrorCode) -> LoxError {
        LoxError::Runtime(RuntimeError {
            func_id: self.closure.borrow().function.id(),
            code,
            addr: self.ip - 1,
        })
    }
}
