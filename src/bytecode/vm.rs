use colored::Colorize;

use crate::bytecode::chunk::Chunk;
use crate::bytecode::debug::disassemble_chunk_instruction;
use crate::bytecode::opcode::OpCode;
use crate::bytecode::value::Value;
use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::RuntimeError;
use crate::error::RuntimeErrorCode;

pub struct VM {
    ip: usize,
    stack: Vec<Value>,
}

macro_rules! expr {
    ($e:expr) => {
        $e
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: Vec::new(),
        }
    }
    pub fn run(&mut self, chunk: &Chunk) -> LoxResult<()> {
        macro_rules! binary_op {
            ($op:tt) => {
                {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(expr!(left $op right));
                }
            };
        }
        while self.ip < chunk.len() {
            #[cfg(feature = "debug_trace_execution")]
            {
                let _ = disassemble_chunk_instruction(chunk, self.ip);
            }
            let op: OpCode = chunk.get_at(self.read_byte()).unwrap().into();
            match op {
                OpCode::Constant => {
                    let value = self.read_constant(chunk)?;
                    self.push(value);
                }
                OpCode::ConstantLong => {
                    let value = self.read_constant_long(chunk)?;
                    self.push(value);
                }
                OpCode::Return => {
                    println!("{}", self.pop() );
                    break;
                }
                OpCode::Negate => {
                    let last_index = self.stack.len() - 1;
                    self.stack[last_index] = -self.stack[last_index];
                },
                OpCode::Add => binary_op!(+),
                OpCode::Subtract => binary_op!(-),
                OpCode::Multiply => binary_op!(*),
                OpCode::Divide => binary_op!(/),
            }
            #[cfg(feature = "debug_trace_execution")]
            {
                self.print_stack();
            }
        }
        Ok(())
    }
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }
    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap() // stacked in a way that this is a legitimate panic if it fails
    }
    fn print_stack(&self) {
        println!("{:=^32}", "STACK".to_string());
        for value in &self.stack {
            println!("| [{:<26}] |", value.to_string().bright_yellow());
        }
        println!("{:=^32}", "".to_string());
    }
    fn read_byte(&mut self) -> usize {
        let res = self.ip;
        self.ip += 1;
        res
    }
    fn read_constant(&mut self, chunk: &Chunk) -> LoxResult<Value> {
        let constant_addr = chunk
            .get_at(self.read_byte())
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfChunkBounds))?;
        chunk
            .get_constant(constant_addr as usize)
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfConstantsBounds))
    }
    fn read_constant_long(&mut self, chunk: &Chunk) -> LoxResult<Value> {
        let constant_addr = chunk
            .get_constant_long_addr(self.read_byte())?;
        self.ip += 2;
        chunk
            .get_constant(constant_addr as usize)
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfConstantsBounds))
    }
    fn error(&self, code: RuntimeErrorCode) -> LoxError {
        LoxError::Runtime(RuntimeError {
            code,
            addr: self.ip,
        })
    }
}
