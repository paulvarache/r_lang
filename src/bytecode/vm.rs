use std::collections::HashMap;

use colored::Colorize;

use crate::bytecode::chunk::Chunk;
use crate::bytecode::debug::disassemble_chunk_instruction;
use crate::bytecode::local;
use crate::bytecode::opcode::OpCode;
use crate::bytecode::value::Value;
use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::RuntimeError;
use crate::error::RuntimeErrorCode;

pub struct VM {
    ip: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

macro_rules! expr {
    ($e:expr) => {
        $e
    };
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }
    pub fn run(&mut self, chunk: &Chunk) -> LoxResult<()> {
        macro_rules! guard_number_binary_op {
            () => {{
                if !matches!(
                    (self.peek(0), self.peek(1)),
                    (&Value::Number(_), &Value::Number(_))
                ) {
                    return Err(self.error(RuntimeErrorCode::NumberBinaryExprOperandsIncorrectType));
                }
            }};
        }
        macro_rules! number_binary_op {
            ($op:tt) => {
                {
                    guard_number_binary_op!();
                    let right = self.pop();
                    let left = self.pop();
                    self.push(expr!(&left $op &right));
                }
            };
        }
        while self.ip < chunk.len() {
            #[cfg(feature = "debug_trace_execution")]
            {
                let _ = disassemble_chunk_instruction(chunk, self.ip);
            }
            let op: OpCode = chunk.get_at(self.advance()).unwrap().into();
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
                    println!("{:?}", self.pop());
                    break;
                }
                OpCode::Negate => {
                    let last_index = self.stack.len() - 1;
                    let value = &self.stack[last_index];
                    if matches!(value, &Value::Number(_)) {
                        self.stack[last_index] = -value;
                    } else {
                        return Err(self.error(RuntimeErrorCode::UnaryMinusInvalidType));
                    }
                }
                OpCode::Add => {
                    if !matches!(
                        (self.peek(0), self.peek(1)),
                        (&Value::Number(_), &Value::Number(_))
                    ) && !matches!(
                        (self.peek(0), self.peek(1)),
                        (&Value::String(_), &Value::String(_))
                    ) {
                        return Err(
                            self.error(RuntimeErrorCode::NumberBinaryExprOperandsIncorrectType)
                        );
                    }
                    let right = self.pop();
                    let left = self.pop();
                    self.push(&left + &right);
                }
                OpCode::Subtract => number_binary_op!(-),
                OpCode::Multiply => number_binary_op!(*),
                OpCode::Divide => number_binary_op!(/),
                OpCode::Nil => self.push(Value::Nil),
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Not => {
                    let last_index = self.stack.len() - 1;
                    self.stack[last_index] = Value::Bool(self.peek(0).is_falsey());
                }
                OpCode::Equal => {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(Value::Bool(&left == &right));
                }
                OpCode::Greater => {
                    guard_number_binary_op!();
                    let right = self.pop();
                    let left = self.pop();
                    self.push(Value::Bool(&left > &right));
                }
                OpCode::Less => {
                    guard_number_binary_op!();
                    let right = self.pop();
                    let left = self.pop();
                    self.push(Value::Bool(&left < &right));
                }
                OpCode::Print => println!("{:?}", self.pop()),
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefineGlobal => {
                    let value = self.read_constant(&chunk)?;
                    if let Value::String(name) = value {
                        let p = self.pop();
                        self.globals.insert(name, p.clone());
                    }
                }
                OpCode::GlobalGet => {
                    let name = self.read_constant(&chunk)?;
                    if let Value::String(name) = name {
                        if let Some(value) = self.globals.get(&name) {
                            self.push(value.clone());
                        } else {
                            return Err(self.error(RuntimeErrorCode::UndefinedGlobal));
                        }
                    }
                }
                OpCode::GlobalSet => {
                    let name = self.read_constant(&chunk)?;
                    if let Value::String(name) = name {
                        if !self.globals.contains_key(&name) {
                            return Err(self.error(RuntimeErrorCode::UndefinedGlobal));
                        }
                        let value = self.peek(0);
                        self.globals.insert(name, value.clone());
                    }
                }
                OpCode::Popn => {
                    let n = self.read_byte(chunk)?;
                    self.stack
                        .truncate(self.stack.len().saturating_sub(n as usize));
                }
                OpCode::LocalGet => {
                    let local_addr = self.read_byte(chunk)?;

                    self.push(self.stack[local_addr as usize].clone());
                }
                OpCode::LocalSet => {
                    let local_addr = self.read_byte(chunk)?;

                    self.stack[local_addr as usize] = self.peek(0).clone();
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short(chunk)?;

                    if self.peek(0).is_falsey() {
                        self.ip += offset as usize;
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short(chunk)?;

                    self.ip += offset as usize;
                }
                OpCode::Loop => {
                    let offset = self.read_short(chunk)?;

                    self.ip -= offset as usize;
                }
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
    fn peek(&self, n: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - n]
    }
    fn print_stack(&self) {
        println!("{:=^32}", "STACK".to_string());
        for value in &self.stack {
            println!("| [{:<26}] |", format!("{:?}", value).bright_yellow());
        }
        println!("{:=^32}", "".to_string());
    }
    fn advance(&mut self) -> usize {
        let res = self.ip;
        self.ip += 1;
        res
    }
    fn read_byte(&mut self, chunk: &Chunk) -> LoxResult<u8> {
        chunk
            .get_at(self.advance())
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfChunkBounds))
    }
    fn read_short(&mut self, chunk: &Chunk) -> LoxResult<u16> {
        let n1 = self.read_byte(chunk)?;
        let n2 = self.read_byte(chunk)?;

        Ok(u16::from(n1) << 8 | u16::from(n2))
    }
    fn read_constant(&mut self, chunk: &Chunk) -> LoxResult<Value> {
        let constant_addr = self.read_byte(chunk)?;
        chunk
            .get_constant(constant_addr as usize)
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfConstantsBounds))
    }
    fn read_constant_long(&mut self, chunk: &Chunk) -> LoxResult<Value> {
        let constant_addr = chunk.get_constant_long_addr(self.advance())?;
        self.ip += 2;
        chunk
            .get_constant(constant_addr as usize)
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfConstantsBounds))
    }
    fn error(&self, code: RuntimeErrorCode) -> LoxError {
        LoxError::Runtime(RuntimeError {
            code,
            addr: self.ip - 1,
        })
    }
}
