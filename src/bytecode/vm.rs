use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use colored::Colorize;

use crate::bytecode::closure::Closure;
use crate::bytecode::debug::disassemble_chunk_instruction;
use crate::bytecode::opcode::OpCode;
use crate::bytecode::value::Value;
use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::RuntimeError;
use crate::error::RuntimeErrorCode;

use super::call_frame::CallFrame;
use super::function::Function;
use super::upvalue::Upvalue;
use super::value::NativeFunction;

pub struct VM {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    pub frames: Vec<RefCell<CallFrame>>,
}

macro_rules! expr {
    ($e:expr) => {
        $e
    };
}

fn native_clock(_args: Vec<Value>) -> Value {
    Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time is before Unix Epoch x.x")
            .as_secs_f64(),
    )
}

impl VM {
    pub fn new() -> Self {
        let mut s = Self {
            stack: Vec::new(),
            globals: HashMap::new(),
            frames: Vec::new(),
        };

        s.define_native("clock", native_clock);

        s
    }
    fn current_frame(&self) -> &RefCell<CallFrame> {
        self.frames
            .last()
            .expect("can't start using call frame before any exists")
    }
    pub fn run(&mut self) -> LoxResult<()> {
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

        while self.current_frame().borrow().ip
            < self
                .current_frame()
                .borrow()
                .closure
                .borrow()
                .function
                .chunk
                .len()
        {
            #[cfg(feature = "debug_trace_execution")]
            {
                let _ = disassemble_chunk_instruction(
                    &self
                        .current_frame()
                        .borrow()
                        .closure
                        .borrow()
                        .function
                        .chunk,
                    self.current_frame().borrow().ip,
                );
            }
            let addr = self.current_frame().borrow_mut().advance();
            let op: OpCode = self.current_frame().borrow().get_byte(addr).unwrap().into();
            match op {
                OpCode::Constant => {
                    let value = self.read_constant()?;
                    self.push(value);
                }
                OpCode::ConstantLong => {
                    let value = self.read_constant_long()?;
                    self.push(value);
                }
                OpCode::Return => {
                    let result = self.pop();
                    if self.frames.len() == 1 {
                        self.pop();
                        return Ok(());
                    }
                    let len = self.current_frame().borrow().slots_offset - 1;
                    self.stack.truncate(len);
                    self.push(result);
                    self.frames.pop();
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
                OpCode::Print => println!("{}", self.pop()),
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefineGlobal => {
                    let value = self.read_constant()?;
                    if let Value::String(name) = value {
                        let p = self.pop();
                        self.globals.insert(name, p.clone());
                    }
                }
                OpCode::GlobalGet => {
                    let name = self.read_constant()?;
                    if let Value::String(name) = name {
                        if let Some(value) = self.globals.get(&name) {
                            self.push(value.clone());
                        } else {
                            return Err(self.error(RuntimeErrorCode::UndefinedGlobal));
                        }
                    }
                }
                OpCode::GlobalSet => {
                    let name = self.read_constant()?;
                    if let Value::String(name) = name {
                        if !self.globals.contains_key(&name) {
                            return Err(self.error(RuntimeErrorCode::UndefinedGlobal));
                        }
                        let value = self.peek(0);
                        self.globals.insert(name, value.clone());
                    }
                }
                OpCode::Popn => {
                    let n = self.current_frame().borrow_mut().read_byte()?;
                    self.stack
                        .truncate(self.stack.len().saturating_sub(n as usize));
                }
                OpCode::LocalGet => {
                    let local_addr = self.current_frame().borrow_mut().read_byte()?;
                    let slots_offset = self.current_frame().borrow().slots_offset;

                    self.push(self.stack[slots_offset + local_addr as usize].clone());
                }
                OpCode::LocalSet => {
                    let local_addr = self.current_frame().borrow_mut().read_byte()?;
                    let slots_offset = self.current_frame().borrow().slots_offset;

                    self.stack[slots_offset + local_addr as usize] = self.peek(0).clone();
                }
                OpCode::JumpIfFalse => {
                    let offset = self.current_frame().borrow_mut().read_short()?;

                    if self.peek(0).is_falsey() {
                        self.current_frame().borrow_mut().ip += offset as usize;
                    }
                }
                OpCode::Jump => {
                    let offset = self.current_frame().borrow_mut().read_short()?;

                    self.current_frame().borrow_mut().ip += offset as usize;
                }
                OpCode::Loop => {
                    let offset = self.current_frame().borrow_mut().read_short()?;

                    self.current_frame().borrow_mut().ip -= offset as usize;
                }
                OpCode::Call => {
                    let arg_count = self.current_frame().borrow_mut().read_byte()?;
                    let value = self.peek(arg_count.into()).clone();
                    self.call_value(&value, arg_count)?;
                }
                OpCode::Closure => {
                    let value = self.read_constant()?;
                    if let Value::Func(function) = value {
                        let mut closure = Closure::new(&function);

                        for i in 0..closure.function.upvalue_count {
                            let is_local = self.current_frame().borrow_mut().read_byte()?;
                            let addr = self.current_frame().borrow_mut().read_byte()?;
                            if is_local == 1 {
                                let value = self.capture_upvalue(
                                    &self.stack[self.current_frame().borrow().slots_offset
                                        + addr as usize],
                                );
                                self.current_frame()
                                    .borrow_mut()
                                    .set_upvalue(i as usize, &value);
                            } else {
                                closure.upvalues[i as usize] =
                                    self.current_frame().borrow().get_upvalue(addr as usize);
                            }
                        }
                        self.push(Value::Closure(Rc::new(RefCell::new(closure))));
                    }
                }
                OpCode::UpvalueGet => {
                    let slot = self.current_frame().borrow_mut().read_byte()?;
                    let value = self.current_frame().borrow().get_upvalue(slot as usize);
                    self.push(value);
                }
                OpCode::UpvalueSet => {
                    let slot = self.current_frame().borrow_mut().read_byte()?;
                    self.current_frame()
                        .borrow_mut()
                        .set_upvalue(slot as usize, &self.peek(0));
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
    fn call_value(&mut self, value: &Value, arg_count: u8) -> LoxResult<()> {
        match value {
            Value::Closure(closure) => self.call(closure, arg_count),
            Value::Native(native) => {
                let end = self.stack.len();
                let start = end - arg_count as usize;
                let args: Vec<Value> = self.stack.drain(start..end).collect();
                let value = native(args);
                self.push(value);
                Ok(())
            }
            _ => Err(self.error(RuntimeErrorCode::CallNonFunctionValue)),
        }
    }
    pub fn call(&mut self, closure: &Rc<RefCell<Closure>>, arg_count: u8) -> LoxResult<()> {
        if arg_count != closure.borrow().function.arity() {
            return Err(self.error(RuntimeErrorCode::FunctionCallArityMismatch));
        }
        self.frames.push(RefCell::new(CallFrame::new(
            &Rc::clone(closure),
            self.stack.len() - arg_count as usize,
        )));
        Ok(())
    }
    pub fn define_native(&mut self, name: &str, native: NativeFunction) {
        self.globals.insert(name.to_string(), Value::Native(native));
    }
    fn peek(&self, n: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - n]
    }
    fn print_stack(&self) {
        println!("{:=^32}", "STACK".to_string());
        for value in &self.stack {
            println!("| [{:<26}] |", format!("{}", value).bright_yellow());
        }
        println!("{:=^32}", "".to_string());
    }
    fn read_constant(&mut self) -> LoxResult<Value> {
        let constant_addr = self.current_frame().borrow_mut().read_byte()?;
        self.current_frame()
            .borrow_mut()
            .closure
            .borrow()
            .function
            .chunk
            .get_constant(constant_addr as usize)
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfConstantsBounds))
    }
    fn read_constant_long(&mut self) -> LoxResult<Value> {
        let constant_addr = self
            .current_frame()
            .borrow_mut()
            .closure
            .borrow()
            .function
            .chunk
            .get_constant_long_addr(self.current_frame().borrow_mut().advance())?;
        self.current_frame().borrow_mut().ip += 2;
        self.current_frame()
            .borrow_mut()
            .closure
            .borrow()
            .function
            .chunk
            .get_constant(constant_addr as usize)
            .ok_or_else(|| self.error(RuntimeErrorCode::OutOfConstantsBounds))
    }
    fn error(&self, code: RuntimeErrorCode) -> LoxError {
        LoxError::Runtime(RuntimeError {
            func_id: self.current_frame().borrow().closure.borrow().function.id(),
            code,
            addr: self.current_frame().borrow().ip - 1,
        })
    }

    fn capture_upvalue(&self, value: &Value) -> Value {
        Value::Upvalue(Rc::new(value.clone()))
    }
}
