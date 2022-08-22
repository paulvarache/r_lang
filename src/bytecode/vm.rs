use std::collections::HashMap;
use std::rc::Rc;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use colored::Colorize;

use crate::bytecode::class::Class;
use crate::bytecode::closure::Closure;
use crate::bytecode::debug::disassemble_chunk_instruction;
use crate::bytecode::opcode::OpCode;
use crate::bytecode::upvalue::Upvalue;
use crate::bytecode::value::Value;
use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::RuntimeError;
use crate::error::RuntimeErrorCode;

use super::call_frame::CallFrame;
use super::closure;
use super::instance::Instance;
use super::value::NativeFunction;

pub struct VM {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    pub frames: Vec<CallFrame>,
    closure_upvalues: HashMap<usize, HashMap<u8, Upvalue>>,
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
            closure_upvalues: HashMap::new(),
        };

        s.define_native("clock", native_clock);

        s
    }
    fn pop_frame(&mut self) -> CallFrame {
        self.frames
            .pop()
            .expect("can't start using call frame before any exists")
    }
    fn push_frame(&mut self, call_frame: CallFrame) {
        self.frames.push(call_frame);
    }
    pub fn run(&mut self) -> LoxResult<()> {
        let mut current_frame = self.pop_frame();
        loop {
            if current_frame.ip >= current_frame.closure.function.chunk.len() {
                break;
            }
            macro_rules! guard_number_binary_op {
                () => {{
                    if !matches!(
                        (self.peek(0), self.peek(1)),
                        (&Value::Number(_), &Value::Number(_))
                    ) {
                        return Err(self.error(
                            current_frame,
                            RuntimeErrorCode::NumberBinaryExprOperandsIncorrectType,
                        ));
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
            #[cfg(feature = "debug_trace_execution")]
            {
                let _ = disassemble_chunk_instruction(
                    &current_frame.closure.function.chunk,
                    current_frame.ip,
                );
            }
            let addr = current_frame.advance();
            let op: OpCode = current_frame.get_byte(addr).unwrap().into();
            match op {
                OpCode::Constant => {
                    let value = current_frame.read_constant()?;
                    self.push(value.clone());
                }
                OpCode::ConstantLong => {
                    let value = current_frame.read_constant_long()?;
                    self.push(value.clone());
                }
                OpCode::Return => {
                    let result = self.pop();
                    if self.frames.len() == 0 {
                        return Ok(());
                    }
                    let last_local_addr = current_frame.slots_offset.clone();
                    self.close_upvalues(last_local_addr);
                    let len = last_local_addr;
                    self.stack.truncate(len);
                    self.push(result);
                    self.closure_upvalues.remove(&current_frame.closure.id);
                    current_frame = self.pop_frame();
                }
                OpCode::Negate => {
                    let last_index = self.stack.len() - 1;
                    let value = &self.stack[last_index];
                    if matches!(value, &Value::Number(_)) {
                        self.stack[last_index] = -value;
                    } else {
                        return Err(
                            self.error(current_frame, RuntimeErrorCode::UnaryMinusInvalidType)
                        );
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
                        return Err(self.error(
                            current_frame,
                            RuntimeErrorCode::NumberBinaryExprOperandsIncorrectType,
                        ));
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
                    let value = current_frame.read_constant()?;
                    if let Value::String(name) = value {
                        let p = self.pop();
                        self.globals.insert(name.clone(), p);
                    }
                }
                OpCode::GlobalGet => {
                    let name = current_frame.read_constant()?;
                    if let Value::String(name) = name {
                        if let Some(value) = self.globals.get(name) {
                            self.push(value.clone());
                        } else {
                            return Err(
                                self.error(current_frame, RuntimeErrorCode::UndefinedGlobal)
                            );
                        }
                    }
                }
                OpCode::GlobalSet => {
                    let name = current_frame.read_constant()?;
                    if let Value::String(name) = name {
                        if !self.globals.contains_key(name) {
                            return Err(
                                self.error(current_frame, RuntimeErrorCode::UndefinedGlobal)
                            );
                        }
                        let value = self.peek(0);
                        self.globals.insert(name.clone(), value.clone());
                    }
                }
                OpCode::Popn => {
                    let n = current_frame.read_byte()?;
                    self.stack
                        .truncate(self.stack.len().saturating_sub(n as usize));
                }
                OpCode::LocalGet => {
                    let local_addr = current_frame.read_byte()?;
                    let slots_offset = current_frame.slots_offset;

                    self.push(self.stack[slots_offset + local_addr as usize].clone());
                }
                OpCode::LocalSet => {
                    let local_addr = current_frame.read_byte()?;
                    let slots_offset = current_frame.slots_offset;

                    self.stack[slots_offset + local_addr as usize] = self.peek(0).clone();
                }
                OpCode::JumpIfFalse => {
                    let offset = current_frame.read_short()?;

                    if self.peek(0).is_falsey() {
                        current_frame.ip += offset as usize;
                    }
                }
                OpCode::Jump => {
                    let offset = current_frame.read_short()?;

                    current_frame.ip += offset as usize;
                }
                OpCode::Loop => {
                    let offset = current_frame.read_short()?;

                    current_frame.ip -= offset as usize;
                }
                OpCode::Call => {
                    let arg_count = current_frame.read_byte()?;
                    let value = self.peek(arg_count.into()).clone();
                    current_frame = self.call_value(current_frame, &value, arg_count)?;
                }
                OpCode::Closure => {
                    let value = current_frame.read_constant()?;
                    if let Value::Func(function) = value {
                        let closure = Closure::new(&function);
                        self.closure_upvalues.insert(closure.id, HashMap::new());

                        for i in 0..closure.function.upvalue_count {
                            let is_local = current_frame.read_byte()?;
                            let slot = current_frame.read_byte()?;
                            if is_local == 1 {
                                let value =
                                    self.capture_upvalue(current_frame.slots_offset as u8 + slot);
                                let upvalues =
                                    self.closure_upvalues.get_mut(&closure.id).expect("");

                                upvalues.insert(i, value);
                            } else {
                                let current_value = self
                                    .closure_upvalues
                                    .get(&current_frame.closure.id)
                                    .expect("")
                                    .get(&slot)
                                    .unwrap()
                                    .clone();
                                let upvalues =
                                    self.closure_upvalues.get_mut(&closure.id).expect("");
                                upvalues.insert(i, current_value);
                            }
                        }
                        self.push(Value::Closure(Rc::new(closure)));
                    }
                }
                OpCode::UpvalueGet => {
                    let slot = current_frame.read_byte()?;
                    let upvalues = self
                        .closure_upvalues
                        .get(&current_frame.closure.id)
                        .expect("");
                    match &upvalues.get(&slot) {
                        Some(Upvalue::Open(stack_addr)) => {
                            self.push(self.stack[*stack_addr as usize].clone());
                        }
                        Some(Upvalue::Closed(value)) => self.push(value.clone()),
                        _ => {}
                    }
                }
                OpCode::UpvalueSet => {
                    let slot = current_frame.read_byte()?;
                    let value = self.peek(0).clone();
                    let current_closure_id = current_frame.closure.id;
                    let upvalues = self
                        .closure_upvalues
                        .get_mut(&current_closure_id)
                        .expect("");
                    // Closed upvalue manages its updates on the heap (closure_upvalues)
                    if matches!(upvalues.get(&slot), Some(Upvalue::Closed(_))) {
                        upvalues.insert(slot, Upvalue::Closed(value));
                    } else if let Some(Upvalue::Open(stack_addr)) = upvalues.get(&slot) {
                        // Open upvalues contain the address of the value on the stack
                        self.stack[*stack_addr as usize] = value;
                    }
                }
                OpCode::CloseUpvalue => {
                    let addr = current_frame.ip;
                    let value = self.pop();
                    let current_closure_id = current_frame.closure.id;
                    let upvalues = self
                        .closure_upvalues
                        .get_mut(&current_closure_id)
                        .expect("");
                    upvalues.insert(addr as u8, Upvalue::Closed(value));
                }
                OpCode::Class => {
                    let name = current_frame.read_constant()?;
                    if let Value::String(name) = name {
                        self.push(Value::Class(Rc::new(Class::new(name.clone()))));
                    }
                }
                OpCode::PropertySet => {
                    let name = current_frame.read_constant()?;
                    let value = self.pop();
                    let instance = self.pop();
                    if let Value::Instance(instance) = instance {
                        instance.set_field(&name, &value);
                        self.push(value);
                    } else {
                        return Err(
                            self.error(current_frame, RuntimeErrorCode::NonInstancePropertyAccess)
                        );
                    }
                }
                OpCode::PropertyGet => {
                    let name = current_frame.read_constant()?;
                    let instance = self.peek(0).clone();
                    if let Value::Instance(instance) = instance {
                        let value = instance.get_field(&name);
                        if let Some(value) = value {
                            self.pop();
                            self.push(value);
                        } else if !self.bind_method(&instance.class, &name) {
                            return Err(
                                self.error(current_frame, RuntimeErrorCode::UndefinedProperty)
                            );
                        }
                    } else {
                        return Err(
                            self.error(current_frame, RuntimeErrorCode::NonInstancePropertyAccess)
                        );
                    }
                }
                OpCode::Method => {
                    let name = current_frame.read_constant()?;
                    if let Value::String(name) = name {
                        self.define_method(name.clone());
                    }
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
    fn call_value(
        &mut self,
        current_frame: CallFrame,
        value: &Value,
        arg_count: u8,
    ) -> LoxResult<CallFrame> {
        match value {
            Value::Closure(closure) => {
                self.push_frame(current_frame);
                self.call(closure, arg_count)
            }
            Value::BoundMethod(receiver, closure) => {
                let slot_zero_addr = self.stack.len() - arg_count as usize - 1;
                self.stack[slot_zero_addr] = receiver.as_ref().clone();
                self.push_frame(current_frame);
                self.call(closure, arg_count)
            }
            Value::Class(class) => {
                let instance = Value::Instance(Rc::new(Instance::new(class)));
                let slot_zero_addr = self.stack.len() - arg_count as usize - 1;
                self.stack[slot_zero_addr] = instance;
                if let Some(init_closure) = class.get_method(&"init".to_string()) {
                    self.push_frame(current_frame);
                    return self.call(&init_closure, arg_count);
                } else if arg_count != 0 {
                    return Err(self.error(current_frame, RuntimeErrorCode::ClassInitializerArityMismatch))
                }
                Ok(current_frame)
            }
            Value::Native(native) => {
                let end = self.stack.len();
                let start = end - arg_count as usize;
                let args: Vec<Value> = self.stack.drain(start..end).collect();
                let value = native(args);
                self.push(value);
                Ok(current_frame)
            }
            _ => Err(self.error(current_frame, RuntimeErrorCode::CallNonFunctionValue)),
        }
    }
    pub fn call(&mut self, closure: &Rc<Closure>, arg_count: u8) -> LoxResult<CallFrame> {
        let arity = closure.function.arity();
        let new_frame = CallFrame::new(closure, self.stack.len() - arg_count as usize - 1);
        if arg_count != arity {
            return Err(self.error(new_frame, RuntimeErrorCode::FunctionCallArityMismatch));
        }
        Ok(new_frame)
    }
    pub fn call_script(&mut self, closure: Closure) {
        self.closure_upvalues.insert(closure.id, HashMap::new());
        let cl = Rc::new(closure);
        self.push(Value::Closure(Rc::clone(&cl)));
        self.push_frame(CallFrame::new(&cl, self.stack.len() as usize - 1))
    }
    pub fn define_native(&mut self, name: &str, native: NativeFunction) {
        self.globals.insert(name.to_string(), Value::Native(native));
    }
    fn peek(&self, n: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - n]
    }
    fn print_stack(&self) {
        for value in &self.stack {
            print!("[ {} ]", format!("{}", value).bright_yellow());
        }
        println!("");
    }
    fn error(&self, current_frame: CallFrame, code: RuntimeErrorCode) -> LoxError {
        LoxError::Runtime(RuntimeError {
            func_id: current_frame.closure.function.id(),
            code,
            addr: current_frame.ip - 1,
        })
    }

    fn capture_upvalue(&self, addr: u8) -> Upvalue {
        Upvalue::Open(addr)
    }

    pub fn close_upvalues(&mut self, last_addr: usize) {
        let closure_ids: Vec<usize> = self.closure_upvalues.keys().map(|k| k.clone()).collect();

        for id in closure_ids {
            let upvalues = self.closure_upvalues.get_mut(&id).unwrap();

            let keys: Vec<u8> = upvalues.keys().map(|k| k.clone()).collect();

            for k in keys {
                if let Some(Upvalue::Open(addr)) = upvalues.get(&k) {
                    if *addr >= last_addr as u8 {
                        let value = self.stack[*addr as usize].clone();
                        upvalues.insert(k, Upvalue::Closed(value));
                    }
                }
            }
        }
    }

    pub fn define_method(&mut self, name: String) {
        let closure = self.peek(0);
        let class = self.peek(1);
        if let Value::Class(class) = class {
            if let Value::Closure(closure) = closure {
                class.add_method(name, closure);
                self.pop();
            }
        }
    }

    pub fn bind_method(&mut self, class: &Rc<Class>, name: &Value) -> bool {
        if let Value::String(name) = name {
            let closure = class.get_method(name);
            if let Some(closure) = closure {
                let receiver = self.peek(0);
                let bound = Value::BoundMethod(Rc::new(receiver.clone()), Rc::clone(&closure));
                self.pop();
                self.push(bound);
                return true;
            }
        }
        return false;
    }
}
