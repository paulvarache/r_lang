use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use super::chunk::Chunk;

pub enum FunctionType {
    Function,
    Script,
}

static FUNCTION_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Debug)]
pub struct Function {
    id: usize,
    arity: u8,
    pub chunk: Chunk,
    name: String,
    pub upvalue_count: u8,
}

impl Function {
    pub fn new(name: String, chunk: Chunk, arity: u8, upvalue_count: u8) -> Self {
        Self {
            id: FUNCTION_COUNTER.fetch_add(1, Ordering::SeqCst),
            arity,
            chunk,
            name,
            upvalue_count,
        }
    }
    pub fn id(&self) -> usize {
        self.id
    }
    pub fn name(&self) -> String {
        self.name.clone()
    }
    pub fn arity(&self) -> u8 {
        self.arity
    }
}
