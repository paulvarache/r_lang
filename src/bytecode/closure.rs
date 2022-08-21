use std::rc::Rc;

use super::function::Function;
use super::value::Value;

#[derive(Clone, Debug)]
pub struct Closure {
    pub function: Rc<Function>,
    pub upvalues: Vec<Value>,
}

impl Closure {
    pub fn new(function: &Rc<Function>) -> Self {
        Self {
            function: Rc::clone(function),
            upvalues: vec![Value::Nil; function.upvalue_count.into()],
        }
    }
}
