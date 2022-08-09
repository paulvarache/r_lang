use std::fmt;
use std::rc::Rc;

use crate::interpreter::Interpreter;
use crate::lox_error::LoxResult;
use crate::scanner::value::Value;

#[derive(Debug)]
pub struct Callable {
    pub func: Rc<dyn LoxCallable>,
}

impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.func, &other.func)
    }
}

pub trait LoxCallable {
    fn call(&self, interpreter: &Interpreter, args: Vec<Value>) -> LoxResult<Value>;
    fn arity(&self) -> usize;
}

impl fmt::Debug for dyn LoxCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LoxCallable trait")
    }
}
