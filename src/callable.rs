use std::rc::Rc;

use crate::interpreter::Interpreter;
use crate::lox_error::LoxResult;
use crate::scanner::value::Value;

pub struct Callable {
    func: Rc<dyn LoxCallable>,
}

pub trait LoxCallable {
    fn call(&self, interpreter: &Interpreter, args: Vec<Value>) -> LoxResult<Value>;
}
