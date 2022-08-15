use std::rc::Rc;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use crate::callable::LoxCallable;
use crate::class::LoxClass;
use crate::interpreter::Interpreter;
use crate::error::LoxResult;
use crate::scanner::value::Value;

pub struct DateNative {}

impl LoxCallable for DateNative {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: Vec<Value>,
        _class: Option<Rc<LoxClass>>,
    ) -> LoxResult<Value> {
        Ok(Value::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("Time is before Unix Epoch x.x")
                .as_secs_f64(),
        ))
    }

    fn arity(&self) -> usize {
        0
    }
}
