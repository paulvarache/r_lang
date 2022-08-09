use std::cell::RefCell;
use std::f32::consts::E;
use std::rc::Rc;

use crate::ast::FunctionStmt;
use crate::ast::Stmt;
use crate::callable::LoxCallable;
use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::lox_error::LoxError;
use crate::lox_error::LoxResult;
use crate::scanner::token::Span;
use crate::scanner::token::Token;
use crate::scanner::value::Value;

#[derive(Debug)]
pub struct LoxFunction {
    name: Token,
    params: Rc<Vec<Token>>,
    body: Rc<Vec<Rc<Stmt>>>,
    closure: Rc<RefCell<Environment>>,
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name.ttype == other.name.ttype
            && Rc::ptr_eq(&self.params, &other.params)
            && Rc::ptr_eq(&self.body, &other.body)
            && Rc::ptr_eq(&self.closure, &other.closure)
    }
}

impl LoxFunction {
    pub fn new(definition: &FunctionStmt, closure: &Rc<RefCell<Environment>>) -> Self {
        Self {
            name: definition.name.clone(),
            params: Rc::clone(&definition.params),
            body: Rc::clone(&definition.body),
            closure: Rc::clone(closure),
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &Interpreter, args: Vec<Value>) -> LoxResult<Value> {
        let mut e = Environment::new_with_enclosing(Rc::clone(&self.closure));
        for (name, value) in self.params.iter().zip(args.iter()) {
            e.define(&name.as_string(), value.clone());
        }
        let result = interpreter.execute_block(&self.body, e);
        match result {
            Err(LoxError::Return(value)) => {
                Ok(value)
            },
            Err(e) => Err(e),
            Ok(_) => Ok(Value::Nil)
        }
    }
    fn arity(&self) -> usize {
        self.params.len()
    }
}
