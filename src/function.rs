use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::FunctionStmt;
use crate::ast::Stmt;
use crate::callable::LoxCallable;
use crate::class::LoxClass;
use crate::class::LoxInstance;
use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::error::LoxError;
use crate::error::LoxResult;
use crate::scanner::token::Token;
use crate::scanner::value::Value;

#[derive(Debug)]
pub struct LoxFunction {
    name: Token,
    params: Rc<Vec<Token>>,
    body: Rc<Vec<Rc<Stmt>>>,
    closure: Rc<RefCell<Environment>>,
    is_initializer: bool,
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
    pub fn new(definition: &FunctionStmt, closure: &Rc<RefCell<Environment>>, is_initializer: bool) -> Self {
        Self {
            name: definition.name.clone(),
            params: Rc::clone(&definition.params),
            body: Rc::clone(&definition.body),
            closure: Rc::clone(closure),
            is_initializer,
        }
    }
    pub fn bind(&self, inst: &Rc<LoxInstance>) -> LoxFunction {
        let mut e = Environment::new_with_enclosing(Rc::clone(&self.closure));
        e.define("this", Value::Instance(Rc::clone(inst)));
        LoxFunction {
            name: self.name.clone(),
            params: Rc::clone(&self.params),
            body: Rc::clone(&self.body),
            closure: Rc::new(RefCell::new(e)),
            is_initializer: self.is_initializer,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &Interpreter, args: Vec<Value>, _class: Option<Rc<LoxClass>>) -> LoxResult<Value> {
        let mut e = Environment::new_with_enclosing(Rc::clone(&self.closure));
        for (name, value) in self.params.iter().zip(args.iter()) {
            e.define(&name.as_string(), value.clone());
        }
        let result = interpreter.execute_block(&self.body, e);
        if self.is_initializer {
            return Ok(self.closure.borrow().get_at("this", 0)?);
        }
        match result {
            Err(LoxError::Return(value)) => {
                if self.is_initializer {
                    Ok(self.closure.borrow().get_at("this", 0)?)
                } else {
                    Ok(value)
                }
            },
            Err(e) => Err(e),
            Ok(_) => Ok(Value::Nil),
        }
    }
    fn arity(&self) -> usize {
        self.params.len()
    }
}
