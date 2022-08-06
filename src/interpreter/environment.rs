use std::collections::HashMap;

use crate::{scanner::value::Value, lox_error::LoxError, ast::{LiteralExpr, Expr}};

pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self { values: HashMap::new() }
    }

    pub fn define(&mut self, name: String, value: Value){
        self.values.insert(name, value);
    }

    pub fn get(&mut self, name: String) -> Result<Value, LoxError> {
        match self.values.get(&name) {
            Some(val) => Ok(val.clone()),
            None => Err(LoxError::interpreter(Expr::Literal(LiteralExpr { value: Value::Nil }), format!("undefined variable {}", name)))
        }
    }
}
