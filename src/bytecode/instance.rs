use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::class::Class;
use super::value::Value;

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Rc<Class>,
    pub fields: RefCell<HashMap<String, Value>>,
}

impl Instance {
    pub fn new(class: &Rc<Class>) -> Self {
        Self {
            class: Rc::clone(class),
            fields: RefCell::new(HashMap::new()),
        }
    }
    pub fn get_field(&self, name: &Value) -> Option<Value> {
        if let Value::String(name) = name {
            return self.fields.borrow().get(name).map(|v| v.clone());
        }
        None
    }
    pub fn set_field(&self, name: &Value, value: &Value) {
        if let Value::String(name) = name {
            self.fields.borrow_mut().insert(name.clone(), value.clone());
        }
    }
}
