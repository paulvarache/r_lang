use std::collections::HashMap;

use super::value::Value;

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    fields: HashMap<String, Value>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            fields: HashMap::new(),
        }
    }
}
