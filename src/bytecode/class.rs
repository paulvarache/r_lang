use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::closure::Closure;

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    methods: RefCell<HashMap<String, Rc<Closure>>>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            methods: RefCell::new(HashMap::new()),
        }
    }
    pub fn add_method(&self, name: String, closure: &Rc<Closure>) {
        self.methods.borrow_mut().insert(name, Rc::clone(closure));
    }
    pub fn get_method(&self, name: &String) -> Option<Rc<Closure>> {
        self.methods.borrow().get(name).map(|c| c.clone())
    }
    pub fn inherit_from(&self, superclass: &Rc<Class>) {
        self.methods.borrow_mut().extend(
            superclass
                .methods
                .borrow()
                .iter()
                .map(|(k, v)| (k.clone(), v.clone())),
        );
    }
}
