use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::FunctionStmt;
use crate::callable::Callable;
use crate::callable::LoxCallable;
use crate::environment::Environment;
use crate::function::LoxFunction;
use crate::interpreter::Interpreter;
use crate::error::LoxResult;
use crate::scanner::token::Token;
use crate::scanner::value::Value;

pub const CONSTRUCTOR_NAME: &str = "init";

#[derive(Debug)]
pub struct LoxClass {
    pub name: Token,
    methods: Rc<RefCell<HashMap<String, Rc<LoxFunction>>>>,
    superclass: Option<Rc<LoxClass>>,
}

impl LoxClass {
    pub fn new(
        name: &Token,
        methods: &Vec<Rc<FunctionStmt>>,
        superclass: Option<Rc<LoxClass>>,
        e: &Rc<RefCell<Environment>>,
    ) -> Self {
        let mut methods_hash = HashMap::new();
        for m in methods {
            let fun = LoxFunction::new(m, e, m.name.lexeme == CONSTRUCTOR_NAME);
            methods_hash.insert(m.name.lexeme.clone(), Rc::new(fun));
        }
        Self {
            name: name.clone(),
            methods: Rc::new(RefCell::new(methods_hash)),
            superclass,
        }
    }
    fn instantiate(
        &self,
        interpreter: &Interpreter,
        args: Vec<Value>,
        class: Rc<LoxClass>,
    ) -> LoxResult<Value> {
        let inst = Rc::new(LoxInstance::new(&self.name, class));
        if let Some(init_func) = self.find_method(CONSTRUCTOR_NAME) {
            let bound = self.bind_method(init_func, &inst);
            bound.func.call(interpreter, args, None)?;
        }
        Ok(Value::Instance(inst))
    }
    pub fn find_method(&self, name: &str) -> Option<Rc<LoxFunction>> {
        self.methods.borrow().get(name).map(|v| v.clone())
    }
    pub fn bind_method(&self, fun: Rc<LoxFunction>, inst: &Rc<LoxInstance>) -> Callable {
        let func = Rc::new(fun.bind(inst));
        Callable { func: func }
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: Vec<Value>,
        class: Option<Rc<LoxClass>>,
    ) -> LoxResult<Value> {
        self.instantiate(interpreter, args, class.unwrap())
    }

    fn arity(&self) -> usize {
        if let Some(init_func) = self.methods.borrow().get(CONSTRUCTOR_NAME) {
            init_func.arity()
        } else {
            0
        }
    }
}

impl PartialEq for LoxClass {
    fn eq(&self, other: &Self) -> bool {
        self.name.ttype == other.name.ttype
    }
}

#[derive(Debug)]
pub struct LoxInstance {
    fields: RefCell<HashMap<String, Value>>,
    class: Rc<LoxClass>,
    pub name: Token,
}

impl LoxInstance {
    pub fn new(name: &Token, class: Rc<LoxClass>) -> Self {
        Self {
            fields: RefCell::new(HashMap::new()),
            class,
            name: name.clone(),
        }
    }
    pub fn get(&self, token: &Token, inst: &Rc<LoxInstance>) -> Option<Value> {
        self.fields
            .borrow()
            .get(&token.lexeme)
            .map(|v| v.clone())
            .or_else(|| {
                self.class
                    .find_method(&token.lexeme)
                    .map(|f| Value::Func(Rc::new(self.class.bind_method(f, inst))))
                    .or_else(|| {
                        if let Some(superclass) = &self.class.superclass {
                            superclass
                                .find_method(&token.lexeme)
                                .map(|f| Value::Func(Rc::new(superclass.bind_method(f, inst))))
                        } else {
                            None
                        }
                    })
            })
    }
    pub fn set(&self, token: &Token, value: &Value) {
        self.fields
            .borrow_mut()
            .insert(token.lexeme.clone(), value.clone());
    }
}

impl PartialEq for LoxInstance {
    fn eq(&self, other: &Self) -> bool {
        self.name.ttype == other.name.ttype
    }
}
