use std::fmt::Display;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Neg;
use std::ops::Sub;
use std::rc::Rc;

use super::class::Class;
use super::closure::Closure;
use super::function::Function;
use super::instance::Instance;

pub type NativeFunction = fn(args: Vec<Value>) -> Value;

#[derive(Clone, Debug)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Func(Rc<Function>),
    Closure(Rc<Closure>),
    Native(NativeFunction),
    Class(Rc<Class>),
    Instance(Rc<Instance>),
    BoundMethod(Rc<Value>, Rc<Closure>),
    Nil,
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Bool(false) | Value::Nil)
    }
}

macro_rules! expr {
    ($e:expr) => {
        $e
    };
}

macro_rules! binary_op {
    ($i:ident, $j:ident, $op:tt) => {
        impl $i for &Value {
            type Output = Value;

            fn $j(self, rhs: Self) -> Value {
                match (self, rhs) {
                    (&Value::Number(a), Value::Number(b)) => Value::Number(expr!(a $op b)),
                    _ => panic!("Invalid operation")
                }
            }
        }
    };
}

impl Add for &Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Value {
        match (self.clone(), rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
            (Value::String(a), Value::String(b)) => Value::String(a + b),
            _ => panic!("Invalid operation"),
        }
    }
}

binary_op!(Sub, sub, -);
binary_op!(Mul, mul, *);
binary_op!(Div, div, /);

impl Neg for &Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            &Value::Number(n) => Value::Number(-n),
            _ => panic!("Invalid operation"),
        }
    }
}

impl PartialEq for &Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Value::Number(a), &Value::Number(b)) => a == b,
            (&Value::Bool(a), &Value::Bool(b)) => a == b,
            (&Value::String(a), &Value::String(b)) => a == b,
            (&Value::Nil, &Value::Nil) => true,
            _ => false,
        }
    }
}

impl PartialOrd for &Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (&Value::Number(a), &Value::Number(b)) => a.partial_cmp(b),
            _ => panic!("Invalid operation"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
            Value::Func(func) => write!(f, "<fn {}>", func.name()),
            Value::Closure(closure) => write!(f, "<fn {}>", closure.function.name()),
            Value::Native(_) => write!(f, "<native fb>"),
            Value::Class(class) => write!(f, "<class {}>", class.name),
            Value::Instance(instance) => write!(f, "<{} instance>", instance.class.name),
            Value::BoundMethod(value, closure) => {
                if let Value::Instance(instance) = value.as_ref() {
                    return write!(
                        f,
                        "<fn {}> bound to <{} instance>",
                        closure.function.name(),
                        instance.class.name
                    );
                }
                Ok(())
            }
        }
    }
}
