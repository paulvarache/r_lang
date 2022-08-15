use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Neg;
use std::ops::Sub;

#[derive(Clone, Debug)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Bool(false)| Value::Nil)
    }
}

macro_rules! expr {
    ($e:expr) => {
        $e
    }
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
            _ => panic!("Invalid operation")
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
            _ => panic!("Invalid operation")
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
            _ => false
        }
    }
}

impl PartialOrd for &Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (&Value::Number(a), &Value::Number(b)) => a.partial_cmp(b),
            _ => panic!("Invalid operation")
        }
    }
}