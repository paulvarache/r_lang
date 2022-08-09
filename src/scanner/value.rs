use std::{fmt, ops, cmp::{self, Ordering}, rc::Rc};

use crate::{function::LoxFunction, callable::{LoxCallable, Callable}};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Func(Rc<Callable>),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "String({})", s),
            Value::Number(n) => write!(f, "Number({})", n),
            Value::Bool(b) => write!(f, "Bool({})", b),
            Value::Func(_) => write!(f, "Func()"),
            Value::Nil => write!(f, "Nil"),
        }
    }
}

impl ops::Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        if let Value::Number(l) = self {
            if let Value::Number(r) = rhs {
                return Value::Number(l - r);
            }
        }
        Value::Nil
    }
}

impl ops::Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        if let Value::Number(l) = self {
            if let Value::Number(r) = rhs {
                return Value::Number(l + r);
            }
        }
        if let Value::String(l) = self {
            if let Value::String(r) = rhs {
                return Value::String(format!("{}{}", l, r));
            }
        }
        Value::Nil
    }
}

impl ops::Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        if let Value::Number(l) = self {
            if let Value::Number(r) = rhs {
                return Value::Number(l / r);
            }
        }
        Value::Nil
    }
}
impl ops::Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        if let Value::Number(l) = self {
            if let Value::Number(r) = rhs {
                return Value::Number(l * r);
            }
        }
        Value::Nil
    }
}
impl ops::Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        if let Value::Number(l) = self {
            return Value::Number(-l);
        }
        Value::Nil
    }
}
impl cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (Value::Nil, Value::Nil) => Some(Ordering::Equal),
            (Value::Number(left), Value::Number(right)) => left.partial_cmp(right),
            (Value::String(left), Value::String(right)) => left.partial_cmp(right),
            (Value::Bool(left), Value::Bool(right)) => left.partial_cmp(right),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::Value;

    #[test]
    fn test_value_sub() {
        assert_eq!(Value::Number(7.) - Value::Number(2.), Value::Number(5.));
        assert_eq!(
            Value::String("hi".to_string()) - Value::Number(2.),
            Value::Nil
        );
        assert_eq!(Value::Number(7.) - Value::Bool(true), Value::Nil);
        assert_eq!(Value::Number(7.) - Value::Nil, Value::Nil);
    }
    #[test]
    fn test_value_add() {
        assert_eq!(Value::Number(7.) + Value::Number(2.), Value::Number(9.));
        assert_eq!(
            Value::String("hi".to_string()) + Value::Number(2.),
            Value::Nil
        );
        assert_eq!(
            Value::String("hi".to_string()) + Value::String("hello".to_string()),
            Value::String("hihello".to_string())
        );
        assert_eq!(Value::Number(7.) + Value::Bool(true), Value::Nil);
        assert_eq!(Value::Number(7.) + Value::Nil, Value::Nil);
    }
    #[test]
    fn test_value_div() {
        assert_eq!(Value::Number(7.) / Value::Number(2.), Value::Number(3.5));
        assert_eq!(
            Value::String("hi".to_string()) / Value::Number(2.),
            Value::Nil
        );
        assert_eq!(Value::Number(7.) / Value::Bool(true), Value::Nil);
        assert_eq!(Value::Number(7.) / Value::Nil, Value::Nil);
    }
    #[test]
    fn test_value_mul() {
        assert_eq!(Value::Number(7.) * Value::Number(2.), Value::Number(14.0));
        assert_eq!(
            Value::String("hi".to_string()) * Value::Number(2.),
            Value::Nil
        );
        assert_eq!(Value::Number(7.) * Value::Bool(true), Value::Nil);
        assert_eq!(Value::Number(7.) * Value::Nil, Value::Nil);
    }

    #[test]
    fn test_value_gt() {
        assert!(Value::Number(7.) > Value::Number(2.));
        assert!(!(Value::String("hi".to_string()) > Value::Number(2.))); 
    }
    #[test]
    fn test_value_eq() {
        assert!(Value::Number(7.) == Value::Number(7.));
        assert!(!(Value::String("hi".to_string()) == Value::Number(2.))); 
        assert!(Value::String("hi".to_string()) == Value::String("hi".to_string())); 
    }
}
