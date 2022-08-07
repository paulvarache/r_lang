use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use crate::{
    lox_error::{LoxError, LoxResult},
    scanner::{token::Token, value::Value},
};

type Link = Option<Rc<RefCell<Environment>>>;

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Link,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }
    pub fn new_with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }
    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }
    pub fn get(&self, name: &Token) -> LoxResult<Value> {
        if let Some(value) = self.values.get(&name.as_string()) {
            Ok(value.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            Err(LoxError::interpreter(
                name,
                format!("undefined variable {}", name.as_string()),
            ))
        }
    }
    pub fn assign(&mut self, name: &Token, value: Value) -> Result<(), LoxError> {
        if let Entry::Occupied(mut val) = self.values.entry(name.as_string()) {
            val.insert(value);
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(LoxError::interpreter(
                name,
                format!("undefined variable {}", name.as_string()),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::environment::Environment;
    use crate::scanner::token::Token;
    use crate::scanner::token_type::TokenType;
    use crate::scanner::value::Value;

    fn token_lex(ttype: TokenType, lexeme: &str) -> Token {
        Token::new(ttype, lexeme.to_string(), 0, None)
    }

    #[test]
    fn can_define() {
        let mut e = Environment::new();
        e.define("hello", Value::Number(9.0));

        assert!(e.values.contains_key("hello"));
        assert_eq!(
            e.values.get(&"hello".to_string()).unwrap(),
            &Value::Number(9.0)
        )
    }

    #[test]
    fn can_assign() {
        let mut e = Environment::new();
        e.define("hello", Value::Number(9.0));

        let tok = token_lex(TokenType::Identifier, "hello");

        assert!(e.assign(&tok, Value::Number(7.0)).is_ok());

        assert!(e.values.contains_key("hello"));
        assert_eq!(
            e.values.get(&"hello".to_string()).unwrap(),
            &Value::Number(7.0)
        )
    }

    #[test]
    fn fail_assign_to_undefined() {
        let mut e = Environment::new();

        let tok = token_lex(TokenType::Identifier, "nope");

        assert!(e.assign(&tok, Value::Number(7.0)).is_err());
    }

    #[test]
    fn get_when_undefined() {
        let e = Environment::new();
        let tok = token_lex(TokenType::Identifier, "nope");

        assert!(e.get(&tok).is_err());
    }

    #[test]
    fn can_read_from_enclosing() {
        let e = Rc::new(RefCell::new(Environment::new()));
        e.borrow_mut().define("hello", Value::Number(9.0));

        let f = Environment::new_with_enclosing(e);

        let tok = token_lex(TokenType::Identifier, "hello");

        assert_eq!(f.get(&tok).ok(), Some(Value::Number(9.0)))
    }

    #[test]
    fn can_assign_to_enclosing() {
        let e = Rc::new(RefCell::new(Environment::new()));
        e.borrow_mut().define("hello", Value::Number(9.0));

        let mut f = Environment::new_with_enclosing(e);

        let tok = token_lex(TokenType::Identifier, "hello");

        assert!(f.assign(&tok, Value::Number(7.0)).is_ok());

        assert_eq!(f.get(&tok).ok(), Some(Value::Number(7.0)));
    }
}
