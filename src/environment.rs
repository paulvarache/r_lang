use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::scanner::token::Token;
use crate::scanner::value::Value;

type Link = Option<Rc<RefCell<Environment>>>;

#[derive(Clone, Debug)]
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
    pub fn get(&self, name: &Token) -> Option<Value> {
        if let Some(value) = self.values.get(&name.as_string()) {
            Some(value.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.as_ref().borrow().get(name)
        } else {
            None
        }
    }
    pub fn assign(&mut self, name: &Token, value: Value) -> bool {
        if let Entry::Occupied(mut val) = self.values.entry(name.as_string()) {
            val.insert(value);
            return true;
        } else if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow_mut().assign(name, value);
        } else {
            return false;
        }
    }
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.values.iter().try_for_each(|(name, value)| writeln!(f, "> {} => {}", name, value))?;
        if let Some(enc) = &self.enclosing {
            writeln!(f, ">  |")?;
            writeln!(f, ">  v")?;
            let t = enc.as_ref().borrow();
            writeln!(f, ">  {}", t)
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::environment::Environment;
    use crate::scanner::token::Span;
    use crate::scanner::token::Token;
    use crate::scanner::token_type::TokenType;
    use crate::scanner::value::Value;

    fn token_lex(ttype: TokenType, lexeme: &str) -> Token {
        Token::new(ttype, lexeme.to_string(), Span::new(0, 0, 0, 0), None)
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

        assert!(e.assign(&tok, Value::Number(7.0)));

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

        assert!(!e.assign(&tok, Value::Number(7.0)));
    }

    #[test]
    fn get_when_undefined() {
        let e = Environment::new();
        let tok = token_lex(TokenType::Identifier, "nope");

        assert!(e.get(&tok).is_none());
    }

    #[test]
    fn can_read_from_enclosing() {
        let e = Rc::new(RefCell::new(Environment::new()));
        e.borrow_mut().define("hello", Value::Number(9.0));

        let f = Environment::new_with_enclosing(e);

        let tok = token_lex(TokenType::Identifier, "hello");

        assert_eq!(f.get(&tok), Some(Value::Number(9.0)))
    }

    #[test]
    fn can_assign_to_enclosing() {
        let e = Rc::new(RefCell::new(Environment::new()));
        e.borrow_mut().define("hello", Value::Number(9.0));

        let mut f = Environment::new_with_enclosing(e);

        let tok = token_lex(TokenType::Identifier, "hello");

        assert!(f.assign(&tok, Value::Number(7.0)));

        assert_eq!(f.get(&tok), Some(Value::Number(7.0)));
    }
}
