use crate::scanner::token::Token;

pub struct Local {
    pub depth: Option<usize>,
    pub name: Token,
    pub is_captured: bool,
}

impl Local {
    pub fn new(name: &Token) -> Self {
        Self {
            depth: None,
            name: name.clone(),
            is_captured: false,
        }
    }
    pub fn initialize(&mut self, depth: usize) {
        self.depth = Some(depth)
    }
}
