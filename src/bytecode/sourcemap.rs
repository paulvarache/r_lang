use std::collections::HashMap;

use crate::scanner::span::Span;

#[derive(Clone, Debug)]
pub struct Sourcemap {
    spans: HashMap<usize, Span>,
}

impl Sourcemap {
    pub fn new() -> Self {
        Self {
            spans: HashMap::new(),
        }
    }

    pub fn describe_byte(&mut self, addr: usize, span: Span) {
        self.spans.insert(addr, span);
    }
    pub fn locate_byte(&self, addr: usize) -> Option<&Span> {
        self.spans.get(&addr)
    }
}
