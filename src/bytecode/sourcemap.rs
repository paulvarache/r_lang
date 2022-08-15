use std::collections::HashMap;

use crate::scanner::span::Span;

pub struct Sourcemap {
    spans: HashMap<usize, Span>,
}

impl Sourcemap {
    pub fn new() -> Self {
        Self {
            spans: HashMap::new(),
        }
    }

    pub fn describe_byte(&mut self, byte: usize, span: Span) {
        self.spans.insert(byte, span);
    }
}
