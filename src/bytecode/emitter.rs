use crate::scanner::span::Span;

use super::chunk::Chunk;
use super::opcode::OpCode;
use super::sourcemap::Sourcemap;
use super::value::Value;

#[derive(Clone, Debug)]
pub struct Emitter {
    chunk: Box<Chunk>,
    sourcemaps: Sourcemap,
}

impl Emitter {
    pub fn new() -> Self {
        Self {
            chunk: Box::new(Chunk::new()),
            sourcemaps: Sourcemap::new(),
        }
    }
    fn write(&mut self, byte: u8, span: Span) {
        let index = self.chunk.len();
        self.chunk.write(byte);
        self.sourcemaps.describe_byte(index, span);
    }
    pub fn emit(&mut self, byte: u8, span: Span) {
        self.write(byte, span);
    }

    pub fn emit_constant(&mut self, value: Value, span: Span) {
        let const_addr = self.chunk.add_constant(value);
        self.write(OpCode::Constant.into(), span);
        self.write(const_addr, span);
    }

    pub fn get_chunk(&self) -> Box<Chunk> {
        Box::clone(&self.chunk)
    }
    pub fn locate_byte(&self, addr: usize) -> Option<Span> {
        self.sourcemaps.locate_byte(addr).map(|s| s.clone())
    }

    pub fn make_constant(&mut self, value: Value) -> u8 {
        self.chunk.add_constant(value)
    }

    pub fn patch(&mut self, addr: usize, byte: u8) {
        self.chunk.write_at(addr, byte);
    }
    pub fn current_addr(&self) -> usize {
        self.chunk.len()
    }
}
