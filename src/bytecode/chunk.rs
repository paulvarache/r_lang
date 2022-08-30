use crate::bytecode::opcode::OpCode;
use crate::bytecode::value::Value;
use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::RuntimeError;

#[derive(Clone, Debug)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
        }
    }
    pub fn write(&mut self, byte: u8) {
        self.code.push(byte)
    }
    pub fn write_at(&mut self, addr: usize, byte: u8) {
        self.code[addr] = byte;
    }
    pub fn write_u24(&mut self, n: u32) {
        let bytes = n.to_le_bytes();
        self.write_n(&[bytes[0], bytes[1], bytes[2]]);
    }
    pub fn write_n(&mut self, bytes: &[u8]) {
        for byte in bytes {
            self.write(*byte);
        }
    }
    pub fn write_opcode(&mut self, opcode: OpCode) {
        self.write(opcode.into())
    }
    pub fn add_constant(&mut self, value: Value) -> u32 {
        self.constants
            .iter()
            .position(|v| &value == v)
            .unwrap_or_else(|| {
                let addr = self.constants.len();
                self.constants.push(value);
                addr
            }) as u32
    }
    pub fn get_constant(&self, index: usize) -> Option<&Value> {
        self.constants.get(index)
    }
    pub fn free(&mut self) {
        self.code = Vec::new();
        self.constants = Vec::new();
    }
    pub fn len(&self) -> usize {
        self.code.len()
    }
    pub fn get_at(&self, index: usize) -> Option<u8> {
        self.code.get(index).map(|c| c.clone())
    }
    /// Get the usize code address for a long constant
    /// This will read the 3 bytes at the given address
    /// merge it as a u24 (u32 really) and use it to
    pub fn get_constant_long_addr(&self, index: usize) -> LoxResult<usize> {
        let indices = [index, index + 1, index + 2];
        let mut values: [u8; 4] = [0, 0, 0, 0];
        for (i, index) in indices.iter().enumerate() {
            values[i] = self.get_at(*index).ok_or_else(|| {
                LoxError::Runtime(RuntimeError {
                    func_id: 0,
                    code: crate::error::RuntimeErrorCode::OutOfChunkBounds,
                    addr: *index,
                })
            })?
        }

        let addr = u32::from_le_bytes(values);
        return Ok(addr as usize);
    }
}
