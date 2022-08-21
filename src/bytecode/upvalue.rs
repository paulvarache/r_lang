#[derive(Debug, Clone)]
pub struct Upvalue {
    pub is_local: bool,
    pub addr: u8,
}

impl Upvalue {
    pub fn new(is_local: bool, addr: u8) -> Self {
        Self { is_local, addr }
    }
}
