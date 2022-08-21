use super::value::Value;

#[derive(Debug, Clone)]
pub enum Upvalue {
    None,
    Open(u8),
    Closed(Value),
}
