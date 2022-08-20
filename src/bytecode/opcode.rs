use r_ast::OpCodeEnum;

#[repr(u8)]
#[derive(Clone, OpCodeEnum)]
pub enum OpCode {
    Constant,
    ConstantLong,
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Return,
    Print,
    Pop,
    Popn,
    DefineGlobal,
    GlobalGet,
    GlobalSet,
    LocalGet,
    LocalSet,
    JumpIfFalse,
    Jump,
    Loop,
    Call,
}
