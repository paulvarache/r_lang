use r_ast::OpCodeEnum;

#[repr(u8)]
#[derive(Clone, OpCodeEnum)]
pub enum OpCode {
    Constant,
    ConstantLong,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Return
}
