use crate::bytecode::chunk::Chunk;
use crate::bytecode::opcode::OpCode;
use crate::bytecode::value::Value;
use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::RuntimeError;
use crate::error::RuntimeErrorCode;
use colored::*;

pub fn disassemble_chunk<T: ToString>(chunk: &Chunk, name: T) -> LoxResult<()> {
    println!("{:=^32}", name.to_string());
    let mut offset = 0;

    while offset < chunk.len() {
        offset = disassemble_chunk_instruction(chunk, offset)?;
    }
    Ok(())
}

pub fn disassemble_chunk_instruction(chunk: &Chunk, offset: usize) -> LoxResult<usize> {
    let instruction = chunk.get_at(offset);
    if let Some(instruction) = instruction {
        print_offset(offset);
        let instr: OpCode = instruction.into();
        print_opcode(instr.clone());
        let new_offset = match instr {
            OpCode::Return => offset + 1,
            OpCode::Constant => {
                print_constant(chunk, offset + 1)?;
                offset + 2
            }
            OpCode::DefineGlobal | OpCode::GlobalGet | OpCode::GlobalSet | OpCode::Class => {
                print_variable(chunk, offset + 1)?;
                offset + 2
            }
            OpCode::ConstantLong => {
                print_constant_long(chunk, offset + 1)?;
                offset + 4
            }
            OpCode::Popn
            | OpCode::LocalGet
            | OpCode::LocalSet
            | OpCode::UpvalueGet
            | OpCode::UpvalueSet => {
                let n = get_byte(chunk, offset + 1)?;
                print!("{}", n.to_string().bright_green());
                offset + 2
            }
            OpCode::Negate
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::Not
            | OpCode::Nil
            | OpCode::True
            | OpCode::Equal
            | OpCode::Greater
            | OpCode::Less
            | OpCode::Print
            | OpCode::Pop
            | OpCode::CloseUpvalue
            | OpCode::False => offset + 1,
            OpCode::JumpIfFalse | OpCode::Jump | OpCode::Loop => {
                let n1 = get_byte(chunk, offset + 1)?;
                let n2 = get_byte(chunk, offset + 2)?;

                let n = u16::from(n1) << 8 | u16::from(n2);

                print!("{}", n.to_string().bright_green());
                offset + 3
            }
            OpCode::Call => {
                let arg_count = get_byte(chunk, offset + 1)?;
                print!("{} args", arg_count.to_string().bright_green());
                offset + 2
            }
            OpCode::Closure => {
                let mut i = offset + 1;
                let value = get_constant(chunk, i)?;
                if let Value::Func(function) = value {
                    print!("{}", format!("<fn {}>", function.name()).bright_yellow());
                    for _ in 0..function.upvalue_count {
                        let is_local = get_byte(chunk, i + 1)?;
                        let addr = get_byte(chunk, i + 2)?;
                        println!("");
                        print_offset(i);
                        print_upvalue(is_local == 1, addr);
                        i += 2;
                    }
                }
                i
            }
        };
        println!("");
        Ok(new_offset)
    } else {
        Ok(offset + 1)
    }
}

fn print_upvalue(is_local: bool, addr: u8) {
    let nil = "";
    print!(
        "|{nil:<23}{}: {:04}",
        (if is_local { "local" } else { "upvalue" }).yellow(),
        addr
    );
}

fn print_offset(offset: usize) {
    print!("{}", format!("{offset:04} ").bright_white())
}
fn print_opcode<T: ToString>(name: T) {
    print!("{:<24}", name.to_string().bright_blue());
}

fn print_value(value: Value) {
    let formatted = format!("{value}");
    print!("{}", formatted.bright_yellow())
}

fn get_byte(chunk: &Chunk, index: usize) -> LoxResult<u8> {
    chunk.get_at(index).ok_or_else(|| {
        LoxError::Runtime(RuntimeError {
            func_id: 0,
            code: RuntimeErrorCode::OutOfChunkBounds,
            addr: index,
        })
    })
}

fn get_constant(chunk: &Chunk, index: usize) -> LoxResult<Value> {
    let constant_addr = get_byte(chunk, index)?;
    chunk.get_constant(constant_addr as usize).ok_or_else(|| {
        LoxError::Runtime(RuntimeError {
            func_id: 0,
            code: RuntimeErrorCode::OutOfConstantsBounds,
            addr: index,
        })
    })
}

fn print_variable(chunk: &Chunk, index: usize) -> LoxResult<()> {
    let value = get_constant(chunk, index)?;
    if let Value::String(value) = value {
        print!("{}", value.bright_purple())
    }
    Ok(())
}

fn print_constant(chunk: &Chunk, index: usize) -> LoxResult<()> {
    let value = get_constant(chunk, index)?;
    print_value(value);
    Ok(())
}
fn print_constant_long(chunk: &Chunk, index: usize) -> LoxResult<()> {
    let constant_addr = chunk.get_constant_long_addr(index)?;
    let value = chunk.get_constant(constant_addr as usize).ok_or_else(|| {
        LoxError::Runtime(RuntimeError {
            func_id: 0,
            code: RuntimeErrorCode::OutOfChunkBounds,
            addr: index,
        })
    })?;
    print_value(value);
    Ok(())
}
