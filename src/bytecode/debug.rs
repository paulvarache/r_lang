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
            OpCode::ConstantLong => {
                print_constant_long(chunk, offset + 1)?;
                offset + 4
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
            | OpCode::False => offset + 1,
        };
        println!("");
        Ok(new_offset)
    } else {
        Ok(offset + 1)
    }
}

fn print_offset(offset: usize) {
    print!("{}", format!("{offset:04} ").bright_white())
}
fn print_opcode<T: ToString>(name: T) {
    print!("{:<24}", name.to_string().bright_blue());
}

fn print_value(value: Value) {
    print!("{}", format!("{:?}", value).bright_yellow())
}

fn print_constant(chunk: &Chunk, index: usize) -> LoxResult<()> {
    let constant_addr = chunk.get_at(index).ok_or_else(|| {
        LoxError::Runtime(RuntimeError {
            code: RuntimeErrorCode::OutOfChunkBounds,
            addr: index,
        })
    })?;
    let value = chunk.get_constant(constant_addr as usize).ok_or_else(|| {
        LoxError::Runtime(RuntimeError {
            code: RuntimeErrorCode::OutOfConstantsBounds,
            addr: index,
        })
    })?;
    print_value(value);
    Ok(())
}
fn print_constant_long(chunk: &Chunk, index: usize) -> LoxResult<()> {
    let constant_addr = chunk.get_constant_long_addr(index)?;
    let value = chunk.get_constant(constant_addr as usize).ok_or_else(|| {
        LoxError::Runtime(RuntimeError {
            code: RuntimeErrorCode::OutOfChunkBounds,
            addr: index,
        })
    })?;
    print_value(value);
    Ok(())
}
