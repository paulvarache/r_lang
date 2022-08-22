use std::rc::Rc;

use super::value::Value;

#[derive(Debug, Clone)]
pub enum Upvalue {
    Open(u8),
    Closed(Value),
}

// Stored on the heap using a Vec<Upvalue> inside a HashMap<usize, Vec<Upvalue>>. Key of the hasmap is
// closure id

// Opened upvalues are references by a (usize, usize) tuple (closure_id, upvalue slot) in a  linked list
// Sorted by target value address on the stack

// type UpValueListLink = Option<Box<UpvalueNode>>;

// pub struct UpvalueNode {
//     pub next: UpValueListLink,
//     pub upvalue_addr: (usize, usize),
//     pub local_addr: usize,
// }

// pub struct UpvalueList {
//     head: UpValueListLink
// }

// impl UpvalueList {
//     pub fn get_or_create(&mut self, addr: usize) {
//         let mut cursor = &self.head;
//         loop {
//             if let Some(c) = cursor {
//                 if c.local_addr <= addr {
//                     break;
//                 }
//                 cursor = &c.next;
//             } else {
//                 break;
//             }
//         }
//         if let Some(c) = cursor {
//             if c.local_addr == addr {
//                 return c.upvalue_addr;
//             }
//         }

//     }
// }