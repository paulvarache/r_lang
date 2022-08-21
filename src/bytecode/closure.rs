use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use super::function::Function;

static CLOSURE_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Debug)]
pub struct Closure {
    pub id: usize,
    pub function: Rc<Function>,
}

impl Closure {
    pub fn new(function: &Rc<Function>) -> Self {
        Self {
            id: CLOSURE_COUNTER.fetch_add(1, Ordering::SeqCst),
            function: Rc::clone(function),
        }
    }
}
