use std::rc::Rc;

use crate::callable::LoxCallable;

pub struct LoxNative {
    func: Rc<dyn LoxCallable>
}
