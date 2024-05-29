use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
    rc::Rc,
};

use super::{functions::ObjFunction, object::ObjType, upvalue::ObjUpValue};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjClosure {
    pub function: ObjFunction,
    pub upvalues: Vec<Rc<RefCell<ObjUpValue>>>,
    pub upvalue_count: usize,
}

impl ObjClosure {
    pub fn new(function: ObjFunction) -> Self {
        let upvalue_count = function.upvalue_count;
        Self {
            function,
            upvalues: Vec::new(),
            upvalue_count,
        }
    }

    pub fn print(&self) {
        print!("<Closure>");
    }
}

impl Hash for ObjClosure {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.function.hash(state);
        for upvalue in &self.upvalues {
            let value = upvalue.borrow();
            value.hash(state);
        }
        self.upvalue_count.hash(state);
    }
}

impl PartialEq<ObjType> for ObjClosure {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Closure(closure) => closure.function == self.function,
            _ => false,
        }
    }
}
