use std::{cell::RefCell, rc::Rc};

use crate::value::Value;

use super::object::ObjType;

pub type NativeFn = fn(usize, &[Rc<RefCell<Value>>]) -> Result<Value, String>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjNative {
    pub function: NativeFn,
}

impl ObjNative {
    pub fn new(function: NativeFn) -> Self {
        Self { function }
    }

    pub fn print(&self) {
        print!("<NativeFn>");
    }
}

impl PartialEq<ObjType> for ObjNative {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Native(native) => native.function == self.function,
            _ => false,
        }
    }
}
