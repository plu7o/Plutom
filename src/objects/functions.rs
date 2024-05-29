use core::fmt;

use crate::compiler::chunk::Chunk;

use super::{object::ObjType, string::ObjString};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjFunction {
    pub name: Option<ObjString>,
    pub upvalue_count: usize,
    pub arity: usize,
    pub chunk: Chunk,
}

impl ObjFunction {
    pub fn new() -> Self {
        Self {
            name: None,
            upvalue_count: 0,
            arity: 0,
            chunk: Chunk::init(),
        }
    }

    pub fn print(&self) {
        match &self.name {
            Some(name) => print!("<fn {}>", name.value),
            None => print!("<Script>"),
        }
    }
}

impl fmt::Display for ObjFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.name {
            Some(name) => write!(f, "<fn {}>", name.value),
            None => write!(f, "<Script>"),
        }
    }
}

impl PartialEq<ObjType> for ObjFunction {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Function(function) => match &function.name {
                Some(name) => {
                    if let Some(self_name) = &self.name {
                        name.value == self_name.value
                    } else {
                        false
                    }
                }
                None => self.name.is_none(),
            },
            _ => false,
        }
    }
}
