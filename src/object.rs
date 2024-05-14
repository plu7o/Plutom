use crate::{chunk::Chunk, value::Value};

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ObjType {
    String(ObjString),
    Function(ObjFunction),
    Native(ObjNative),
    Closure(ObjClosure),
    List(ObjList),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjString {
    pub len: usize,
    pub chars: String,
}

impl ObjString {
    pub fn new(name: String) -> Self {
        Self {
            len: name.len(),
            chars: name,
        }
    }
}

impl Default for ObjString {
    fn default() -> Self {
        Self {
            len: 0,
            chars: String::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ObjFunction {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<ObjString>,
}

impl ObjFunction {
    pub fn new() -> Self {
        Self {
            arity: 0,
            name: None,
            chunk: Chunk::init(),
        }
    }
}

type NativeFn = fn(usize, &[Value]) -> Result<Value, String>;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ObjNative {
    pub function: NativeFn,
}

impl ObjNative {
    pub fn new(function: NativeFn) -> Self {
        Self { function }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ObjClosure {
    pub function: ObjFunction,
}

impl ObjClosure {
    pub fn new(function: ObjFunction) -> Self {
        Self { function }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ObjList {
    pub len: usize,
    pub items: Vec<Value>,
}

impl ObjList {
    pub fn new(items: Vec<Value>) -> Self {
        Self {
            len: items.len(),
            items,
        }
    }
}
