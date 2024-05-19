use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

use crate::value::Value;

use super::object::ObjType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjMap {
    pub dict: HashMap<Value, Value>,
}

impl ObjMap {
    pub fn new(dict: HashMap<Value, Value>) -> Self {
        Self { dict }
    }

    pub fn print(&self) {
        println!("{{");
        for (key, value) in &self.dict {
            print!("    {}", key);
            print!(": {}", value);
            println!();
        }
        print!("}}");
    }
}

impl Hash for ObjMap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (key, value) in &self.dict {
            key.hash(state);
            value.hash(state);
        }
    }
}

impl PartialEq<ObjType> for ObjMap {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Map(map) => map.dict == self.dict,
            _ => false,
        }
    }
}
