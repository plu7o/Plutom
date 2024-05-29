use std::cmp::Ordering;

use super::object::ObjType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjString {
    pub value: String,
}

impl ObjString {
    pub fn new(name: String) -> Self {
        Self { value: name }
    }

    pub fn print(&self) {
        print!("{}", self.value);
    }
}

impl Default for ObjString {
    fn default() -> Self {
        Self {
            value: String::new(),
        }
    }
}

impl PartialEq<ObjType> for ObjString {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Str(string) => string.value == self.value,
            _ => false,
        }
    }
}

impl PartialOrd for ObjString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl PartialOrd<ObjType> for ObjString {
    fn partial_cmp(&self, other: &ObjType) -> Option<Ordering> {
        match other {
            ObjType::Str(b) => self.value.partial_cmp(&b.value),
            _ => Some(Ordering::Equal),
        }
    }
}
