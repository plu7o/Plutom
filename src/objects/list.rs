use crate::value::Value;

use super::object::ObjType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjList {
    pub items: Vec<Value>,
}

impl ObjList {
    pub fn new(items: Vec<Value>) -> Self {
        Self { items }
    }

    pub fn print(&self) {
        print!("[");
        for (i, item) in self.items.iter().enumerate() {
            item.print();
            if i != self.items.len() - 1 {
                print!(", ");
            }
        }
        print!("]")
    }
}

impl PartialEq<ObjType> for ObjList {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::List(list) => list.items == self.items,
            _ => false,
        }
    }
}
