use crate::value::Value;
use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjUpValue {
    pub location: Rc<RefCell<Value>>,
    pub closed: Option<Rc<RefCell<Value>>>,
}

impl ObjUpValue {
    pub fn new(location: Rc<RefCell<Value>>) -> Self {
        Self {
            location,
            closed: None,
        }
    }

    pub fn print(&self) {
        println!("{:#?}", self);
    }
}

impl Hash for ObjUpValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Borrow the inner Value and hash it
        let value = self.location.borrow();
        value.hash(state);
    }
}
