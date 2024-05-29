use super::object::ObjType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjBool {
    pub value: bool,
}

impl ObjBool {
    pub fn new(value: bool) -> Self {
        Self { value }
    }

    pub fn print(&self) {
        if self.value {
            print!("true")
        } else {
            print!("false")
        }
    }
}

impl PartialEq<ObjType> for ObjBool {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Bool(bool) => bool.value == self.value,
            _ => false,
        }
    }
}
