use super::object::ObjType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjNone {}

impl ObjNone {
    pub fn new(value: bool) -> Self {
        Self {}
    }

    pub fn print(&self) {
        print!("none");
    }
}

impl PartialEq<ObjType> for ObjNone {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::None(none) => none == self,
            _ => false,
        }
    }
}
