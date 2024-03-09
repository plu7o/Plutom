#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ObjType {
    String(ObjString),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjString {
    pub len: usize,
    pub chars: String,
}
