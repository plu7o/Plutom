use core::fmt;
use std::cmp::Ordering;

use super::{
    bool::ObjBool, closures::ObjClosure, float::ObjFloat, functions::ObjFunction, integer::ObjInt,
    list::ObjList, map::ObjMap, native::ObjNative, none::ObjNone, string::ObjString,
    upvalue::ObjUpValue,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum ObjType {
    None(ObjNone),
    Bool(ObjBool),
    Int(ObjInt),
    Float(ObjFloat),
    Str(ObjString),
    List(ObjList),
    Map(ObjMap),
    Function(ObjFunction),
    Native(ObjNative),
    Closure(ObjClosure),
    UpValue(ObjUpValue),
}

impl fmt::Display for ObjType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

impl PartialOrd for ObjType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            ObjType::Float(float) => float.partial_cmp(other),
            ObjType::Int(int) => int.partial_cmp(other),
            ObjType::Str(str) => str.partial_cmp(other),
            _ => None,
        }
    }
}
