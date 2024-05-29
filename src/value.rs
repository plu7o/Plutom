use core::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::objects::{
    bool::ObjBool,
    closures::ObjClosure,
    float::ObjFloat,
    functions::ObjFunction,
    integer::ObjInt,
    list::ObjList,
    map::ObjMap,
    native::{NativeFn, ObjNative},
    object::ObjType,
    string::ObjString,
    upvalue::ObjUpValue,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueType {
    Object(ObjType),
    None,
}

#[derive(Debug, Clone, Eq, Hash)]
pub struct Value {
    pub _type: ValueType,
}

macro_rules! is_type {
    ($val_type:expr, $object:path) => {{
        match $val_type {
            ValueType::Object(obj) => match obj {
                $object(_) => true,
                _ => false,
            },
            _ => false,
        }
    }};
}

macro_rules! as_type {
    ($val_type:expr, $object:path, $msg:expr) => {{
        match $val_type {
            ValueType::Object(obj_type) => match obj_type {
                $object(obj) => obj,
                _ => panic!("ValueError: {} but got {:#?}", $msg, obj_type),
            },
            _ => panic!("ValueError: {} but got {:#?}", $msg, $val_type),
        }
    }};
}

#[allow(dead_code)]
impl Value {
    pub fn new(value: ValueType) -> Self {
        Self { _type: value }
    }

    pub fn none() -> Value {
        Value::new(ValueType::None)
    }

    pub fn object(object: ObjType) -> Value {
        Value::new(ValueType::Object(object))
    }

    pub fn bool(value: bool) -> Value {
        Value::object(ObjType::Bool(ObjBool::new(value)))
    }

    pub fn int(value: i64) -> Value {
        Value::object(ObjType::Int(ObjInt::new(value)))
    }

    pub fn float(value: f64) -> Value {
        Value::object(ObjType::Float(ObjFloat::new(value)))
    }

    pub fn string(string: String) -> Value {
        Value::object(ObjType::Str(ObjString::new(string)))
    }

    pub fn function(function: ObjFunction) -> Value {
        Value::object(ObjType::Function(function))
    }

    pub fn closure(closure: ObjClosure) -> Value {
        Value::object(ObjType::Closure(closure))
    }

    pub fn native(function: NativeFn) -> Value {
        Value::object(ObjType::Native(ObjNative::new(function)))
    }

    pub fn list(items: Vec<Value>) -> Value {
        Value::object(ObjType::List(ObjList::new(items)))
    }

    pub fn map(dict: HashMap<Value, Value>) -> Value {
        Value::object(ObjType::Map(ObjMap::new(dict)))
    }

    pub fn upvalue(loccation: Rc<RefCell<Value>>) -> Value {
        Value::object(ObjType::UpValue(ObjUpValue::new(loccation)))
    }

    pub fn as_none(&self) -> &ValueType {
        match &self._type {
            ValueType::None => &self._type,
            _ => panic!("ValueError: Expected None but got {:#?}", self._type),
        }
    }

    pub fn as_object(&self) -> &ObjType {
        match &self._type {
            ValueType::Object(obj) => obj,
            _ => panic!("ValueError: Expected Object but got {:#?}", self._type),
        }
    }

    pub fn as_mut_object(&mut self) -> &mut ObjType {
        let val_type = &mut self._type;
        match val_type {
            ValueType::Object(obj) => obj,
            _ => panic!("ValueError: Expected Object but got {:#?}", val_type),
        }
    }

    pub fn as_bool(&self) -> &ObjBool {
        as_type!(&self._type, ObjType::Bool, "Expected ObjBool")
    }

    pub fn as_int(&self) -> &ObjInt {
        as_type!(&self._type, ObjType::Int, "Expected ObjInt")
    }

    pub fn as_float(&self) -> &ObjFloat {
        as_type!(&self._type, ObjType::Float, "Expected ObjFloat")
    }

    pub fn as_string(&self) -> &ObjString {
        as_type!(&self._type, ObjType::Str, "Expected ObjString")
    }

    pub fn as_function(&self) -> &ObjFunction {
        as_type!(&self._type, ObjType::Function, "Expected ObjFunction")
    }

    pub fn as_closure(&self) -> &ObjClosure {
        as_type!(&self._type, ObjType::Closure, "Expected ObjClosure")
    }

    pub fn as_native(&self) -> &ObjNative {
        as_type!(&self._type, ObjType::Native, "Expected ObjNative")
    }

    pub fn as_list(&self) -> &ObjList {
        as_type!(&self._type, ObjType::List, "Expected ObjList")
    }

    pub fn as_map(&self) -> &ObjMap {
        as_type!(&self._type, ObjType::Map, "Expected ObjList")
    }

    pub fn as_upvalue(&self) -> &ObjUpValue {
        as_type!(&self._type, ObjType::UpValue, "Expected ObjUpValue")
    }

    pub fn is_none(&self) -> bool {
        match self._type {
            ValueType::None => true,
            _ => false,
        }
    }

    pub fn is_object(&self) -> bool {
        match self._type {
            ValueType::Object(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        is_type!(&self._type, ObjType::Bool)
    }

    pub fn is_int(&self) -> bool {
        is_type!(&self._type, ObjType::Int)
    }

    pub fn is_float(&self) -> bool {
        is_type!(&self._type, ObjType::Float)
    }

    pub fn is_number(&self) -> bool {
        is_type!(&self._type, ObjType::Int) || is_type!(&self._type, ObjType::Float)
    }

    pub fn is_string(&self) -> bool {
        is_type!(&self._type, ObjType::Str)
    }

    pub fn is_function(&self) -> bool {
        is_type!(&self._type, ObjType::Function)
    }

    pub fn is_closure(&self) -> bool {
        is_type!(&self._type, ObjType::Closure)
    }

    pub fn is_native(&self) -> bool {
        is_type!(&self._type, ObjType::Native)
    }

    pub fn is_list(&self) -> bool {
        is_type!(&self._type, ObjType::List)
    }

    pub fn is_map(&self) -> bool {
        is_type!(&self._type, ObjType::Map)
    }

    pub fn is_upvalue(&self) -> bool {
        is_type!(&self._type, ObjType::UpValue)
    }

    pub fn print(&self) {
        match &self._type {
            ValueType::Object(object) => match object {
                ObjType::Bool(bool) => bool.print(),
                ObjType::Int(integer) => integer.print(),
                ObjType::Float(float) => float.print(),
                ObjType::Str(string) => string.print(),
                ObjType::Function(func) => func.print(),
                ObjType::Native(native) => native.print(),
                ObjType::Closure(closure) => closure.print(),
                ObjType::List(list) => list.print(),
                ObjType::Map(map) => map.print(),
                ObjType::UpValue(upvalue) => upvalue.print(),
                ObjType::None(_) => print!("none"),
            },
            ValueType::None => print!("none"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self._type {
            ValueType::Object(object) => match object {
                ObjType::Bool(bool) => write!(f, "Bool({})", bool.value),
                ObjType::Int(int) => write!(f, "Int({})", int.value),
                ObjType::Float(float) => write!(f, "Float({})", float.value.raw),
                ObjType::Str(string) => write!(f, "String('{}')", string.value),
                ObjType::Function(func) => write!(f, "{}", func),
                ObjType::Native(_) => write!(f, "<NativeFn>"),
                ObjType::Closure(closure) => write!(f, "<Closure{}>", closure.function),
                ObjType::List(list) => {
                    let mut format = "List[".to_string();
                    for (i, item) in list.items.iter().enumerate() {
                        format.push_str(&format!("{}", item));
                        if i != list.items.len() - 1 {
                            format.push_str(", ");
                        }
                    }
                    format.push_str("]");
                    write!(f, "{}", format)
                }
                ObjType::Map(map) => {
                    let mut format = "Map{ ".to_string();
                    for (key, value) in &map.dict {
                        format.push_str(&format!("{}", key));
                        format.push_str(&format!(": {}, ", value));
                    }
                    format.push_str("}");
                    write!(f, "{}", format)
                }
                ObjType::UpValue(upvalue) => write!(f, "{:#?}", upvalue),
                ObjType::None(_) => write!(f, "None"),
            },
            ValueType::None => write!(f, "None"),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self._type != other._type {
            return None;
        }

        match &self._type {
            ValueType::Object(object) => object.partial_cmp(other.as_object()),
            ValueType::None => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self._type != other._type {
            return false;
        }

        match &self._type {
            ValueType::Object(object) => object == other.as_object(),
            ValueType::None => true,
        }
    }
}
