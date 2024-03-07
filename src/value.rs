#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    Bool(bool),
    Number(f64),
    Object(Obj),
    None,
}
#[derive(Debug, Clone, Copy)]
pub struct Value {
    _type: ValueType,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ObjType {
    String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Obj {
    _type: ObjType,
}

struct ObjString {
    _type: ObjType,
    string: String,
}

impl Value {
    pub fn new(value: ValueType) -> Self {
        Self { _type: value }
    }

    pub fn none_val() -> Value {
        Value::new(ValueType::None)
    }

    pub fn is_none(&self) -> bool {
        match self._type {
            ValueType::None => true,
            _ => false,
        }
    }
    pub fn bool_val(value: bool) -> Value {
        Value::new(ValueType::Bool(value))
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self._type {
            ValueType::Bool(value) => Some(value),
            _ => None,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self._type {
            ValueType::Bool(_) => true,
            _ => false,
        }
    }

    pub fn number_val(value: f64) -> Value {
        Value::new(ValueType::Number(value))
    }

    pub fn as_number(&self) -> Option<f64> {
        match self._type {
            ValueType::Number(value) => Some(value),
            _ => None,
        }
    }

    pub fn is_number(&self) -> bool {
        match self._type {
            ValueType::Number(_) => true,
            _ => false,
        }
    }

    pub fn obj_val(object: Obj) -> Value {
        Value::new(ValueType::Object(object))
    }

    pub fn as_obj(&self) -> Option<Obj> {
        match self._type {
            ValueType::Object(Obj) => Some(Obj),
            _ => None,
        }
    }

    pub fn is_obj(&self) -> bool {
        match self._type {
            ValueType::Object(_) => true,
            _ => false,
        }
    }
}

pub fn values_equal(a: Value, b: Value) -> bool {
    if a._type != b._type {
        return false;
    }
    match a._type {
        ValueType::Bool(a) => a == b.as_bool().unwrap(),
        ValueType::Number(a) => a == b.as_number().unwrap(),
        ValueType::None => true,
        _ => false,
    }
}

pub fn print_value(value: &Value) {
    match value._type {
        ValueType::Bool(value) => {
            if value {
                print!("true")
            } else {
                print!("false")
            }
        }
        ValueType::Number(value) => print!("{}", value),
        ValueType::None => print!("none"),
        _ => (),
    }
}
