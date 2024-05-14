use crate::object::{ObjFunction, ObjList, ObjString, ObjType};

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ValueType {
    Bool(bool),
    Number(i64),
    Object(ObjType),
    None,
}

#[derive(Debug, Clone, Hash)]
pub struct Value {
    _type: ValueType,
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

    pub fn as_bool(&self) -> bool {
        match self._type {
            ValueType::Bool(value) => value,
            _ => panic!("ValueError: {:?} is not a boolean", self._type),
        }
    }

    pub fn is_bool(&self) -> bool {
        match self._type {
            ValueType::Bool(_) => true,
            _ => false,
        }
    }

    pub fn number_val(value: i64) -> Value {
        Value::new(ValueType::Number(value))
    }

    pub fn as_number(&self) -> i64 {
        match self._type {
            ValueType::Number(value) => value,
            _ => panic!("ValueError: {:?} is not a number", self._type),
        }
    }

    pub fn is_number(&self) -> bool {
        match self._type {
            ValueType::Number(_) => true,
            _ => false,
        }
    }

    pub fn as_string(&self) -> &ObjString {
        match self.as_obj() {
            ObjType::String(string) => string,
            _ => panic!("{:?} is not a string", self._type),
        }
    }

    pub fn is_string(&self) -> bool {
        match &self._type {
            ValueType::Object(obj) => match obj {
                ObjType::String(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn string_val(s: String) -> Value {
        Value::obj_val(ObjType::String(ObjString::new(s)))
    }

    // pub fn is_function(&self) -> bool {
    //     match &self._type {
    //         ValueType::Object(obj) => match obj {
    //             ObjType::Function(_) => true,
    //             _ => false,
    //         },
    //         _ => false,
    //     }
    // }
    //
    pub fn as_function(&self) -> &ObjFunction {
        match self.as_obj() {
            ObjType::Function(func) => func,
            _ => panic!("{:?} is not a function objects", self._type),
        }
    }

    pub fn as_list(&self) -> &ObjList {
        match self.as_obj() {
            ObjType::List(list) => list,
            _ => panic!("{:?} is not a list object", self._type),
        }
    }

    pub fn is_list(&self) -> bool {
        match &self._type {
            ValueType::Object(obj) => match obj {
                ObjType::List(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    //
    // pub fn is_native(&self) -> bool {
    //     match &self._type {
    //         ValueType::Object(obj) => match obj {
    //             ObjType::Native(_) => true,
    //             _ => false,
    //         },
    //         _ => false,
    //     }
    // }
    //
    // pub fn as_native(&self) -> &ObjNative {
    //     match self.as_obj() {
    //         ObjType::Native(func) => func,
    //         _ => panic!("{:?} is not a function objects", self._type),
    //     }
    // }

    pub fn obj_val(object: ObjType) -> Value {
        Value::new(ValueType::Object(object))
    }

    pub fn as_obj(&self) -> &ObjType {
        match &self._type {
            ValueType::Object(obj) => obj,
            _ => panic!("ValueError: {:?} is not a object", self._type),
        }
    }

    pub fn is_obj(&self) -> bool {
        match self._type {
            ValueType::Object(_) => true,
            _ => false,
        }
    }

    pub fn is_obj_type(&self, _type: &ObjType) -> bool {
        self.is_obj() && (self.as_obj() == _type)
    }

    pub fn print(&self) {
        match &self._type {
            ValueType::Bool(value) => {
                if *value {
                    print!("true")
                } else {
                    print!("false")
                }
            }
            ValueType::Number(value) => print!("{}", value),
            ValueType::Object(obj) => match obj {
                ObjType::String(string) => print!("{}", string.chars),
                ObjType::Function(func) => match &func.name {
                    Some(name) => print!("<fn {}>", name.chars),
                    None => print!("<Script>"),
                },
                ObjType::Native(_) => print!("<native fn>"),
                ObjType::Closure(_) => print!("<closure fn>"),
                ObjType::List(list) => {
                    print!("[");
                    for (i, item) in list.items.iter().enumerate() {
                        item.print();
                        if i != list.items.len() - 1 {
                            print!(", ");
                        }
                    }
                    print!("]")
                }
            },
            ValueType::None => print!("none"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self._type != other._type {
            return false;
        }
        match &self._type {
            ValueType::Bool(a) => *a == other.as_bool(),
            ValueType::Number(a) => *a == other.as_number(),
            ValueType::Object(a) => a == other.as_obj(),
            ValueType::None => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_string() {
        let _tests = vec![
            Value::obj_val(ObjType::String(ObjString {
                len: 0,
                chars: String::new(),
            })),
            Value::obj_val(ObjType::String(ObjString {
                len: 0,
                chars: "mojdfjdnf".to_string(),
            })),
            Value::number_val(0),
            Value::bool_val(true),
            Value::none_val(),
        ];

        assert_eq!(_tests[0].is_string(), true);
        assert_eq!(_tests[1].is_string(), true);
        assert_eq!(_tests[2].is_string(), false);
        assert_eq!(_tests[3].is_string(), false);
        assert_eq!(_tests[4].is_string(), false);
    }

    #[test]
    fn test_is_obj() {
        let _tests = vec![
            Value::obj_val(ObjType::String(ObjString {
                len: 0,
                chars: String::new(),
            })),
            Value::number_val(0),
            Value::bool_val(true),
            Value::none_val(),
        ];

        assert_eq!(_tests[0].is_obj(), true);
        assert_eq!(_tests[1].is_obj(), false);
        assert_eq!(_tests[2].is_obj(), false);
        assert_eq!(_tests[3].is_obj(), false);
    }

    #[test]
    fn test_equality() {
        let _tests = vec![
            Value::obj_val(ObjType::String(ObjString {
                len: 0,
                chars: String::new(),
            })),
            Value::number_val(0),
            Value::bool_val(true),
            Value::none_val(),
        ];

        assert_ne!(
            _tests[0],
            Value::obj_val(ObjType::String(ObjString {
                len: 0,
                chars: "fdssdfsdfsdf".to_owned(),
            }))
        );
        assert_eq!(_tests[1], Value::number_val(0));
        assert_eq!(_tests[2], Value::bool_val(true));
        assert_eq!(_tests[3], Value::none_val());
    }
}
