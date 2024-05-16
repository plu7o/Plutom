use crate::{chunk::Chunk, value::Value};
use core::fmt;
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    ops::{Add, Div, Mul, Sub},
};

pub type NativeFn = fn(usize, &[Value]) -> Result<Value, String>;

#[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
pub struct Float {
    pub raw: f64,
}
impl Hash for Float {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let bits = self.raw.to_bits();
        bits.hash(state);
    }
}
impl Eq for Float {}

#[derive(Debug, Clone, Eq, Hash)]
pub enum ObjType {
    Bool(ObjBool),
    Int(ObjInt),
    Float(ObjFloat),
    Str(ObjString),
    Function(ObjFunction),
    Native(ObjNative),
    Closure(ObjClosure),
    List(ObjList),
}

impl fmt::Display for ObjType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

impl PartialEq for ObjType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ObjType::Bool(bool) => bool == other,
            ObjType::Str(string) => string == other,
            ObjType::Int(integer) => integer == other,
            ObjType::Float(float) => float == other,
            ObjType::Function(function) => function == other,
            ObjType::Closure(closure) => closure == other,
            ObjType::Native(native) => native == other,
            ObjType::List(list) => list == other,
        }
    }
}

impl PartialOrd for ObjType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            ObjType::Float(float) => float.partial_cmp(other),
            ObjType::Int(int) => int.partial_cmp(other),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjString {
    pub chars: String,
}

impl ObjString {
    pub fn new(name: String) -> Self {
        Self { chars: name }
    }

    pub fn print(&self) {
        print!("{}", self.chars);
    }
}

impl Default for ObjString {
    fn default() -> Self {
        Self {
            chars: String::new(),
        }
    }
}

impl PartialEq<ObjType> for ObjString {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Str(string) => string.chars == self.chars,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjFunction {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<ObjString>,
}

impl ObjFunction {
    pub fn new() -> Self {
        Self {
            arity: 0,
            name: None,
            chunk: Chunk::init(),
        }
    }

    pub fn print(&self) {
        match &self.name {
            Some(name) => print!("<fn {}>", name.chars),
            None => print!("<Script>"),
        }
    }
}

impl PartialEq<ObjType> for ObjFunction {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Function(function) => match &function.name {
                Some(name) => {
                    if let Some(self_name) = &self.name {
                        name.chars == self_name.chars
                    } else {
                        false
                    }
                }
                None => self.name.is_none(),
            },
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjNative {
    pub function: NativeFn,
}

impl ObjNative {
    pub fn new(function: NativeFn) -> Self {
        Self { function }
    }

    pub fn print(&self) {
        print!("<NativeFn>");
    }
}

impl PartialEq<ObjType> for ObjNative {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Native(native) => native.function == self.function,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjClosure {
    pub function: ObjFunction,
}

impl ObjClosure {
    pub fn new(function: ObjFunction) -> Self {
        Self { function }
    }

    pub fn print(&self) {
        print!("<Closure>");
    }
}

impl PartialEq<ObjType> for ObjClosure {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Closure(closure) => closure.function == self.function,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjList {
    pub len: usize,
    pub items: Vec<Value>,
}

impl ObjList {
    pub fn new(items: Vec<Value>) -> Self {
        Self {
            len: items.len(),
            items,
        }
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

#[derive(Debug, Clone, Hash, Eq)]
pub struct ObjInt {
    pub value: i64,
}

impl ObjInt {
    pub fn new(value: i64) -> Self {
        Self { value }
    }

    pub fn print(&self) {
        print!("{}", self.value);
    }
}

impl fmt::Display for ObjInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

impl Sub for ObjInt {
    type Output = i64;
    fn sub(self, rhs: Self) -> Self::Output {
        self.value - rhs.value
    }
}

impl Sub<ObjFloat> for ObjInt {
    type Output = f64;
    fn sub(self, rhs: ObjFloat) -> Self::Output {
        (self.value as f64) - rhs.value.raw
    }
}

impl Add for ObjInt {
    type Output = i64;
    fn add(self, rhs: Self) -> Self::Output {
        self.value + rhs.value
    }
}

impl Add<ObjFloat> for ObjInt {
    type Output = f64;
    fn add(self, rhs: ObjFloat) -> Self::Output {
        (self.value as f64) + rhs.value.raw
    }
}

impl Div for ObjInt {
    type Output = i64;
    fn div(self, rhs: Self) -> Self::Output {
        self.value / rhs.value
    }
}

impl Div<ObjFloat> for ObjInt {
    type Output = f64;
    fn div(self, rhs: ObjFloat) -> Self::Output {
        (self.value as f64) / rhs.value.raw
    }
}

impl Mul for ObjInt {
    type Output = i64;
    fn mul(self, rhs: Self) -> Self::Output {
        self.value * rhs.value
    }
}

impl Mul<ObjFloat> for ObjInt {
    type Output = f64;
    fn mul(self, rhs: ObjFloat) -> Self::Output {
        (self.value as f64) * rhs.value.raw
    }
}

impl PartialEq for ObjInt {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl PartialEq<ObjType> for ObjInt {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Int(int) => int.value == self.value,
            _ => false,
        }
    }
}

impl PartialOrd for ObjInt {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl PartialOrd<ObjType> for ObjInt {
    fn partial_cmp(&self, other: &ObjType) -> Option<Ordering> {
        match other {
            ObjType::Int(b) => self.value.partial_cmp(&b.value),
            ObjType::Float(b) => b.value.raw.partial_cmp(&(self.value as f64)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Hash, Eq)]
pub struct ObjFloat {
    pub value: Float,
}

impl ObjFloat {
    pub fn new(value: f64) -> Self {
        Self {
            value: Float { raw: value },
        }
    }

    pub fn print(&self) {
        print!("{:#?}", self.value.raw);
    }
}

impl fmt::Display for ObjFloat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

impl Sub for ObjFloat {
    type Output = f64;
    fn sub(self, rhs: Self) -> Self::Output {
        self.value.raw - rhs.value.raw
    }
}

impl Sub<ObjInt> for ObjFloat {
    type Output = f64;
    fn sub(self, rhs: ObjInt) -> Self::Output {
        self.value.raw - rhs.value as f64
    }
}

impl Add for ObjFloat {
    type Output = f64;
    fn add(self, rhs: Self) -> Self::Output {
        self.value.raw + rhs.value.raw
    }
}

impl Add<ObjInt> for ObjFloat {
    type Output = f64;
    fn add(self, rhs: ObjInt) -> Self::Output {
        self.value.raw + rhs.value as f64
    }
}

impl Div for ObjFloat {
    type Output = f64;
    fn div(self, rhs: Self) -> Self::Output {
        self.value.raw / rhs.value.raw
    }
}

impl Div<ObjInt> for ObjFloat {
    type Output = f64;
    fn div(self, rhs: ObjInt) -> Self::Output {
        self.value.raw / rhs.value as f64
    }
}

impl Mul for ObjFloat {
    type Output = f64;
    fn mul(self, rhs: Self) -> Self::Output {
        self.value.raw * rhs.value.raw
    }
}

impl Mul<ObjInt> for ObjFloat {
    type Output = f64;
    fn mul(self, rhs: ObjInt) -> Self::Output {
        self.value.raw * rhs.value as f64
    }
}

impl PartialEq for ObjFloat {
    fn eq(&self, other: &Self) -> bool {
        self.value.raw == other.value.raw
    }
}

impl PartialEq<ObjType> for ObjFloat {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Float(float) => float.value == self.value,
            _ => false,
        }
    }
}

impl PartialOrd for ObjFloat {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.raw.partial_cmp(&other.value.raw)
    }
}

impl PartialOrd<ObjType> for ObjFloat {
    fn partial_cmp(&self, other: &ObjType) -> Option<Ordering> {
        match other {
            ObjType::Int(b) => self.value.raw.partial_cmp(&(b.value as f64)),
            ObjType::Float(b) => self.value.raw.partial_cmp(&b.value.raw),
            _ => None,
        }
    }
}
