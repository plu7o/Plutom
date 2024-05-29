use core::fmt;
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    ops::{Add, Div, Mul, Sub},
};

use super::{integer::ObjInt, object::ObjType};

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

impl PartialOrd for ObjFloat {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.raw.partial_cmp(&other.value.raw)
    }
}

impl PartialEq<ObjInt> for ObjFloat {
    fn eq(&self, other: &ObjInt) -> bool {
        self.value.raw == other.value as f64
    }
}

impl PartialOrd<ObjInt> for ObjFloat {
    fn partial_cmp(&self, other: &ObjInt) -> Option<Ordering> {
        self.value.raw.partial_cmp(&(other.value as f64))
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

impl PartialOrd<ObjType> for ObjFloat {
    fn partial_cmp(&self, other: &ObjType) -> Option<Ordering> {
        match other {
            ObjType::Int(b) => self.value.raw.partial_cmp(&(b.value as f64)),
            ObjType::Float(b) => self.value.raw.partial_cmp(&b.value.raw),
            _ => None,
        }
    }
}
