use core::fmt;
use std::{
    cmp::Ordering,
    ops::{Add, Div, Mul, Sub},
};

use super::{float::ObjFloat, object::ObjType};

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

impl PartialOrd for ObjInt {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl PartialEq<ObjFloat> for ObjInt {
    fn eq(&self, other: &ObjFloat) -> bool {
        self.value as f64 == other.value.raw
    }
}

impl PartialOrd<ObjFloat> for ObjInt {
    fn partial_cmp(&self, other: &ObjFloat) -> Option<Ordering> {
        (self.value as f64).partial_cmp(&other.value.raw)
    }
}

impl PartialEq<ObjType> for ObjInt {
    fn eq(&self, other: &ObjType) -> bool {
        match other {
            ObjType::Int(b) => self == b,
            ObjType::Float(b) => self == b,
            _ => false,
        }
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
