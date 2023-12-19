use std::{fmt::Display, write, ops::{Add, Sub, Mul, Div, Not}};

use crate::GLLBlockLabel;
pub use wagon_codegen::value::Value as InnerValue;
use wagon_codegen::value::Valueable;

#[derive(Debug, Eq, Hash, Clone)]
pub enum Value<'a> {
    Value(InnerValue<Value<'a>>),
	Label(GLLBlockLabel<'a>),
}

impl<'a> Valueable for Value<'a> {}

impl PartialEq for Value<'_> { // For some reason the derive breaks but just copying it over is fine
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Value(f0_self), Value::Value(f0_other)) => f0_self.eq(f0_other),
            (Value::Label(f0_self), Value::Label(f0_other)) => f0_self.eq(f0_other),
            _unused => false,
        }
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Value(v) => write!(f, "{}", v),
            Value::Label(v) => write!(f, "{}", v.to_string()),
        }
    }
}

impl<'a> Value<'a> {
	fn is_truthy(&self) -> bool {
		match self {
		    Value::Value(v) => v.is_truthy(),
		    Value::Label(l) => l.is_eps(),
		}
	}

	pub fn to_int(&self) -> i32 {
		match self {
			Value::Value(v) => v.to_int(),
			o => if o.is_truthy() { 1 } else { 0 }
		}
	}

	pub fn to_float(&self) -> f32 {
		match self {
			Value::Value(v) => v.to_float(),
			o => if o.is_truthy() { 1.0 } else { 0.0 }
		}
	}

	pub fn pow(self, rhs: Value<'a>) -> Value<'a> {
		match (self, rhs) {
            (Value::Value(v1), Value::Value(v2)) => Value::Value(v1.pow(v2)),
            (v1, v2) => panic!("Type Error! Can not perform ** on {:?} and {:?}", v1, v2)
        }
	}
}

impl From<Value<'_>> for i32 { // Can't genericize these because Rust doesn't allow it
    fn from(value: Value) -> Self {
        value.to_int()
    }
}

impl From<Value<'_>> for f32 {
    fn from(value: Value) -> Self {
        value.to_float()
    }
}

impl From<Value<'_>> for bool {
    fn from(value: Value) -> Self {
        value.is_truthy()
    }
}

impl<'a, T> From<T> for Value<'a> where InnerValue<Value<'a>>: From<T> {
    fn from(value: T) -> Self {
        Value::Value(InnerValue::from(value))
    }
}

impl<'a> From<Value<'a>> for GLLBlockLabel<'a> {
    fn from(value: Value<'a>) -> Self {
        match value {
            Value::Label(l) => l,
            other => panic!("Type error! Expected label value but got {:?}", other)
        }
    }
}

impl<'a> Add for Value<'a> {
    type Output = Value<'a>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Value(v1), Value::Value(v2)) => Value::Value(v1 + v2),
            (v1, v2) => panic!("Type Error! Can not perform + on {:?} and {:?}", v1, v2)
        }
    }
}

impl<'a> Sub for Value<'a> {
    type Output = Value<'a>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Value(v1), Value::Value(v2)) => Value::Value(v1 - v2),
            (v1, v2) => panic!("Type Error! Can not perform - on {:?} and {:?}", v1, v2)
        }
    }
}

impl<'a> Mul for Value<'a> {
    type Output = Value<'a>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Value(v1), Value::Value(v2)) => Value::Value(v1 * v2),
            (v1, v2) => panic!("Type Error! Can not perform * on {:?} and {:?}", v1, v2)
        }
    }
}

impl<'a> Div for Value<'a> {
    type Output = Value<'a>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Value(v1), Value::Value(v2)) => Value::Value(v1 / v2),
            (v1, v2) => panic!("Type Error! Can not perform / on {:?} and {:?}", v1, v2)
        }
    }
}

impl<'a> Not for Value<'a> {
    type Output = Value<'a>;

    fn not(self) -> Self::Output {
        match self {
            Value::Value(v) => Value::Value(!v),
            v => panic!("Type Error! Can only negate booleans, not {:?}", v)
        }
    }
}

impl<'a> Ord for Value<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Value::Value(v1), Value::Value(v2)) => v1.cmp(v2),
            (v1, v2) => panic!("Type Error! Can not compare {:?} with {:?}", v1, v2)
        }
    }
}

impl <'a> PartialOrd for Value<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

