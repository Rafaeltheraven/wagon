
use std::collections::BTreeMap;
use std::fmt::Display;
use std::ops::{Add, Sub, Mul, Div, Not};
use ordered_float::{NotNan, Pow};

use crate::{Valueable, ValueResult, ValueError};

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
/// The most basic types that a value can ever be.
///
/// For recursion reasons, this type is generic over any other type that can be seen as a Value (see [`Valueable`]).
pub enum Value<T: Valueable> {
    /// A bool.
    Bool(bool),
    /// A string.
    String(String),
    /// A whole number.
    Natural(i32),
    /// A float.
    Float(NotNan<f32>),
    /// A dictionary.
    Dict(BTreeMap<String, T>),
    /// A list.
    Array(Vec<T>)
}

impl<T: Valueable> Valueable for Value<T> {
    fn is_truthy(&self) -> ValueResult<bool, Self> {
        Ok(match self {
            Value::Bool(b) => *b,
            Value::String(s) => !s.is_empty(),
            Value::Natural(n) => n > &0,
            Value::Float(f) => !f.eq(&0.0),
            Value::Dict(m) => !m.is_empty(),
            Value::Array(a) => !a.is_empty(),
        })
    }

    fn to_int(&self) -> ValueResult<i32, Self> {
        Ok(match self {
            Value::Natural(n) => *n,
            Value::Float(f) => f.round() as i32,
            o => if o.is_truthy()? { 1 } else { 0 }
        })
    }

    #[allow(clippy::cast_possible_truncation)]
    fn to_float(&self) -> ValueResult<f32, Self> {
        Ok(match self {
            Value::Natural(n) => *n as f32,
            Value::Float(f) => f.into_inner(),
            o => if o.is_truthy()? { 1.0 } else { 0.0 }
        })
    }

    fn pow(&self, rhs: &Value<T>) -> ValueResult<Self, Self> {
        match (self, rhs) {
            (Value::Bool(true), Value::Natural(_)) | (Value::Natural(_), Value::Bool(false)) => Ok(Value::Natural(1)),
            (Value::Bool(false), Value::Natural(_)) => Ok(Value::Natural(0)),
            (Value::Natural(i), Value::Bool(true)) => Ok(Value::Natural(*i)),
            (Value::Bool(true), Value::Float(_)) | (Value::Float(_), Value::Bool(false)) => Ok(Value::Float(NotNan::new(1.0)?)),
            (Value::Bool(false), Value::Float(_)) => Ok(Value::Float(NotNan::new(0.0)?)),
            (Value::Float(f), Value::Bool(true)) => Ok(Value::Float(*f)),
            (Value::Natural(i1), Value::Natural(i2)) if i2 >= &0 => Ok(Value::Natural((*i1).pow((*i2).try_into()?))),
            (Value::Natural(i1), Value::Natural(i2)) => Ok(Value::Float(NotNan::new(1.0 / (i1.pow((-i2) as u32) as f32))?)), // Power of negative number is division
            (Value::Natural(i), Value::Float(f)) => Ok(Value::Float(NotNan::new(*i as f32)?.pow(f))),
            (Value::Float(f), Value::Natural(i)) => Ok(Value::Float(f.pow(i))),
            (Value::Float(f1), Value::Float(f2)) => Ok(Value::Float(f1.pow(f2))),
            (v1, v2) => Err(ValueError::OperationError(v1.clone(), v2.clone(), "**".to_owned()))
        }
    }

    fn display_numerical(&self) -> ValueResult<String, Self> {
        Ok(match self {
            Value::Float(f) => f.to_string(),
            other => other.to_int()?.to_string()
        })
    }
}

impl<T: Valueable> Display for Value<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Natural(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Dict(v) => write!(f, "{:?}", v),
            Value::Array(v) => write!(f, "{:?}", v),
        }
    }
}

impl<T: Valueable> From<Value<T>> for i32 {
    fn from(value: Value<T>) -> Self {
        value.to_int().expect("This conversion can not fail")
    }
}

impl<T: Valueable> From<Value<T>> for f32 {
    fn from(value: Value<T>) -> Self {
        value.to_float().expect("This conversion can not fail")
    }
}

impl<T: Valueable> From<Value<T>> for bool {
    fn from(value: Value<T>) -> Self {
        value.is_truthy().expect("This conversion can not fail")
    }
}

impl<T: Valueable> From<bool> for Value<T> {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl<T: Valueable> From<String> for Value<T> {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl<T: Valueable> From<i32> for Value<T> {
    fn from(value: i32) -> Self {
        Value::Natural(value)
    }
}

impl<T: Valueable> TryFrom<f32> for Value<T> {
    type Error = ValueError<Self>;

    fn try_from(value: f32) -> Result<Self, Self::Error> {
        Ok(Value::Float(NotNan::try_from(value)?))
    }
}

impl<T: Valueable> From<BTreeMap<String, T>> for Value<T> {
    fn from(value: BTreeMap<String, T>) -> Self {
        Self::Dict(value)
    }
}

impl<T: Valueable> From<Vec<T>> for Value<T> {
    fn from(value: Vec<T>) -> Self {
        Self::Array(value)
    }
}

impl<T: Valueable> Add for Value<T> {
    type Output = ValueResult<Self, Self>;

    fn add(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(b1 || b2), // or
            (Value::Bool(b), Value::Natural(i)) | (Value::Natural(i), Value::Bool(b)) => Value::Natural(b as i32 + i),
            (Value::Bool(b), Value::Float(f)) | (Value::Float(f), Value::Bool(b)) => Value::Float(NotNan::new(b as u8 as f32)? + f),
            (Value::String(s1), Value::String(s2)) => Value::String(s1.add(&s2)),
            (Value::Natural(i1), Value::Natural(i2)) => Value::Natural(i1 + i2),
            (Value::Natural(i), Value::Float(f)) | (Value::Float(f), Value::Natural(i)) => Value::Float(NotNan::new(i as f32)? + f),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 + f2),
            (Value::Dict(d1), Value::Dict(d2)) => Value::Dict(d1.into_iter().chain(d2).collect()),
            (Value::Array(a1), Value::Array(a2)) => Value::Array(a1.into_iter().chain(a2).collect()),
            (v1, v2) => return Err(ValueError::OperationError(v1, v2, "+".to_owned()))
        })
    }
}

impl<T: Valueable> Sub for Value<T> {
    type Output = ValueResult<Self, Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(b1 && !b2), // nand
            (Value::Bool(b), Value::Natural(i)) | (Value::Natural(i), Value::Bool(b)) => Value::Natural(b as i32 - i),
            (Value::Bool(b), Value::Float(f)) | (Value::Float(f), Value::Bool(b)) => Value::Float(NotNan::new(b as u8 as f32)? - f),
            (Value::Natural(i1), Value::Natural(i2)) => Value::Natural(i1 - i2),
            (Value::Natural(i), Value::Float(f)) | (Value::Float(f), Value::Natural(i)) => Value::Float(NotNan::new(i as f32)? - f),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 - f2),
            (Value::Array(a1), Value::Array(a2)) => Value::Array(a1.into_iter().filter(|x| !a2.contains(x)).collect()),
            (v1, v2) => return Err(ValueError::OperationError(v1, v2, "-".to_owned()))
        })
    }
}

impl<T: Valueable> Mul for Value<T> {
    type Output = ValueResult<Self, Self>;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(b1 && b2), // and
            (Value::Bool(b), Value::Natural(i)) | (Value::Natural(i), Value::Bool(b)) => Value::Natural(b as i32 * i),
            (Value::Bool(b), Value::Float(f)) | (Value::Float(f), Value::Bool(b)) => Value::Float(NotNan::new(b as u8 as f32)? * f),
            (Value::Natural(i1), Value::Natural(i2)) => Value::Natural(i1 * i2),
            (Value::Natural(i), Value::Float(f)) | (Value::Float(f), Value::Natural(i)) => Value::Float(NotNan::new(i as f32)? * f),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 * f2),
            (v1, v2) => return Err(ValueError::OperationError(v1, v2, "*".to_owned()))
        })
    }
}

impl<T: Valueable> Div for Value<T> {
    type Output = ValueResult<Self, Self>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Bool(b1), Value::Bool(b2)) => Value::Bool((b1 || b2) && !(b1 && b2)), // xor
            (Value::Bool(b), Value::Natural(i)) => Value::Natural(b as i32 / i), 
            (Value::Natural(i), Value::Bool(b)) => Value::Natural(i / b as i32),
            (Value::Bool(b), Value::Float(f)) => Value::Float(NotNan::new(b as u8 as f32)? / f),
            (Value::Float(f), Value::Bool(b)) => Value::Float(f / NotNan::new(b as u8 as f32)?),

            (Value::Natural(i1), Value::Natural(i2)) => Value::Natural(i1 / i2),
            (Value::Natural(i), Value::Float(f)) => Value::Float(NotNan::new(i as f32)? / f),
            (Value::Float(f), Value::Natural(i)) => Value::Float(f / NotNan::new(i as f32)?),

            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 / f2),
            (v1, v2) => return Err(ValueError::OperationError(v1, v2, "/".to_owned()))
        })
    }
}

impl<T: Valueable> Not for Value<T> {
    type Output = ValueResult<Self, Self>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            v => Err(ValueError::NegationError(v))
        }
    }
}

impl<T: Valueable> PartialOrd for Value<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::String(s1), Value::String(s2)) => s1.partial_cmp(s2),
            (Value::Natural(i1), Value::Natural(i2)) => i1.partial_cmp(i2),
            (Value::Float(f1), Value::Float(f2)) => f1.partial_cmp(f2),
            (Value::Bool(b1), Value::Bool(b2)) => b1.partial_cmp(b2),
            (Value::Bool(b), Value::Natural(i)) => {
                if *b {
                    1.partial_cmp(i)
                } else {
                    0.partial_cmp(i)
                }
            },
            (Value::Natural(i), Value::Bool(b)) => {
                if *b {
                    i.partial_cmp(&1)
                } else {
                    i.partial_cmp(&0)
                }
            },
            (Value::Bool(b), Value::Float(f)) => {
                let bool_float: NotNan<f32> = NotNan::from(*b as i8);
                bool_float.partial_cmp(f)
            },
            (Value::Float(f), Value::Bool(b)) => {
                let bool_float: NotNan<f32> = NotNan::from(*b as i8);
                f.partial_cmp(&bool_float)
            },
            (_, _) => None
        }
    }
}
