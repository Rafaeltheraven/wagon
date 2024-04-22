
use std::collections::BTreeMap;
use std::fmt::Display;
use std::ops::{Add, Sub, Mul, Div, Not, Rem};
use ordered_float::NotNan;

pub use num_traits::Pow;

use crate::{ValueError, ValueResult, Valueable};

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
            Self::Bool(b) => *b,
            Self::String(s) => !s.is_empty(),
            Self::Natural(n) => n > &0,
            Self::Float(f) => !f.eq(&0.0),
            Self::Dict(m) => !m.is_empty(),
            Self::Array(a) => !a.is_empty(),
        })
    }

    #[allow(clippy::cast_possible_truncation)]
    fn to_int(&self) -> ValueResult<i32, Self> {
        Ok(match self {
            Self::Natural(n) => *n,
            Self::Float(f) => f.round() as i32,
            o => i32::from(o.is_truthy()?)
        })
    }

    fn to_float(&self) -> ValueResult<f32, Self> {
        Ok(match self {
            Self::Natural(n) => *n as f32,
            Self::Float(f) => f.into_inner(),
            o => if o.is_truthy()? { 1.0 } else { 0.0 }
        })
    }

    fn display_numerical(&self) -> ValueResult<String, Self> {
        Ok(match self {
            Self::Float(f) => f.to_string(),
            other => other.to_int()?.to_string()
        })
    }
}

impl<T: Valueable> Display for Value<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(v) => write!(f, "{v}"),
            Self::String(v) => write!(f, "{v}"),
            Self::Natural(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Dict(v) => write!(f, "{v:?}"),
            Self::Array(v) => write!(f, "{v:?}"),
        }
    }
}

impl<T: Valueable> From<Value<T>> for i32 {
    fn from(value: Value<T>) -> Self {
        #[allow(clippy::expect_used)]
        value.to_int().expect("This conversion can not fail")
    }
}

impl<T: Valueable> From<Value<T>> for f32 {
    fn from(value: Value<T>) -> Self {
        #[allow(clippy::expect_used)]
        value.to_float().expect("This conversion can not fail")
    }
}

impl<T: Valueable> From<Value<T>> for bool {
    fn from(value: Value<T>) -> Self {
        #[allow(clippy::expect_used)]
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

impl<T: Valueable> From<&str> for Value<T> {
    fn from(value: &str) -> Self {
        Self::String(String::from(value))
    }
}

impl<T: Valueable> From<i32> for Value<T> {
    fn from(value: i32) -> Self {
        Self::Natural(value)
    }
}

impl<T: Valueable> TryFrom<f32> for Value<T> {
    type Error = ValueError<Self>;

    fn try_from(value: f32) -> Result<Self, Self::Error> {
        Ok(Self::Float(NotNan::try_from(value)?))
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
            (Self::Bool(b1), Self::Bool(b2)) => Self::Bool(b1 || b2), // or
            (Self::Bool(b), Self::Natural(i)) | (Self::Natural(i), Self::Bool(b)) => Self::Natural(i32::from(b) + i),
            (Self::Bool(b), Self::Float(f)) | (Self::Float(f), Self::Bool(b)) => Self::Float(NotNan::new(f32::from(u8::from(b)))? + f),
            (Self::String(s1), Self::String(s2)) => Self::String(s1.add(&s2)),
            (Self::Natural(i1), Self::Natural(i2)) => Self::Natural(i1 + i2),
            (Self::Natural(i), Self::Float(f)) | (Self::Float(f), Self::Natural(i)) => Self::Float(NotNan::new(i as f32)? + f),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 + f2),
            (Self::Dict(d1), Self::Dict(d2)) => Self::Dict(d1.into_iter().chain(d2).collect()),
            (Self::Array(a1), Self::Array(a2)) => Self::Array(a1.into_iter().chain(a2).collect()),
            (v1, v2) => return Err(ValueError::OperationError(v1, v2, "+".to_owned()))
        })
    }
}

impl<T: Valueable> Sub for Value<T> {
    type Output = ValueResult<Self, Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Bool(b1), Self::Bool(b2)) => Self::Bool(b1 && !b2), // nand
            (Self::Bool(b), Self::Natural(i)) | (Self::Natural(i), Self::Bool(b)) => Self::Natural(i32::from(b) - i),
            (Self::Bool(b), Self::Float(f)) | (Self::Float(f), Self::Bool(b)) => Self::Float(NotNan::new(f32::from(u8::from(b)))? - f),
            (Self::Natural(i1), Self::Natural(i2)) => Self::Natural(i1 - i2),
            (Self::Natural(i), Self::Float(f)) | (Self::Float(f), Self::Natural(i)) => Self::Float(NotNan::new(i as f32)? - f),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 - f2),
            (Self::Array(a1), Self::Array(a2)) => Self::Array(a1.into_iter().filter(|x| !a2.contains(x)).collect()),
            (v1, v2) => return Err(ValueError::OperationError(v1, v2, "-".to_owned()))
        })
    }
}

impl<T: Valueable> Mul for Value<T> {
    type Output = ValueResult<Self, Self>;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Bool(b1), Self::Bool(b2)) => Self::Bool(b1 && b2), // and
            (Self::Bool(b), Self::Natural(i)) | (Self::Natural(i), Self::Bool(b)) => Self::Natural(i32::from(b) * i),
            (Self::Bool(b), Self::Float(f)) | (Self::Float(f), Self::Bool(b)) => Self::Float(NotNan::new(f32::from(u8::from(b)))? * f),
            (Self::Natural(i1), Self::Natural(i2)) => Self::Natural(i1 * i2),
            (Self::Natural(i), Self::Float(f)) | (Self::Float(f), Self::Natural(i)) => Self::Float(NotNan::new(i as f32)? * f),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 * f2),
            (v1, v2) => return Err(ValueError::OperationError(v1, v2, "*".to_owned()))
        })
    }
}

impl<T: Valueable> Div for Value<T> {
    type Output = ValueResult<Self, Self>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Bool(b1), Self::Bool(b2)) => Self::Bool((b1 || b2) && !(b1 && b2)), // xor
            (Self::Bool(b), Self::Natural(i)) => Self::Natural(i32::from(b) / i), 
            (Self::Natural(i), Self::Bool(b)) => Self::Natural(i / i32::from(b)),
            (Self::Bool(b), Self::Float(f)) => Self::Float(NotNan::new(f32::from(u8::from(b)))? / f),
            (Self::Float(f), Self::Bool(b)) => Self::Float(f / NotNan::new(f32::from(u8::from(b)))?),

            (Self::Natural(i1), Self::Natural(i2)) => Self::Natural(i1 / i2),
            (Self::Natural(i), Self::Float(f)) => Self::Float(NotNan::new(i as f32)? / f),
            (Self::Float(f), Self::Natural(i)) => Self::Float(f / NotNan::new(i as f32)?),

            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 / f2),
            (v1, v2) => return Err(ValueError::OperationError(v1, v2, "/".to_owned()))
        })
    }
}

impl<T: Valueable> Not for Value<T> {
    type Output = ValueResult<Self, Self>;

    fn not(self) -> Self::Output {
        match self {
            Self::Bool(b) => Ok(Self::Bool(!b)),
            v => Err(ValueError::NegationError(v))
        }
    }
}

impl<T: Valueable> Rem for Value<T> {
    type Output = ValueResult<Self, Self>;

    fn rem(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Bool(b), Self::Natural(i)) => Self::Natural(i32::from(b) % i), 
            (Self::Natural(i), Self::Bool(b)) => Self::Natural(i % i32::from(b)),
            (Self::Bool(b), Self::Float(f)) => Self::Float(NotNan::new(f32::from(u8::from(b)))? % f),
            (Self::Float(f), Self::Bool(b)) => Self::Float(f % NotNan::new(f32::from(u8::from(b)))?),

            (Self::Natural(i1), Self::Natural(i2)) => Self::Natural(i1 % i2),
            (Self::Natural(i), Self::Float(f)) => Self::Float(NotNan::new(i as f32)? % f),
            (Self::Float(f), Self::Natural(i)) => Self::Float(f % NotNan::new(i as f32)?),

            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1 % f2),
            (v1, v2) => return Err(ValueError::OperationError(v1, v2, "%".to_owned()))
        })
    }
}

impl<T: Valueable> Pow<Self> for Value<T> {
    type Output = ValueResult<Self, Self>;

    #[allow(clippy::cast_sign_loss)]
    fn pow(self, rhs: Self) -> Self::Output { 
        match (self, rhs) {
            (Self::Bool(true), Self::Natural(_)) | (Self::Natural(_), Self::Bool(false)) => Ok(Self::Natural(1)),
            (Self::Bool(false), Self::Natural(_)) => Ok(Self::Natural(0)),
            (Self::Natural(i), Self::Bool(true)) => Ok(Self::Natural(i)),
            (Self::Bool(true), Self::Float(_)) | (Self::Float(_), Self::Bool(false)) => Ok(Self::Float(NotNan::new(1.0)?)),
            (Self::Bool(false), Self::Float(_)) => Ok(Self::Float(NotNan::new(0.0)?)),
            (Self::Float(f), Self::Bool(true)) => Ok(Self::Float(f)),
            (Self::Natural(i1), Self::Natural(i2)) if i2 >= 0 => Ok(Self::Natural((i1).pow((i2).try_into()?))),
            (Self::Natural(i1), Self::Natural(i2)) => Ok(Self::Float(NotNan::new(1.0 / (i1.pow((-i2) as u32) as f32))?)), // Power of negative number is division
            (Self::Natural(i), Self::Float(f)) => Ok(Self::Float(NotNan::new(i as f32)?.pow(f))),
            (Self::Float(f), Self::Natural(i)) => Ok(Self::Float(f.pow(i))),
            (Self::Float(f1), Self::Float(f2)) => Ok(Self::Float(f1.pow(f2))),
            (v1, v2) => Err(ValueError::OperationError(v1, v2, "**".to_owned()))
        }
    }
}

impl<T: Valueable> PartialOrd for Value<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::String(s1), Self::String(s2)) => s1.partial_cmp(s2),
            (Self::Natural(i1), Self::Natural(i2)) => i1.partial_cmp(i2),
            (Self::Float(f1), Self::Float(f2)) => f1.partial_cmp(f2),
            (Self::Bool(b1), Self::Bool(b2)) => b1.partial_cmp(b2),
            (Self::Bool(b), Self::Natural(i)) => {
                if *b {
                    1.partial_cmp(i)
                } else {
                    0.partial_cmp(i)
                }
            },
            (Self::Natural(i), Self::Bool(b)) => {
                if *b {
                    i.partial_cmp(&1)
                } else {
                    i.partial_cmp(&0)
                }
            },
            (Self::Bool(b), Self::Float(f)) => {
                let bool_float: NotNan<f32> = NotNan::from(i8::from(*b));
                bool_float.partial_cmp(f)
            },
            (Self::Float(f), Self::Bool(b)) => {
                let bool_float: NotNan<f32> = NotNan::from(i8::from(*b));
                f.partial_cmp(&bool_float)
            },
            (_, _) => None
        }
    }
}
