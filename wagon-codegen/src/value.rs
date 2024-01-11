/// Because of the strict type system in Rust, it's nice to have a more lenient data structure that can deal with various types and operations called on it.
/// 
/// This module holds code for those purposes.

use std::{collections::BTreeMap, fmt::Display, write, ops::{Add, Sub, Mul, Div, Not}};

use ordered_float::{NotNan, Pow};
use wagon_parser::parser::atom::Atom;

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

/// A trait to allow "extension" of the [`Value`] enum.
///
/// Sometimes, the basic types supported by `Value` are not enough and the newtype pattern is required to extend it.
/// Registering this newtype as `Valueable` means that it supports all common operations associated with a `Value`.
pub trait Valueable: std::fmt::Debug + PartialEq + std::hash::Hash + Eq + Clone {
    /// Is this value seen as `true` or `false`?
    fn is_truthy(&self) -> bool;
    /// Convert the value to a regular [`i32`].
    fn to_int(&self) -> i32;
    /// Convert the value to a regular [`f32`].
    fn to_float(&self) -> f32;
    /// Calculate this value to the power of another.
    fn pow(&self, rhs: &Self) -> Self;
    /// Get a string representation of the value, as if it were a number. 
    fn display_numerical(&self) -> String;
}

/// A second trait for "extension" of the [`Value`] enum.
///
/// This is intended to "extract" the inner value if possible.
pub trait ToValue<T: Valueable> {
    /// Return a reference to the [`Value`] that this type encompasses.
    fn to_value(&self) -> &Value<T>;
}

/// A [`Valueable`] that can hold a list/dict mapping to other values.
///
/// Because of recursion limits in Rust, [`Value`] can not be implemented over itself. 
/// `RecursiveValue` is, for all intents and purposes, a `Value` implemented over itself. 
///
/// If the basic [`Value`] enum is enough for your purposes, you will want to use `RecursiveValue`.
///
/// # Example
/// Instead of doing this: 
/// ```
/// use wagon_codegen::value::Value;
/// ```
/// Do this:
/// ```
/// use wagon_codegen::value::RecursiveValue as Value;
/// ```
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct RecursiveValue(Value<RecursiveValue>);

impl ToValue<Self> for RecursiveValue {
    fn to_value(&self) -> &Value<Self> {
        &self.0
    }
}

impl<T: Valueable> Valueable for Value<T> {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::String(s) => !s.is_empty(),
            Value::Natural(n) => n > &0,
            Value::Float(f) => !f.eq(&0.0),
            Value::Dict(m) => !m.is_empty(),
            Value::Array(a) => !a.is_empty(),
        }
    }

    fn to_int(&self) -> i32 {
        match self {
            Value::Natural(n) => *n,
            Value::Float(f) => f.round() as i32,
            o => if o.is_truthy() { 1 } else { 0 }
        }
    }

    fn to_float(&self) -> f32 {
        match self {
            Value::Natural(n) => *n as f32,
            Value::Float(f) => f.into_inner(),
            o => if o.is_truthy() { 1.0 } else { 0.0 }
        }
    }

    fn pow(&self, rhs: &Value<T>) -> Value<T> {
        match (self, rhs) {
            (Value::Bool(true), Value::Natural(_)) | (Value::Natural(_), Value::Bool(false)) => Value::Natural(1),
            (Value::Bool(false), Value::Natural(_)) => Value::Natural(0),
            (Value::Natural(i), Value::Bool(true)) => Value::Natural(*i),
            (Value::Bool(true), Value::Float(_)) | (Value::Float(_), Value::Bool(false)) => Value::Float(NotNan::new(1.0).unwrap()),
            (Value::Bool(false), Value::Float(_)) => Value::Float(NotNan::new(0.0).unwrap()),
            (Value::Float(f), Value::Bool(true)) => Value::Float(*f),
            (Value::Natural(i1), Value::Natural(i2)) if i2 >= &0 => Value::Natural((*i1).pow((*i2).try_into().unwrap())),
            (Value::Natural(i1), Value::Natural(i2)) => Value::Float(NotNan::new(1.0 / (i1.pow((-i2) as u32) as f32)).unwrap()), // Power of negative number is division
            (Value::Natural(i), Value::Float(f)) => Value::Float(NotNan::new(*i as f32).unwrap().pow(f)),
            (Value::Float(f), Value::Natural(i)) => Value::Float(f.pow(i)),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1.pow(f2)),
            (v1, v2) => panic!("Type Error! Can not perform ** on {:?} and {:?}", v1, v2)
        }
    }

    fn display_numerical(&self) -> String {
        match self {
            Value::Float(f) => f.to_string(),
            other => other.to_int().to_string()
        }
    }
}

impl<T: ToValue<T> + From<Value<T>> + Clone + Eq + std::hash::Hash + std::fmt::Debug> Valueable for T {
    fn is_truthy(&self) -> bool {
        self.to_value().is_truthy()
    }

    fn to_int(&self) -> i32 {
        self.to_value().to_int()
    }

    fn to_float(&self) -> f32 {
        self.to_value().to_float()
    }

    fn pow(&self, rhs: &Self) -> Self {
        Self::from(self.to_value().pow(rhs.to_value()))
    }

    fn display_numerical(&self) -> String {
        self.to_value().display_numerical()
    }
}

impl std::ops::Deref for RecursiveValue {
    type Target = Value<RecursiveValue>;

    fn deref(&self) -> &Self::Target {
        &self.0
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

impl From<Value<RecursiveValue>> for RecursiveValue {
    fn from(value: Value<Self>) -> Self {
        Self(value)
    }
}

impl<T: Valueable> From<Value<T>> for i32 {
    fn from(value: Value<T>) -> Self {
        value.to_int()
    }
}

impl<T: Valueable> From<Value<T>> for f32 {
    fn from(value: Value<T>) -> Self {
        value.to_float()
    }
}

impl<T: Valueable> From<Value<T>> for bool {
    fn from(value: Value<T>) -> Self {
        value.is_truthy()
    }
}

impl From<RecursiveValue> for Value<RecursiveValue> {
    fn from(value: RecursiveValue) -> Self {
        value.0
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

impl<T: Valueable> From<f32> for Value<T> {
	fn from(value: f32) -> Self {
        Value::Float(NotNan::new(value).expect("Got a NaN float"))
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
    type Output = Value<T>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(b1 || b2), // or
            (Value::Bool(b), Value::Natural(i)) | (Value::Natural(i), Value::Bool(b)) => Value::Natural(b as i32 + i),
            (Value::Bool(b), Value::Float(f)) | (Value::Float(f), Value::Bool(b)) => Value::Float(NotNan::new(b as u8 as f32).unwrap() + f),
            (Value::String(s1), Value::String(s2)) => Value::String(s1.add(&s2)),
            (Value::Natural(i1), Value::Natural(i2)) => Value::Natural(i1 + i2),
            (Value::Natural(i), Value::Float(f)) | (Value::Float(f), Value::Natural(i)) => Value::Float(NotNan::new(i as f32).unwrap() + f),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 + f2),
            (Value::Dict(d1), Value::Dict(d2)) => Value::Dict(d1.into_iter().chain(d2).collect()),
            (Value::Array(a1), Value::Array(a2)) => Value::Array(a1.into_iter().chain(a2).collect()),
            (v1, v2) => panic!("Type Error! Can not perform + on {:?} and {:?}", v1, v2)
        }
    }
}

impl<T: Valueable> Sub for Value<T> {
    type Output = Value<T>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(b1 && !b2), // nand
            (Value::Bool(b), Value::Natural(i)) | (Value::Natural(i), Value::Bool(b)) => Value::Natural(b as i32 - i),
            (Value::Bool(b), Value::Float(f)) | (Value::Float(f), Value::Bool(b)) => Value::Float(NotNan::new(b as u8 as f32).unwrap() - f),
            (Value::Natural(i1), Value::Natural(i2)) => Value::Natural(i1 - i2),
            (Value::Natural(i), Value::Float(f)) | (Value::Float(f), Value::Natural(i)) => Value::Float(NotNan::new(i as f32).unwrap() - f),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 - f2),
            (Value::Array(a1), Value::Array(a2)) => Value::Array(a1.into_iter().filter(|x| !a2.contains(x)).collect()),
            (v1, v2) => panic!("Type Error! Can not perform - on {:?} and {:?}", v1, v2)
        }
    }
}

impl<T: Valueable> Mul for Value<T> {
    type Output = Value<T>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(b1 && b2), // and
            (Value::Bool(b), Value::Natural(i)) | (Value::Natural(i), Value::Bool(b)) => Value::Natural(b as i32 * i),
            (Value::Bool(b), Value::Float(f)) | (Value::Float(f), Value::Bool(b)) => Value::Float(NotNan::new(b as u8 as f32).unwrap() * f),
            (Value::Natural(i1), Value::Natural(i2)) => Value::Natural(i1 * i2),
            (Value::Natural(i), Value::Float(f)) | (Value::Float(f), Value::Natural(i)) => Value::Float(NotNan::new(i as f32).unwrap() * f),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 * f2),
            (v1, v2) => panic!("Type Error! Can not perform * on {:?} and {:?}", v1, v2)
        }
    }
}

impl<T: Valueable> Div for Value<T> {
    type Output = Value<T>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Bool(b1), Value::Bool(b2)) => Value::Bool((b1 || b2) && !(b1 && b2)), // xor
            (Value::Bool(b), Value::Natural(i)) => Value::Natural(b as i32 / i), 
            (Value::Natural(i), Value::Bool(b)) => Value::Natural(i / b as i32),
            (Value::Bool(b), Value::Float(f)) => Value::Float(NotNan::new(b as u8 as f32).unwrap() / f),
            (Value::Float(f), Value::Bool(b)) => Value::Float(f / NotNan::new(b as u8 as f32).unwrap()),

            (Value::Natural(i1), Value::Natural(i2)) => Value::Natural(i1 / i2),
            (Value::Natural(i), Value::Float(f)) => Value::Float(NotNan::new(i as f32).unwrap() / f),
            (Value::Float(f), Value::Natural(i)) => Value::Float(f / NotNan::new(i as f32).unwrap()),

            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 / f2),
            (v1, v2) => panic!("Type Error! Can not perform / on {:?} and {:?}", v1, v2)
        }
    }
}

impl<T: Valueable> Not for Value<T> {
    type Output = Value<T>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(b) => Value::Bool(!b),
            v => panic!("Type Error! Can only negate booleans, not {:?}", v)
        }
    }
}

impl<T: Valueable> Ord for Value<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Value::String(s1), Value::String(s2)) => s1.cmp(s2),
            (Value::Natural(i1), Value::Natural(i2)) => i1.cmp(i2),
            (Value::Float(f1), Value::Float(f2)) => f1.cmp(f2),
            (Value::Bool(b1), Value::Bool(b2)) => b1.cmp(b2),
            (Value::Bool(b), Value::Natural(i)) => {
            	if *b {
            		1.cmp(i)
            	} else {
            		0.cmp(i)
            	}
            },
            (Value::Natural(i), Value::Bool(b)) => {
            	if *b {
            		i.cmp(&1)
            	} else {
            		i.cmp(&0)
            	}
            },
            (Value::Bool(b), Value::Float(f)) => {
            	if *b {
            		NotNan::new(1.0).unwrap().cmp(f)
            	} else {
            		NotNan::new(0.0).unwrap().cmp(f)
            	}
            },
            (Value::Float(f), Value::Bool(b)) => {
            	if *b {
            		f.cmp(&NotNan::new(1.0).unwrap())
            	} else {
            		f.cmp(&NotNan::new(0.0).unwrap())
            	}
            },
            (v1, v2) => panic!("Type Error! Can not compare {:?} with {:?}", v1, v2)
        }
    }
}

impl<T: Valueable> PartialOrd for Value<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
/// An error for when we fail to convert to a `Value` from an `Atom`.
///
/// In many cases, it is nice to easily convert a `wagon-parser::parser::atom::Atom` into a `Value` since they are essentially the same information.
/// However, this is not always fully possible. This struct represents that error.
pub struct FromAtomError(Atom);

impl Display for FromAtomError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Cannot convert {} to a Value", self.0)
    }
}

impl std::error::Error for FromAtomError {}

impl<T: Valueable> TryFrom<Atom> for Value<T> {
    type Error = FromAtomError;

    fn try_from(value: Atom) -> Result<Self, Self::Error> {
        match value {
            Atom::Ident(_) | Atom::Dict(_) | Atom::Group(_) => Err(FromAtomError(value)),
            Atom::LitBool(b) => Ok(Self::Bool(b)),
            Atom::LitNum(i) => Ok(Self::Natural(i)),
            Atom::LitFloat(f) => Ok(Self::Float(f)),
            Atom::LitString(s) => Ok(Self::String(s)),
        }
    }
}

impl TryFrom<Atom> for RecursiveValue {
    type Error = FromAtomError;

    fn try_from(value: Atom) -> Result<Self, Self::Error> {
        Ok(Self::from(Value::try_from(value)?))
    }
}