#![warn(missing_docs)]
//! Pseudo-dynamically typed values for WAGon.
//!
//! Because of the strict type system in Rust, it's nice to have a more lenient data structure that can deal with various types and operations called on it.
//! 
//! This crate holds code for those purposes.

mod value;
mod valueable;
mod result;

use std::fmt::Display;

pub use value::{Value, Pow};
pub use valueable::{Valueable, ToValue};
pub use result::{ValueError, ValueResult};

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
/// use wagon_value::Value;
/// ```
/// Do this:
/// ```
/// use wagon_value::RecursiveValue as Value;
/// ```
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct RecursiveValue(Value<RecursiveValue>);

impl Display for RecursiveValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl ToValue<Self> for RecursiveValue {
    fn to_value(&self) -> &Value<Self> {
        &self.0
    }
}

impl std::ops::Deref for RecursiveValue {
    type Target = Value<Self>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Value<Self>> for RecursiveValue {
    fn from(value: Value<Self>) -> Self {
        Self(value)
    }
}

impl From<RecursiveValue> for Value<RecursiveValue> {
    fn from(value: RecursiveValue) -> Self {
        value.0
    }
}
