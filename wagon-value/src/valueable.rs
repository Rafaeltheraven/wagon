use crate::{Value, ValueResult};

/// A trait to allow "extension" of the [`Value`] enum.
///
/// Sometimes, the basic types supported by `Value` are not enough and the newtype pattern is required to extend it.
/// Registering this newtype as `Valueable` means that it supports all common operations associated with a `Value`.
pub trait Valueable: std::fmt::Debug + std::fmt::Display + PartialEq + std::hash::Hash + Eq + Clone {
    /// Is this value seen as `true` or `false`?
    fn is_truthy(&self) -> ValueResult<bool, Self>;
    /// Convert the value to a regular [`i32`].
    fn to_int(&self) -> ValueResult<i32, Self>;
    /// Convert the value to a regular [`f32`].
    fn to_float(&self) -> ValueResult<f32, Self>;
    /// Calculate this value to the power of another.
    fn pow(&self, rhs: &Self) -> ValueResult<Self, Self>;
    /// Get a string representation of the value, as if it were a number. 
    fn display_numerical(&self) -> ValueResult<String, Self>;
}

/// A second trait for "extension" of the [`Value`] enum.
///
/// This is intended to "extract" the inner value if possible.
pub trait ToValue<T: Valueable> {
    /// Return a reference to the [`Value`] that this type encompasses.
    fn to_value(&self) -> &Value<T>;
}

impl<T: ToValue<T> + From<Value<T>> + Clone + Eq + std::hash::Hash + std::fmt::Debug + std::fmt::Display> Valueable for T {
    fn is_truthy(&self) -> ValueResult<bool, Self> {
        Ok(self.to_value().is_truthy()?)
    }

    fn to_int(&self) -> ValueResult<i32, Self> {
        Ok(self.to_value().to_int()?)
    }

    fn to_float(&self) -> ValueResult<f32, Self> {
        Ok(self.to_value().to_float()?)
    }

    fn pow<'a>(&'a self, rhs: &'a Self) -> ValueResult<Self, Self> {
        Ok(Self::from(self.to_value().pow(rhs.to_value())?))
    }

    fn display_numerical(&self) -> ValueResult<String, Self> {
        Ok(self.to_value().display_numerical()?)
    }
}
