use crate::{Value, ValueResult};

/// A trait to allow "extension" of the [`Value`] enum.
///
/// Sometimes, the basic types supported by `Value` are not enough and the newtype pattern is required to extend it.
/// Registering this newtype as `Valueable` means that it supports all common operations associated with a `Value`.
///
/// Besides implementing the required methods. Any type that implements [`Valueable`] is required to implement the following traits: 
/// * [`std::fmt::Debug`]
/// * [`std::fmt::Display`]
/// * [`Clone`]
/// * [`Eq`]
/// * [`PartialEq`]
/// * [`PartialOrd`]
/// * [`std::hash::Hash`]
/// * [`std::ops::Add`]
/// * [`std::ops::Sub`]
/// * [`std::ops::Mul`]
/// * [`std::ops::Div`]
/// * [`std::ops::Rem`]
/// * [`num_traits::Pow<Self>`]
/// This is required such that we can expect any `Valueable` type to work more or less the same.
///
/// Most of the required traits can be automatically derived. You can use [`wagon_macros::ValueOps`] (or it's associated methods) to 
/// automatically derive implementations for all the operative traits (`Add`, `Sub`, etc).
pub trait Valueable: 
    std::fmt::Debug 
    + std::fmt::Display 
    + Clone
    + Eq 
    + PartialEq 
    + PartialOrd
    + std::hash::Hash 
    + std::ops::Add
    + std::ops::Sub
    + std::ops::Mul
    + std::ops::Div
    + std::ops::Rem
    + num_traits::Pow<Self> 
    {
    /// Is this value seen as `true` or `false`?
    ///
    /// # Errors
    /// Should return an error if this value can not be converted into a `bool`.
    fn is_truthy(&self) -> ValueResult<bool, Self>;
    /// Convert the value to a regular [`i32`].
    ///
    /// # Errors
    /// Should return an error if this value can not be converted into an `i32`.
    fn to_int(&self) -> ValueResult<i32, Self>;
    /// Convert the value to a regular [`f32`].
    ///
    /// # Errors
    /// Should return an error if this value can not be converted into an `f32`.
    fn to_float(&self) -> ValueResult<f32, Self>;
    /// Get a string representation of the value, as if it were a number. 
    ///
    /// # Errors
    /// Should return an error if this value can not be displayed as a number
    fn display_numerical(&self) -> ValueResult<String, Self>;
}

/// A second trait for "extension" of the [`Value`] enum.
///
/// This is intended to "extract" the inner value if possible and makes automatic implementation
/// of [`Valueable`] possible. 
/// If your type properly wraps [`Value`] (as in, it implements [`ToValue`], [`From<Value<Self>>`] and all the expected operations),
/// [`Valueable`] is automatically implemented by simply taking the value returned by [`ToValue::to_value`] and using that implementation. 
/// If you do not desire this behaviour, do not implement [`ToValue`].
pub trait ToValue<T: Valueable> {
    /// Return a reference to the [`Value`] that this type encompasses.
    fn to_value(&self) -> &Value<T>;
}

impl<T: 
    ToValue<T> 
    + From<Value<T>> 
    + Clone
    + Eq 
    + PartialEq 
    + PartialOrd
    + std::fmt::Debug 
    + std::fmt::Display 
    + std::hash::Hash 
    + std::ops::Add
    + std::ops::Sub
    + std::ops::Mul
    + std::ops::Div
    + std::ops::Rem
    + num_traits::Pow<T>> 
    Valueable for T {
    fn is_truthy(&self) -> ValueResult<bool, Self> {
        Ok(self.to_value().is_truthy()?)
    }

    fn to_int(&self) -> ValueResult<i32, Self> {
        Ok(self.to_value().to_int()?)
    }

    fn to_float(&self) -> ValueResult<f32, Self> {
        Ok(self.to_value().to_float()?)
    }

    fn display_numerical(&self) -> ValueResult<String, Self> {
        Ok(self.to_value().display_numerical()?)
    }
}
