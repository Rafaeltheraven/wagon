use std::error::Error;
use std::fmt::Display;
use crate::{Value, Valueable};

/// A result type for operations on a [`Valueable`].
///
/// Either returns something of type `T`. Or a [`ValueError`] over `U`.
pub type ValueResult<T, U> = Result<T, ValueError<U>>;

#[derive(Debug, Eq, PartialEq, Clone)]
/// Errors that can occur while dealing with semi-dynamic [`Value`]s.
pub enum ValueError<T: Valueable> {
    /// Something horrible happened for which we have no specific error.
    Fatal(String),
    /// Tried to perform some operation on two incompatible values.
    OperationError(T, T, String),
    /// Tried to perform an operation on a float which is NaN.
    FloatIsNan(ordered_float::FloatIsNan),
    /// Tried to convert something to an int and failed.
    IntConversionError(std::num::TryFromIntError),
    /// This type can not be negated.
    NegationError(T),
    /// Tried to compare a value of to another value and failed.
    ComparisonError(T, T)
}

impl<T: Valueable> Error for ValueError<T> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ValueError::FloatIsNan(e) => Some(e),
            ValueError::IntConversionError(e) => Some(e),
            _ => None
        }
    }
}

impl<T: Valueable> Display for ValueError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueError::Fatal(s) => write!(f, "{}", s),
            ValueError::OperationError(v1, v2, o) => write!(f, "Can not perform {} on values of type {:?} and {:?}", o, v1, v2),
            ValueError::FloatIsNan(e) => e.fmt(f),
            ValueError::IntConversionError(e) => e.fmt(f),
            ValueError::NegationError(v) => write!(f, "Can not negate value of type {:?}", v),
            ValueError::ComparisonError(v1, v2) => write!(f, "Can not compare value of type {:?} with value of type {:?}", v1, v2),
        }
    }
}

impl<T: Valueable> From<ordered_float::FloatIsNan> for ValueError<T> {
    fn from(value: ordered_float::FloatIsNan) -> Self {
        Self::FloatIsNan(value)
    }
}

impl<T: Valueable> From<std::num::TryFromIntError> for ValueError<T> {
    fn from(value: std::num::TryFromIntError) -> Self {
        Self::IntConversionError(value)
    }
}

impl<T: Valueable + From<Value<T>>> From<ValueError<Value<T>>> for ValueError<T> {
    fn from(value: ValueError<Value<T>>) -> Self {
        match value {
            ValueError::Fatal(f) => Self::Fatal(f),
            ValueError::OperationError(v1, v2, o) => Self::OperationError(T::from(v1.clone()), T::from(v2.clone()), o),
            ValueError::FloatIsNan(e) => Self::FloatIsNan(e),
            ValueError::IntConversionError(e) => Self::IntConversionError(e),
            ValueError::NegationError(v) => Self::NegationError(T::from(v.clone())),
            ValueError::ComparisonError(v1, v2) => Self::ComparisonError(T::from(v1.clone()), T::from(v2.clone())),
        }
    }
}
