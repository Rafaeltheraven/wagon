use std::error::Error;
use std::fmt::Display;
use crate::{Value, Valueable};

/// A result type for operations on a [`Valueable`].
///
/// Either returns something of type `T`. Or a [`ValueError`] over `U`.
pub type ValueResult<T, U> = Result<T, ValueError<U>>;

#[derive(Debug, Eq, PartialEq, Clone)]
/// Errors that can occur while dealing with semi-dynamic [`Value`]s.
///
/// A [`ValueError<Value<T>>`] can be automatically converted into a [`ValueError<T>`] when you are 
/// implementing your own [`Valueable`] type, but the compiler may require some manual convincing to get it to work
/// (as in, you will have to call `from` manually).
pub enum ValueError<T: Valueable> {
    /// Something horrible happened for which we have no specific error.
    Fatal(String),
    /// Tried to perform some operation on two incompatible values.
    OperationError(T, T, String),
    /// Tried to perform an operation on a float which is NaN.
    FloatIsNan(ordered_float::FloatIsNan),
    /// Tried to convert something from an int and failed.
    IntConversionError(std::num::TryFromIntError),
    /// This type can not be negated.
    NegationError(T),
    /// Tried to compare a value to another value and failed.
    ComparisonError(T, T),
    /// Tried to convert `T` to [`Value`] but failed
    ConversionError(T),
}

impl<T: Valueable> Error for ValueError<T> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::FloatIsNan(e) => Some(e),
            Self::IntConversionError(e) => Some(e),
            _ => None
        }
    }
}

impl<T: Valueable> Display for ValueError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fatal(s) => write!(f, "{s}"),
            Self::OperationError(v1, v2, o) => write!(f, "Can not perform {o} on values of type {v1:?} and {v2:?}"),
            Self::FloatIsNan(e) => e.fmt(f),
            Self::IntConversionError(e) => e.fmt(f),
            Self::NegationError(v) => write!(f, "Can not negate value of type {v:?}"),
            Self::ComparisonError(v1, v2) => write!(f, "Can not compare value of type {v1:?} with value of type {v2:?}"),
            Self::ConversionError(v) => write!(f, "Unable to convert {v:?} to inner value type")
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
            ValueError::OperationError(v1, v2, o) => Self::OperationError(T::from(v1), T::from(v2), o),
            ValueError::FloatIsNan(e) => Self::FloatIsNan(e),
            ValueError::IntConversionError(e) => Self::IntConversionError(e),
            ValueError::NegationError(v) => Self::NegationError(T::from(v)),
            ValueError::ComparisonError(v1, v2) => Self::ComparisonError(T::from(v1), T::from(v2)),
            ValueError::ConversionError(v) => Self::ConversionError(T::from(v))
        }
    }
}

impl<T: Valueable> From<std::convert::Infallible> for ValueError<T> {
    fn from(e: std::convert::Infallible) -> Self {
        Self::Fatal(format!("Got an error from infallible, which should be impossible: {e:?}"))
    }
}
