use std::collections::BTreeMap;
use std::{fmt::Display, write, error::Error};

use crate::GLLBlockLabel;
pub(crate) use wagon_value::Value as InnerValue;
pub(crate) use wagon_value::ValueError as InnerValueError;
pub(crate) use wagon_value::ValueResult as InnerValueResult;
pub use wagon_value::Valueable;

use wagon_macros::{ValueAdd, ValueSub, ValueMul, ValueDiv, ValueRem, ValuePow};

#[derive(Debug, Eq, Hash, Clone, ValueAdd, ValueSub, ValueMul, ValueDiv, ValueRem, ValuePow)]
/// An extension of [`wagon_value::Value`] that adds [`GLLBlockLabel`] as a possible type.
pub enum Value<'a> {
    /// Any regular [`wagon_value::Value`]
    #[value_variant]
    Value(InnerValue<Value<'a>>),
    /// A [`GLLBlockLabel`]
	Label(GLLBlockLabel<'a>),
}

#[derive(Debug)]
/// An extension of [`wagon_value::ValueError`] for specific errors related to dealing with [`GLLBlockLabel`].
pub enum ValueError<'a> {
    /// Any regular [`wagon_value::ValueError`]
    ValueError(InnerValueError<Value<'a>>),
    /// An error occured trying to convert a [`Value`] to a [`GLLBlockLabel`].
    ConvertToLabel(Value<'a>)
}

/// An quick result that either returns `T` or a [`ValueError`].
pub type ValueResult<'a, T> = Result<T, ValueError<'a>>;

impl<'a> From<InnerValueError<Value<'a>>> for ValueError<'a> {
    fn from(value: InnerValueError<Value<'a>>) -> Self {
        Self::ValueError(value)
    }
}

impl<'a> From<InnerValueError<InnerValue<Value<'a>>>> for ValueError<'a> {
    fn from(value: InnerValueError<InnerValue<Value<'a>>>) -> Self {
        Self::ValueError(value.into())
    }
}

impl Display for ValueError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueError::ValueError(e) => e.fmt(f),
            ValueError::ConvertToLabel(v) => write!(f, "Failed converting {v} to label"),
        }
    }
}

impl Error for ValueError<'static> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ValueError::ValueError(e) => Some(e),
            ValueError::ConvertToLabel(_) => None,
        }
    }
}

impl<'a> Valueable for Value<'a> {
    fn is_truthy(&self) -> InnerValueResult<bool, Self> {
        match self {
            Value::Value(v) => Ok(v.is_truthy()?),
            Value::Label(l) => Ok(l.is_eps()),
        }
    }

    fn to_int(&self) -> InnerValueResult<i32, Self> {
        match self {
            Value::Value(v) => Ok(v.to_int()?),
            o @ Value::Label(_) => Ok(i32::from(o.is_truthy()?))
        }
    }

    fn to_float(&self) -> InnerValueResult<f32, Self> {
        match self {
            Value::Value(v) => Ok(v.to_float()?),
            o @ Value::Label(_) => Ok(if o.is_truthy()? { 1.0 } else { 0.0 })
        }
    }

    fn display_numerical(&self) -> InnerValueResult<String, Self> {
        match self {
            Value::Value(v) => Ok(v.display_numerical()?),
            other @ Value::Label(_) => Ok(other.to_int()?.to_string())
        }
    }
}

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
            Value::Value(v) => write!(f, "{v}"),
            Value::Label(v) => write!(f, "{}", v.to_string()),
        }
    }
}

impl From<Value<'_>> for i32 { // Can't genericize these because Rust doesn't allow it
    #[allow(clippy::expect_used)]
    fn from(value: Value) -> Self {
        value.to_int().expect("This conversion can not fail")
    }
}

impl From<Value<'_>> for f32 {
    #[allow(clippy::expect_used)]
    fn from(value: Value) -> Self {
        value.to_float().expect("This conversion can not fail")
    }
}

impl From<Value<'_>> for bool {
    #[allow(clippy::expect_used)]
    fn from(value: Value) -> Self {
        value.is_truthy().expect("This conversion can not fail")
    }
}

impl<'a> From<bool> for Value<'a> {
    fn from(value: bool) -> Self {
        Self::Value(InnerValue::Bool(value))
    }
}

impl<'a> From<String> for Value<'a> {
    fn from(value: String) -> Self {
        Self::Value(InnerValue::String(value))
    }
}

impl<'a> From<i32> for Value<'a> {
    fn from(value: i32) -> Self {
        Self::Value(InnerValue::Natural(value))
    }
}

impl<'a> From<BTreeMap<String, Value<'a>>> for Value<'a> {
    fn from(value: BTreeMap<String, Value<'a>>) -> Self {
        Self::Value(InnerValue::Dict(value))
    }
}

impl<'a> From<Vec<Value<'a>>> for Value<'a> {
    fn from(value: Vec<Value<'a>>) -> Self {
        Self::Value(InnerValue::Array(value))
    }
}

impl<'a> From<InnerValue<Value<'a>>> for Value<'a> {
    fn from(value: InnerValue<Value<'a>>) -> Self {
        Self::Value(value)
    }
}

impl<'a> TryFrom<f32> for Value<'a> {
    type Error = ValueError<'a>;

    fn try_from(value: f32) -> Result<Self, Self::Error> {
        match InnerValue::try_from(value) {
            Ok(v) => Ok(Self::Value(v)),
            Err(e) => Err(InnerValueError::<Self>::from(e).into()),
        }
    }
}

impl<'a> TryFrom<Value<'a>> for GLLBlockLabel<'a> {
    type Error = ValueError<'a>;

    fn try_from(value: Value<'a>) -> Result<Self, Self::Error> {
        match value {
            Value::Label(l) => Ok(l),
            other @ Value::Value(_) => Err(ValueError::ConvertToLabel(other))
        }
    }
}

impl<'a> TryFrom<Value<'a>> for InnerValue<Value<'a>> {
    type Error = InnerValueError<Value<'a>>;

    fn try_from(value: Value<'a>) -> Result<Self, Self::Error> {
        match value {
            Value::Value(v) => Ok(v),
            other @ Value::Label(_) => Err(InnerValueError::ConversionError(other)),
        }
    }
}

impl<'a> std::ops::Not for Value<'a> {
    type Output = ValueResult<'a, Self>;

    fn not(self) -> Self::Output {
        match self {
            Value::Value(v) => Ok(Value::Value((!v)?)),
            v @ Value::Label(_) => Err(InnerValueError::NegationError(v).into())
        }
    }
}

impl<'a> PartialOrd for Value<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Value(v1), Value::Value(v2)) => v1.partial_cmp(v2),
            _ => None
        }
    }
}

impl<'a> PartialEq<InnerValue<Self>> for Value<'a> {
    fn eq(&self, other: &InnerValue<Self>) -> bool {
        match self {
            Value::Value(v) => v == other,
            Value::Label(_) => false,
        }
    }
}

impl<'a> PartialEq<Value<'a>> for InnerValue<Value<'a>> {
    fn eq(&self, other: &Value<'a>) -> bool {
        match other {
            Value::Value(v) => v == self,
            Value::Label(_) => false,
        }
    }
}

impl<'a> PartialOrd<InnerValue<Self>> for Value<'a> {
    fn partial_cmp(&self, other: &InnerValue<Self>) -> Option<std::cmp::Ordering> {
        match self {
            Value::Value(v) => v.partial_cmp(other),
            Value::Label(_) => None,
        }
    }
}

impl<'a> PartialOrd<Value<'a>> for InnerValue<Value<'a>> {
    fn partial_cmp(&self, other: &Value<'a>) -> Option<std::cmp::Ordering> {
        match other {
            Value::Value(v) => self.partial_cmp(v),
            Value::Label(_) => None,
        }
    }
}