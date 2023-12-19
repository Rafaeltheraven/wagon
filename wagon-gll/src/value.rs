use std::{collections::BTreeMap, fmt::Display, write, ops::{Add, Sub, Mul, Div, Not}};

use ordered_float::{NotNan, Pow};

use crate::GLLBlockLabel;

#[derive(Debug, Eq, Hash, Clone)]
pub enum Value<'a> {
	Bool(bool),
	String(String),
	Natural(i32),
	Float(NotNan<f32>),
	Label(GLLBlockLabel<'a>),
	Dict(BTreeMap<String, Value<'a>>),
	Array(Vec<Value<'a>>)
}

// For some reason the derive breaks here but manually copying over the generated code works
impl PartialEq for Value<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(f0_self), Value::Bool(f0_other)) => f0_self.eq(f0_other),
            (Value::String(f0_self), Value::String(f0_other)) => f0_self.eq(f0_other),
            (Value::Natural(f0_self), Value::Natural(f0_other)) => f0_self.eq(f0_other),
            (Value::Float(f0_self), Value::Float(f0_other)) => f0_self.eq(f0_other),
            (Value::Label(f0_self), Value::Label(f0_other)) => f0_self.eq(f0_other),
            (Value::Dict(f0_self), Value::Dict(f0_other)) => f0_self.eq(f0_other),
            (Value::Array(f0_self), Value::Array(f0_other)) => f0_self.eq(f0_other),
            _unused => false,
        }
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Natural(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Label(v) => write!(f, "{}", v.to_string()),
            Value::Dict(v) => write!(f, "{:?}", v),
            Value::Array(v) => write!(f, "{:?}", v),
        }
    }
}

impl Value<'_> {
	fn is_truthy(&self) -> bool {
		match self {
		    Value::Bool(b) => *b,
		    Value::String(s) => !s.is_empty(),
		    Value::Natural(n) => n > &0,
		    Value::Float(f) => !f.eq(&0.0),
		    Value::Label(l) => l.is_eps(),
		    Value::Dict(m) => !m.is_empty(),
		    Value::Array(a) => !a.is_empty(),
		}
	}

	pub(crate) fn to_int(&self) -> i32 {
		match self {
			Value::Natural(n) => *n,
			Value::Float(f) => f.round() as i32,
			o => if o.is_truthy() { 1 } else { 0 }
		}
	}

	pub(crate) fn to_float(&self) -> f32 {
		match self {
			Value::Natural(n) => *n as f32,
			Value::Float(f) => f.into_inner(),
			o => if o.is_truthy() { 1.0 } else { 0.0 }
		}
	}

	pub fn pow(self, rhs: Value) -> Value {
		match (self, rhs) {
            (Value::Bool(true), Value::Natural(_)) | (Value::Natural(_), Value::Bool(false)) => Value::Natural(1),
            (Value::Bool(false), Value::Natural(_)) => Value::Natural(0),
            (Value::Natural(i), Value::Bool(true)) => Value::Natural(i),
            (Value::Bool(true), Value::Float(_)) | (Value::Float(_), Value::Bool(false)) => Value::Float(NotNan::new(1.0).unwrap()),
            (Value::Bool(false), Value::Float(_)) => Value::Float(NotNan::new(0.0).unwrap()),
            (Value::Float(f), Value::Bool(true)) => Value::Float(f),
            (Value::Natural(i1), Value::Natural(i2)) if i2 >= 0 => Value::Natural(i1.pow(i2.try_into().unwrap())),
            (Value::Natural(i1), Value::Natural(i2)) => Value::Float(NotNan::new(1.0 / (i1.pow((-i2) as u32) as f32)).unwrap()), // Power of negative number is division
            (Value::Natural(i), Value::Float(f)) => Value::Float(NotNan::new(i as f32).unwrap().pow(f)),
            (Value::Float(f), Value::Natural(i)) => Value::Float(f.pow(i)),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1.pow(f2)),
            (v1, v2) => panic!("Type Error! Can not perform ** on {:?} and {:?}", v1, v2)
        }
	}
}

impl From<Value<'_>> for i32 {
    fn from(value: Value<'_>) -> Self {
        value.to_int()
    }
}

impl From<Value<'_>> for f32 {
    fn from(value: Value<'_>) -> Self {
        value.to_float()
    }
}

impl From<Value<'_>> for bool {
    fn from(value: Value<'_>) -> Self {
        value.is_truthy()
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

impl From<bool> for Value<'_> {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<String> for Value<'_> {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<i32> for Value<'_> {
	fn from(value: i32) -> Self {
        Value::Natural(value)
    }
}

impl From<f32> for Value<'_> {
	fn from(value: f32) -> Self {
        Value::Float(NotNan::new(value).expect("Got a NaN float"))
    }
}

impl<'a> From<GLLBlockLabel<'a>> for Value<'a> {
    fn from(value: GLLBlockLabel<'a>) -> Self {
        Value::Label(value)
    }
}

impl<'a> From<BTreeMap<String, Value<'a>>> for Value<'a> {
    fn from(value: BTreeMap<String, Value<'a>>) -> Self {
        Self::Dict(value)
    }
}

impl<'a> From<Vec<Value<'a>>> for Value<'a> {
    fn from(value: Vec<Value<'a>>) -> Self {
        Self::Array(value)
    }
}

impl<'a> Add for Value<'a> {
    type Output = Value<'a>;

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

impl<'a> Sub for Value<'a> {
    type Output = Value<'a>;

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

impl<'a> Mul for Value<'a> {
    type Output = Value<'a>;

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

impl<'a> Div for Value<'a> {
    type Output = Value<'a>;

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

impl<'a> Not for Value<'a> {
    type Output = Value<'a>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(b) => Value::Bool(!b),
            v => panic!("Type Error! Can only negate booleans, not {:?}", v)
        }
    }
}

impl<'a> Ord for Value<'a> {
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

impl <'a> PartialOrd for Value<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

