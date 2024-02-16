use std::fmt::Display;
use std::hash::Hash;
use std::write;

use super::{Parse, LexerBridge, ParseResult, Tokens, WagParseError, Ident, SpannableNode, expression::Expression, ResultNext};
use super::helpers::{between, between_right};
use crate::either_token;
use crate::firstpass::GetReqAttributes;

use wagon_lexer::{math::Math, Spannable};
use wagon_macros::match_error;
use wagon_utils::ConversionError;
use wagon_value::{Valueable, Value, RecursiveValue};

use ordered_float::NotNan;

use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
/// A python-style dictionary.
///
/// Is of the form [`Ident`][[`Expression`]].
pub struct Dictionary(Ident, Expression);

impl Dictionary {
	/// Deconstruct the dictionary into it's [`Ident`] and [`Expression`].
	#[must_use] 
	pub const fn deconstruct(&self) -> (&Ident, &Expression) {
		(&self.0, &self.1)
	}
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[new_unspanned]
/// The base elements of each expression.
///
/// The data in here is kind of similar to [`wagon_value::Value`] and `TryFrom` is implemented for it as a result.
/// 
/// However, an `Atom` includes additional syntactic data, which is not important (or even not available) for `Value` (for example, an [`Ident`]).
/// As a result, [`Atom::Ident`], [`Atom::Dict`] and [`Atom::Group`] can not be directly converted and manual implementation is required.
pub enum Atom {
	/// An [`Ident`].
	Ident(Ident),
	/// A [`Dictionary`].
	Dict(Dictionary),
	/// A [`bool`].
	LitBool(bool),
	/// An [`i32`].
	LitNum(i32),
	/// An [`f32`].
	LitFloat(NotNan<f32>),
	/// A [`String`].
	LitString(String),
	/// Another full [`Expression`]. Enclosed by `()`.
	Group(SpannableNode<Expression>)
}

impl Parse for Atom {

	fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> {
	    match_error!(match lexer.next_result()? {
	    	#[expect("identifier or dictionary")]
	        either_token!(Identifier(x)) => {
	        	if let Ok(inner) = between(lexer, &Tokens::MathToken(Math::LBr), &Tokens::MathToken(Math::RBr)) {
	        		Ok(Self::Dict(Dictionary(x, inner)))
	        	} else {
	        		Ok(Self::Ident(x))
	        	}
	        },
	        Tokens::MathToken(Math::LitBool(x)) => Ok(Self::LitBool(x)),
	        Tokens::MathToken(Math::LitInt(x)) => Ok(Self::LitNum(x)),
	        Tokens::MathToken(Math::LitFloat(x)) => {
	        	match NotNan::new(x) {
				    Ok(f) => Ok(Self::LitFloat(f)),
				    Err(e) => Err(WagParseError::FloatError(e, lexer.span())),
				}
	        },
	        #[expect("string")]
	        either_token!(LitString(x)) => Ok(Self::LitString(x)),
	        Tokens::MathToken(Math::LPar) => {
	        	let resp = between_right(lexer, &Tokens::MathToken(Math::RPar))?;
	        	Ok(Self::Group(resp))
	        },
	    })
	}
}

impl GetReqAttributes for Atom {
    fn get_req_attributes(&self) -> crate::firstpass::ReqAttributes {
        match self {
        	Self::Ident(i) => { 
        		let mut req = crate::firstpass::ReqAttributes::new();
        		req.insert(i.clone().into());
        		req
        	},
        	Self::Group(e) => e.get_req_attributes(),
        	_ => crate::firstpass::ReqAttributes::new()
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(x) => write!(f, "{x}"),
            Self::Dict(x) => write!(f, "{x}"),
            Self::LitBool(x) => write!(f, "{x}"),
            Self::LitNum(x) => write!(f, "{x}"),
            Self::LitFloat(x) => write!(f, "{x}"),
            Self::LitString(x) => write!(f, "\"{x}\""),
            Self::Group(x) => write!(f, "({x})"),
        }
    }
}

impl Display for Dictionary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.0, self.1)
    }
}

impl<T: Valueable> TryFrom<Atom> for Value<T> {
    type Error = ConversionError<Atom, Self>;

    fn try_from(value: Atom) -> Result<Self, Self::Error> {
        match value {
            Atom::LitBool(b) => Ok(Self::Bool(b)),
            Atom::LitNum(i) => Ok(Self::Natural(i)),
            Atom::LitFloat(f) => Ok(Self::Float(f)),
            Atom::LitString(s) => Ok(Self::String(s)),
            other => Err(ConversionError::new(other)),
        }
    }
}

impl TryFrom<Atom> for RecursiveValue {
    type Error = ConversionError<Atom, Self>;

    fn try_from(value: Atom) -> Result<Self, Self::Error> {
        Ok(Self::from(Value::try_from(value).map_err(wagon_utils::ConversionError::convert)?))
    }
}
