use crate::lexer::UnsafeNext;
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError};
use super::helpers::{between, between_right};
use crate::either_token;
use crate::lexer::{math::Math, productions::Productions, ident::Ident, Spannable};

use wagon_macros::match_error;
use super::expression::Expression;

#[derive(PartialEq, Debug)]
pub(crate) struct Dictionary(Ident, Expression);

#[derive(PartialEq, Debug)]
pub(crate) enum Atom {
	Ident(Ident),
	Dict(Dictionary),
	LitBool(bool),
	LitNum(i32),
	LitFloat(f32),
	LitString(String),
	Group(Expression)
}

impl Parse for Atom {

	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
	    match_error!(match lexer.next_unwrap() {
	    	#[expect("identifier or dictionary")]
	        either_token!(Identifier(x)) => {
	        	if let Ok(inner) = between(lexer, Tokens::MathToken(Math::LBr), Tokens::MathToken(Math::RBr)) {
	        		Ok(Self::Dict(Dictionary(x, inner)))
	        	} else {
	        		Ok(Self::Ident(x))
	        	}
	        },
	        Tokens::MathToken(Math::LitBool(x)) => Ok(Self::LitBool(x)),
	        Tokens::MathToken(Math::LitInt(x)) => Ok(Self::LitNum(x)),
	        Tokens::MathToken(Math::LitFloat(x)) => Ok(Self::LitFloat(x)),
	        #[expect("string")]
	        either_token!(LitString(x)) => Ok(Self::LitString(x)),
	        Tokens::MathToken(Math::LPar) => {
	        	let resp = between_right(lexer, Tokens::MathToken(Math::RPar))?;
	        	Ok(Self::Group(resp))
	        },
	    })
	}
}