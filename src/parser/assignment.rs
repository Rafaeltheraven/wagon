use crate::lexer::UnsafeNext;
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError};
use crate::lexer::{math::Math, ident::Ident, Spannable};

use wagon_macros::match_error;
use super::expression::Expression;

#[derive(PartialEq, Debug)]
pub(crate) struct Assignment {
	pub(crate) ident: Ident,
	pub(crate) expr: Expression
}

impl Parse for Assignment {
	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
		let ident = match_error!(match lexer.next_unwrap() {
			Tokens::MathToken(Math::Identifier(x)) => Ok(x),
		})?;
		let next = lexer.next_unwrap();
		if next != Tokens::MathToken(Math::Assigns) {
			Err(WagParseError::Unexpected { span: lexer.span(), offender: next, expected: vec!["=".to_string()] })
		} else {
			Ok(Self {ident, expr: Expression::parse(lexer)?})
		}
	}
}