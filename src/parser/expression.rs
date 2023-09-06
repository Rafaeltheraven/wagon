use super::{Parse, PeekLexer, ParseResult, Tokens, Spannable, WagParseError};
use crate::lexer::{math::Math, UnsafeNext, UnsafePeek};

use wagon_macros::match_error;
use super::disjunct::Disjunct;

#[derive(PartialEq, Debug)]
pub(crate) enum Expression {
	Subproc(String),
	If {
		this: Disjunct,
		then: Disjunct,
		r#else: Option<Box<Expression>>
	},
	Disjunct(Disjunct),
}

impl Parse for Expression {

	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> { 
		match lexer.peek_unwrap() {
			Tokens::MathToken(Math::If) => {lexer.next(); Self::parse_if(lexer)},
			Tokens::MathToken(Math::Bash(expr)) => {let resp = Ok(Self::Subproc(expr.to_string())); lexer.next(); resp},
			_ => Ok(Self::Disjunct(Disjunct::parse(lexer)?))
		}
	}
}

impl Expression {

	fn parse_if(lexer: &mut PeekLexer) -> ParseResult<Self> {
		let this = Disjunct::parse(lexer)?;
		let then = match_error!(match lexer.next_unwrap() {
			Tokens::MathToken(Math::Then) => Disjunct::parse(lexer)
		})?;
		let r#else = match lexer.peek_unwrap() {
		    Tokens::MathToken(Math::Else) => {lexer.next(); Some(Box::new(Expression::parse(lexer)?))},
		    _ => None
		};
		Ok(Self::If { this, then, r#else })
	}

}