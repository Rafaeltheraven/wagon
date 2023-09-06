use super::{Parse, PeekLexer, ParseResult, ParseOption, Tokens};


use crate::lexer::{math::Math, UnsafePeek};

use super::helpers::TokenMapper;
use wagon_macros::TokenMapper;
use super::factor::Factor;

/*
Term -> Term Op Factor | Factor
|
V
Term -> Factor Term'
Term' -> Op Factor Term' | epsilon
*/

#[derive(PartialEq, Debug)]
pub(crate) struct Term {
	pub(crate) left: Factor,
	pub(crate) cont: Option<TermP>
}

#[derive(PartialEq, Debug)]
pub(crate) struct TermP {
	pub(crate) op: Op2,
	pub(crate) right: Factor,
	pub(crate) cont: Box<Option<TermP>>
}

impl Parse for Term {

	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
		Ok(Self {
			left: Factor::parse(lexer)?,
			cont: TermP::parse_option(lexer)?
		})
	}
}

impl ParseOption for TermP {

	fn parse_option(lexer: &mut PeekLexer) -> ParseResult<Option<Self>> where Self: Sized {
	    if let Some(op) = Op2::token_to_enum(lexer.peek_unwrap()) {
	    	lexer.next();
	    	Ok(Some(TermP { op, right: Factor::parse(lexer)?, cont: Box::new(TermP::parse_option(lexer)?) }))
	    } else {
	    	Ok(None)
	    }
	}
}

#[derive(TokenMapper, PartialEq, Debug)]
pub(crate) enum Op2 {
	Mul,
	Div,
	Floor,
	Mod
}
