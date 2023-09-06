use super::{Parse, PeekLexer, ParseResult, ParseOption, Tokens};

use super::helpers::TokenMapper;
use crate::lexer::{math::Math, UnsafePeek};

use super::sum::Sum;
use wagon_macros::TokenMapper;

#[derive(PartialEq, Debug)]
pub(crate) struct Comparison {
	pub(crate) sum: Sum,
	pub(crate) comp: Option<Comp>
}

#[derive(PartialEq, Debug)]
pub(crate) struct Comp {
	pub(crate) op: CompOp,
	pub(crate) right: Sum
}

#[derive(TokenMapper, PartialEq, Debug)]
pub(crate) enum CompOp {
	Eq,
	Neq,
	Lte,
	Lt,
	Gte,
	Gt,
	In
}

impl Parse for Comparison {

    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized {
        Ok(Self { sum: Sum::parse(lexer)?, comp: Comp::parse_option(lexer)? })
    }

}

impl ParseOption for Comp {

    fn parse_option(lexer: &mut PeekLexer) -> ParseResult<Option<Self>> where Self: Sized {
        if let Some(op) = CompOp::token_to_enum(lexer.peek_unwrap()) {
        	lexer.next();
        	Ok(Some(Self { op, right: Sum::parse(lexer)?}))
        } else {
        	Ok(None)
        }
    }

}