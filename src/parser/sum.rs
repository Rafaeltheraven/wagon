use super::{Parse, PeekLexer, ParseResult, ParseOption, Tokens};

use crate::lexer::{math::Math, UnsafePeek};

use super::term::Term;
use super::helpers::TokenMapper;
use wagon_macros::TokenMapper;

#[derive(PartialEq, Debug)]
pub(crate) struct Sum {
	pub(crate) left: Term,
	pub(crate) cont: Option<SumP>
}

#[derive(PartialEq, Debug)]
pub(crate) struct SumP {
	pub(crate) op: Op1,
	pub(crate) right: Term,
	pub(crate) cont: Box<Option<SumP>>
}

impl Parse for Sum {

	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
		Ok(Self {
			left: Term::parse(lexer)?,
			cont: SumP::parse_option(lexer)?
		})
	}
}

impl ParseOption for SumP {

	fn parse_option(lexer: &mut PeekLexer) -> ParseResult<Option<Self>> where Self: Sized {
	    if let Some(op) = Op1::token_to_enum(lexer.peek_unwrap()) {
	    	lexer.next();
	    	Ok(Some(SumP { op, right: Term::parse(lexer)?, cont: Box::new(SumP::parse_option(lexer)?) }))
	    } else {
	    	Ok(None)
	    }
	}
}

#[derive(TokenMapper, PartialEq, Debug)]
pub(crate) enum Op1 {
	Add,
	Sub
}
