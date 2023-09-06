use crate::lexer::{UnsafePeek, Spannable};
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError};
use super::helpers::between;

use crate::lexer::{productions::Productions, math::Math};

use super::expression::Expression;
use super::chunk::Chunk;

#[derive(PartialEq, Debug)]
pub(crate) struct Rhs {
	pub(crate) weight: Option<Expression>,
	pub(crate) chunks: Vec<Chunk>
}

impl Parse for Rhs {
	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
		Ok(Self {weight: Self::parse_weight(lexer)?, chunks: Self::parse_chunks(lexer)?})
	}
}

impl Rhs {

	fn parse_weight(lexer: &mut PeekLexer) -> ParseResult<Option<Expression>> {
		match lexer.peek_unwrap() {
			Tokens::ProductionToken(Productions::LBr) => Ok(Some(between(lexer, Tokens::ProductionToken(Productions::LBr), Tokens::MathToken(Math::RBr))?)),
			_ => Ok(None)
		}
	}

	fn parse_chunks(lexer: &mut PeekLexer) -> ParseResult<Vec<Chunk>> {
		let mut resp = Vec::new();
		resp.push(Chunk::parse(lexer)?);
		let mut check = lexer.peek();
		while check.is_some() && check != Some(&Ok(Tokens::ProductionToken(Productions::Alternative))) && check != Some(&Ok(Tokens::ProductionToken(Productions::Semi))) {
			if matches!(check, Some(Err(_))) {
				return Err(WagParseError::Fatal((lexer.span(), "An unknown error occurred during tokenizing".to_string())))
			}
			resp.push(Chunk::parse(lexer)?);
			check = lexer.peek();
		}
		Ok(resp)
	}

}