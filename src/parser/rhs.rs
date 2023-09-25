use std::matches;

use crate::lexer::{UnsafePeek, Spannable};
use super::ast::ToAst;
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError};
use super::helpers::{between};

use crate::lexer::{productions::Productions, math::Math};

use super::expression::Expression;
use super::chunk::Chunk;

#[derive(PartialEq, Debug, Eq, Hash)]
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
		if lexer.peek() != Some(&Ok(Tokens::ProductionToken(Productions::Semi))) { // If we immediately encounter a ;, this is an empty rule
			resp.push(Chunk::parse(lexer)?);
			let mut check = lexer.peek();
			while check.is_some() && check != Some(&Ok(Tokens::ProductionToken(Productions::Alternative))) && check != Some(&Ok(Tokens::ProductionToken(Productions::Semi))) {
				if matches!(check, Some(Err(_))) {
					return Err(WagParseError::Fatal((lexer.span(), "An unknown error occurred during tokenizing".to_string())))
				}
				resp.push(Chunk::parse(lexer)?);
				check = lexer.peek();
			}
		}
		Ok(resp)
	}
}

impl ToAst for Rhs {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        let node = Self::add_vec_children(super::ast::WagNode::Rhs(self.weight.is_some()), self.chunks, ast);
        if let Some(expr) = self.weight {
        	let child = expr.to_ast(ast);
        	ast.add_edge(node, child, ());
        }
        node
    }
}