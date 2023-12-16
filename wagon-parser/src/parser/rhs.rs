use crate::parser::Span;
use std::matches;

use wagon_lexer::{Spannable};
use super::SpannableNode;
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError, chunk::{Chunk}, expression::Expression, symbol::Symbol, ToAst, WagNode, WagIx, WagTree};
use super::helpers::{between};

use wagon_lexer::{productions::Productions, math::Math};

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash)]
#[cfg_attr(test, new_unspanned)]
pub struct Rhs {
	pub weight: Option<SpannableNode<Expression>>,
	pub probability: Option<SpannableNode<Expression>>,
	pub chunks: Vec<SpannableNode<Chunk>>,
}

impl Parse for Rhs {
	fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> {
		Ok(Self {weight: Self::parse_weight(lexer)?, probability: Self::parse_probability(lexer)?, chunks: Self::parse_chunks(lexer)?})
	}
}

impl Rhs {

	fn parse_weight(lexer: &mut PeekLexer) -> ParseResult<Option<SpannableNode<Expression>>> {
		match lexer.peek() {
			Some(Ok(Tokens::ProductionToken(Productions::LBr))) => Ok(Some(between(lexer, Tokens::ProductionToken(Productions::LBr), Tokens::MathToken(Math::RBr))?)),
			_ => Ok(None)
		}
	}

	fn parse_probability(lexer: &mut PeekLexer) -> ParseResult<Option<SpannableNode<Expression>>> {
		match lexer.peek() {
			Some(Ok(Tokens::ProductionToken(Productions::Colon))) => Ok(Some(between(lexer, Tokens::ProductionToken(Productions::Colon), Tokens::MathToken(Math::Colon))?)),
			_ => Ok(None)
		}
	}

	fn parse_chunks(lexer: &mut PeekLexer) -> ParseResult<Vec<SpannableNode<Chunk>>> {
		let mut resp = Vec::new();
		if lexer.peek() != Some(&Ok(Tokens::ProductionToken(Productions::Semi))) { // If we immediately encounter a ;, this is an empty rule
			resp.push(SpannableNode::parse(lexer)?);
			let mut check = lexer.peek();
			while check.is_some() && check != Some(&Ok(Tokens::ProductionToken(Productions::Alternative))) && check != Some(&Ok(Tokens::ProductionToken(Productions::Semi))) {
				if matches!(check, Some(Err(_))) {
					return Err(WagParseError::Fatal((lexer.span(), "An unknown error occurred during tokenizing".to_string())))
				}
				resp.push(SpannableNode::parse(lexer)?);
				check = lexer.peek();
			}
		} else {
			resp.push(SpannableNode::new(Chunk::empty(), lexer.span()))
		}
		Ok(resp)
	}

	pub(crate) fn empty() -> Self {
		Self {
            weight: None,
            probability: None,
            chunks: vec![
                Chunk::empty().into()
            ]
        }
	}

	pub(crate) fn empty_spanned(span: Span) -> SpannableNode<Self> {
		SpannableNode::new(Self {
			weight: None,
			probability: None,
			chunks: vec![
				Chunk::empty_spanned(span.clone())
			]
		}, span)
	}

	pub(crate) fn simple_ident(ident: &str) -> Self {
		Self {
			weight: None,
			probability: None,
			chunks: vec![
				Chunk::simple_ident(ident).into()
			]
		}
	}

	pub(crate) fn simple_terminal(term: &str) -> Self {
		Self {
			weight: None,
			probability: None,
			chunks: vec![Chunk::simple_terminal(term).into()],
		}
	}

	pub fn blocks(self) -> Vec<Vec<Symbol>> {
		let mut blocks = Vec::new();
		let mut curr = Vec::new();
		for chunk in self.chunks.into_iter() {
			let symbols = match chunk.into_inner() {
				Chunk { ebnf: Some(_), .. } => panic!("{:?}", "Encountered an EBNF-chunk when calculating GLL-blocks. Should have been factored out"),
				c => c.extract_symbols(), // Deal with groups
			};
			for symbol in symbols {
				let is_terminal = symbol.is_terminal();
				curr.push(symbol);
				if !is_terminal {
					blocks.push(curr);
					curr = Vec::new();
				}
			}
		}
		blocks.push(curr);
		blocks
	}
}

impl ToAst for Rhs {
    fn to_ast(self, ast: &mut WagTree) -> WagIx {
        let node = Self::add_vec_children(WagNode::Rhs(self.weight.is_some()), self.chunks, ast);
        if let Some(expr) = self.weight {
        	let child = expr.to_ast(ast);
        	ast.add_edge(node, child, ());
        }
        node
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::chunk::ChunkP;
	use crate::parser::{chunk::Chunk, symbol::Symbol};

    use super::Rhs;

    use pretty_assertions::assert_eq;
    use wagon_macros::unspanned_tree;


	#[test]
	fn test_simple_gll_blocks() {
		let rhs = unspanned_tree!(Rhs {
		    weight: None,
		    probability: None,
		    chunks: vec![
		    	Chunk::simple_terminal("a"),
		    	Chunk::simple_terminal("b"),
		    	Chunk::simple_ident("C"),
		    	Chunk::simple_ident("D"),
		    	Chunk {
		    		chunk: ChunkP::Group(vec![Chunk::simple_terminal("e"), Chunk::simple_ident("F")]),
		    		ebnf: None
		    	}
		    ],
		});
		let blocks = rhs.blocks();
		let expected = vec![
			vec![
				Symbol::simple_terminal("a"),
		    	Symbol::simple_terminal("b"),
		    	Symbol::simple_ident("C"),
			],
			vec![
				Symbol::simple_ident("D")
			],
			vec![
				Symbol::simple_terminal("e"),
				Symbol::simple_ident("F")
			],
			vec![]
		];
		assert_eq!(blocks, expected);

	}

}