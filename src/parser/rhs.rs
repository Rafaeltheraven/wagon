use std::matches;

use crate::lexer::{UnsafePeek, Spannable};
use super::ast::ToAst;
use super::{Parse, PeekLexer, ParseResult, Tokens, WagParseError, chunk::{Chunk, ChunkP}, expression::Expression, symbol::Symbol};
use super::helpers::{between};

use crate::lexer::{productions::Productions, math::Math};

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
		} else {
			resp.push(Chunk { chunk: super::chunk::ChunkP::Unit(super::symbol::Symbol::Epsilon), ebnf: None })
		}
		Ok(resp)
	}

	pub(crate) fn empty() -> Self {
		Self {
            weight: None,
            chunks: vec![
                Chunk {
                    ebnf: None,
                    chunk: ChunkP::Unit(Symbol::Epsilon)
                }
            ]
        }
	}

	pub(crate) fn simple_ident(ident: &str) -> Self {
		Self {
			weight: None,
			chunks: vec![
				Chunk::simple_ident(ident)
			]
		}
	}

	pub(crate) fn blocks(self) -> Vec<Vec<Symbol>> {
		let mut blocks = Vec::new();
		let mut curr = Vec::new();
		for chunk in self.chunks.into_iter() {
			let symbols = match chunk {
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
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        let node = Self::add_vec_children(super::ast::WagNode::Rhs(self.weight.is_some()), self.chunks, ast);
        if let Some(expr) = self.weight {
        	let child = expr.to_ast(ast);
        	ast.add_edge(node, child, ());
        }
        node
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{chunk::Chunk, symbol::Symbol};

    use super::Rhs;

    use pretty_assertions::assert_eq;


	#[test]
	fn test_simple_gll_blocks() {
		let rhs = Rhs {
		    weight: None,
		    chunks: vec![
		    	Chunk::simple_terminal("a"),
		    	Chunk::simple_terminal("b"),
		    	Chunk::simple_ident("C"),
		    	Chunk::simple_ident("D"),
		    	Chunk {
		    		ebnf: None,
		    		chunk: crate::parser::chunk::ChunkP::Group(vec![Chunk::simple_terminal("e"), Chunk::simple_ident("F")])
		    	}
		    ],
		};
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