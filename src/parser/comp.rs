use super::ast::ToAst;
use super::{Parse, PeekLexer, ParseResult, ParseOption, Tokens};

use super::helpers::TokenMapper;
use crate::lexer::{math::Math, UnsafePeek};

use super::sum::Sum;
use wagon_macros::TokenMapper;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub(crate) struct Comparison {
	pub(crate) sum: Sum,
	pub(crate) comp: Option<Comp>
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub(crate) struct Comp {
	pub(crate) op: CompOp,
	pub(crate) right: Sum
}

#[derive(TokenMapper, PartialEq, Debug, Eq, Hash, Clone)]
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

impl ToAst for Comparison {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        let node = if let Some(op) = self.comp {
        	let node = ast.add_node(super::ast::WagNode::Comparison(Some(op.op)));
        	let child = op.right.to_ast(ast);
        	ast.add_edge(node, child, ());
        	node
        } else {
        	ast.add_node(super::ast::WagNode::Comparison(None))
        };
        let child = self.sum.to_ast(ast);
        ast.add_edge(node, child, ());
        node
    }
}