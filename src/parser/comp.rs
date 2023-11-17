use std::fmt::Display;
use std::write;

use super::ast::ToAst;
use super::{Parse, PeekLexer, ParseResult, ParseOption, Tokens, SpannableNode};

use super::helpers::TokenMapper;

use crate::lexer::{math::Math, UnsafePeek};

use super::sum::Sum;
use wagon_macros::TokenMapper;

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
pub(crate) struct Comparison {
	pub(crate) sum: SpannableNode<Sum>,
	pub(crate) comp: Option<Comp>
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
pub(crate) struct Comp {
	pub(crate) op: CompOp,
	pub(crate) right: SpannableNode<Sum>
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
        Ok(Self { sum: SpannableNode::parse(lexer)?, comp: Comp::parse_option(lexer)? })
    }

}

impl ParseOption for Comp {

    fn parse_option(lexer: &mut PeekLexer) -> ParseResult<Option<Self>> where Self: Sized {
        if let Some(op) = CompOp::token_to_enum(lexer.peek_unwrap()) {
        	lexer.next();
        	Ok(Some(Self { op, right: SpannableNode::parse(lexer)?}))
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

impl Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = &self.comp {
            write!(f, "{} {} {}", self.sum, c.op, c.right)
        } else {
            write!(f, "{}", self.sum)
        }
    }
}

impl Display for CompOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompOp::Eq => write!(f, "=="),
            CompOp::Neq => write!(f, "!="),
            CompOp::Lte => write!(f, "<="),
            CompOp::Lt => write!(f, "<"),
            CompOp::Gte => write!(f, ">="),
            CompOp::Gt => write!(f, ">"),
            CompOp::In => write!(f, "in"),
        }
    }
}