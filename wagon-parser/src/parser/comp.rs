use std::fmt::Display;
use std::write;

use super::{Parse, PeekLexer, ParseResult, ParseOption, Tokens, SpannableNode, ToAst, WagNode, WagIx, WagTree};

use super::helpers::TokenMapper;

use wagon_lexer::{math::Math, UnsafePeek};

use super::sum::Sum;
use wagon_macros::TokenMapper;

use quote::{ToTokens, quote};

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// A comparison between two [`Sum`]s.
///
/// If `comp == None`, then this is just a `Sum`.
pub struct Comparison {
    /// The left-hand side of the comparison
	pub sum: SpannableNode<Sum>,
    /// The optional operator and right-hand side.
	pub comp: Option<Comp>
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// The operator and right-hand side of a comparison.
pub struct Comp {
    /// The operator.
	pub op: CompOp,
    /// The right-hand side.
	pub right: SpannableNode<Sum>
}

#[derive(TokenMapper, PartialEq, Debug, Eq, Hash, Clone)]
/// All possible comparison operators.
pub enum CompOp {
    /// `==`
	Eq,
    /// `!=` 
	Neq,
    /// `<=`
	Lte,
    /// `<`
	Lt,
    /// `>=`
	Gte,
    /// `>`
	Gt,
    /// `in`
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
    fn to_ast(self, ast: &mut WagTree) -> WagIx {
        let node = if let Some(op) = self.comp {
        	let node = ast.add_node(WagNode::Comparison(Some(op.op)));
        	let child = op.right.to_ast(ast);
        	ast.add_edge(node, child, ());
        	node
        } else {
        	ast.add_node(WagNode::Comparison(None))
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

impl ToTokens for CompOp {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        match self {
            CompOp::Eq => tokens.extend(quote!(==)),
            CompOp::Neq => tokens.extend(quote!(!=)),
            CompOp::Lte => tokens.extend(quote!(<=)),
            CompOp::Lt => tokens.extend(quote!(<)),
            CompOp::Gte => tokens.extend(quote!(>=)),
            CompOp::Gt => tokens.extend(quote!(>)),
            CompOp::In => panic!("Should be a special case!"),
        };
    }
}
