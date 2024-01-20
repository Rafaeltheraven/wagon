use std::fmt::Display;
use std::write;

use super::{Parse, LexerBridge, ParseResult, ParseOption, Tokens, SpannableNode, ToAst, WagNode, WagIx, WagTree, ResultPeek};

use super::helpers::TokenMapper;

use wagon_lexer::math::Math;

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

    fn parse(lexer: &mut LexerBridge) -> ParseResult<Self> where Self: Sized {
        Ok(Self { sum: SpannableNode::parse(lexer)?, comp: Comp::parse_option(lexer)? })
    }

}

impl ParseOption for Comp {

    fn parse_option(lexer: &mut LexerBridge) -> ParseResult<Option<Self>> where Self: Sized {
        if let Some(op) = CompOp::token_to_enum(lexer.peek_result()?) {
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
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
            Self::Lte => write!(f, "<="),
            Self::Lt => write!(f, "<"),
            Self::Gte => write!(f, ">="),
            Self::Gt => write!(f, ">"),
            Self::In => write!(f, "in"),
        }
    }
}

impl ToTokens for CompOp {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        match self {
            Self::Eq => tokens.extend(quote!(==)),
            Self::Neq => tokens.extend(quote!(!=)),
            Self::Lte => tokens.extend(quote!(<=)),
            Self::Lt => tokens.extend(quote!(<)),
            Self::Gte => tokens.extend(quote!(>=)),
            Self::Gt => tokens.extend(quote!(>)),
            Self::In => unimplemented!("Should be a special case!"),
        };
    }
}
