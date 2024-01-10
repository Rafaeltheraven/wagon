use std::{fmt::Display, write};

use super::{Parse, PeekLexer, ParseResult, Tokens, SpannableNode, ToAst, WagNode, WagIx, WagTree};

use wagon_lexer::math::Math;

use super::comp::Comparison;

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
/// Either another `Inverse`, prepend by `!` or just a `[Comparison]`.
pub enum Inverse {
    /// `!`
	Not(Box<SpannableNode<Inverse>>),
    /// The next layer down.
	Comparison(SpannableNode<Comparison>)
}

pub(crate) enum InverseNode {
    Not,
    Comparison
}

impl Parse for Inverse {

    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized {
        if lexer.next_if_eq(&Ok(Tokens::MathToken(Math::Not))).is_some() {
            Ok(Self::Not(Box::new(SpannableNode::parse(lexer)?)))
        } else {
            Ok(Self::Comparison(SpannableNode::parse(lexer)?))
        }
    }

}

impl ToAst for Inverse {
    fn to_ast(self, ast: &mut WagTree) -> WagIx {
        match self {
            Inverse::Not(b) => {
                let node = ast.add_node(WagNode::Inverse(InverseNode::Not));
                let child = (*b).to_ast(ast);
                ast.add_edge(node, child, ());
                node
            },
            Inverse::Comparison(c) => {
                let node = ast.add_node(WagNode::Inverse(InverseNode::Comparison));
                let child = c.to_ast(ast);
                ast.add_edge(node, child, ());
                node
            },
        }
    }
}

impl Display for Inverse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Inverse::Not(i) => write!(f, "not {}", i),
            Inverse::Comparison(c) => write!(f, "{}", c),
        }
    }
}