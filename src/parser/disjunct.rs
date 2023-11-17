use std::{fmt::Display, write};

use super::{Parse, PeekLexer, ParseResult, Tokens, ast::ToAst, SpannableNode};
use crate::lexer::{math::Math};
use super::conjunct::Conjunct;

#[cfg(test)]
use wagon_macros::new_unspanned;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
#[cfg_attr(test, new_unspanned)]
pub(crate) struct Disjunct(pub(crate) Vec<SpannableNode<Conjunct>>);

impl Parse for Disjunct {
    
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized {
        Ok(Self(SpannableNode::parse_sep(lexer, Tokens::MathToken(Math::And))?))
    }

}

impl ToAst for Disjunct {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        let node = super::ast::WagNode::Disjunct;
        Self::add_vec_children(node, self.0, ast)
    }
}

impl Display for Disjunct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" and "))
    }
}