use std::fmt::Display;

use super::ast::ToAst;
use super::{Parse, PeekLexer, ParseResult, Tokens};
use crate::lexer::{math::Math};

use super::inverse::Inverse;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub(crate) struct Conjunct(pub(crate) Vec<Inverse>);

impl Parse for Conjunct {
    
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized {
        Ok(Self(Inverse::parse_sep(lexer, Tokens::MathToken(Math::Or))?))
    }

}

impl ToAst for Conjunct {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        let node = super::ast::WagNode::Conjunct;
        Self::add_vec_children(node, self.0, ast)
    }
}

impl Display for Conjunct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" or "))
    }
}