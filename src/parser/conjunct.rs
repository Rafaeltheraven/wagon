use super::ast::ToAst;
use super::{Parse, PeekLexer, ParseResult, Tokens};
use crate::lexer::{math::Math};

use super::inverse::Inverse;

#[derive(PartialEq, Debug, Eq, Hash)]
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