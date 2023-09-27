use super::{Parse, PeekLexer, ParseResult, Tokens, ast::ToAst};
use crate::lexer::{math::Math};
use super::conjunct::Conjunct;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub(crate) struct Disjunct(pub(crate) Vec<Conjunct>);

impl Parse for Disjunct {
    
    fn parse(lexer: &mut PeekLexer) -> ParseResult<Self> where Self: Sized {
        Ok(Self(Conjunct::parse_sep(lexer, Tokens::MathToken(Math::And))?))
    }

}

impl ToAst for Disjunct {
    fn to_ast(self, ast: &mut super::ast::WagTree) -> super::ast::WagIx {
        let node = super::ast::WagNode::Disjunct;
        Self::add_vec_children(node, self.0, ast)
    }
}